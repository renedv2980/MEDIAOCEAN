*          DATA SET SRDAR00    AT LEVEL 029 AS OF 06/10/20                      
*PHASE T16100C                                                                  
T16100   TITLE 'SRDAR00 ($DARE) - DARE UPDATE FACILITY'                         
T16100   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKX-WORKD,**$DAR**,RA,CLEAR=YES,RR=RE                          
         USING WORKD,RC            RC = A(WORKING STORAGE)                      
         ST    RE,RELO                                                          
         ST    RD,SAVERD                                                        
*                                                                               
         MVC   SRPARMS(8*4),0(R1)  SAVE SERVICE REQUEST PARAMETER LIST          
SRPARMSD USING SRPARMD,SRPARMS                                                  
*                                                                               
         L     R9,SRPARMSD.SRQASYSF                                             
         USING SYSFACD,R9          R9 = A(SYSTEM FACILITIES)                    
         L     RF,VSYSFAC2         GET THE SPOT SYSFAC                          
         USING SPSYSFAC,RF                                                      
         MVC   VRECUP,SRECUP                                                    
         DROP  RF                                                               
*                                                                               
         BAS   RE,INITLIZE         INITIALIZE COMMON VARIABLES                  
*                                                                               
         BAS   RE,CKCTLSYS         CHECK STATUS OF CONTROL SYSTEM               
         BNE   XIT                 READ-ONLY, LEAVE                             
*                                                                               
         GOTO1 VGETFACT,DMCB,0                                                  
         L     RE,0(R1)                                                         
         USING FACTSD,RE                                                        
         MVC   THESYSID,FASYSID    SAVE THE SYSTEM ID NUMBER                    
         DROP  RE                                                               
*                                                                               
         L     RF,AFACIDT          CHECK THE SYSTEM TYPE                        
         USING FACITABD,RF                                                      
SRDAR_00 CLI   0(RF),X'FF'                                                      
         JE    *+2                 THIS BETTER EXIST ON FACIDTAB                
*                                                                               
         CLC   FACIID,THESYSID                                                  
         BE    *+12                                                             
         LA    RF,L'FACITAB(RF)                                                 
         B     SRDAR_00                                                         
*                                                                               
         TM    FACIFL,FACIREP      ARE WE ON THE REP SYSTEM?                    
         BNZ   SRDARREP            YES                                          
         DROP  RF                                                               
*                                                                               
         CLI   THESYSID,6          ARE WE ON THE MEL SYSTEM?                    
         BE    XIT                 YES, DON'T PROCESS ANYTHING                  
         CLI   THESYSID,11         ARE WE ON THE CSC SYSTEM?                    
         BE    XIT                 YES, DON'T PROCESS ANYTHING                  
         CLI   THESYSID,13         ARE WE ON THE ADV8?                          
         BE    XIT                 YES, DON'T PROCESS ANYTHING FOR NOW          
*                                                                               
         MVI   WHCHEDCT,C'A'       NO, USE THE ADV EDICT FILE                   
         BAS   RE,PROCEDCT         PROCESS THE EDICT FILE                       
         CLI   THESYSID,1          ARE WE ON THE TST SYSTEM?                    
         BE    SRDARREP                                                         
         CLI   THESYSID,15             OR ON THE FQA SYSTEM?                    
         BNE   SRDARX              NO                                           
*                                  YES, THEN PROCESS REP EDICT FILE TOO         
SRDARREP MVI   WHCHEDCT,C'R'       USE THE REP EDICT FILE                       
         BAS   RE,PROCEDCT         PROCESS THE EDICT FILE                       
*                                                                               
SRDARX   BRAS  RE,UPDATCTL         NEED TO WRITE TO CTFILE?                     
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* INITIALIZES COMMON VARIABLES                                                  
***********************************************************************         
INITLIZE NTR1                                                                   
         MVI   BITFLAG1,0                                                       
*                                                                               
         LR    R7,RC                                                            
         AHI   R7,IOA1-WORKD                                                    
         ST    R7,AIO1                                                          
*                                                                               
         LR    R7,RC                                                            
         AHI   R7,IOA2-WORKD                                                    
         ST    R7,AIO2                                                          
*                                                                               
         LR    R7,RC                                                            
         AHI   R7,SPULAREA-WORKD                                                
         ST    R7,ASPLAREA                                                      
*                                                                               
         LR    R7,RC               FOR SAVING EDICT RECORDS                     
         AHI   R7,WRKRIOA-WORKD                                                 
         ST    R7,AWRKRIOA                                                      
*                                                                               
         MVC   AWRKRBUF,SRPARMSD.SRQATIA                                        
*                                                                               
         LR    R7,RC               FOR EDICT                                    
         AHI   R7,HUGEBLCK-WORKD                                                
         ST    R7,AHUGEBLK                                                      
*                                                                               
         L     R8,VSSB             R8 = A(SSB)                                  
         USING SSBD,R8                                                          
         NI    SSBJFLAG,255-SSBJFWKR STOP ABEND LOOPS                           
         MVC   AFACIDT,SSBAFID                                                  
         MVC   SYSNAME,SSBSYSNA                                                 
         MVC   SYSN1,SSBSYSN1                                                   
         MVC   RECLEN,SSBTWAL      SAVE TEMPSTR TWA RECORD LENGTH               
         NI    SSBDARFL,X'FF'-SSBDRSDR     DON'T HAVE TASKER CALL AGAIN         
         DROP  R8                                                               
*                                                                               
         L     R1,SRPARMSD.SRQAUTL                                              
         USING UTLD,R1                                                          
         OI    TPRGIND,X'14'       SET CONVERTED MAX IOS                        
         MVC   TERMNUM,TNUM        SAVE TERMINAL NUMBER                         
         MVC   USERNUM,TUSER       SAVE USER ID NUMBER                          
         XC    TUSER,TUSER         DUMMY TERMINALS SHOULD NOT HAVE ID #         
         DROP  R1                                                               
***************                                                                 
* COMFACS STUFF                                                                 
***************                                                                 
         L     R1,SRPARMSD.SRQACOMF    R1 = A(COMFACS)                          
         USING COMFACSD,R1                                                      
         MVC   VADDAY,CADDAY                                                    
         MVC   VDATCON,CDATCON                                                  
         MVC   VGETFACT,CGETFACT                                                
         MVC   VHELLO,CHELLO                                                    
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VLOCKET,CLOCKET                                                  
         MVC   VSWITCH,CSWITCH                                                  
         DROP  R1                                                               
         L     R1,RELO                                                          
         L     R0,=A(GOMSPACK)                                                  
         AR    R0,R1                                                            
         ST    R0,VMSPACK                                                       
         L     R0,=A(GOMSUNPK)                                                  
         AR    R0,R1                                                            
         ST    R0,VMSUNPK                                                       
***************                                                                 
* CORERES STUFF                                                                 
***************                                                                 
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4,=X'D9000A0C'    SPOOL                                     
         GOTO1 VCALLOV,DMCB                                                     
         MVC   ASPOOL,DMCB                                                      
*                                                                               
         MVI   DMCB+7,X'15'           CLUNPK                                    
         GOTO1 VCALLOV,DMCB                                                     
         MVC   VCLUNPK,DMCB                                                     
*                                                                               
         MVI   DMCB+7,X'7A'           STAPACK                                   
         GOTO1 VCALLOV,DMCB                                                     
         MVC   ASTAPACK,DMCB                                                    
*                                                                               
         MVI   DMCB+7,X'AC'           REPFACS                                   
         GOTO1 VCALLOV,DMCB                                                     
         MVC   AREPFACS,DMCB                                                    
*                                                                               
         MVI   DMCB+7,X'3E'           DDGETDARE                                 
         GOTO1 VCALLOV,DMCB                                                     
         MVC   AGETDARE,DMCB                                                    
*                                                                               
         MVI   DMCB+7,X'2B'           SPGETBUY                                  
         GOTO1 VCALLOV,DMCB                                                     
         MVC   AGETBUY,DMCB                                                     
***************                                                                 
* GET TODAY'S DATE                                                              
***************                                                                 
         GOTO1 VDATCON,DMCB,(5,0),(0,WORK)                                      
*                                                                               
         GOTO1 VDATCON,DMCB,(0,WORK),(3,BTODAY)                                 
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,PACKOF4B                                                      
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
*                                                                               
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         BL    INIT10                                                           
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 VADDAY,DMCB,WORK,WORK,F'1'                                       
*                                                                               
INIT10   GOTO1 VDATCON,DMCB,(3,BTODAY),(15,JDTTODAY)                            
         LLC   R1,BTODAY+2         GET DATE NUMBER                              
         CVD   R1,DUB                                                           
         L     R1,DUB+4                                                         
         SRL   R1,4                                                             
         STCM  R1,1,DATENUM                                                     
*                                                                               
INITX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CHECKS THE STATUS OF THE CONTROL SYSTEM TO SEE IF READ-ONLY                   
*                                                                               
* ON EXIT:     (CC)                EQ - CONTROL SYSTEM IS WRITABLE              
*                                  NE - CONTROL SYSTEM IS READ-ONLY             
***********************************************************************         
CKCTLSYS NTR1                                                                   
         L     R1,VSELIST                                                       
         ZICM  RE,0(R1),2          SET UP BXLE USING (RE,RF)                    
         ICM   RF,15,2(R1)                                                      
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
*                                                                               
CCSYS10  CLC   SENAME,=C'CONTROL'                                               
         BNE   CCSYS20                                                          
         TM    SEIND,SEISETRO      SET TO READ-ONLY STATUS?                     
         BNZ   CCSYSNO                                                          
         B     CCSYSYES                                                         
*                                                                               
CCSYS20  BXLE  R1,RE,CCSYS10       CHECK NEXT ENTRY IN SELIST                   
*                                                                               
CCSYSYES B     YES                                                              
*                                                                               
CCSYSNO  B     NO                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE FILE WHERE RECORDS ARE READ                                     
***********************************************************************         
PROCEDCT NTR1                                                                   
         BAS   RE,DAREINIT         INITIALIZE EDICT/DARE STUFF                  
         BNE   PRCSSXIT            EDICT FILE NO-OPED, NOTHING TO DO            
*                                                                               
         L     R7,AHUGEBLK         R7 = A(HUGE BLOCK)                           
         USING EDFILD,R7                                                        
*                                                                               
         BAS   RE,SETUPSSB         GET SSB STUFF                                
         BE    PRCSS20             GOT TODAY'S LATEST INFO                      
*                                                                               
PRCSS10  LA    R2,1                                                             
PRCSS15  STC   R2,EDCTFDSK+2       BLOCK NUMBER                                 
         MVI   EDCTFDSK+3,0                                                     
*                                                                               
PRCSS20  TM    BITFLAG1,BF1SKPTB   SKIP TRACK & BLOCK ?                         
         BNZ   *+8                                                              
         BAS   RE,TRKBLK           GETS RIGHT TRACK & BLOCK                     
*                                                                               
         BAS   RE,READBLK             DADDS CALL                                
         BNE   PRCSSX                                                           
*                                                                               
         CLI   RECSKIP,0           TAB INTO BLOCK?                              
         BE    PRCSS25                                                          
         BAS   RE,RECBUMP                                                       
         B     PRCSS30                                                          
*                                                                               
PRCSS25  LA    R1,1                FIRST RECORD IN BLOCK                        
         STC   R1,RECNUM                                                        
         OI    BITFLAG1,BF1FRSTR                                                
*                                                                               
PRCSS30  CLI   EDFMON,EDFMONPQ     IS THIS A PERMANENT RECORD?                  
         BE    PRCSSNXR            YES - IGNORE                                 
*                                                                               
         TM    BITFLAG1,BF1YSDAY   PRIOR DATE'S DATA?                           
         BZ    PRCSS33             NO, TODAY'S DATA                             
*                                                                               
         CLC   EDFMON,PRIORDAT+1   DONE WITH A PRIOR DATE?                      
         BE    PRCSS35              - NO                                        
*                                   - YES CHECK NEXT DATE                       
         GOTO1 VDATCON,DMCB,(3,PRIORDAT),(0,DUB)                                
         GOTOR VADDAY,DMCB,DUB,DUB,F'1'                                         
         GOTOR VDATCON,DMCB,(0,DUB),(3,PRIORDAT)                                
*                                                                               
         LA    RE,PRIORDAT                                                      
         CLC   PRIORDAT,BTODAY     IS PRIOR DATE EARLIER THAN TODAY?            
         BL    PRCSS31              YES - PROCESS PRIOR DATE                    
         LA    RE,BTODAY            OTHERWISE, DO TODAY                         
         NI    BITFLAG1,X'FF'-BF1YSDAY   ===>  TODAY  <===                      
*                                                                               
PRCSS31  GOTO1 DAREDATE,DMCB,(RE)     NO, NO MORE FOR PRIOR DATE                
*                                                                               
*                                                                               
         MVC   EDCTFDSK(L'FRSTTRK),FRSTTRK                                      
         MVI   EDCTFDSK+2,1                                                     
         MVI   EDCTFDSK+3,0                                                     
         MVI   RECNUM,1                                                         
         BRAS  RE,UPDATSSB                                                      
         B     PRCSSXIT                                                         
*                                                                               
PRCSS33  CLC   EDFMON,BTODAY+1     DONE WITH TODAY'S EDICT?                     
         BNE   PRCSSX              YES, DO OUR CLEANUP                          
*                                                                               
PRCSS35  CLI   EDFSTAT,EDFNOOP     NO-OP RECORD?                                
         BE    PRCSSNXR            YES - IGNORE                                 
         CLI   EDFSYS,EDFDAREQ     DARE RECEIVED RECORD?                        
         BNE   PRCSSNXR            NO                                           
*                                                                               
PRCSS40  BAS   RE,WORKITIN         PROCESS THE DARE RECORD                      
*                                                                               
         TM    BITFLAG1,BF1DPEND   DARE TRANSACTION PENDING?                    
         BNZ   PRCSSNXR                                                         
         GOTO1 VDATAMGR,DMCB,(0,=C'COMMIT'),0  COMMIT                           
*                                                                               
PRCSSNXR GOTO1 VGETFACT,DMCB,(X'80',0),F#TCBD                                   
         L     R1,0(R1)            GET A(TCB)                                   
         USING F@TCBD,R1                                                        
         CLC   F@BIOCNT,=H'8500'   1500 IO BREATHING ROOM                       
         BNL   PRCSSX                                                           
         DROP  R1                                                               
*                                                                               
PRCSS50  AH    R7,EDCTLRCL         BUMP TO THE NEXT RECORD                      
         LLC   R1,RECNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,RECNUM                                                        
*                                                                               
         CH    R1,EDCTRPBQ         ANY MORE RECORDS IN THIS BLOCK?              
         BNH   PRCSS30             YES                                          
*                                                                               
         LLC   R2,EDCTFDSK+2       NO                                           
         LA    R2,1(R2)                                                         
         MVI   RECSKIP,0                                                        
         OI    BITFLAG1,BF1SKPTB   SKIP TRACK/BLOCK READ                        
*                                                                               
         L     R7,AHUGEBLK         R7 = A(HUGE BLOCK)                           
*                                                                               
         CH    R2,EDCTFRPT         ANY MORE BLOCKS ON THIS TRACK?               
         BNH   PRCSS15             YES                                          
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,3,EDCTFDSK                                                    
         LA    R2,1(R2)                                                         
         CLM   R2,3,EDCTFLST       ANY MORE TRACKS?                             
         BH    PRCSSX                                                           
         STCM  R2,3,EDCTFDSK       TRACK NUMBER                                 
         B     PRCSS10             YES                                          
*                                                                               
PRCSSX   TM    BITFLAG1,BF1DPEND   DARE TRANSACTION PENDING?                    
         BNZ   *+8                 YES, WAIT FOR NEXT TIMER POP                 
         BRAS  RE,UPDATSSB         NO, UPDATE THE SSB                           
PRCSSXIT B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* INITIALIZES EDICT/DARE STUFF                                                  
***********************************************************************         
DAREINIT NTR1                                                                   
***************                                                                 
* GET ADDRESS OF EDICT FILE                                                     
***************                                                                 
         CLI   WHCHEDCT,C'R'                         REP EDICT FILE?            
         BE    DINIT10                                                          
         GOTO1 VDATAMGR,DMCB,=C'DTFADD',=C'EDCTA'    NO, SOME ADV EDICT         
         B     DINIT20                                                          
*                                                                               
DINIT10  GOTO1 VDATAMGR,DMCB,=C'DTFADD',=C'EDCTR'    YES, REP'S EDICT           
DINIT20  L     RE,DMCB+12          A(DTF)                                       
         TM    36(RE),X'40'        IS THIS FILE NO-OP?                          
         BNZ   DINITNO                                                          
*                                                                               
         MVC   EDICTFL,12(R1)      A(EDICT DCB)                                 
         MVI   EDICTFL,0           CLEAR HOB                                    
***************                                                                 
* FIND NUMBER OF PHYSICAL RECORDS PER BLOCK                                     
***************                                                                 
         GOTO1 VDADDS,DMCB,RDID,AHUGEBLK,0,EDICTFL,=X'00010100',0               
         OC    8(2,R1),8(R1)                                                    
         JNZ   *+2                 DIE ON ANY ERROR                             
*                                                                               
         L     R7,AHUGEBLK                                                      
         USING EDFILD,R7                                                        
         CLI   EDFMON,EDFMONPQ                                                  
         JNE   *+2                                                              
         MVC   EDCTFTPD,EDFTKPDY   SAVE NUMBER OF TRACKS PER DAY                
         LLC   R0,EDFBKPTK         SAVE NUMBER OF RECORDS PER BLOCK             
         STH   R0,EDCTFRPT                                                      
         LLC   R0,EDFRCPBK         SAVE NUMBER OF RECORDS PER BLOCK             
         STH   R0,EDCTRPBQ                                                      
         MVC   EDCTLRCL,EDFLRECL   SAVE LENGTH OF A LOGICAL RECORD              
         DROP  R7                                                               
*                                                                               
         GOTO1 DAREDATE,DMCB,BTODAY  CALCULATE TRACK INFO FOR TODAY             
         MVC   EDCTFDSK(L'FRSTTRK),FRSTTRK   TRACK NUMBER                       
*                                                                               
DINITYES B     YES                                                              
*                                                                               
DINITNO  B     NO                                                               
         SPACE 2                                                                
***********************************************************************         
* CALCULATES TRACK INFORMATION FOR A SPECIFIED DATE                             
*                                                                               
* ON ENTRY:    PARAM 1             A(YMD 3-BYTE BINARY)                         
***********************************************************************         
DAREDATE NTR1                                                                   
         L     RE,0(R1)            RE=A(DATE)                                   
*                                                                               
         LLC   R2,2(RE)            DAY NUMBER                                   
         BCTR  R2,0                                                             
         MH    R2,EDCTFTPD                                                      
         LA    R2,1(R2)            R2 = STARTING TRACK NUMBER FOR DATE          
         STCM  R2,3,FRSTTRK                                                     
         LH    R1,EDCTFTPD                                                      
         BCTR  R1,0                                                             
         AR    R1,R2                                                            
         STCM  R1,3,EDCTFLST       LAST TRACK NUMBER FOR DATE                   
*                                                                               
DDATEX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS INFORMATION FOR DARE FROM THE SSB                           
***********************************************************************         
SETUPSSB NTR1                                                                   
         L     RE,AWRKRIOA         CLEAR THE WORKER IOAREA FIRST                
         LH    RF,=Y(WRECQLNQ)                                                  
         XCEFL                                                                  
         L     RE,AWRKRIOA         CLEAR THE WORKER IOAREA FIRST                
         MVI   1(RE),2             NO ENTRIES                                   
*                                                                               
         L     RE,VSSB                                                          
         USING SSBD,RE                                                          
*&&DO                                                                           
         CLC   BTODAY,SSBDARDT     GOT SOMETHING FOR TODAY?                     
         BNE   SSSB10              NO, CHECK THE FILE                           
*&&                                                                             
         LA    R1,SSBDAREA         IF TODAY, BYTES SHOULDN'T BE NULLS           
         CLI   WHCHEDCT,C'A'                                                    
         BE    *+8                                                              
         LA    R1,SSBDARER                                                      
         OC    0(L'SSBDAREA,R1),0(R1)                                           
         BZ    SSSB10               IF NULLS, CHECK THE FILE                    
         CLC   DATENUM,0(R1)       MATCH ON DAY?                                
         BE    SSSB50               NO, CHECK THE FILE                          
         DROP  RE                                                               
*                                  NO, SAVED INFO FOR TODAY?                    
SSSB10   MVI   DMCB,X'0A'          SWITCH TO CONTROL SYSTEM                     
         BAS   RE,SWTCHSYS         SUCCESSFUL SWITCH?                           
         BNE   EXITPRG             EXIT PROGRAM, SHOULD WAIT FOR IT             
*                                                                               
         XC    KEY,KEY             SEE IF IT EXISTS FOR THIS SYSTEM             
         LA    R6,KEY                                                           
         USING CTDARKEY,R6                                                      
         MVI   CTDARTYP,CTDARTYQ                                                
         MVI   CTDARSUB,CTDARSBQ                                                
         MVC   CTDARSYS,THESYSID                                                
         DROP  R6                                                               
*                                                                               
         MVI   DMINBTS,X'08'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,HIGHCT                                                        
         CLI   DMCB+8,2            RECORD DELETED?                              
         BE    SSSBNO                                                           
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
*                                                                               
         L     R6,AIO1             CHECK THE RECORD                             
         USING CTDARREC,R6                                                      
         CLC   CTDARKEY,KEY        FOUND RECORD FOR THIS SYSTEM?                
         BNE   SSSBNO              NO                                           
*                                                                               
         LA    R6,CTDAR1ST                                                      
         MVI   ELCODE,CTDARELQ     LOOK FOR THE EDICT INFO ELEMENT              
         BAS   RE,FIRSTEL                                                       
         BNE   SSSBNO                                                           
*                                                                               
         USING CTDARELD,R6                                                      
         XC    ELEM,ELEM           SAVE THIS OFF SO WE CAN COMPARE              
         MVC   ELEM(CTDARLNQ),CTDAREL                                           
*                                                                               
         L     RE,VSSB                                                          
         USING SSBD,RE                                                          
         LA    R2,SSBDAREA                                                      
         LA    R3,CTDAREDA                                                      
         CLI   WHCHEDCT,C'A'                                                    
         BE    *+12                                                             
         LA    R2,SSBDARER                                                      
         LA    R3,CTDAREDR                                                      
         DROP  RE                                                               
*                                                                               
         OC    0(L'CTDAREDA,R3),0(R3)  BETTER HAVE SOMETHING!                   
         JZ    *+2                                                              
         MVC   0(L'SSBDAREA,R2),0(R3)                                           
*                                                                               
         CLC   0(1,R3),DATENUM     DOES THE DATE NUMBER MATCH?                  
         BE    SSSB30                                                           
         BH    SSSB20              GO BACK A MONTH                              
         MVC   PRIORDAT,BTODAY                                                  
SSSB15   XC    DUB,DUB                                                          
         MVC   DUB+6(1),0(03)                                                   
         MVI   DUB+7,X'0C'                                                      
         SRP   DUB,64-1,0                                                       
         CVB   R1,DUB                                                           
         STCM  R1,1,PRIORDAT+2                                                  
         OI    BITFLAG1,BF1YSDAY   YESTERDAY'S OR SOME PRIOR DAY'S              
         GOTO1 DAREDATE,DMCB,PRIORDAT                                           
         B     SSSB30                                                           
*                                                                               
SSSB20   DS    0H                  GO BACK A MONTH                              
         GOTO1 VDATCON,DMCB,(3,BTODAY),(0,DUB)                                  
         GOTOR VADDAY,DMCB,(C'M',DUB),DUB,F'-1'     MINUS 1 MONTH               
         GOTOR VDATCON,DMCB,(0,DUB),(3,PRIORDAT)                                
         B     SSSB15                                                           
*                                                                               
SSSB30   LA    R6,KEY              TST HAS ITS OWN GENFIL NOW                   
         XC    KEY,KEY                                                          
         USING GEDARKEY,R6                                                      
         MVI   GEDARSYS,GEDARSYQ                                                
         MVI   GEDARTYP,GEDARTYQ                                                
         MVI   GEDARSTY,GEDARSTQ                                                
         MVC   GEDARFPK,THESYSID                                                
*                                                                               
         BRAS  RE,HIGHGD                                                        
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
*                                                                               
         CLC   KEY(L'GEDARKEY),KEYSAVE                                          
         BNE   SSSB50              OK, THIS DOESN'T EXIST ON GENDIR YET         
*                                                                               
         MVC   AIO,AIO2                                                         
         BRAS  RE,GETGFL                                                        
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
*                                                                               
         L     R6,AIO2                                                          
         LA    R6,GEDAR1ST                                                      
SSSB42   CLI   0(R6),0                                                          
         BE    SSSB40ER            WE HAVE A PROBLEM                            
*                                                                               
         CLI   0(R6),GEDARELQ      X'10' - SAVED EDICT INFO ELEM                
         BE    SSSB44                                                           
         LLC   R0,1(R6)            "ZIC   R0,1(R6)" IN 1 MACHINE INSTR          
         AR    R6,R0                                                            
         B     SSSB42                                                           
*                                                                               
SSSB44   LLC   RE,1(R6)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   ELEM(0),0(R6)       MAKE SURE THIS MATCHES W/ CFILE ONE          
         BE    SSSB50                                                           
*                                                                               
SSSB40ER DS    0H                                                               
         GOTO1 VHEXOUT,DMCB,THESYSID,SSBPBSYS,L'THESYSID                        
         GOTO1 VHEXOUT,DMCB,ELEM,SSBPBCTF,CTDARLNQ                              
         GOTO1 VHEXOUT,DMCB,0(R6),SSBPBGNF,GEDARLNQ                             
         WTO   TEXT=((SSBSTARL,D),(SSBPROB1,D),(SSBPROB2,D),           X        
               (SSBPROB3,D),(SSBPROB4,D),(SSBSTARL,D),(0,E)),DESC=2             
*                                                                               
         L     RD,SAVERD           LEAVE SRDAR00 ENTIRELY                       
         J     XIT                 SO WE DO NOT PROCESS ANYTHING                
*                                                                               
SSBSTARL DC    H'72'                                                            
         DC    72C'*'                                                           
*                                                                               
SSBPROB1 DC    H'48'                                                            
         DC    CL48'**** CTFILE AND GENFIL EDICT INFO DOES NOT MATCH'           
SSBPROB2 DC    H'49'                                                            
         DC    CL49'** PLEASE CONTACT WHOA AT EXT5324 OR 917-575-1226'          
*                                                                               
SSBPROB3 DC    H'45'                                                            
         DC    CL7'SYSID: '                                                     
SSBPBSYS DC    CL3'99 '                                                         
         DC    CL9' CTFILE: '                                                   
SSBPBCTF DC    CL26'?'                                                          
*                                                                               
SSBPROB4 DC    H'45'                                                            
         DC    CL10' '                                                          
         DC    CL9' GENFIL: '                                                   
SSBPBGNF DC    CL26'?'                                                          
*                                                                               
SSSB50   L     RE,VSSB                                                          
         USING SSBD,RE                                                          
         LA    R1,SSBDAREA                                                      
         CLI   WHCHEDCT,C'A'                                                    
         BE    *+8                                                              
         LA    R1,SSBDARER                                                      
*                                                                               
         MVC   EDCTFDSP,0(R1)      RESTORE LAST DAY DISP READ                   
         GOTO1 =A(DSKCNVRT),DMCB,(X'80',0),RR=RELO                              
*                                                                               
         MVI   RECSKIP,0                                                        
         OC    EDCTFDSP+1(3),EDCTFDSP+1  NO DISP AFTER THE DAY NUMBER?          
         BZ    *+10                      NONE, DON'T SKIP                       
         MVC   RECSKIP,RECNUM      AND WHICH RECORD NUMBER WE'RE UPTO           
         DROP  RE                                                               
*                                                                               
SSSBYES  B     YES                                                              
*                                                                               
SSSBNO   B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* TO FIND NEW STARTING POINT -- GIVEN RECSKIP = # RECORD TO SKIP                
*                               INTO THE BLOCK                                  
***********************************************************************         
RECBUMP  NTR1                                                                   
         LLC   R1,RECSKIP                                                       
         BCTR  R1,0                                                             
         STC   R1,RECSKIP                                                       
*                                                                               
         LA    R1,1(R1)            ADD BACK THAT 1                              
         CH    R1,EDCTRPBQ         IF LAST REC WAS LAST IN BLOCK                
         BL    RECB05              NO                                           
         MVI   RECSKIP,0           YES, THEN JUST READ NEXT BLOCK               
         SR    R1,R1                                                            
         LLC   R2,EDCTFDSK+2       BLOCK NUMBER                                 
         OI    BITFLAG1,BF1SKIPB   AND DON'T SKIP INTO IT                       
         B     RECB50                                                           
*                                                                               
RECB05   SR    R1,R1                                                            
         B     RECB40                                                           
RECB10   STCM  R2,3,EDCTFDSK       TRACK NUMBER                                 
*                                                                               
         LA    R2,1                                                             
RECB20   STC   R2,EDCTFDSK+2       BLOCK NUMBER                                 
         BAS   RE,READBLK                                                       
         L     R7,AHUGEBLK         R7-POINTER IN BLOCK                          
         USING EDFILD,R7                                                        
*                                                                               
RECB40   TM    BITFLAG1,BF1SKIPB   SKIP ANY MORE BUMPING?                       
         BO    RECBYES                                                          
         AH    R7,EDCTLRCL         BUMP TO NEXT RECORD                          
         LA    R1,1(R1)                                                         
         LLC   R2,RECSKIP                                                       
         CR    R1,R2               HAVE WE SKIPPED ENOUGH YET?                  
         BH    RECBYES             YES                                          
         CH    R1,EDCTRPBQ         ANY MORE RECORDS IN THIS BLOCK?              
         BNH   RECB40                                                           
*                                                                               
RECB50   LLC   R2,EDCTFDSK+2                                                    
         LA    R2,1(R2)            NO                                           
         CH    R2,EDCTFRPT         ANY MORE BLOCKS ON THIS TRACK?               
         BNH   RECB20              YES                                          
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,3,EDCTFDSK                                                    
         LA    R2,1(R2)                                                         
         CLM   R2,3,EDCTFLST                                                    
         BNH   RECB10                                                           
         DC    H'0'                ALL TRACKS FOR TODAY ARE USED                
*                                                                               
RECBYES  LA    R1,1(R1)                                                         
         STC   R1,RECNUM                                                        
*                                                                               
RECBX    XIT1  REGS=(R7)                                                        
         EJECT                                                                  
***********************************************************************         
* CALCULATE NEW DISK ADDRESS FROM SAVED STORAGE                                 
***********************************************************************         
TRKBLK   DS    0H                                                               
         OC    DAFILT,DAFILT                                                    
         BZ    TRKB10                                                           
         MVC   DSKAD,DAFILT                                                     
         XC    DAFILT,DAFILT                                                    
*                                                                               
TRKB10   OC    DSKAD,DSKAD                                                      
         BZ    TRKBX                                                            
         XC    EDCTFDSK,EDCTFDSK                                                
         MVC   EDCTFDSK(3),DSKAD   MOVE IN TRACK AND BLOCK NUMS                 
         MVC   RECSKIP,DSKAD+3     LAST REC NUM                                 
*                                                                               
TRKBX    BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
* PUTS THE RECORD IN AIO1 INTO THE WORKER IO AREA                               
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
*                                                                               
* NOTE: FORMAT OF THE RECORD IN AIO1 IS:                                        
*              BYTE  0             LENGTH OF THE RECORD (SAY N)                 
*              BYTES 1-(N-1)       ACTUAL RECORD                                
***********************************************************************         
PUTINIOA NTR1                                                                   
         L     R6,AWRKRIOA         R6 = A(WHERE TO PUT THE RECORD)              
         OC    0(2,R6),0(R6)       IF WE HAVE AN ERROR                          
         BZ    PIOAX               THEN DON'T ADD TO IOA                        
*                                                                               
         ZICM  RE,0(R6),2                                                       
         AR    R6,RE                                                            
*                                                                               
         CLC   =C'ORDLIN',0(R7)    WE HAVE A LOT OF ORDLINS?                    
         JNE   PIOA020                                                          
         CLC   ROLNRLIN-RORDLIND(4,R7),=C'0256'   MORE THAN 256 LINES?          
         JH    PIOAX                           YES, DON'T KEEP                  
*                                                                               
PIOA020  L     RE,AIO1             COPY RECORD OVER TO WORKER RECORD            
         ZICM  R1,0(RE),1                                                       
         BZ    PIOAX                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(RE)                                                    
*                                                                               
         LA    RE,1(R1,R6)         RE = A(WHERE TO PUT NEXT RECORD)             
         L     R6,AWRKRIOA                                                      
         SR    RE,R6                                                            
         STCM  RE,3,0(R6)                                                       
         CH    RE,=Y(WRECQLNQ)     MAKE SURE WE DON'T EXCEED THE LENGTH         
         BNH   PIOAX                                                            
         LHI   R1,*-T16100         CAN'T FIT INTO AWRKRIOA                      
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
PIOAX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DETERMINES WHICH TYPE OF DARE RECORD WE HAVE AND THEN ROUTES IT TO            
* THE CORRECT PROCESSING ROUTINE                                                
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD)                              
***********************************************************************         
WORKITIN NTR1                                                                   
         USING EDFILD,R7                                                        
         MVI   THISISMG,C'N'       DEFAULT TO NOT MG ACTION                     
         MVI   SVSTAT,0            CLEAR SAVED STATUS                           
*                                                                               
WITIN00  LA    R6,EDFDARE          R6 = A(DARE REC WITHOUT EDICT INFO)          
         LA    R1,DAREOBJS                                                      
         USING DAREOBJD,R1                                                      
*                                                                               
WITIN10  CLI   DOBJTID,0           MATCHED ON TRANSMISSION ID?                  
         BE    WITINNO             NO                                           
*                                                                               
WITIN15  CLC   DOBJTID,0(R6)       MATCH ON THIS TRANSMISSION ID?               
         BE    WITIN20                                                          
         LA    R1,DOBJNXT          NO, CHECK NEXT TRANSMISSION ID               
         B     WITIN10                                                          
*                                                                               
WITIN20  MVC   DOBJNUMB,DOBJRIDN   YES, SAVE THE RETURN ID NUMBER               
         DROP  R1                                                               
*                                                                               
         LR    R7,R6               TO FREE R6 FOR GENERAL IO STUFF              
         LLC   RF,DOBJNUMB                                                      
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     WITINTBL(RF)                                                     
*                                                                               
WITINTBL B     PROCDLNT                                                         
         B     PROCOAPP                                                         
         B     PROCOREJ                                                         
         B     PROCORCM                                                         
         B     PROCOTLR                                                         
         B     PROCOCFM                                                         
         B     PROCOLIN                                                         
         B     PROCERNT                                                         
         B     PROCARCL                                                         
         B     PROCORAK                                                         
         B     PROCMGOK                                                         
         B     PROCMGCN                                                         
         B     PROCDFAX                                                         
         B     PROCCNFX                                                         
         B     PROCMKGX                                                         
         B     PROCERFX                                                         
         B     PROCSALE                                                         
         B     PROCURL                                                          
         B     PROCMO1                                                          
         B     PROCRDAR                                                         
*                                                                               
WITINNO  B     NO                                                               
         DROP  R7                                                               
*                                                                               
DAREOBJS DC    CL6'DLNNOT',AL1(DOBJDLNQ)                                        
         DC    CL6'ORDAPP',AL1(DOBJOAPQ)                                        
         DC    CL6'ORDREJ',AL1(DOBJORJQ)                                        
         DC    CL6'ORDCOM',AL1(DOBJOCMQ)                                        
         DC    CL6'ORDTLR',AL1(DOBJOTRQ)                                        
         DC    CL6'ORDCFM',AL1(DOBJOCFQ)                                        
         DC    CL6'ORDLIN',AL1(DOBJOLNQ)                                        
         DC    CL6'ERRNOT',AL1(DOBJERRQ)                                        
         DC    CL6'AGYRCL',AL1(DOBJARCQ)                                        
         DC    CL6'ORDRCL',AL1(DOBJORAQ)                                        
         DC    CL6'MKGROK',AL1(DOBJMOKQ)                                        
         DC    CL6'MKGCAN',AL1(DOBJMCNQ)                                        
         DC    CL6'DLNFAX',AL1(DOBJDFXQ)                                        
         DC    CL6'CANFAX',AL1(DOBJCFXQ)                                        
         DC    CL6'MKGHDR',AL1(DOBJMKGQ)                                        
         DC    CL6'ERRFAX',AL1(DOBJEFXQ)                                        
         DC    CL6'ORDSAL',AL1(DOBJSALE)                                        
         DC    CL6'ORDURL',AL1(DOBJURL)                                         
         DC    CL6'ORDMO1',AL1(DOBJMO1)                                         
         DC    CL6'AGYHDR',AL1(DOBJRDRQ)                                        
         DC    CL6'AGYCAN',AL1(DOBJRDRQ)                                        
         DC    CL6'MKGAPP',AL1(DOBJRDRQ)                                        
         DC    CL6'MKGREJ',AL1(DOBJRDRQ)                                        
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
XSPREC   DC    AL2(42,32,5972)                                                  
***********************************************************************         
* PROCESSES THE DELIVERY NOTIFICATION FOR AGENCY                                
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCDLNT DS    0H                                                               
         USING RDLNNOTD,R7                                                      
PDLNTD   USING RTN2SNDR,RDNTRTRN                                                
         MVC   QREPCON,RDNTRPCN    COPY THESE VALUES                            
         MVC   QRETURN,RDNTRTRN                                                 
*                                                                               
         CLI   WHCHEDCT,C'R'       REP EDICT IN USE?                            
         BE    PROCDLNR                                                         
*                                                                               
         MVI   QMED,C'T'                                                        
         CLI   PDLNTD.RTNAGYMD+1,C'2'  (---MD OF AGYMD IS 2?)                   
         BNE   *+8                                                              
         MVI   QMED,C'R'                                                        
*                                                                               
         GOTO1 VHEXIN,DMCB,PDLNTD.RTNAGYMD,BAGYMD,2                             
         MVC   AGENCY,PDLNTD.RTNPWRCD                                           
         GOTO1 VHEXIN,DMCB,PDLNTD.RTNSYSID,BYTE,2                               
         BAS   RE,SPANKED          AGY GOT SPANKED TO DIFF ADV?                 
*                                                                               
         NI    MISCFLG1,X'FF'-MF1XMTUP  NO UPDATES TO XMT ELEM                  
*                                                                               
         CLC   BYTE,THESYSID       WRONG FACPAK?                                
         BNE   PDLNTNO             YES                                          
*                                                                               
         DROP  PDLNTD                                                           
*                                                                               
         GOTO1 CALCORDR,DMCB,RDNTORDR   CONVERT ORDER NUMBER TO BINARY          
         BNE   PDLNTNO                                                          
*                                                                               
         MVC   USERID,RDNTTOID                                                  
         BAS   RE,SWTCHCTL         SWITCH CONTROL TO SPOT SYSTEM                
         BNE   PDLNTNO                                                          
*                                                                               
         MVC   QMGGROUP,MDNTOFRI-MDLNNOTD(R7)   COULD BE DELNOT FOR MKG         
*                                                                               
         BRAS  RE,GETORDER         READ ORDER RECORD INTO AIO1                  
         BNE   PDLNTNO                                                          
         CLC   QMGGROUP,=C'  '     DELNOT FOR MKGD?                             
         BH    PDLNT250                                                         
*                                                                               
         NI    BITFLAG2,X'FF'-BF2SPNDG                                          
         NI    MISCFLG1,X'FF'-MF1NOXMT    ASSUME XMT ELEM EXISTS                
         SR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
PDLNT10  CLI   0(R6),0                                                          
         BNE   PDLNT15                                                          
         LHI   R1,REFORDNT         NEVER TRANSMITTED                            
         BRAS  RE,SNDERROR                                                      
         B     PDLNTNO                                                          
*                                                                               
PDLNT15  CLI   0(R6),DOXMTELQ                                                   
         BE    PDLNT20                                                          
PDLNT17  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PDLNT10                                                          
*                                                                               
         USING DOXMTELD,R6                                                      
PDLNT20  CLI   DOXMTSTA,QSNTPNDG   IMMEDIATE UNLOCKED?                          
         BNE   PDLNT22                                                          
         OI    BITFLAG2,BF2SPNDG                                                
         B     PDLNT17             YES, PUT DELNOT IN NEXT XMT ELEM             
*                                                                               
PDLNT22  GOTO1 VDATCON,DMCB,(0,RDNTDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,RDNTTIME,DUB+L'DOXMTDND,L'RDNTTIME                   
*                                                                               
         OC    DOXMTDND,DOXMTDND   IF NO DELIVERY DATE OR TIME ALREADY          
         BZ    PDLNT30                                                          
         OC    DOXMTDNT,DOXMTDNT                                                
         BZ    PDLNT30             THEN USE EDICT RECORD'S DATE/TIME            
*                                                                               
         CLC   DOXMTDND,DUB               IS ELEM'S DATE MORE RECENT?           
         BH    PDLNTNO                    YES, IGNORE THIS RECORD               
         BL    PDLNT30                    NO, OLDER                             
         CLC   DOXMTDNT,DUB+L'DOXMTDND    IS ELEM'S TIME MORE RECENT?           
         BNL   PDLNTNO                    YES OR SAME, IGNORE THIS REC          
*                                                                               
PDLNT30  MVC   DOXMTDND,DUB                                                     
         MVC   DOXMTDNT,DUB+L'DOXMTDND                                          
         MVC   DOXMTDID,DLNFRID    SAVE WHERE DELNOT CAME FROM                  
         MVI   SVSTAT,MNSTDELV     SINCE ORDER DSECT DOESN'T HAVE THIS          
*                                                                               
         CLC   MDNTEFLG-MDLNNOTD(L'MDNTEFLG,R7),=C'309' IF REP IS DOWN,         
         BNE   PDLNT40                                                          
         MVI   DOXMTSTA,QERRORED   ORDER IS IN ERROR                            
         MVI   SVSTAT,QERRORED                                                  
*                                                                               
PDLNT40  XC    ELEM,ELEM           COPY TRANSMISSION ELEMENT                    
         MVC   ELEM(DOXMTLNQ),DOXMTEL                                           
         DROP  R6                                                               
*                                                                               
PDLNT99  OI    MISCFLG1,MF1XMTUP   XMT HAS BEEN UPDATED                         
*                                                                               
***************                                                                 
* TIME TO PROCESS THE STATUS HISTORY ELEMENTS                                   
***************                                                                 
PDLNT100 DS    0H                                                               
         NI    BITFLAG3,X'FF'-BF3SPNDG                                          
         SR    R0,R0                                                            
         XC    SVDOSPEL,SVDOSPEL                                                
         XC    FULL,FULL          ** DON'T CLOBBER FULL **                      
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
PDLNT110 CLI   0(R6),0                                                          
         BNE   PDLNT112                                                         
         TM    MISCFLG1,MF1NOXMT          DOES IT HAVE XMT ELEMS?               
         BZ    PDLNTNO                    YES, THEN DON'T SHOW ERROR            
         LHI   R1,REFORDNT         NEVER TRANSMITTED                            
         BRAS  RE,SNDERROR                                                      
         B     PDLNTNO                                                          
*                                                                               
PDLNT112 CLI   0(R6),DOSPELQ                                                    
         BNE   PDLNT115                                                         
         MVC   REVISION,DOSPREVN-DOSPELD(R6)      SAVE REVISION                 
         XR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     PDLNT117                                                         
         MVC   SVDOSPEL(0),0(R6)                                                
*                                                                               
PDLNT115 CLI   0(R6),DOSTELQ                                                    
         BE    PDLNT120                                                         
PDLNT117 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PDLNT110                                                         
*                                                                               
         USING DOSTELD,R6                                                       
PDLNT120 CLI   DOSTSTAT,QSNTPNDG   IMMEDIATE UNLOCKED?                          
         BNE   PDLNT122                                                         
         OI    BITFLAG3,BF3SPNDG                                                
         B     PDLNT117            YES, SKIP THIS RECORD                        
*                                                                               
PDLNT122 CLI   DOSTSTAT,QAPP       APPROVED?                                    
         BE    PDLNT117            YES, SKIP THIS RECORD                        
*                                                                               
         GOTO1 VDATCON,DMCB,(0,RDNTDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,RDNTTIME,DUB+L'DOSTDATE,L'RDNTTIME                   
*                                                                               
         CLI   DOSTSTAT,DDLVRD     DO WE HAVE A DELIVERY STATUS?                
         BNE   PDLNT140                                                         
*                                                                               
         CLC   DOSTDATE,DUB        IS DATE MORE RECENT?                         
         BH    PDLNTNO             YES, IGNORE THIS RECORD                      
         BL    PDLNT130            NO, OLDER                                    
         CLC   DOSTTIME,DUB+L'DOSTDATE   IS TIME MORE RECENT?                   
         BNL   PDLNTNO             YES OR SAME, IGNORE THIS RECORD              
*                                                                               
PDLNT130 OC    FULL,FULL                                                        
         BNZ   PDLNT143            THEN IT MUST BE DELNOT FOR AGYCAN            
         MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         MVC   DOSTIDNM,DLNFRID    SAVE WHERE DELNOT CAME FROM                  
         B     PDLNT150                                                         
*                                                                               
PDLNT140 CLI   DOSTSTAT,QRCLAPPR   CHECK DELNOT AFTER RECALL ACK                
         BL    PDLNT144                                                         
         CLI   DOSTSTAT,QRCLUNKN                                                
         BL    PDLNT141                                                         
         CLI   DOSTSTAT,QRCLTRNS                                                
         BL    PDLNT144                                                         
         CLI   DOSTSTAT,QRCLWIP                                                 
         BH    PDLNT144                                                         
PDLNT141 CLI   DOSTSTAT,QRCLDELN                                                
         BNE   PDLNT117                                                         
         CLI   REVISION,0                                                       
         BE    PDLNT117                                                         
         ST    R6,FULL             SAVE IN CASE DELNOT IS FOR AGYCAN            
         B     PDLNT117                                                         
*                                                                               
PDLNT143 L     R6,FULL             THEN IT MUST BE DELNOT FOR AGYCAN            
*                                                                               
PDLNT144 TM    MISCFLG1,MF1NOXMT                                                
         BZ    PDLNT145                                                         
         XC    ELEM,ELEM                                                        
         MVC   ELEM(DOSTLNQ2),DOSTEL                                            
         DROP  R6                                                               
*                                                                               
PDLNT145 LA    R2,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
         USING DOSTELD,R2                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   DOSTEL,DOSTELQ                                                   
         MVI   DOSTLEN,DOSTLNQ2                                                 
         MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         MVC   DOSTIDNM,DLNFRID    SAVE WHERE DELNOT CAME FROM                  
         MVI   DOSTSTAT,DDLVRD                                                  
         MVI   SVSTAT,DDLVRD                                                    
         DROP  R2                                                               
*                                                                               
         GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
*                                                                               
PDLNT150 MVI   SVSTAT,DDLVRD       SINCE ORDER DSECT DOESN'T HAVE THIS          
*********                                                                       
* DO WE NEED THE REP PREFIX/OFFICE? (WON'T NEED THIS WHEN EVERYONE IS           
*    OM DESKTOP).  NEED IT AGENCIES WHO ARE HALF ON/HALF OFF OM DESKTOP         
*********                                                                       
         CLI   SVDOSPEL+DOSPIMTH-DOSPEL,C'I'   DO WE HAVE INBOX METHOD?         
         BNE   PDLNT199                        NO, NOTHING TO DO THEN           
*  LAST SENT FROM OM DESKTOP?                                                   
         TM    SVDOSPEL+DOSPFLG2-DOSPEL,DOSPOMDT LAST SENT ON OM DSKTP?         
         BNZ   PDLNT199                          YES, LEAVE DOWIG ALONE         
*                                                                               
         XC    ELEM,ELEM                                                        
PDLNTWGD USING DOWIGELD,ELEM                                                    
         L     R6,AIO1             LOOK FOR THE DOWIG ELEM                      
         XR    R0,R0                                                            
         LA    R6,DORFRST-DOKEY(R6)                                             
PDLNT153 CLI   0(R6),0                                                          
         BNE   PDLNT156                                                         
         MVI   PDLNTWGD.DOWIGEL,DOWIGELQ   NONE                                 
         MVI   PDLNTWGD.DOWIGLEN,DOWIGLNQ   LENGTH SHOULD BE X'46'              
         MVI   PDLNTWGD.DOWIGMTH,C'I'       AND METHOD IS INBOX                 
         B     PDLNT170                                                         
*                                                                               
PDLNT156 CLI   0(R6),DOWIGELQ      DO WE HAVE A DOWIG ELEM?                     
         BE    PDLNT159                                                         
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PDLNT153                                                         
*                                                                               
         USING DOWIGELD,R6                                                      
PDLNT159 LLC   R1,DOWIGLEN          MOVE DOWIG ELEM INTO ELEM                   
         BCTR  R1,0                    SO THAT WE ONLY NEED ONE TEST            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),DOWIGEL                                                  
*                                                                               
         CLI   DOWIGLEN,X'43'      ARE WE THE OLD LENGTH?                       
         BH    PDLNT160            NO, WE'RE HIGHER SO NO WORRIES               
*  OLD ELEM'S REP & OFFICE WAS ORG AT DOWIGFXN AND ONLY 10 BYTES                
         MVI   PDLNTWGD.DOWIGLEN,DOWIGLNQ    NEW LENGTH                         
         CLI   PDLNTWGD.DOWIGMTH,C'I'       IF METHOD IS INBOX                  
         BNE   PDLNT160                     OTHERWISE LEAVE IT ALONE            
         MVC   PDLNTWGD.DOWIGRPP,DOWIGFXN                                       
         MVC   PDLNTWGD.DOWIGRPO,DOWIGFXN+L'DOWIGRPP                            
         XC    PDLNTWGD.DOWIGFXN,PDLNTWGD.DOWIGFXN                              
*                                                                               
PDLNT160 GOTO1 VRECUP,DMCB,(C'S',AIO1),(R6),(R6)  DELETE OLD DOWIG              
*                                                                               
PDLNT170 CLI   SVDOSPEL+DOSPIDST-DOSPELD,C'S'  WAS IT TO STATION INBOX?         
         BNE   PDLNT172                                                         
         MVC   PDLNTWGD.DOWIGRPP(L'RDNTFRID),RDNTFRID                           
         B     PDLNT180                                                         
*                                                                               
PDLNT172 GOTOR AGETDARE,DMCB,(C'U',0),RDNTFRID,AIO2,F'6000',VDATAMGR            
         BNE   PDLNT180                                                         
*                                                                               
         L     RE,AIO2                                                          
         USING DAREPTD,RE                                                       
         LLC   R1,DAPTLPFX                                                      
         BCTR  R1,0                                                             
PDLNT175 EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PDLNTWGD.DOWIGRPP(0),RDNTFRID                                    
         OC    PDLNTWGD.DOWIGRPP,=CL8' '                                        
         TM    DAPTFLG1,DPF1NOOF   X'80-REP USES OFFICE?                        
         BNZ   PDLNT180            NO                                           
         LA    R1,RDNTFRID+1(R1)   YES, COPY THE OFFICE (AFTER PREFIX)          
         MVC   PDLNTWGD.DOWIGRPO(2),0(R1)                                       
*                                                                               
PDLNT180 GOTO1 VRECUP,DMCB,(C'S',AIO1),ELEM,(R6)  ADD IN NEW DOWIG              
*                                                                               
PDLNT199 B     PDLNT300                                                         
         DROP  PDLNTWGD,RE                                                      
***************                                                                 
* DELNOT'S FOR AGY REPSONSE TO REP MAKEGOODS                                    
***************                                                                 
PDLNT250 DS    0H                                                               
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
         MVC   QBUYER,DOIDBYR      SAVE THE BUYER CODE                          
         XC    WORK,WORK                                                        
         MVC   WORK+2(L'DOISTA),DOISTA                                          
         DROP  R6                                                               
         GOTO1 VMSUNPK,DMCB,WORK,WORK+10,QSTA                                   
         CLI   QSTA+4,C' '                                                      
         BNE   *+8                                                              
         MVI   QSTA+4,C'T'                                                      
*                                                                               
         BRAS  RE,GETNOTCE         READ THE MAKEGOOD NOTICE                     
         BNE   PDLNTNO                                                          
*                                                                               
         XC    ELEM,ELEM           CREATE STATUS ELEMENT                        
         LA    R6,ELEM                                                          
         USING MNSTELD,R6                                                       
         MVI   MNSTEL,MNSTELQ                                                   
         MVI   MNSTLEN,MNSTLENQ                                                 
         GOTO1 VDATCON,DMCB,(0,RDNTDATE),(19,MNSTDATE)                          
         GOTO1 VHEXIN,DMCB,RDNTTIME,MNSTTIME,L'RDNTTIME                         
         MVI   MNSTSTAT,MNSTDELV   DELIVERED STATUS                             
         MVI   SVSTAT,MNSTDELV     SAVE IT FOR LATER                            
         MVI   THISISMG,C'Y'                                                    
*                                                                               
         L     RF,AIO1             LATEST FIRST                                 
         LA    R6,MNXFRST-MNXKEY(RF)                                            
         CLI   QSTA,C'0'           CABLE?                                       
         BNL   *+8                                                              
         LA    R6,MNRFRST-MNKEY(RF)                                             
*                             FIRST ELEMENT SHOULD BE STATUS ELEM!!!            
PDLNT255 CLI   MNSTSTAT,MNSTCANM   IS IT A CANMORE STATUS?                      
         BE    PDLNT260                                                         
         CLI   MNSTSTAT,MNSTCAN    OR A CANCELLED STATUS?                       
         BE    PDLNT290                                                         
         CLI   MNSTSTAT,MNSTSAPP   OR SELF-APPLIED?                             
         BNE   PDLNT290                                                         
         DROP  R6                                                               
*                                  YES, THEN ADD DELNOT AFTER                   
PDLNT260 LLC   R1,1(R6)                                                         
         AR    R6,R1               BUMP IT                                      
         B     PDLNT255            AND CHECK AGAIN                              
*                                                                               
PDLNT290 GOTO1 VRECUP,DMCB,(C'S',AIO1),ELEM,(R6)  ADD STATUS                    
         ORG   *-2                                                              
         CLI   QSTA,C'0'           CABLE?                                       
         BL    PDLNT295                                                         
         MVI   0(R1),X'FE'         CAN'T USE C'T', BC RECUP NOT CHANGED         
         LAY   RE,XSPREC            TO USE MAX REC SIZE IN DMFILTAB HAS         
         ST    RE,12(R1)            FOR XSPFIL                                  
PDLNT295 BASR  RE,RF                                                            
         XC    ELEM,ELEM                                                        
*                                                                               
PDLNT300 XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         CLC   QMGGROUP,=C'  '     DELNOT FOR MKGD?                             
         BNH   PDLNT305                                                         
         MVC   KEY(L'MNXKEY),0(R6)                                              
         LAY   RF,XPUT                                                          
         CLI   QSTA,C'0'           CABLE?                                       
         BNL   PDLNT310                                                         
PDLNT305 MVC   KEY(L'MNKEY),0(R6)                                               
         LAY   RF,PUT                                                           
PDLNT310 BASR  RE,RF                                                            
*                                                                               
         BAS   RE,UPDATSB1                                                      
*                                                                               
         CLC   QMGGROUP,=C'  '     DELNOT FOR MKGD?                             
         BH    PDLNTYS5                                                         
*                                                                               
         LA    R1,ELEM+DOXMTSTA-DOXMTEL       R1=A(STATUS FIELD)                
         TM    MISCFLG1,MF1NOXMT                                                
         BZ    PDLNTYES                                                         
         LA    R1,ELEM+DOSTSTAT-DOSTEL                                          
PDLNTYES CLI   0(R1),QRCLDELN      RECALLED, REP DELIVERED?                     
         BNE   PDLNTYS1                                                         
         CLI   REVISION,0          AND REVISION?                                
         BNE   PDLNTYS5            YES, SKIP COLOR AND EXTRA                    
*                                                                               
PDLNTYS1 CLI   0(R1),QNODARE       NOT DARE?                                    
         BE    PDLNTYS5            YES, DON'T CARE ABOUT CLR                    
         TM    BITFLAG2,BF2SPNDG   WAS X'11' SNTPNDING?                         
         BO    PDLNTYS5            YES, DON'T CARE ABOUT CLR                    
         TM    BITFLAG3,BF3SPNDG   WAS X'12' SNTPNDING?                         
         BO    PDLNTYS5            YES, DON'T CARE ABOUT CLR                    
         BRAS  RE,BLDCOLOR                                                      
*                                                                               
PDLNTYS5 BRAS  RE,DAREMAIL                                                      
*                                                                               
         BRAS  RE,CHKFRCHG         BUILD CHECK FOR CHANGE KEY                   
         B     YES                                                              
*                                                                               
PDLNTNO  TM    MISCFLG1,MF1XMTUP   HAVE I MADE CHANGES TO XMT ELEM?             
         BO    PDLNT300            YES                                          
         B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE DELIVERY NOTIFICATION FOR REP                                   
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCDLNR DS    0H                                                               
         MVC   REP,RDNTTOID        GET FROM ID FOR REP IDENTIFICATION           
         BAS   RE,SWTCHCTR         SWITCH CONTROL TO REP  SYSTEM                
         BNE   NO                  NOT SUCCESSFUL                               
*                                                                               
         BRAS  RE,PROCRDLN                                                      
*                                                                               
         B     XIT                                                              
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE ORDER APPROVAL                                                  
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCOAPP DS    0H                                                               
         USING RORDAPPD,R7                                                      
POAPPD   USING RTN2SNDR,ROAPRTRN                                                
         GOTO1 VHEXIN,DMCB,POAPPD.RTNAGYMD,BAGYMD,2                             
*                                                                               
         NI    MISCFLG3,X'FF'-MF3INIOA                                          
         L     RF,4(RD)            GO BACK D CHAIN                              
         L     RE,48(RF)           GET A(EDICT RECORD)                          
POAPP00  AH    RE,EDCTLRCL         BUMP TO THE NEXT RECORD                      
         OC    0(4,RE),0(RE)       EOF?                                         
         BZ    POAPP05             MUST BE A NORMAL APPROVAL                    
         USING EDFILD,RE                                                        
*                                                                               
         LLC   R1,RECNUM                                                        
         LA    R1,1(R1)                                                         
         CH    R1,EDCTRPBQ         ANY MORE RECORDS IN THIS BLOCK?              
         BH    POAPP05             NO, ASSUME NORMAL APPROVAL                   
*                                                                               
         CLI   EDFSYS,EDFDAREQ     DARE RECEIVED RECORD?                        
         BNE   POAPP00                                                          
*                                                                               
         LA    RF,EDFDARE                                                       
         CLC   =CL6'ORDMO1',0(RF)  DO WE HAVE AN MO TRANSMISSION?               
         BE    POAPP01                                                          
         CLC   =CL6'ORDTLR',0(RF)                                               
         BNE   POAPP05                                                          
*                                                                               
POAPP01  OI    MISCFLG3,MF3INIOA                                                
         L     R6,AIO1             SAVE THIS FIRST                              
         MVI   0(R6),RORDAPPL+1    1 BYTE LENGTH + L'ORDAPP                     
         MVC   1(RORDAPPL,R6),0(R7)                                             
         BAS   RE,PUTINIOA                                                      
*                                                                               
         OI    BITFLAG1,BF1DPEND   GOT A PENDING DARE RECORD                    
         B     POAPPYES                                                         
*                                                                               
POAPP05  GOTO1 =A(APPROVAL),DMCB,(RC),RR=RELO                                   
POAPPYES BE    YES                                                              
POAPPNO  B     NO                                                               
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE SALESPERSON REASSIGNMENT                                        
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCSALE DS    0H                                                               
         USING RORDSALD,R7                                                      
         L     R6,AIO1             SAVE THIS FIRST                              
         MVI   0(R6),RORDSALL+1                                                 
         MVC   1(RORDSALL,R6),0(R7)                                             
         BAS   RE,PUTINIOA                                                      
*                                                                               
POSALX   B     YES                                                              
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE URL ON CONFIRMATION                                             
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCURL  DS    0H                                                               
         USING RORDURLD,R7                                                      
         L     R6,AIO1             SAVE THIS FIRST                              
         MVI   0(R6),RORDURLL+1                                                 
         MVC   1(RORDURLL,R6),0(R7)                                             
         BAS   RE,PUTINIOA                                                      
*                                                                               
POURLX   B     YES                                                              
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE MEDIA OCEAN FLAGS                                               
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCMO1  DS    0H                                                               
         USING RORDMO1D,R7                                                      
         L     R6,AIO1             SAVE THIS FIRST                              
         MVI   0(R6),RORDMO1L+1                                                 
         MVC   1(RORDMO1L,R6),0(R7)                                             
         BAS   RE,PUTINIOA                                                      
*                                                                               
POMO1X   B     YES                                                              
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE ORDER REJECTION                                                 
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCOREJ DS    0H                                                               
         USING RORDREJD,R7                                                      
         L     R6,AIO1             SAVE THIS FIRST                              
         MVI   0(R6),RORDREJL+1                                                 
         MVC   1(RORDREJL,R6),0(R7)                                             
         BAS   RE,PUTINIOA                                                      
*                                                                               
         OI    BITFLAG1,BF1DPEND   GOT A PENDING DARE RECORD                    
*                                                                               
POREJX   B     YES                                                              
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE ORDER REJECTION COMMENT                                         
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCORCM DS    0H                                                               
         USING RORDCOMD,R7                                                      
         L     R6,AIO1             SAVE THIS FIRST                              
         MVI   0(R6),RORDCOML+1                                                 
         MVC   1(RORDCOML,R6),0(R7)                                             
         BAS   RE,PUTINIOA                                                      
*                                                                               
PORCMX   B     YES                                                              
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE ORDER TRAILER                                                   
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCOTLR DS    0H                                                               
         USING RORDTLRD,R7                                                      
         L     R6,AIO1             SAVE THIS FIRST                              
         MVI   0(R6),RORDTLRL+1                                                 
         MVC   1(RORDTLRL,R6),0(R7)                                             
         BAS   RE,PUTINIOA                                                      
*                                                                               
         NI    BITFLAG1,X'FF'-BF1DPEND    NO MORE PENDING DARE RECORDS          
*                                                                               
         L     R6,AWRKRIOA                                                      
         OC    0(2,R6),0(R6)       ENCOUNTERED AN ERROR BEFORE?                 
         BZ    POTLR50             YES                                          
*                                                                               
         LA    R6,2(R6)                                                         
         CLC   1(L'RORJTID,R6),=C'ORDREJ'                                       
         BNE   POTLR20                                                          
         GOTO1 =A(REJECT),DMCB,(RC),RR=RELO                                     
         B     POTLR50                                                          
*                                                                               
POTLR20  CLC   1(L'ROCFTID,R6),=C'ORDCFM'                                       
         BNE   POTLR30                                                          
         GOTO1 =A(CONFIRM),DMCB,(RC),RR=RELO                                    
         B     POTLR50                                                          
*                                                                               
POTLR30  CLC   1(L'ROCFTID,R6),=C'ORDAPP'                                       
         BNE   POTLR35                                                          
         GOTO1 =A(APPROVAL),DMCB,(RC),RR=RELO                                   
         B     POTLR50                                                          
*                                                                               
POTLR35  CLC   1(L'ROCFTID,R6),=C'ORDRCL'                                       
         BNE   POTLR40                                                          
         BRAS  RE,RCLAKNWL                                                      
         B     POTLR50                                                          
*                                                                               
POTLR40  LHI   R1,REFTLRWR           FOR WRONG REC TYPE                         
         BRAS  RE,SNDERROR                                                      
*                                                                               
* NEEDS TO CLEAR OTHERWISE GARBAGE LEFTOVER                                     
*                                                                               
         L     RE,AWRKRIOA         RESET THE WORKER IOAREA                      
         LH    RF,=Y(WRECQLNQ)                                                  
         XCEFL                                                                  
         L     RE,AWRKRIOA                                                      
         MVI   1(RE),2                                                          
*                                                                               
         BAS   RE,UPDATSB1                                                      
         B     POTLRNO                                                          
*                                                                               
POTLR50  L     RE,AWRKRIOA         RESET THE WORKER IOAREA                      
         LH    RF,=Y(WRECQLNQ)                                                  
         XCEFL                                                                  
         L     RE,AWRKRIOA                                                      
         MVI   1(RE),2                                                          
*                                                                               
         BAS   RE,UPDATSB1                                                      
*                                                                               
POTLRYES B     YES                                                              
*                                                                               
POTLRNO  B     NO                                                               
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE ORDER CONFIRMATION                                              
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCOCFM DS    0H                                                               
         USING RORDCFMD,R7                                                      
         MVC   QREPCON,ROCFRPCN                                                 
         MVC   QRETURN,ROCFRTRN                                                 
*                                                                               
         L     R6,AIO1             SAVE THIS FIRST                              
         MVI   0(R6),RORDCFML+1                                                 
         MVC   1(RORDCFML,R6),0(R7)                                             
         BAS   RE,PUTINIOA                                                      
*                                                                               
         OI    BITFLAG1,BF1DPEND   GOT A PENDING DARE RECORD                    
*                                                                               
POCFMX   B     YES                                                              
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE ORDER LINE EQUIVALENTS                                          
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCOLIN DS    0H                                                               
         USING RORDLIND,R7                                                      
         LA    RF,L'ROLNBLIN       MAKE SURE AGENCY BUYLINE NUMERIC             
         LA    RE,ROLNBLIN                                                      
POLIN10  CLI   0(RE),C'0'                                                       
         BL    POLIN20                                                          
         CLI   0(RE),C'9'                                                       
         BH    POLIN20                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,POLIN10                                                       
         B     POLIN30                                                          
*                                                                               
POLIN20  L     R1,AWRKRIOA                                                      
         LR    R0,R7                                                            
         LA    R7,3(R1)                                                         
         LHI   R1,REFBDABL    BAD AGENCY BUYLINE                                
         BRAS  RE,SNDERROR                                                      
         LR    R7,R0                                                            
         L     R1,AWRKRIOA         CLEAR THIS SO TRAILER KNOWS WE               
         XC    0(2,R1),0(R1)         HAVE AN ERROR                              
         B     POLINX                                                           
*                                                                               
POLIN30  PACK  DUB,ROLNBLIN        AGENCY BUYLINE IS FROM 1-255                 
         CVB   R1,DUB                                                           
         LTR   R1,R1               CANNOT HAVE 0                                
         BZ    POLIN20                                                          
         CHI   R1,999              CANNOT HAVE MORE THAN 255                    
         BH    POLIN20                                                          
*                                                                               
         L     R6,AIO1             SAVE THIS FIRST                              
****     MVI   0(R6),ROLNRLIN+L'ROLNRLIN-RORDLIND+1                             
         MVC   1(ROLNRLIN+L'ROLNRLIN-RORDLIND,R6),0(R7)                         
         LA    R6,ROLNRLIN-RORDLIND+L'ROLNRLIN-3(R6)                  )         
*                                  R6 IS A(4 LAST CHAR)                         
POLIN35  CLC   0(4,R6),=C'0000'    TO COMPRESS TO AVOID OVERFLOW                
         BH    *+12                                                             
         AHI   R6,-4                                                            
         B     POLIN35             SAFE BECAUSE OF THE AGY BUYLINE              
                                                                                
         LA    R1,4(R6)            BECAUSE OF THE LAST 4 CHARACTERS             
         L     R6,AIO1                                                          
         SR    R1,R6                                                            
         STC   R1,0(R6)                                                         
         BAS   RE,PUTINIOA                                                      
*                                                                               
POLINX   B     YES                                                              
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE ERROR NOTIFICATION FOR AGENCY                                   
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCERNT DS    0H                                                               
         BRAS  RE,ERRNOTCE                                                      
         BNE   NO                                                               
         B     YES                                                              
***********************************************************************         
* PROCESSES THE AGENCY RECALL FOR REP                                           
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCARCL DS    0H                                                               
         BRAS  RE,AGYRCALL                                                      
         BNE   NO                                                               
         B     YES                                                              
***********************************************************************         
* PROCESSES THE ORDER RECALL ACKNOWLEDGEMENT                                    
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCORAK DS    0H                                                               
         USING RORDRCLD,R7                                                      
RECLLD   USING RTN2SNDR,RORCRTRN                                                
         GOTO1 VHEXIN,DMCB,RECLLD.RTNAGYMD,BAGYMD,2                             
*                                                                               
         BRAS  RE,RCLAKNWL                                                      
         BNE   NO                                                               
         B     YES                                                              
         DROP  R7                                                               
***********************************************************************         
* PROCESSES THE MAKEGOOD REP OK (MAKEGOOD CONFIRMATION)                         
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCMGOK DS    0H                                                               
         BRAS  RE,MKGDCNFM                                                      
         BNE   NO                                                               
         B     YES                                                              
***********************************************************************         
* PROCESSES THE MAKEGOOD REP CANCELLATION                                       
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCMGCN DS    0H                                                               
         BRAS  RE,MKGDCNCL                                                      
         BNE   NO                                                               
         B     YES                                                              
***********************************************************************         
* PROCESSES THE DARE FAX DELIVERY NOTIFICATION                                  
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCDFAX DS    0H                                                               
         BRAS  RE,DAREDLFX                                                      
         BNE   NO                                                               
         B     YES                                                              
***********************************************************************         
* PROCESSES THE DARE FAX CANCELLATION                                           
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCCNFX DS    0H                                                               
         BRAS  RE,DARECNFX                                                      
         BNE   NO                                                               
         B     YES                                                              
***********************************************************************         
* PROCESSES THE MAKEGOOD HEADER                                                 
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCMKGX DS    0H                                                               
         BRAS  RE,DAREMKGX                                                      
         BNE   NO                                                               
         B     YES                                                              
***********************************************************************         
* PROCESSES THE REPDARE MESSAGES                                                
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCRDAR DS    0H                                                               
         BRAS  RE,DARERDAR                                                      
         BNE   NO                                                               
         B     YES                                                              
***********************************************************************         
* PROCESSES THE DARE FAX ERROR                                                  
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCERFX DS    0H                                                               
         BRAS  RE,DAREERFX                                                      
         BNE   NO                                                               
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*  READ BLOCK                                                                   
***********************************************************************         
READBLK  NTR1                                                                   
         SR    R1,R1                                                            
         ICM   R1,3,EDCTFDSK       IS TRACK FOR THIS DAY                        
         LA    R1,1(R1)                                                         
         CLM   R1,3,EDCTFLST                                                    
         BH    RDBLKNO                                                          
*                                                                               
         GOTO1 VDADDS,DMCB,RDID,AHUGEBLK,0,EDICTFL,EDCTFDSK,0                   
         OC    8(2,R1),8(R1)                                                    
         JNZ   *+2                                                              
*                                                                               
RDBLKYES B     YES                                                              
*                                                                               
RDBLKNO  B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE UPDATES INFORMATION FOR DARE IN THE SSB                          
*                                                                               
* CALLED BY THE OBJECTS SO RECNUM IS CORRECT NOT ONE OFF                        
***********************************************************************         
UPDATSB1 NTR1                                                                   
         GOTO1 =A(DSKCNVRT),DMCB,0,RR=RELO                                      
*                                                                               
         L     RE,VSSB                                                          
         USING SSBD,RE                                                          
*                                                                               
         MVC   SSBDARDT,BTODAY     GOT SOMETHING FOR TODAY                      
         TM    BITFLAG1,BF1YSDAY                                                
         BZ    *+10                                                             
         MVC   SSBDARDT,PRIORDAT                                                
*                                                                               
         LA    R1,SSBDAREA                                                      
         CLI   WHCHEDCT,C'A'                                                    
         BE    *+8                                                              
         LA    R1,SSBDARER                                                      
         MVC   0(4,R1),EDCTFDSP    SAVE LAST DAY DISP WE'RE UPTO                
         DROP  RE                                                               
*                                                                               
USSB1X   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SWITCHES CONTROL TO THE CORRECT SPOT SYSTEM                                   
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
*              USERID              SET TO ID OF SPOT AGENCY                     
*                                                                               
* ON EXIT:     SPTSENUM            AGENCY'S SPOT SENUM                          
*              DLNFRID             ID OF WHERE DLNNOT CAME FROM (REP)           
***********************************************************************         
SWTCHCTL NTR1                                                                   
         GOTO1 =A(SWTCHSPT),RR=RELO                                             
SWCTLX   B     XIT                                                              
*                                                                               
***********************************************************************         
* SWITCHES CONTROL TO THE REP  SYSTEM                                           
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
*                                                                               
* ON EXIT:     SPTSENUM            REP'S SENUM                                  
***********************************************************************         
SWTCHCTR NTR1                                                                   
         MVI   DMCB,X'0A'          SWITCH TO CONTROL SYSTEM                     
         BAS   RE,SWTCHSYS         SUCCESSFUL SWITCH?                           
         BNE   EXITPRG             NO                                           
*                                                                               
         XC    KEY,KEY             LOOK FOR THE ID RECORD                       
         LA    R4,KEY                                                           
         USING CTIREC,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,REP          INSERT REP CODE                              
         DROP  R4                                                               
*                                                                               
         MVC   AIO,AIO1                                                         
         BRAS  RE,HIGHCT                                                        
*                                                                               
         L     R6,AIO1                                                          
         USING CTIREC,R6                                                        
         CLC   CTIKEY,KEY          ERR IF WE CAN'T FIND THE ACCESS REC          
         BE    SWCTR05                                                          
SWCTRERR CLC   =C'ERRNOT',0(R7)           ERROR FROM ERRNOT?                    
         BE    SWCTRNO                                                          
         LHI   R1,REFRIDNV          NO, REP ID NOT VALID                        
         BRAS  RE,SNDERROR                                                      
         B     SWCTRNO                                                          
*                                                                               
SWCTR05  SR    R0,R0                                                            
         LA    R6,CTIDATA                                                       
         USING CTSYSD,R6                                                        
SWCTR10  CLI   0(R6),0                                                          
         BE    SWCTRERR            ERROR IF NO REP SYS ELEM                     
         CLI   0(R6),X'21'                                                      
         BNE   *+12                                                             
         CLI   CTSYSNUM,8          LOOK FOR REP SYSTEM AUTH                     
         BE    SWCTR20                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     SWCTR10                                                          
*                                                                               
SWCTR20  MVC   SPTSENUM,CTSYSSE                                                 
         DROP  R6                                                               
*                                                                               
         L     R1,VSELIST                                                       
         ZICM  RE,0(R1),2          SET UP BXLE USING (RE,RF)                    
         ICM   RF,15,2(R1)                                                      
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
*                                                                               
SWCTR30  CLC   SESYS,SPTSENUM                                                   
         BNE   SWCTR33                                                          
         TM    SEIND,SEISETRO+SEIRONLY    READ-ONLY STATUS?                     
         BNZ   SWCTRNO                                                          
         B     SWCTR36                                                          
*                                                                               
SWCTR33  BXLE  R1,RE,SWCTR30       CHECK NEXT ENTRY IN SELIST                   
         B     SWCTRNO                                                          
*                                                                               
SWCTR36  MVC   DMCB(1),SPTSENUM    SWITCH TO REP  SYSTEM                        
         BAS   RE,SWTCHSYS         SUCCESSFUL SWITCH?                           
         BE    SWCTR40                                                          
         CLI   DMCB+4,2            SWITCHED, BUT SYSTEM NOT OPENED?             
***BAD   BE    EXITPRG             YES, SHOULD WAIT UNTIL OPENED                
***BAD   B     SWCTRERR            USER NOT AUTHORIZED                          
*                                                                               
         BNE   SWCTRERR            USER NOT AUTHORIZED                          
         MVI   DMCB,X'0A'          SWITCH TO CONTROL SYSTEM                     
         BAS   RE,SWTCHSYS         SUCCESSFUL SWITCH?                           
         BNE   EXITPRG             NO                                           
         B     SWCTRNO             SKIP THE DAMN ENTRY                          
*                                                                               
SWCTR40  L     R6,AIO1                                                          
         USING CTIREC,R6                                                        
         LA    R6,CTIDATA          NOW FIND POWER CODE FOR REP                  
SWCTR45  CLI   0(R6),0                                                          
         BE    SWCTRERR            ERROR IF NO REP POWER CODE ELEMENT           
         CLI   0(R6),X'06'                                                      
         BE    SWCTR50                                                          
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     SWCTR45             GO BACK FOR NEXT ELEMENT                     
*                                                                               
SWCTR50  MVC   POWERCDE,2(R6)      SAVE POWER CODE                              
*                                                                               
SWCTRYES B     YES                                                              
*                                                                               
SWCTRNO  B     NO                                                               
         DROP  R6,R1                                                            
         EJECT                                                                  
***********************************************************************         
* SWITCHES SYSTEM WHETHER THE PROGRAM HAS BEEN AUTHORIZED TO SWITCH             
* SYSTEMS OR NOT                                                                
*                                                                               
* ON ENTRY:    DMCB    BYTE  0     SYSTEM SENUM TO SWITCH TO                    
***********************************************************************         
SWTCHSYS NTR1                                                                   
         MVC   DMCB+1(3),=4X'FF'   DON'T CARE IF PROGRAM IS AUTHORIZED          
         GOTO1 VSWITCH,DMCB,,0                                                  
         CLI   DMCB+4,0            SUCCESSFUL?                                  
         BNE   SWSYSNO             NO                                           
*                                                                               
SWSYSYES B     YES                                                              
*                                                                               
SWSYSNO  B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* GETS THE AGENCY HEADER RECORD FOR UPDATE AND PUTS IT IN AIO1                  
*                                                                               
* ON ENTRY:    PARAM1, HOB         RECORD TYPE X'41' OR X'51'                   
*              PARAM1              STATION CODE                                 
*              PARAM2              AGENCY ROUTING CODE                          
*              PARAM3              DARE ORDER #                                 
*                                                                               
* ON EXIT:     CONDITION CODE      NE - RECORD DOESN'T EXIST                    
*                                  EQ - RECORD IS IN AIO1                       
***********************************************************************         
GETRPORD NTR1                                                                   
         XC    KEY,KEY             SETUP THE AGENCY HEADER KEY                  
         LA    R4,KEY                                                           
         USING RDARKEY,R4                                                       
         MVC   RDARKTYP,0(R1)      INSERT KEY TYPE                              
         MVI   RDARKRT,X'10'       AGENCY HEADER RECORD TYPE                    
         MVC   RDARKREP,POWERCDE   INSERT POWER CODE                            
         L     R2,0(R1)            R2 = A(STATION CODE)                         
         MVC   RDARKSTA,0(R2)                                                   
         MVI   RDARKSTA+5,C' '     BECAUSE SRRDR00 DOES IT                      
         L     R2,4(R1)            R2 = A(AGENCY ROUTING CODE)                  
         MVC   RDARKAGY(L'RDARKAGY+L'RDARKAOF),0(R2)                            
         L     R2,8(R1)            R2 = A(DARE ORDER #)                         
         GOTO1 VHEXIN,DMCB,0(R2),RDARKORD,L'RDNTORDR                            
*                                                                               
         L     R3,AIO1                                                          
DARED    USING RDARREC,R3                                                       
         MVC   DARED.RDARKEY,RDARKEY                                            
*                                                                               
         BRAS  RE,HIGHREP                                                       
*                                                                               
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE   DID WE FIND IT?                  
         BE    GRPOR20                         YES                              
*                                                                               
* EXACT KEY NOT FOUND                                                           
* NOW LOOK FOR THE ORDER # OF ALL OF THE OFFICES UNDER THIS AGENCY              
* (AGENCY OFFICE HAS CHANGED)                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKAOF-RDARKEY),DARED.RDARKEY                              
*                                                                               
GRPOR10  EQU    *                  LOOP THRU ALL OFFICES FOR THIS ORD#          
         BRAS  RE,HIGHREP                                                       
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(RDARKAOF-RDARKEY),KEY                                    
         BNE   GRPORNO             NO  - SET CC TO NO                           
*                                                                               
         MVC   RDARKORD,DARED.RDARKORD                                          
*                                                                               
         BRAS  RE,HIGHREP                                                       
*                                                                               
         CLC   KEYSAVE(RDARKRT-RDARKEY),KEY                                     
         BE    GRPOR20             SAME THRU ORDER NUMBER?                      
*                                                                               
         XC    KEY,KEY             SKIP TO NEXT OFFICE                          
         MVC   KEY(RDARKORD-RDARKEY),KEYSAVE                                    
         XR    R1,R1                                                            
         ICM   R1,3,RDARKAOF                                                    
         AHI   R1,1                                                             
         STCM  R1,3,RDARKAOF                                                    
         B     GRPOR10                                                          
*********                                                                       
* READ AGENCY HEADER RECORD FOR UDPATE INTO AIO1                                
*********                                                                       
GRPOR20  EQU   *                                                                
         MVC   AIO,AIO1                                                         
         MVI   DMINBTS,X'80'                                                    
         BRAS  RE,GETREP                                                        
*                                                                               
GRPORYES B     YES                                                              
*                                                                               
GRPORNO  B     NO                                                               
         DROP  DARED,R4                                                         
         EJECT                                                                  
***********************************************************************         
* TEST TO SEE IF AGENCY GOT SPANKED FROM ONE ADV TO ANOTHER                     
***********************************************************************         
SPANKED  NTR1                                                                   
         LA    RE,SPANKTBL                                                      
         NI    MISCFLG3,X'FF'-MF3SPNKD                                          
*                                                                               
SPNK10   CLI   0(RE),X'FF'                                                      
         BE    SPNKEXIT                                                         
*                                                                               
         CLC   AGENCY,0(RE)        MATCHES AN AGENCY THAT GOT SPANKED?          
         BNE   SPNK20                                                           
         CLC   THESYSID,2(RE)      ARE WE ON THE OLD ADV?                       
         BE    SPNKNO                                                           
         CLC   THESYSID,3(RE)      ARE WE ON THE NEW ADV?                       
         BE    SPNKYES                                                          
         B     SPNKNO                                                           
*                                                                               
SPNK20   LA    RE,5(RE)            LOOP THROUGH ALL SPANKED AGENCIES            
         B     SPNK10                                                           
*                                                                               
SPNKYES  MVC   BYTE,3(RE)                                                       
         NI    BAGYMD,X'0F'        CLEAR AGY NIBBLE                             
         OC    BAGYMD,4(RE)        USE NEW SPANKED AGY NIBBLE                   
         OI    MISCFLG3,MF3SPNKD   AGENCY WAS SPANKED TO THIS FACPAK            
         B     SPNKEXIT                                                         
*                                                                               
SPNKNO   MVI   BYTE,0                                                           
*                                                                               
SPNKEXIT B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* COMMON EXIT POINTS                                                            
***********************************************************************         
YES      SR    RC,RC               SET CC TO EQ                                 
NO       LTR   RC,RC               SET CC TO NEQ                                
XIT      XIT1                      RETURN TO CALLER                             
*                                                                               
EXITPRG  L     RD,SAVERD           EXIT THE PROGRAM                             
         B     XIT                                                              
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
BNEXTEL  CLI   0(R6),0                                                          
         JE    BNEXTELX                                                         
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         JZ    *+2                                                              
         AR    R6,R0                                                            
BNEXTEL2 CLI   0(R6),0                                                          
         JE    BNEXTELX                                                         
         CLC   ELCDLO,0(R6)                                                     
         JH    BNEXTEL                                                          
         CLC   ELCDHI,0(R6)                                                     
         JL    BNEXTEL                                                          
         CR    RB,RB                                                            
         J     *+6                                                              
BNEXTELX LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
ENQZMSG  DC    CL40'+ENQDEQ+ MEDZ LONG UPDATE     (FACPAK)'                     
ENQXMSG  DC    CL40'+ENQDEQ+ MEDZ UPDATE ENDED    (FACPAK)'                     
ALLSPCES DC    132C' '                                                          
         SPACE 1                                                                
DMRDHI   DC    C'DMRDHI '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMADD    DC    C'DMADD  '                                                       
DMWRT    DC    C'DMWRT  '                                                       
DMUNLK   DC    C'DMUNLK '                                                       
GETREC   DC    C'GETREC '                                                       
PUTREC   DC    C'PUTREC '                                                       
ADDREC   DC    C'ADDREC'                                                        
*                                                                               
*                                                                               
GFILE    DC    CL8'GFILE'                                                       
PRTQUE   DC    CL8'PRTQUE'                                                      
*                                                                               
SPTFILE  DC    C'SPTFILE'                                                       
REPFILE  DC    C'REPFILE'                                                       
SPTDIR   DC    C'SPTDIR'                                                        
REPDIR   DC    C'REPDIR'                                                        
GENDRCT  DC    C'GENDIR'                                                        
CTFILE   DC    C'CTFILE'                                                        
GENFILE  DC    C'GENFIL'                                                        
STATION  DC    C'STATION'                                                       
XSPDIR   DC    CL8'XSPDIR'                                                      
XSPFIL   DC    CL8'XSPFIL'                                                      
         EJECT                                                                  
***********************************************************************         
*        DS    CL2                 AGENCY ACCESS                                
*        DS    X                   OLD ADV #                                    
*        DS    X                   NEW ADV #                                    
*        DS    X                   NEW   AGY   BUT WHICH MEDIA?                 
***********************************************************************         
SPANKTBL DS    0C                                                               
         DC    C'CE',X'02',X'0C',X'10'                                          
*****    DC    C'SF',X'02',X'0C',X'40'   ITG'S CORPA USES THIS                  
         DC    C'DT',X'02',X'0C',X'80'                                          
         DC    C'TH',X'02',X'0C',X'90'                                          
         DC    C'BS',X'02',X'0C',X'50'                                          
         DC    C'DF',X'02',X'0C',X'20'                                          
         DC    C'DW',X'02',X'0C',X'30'                                          
         DC    C'BT',X'02',X'0C',X'60'                                          
         DC    C'NF',X'02',X'0C',X'B0'                                          
         DC    C'TB',X'02',X'0C',X'70'                                          
         DC    C'RH',X'02',X'0C',X'C0'                                          
         DC    C'XD',X'02',X'0C',X'D0'                                          
         DC    C'OM',X'0A',X'05',X'40'                                          
         DC    X'FF'               END-OF-TABLE                                 
         EJECT                                                                  
         LTORG                                                                  
***********************************************************************         
* CONVERTS THE 8 BYTE NUMERIC ORDER NUMBER TO A 4 BYTE BINARY CODE              
*                                                                               
* ON ENTRY:    PARAM 1             A(ORDER NUMBER IN EBCDIC FORM)               
*                                                                               
* ON EXIT:     BINORDER            ORDER NUMBER (FF COMPLEMENT)                 
***********************************************************************         
CALCORDR NTR1  BASE=*,LABEL=*                                                   
         L     R2,DMCB                                                          
         ZAP   FULL,JDTTODAY       CENTURY/YEAR FROM TODAY'S DATE               
         SRP   FULL,61,0                                                        
*                                                                               
         LR    RE,R2               MAKE SURE ORDER NUMBER IS VALID              
         LA    RF,L'RDNTORDR                                                    
CORDR10  CLI   0(RE),C'0'                                                       
         BL    CORDRNO                                                          
         CLI   0(RE),C'9'                                                       
         BH    CORDRNO                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,CORDR10                                                       
*                                                                               
         CLI   1(R2),C'3'          2ND DIGIT OF ORDER > 3?                      
         BNH   CORDR20             NO, ORIGINAL ORDER NUMBER ALGORITHM          
         PACK  DUB,0(8,R2)         NEW STYLE ORDER NUMBER                       
         SP    DUB,=P'04000000'                                                 
         CVB   R1,DUB                                                           
         STCM  R1,15,BINORDER                                                   
         OI    BINORDER,X'80'                                                   
         XC    BINORDER,=4X'FF'                                                 
         B     CORDRYES                                                         
*                                                                               
CORDR20  GOTO1 VHEXIN,DMCB,(R2),BINORDER,L'RDNTORDR                             
         MVC   PACKOF4B,BINORDER                                                
         OI    PACKOF4B+3,X'0F'    CONVERT IT TO PACK                           
         SRP   PACKOF4B,58,0       ISOLATE THE YEAR                             
         MVC   PACKOF4B+2(1),FULL+2 COPY TODAY'S CENTURY                        
*                                                                               
         CP    PACKOF4B+3(1),FULL+3(1) LESS 10 YEARS?                           
         BNH   *+10                                                             
         SP    PACKOF4B,=P'10'     YEAR DIGIT>TODAY'S YEAR DIGIT                
*                                                                               
         SP    PACKOF4B,=P'90'     CALCULATE FROM 1990                          
         SRP   PACKOF4B,4,0                                                     
         OC    PACKOF4B+1(2),BINORDER  STICK IN DAYS IN YEAR                    
         SRP   PACKOF4B,63,0                                                    
*                                                                               
         ZAP   DUB,PACKOF4B        SAVE DATE PORTION                            
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDDT                                                    
         XC    BINORDDT,=4X'FF'                                                 
*                                                                               
         PACK  DUB,4(4,R2)         SAVE SEQUENCE PORTION                        
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDSQ                                                    
         XC    BINORDSQ,=4X'FF'                                                 
*                                                                               
CORDRYES B     YES                                                              
*                                                                               
CORDRNO  CLC   =C'ERRNOT',0(R7)                                                 
         BE    NO                                                               
         LHI   R1,REFBDORD     BAD ORDER NUMBER                                 
         BRAS  RE,SNDERROR                                                      
         B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* PUTS AN ENTRY INTO THE DARE MAIL STORAGE AREA IN THE SSB                      
***********************************************************************         
DAREMAIL NTR1  BASE=*,LABEL=*                                                   
         L     RE,VSSB                                                          
         L     RE,SSBDARTB-SSBD(RE)                                             
*                                                                               
         LA    R1,DAREXAGY                                                      
DMAIL00  CLI   0(R1),0             ANY AGENCIES LEFT IN TABLE?                  
         BE    DMAIL10             NONE                                         
         CLC   AGENCY,0(R1)                                                     
         BE    DMAILX              AGENCY DOESN'T WANT DARE MAIL                
         LA    R1,L'DAREXAGY(R1)                                                
         B     DMAIL00                                                          
*                                                                               
DMAIL10  CLC   0(4,RE),=4X'FF'                                                  
         BE    DMAILX              NO ROOM LEFT                                 
         CLC   SIGNON2H,0(RE)      USER ID MATCH?                               
         BE    DMAIL20             YES                                          
         OC    0(4,RE),0(RE)       EMPTY SLOT?                                  
         BZ    DMAIL20             YES                                          
         LA    RE,4(RE)                                                         
         B     DMAIL10                                                          
*                                                                               
DMAIL20  MVC   0(2,RE),SIGNON2H                                                 
         THMS  DDSTIME=YES                                                      
         ST    R0,PACKOF4B         ADDJUST FOR DDS TIME                         
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
         MVO   FULL,PACKOF4B       PWOS HHMM??                                  
         MVC   2(2,RE),FULL                                                     
***      BAS   RE,SETDAR           SETS THE UTL'S AND STUFF                     
*                                                                               
DMAILX   B     XIT                                                              
*                                                                               
DAREXAGY DS    0CL2                AGENCIES THAT DON'T WANT DAREMAIL            
         DC    C'WI'                                                            
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* THIS GETS THE CLIENT SO WE CAN LOOK UP THE PRODUCT CODES                      
***********************************************************************         
GETCLTRC NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CLTHDRD,R4                                                       
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,BCLT                                                     
         DROP  R4                                                               
*                                                                               
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(L'CKEY),KEYSAVE                                              
         BNE   GCRECNO                                                          
* READ CLIENT RECORD                                                            
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
*                                                                               
GCRECYES B     YES                                                              
*                                                                               
GCRECNO  B     NO                                                               
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* THIS GETS THE EBCDIC PRODUCT CODE FOR THE BINARY PRODUCT CODE                 
*                                                                               
* ON ENTRY:    PARAM 1  BYTE 0     BINARY PRODUCT CODE                          
*                       BYTES 1-3  A(EBCDIC PRODUCT CODE)                       
***********************************************************************         
GETQPRD  NTR1  BASE=*,LABEL=*                                                   
         CLI   0(R1),0             NO BINARY CODE?                              
         BNE   GQPRD00                                                          
*                                                                               
         L     RF,0(R1)                                                         
         LA    RF,0(RF)                                                         
         XC    0(3,RF),0(RF)       THEN CLEAR OUT THIS FIELD                    
         B     GQPRDX                                                           
*                                                                               
GQPRD00  L     R6,AIO1             ELSE SEARCH THROUGH CLIENT PRD LIST          
         USING CLTHDRD,R6                                                       
         LA    RE,CLIST                                                         
GQPRD10  OC    0(3,RE),0(RE)                                                    
         BNZ   GQPRD15                                                          
         LHI   R1,*-T16100         NO SUCH PRODUCT!!                            
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
GQPRD15  CLC   3(1,RE),0(R1)                                                    
         BE    *+12                                                             
         LA    RE,4(RE)                                                         
         B     GQPRD10                                                          
*                                                                               
         L     RF,0(R1)                                                         
         LA    RF,0(RF)                                                         
         MVC   0(3,RF),0(RE)                                                    
*                                                                               
GQPRDX   B     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* OPENS A NEW WORKER FILE FOR CREATE                                            
***********************************************************************         
WRKRCREA NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO2                                                          
         XC    0(256,R4),0(R4)     CLEAR SFH                                    
         USING WLHDRD,R4                                                        
         MVC   WLSOFLAB,=C'*SOFSOF*'                                            
         MVC   WLUSRID,SIGNON2H                                                 
         MVC   WLSYSPRG,=C'DAR'                                                 
         MVC   WLSUBPRG,SYSN1       *** FACPAK ID                               
         MVI   WLDAY,0                                                          
*                                                                               
         MVI   WLCLASS,C'T'        CLASS 'T'                                    
         MVI   WLTYPE,C'A'         TYPE A FOR IMMEDIATE EXECUTION               
         MVI   WLATTB,WLATOBJ      SET OBJECT CODED DATA FLAG                   
         MVC   WLDESC,=CL16' '     FILL IN DESC '$MAD LUIDLUID   '              
         MVC   WLDESC(4),=C'$DAR'                                               
         L     RF,SRPARMSD.SRQAUTL                                              
         USING UTLD,RF                                                          
         MVC   WLDESC+5(8),TSYM                                                 
         DROP  RF                                                               
*                                                                               
         GOTOR WRKRSEND,DMCB,AIO2                                               
*                                                                               
         MVC   WRKFILNO,WLREPRNO   EXTRACT FILE NUMBER                          
*                                                                               
WCREAX   B     YES                                                              
         DROP  R4                                                               
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* SEND DIRECTLY TO WORKER FILE                                                  
*                                                                               
* ON ENTRY:    PARAM 1             A(RECORD TO BE SENT)                         
***********************************************************************         
WRKRSEND NTR1  BASE=*,LABEL=*                                                   
         L     R4,0(R1)                                                         
*                                                                               
         LA    R3,ELEM             FILL INDEX KEY WITH USER ID                  
         USING UKRECD,R3                                                        
         XC    ELEM,ELEM                                                        
         MVC   UKUSRID,SIGNON2H    OF THE AGENCY                                
         DROP  R3                                                               
*                                  CALL DATAMGR TO CREATE FILE                  
         GOTO1 VDATAMGR,DMCB,=C'DMPRINT',=C'WRKFILE',(R3),(R4),AWRKRBUF         
         CLI   8(R1),0                                                          
         BE    YES                                                              
         MVC   BYTE,8(R1)          SAVE THE FLAG                                
         LHI   R1,*-T16100         WRKR FILE FULL???? MOST LIKELY               
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* CLOSE THE WORKER FILE                                                         
***********************************************************************         
WRKRCLOS NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO2                                                          
         USING WLHDRD,R4                                                        
         XC    0(WLSOFEND-WLHDRD,R4),0(R4)                                      
         MVC   WLSOFLAB,=C'*EOFEOF*'                                            
*                                                                               
         GOTOR WRKRSEND,DMCB,AIO2                                               
*                                                                               
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
*&&DO                                                                           
* CHKMOREP SHOULD NOT BE NECESSARY AS THE ID OF SENDER AND RECIEVER ARE         
* ALWAYS SAVED BY THE SELLING SYSTEM. WHENEVER MESSAGES ARE SENT TO THE         
* BUYER, SAVED IDS ARE REVERSED PRESERVING THE -MO, -WO, OR -DS.                
***********************************************************************         
* CHECK MEDIA OCEAN REP TABLE                                                   
*                                                                               
* ON ENTRY: PARAM1   A(ORIGIN ID IN EDICT HEADER)                               
*           PARAM2   A(ORDER # IN EDICT HEADER)                                 
*                                                                               
* ON EXIT : CC       EQ = UPDATED ORIGIN ID!!                                   
*                                                                               
***********************************************************************         
CHKMOREP NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            A(ORIGIN ID)                                 
         L     R3,4(R1)            A(ORDER #)                                   
         BAS   RE,CLCORDDT         CALC ORDER DATE BASED ON ORDER #             
*                                                                               
         LA    R1,MOREPTAB                                                      
MEDOA10  CLI   0(R1),X'FF'         EOT?                                         
         BE    MEDOA30             YES                                          
         LLC   R4,1(R1)            LENGTH OF REP ENTRY                          
         BCTR  R4,0                MINUS 1 FOR EX!                              
         EX    R4,*+8              MATCH ON REP?                                
         BE    MEDOA15             YES, CHECK OFFICE                            
         CLC   2(0,R1),0(R2)                                                    
         LLC   R4,0(R1)            NO, BUMP TO NEXT REP                         
         AR    R1,R4                                                            
         B     MEDOA10                                                          
*                                                                               
***  POINT TO OFFICE FOR COMPARE                                                
MEDOA15  LLC   R4,1(R1)                                                         
         LA    R6,0(R2)                                                         
         LA    R6,0(R4,R6)         R6=A(OFFICE CODE IN EDICT)                   
         LA    R1,2(R4,R1)         R1=A(OFFICE CODE IN TABLE)                   
*                                                                               
MEDOA20  CLI   0(R1),0             END OF REP/OFFICE LIST?                      
         BE    MEDOANO             DO NOTHING                                   
         CLC   0(2,R1),0(R6)       MATCH ON OFFICE?                             
         BE    MEDOA25             YES                                          
         LA    R1,5(R1)            NO, BUMP TO NEXT OFFICE IN TABLE             
         B     MEDOA20                                                          
*                                                                               
MEDOA25  CLC   ORDRDATE(3),2(R1)   ON/AFTER THE CUTOFF DATE?                    
         BNL   MEDOANO             YES                                          
         MVC   2(3,R6),=C'-DS'     MOVE IT IN                                   
         B     MEDOAYES                                                         
***********************************                                             
* NBC CUTOVER TO WIDEORBIT                                                      
* REP-DATE  OR  LOCAL STATION-DATE                                              
***********************************                                             
MEDOA30  LA    R1,WORPDTAB                                                      
MEDOA33  CLI   0(R1),X'FF'         EOT?                                         
         BE    MEDOA40             YES                                          
         LLC   R4,3(R1)            L(REP/LOCAL STATION) ENTRY                   
         BCTR  R4,0                MINUS 1 FOR EX!                              
         EX    R4,*+8              MATCH ON REP/LOCAL STATION?                  
         BE    MEDOA36             YES, CHECK DATE                              
         CLC   4(0,R1),0(R2)                                                    
         LLC   R4,3(R1)            NO, BUMP TO NEXT REP                         
         LA    R1,4(R1,R4)                                                      
         B     MEDOA33                                                          
*                                                                               
***  POINT TO DATE FOR COMPARE                                                  
MEDOA36  LLC   R4,3(R1)                                                         
         LA    R6,0(R2)                                                         
*                                                                               
         CLC   =C'NBC',0(R2)       IS IT A REP?                                 
         BE    MEDOA37                                                          
         CLC   =C'MON',0(R2)                                                    
         BNE   *+12                                                             
MEDOA37  LA    R6,2(R4,R6)         R6=A(AFTER OFFICE CODE IN EDICT)             
         B     MEDOA39                                                          
*                                                                               
         BCTR  R4,0                TO ACCOUNT FOR SPACE IN ENTRY                
         LA    R6,0(R4,R6)         R6=A(AFTER LOCAL STATION IN EDICT)           
*                                                                               
MEDOA39  CLC   ORDRDATE(3),0(R1)   ON/AFTER THE CUTOFF DATE?                    
         BL    MEDOANO             NO, NO CHANGE TO THE RECEIVING ID            
         MVC   0(3,R6),=C'-WO'     GOES TO WIDEORBIT                            
         B     MEDOAYES                                                         
***********************************                                             
* REP-OFFICE-DATE                                                               
*                                                                               
* WE HAVE OTHER REP-OFFICE THAT ARE GOING TO MEDIAOCEAN BY DATE                 
* **NOTE**                                                                      
* IF WE MATCH ON REP-OFFICE BUT NOT DATE, ORDER STAYS ON REPPAK                 
***********************************                                             
MEDOA40  LA    R1,MORODTAB                                                      
MEDOA43  CLI   0(R1),X'FF'         EOT?                                         
         BE    MEDOA50             YES                                          
         LLC   R4,3(R1)            L(REP-OFFICE) ENTRY                          
         BCTR  R4,0                MINUS 1 FOR EX!                              
         EX    R4,*+8              MATCH ON REP-OFFICE?                         
         BE    MEDOA46             YES, CHECK DATE                              
         CLC   4(0,R1),0(R2)                                                    
         LLC   R4,3(R1)            NO, BUMP TO NEXT REP                         
         LA    R1,4(R1,R4)                                                      
         B     MEDOA43                                                          
*                                                                               
***  POINT TO DATE FOR COMPARE                                                  
MEDOA46  LLC   R4,3(R1)                                                         
         LA    R6,0(R2)                                                         
         LA    R6,0(R4,R6)         R6=A(AFTER OFFICE CODE IN EDICT)             
*                                                                               
         CLC   ORDRDATE(3),0(R1)   ON/AFTER THE CUTOFF DATE?                    
         BNL   MEDOANO             NO, NO CHANGE TO THE RECEIVING ID            
         MVC   0(3,R6),=C'-MO'     GOES TO MEDIAOCEAN                           
         B     MEDOAYES                                                         
***********************************                                             
* REP-OFFICE-STATION-DATE                                                       
*                                                                               
* WE HAVE OTHER REP-OFFICE THAT ARE GOING TO MEDIAOCEAN BY STATION/DATE         
* **NOTE**                                                                      
* IF WE MATCH ON REP-OFFICE BUT NOT STATION/DATE, ORDER STAYS ON REPPAK         
***********************************                                             
MEDOA50  LA    R1,MOOFSTTB                                                      
MEDOA55  CLI   0(R1),X'FF'         EOT?                                         
         BE    MEDOA80             YES                                          
         LLC   R4,2(R1)            L(REP-OFFICE) ENTRY                          
         BCTR  R4,0                MINUS 1 FOR EX!                              
         EX    R4,*+8              MATCH ON REP-OFFICE?                         
         BE    MEDOA60             YES, CHECK STATION AND DATE                  
         CLC   3(0,R1),0(R2)                                                    
         XR    R4,R4               NO, BUMP TO NEXT REP                         
         ICM   R4,3,0(R1)                                                       
         AR    R1,R4                                                            
         B     MEDOA55                                                          
*                                                                               
***  POINT TO STATION/DATE FOR COMPARE                                          
MEDOA60  LLC   R4,2(R1)                                                         
         LA    R6,0(R2)                                                         
         LA    R6,0(R4,R6)         R6=A(AFTER OFFICE CODE IN EDICT)             
         LA    R1,3(R4,R1)         R1=A(OFFICE CODE IN TABLE)                   
*                                                                               
MEDOA65  CLI   0(R1),0             END OF STATION/DATE LIST?                    
         BE    MEDOANO             THEN STAYS ON REPPAK                         
*                                                                               
         CLC   QSTA,0(R1)          MATCHES THIS STATION?                        
         BE    MEDOA70                                                          
         LA    R1,14(R1)           NO, NEXT STA/DATE FOR REP-OFFICE             
         B     MEDOA65                                                          
*                                                                               
MEDOA70  OC    5(3,1),5(R1)        WAS THE STATION ALREADY CONVERTED?           
         BZ    MEDOA75             YES, AUTOMATICALLY GO TO MEDIAOCEAN          
         CLC   ORDRDATE(3),5(R1)   ON/AFTER THE CUTOFF DATE?                    
         BL    MEDOA80             NO, NO CHANGE TO THE RECEIVING ID            
*                                                                               
         OC    8(3,R1),8(R1)       ANY END DATE?                                
         BZ    MEDOA75             NONE, GOES TO MEDIAOCEAN                     
         CLC   ORDRDATE(3),8(R1)   ON/AFTER THE END DATE (REPPAK)?              
         BL    MEDOA75             NO, STAYS ON MEDIAOCEAN                      
*                                                                               
         CLC   ORDRDATE(3),11(R1)  ON/AFTER NEW START DATE (-MO AGAIN)?         
         BL    MEDOA8O             NO, NO CHANGE TO THE RECEIVING ID            
MEDOA75  MVC   0(3,R6),=C'-MO'     GOES TO MEDIAOCEAN                           
***********************************                                             
* WIDEORBIT  REP-OFFICE-STATION-DATE                                            
*                                                                               
* WE HAVE OTHER REP-OFFICE THAT ARE GOING TO WIDEORBIT BY STATION/DATE          
* **NOTE**                                                                      
* IF WE MATCH ON REP-OFFICE BUT NOT STATION/DATE, ORDER STAYS ON REPPAK         
***********************************                                             
MEDOA80  LA    R1,MOOFSTTB                                                      
         AHI   R1,WOOFSTTB-MOOFSTTB                                             
MEDOA85  CLI   0(R1),X'FF'         EOT?                                         
         BE    MEDOA110            YES                                          
         LLC   R4,2(R1)            L(REP-OFFICE) ENTRY                          
         BCTR  R4,0                MINUS 1 FOR EX!                              
         EX    R4,*+8              MATCH ON REP-OFFICE?                         
         BE    MEDOA90             YES, CHECK STATION AND DATE                  
         CLC   3(0,R1),0(R2)                                                    
         XR    R4,R4               NO, BUMP TO NEXT REP                         
         ICM   R4,3,0(R1)                                                       
         AR    R1,R4                                                            
         B     MEDOA85                                                          
*                                                                               
***  POINT TO STATION/DATE FOR COMPARE                                          
MEDOA90  LLC   R4,2(R1)                                                         
         LA    R6,0(R2)                                                         
         LA    R6,0(R4,R6)         R6=A(AFTER OFFICE CODE IN EDICT)             
         LA    R1,3(R4,R1)         R1=A(OFFICE CODE IN TABLE)                   
*                                                                               
MEDOA95  CLI   0(R1),0             END OF STATION/DATE LIST?                    
         BE    MEDOA110            THEN STAYS ON REPPAK                         
*                                                                               
         CLC   QSTA,0(R1)          MATCHES THIS STATION?                        
         BE    MEDOA100                                                         
         LA    R1,14(R1)           NO, NEXT STA/DATE FOR REP-OFFICE             
         B     MEDOA95                                                          
*                                                                               
MEDOA100 OC    5(3,1),5(R1)        WAS THE STATION ALREADY CONVERTED?           
         BZ    MEDOA105            YES, AUTOMATICALLY GO TO WIDEORBIT           
         CLC   ORDRDATE(3),5(R1)   ON/AFTER THE CUTOFF DATE?                    
         BL    MEDOA110            NO, NO CHANGE TO THE RECEIVING ID            
*                                                                               
         OC    8(3,R1),8(R1)       ANY END DATE?                                
         BZ    MEDOA105            NONE, GOES TO WIDEORBIT                      
         CLC   ORDRDATE(3),8(R1)   ON/AFTER THE END DATE (REPPAK)?              
         BL    MEDOA105            NO, STAYS ON WIDEORBIT                       
*                                                                               
         CLC   ORDRDATE(3),11(R1)  ON/AFTER NEW START DATE (-WO AGAIN)?         
         BL    MEDOA110            NO, NO CHANGE TO THE RECEIVING ID            
MEDOA105 MVC   0(3,R6),=C'-WO'     GOES TO WIDEORBIT                            
*                                                                               
MEDOA110 DS    0H                                                               
*                                                                               
MEDOAYES B     YES                                                              
*                                                                               
MEDOANO  B     NO                                                               
*************************************                                           
* SET ORDRDATE TO DATE (PWOS JULIAN) BASED OFF ORDER #                          
*                                                                               
* ON ENTRY     (R3)                A(EBCDIC ORDER #)                            
*                                                                               
* ON EXIT      ORDRDATE(3)         IS PWOS JULIAN                               
*************************************                                           
CLCORDDT NTR1                                                                   
         MVC   ORDRDATE,JDTTODAY                                                
         NC    ORDRDATE,=X'FFF0000F'  TO GET CURRENT DECADE                     
         GOTO1 VHEXIN,DMCB,0(R3),FULL,6                                         
         OI    FULL+2,X'0F'                                                     
         SRP   FULL(3),63,0                                                     
         OC    ORDRDATE+1(3),FULL     MAKE ORDER # FOR CURRENT DECADE           
*                                                                               
         CLC   ORDRDATE(2),JDTTODAY   IS ORDER'S YEAR > TODAY'S YEAR?           
         BNH   *+10                   NO, THIS DECADE IS GOOD                   
         SP    ORDRDATE,=P'0010000'   YES, ORDER FROM LAST DECADE               
*                                                                               
         SRP   ORDRDATE,1,0           ORDRDATE(3) IS PWOS JUL                   
         B     XIT                                                              
         LTORG                                                                  
       ++INCLUDE DDMOREPTAB                                                     
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* ADD XPOT KEY FOR CHECK FOR CHANGE SUPPORT                                     
* ON ENTRY : AIO1      DARE ORDER RECORD                                        
******       AGYMD     AGENCY/MEDIA                                             
******       BUYER     BUYER CODE                                               
******       BINORDER  BINARY ORDER#                                            
*            QMGGROUP  GROUP CODE                                               
*            SVORDDA   ORDER RECORD D/A                                         
*            SVMKNDA   NOTICE RECORD D/A                                        
*                                                                               
*  ON EXIT : XSPOT KEYS ADDED                                                   
***********************************************************************         
CHKFRCHG NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
CFCD     USING MNKEY,KEY                                                        
         MVI   CFCD.MNDKTYPE,MNDKTYPQ       X'0D'                               
         MVI   CFCD.MNDKSTYP,MNDKSTYQ       X'BC'                               
*                                                                               
         MVC   CFCD.MNDKAGMD,BAGYMD         AGENCY MEDIA                        
         MVC   CFCD.MNDKORDR,BINORDER       ORDER NUMBER                        
         MVC   CFCD.MNDKBYR,QBUYER          BUYER    (FROM GETORDER)            
         MVC   CFCD.MNDKCLT,BCLT            CLIENT                              
         MVC   CFCD.MNDKPRD,BPRD            PRODUCT                             
         MVC   CFCD.MNDKEST,BEST            ESTIMATE                            
         MVC   CFCD.MNDKMKT,BMKTSTA         MARKET   (FROM GETORDER)            
*                                                                               
         GOTOR VDATCON,DMCB,(5,0),(2,CFCD.MNDKDATE)                             
         THMS  DDSTIME=YES                                                      
         STCM  R0,15,PACKOF4B                                                   
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
*                                                                               
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         BL    CFC010                                                           
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTOR VDATCON,DMCB,(5,0),(0,DUB)   THE TIME                            
         GOTOR VADDAY,DMCB,DUB,DUB,F'1'                                         
         GOTOR VDATCON,DMCB,(0,DUB),(2,CFCD.MNDKDATE)                           
*                                                                               
CFC010   TIME  TU                                                               
         STCM  R0,15,CFCD.MNDKTIME                                              
         XC    CFCD.MNDKTIME,=4X'FF'                                            
*                                                                               
         MVC   KEY+36(4),SVORDDA   D/A OF ORDER RECORD (FROM GETORDER)          
         BRAS  RE,ADDXKEY                                                       
*                                                                               
         CLC   QMGGROUP,=C'   '    NO MAKEGOOD, SO WE'RE DONE                   
         JNH   XIT                                                              
         MVC   CFCD.MNDKGPCD,QMGGROUP       GROUP CODE                          
         MVC   KEY+36(4),SVMKNDA   D/A OF MKGD NOTICE REC (GETNOTCE)            
         BRAS  RE,ADDXKEY                                                       
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE UPDATES INFORMATION FOR DARE IN THE SSB                          
*                                                                               
* CALLED BY PROCESS WHEN IT ENCOUNTERS A EDICT RECORD NOT IN TODAY'S            
* MONTH                                                                         
***********************************************************************         
UPDATSSB NTR1  BASE=*,LABEL=*                                                   
         LLC   RF,RECNUM                                                        
         BCTR  RF,0                                                             
         STC   RF,RECNUM                                                        
         GOTO1 =A(DSKCNVRT),DMCB,0,RR=RELO                                      
         LLC   RF,RECNUM                                                        
         LA    RF,1(RF)                                                         
         STC   RF,RECNUM                                                        
*                                                                               
         L     RE,VSSB                                                          
         USING SSBD,RE                                                          
*                                                                               
         MVC   SSBDARDT,BTODAY     GOT SOMETHING FOR TODAY                      
         TM    BITFLAG1,BF1YSDAY                                                
         JZ    *+10                                                             
         MVC   SSBDARDT,PRIORDAT                                                
*                                                                               
         LA    R1,SSBDAREA                                                      
         CLI   WHCHEDCT,C'A'                                                    
         JE    *+8                                                              
         LA    R1,SSBDARER                                                      
         MVC   0(4,R1),EDCTFDSP    SAVE LAST DAY DISP READ                      
         DROP  RE                                                               
*                                                                               
USSBX    J     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* PROCESSES THE DELIVERY NOTIFICATION FOR REP                                   
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCRDLN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING RDLNNOTD,R7                                                      
         CLC   =CL8' ',RDNTRPCN                                                 
         BNL   PDLNRNO                                                          
         CLC   RDNTRPCN,=8C'0'                                                  
         BE    PDLNRNO                                                          
         CLC   =CL6'UNDARE',RDNTRPCN                                            
         BE    PDLNRNO                                                          
*                                        READ REP CONTRACT INTO AIO1            
         GOTO1 =A(GETRPCON),DMCB,RDNTRPCN,RR=RELO                               
         BNE   PDLNRNO                                                          
*&&DO                                                                           
* THIS IS COMMENTED OUT SINCE RCONDRDD IS ACTUALLY THE ORDER RECEIVED           
* DATE (SKUI 11/26/01)                                                          
         SR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,RCONELEM-RCONREC(R6)                                          
PDLNR10  CLI   0(R6),0                                                          
         BE    PDLNR30             NOT LINKED TO CONTRACT, SKIP                 
*                                                                               
PDLNR15  CLI   0(R6),X'1D'                                                      
         BE    PDLNR20                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PDLNR10                                                          
*                                                                               
         USING RCONDREL,R6                                                      
*                                                                               
PDLNR20  EQU   *                                                                
         GOTO1 VDATCON,DMCB,(0,RDNTDATE),(2,RCONDRDD)                           
*                                  INSERT DATE DELIVERED                        
         GOTO1 VHEXIN,DMCB,RDNTTIME,RCONDRTD,L'RDNTTIME                         
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         L     R6,AIO1             RESET A(IO AREA)                             
         MVC   KEY(L'RCONKEY),0(R6)                                             
*                                  LOAD UP THE KEY AGAIN                        
         MVC   AIO,AIO1                                                         
         BRAS  RE,PUTREP                                                        
*                                  REWRITE THE RECORD                           
*&&                                                                             
*                                                                               
* CHECK IF THIS IS A DELIVERY NOTIFICATION FOR A MAKEGOOD OFFER                 
*                                                                               
         USING MDLNNOTD,R7                                                      
         OC    MDNTOFRI,MDNTOFRI                                                
         BZ    PDLNR30                                                          
         CLC   MDNTOFRI,ALLSPCES                                                
         BNE   PDLNR200                                                         
         DROP  R7                                                               
*                                                                               
* UDPATE REP DARE ORDER WITH DELIVERY NOTIFICATION                              
*                                                                               
* READ REP AGENCY RECORD TO GET AGENCY ROUTING CODE SO WE CAN FIND              
* THE AGENCY ORDER IN THE REP'S FILE                                            
*                                                                               
PDLNR30  DS    0H                                                               
         GOTO1 =A(GETREPDR),RR=RELO                                             
         BNZ   PDLNR500                                                         
*                                                                               
* RECORD AUDIT TRAIL FOR REP DARE ORDER                                         
*                                                                               
         GOTO1 =A(AUDTRAIL),DMCB,('DHDELNOT',0),RR=RELO                         
*                                                                               
         B     PDLNR500                                                         
*                                                                               
* UPDATE MAKEGOOD OFFER WITH DELIVERY NOTIFICATION                              
*                                                                               
PDLNR200 DS    0H                                                               
         USING MDLNNOTD,R7                                                      
*                                                                               
         L     R6,AIO1                                                          
         USING RCONREC,R6                                                       
         LA    R4,KEY                                                           
         USING RMKGKEY,R4                                                       
         XC    KEY,KEY                                                          
         MVI   RMKGKTYP,X'11'                                                   
         MVC   RMKGKREP,RCONKREP                                                
         MVC   RMKGKOFF,RCONKOFF                                                
         MVC   RMKGKSTA,RCONKSTA                                                
*                                                                               
         ZAP   WORK(5),=P'0'       CHANGE FROM PWOS TO PWS                      
         MVO   WORK(5),RCONKCON                                                 
         ZAP   WORK+10(5),=P'99999999' GET 9'S COMPLEMENT                       
         SP    WORK+10(5),WORK(5)                                               
         MVO   WORK(5),WORK+10(5)  CHANGE TO PWOS                               
*                                                                               
         PACK  RMKGKCON(1),WORK+3(1) REVERSE THE COMPLIMENT                     
         PACK  RMKGKCON+1(1),WORK+2(1)                                          
         PACK  RMKGKCON+2(1),WORK+1(1)                                          
         PACK  RMKGKCON+3(1),WORK(1)                                            
*                                                                               
         MVC   RMKGKGRP,MDNTOFRI                                                
         DROP  R4,R6                                                            
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,DMRDHI,=C'REPDIR',KEY,KEY                          
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
*                                                                               
         CLC   KEY(L'RMKGKEY),KEYSAVE                                           
         BNE   PDLNR500                                                         
*                                                                               
         MVI   DMINBTS,X'88'       READ FOR DELETED IN CASE RECORD              
         MVC   AIO,AIO2            GOT DELETED BY USER SINCE LAST RDHI          
         BRAS  RE,GETREP                                                        
*                                                                               
         XC    ELEM,ELEM                                                        
MGATD    USING RMKGATEM,ELEM                                                    
         MVI   MGATD.RMKGATCD,X'02'                                             
         MVI   MGATD.RMKGATLN,RMKGATLQ                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(2,MGATD.RMKGATDT)                            
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,PACKOF4B                                                      
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
*                                                                               
         ICM   R1,15,PACKOF4B                                                   
         SRL   R1,4                                                             
         STCM  R1,B'0111',MGATD.RMKGATTM                                        
*                                                                               
         PACK  DUB,MDNTSEQN                                                     
         CVB   R1,DUB                                                           
         STC   R1,MGATD.RMKGATVR                                                
*                                                                               
* DEFENSIVE CODING. HARRIS IS SENDING US 100+ DUPLICATE DLNNOTS THAT IS         
* CAUSING RECORD OVERFLOW. THIS WILL CHECK TO SEE IF WE'VE ALREADY              
* RECEIVED A DLNNOT FOR A PARTICULAR VERSION AND NOT UPDATE IF WE DID           
*                                                                               
         L     R6,AIO2                                                          
         MVC   DATADISP,=H'34'     SET UP FOR REPFILE                           
         MVI   ELCODE,X'02'        GET DARE ELEMENT                             
         BRAS  RE,GETEL                                                         
         BNE   PDLNR300                                                         
PDLNR280 LR    R4,R6                                                            
         BRAS  RE,NEXTEL                                                        
         BE    PDLNR280                                                         
         USING RMKGATEM,R4         CHECK VERSION# AND DLNNOT FLAG               
         CLC   RMKGATVR(2),MGATD.RMKGATVR                                       
         BE    PDLNR500                                                         
         DROP  R4,MGATD                                                         
*                                                                               
PDLNR300 DS    0H                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),AIO2,ELEM,0                       
         CLI   DMCB+12,0                                                        
         BNE   PDLNR500                                                         
*                                                                               
         GOTO1 VDATAMGR,DMCB,PUTREC,=C'REPFILE',KEY,AIO2,DMWORK                 
*                                  REWRITE THE RECORD                           
PDLNR500 DS    0H                                                               
         BAS   RE,UPDATSB1                                                      
*                                                                               
PDLNRYES B     YES                                                              
*                                                                               
PDLNRNO  B     NO                                                               
         DROP  R7                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PUTS OUT A MESSAGE TO THE CONSOLE AND HARDCOPY                                
*                                                                               
* ON ENTRY:    WHCHEDCT            ON WHAT EDICT FILE PROBLEM OCCURED           
*              EDCTFDSK            ADDRESS OF THE EDICT RECORD                  
*              PARAM  1            OFFSET FROM BEGINNING OF PROGRAM             
***********************************************************************         
WTOMSG   NTR1  BASE=*,LABEL=*                                                   
         XC    MSG2,MSG2                                                        
         MVC   MSG2(4),DMCB        COPY OFFSET FROM BEG OF PROGRAM              
         MVC   MSG2+4(4),EDCTFDSK                                               
         MVC   MSG2+7(1),RECNUM                                                 
         MVC   MSG1SYS,WHCHEDCT                                                 
         GOTO1 VHEXOUT,DMCB,MSG2+4,MSG1ADDR,4                                   
         GOTO1 VHEXOUT,DMCB,MSG2,MSG1DISP,4                                     
*                                                                               
         XC    MSG2,MSG2                                                        
         MVC   MSG2,0(R7)                                                       
         XR    R0,R0                                                            
         WTO   TEXT=((MSGSTARL,D),(MSGPROBL,D),(MSGSTARL,D),(MSG1L,D), X        
               (MSG2L,D),(0,E)),DESC=2                                          
***********************                                                         
***********************                                                         
         LLC   R1,RECNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,RECNUM                                                        
*                                                                               
         CH    R1,EDCTRPBQ         ANY MORE RECORDS IN THIS BLOCK?              
         BNH   WTO30               YES, NO PROBLEM                              
*                                                                               
         LLC   R2,EDCTFDSK+2       NO                                           
         LA    R2,1(R2)                                                         
         MVI   RECSKIP,0                                                        
         OI    BITFLAG1,BF1SKPTB   SKIP TRACK/BLOCK READ                        
*                                                                               
         CH    R2,EDCTFRPT         ANY MORE BLOCKS ON THIS TRACK?               
         BNH   WTO20               YES                                          
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,3,EDCTFDSK                                                    
         LA    R2,1(R2)                                                         
         CLM   R2,3,EDCTFLST       ANY MORE TRACKS?                             
         BH    WTO30               NO MORE, UPDATE AS WELL                      
         STCM  R2,3,EDCTFDSK       TRACK NUMBER                                 
*                                                                               
WTO10    LA    R2,1                                                             
WTO20    STC   R2,EDCTFDSK+2       BLOCK NUMBER                                 
         MVI   EDCTFDSK+3,0                                                     
*                                                                               
WTO30    BRAS  RE,UPDATSSB                                                      
         DC    H'0'                SO WE GET AN IMAGE                           
         LTORG                                                                  
MSGSTARL DC    H'80'                                                            
         DC    80C'*'                                                           
MSGPROBL DC    H'80'                                                            
         DC    CL49'**$DAR PROBLEM**  PLEASE CONTACT  WHOA AT EXT5324'          
         DC    CL31' IF YOU SEE THIS MESSAGE!!!!!!'                             
MSG1L    DC    H'80'                                                            
MSG1     DC    CL80' '                                                          
         ORG   MSG1                                                             
         DC    CL04'EDCT'                                                       
MSG1SYS  DC    CL01'?'                                                          
         DC    CL07', ADDR='                                                    
MSG1ADDR DC    CL08'????????'                                                   
         DC    CL07', DISP='                                                    
MSG1DISP DC    CL08'????????'                                                   
         ORG   MSG1+L'MSG1                                                      
MSG2L    DC    H'80'                                                            
MSG2     DC    CL80' '                                                          
***********************************************************************         
* SWITCHES CONTROL TO THE CORRECT SPOT SYSTEM                                   
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
*              USERID              SET TO ID OF SPOT AGENCY                     
*                                                                               
* ON EXIT:     SPTSENUM            AGENCY'S SPOT SENUM                          
*              DLNFRID             ID OF WHERE DLNNOT CAME FROM (REP)           
*              ROUTNGCD            ROUTING CODE                                 
***********************************************************************         
SWTCHSPT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   DMCB,X'0A'          SWITCH TO CONTROL SYSTEM                     
         BAS   RE,SWTCHSYS         SUCCESSFUL SWITCH?                           
         BNE   EXITPRG             EXIT PROGRAM, SHOULD WAIT FOR IT             
*                                                                               
         XC    KEY,KEY             LOOK FOR THE USER ID RECORD                  
         LA    R4,KEY                                                           
         USING CTIREC,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,USERID                                                    
         DROP  R4                                                               
*                                                                               
         MVC   AIO,AIO1                                                         
         BRAS  RE,HIGHCT                                                        
*                                                                               
         L     R6,AIO1                                                          
         USING CTIREC,R6                                                        
*                                                                               
         CLC   CTIKEY,KEY          ERR IF WE CAN'T FIND THE ID REC              
         BE    SWSPT00                                                          
SWSPTERR CLC   =C'ERRNOT',0(R7)        ERROR ON ERRNOT?                         
         BE    SWSPTNO                                                          
         LHI   R1,REFAIDNV             AGY ID NOT VALID                         
         BRAS  RE,SNDERROR                                                      
         B     SWSPTNO                                                          
*                                                                               
SWSPT00  SR    R0,R0                                                            
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,2            DESCRIPTION ELEM FOR ID NUM                  
         BAS   RE,FIRSTEL                                                       
         BNE   SWSPTERR                                                         
         MVC   SIGNON2H,2(R6)      SAVE ID NUM OF AGENCY ID                     
*                                                                               
         NI    BITFLAG1,X'FF'-BF1PSSWD   PASSWORD NOT REQUIRED YET              
         L     R6,AIO1                                                          
         USING CTIREC,R6                                                        
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,7                                                         
         BAS   RE,FIRSTEL                                                       
         BNE   SWSPT05             NOT REQUIRED IF NO X'07' ELEM                
         TM    2(R6),X'80'         PASSWORD REQUIRED?                           
         BZ    *+8                                                              
         OI    BITFLAG1,BF1PSSWD                                                
*                                                                               
SWSPT05  L     R6,AIO1                                                          
         USING CTIREC,R6                                                        
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'21'                                                     
         BAS   RE,FIRSTEL                                                       
         BNE   SWSPTERR            ERROR IF WE CAN'T FIND SPOT SYS ELEM         
         USING CTSYSD,R6                                                        
SWSPT10  CLI   CTSYSNUM,2                                                       
         BE    SWSPT15                                                          
         BAS   RE,NEXTEL                                                        
         BE    SWSPT10                                                          
         B     SWSPTERR                                                         
*                                                                               
SWSPT15  MVC   SPTSENUM,CTSYSSE                                                 
         DROP  R6                                                               
***************                                                                 
* GET THE ROUTING CODE                                                          
***************                                                                 
         XC    ROUTNGCD,ROUTNGCD                                                
         L     R6,AIO1                                                          
         USING CTIREC,R6                                                        
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'33'        US AGENCY EXTRA INFO ELEMENT                 
*                                                                               
         BAS   RE,FIRSTEL                                                       
         BE    SWSPT20                                                          
         CLC   =C'DLNFAX',0(R7)    NOT REQUIRED FOR DLNFAX                      
         BE    SWSPT30                                                          
         B     SWSPTERR            ERROR IF WE CAN'T FIND ELEM                  
*                                                                               
         USING CTUSAD,R6                                                        
SWSPT20  MVC   ROUTNGCD,CTUSADRC                                                
         DROP  R6                                                               
***************                                                                 
* SPECIAL CODE FOR MAKEGOOD REP OK                                              
***************                                                                 
SWSPT30  CLC   =C'MKGROK',0(R7)    PROCESSING FOR A MAKEGOOD REP OK?            
         BNE   SWSPT50                                                          
*                                                                               
         TM    BITFLAG1,BF1PSSWD   YES, PASSWORD IS REQUIRED                    
         BNZ   SWSPT60                  YES, WE KNOW ALREADY                    
         L     R6,AIO1                  NO, FIND OUT IF IT IS ELSEWHERE         
         USING CTIREC,R6                                                        
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,7            SEE SRCON00 LABEL VALSYS4 (IDOPTS)           
         BAS   RE,FIRSTEL                                                       
         BNE   SWSPT60                                                          
         TM    2(R6),X'80'         USER ID REQUIRES PASSWORD?                   
         BZ    *+12                                                             
         OI    BITFLAG1,BF1PSSWD   YES, PASSWORD IS REQUIRED                    
         B     SWSPT60                                                          
         DROP  R6                                                               
***************                                                                 
* SPECIAL CODE FOR DELIVERY NOTIFICATIONS                                       
***************                                                                 
SWSPT50  CLC   =C'DLNNOT',0(R7)    PROCESSING FOR A DELIVERY NOTICE?            
         BE    *+14                                                             
         CLC   =C'DLNFAX',0(R7)                     OR DELIVERY FAX?            
         BNE   SWSPT60                                                          
         USING RDLNNOTD,R7                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIREC,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,RDNTFRID                                                  
         DROP  R4,R7                                                            
*                                                                               
         MVC   AIO,AIO1                                                         
         BRAS  RE,HIGHCT                                                        
*                                                                               
         L     R6,AIO1                                                          
         USING CTIREC,R6                                                        
         CLC   CTIKEY,KEY                                                       
         BE    SWSPT50A                                                         
         CLC   =C'DLNFAX',0(R7)    ON DELIVERY FAX?                             
         BNE   SWSPTNO                                                          
         MVC   DLNFRID,=X'FFFF'    LEAVE DESTID AS FAX                          
         B     SWSPT60                                                          
*                                                                               
SWSPT50A SR    R0,R0                                                            
         LA    R6,CTIDATA                                                       
SWSPT51  CLI   0(R6),0                                                          
         BNE   SWSPT51A                                                         
         LHI   R1,REFRIDNV           SENDER ID NOT VALID                        
         BRAS  RE,SNDERROR                                                      
         B     SWSPTNO                                                          
*                                                                               
SWSPT51A CLI   0(R6),2             DESCRIPTION ELEM FOR ID NUM                  
         BE    SWSPT52                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     SWSPT51                                                          
*                                                                               
SWSPT52  MVC   DLNFRID,2(R6)       SAVE ID NUM OF DESTINATION ID                
         B     SWSPT60                                                          
         DROP  R6                                                               
*                                                                               
SWSPT60  MVC   DMCB(1),SPTSENUM    SWITCH TO SPOT SYSTEM                        
         BAS   RE,SWTCHSYS         SUCCESSFUL SWITCH?                           
         BE    SWSPT70                                                          
         CLI   DMCB+4,2            SWITCHED, BUT SYSTEM NOT OPENED?             
         BNE   SWSPTERR            NO, USER NOT AUTHORIZED                      
*                                                                               
         CLI   THESYSID,1          ARE WE ON THE TST SYSTEM?                    
         BE    EXITPRG                                                          
         CLI   THESYSID,15             OR ON THE FQA SYSTEM?                    
         BE    EXITPRG             THEN WE SHOULD WAIT UNTIL OPENED             
********                                                                        
* LET'S TEST AGAINST LIST OF SENUM TO FIND OUT WHAT FACPAK. WE MIGHT            
* HAVE AN ERRONEOUS ENTRY IN THE SPANKTBL IF THE FACPAK DOES MATCH              
********                                                                        
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+8+3(1),SPTSENUM                                             
         GOTO1 VDMOD000,DMCB,VFINDSYS,(1,0)  SERVICE SYSTEM                     
*                                                                               
         L     R1,4(R1)            LOOK AT SYSSTAB                              
         AHI   R1,-4                                                            
         L     R1,0(R1)                                                         
         LLC   RE,SPTSENUM         FACPAK FOR THIS SENUM IS STORED IN           
         AR    RE,R1                                                            
         MVC   HALF(1),0(RE)         DISPL OF SENUM INTO SYSSTAB                
*                                                                               
         CLC   THESYSID,HALF       FACPAKS MATCH?                               
         BE    EXITPRG             YES, SHOULD WAIT UNTIL OPENED                
SWSPTWTO LHI   R1,*-T16100                                                      
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
ON31     O     RE,=XL4'80000000'                                                
         BSM   0,RE                                                             
*                                                                               
OFF31    N     RE,=XL4'7FFFFFFF'                                                
         BSM   0,RE                                                             
*                                                                               
SWSPT70  XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
         DROP  R4                                                               
*                                                                               
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(L'AGYKEY),KEYSAVE                                            
         BNE   SWSPTYES                                                         
*                                                                               
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
*                                                                               
         L     R6,AIO1                                                          
         USING AGYKEY,R6                                                        
         MVC   SVAPRF07,AGYPROF+07   SAVE AGY PROFILE BYTE #8 (OFF 7)           
         DROP  R6                                                               
*                                                                               
SWSPTYES B     YES                                                              
*                                                                               
SWSPTNO  B     NO                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROVIDE MSPACK/MSUNPK ENTRY POINTS FOR LINKAGE TO STAPACK                     
***********************************************************************         
GOMSPACK NTR1  BASE=*,WORK=(R4,8)                                               
         LR    R5,R1               SAVE CALLERS R1                              
*                                                                               
         USING STAPACKD,R4                                                      
         XC    0(32,R4),0(R4)                                                   
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,SVAPRF07                                                
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,SRPARMSD.SRQACOMF                                       
         L     RE,0(R5)            GET A(MKT)                                   
         MVC   STAPQMKT,0(RE)                                                   
         L     RE,4(R5)            GET A(STA)                                   
         MVC   STAPQSTA(8),0(RE)                                                
         MVC   STAPQNET,=C'   '                                                 
GOMSP05  GOTO1 ASTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         BE    GOMSP10                                                          
         LHI   R1,*-T16100         GOT A STAPACK ERROR                          
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
GOMSP10  L     RE,8(R5)            GET A(MKTSTA)                                
         MVC   0(5,RE),STAPMKST                                                 
         XIT1                                                                   
         LTORG                                                                  
         DROP  R4                                                               
***********************************************************************         
* PROVIDE MSUNPK ENTRY POINT FOR LINKAGE TO STAPACK                             
***********************************************************************         
GOMSUNPK NTR1  BASE=*,WORK=(R4,8)                                               
         LR    R5,R1               SAVE CALLERS R1                              
*                                                                               
         USING STAPACKD,R4                                                      
         XC    0(32,R4),0(R4)                                                   
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,SVAPRF07                                                
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,SRPARMSD.SRQACOMF                                       
         L     RE,0(R5)            GET A(MKTSTA)                                
         MVC   STAPMKST,0(RE)                                                   
*                                                                               
         GOTO1 ASTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
*****    B     *+6    <--- MHER 1/17/95  IGNORE ERRORS                          
*****    DC    H'0'                                                             
*                                                                               
         L     RE,4(R5)            GET A(MKT)                                   
         MVC   0(4,RE),STAPQMKT                                                 
         L     RE,8(R5)            GET A(STA)                                   
         MVC   0(5,RE),STAPQSTA    ALWAYS MOVE 5 STATION BYTES                  
         TM    0(R5),X'80'         DOES USER WANT 8 BYTES                       
         BZ    *+10                                                             
         MVC   0(8,RE),STAPQSTA                                                 
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SENDS A DXX NOTIFICATION TO UNDO SELF APPLY                                   
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD)                              
*                                                                               
***********************************************************************         
SENDDXX  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,ASPLAREA                                                      
         LA    RF,L'SPULAREA                                                    
         XCEFL                                                                  
*                                                                               
         L     R8,ASPLAREA                                                      
         USING SPOOLD,R8                                                        
         MVC   SPOOLID,=C'DAR'                                                  
         MVI   USERLANG,0          ENGLISH                                      
         MVC   SPOOLDM,VDATAMGR                                                 
         MVC   RCDATCON,VDATCON                                                 
         MVC   RCCOMFAC,SRPARMSD.SRQACOMF                                       
         MVC   SPOOLBUF,SRPARMSD.SRQATIA                                        
*                                                                               
         LA    R2,SPOOLKEY                                                      
         USING PQPLD,R2                                                         
         MVI   PLCLASS,C'Z'        ERROR REPORT STAYS HERE !                    
         MVC   PLSUBID,=C'DXX'                                                  
         MVC   PLUSER,=X'0011'     SET TO SJR                                   
         MVC   PLDESC(11),=CL11'*MKGD ERR*'                                     
*                                                                               
         XC    P1,P1               OPEN PRINTQ ENTRY                            
         BAS   RE,SNDDPRNT                                                      
         B     SNDD10                                                           
*                                                                               
SNDDPRNT LR    R0,RE                                                            
         GOTO1 ASPOOL,DMCB,(R8)                                                 
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
SNDD10   MVC   SPOOLRPN,PLREPNO                                                 
         MVI   PLCC,0                                                           
*                                                                               
         MVC   P1+26(27),=C'** MAKEGOOD ERROR REPORT **'                        
         BAS   RE,SNDDPRNT                                                      
         MVI   P1+26,C'-'                                                       
         MVC   P1+27(26),P1+26                                                  
         BAS   RE,SNDDPRNT                                                      
*                                                                               
         XC    P,P                                                              
         BAS   RE,SNDDPRNT         SKIP A LINE                                  
         MVC   P(8),RDNTTOID-RDLNNOTD(R7)  AGENCY ID                            
         MVC   P+9(40),=C'SELF APPLIED MAKEGOOD HAS BEEN CANCELLED'             
         BAS   RE,SNDDPRNT                                                      
*                                                                               
         MVC   P+4(5),=C'MEDIA'                                                 
         MVC   P+14(L'QMED),QMED                                                
         MVC   P+46(5),=C'BUYER'                                                
         MVC   P+54(L'QBUYER),QBUYER                                            
         BAS   RE,SNDDPRNT                                                      
*                                                                               
         MVC   P+4(6),=C'CLIENT'                                                
         MVC   P+14(L'QCLT),QCLT                                                
         MVC   P+46(7),=C'PRODUCT'                                              
         MVC   P+54(L'QPRD1),QPRD1                                              
         BAS   RE,SNDDPRNT                                                      
*                                                                               
         MVC   P+4(8),=C'ESTIMATE'                                              
         MVC   P+14(L'QEST1),QEST1                                              
         BAS   RE,SNDDPRNT                                                      
*                                                                               
         MVC   P+4(7),=C'STATION'                                               
         MVC   P+14(L'QSTA),QSTA                                                
         BAS   RE,SNDDPRNT                                                      
*                                                                               
         MVC   P+4(4),=C'MGA='                                                  
         MVC   P+8(L'QMNUMCD),QMNUMCD                                           
         MVC   P+46(5),=C'ORDER'                                                
         MVC   P+52(8),6(R7)                                                    
         BAS   RE,SNDDPRNT                                                      
*                                                                               
         MVC   P(26),=CL26'*** END OF DDS MESSAGE ***'                          
         BAS   RE,SNDDPRNT                                                      
*                                                                               
         MVI   SPMODE,X'FF'        CLOSE THE PRINT QUEUE                        
         BAS   RE,SNDDPRNT                                                      
         B     XIT                                                              
         DROP  R2,R8                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SENDS AN ERROR NOTIFICATION OUT                                               
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD)                              
*              R1 HAS ERROR NUMBER (X'80'=DON'T REVERSE SNDR/RCVR)              
***********************************************************************         
SNDERROR NTR1  BASE=*,LABEL=*                                                   
         ST    R1,FULL             SAVE ERROR NUMBER                            
*                                                                               
         L     RE,ASPLAREA                                                      
         LA    RF,L'SPULAREA                                                    
         XCEFL                                                                  
*                                                                               
         L     R8,ASPLAREA                                                      
         USING SPOOLD,R8                                                        
         MVC   SPOOLID,=C'DAR'                                                  
         MVI   USERLANG,0          ENGLISH                                      
         MVC   SPOOLDM,VDATAMGR                                                 
         MVC   RCDATCON,VDATCON                                                 
         MVC   RCCOMFAC,SRPARMSD.SRQACOMF                                       
         MVC   SPOOLBUF,SRPARMSD.SRQATIA                                        
*                                                                               
         LA    R2,SPOOLKEY                                                      
         USING PQPLD,R2                                                         
         XC    SPOOLKEY,SPOOLKEY                                                
         MVC   PLSUBID,=C'DAR'                                                  
         MVC   PLUSER,=X'0011'    **** SJR FOR NOW                              
         MVC   PLDESC,=CL11'ERRNOT'                                             
         MVI   PLCLASS,C'G'                                                     
         OI    SPOOLIND,SPUINIT    ALLOWS ME TO SET THE CLASS                   
*                                                                               
         XC    P1,P1               OPEN PRINTQ ENTRY                            
         BAS   RE,SNDEPRNT                                                      
         B     SNDE10                                                           
*                                                                               
SNDEPRNT LR    R0,RE                                                            
         GOTO1 ASPOOL,DMCB,(R8)                                                 
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
SNDE10   MVC   SPOOLRPN,PLREPNO                                                 
         MVI   PLCC,0                                                           
         DROP  R2                                                               
***************                                                                 
* PUT OUT EDICT HEADER RECORD                                                   
***************                                                                 
         MVC   P1+4(5),=C'*HDR*'    HEADER RECORD                               
         MVC   P1+9(14),=CL14'EDICT=*DDSDARA'                                   
         MVI   P1+34,C'W'           WIDE REPORT                                 
         MVI   P1+35,C'P'           /PAGE FOR EASYLINK AND SUPRESS TOP          
*                                                                               
         MVC   P2(14),=CL14'++DDS DAERRTRN'                                     
         BAS   RE,SNDEPRNT                                                      
*                                                                               
         LA    R2,P                                                             
         USING RDLNNOTD,R2                                                      
         EDIT  (B3,FULL+1),(3,RDNTEFLG),FILL=0                                  
         MVC   RDNTTID,=C'ERRNOT'  ERROR NOTIFICATION                           
         MVC   RDNTORDR,6(R7)      ORDER # IS ALWAYS 6 BYTES FROM BEG.          
         MVC   RDNTFRID,RDNTTOID-RDLNNOTD(R7)   SWAP TO/FROM                    
         MVC   RDNTTOID,RDNTFRID-RDLNNOTD(R7)                                   
         TM    FULL,X'80'                       DON'T SWAP SNDR/RCVR?           
         BZ    *+16                                                             
         MVC   RDNTFRID,RDNTFRID-RDLNNOTD(R7)   DON'T SWAP                      
         MVC   RDNTTOID,RDNTTOID-RDLNNOTD(R7)                                   
         GOTO1 VDATCON,DMCB,(5,0),(X'20',RDNTDATE)                              
*                                                                               
**** SPECIAL CODE FOR MEDIA OCEAN CUTOFF DATE *** HWON 11/12/02                 
*&&DO                                                                           
         GOTOR CHKMOREP,DMCB,RDNTTOID,RDNTORDR                                  
*&&                                                                             
**** SPECIAL CODE FOR MEDIA OCEAN CUTOFF DATE *** HWON 11/12/02                 
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,PACKOF4B                                                      
         ST    R1,DUB                                                           
         AP    PACKOF4B,DUB(4)                                                  
*                                                                               
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         BL    SNDE20                                                           
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 VADDAY,DMCB,RDNTDATE,(X'20',RDNTDATE),F'1'                       
*                                                                               
SNDE20   ICM   R1,15,PACKOF4B                                                   
         SRL   R1,12               GET RID OF SECONDS AND SIGN                  
         STH   R1,HALF                                                          
         GOTO1 VHEXOUT,DMCB,HALF,RDNTTIME,L'HALF                                
*                                                                               
         MVC   RDNTRPCN,QREPCON                                                 
         MVC   RDNTRTRN,QRETURN                                                 
*                                                                               
         CLC   =C'MKG',0(R7)       ERROR FOR MAKEGOOD?                          
         BE    SNDE22                                                           
         CLC   =C'VARHDR',0(R7)    ERROR FOR VARIOUS ORDER?                     
         BE    SNDE20A                                                          
         CLC   =C'AGY',0(R7)       ERROR FOR AGY MESSAGE?                       
         BNE   SNDE21                                                           
         CLC   =C'AGYHDR',0(R7)                                                 
         BNE   SNDE20B                                                          
***************                                                                 
* RECEIVED DATE/TIME FOR  AGYHDR  TYPES (INCLUDING VARHDR)                      
***************                                                                 
SNDE20A  MVC   RDNTTDTE,PAHDDATE-PAGYHDRD(R7)                                   
         MVC   RDNTTTIM,PAHDTIME-PAGYHDRD(R7)                                   
         B     SNDE30                                                           
***************                                                                 
* RECEIVED DATE/TIME FOR  AGYRCL  TYPES (INCLUDING AGYCAN)                      
***************                                                                 
SNDE20B  MVC   RDNTTDTE,PARCDATE-PAGYRCLD(R7)                                   
         MVC   RDNTTTIM,PARCTIME-PAGYRCLD(R7)                                   
         B     SNDE30                                                           
***************                                                                 
* RECEIVED DATE/TIME FOR  NON-AGY & NON-MKG  TYPES (DEFAULT TYPE)               
***************                                                                 
SNDE21   MVC   RDNTTDTE,RDNTDATE-RDLNNOTD(R7)   RECEIVED DATE/TIME              
         MVC   RDNTTTIM,RDNTTIME-RDLNNOTD(R7)                                   
         B     SNDE30                                                           
*                                                                               
         USING MDLNNOTD,R2                                                      
SNDE22   CLC   =C'MKGHDR',0(R7)    ERROR FOR MAKEGOOD OFFER                     
         BNE   SNDE24                                                           
         MVC   MDNTTDTE,MOHDDATE-MOFRHDRD(R7)                                   
         MVC   MDNTTTIM,MOHDTIME-MOFRHDRD(R7)                                   
         MVC   MDNTOFRI,MOHDOFRI-MOFRHDRD(R7)                                   
         MVC   MDNTSEQN,MOHDSEQN-MOFRHDRD(R7)                                   
         B     SNDE30                                                           
*                                                                               
SNDE24   MVC   MDNTTDTE,MOAPDATE-MOFRAPPD(R7)  ALL OTHER MAKEGOOD FORMS         
         MVC   MDNTTTIM,MOAPTIME-MOFRAPPD(R7)                                   
         MVC   MDNTOFRI,MOAPOFRI-MOFRAPPD(R7)                                   
         MVC   MDNTSEQN,MOAPSEQN-MOFRAPPD(R7)                                   
*                                                                               
SNDE30   BAS   RE,SNDEPRNT                                                      
*                                                                               
         MVC   P(26),=CL26'*** END OF DDS MESSAGE ***'                          
         BAS   RE,SNDEPRNT                                                      
*                                                                               
         MVI   SPMODE,X'FF'        YES, CLOSE PRINTQ ENTRY                      
         GOTO1 ASPOOL,DMCB,(R8)                                                 
*                                                                               
SNDEX    B     XIT                                                              
         DROP  R2,R8                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SENDS AN AGYCAN OUT FOR REVISIONS  (ONLY CALLED BY ORDRCL)                    
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD FOR ORDRCL)                   
***********************************************************************         
SNDAGYCN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,ASPLAREA                                                      
         LA    RF,L'SPULAREA                                                    
         XCEFL                                                                  
*                                                                               
         L     R8,ASPLAREA                                                      
         USING SPOOLD,R8                                                        
         MVC   SPOOLID,=C'DAR'                                                  
         MVI   USERLANG,0          ENGLISH                                      
         MVC   SPOOLDM,VDATAMGR                                                 
         MVC   RCDATCON,VDATCON                                                 
         MVC   RCCOMFAC,SRPARMSD.SRQACOMF                                       
         MVC   SPOOLBUF,SRPARMSD.SRQATIA                                        
*                                                                               
         LA    R2,SPOOLKEY                                                      
         USING PQPLD,R2                                                         
         XC    SPOOLKEY,SPOOLKEY                                                
         MVC   PLSUBID,=C'DAR'                                                  
         MVC   PLUSER,=X'0011'    **** SJR FOR NOW                              
         MVC   PLDESC,=CL11'REVCAN'                                             
         MVI   PLCLASS,C'G'                                                     
         OI    SPOOLIND,SPUINIT    ALLOWS ME TO SET THE CLASS                   
*                                                                               
         XC    P1,P1               OPEN PRINTQ ENTRY                            
         BAS   RE,SACNPRNT                                                      
         B     SACN10                                                           
*                                                                               
SACNPRNT LR    R0,RE                                                            
         GOTO1 ASPOOL,DMCB,(R8)                                                 
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
SACN10   MVC   SPOOLRPN,PLREPNO                                                 
         MVI   PLCC,0                                                           
         DROP  R2                                                               
***************                                                                 
* PUT OUT EDICT HEADER RECORD                                                   
***************                                                                 
         MVC   P1+4(5),=C'*HDR*'    HEADER RECORD                               
         MVC   P1+9(14),=CL14'EDICT=*DDSDARA'                                   
         MVI   P1+34,C'W'           WIDE REPORT                                 
         MVI   P1+35,C'P'           /PAGE FOR EASYLINK AND SUPRESS TOP          
*                                                                               
         MVC   P2(14),=CL14'++DDS DANOTTRN'                                     
         BAS   RE,SACNPRNT                                                      
*                                                                               
         LA    R2,P                                                             
         USING PAGYCAND,R2                                                      
         MVC   PACNTID,=C'AGYCAN'  ORDER CANCELLATION (NOTDARE)                 
         MVC   PACNORDR,6(R7)      ORDER # IS ALWAYS 6 BYTES FROM BEG.          
         MVC   PACNFRID,RORCTOID-RORDRCLD(R7)                                   
         MVC   PACNTOID,RORCFRID-RORDRCLD(R7)                                   
         MVC   PACNROUT,ROUTNGCD                                                
*                                                                               
**** SPECIAL CODE FOR MEDIA OCEAN CUTOFF DATE *** HWON 11/12/02                 
*&&DO                                                                           
         GOTOR CHKMOREP,DMCB,PACNTOID,PACNORDR                                  
*&&                                                                             
**** SPECIAL CODE FOR MEDIA OCEAN CUTOFF DATE *** HWON 11/12/02                 
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(X'20',PACNDATE)                              
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,PACKOF4B                                                      
*                                                                               
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
*                                                                               
         CP    PACKOF4B,=P'240000'     PAST MIDNIGHT?                           
         BL    SACN20                                                           
         SP    PACKOF4B,=P'240000'     YES, BUMP TO NEXT DAY AND ADJUST         
         GOTO1 VADDAY,DMCB,PACNDATE,(X'20',PACNDATE),F'1'                       
*                                                                               
SACN20   ICM   R1,15,PACKOF4B                                                   
         SRL   R1,12               GET RID OF SECONDS AND SIGN                  
         STH   R1,HALF                                                          
         GOTO1 VHEXOUT,DMCB,HALF,PACNTIME,L'HALF                                
*                                                                               
         MVC   PACNQSTA,RORCQSTA-RORDRCLD(R7)                                   
         MVC   PACNRPCN,RORCRPCN-RORDRCLD(R7)                                   
         MVC   PACNRTRN,RORCRTRN-RORDRCLD(R7)                                   
         MVC   PACNOLDS,RORCOLDS-RORDRCLD(R7)                                   
         MVI   PACNCFLG,C'R'       CANCELLING THIS REVISION                     
         BAS   RE,SACNPRNT                                                      
*                                                                               
         MVC   P(26),=CL26'*** END OF DDS MESSAGE ***'                          
         BAS   RE,SACNPRNT                                                      
*                                                                               
         MVI   SPMODE,X'FF'        CLOSE THE PRINTQ                             
         BAS   RE,SACNPRNT                                                      
*                                                                               
SACNX    B     XIT                                                              
         DROP  R2,R8                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GETS THE REP CONTRACT RECORD FOR UPDATE AND PUTS IT IN AIO1                   
*                                                                               
* ON ENTRY:    PARAM1              REP CONTRACT #                               
*              R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
GETRPCON NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,0(R1)            SET A(REP ORDER #)                           
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RCONKEY,R4                                                       
         MVI   RCONPTYP,X'8C'      INSERT KEY TYPE                              
         MVC   RCONPREP,POWERCDE   INSERT POWER CODE                            
*                                                                               
         LR    RE,R3               MAKE SURE ORDER NUMBER IS VALID              
         LA    RF,L'RDNTRPCN                                                    
GRPCN10  CLI   0(RE),C'0'                                                       
         BL    GRPCNNO2                                                         
         CLI   0(RE),C'9'                                                       
         BH    GRPCNNO2                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,GRPCN10                                                       
*                                                                               
         PACK  DUB(8),0(8,R3)                                                   
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB+3(5)                                              
*                                  CALCULATE 9'S COMP                           
         MVO   WORK(5),WORK+10(5)                                               
         MVC   RCONPCON,WORK                                                    
         DROP  R4                                                               
*                                                                               
         BRAS  RE,HIGHREP                                                       
*                                                                               
         CLC   KEY(RCONPCON+4-RCONPTYP),KEYSAVE                                 
         BNE   GRPCNNO                                                          
*********                                                                       
* READ ORDER RECORD FOR UPDATE                                                  
*********                                                                       
         MVI   DMINBTS,X'88'       READ FOR DELETED JUST IN CASE                
         MVC   AIO,AIO1               REP DELETES CONTRACT                      
         BRAS  RE,GETREP                                                        
*                                                                               
GRPCNYES B     YES                                                              
*                                                                               
GRPCNNO  LHI   R1,REFRCNNE         CONTRACT DOES NOT EXIST                      
         B     *+8                                                              
*                                                                               
GRPCNNO2 LHI   R1,REFBDCON         BAD REP CONTRACT                             
*                                                                               
         CLC   =C'ERRNOT',0(R7)           ERROR ON ERRNOT?                      
         BE    NO                                                               
         BRAS  RE,SNDERROR                                                      
         B     NO                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* RECORD AUDIT TRAIL FOR REP DARE ORDER RECORD                                  
*                                                                               
* ON ENTRY:    PARAM1,BYTE 0       AUDIT TRAIL ACTION EQUATE                    
*                     BYTE 2-3     IF BYTE 0=0, ERROR NUMBER                    
*                                                                               
* AIO1 MUST CONTAIN REP DARE ORDER HEADER RECORD                                
***********************************************************************         
AUDTRAIL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   ORDAUDTR,0(R1)                                                   
*                                                                               
         CLI   ORDAUDTR,0                                                       
         BNE   AUDTR10                                                          
         ZICM  R3,2(R1),2                                                       
*                                                                               
AUDTR10  DS    0H                                                               
         L     R6,AIO1             R6 = A(AGENCY HEADER ELEMENT)                
         USING RDARREC,R6                                                       
         MVC   REVISION,RDARRNUM   SAVE AGY REVISION NUMBER                     
         DROP  R6                                                               
*                                                                               
         MVC   KEY(L'RDARKEY),0(R6)                                             
         LA    R6,KEY                                                           
         USING RDARKEY,R6                                                       
         MVI   RDARKRT,X'70'       TYPE AUDIT TRAIL                             
         XC    RDARKSEQ(2),RDARKSEQ                                             
         DROP  R6                                                               
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,DMRDHI,=C'REPDIR',KEY,KEY                          
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
*                                                                               
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE                                    
         BNE   AUDTRX                                                           
*                                                                               
         MVI   DMINBTS,X'80'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,GETREP                                                        
*                                                                               
* ADD AUDIT TRAIL ELEMENT                                                       
*                                                                               
         XC    ELEM,ELEM                                                        
DRATD    USING RDARHSEM,ELEM                                                    
         MVI   DRATD.RDARHSCD,X'50'                                             
         MVI   DRATD.RDARHSLN,RDARHSLQ                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(2,DRATD.RDARHSDT)                            
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,PACKOF4B                                                      
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
*                                                                               
         ICM   R1,15,PACKOF4B                                                   
         SRL   R1,4                                                             
         STCM  R1,B'0111',DRATD.RDARHSTM                                        
*                                                                               
         CLI   ORDAUDTR,0          IF ERRNOT, INSERT ERROR NUMBER               
         BNE   AUDTR20                                                          
         STCM  R3,B'0011',DRATD.RDARHSER                                        
         B     AUDTR30                                                          
*                                                                               
AUDTR20  DS    0H                  ELSE INSERT ACTION CODE                      
         MVI   DRATD.RDARHSER,X'FF'                                             
         MVC   DRATD.RDARHSAC,ORDAUDTR                                          
*                                                                               
AUDTR30  DS    0H                                                               
         MVC   DRATD.RDARHSVR,REVISION                                          
         DROP  DRATD                                                            
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),AIO1,ELEM,0                       
         CLI   DMCB+12,0                                                        
         BNE   AUDTRX                                                           
*                                                                               
         GOTO1 VDATAMGR,DMCB,PUTREC,=C'REPFILE',KEY,AIO1,DMWORK                 
*                                  REWRITE THE RECORD                           
AUDTRX   DS    0H                                                               
         B     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CONVERTS EDICT DAY DISPLACEMENT TO DISK ADDRESS AND              
* VICE VERSA                                                                    
*                                                                               
* ON ENTRY:    PARAM 1   X'80'     ON : DAY DISP  -> DISK ADDR                  
*                                  OFF: DISK ADDR -> DAY DISP                   
*              EDCTFDSP            DAY DISP                                     
*              EDCTFDSK            LAST TRACK AND BLOCK READ                    
*              RECNUM              WHICH RECORD IN BLOCK                        
*                                                                               
* WARNING:  DUB AND HALF  GETS CLOBBERED                                        
***********************************************************************         
DSKCNVRT NTR1  BASE=*,LABEL=*                                                   
         TM    0(R1),X'80'         DAY DISP -> DISK ADDR?                       
         BNZ   DCNVRT50            YES                                          
***********************************                                             
* CONVERTING  DISK ADDR  TO  DAY DISP  FORMAT                                   
***********************************                                             
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,EDCTFDSK       TRACK #                                      
         BCTR  R1,0                                                             
         LH    RF,EDCTFTPD         TRACKS/DAY                                   
         DR    R0,RF               R1 = QUOTIENT                                
         LA    R1,1(R1)            R1 = DAY NUMBER                              
         CVD   R1,DUB                                                           
         SRP   DUB(8),1,0          MULTIPLY BY 10                               
         MVC   EDCTFDSP(1),DUB+6   FIRST 2 DIGITS (PWOS) ARE DAY NUMBER         
*                                                                               
         MH    R0,EDCTFRPT         R0 = BLOCKS UP TO THIS TRACK                 
         LLC   R1,EDCTFDSK+2       R1 = BLOCK NUMBER FROM DISK ADDR             
         BCTR  R1,0                                                             
         AR    R0,R1                                                            
         MVC   HALF,EDCTRPBQ                                                    
         MH    R0,HALF                                                          
         LLC   R1,RECNUM           R1 = WHICH RECORD NUMBER                     
         AR    R0,R1                                                            
         CVD   R0,DUB                                                           
         SRP   DUB(8),1,0                                                       
         MVC   EDCTFDSP+1(3),DUB+4                                              
         B     DCNVRTX                                                          
***********************************                                             
* CONVERTING  DAY DISP  TO  DISK ADDR  FORMAT                                   
***********************************                                             
DCNVRT50 XC    DUB,DUB                                                          
         MVC   DUB+6(1),EDCTFDSP   DAY NUMBER                                   
         MVI   DUB+7,X'0F'                                                      
         SRP   DUB(8),64-1,0       DIVIDE BY 10                                 
         CVB   RF,DUB                                                           
         BCTR  RF,0                                                             
         MH    RF,EDCTFTPD         RF = STARTING TRACK NUMBER THIS DAY          
*                                                                               
         MVC   DUB+4(3),EDCTFDSP+1 DAY NUMBER                                   
         MVI   DUB+7,X'0F'                                                      
         SRP   DUB(8),64-1,0       DIVIDE BY 10                                 
         CVB   R1,DUB              R1 = RECORD OFFSET INTO DAY                  
         LTR   R1,R1                                                            
         BZ    *+6                                                              
         BCTR  R1,0                                                             
*                                                                               
         SR    R0,R0                                                            
         LH    RE,EDCTRPBQ         # RECORDS/BLOCK                              
         DR    R0,RE                                                            
         AHI   R0,1                                                             
         STC   R0,RECNUM           R0 = LOGICAL RECORD NUMBER                   
         SR    R0,R0                                                            
         LH    RE,EDCTFRPT         # OF BLOCKS/TRACK                            
         DR    R0,RE                                                            
         LA    RF,1(R1,RF)         RF = TRACK NUMBER                            
         STCM  RF,3,EDCTFDSK                                                    
         AHI   R0,1                                                             
         STC   R0,EDCTFDSK+2       R0 = BLOCK NUMBER                            
*                                                                               
DCNVRTX  B     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE UPDATES INFORMATION FOR DARE ON CTFILE SO IN CASE THE            
* SYSTEM GETS BROUGHT DOWN, WE HAVE A STARTING POINT INSTEAD OF                 
* STARTING FROM THE FIRST RECORD IN THE EDICT FILE.                             
***********************************************************************         
         SPACE 1                                                                
UPDATCTL NTR1  BASE=*,LABEL=*                                                   
         L     R9,SRPARMSD.SRQASYSF                                             
*                                                                               
         L     RE,VSSB                                                          
         USING SSBD,RE                                                          
*********                                                                       
* REMOVED CODE THAT WOULD WRITE OUT TO CFILE EVERY 8TH TIME                     
* WE SHOULD ALWAYS UPDATE THE DATA SO THAT WE NEVER REPROCESS ENTRIES           
*********                                                                       
**********                                                                      
**** CODE COMMENTED OUT AS                                                      
**** SSBDARDT WILL ALWAYS BE SET AS PROCESS WILL ALWAYS BE CALLED               
**********                                                                      
*&&DO                                                                           
         MVC   SSBDARDT,BTODAY                                                  
*&&                                                                             
         L     R6,AIO2             BUILD THE RECORD                             
         USING CTDARREC,R6                                                      
         XC    CTDARKEY,CTDARKEY                                                
         MVI   CTDARTYP,CTDARTYQ                                                
         MVI   CTDARSUB,CTDARSBQ                                                
         MVC   CTDARSYS,THESYSID                                                
         MVC   CTDARLEN,=Y(CTDAR1ST-CTDARREC)  L(REC WITHOUT ELEMENTS)          
*                                                                               
         LA    R6,ELEM                                                          
         USING CTDARELD,R6                                                      
         XC    ELEM,ELEM                                                        
         MVI   CTDAREL,CTDARELQ                                                 
         MVI   CTDARLN,CTDARLNQ                                                 
         MVC   CTDARDAT,SSBDARDT                                                
         MVC   CTDAREDA,SSBDAREA                                                
         MVC   CTDAREDR,SSBDARER                                                
         DROP  RE                                                               
*                                                                               
         L     R6,AIO2             BUILD THE RECORD                             
         USING CTDARREC,R6                                                      
         LA    R6,CTDAR1ST                                                      
         DROP  R6                                                               
*                                                                               
         LA    R1,ELEM+128         CTFILE IS NOT DEFINED FOR RECUP              
         MVC   0(2,R1),=Y(CTDAR1ST-CTDARREC)                                    
         MVC   2(2,R1),=Y(CTDARLEN-CTDARREC)                                    
         MVC   4(2,R1),=H'1000'                                                 
         GOTO1 VRECUP,DMCB,(X'FE',AIO2),ELEM,(R6),ELEM+128                      
*                                                                               
         MVI   DMCB,X'0A'          SWITCH TO CONTROL SYSTEM                     
         BAS   RE,SWTCHSYS         SUCCESSFUL SWITCH?                           
         BNE   EXITPRG             EXIT PROGRAM, SHOULD WAIT FOR IT             
*                                                                               
         L     R6,AIO2             BUILD THE RECORD                             
         USING CTDARREC,R6                                                      
         XC    KEY,KEY             SEE IF IT EXISTS FOR THIS SYSTEM             
         MVC   KEY(L'CTDARKEY),CTDARKEY                                         
*                                                                               
         MVI   DMINBTS,X'08'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,HIGHCT                                                        
*                                                                               
         CLI   DMCB+8,2            RECORD DELETED?                              
         BE    UPCTL10                                                          
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
*                                                                               
UPCTL10  L     R6,AIO1                                                          
*                                                                               
         L     R0,AIO2             IF NO CHANGE TO THE RECORD                   
         ZICM  R1,CTDARLEN,2                                                    
         L     RE,AIO1                                                          
         LR    RF,R1                                                            
         CLCL  R0,RE                                                            
         BE    UPCTLX              THEN NOTHING TO WRITE OUT                    
*                                                                               
         MVC   AIO,AIO2            ADD/WRITE OUT OF IO2                         
         CLC   CTDARKEY,KEY        ADD ONE IF WE CAN'T FIND IT                  
         BE    UPCTL15                                                          
         BRAS  RE,ADDCT                                                         
         B     UPCTL20                                                          
*                                                                               
UPCTL15  MVI   DMINBTS,X'88'                                                    
         MVC   AIO,AIO1            NEED THIS FOR RDUPDATE                       
         BRAS  RE,HIGHCT                                                        
         MVC   AIO,AIO2            WRITING OUT AIO2                             
         BRAS  RE,WRTCT                                                         
*                                                                               
UPCTL20  CLI   THESYSID,1          TST  SYSTEM?                                 
         BE    UPCTLX              YES, TST CAN'T UPDATE GENFIL                 
*                                                                               
         L     R6,AIO2             BUILD THE RECORD                             
         USING GEDARREC,R6                                                      
         XC    0(255,R6),0(R6)                                                  
         MVI   GEDARSYS,GEDARSYQ                                                
         MVI   GEDARTYP,GEDARTYQ                                                
         MVI   GEDARSTY,GEDARSTQ                                                
         MVC   GEDARFPK,THESYSID                                                
         MVC   GEDARLEN,=Y(GEDAR1ST-GEDARREC+GEDARLNQ)                          
         MVC   GEDAR1ST(GEDARLNQ),ELEM                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'GEDARKEY),GEDARKEY                                         
         MVI   DMINBTS,X'08'                                                    
         BRAS  RE,HIGHGD                                                        
         CLI   DMCB+8,2            RECORD DELETED?                              
         BE    UPCTL25                                                          
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
*                                                                               
UPCTL25  CLC   KEY(L'GEDARKEY),KEYSAVE                                          
         BNE   UPCTL40             OK, NEED TO ADD ON GENFIL                    
*                                                                               
         MVI   DMINBTS,X'88'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,GETGFL                                                        
         CLI   DMCB+8,2            RECORD DELETED?                              
         BE    UPCTL30                                                          
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
*                                                                               
UPCTL30  MVC   AIO,AIO2            WRITING OUT AIO2                             
         BRAS  RE,PUTGFL                                                        
         B     UPCTLX                                                           
*                                                                               
UPCTL40  MVC   AIO,AIO2            ADDING AIO2                                  
         XC    KEY,KEY                                                          
         L     R6,AIO2             BUILD THE RECORD                             
         MVC   KEY(L'GEDARKEY),0(R6)                                            
         BRAS  RE,ADDGFL                                                        
*                                                                               
UPCTLX   NI    DMINBTS,X'FF'-X'88'                                              
         B     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE ERROR NOTIFICATION FOR AGENCY                                   
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
ERRNOTCE NTR1  BASE=*,LABEL=*                                                   
         L     R9,SRPARMSD.SRQASYSF                                             
*                                                                               
         USING RDLNNOTD,R7                                                      
PERNTD   USING RTN2SNDR,RDNTRTRN                                                
         MVC   QREPCON,RDNTRPCN                                                 
         MVC   QRETURN,RDNTRTRN                                                 
*                                                                               
         CLI   WHCHEDCT,C'R'       REP EDICT IN USE?                            
         BE    PROCERNR                                                         
*                                                                               
         MVI   QMED,C'T'                                                        
         CLI   PERNTD.RTNAGYMD+1,C'2'  (---MD OF AGYMD IS 2?)                   
         BNE   *+8                                                              
         MVI   QMED,C'R'                                                        
*                                                                               
         GOTO1 VHEXIN,DMCB,PERNTD.RTNAGYMD,BAGYMD,2                             
         MVC   AGENCY,PERNTD.RTNPWRCD                                           
         GOTO1 VHEXIN,DMCB,PERNTD.RTNSYSID,BYTE,2                               
         BAS   RE,SPANKED                                                       
*                                                                               
         NI    MISCFLG1,X'FF'-MF1XMTUP                                          
*                                                                               
         CLC   BYTE,THESYSID       WRONG FACPAK?                                
         BNE   PERNTNO             YES, AGENCY DOESN'T BELONG HERE              
*                                                                               
         DROP  PERNTD                                                           
*                                                                               
         GOTO1 CALCORDR,DMCB,RDNTORDR    CONVERT ORDER NUMBER TO BINARY         
         BNE   PERNTNO                                                          
*                                                                               
         MVC   USERID,RDNTTOID                                                  
         BAS   RE,SWTCHCTL         SWTICH CONTROL TO SPOT SYSTEM                
         BNE   PERNTNO                                                          
*                                                                               
         BRAS  RE,GETORDER         READ ORDER RECORD INTO AIO1                  
         BNE   PERNTNO                                                          
*                                                                               
         CLC   MDNTOFRI-MDLNNOTD(L'MDNTOFRI,R7),=C'   '  FOR MKGD GRP?          
         BNH   PERNT10                                    NO                    
*                                                                               
* ERROR IS FOR THE MAKEGOOD                                                     
*                                                                               
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
         MVC   QBUYER,DOIDBYR      SAVE THE BUYER CODE                          
         MVC   BCLT,DOIDCLT                                                     
         GOTO1 VCLUNPK,DMCB,DOIDCLT,QCLT                                        
         MVC   QPRD1,DOIDPRD                                                    
         MVC   QPRD2,DOIDPRD2                                                   
         MVC   BEST,DOIDEST                                                     
         EDIT  (B1,DOIDEST),(3,QEST1),FILL=0                                    
         XC    WORK,WORK                                                        
         MVC   WORK+2(L'DOISTA),DOISTA                                          
         GOTO1 VMSUNPK,DMCB,WORK,WORK+10,QSTA                                   
         DROP  R6                                                               
         MVC   QMGGROUP,MDNTOFRI-MDLNNOTD(R7)   MAKEGOOD GROUP CODE             
*                                                                               
         BRAS  RE,GETNOTCE         READ THE MAKEGOOD NOTICE                     
         BNE   PERNTNO                                                          
*                                                                               
         XC    ELEM,ELEM           WRITE A ERROR STATUS ELEM TO REC             
         LA    R6,ELEM                                                          
         USING MNSTELD,R6                                                       
         MVI   MNSTEL,MNSTELQ                                                   
         MVI   MNSTLEN,MNSTELNQ                                                 
         GOTO1 VDATCON,DMCB,(0,RDNTDATE),(19,MNSTDATE)                          
         GOTO1 VHEXIN,DMCB,RDNTTIME,MNSTTIME,L'RDNTTIME                         
         MVI   MNSTSTAT,MNSTERR    ERROR STATUS                                 
         MVI   SVSTAT,MNSTERR      SAVE IT FOR LATER                            
         MVI   THISISMG,C'Y'                                                    
*                                                                               
         PACK  DUB,RDNTEFLG                                                     
         CVB   R1,DUB                                                           
         STCM  R1,3,MNSTERRN       SAVE ERROR NUMBER                            
*****                                                                           
         CHI   R1,105              HARRIS REPROCESS BUG?                        
         BE    PERNTNO             YES, WE DON'T WANT TO PATCH ANYMORE          
*****                                                                           
         DROP  R6                                                               
*                                                                               
         L     R2,AIO1             LOOK FOR LAST STATUS                         
         LA    R6,MNRFRST-MNKEY(R2)                                             
         GOTO1 VRECUP,DMCB,(C'S',AIO1),ELEM,(R6)                                
         ORG   *-2                                                              
         CLI   QSTA,C'0'           CABLE?                                       
         BL    PERNT09              NO                                          
         LA    R6,MNXFRST-MNXKEY(R2)                                            
         ST    R6,8(R1)                                                         
         MVI   0(R1),X'FE'         CAN'T USE C'T', BC RECUP NOT CHANGED         
         LAY   RE,XSPREC            TO USE MAX REC SIZE IN DMFILTAB HAS         
         ST    RE,12(R1)            FOR XSPFIL                                  
PERNT09  BASR  RE,RF                                                            
         B     PERNTWRT            AND WRITE THE RECORD BACK OUT                
*                                                                               
* ERROR IS FOR THE ORDER                                                        
*                                                                               
PERNT10  L     R6,AIO1             REMOVE ANY OLD ERROR COMMENTS                
         USING DOKEY,R6                                                         
         LA    R6,DORFRST                                                       
PERNT13  CLI   0(R6),0                                                          
         BE    PERNT20                                                          
         CLI   0(R6),DOCOMELQ                                                   
         BE    PERNT16                                                          
         CLI   1(R6),1                                                          
         JNH   *+2                                                              
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PERNT13                                                          
PERNT16  GOTO1 VRECUP,DMCB,(C'S',AIO1),(R6)                                     
         B     PERNT10                                                          
*                                                                               
PERNT20  LA    R2,ELEM             ADD THE ERROR COMMENT TO RECORD              
         USING DOCOMELD,R2                                                      
         XC    ELEM,ELEM                                                        
         MVI   DOCOMEL,DOCOMELQ                                                 
         MVI   DOCOMLEN,DOCOMOVH+L'RDNTEFLG                                     
         MVC   DOCOMTXT(3),RDNTEFLG      COPY THE NUMBER FOR NOW                
*                                                                               
         GOTO1 VRECUP,DMCB,(C'S',AIO1),ELEM,(R6)                                
*                                                                               
         NI    MISCFLG1,X'FF'-MF1NOXMT    ASSUME XMT ELEM EXISTS                
         SR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
PERNT30  CLI   0(R6),0                                                          
         BE    PERNTNO                                                          
*                                                                               
PERNT35  CLI   0(R6),DOXMTELQ                                                   
         BE    PERNT40                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PERNT30                                                          
*                                                                               
         USING DOXMTELD,R6                                                      
PERNT40  CLI   DOXMTSTA,QCFMD      ERROR ON A CONFIRMED ORDER?                  
         BE    PERNTNO             NOT GOING TO HAVE ERRNOT ON A ERRNOT         
         CLI   DOXMTSTA,QRJCT      ERROR ON AN REJECTED ORDER?                  
         BE    PERNTNO                                                          
         CLI   DOXMTSTA,QUNDARE    ERROR ON AN UNDARED ORDER?                   
         BE    PERNTNO                                                          
         CLI   DOXMTSTA,QNODARE    ERROR ON AN NOTDARED ORDER?                  
         BE    PERNTNO                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(0,RDNTDATE),(19,DOXMTSTD)                          
         GOTO1 VHEXIN,DMCB,RDNTTIME,DOXMTSTT,L'RDNTTIME                         
         MVI   DOXMTSTA,QERRORED      ORDER IS IN ERROR                         
         MVI   SVSTAT,QERRORED                                                  
         DROP  R6                                                               
*                                                                               
PERNT99  OI    MISCFLG1,MF1XMTUP                                                
***************                                                                 
* TIME TO PROCESS STATUS HISTORY ELEMENT                                        
***************                                                                 
PERNT100 SR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
PERNT110 CLI   0(R6),0                                                          
         BE    PERNTNO                    YES, THEN DON'T GIVE ERROR            
PERNT115 CLI   0(R6),DOSTELQ                                                    
         BE    PERNT120                                                         
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PERNT110                                                         
*                                                                               
         USING DOSTELD,R6                                                       
PERNT120 CLI   DOSTSTAT,QCFMD      ERROR ON A CONFIRMED ORDER?                  
         BE    PERNTNO             NOT GOING TO HAVE ERRNOT ON A ERRNOT         
         CLI   DOSTSTAT,QRJCT      ERROR ON A REJECTED ORDER?                   
         BE    PERNTNO                                                          
         CLI   DOSTSTAT,QUNDARE    ERROR ON A UNDARED ORDER?                    
         BE    PERNTNO                                                          
         CLI   DOSTSTAT,QNODARE    ERROR ON A NOTDARED ORDER?                   
         BE    PERNTNO                                                          
         DROP  R6                                                               
*                                                                               
         GOTO1 VDATCON,DMCB,(0,RDNTDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,RDNTTIME,DUB+L'DOSTDATE,L'RDNTTIME                   
*                                                                               
         LA    R2,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
         USING DOSTELD,R2                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   DOSTEL,DOSTELQ                                                   
         MVI   DOSTLEN,DOSTLNQ                                                  
         MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         MVI   DOSTSTAT,QERRORED                                                
         MVI   SVSTAT,QERRORED                                                  
         DROP  R2                                                               
*                                                                               
         L     R6,AIO                                                           
         LA    R6,DORFRST-DOKEY(R6)  R6 = A(FIRST ELEMENT IN RECORD)            
PERNT130 CLI   0(R6),0          ADD AFTER ALL TRANSMISSION ELEMENTS             
         BE    PERNT140         AND BEFORE AN AUDIT ELEMENT                     
         CLI   0(R6),DORPELQ2                                                   
         BE    PERNT140                                                         
         CLI   0(R6),DOSTELQ                                                    
         BNL   PERNT140                                                         
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PERNT130                                                         
*                                                                               
PERNT140 GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
*                                                                               
PERNTWRT XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         ST    R6,AIO                                                           
*                                                                               
         LAY   RF,PUT                                                           
         CLC   MDNTOFRI-MDLNNOTD(L'MDNTOFRI,R7),=C'   '  FOR MKGD GRP?          
         BNH   PERNTWR2                     NO                                  
         CLI   QSTA,C'0'                   CABLE MG?                            
         BL    PERNTWR2                     NO                                  
         MVC   KEY(L'MNXKEY),0(R6)                                              
         LAY   RF,XPUT                                                          
PERNTWR2 BASR  RE,RF                                                            
*                                                                               
         BRAS  RE,CHKFRCHG         BUILD CHECK FOR CHANGES KEY                  
*                                                                               
         BAS   RE,UPDATSB1                                                      
*                                                                               
PERNTYES BRAS  RE,BLDCOLOR                                                      
         BRAS  RE,DAREMAIL                                                      
         B     YES                                                              
*                                                                               
PERNTNO  TM    MISCFLG1,MF1XMTUP                                                
         BNZ   PERNTWRT                                                         
         B     NO                                                               
         SPACE 2                                                                
***********************************************************************         
* PROCESSES THE ERROR NOTIFICATION FOR REP                                      
* CAN ONLY PROCESS DARE ORDERS LINKED TO A CONTRACT                             
* ALSO PROCESSES MAKEGOOD OFFERS                                                
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
PROCERNR DS    0H                                                               
         USING MDLNNOTD,R7                                                      
*                                                                               
         CLC   =CL8' ',MDNTRPCN                                                 
         BNL   PERNRNO                                                          
         CLC   MDNTRPCN,=8C'0'                                                  
         BE    PERNRNO                                                          
*                                                                               
         MVC   REP,MDNTTOID                                                     
         BAS   RE,SWTCHCTR                                                      
         BNE   PERNRNO             NOT SUCCESSFUL                               
*                                  READ REP CONTRACT INTO AIO1                  
         GOTO1 =A(GETRPCON),DMCB,MDNTRPCN,RR=RELO                               
         BNE   PERNRNO                                                          
*                                                                               
         OC    MDNTOFRI,MDNTOFRI   CHECK IF ERROR IS FOR ORDER                  
         BZ    PERNR100            OR                                           
         CLC   MDNTOFRI,ALLSPCES   FOR MAKEGOODS                                
         BE    PERNR100                                                         
*                                                                               
* RECORD ERROR FOR MAKEGOOD                                                     
*                                                                               
         GOTO1 =A(GETRPMKG),RR=RELO READ REP MAKEGOOD OFFER INTO AIO1           
         BNE   PERNRNO                                                          
*                                                                               
         LA    R3,ELEM             BUILD AUDIT TRAIL ELEMENT                    
         USING RMKGATEM,R3                                                      
         XC    ELEM,ELEM                                                        
         MVI   RMKGATCD,X'02'                                                   
         MVI   RMKGATLN,RMKGAL2Q                                                
         GOTO1 VDATCON,DMCB,(0,MDNTDATE),(2,RMKGATDT)                           
         GOTO1 VHEXIN,DMCB,MDNTTIME,RMKGATTM,L'MDNTTIME                         
         PACK  DUB,MDNTSEQN        VERSION/SEQUENCE NUMBER                      
         CVB   R1,DUB                                                           
         STC   R1,RMKGATVR                                                      
*                                                                               
         MVI   RMKGATAT,X'80'      SET ERROR CONDITION                          
*                                                                               
         LA    RF,L'MDNTEFLG       CHECK ALL NUMERIC                            
         LA    RE,MDNTEFLG                                                      
PERNR04  CLI   0(RE),C'0'          CAN'T HANDLE, SET TO 0                       
         BL    PERNR09                                                          
         CLI   0(RE),C'9'                                                       
         BH    PERNR09                                                          
         LA    RE,1(RE)                                                         
         BCT   RF,PERNR04                                                       
*                                                                               
         PACK  DUB,MDNTEFLG        ERROR NUMBER                                 
         CVB   R1,DUB                                                           
         STCM  R1,3,RMKGATEN                                                    
         DROP  R3                                                               
*                                                                               
PERNR09  DS    0H                                                               
         L     R6,AIO1                                                          
         USING RMKGRECD,R6                                                      
         MVI   RMKGSFG1,RMGF1MER   SET ERROR RECEIVED                           
         DROP  R6                                                               
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,RMKGELEM-RMKGREC(R6)                                          
PERNR10  CLI   0(R6),0             FIND LAST AUDIT TRAIL ELEMENT                
         BE    PERNR20                                                          
         CLI   0(R6),X'02'                                                      
         BH    PERNR20                                                          
         LLC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         B     PERNR10                                                          
*                                                                               
PERNR20  DS    0H                  ADD ERROR AUDIT TRAIL ELEMENT                
         GOTO1 VRECUP,DMCB,(C'R',AIO1),ELEM,(R6)                                
*                                                                               
         MVC   AIO,AIO1                                                         
         BRAS  RE,PUTREP                                                        
*                                                                               
         B     PERNRYES                                                         
*                                                                               
* RECORD ERROR FOR ORDER                                                        
*                                                                               
PERNR100 DS    0H                                                               
         GOTO1 =A(GETREPDR),RR=RELO                                             
         BNZ   PERNRNO                                                          
*                                                                               
         SR    R3,R3                                                            
         LA    RF,L'MDNTEFLG       CHECK ALL NUMERIC                            
         LA    RE,MDNTEFLG                                                      
PERNR110 CLI   0(RE),C'0'                                                       
         BL    PERNR130            CAN'T HANDLE, SET TO 0                       
         CLI   0(RE),C'9'                                                       
         BH    PERNR130                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,PERNR110                                                      
*                                                                               
PERNR120 DS    0H                                                               
         PACK  DUB,MDNTEFLG        ERROR NUMBER                                 
         CVB   R3,DUB                                                           
*                                                                               
PERNR130 DS    0H                                                               
         GOTO1 =A(AUDTRAIL),DMCB,(0,(R3)),RR=RELO                               
*                                                                               
PERNRYES DS    0H                                                               
         BAS   RE,UPDATSB1                                                      
         B     YES                                                              
*                                                                               
PERNRNO  DS    0H                                                               
         BAS   RE,UPDATSB1                                                      
         B     NO                                                               
         DROP  R7                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GETS THE REP MAKEGOOD RECORD FOR UPDATE AND PUTS IT IN AIO1                   
*                                                                               
* ON ENTRY:    AIO1                CONTRACT RECORD                              
*              R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
GETRPMKG NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'**GRMG**'                                                    
*                                                                               
         USING MDLNNOTD,R7                                                      
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RMKGRECD,R4                                                      
         MVI   RMKGKTYP,X'11'      INSERT KEY TYPE                              
         MVC   RMKGKREP,POWERCDE   INSERT POWER CODE                            
*                                                                               
         L     R6,AIO1                                                          
         USING RCONRECD,R6                                                      
         MVC   RMKGKOFF,RCONKOFF   INSERT OFFICE                                
         MVC   RMKGKSTA,RCONKSTA   INSERT STATION                               
         DROP  R6                                                               
*                                                                               
         PACK  DUB(8),MDNTRPCN                                                  
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB+3(5)                                              
*                                  CALCULATE 9'S COMP                           
         MVO   WORK+15(5),WORK+10(5)                                            
*                                  REVERSE THE COMPLEMENT                       
         PACK  RMKGKCON+0(1),WORK+18(1)                                         
         PACK  RMKGKCON+1(1),WORK+17(1)                                         
         PACK  RMKGKCON+2(1),WORK+16(1)                                         
         PACK  RMKGKCON+3(1),WORK+15(1)                                         
*                                                                               
         MVC   RMKGKGRP,MDNTOFRI   INSERT GROUP ID                              
         DROP  R4                                                               
*                                                                               
         BRAS  RE,HIGHREP                                                       
*                                                                               
         CLC   KEY(L'RMKGKEY),KEYSAVE                                           
         BNE   GRPMGNO                                                          
*********                                                                       
* READ MAKEGOOD OFFER RECORD FOR UPDATE                                         
*********                                                                       
         MVI   DMINBTS,X'80'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,GETREP                                                        
*                                                                               
GRPMGYES B     YES                                                              
*                                                                               
GRPMGNO  B     NO                                                               
         DROP  R7                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE AGENCY RECALL FOR REP                                           
*                                                                               
* ***NOTE***  REP SIDE CANNOT RELY ON THE AGYMD IN THE RETURN TO SENDER         
*             AS A NON-DDS SPOT AGENCY CAN SEND A RECALL                        
*                                                                               
* ** BIG ***  THIS ROUTINE USES WRKRIOA TO BUILD PASSIVE KEYS FOR THE           
* ***NOTE***  REP DARE HEADER RECORD. DO NOT CLOBBER THIS AREA IN THIS          
*             ROUTINE !!                                                        
***********************************************************************         
AGYRCALL NTR1  BASE=*,LABEL=*                                                   
         L     R9,SRPARMSD.SRQASYSF                                             
*                                                                               
         USING PAGYRCLD,R7                                                      
*                                                                               
         L     R3,AWRKRIOA                                                      
         USING REPDKEYD,R3                                                      
*                                                                               
         MVC   QREPCON,PARCRPCN    COPY THESE VALUES                            
         MVC   QRETURN,PARCRTRN                                                 
*                                                                               
         MVC   REP,PARCTOID        GET TO ID FOR REP IDENTIFICATION             
         BAS   RE,SWTCHCTR         SWITCH CONTROL TO REP  SYSTEM                
         BNE   AGRCLNO             NOT SUCCESSFUL                               
*                                                                               
         XC    DKEY,DKEY           WE'LL USE DKEY TO STORE A LIST               
         MVC   DKEY(6),PARCQSTA    TV WILL HAVE 2 SETS OF CALLS                 
         MVC   DKEY+6(6),PARCOLDS     OF 6 CHARACTERS EACH                      
*&&DO                                                                           
         CLC   PARCQSTA+4(2),=C'00'   STATION UNIQUE ID?                        
         BL    AGRCL30                NO, JUST TV WITH 2 SETS                   
*                                                                               
         OI    MISCFLG3,MF3RADIO   DON'T FORGET TO TURN OFF LATER               
         XC    KEY,KEY             YES, ONLY DDS AGYS WOULD SEND THIS           
         LA    R2,KEY                                                           
         USING RSTAKEY,R2                                                       
         MVI   RSTUKTYP,X'83'      REP STATION RECORDS                          
         MVI   RSTUKSTP,X'08'          PASSIVE BY REP POWER/UID                 
         MVC   RSTUKREP,POWERCDE   REP'S POWER CODE                             
         MVC   RSTUKUID,PARCQSTA   STATION'S UID                                
*                                                                               
         BRAS  RE,HIGHREP                                                       
         CLC   KEY(RSTUKSTA-RSTAKEY),KEYSAVE  SAME UPTO/INCL UNIQUE ID?         
         BNE   AGRCLNO             UNKNOWN                                      
*                                                                               
         XC    DKEY,DKEY           WE'LL USE DKEY TO STORE A LIST               
         MVC   DKEY(5),RSTUKSTA    CURRENT CALL LETTERS ON REP SIDE             
*&&                                                                             
* READ REP ORDER INTO AIO1                                                      
AGRCL30  LA    R2,DKEY                                                          
AGRCL35  GOTO1 GETRPORD,DMCB,(X'41',(R2)),PARCROUT,PARCORDR                     
         BE    AGRCL50             REP DARE AGENCY HEADER RECORD EXISTS         
*                                                                               
         LA    R0,DKEY+6           ALREADY PROCESSED ALL THE OLD CALLS?         
         CR    R2,R0                                                            
         BE    AGRCL40             YES, LOOK UNDER CONFIRMED ORDERS             
*                                                                               
         AHI   R2,6                ADVANCE TO NEXT CALL LETTER                  
         CLC   =CL6' ',0(R2)       CHK IF OLD CALL LETTER STILL IN USE          
         BL    AGRCL35             YES THEY ARE, CHECK WITH THESE               
*                                                                               
AGRCL40  LA    R2,DKEY                                                          
AGRCL45  GOTO1 GETRPORD,DMCB,(X'51',(R2)),PARCROUT,PARCORDR                     
         BE    AGRCLCNF            REP STATUS OF CONFIRMED                      
*                                                                               
         LA    R0,DKEY+6           ALREADY PROCESSED ALL THE OLD CALLS?         
         CR    R2,R0                                                            
         BE    AGRCLUNK            YES, UNKNOWN STATUS                          
*                                                                               
         AHI   R2,6                                                             
         CLC   =CL6' ',0(R2)       CHK IF OLD CALL LETTER STILL IN USE          
         BL    AGRCL45             YES THEY ARE, CHECK WITH THESE               
         B     AGRCLUNK            NO, UNKNOWN STATUS                           
*                                                                               
AGRCL50  L     R6,AIO1             R6 = A(AGENCY HEADER ELEMENT)                
         LA    R6,RDARELEM-RDARRECD(R6)                                         
         CLI   0(R6),X'01'                                                      
         JNE   *+2                 X'01' BETTER EXIST                           
*                                                                               
         USING RDARELEM,R6                                                      
         MVC   REVISION,RDARRNUM               SAVE AGY REVISION NUMBER         
         MVC   QREPCON(L'RDARREP#),RDARREP#    SAVE BINARY REP CONTRACT         
         GOTO1 VHEXOUT,DMCB,QREPCON,PARCRPCN,L'RDARREP#                         
*                                                                               
         CLI   RDARBSTS,C'R'       REJECTED?                                    
         BE    AGRCLREJ            YES, DON'T MARK RECORDS, SEND ACKN           
*                                                                               
         MVI   BYTE,0              ALLOW RECALL EVEN IF APPROVED                
         CLI   RDARBSTS,C'A'       APPROVED?                                    
         BNE   AGRCL60                                                          
         MVI   BYTE,C'A'           YES, DON'T CHNG DARE RECORD SO FAST          
         B     AGRCL110                 CHECK DATES/TIMES                       
*                                                                               
AGRCL60  DS    0H                                                               
*                                                                               
* BUILD LIST OF PASSIVE KEYS FOR EXISTING DARE X'41' ORDER                      
*    ROLDKEYS:  KEY BUILD AREA FOR OLD AGENCY ORDER RECORD                      
*        AIO1:  CURRENT LOCATION OF OLD AGENCY ORDER RECORD                     
*      RKEYIO:  IO AREA USED BY PASSIVE KEY BLACK BOX                           
*                                                                               
         GOTO1 (RFGENDTR,AREPFACS),DMCB,(X'41',SRPARMSD.SRQACOMF),     X        
               ROLDKEYS,AIO1,RKEYIO                                             
*                                                                               
         MVI   RDARBSTS,C'C'       MARK RECORD AS RECALLED                      
         DROP  R6                                                               
*                                                                               
* SAVE DATE/TIME OF RECALL IN X'40' ELEMENT                                     
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING RDARRKEM,R4                                                      
         MVI   RDARRKCD,X'40'                                                   
         MVI   RDARRKLN,RDARRKLQ                                                
         GOTO1 VDATCON,DMCB,(5,0),(2,RDARRKDT)                                  
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,PACKOF4B                                                      
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
*                                                                               
         ICM   R1,15,PACKOF4B                                                   
         SRL   R1,12                                                            
         STCM  R1,3,RDARRKTM                                                    
         DROP  R4                                                               
*                                                                               
AGRCL65  LLC   RF,1(R6)            SEE IF ELEMENT EXISTS FIRST                  
         AR    R6,RF                                                            
*                                                                               
         CLI   0(R6),0             END OF RECORD?                               
         BE    AGRCL70             YES, ADD IT WHERE RE IS POINTING             
*                                                                               
         CLI   0(R6),X'40'                                                      
         BL    AGRCL65                                                          
         BH    AGRCL70             FOUND, DELETE IT                             
         GOTO1 VRECUP,DMCB,(C'R',AIO1),(R6)                                     
*                                                                               
AGRCL70  DS    0H                  ADD A NEW X'40' ELEMENT                      
         GOTO1 VRECUP,DMCB,(C'R',AIO1),ELEM,(R6)                                
*                                                                               
         MVC   AIO,AIO1   <=== KEEP REP DARE ORDER FOR AUDIT TRAIL              
         BRAS  RE,PUTREP                                                        
*                                                                               
         MVC   RKEYDA,KEY+28       SAVE OFF D/A FOR PASSIVE KEYS                
*                                                                               
* BUILD LIST OF PASSIVE KEYS FOR NEW DARE X'41' ORDER                           
*    RNEWKEYS:  KEY BUILD AREA FOR CHANGED AGENCY ORDER RECORD                  
*        AIO1:  CURRENT LOCATION OF CHANGED AGENCY ORDER RECORD                 
*      RKEYIO:  IO AREA USED BY PASSIVE KEY BLACK BOX                           
*                                                                               
         GOTO1 (RFGENDTR,AREPFACS),DMCB,(X'41',SRPARMSD.SRQACOMF),     X        
               RNEWKEYS,AIO1,RKEYIO                                             
*                                                                               
* COMPARE 2 LISTS OF KEYS AND UPDATE ANY KEYS THAT ARE DIFFERENT                
*    ROLDKEYS:  KEY BUILD AREA FOR ORIGINAL RECORD                              
*    RNEWKEYS:  KEY BUILD AREA FOR CHANGED RECORD                               
*      RKEYDA:  DISK ADDRESS OF AGENCY ORDER RECORD                             
*                                                                               
         GOTO1 (RFGENDTR,AREPFACS),DMCB,(X'02',SRPARMSD.SRQACOMF),     X        
               ROLDKEYS,RNEWKEYS,RKEYDA                                         
*                                                                               
         CLI   PARCQSTA+4,C'A'     AM OR FM?                                    
         BE    *+12                                                             
         CLI   PARCQSTA+4,C'F'     AM OR FM?                                    
         BNE   AGRCL90                                                          
         OI    MISCFLG3,MF3RADIO                                                
******   CLC   PARCQSTA+4(2),=C'00'   STATION UNIQUE ID?                        
******   BL    AGRCL90                NO, NO NEED FOR SALESPERSON               
*                                                                               
         XC    DKEY+32(32),DKEY+32  SALESPERSON CODE AND FLAG                   
         L     R6,AIO              R6 = A(SALESPERSON ELEM)                     
         LA    R6,RDARELEM-RDARRECD(R6)                                         
         XR    R0,R0                                                            
AGRCL72  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    AGRCL90                                                          
*                                                                               
         CLI   0(R6),X'0A'                                                      
         BNE   AGRCL72                                                          
         USING RDARPPEL,R6                                                      
         MVC   DKEY+32(3),RDARPPSP  DKEY+35(X'01) IS UNWIRED FLAG               
         DROP  R6                                                               
*                                                                               
AGRCL74  IC    R0,1(R6)            R6 = A(MISC FLAGS ELEMENT)                   
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    AGRCL80                                                          
*                                                                               
AGRCL76  CLI   0(R6),X'0F'                                                      
         BNE   AGRCL74             X'01' BETTER EXIST                           
         USING RDARFLEM,R6                                                      
         TM    RDARFLG1,X'01'      UNWIRED?                                     
         BZ    *+8                                                              
         OI    DKEY+35,X'01'                                                    
         DROP  R6                                                               
*                                                                               
AGRCL80  XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING GSPLKEY,R6                                                       
         MVI   GSPLKTYP,GSPLRECQ   X'71'                                        
         MVC   GSPLKREP,POWERCDE                                                
*                                                                               
         BRAS  RE,HIGHGD                                                        
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
*                                                                               
         MVC   HALF,POWERCDE       SAVE REP ALPHA TO USE HERE                   
         CLC   KEYSAVE(GSPLMREP-GSPLKEY),KEY   REP HAS A MASTER?                
         BNE   *+10                            NO                               
         MVC   HALF,GSPLMREP       USE MASTER REP'S POWER CODE INSTEAD          
***********************************************************************         
* FIND THE SALESPERSON CODE                                                     
***********************************************************************         
         XC    KEY,KEY                                                          
         USING GSPLKEY,R6                                                       
         MVI   GSPLPTYP,GSPLRECQ   X'71' - ACTIVE SALESPERSON CODE              
*                                                                               
         MVI   GSPLPSTP,X'01'      ASSUME SALESPERSON FIRST                     
         TM    DKEY+35,X'01'       IS IT UNWIRED?                               
         BZ    *+8                                                              
         MVI   GSPLPSTP,X'02'      YES, GET POINTPERSON THEN                    
*                                                                               
         MVC   GSPLKREP,HALF                                                    
         MVC   GSPLKSAL,DKEY+32    SALESPERSON CODE IN X'0A' ELEM               
*                                                                               
         BRAS  RE,HIGHGD                                                        
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
*                                                                               
         CLC   KEYSAVE(L'GSPLKEY),KEY   MATCHES MY SALESPERSON?                 
         BE    *+14                     YES                                     
         XC    DKEY+32(32),DKEY+32   CLEAR SALESPERSON CODE AND FLAG            
         B     AGRCL90               SO WE KNOW NOT TO PUT IT OUT               
*                                                                               
         MVC   AIO,AIO2                                                         
         BRAS  RE,GETGFL                                                        
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
*                                                                               
         L     R6,AIO2                                                          
         MVC   DKEY+40(L'GSPLSPNM),GSPLSPNM  SAVE SALESPERSON CODE              
         DROP  R6                                                               
***********************************************************************         
* RECORD AUDIT TRAIL FOR REP DARE ORDER                                         
***********************************************************************         
AGRCL90  GOTO1 =A(AUDTRAIL),DMCB,('DHRECALQ',0),RR=RELO                         
*                                                                               
         CLI   BYTE,C'A'           ALL DONE IF APPROVE OR TRANSMITTED           
         BE    AGRCLUPD                                                         
         CLI   BYTE,C'T'                    AT THIS POINT                       
         BE    AGRCLUPD                                                         
*                                                                               
         OC    QREPCON(L'RDARREP#),QREPCON   ANY REP CONTRACT NUMBER?           
         BZ    AGRCLYES                      NONE, SEND RECALL ACKN (Y)         
*                                                                               
*                                        READ REP CONTRACT INTO AIO1            
AGRCL110 GOTO1 =A(GETRPCON),DMCB,PARCRPCN,RR=RELO                               
         BNE   AGRCLYES                                                         
*                                                                               
         SR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,RCONELEM-RCONREC(R6)                                          
AGRCL130 CLI   0(R6),0             DARE AGENCY ORDER ELEMENT EXISTS?            
         BE    AGRCLYES                                                         
*                                                                               
         CLI   0(R6),X'1D'                                                      
         BE    AGRCL140                                                         
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     AGRCL130                                                         
*                                                                               
         USING RCONDREL,R6                                                      
AGRCL140 DS    0H                  TURN OFF APPROVED OR REJECTED BITS           
         NI    RCONDRFG,X'FF'-X'40'-X'20'                                       
         OI    RCONDRFG,X'10'       TURN ON RECALLED BIT                        
*                                                                               
         MVC   AIO,AIO1                                                         
         BRAS  RE,PUTREP                                                        
*                                                                               
         CLI   BYTE,C'A'           RECALLING ON AN APPROVED ORDER?              
         BNE   AGRCLYES            NO, CAN'T BE SENT TO STATION THEN            
         MVC   WORK(L'RCONDRDA+L'RCONDRTA),RCONDRDA  SAVE DATE/TIME APP         
         BAS   RE,CHKTRANS                                                      
         BNE   AGRCLTRN            REP SENT ORDER TO STATION                    
         B     AGRCLYES            PLAIN OLD APPROVED                           
*                                                                               
AGRCLCNF MVI   BYTE,C'C'           ORDER HAS BEEN CONFIRMED                     
         B     AGRCLSND                                                         
*                                                                               
AGRCLUNK MVI   BYTE,C'U'           UNKNOWN STATUS (REP NEVER GOT ORDER)         
         B     AGRCLSND                                                         
*                                                                               
AGRCLREJ MVI   BYTE,C'R'           ORDER HAS BEEN REJECTED                      
         B     AGRCLSND                                                         
*                                                                               
AGRCLTRN MVI   BYTE,C'T'           ORDER HAS BEEN TRANSMITTED                   
         B     AGRCLSND                                                         
*                                                                               
AGRCLYES CLI   BYTE,C'A'           ORDER HAS BEEN APPROVED                      
         BE    AGRCLSND                                                         
         MVI   BYTE,C'D'           ORDER WAS DELIVERED TO REP                   
********                                                                        
         CLI   REVISION,0          RECALLING A REVISION?                        
         BE    AGRCLSND            NO, ORIGINAL ORDER                           
*                                                                               
         L     R6,AIO1             YES, SEE IF WE HAVE WIP                      
         LA    R6,RCONELEM-RCONREC(R6)                                          
         MVI   ELCODE,X'20'        FIND REP VERSION NUMBER                      
         BAS   RE,FIRSTEL                                                       
         USING RCONSEND,R6                                                      
         MVC   HALF(1),RCONSRV     REP VERSION NUMBER                           
         MVI   ELCODE,X'22'        FIND MOST RECENT MOD CONFIRMED AT V#         
         BAS   RE,FIRSTEL                                                       
         USING RMODELEM,R6                                                      
         CLC   HALF(1),RMODEL1V    ARE THESE 2 NUMBERS THE SAME?                
         BE    AGRCLSND            YES, NOT IN WIP                              
         MVI   BYTE,C'W'           WE HAVE A WIP!                               
         DROP  R6                                                               
********                                                                        
AGRCLSND GOTO1 SNDRACKN,DMCB,(BYTE,0)                                           
*                                                                               
         CLI   BYTE,C'T'           APPROVED OR XMITTED TO STATION?              
         BE    *+12                                                             
         CLI   BYTE,C'A'                                                        
         BNE   AGRCLUPD            NO, WE UPDATED DARE RECORD ALREADY           
*                                                                               
         LA    R2,DKEY                                                          
AGRCL150 GOTO1 GETRPORD,DMCB,(X'41',(R2)),PARCROUT,PARCORDR                     
         BE    AGRCL155                                                         
*                                                                               
         LA    R0,DKEY+12          ALREADY PROCESSED ALL THE OLD CALLS?         
         CR    R2,R0                                                            
         BE    AGRCL155            YES, LOOK UNDER CONFIRMED ORDERS             
*                                                                               
         AHI   R2,6                ADVANCE TO NEXT CALL LETTER                  
         CLC   =CL6' ',0(R2)       CHK IF OLD CALL LETTER STILL IN USE          
         BL    AGRCL150            YES THEY ARE, CHECK WITH THESE               
         DC    H'0'                PREVIOUSLY CHECKED, SHOULDN'T HAPPEN         
*                                                                               
AGRCL155 DS    0H                                                               
         L     R6,AIO1             R6 = A(AGENCY HEADER ELEMENT)                
         LA    R6,RDARELEM-RDARRECD(R6)                                         
         CLI   0(R6),X'01'                                                      
         BE    AGRCL60             X'01' BETTER EXIST, CHANGE TO RECALL         
         DC    H'0'                                                             
*                                                                               
AGRCLUPD BAS   RE,UPDATSB1         UDPATE SSB                                   
         B     YES                 RETURN WITH NO PROBLEMS                      
*                                                                               
AGRCLNO  B     NO                                                               
         DROP  R3,R7                                                            
         EJECT                                                                  
***********************************************************************         
* CHECKS TO SEE IF CONTRACT HAS BEEN TRANSMITTED TO STATION AFTER               
* THE APPROVAL                                                                  
*                                                                               
* ON ENTRY:    WORK    BYTES 0-1   DATE APPROVED                                
*                      BYTES 2-3   TIME APPROVED                                
*                                                                               
* ON EXIT:     (CC)                EQ - BEFORE THE APPROVAL                     
*                                  NE - AFTER THE APPROVAL                      
***********************************************************************         
CHKTRANS NTR1                                                                   
         L     R6,AIO1                                                          
         LA    R6,RCONELEM-RCONREC(R6)                                          
*                                                                               
CKTRN10  CLI   0(R6),0             ELEMENT FOR SEND EXISTS?                     
         BE    CKTRNYES            NO, SIMPLE APPROVE                           
*                                                                               
         CLI   0(R6),X'20'                                                      
         BE    CKTRN20                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CKTRN10                                                          
*                                                                               
         USING RCONSEND,R6                                                      
CKTRN20  CLC   WORK(L'RCONSRDT),RCONSRDT                                        
         BL    CKTRNNO                                                          
         BH    CKTRNYES                                                         
         GOTO1 VHEXOUT,DMCB,WORK+L'RCONDRDA,WORK+16,L'RCONDRDA                  
         CLC   WORK+16(L'RCONDRDA*2),RCONSRTI                                   
         BNH   CKTRNNO                                                          
*                                                                               
CKTRNYES B     YES                                                              
*                                                                               
CKTRNNO  B     NO                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* SENDS A RECALL ACKNOWLEDGEMENT                                                
*                                                                               
* ON ENTRY:    R7                  A(EDICT RECORD)                              
*              PARAM 1  BYTE 0     CODE TO SEND BACK TO AGENCY                  
*                                                                               
* NOTE: BYTE GETS CLOBBERED                                                     
***********************************************************************         
SNDRACKN NTR1                                                                   
         MVC   BYTE,0(R1)                                                       
*                                                                               
         L     RE,ASPLAREA                                                      
         LA    RF,L'SPULAREA                                                    
         XCEFL                                                                  
*                                                                               
         L     R8,ASPLAREA                                                      
         USING SPOOLD,R8                                                        
         MVC   SPOOLID,=C'DAR'                                                  
         MVI   USERLANG,0          ENGLISH                                      
         MVC   SPOOLDM,VDATAMGR                                                 
         MVC   RCDATCON,VDATCON                                                 
         MVC   RCCOMFAC,SRPARMSD.SRQACOMF                                       
         MVC   SPOOLBUF,SRPARMSD.SRQATIA                                        
*                                                                               
         LA    R2,SPOOLKEY                                                      
         USING PQPLD,R2                                                         
         XC    SPOOLKEY,SPOOLKEY                                                
         MVC   PLSUBID,=C'DAR'                                                  
         MVC   PLUSER,=X'0011'    **** SJR FOR NOW                              
         MVC   PLDESC,=CL11'ORDRCL'                                             
         MVI   PLCLASS,C'G'                                                     
         OI    SPOOLIND,SPUINIT    ALLOWS ME TO SET THE CLASS                   
*                                                                               
         XC    P1,P1               OPEN PRINTQ ENTRY                            
         BAS   RE,SNDAPRNT                                                      
         B     SNDA10                                                           
*                                                                               
SNDAPRNT LR    R0,RE                                                            
         GOTO1 ASPOOL,DMCB,(R8)                                                 
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
SNDA10   MVC   SPOOLRPN,PLREPNO                                                 
         MVI   PLCC,0                                                           
         DROP  R2                                                               
***************                                                                 
* PUT OUT EDICT HEADER RECORD                                                   
***************                                                                 
         MVC   P1+4(5),=C'*HDR*'    HEADER RECORD                               
         MVC   P1+9(14),=CL14'EDICT=*DDSDARA'                                   
         MVI   P1+34,C'W'           WIDE REPORT                                 
         MVI   P1+35,C'P'           /PAGE FOR EASYLINK AND SUPRESS TOP          
*                                                                               
         MVC   P2(14),=CL14'++DDS DAACKTRN'                                     
         BAS   RE,SNDAPRNT                                                      
*                                                                               
         LA    R2,P                                                             
         USING RORDRCLD,R2                                                      
         MVC   RORCTID,=C'ORDRCL'  RECALL ACKNOWLEDGEMENT                       
         MVC   RORCORDR,6(R7)      ORDER # IS ALWAYS 6 BYTES FROM BEG.          
         MVC   RORCFRID,PARCTOID-PAGYRCLD(R7)   SWAP TO/FROM                    
         MVC   RORCTOID,PARCFRID-PAGYRCLD(R7)                                   
*                                                                               
**** SPECIAL CODE FOR MEDIA OCEAN CUTOFF DATE *** HWON 11/12/02                 
*&&DO                                                                           
         GOTOR CHKMOREP,DMCB,RORCTOID,RORCORDR                                  
*&&                                                                             
**** SPECIAL CODE FOR MEDIA OCEAN CUTOFF DATE *** HWON 11/12/02                 
*                                                                               
         NI    MISCFLG3,X'FF'-MF3RADIO                                          
         MVC   RORCQSTA,PARCQSTA-PAGYRCLD(R7)                                   
*                                                                               
         CLI   RORCQSTA+4,C'A'     AM OR FM?                                    
         BE    *+12                                                             
         CLI   RORCQSTA+4,C'F'                                                  
         BNE   *+8                                                              
*****    CLC   RORCQSTA+4(2),=C'00'                                             
*****    BL    *+8                                                              
         OI    MISCFLG3,MF3RADIO   RADIO ORDER, NEEDS A ORDTLR                  
*                                                                               
         MVC   RORCRPCN,PARCRPCN-PAGYRCLD(R7)                                   
         MVC   RORCRTRN,PARCRTRN-PAGYRCLD(R7)                                   
         MVC   RORCACCP,BYTE                                                    
         GOTO1 VDATCON,DMCB,(5,0),(X'20',RORCDATE)                              
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,PACKOF4B                                                      
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
*                                                                               
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         BL    SNDA20                                                           
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 VADDAY,DMCB,RORCDATE,(X'20',RORCDATE),F'1'                       
*                                                                               
SNDA20   ICM   R1,15,PACKOF4B                                                   
         SRL   R1,12               GET RID OF SECONDS AND SIGN                  
         STH   R1,HALF                                                          
         GOTO1 VHEXOUT,DMCB,HALF,RORCTIME,L'HALF                                
*                                                                               
         BAS   RE,SNDAPRNT                                                      
*                                                                               
         TM    MISCFLG3,MF3RADIO                                                
         BZ    SNDA30                                                           
         NI    MISCFLG3,X'FF'-MF3RADIO                                          
*                                                                               
         OC    DKEY+32(3),DKEY+32  ANY SALESPERSON?                             
         BZ    SNDA25              NO                                           
         LA    R2,P                                                             
         USING RORDSALD,R2                                                      
         MVC   ROSPTID,=CL6'ORDSAL'                                             
         MVC   ROSPORDR,6(R7)      ORDER # IS ALWAYS 6 BYTES FROM BEG.          
         MVC   ROSPSALP,DKEY+32     SALESPERSON CODE                            
         MVC   ROSPSALN,DKEY+40     SALESPERSON NAME FROM BEFORE                
         BAS   RE,SNDAPRNT                                                      
*                                                                               
SNDA25   LA    R2,P                                                             
         USING RORDTLRD,R2                                                      
         MVC   ROTRTID,=CL6'ORDTLR'                                             
         MVC   ROTRORDR,6(R7)      ORDER # IS ALWAYS 6 BYTES FROM BEG.          
         MVC   ROTRRCCT,=C'000002'  RECORD COUNT FOR ENCAPSULATED MSG           
         OC    DKEY+32(3),DKEY+32  ANY SALESPERSON?                             
         BZ    *+10                                                             
         MVC   ROTRRCCT,=C'000003'                                              
         BAS   RE,SNDAPRNT                                                      
*                                                                               
SNDA30   MVC   P(26),=CL26'*** END OF DDS MESSAGE ***'                          
         BAS   RE,SNDAPRNT                                                      
*                                                                               
         MVI   SPMODE,X'FF'        YES, CLOSE PRINTQ ENTRY                      
         GOTO1 ASPOOL,DMCB,(R8)                                                 
*                                                                               
SNDAX    B     XIT                                                              
         DROP  R2,R8                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE ORDER RECALL ACKNOWLEDGEMENT                                    
***********************************************************************         
RCLAKNWL NTR1  BASE=*,LABEL=*                                                   
         L     R9,SRPARMSD.SRQASYSF                                             
*                                                                               
         USING RORDRCLD,R7                                                      
RCLAKD   USING RTN2SNDR,RORCRTRN                                                
*                                                                               
         MVI   QMED,C'T'                                                        
         CLI   RCLAKD.RTNAGYMD+1,C'2' AM I RADIO?                               
         BNE   *+8                    NO                                        
         MVI   QMED,C'R'                                                        
*                                                                               
         GOTO1 VHEXIN,DMCB,RCLAKD.RTNAGYMD,BAGYMD,2                             
         MVC   AGENCY,RCLAKD.RTNPWRCD                                           
         GOTO1 VHEXIN,DMCB,RCLAKD.RTNSYSID,BYTE,2                               
         BAS   RE,SPANKED                                                       
*                                                                               
         NI    MISCFLG1,X'FF'-MF1XMTUP                                          
*                                                                               
         CLC   BYTE,THESYSID       WRONG FACPAK?                                
         BNE   RCLAKNO             YES, AGENCY DOESN'T BELONG HERE              
*                                                                               
         DROP  RCLAKD                                                           
*                                                                               
         MVC   QREPCON,RORCRPCN    COPY THESE VALUES                            
         MVC   QRETURN,RORCRTRN                                                 
*                                                                               
         GOTO1 CALCORDR,DMCB,RORCORDR   CONVERT ORDER NUMBER TO BINARY          
         BNE   RCLAKNO                                                          
*                                                                               
         MVC   USERID,RORCTOID                                                  
         BAS   RE,SWTCHCTL         SWITCH CONTROL TO SPOT SYSTEM                
         BNE   RCLAKNO                                                          
*                                                                               
         BRAS  RE,GETORDER         READ ORDER RECORD INTO AIO1                  
         BNE   RCLAKNO                                                          
*                                                                               
         NI    BITFLAG2,X'FF'-BF2SPNDG                                          
         NI    MISCFLG1,X'FF'-MF1NOXMT   ASSUME XMT ELEM EXISTS                 
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
         LA    R2,DOIDCON          DON'T OVERWRITE CONTRACT IF REVISION         
         DROP  R6                                                               
*                                                                               
         SR    R0,R0                                                            
         MVI   REVISION,0                                                       
RCLAK10  CLI   0(R6),0                                                          
         BNE   RCLAK12                                                          
         LHI   R1,REFORDNT          ORD NOT TRANSMITTED                         
         BRAS  RE,SNDERROR                                                      
         B     RCLAKNO                                                          
*                                                                               
         USING DOSPELD,R6                                                       
RCLAK12  CLI   0(R6),DOSPELQ                                                    
         BNE   *+10                                                             
         MVC   REVISION,DOSPREVN   SAVE REVISION NUMBER                         
*                                                                               
         CLI   0(R6),DOXMTELQ                                                   
         BE    RCLAK16                                                          
RCLAK14  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RCLAK10                                                          
*                                                                               
         USING DOXMTELD,R6                                                      
RCLAK16  CLI   DOXMTSTA,QSNTPNDG   SEND PENDING?                                
         BNE   RCLAK18                                                          
         OI    BITFLAG2,BF2SPNDG   YES, THIS XMT ELEM HAS NO DEST ID            
         ST    R6,FULL     <====== DON'T CLOBBER THIS!!                         
         B     RCLAK14             GET THE NEXT XMT ELEM                        
*                                                                               
RCLAK18  CLI   DOXMTSTA,QSNTXCNF   SENT CANCELLED, PARTIAL CONFIRM?             
         BE    RCLAKNO                                                          
         CLI   DOXMTSTA,QSNTXREJ     OR   SEND CANCELLED, REJECTED?             
         BE    RCLAKNO                                                          
         CLI   DOXMTSTA,QTOBESNT     OR   TO BE SENT VIA SCRIPT?                
         BE    RCLAKNO             YES, LET THEM RESEND THE ORDER               
*                                                                               
         OC    DOXMTDID,DOXMTDID   PROBLEM IF NO DEST ID                        
         BNZ   RCLAK20                                                          
         IC    R0,1(R6)            CHECK FOR PREVIOUS XMT                       
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),DOXMTELQ      IS IT A XMT ELEM?                            
         BNE   RCLAK19                                                          
         OC    DOXMTDID,DOXMTDID   WAS THERE A DESTID?                          
         BNZ   RCLAKNO             YES: DO NOTHING.                             
         CLI   DOXMTSTA,QTOBESNT     OR   TO BE SENT VIA SCRIPT?                
         BE    RCLAKNO             YES, LET THEM RESEND THE ORDER               
RCLAK19  LHI   R1,*-T16100                                                      
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
RCLAK20  GOTO1 VDATCON,DMCB,(0,RORCDATE),(19,WORK)                              
         GOTO1 VHEXIN,DMCB,RORCTIME,WORK+L'DOXMTSTD,L'RORCTIME                  
         CLI   REVISION,0          ARE WE IN REVISION?                          
         BNE   *+10                YES                                          
         MVC   0(L'DOIDCON,R2),RORCRPCN    SAVE THE REP CONTRACT NUMBER         
*                                                                               
         OC    DOXMTSTD,DOXMTSTD   IF NO DATE OR TIME ALREADY                   
         BZ    RCLAK40                                                          
         OC    DOXMTSTT,DOXMTSTT                                                
         BZ    RCLAK30             THEN USE EDICT RECORD'S DATE/TIME            
*                                                                               
         CLC   DOXMTSTD,WORK              IS ELEM'S DATE MORE RECENT?           
         BH    RCLAKNO                    YES, IGNORE THIS RECORD               
         BL    RCLAK30                    NO, OLDER                             
         CLC   DOXMTSTT,WORK+L'DOXMTSTD    IS ELEM'S TIME MORE RECENT?          
         BNH   RCLAK30                    NO, CONTINUE PROCESSING               
         GOTO1 COMPTIME,DMCB,DOXMTSTD,WORK                                      
         BNE   RCLAKNO                 YES, NOT W/IN 60 MIN, IGNORE REC         
*                                                                               
RCLAK30  CLI   DOXMTSTA,QRECALL    AGENCY ORDER IN RECALL STATUS?               
         BE    RCLAK40                                                          
         CLI   DOXMTSTA,QCFMD      IF CONFIRMED                                 
         BE    RCLAKNO             THEN DON'T MARK FOR RECALL ACK.              
         CLI   DOXMTSTA,QRJCT      IF REJECTED                                  
         BE    RCLAKNO             THEN DON'T MARK FOR RECALL ACK.              
         CLI   DOXMTSTA,QUNDARE    IF UNDARED                                   
         BE    RCLAKNO             THEN DON'T MARK FOR RECALL ACK.              
         LHI   R1,REFCCHNG                                                      
         BRAS  RE,SNDERROR                                                      
         B     RCLAKNO                                                          
*                                                                               
RCLAK40  DS    0H                                                               
         CLI   RORCACCP,C'A'       RECALL, REP STATUS APPROVED?                 
         BNE   *+12                                                             
         MVI   DOXMTSTA,QRCLAPPR   YES                                          
         B     RCLAK50                                                          
*                                                                               
         CLI   RORCACCP,C'D'       RECALL, REP STATUS DELIVERED?                
         BNE   *+12                                                             
         MVI   DOXMTSTA,QRCLDELN   YES                                          
         B     RCLAK50                                                          
*                                                                               
         CLI   RORCACCP,C'T'       RECALL, REP STATUS TRANSMITTED?              
         BNE   *+12                                                             
         MVI   DOXMTSTA,QRCLTRNS   YES                                          
         B     RCLAK50                                                          
*                                                                               
         CLI   RORCACCP,C'W'       RECALL, REP STATUS WIP?                      
         BNE   *+12                                                             
         MVI   DOXMTSTA,QRCLWIP    YES                                          
         B     RCLAK50                                                          
*                                                                               
         CLI   RORCACCP,C'C'       RECALL, REP STATUS CONFIRMED?                
         BNE   *+12                                                             
         MVI   DOXMTSTA,QRCLCONF   YES                                          
         B     RCLAK53                                                          
*                                                                               
         CLI   RORCACCP,C'R'       RECALL, REP STATUS REJECTED?                 
         BNE   *+12                                                             
         MVI   DOXMTSTA,QRCLREJD   YES                                          
         B     RCLAK56                                                          
*                                                                               
         MVI   DOXMTSTA,QRCLUNKN   RECALL, REP STATUS UNKNOWN                   
*                                                                               
RCLAK50  MVC   SVSTAT,DOXMTSTA     SAVE FOR LATER                               
         TM    BITFLAG2,BF2SPNDG   WE HAD A SENT PENDING?                       
         BZ    RCLAK99                                                          
         L     R6,FULL       <==== THIS BETTER NOT GET CLOBBERED!!              
         MVI   DOXMTSTA,QTOBESNT                                                
         B     RCLAK59                                                          
*                                                                               
RCLAK53  MVC   SVSTAT,DOXMTSTA     SAVE FOR LATER                               
         TM    BITFLAG2,BF2SPNDG   WE HAD A SENT PENDING?                       
         BZ    RCLAK99                                                          
****     L     R6,FULL       <==== COMMENTED SO CONFIRM CAN SET STATUS          
****     MVI   DOXMTSTA,QSNTXCNF                                                
         NI    BITFLAG2,X'FF'-BF2SPNDG   NOT "TO BE SENT"                       
         B     RCLAK59                                                          
*                                                                               
RCLAK56  MVC   SVSTAT,DOXMTSTA     SAVE FOR LATER                               
         TM    BITFLAG2,BF2SPNDG   WE HAD A SENT PENDING?                       
         BZ    RCLAK99                                                          
         L     R6,FULL       <==== THIS BETTER NOT GET CLOBBERED!!              
         MVI   DOXMTSTA,QSNTXREJ                                                
         NI    BITFLAG2,X'FF'-BF2SPNDG   NOT "TO BE SENT"                       
*                                                                               
RCLAK59  MVC   DOXMTSTD,WORK                                                    
         MVC   DOXMTSTT,WORK+L'DOXMTSTD                                         
         DROP  R6                                                               
*                                                                               
RCLAK99  OI    MISCFLG1,MF1XMTUP                                                
*                                                                               
RCLAK100 NI    BITFLAG3,X'FF'-BF3SPNDG    NOT A SENT PENDING ORDER              
         NI    MISCFLG2,X'FF'-MF2DIDNM    NO DESTID..                           
         B     RCLAK103                                                         
*                                                                               
RCLAK102 OI    MISCFLG2,MF2DIDNM   FOUND A DESTID                               
RCLAK103 SR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
RCLAK110 CLI   0(R6),0                                                          
         BNE   RCLAK115                                                         
         TM    MISCFLG1,MF1NOXMT   DOES IT HAVE XMT ELEMS?                      
         BZ    RCLAKNO             YES, THEN DON'T GIVE ERROR                   
         LHI   R1,REFORDNT         NEVER TRANSMITTED                            
         BRAS  RE,SNDERROR                                                      
         B     RCLAKNO                                                          
*                                                                               
RCLAK115 CLI   0(R6),DOSTELQ                                                    
         BE    RCLAK120                                                         
RCLAK116 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RCLAK110                                                         
*                                                                               
         USING DOSTELD,R6                                                       
RCLAK120 TM    MISCFLG2,MF2DIDNM   DESTID FOUND?                                
         BNZ   RCLAK130            YES, SKIP THIS CHECK                         
         CLI   DOSTSTAT,DDLVRD     NO, DELIVERY NOTICE?                         
         BNE   RCLAK116            - NO, CHECK NEXT                             
         OC    DOSTIDNM,DOSTIDNM   - YES, DEST ID???                            
         BNZ   RCLAK102                - YES, WE FOUND IT!!                     
         LHI   R1,*-T16100             - NO, SEND ERROR!!                       
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
RCLAK130 CLI   DOSTSTAT,DDLVRD     SKIP DELIVERY NOTICE                         
         BE    RCLAK116                                                         
         CLI   DOSTSTAT,QSNTPNDG   SEND PENDING?                                
         BNE   RCLAK135                                                         
         OI    BITFLAG3,BF3SPNDG                                                
         B     RCLAK116                                                         
*                                                                               
RCLAK135 CLI   DOSTSTAT,QSNTXCNF   SEND CANCELLED, PARTIAL CONFIRM?             
         BE    RCLAKNO                                                          
         CLI   DOSTSTAT,QSNTXREJ     OR SEND CANCELLED, REJECTED?               
         BE    RCLAKNO                                                          
         CLI   DOSTSTAT,QTOBESNT     OR TO BE SENT VIA SCRIPT?                  
         BE    RCLAKNO                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(0,RORCDATE),(19,WORK)                              
         GOTO1 VHEXIN,DMCB,RORCTIME,WORK+L'DOSTDATE,L'RORCTIME                  
         CLI   REVISION,0          ARE WE IN A REVISION?                        
         BNE   *+10                                                             
         MVC   0(L'DOIDCON,R2),RORCRPCN  SAVE THE REP CONTRACT NUMBER           
*                                                                               
         OC    DOSTTIME,DOSTTIME                                                
         BZ    RCLAK140                                                         
*                                                                               
         CLC   DOSTDATE,WORK              IS ELEM'S DATE MORE RECENT?           
         BH    RCLAKNO                    YES, IGNORE THIS RECORD               
         BL    RCLAK140                   NO, OLDER                             
         CLC   DOSTTIME,WORK+L'DOSTDATE    IS ELEM'S TIME MORE RECENT?          
         BNH   RCLAK140                   NO, CONTINUE PROCESSING               
         GOTO1 COMPTIME,DMCB,DOSTDATE,WORK                                      
         BNE   RCLAKNO                 YES, NOT W/IN 60 MIN, IGNORE REC         
*                                                                               
RCLAK140 CLI   DOSTSTAT,QRECALL                                                 
         BE    RCLAK145                                                         
         CLI   DOSTSTAT,DSENT      DON'T BOTHER, ALREADY RESENT                 
         BE    RCLAKNO                                                          
         CLI   DOSTSTAT,QCFMD                                                   
         BE    RCLAKNO                                                          
         CLI   DOSTSTAT,QRJCT                                                   
         BE    RCLAKNO                                                          
         CLI   DOSTSTAT,QUNDARE                                                 
         BE    RCLAKNO                                                          
*                                                                               
         TM    MISCFLG1,MF1NOXMT          DOES IT HAVE XMT ELEMS?               
         BZ    RCLAKNO                    YES, THEN DON'T GIVE ERROR            
         LHI   R1,REFCCHNG                                                      
         BRAS  RE,SNDERROR                                                      
         B     RCLAKNO                                                          
         DROP  R6                                                               
*                                                                               
RCLAK145 DS    0H                                                               
         LA    R2,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
         USING DOSTELD,R2                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   DOSTEL,DOSTELQ                                                   
         MVI   DOSTLEN,DOSTLNQ                                                  
         MVC   DOSTDATE,WORK                                                    
         MVC   DOSTTIME,WORK+L'DOSTDATE                                         
*                                                                               
         CLI   RORCACCP,C'A'       RECALL, REP STATUS APPROVED?                 
         BNE   *+12                                                             
         MVI   DOSTSTAT,QRCLAPPR   YES                                          
         B     RCLAK150                                                         
*                                                                               
         CLI   RORCACCP,C'D'       RECALL, REP STATUS DELIVERED?                
         BNE   *+12                                                             
         MVI   DOSTSTAT,QRCLDELN   YES                                          
         B     RCLAK150                                                         
*                                                                               
         CLI   RORCACCP,C'T'       RECALL, REP STATUS TRANSMITTED?              
         BNE   *+12                                                             
         MVI   DOSTSTAT,QRCLTRNS   YES                                          
         B     RCLAK150                                                         
*                                                                               
         CLI   RORCACCP,C'W'       RECALL, REP STATUS WIP?                      
         BNE   *+12                                                             
         MVI   DOSTSTAT,QRCLWIP    YES                                          
         B     RCLAK150                                                         
*                                                                               
         CLI   RORCACCP,C'C'       RECALL, REP STATUS CONFIRMED?                
         BNE   *+12                                                             
         MVI   DOSTSTAT,QRCLCONF   YES                                          
         B     RCLAK150                                                         
*                                                                               
         CLI   RORCACCP,C'R'       RECALL, REP STATUS REJECTED?                 
         BNE   *+12                                                             
         MVI   DOSTSTAT,QRCLREJD   YES                                          
         B     RCLAK150                                                         
*                                                                               
         MVI   DOSTSTAT,QRCLUNKN   RECALL, REP STATUS UNKNOWN                   
*                                                                               
RCLAK150 L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)  R6 = A(FIRST ELEMENT IN RECORD)            
RCLAK155 CLI   0(R6),0          ADD AFTER ALL TRANSMISSION ELEMENTS             
         BE    RCLAK160         AND BEFORE AN AUDIT ELEMENT                     
         CLI   0(R6),DORPELQ2                                                   
         BE    RCLAK160                                                         
         CLI   0(R6),DOSTELQ                                                    
         BNL   RCLAK160                                                         
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RCLAK155                                                         
*                                                                               
RCLAK160 GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
         MVC   SVSTAT,DOSTSTAT                                                  
         MVC   DOSAELEM(DOSTLNQ2),DOSTELEM                                      
*                                                                               
         TM    BITFLAG3,BF3SPNDG                                                
         BZ    RCLAK300                                                         
*                                                                               
         CLI   SVSTAT,QRCLCONF     RECALL, REP STATUS CONFIRMED?                
         BNE   RCLAK165            NO                                           
         NI    BITFLAG3,X'FF'-BF3SPNDG  YES: NOT "TO BE SENT"                   
         B     RCLAK300                                                         
*                                                                               
RCLAK165 MVI   DOSTSTAT,QTOBESNT                                                
         CLI   SVSTAT,QRCLREJD                                                  
         BNE   RCLAK170                                                         
         MVI   DOSTSTAT,QSNTXREJ                                                
         NI    BITFLAG3,X'FF'-BF3SPNDG  YES: NOT "TO BE SENT"                   
         DROP  R2                                                               
*                                                                               
RCLAK170 GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
*                                                                               
RCLAK200 TM    MISCFLG3,MF3RADIO   ARE WE RADIO?                                
         BZ    RCLAK300                                                         
*                                                                               
         LLC   R2,0(R3)            BUMP TOP NEXT RECORD TYPE                    
         AR    R3,R2                                                            
         CLC   =C'ORDSAL',1(R3)    SALESPERSON REASSIGNMENT?                    
         BNE   RCLAK300            NONE, JUST EXIT                              
         LA    R4,1(R3)            YES, WE HAVE ONE                             
         USING RORDSALD,R4                                                      
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6) X'01'                                       
         USING DOIDELD,R6                                                       
         XC    DOIDSPER,DOIDSPER                                                
         MVC   DOIDSPER(L'ROSPSALN),ROSPSALN   NO, UPDATE SALESPERSON           
         OC    DOIDSPER,=25C' '                                                 
         OI    MISCFLG3,MF3SALPR                                                
         DROP  R4                                                               
*                                                                               
RCLAK210 LLC   R1,0(R3)                                                         
         AR    R3,R1               BUMP TO NEXT SAVED ENTRY                     
*                                                                               
RCLAK250 CLC   =C'ORDTLR',1(R3)                                                 
         BE    RCLAK300                                                         
         LHI   R1,*-T16100                                                      
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
RCLAK300 XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         ST    R6,AIO                                                           
         BRAS  RE,PUT                                                           
*                                                                               
         BRAS  RE,CHKFRCHG         BUILD CHECK FOR CHANGES KEY                  
*                                                                               
         TM    MISCFLG3,MF3SALPR   DID THE SALESPERSON CHANGE?                  
         BZ    *+8                 NO                                           
         BRAS  RE,SPREASGN         YES, UPDATE SALESPERSON RECORD               
*                                                                               
         TM    MISCFLG3,MF3RADIO                                                
         BZ    *+12                                                             
         L     R3,AWRKRIOA                                                      
         LA    R7,3(R3)            R7 = A(ORDRCL HEADER)                        
*                                                                               
         CLI   RORCACCP,C'D'                                                    
         BNE   RCLAK310                                                         
         CLI   REVISION,0                                                       
         BE    RCLAK310            REGULAR RECALL                               
         TM    BITFLAG2,BF2SPNDG   THIS BIT IS NOW "TO BE SENT"                 
         BNZ   RCLAK310            YES!                                         
         TM    MISCFLG3,MF3RADIO   RADIO ORDER RECALLED?                        
         BNZ   RCLAK310            YES, NO AGYCAN NEEDED AS PER SKUI            
         BRAS  RE,SNDAGYCN                                                      
*                                                                               
RCLAK310 TM    BITFLAG2,BF2SPNDG   THIS BIT IS NOW "TO BE SENT"                 
         BZ    RCLAK320            NO                                           
         BRAS  RE,SNDSCRPT                                                      
*                                                                               
RCLAK320 BAS   RE,UPDATSB1                                                      
*                                                                               
RCLAKYES BRAS  RE,BLDCOLOR                                                      
         BRAS  RE,DAREMAIL                                                      
         NI    MISCFLG3,X'FF'-MF3RADIO                                          
         B     YES                                                              
*                                                                               
RCLAKNO  TM    MISCFLG1,MF1XMTUP                                                
         BNZ   RCLAK300                                                         
         B     NO                                                               
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CALCULATES IF TWO DATE/TIME ARE WITHIN 16 MINUTES OF             
* EACH OHTER                                                                    
*                                                                               
* ON ENTRY:    PARAM 1             A(1ST DATE/TIME)                             
*              PARAM 2             A(2ND DATE/TIME)                             
*                                                                               
* ON EXIT:     CC                  YES - WITHIN 16 MINUTES                      
*                                                                               
* NOTE: DATE FORMAT IS JULIAN PWOS (CYYDDD)                                     
*       TIME FORMAT IS HEXIN MILITARY (HHMM)                                    
*                                                                               
* WARNING:     WORK GETS CLOBBERED                                              
***********************************************************************         
COMPTIME NTR1                                                                   
         L     R5,0(R1)                                                         
         L     R6,4(R1)                                                         
*                                                                               
         CLC   0(3,R5),0(R6)       SAME DATE?                                   
         BNE   CMPTMNO             NO, EXIT FOR NOW                             
*                                                                               
         MVC   BYTE,3(R5)          GET HOURS AND MULTIPLE BY 60 MINUTES         
         NI    BYTE,X'F0'                                                       
         LLC   R1,BYTE                                                          
         SRL   R1,4                                                             
         MH    R1,=H'600'                                                       
         CVD   R1,DUB                                                           
         ZAP   PACKOF4B,DUB                                                     
*                                                                               
         MVC   BYTE,3(R5)                                                       
         NI    BYTE,X'0F'                                                       
         LLC   R1,BYTE                                                          
         MH    R1,=H'60'                                                        
         CVD   R1,DUB                                                           
         AP    PACKOF4B,DUB                                                     
*                                                                               
         MVC   BYTE,4(R5)          ADD MINUTES TO HOUR-MINUTES                  
         NI    BYTE,X'F0'                                                       
         LLC   R1,BYTE                                                          
         SRL   R1,4                                                             
         MH    R1,=H'10'                                                        
         CVD   R1,DUB                                                           
         AP    PACKOF4B,DUB                                                     
*                                                                               
         MVC   BYTE,4(R5)                                                       
         NI    BYTE,X'0F'                                                       
         LLC   R1,BYTE                                                          
         CVD   R1,DUB                                                           
         AP    DUB,PACKOF4B                                                     
         CVB   RE,DUB                                                           
*                                                                               
         MVC   BYTE,3(R6)          GET HOURS AND MULTIPLE BY 60 MINUTES         
         NI    BYTE,X'F0'                                                       
         LLC   R1,BYTE                                                          
         SRL   R1,4                                                             
         MH    R1,=H'600'                                                       
         CVD   R1,DUB                                                           
         ZAP   PACKOF4B,DUB                                                     
*                                                                               
         MVC   BYTE,3(R6)                                                       
         NI    BYTE,X'0F'                                                       
         LLC   R1,BYTE                                                          
         MH    R1,=H'60'                                                        
         CVD   R1,DUB                                                           
         AP    PACKOF4B,DUB                                                     
*                                                                               
         MVC   BYTE,4(R6)          ADD MINUTES TO HOUR-MINUTES                  
         NI    BYTE,X'F0'                                                       
         LLC   R1,BYTE                                                          
         SRL   R1,4                                                             
         MH    R1,=H'10'                                                        
         CVD   R1,DUB                                                           
         AP    PACKOF4B,DUB                                                     
*                                                                               
         MVC   BYTE,4(R6)                                                       
         NI    BYTE,X'0F'                                                       
         LLC   R1,BYTE                                                          
         CVD   R1,DUB                                                           
         AP    DUB,PACKOF4B                                                     
         CVB   RF,DUB                                                           
*                                                                               
         SR    RE,RF               CALCULATE AND STORE DIFFERENCE IN            
         LPR   RE,RE                   MINUTES                                  
         ST    RE,DMCB                                                          
*                                                                               
         CH    RE,=H'60'    **  CHANGED TO 60!!! STUPID SB2 ENCODA!! **         
***      CH    RE,=H'16'           ARE (DATE/TIME)S WITHIN 16 MINUTES?          
         BNL   CMPTMNO             NO, SUX TO BE ENCODA                         
*                                                                               
CMPTMYES B     YES                                                              
*                                                                               
CMPTMNO  B     NO                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SCRIPT SETUP FOR AUTO-SEND                                                    
***********************************************************************         
SNDSCRPT NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,GETORDER         READ THE ORDER RECORD TO AIO1                
*                                                                               
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         LA    R6,DORFRST-DOKEY(R6)                                             
         XR    R0,R0                                                            
SSCRP10  CLI   0(R6),0                                                          
         BE    SSCRP50                                                          
         CLI   0(R6),DOSPELQ       X'03' - SUPPLEMENTARY ID ELEMENT             
         BE    SSCRP15                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     SSCRP10                                                          
*                                                                               
         USING DOSPELD,R6                                                       
SSCRP15  TM    DOSPFLG2,DOSPOMDT   ORDER PREVIOUSLY SENT FROM LINK?             
         BZ    SSCRP50             NO, NORMAL ORDXMT SCRIPT                     
         BRAS  RE,WRKRCREA                                                      
*                                                                               
         L     R2,AIO2                                                          
         XC    0(256,R2),0(R2)                                                  
         LA    R2,4(R2)                                                         
         MVC   0(4,R2),=F'2101'                                                 
         MVC   4(6,R2),=C'SCRIPT'                                               
         MVC   10(8,R2),=C'SPLNKSND'                                            
         MVI   18(R2),C'I'                                                      
         MVC   30(5,R2),=C'00006'    HEADER LENGTH                              
         MVC   35(5,R2),=C'00100'    INPUT  DATA LENGTH                         
         MVC   40(5,R2),=C'00020'    OUTPUT DATA LENGTH                         
         MVI   45(R2),C'Y'                                                      
         L     R2,AIO2                                                          
         MVC   0(2,R2),=H'50'        46 + 4 BYTES FOR QSAM                      
*                                                                               
         GOTOR WRKRSEND,DMCB,AIO2                                               
*                                                                               
         L     R2,AIO2                                                          
         XC    0(256,R2),0(R2)                                                  
         LA    R2,4(R2)                                                         
         MVC   0(4,R2),=F'2102'                                                 
         MVC   4(6,R2),=C'000001'                                               
         MVC   10(20,R2),=CL20'SIGN-ON INFORMATION '                            
*                                                                               
         MVC   30(8,R2),USERID                                                  
         TM    BITFLAG1,BF1PSSWD   PASSWORD REQUIRED FOR SPOT SYSTEM?           
         BZ    *+10                                                             
         MVC   38(3,R2),=C'DDS'    YES, THEN USE THE EVER USEFUL DDS            
         CLC   =C'TMNY',USERID     TMNY ID?                                     
         BNE   *+10                                                             
         MVC   38(3,R2),=C'ANN'    YES, DDS PASSWORD DOESN'T WORK               
         L     R2,AIO2                                                          
         MVC   0(2,R2),=H'50'        46 + 4 BYTES FOR QSAM                      
*&&DO                                                                           
         MVC   30(8,R2),=CL8'NEWSTYLE'                                          
         MVC   30+16(8,R2),USERID                                               
         MVC   30+16+8(8,R2),=CL8',DDSDARE'                                     
         MVC   30+60(4,R2),=C'T214'   USERID+PID (44 BYTES)                     
         L     R2,AIO2                                                          
         MVC   0(2,R2),=H'98'        94 + 4 BYTES FOR QSAM                      
*&&                                                                             
         GOTOR WRKRSEND,DMCB,AIO2                                               
*                                                                               
         L     R2,AIO2                                                          
         XC    0(256,R2),0(R2)                                                  
         LA    R2,4(R2)                                                         
         MVC   0(4,R2),=F'2102'                                                 
         MVC   4(6,R2),=C'000002'                                               
         MVC   10(20,R2),=CL20'SENDING WITH $LINK'                              
*                                                                               
         LA    R2,30(R2)                                                        
         MVC   0(1,R2),QMED                                                     
         LA    R2,1(R2)                                                         
*                                                                               
         MVC   FULL,BINORDER       SHOW THE ORDER NUMBER                        
         XC    FULL,=4X'FF'                                                     
         TM    FULL,X'80'          NEW STYLE ORDER NUMBER?                      
         BZ    SSCRP20                                                          
         NI    FULL,X'FF'-X'80'                                                 
         ICM   R1,15,FULL                                                       
         CVD   R1,DUB                                                           
         AP    DUB,=P'04000000'                                                 
         OI    DUB+7,X'0F'                                                      
         UNPK  0(8,R2),DUB                                                      
         B     SSCRP30                                                          
*                                                                               
SSCRP20  L     R1,FULL                                                          
         AHI   R1,1                                                             
         ZICM  R1,FULL,2                                                        
         CVD   R1,DUB                                                           
         UNPK  0(4,R2),DUB                                                      
         OI    3(R2),X'F0'                                                      
*                                                                               
         ZICM  R3,FULL+2,2                                                      
         CVD   R3,DUB                                                           
         UNPK  4(4,R2),DUB                                                      
         OI    4+3(R2),X'F0'      SEQUENCE NUMBER                               
*                                                                               
SSCRP30  LA    R2,8(R2)                                                         
         L     RE,AIO2                                                          
         SR    R2,RE                                                            
         STH   R2,0(RE)            L'DATA + 4 BYTES FOR QSAM                    
*                                                                               
         GOTOR WRKRSEND,DMCB,AIO2                                               
         BRAS  RE,WRKRCLOS                                                      
         B     SSCRPX                                                           
*                                                                               
SSCRP50  L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
         MVC   QBUYER,DOIDBYR      SAVE THE BUYER CODE                          
         MVC   BCLT,DOIDCLT                                                     
         GOTO1 VCLUNPK,DMCB,DOIDCLT,QCLT                                        
         MVC   QPRD1(1),DOIDPRD    SAVE BINARY PRODUCT CODES                    
         MVC   QPRD2(1),DOIDPRD2                                                
         MVC   BEST,DOIDEST                                                     
         EDIT  (B1,DOIDEST),(3,QEST1),FILL=0                                    
         XC    WORK,WORK                                                        
         MVC   WORK+2(L'DOISTA),DOISTA                                          
         GOTO1 VMSUNPK,DMCB,WORK,WORK+10,QSTA                                   
         MVC   QFLTNUM,=C'  '                                                   
         MVI   QCSHTRDE,C' '                                                    
         CLI   DOIDFLTN,0                                                       
         BE    SSCRP60                                                          
         EDIT  (B1,DOIDFLTN),(2,QFLTNUM),FILL=0                                 
*                                                                               
SSCRP60  XR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    SSCRP70                                                          
         CLI   0(R6),DOSPELQ       SUPP ID ELEM (X'03')?                        
         BH    SSCRP70                                                          
         BL    SSCRP60                                                          
         USING DOSPELD,R6                                                       
         CLI   DOSPLEN,DOSPLNQ                                                  
         BNH   SSCRP70                                                          
         CLI   DOSPTMTH,0          ANY METHOD WHATSOEVER?                       
         BE    SSCRP70                                                          
         CLC   =C'000',DOSPTDAT    ANY TRADE REP SPECIFIED?                     
         BNL   SSCRP70             NONE                                         
*                                                                               
         MVI   QCSHTRDE,C'C'                                                    
         TM    DOSPTMTH,X'40'      TRADE?                                       
         BZ    *+8                                                              
         MVI   QCSHTRDE,C'T'                                                    
         DROP  R6                                                               
*                                                                               
SSCRP70  BRAS  RE,GETCLTRC         GET THE CLIENT RECORD                        
         BE    SSCRP80                                                          
         L     R1,=AL4(*-T16100)                                                
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
SSCRP80  L     R6,AIO1                                                          
         USING CLTHDRD,R6                                                       
         MVC   SVCPRF00,CPROF+0                                                 
         GOTO1 VCLUNPK,DMCB,(CPROF+6,BCLT),QCLT                                 
         MVC   SVCOFFC,COFFICE                                                  
         DROP  R6                                                               
*                                                                               
         MVC   BPRD,QPRD1                                                       
         GOTOR GETQPRD,DMCB,(QPRD1,QPRD1)                                       
         GOTOR GETQPRD,DMCB,(QPRD2,QPRD2)                                       
*                                                                               
         BRAS  RE,WRKRCREA                                                      
*                                                                               
         L     R1,AIO2                                                          
         XC    0(256,R1),0(R1)                                                  
         LA    R1,4(R1)                                                         
         MVC   0(4,R1),=F'2101'                                                 
         MVC   4(6,R1),=C'SCRIPT'                                               
         MVC   10(8,R1),=C'SPORDXMT'                                            
         MVI   18(R1),C'I'                                                      
         MVC   30(5,R1),=C'00006'    HEADER LENGTH                              
         MVC   35(5,R1),=C'00100'    INPUT  DATA LENGTH                         
         MVC   40(5,R1),=C'00020'    OUTPUT DATA LENGTH                         
         MVI   45(R1),C'Y'                                                      
         L     R1,AIO2                                                          
         MVC   0(2,R1),=H'50'        46 + 4 BYTES FOR QSAM                      
*                                                                               
         GOTOR WRKRSEND,DMCB,AIO2                                               
*                                                                               
         L     R1,AIO2                                                          
         XC    0(256,R1),0(R1)                                                  
         LA    R1,4(R1)                                                         
         MVC   0(4,R1),=F'2102'                                                 
         MVC   4(6,R1),=C'000001'                                               
         MVC   10(20,R1),=CL20'SIGN-ON INFORMATION '                            
*                                                                               
         MVC   30(8,R1),USERID                                                  
         TM    BITFLAG1,BF1PSSWD   PASSWORD REQUIRED FOR SPOT SYSTEM?           
         BZ    *+10                                                             
         MVC   38(3,R1),=C'DDS'    YES, THEN USE THE EVER USEFUL DDS            
         CLC   =C'TMNY',USERID     TMNY ID?                                     
         BNE   *+10                                                             
         MVC   38(3,R1),=C'ANN'    YES, DDS PASSWORD DOESN'T WORK               
         L     R1,AIO2                                                          
         MVC   0(2,R1),=H'50'        46 + 4 BYTES FOR QSAM                      
*&&DO                                                                           
         MVC   30(8,R1),=CL8'NEWSTYLE'                                          
         MVC   30+16(8,R1),USERID                                               
         MVC   30+16+8(8,R1),=CL8',DDSDARE'                                     
         MVC   30+60(4,R1),=C'T214'   USERID+PID (44 BYTES)                     
         L     R1,AIO2                                                          
         MVC   0(2,R1),=H'98'        94 + 4 BYTES FOR QSAM                      
*&&                                                                             
         GOTOR WRKRSEND,DMCB,AIO2                                               
*                                                                               
         L     R1,AIO2                                                          
         XC    0(256,R1),0(R1)                                                  
         LA    R1,4(R1)                                                         
         MVC   0(4,R1),=F'2102'                                                 
         MVC   4(6,R1),=C'000002'                                               
         MVC   10(20,R1),=CL20'$DAR TO ORDER/SEND'                              
         LA    R1,30(R1)                                                        
         USING LAYOUT2D,R1                                                      
         MVC   LAY2MED,QMED                                                     
         MVC   LAY2BYR,QBUYER                                                   
         MVC   LAY2MTHD,QCSHTRDE                                                
         MVC   LAY2CLT,QCLT                                                     
*                                                                               
         MVC   LAY2PRDS(3),QPRD1                                                
         CLI   QPRD2,C' '          ANY PIGGYBACK?                               
         BNH   *+14                                                             
         MVI   LAY2PRDS+3,C'-'     YES                                          
         MVC   LAY2PRDS+4(3),QPRD2                                              
*                                                                               
         MVC   LAY2EST(L'QEST1),QEST1                                           
         CLC   QFLTNUM,=C'00'                                                   
         BNH   *+14                                                             
         MVI   LAY2EST+3,C'-'                                                   
         MVC   LAY2EST+4(2),QFLTNUM                                             
*                                                                               
         MVC   LAY2STA,QSTA                                                     
         LA    R1,LAY2END                                                       
         DROP  R1                                                               
         L     RE,AIO2                                                          
         SR    R1,RE                                                            
         STH   R1,0(RE)            L'DATA + 4 BYTES FOR QSAM                    
*                                                                               
         GOTOR WRKRSEND,DMCB,AIO2                                               
*                                                                               
         BRAS  RE,WRKRCLOS                                                      
*                                                                               
SSCRPX   L     RE,AWRKRIOA         RESET WORKER IO AREA FOR EDICT RECS          
         MVI   0(RE),0                                                          
         MVI   1(RE),2                                                          
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE MAKEGOOD ORDER CONFIRMATION                                     
***********************************************************************         
MKGDCNFM NTR1  BASE=*,LABEL=*                                                   
         L     R9,SRPARMSD.SRQASYSF                                             
*                                                                               
         USING MOFRCFMD,R7                                                      
MCNFMD   USING RTN2SNDR,MOCFRTNS                                                
*                                                                               
         MVI   QMED,C'T'                                                        
         CLI   MCNFMD.RTNAGYMD+1,C'2'  (---MD OF AGYMD IS 2?)                   
         BNE   *+8                                                              
         MVI   QMED,C'R'                                                        
*                                                                               
         GOTO1 VHEXIN,DMCB,MCNFMD.RTNAGYMD,BAGYMD,2                             
         MVC   AGENCY,MCNFMD.RTNPWRCD                                           
         GOTO1 VHEXIN,DMCB,MCNFMD.RTNSYSID,BYTE,2                               
         BAS   RE,SPANKED                                                       
         CLC   BYTE,THESYSID       WRONG FACPAK?                                
         BNE   MCNFMNO             YES, AGENCY DOESN'T BELONG HERE              
*                                                                               
         DROP  MCNFMD                                                           
*                                                                               
         MVC   QREPCON,MOCFRPCN    COPY THESE VALUES                            
         MVC   QRETURN,MOCFRTNS                                                 
*                                                                               
         GOTO1 CALCORDR,DMCB,MOCFORDR   CONVERT ORDER NUMBER TO BINARY          
         BNE   MCNFMNO                                                          
*                                                                               
         MVC   USERID,MOCFTOID                                                  
         BAS   RE,SWTCHCTL         SWITCH CONTROL TO SPOT SYSTEM                
         BNE   MCNFMNO                                                          
*                                                                               
         BRAS  RE,GETORDER         READ ORDER RECORD IN AIO1                    
         BNE   MCNFMNO                                                          
*                                                                               
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
         MVC   QBUYER,DOIDBYR      SAVE THE BUYER CODE                          
         MVC   BCLT,DOIDCLT                                                     
         GOTO1 VCLUNPK,DMCB,DOIDCLT,QCLT                                        
         MVC   QPRD1(1),DOIDPRD    SAVE BINARY PRODUCT CODES                    
         MVC   QPRD2(1),DOIDPRD2                                                
         MVC   BEST,DOIDEST                                                     
         EDIT  (B1,DOIDEST),(3,QEST1),FILL=0                                    
         XC    WORK,WORK                                                        
         MVC   WORK+2(L'DOISTA),DOISTA                                          
         GOTO1 VMSUNPK,DMCB,WORK,WORK+10,QSTA                                   
         CLI   QSTA,C'0'           IF CABLE, SKIP THIS                          
         BNL   MCNFM035                                                         
         CLI   QSTA+4,C' '                                                      
         BNE   MCNFM035                                                         
         MVI   QSTA+4,C'T'                                                      
MCNFM035 MVC   QFLTNUM,=C'  '                                                   
         MVI   QCSHTRDE,C' '                                                    
         CLI   DOIDFLTN,0                                                       
         BE    MCNFM040                                                         
         EDIT  (B1,DOIDFLTN),(2,QFLTNUM),FILL=0                                 
MCNFM040 MVC   QMGGROUP,MOCFOFRI   MAKEGOOD GROUP CODE                          
*                                                                               
MCNFM050 XR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    MCNFM060                                                         
         CLI   0(R6),DOSPELQ       SUPP ID ELEM (X'03')?                        
         BH    MCNFM060                                                         
         BL    MCNFM050                                                         
*                                                                               
         USING DOSPELD,R6                                                       
         CLI   DOSPLEN,DOSPLNQ                                                  
         BNH   MCNFM060                                                         
         CLI   DOSPTMTH,0          ANY METHOD WHATSOEVER?                       
         BE    MCNFM060                                                         
         CLC   DOSPTDAT(3),=C'   ' ANY TRADE DATA SPECIFIED?                    
         BNH   MCNFM060            NONE                                         
*                                                                               
         MVI   QCSHTRDE,C'C'                                                    
         TM    DOSPTMTH,X'40'      UPPERCASE MEANS TRADE!                       
         BZ    *+8                                                              
         MVI   QCSHTRDE,C'T'                                                    
         DROP  R6                                                               
*                                                                               
MCNFM060 BRAS  RE,GETCLTRC         GET THE CLIENT RECORD                        
         BE    MCNFM070                                                         
         L     R1,=AL4(*-T16100)                                                
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
MCNFM070 L     R6,AIO1                                                          
         USING CLTHDRD,R6                                                       
         MVC   SVCPRF00,CPROF+0                                                 
         GOTO1 VCLUNPK,DMCB,(CPROF+6,BCLT),QCLT                                 
         MVC   SVCOFFC,COFFICE                                                  
         DROP  R6                                                               
*                                                                               
         MVC   BPRD,QPRD1                                                       
         GOTOR GETQPRD,DMCB,(QPRD1,QPRD1)                                       
         GOTOR GETQPRD,DMCB,(QPRD2,QPRD2)                                       
*                                                                               
         BAS   RE,ESTLOCKD         TEST IF ESTIMATE IS LOCKED                   
*                                                                               
         OC    ESTPW,ESTPW         ANY PW PCT?                                  
         BNZ   *+14                                                             
         OC    ESTCOST2,ESTCOST2   ANY COST2 PCT?                               
         BZ    *+8                                                              
         BAS   RE,GETPWC2          TEST IF PW LOCKED                            
*                                                                               
         TM    BITFLAG2,BF2ELCKD   ESTIMATE ALREADY LOCKED?                     
         BNZ   MCNFM080            YES, DON'T NEED TO CHECK THE PROFILE         
*****                                                                           
         L     R1,SRPARMSD.SRQAUTL                                              
         USING UTLD,R1                                                          
         MVC   TUSER,SIGNON2H      FOOL GETPROF                                 
         DROP  R1                                                               
*****                                                                           
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SDAR'    <=== NEED LOWER CASE 'S'                     
         NI    WORK,X'FF'-X'40'    MAKE IT LOWERCASE                            
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
         CLI   SVCOFFC,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCOFFC                                               
*                                                                               
         L     RF,SRPARMSD.SRQACOMF                                             
         L     RF,CGETPROF-COMFACSD(RF)                                         
         XC    PROFDAR,PROFDAR                                                  
         GOTO1 (RF),DMCB,WORK,PROFDAR,VDATAMGR                                  
*                                                                               
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         MVC   WORK(4),=C'S0OM'    OM PROFILE TELLS US IF DAR OR OM             
         MVC   WORK+4(2),AGENCY                                                 
         LA    R4,WORK+16                                                       
         GOTO1 (RF),DMCB,WORK,(R4)                                              
*****                                                                           
         L     R1,SRPARMSD.SRQAUTL                                              
         USING UTLD,R1                                                          
         XC    TUSER,TUSER         RESET THE ID NUMBER IN THE UTL               
         DROP  R1                                                               
*****                                                                           
         LA    R1,DMCB                                                          
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         CLI   PDARONHD,C'Y'       SET TO ON HOLD IF MAKEGOOD OKAY?             
         BNE   MCNFM080                                                         
         OI    BITFLAG2,BF2ELCKD   YES, LOOKS LIKE ESTIMATE LOCKED              
*                                                                               
MCNFM080 BRAS  RE,GETNOTCE         READ MAKEGOOD NOTICE RECORD IN AIO1          
         BNE   MCNFMNO                                                          
*                                                                               
         L     RF,AIO1             LOOK FOR LASTEST STATUS                      
         LA    R6,MNXFRST-MNXKEY(RF)                                            
         CLI   QSTA,C'0'           CABLE?                                       
         BNL   *+8                                                              
         LA    R6,MNRFRST-MNKEY(RF)                                             
         MVI   ELCODE,MNSTELQ                                                   
         BAS   RE,FIRSTEL                                                       
         BNE   MCNFM110            ERROR, COULD BE A NEW NOTICE                 
         USING MNSTELD,R6                                                       
*                                                                               
MCNFM090 CLI   MNSTSTAT,MNSTDELV   DELIVERED STATUS?                            
         BNE   MCNFM100                                                         
         BAS   RE,NEXTEL           THEN SEE IF DELIVERED APPROVAL               
         BNE   MCNFM110                                                         
         B     MCNFM090                                                         
*                                                                               
MCNFM100 CLI   MNSTSTAT,MNSTGOIN   GOING TO BE OKAY?                            
         BE    MCNFMNO                                                          
         CLI   MNSTSTAT,MNSTOKAY         OKAYED?                                
         BE    MCNFMNO                                                          
         CLI   MNSTSTAT,MNSTHOLD         ON HOLD?                               
         BE    MCNFMNO             THEN DON'T PROCESS THIS AGAIN                
*                                                                               
         CLI   MNSTSTAT,MNSTAPP    APPROVED STATUS?                             
         BE    MCNFM120                                                         
         CLI   MNSTSTAT,MNSTSAPP   SELF APPLIED?                                
         BNE   MCNFM110                                                         
         NI    BITFLAG2,X'FF'-BF2ELCKD+BF2PWLCK  CLEAR THOSE FLAGS!             
         B     MCNFM230                                                         
         DROP  R6                                                               
MCNFM110 LHI   R1,REFMNAPP                                                      
         BRAS  RE,SNDERROR                                                      
         B     MCNFMNO                                                          
*                                                                               
MCNFM120 TM    BITFLAG2,BF2ELCKD+BF2PWLCK    LOCKED ALREADY?                    
         BNZ   MCNFM230                                                         
         OC    ESTLOKYM,ESTLOKYM   ANY LOCKOUT PERIODS?                         
         BZ    MCNFM230                                                         
*                                                                               
         MVC   HALF(2),ESTLOKYM                                                 
         NI    HALF+1,X'FF'-X'C0'  REMOVE PRIOR AND SUBSEQUENT BITS             
         L     RF,AIO1                                                          
         LA    R6,MNXFRST-MNXKEY(RF)                                            
         CLI   QSTA,C'0'           CABLE?                                       
         BNL   *+8                                                              
         LA    R6,MNRFRST-MNKEY(RF)                                             
         MVI   ELCODE,MNMSELQ      LOOK FOR MISSED SPOTS (X'10')                
         BAS   RE,FIRSTEL                                                       
         BNE   MCNFM170            NO MISSED SPOTS, CHECK MG SPOTS              
         USING MNMSELD,R6                                                       
MCNFM130 GOTO1 VDATCON,DMCB,(8,MNMSBDAT),(3,FULL)                               
*                                                                               
         TM    ESTLOKYM+1,X'C0'    PRIOR OR SUBSEQUENT?                         
         BNZ   MCNFM150            COULD BE EITHER                              
         CLC   HALF,FULL           MISSED DATE MATCHES MONTH?                   
         BNE   MCNFM140            NO, CHEXT THE NEXT MISSED SPOT               
         OI    BITFLAG2,BF2ELCKD   YES, CONSIDER IT LOCKED                      
         B     MCNFM230                                                         
*                                                                               
MCNFM140 BAS   RE,NEXTEL           NO MORE MISSED SPOTS?                        
         BNE   MCNFM170            CHECK MG SPOTS THEN                          
         B     MCNFM130            OTHERWISE GO BACK FOR MORE                   
*                                                                               
MCNFM150 TM    ESTLOKYM+1,X'80'    THE MONTH AND PRIOR?                         
         BZ    MCNFM160            NO, THE MONTH AND SUBSEQUENT                 
         CLC   FULL(2),HALF        MISSED DATE <= LOCKED MONTH?                 
         BH    MCNFM140            NO, HIGHER                                   
         OI    BITFLAG2,BF2ELCKD   YES, CONSIDER IT LOCKED                      
         B     MCNFM230                                                         
*                                                                               
MCNFM160 CLC   FULL(2),HALF        MISSED DATE >= LOCKED MONTH?                 
         BL    MCNFM140            NO, CHEXT THE NEXT MISSED SPOT               
         OI    BITFLAG2,BF2ELCKD   YES, CONSIDER IT LOCKED                      
         B     MCNFM230                                                         
*                                                                               
MCNFM170 BRAS  RE,GETOFFER                                                      
*                                                                               
         L     RF,AIO1                                                          
         LA    R6,MOXFRST-MOXKEY(RF)                                            
         CLI   QSTA,C'0'           CABLE?                                       
         BNL   *+8                                                              
         LA    R6,MORFRST-MOKEY(RF)                                             
         MVI   ELCODE,MOBDELQ      LOOK FOR MAKEGOOD SPOTS (X'50')              
         BAS   RE,FIRSTEL                                                       
         BNE   MCNFM220            NO OFFERED SPOTS, CHECK MISSED SPOTS         
*                                                                               
         USING MOBDELD,R6                                                       
MCNFM180 GOTO1 VDATCON,DMCB,(8,MOBDBDAT),(8,WORK)                               
         MVC   WORK+8(2),=C'-('                                                 
         XR    R0,R0                                                            
         IC    R0,MOBDNWKS                                                      
         CVD   R0,DUB                                                           
         UNPK  WORK+10(2),DUB      CAN'T HAVE MORE THAN 99 WKS                  
         OI    WORK+11,X'F0'                                                    
         MVC   WORK+12(2),=C'W)'                                                
         LA    R0,WORK                                                          
         ST    R0,DMCB                                                          
         MVI   DMCB,14                                                          
         L     R1,SRPARMSD.SRQACOMF    R1 = A(COMFACS)                          
         USING COMFACSD,R1                                                      
         L     RF,CPERVAL                                                       
         DROP  R1                                                               
         GOTO1 (RF),DMCB,,(X'10',ELEM)  LESS 1 DAY FOR THE END DATE             
*                                                                               
         LA    R1,ELEM                                                          
         USING PERVALD,R1                                                       
         MVC   FULL(2),PVALBSTA    SAVE YM OF THE START DATE                    
         MVC   FULL+2(2),PVALBEND  SAVE YM OF THE END   DATE                    
         DROP  R1                                                               
*                                                                               
         TM    ESTLOKYM+1,X'C0'    PRIOR OR SUBSEQUENT?                         
         BNZ   MCNFM200            COULD BE EITHER                              
         CLC   HALF,FULL           MAKEGOOD DATE MATCHES MONTH?                 
         BE    *+14                                                             
         CLC   HALF,FULL+2                                                      
         BNE   MCNFM190            NO, CHEXT THE NEXT MAKEGOOD SPOT             
         OI    BITFLAG2,BF2ELCKD   YES, CONSIDER IT LOCKED                      
         B     MCNFM220                                                         
*                                                                               
MCNFM190 BAS   RE,NEXTEL           NO MORE MISSED SPOTS?                        
         BNE   MCNFM220            CHECK MG SPOTS THEN                          
         B     MCNFM180            OTHERWISE GO BACK FOR MORE                   
*                                                                               
MCNFM200 TM    ESTLOKYM+1,X'80'    THE MONTH AND PRIOR?                         
         BZ    MCNFM210            NO, THE MONTH AND SUBSEQUENT                 
         CLC   FULL(2),HALF        MAKEGOOD START DATE <= MONTH?                
         BH    MCNFM190            NO, NEXT MAKEGOOD SPOT                       
         OI    BITFLAG2,BF2ELCKD   YES, CONSIDER IT LOCKED                      
         B     MCNFM220                                                         
*                                                                               
MCNFM210 CLC   FULL+2(2),HALF      MAKEGOOD END DATE >= LOCKED MONTH?           
         BL    MCNFM190            NO, NEXT MAKEGOOD SPOT                       
         OI    BITFLAG2,BF2ELCKD   YES, CONSIDER IT LOCKED                      
*                                                                               
MCNFM220 BRAS  RE,GETNOTCE         RESTORE THE NOTICE RECORD TO AIO1            
*                                                                               
MCNFM230 L     RF,AIO1             MIGHT OF SKIPPED DELIVERED STATUS            
         LA    R6,MNXFRST-MNXKEY(RF)                                            
         CLI   QSTA,C'0'           CABLE?                                       
         BNL   *+8                                                              
         LA    R6,MNRFRST-MNKEY(RF)                                             
         MVI   ELCODE,MNSTELQ                                                   
         BAS   RE,FIRSTEL                                                       
         USING MNSTELD,R6                                                       
*                                                                               
         XC    ELEM,ELEM                                                        
MCNFM240 USING MNSTELD,ELEM                                                     
         MVI   MCNFM240.MNSTEL,MNSTELQ                                          
         MVI   MCNFM240.MNSTLEN,MNSTLENQ                                        
         GOTO1 VDATCON,DMCB,(5,0),(0,WORK)                                      
         THMS  DDSTIME=YES                                                      
         ST    R0,PACKOF4B                                                      
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         BL    MCNFM250                                                         
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 VADDAY,DMCB,WORK,WORK,F'1'                                       
*                                                                               
MCNFM250 GOTO1 VDATCON,DMCB,(0,WORK),(19,MCNFM240.MNSTDATE)                     
         L     R1,PACKOF4B                                                      
         SRL   R1,12                                                            
         STCM  R1,3,MCNFM240.MNSTTIME                                           
*                                                                               
         MVI   MCNFM240.MNSTSTAT,MNSTGOIN   GOING TO BE OKAYED                  
         TM    BITFLAG2,BF2ELCKD+BF2PWLCK                                       
         BZ    *+8                                                              
         MVI   MCNFM240.MNSTSTAT,MNSTHOLD   HOLD, EST OR PW LOCKED              
         CLI   MNSTSTAT,MNSTSAPP            WAS IT SELF APPLIED?                
         BNE   *+8                                                              
         MVI   MCNFM240.MNSTSTAT,MNSTOKAY   YES, THE OKAY!                      
*                                                                               
         MVC   SVSTAT,MCNFM240.MNSTSTAT     SAVE FOR LATER                      
         MVI   THISISMG,C'Y'                                                    
         DROP  MCNFM240                                                         
*                                                                               
         GOTO1 VRECUP,DMCB,(C'S',AIO1),ELEM,(R6)  ADD STATUS                    
         ORG   *-2                                                              
         CLI   QSTA,C'0'           CABLE?                                       
         BL    MCNFM255             NO                                          
         MVI   0(R1),X'FE'         CAN'T USE C'T', BC RECUP NOT CHANGED         
         LAY   RE,XSPREC            TO USE MAX REC SIZE IN DMFILTAB HAS         
         ST    RE,12(R1)            FOR XSPFIL                                  
MCNFM255 BASR  RE,RF                                                            
*                                                                               
         MVC   AIO,AIO1                                                         
         LAY   RF,XPUT                                                          
         CLI   QSTA,C'0'           CABLE?                                       
         BNL   *+10                                                             
         LAY   RF,PUT                                                           
         BASR  RE,RF                                                            
*                                                                               
         BRAS  RE,CHKFRCHG         BUILD CHECK FOR CHANGES KEY                  
*                                  ESTIMATE OR PW LOCKED?                       
         TM    BITFLAG2,BF2ELCKD+BF2PWLCK                                       
         BNZ   MCNFMYES            YES, LET USER START SCRIPT PROCESS           
         CLI   SVSTAT,MNSTOKAY     SELF APPLIED? OKAY == SELFAPPLIED            
         BE    MCNFMYES            YES, MAKEGOOD ALREADY APPLIED                
         BRAS  RE,WRKRCREA                                                      
*                                                                               
         L     R1,AIO2                                                          
         XC    0(256,R1),0(R1)                                                  
         LA    R1,4(R1)                                                         
         MVC   0(4,R1),=F'2101'                                                 
         MVC   4(6,R1),=C'SCRIPT'                                               
         MVC   10(8,R1),=C'SPMGEACC'                                            
         MVI   18(R1),C'I'                                                      
         MVC   30(5,R1),=C'00006'    HEADER LENGTH                              
         MVC   35(5,R1),=C'00100'    INPUT  DATA LENGTH                         
         MVC   40(5,R1),=C'00020'    OUTPUT DATA LENGTH                         
         MVI   45(R1),C'Y'                                                      
         L     R1,AIO2                                                          
         MVC   0(2,R1),=H'50'        46 + 4 BYTES FOR QSAM                      
*                                                                               
         GOTOR WRKRSEND,DMCB,AIO2                                               
*                                                                               
         L     R1,AIO2                                                          
         XC    0(256,R1),0(R1)                                                  
         LA    R1,4(R1)                                                         
         MVC   0(4,R1),=F'2102'                                                 
         MVC   4(6,R1),=C'000001'                                               
         MVC   10(20,R1),=CL20'SIGN-ON INFORMATION '                            
*                                                                               
         MVC   30(8,R1),MOCFTOID                                                
         TM    BITFLAG1,BF1PSSWD   PASSWORD REQUIRED FOR SPOT SYSTEM?           
         BZ    *+10                                                             
         MVC   38(3,R1),=C'DDS'    YES, THEN USE THE EVER USEFUL DDS            
         L     R1,AIO2                                                          
         MVC   0(2,R1),=H'50'        46 + 4 BYTES FOR QSAM                      
*&&DO                                                                           
         MVC   30(8,R1),=CL8'NEWSTYLE'                                          
         MVC   30+16(8,R1),MOCFTOID                                             
         MVC   30+16+8(9,R1),=CL9',$SPTDARE'                                    
         MVC   30+60(4,R1),=C'T214'   USERID+PID (44 BYTES)                     
         L     R1,AIO2                                                          
         MVC   0(2,R1),=H'98'        94 + 4 BYTES FOR QSAM                      
*&&                                                                             
         GOTOR WRKRSEND,DMCB,AIO2                                               
*                                                                               
         L     R1,AIO2                                                          
         XC    0(256,R1),0(R1)                                                  
         LA    R1,4(R1)                                                         
         MVC   0(4,R1),=F'2102'                                                 
         MVC   4(6,R1),=C'000002'                                               
         MVC   10(20,R1),=CL20'$SPTDARE TO SPOT/BUY'                            
         LA    R1,30(R1)                                                        
         USING LAYOUTD,R1                                                       
         MVC   LAYMED,QMED                                                      
         MVC   LAYBYR,QBUYER                                                    
         MVC   LAYCLT,QCLT                                                      
*                                                                               
         MVC   LAYPRD,QPRD1                                                     
         BRAS  RE,ISITPOLE         IS IT A BRAND ON A POL ESTIMATE?             
         BNE   *+10                NO                                           
         MVC   LAYPRD,=C'POL'      YES, MUST HAVE POL IN BUY HEADER             
*                                                                               
         MVC   LAYEST,QEST1                                                     
         MVC   LAYSTA,QSTA                                                      
*                                                                               
         MVC   LAYFLT,QFLTNUM      EXTRA FIELDS NEEDED BY MGEACC                
         MVC   LAYGRPCD,QMGGROUP                                                
         MVC   LAYCORT,QCSHTRDE                                                 
         MVC   LAYPRD1,QPRD1                                                    
         OC    LAYPRD1,=CL3'   '                                                
         MVC   LAYPRD2,QPRD2                                                    
         OC    LAYPRD2,=CL3'   '                                                
*                                                                               
         CLC   LAYPRD2,=CL3'   '   PIGGYBACK?                                   
         BNH   *+10                                                             
         MVC   LAYPRD,=C'POL'      YEAH, NEEDS TO BE POL FOR SPMGEACC           
*                                                                               
         LA    R1,LAYEND                                                        
         DROP  R1                                                               
         L     RE,AIO2                                                          
         SR    R1,RE                                                            
         STH   R1,0(RE)            L'DATA + 4 BYTES FOR QSAM                    
*                                                                               
         GOTOR WRKRSEND,DMCB,AIO2                                               
*                                                                               
         BRAS  RE,WRKRCLOS                                                      
*                                                                               
         L     RE,AWRKRIOA         RESET WORKER IO AREA FOR EDICT RECS          
         MVI   0(RE),0                                                          
         MVI   1(RE),2                                                          
*                                                                               
         BAS   RE,UPDATSB1                                                      
*                                                                               
MCNFMYES BRAS  RE,BLDCOLOR                                                      
         BRAS  RE,DAREMAIL                                                      
         B     YES                                                              
*                                                                               
MCNFMNO  B     NO                                                               
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* TEST IF ESTIMATE IS LOCKED                                                    
***********************************************************************         
ESTLOCKD NTR1                                                                   
         XC    ESTPW,ESTPW         CLEAR OUT ANY PW PCT                         
         XC    ESTCOST2,ESTCOST2   CLEAR OUT ANY COST2 PCT                      
         XC    ESTLOKYM,ESTLOKYM   CLEAR OUT ANY LOCK YEAR/MONTH                
         MVI   BYTE,0                                                           
         NI    BITFLAG2,X'FF'-BF2ELCKD-BF2PWLCK                                 
*                                                                               
ELCKD10  LA    R6,KEY                                                           
         USING EKEY,R6                                                          
         XC    KEY,KEY                                                          
         MVC   EKEYAM,BAGYMD       TEST IF POL EST LOCKED FIRST                 
         MVC   EKEYCLT,BCLT                                                     
*                                                                               
         CLI   BYTE,0                                                           
         BNE   *+14                                                             
         MVC   EKEYPRD,=C'POL'                                                  
         B     ELCKD15                                                          
*                                                                               
         CLI   BYTE,1                                                           
         BNE   *+14                                                             
         MVC   EKEYPRD,QPRD1                                                    
         B     ELCKD15                                                          
*                                                                               
         CLI   BYTE,2                                                           
         BNE   *+10                                                             
         MVC   EKEYPRD,QPRD2                                                    
*                                                                               
ELCKD15  OC    EKEYPRD,EKEYPRD     CAN'T HAVE A NULL PRODUCT                    
         BZ    ELCKDX                                                           
*                                                                               
         MVC   EKEYEST,BEST                                                     
*                                                                               
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(ELEN-EKEY),KEYSAVE                                           
         BNE   ELCKD20                                                          
         TM    KEY+L'EKEY,X'0C'    HELD OR LOCKED?                              
         BZ    *+12                                                             
         OI    BITFLAG2,BF2ELCKD   YES                                          
         B     ELCKDX                                                           
*                                                                               
         OC    ESTPW,ESTPW         DO WE HAVE PW PCT ALREADY?                   
         BNZ   ELCKD20             YES                                          
         OC    ESTCOST2,ESTCOST2   DO WE HAVE PW PCT ALREADY?                   
         BNZ   ELCKD20             YES                                          
*                                                                               
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
         CLI   DMCB+8,0                                                         
         BE    ELCKD17                                                          
         L     R1,=AL4(*-T16100)   DELETED RECORD BUT KEY IS NOT                
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
ELCKD17  L     R6,AIO1                                                          
         USING ESTHDR,R6                                                        
         MVC   ESTPW,EPWPCT        COPY THE PW PCT                              
         MVC   ESTCOST2,ECOST2     COPY THE COST2 PCT                           
*                                                                               
         OC    ESTLOKYM,ESTLOKYM                                                
         BNZ   *+10                                                             
         MVC   ESTLOKYM,ELOCKYM    COPY THE LOCKYM                              
         DROP  R6                                                               
*                                                                               
ELCKD20  CLI   BYTE,2              TEST ALL PRODUCT COMBINATIONS?               
         BNL   ELCKDX              YES, EST IS NOT LOCKED                       
*                                                                               
         LLC   R1,BYTE             BUMP TO NEXT PRODUCT                         
         LA    R1,1(R1)                                                         
         STC   R1,BYTE                                                          
         B     ELCKD10                                                          
*                                                                               
ELCKDX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ PW STATUS RECORD                                              
***********************************************************************         
GETPWC2  NTR1                                                                   
         BAS   RE,FNDMRKT          FIND MARKET NUMBER FOR STATION               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PWFKEY,R6                                                        
         MVC   PWKTYP,=X'0D7A'                                                  
         MVC   PWKAGMD,BAGYMD                                                   
         MVC   PWKCLT,BCLT                                                      
         MVC   PWKPRD,BPRD                                                      
         MVC   PWKEST,BEST                                                      
         MVC   PWKMKT,BMKTSTA                                                   
         DROP  R6                                                               
*                                                                               
         BRAS  RE,HIGH                                                          
         CLC   KEY(13),KEYSAVE       TEST FOUND                                 
         BNE   GETPWX                IF NOT, CAN'T BE LOCKED                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
*                                                                               
         L     R6,AIO1                                                          
         USING PWRECD,R6                                                        
         TM    PWGNFLG,X'C0'         ANY BUY LOCKED BITS?                       
         BZ    *+8                                                              
         OI    BITFLAG2,BF2PWLCK     YES                                        
         DROP  R6                                                               
*                                                                               
GETPWX   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ STATION RECORD FOR THE MARKET NUMBER                          
***********************************************************************         
FNDMRKT  NTR1                                                                   
         XC    KEY,KEY             READ STATION MASTER RECORD                   
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         LA    R6,KEY                                                           
         USING STAKEY,R6                                                        
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,QMED                                                     
         MVC   STAKCALL,QSTA                                                    
         CLI   STAKCALL+4,C' '                                                  
         BH    *+8                                                              
         MVI   STAKCALL+4,C'T'                                                  
*                                                                               
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,QCLT                                                     
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMRDHI,STATION,KEY,AIO1                            
         L     R6,AIO1                                                          
         CLC   KEY(STAKCLT-STAKEY),0(R6)  SAME UPTO CLIENT?                     
         JNE   *+2                                                              
         CLC   STAKCLT,QCLT                                                     
         BE    FMRKT10                                                          
         LA    R6,KEY                                                           
         MVI   STAKCLT,C'0'                                                     
         MVC   STAKCLT+1(L'STAKCLT-1),STAKCLT                                   
         GOTO1 VDATAMGR,DMCB,DMRDHI,STATION,KEY,AIO1                            
         CLC   KEY(STAKFILL-STAKEY),0(R6)  SAME UPTO CLIENT?                    
         JNE   *+2                                                              
*                                                                               
FMRKT10  L     R6,AIO1                                                          
         USING STARECD,R6                                                       
         GOTO1 VMSPACK,DMCB,SMKT,STAKCALL,BMKTSTA                               
         DROP  R6                                                               
*                                                                               
FMRKTX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FIGURES OUT IF THE ESTIMATE IS A POL ESTIMATE                                 
*                                                                               
* ON EXIT:     (CC)                EQ, BRAND ON A POL EST                       
*                                  NE, NOT BRAND ON POL EST                     
***********************************************************************         
ISITPOLE NTR1  BASE=*,LABEL=*                                                   
         CLI   SVCPRF00,C'0'       BRAND BUYING CLIENT?                         
         JNE   ISITPNO             NO                                           
*                                                                               
         XC    KEY,KEY             YES, DO WE HAVE A POL ESTIMATE?              
         LA    R4,KEY                                                           
         USING ESTHDRD,R4                                                       
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,BEST                                                     
         DROP  R4                                                               
*                                                                               
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(L'EKEY),KEYSAVE                                              
         JNE   ISITPNO             DON'T HAVE A POL ESTIMATE                    
*                                                                               
ISITPYES B     YES                 GOT A POL ESTIMATE                           
*                                                                               
ISITPNO  B     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE FAX DELIVERY NOTIFICATION                                       
***********************************************************************         
DAREDLFX NTR1  BASE=*,LABEL=*                                                   
         L     R9,SRPARMSD.SRQASYSF                                             
*                                                                               
         USING RDLNFAXD,R7                                                      
DLFAXD   USING RTN2SNDR,RDFXRTRN                                                
*                                                                               
         MVC   AGENCY,DLFAXD.RTNPWRCD                                           
*                                                                               
         MVI   QMED,C'T'                                                        
         CLI   DLFAXD.RTNAGYMD+1,C'2'  (---MD OF AGYMD IS 2?)                   
         BNE   *+8                                                              
         MVI   QMED,C'R'                                                        
*                                                                               
         GOTO1 VHEXIN,DMCB,DLFAXD.RTNAGYMD,BAGYMD,2                             
*                                                                               
         MVC   QRETURN,RDFXRTRN    COPY THIS VALUE                              
*                                                                               
         GOTO1 VHEXIN,DMCB,DLFAXD.RTNSYSID,BYTE,2                               
         BAS   RE,SPANKED                                                       
*                                                                               
         NI    MISCFLG1,X'FF'-MF1XMTUP   NO UPDATES TO XMT ELEM                 
*                                                                               
         CLC   BYTE,THESYSID       WRONG FACPAK?                                
         BNE   DLFAXNO             YES, AGENCY DOESN'T BELONG HERE              
         DROP  DLFAXD                                                           
*                                                                               
         GOTO1 CALCORDR,DMCB,RDFXORDR   CONVERT ORDER NUMBER TO BINARY          
         BNE   DLFAXNO                                                          
*                                                                               
         MVC   USERID,RDFXTOID                                                  
         BAS   RE,SWTCHCTL         SWITCH CONTROL TO SPOT SYSTEM                
         BNE   DLFAXNO             NO                                           
*                                                                               
         BRAS  RE,GETORDER         READ ORDER RECORD INTO AIO1                  
         BNE   DLFAXNO                                                          
*                                                                               
         NI    MISCFLG1,X'FF'-MF1NOXMT    ASSUME XMT ELEM EXISTS                
         SR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
DLFAX10  CLI   0(R6),0                                                          
         BNE   DLFAX15                                                          
         LHI   R1,REFORDNT         ORD NOT TRANSMITTED                          
         BRAS  RE,SNDERROR                                                      
         B     DLFAXNO                                                          
*                                                                               
DLFAX15  CLI   0(R6),DOXMTELQ                                                   
         BE    DLFAX20                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     DLFAX10                                                          
*                                                                               
         USING DOXMTELD,R6                                                      
DLFAX20  GOTO1 VDATCON,DMCB,(0,RDFXDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,RDFXTIME,DUB+L'DOXMTDND,L'RDFXTIME                   
*                                                                               
         OC    DOXMTDND,DOXMTDND   IF NO DELIVERY DATE OR TIME ALREADY          
         BZ    DLFAX30                                                          
         OC    DOXMTDNT,DOXMTDNT                                                
         BZ    DLFAX30             THEN USE EDICT RECORD'S DATE/TIME            
*                                                                               
         CLC   DOXMTDND,DUB               IS ELEM'S DATE MORE RECENT?           
         BH    DLFAXNO                    YES, IGNORE THIS RECORD               
         BL    DLFAX30                    NO, OLDER                             
         CLC   DOXMTDNT,DUB+L'DOXMTDND    IS ELEM'S TIME MORE RECENT?           
         BNL   DLFAXNO                    YES OR SAME, IGNORE THIS REC          
*                                                                               
DLFAX30  XC    DOXMTDND,DOXMTDND                                                
         MVC   DOXMTDNT,DLNFRID    SAVE WHERE DELNOT CAME FROM                  
*                                                                               
         MVC   DOXMTSTD,DUB                                                     
         MVC   DOXMTSTT,DUB+L'DOXMTDND                                          
         MVI   DOXMTSTA,QFAXDLVD                                                
         CLC   DOXMTDID,=X'FFFD'   IS IT ACTUALLY AN EMAIL?                     
         BNE   *+8                                                              
         MVI   DOXMTSTA,DEMDLVD                                                 
         MVC   SVSTAT,DOXMTSTA                                                  
         DROP  R6                                                               
*                                                                               
DLFAX99  OI    MISCFLG1,MF1XMTUP                                                
*                                                                               
DLFAX100 SR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
DLFAX110 CLI   0(R6),0                                                          
         BNE   DLFAX115                                                         
         TM    MISCFLG1,MF1NOXMT          DOES IT HAVE XMT ELEMS?               
         BZ    DLFAXNO                    YES, THEN DON'T GIVE ERROR            
         LHI   R1,REFORDNT         ORD NOT TRANSMITTED                          
         BRAS  RE,SNDERROR                                                      
         B     DLFAXNO                                                          
*                                                                               
DLFAX115 CLI   0(R6),DOSTELQ                                                    
         BE    DLFAX120                                                         
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     DLFAX110                                                         
*                                                                               
         USING DOSTELD,R6                                                       
DLFAX120 GOTO1 VDATCON,DMCB,(0,RDFXDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,RDFXTIME,DUB+L'DOSTDATE,L'RDFXTIME                   
*                                                                               
         CLI   DOSTSTAT,DFXSENT                                                 
         BE    DLFAX140                                                         
         CLI   DOSTSTAT,DFXRSNT                                                 
         BE    DLFAX140                                                         
         CLI   DOSTSTAT,DEMSENT                                                 
         BE    DLFAX140                                                         
*                                                                               
         CLC   DOSTDATE,DUB               IS ELEM'S DATE MORE RECENT?           
         BH    DLFAXNO                    YES, IGNORE THIS RECORD               
         BL    DLFAX130                   NO, OLDER                             
         CLC   DOSTTIME,DUB+L'DOSTDATE    IS ELEM'S TIME MORE RECENT?           
         BNL   DLFAXNO                    YES OR SAME, IGNORE THIS REC          
*                                                                               
DLFAX130 CLI   DOSTSTAT,DFXDLVD                                                 
         BE    *+12                                                             
         CLI   DOSTSTAT,DEMDLVD                                                 
         BNE   DLFAXNO                                                          
         MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         MVC   DOSTIDNM,DLNFRID                                                 
         MVC   SVSTAT,DOSTSTAT                                                  
         CLI   DOSTSTAT,DEMDLVD                                                 
         BNE   DLFAX190                                                         
         MVC   DOSTIDNM,=X'FFFD'                                                
         B     DLFAX190                                                         
*                                                                               
DLFAX140 LA    R2,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
DLFXDOST USING DOSTELD,R2                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   DLFXDOST.DOSTEL,DOSTELQ                                          
         MVI   DLFXDOST.DOSTLEN,DOSTLNQ2                                        
         MVC   DLFXDOST.DOSTDATE,DUB                                            
         MVC   DLFXDOST.DOSTTIME,DUB+L'DOSTDATE                                 
         MVI   DLFXDOST.DOSTSTAT,DFXDLVD                                        
         MVC   DLFXDOST.DOSTIDNM,DLNFRID                                        
         CLI   DOSTSTAT,DEMSENT                                                 
         BNE   DLFAX145                                                         
         MVI   DLFXDOST.DOSTSTAT,DEMDLVD                                        
         MVC   DLFXDOST.DOSTIDNM,=X'FFFD'                                       
*                                                                               
DLFAX145 MVC   SVSTAT,DLFXDOST.DOSTSTAT                                         
         DROP  DLFXDOST,R6                                                      
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)  R6 = A(FIRST ELEMENT IN RECORD)            
DLFAX150 CLI   0(R6),0          ADD AFTER ALL TRANSMISSION ELEMENTS             
         BE    DLFAX160         AND BEFORE AN AUDIT ELEMENT                     
         CLI   0(R6),DORPELQ2                                                   
         BE    DLFAX160                                                         
         CLI   0(R6),DOSTELQ                                                    
         BNL   DLFAX160                                                         
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     DLFAX150                                                         
*                                                                               
DLFAX160 GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
*                                                                               
DLFAX190 XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         MVC   AIO,AIO1                                                         
         BRAS  RE,PUT                                                           
*                                                                               
         BRAS  RE,CHKFRCHG         BUILD CHECK FOR CHANGES KEY                  
*                                                                               
         BAS   RE,UPDATSB1                                                      
*                                                                               
DLFAXYES BRAS  RE,BLDCOLOR                                                      
         BRAS  RE,DAREMAIL                                                      
         B     YES                                                              
*                                                                               
DLFAXNO  TM    MISCFLG1,MF1XMTUP                                                
         BNZ   DLFAX190                                                         
         B     NO                                                               
         DROP  R7                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE FAX CANCELLATION                                                
***********************************************************************         
DARECNFX NTR1  BASE=*,LABEL=*                                                   
         L     R9,SRPARMSD.SRQASYSF                                             
*                                                                               
         USING RDLNFAXD,R7                                                      
CNFAXD   USING RTN2SNDR,RDFXRTRN                                                
*                                                                               
         MVI   QMED,C'T'                                                        
         CLI   CNFAXD.RTNAGYMD+1,C'2'  (---MD OF AGYMD IS 2?)                   
         BNE   *+8                                                              
         MVI   QMED,C'R'                                                        
*                                                                               
         GOTO1 VHEXIN,DMCB,CNFAXD.RTNAGYMD,BAGYMD,2                             
         MVC   AGENCY,CNFAXD.RTNPWRCD                                           
         GOTO1 VHEXIN,DMCB,CNFAXD.RTNSYSID,BYTE,2                               
*                                                                               
         BAS   RE,SPANKED                                                       
*                                                                               
         NI    MISCFLG1,X'FF'-MF1XMTUP                                          
*                                                                               
         CLC   BYTE,THESYSID       WRONG FACPAK?                                
         BNE   CNFAXNO             YES, AGENCY DOESN'T BELONG HERE              
         DROP  CNFAXD                                                           
*                                                                               
         MVC   QRETURN,RDFXRTRN    COPY THIS VALUE                              
*                                                                               
         GOTO1 CALCORDR,DMCB,RDFXORDR   CONVERT ORDER NUMBER TO BINARY          
         BNE   CNFAXNO                                                          
*                                                                               
         MVC   USERID,RDFXTOID                                                  
         BAS   RE,SWTCHCTL         SWITCH CONTROL TO SPOT SYSTEM                
         BNE   CNFAXNO                                                          
*                                                                               
         BRAS  RE,GETORDER         READ ORDER RECORD INTO AIO1                  
         BNE   CNFAXNO                                                          
*                                                                               
         NI    MISCFLG1,X'FF'-MF1NOXMT   ASSUME XMT ELEM EXISTS                 
         SR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
CNFAX10  CLI   0(R6),0                                                          
         BNE   CNFAX15                                                          
         LHI   R1,REFORDNT          ORD NOT TRANSMITTED                         
         BRAS  RE,SNDERROR                                                      
         B     CNFAXNO                                                          
*                                                                               
CNFAX15  CLI   0(R6),DOXMTELQ                                                   
         BE    CNFAX20                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CNFAX10                                                          
*                                                                               
         USING DOXMTELD,R6                                                      
CNFAX20  GOTO1 VDATCON,DMCB,(0,RDFXDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,RDFXTIME,DUB+L'DOXMTDND,L'RDFXTIME                   
*                                                                               
         OC    DOXMTDND,DOXMTDND   IF NO DELIVERY DATE OR TIME ALREADY          
         BZ    CNFAX30                                                          
         OC    DOXMTDNT,DOXMTDNT                                                
         BZ    CNFAX30             THEN USE EDICT RECORD'S DATE/TIME            
*                                                                               
         CLC   DOXMTDND,DUB               IS ELEM'S DATE MORE RECENT?           
         BH    CNFAXNO                    YES, IGNORE THIS RECORD               
         BL    CNFAX30                    NO, OLDER                             
         CLC   DOXMTDNT,DUB+L'DOXMTDND    IS ELEM'S TIME MORE RECENT?           
         BNL   CNFAXNO                    YES OR SAME, IGNORE THIS REC          
*                                                                               
CNFAX30  MVC   DOXMTDND,DUB                                                     
         MVC   DOXMTDNT,DUB+L'DOXMTDND                                          
*                                                                               
         MVC   DOXMTSTD,DOXMTDND                                                
         MVC   DOXMTSTT,DOXMTDNT                                                
         MVI   DOXMTSTA,QFAXCNCL                                                
         CLC   DOXMTDID,=X'FFFD'          IS AN EMAIL?                          
         BNE   *+8                                                              
         MVI   DOXMTSTA,QERRORED                                                
         MVC   SVSTAT,DOXMTSTA                                                  
         DROP  R6                                                               
*                                                                               
CNFAX99  OI    MISCFLG1,MF1XMTUP                                                
*                                                                               
CNFAX100 SR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
CNFAX110 CLI   0(R6),0                                                          
         BNE   CNFAX115                                                         
         TM    MISCFLG1,MF1NOXMT          DOES IT HAVE XMT ELEMS?               
         BZ    CNFAXNO                    YES, THEN DON'T GIVE ERROR            
         LHI   R1,REFORDNT         ORD NOT TRANSMITTED                          
         BRAS  RE,SNDERROR                                                      
         B     CNFAXNO                                                          
*                                                                               
CNFAX115 CLI   0(R6),DOSTELQ                                                    
         BE    CNFAX120                                                         
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CNFAX110                                                         
*                                                                               
         USING DOSTELD,R6                                                       
CNFAX120 GOTO1 VDATCON,DMCB,(0,RDFXDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,RDFXTIME,DUB+L'DOSTDATE,L'RDFXTIME                   
*                                                                               
         CLI   DOSTSTAT,DFXSENT                                                 
         BE    CNFAX140                                                         
         CLI   DOSTSTAT,DFXRSNT                                                 
         BE    CNFAX140                                                         
         CLI   DOSTSTAT,DEMSENT                                                 
         BE    CNFAX140                                                         
*                                                                               
         CLC   DOSTDATE,DUB               IS ELEM'S DATE MORE RECENT?           
         BH    CNFAXNO                    YES, IGNORE THIS RECORD               
         BL    CNFAX130                   NO, OLDER                             
         CLC   DOSTTIME,DUB+L'DOSTDATE    IS ELEM'S TIME MORE RECENT?           
         BNL   CNFAXNO                    YES OR SAME, IGNORE THIS REC          
*                                                                               
CNFAX130 CLI   DOSTSTAT,QFAXCNCL                                                
         BE    CNFAX135                                                         
         CLI   DOSTSTAT,QERRORED                                                
         BNE   CNFAXNO                                                          
CNFAX135 MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         MVC   SVSTAT,DOSTSTAT                                                  
         B     CNFAX190                                                         
*                                                                               
CNFAX140 LA    R2,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
CNFXDOST USING DOSTELD,R2                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   CNFXDOST.DOSTEL,DOSTELQ                                          
         MVI   CNFXDOST.DOSTLEN,DOSTLNQ                                         
         MVC   CNFXDOST.DOSTDATE,DUB                                            
         MVC   CNFXDOST.DOSTTIME,DUB+L'DOSTDATE                                 
         MVI   CNFXDOST.DOSTSTAT,QFAXCNCL                                       
         CLI   DOSTSTAT,DEMSENT       ??? IS AN EMAIL???                        
         BNE   CNFAX145                                                         
         MVI   CNFXDOST.DOSTLEN,DOSTLNQ                                         
         MVI   CNFXDOST.DOSTSTAT,QERRORED   YES: SHOW AS ERROR                  
*                                                                               
CNFAX145 MVC   SVSTAT,CNFXDOST.DOSTSTAT                                         
         DROP  CNFXDOST,R6                                                      
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)  R6 = A(FIRST ELEMENT IN RECORD)            
CNFAX150 CLI   0(R6),0          ADD AFTER ALL TRANSMISSION ELEMENTS             
         BE    CNFAX160         AND BEFORE AN AUDIT ELEMENT                     
         CLI   0(R6),DORPELQ2                                                   
         BE    CNFAX160                                                         
         CLI   0(R6),DOSTELQ                                                    
         BNL   CNFAX160                                                         
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CNFAX150                                                         
*                                                                               
CNFAX160 GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
*                                                                               
CNFAX190 XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         ST    R6,AIO                                                           
         BRAS  RE,PUT                                                           
*                                                                               
         BRAS  RE,CHKFRCHG         BUILD CHECK FOR CHANGES KEY                  
*                                                                               
         BAS   RE,UPDATSB1                                                      
*                                                                               
CNFAXYES BRAS  RE,BLDCOLOR                                                      
         BRAS  RE,DAREMAIL                                                      
         B     YES                                                              
*                                                                               
CNFAXNO  TM    MISCFLG1,MF1XMTUP                                                
         BNZ   CNFAX190                                                         
         B     NO                                                               
         DROP  R7                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE FAX ERROR                                                       
***********************************************************************         
DAREERFX NTR1  BASE=*,LABEL=*                                                   
         L     R9,SRPARMSD.SRQASYSF                                             
*                                                                               
         USING RDLNFAXD,R7                                                      
ERFAXD   USING RTN2SNDR,RDFXRTRN                                                
*                                                                               
         MVI   QMED,C'T'                                                        
         CLI   ERFAXD.RTNAGYMD+1,C'2'  (---MD OF AGYMD IS 2?)                   
         BNE   *+8                                                              
         MVI   QMED,C'R'                                                        
*                                                                               
         GOTO1 VHEXIN,DMCB,ERFAXD.RTNAGYMD,BAGYMD,2                             
         MVC   AGENCY,ERFAXD.RTNPWRCD                                           
         GOTO1 VHEXIN,DMCB,ERFAXD.RTNSYSID,BYTE,2                               
*                                                                               
         BAS   RE,SPANKED                                                       
*                                                                               
         NI    MISCFLG1,X'FF'-MF1XMTUP                                          
*                                                                               
         CLC   BYTE,THESYSID       WRONG FACPAK?                                
         BNE   ERFAXNO             YES, AGENCY DOESN'T BELONG HERE              
         DROP  ERFAXD                                                           
*                                                                               
         MVC   QRETURN,RDFXRTRN    COPY THIS VALUE                              
*                                                                               
         GOTO1 CALCORDR,DMCB,RDFXORDR   CONVERT ORDER NUMBER TO BINARY          
         BNE   ERFAXNO                                                          
*                                                                               
         MVC   USERID,RDFXTOID                                                  
         BAS   RE,SWTCHCTL         SWITCH CONTROL TO SPOT SYSTEM                
         BNE   ERFAXNO                                                          
*                                                                               
         BRAS  RE,GETORDER         READ ORDER RECORD INTO AIO1                  
         BNE   ERFAXNO                                                          
*                                                                               
         NI    MISCFLG1,X'FF'-MF1NOXMT    ASSUME XMT ELEM EXISTS                
         SR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
ERFAX10  CLI   0(R6),0                                                          
         BNE   ERFAX15                                                          
         LHI   R1,REFORDNT          ORD NOT TRANSMITTED                         
         BRAS  RE,SNDERROR                                                      
         B     ERFAXNO                                                          
*                                                                               
ERFAX15  CLI   0(R6),DOXMTELQ                                                   
         BE    ERFAX20                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     ERFAX10                                                          
*                                                                               
         USING DOXMTELD,R6                                                      
ERFAX20  GOTO1 VDATCON,DMCB,(0,RDFXDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,RDFXTIME,DUB+L'DOXMTDND,L'RDFXTIME                   
*                                                                               
         OC    DOXMTDND,DOXMTDND   IF NO DELIVERY DATE OR TIME ALREADY          
         BZ    ERFAX30                                                          
         OC    DOXMTDNT,DOXMTDNT                                                
         BZ    ERFAX30             THEN USE EDICT RECORD'S DATE/TIME            
*                                                                               
         CLC   DOXMTDND,DUB               IS ELEM'S DATE MORE RECENT?           
         BH    ERFAXNO                    YES, IGNORE THIS RECORD               
         BL    ERFAX30                    NO, OLDER                             
         CLC   DOXMTDNT,DUB+L'DOXMTDND    IS ELEM'S TIME MORE RECENT?           
         BNL   ERFAXNO                    YES OR SAME, IGNORE THIS REC          
*                                                                               
ERFAX30  MVC   DOXMTDND,DUB                                                     
         MVC   DOXMTDNT,DUB+L'DOXMTDND                                          
*                                                                               
         MVC   DOXMTSTD,DOXMTDND                                                
         MVC   DOXMTSTT,DOXMTDNT                                                
         MVI   DOXMTSTA,QERRORED                                                
         MVI   SVSTAT,QERRORED                                                  
         DROP  R6                                                               
*                                                                               
ERFAX99  OI    MISCFLG1,MF1XMTUP                                                
*                                                                               
ERFAX100 SR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
ERFAX110 CLI   0(R6),0                                                          
         BNE   ERFAX115                                                         
         TM    MISCFLG1,MF1NOXMT          DOES IT HAVE XMT ELEMS?               
         BZ    ERFAXNO                    YES, THEN DON'T GIVE ERROR            
         LHI   R1,REFORDNT         ORD NOT TRANSMITTED                          
         BRAS  RE,SNDERROR                                                      
         B     ERFAXNO                                                          
*                                                                               
ERFAX115 CLI   0(R6),DOSTELQ                                                    
         BE    ERFAX120                                                         
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     ERFAX110                                                         
*                                                                               
         USING DOSTELD,R6                                                       
ERFAX120 GOTO1 VDATCON,DMCB,(0,RDFXDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,RDFXTIME,DUB+L'DOSTDATE,L'RDFXTIME                   
*                                                                               
         CLI   DOSTSTAT,DFXSENT                                                 
         BE    ERFAX140                                                         
         CLI   DOSTSTAT,DFXRSNT                                                 
         BE    ERFAX140                                                         
         CLI   DOSTSTAT,DEMSENT                                                 
         BE    ERFAX140                                                         
*                                                                               
         CLC   DOSTDATE,DUB               IS ELEM'S DATE MORE RECENT?           
         BH    ERFAXNO                    YES, IGNORE THIS RECORD               
         BL    ERFAX130                   NO, OLDER                             
         CLC   DOSTTIME,DUB+L'DOSTDATE    IS ELEM'S TIME MORE RECENT?           
         BNL   ERFAXNO                    YES OR SAME, IGNORE THIS REC          
*                                                                               
ERFAX130 CLI   DOSTSTAT,QERRORED                                                
         BNE   ERFAXNO                                                          
         MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         B     ERFAX190                                                         
*                                                                               
ERFAX140 LA    R2,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
ERFXDOST USING DOSTELD,R2                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   ERFXDOST.DOSTEL,DOSTELQ                                          
         MVI   ERFXDOST.DOSTLEN,DOSTLNQ                                         
         MVC   ERFXDOST.DOSTDATE,DUB                                            
         MVC   ERFXDOST.DOSTTIME,DUB+L'DOSTDATE                                 
         MVI   ERFXDOST.DOSTSTAT,QERRORED                                       
         DROP  ERFXDOST,R6                                                      
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)  R6 = A(FIRST ELEMENT IN RECORD)            
ERFAX150 CLI   0(R6),0          ADD AFTER ALL TRANSMISSION ELEMENTS             
         BE    ERFAX160         AND BEFORE AN AUDIT ELEMENT                     
         CLI   0(R6),DORPELQ2                                                   
         BE    ERFAX160                                                         
         CLI   0(R6),DOSTELQ                                                    
         BNL   ERFAX160                                                         
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     ERFAX150                                                         
*                                                                               
ERFAX160 GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
*                                                                               
ERFAX190 MVI   SVSTAT,QERRORED                                                  
*                                                                               
ERFAX300 L     R6,AIO1             REMOVE ANY OLD ERROR COMMENTS                
         USING DOKEY,R6                                                         
         XR    R0,R0                                                            
         LA    R6,DORFRST                                                       
ERFAX305 CLI   0(R6),0                                                          
         BE    ERFAX315                                                         
         CLI   0(R6),DOCOMELQ                                                   
         BE    ERFAX310                                                         
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     ERFAX305                                                         
ERFAX310 GOTO1 VRECUP,DMCB,(C'S',AIO1),(R6)                                     
         B     ERFAX305                                                         
*                                                                               
ERFAX315 LA    R2,ELEM             ADD THE ERROR COMMENT TO RECORD              
         USING DOCOMELD,R2                                                      
         XC    ELEM,ELEM                                                        
         MVI   DOCOMEL,DOCOMELQ                                                 
         MVI   DOCOMLEN,DOCOMOVH+L'RDNTEFLG                                     
         MVC   DOCOMTXT(3),RDFXERR  COPY THE NUMBER FOR NOW                     
         GOTO1 VRECUP,DMCB,(C'S',AIO1),ELEM,(R6)                                
*                                                                               
         XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         ST    R6,AIO                                                           
         BRAS  RE,PUT                                                           
*                                                                               
         BRAS  RE,CHKFRCHG         BUILD CHECK FOR CHANGES KEY                  
*                                                                               
         BAS   RE,UPDATSB1                                                      
*                                                                               
ERFAXYES BRAS  RE,BLDCOLOR                                                      
         BRAS  RE,DAREMAIL                                                      
         B     YES                                                              
*                                                                               
ERFAXNO  TM    MISCFLG1,MF1XMTUP                                                
         BNZ   ERFAX300                                                         
         B     NO                                                               
         DROP  R7                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE MAKEGOOD HEADER                                                 
***********************************************************************         
DAREMKGX NTR1  BASE=*,LABEL=*                                                   
         L     R9,SRPARMSD.SRQASYSF                                             
*                                                                               
         USING PMKGHDRD,R7                                                      
MKHDRD   USING RTN2SNDR,PMKGRTNS                                                
         MVI   QMED,C'T'                                                        
         CLI   MKHDRD.RTNAGYMD+1,C'2'  (---MD OF AGYMD IS 2?)                   
         BNE   *+8                                                              
         MVI   QMED,C'R'                                                        
*                                                                               
         GOTO1 VHEXIN,DMCB,MKHDRD.RTNAGYMD,BAGYMD,2                             
         MVC   AGENCY,MKHDRD.RTNPWRCD                                           
         GOTO1 VHEXIN,DMCB,MKHDRD.RTNSYSID,BYTE,2                               
         BAS   RE,SPANKED          AGY GOT SPANKED TO DIFF ADV?                 
         CLC   BYTE,THESYSID       WRONG FACPAK?                                
         BNE   MKHDRNO             YES                                          
         DROP  MKHDRD                                                           
*                                                                               
         MVI   DMCB,X'01'          SWITCH TO SERVICE SYSTEM                     
         BAS   RE,SWTCHSYS           OTHERWISE DEATH IN CALLOV                  
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB,X'01'                                                       
         GOTO1 VCALLOV,DMCB                  GET PHASE-LIST ADDRESS             
         CLI   DMCB+4,X'FF'                                                     
         JE    *+2                                                              
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,SRPARMS,0           RUN $DMG (T16101)                  
*                                                                               
         CLI   DMCB+4,X'01'                                                     
         JE    EXITPRG             FAILED DUE TO FILE ACCESS                    
*                                  -WAIT AND TRY AGAIN.                         
         BAS   RE,UPDATSB1                                                      
*                                                                               
MKHDRYES B     YES                                                              
*                                                                               
MKHDRNO  B     NO                                                               
         DROP  R7                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE MESSAGES THAT GO TO $RDR (WILL BE OVERLAY SRDAR10)              
*                                                                               
* $RDR LOOKS UP THE USERID AND THEN SEES IF THIS IS THE CORRECT REPPAK          
*   TO PROCESS THE PQ MESSAGE.  DON'T NEED SRDAR00 TO DO THIS WORK, SO          
*   ALL THIS DOES IS CALL SRDAR10 WHEN IT SEES AN AGYHDR.                       
***********************************************************************         
DARERDAR NTR1  BASE=*,LABEL=*                                                   
         L     R9,SRPARMSD.SRQASYSF                                             
*                                                                               
         MVI   DMCB,X'01'          SWITCH TO SERVICE SYSTEM                     
         BAS   RE,SWTCHSYS           OTHERWISE DEATH IN CALLOV                  
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB,X'10'                                                       
         GOTO1 VCALLOV,DMCB                  GET PHASE-LIST ADDRESS             
         CLI   DMCB+4,X'FF'                                                     
         JE    *+2                                                              
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,SRPARMS,0           RUN $RDR (T16110)                  
*                                                                               
         CLI   DMCB+4,X'02'                                                     
         JE    DRDARUSB            CAN'T PROCESS IT IN THIS FACPAK              
*                                  -MOVE ON TO NEXT ONE.                        
         CLI   DMCB+4,X'01'        FILE ACCESS?                                 
         JNE   DRDARUSB            YES                                          
*                                  NO, BUT                                      
         CLI   THESYSID,1           ARE WE ON THE TST SYSTEM?                   
         BE    DRDARUSB              YES, DON'T WAIT                            
         CLI   THESYSID,15           OR ON THE FQA SYSTEM?                      
         BNE   EXITPRG                NO, FAILED DUE TO FILE ACCESS             
*                                      -WAIT AND TRY AGAIN.                     
DRDARUSB BAS   RE,UPDATSB1                                                      
*                                                                               
DRDARYES B     YES                                                              
*                                                                               
DRDARNO  B     NO                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESSES THE MAKEGOOD ORDER CANCELLATION                                     
***********************************************************************         
MKGDCNCL NTR1  BASE=*,LABEL=*                                                   
         L     R9,SRPARMSD.SRQASYSF                                             
*                                                                               
         USING MOFRCAND,R7                                                      
MCNCLD   USING RTN2SNDR,MOCNRTNS                                                
*                                                                               
         MVI   QMED,C'T'                                                        
         CLI   MCNCLD.RTNAGYMD+1,C'2'  (---MD OF AGYMD IS 2?)                   
         BNE   *+8                                                              
         MVI   QMED,C'R'                                                        
*                                                                               
         GOTO1 VHEXIN,DMCB,MCNCLD.RTNAGYMD,BAGYMD,2                             
         MVC   AGENCY,MCNCLD.RTNPWRCD                                           
         GOTO1 VHEXIN,DMCB,MCNCLD.RTNSYSID,BYTE,2                               
         BAS   RE,SPANKED                                                       
         CLC   BYTE,THESYSID       WRONG FACPAK?                                
         BNE   MCNCLNO             YES, AGENCY DOESN'T BELONG HERE              
*                                                                               
         DROP  MCNCLD                                                           
*                                                                               
         MVC   QREPCON,MOCNRPCN    COPY THESE VALUES                            
         MVC   QRETURN,MOCNRTNS                                                 
*                                                                               
         GOTO1 CALCORDR,DMCB,MOCNORDR   CONVERT ORDER NUMBER TO BINARY          
         BNE   MCNCLNO                                                          
*                                                                               
         MVC   USERID,MOCNTOID                                                  
         BAS   RE,SWTCHCTL         SWITCH CONTROL TO SPOT SYSTEM                
         BNE   MCNCLNO                                                          
*                                                                               
         BRAS  RE,GETORDER         READ ORDER RECORD IN AIO1                    
         BNE   MCNCLNO                                                          
*                                                                               
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
         MVC   QBUYER,DOIDBYR      SAVE THE BUYER CODE                          
         MVC   BCLT,DOIDCLT                                                     
         GOTO1 VCLUNPK,DMCB,DOIDCLT,QCLT                                        
         MVC   BPRD,DOIDPRD                                                     
         MVC   QPRD1(L'DOIDPRD),DOIDPRD                                         
         MVC   QPRD2(L'DOIDPRD2),DOIDPRD2                                       
         MVC   BEST,DOIDEST                                                     
         EDIT  (B1,DOIDEST),(3,QEST1),FILL=0                                    
         XC    WORK,WORK                                                        
         MVC   WORK+2(L'DOISTA),DOISTA                                          
         GOTO1 VMSUNPK,DMCB,WORK,WORK+10,QSTA                                   
         CLI   QSTA+4,C' '                                                      
         BNE   *+8                                                              
         MVI   QSTA+4,C'T'                                                      
         MVC   QMGGROUP,MOCNOFRI   MAKEGOOD GROUP CODE                          
*                                                                               
         XC    KEY,KEY             READ STATION MASTER RECORD                   
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),QMED                                                    
         XC    BMKTSTA,BMKTSTA                                                  
         MVC   BMKTSTA+2(L'DOISTA),DOISTA                                       
         GOTO1 VMSUNPK,DMCB,BMKTSTA,FULL,KEY+2                                  
         CLI   KEY+6,C' '          ANY BAND?                                    
         BH    *+8                                                              
         MVI   KEY+6,C'T'          NO, TELEVISION                               
         MVC   KEY+7(2),AGENCY                                                  
         GOTO1 VCLUNPK,DMCB,DOIDCLT,KEY+9                                       
*                                                                               
         DROP  R6                                                               
         GOTO1 VDATAMGR,DMCB,DMREAD,STATION,KEY,AIO1                            
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         L     R4,AIO1                                                          
         USING STARECD,R4                                                       
         GOTO1 VMSPACK,DMCB,SMKT,STAKCALL,BMKTSTA                               
         DROP  R4                                                               
*                                                                               
MCNCL010 BRAS  RE,GETCLTRC         GET THE CLIENT RECORD                        
         BE    MCNCL020                                                         
         L     R1,=AL4(*-T16100)                                                
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
MCNCL020 L     R6,AIO1                                                          
         USING CLTHDRD,R6                                                       
         MVC   SVCPRF00,CPROF+0                                                 
         GOTO1 VCLUNPK,DMCB,(CPROF+6,BCLT),QCLT                                 
         MVC   SVCOFFC,COFFICE                                                  
         GOTOR GETQPRD,DMCB,(QPRD1,QPRD1)                                       
         DROP  R6                                                               
*                                                                               
* GET OM PARITAL CONFIRM WORKFLOW PROFILE?                                      
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0OM'             ROF                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
*                                                                               
         MVC   WORK+7(3),QCLT                LOWER CASE 'S'                     
         CLI   SVCOFFC,C' '                LOWERCASE                            
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCOFFC                                               
*****                                                                           
         L     R1,SRPARMSD.SRQAUTL                                              
         USING UTLD,R1                                                          
         MVC   TUSER,SIGNON2H      FOOL GETPROF                                 
         DROP  R1                                                               
*****                                                                           
         L     RF,SRPARMSD.SRQACOMF                                             
         L     RF,CGETPROF-COMFACSD(RF)                                         
         XC    PROFOM,PROFOM                                                    
         GOTO1 (RF),DMCB,WORK,PROFOM,VDATAMGR                                   
*****                                                                           
         L     R1,SRPARMSD.SRQAUTL                                              
         USING UTLD,R1                                                          
         XC    TUSER,TUSER         RESET THE ID NUMBER IN THE UTL               
         DROP  R1                                                               
***************                                                                 
         BRAS  RE,GETOFFER         READ MAKEGOOD OFFER RECORD IN AIO1           
         BNE   MCNCLNO                                                          
*                                                                               
         BRAS  RE,VERLKUP          CHECK VERSION                                
         BNE   MCNCLNO                                                          
*                                                                               
MCNCL060 BRAS  RE,GETNOTCE         READ MAKEGOOD NOTICE RECORD IN AIO1          
         BNE   MCNCLNO                                                          
*                                                                               
         L     RF,AIO1             LOOK FOR LAST STATUS                         
         LA    R6,MNXFRST-MNXKEY(RF)                                            
         CLI   QSTA,C'0'           CABLE?                                       
         BNL   *+8                                                              
         LA    R6,MNRFRST-MNKEY(RF)                                             
         MVI   ELCODE,MNSTELQ                                                   
         BAS   RE,FIRSTEL                                                       
         BNE   MCNCL100            ERROR, COULD BE A NEW NOTICE                 
         USING MNSTELD,R6                                                       
*                                                                               
MCNCL070 CLI   MNSTSTAT,MNSTSAPP   SELF APPLIED?                                
         BNE   MCNCL080                                                         
         BRAS  RE,SENDDXX          SEND A DXX                                   
         B     MCNCL100            AND CANCEL THE MAKEGOOD                      
*                                                                               
** SHOULD ALWAYS CANCEL THE MAKEGOOD UNLESS IT IS ALREADY OKAYED.               
*                                                                               
MCNCL080 CLI   MNSTSTAT,MNSTOKAY   OKAYED BY THE REP ALREADY?                   
         BNE   MCNCL090                                                         
*                                                                               
         LHI   R1,DMGORDOK          MKGD OKAYED                                 
         BRAS  RE,SNDERROR                                                      
         B     MCNCLNO                                                          
*                                                                               
MCNCL090 CLI   MNSTSTAT,MNSTCAN    SKIP IF ALREADY CANCELLED                    
         BE    MCNCLNO             - JUST IGNORE                                
*                                                                               
         TM    BITFLAG2,BF2MGSEQ   PROBLEM WITH MG SEQ # NOT THE SAME           
         BZ    MCNCL100                                                         
         CLI   MNSTSTAT,MNSTAMND   AMEND STATUS?                                
         BNE   MCNCL100                                                         
         CLI   MOCNNEWO,C'Y'       MORE TO FOLLOW?                              
         BNE   MCNCL100                                                         
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         DROP  R6                                                               
*                                                                               
MCNCL100 XC    ELEM,ELEM           WRITE A CANCELLED ELEMENT TO REC             
         LA    R4,ELEM                                                          
         USING MNSTELD,R4                                                       
         MVI   MNSTEL,MNSTELQ                                                   
         MVI   MNSTLEN,MNSTLENQ                                                 
         GOTO1 VDATCON,DMCB,(0,MOCNDATE),(19,MNSTDATE)                          
         GOTO1 VHEXIN,DMCB,MOCNTIME,MNSTTIME,L'MOCNTIME                         
         MVC   SVDATTIM,MNSTDATE                                                
*                                                                               
         MVI   MNSTSTAT,MNSTCAN    CANCELLED STATUS                             
         CLI   MOCNNEWO,C'Y'       MORE TO FOLLOW?                              
         BNE   MCNCL110                                                         
         MVI   MNSTSTAT,MNSTCANM   YES                                          
*                                                                               
MCNCL110 MVC   SVSTAT,MNSTSTAT     SAVE STATUS FOR LATER                        
         MVI   THISISMG,C'Y'                                                    
         DROP  R4                                                               
*                                                                               
         L     R2,AIO1                                                          
         LA    R6,MNRFRST-MNKEY(R2)    LOOK FOR LAST STATUS                     
         GOTO1 VRECUP,DMCB,(C'S',AIO1),ELEM,(R6)                                
         ORG   *-2                                                              
         CLI   QSTA,C'0'           CABLE?                                       
         BL    MCNCL115             NO                                          
         LA    R6,MNXFRST-MNXKEY(R2)                                            
         ST    R6,8(R1)                                                         
         MVI   0(R1),X'FE'         CAN'T USE C'T', BC RECUP NOT CHANGED         
         LAY   RE,XSPREC            TO USE MAX REC SIZE IN DMFILTAB HAS         
         ST    RE,12(R1)            FOR XSPFIL                                  
MCNCL115 BASR  RE,RF                                                            
*                                                                               
         LAY   RF,DELNOTIC         RF = A(DELETE NOTICE RECORD)                 
         CLI   MOCNNEWO,C'Y'       DO WE DELETE THE NOTICE RECS?                
         BNE   MCNCL120             YES, GOTO TO DELNOTIC                       
         LAY   RF,XPUT              NO, THEN PUT RECORD BACK                    
         CLI   QSTA,C'0'             CABLE?                                     
         BNL   MCNCL120                YES, GO TO XPUT                          
         LAY   RF,PUT                  NO, GO TO PUT                            
MCNCL120 BASR  RE,RF                                                            
*                                                                               
NCNCL130 BRAS  RE,CHKFRCHG         BUILD CHECK FOR CHANGES KEY                  
*                                                                               
         BAS   RE,UPDATSB1                                                      
*                                                                               
         BRAS  RE,UNPSPOTS         UNPEND THE SPOTS                             
*                                                                               
         CLI   MOCNNEWO,C'Y'       ANY MORE TO FOLLOW?                          
         BE    MCNCLYES            YES                                          
*                                                                               
         BRAS  RE,DELOFFER         DELETE ALL OFFER RECORDS                     
*                                                                               
MCNCLYES BRAS  RE,BLDCOLOR                                                      
         BRAS  RE,DAREMAIL                                                      
         B     YES                                                              
*                                                                               
MCNCLNO  B     NO                                                               
*                                                                               
         EJECT                                                                  
**********                                                                      
* DELETE NOTICE RECORDS                                                         
**********                                                                      
DELNOTIC NTR1  LABEL=*                                                          
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         CLI   QSTA,C'0'               CABLE?                                   
         BNL   DLNT050                                                          
*                                                                               
         BRAS  RE,DELSREC          DELETE THE SPOT RECORD                       
*                                                                               
         XC    KEY,KEY             DELETE THE SPOT KEY TOO                      
         MVC   KEY(L'MNKEY),0(R6)                                               
         BRAS  RE,DELSKEY                                                       
         J     DLNTX                                                            
*                                                                               
DLNT050  BRAS  RE,DELXREC          DELETE THE X-SPOT RECORD                     
*                                                                               
         XC    KEY,KEY             NO, DELETE THE KEY TOO                       
         MVC   KEY(L'MNXKEY),0(R6)                                              
         BRAS  RE,DELXKEY                                                       
*                                                                               
         BRAS  RE,XSEQ                                                          
         CLC   KEY(MNXKSTTN-MNXKEY),KEYSAVE                                     
         JNE   DLNT060             NO MORE, EXIT                                
*                                                                               
         MVI   DMINBTS,X'80'                                                    
         BRAS  RE,XGET                                                          
         J     DLNT050                                                          
*                                                                               
DLNT060  XC    KEY,KEY             NEED TO RESTORE NOTICE RECORD                
         MVC   KEY(MNXKSTTN-MNXKEY),0(R6)                                       
         MVI   DMINBTS,X'08'       READ FOR DELETED                             
         BRAS  RE,XHIGH                                                         
         MVI   DMINBTS,X'08'                                                    
         BRAS  RE,XGET                                                          
*                                                                               
DLNTX    J     XIT                                                              
******************                                                              
* DELETE THE SPOT OFFER RECORDS                                                 
******************                                                              
DELOFFER NTR1  LABEL=*                                                          
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         CLI   QSTA,C'0'           CABLE?                                       
         BNL   DLOF050                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING MOKEY,R4                                                         
         MVI   MOKTYPE,MOKTYPQ                                                  
         MVI   MOKSUBTY,MOKSTYPQ                                                
         MVC   MOKAGMD,BAGYMD                                                   
         MVC   MOKORDER,BINORDER                                                
         MVC   MOKMGCD,QMGGROUP                                                 
         DROP  R4                                                               
*                                                                               
         BRAS  RE,HIGH                                                          
*                                                                               
DLOF010  CLC   KEY(MOKSEQ-MOKEY),KEYSAVE                                        
         JNE   DLOFX               EXIT WHEN FINISHED                           
*                                                                               
         BRAS  RE,DELSKEY                                                       
*                                                                               
         MVI   DMINBTS,X'80'                                                    
         BRAS  RE,GET                                                           
*                                                                               
         BRAS  RE,DELSREC                                                       
*                                                                               
         BRAS  RE,SEQ                                                           
         J     DLOF010                                                          
******************                                                              
* DELETE THE XSPOT OFFER RECORDS                                                
******************                                                              
DLOF050  XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING MOXKEY,R4                                                        
         MVI   MOXKTYPE,MOXKTYPQ                                                
         MVI   MOXKSBTY,MOXKSBTQ                                                
         MVC   MOXKAGMD,BAGYMD                                                  
         MVC   MOXKORDR,BINORDER                                                
         MVC   MOXKMGCD,QMGGROUP                                                
         DROP  R4                                                               
*                                                                               
         BRAS  RE,XHIGH                                                         
*                                                                               
DLOF060  CLC   KEY(MOXKSTTN-MOXKEY),KEYSAVE                                     
         JNE   DLOFX               EXIT WHEN FINISHED                           
*                                                                               
         BRAS  RE,DELXKEY                                                       
*                                                                               
         MVI   DMINBTS,X'80'                                                    
         BRAS  RE,XGET                                                          
*                                                                               
         BRAS  RE,DELXREC                                                       
*                                                                               
         BRAS  RE,XSEQ                                                          
         B     DLOF060                                                          
*                                                                               
DLOFX    J     XIT                                                              
*************                                                                   
* DELETE SPOT RECORD                                                            
*************                                                                   
DELSREC  NTR1                                                                   
         OI    15(R6),X'80'        MARK SPOT DELETED                            
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R6)                                                    
         BRAS  RE,PUT                                                           
         J     XIT                                                              
*************                                                                   
* DELETE X-SPOT RECORD                                                          
*************                                                                   
DELXREC  NTR1                                                                   
         OI    34(R6),X'80'        MARK XPOT REC DELETED                        
         XC    KEY,KEY                                                          
         MVC   KEY(32),0(R6)                                                    
         BRAS  RE,XPUT                                                          
         J     XIT                                                              
*************                                                                   
* DELETE SPOT KEY                                                               
*************                                                                   
DELSKEY  NTR1                                                                   
         MVI   DMINBTS,X'80'       READ FOR UPDATE                              
         BRAS  RE,HIGH                                                          
         CLC   KEY(13),KEYSAVE                                                  
         MVC   KEYSAVE,KEY                                                      
         JNE   *+2                                                              
*                                                                               
         OI    KEY+13,X'80'        DELETE THE KEY ALSO                          
         BRAS  RE,WRITE                                                         
         J     XIT                                                              
*************                                                                   
* DELETE X-SPOT KEY                                                             
*************                                                                   
DELXKEY  NTR1                                                                   
         MVI   DMINBTS,X'80'       READ FOR UPDATE                              
         BRAS  RE,XHIGH                                                         
         CLC   KEY(32),KEYSAVE                                                  
         MVC   KEYSAVE,KEY                                                      
         JNE   *+2                                                              
*                                                                               
         OI    KEY+32,X'80'        DELETE THE KEY ALSO                          
         BRAS  RE,XWRITE                                                        
         J     XIT                                                              
***********************************************************************         
* UNPEND THE SPOTS                                                              
***********************************************************************         
UNPSPOTS NTR1  LABEL=*,WORK=(R7,UNPWORKL)                                       
         USING UNPWORKD,R7         R7=A(LOCAL W/S)                              
         XC    UNPKEY,UNPKEY                                                    
*                                                                               
         MVI   GBYACT,GBYINIT                                                   
         BRAS  RE,GOGETBUY                                                      
         MVC   SV1OR2,GBY1OR2                                                   
*                                                                               
         CLC   AGENCY,=C'SJ'       AGENCY SJ?                                   
         BNE   UNSPT005             NO                                          
         CLC   BCLT,=X'CC2B'       CLIENT TBL?                                  
         BE    *+10                                                             
         CLC   BCLT,=X'BCC9'       CLIENT PG0?                                  
         BE    *+10                 YES                                         
         CLC   BCLT,=X'BCDA'       CLIENT PG1?                                  
         BNE   UNSPT005             NO, NONE OF THESE CLIENTS                   
         MVI   SV1OR2,2            TURN ON 2-BYTE BLN                           
*                                                                               
UNSPT005 L     RF,AIO1                                                          
         LA    R6,MNXFRST-MNXKEY(RF)                                            
         CLI   QSTA,C'0'           CABLE?                                       
         BNL   *+8                                                              
         LA    R6,MNRFRST-MNKEY(RF) MOST RECENT STATUS ELEM                     
         B     UNSPT020            SKIP 1ST STATUS ELEMENT                      
         USING MNSTELD,R6                                                       
UNSPT010 CLI   MNSTEL,MNSTELQ      STILL A STATUS ELEM?                         
         BNE   UNSPTX                                                           
         CLI   MNSTSTAT,MNSTAPP    WAS IT APPROVED?                             
         BE    UNSPT030            YUP...NEED TO TURN OFF MKGD PNDING           
         CLI   MNSTSTAT,MNSTDELV                                                
         BE    *+12                                                             
         CLI   MNSTSTAT,MNSTERR                                                 
         BNE   UNSPTX                                                           
*                                                                               
UNSPT020 LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     UNSPT010                                                         
         DROP  R6                                                               
*                                                                               
UNSPT030 L     RE,ASPLAREA                        MP STORAGE                    
         LA    RF,L'SPULAREA                                                    
         XCEFL                                                                  
*                                                                               
         CLI   QSTA,C'0'           CABLE?                                       
         BL    UNSPT035             NO                                          
         XC    KEY,KEY                                                          
         OC    KEY,UNPKEY          HAVE PREVIOUS KEY?                           
         BNZ   UNSPT033             YES                                         
         L     RF,AIO1                                                          
         MVC   KEY(L'MNXKEY),0(RF)                                              
*                                                                               
UNSPT033 LLC   R0,KEY+MNXKSEQ-MNXKEY                                            
         AHI   R0,R1                                                            
         STC   R0,KEY+MNXKSEQ-MNXKEY   READ THE NEXT NETWORK/SEQ                
*                                                                               
         MVI   DMINBTS,X'08'       READ FOR DELETED                             
         BRAS  RE,XHIGH                                                         
*                                                                               
         CLC   KEY(MNXKSTTN-MNXKEY),KEYSAVE                                     
         JNE   UNSPT070                                                         
         MVI   DMINBTS,X'08'       READ FOR DELETED                             
         MVC   AIO,AIO1                                                         
         BRAS  RE,XGET                                                          
         MVC   UNPKEY(L'MNXKEY),KEY                                             
         MVC   BMKTSTA+2(L'MNXKSTTN),KEY+MNXKSTTN-MNXKEY                        
*                                                                               
UNSPT035 L     R5,ASPLAREA                                                      
         L     RF,AIO1             READ MISSED BUYLINE ELEMS                    
         LA    R6,MNXFRST-MNXKEY(RF)                                            
         CLI   QSTA,C'0'           CABLE?                                       
         BNL   *+8                                                              
         LA    R6,MNRFRST-MNKEY(RF)                                             
         MVI   ELCODE,MNMSELQ                                                   
         BAS   RE,FIRSTEL                                                       
         B     *+8                                                              
         USING MNMSELD,R6                                                       
*                                                                               
UNSPT040 BAS   RE,NEXTEL                                                        
         BNE   UNSPT070                                                         
*****    CLI   MNMSFND,MNMSYSFD    WE FOUND THIS SPOT?                          
*****    BNE   UNSPT040            NO... GET NEXT ONE                           
*                                                                               
         MVC   NUMSPOTS,MNMSNSPT                                                
         USING BUYINFD,R5          TEMPORARILY STORE BUYLINE INFO               
*                                                                               
UNSPT050 MVC   STRTIM(4),MNMSSTIM  FOR MISSED SPOTS IN ELEM                     
         MVC   SPTLEN,MNMSTSLN                                                  
         MVC   SPTCST,MNMSCOST                                                  
         GOTO1 VDATCON,DMCB,(8,MNMSBDAT),(2,SPTDAT)                             
         MVC   LINNO+1(1),MNMSBLIN                                              
         OC    MNMSBLN2,MNMSBLN2                                                
         BZ    *+10                                                             
         MVC   LINNO,MNMSBLN2                                                   
*                                                                               
         LA    R5,BYINFLN(R5)                                                   
         LLC   R1,NUMSPOTS                                                      
         BCT   R1,UNSPT060         DECREMENT NUMBER OF SPOTS                    
         B     UNSPT040            IF NO MORE -> CHECK FOR NEXT ELEM            
UNSPT060 STC   R1,NUMSPOTS         ELSE STORE NEW LOWER NUMBER                  
         B     UNSPT050            AND BUILD ANOTHER LINE                       
         DROP  R6                                                               
*                                                                               
UNSPT070 L     R5,ASPLAREA                                                      
         OC    0(BYINFLN,R5),0(R5)                                              
         BE    UNSPTX              NO SPOTS WERE FOUND                          
*                                                                               
UNSPT080 LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         USING BUYKEY,R6                                                        
         MVC   BUYKAM,BAGYMD                                                    
         MVC   BUYKCLT,BCLT                                                     
*                                                                               
         MVI   BUYKPRD,X'FF'                                                    
*                                                                               
         MVC   BUYMSTA,BMKTSTA                                                  
         MVC   BUYKEST,BEST                                                     
         MVC   BUYKBUY+1(2),LINNO                                               
         MVI   GBYACT,GBYHIGH                                                   
         BRAS  RE,HIGH                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BNE   UNSPT090                                                         
*                                                                               
         MVI   DMINBTS,X'80'                                                    
         MVC   AIO,AIO1                                                         
         MVI   GBYACT,GBYGET                                                    
         BRAS  RE,GET                                                           
*                                                                               
         BRAS  RE,UNPBUY           UNPEND SPOTS IN BUY                          
*                                                                               
UNSPT090 LA    R5,BYINFLN(R5)                                                   
         OC    0(BYINFLN,R5),0(R5)   ANOTHER MISSED SPOT?                       
         BNZ   UNSPT080              YES, GO GET IT                             
*                                                                               
         CLI   QSTA,C'0'           CABLE?                                       
         BNL   UNSPT030             YES                                         
UNSPTX   J     XIT                                                              
         DROP  R5,R6,R7                                                         
*********                                                                       
* UNPEND THE SPOTS                                                              
*********                                                                       
UNPXPOTS DS    0H                                                               
         J     XIT                                                              
UNPWORKD DSECT ,                   ** UNPSPOTS LOCAL W/S **                     
UNPKEY   DS    XL64                                                             
UNPWORKL EQU   *-UNPWORKD                                                       
T16100   CSECT ,                                                                
         EJECT                                                                  
***********************************************************************         
* HAVE BUY, NOW UNPEND SPOTS                                                    
***********************************************************************         
         USING BUYINFD,R5          TEMPORARILY STORE BUYLINE INFO               
UNPBUY   NTR1  LABEL=*                                                          
         USING BUYRECD,R6                                                       
         L     R6,AIO1                                                          
***********************                                                         
*** WE SHOULDN'T CARE WHETHER THE SPOT LENGTH OR TIMES CHANGE IN THE            
*** BUY BECAUSE THIS SHOULD DEFINITELY BE THE BUYLINE FOR THE MAKEGOOD.         
*** WE SHOULD ONLY WORRY BAOUT WHETHER THE SPOT EXISTS FOR THE PRODUCTS         
*** FOR THE DARE ORDER.                                                         
***********************                                                         
*&&DO                                                                           
         CLC   BDSEC,SPTLEN                                                     
         BNE   UNPBUYX                                                          
         CLC   BDCOST,SPTCST                                                    
         BNE   UNPBUYX                                                          
*                                                                               
         CLC   BDTIMST(4),STRTIM                                                
         BE    UNBUY020                                                         
         MVI   ELCDLO,X'67'        IF RECORD TIME ISN'T EQUAL                   
         MVI   ELCDHI,X'67'                                                     
         LA    R6,NDELEM                                                        
UNBUY010 BAS   RE,BNEXTEL          CHECK FOR AN ORBIT ELEM                      
         BNE   UNPBUYX                                                          
         CLC   5(4,R6),STRTIM      IF THERE IS ONE                              
         BNE   UNBUY010            CHECK THAT TIME                              
*&&                                                                             
***********************************************************************         
*** END OF THIS COMMENTED OUT BLOCK OF CODE                                     
***********************************************************************         
UNBUY020 L     R6,AIO1                                                          
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
         LA    R6,NDELEM                                                        
         USING REGELEM,R6                                                       
         CLI   0(R6),X'0B'                                                      
         BE    UNBUY040                                                         
         CLI   0(R6),X'0C'                                                      
         BE    UNBUY040                                                         
UNBUY030 BAS   RE,BNEXTEL                                                       
         BNE   UNPBUYX                                                          
*                                                                               
UNBUY040 CLC   RDATE,SPTDAT                                                     
         BNE   UNBUY030                                                         
         TM    RSTATUS,X'10'       IS THIS SPOT MAKEGOOD PENDING?               
         BZ    UNBUY030            NO, GET NEXT SPOT                            
**  CHECK THE PRODUCT(S) FOR THIS SPOT ELEMENT                                  
         CLC   RPPRD,BPRD          DO WE HAVE OUR SPOT?                         
         BE    UNBUY050            POSSIBLE                                     
         CLI   QPRD2,0             ANY PIGGYBACK PRODUCT?                       
         BE    UNBUY030            NONE, GET NEXT SPOT                          
         CLI   RLEN,18             SPOT FOR PRD1-PRD2?                          
         BL    UNBUY030            NO                                           
         CLC   RPPRD,QPRD2         IS IT REVERSED (PRD2-PRD1)?                  
         BNE   UNBUY030                                                         
         CLC   RPPRD+L'RPALLOC,BPRD                                             
         BNE   UNBUY030            NO                                           
         B     UNBUY060            YES, UNPEND THIS SPOT                        
*                                                                               
UNBUY050 CLI   RLEN,18             SPOT FOR PRD1-PRD2?                          
         BNL   *+12                YES                                          
         CLI   QPRD2,0             NO, ANY PIGGYBACK PRODUCT?                   
         BE    UNBUY060                NONE, UNPEND THIS SPOT                   
         CLC   RPPRD+L'RPALLOC,QPRD2  ALSO MAKE SURE PIGGYBACK MATCHES          
         BNE   UNBUY030               NO IT DIDN'T, GET NEXT SPOT               
*                                                                               
UNBUY060 NI    RSTATUS,X'EF'       TURN OFF MAKEGOOD PENDING                    
*                                                                               
         XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'BUYKEY),0(R6)                                              
         ST    R6,AIO                                                           
         MVI   GBYACT,GBYPUT                                                    
         BRAS  RE,PUT                                                           
UNPBUYX  J     XIT                                                              
         DROP  R6,R5                                                            
***********************************************************************         
* LOOKUP MAKEGOOD VERSION# AND RETURN ERROR IF NO MATCH                         
*  ON ENTRY :   AIO1    A(MAKEGOOD OFFER RECORD)                                
*               QSTA    STATION                                                 
*                                                                               
***********************************************************************         
         USING MOFRCAND,R7                                                      
VERLKUP  NTR1  LABEL=*                                                          
*                                                                               
         L     RF,AIO1             LOOK FOR VERSION ELEMENT                     
         MVC   DATADISP,=H'24'                                                  
         LA    R6,MORFRST-MOKEY(RF)                                             
         CLI   QSTA,C'0'           CABLE?                                       
         BL    VRLK010             NO                                           
         MVC   DATADISP,=H'34'                                                  
         LA    R6,MOXFRST-MOXKEY(RF)                                            
VRLK010  CLI   0(R6),MOSQELQ                                                    
         BE    VRLK030             TO REDUCE NUMBER OF DEATHS                   
*                                                                               
VRLK020  LHI   R1,REFMOSEQ          SEQ # NOT SAME                              
         BRAS  RE,SNDERROR                                                      
*                                                                               
         LHI   R1,99                DID NOT UNPEND                              
         O     R1,=X'80000000'                                                  
         BRAS  RE,SNDERROR                                                      
         J     NO                                                               
*                                                                               
         USING MOSQELD,R6                                                       
VRLK030  LLC   R1,MOSQNUM                                                       
         CVD   R1,DUB                                                           
         UNPK  WORK(2),DUB                                                      
         OI    WORK+1,X'F0'                                                     
         NI    BITFLAG2,X'FF'-BF2MGSEQ   NO PROB WITH MG SEQ #'S YET            
         CLC   MOCNSEQN,WORK       SAME OR HIGHER SEQUENCE NUMBER?              
         BNL   VRLK040                                                          
         OI    BITFLAG2,BF2MGSEQ   PROBLEM WITH MG SEQ # NOT THE SAME           
         DROP  R6                                                               
*                                                                               
VRLK040  L     R6,AIO1                                                          
         MVI   ELCODE,MNUMELQ                                                   
         BAS   RE,GETEL                                                         
         JNE   VRLKX                                                            
         USING MNUMELD,R6                                                       
         MVC   QMNUMCD,MNUMCD                                                   
VRLKX    J     YES                                                              
         DROP  R6,R7                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS PROCESSES THE APPROVAL ORDER                                             
***********************************************************************         
APPROVAL NTR1  BASE=*,LABEL=*                                                   
*        NMOD1 0,**APPV**                                                       
         L     RC,0(R1)                                                         
         L     R9,SRPARMSD.SRQASYSF                                             
*                                                                               
         TM    MISCFLG3,MF3INIOA                                                
         BZ    APPRV01                                                          
*                                                                               
         L     R3,AWRKRIOA                                                      
         LA    R3,2(R3)                                                         
         LLC   R2,0(R3)            R2 = L(ORDER APPROVAL HEADER)                
         LA    R7,1(R3)            R7 = A(ORDER APPROVAL HEADER)                
*                                                                               
APPRV01  DS    0H                                                               
         USING RORDAPPD,R7                                                      
APPRVD   USING RTN2SNDR,ROAPRTRN                                                
*                                                                               
         MVI   QMED,C'T'                                                        
         CLI   APPRVD.RTNAGYMD+1,C'2' AM I RADIO?                               
         BNE   *+8                                                              
         MVI   QMED,C'R'                                                        
*                                                                               
         GOTO1 VHEXIN,DMCB,APPRVD.RTNAGYMD,BAGYMD,2                             
*                                                                               
APPRV05  DS    0H                                                               
         MVC   AGENCY,APPRVD.RTNPWRCD                                           
         GOTO1 VHEXIN,DMCB,APPRVD.RTNSYSID,BYTE,2                               
*                                                                               
         NI    MISCFLG1,X'FF'-MF1XMTUP NO UPDATES TO XMT ELEM                   
*                                                                               
         BAS   RE,SPANKED                                                       
         CLC   BYTE,THESYSID       WRONG FACPAK?                                
         BNE   APPRVNO             YES, AGENCY DOESN'T BELONG HERE              
*                                                                               
         DROP  APPRVD                                                           
*                                                                               
         MVC   QREPCON,ROAPRPCN    COPY THESE VALUES                            
         MVC   QRETURN,ROAPRTRN                                                 
*                                                                               
         GOTO1 CALCORDR,DMCB,ROAPORDR   CONVERT ORDER NUMBER TO BINARY          
         BNE   APPRVNO                                                          
*                                                                               
         MVC   USERID,ROAPTOID                                                  
         BAS   RE,SWTCHCTL         SWITCH CONTROL TO SPOT SYSTEM                
         BNE   APPRVNO                                                          
*                                                                               
         BRAS  RE,GETORDER         READ ORDER RECORD INTO AIO1                  
         BNE   APPRVNO                                                          
*                                                                               
         NI    MISCFLG1,X'FF'-MF1NOXMT  ASSUME XMT ELEM EXISTS                  
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
         MVC   DOIDCON,ROAPRPCN    SAVE THE REP CONTRACT NUMBER                 
         DROP  R6                                                               
*                                                                               
         SR    R0,R0                                                            
APPRV10  CLI   0(R6),0                                                          
         BNE   APPRV15                                                          
         LHI   R1,REFORDNT         ORDER NOT TRANSMITTED                        
         BRAS  RE,SNDERROR                                                      
         B     APPRVNO                                                          
*                                                                               
APPRV15  CLI   0(R6),DOXMTELQ                                                   
         BE    APPRV20                                                          
APPRV17  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     APPRV10                                                          
*                                                                               
         USING DOXMTELD,R6                                                      
APPRV20  GOTO1 VDATCON,DMCB,(0,ROAPDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,ROAPTIME,DUB+L'DOXMTSTD,L'ROAPTIME                   
*                                                                               
         OC    DOXMTSTD,DOXMTSTD   IF NO DATE OR TIME ALREADY                   
         BZ    APPRV40                                                          
         OC    DOXMTSTT,DOXMTSTT                                                
         BZ    APPRV30             THEN USE EDICT RECORD'S DATE/TIME            
*                                                                               
         CLC   DOXMTSTD,DUB               IS ELEM'S DATE MORE RECENT?           
         BH    APPRVNO                    YES, IGNORE THIS RECORD               
         BL    APPRV30                    NO, OLDER                             
         CLC   DOXMTSTT,DUB+L'DOXMTSTD    IS ELEM'S TIME MORE RECENT?           
         BNH   APPRV30                                                          
         CLC   QREPCON,=C'00000000'    HAVE A CONTRACT IN THE MESSAGE?          
         BNH   APPRVNO                                                          
         B     APPRV99                    UPDATE CONTRACT NUMBER                
*                                                                               
APPRV30  CLI   DOXMTSTA,QAPP       CAN'T CHANGE ANYTHING BUT APPROVED           
         BE    APPRV40                STATUS                                    
*                                                                               
         CLI   DOXMTSTA,QSNTPNDG   IF SENT PENDING, DON'T MARK X'11'            
         BE    APPRV100               BUT MARK X'12'                            
*                                                                               
         CLI   DOXMTSTA,QRECALL    IF RECALLING, DON'T MARK X'11'               
         BL    APPRV35                BUT MARK X'12'                            
         BE    APPRV100            O/W RECALL STATUSES THEN DON'T MARK          
         CLI   DOXMTSTA,QRCLUNKN      RECORD AS APPROVED                        
         BNH   APPRVNO                                                          
         CLI   DOXMTSTA,QRCLTRNS                                                
         BE    APPRVNO                                                          
*                                                                               
APPRV35  LHI   R1,REFCCHNG                                                      
         BRAS  RE,SNDERROR                                                      
         B     APPRVNO                                                          
*                                                                               
APPRV40  MVC   DOXMTSTD,DUB                                                     
         MVC   DOXMTSTT,DUB+L'DOXMTSTD                                          
         MVI   DOXMTSTA,QAPP       APPROVED                                     
         MVI   SVSTAT,QAPP         SAVE FOR LATER                               
         DROP  R6                                                               
*                                                                               
APPRV99  OI    MISCFLG1,MF1XMTUP                                                
***************                                                                 
* TIME TO PROCESS THE STATUS HISTORY ELEMENTS                                   
***************                                                                 
APPRV100 NI    BITFLAG3,X'FF'-BF3RCLNG-BF3DLVRD                                 
         SR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
APPRV110 CLI   0(R6),0                                                          
         BNE   APPRV115                                                         
         TM    MISCFLG1,MF1NOXMT          DOES IT HAVE XMT ELEMS?               
         BZ    APPRVNO                    YES, THEN DON'T GIVE ERROR            
         LHI   R1,REFORDNT         NEVER TRANSMITTED                            
         BRAS  RE,SNDERROR                                                      
         B     APPRVNO                                                          
*                                                                               
APPRV115 CLI   0(R6),DOSTELQ                                                    
         BE    APPRV120                                                         
APPRV116 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     APPRV110                                                         
*                                                                               
         USING DOSTELD,R6                                                       
APPRV120 CLI   DOSTSTAT,DDLVRD     IF DELIVERED, SKIP IT                        
         BNE   APPRV122                                                         
         OI    BITFLAG3,BF3DLVRD                                                
         ST    R6,FULL             DON'T CLOBBER FULL, NEED THIS!!!             
         B     APPRV116                                                         
*                                                                               
APPRV122 CLI   DOSTSTAT,QSNTPNDG   IS IT QSNTPNDG?                              
         BE    APPRV116            BUMP TO NEXT ONE                             
*                                                                               
APPRV125 GOTO1 VDATCON,DMCB,(0,ROAPDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,ROAPTIME,DUB+L'DOSTDATE,L'ROAPTIME                   
*                                                                               
         CLI   DOSTSTAT,DSENT      RECENTLY SENT?                               
         BE    APPRV150                                                         
*                                                                               
         CLC   DOSTDATE,DUB               IS ELEM'S DATE MORE RECENT?           
         BH    APPRVNO                    YES, IGNORE THIS RECORD               
         BL    APPRV130                   NO, OLDER                             
         CLC   DOSTTIME,DUB+L'DOSTTIME    IS ELEM'S TIME MORE RECENT?           
         BH    APPRVNO                    YES, IGNORE THIS REC                  
*                                                                               
APPRV130 CLI   DOSTSTAT,QAPP                                                    
         BE    APPRV140                                                         
*                                                                               
         CLI   DOSTSTAT,QRECALL    IF RECALL STATUSES THEN DON'T MARK           
         BL    APPRV135               RECORD AS APPROVED                        
         BNE   APPRV132                                                         
         OI    BITFLAG3,BF3RCLNG   RECALL WAS JUST SENT                         
         TM    BITFLAG3,BF3DLVRD   WAS IT ALREADY DELIVERD?                     
         BNZ   APPRV116            YES, FULL SAVED ALREADY                      
         ST    R6,FULL             DON'T CLOBBER FULL, WE NEED THIS!!!          
         B     APPRV116                                                         
*                                                                               
APPRV132 CLI   DOSTSTAT,QRCLUNKN   IF RECALL STATUSES THEN DON'T MARK           
         BNH   APPRVNO                RECORD AS APPROVED                        
         CLI   DOSTSTAT,QRCLTRNS                                                
         BE    APPRVNO                                                          
*                                                                               
APPRV135 TM    MISCFLG1,MF1NOXMT          DOES IT HAVE XMT ELEMS?               
         BZ    APPRVNO                    YES, THEN DON'T GIVE ERROR            
         LHI   R1,REFCCHNG                CAN'T CHANGE THE RECORD               
         BRAS  RE,SNDERROR                                                      
         B     APPRVNO                                                          
*                                                                               
APPRV140 MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         MVI   DOSTSTAT,QAPP                                                    
         MVC   DOSAELEM(DOSTLNQ2),DOSTEL                                        
         B     APPRV190                                                         
         DROP  R6                                                               
*                                                                               
APPRV150 LA    R4,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
         USING DOSTELD,R4                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   DOSTEL,DOSTELQ                                                   
         MVI   DOSTLEN,DOSTLNQ                                                  
         MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         MVI   DOSTSTAT,QAPP                                                    
         MVC   DOSAELEM(DOSTLNQ2),DOSTEL                                        
         DROP  R4                                                               
*                                                                               
         L     R6,FULL                                                          
         TM    BITFLAG3,BF3RCLNG   DID WE PREVIOUSLY RECALL?                    
         BNZ   APPRV170            YES, INSERT ELEM AT FULL                     
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)  R6 = A(FIRST ELEMENT IN RECORD)            
APPRV160 CLI   0(R6),0          ADD AFTER ALL TRANSMISSION ELEMENTS             
         BE    APPRV170         AND BEFORE AN AUDIT ELEMENT                     
         CLI   0(R6),DORPELQ2                                                   
         BE    APPRV170                                                         
         CLI   0(R6),DOSTELQ                                                    
         BNL   APPRV170                                                         
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     APPRV160                                                         
*                                                                               
APPRV170 GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
*                                                                               
APPRV190 MVI   SVSTAT,QAPP                                                      
*********                                                                       
* SALESPERSON REASSIGNMENT (ONLY FOR RADIO)                                     
*********                                                                       
APPRV200 TM    MISCFLG3,MF3INIOA   DO WE HAVE MORE RECORD IN IO?                
         BZ    APPRV300                                                         
*                                                                               
         AR    R3,R2                                                            
         CLC   =C'ORDMO1',1(R3)    MEDIAOCEAN TRANSMISSION?                     
         BNE   APPRV300            NONE, JUST EXIT                              
         LA    R4,1(R3)            YES, WE HAVE ONE                             
         USING RORDMO1D,R4                                                      
         L     R6,AIO1                                                          
         MVC   DATADISP,=H'24'                                                  
         MVI   ELCODE,DOSPELQ      X'03' GET SUPPLEMENTARY ELEMENT              
         BRAS  RE,GETEL                                                         
         USING DOSPELD,R6                                                       
         CLI   RMO1ISMO,C'Y'       IS THIS MEDIAOCEAN?                          
         BNE   *+8                                                              
         OI    DOSPFLG2,DOSPMOCN   X'10' - SET THE MO ICON BIT                  
         CLI   RMO1TRAF,C'Y'       IS THIS MEDIAOCEAN?                          
         BNE   *+8                                                              
         OI    DOSPFLG2,DOSPMTRF   X'08' - SET THE MO TRAFFIC ICON BIT          
         DROP  R4                                                               
*                                                                               
APPRV210 LLC   R1,0(R3)                                                         
         AR    R3,R1               BUMP TO NEXT SAVED ENTRY                     
*                                                                               
APPRV250 CLC   =C'ORDTLR',1(R3)                                                 
         BE    APPRV300                                                         
         L     R1,=AL4(*-T16100)                                                
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
APPRV300 XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         MVC   AIO,AIO1                                                         
         BRAS  RE,PUT                                                           
*                                                                               
         BRAS  RE,CHKFRCHG         BUILD CHECK FOR CHANGES KEY                  
*                                                                               
         GOTO1 =A(SEEDCONN),DMCB,(RC),ROAPRPCN,RR=RELO                          
*                                                                               
         BAS   RE,UPDATSB1                                                      
*                                                                               
APPRVYES BRAS  RE,BLDCOLOR                                                      
         BRAS  RE,DAREMAIL                                                      
         NI    MISCFLG3,X'FF'-MF3INIOA                                          
         B     YES                                                              
*                                                                               
APPRVNO  TM    MISCFLG1,MF1XMTUP                                                
         BNZ   APPRV300                                                         
         NI    MISCFLG3,X'FF'-MF3INIOA                                          
         B     NO                                                               
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* THIS PROCESSES THE REJECTION ORDER                                            
***********************************************************************         
REJECT   NTR1  BASE=*,LABEL=*                                                   
******** NMOD1 0,**RJCT** *************                                         
         L     RC,0(R1)                                                         
         L     R9,SRPARMSD.SRQASYSF                                             
*                                                                               
         L     R3,AWRKRIOA                                                      
         LA    R3,2(R3)                                                         
         LLC   R2,0(R3)            R2 = L(ORDER REJECTION HEADER)               
         LA    R7,1(R3)            R7 = A(ORDER REJECTION HEADER)               
*****                                                                           
* CHECK FOR AMENDED STATUS!!!                                                   
*****                                                                           
         NI    MISCFLG3,X'FF'-MF3AMEND                                          
RJCT03   CLC   =C'ORDTLR',1(R3)                                                 
         BE    RJCT07                                                           
         CLC   =C'ORDCOM',1(R3)                                                 
         BE    RJCT05                                                           
         LLC   R2,0(R3)                                                         
         AR    R3,R2                                                            
         LA    R7,1(R3)                                                         
         B     RJCT03                                                           
         USING RORDCOMD,R7                                                      
RJCT05   CLC   =C'*** AMEND ***',ROCMTEXT  AMENDED?                             
         BNE   *+8                                                              
         OI    MISCFLG3,MF3AMEND         YES!!                                  
         DROP  R7                                                               
*                                                                               
RJCT07   L     R3,AWRKRIOA                                                      
         LA    R3,2(R3)                                                         
         LLC   R2,0(R3)            R2 = L(ORDER REJECTION HEADER)               
         LA    R7,1(R3)            R7 = A(ORDER REJECTION HEADER)               
*********                                                                       
* ORDER REJECTION HEADER                                                        
*********                                                                       
         USING RORDREJD,R7                                                      
RJCTD    USING RTN2SNDR,RORJRTRN                                                
         GOTO1 VHEXIN,DMCB,RJCTD.RTNAGYMD,BAGYMD,2                              
         MVC   AGENCY,RJCTD.RTNPWRCD                                            
         GOTO1 VHEXIN,DMCB,RJCTD.RTNSYSID,BYTE,2                                
*                                                                               
         NI    MISCFLG3,X'FF'-MF3SALPR-MF3RADIO                                 
         MVI   QMED,C'T'                                                        
         CLI   RJCTD.RTNAGYMD+1,C'2'  (---MD OF AGYMD IS 2?)                    
         BNE   *+8                                                              
         MVI   QMED,C'R'            RADIO!!                                     
*                                                                               
         BAS   RE,SPANKED                                                       
*                                                                               
         NI    MISCFLG1,X'FF'-MF1XMTUP                                          
*                                                                               
         CLC   BYTE,THESYSID       WRONG FACPAK?                                
         BNE   RJCTNO              YES, AGENCY DOESN'T BELONG HERE              
*                                                                               
         DROP  RJCTD                                                            
*                                                                               
         MVC   QREPCON,RORJRPCN    COPY THESE VALUES                            
         MVC   QRETURN,RORJRTRN                                                 
*                                                                               
         GOTO1 CALCORDR,DMCB,RORJORDR    CONVERT ORDER NUMBER TO BINARY         
         BNE   RJCTNO                                                           
*                                                                               
         MVC   USERID,RORJTOID                                                  
         BAS   RE,SWTCHCTL         SWITCH CONTROL TO SPOT SYSTEM                
         BNE   RJCTNO                                                           
*                                                                               
         BRAS  RE,GETORDER                                                      
         BNE   RJCTNO                                                           
*                                                                               
         NI    MISCFLG1,X'FF'-MF1NOXMT    ASSUME XMT ELEM EXISTS                
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
         MVC   DOIDCON,RORJRPCN                                                 
         DROP  R6                                                               
*                                                                               
         SR    R0,R0                                                            
RJCT10   CLI   0(R6),0                                                          
         BNE   RJCT15                                                           
         OI    MISCFLG1,MF1NOXMT          NO TRANMISION ELEMENT                 
         B     RJCT100                                                          
*                                                                               
RJCT15   CLI   0(R6),DOXMTELQ                                                   
         BE    RJCT20                                                           
*                                                                               
         CLI   0(R6),DOSPELQ       SUPPLEMENTAL ELEMENT                         
         BE    RJCT17                                                           
RJCT15A  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RJCT10                                                           
*                                                                               
         USING DOSPELD,R6                                                       
RJCT17   MVC   VERSION#,DOSPVER#                                                
         CLI   DOSPREVN,0                                                       
         BE    RJCT15A                                                          
         CLC   =C'UNDARE',RORJRPCN                                              
         BNE   RJCT15A                                                          
         LHI   R1,928          CAN'T UNDARE REVISIONS                           
         BRAS  RE,SNDERROR                                                      
*                                                                               
         LHI   R1,928                                                           
         O     R1,=X'80000000' SO AGY KNOWS ALSO                                
         BRAS  RE,SNDERROR                                                      
         B     RJCTNO                                                           
*                                                                               
         USING DOXMTELD,R6                                                      
RJCT20   CLI   DOXMTSTA,QSNTPNDG                                                
         BNE   *+16                                                             
         MVI   DOXMTSTA,QSNTXREJ   CAN'T AUTO-SEND IF REJECTED                  
         OI    MISCFLG1,MF1XMTUP                                                
         B     RJCT15A                                                          
*                                                                               
         CLI   DOXMTSTA,QSNTXREJ                                                
         BE    RJCT15A                                                          
         CLI   DOXMTSTA,QSNTXCNF                                                
         BE    RJCT15A                                                          
         CLI   DOXMTSTA,QTOBESNT                                                
         BE    RJCT15A                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(0,RORJDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,RORJTIME,DUB+L'DOXMTSTD,L'RORJTIME                   
*                                                                               
         OC    DOXMTSTD,DOXMTSTD   IF NO DATE OR TIME ALREADY                   
         BZ    RJCT26                                                           
         OC    DOXMTSTT,DOXMTSTT                                                
         BZ    RJCT22              THEN USE EDICT RECORD'S DATE/TIME            
*                                                                               
         CLI   DOXMTSTA,QRECALL    IF IT'S RECALL STATUS                        
         BL    RJCT21                                                           
         CLI   DOXMTSTA,QRCLUNKN                                                
         BNH   RJCT24              THEN WE CAN OVERWRITE                        
         CLI   DOXMTSTA,QRCLTRNS                                                
         BE    RJCT24                                                           
         CLI   DOXMTSTA,QRCLWIP                                                 
         BE    RJCT24                                                           
*                                                                               
RJCT21   CLC   DOXMTSTD,DUB               IS ELEM'S DATE MORE RECENT?           
         BH    RJCTNO                     YES, IGNORE THIS RECORD               
         BL    RJCT22                     NO, OLDER                             
         CLC   DOXMTSTT,DUB+L'DOXMTSTD    IS ELEM'S TIME MORE RECENT?           
         BNH   RJCT22                                                           
         CLC   QREPCON,=C'00000000'    HAVE A CONTRACT IN THE REJECT?           
         BNH   RJCTNO                     NO, IGNORE THIS RECORD                
         B     RJCT99                     UPDATE CONTRACT NUMBER                
*                                                                               
RJCT22   CLI   DOXMTSTA,QNODARE    SKIP THE RESPONSE TO A NOTDARE               
         BNE   RJCT24                                                           
         CLC   =C'NOTDARE',RORJRPCN                                             
         BE    RJCTNO                                                           
         CLC   =C'UNDARE',RORJRPCN                                              
         BE    RJCTNO                                                           
         LHI   R1,REFBDCON      BAD CONTRACT                                    
         BRAS  RE,SNDERROR                                                      
         B     RJCTNO                                                           
*                                                                               
RJCT24   CLI   DOXMTSTA,QAPP       CAN'T CHANGE ANYTHING BUT APPROVED,          
         BE    RJCT26                  RECALL STATUSES                          
*                                                                               
         CLI   DOXMTSTA,QRECALL                                                 
         BL    RJCT25                                                           
         CLI   DOXMTSTA,QRCLUNKN                                                
         BNH   RJCT26                                                           
         CLI   DOXMTSTA,QRCLTRNS                                                
         BE    RJCT26                                                           
         CLI   DOXMTSTA,QRCLWIP                                                 
         BE    RJCT26                                                           
*                                                                               
RJCT25   LHI   R1,REFCCHNG         BAD CONTRACT                                 
         BRAS  RE,SNDERROR                                                      
         B     RJCTNO                                                           
*                                                                               
RJCT26   MVC   DOXMTSTD,DUB                                                     
         MVC   DOXMTSTT,DUB+L'DOXMTSTD                                          
         MVI   DOXMTSTA,QRJCT      REJECTED                                     
         CLC   =C'UNDARE',RORJRPCN                                              
         BNE   *+8                                                              
         MVI   DOXMTSTA,QUNDARE    UNDARED, NEVER LET USER SEND AGAIN           
         MVC   SVSTAT,DOXMTSTA                                                  
         DROP  R6                                                               
*                                                                               
RJCT99   OI    MISCFLG1,MF1XMTUP                                                
*                                                                               
RJCT100  NI    BITFLAG3,X'FF'-BF3SPNDG-BF3RCLRJ                                 
         SR    R0,R0                                                            
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)  R6 = A(FIRST ELEMENT IN RECORD)            
RJCT110  CLI   0(R6),0                                                          
         BNE   RJCT115                                                          
         TM    MISCFLG1,MF1NOXMT          DOES IT HAVE XMT ELEMS?               
         BZ    RJCTNO                     YES, THEN DON'T GIVE ERROR            
         LHI   R1,REFORDNT         NEVER TRANSMITTED                            
         BRAS  RE,SNDERROR                                                      
         B     RJCTNO                                                           
*                                                                               
RJCT115  CLI   0(R6),DOSTELQ                                                    
         BE    RJCT120                                                          
RJCT117  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RJCT110                                                          
*                                                                               
         USING DOSTELD,R6                                                       
RJCT120  CLI   DOSTSTAT,DDLVRD                                                  
         BE    RJCT117                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(0,RORJDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,RORJTIME,DUB+L'DOXMTSTD,L'RORJTIME                   
*                                                                               
         CLI   DOSTSTAT,QSNTPNDG                                                
         BNE   RJCT130                                                          
         DROP  R6                                                               
*                                                                               
         OI    BITFLAG3,BF3SPNDG   SET THIS BIT                                 
         LA    R4,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
         USING DOSTELD,R4                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   DOSTEL,DOSTELQ                                                   
         MVI   DOSTLEN,DOSTLNQ                                                  
         MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         MVI   DOSTSTAT,QSNTXREJ                                                
         DROP  R4                                                               
*                                                                               
         L     R6,AIO                                                           
         LA    R6,DORFRST-DOKEY(R6)  R6 = A(FIRST ELEMENT IN RECORD)            
RJCT125  CLI   0(R6),0          ADD AFTER ALL TRANSMISSION ELEMENTS             
         BE    RJCT126          AND BEFORE AN AUDIT ELEMENT                     
         CLI   0(R6),DORPELQ2                                                   
         BE    RJCT126                                                          
         CLI   0(R6),DOSTELQ                                                    
         BNL   RJCT126                                                          
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RJCT125                                                          
*                                                                               
RJCT126  GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
*                                                                               
RJCT127  SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RJCT117    BUMP AGAIN, WE ARE POINTING AT QSNTPNDG               
*                                                                               
         USING DOSTELD,R6                                                       
RJCT130  CLI   DOSTSTAT,QSNTXREJ   AM I QSNTXREJ?                               
         BE    RJCT165             YES, CAME FROM RCLAKNWL, SO JUST             
*                                     REJECT ORDER!!!                           
RJCT131  CLI   DOSTSTAT,QSNTXCNF                                                
         BE    RJCT117                                                          
         CLI   DOSTSTAT,QTOBESNT                                                
         BE    RJCT117                                                          
*                                                                               
         CLI   DOSTSTAT,DSENT                                                   
         BE    RJCT165                                                          
         OC    DOSTTIME,DOSTTIME                                                
         BZ    RJCT140                                                          
*                                                                               
         CLI   DOSTSTAT,QRECALL    IF IT'S RECALL STATUS                        
         BL    RJCT135                                                          
         CLI   DOSTSTAT,QRCLUNKN                                                
         BNH   RJCT165             THEN WE CAN ADD REJECT ELEM                  
         CLI   DOSTSTAT,QRCLTRNS                                                
         BE    RJCT165                                                          
         CLI   DOSTSTAT,QRCLWIP                                                 
         BE    RJCT165                                                          
*                                                                               
RJCT135  CLC   DOSTDATE,DUB        ELSE, CHECK DATE/TIME MORE RECENT            
         BH    RJCTNO              IGNORE THIS RECORD                           
         BL    RJCT140                                                          
         CLC   DOSTTIME,DUB+L'DOSTDATE   IS ELEM'S TIME MORE RECENT?            
         BH    RJCTNO                    YES, IGNORE THIS RECORD.               
*                                                                               
RJCT140  CLI   DOSTSTAT,QNODARE    SKIP THE RESPONSE TO A NOTDARE               
         BNE   RJCT145                                                          
         CLC   =C'NOTDARE',RORJRPCN                                             
         BE    RJCTNO                                                           
         CLC   =C'UNDARE',RORJRPCN                                              
         BE    RJCTNO                                                           
         TM    MISCFLG1,MF1NOXMT          DOES IT HAVE XMT ELEMS?               
         BZ    RJCTNO                     YES, THEN DON'T GIVE ERROR            
         LHI   R1,REFBDCON         BAD CONTRACT                                 
         BRAS  RE,SNDERROR                                                      
         B     RJCTNO                                                           
*                                                                               
RJCT145  CLI   DOSTSTAT,QRJCT                                                   
         BNE   RJCT150                                                          
         MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         MVI   SVSTAT,QRJCT                                                     
         MVC   DOSAELEM(DOSTLNQ2),DOSTEL                                        
         B     RJCT200                                                          
*                                                                               
RJCT150  CLI   DOSTSTAT,QAPP                                                    
         BE    RJCT165                                                          
*                                                                               
         CLI   DOSTSTAT,QRECALL                                                 
         BL    RJCT160                                                          
         CLI   DOSTSTAT,QRCLUNKN                                                
         BNH   RJCT165                                                          
         CLI   DOSTSTAT,QRCLTRNS                                                
         BE    RJCT165                                                          
         CLI   DOSTSTAT,QRCLWIP                                                 
         BE    RJCT165                                                          
         DROP  R6                                                               
*                                                                               
RJCT160  TM    MISCFLG1,MF1NOXMT          DOES IT HAVE XMT ELEMS?               
         BZ    RJCTNO                     YES, THEN DON'T GIVE ERROR            
         LHI   R1,REFCCHNG         BAD CONTRACT                                 
         BRAS  RE,SNDERROR                                                      
         B     RJCTNO                                                           
*                                                                               
RJCT165  LA    R4,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
         USING DOSTELD,R4                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   DOSTEL,DOSTELQ                                                   
         MVI   DOSTLEN,DOSTLNQ                                                  
         MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         MVI   DOSTSTAT,QRJCT                                                   
*                                                                               
         MVC   DOSAELEM(DOSTLNQ2),DOSTELEM                                      
*                                                                               
         TM    MISCFLG3,MF3AMEND   AMENDED?                                     
         BZ    *+8                                                              
         MVI   DOSTLEN,DOSTLNQ3                                                 
*                                                                               
         CLC   =C'UNDARE',RORJRPCN                                              
         BNE   *+8                                                              
         MVI   DOSTSTAT,QUNDARE    UNDARED, NEVER LET USER SEND AGAIN           
*                                                                               
         MVC   SVSTAT,DOSTSTAT                                                  
         DROP  R4                                                               
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)  R6 = A(FIRST ELEMENT IN RECORD)            
RJCT170  CLI   0(R6),0          ADD AFTER ALL TRANSMISSION ELEMENTS             
         BE    RJCT180          AND BEFORE AN AUDIT ELEMENT                     
         CLI   0(R6),DORPELQ2                                                   
         BE    RJCT180                                                          
         CLI   0(R6),DOSTELQ                                                    
         BNL   RJCT180                                                          
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RJCT170                                                          
                                                                                
RJCT180  GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
*********                                                                       
* SALESPERSON REASSIGNMENT (ONLY FOR RADIO)                                     
*********                                                                       
RJCT200  AR    R3,R2                                                            
*                                                                               
         CLC   =C'ORDSAL',1(R3)                                                 
         BNE   RJCT200E                                                         
*                                                                               
         TM    MISCFLG3,MF3RADIO   AM I RADIO?                                  
         BZ    RJCT200B            SKIP AND IGNORE IF RADIO!!                   
         NI    MISCFLG3,X'FF'-MF3RADIO                                          
*                                                                               
         LA    R4,1(R3)            YES, WE HAVE ONE                             
         USING RORDSALD,R4                                                      
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6) X'01'                                       
         USING DOIDELD,R6                                                       
         XC    DOIDSPER,DOIDSPER                                                
         MVC   DOIDSPER(L'ROSPSALN),ROSPSALN   NO, UPDATE SALESPERSON           
         OC    DOIDSPER,=25C' '                                                 
         OI    MISCFLG3,MF3SALPR                                                
         DROP  R4                                                               
*                                                                               
RJCT200B LLC   R1,0(R3)                                                         
         AR    R3,R1               BUMP TO NEXT SAVED ENTRY                     
*                                                                               
RJCT200E XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         ST    R6,AIO                                                           
         BRAS  RE,PUT                                                           
*                                                                               
         GOTO1 =A(SEEDCONN),DMCB,(RC),RORJRPCN,RR=RELO                          
         DROP  R7                                                               
*                                                                               
*********                                                                       
* ORDER REJECTION COMMENT                                                       
*********                                                                       
RJCT210  CLC   =C'ORDCOM',1(R3)                                                 
         BNE   RJCT300                                                          
*                                                                               
         NI    BITFLAG1,X'FF'-BF1NWCOM-BF1IGRCM                                 
         XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         MVI   KEY+DOKCMT-DOKEY,2  LOOK FOR REP COMMENT RECORD                  
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(L'DOKEY),KEYSAVE  REP COMMENT RECORD DOESN'T EXIST?          
         BE    RJCT230                                                          
         OI    BITFLAG1,BF1NWCOM     NO, NEED A NEW REP COMMENT RECORD          
         L     RE,AIO1                                                          
         LA    RF,2000                                                          
         XCEFL                                                                  
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         MVC   0(L'DOKEY,R6),KEYSAVE                                            
         MVC   DORAGY,AGENCY                                                    
         DROP  R6                                                               
         B     RJCT240                                                          
*                                                                               
RJCT230  MVI   DMINBTS,X'80'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
*                                                                               
RJCT240  L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         MVC   DORLEN,=Y(DORFRST-DOKEY)  REMOVES ALL COMMENT ELEMENTS           
         LA    R6,DORFRST                                                       
         DROP  R6                                                               
*                                                                               
         LA    R2,ELEM             ADD THE COMMENTS TO RECORD                   
         USING DOCOMELD,R2                                                      
         XC    ELEM,ELEM                                                        
         MVI   DOCOMEL,DOCOMELQ                                                 
*                                                                               
RJCT250  LA    R7,1(R3)                                                         
         USING RORDCOMD,R7                                                      
*                                                                               
         CLC   =C'** AMEND **',DOCOMTXT  SKIP THIS COMMENT                      
         BE    RJCT263                                                          
*                                                                               
         MVC   DOCOMTXT(4),ROCMBLIN   BUYLINE GOES IN COMMENT                   
         MVI   DOCOMTXT+5,C'-'                                                  
         LLC   R1,0(R3)            L(LENGTH AND LINE)                           
         SH    R1,=Y(ROCMTEXT-RORDCOMD+2)                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DOCOMTXT+7(0),ROCMTEXT                                           
         OC    DOCOMTXT(L'ROCMTEXT+7),ALLSPCES                                  
*                                                                               
         LA    R1,DOCOMTXT+L'ROCMTEXT+7-1                                       
RJCT255  CLI   0(R1),C' '          CALCULATE LENGTH OF COMMENT                  
         BNE   RJCT260                                                          
         BCT   R1,RJCT255                                                       
         L     R1,=AL4(*-T16100)                                                
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
RJCT260  LR    R0,R1                                                            
         SR    R0,R2                                                            
         AH    R0,=H'1'                                                         
         STC   R0,DOCOMLEN                                                      
*                                                                               
         LLC   R1,DOCOMLIN         INCREMENT LINE NUMBER                        
         LA    R1,1(R1)                                                         
         STC   R1,DOCOMLIN                                                      
         DROP  R2                                                               
*                                                                               
         GOTO1 VRECUP,DMCB,(C'S',AIO1),ELEM,(C'R',(R6))                         
         CLI   8(R1),C'R'          RECORD OVERFLOW?                             
         BE    *+12                                                             
         OI    BITFLAG1,BF1IGRCM   YES, IGNORE REST OF REP COMMENTS             
         B     RJCT263                                                          
*                                                                               
         LLC   R0,ELEM+1                                                        
         AR    R6,R0               R6 = A(NEXT INSERTION ADDRESS)               
RJCT263  LLC   R1,0(R3)                                                         
         AR    R3,R1                                                            
         CLC   =C'ORDCOM',1(R3)    GOT ANOTHER COMMENT LINE?                    
         BNE   RJCT266                                                          
         TM    BITFLAG1,BF1IGRCM   YES, IGNORE IT?                              
         BZ    RJCT250                  NO                                      
         B     RJCT263                  YES                                     
*                                                                               
RJCT266  L     R6,AIO1                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
*                                                                               
         TM    BITFLAG1,BF1NWCOM   NEW REP COMMENT RECORD?                      
         BZ    RJCT270                                                          
         MVC   AIO,AIO1                                                         
         BRAS  RE,ADD                                                           
         B     RJCT300                                                          
*                                                                               
RJCT270  MVC   AIO,AIO1                                                         
         BRAS  RE,PUT                                                           
*                                                                               
RJCT300  CLC   =C'ORDLIN',1(R3)                                                 
         BE    RJCT310                                                          
         CLC   =C'ORDURL',1(R3)                                                 
         BNE   RJCT400                                                          
*                                                                               
RJCT310  NI    BITFLAG2,X'FF'-BF2NWURL                                          
         XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         MVI   KEY+DOKCMT-DOKEY,DOKRURLQ  LOOK FOR REJECTION URL RECORD         
*                                                                               
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(L'DOKEY),KEYSAVE  URL RECORD DOESN'T EXIST?                  
         BE    RJCT320                                                          
         OI    BITFLAG2,BF2NWURL     NO, NEED A NEW URL RECORD                  
         L     RE,AIO1                                                          
         LA    RF,2000                                                          
         XCEFL                                                                  
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         MVC   0(L'DOKEY,R6),KEYSAVE                                            
         MVC   DORAGY,AGENCY                                                    
         DROP  R6                                                               
         B     RJCT330                                                          
*                                                                               
RJCT320  MVI   DMINBTS,X'80'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
*                                                                               
RJCT330  L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
*                                                                               
         CLC   =C'ORDLIN',1(R3)                                                 
         BNE   RJCT350                                                          
         LA    R2,ELEM             ADD THE REJECT LINE NUMBER RECORD            
         USING DRLINELD,R2                                                      
         XC    ELEM,ELEM                                                        
         MVI   DRLINEL,DRLINELQ                                                 
         MVC   DRLINVER,VERSION#                                                
         LA    RE,DRLINTXT                                                      
*                                                                               
RJCT340  LA    R4,1(R3)            YES, WE HAVE ONE                             
         USING RORDLIND,R4                                                      
         PACK  DUB,ROLNBLIN        CONVERT THE BUYLINE TO BINARY                
         CVB   R1,DUB                                                           
         STCM  R1,3,0(RE)                                                       
*                                                                               
         LLC   R1,0(R3)            BUMP TO NEXT                                 
         AR    R3,R1                                                            
         LA    RE,L'DRLINTXT(RE)   BUMP                                         
         CLC   =C'ORDLIN',1(R3)                                                 
         BE    RJCT340                                                          
         SR    RE,R2               GET LENGTH OF LINE TEXT                      
         AHI   RE,DRLINOVH                                                      
         STC   RE,DRLINLEN                                                      
         GOTO1 VRECUP,DMCB,(C'S',AIO1),ELEM,(C'R',(R6))                         
*                                                                               
RJCT350  CLC   =C'ORDURL',1(R3)                                                 
         BNE   RJCT380             NO                                           
         LA    R4,1(R3)            YES, WE HAVE ONE                             
         USING RORDURLD,R4                                                      
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
*                                                                               
         LA    R2,ELEM             ADD THE URL TO RECORD                        
         USING DRURLELD,R2                                                      
         XC    ELEM,ELEM                                                        
         MVI   DRURLEL,DRURLELQ                                                 
         MVC   DRURLVER,VERSION#                                                
         MVC   DRURLTXT(L'ROURTEXT),ROURTEXT                                    
*                                                                               
         LA    R0,DRURLOVH         LENGTH OF OVERHEAD BEFORE TEXT               
         CLI   ROURCONT,C'*'       MORE TO FOLLOW?                              
         BNE   RJCT365                                                          
         LLC   R1,0(R3)            BUMP TO NEXT                                 
         AR    R3,R1                                                            
*                                                                               
         LA    R4,1(R3)                                                         
         MVC   DRURLTXT+L'ROURTEXT(L'ROURTEXT),ROURTEXT                         
         AHI   R0,L'ROURTEXT       ADD LENGTH OF FULL TEXT                      
*                                                                               
RJCT365  LA    R5,ROURTEXT+L'ROURTEXT-1                                         
RJCT370  CLI   0(R5),C' '                                                       
         BH    RJCT375                                                          
         BCT   R5,RJCT370                                                       
         L     R1,=AL4(*-T16100)                                                
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
RJCT375  LA    R5,1(R5)            BUMP 1 PAST LAST TO GET CORRECT LEN          
         LA    R1,ROURTEXT                                                      
         SR    R5,R1               SUBTRACT TO GET CORRECT LENGTH               
         AR    R0,R5               ADD TO PREVIOUS LENGTH IN R0                 
         STC   R0,DRURLLEN                                                      
         GOTO1 VRECUP,DMCB,(C'S',AIO1),ELEM,(C'R',(R6))                         
*                                                                               
         L     R6,AIO1                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
*                                                                               
         LLC   R1,0(R3)            BUMP TO NEXT                                 
         AR    R3,R1                                                            
*                                                                               
RJCT380  TM    BITFLAG2,BF2NWURL   NEW REP COMMENT RECORD?                      
         BZ    RJCT390                                                          
         MVC   AIO,AIO1                                                         
         BRAS  RE,ADD                                                           
         B     RJCT400                                                          
*                                                                               
RJCT390  MVC   AIO,AIO1                                                         
         BRAS  RE,PUT              ORDER COMMENTS FINISHED                      
*                                                                               
RJCT400  CLC   =C'ORDTLR',1(R3)                                                 
         BE    RJCTYES                                                          
         L     R1,=A(*-T16100)                                                  
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
RJCTYES  BRAS  RE,CHKFRCHG         BUILD CHECK FOR CHANGES KEY                  
         BRAS  RE,BLDCOLOR                                                      
         BRAS  RE,DAREMAIL                                                      
         NI    MISCFLG3,X'FF'-MF3RADIO                                          
         B     YES                                                              
*                                                                               
RJCTNO   TM    MISCFLG1,MF1XMTUP                                                
         BNZ   RJCT200                                                          
         NI    MISCFLG3,X'FF'-MF3RADIO                                          
         B     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS PROCESSES THE CONFIRMATION ORDER                                         
***********************************************************************         
CONFIRM  NTR1  BASE=*,LABEL=*                                                   
*        NMOD1 0,**CNFM**                                                       
         L     RC,0(R1)                                                         
         L     R9,SRPARMSD.SRQASYSF                                             
*                                                                               
         L     R3,AWRKRIOA                                                      
         LA    R3,2(R3)                                                         
         LLC   R2,0(R3)            R2 = L(ORDER CONFIRMATION HEADER)            
         LA    R7,1(R3)            R7 = A(ORDER CONFIRMATION HEADER)            
*********                                                                       
* ORDER CONFIRMATION HEADER                                                     
*********                                                                       
         USING RORDCFMD,R7                                                      
CNFMD    USING RTN2SNDR,ROCFRTRN                                                
*                                                                               
         MVI   QMED,C'T'                                                        
         NI    MISCFLG3,X'FF'-MF3RADIO-MF3OK2CF-MF3SALPR                        
         CLI   CNFMD.RTNAGYMD+1,C'2'  (---MD OF AGYMD IS 2?)                    
         BNE   CNFM03                                                           
         OI    MISCFLG3,MF3RADIO    RADIO!!                                     
         MVI   QMED,C'R'                                                        
*                                                                               
CNFM03   GOTO1 VHEXIN,DMCB,CNFMD.RTNAGYMD,BAGYMD,2                              
         MVC   AGENCY,CNFMD.RTNPWRCD                                            
         GOTO1 VHEXIN,DMCB,CNFMD.RTNSYSID,BYTE,2                                
*                                                                               
         BAS   RE,SPANKED                                                       
*                                                                               
         NI    MISCFLG1,X'FF'-MF1XMTUP                                          
*                                                                               
         CLC   BYTE,THESYSID       WRONG FACPAK?                                
         BNE   CNFMNO              YES, AGENCY DOESN'T BELONG HERE              
*                                                                               
         DROP  CNFMD                                                            
*                                                                               
         MVC   QREPCON,ROCFRPCN    COPY THESE VALUES                            
         MVC   QRETURN,ROCFRTRN                                                 
*                                                                               
         GOTO1 CALCORDR,DMCB,ROCFORDR    CONVERT ORDER NUMBER TO BINARY         
         BNE   CNFMNO                                                           
*                                                                               
         MVC   USERID,ROCFTOID                                                  
         BAS   RE,SWTCHCTL         SWITCH CONTROL TO SPOT SYSTEM                
         BNE   CNFMNO                                                           
***************                                                                 
* GET OM PROFILE                                                                
***************                                                                 
         BRAS  RE,GETORDER                                                      
         BNE   CNFMNO                                                           
***                                                                             
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         LA    R6,DORFRST                                                       
         USING DOIDELD,R6                                                       
         MVC   BCLT,DOIDCLT                                                     
*                                                                               
         BRAS  RE,GETCLTRC         GET THE CLIENT RECORD                        
         BE    CNFM05                                                           
         L     R1,=A(*-T16100)                                                  
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
         DROP  R6                                                               
*                                                                               
CNFM05   L     R6,AIO1                                                          
         USING CLTHDRD,R6                                                       
         MVC   SVCPRF00,CPROF+0                                                 
         GOTO1 VCLUNPK,DMCB,(CPROF+6,BCLT),QCLT                                 
         MVC   SVCOFFC,COFFICE                                                  
         DROP  R6                                                               
*                                                                               
* GET OM PARITAL CONFIRM WORKFLOW PROFILE?                                      
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0OM'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
*                                                                               
         MVC   WORK+7(3),QCLT                                                   
         CLI   SVCOFFC,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCOFFC                                               
*****                                                                           
         L     R1,SRPARMSD.SRQAUTL                                              
         USING UTLD,R1                                                          
         MVC   TUSER,SIGNON2H      FOOL GETPROF                                 
         DROP  R1                                                               
*****                                                                           
         L     RF,SRPARMSD.SRQACOMF                                             
         L     RF,CGETPROF-COMFACSD(RF)                                         
         XC    PROFOM,PROFOM                                                    
         GOTO1 (RF),DMCB,WORK,PROFOM,VDATAMGR                                   
*****                                                                           
         L     R1,SRPARMSD.SRQAUTL                                              
         USING UTLD,R1                                                          
         XC    TUSER,TUSER         RESET THE ID NUMBER IN THE UTL               
         DROP  R1                                                               
***************                                                                 
* GOT OM PROFILE!! RESTORE AIO1 TO ORDER RECORD!!                               
***************                                                                 
         BRAS  RE,GETORDER                                                      
         BNE   CNFMNO                                                           
*                                                                               
         XC    POLORDER,POLORDER                                                
         L     R6,AIO1             DO WE HAVE A BRAND ORDER?                    
         USING DOKEY,R6                                                         
         LA    R6,DORFRST                                                       
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),DOI2ELQ                                                    
         BNE   CNFM08                                                           
         USING DOI2ELD,R6                                                       
         OC    DOI2OORD,DOI2OORD                                                
         BZ    CNFM08                                                           
         MVC   POLORDER,DOI2OORD   SAVE POL ORDER NUMBER                        
         L     R6,AIO1                                                          
         B     CNFM10                                                           
*                                                                               
CNFM08   L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         OI    DORSTAT,X'01'       SET CONFIRMED BIT ON                         
CNFM10   LA    R6,DORFRST                                                       
         USING DOIDELD,R6                                                       
         MVC   DOIDCON,ROCFRPCN                                                 
         NI    BITFLAG2,X'FF'-BF2VARCN                                          
*                                                                               
         LLC   R0,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,R0                                                            
*                                                                               
         CLI   DOIDPRD,X'FF'       POL ORDER?                                   
         BNE   CNFM15                                                           
*                                                                               
         CLI   0(R6),DOI2ELQ       DO WE HAVE A 2NDARY ID ELEM?                 
         BE    CNFM12                                                           
         L     R1,=A(*-T16100)     ALL POL ORDERS SHOULD HAVE THIS ELEM         
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
         USING DOI2ELD,R6                                                       
CNFM12   TM    DOI2FLG1,DOI2FVAR   ALREADY A VAR ORDER?                         
         BZ    CNFM15                                                           
         OI    DOI2FLG1,DOI2FVCN   VAR ORDER IS NOW CONFIRMED                   
         OI    BITFLAG2,BF2VARCN                                                
         DROP  R6                                                               
         LLC   R0,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,R0                                                            
*                                                                               
CNFM15   CLI   0(R6),DOSPELQ       DO WE HAVE A 2NDARY ID ELEM?                 
         BE    CNFM20                                                           
         L     R1,=A(*-T16100)     ALL POL ORDERS SHOULD HAVE THIS ELEM         
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
         USING DOSPELD,R6                                                       
CNFM20   MVC   REVISION,DOSPREVN   SAVE THE REVISION                            
         DROP  R6                                                               
*                                                                               
         NI    MISCFLG1,X'FF'-MF1NOXMT    ASSUME XMT ELEM EXISTS                
         NI    BITFLAG2,X'FF'-BF2SPNDG                                          
CNFM25   CLI   0(R6),0                                                          
         BNE   CNFM30                                                           
         LHI   R1,REFORDNT          ORD NOT TRANSMITTED                         
         BRAS  RE,SNDERROR                                                      
         B     CNFMNO                                                           
*                                                                               
CNFM30   CLI   0(R6),DOXMTELQ                                                   
         BE    CNFM40                                                           
CNFM35   IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CNFM25                                                           
*                                                                               
         USING DOXMTELD,R6                                                      
CNFM40   CLI   DOXMTSTA,QSNTPNDG                                                
         BNE   CNFM45                                                           
         OI    BITFLAG2,BF2SPNDG   SEND PENDING                                 
         MVI   DOXMTSTA,QTOBESNT   CAN'T AUTO-SEND IF PARTIAL CONFIRM           
         MVI   SVSTAT,QTOBESNT                                                  
         OI    MISCFLG1,MF1XMTUP                                                
         B     CNFM35                                                           
*                                                                               
CNFM45   CLI   DOXMTSTA,QSNTXREJ                                                
         BE    CNFM35                                                           
         CLI   DOXMTSTA,QSNTXCNF                                                
         BE    CNFM35                                                           
         CLI   DOXMTSTA,QTOBESNT                                                
         BE    CNFM35                                                           
*                                                                               
         GOTO1 VDATCON,DMCB,(0,ROCFDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,ROCFTIME,DUB+L'DOXMTSTD,L'ROCFTIME                   
*                                                                               
         OC    DOXMTSTD,DOXMTSTD   IF NO DATE OR TIME ALREADY                   
         BZ    CNFM75                                                           
         OC    DOXMTSTT,DOXMTSTT                                                
         BZ    CNFM55              THEN USE EDICT RECORD'S DATE/TIME            
*                                                                               
         CLI   DOXMTSTA,QRECALL    IF IT IS A RECALL STATUS                     
         BL    CNFM50                                                           
         CLI   DOXMTSTA,QRCLUNKN                                                
         BNH   CNFM55                                                           
         CLI   DOXMTSTA,QRCLTRNS                                                
         BE    CNFM55                                                           
         CLI   DOXMTSTA,QRCLWIP                                                 
         BE    CNFM55              THEN WE CAN OVERWRITE W/ A CONFIRM           
*                                                                               
CNFM50   CLC   DOXMTSTD,DUB        ELSE CHECK DATE/TIME MORE RECENT             
         BH    CNFMNO              IGNORE THIS RECORD                           
         BL    CNFM55                                                           
         CLC   DOXMTSTT,DUB+L'DOXMTSTD    IS ELEM'S TIME MORE RECENT?           
         BH    CNFMNO                     YES, IGNORE THIS REC                  
*                                                                               
CNFM55   CLI   DOXMTSTA,QAPP       CAN'T CHANGE ANYTHING BUT APPROVED           
         BE    CNFM75                  OR RECALL STATUSES                       
*                                                                               
         CLI   DOXMTSTA,QRECALL                                                 
         BL    CNFM70                                                           
         CLI   DOXMTSTA,QRCLUNKN                                                
         BNH   CNFM75                                                           
         CLI   DOXMTSTA,QRCLTRNS                                                
         BE    CNFM75                                                           
         CLI   DOXMTSTA,QRCLWIP                                                 
         BE    CNFM75              THEN WE CAN OVERWRITE W/ A CONFIRM           
CNFM70   LHI   R1,REFCCHNG                                                      
         BRAS  RE,SNDERROR                                                      
         B     CNFMNO                                                           
*                                                                               
CNFM75   MVC   DOXMTSTD,DUB                                                     
         MVC   DOXMTSTT,DUB+L'DOXMTSTD                                          
*                                                                               
         MVI   DOXMTSTA,QCFMD      CONFIRMED                                    
         OC    POLORDER,POLORDER                                                
         BZ    *+8                                                              
         MVI   DOXMTSTA,QCFMDPND   CONFIRM PENDING                              
*                                                                               
         TM    BITFLAG2,BF2SPNDG                                                
         BNZ   *+10                                                             
         MVC   SVSTAT,DOXMTSTA                                                  
         DROP  R6                                                               
*                                                                               
CNFM99   OI    MISCFLG1,MF1XMTUP                                                
*                                                                               
CNFM100  SR    R0,R0                                                            
         NI    BITFLAG3,X'FF'-BF3SPNDG-BF3RCLCF-BF3CNFMD                        
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)  R6 = A(FIRST ELEMENT IN RECORD)            
CNFM110  CLI   0(R6),0                                                          
         BNE   CNFM115                                                          
         TM    MISCFLG1,MF1NOXMT          DOES IT HAVE XMT ELEMS?               
         BZ    CNFMNO                     YES, THEN DON'T GIVE ERROR            
         LHI   R1,REFORDNT                                                      
         BRAS  RE,SNDERROR                                                      
         B     CNFMNO                                                           
*                                                                               
CNFM115  CLI   0(R6),DOSTELQ                                                    
         BE    CNFM120                                                          
CNFM117  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CNFM110                                                          
*                                                                               
         USING DOSTELD,R6                                                       
CNFM120  CLI   DOSTSTAT,DDLVRD                                                  
         BE    CNFM117                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(0,ROCFDATE),(19,DUB)                               
         GOTO1 VHEXIN,DMCB,ROCFTIME,DUB+L'DOSTDATE,L'ROCFTIME                   
*                                                                               
         CLI   DOSTSTAT,QSNTPNDG                                                
         BE    CNFM120A                                                         
         DROP  R6                                                               
*                                                                               
         TM    BITFLAG3,BF3RCLCF   DID I COME BACK TO CHK FOR QSNTPNDG          
         BNZ   CNFM199             YES, DIDN'T FIND IT, EXIT                    
         B     CNFM130             NO                                           
*                                                                               
CNFM120A OI    BITFLAG3,BF3SPNDG                                                
         LA    R4,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
         USING DOSTELD,R4                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   DOSTEL,DOSTELQ                                                   
         MVI   DOSTLEN,DOSTLNQ                                                  
         MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         MVI   DOSTSTAT,QTOBESNT                                                
         MVI   SVSTAT,QTOBESNT                                                  
         DROP  R4                                                               
*                                                                               
         L     R6,AIO                                                           
         LA    R6,DORFRST-DOKEY(R6)  R6 = A(FIRST ELEMENT IN RECORD)            
CNFM125  CLI   0(R6),0          ADD AFTER ALL TRANSMISSION ELEMENTS             
         BE    CNFM126          AND BEFORE AN AUDIT ELEMENT                     
         CLI   0(R6),DORPELQ2                                                   
         BE    CNFM126                                                          
         CLI   0(R6),DOSTELQ                                                    
         BNL   CNFM126                                                          
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CNFM125                                                          
*                                                                               
CNFM126  GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
*                                                                               
         TM    BITFLAG3,BF3RCLCF   DID I COME BACK TO CHK FOR QSNTPNDG          
         BNZ   CNFM199             YES, FOUND IT, NO NEED TO CONTINUE           
*                                                                               
CNFM127  SR    R0,R0     CALLING HERE WILL BUMP YOU TWICE!!                     
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         ST    R6,FULL             <=== NEED THIS, DO NOT CLOBBER!!!            
         B     CNFM117   BUMP AGAIN, WE ARE POINTING AT QSNTPNDG                
*                                                                               
         USING DOSTELD,R6                                                       
CNFM130  CLI   DOSTSTAT,QSNTXREJ                                                
         BE    CNFM117                                                          
         CLI   DOSTSTAT,QSNTXCNF                                                
         BE    CNFM117                                                          
         CLI   DOSTSTAT,QTOBESNT                                                
         BE    CNFM117                                                          
*                                                                               
         CLI   DOSTSTAT,DSENT                                                   
         BE    CNFM165                                                          
         OC    DOSTTIME,DOSTTIME                                                
         BZ    CNFM140                                                          
*                                                                               
         CLI   DOSTSTAT,QRECALL    IF IT'S RECALL STATUS                        
         BL    CNFM135                                                          
         CLI   DOSTSTAT,QRCLUNKN                                                
         BNH   CNFM165             THEN WE CAN ADD CONFIRM STATUS               
         CLI   DOSTSTAT,QRCLTRNS                                                
         BE    CNFM165                                                          
         CLI   DOSTSTAT,QRCLWIP                                                 
         BE    CNFM165                                                          
*                                                                               
CNFM135  CLC   DOSTDATE,DUB        ELSE, CHECK DATE/TIME MORE RECENT?           
         BH    CNFMNO              IGNORE THIS RECORD.                          
         BL    CNFM140                                                          
         CLC   DOSTTIME,DUB+L'DOSTDATE   IS ELEM''S TIME MORE RECENT?           
         BH    CNFMNO                    YES, IGNORE THIS RECORD.               
*                                                                               
CNFM140  CLI   DOSTSTAT,QCFMD                                                   
         BE    CNFM145                                                          
         CLI   DOSTSTAT,QCFMDPND                                                
         BNE   CNFM150                                                          
CNFM145  MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         MVC   DOSAELEM(DOSTLNQ2),DOSTEL                                        
         TM    BITFLAG3,BF3SPNDG                                                
         BNZ   CNFM200                                                          
         MVC   SVSTAT,DOSTSTAT                                                  
         B     CNFM200                                                          
*                                                                               
CNFM150  CLI   DOSTSTAT,QAPP                                                    
         BE    CNFM165                                                          
*                                                                               
         CLI   DOSTSTAT,QRECALL                                                 
         BL    CNFM160                                                          
         CLI   DOSTSTAT,QRCLUNKN                                                
         BNH   CNFM165                                                          
         CLI   DOSTSTAT,QRCLTRNS                                                
         BE    CNFM165                                                          
         CLI   DOSTSTAT,QRCLWIP                                                 
         BE    CNFM165                                                          
CNFM160  TM    MISCFLG1,MF1NOXMT          DOES IT HAVE XMT ELEMS?               
         BZ    CNFMNO                     YES, THEN DON'T GIVE ERROR            
         LHI   R1,REFCCHNG         BAD CONTRACT                                 
         BRAS  RE,SNDERROR                                                      
         B     CNFMNO                                                           
*                                                                               
CNFM165  DS    0H                                                               
         CLI   DOSTSTAT,QRCLCONF   AM I RECALL CONFIRM?                         
         BNE   CNFM166                                                          
         OI    BITFLAG3,BF3RCLCF                                                
         DROP  R6                                                               
*                                                                               
CNFM166  LA    R4,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
         USING DOSTELD,R4                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   DOSTEL,DOSTELQ                                                   
         MVI   DOSTLEN,DOSTLNQ3                                                 
         MVC   DOSTDATE,DUB                                                     
         MVC   DOSTTIME,DUB+L'DOSTDATE                                          
         MVI   DOSTSTAT,QCFMD                                                   
         MVC   DOSAELEM(DOSTLNQ2),DOSTELEM                                      
         OC    POLORDER,POLORDER                                                
         BZ    *+12                                                             
         MVI   DOSTLEN,DOSTLNQ                                                  
         MVI   DOSTSTAT,QCFMDPND   CONFIRM PENDING                              
*                                                                               
         L     R6,FULL             POINT TO STATUS BEFORE QTOBESNT              
         TM    BITFLAG3,BF3SPNDG   DO I WANT TO ADD IT THERE?                   
         BNZ   CNFM180             YES                                          
         MVC   SVSTAT,DOSTSTAT                                                  
         DROP  R4                                                               
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)  R6 = A(FIRST ELEMENT IN RECORD)            
CNFM170  CLI   0(R6),0          ADD AFTER ALL TRANSMISSION ELEMENTS             
         BE    CNFM180          AND BEFORE AN AUDIT ELEMENT                     
         CLI   0(R6),DORPELQ2                                                   
         BE    CNFM180                                                          
         CLI   0(R6),DOSTELQ                                                    
         BNL   CNFM180                                                          
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CNFM170                                                          
                                                                                
CNFM180  GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
*                                                                               
         TM    BITFLAG3,BF3RCLCF   WAS IT QRCLCONF BEFORE?                      
         BNZ   CNFM127             YES, MUST CHECK FOR QSNTPNDG                 
*                                                                               
CNFM199  DS    0H                                                               
*********                                                                       
* ORDER LINE NUMBER EQUIVALENTS                                                 
*********                                                                       
CNFM200  AR    R3,R2                                                            
*                                                                               
         NI    BITFLAG2,X'FF'-BF2CNFCM                                          
         MVI   QCSHTRDE,C' '                                                    
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
         MVI   ELCDLO,DOSPELQ                                                   
         MVI   ELCDHI,DOSPELQ                                                   
         BRAS  RE,BNEXTEL                                                       
         BNE   CNFM210                                                          
*                                                                               
         USING DOSPELD,R6                                                       
         NI    DOSPFLG1,X'FF'-DOSPCFCM   MIGHT NOT BE A PARTIAL CONFM           
         TM    DOSPFLG3,DOSPFVDA         WAS IT LAST SENT TO VIDEA?             
         BZ    CNFM205                                                          
         L     R1,AIO1                                                          
         OI    DORSTAT-DOKEY(R1),DORSVDAQ YES, VIDEA CONFIRMED ORDER            
*                                                                               
CNFM205  CLC   =C'000',DOSPTDAT    ANY TRADE REP SPECIFIED?                     
         BNL   CNFM210             NONE                                         
         MVI   QCSHTRDE,C'C'                                                    
         TM    DOSPTMTH,X'40'                                                   
         BZ    *+8                                                              
         MVI   QCSHTRDE,C'T'                                                    
         DROP  R6                                                               
*                                                                               
CNFM210  CLC   =C'ORDURL',1(R3)    SKIP ORDURL'S FOR NOW                        
         BE    CNFM290                                                          
*                                                                               
         CLC   =C'ORDCOM',1(R3)    SKIP ORDCOM'S FOR NOW                        
         BNE   CNFM250                                                          
         TM    BITFLAG2,BF2CNFCM                                                
         BNZ   CNFM290                                                          
         OI    BITFLAG2,BF2CNFCM   WE HAVE CONFIRM WITH COMMENTS                
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
         SR    R0,R0                                                            
CNFM215  CLI   0(R6),0             SUPPLEMENTARY ID ELEMENT?                    
         BE    CNFM245                                                          
         CLI   0(R6),DOSPELQ                                                    
         BH    CNFM245             NONE                                         
         BE    CNFM220             YES, TURN ON THE APPROPRIATE BIT             
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CNFM215                                                          
*                                                                               
         USING DOSPELD,R6                                                       
CNFM220  OI    DOSPFLG1,DOSPCFCM   GOT A CONFIRM WITH COMMENTS                  
*                                                                               
         TM    BITFLAG2,BF2SPNDG   SEND PENDING AS WELL?                        
         BZ    CNFM230                                                          
CNFM225  IC    R0,1(R6)            NEED TO FIND XMT ELEM WITH QSNTPNDG          
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    CNFM230                                                          
         CLI   0(R6),DOXMTELQ                                                   
         BNE   CNFM225                                                          
         USING DOXMTELD,R6                                                      
         CLI   DOXMTSTA,QTOBESNT   GOING TO SEND IT OUT?                        
         BNE   CNFM230                                                          
         MVI   DOXMTSTA,QSNTXCNF   NO CAN DO, BUYER SHOULD READ CMNTS           
         MVI   SVSTAT,QSNTXCNF                                                  
         NI    BITFLAG2,X'FF'-BF2SPNDG    NOT PENDING ANYMORE                   
         OI    MISCFLG1,MF1XMTUP                                                
         DROP  R6                                                               
*                                                                               
CNFM230  L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
         SR    R0,R0                                                            
CNFM235  CLI   0(R6),0                                                          
         BE    CNFM290                                                          
         CLI   0(R6),DOSTELQ     NEED TO FIND THE X'12' ELEM W/QSNTPNDG         
         BE    CNFM240                                                          
CNFM237  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CNFM235                                                          
*                                                                               
         USING DOSTELD,R6                                                       
*                                                                               
CNFM240  CLI   DOSTSTAT,QTOBESNT                                                
         BNE   CNFM242                                                          
*                                                                               
         TM    BITFLAG2,BF2SPNDG   SEND PENDING AS WELL?                        
         BZ    CNFM237                                                          
*                                                                               
         MVC   ELEM(L'DOSTSTAT),0(R6)                                           
         GOTO1 VRECUP,DMCB,(C'S',AIO1),(R6)   DELETE IT!!                       
         DROP  R6                                                               
*                           AFTER THIS DELETE, R6 = A(QCFMD ELEM)               
         LR    R3,R6               SAVE ADDR OF CONFIRMED                       
         LA    R4,DOSTELEM        AND ADD QSNTXCNF AFTER QCFMD!!!               
         USING DOSTELD,R4                                                       
         MVI   DOSTSTAT,QSNTXCNF                                                
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
         DROP  R4                                                               
*                                                                               
         MVI   SVSTAT,QSNTXCNF                                                  
         NI    BITFLAG3,X'FF'-BF3SPNDG                                          
         LR    R6,R3                                                            
*                                                                               
         USING DOSTELD,R6                                                       
CNFM242  CLI   DOSTSTAT,QCFMD                                                   
         BNE   CNFM290                                                          
         CLI   DOSTLEN,DOSTLNQ3                                                 
         BNE   CNFM290                                                          
         OI    DOSTTYPE,DCNFMCOM                                                
         B     CNFM290                                                          
         DROP  R6                                                               
*                                                                               
CNFM245  XC    ELEM,ELEM           R6 = A(TO INSERT SUPP ELEM IN REC)           
         LA    R1,ELEM                                                          
         USING DOSPELD,R1                                                       
         MVI   DOSPEL,DOSPELQ                                                   
         MVI   DOSPLEN,DOSPLNQ                                                  
         OI    DOSPFLG1,DOSPCFCM                                                
         DROP  R1                                                               
         GOTO1 VRECUP,DMCB,(C'S',AIO1),ELEM,(R6)                                
         B     CNFM290                                                          
*                                                                               
CNFM250  CLC   =C'ORDLIN',1(R3)                                                 
         BNE   CNFM295                                                          
*                                                                               
CNFM255  LA    R4,1(R3)                                                         
         USING RORDLIND,R4                                                      
         PACK  DUB,ROLNBLIN        CONVERT THE BUYLINE TO BINARY                
         CVB   R1,DUB                                                           
         STH   R1,HALF                                                          
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
CNFM260  CLI   0(R6),0             END OF RECORD                                
         BE    CNFM290             AGY BYLN NOT IN ORD, DON'T SEND ERR          
*                                                                               
         CLI   HALF,0              HAVE 2 BYTE BUYLINE                          
         BNE   CNFM265              YES, SKIP X'22' ELEMENTS                    
         CLI   0(R6),DOBUYELQ      FIND CORRECT X'22' 1-BT BUYLN ELEM           
         BNE   CNFM265              NO                                          
         CLC   HALF+1(1),DOBUYSPT-DOBUYELD(R6)                                  
         BE    CNFM270                                                          
         B     CNFM267                                                          
*                                                                               
CNFM265  CLI   0(R6),DOBY2ELQ      FIND CORRECT X'23' 2-BT BUYLN ELEM           
         BNE   CNFM267              NO                                          
         CLC   HALF,DOBY2SPT-DOBY2ELD(R6)                                       
         BE    CNFM270                                                          
CNFM267  LLC   R0,1(R6)            CHECK NEXT ELEM IN RECORD                    
         AR    R6,R0                                                            
         B     CNFM260                                                          
*                                                                               
CNFM270  XC    ELEM,ELEM           COPY THE BUYLINE ELEMENT                     
         LLC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)                                                    
*                                                                               
         LLC   RF,ELEM+1                                                        
         LA    RF,ELEM(RF)                                                      
*                                                                               
         LA    RE,ROLNRLIN         STICK IN REP BUYLINES                        
         LA    R0,10               MAX OF 10 IN EACH ORDLIN                     
*                                                                               
         LLC   R1,0(R3)            GET LENGTH OF THIS ORDLIN                    
         LA    R1,0(R1,R3)         ADDRESS AFTER THIS ORDLIN                    
CNFM275  CR    RE,R1               END OF LIST?                                 
         BNL   CNFM280                                                          
         OC    0(L'ROLNRLIN/10,RE),0(RE)     END OF REP BUYLINE LIST?           
         BZ    CNFM280                                                          
         CLC   0(L'ROLNRLIN/10,RE),ALLSPCES                                     
         BE    CNFM280             YES                                          
         CLC   0(L'ROLNRLIN/10,RE),=4C'0'    ZEROS                              
         BE    CNFM280             YES                                          
         PACK  DUB,0(L'ROLNRLIN/10,RE)                                          
         CVB   R1,DUB                                                           
         STCM  R1,3,0(RF)                                                       
         LA    RF,L'DOBUYREP(RF)     PUT REP BUYLINE IN OUR ELEMENT AND         
         LA    RE,L'ROLNRLIN/10(RE)    CHECK NEXT REP BUYLINE                   
         BCT   R0,CNFM275            ONLY PROCESS MAX 10 BUYLINES               
*                                                                               
CNFM280  LA    RE,ELEM             CALCULATE LENGTH OF THE BUYLINE ELEM         
         SR    RF,RE                                                            
         STC   RF,ELEM+1                                                        
*                                                                               
         CLM   RF,1,1(R6)          ELEMENT DIDN'T CHANGE IN SIZE?               
         BNE   CNFM285                                                          
         LLC   R1,ELEM+1           NO, MIGHT BE DIFFERENT BUYLINES, SO          
         BCTR  R1,0                    COPY IT OVER TO OLD ELEMENT              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),ELEM                                                     
         B     CNFM290                                                          
*                                                                               
CNFM285  GOTO1 VRECUP,DMCB,(C'S',AIO1),(R6),(R6)                                
         GOTO1 VRECUP,DMCB,(C'S',AIO1),ELEM,(R6)                                
*                                                                               
CNFM290  LLC   R0,0(R3)                                                         
         AR    R3,R0                                                            
         B     CNFM210                                                          
         DROP  R4                                                               
*********                                                                       
* DONE WITH THE ORDER RECORD                                                    
*********                                                                       
CNFM295  XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         MVC   AIO,AIO1                                                         
         BRAS  RE,PUT                                                           
*                                                                               
         GOTO1 =A(SEEDCONN),DMCB,(RC),ROCFRPCN,RR=RELO                          
*                                                                               
         CLC   =C'ORDTLR',1(R3)                                                 
         BE    CNFM299                                                          
         L     R1,=AL4(*-T16100)                                                
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
CNFM299  BRAS  RE,CHKFRCHG         BUILD CHECK FOR CHANGES KEY                  
         TM    MISCFLG3,MF3SALPR                                                
         BZ    CNFM300                                                          
         BRAS  RE,SPREASGN                                                      
*********                                                                       
* PROCESS ORDER CONFIRMATION COMMENT AND URL                                    
*********                                                                       
CNFM300  L     R3,AWRKRIOA                                                      
         LA    R3,2(R3)                                                         
         LLC   R2,0(R3)            R2 = L(ORDER CONFIRMATION HEADER)            
         LA    R7,1(R3)            R7 = A(ORDER CONFIRMATION HEADER)            
*                                                                               
         AR    R3,R2                                                            
         CLC   =C'ORDSAL',1(R3)    ALREADY PROCESSED THESE                      
         BNE   CNFM310                                                          
         LLC   R1,0(R3)            SKIP IT                                      
         AR    R3,R1                                                            
*********                                                                       
* ORDER URL                                                                     
*********                                                                       
CNFM310  CLC   =C'ORDURL',1(R3)    DID WE GET A URL CONFIRMATION?               
         BNE   CNFM400             NO                                           
*                                                                               
         LA    R4,1(R3)            YES, WE HAVE ONE                             
         USING RORDURLD,R4                                                      
         NI    BITFLAG2,X'FF'-BF2NWURL                                          
         XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         MVI   KEY+DOKCMT-DOKEY,DOKCURLQ  LOOK FOR URL RECORD                   
*                                                                               
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(L'DOKEY),KEYSAVE  URL RECORD DOESN'T EXIST?                  
         BE    CNFM315                                                          
         OI    BITFLAG2,BF2NWURL     NO, NEED A NEW URL RECORD                  
         L     RE,AIO1                                                          
         LA    RF,2000                                                          
         XCEFL                                                                  
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         MVC   0(L'DOKEY,R6),KEYSAVE                                            
         MVC   DORAGY,AGENCY                                                    
         DROP  R6                                                               
         B     CNFM320                                                          
*                                                                               
CNFM315  MVI   DMINBTS,X'80'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
*                                                                               
CNFM320  L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         LA    R6,DORFRST                                                       
         DROP  R6                                                               
*                                                                               
         LA    R2,ELEM             ADD THE URL TO RECORD                        
         USING DOCM2ELD,R2                                                      
         XC    ELEM,ELEM                                                        
         MVI   DOCM2EL,DOCM2ELQ                                                 
         MVC   DOCM2REV,REVISION                                                
         MVC   DOCM2TXT(L'ROURTEXT),ROURTEXT                                    
*                                                                               
         LA    R0,DOCM2OVH         LENGTH OF OVERHEAD BEFORE TEXT               
         CLI   ROURCONT,C'*'       MORE TO FOLLOW?                              
         BNE   CNFM330                                                          
         LLC   R1,0(R3)            BUMP TO NEXT                                 
         AR    R3,R1                                                            
*                                                                               
         LA    R4,1(R3)                                                         
         MVC   DOCM2TXT+L'ROURTEXT(L'ROURTEXT),ROURTEXT                         
         AHI   R0,L'ROURTEXT       ADD LENGTH OF FULL TEXT                      
*                                                                               
CNFM330  LA    R5,ROURTEXT+L'ROURTEXT-1                                         
CNFM335  CLI   0(R5),C' '                                                       
         BH    CNFM340                                                          
         BCT   R5,CNFM335                                                       
         L     R1,=AL4(*-T16100)                                                
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
CNFM340  LA    R5,1(R5)            BUMP 1 PAST LAST TO GET CORRECT LEN          
         LA    R1,ROURTEXT                                                      
         SR    R5,R1               SUBTRACT TO GET CORRECT LENGTH               
         AR    R0,R5               ADD TO PREVIOUS LENGTH IN R0                 
         STC   R0,DOCM2LEN                                                      
*                                                                               
CNFM350  GOTO1 VRECUP,DMCB,(C'S',AIO1),ELEM,(C'R',(R6))                         
*                                                                               
         L     R6,AIO1                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
*                                                                               
         TM    BITFLAG2,BF2NWURL   NEW REP COMMENT RECORD?                      
         BZ    CNFM360                                                          
         MVC   AIO,AIO1                                                         
         BRAS  RE,ADD                                                           
         B     CNFM375                                                          
*                                                                               
CNFM360  MVC   AIO,AIO1                                                         
         BRAS  RE,PUT              ORDER COMMENTS FINISHED                      
*                                                                               
CNFM375  LLC   R1,0(R3)            BUMP TO NEXT                                 
         AR    R3,R1                                                            
*********                                                                       
* ORDER CONFIRMATION COMMENT                                                    
*********                                                                       
CNFM400  CLC   =C'ORDCOM',1(R3)                                                 
         BNE   CNFM455                                                          
*                                                                               
         NI    BITFLAG1,X'FF'-BF1NWCOM-BF1IGRCM                                 
         XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         MVI   KEY+DOKCMT-DOKEY,2  LOOK FOR REP COMMENT RECORD                  
*                                                                               
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(L'DOKEY),KEYSAVE  REP COMMENT RECORD DOESN'T EXIST?          
         BE    CNFM410                                                          
         OI    BITFLAG1,BF1NWCOM     NO, NEED A NEW REP COMMENT RECORD          
         L     RE,AIO1                                                          
         LA    RF,2000                                                          
         XCEFL                                                                  
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         MVC   0(L'DOKEY,R6),KEYSAVE                                            
         MVC   DORAGY,AGENCY                                                    
         DROP  R6                                                               
         B     CNFM420                                                          
*                                                                               
CNFM410  MVI   DMINBTS,X'80'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
*                                                                               
CNFM420  L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         MVC   DORLEN,=Y(DORFRST-DOKEY)  REMOVES ALL COMMENT ELEMENTS           
         LA    R6,DORFRST                                                       
         DROP  R6                                                               
*                                                                               
         LA    R2,ELEM             ADD THE COMMENTS TO RECORD                   
         USING DOCOMELD,R2                                                      
         XC    ELEM,ELEM                                                        
         MVI   DOCOMEL,DOCOMELQ                                                 
*                                                                               
CNFM425  LA    R4,1(R3)                                                         
         USING RORDCOMD,R4                                                      
*                                                                               
         MVC   DOCOMTXT(4),ROCMBLIN   BUYLINE GOES IN COMMENT                   
         MVI   DOCOMTXT+5,C'-'                                                  
         LLC   R1,0(R3)            L(LENGTH AND LINE)                           
         SH    R1,=Y(ROCMTEXT-RORDCOMD+2)                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DOCOMTXT+7(0),ROCMTEXT                                           
         OC    DOCOMTXT(L'ROCMTEXT+7),ALLSPCES                                  
         DROP  R4                                                               
*                                                                               
         LA    R1,DOCOMTXT+L'ROCMTEXT+7-1                                       
CNFM430  CLI   0(R1),C' '          CALCULATE LENGTH OF COMMENT                  
         BNE   CNFM435                                                          
         BCT   R1,CNFM430                                                       
         L     R1,=AL4(*-T16100)                                                
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
CNFM435  LR    R0,R1                                                            
         SR    R0,R2                                                            
         AH    R0,=H'1'                                                         
         STC   R0,DOCOMLEN                                                      
*                                                                               
         LLC   R1,DOCOMLIN         INCREMENT LINE NUMBER                        
         LA    R1,1(R1)                                                         
         STC   R1,DOCOMLIN                                                      
         DROP  R2                                                               
*                                                                               
         GOTO1 VRECUP,DMCB,(C'S',AIO1),ELEM,(C'R',(R6))                         
         CLI   8(R1),C'R'          RECORD OVERFLOW?                             
         BE    *+12                                                             
         OI    BITFLAG1,BF1IGRCM   YES, IGNORE REST OF REP COMMENTS             
         B     CNFM440                                                          
*                                                                               
         LLC   R0,ELEM+1                                                        
         AR    R6,R0               R6 = A(NEXT INSERTION ADDRESS)               
CNFM440  LLC   R1,0(R3)                                                         
         AR    R3,R1                                                            
         CLC   =C'ORDCOM',1(R3)    GOT ANOTHER COMMENT LINE?                    
         BNE   CNFM445                                                          
         TM    BITFLAG1,BF1IGRCM   YES, IGNORE IT?                              
         BZ    CNFM425                  NO                                      
         B     CNFM440                  YES                                     
*                                                                               
CNFM445  L     R6,AIO1                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
*                                                                               
         TM    BITFLAG1,BF1NWCOM   NEW REP COMMENT RECORD?                      
         BZ    CNFM450                                                          
         MVC   AIO,AIO1                                                         
         BRAS  RE,ADD                                                           
         B     CNFM455                                                          
*                                                                               
CNFM450  MVC   AIO,AIO1                                                         
         BRAS  RE,PUT              ORDER COMMENTS FINISHED                      
*                                                                               
CNFM455  OC    POLORDER,POLORDER   NEED TO CHECK IF ALL BRANDS                  
         BZ    CNFM460             CONFIRMED?                                   
         BRAS  RE,CHKBORDS         YES                                          
         B     CNFMYES                                                          
*                                                                               
CNFM460  TM    BITFLAG2,BF2VARCN   VAR ORDER IS CONFIRMED NOW?                  
         BZ    CNFMYES             NO, NOTHING LEFT TO DO BUT LEAVE             
*                                                                               
         BRAS  RE,GETORDER                                                      
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
         MVC   QBUYER,DOIDBYR      SAVE THE BUYER CODE                          
         MVC   BCLT,DOIDCLT                                                     
         GOTO1 VCLUNPK,DMCB,BCLT,QCLT                                           
         MVC   BEST,DOIDEST                                                     
         EDIT  (B1,BEST),(3,QEST1),FILL=0                                       
         XC    WORK,WORK                                                        
         MVC   WORK+2(L'DOISTA),DOISTA                                          
         GOTO1 VMSUNPK,DMCB,WORK,WORK+10,QSTA                                   
         MVC   QFLTNUM,=C'  '                                                   
         CLI   DOIDFLTN,0                                                       
         BE    CNFM465                                                          
         EDIT  (B1,DOIDFLTN),(2,QFLTNUM),FILL=0                                 
*                                                                               
CNFM465  L     R0,AIO2             BUILD PRDSLIST IN AIO2 FIRST                 
         LH    R1,=Y(IOA2-IOA1)                                                 
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         L     RE,AIO2                                                          
*                                                                               
CNFM470  CLI   0(R6),0                                                          
         BE    CNFM485                                                          
*                                                                               
         CLI   0(R6),DOVPRELQ                                                   
         BE    CNFM480                                                          
CNFM475  LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CNFM470                                                          
*                                                                               
         USING DOVPRELD,R6                                                      
CNFM480  LLC   R1,DOVPRLEN                                                      
         SH    R1,=Y(DOVPROVH+1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),DOVPRPRD                                                 
         LA    RE,1(R1,RE)                                                      
         B     CNFM475                                                          
*                                                                               
CNFM485  LR    R0,RE               SAVE LAST ADDRESS USED BY PRDSLIST           
         BRAS  RE,GETCLTRC                                                      
         L     R6,AIO1                                                          
         USING CLTHDRD,R6                                                       
*                                                                               
         LR    R1,R0               PUT PRDSLIST 10 BYTES AFTER CLT REC          
         L     R0,AIO2                                                          
         SR    R1,R0                                                            
         LA    R0,CLTHDRL+10(R6)                                                
         L     RE,AIO2                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R4,CLTHDRL+10(R6)   R4 = A(1ST PRD PAIR) IN PRDSLIST             
*                                                                               
CNFM490  CLI   0(R4),0             ARE WE DONE WITH PRDSLIST?                   
         BE    CNFM500                                                          
         MVC   QPRD1,0(R4)                                                      
         MVC   QPRD2,1(R4)                                                      
*                                                                               
         GOTOR GETQPRD,DMCB,(QPRD1,QPRD1)                                       
         GOTOR GETQPRD,DMCB,(QPRD2,QPRD2)                                       
*                                                                               
         BRAS  RE,AUTOSCRP         GENERATE THE SCRIPT FILE                     
*                                                                               
         L     R0,AIO1                                                          
         AH    R0,=Y(IOA2-IOA1)                                                 
         LA    R4,2(R4)                                                         
         CR    R4,R0                                                            
         BL    CNFM490                                                          
*                                                                               
CNFM500  BAS   RE,UPDATSB1                                                      
*                                                                               
CNFMYES  TM    BITFLAG2,BF2SPNDG                                                
         BZ    *+8                                                              
         BRAS  RE,SNDSCRPT                                                      
*                                                                               
         BRAS  RE,BLDCOLOR                                                      
         BRAS  RE,DAREMAIL                                                      
         NI    MISCFLG3,X'FF'-MF3RADIO-MF3OK2CF                                 
         B     YES                                                              
*                                                                               
CNFMNO   TM    MISCFLG1,MF1XMTUP   HAVE I MADE CHANGES TO XMT ELEM?             
         BO    CNFM200             YES                                          
         B     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS GENERATES THE SCRIPT REC IN =NWK SO THAT IT WILL AUTOMATICALLY           
* SEND ORDERS FOR QPRD1,QPRD2                                                   
***********************************************************************         
AUTOSCRP NTR1  BASE=*,LABEL=*                                                   
*****                                                                           
         L     R1,SRPARMSD.SRQAUTL                                              
         USING UTLD,R1                                                          
         MVC   TUSER,SIGNON2H      FOOL GETPROF                                 
         DROP  R1                                                               
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SDAR'    <=== NEED LOWER CASE 'S'                     
         NI    WORK,X'FF'-X'40'    MAKE IT LOWERCASE                            
         MVC   WORK+4(2),AGENCY                                                 
*                                                                               
         L     RF,SRPARMSD.SRQACOMF                                             
         L     RF,CGETPROF-COMFACSD(RF)                                         
         XC    PROFDAR,PROFDAR                                                  
         GOTO1 (RF),DMCB,WORK,PROFDAR,VDATAMGR                                  
*                                                                               
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         MVC   WORK(4),=C'S0OM'    OM PROFILE TELLS US IF DAR OR OM             
         MVC   WORK+4(2),AGENCY                                                 
         LA    R4,WORK+16                                                       
         GOTO1 (RF),DMCB,WORK,(R4)                                              
*                                                                               
         L     R1,SRPARMSD.SRQAUTL                                              
         USING UTLD,R1                                                          
         XC    TUSER,TUSER         RESET THE ID NUMBER                          
         DROP  R1                                                               
*                                                                               
         LA    R1,DMCB                                                          
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*****                                                                           
         BRAS  RE,WRKRCREA                                                      
*                                                                               
         L     R1,AIO2                                                          
         XC    0(256,R1),0(R1)                                                  
         LA    R1,4(R1)                                                         
         MVC   0(4,R1),=F'2101'                                                 
         MVC   4(6,R1),=C'SCRIPT'                                               
         MVC   10(8,R1),=C'SPVARORD'                                            
         MVI   18(R1),C'I'                                                      
         MVC   30(5,R1),=C'00006'    HEADER LENGTH                              
         MVC   35(5,R1),=C'00100'    INPUT  DATA LENGTH                         
         MVC   40(5,R1),=C'00020'    OUTPUT DATA LENGTH                         
         MVI   45(R1),C'Y'                                                      
         L     R1,AIO2                                                          
         MVC   0(2,R1),=H'50'        46 + 4 BYTES FOR QSAM                      
*                                                                               
         GOTOR WRKRSEND,DMCB,AIO2                                               
*                                                                               
         L     R1,AIO2                                                          
         XC    0(256,R1),0(R1)                                                  
         LA    R1,4(R1)                                                         
         MVC   0(4,R1),=F'2102'                                                 
         MVC   4(6,R1),=C'000001'                                               
         MVC   10(20,R1),=CL20'SIGN-ON INFORMATION '                            
         MVC   38(8,R1),=CL8'OM'    SIGN ON TO THE OM PROGRAM                   
*                                                                               
         MVC   30(8,R1),ROCFTOID                                                
         TM    BITFLAG1,BF1PSSWD   PASSWORD REQUIRED FOR SPOT SYSTEM?           
         BZ    *+10                                                             
         MVC   46(3,R1),=C'DDS'    YES, THEN USE THE EVER USEFUL DDS            
         L     R1,AIO2                                                          
         MVC   0(2,R1),=H'58'        54 + 4 BYTES FOR QSAM                      
*&&DO                                                                           
         MVC   30(8,R1),=CL8'NEWSTYLE'                                          
         MVC   30+16(8,R1),ROCFTOID                                             
         MVC   30+16+8(9,R1),=CL9',$SPTDARE'                                    
         MVC   30+60(4,R1),=C'T214'   USERID+PID (44 BYTES)                     
         L     R1,AIO2                                                          
         MVC   0(2,R1),=H'98'        94 + 4 BYTES FOR QSAM                      
*&&                                                                             
         GOTOR WRKRSEND,DMCB,AIO2                                               
*                                                                               
         L     R1,AIO2                                                          
         XC    0(256,R1),0(R1)                                                  
         LA    R1,4(R1)                                                         
         MVC   0(4,R1),=F'2102'                                                 
         MVC   4(6,R1),=C'000002'                                               
         MVC   10(20,R1),=CL20'$SPTDARE TO SPOT/DAR'                            
*                                                                               
         LA    R1,30(R1)                                                        
         USING LAYOUT2D,R1                                                      
         MVC   LAY2MED,QMED                                                     
         MVC   LAY2BYR,QBUYER                                                   
*                                                                               
         CLI   POMUSEOM,C'Y'                                                    
         BNE   *+10                                                             
         MVC   LAY2MTHD,QCSHTRDE                                                
*                                                                               
         MVC   LAY2CLT,QCLT                                                     
         MVC   LAY2STA,QSTA                                                     
         MVC   LAY2OORD,ROCFORDR                                                
*                                                                               
         MVC   LAY2PRDS(3),QPRD1                                                
         CLI   QPRD2,C' '          ANY PIGGYBACK?                               
         BNH   *+14                                                             
         MVI   LAY2PRDS+3,C'-'     YES                                          
         MVC   LAY2PRDS+4(3),QPRD2                                              
*                                                                               
         MVC   LAY2EST(L'QEST1),QEST1                                           
         CLC   QFLTNUM,=C'00'                                                   
         BNH   *+14                                                             
         MVI   LAY2EST+3,C'-'                                                   
         MVC   LAY2EST+4(2),QFLTNUM                                             
*                                                                               
         LA    R1,LAY2END                                                       
         DROP  R1                                                               
         L     RE,AIO2                                                          
         SR    R1,RE                                                            
         STH   R1,0(RE)            L'DATA + 4 BYTES FOR QSAM                    
*                                                                               
         GOTOR WRKRSEND,DMCB,AIO2                                               
*                                                                               
         BRAS  RE,WRKRCLOS                                                      
*                                                                               
         L     RE,AWRKRIOA         RESET WORKER IO AREA FOR EDICT RECS          
         MVI   0(RE),0                                                          
         MVI   1(RE),2                                                          
ASCRPX   B     XIT                                                              
         DROP  R7                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECKS TO SEE IF ALL BRAND ORDERS OF A POL ORDER ARE CONFIRMED                
***********************************************************************         
CHKBORDS NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'**BORD**'                                                    
*                                                                               
         NI    BITFLAG2,X'FF'-BF2ALLPD   NOT ALL PENDING CONFIRMED YET          
         NI    MISCFLG1,X'FF'-MF1XMTUP                                          
*                                                                               
CKBORD00 XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING DOKEY,R6                                                         
         MVI   DVKTYPE,DVKTYPQ                                                  
         MVI   DVKSUBTY,DVKSTYPQ                                                
         MVC   DVKAGMD,BAGYMD                                                   
         MVC   DVKPORD,POLORDER                                                 
         DROP  R6                                                               
*                                                                               
         BRAS  RE,HIGH                                                          
*                                                                               
CKBORD07 CLC   KEY(DVKBORD-DOKEY),KEYSAVE   CHECKED ALL BRAND ORDERS?           
         BNE   CKBORD50            YES                                          
*                                                                               
         TM    BITFLAG2,BF2ALLPD   ALL BRANDS CONFIRM PENDING?                  
         BZ    *+8                                                              
         MVI   DMINBTS,X'80'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
*                                                                               
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         TM    BITFLAG2,BF2ALLPD   ALL BRANDS CONFIRM PENDING?                  
         BZ    *+8                                                              
         OI    DORSTAT,X'01'       YES, MARK AS CONFIRMED                       
*                                                                               
         LA    R6,DORFRST                                                       
CKBORD10 CLI   0(R6),0                                                          
         BNE   CKBORD15                                                         
         L     R1,=A(*-T16100)                                                  
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
CKBORD15 CLI   0(R6),DOXMTELQ                                                   
         BE    CKBORD20                                                         
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CKBORD10                                                         
*                                                                               
         USING DOXMTELD,R6                                                      
CKBORD20 CLI   DOXMTSTA,QCFMDPND   MARKED AS CONFIRM PENDING?                   
         BNE   CKBORDX             NO, WAIT FOR CONFIRM FOR THIS ORDER          
*                                                                               
         TM    BITFLAG2,BF2ALLPD   YES, TIME TO MARK AS CONFIRMED?              
         BZ    CKBORD30                 NO, MORE BRANDS ARE POSSIBLE            
*                                                                               
         MVI   DOXMTSTA,QCFMD           YES, AND WRITE RECORD OUT               
*                                                                               
         MVC   DKEY,KEY                                                         
         XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         ST    R6,AIO                                                           
         BRAS  RE,PUT                                                           
         MVC   KEY,DKEY                                                         
*                                                                               
CKBORD30 BRAS  RE,SEQ                                                           
         B     CKBORD07            CHECK NEXT BRAND ORDER                       
*                                                                               
CKBORD50 TM    BITFLAG2,BF2ALLPD   MARKED AS CONFIRM PENDING ALREADY?           
         BNZ   CKBORDX             YES, WE'RE DONE THEN                         
         OI    BITFLAG2,BF2ALLPD   NO, TIME TO CHANGE TO ALL CONFIRM            
         OI    MISCFLG1,MF1XMTUP                                                
         B     CKBORD00                                                         
*                                                                               
CKBO100  NI    BITFLAG2,X'FF'-BF2ALLPD   NOT ALL PENDING CONFIRMED YET          
         XC    CURDATTM,CURDATTM                                                
*                                                                               
CKBO110  XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING DOKEY,R6                                                         
         MVI   DVKTYPE,DVKTYPQ                                                  
         MVI   DVKSUBTY,DVKSTYPQ                                                
         MVC   DVKAGMD,BAGYMD                                                   
         MVC   DVKPORD,POLORDER                                                 
         DROP  R6                                                               
*                                                                               
         BRAS  RE,HIGH                                                          
*                                                                               
CKBO117  CLC   KEY(DVKBORD-DOKEY),KEYSAVE   CHECKED ALL BRAND ORDERS?           
         BNE   CKBO150             YES                                          
*                                                                               
         TM    BITFLAG2,BF2ALLPD   ALL BRANDS CONFIRM PENDING?                  
         BZ    *+8                                                              
         MVI   DMINBTS,X'80'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
*                                                                               
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         TM    BITFLAG2,BF2ALLPD   ALL BRANDS CONFIRM PENDING?                  
         BZ    *+8                                                              
         OI    DORSTAT,X'01'       YES, MARK AS CONFIRMED                       
*                                                                               
         LA    R6,DORFRST                                                       
CKBO120  CLI   0(R6),0                                                          
         BNE   CKBO125                                                          
         TM    MISCFLG1,MF1XMTUP   DID I UPDATE THE XMT?                        
         BNZ   CKBORDX             YES: DON'T SEND MESSAGE, EXIT!!!             
         L     R1,=A(*-T16100)                                                  
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
CKBO125  CLI   0(R6),DOSTELQ                                                    
         BE    CKBO130                                                          
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CKBO120                                                          
*                                                                               
         USING DOSTELD,R6                                                       
CKBO130  CLI   DOSTSTAT,QCFMDPND   MARKED AS CONFIRM PENDING?                   
         BNE   CKBORDX             NO, WAIT FOR CONFIRM FOR THIS ORDER          
*                                                                               
         TM    BITFLAG2,BF2ALLPD   YES, TIME TO MARK AS CONFIRMED?              
         BNZ   CKBO135                YES                                       
*                                     NO, MORE BRANDS ARE POSSIBLE              
         CLC   DOSTDATE,CURDATE       GET MOST RECENT DATE AND TIME             
         BH    CKBO132                                                          
         BL    CKBO140                                                          
         CLC   DOSTTIME,CURTIME                                                 
         BNH   CKBO140                                                          
CKBO132  MVC   CURDATTM,DOSTDATE                                                
         B     CKBO140                  NO, MORE BRANDS ARE POSSIBLE            
         DROP  R6                                                               
*                                                                               
CKBO135  LA    R4,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
         USING DOSTELD,R4                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   DOSTEL,DOSTELQ                                                   
         MVI   DOSTLEN,DOSTLNQ3                                                 
         MVC   DOSTDATE,CURDATE       MOVE IN MOST RECENT CNFMPDNG DATE         
         MVC   DOSTTIME,CURTIME       AND TIME                                  
         MVI   DOSTSTAT,QCFMD                                                   
*                                                                               
         L     R6,AIO                                                           
         LA    R6,DORFRST-DOKEY(R6)  R6 = A(FIRST ELEMENT IN RECORD)            
CKBO137  CLI   0(R6),0          ADD AFTER ALL TRANSMISSION ELEMENTS             
         BE    CKBO138          AND BEFORE AN AUDIT ELEMENT                     
         CLI   0(R6),DORPELQ2                                                   
         BE    CKBO138                                                          
         CLI   0(R6),DOSTELQ                                                    
         BNL   CKBO138                                                          
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CKBO137                                                          
*                                                                               
CKBO138  GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
*                                                                               
         MVC   DKEY,KEY                                                         
         XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         ST    R6,AIO                                                           
         BRAS  RE,PUT                                                           
         MVC   KEY,DKEY                                                         
*                                                                               
CKBO140  BRAS  RE,SEQ                                                           
         B     CKBO117             CHECK NEXT BRAND ORDER                       
*                                                                               
CKBO150  TM    BITFLAG2,BF2ALLPD   MARKED AS CONFIRM PENDING ALREADY?           
         BNZ   CKBORDX             YES, WE'RE DONE THEN                         
         OI    BITFLAG2,BF2ALLPD   NO, TIME TO CHANGE TO ALL CONFIRM            
         B     CKBO110                                                          
*                                                                               
CKBORDX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
CURDATTM DS    0XL7                                                             
CURDATE  DS    XL3                                                              
CURTIME  DS    XL2                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SEEDS THE CONTRACT NUMBER TO ALL THE BUYLINES THAT ARE           
* PART OF THE AGENCY ORDER.                                                     
*                                                                               
* ON ENTRY:    PARAM 1             (RC)                                         
*              PARAM 2             A(REP CONTRACT NUMBER)                       
*              AIO1                A(AGENCY ORDER RECORD)                       
***********************************************************************         
SEEDCONN NTR1  BASE=*,LABEL=*                                                   
*        NMOD1 0,**SDCN**                                                       
         L     RC,0(R1)                                                         
         L     R9,SRPARMSD.SRQASYSF                                             
*                                                                               
         L     R2,4(R1)                                                         
         MVC   CONTNUMB,0(R2)      MAKE A COPY OF REP CONTRACT NUMBER           
*                                                                               
         L     R6,AIO1                                                          
         USING DOKEY,R6                                                         
         CLI   DOKCMT,0            DO WE HAVE A COMMENT RECORD HERE?            
         BE    SDCN00                                                           
         BRAS  RE,GETORDER         READ ORDER RECORD INTO AIO1                  
*                                                                               
SDCN00   SR    R0,R0                                                            
         XC    TRDEMTHD(9),TRDEMTHD      CLEAR METHOD AND DATA                  
         LA    R6,DORFRST-DOKEY(R6)                                             
SDCN02   CLI   0(R6),0                                                          
         BE    SDCN10                                                           
         CLI   0(R6),DOSPELQ       GET TRADE METHOD AND SEE IF TRADE            
         BE    SDCN04                                                           
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     SDCN02                                                           
*                                                                               
         USING DOSPELD,R6                                                       
SDCN04   CLI   DOSPLEN,DOSPLNQ                                                  
         BNH   SDCN10                                                           
         MVC   TRDEMTHD,DOSPTMTH   HOW TRADE IS DONE (LWRCASE IS CASH)          
         MVC   TRDEDATA,DOSPTDAT                                                
*                                                                               
SDCN10   L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
         CLI   0(R6),1             MUST HAVE A DARE ID ELEMENT                  
         BE    SDCN15                                                           
         L     R1,=A(*-T16100)     WE SHOULD HAVE A DARE ID ELEMENT             
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                  CHECK IF WE CAN SEED CONTRACT NUMBER         
         USING DOIDELD,R6                                                       
SDCN15   LA    R4,KEY                                                           
         XC    KEY,KEY             READ CLIENT RECORD                           
         USING CLTHDRD,R4                                                       
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,DOIDCLT                                                  
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(L'DOKEY),KEYSAVE   SAME BASE KEY?                            
         BE    SDCN20                                                           
         L     R1,=A(*-T16100)     WE SHOULD HAVE A DARE ID ELEMENT             
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
SDCN20   MVI   DMINBTS,X'80'                                                    
         MVC   AIO,AIO2                                                         
         BRAS  RE,GET                                                           
         L     R4,AIO2                                                          
         CLI   CEXTRA+2,C'N'       CAN WE CHANGE THE ID?                        
         BE    SEEDC00                                                          
         CLI   CEXTRA+2,C'Y'                                                    
         BNE   SEEDCX              NO                                           
         DROP  R4                                                               
*                                                                               
SEEDC00  XC    KEY,KEY             READ STATION MASTER RECORD                   
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),QMED                                                    
         XC    BMKTSTA,BMKTSTA                                                  
         MVC   BMKTSTA+2(L'DOISTA),DOISTA                                       
         GOTO1 VMSUNPK,DMCB,BMKTSTA,FULL,KEY+2                                  
         CLI   KEY+6,C' '          ANY BAND?                                    
         BH    *+8                                                              
         MVI   KEY+6,C'T'                                                       
         MVC   KEY+7(2),AGENCY                                                  
         GOTO1 VCLUNPK,DMCB,DOIDCLT,KEY+9                                       
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMREAD,STATION,KEY,AIO2                            
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         L     R4,AIO2                                                          
         USING STARECD,R4                                                       
         GOTO1 VMSPACK,DMCB,SMKT,STAKCALL,BMKTSTA                               
         DROP  R4                                                               
*                                                                               
         MVI   GBYACT,GBYINIT                                                   
         BRAS  RE,GOGETBUY                                                      
         MVC   SV1OR2,GBY1OR2                                                   
*                                                                               
         CLC   AGENCY,=C'SJ'       AGENCY SJ?                                   
         BNE   SEEDC05              NO                                          
         CLC   DOIDCLT,=X'CC2B'    CLIENT TBL?                                  
         BE    *+10                                                             
         CLC   DOIDCLT,=X'BCC9'    CLIENT PG0?                                  
         BE    *+10                 YES                                         
         CLC   DOIDCLT,=X'BCDA'    CLIENT PG1?                                  
         BNE   SEEDC05              NO, NONE OF THESE CLIENTS                   
         MVI   SV1OR2,2            TURN ON 2-BYTE BLN                           
*                                                                               
SEEDC05  XC    KEY,KEY             SET UP TO READ BUYLINES                      
         LA    R4,KEY                                                           
         USING BUYKEY,R4                                                        
         MVC   BUYKAM,BAGYMD                                                    
         MVC   BUYKCLT,DOIDCLT                                                  
         MVC   BUYKPRD,DOIDPRD                                                  
         MVC   PIGPRD,DOIDPRD2     SAVE PIGGY PRODUCT BINARY CODE               
         MVC   PIGEST,DOIDEST      THEN IT IS THE SAME ESTIMATE                 
*                                                                               
         MVC   BUYMSTA,BMKTSTA                                                  
         MVC   BUYKEST,DOIDEST                                                  
*                                                                               
         MVI   GBYACT,GBYHIGH                                                   
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(BUYKBUY-BUYKEY),KEYSAVE SAME BASE KEY?                       
         BNE   SEEDCX                      EXITING IS BETTER THAN DYING         
*                                                                               
         CLI   BUYKBUY,X'FF'       BRAND-POL BUY?                               
         LA    R4,KEYSAVE          KEYSAVE WILL BE THE KEY WE NEED              
         BNE   SEEDCX              NO, NO LONGER SUPPORTED                      
         MVI   BUYKBUY,X'FF'       YES, CHANGE KEYSAVE TO B-POL                 
         B     SEEDC15                                                          
*                                                                               
SEEDC10  CLI   0(R6),0             NO MORE BUYLINE ELEMENTS?                    
         BE    SEEDCX               YES                                         
         CLI   0(R6),DOBUYELQ      BUYLINE ELEMENT?                             
         BE    SEEDC20              YES, GO PROCESS IT                          
         CLI   0(R6),DOBY2ELQ      BUYLINE ELEMENT?                             
         BE    SEEDC25              YES, GO PROCESS IT                          
SEEDC15  LLC   R0,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,R0                                                            
         B     SEEDC10                                                          
*                                                                               
         USING DOBUYELD,R6                                                      
SEEDC20  CLI   BUYKBUY,X'FF'       BRAND-POL BUY?                               
         JNE   *+2                  NO, NON-POL NO LONGER SUPPORTED             
         MVI   BUYKBUY+1,0                                                      
         MVC   BUYKBUY+2(L'DOBUYSPT),DOBUYSPT                                   
         B     SEEDC30                                                          
*&&DO                                                                           
         XC    BUYKBUY,BUYKBUY                                                  
         TM    DOBUYFLG,DOBUYFPG   PIGGYBACK PASSIVE?                           
         BNZ   SEEDC22                                                          
*                                                                               
         MVI   BUYKBUY+2,1         NO, ASSUME BUY REGULAR, NO P/B               
         TM    DOBUYFLG,DOBUYFPA   PIGGYBACK ACTIVE?                            
         BZ    *+8                                                              
         MVI   BUYKBUY+2,2         ASSUME BUY REGULAR, NO P/B                   
         MVC   BUYKBUY+1(L'DOBUYSPT),DOBUYSPT                                   
         B     SEEDC30                                                          
*                                                                               
SEEDC22  MVC   BUYKBUY(L'PIGPRD),PIGPRD         YES, PIGGY PASSIVE              
         MVC   BUYKBUY+1(L'PIGEST),PIGEST                                       
         MVC   BUYKBUY+2(L'DOBUYSPT),DOBUYSPT                                   
         B     SEEDC30                                                          
*&&                                                                             
         DROP  R6                                                               
*                                                                               
         USING DOBY2ELD,R6                                                      
SEEDC25  CLI   BUYKBUY,X'FF'       BRAND-POL BUY?                               
         JNE   *+2                  NO, NON-POL NO LONGER SUPPORTED             
         MVC   BUYKBUY+1(L'DOBY2SPT),DOBY2SPT                                   
         B     SEEDC30                                                          
*&&DO                                                                           
         XC    BUYKBUY,BUYKBUY                                                  
         TM    DOBY2FLG,DOBY2FPG   PIGGYBACK PASSIVE?                           
         BNZ   SEEDC27                                                          
*                                                                               
         MVI   BUYKBUY+2,1         NO, ASSUME BUY REGULAR, NO P/B               
         TM    DOBY2FLG,DOBY2FPA   PIGGYBACK ACTIVE?                            
         BZ    *+8                                                              
         MVI   BUYKBUY+2,2         ASSUME BUY REGULAR, NO P/B                   
         MVC   BUYKBUY+1(L'DOBUYSPT),DOBUYSPT                                   
         B     SEEDC30                                                          
*                                                                               
SEEDC27  MVC   BUYKBUY(L'PIGPRD),PIGPRD         YES, PIGGY PASSIVE              
         MVC   BUYKBUY+1(L'PIGEST),PIGEST                                       
         MVC   BUYKBUY+2(L'DOBUYSPT),DOBUYSPT                                   
*&&                                                                             
         DROP  R6                                                               
SEEDC30  CLC   KEY(BUYKBUY-BUYKEY),KEYSAVE MAKE SURE SAME BASE KEY              
         BE    SEEDC33                                                          
*                                   BUYLINE NOT FOUND                           
         MVC   KEY,KEYSAVE          RESTORE BASE KEY                            
         MVI   GBYACT,GBYHIGH                                                   
         BRAS  RE,HIGH                                                          
         B     SEEDC15              AND CHECK NEXT DOBUY/DOBY2                  
*                                                                               
SEEDC33  CLC   KEY(L'BUYKEY),KEYSAVE       HAVE A MATCH FOR BUYLINE?            
         BE    SEEDC40                     YES                                  
         DROP  R4                                                               
*                                                                               
SEEDC35  MVI   GBYACT,GBYSEQ                                                    
         BRAS  RE,SEQ              NO, CHECK NXT KEY                            
         B     SEEDC30                                                          
*                                                                               
SEEDC40  MVC   AIO,AIO2                                                         
         MVI   DMINBTS,X'88'       READ FOR UPDATE AND DELETED                  
         MVI   GBYACT,GBYGET                                                    
         BRAS  RE,GET                                                           
         CLI   GBYERR,0                                                         
         BE    SEEDC42                                                          
         CLI   GBYERR,X'02'        RECORD DELETED?                              
         BE    SEEDC35             YES, READ NEXT RECORD                        
         DC    H'0'                                                             
*                                                                               
SEEDC42  L     R2,AIO2                                                          
         USING BUYRECD,R2                                                       
         MVC   BYTE,TRDEMTHD                                                    
         OI    BYTE,C' '                                                        
*                                                                               
         CLI   BYTE,C'R'           TRADE DETERMINED BY REP?                     
         BNE   SEEDC49X            NO                                           
         XR    R1,R1               YES,                                         
         ICM   R1,3,BDREP                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(3),DUB                                                      
         CLC   WORK(3),TRDEDATA    BUY'S REP MATCHES TRADE REP?                 
         BNE   SEEDC45             NO                                           
         TM    TRDEMTHD,X'40'      YES, ORDER IS CASH?                          
         BZ    SEEDC35                  YES, NOT A CASH BUY                     
         B     SEEDC49X                                                         
*                                                                               
SEEDC45  TM    TRDEMTHD,X'40'      ORDER IS TRADE?                              
         BNZ   SEEDC35             YES, NOT A TRADE BUY                         
*                                                                               
SEEDC49X DS    0H                                                               
*  PAST THE TRADE METHOD TEST                                                   
         TM    BDCIND2,X'02'       TEST TRADE BUY                               
         BO    SEEDC35             DON'T CHANGE THE CONTRACT                    
         LA    R2,BDELEM           R2 = A(FIRST ELEMENT)                        
         SR    R0,R0                                                            
         DROP  R2                                                               
*                                                                               
SEEDC50  CLI   0(R2),0             END-OF-RECORD?                               
         BE    SEEDC60                                                          
         CLI   0(R2),IDELCODQ      A X'70' OUT THERE ALREADY?                   
         BE    SEEDCNXB             YES, SKIP SEEDING CONTRACT                  
         IC    R0,1(R2)            YES, PUT REP CONTRACT # BEFORE               
         AR    R2,R0                                                            
         B     SEEDC50                                                          
SEEDC60  DS    0H                                                               
*&&DO                                                                           
         CLI   0(R2),IDELCODQ      X'70' - ID ELEMENT?                          
         BNE   SEEDC70                                                          
         CLI   1(R2),15                                                         
         BL    SEEDC70                                                          
         MVC   3(L'CONTNUMB,R2),CONTNUMB   CHANGE THE ID                        
         B     SEEDC80                                                          
         BE    SEEDCNXB            IF WE HAVE ONE, SKIP IT                      
*&&                                                                             
*                                                                               
SEEDC70  XC    ELEM,ELEM           HARDCODED BECAUSE NO REAL DSECT              
         MVI   ELEM,X'70'                                                       
         MVI   ELEM+1,15                                                        
         MVC   ELEM+3(L'CONTNUMB),CONTNUMB                                      
         GOTO1 VRECUP,DMCB,(C'S',AIO2),ELEM,(R2)                                
*                                                                               
SEEDC80  MVC   WORK(L'KEY),KEY                                                  
         XC    KEY,KEY                                                          
         L     RE,AIO2                                                          
         MVC   KEY(L'BUYKEY),0(RE)                                              
         ST    RE,AIO                                                           
         MVI   GBYACT,GBYPUT                                                    
         BRAS  RE,PUT                                                           
         MVC   KEY,WORK            RE-ESTABLISH OUR PREVIOUS KEY                
*                                                                               
SEEDCNXB MVI   GBYACT,GBYSEQ                                                    
         BRAS  RE,SEQ              CHECK NEXT KEY                               
         B     SEEDC15                                                          
*                                                                               
SEEDCX   J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD/WRITE TO THE ORDER SALESPERSON REASSIGNMENT RECORD X'04' TYPE             
*                                                                               
* ON ENTRY: DOSAELEM    HAS THE X'12' ADDED ELEM + SALESPERSON                  
*                                                                               
***********************************************************************         
SPREASGN NTR1  BASE=*,LABEL=*                                                   
         NI    MISCFLG3,X'FF'-MF3SALPR                                          
*                                                                               
         L     R3,AWRKRIOA                                                      
         LA    R3,2(R3)                                                         
         LLC   R2,0(R3)            R2 = L(ORDRCL HEADER)                        
         LA    R7,1(R3)            R7 = A(ORDRCL HEADER)                        
*                                                                               
SPREP05  LLC   R2,0(R3)            BUMP TOP NEXT RECORD TYPE                    
         AR    R3,R2                                                            
         CLC   =C'ORDTLR',1(R3)    ORDER TRAILER?                               
         BE    SPREPX                                                           
         CLC   =C'ORDSAL',1(R3)    SALESPERSON REASSIGNMENT?                    
         BNE   SPREP05             NO                                           
         LA    R4,1(R3)            YES, WE HAVE ONE                             
         USING RORDSALD,R4                                                      
*                                                                               
         XC    KEY,KEY                                                          
         L     R6,AIO1                                                          
         MVC   KEY(L'DOKEY),0(R6)                                               
         MVI   KEY+12,X'04'        SALESPERSON REASSIGNMENT RECORD              
         L     R6,AIO2                                                          
         MVC   0(L'DOKEY,R6),KEY                                                
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'DOKEY),KEYSAVE   DO WE HAVE ONE?                           
         BNE   SPRE20                                                           
         MVI   DMINBTS,X'80'                                                    
         BRAS  RE,GET                                                           
         L     R6,AIO                                                           
         B     SPRE40                                                           
*                                                                               
SPRE20   OI    MISCFLG3,MF3RECNF                                                
         L     RE,AIO                                                           
         LA    RF,2000                                                          
         XCEFL                                                                  
*                                                                               
         L     R6,AIO              NOTE: SALESPERSON REASSIGNMENT REC           
         USING DOKEY,R6                                                         
         MVC   DOKEY,KEYSAVE                                                    
         MVC   DORLEN,=Y(DORFRST-DOKEY)                                         
         MVC   DORAGY,AGENCY                                                    
         DROP  R6                                                               
*                                                                               
SPRE40   LA    R6,DORFRST-DOKEY(R6)                                             
         CLI   0(R6),0             THIS SHOULD NEVER HAPPEN                     
         BE    SPREP50             BUT ADD THE ELEM ANYWAYS                     
OLD      USING DOSALPRD,R6                                                      
         CLC   ROSPSALP,OLD.DOSASALC                                            
         BNE   SPREP50                                                          
         CLC   ROSPSALN,OLD.DOSASALP                                            
         BE    SPREPX                                                           
         MVC   OLD.DOSASALP,ROSPSALN  UPDATE THE NAME                           
         OC    OLD.DOSASALP,=20C' '                                             
         B     SPREP55                                                          
*                                                                               
SPREP50  LA    R3,DOSAELEM                                                      
         USING DOSALPRD,R3                                                      
         MVI   DOSAPEL,DOSAPELQ    X'60'                                        
         MVI   DOSALEN,DOSALNQ2                                                 
         MVC   DOSASALP,ROSPSALN   S/P NAME                                     
         OC    DOSASALP,=20C' '                                                 
         MVC   DOSASALC,ROSPSALP   S/P CODE                                     
         CLI   0(R6),0                                                          
         BE    SPREP54                                                          
         TM    OLD.DOSAFLAG,DOSAPPER                                            
         BZ    SPREP54                                                          
         OI    DOSAFLAG,DOSAPPER                                                
*                                                                               
SPREP54  GOTO1 VRECUP,DMCB,(C'S',AIO),DOSAELEM,(R6)                             
         DROP  R3,R4,OLD                                                        
*                                                                               
         TM    MISCFLG3,MF3RECNF   ADDING OR WRITING?                           
         BNZ   SPREP60             ADDING                                       
SPREP55  GOTO1 PUT                 WRITING                                      
         B     SPREPX                                                           
*                                                                               
SPREP60  XC    KEY,KEY             ADDING                                       
         L     R6,AIO                                                           
         MVC   KEY(L'DOKEY),0(R6)                                               
         BRAS  RE,ADD              DIRECTORY KEY AUTOMATICALLY ADDED            
*                                                                               
SPREPX   NI    MISCFLG3,X'FF'-MF3RECNF                                          
         B     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FOR REP PROCESSING DELNOTS AND ERRNOTS                                        
* GETS THE REP DARE ORDER HEADER RECORD FOR UPDATE AND PUTS IT IN AIO1          
* ROUTINE READS THE REP'S AGENCY RECORD TO GET THE AGENCY ROUTING CODE          
* TO BUILD THE DARE ORDER KEY                                                   
*                                                                               
* IF DARE ORDER TYPE X'41' NOT FOUND, IT'LL LOOK FOR                            
* AND PASS BACK X'51' IF FOUND                                                  
*                                                                               
* ON ENTRY:    AIO1                CONTRACT RECORD                              
*                                                                               
* USES AIO2 TO READ AGENCY RECORD                                               
*                                                                               
* ON EXIT:     CONDITION CODE      NE - RECORD DOESN'T EXIST                    
*                                  EQ - RECORD IS IN AIO1                       
***********************************************************************         
GETREPDR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* READ REP AGENCY RECORD TO GET AGENCY ROUTING CODE SO WE CAN FIND              
* THE AGENCY ORDER IN THE REP'S FILE                                            
*                                                                               
         L     R6,AIO1                                                          
         USING RCONREC,R6                                                       
*                                                                               
         LA    R4,KEY                                                           
         USING RAGY2KEY,R4                                                      
*                                                                               
         XC    RAGY2KEY,RAGY2KEY                                                
         MVI   RAGK2TYP,RAGK2TYQ                                                
         MVC   RAGK2AGY,RCONKAGY                                                
         MVC   RAGK2AOF,RCONKAOF                                                
         MVC   RAGK2REP,RCONKREP                                                
         DROP  R4                                                               
*                                                                               
         BRAS  RE,HIGHREP                                                       
*                                                                               
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE                                    
         BNE   GRPDRNO                                                          
*                                                                               
         MVC   AIO,AIO2                                                         
         BRAS  RE,GETREP                                                        
*                                                                               
         MVI   REPORDTP,X'41'                                                   
*                                                                               
GRPDR35  DS    0H                                                               
         L     R6,AIO1                                                          
         USING RCONREC,R6                                                       
*                                                                               
         L     R5,AIO2                                                          
         USING RAGY2REC,R5                                                      
*                                                                               
         LA    R4,KEY                                                           
         USING RDARKEY,R4                                                       
         XC    KEY,KEY                                                          
         MVC   RDARKTYP,REPORDTP                                                
         MVC   RDARKREP,RCONKREP                                                
         MVC   RDARKSTA(5),RCONKSTA                                             
         CLI   RDARKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RDARKSTA+4,C'T'     MUST SPECIFY IF TV                           
         OC    RDARKSTA,=CL6' '                                                 
         OC    RAGY2DAR,RAGY2DAR   NULL EQUIVALENCY CODE?                       
         BZ    GRPDRNO                                                          
         MVC   RDARKAGY,RAGY2DAR   EQUIVALENCY CODE                             
                                                                                
         L     R6,AIO1                                                          
         USING RCONDREL,R6                                                      
         MVC   DATADISP,=H'34'     SET UP FOR REPFILE                           
         MVI   ELCODE,X'1D'        GET DARE ELEMENT                             
         BRAS  RE,GETEL                                                         
         BNE   GRPDRNO                                                          
*                                                                               
         LA    R5,RAGY2DAR         THERE ARE MAX 4 AGENCY ASSIGNMENT            
         LA    R3,4                COMBINATIONS WE NEED TO CHECK                
         B     GRPDR60                                                          
         DROP  R5                                                               
*                                                                               
** PREP KEY FOR SKIP READING: SKIP TO NEXT AGENCY OFFICE IF AGENCY              
** OFFICE DIDN'T CHANGE                                                         
*                                                                               
PRVKEY   USING RDARKEY,KEYSAVE                                                  
GRPDR40  CLC   RDARKAOF,PRVKEY.RDARKAOF  SAME AGENCY OFFICE?                    
         DROP  PRVKEY                                                           
         BNE   GRPDR50                                                          
         XR    R0,R0                                                            
         ICM   R0,3,RDARKAOF                                                    
         AHI   R0,1                   INCREMENT AGENCY OFFICE FIELD             
         STCM  R0,3,RDARKAOF                                                    
GRPDR50  XC    RDARKORD(7),RDARKORD  CLEAR FELDS AFTER AGENCY OFFICE            
*                                                                               
GRPDR60  DS    0H                                                               
         BRAS  RE,HIGHREP                                                       
*                                                                               
         CLC   KEY(RDARKAOF-RDARKEY),KEYSAVE   SAME KEY?                        
         BNE   GRPDR70                                                          
         XC    RDARKORD(7),RDARKORD  CLEAR FIELDS AFTER AGENCY OFFICE           
         MVC   RDARKORD,RCONDRLK     MOVE IN ORDER # FOR RDHI                   
*                                                                               
         BRAS  RE,HIGHREP                                                       
*                                                                               
         CLC   KEY(RDARKAOF-RDARKEY),KEYSAVE   SAME KEY?                        
         BNE   GRPDR70                                                          
         CLC   RDARKORD,RCONDRLK     SAME ORDER NUMBER?                         
         BNE   GRPDR40               NO -- SKIP READ                            
         CLI   RDARKRT,X'10'         AGENCY HEADER?                             
         BE    GRPDR100              YES -- DARE RECORD BUILT...                
         B     GRPDR90                                                          
         DROP  R6                                                               
*                                                                               
GRPDR70  DS    0H                                                               
         L     R2,AIO2                                                          
         USING RAGY2REC,R2                                                      
         CLI   RAGY2FXL,RAGY2FLQ   ONLY NEWER AGENCY RECORDS                    
         JL    *+2                 HAS MULTI-DARE AGENCY ASSIGNMENTS            
         MVC   KEY,KEYSAVE                                                      
         DROP  R2                                                               
*                                                                               
GRPDR80  LA    R5,5(R5)                                                         
         OC    0(3,R5),0(R5)         NULL EQUIVALENCY CODE?                     
         BZ    GRPDR90                                                          
         CLC   RDARKAGY,0(R5)        SAME EQUIVALENCY CODE?                     
         BNE   *+12                                                             
         BCT   R3,GRPDR80            CHECK NEXT EQUIVALENCY CODE                
         B     GRPDR90                                                          
*                                                                               
         MVC   RDARKAGY,0(R5)      EQUIVALENCY CODE                             
         XC    RDARKAOF(9),RDARKAOF  CLEAR FIELDS AFTER AGENCY CODE             
         BCT   R3,GRPDR60                                                       
         DROP  R4                                                               
*                                                                               
* ORDER WAS NOT FOUND, CHECK IF ORDER IS CONFIRMED ELSE SKIP WRITING            
* AUDIT TRAIL                                                                   
*                                                                               
GRPDR90  DS    0H                                                               
         CLI   REPORDTP,X'51'      CAN'T FIND ACTIVE OR CONFIRM ORDER?          
         BE    GRPDRNO             NOPE, SKIP                                   
         MVI   REPORDTP,X'51'      ELSE CHECK IF ORDER WAS CONFIRMED            
         B     GRPDR35                                                          
*                                                                               
* READ REP ORDER INTO AIO1                                                      
*                                                                               
GRPDR100 DS    0H                                                               
         MVC   AIO,AIO1                                                         
         MVI   DMINBTS,X'80'                                                    
         BRAS  RE,GETREP                                                        
*                                                                               
GRPDRYES B     YES                                                              
*                                                                               
GRPDRNO  B     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* READ THE ORDER RECORD/DELETE CURRENT COLOR PTR/ADD NEW COLOR PTR              
* SVSTAT HAS STATUS OF CURRENT NOTICE REC IN IO1                                
* ELEM CONTAINS STATUS ELEMENT FROM MAKEGOOD NOTICE RECORD                      
*=====================================================================*         
         SPACE 1                                                                
BLDCOLOR NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,GETORDER         READ THE ORDER RECORD TO AIO1                
*                                                                               
         NI    MISCFLG2,X'FF'-MF2CNFCM                                          
         L     R6,AIO                                                           
         LA    R6,DORFRST-DOKEY(R6)                                             
         MVI   ELCDLO,DOSPELQ                                                   
         MVI   ELCDHI,DOSPELQ                                                   
         BRAS  RE,BNEXTEL                                                       
         USING DOSPELD,R6                                                       
         TM    DOSPFLG1,DOSPCFCM          CONFIRM WITH COMMENT?                 
         BZ    *+8                                                              
         OI    MISCFLG2,MF2CNFCM                                                
*                                                                               
BLDCLR3  MVI   BYTE,0                                                           
         CLI   THISISMG,C'Y'       IS THIS FOR A MAKEGOOD?                      
         BNE   BLDCLR5             NO, GET CURRENT ORDER STATUS                 
*                                                                               
*** CODE FOR MKGD ONLY             YES                                          
         BRAS  RE,MKGSTADR         R1 = A(STATUS TABLE)                         
         CLI   SVSTAT,0            WAS SVSTAT SET? ONLY FOR MG!!                
         BE    BLDCLRX             NO, EXIT                                     
         B     BLDCLR10                                                         
*                                                                               
*** CODE FOR ORDER ONLY                                                         
BLDCLR5  BRAS  RE,ORDSTADR         R1 = A(STATUS TABLE)                         
*                                                                               
         CLI   SVSTAT,0            WAS SVSTAT SET?                              
         BNE   BLDCLR10            YES,                                         
         L     R6,AIO1             NO, GET MOST CURRENT ORDSTAT                 
         LA    R6,DORFRST-DOKEY(R6)                                             
         MVI   ELCDLO,DOSTELQ                                                   
         MVI   ELCDHI,DOSTELQ                                                   
         BRAS  RE,BNEXTEL                                                       
         BNE   BLDCLRX                                                          
         USING DOSTELD,R6                                                       
         MVC   SVSTAT,DOSTSTAT     MOVE STATUS                                  
         DROP  R6                                                               
*                                                                               
         USING ORDD,R1                                                          
BLDCLR10 CLC   SVSTAT,ORDSTAT                                                   
         BNE   BLDCLR15                                                         
*                                                                               
         CLI   ORDTYP,0            IS TYPE BIT SET?                             
         BE    BLDCLR20                                                         
*                                                                               
         CLI   POMAUCFM,C'A'       PROFILE SET TO AUTO-CONFIRM?                 
         BE    BLDCLR12                                                         
         CLI   POMAUCFM,C'B'       PROFILE SET TO BUYER-CONFIRM?                
         BNE   BLDCLR20            ALL CONFIRMS ARE BLACK!!!                    
*                                   CHOOSE FIRST CNFM ENTRY IN TABLE            
BLDCLR12 TM    ORDTYP,ORDTCMTS            TEST WITH COMMENTS                    
         BNO   *+12                                                             
         TM    MISCFLG2,MF2CNFCM          ARE THERE COMMENTS ?                  
         BO    BLDCLR20                   YES, GOT IT                           
         TM    ORDTYP,ORDTNCMT            TEST WITH NO COMMENTS                 
         BNO   BLDCLR15                                                         
         TM    MISCFLG2,MF2CNFCM          ARE THERE COMMENTS ?                  
         BZ    BLDCLR20                   NO, GOT IT                            
*                                                                               
BLDCLR15 LA    R1,L'STATTAB(R1)                                                 
         CLI   0(R1),X'FF'                                                      
         BNE   BLDCLR10                                                         
         DC    H'0'                                                             
*                                                                               
BLDCLR20 MVC   BYTE,1(R1)          SAVE CORRESPONDING COLOR                     
         CLI   THISISMG,C'Y'       IS THIS FOR A MAKEGOOD?                      
         BNE   BLDCLR40                                                         
*                                                                               
         SR    R0,R0               LOOK FOR OUR MAKEGOOD COLOR                  
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
         MVI   ELCDLO,MGCOLELQ                                                  
         MVI   ELCDHI,MGCOLELQ                                                  
BLDCLR30 BRAS  RE,BNEXTEL          CAN'T FIND IT?                               
         BE    BLDCLR35                                                         
BLDCLR33 MVI   SVSTAT,0                                                         
         MVI   THISISMG,C'N'       NO LONGER A MAKEGOOD!!!                      
         B     BLDCLR3             RESTORE COLOR BASED ON DOSTSTAT AND          
*                                   AND MGCOLEL                                 
         USING MGCOLELD,R6                                                      
BLDCLR35 CLC   QMGGROUP,MGCOLCOD   FOUND OUR MAKEGOOD?                          
         BNE   BLDCLR30                                                         
         MVC   MGCOLCOL,BYTE       YES, UPDATE THE COLOR                        
         CLI   BYTE,C'K'           CAN WE DELETE THIS MAKEGOOD COLOR?           
         BNE   BLDCLR40                                                         
         GOTO1 VRECUP,DMCB,(C'S',AIO1),(R6),(R6)   YES, DELETE                  
*                                                                               
BLDCLR40 CLI   BYTE,C'G'           TEST IF CURRENT CHANGE GREEN                 
         BE    BLDCLR75            YES - GREEN ALWAYS WINS                      
*                                                                               
         L     R6,AIO1             FIND MG GROUP COLOR ELEMENTS                 
         USING DOKEY,R6                                                         
         LA    R6,DORFRST-DOKEY(R6)                                             
         MVI   ELCDLO,MGCOLELQ                                                  
         MVI   ELCDHI,MGCOLELQ                                                  
*                                                                               
BLDCLR55 BRAS  RE,BNEXTEL                                                       
         BNE   BLDCLR62                                                         
         USING MGCOLEL,R6                                                       
         CLI   MGCOLCOL,C'G'                                                    
         BNE   BLDCLR60                                                         
         MVI   BYTE,C'G'           SET TO GREEN AND DONE                        
         B     BLDCLR75                                                         
*                                                                               
BLDCLR60 CLI   MGCOLCOL,C'R'                                                    
         BNE   BLDCLR55                                                         
         MVI   BYTE,C'R'                                                        
         B     BLDCLR55                                                         
         DROP  R6                                                               
*                                                                               
BLDCLR62 CLI   THISISMG,C'Y'       IS THIS FOR A MAKEGOOD?                      
         BNE   BLDCLR75            NO                                           
         CLI   BYTE,C'K'           YES, AM I BLACK? (NO MORE MG??)              
         BNE   BLDCLR74                                                         
*                                                                               
         CLI   POMAUCFM,C'A'       PROFILE SET TO AUTO-CONFIRM?                 
         BNE   BLDCLR33            NO                                           
         L     R6,AIO              ADD FULLY CONFIRMED STATUS!!                 
         LA    R6,DORFRST-DOKEY(R6)                                             
         MVI   ELCDLO,DOSTELQ                                                   
         MVI   ELCDHI,DOSTELQ                                                   
         BRAS  RE,BNEXTEL                                                       
         USING DOSTELD,R6                                                       
         CLI   DOSTSTAT,QCFMD      AM I CONFIRMED?                              
         BNE   BLDCLR33                                                         
*                                                                               
         CLI   DOSTLEN,DOSTLNQ3    DO I HAVE A TYPE FIELD?                      
         BE    BLDCLR65                                                         
         TM    MISCFLG2,MF2CNFCM                                                
         BZ    BLDCLR33                                                         
         B     BLDCLR67                                                         
BLDCLR65 TM    DOSTTYPE,DCNFMCOM   CONFIRMED WITH COMMENTS?                     
         BZ    BLDCLR69                                                         
         DROP  R6                                                               
*                                                                               
BLDCLR67 LA    R4,DOSTELEM         NEW STATUS ELEMENT (AUDIT TRAIL)             
         USING DOSTELD,R4                                                       
         XC    DOSTELEM,DOSTELEM                                                
         MVI   DOSTEL,DOSTELQ                                                   
         MVI   DOSTLEN,DOSTLNQ3                                                 
         MVC   DOSTDATE(5),SVDATTIM  MOVE IN DATE/TIME OF MG CANCEL             
         MVI   DOSTSTAT,QCFMD                                                   
         OI    DOSTTYPE,DCNFMFUL                                                
*                                                                               
         GOTO1 VRECUP,DMCB,(C'S',AIO1),DOSTELEM,(R6)                            
*                                                                               
BLDCLR69 NI    MISCFLG2,X'FF'-MF2CNFCM                                          
         B     BLDCLR33                                                         
         DROP  R4                                                               
*                                                                               
BLDCLR74 CLI   BYTE,C'G'           YES, IS MKGD GREEN?                          
         BNE   BLDCLR33            NO, CHK DOSTSTAT AND MGCOLEL FOR             
*                                      HIGHEST COLOR                            
BLDCLR75 L     R6,AIO1             FIND COLOR ELEMENT IN ORDER REC              
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
         MVC   QBUYER,DOIDBYR      SAVE THE BUYER CODE                          
         DROP  R6                                                               
*                                                                               
         L     R6,AIO1             FIND COLOR ELEMENT IN ORDER REC              
         USING DOKEY,R6                                                         
         LA    R6,DORFRST-DOKEY(R6)                                             
         MVI   ELCDLO,COLELQ                                                    
         MVI   ELCDHI,COLELQ                                                    
         BRAS  RE,BNEXTEL                                                       
         BNE   BLDCLR80                                                         
*                                                                               
K        USING DSCKTYPE,KEY                                                     
         USING COLOREL,R6                                                       
*                                                                               
         XC    KEY,KEY             READ AND DELETE OLD COLOR POINTER            
         MVI   K.DSCKTYPE,DSCKTYPQ                                              
         MVI   K.DSCKSTYP,DSCKSTYQ                                              
         MVC   K.DSCKAGMD,BAGYMD                                                
         MVC   K.DSCKBYR,QBUYER                                                 
         OI    K.DSCKBYR+2,C' '                                                 
         MVC   K.DSCKSTAT,COLCOL                                                
         MVC   K.DSCKDATE,COLDATE                                               
         MVC   K.DSCKORDR,BINORDER                                              
         MVC   KEY+14(4),SVORDDA                                                
         DROP  K                                                                
         DROP  R6                                                               
*                                                                               
         BRAS  RE,HIGH                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BLDCLR80                  CONVERSION NOT YET RUN !               
         MVI   DMINBTS,X'80'       READ UPDATE NOW                              
         BRAS  RE,HIGH                                                          
         OI    KEY+13,X'80'        DELETE OLD                                   
         BRAS  RE,WRITE                                                         
*                                                                               
Y        USING COLOREL,ELEM+128                                                 
BLDCLR80 XC    ELEM+128(128),ELEM+128    BUILD NEW COLOR ELEMENT                
         MVI   Y.COLEL,COLELQ                                                   
         MVI   Y.COLELLEN,COLLENQ                                               
         MVC   Y.COLCOL,BYTE                                                    
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(2,Y.COLDATE)                                 
         XC    Y.COLDATE,=X'FFFF'       COMPLEMENT DATE                         
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,DORFRST-DOKEY(R6)                                             
         MVI   ELCDLO,COLELQ                                                    
         MVI   ELCDHI,COLELQ                                                    
         BRAS  RE,BNEXTEL                                                       
         BNE   BLDCLR85                                                         
*                                                                               
BLDCLR82 GOTO1 VRECUP,DMCB,AIO1,(R6)    DELETE OLD COLOR ELEM                   
         BRAS  RE,BNEXTEL2              IN CASE ANY MORE AROUND                 
         BE    BLDCLR82                                                         
*                                                                               
BLDCLR85 GOTO1 VRECUP,DMCB,AIO1,ELEM+128,(R6)   ADD NEW                         
*                                                                               
         BRAS  RE,PUT              WRITE THE ORDER RECORD                       
***************                                                                 
* COMMENTED THIS CODE OUT BECAUSE EVEN A MKGCAN AFFECTS THE ORDER COLOR         
***************                                                                 
*&&DO                                                                           
         CLI   THISISMG,C'Y'       THIS IS FOR A MAKEGOOD?                      
         BNE   BLDCLR87            'C' IS CONFIRM FOR ORDERS                    
         CLI   SVSTAT,MNSTCAN      TEST CANCELLED                               
         BE    BLDCLRX             YES - JUST EXIT                              
*&&                                                                             
***************                                                                 
* COMMENTED THIS CODE OUT BECAUSE EVEN A MKGCAN AFFECTS THE ORDER COLOR         
***************                                                                 
K        USING DSCKTYPE,KEY                                                     
BLDCLR87 XC    KEY,KEY             CREATE NEW COLOR POINTER                     
         MVI   K.DSCKTYPE,DSCKTYPQ                                              
         MVI   K.DSCKSTYP,DSCKSTYQ                                              
         MVC   K.DSCKAGMD,BAGYMD                                                
         MVC   K.DSCKBYR,QBUYER                                                 
         OI    K.DSCKBYR+2,C' '                                                 
         MVC   K.DSCKSTAT,Y.COLCOL                                              
         MVC   K.DSCKDATE,Y.COLDATE                                             
         MVC   K.DSCKORDR,BINORDER                                              
         MVC   KEY+14(4),SVORDDA   MOVE SAVED DISK ADDRESS                      
         DROP  K,Y                                                              
*                                                                               
         MVI   DMINBTS,X'08'       SET TO PASS DELETES                          
         BRAS  RE,HIGH                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BLDCLR90                                                         
         MVI   DMINBTS,X'88'       UPDATING NOW                                 
         BRAS  RE,HIGH                                                          
         NI    KEY+13,X'7F'        UNDELETE                                     
         BRAS  RE,WRITE                                                         
         B     BLDCLRX                                                          
*                                                                               
BLDCLR90 MVC   KEY,KEYSAVE         RESTORE                                      
         BRAS  RE,ADDDIR                                                        
*                                                                               
BLDCLRX  J     YES                                                              
*                                                                               
ORDSTADR BASR  R1,RE               POINT R1 TO STATUS TABLE                     
STATTAB  DS    0XL3                                                             
         DC    AL1(DSENT),C'G',AL1(0)                                           
         DC    AL1(DFXSENT),C'G',AL1(0)                                         
         DC    AL1(DEMSENT),C'G',AL1(0)                                         
         DC    AL1(QRJCT),C'G',AL1(0)                                           
         DC    AL1(QEMPTY),C'G',AL1(0)                                          
         DC    AL1(QERRORED),C'G',AL1(0)                                        
         DC    AL1(QFAXCNCL),C'G',AL1(0)                                        
         DC    AL1(QRCLAPPR),C'G',AL1(0)                                        
         DC    AL1(QRCLDELN),C'G',AL1(0)                                        
         DC    AL1(QRCLUNKN),C'G',AL1(0)                                        
         DC    AL1(QRCLTRNS),C'G',AL1(0)                                        
         DC    AL1(QRCLWIP),C'G',AL1(0)                                         
         DC    AL1(QSNTPNDG),C'G',AL1(0)                                        
         DC    AL1(QSNTXCNF),C'G',AL1(0)                                        
         DC    AL1(QSNTXREJ),C'G',AL1(0)                                        
         DC    AL1(QTOBESNT),C'G',AL1(0)                                        
*                                                                               
         DC    AL1(QAPP),C'R',AL1(0)                                            
         DC    AL1(QRECALL),C'R',AL1(0)                                         
         DC    AL1(QRCLCONF),C'R',AL1(0)                                        
         DC    AL1(QRCLREJD),C'R',AL1(0)                                        
         DC    AL1(MNSTDELV),C'R',AL1(0) B/C OF TRILOGY, WE NEED DELVRD         
         DC    AL1(DDLVRD),C'R',AL1(0)                                          
         DC    AL1(QFAXDLVD),C'R',AL1(0)                                        
         DC    AL1(DFXDLVD),C'R',AL1(0)                                         
         DC    AL1(DEMDLVD),C'R',AL1(0)                                         
*                                                                               
* DON'T CHANGE ORDER OF QCFMD, I HAVE CODE DEPENDING ON THIS ORDER!!            
         DC    AL1(QCFMD),C'K',AL1(ORDTNCMT) CONFIRMED W/O COMMENTS             
         DC    AL1(QCFMD),C'R',AL1(ORDTCMTS) CONFIRMED WITH COMMENTS            
*                                                                               
         DC    AL1(QBYRCNFM),C'K',AL1(0)                                        
         DC    AL1(QCFMDPND),C'K',AL1(0)                                        
         DC    AL1(QNODARE),C'K',AL1(0)                                         
         DC    AL1(QUNDARE),C'K',AL1(0)                                         
         DC    X'FF'                                                            
*                                                                               
MKGSTADR BASR  R1,RE               POINT R1 TO STATUS TABLE                     
         DC    AL1(MNSTNEW),C'G',AL1(0)                                         
         DC    AL1(MNSTAMND),C'G',AL1(0)                                        
         DC    AL1(MNSTGOIN),C'G',AL1(0)                                        
         DC    AL1(MNSTHOLD),C'G',AL1(0)                                        
         DC    AL1(MNSTERR),C'G',AL1(0)                                         
*                                                                               
         DC    AL1(MNSTAPP),C'R',AL1(0)                                         
         DC    AL1(MNSTREJ),C'R',AL1(0)                                         
         DC    AL1(MNSTCANM),C'R',AL1(0)                                        
         DC    AL1(MNSTDELV),C'R',AL1(0)                                        
         DC    AL1(MNSTSAPP),C'R',AL1(0)                                        
*                                                                               
         DC    AL1(MNSTCAN),C'K',AL1(0)                                         
         DC    AL1(MNSTOKAY),C'K',AL1(0)                                        
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GETS THE AGENCY MAKEGOOD NOTICE RECORD AND PUTS IT IN AIO1                    
*                                                                               
* ON ENTRY:    BAGYMD              BINARY AGENCY/MEDIA                          
*              BINORDER            ORDER NUMBER (FF COMPLEMENT)                 
*              R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
GETNOTCE NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   QSTA,C'0'           CABLE?                                       
         BNL   GETNOT50                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING MNKEY,R4                                                         
         MVI   MNKTYPE,MNKTYPQ                                                  
         MVI   MNKSUBTY,MNKSTYPQ                                                
         MVC   MNKAGMD,BAGYMD                                                   
         MVC   MNKBYR,QBUYER                                                    
         MVC   MNKORDER,BINORDER                                                
         MVC   MNKGROUP,QMGGROUP                                                
         DROP  R4                                                               
*                                                                               
         BRAS  RE,HIGH                                                          
         CLC   KEY(L'MNKEY),KEYSAVE                                             
         BNE   GNTCENO                                                          
*********                                                                       
* READ SPOT NOTICE RECORD FOR UPDATE                                            
*********                                                                       
         MVC   SVMKNDA,KEY+14      SAVE DISK ADDRESS FOR PASSIVES               
         MVI   DMINBTS,X'88'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
         CLI   DMCB+8,0            DELETED ?                                    
         BE    GNTCEYES                                                         
         B     GNTCENO             DO NOT DIE BECAUSE OF THIS!!!                
*********                                                                       
* READ X-SPOT CABLE NOTICE RECORD                                               
*********                                                                       
*                                                                               
GETNOT50 DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING MNXKEY,R4                                                        
         MVI   MNXKTYPE,MNXKTYPQ                                                
         MVI   MNXKSBTY,MNXKSBTQ                                                
         MVC   MNXKAGMD,BAGYMD                                                  
         MVC   MNXKORDR,BINORDER                                                
         MVC   MNXKGRP,QMGGROUP                                                 
         DROP  R4                                                               
*                                                                               
         BRAS  RE,XHIGH                                                         
*                                                                               
         CLC   KEY(L'MNXKEY),KEYSAVE                                            
         BNE   GOFFRNO                                                          
*                                                                               
*********                                                                       
* READ CABLE NOTICE RECORD FOR UPDATE                                           
*********                                                                       
         MVC   SVMKNDA,KEY+36      SAVE DISK ADDRESS FOR PASSIVES               
         MVI   DMINBTS,X'88'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,XGET                                                          
         CLI   DMCB+8,0            DELETED ?                                    
         BE    GNTCEYES                                                         
         B     GNTCENO             DO NOT DIE BECAUSE OF THIS!!!                
*&&DO                                                                           
         L     R1,=A(*-T16100)     DELETED RECORD BUT KEY IS NOT                
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*&&                                                                             
GNTCEYES J     YES                                                              
*                                                                               
GNTCENO  CLC   =C'ERRNOT',0(R7)           ERROR ON ERRNOT?                      
         JE    NO                                                               
         LHI   R1,REFMGDNE                OFFER DOES NOT EXIST                  
         BRAS  RE,SNDERROR                                                      
         J     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GETS THE AGENCY ORDER RECORD FOR UPDATE AND PUTS IT IN AIO1                   
*                                                                               
* ON ENTRY:    BAGYMD              BINARY AGENCY/MEDIA                          
*              BINORDER            ORDER NUMBER (FF COMPLEMENT)                 
*              R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
GETORDER NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING DOKEY,R4                                                         
         MVI   DOKTYPE,DOKTYPQ                                                  
         MVI   DOKSUBTY,DOKSTYPQ                                                
         MVC   DOKAGMD,BAGYMD                                                   
         MVC   DOKORDER,BINORDER                                                
*                                                                               
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(DOKSTA-DOKEY),KEYSAVE                                        
         BNE   GORDRNO                                                          
         CLI   DOKCMT,0            WE HAVE A COMMENT INSTEAD?                   
         BNE   GORDRNO             NO GOOD, ERROR, BETTER THAN DC H'0'          
         DROP  R4                                                               
*********                                                                       
* READ ORDER RECORD FOR UPDATE                                                  
*********                                                                       
         MVC   SVORDDA,KEY+14      SAVE DISK ADDRESS FOR PASSIVES               
         MVI   DMINBTS,X'80'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
*                                                                               
         L     R6,AIO                                                           
         LA    R6,DORFRST-DOKEY(R6)                                             
         USING DOIDELD,R6                                                       
         MVC   QBUYER,DOIDBYR      SAVING VALUES FOR 0DBC XSPKEY                
         MVC   BCLT,DOIDCLT                                                     
         MVC   BPRD,DOIDPRD                                                     
         MVC   BEST,DOIDEST                                                     
*                                                                               
         MVI   ELCODE,DOSPELQ                                                   
         BAS   RE,NEXTEL                                                        
         USING DOSPELD,R6                                                       
         MVC   BMKTSTA(2),DOSPMKT         MARKET                                
         DROP  R6                                                               
*                                                                               
GORDRYES J     YES                                                              
*                                                                               
GORDRNO  CLC   =C'ERRNOT',0(R7)           ERROR ON ERRNOT?                      
         JE    NO                                                               
         LHI   R1,REFORDNE          ORDER DOES NOT EXIST                        
         BRAS  RE,SNDERROR                                                      
         J     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GETS THE AGENCY MAKEGOOD OFFER RECORD AND PUTS IT IN AIO1                     
*                                                                               
* ON ENTRY:    BAGYMD              BINARY AGENCY/MEDIA                          
*              BINORDER            ORDER NUMBER (FF COMPLEMENT)                 
*              R7                  A(EDICT RECORD W/O EDICT INFO)               
***********************************************************************         
GETOFFER NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   QSTA,C'0'           CABLE?                                       
         BNL   GETOFF50                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING MOKEY,R4                                                         
         MVI   MOKTYPE,MOKTYPQ                                                  
         MVI   MOKSUBTY,MOKSTYPQ                                                
         MVC   MOKAGMD,BAGYMD                                                   
         MVC   MOKORDER,BINORDER                                                
         MVC   MOKMGCD,QMGGROUP                                                 
         DROP  R4                                                               
*                                                                               
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(L'MOKEY),KEYSAVE                                             
         BNE   GOFFRNO                                                          
*                                                                               
         MVI   DMINBTS,X'08'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,GET                                                           
         CLI   DMCB+8,0                                                         
         BE    GOFFRYES                                                         
         L     R1,=A(*-T16100)     DELETED RECORD BUT KEY IS NOT                
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
* THIS IS A CABLE MAKEGOOD, READ THE X-SPOT FILE                                
*                                                                               
GETOFF50 DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING MOXKEY,R4                                                        
         MVI   MOXKTYPE,MOXKTYPQ                                                
         MVI   MOXKSBTY,MOXKSBTQ                                                
         MVC   MOXKAGMD,BAGYMD                                                  
         MVC   MOXKORDR,BINORDER                                                
         MVC   MOXKMGCD,QMGGROUP                                                
         DROP  R4                                                               
*                                                                               
         BRAS  RE,XHIGH                                                         
*                                                                               
         CLC   KEY(L'MOXKEY),KEYSAVE                                            
         BNE   GOFFRNO                                                          
*                                                                               
         MVI   DMINBTS,X'08'                                                    
         MVC   AIO,AIO1                                                         
         BRAS  RE,XGET                                                          
         CLI   DMCB+8,0                                                         
         BE    GOFFRYES                                                         
         L     R1,=A(*-T16100)     DELETED RECORD BUT KEY IS NOT                
         ST    R1,DMCB                                                          
         BRAS  RE,WTOMSG                                                        
*                                                                               
GOFFRYES J     YES                                                              
*                                                                               
GOFFRNO  CLC   =C'ERRNOT',0(R7)           ERROR ON ERRNOT?                      
         JE    NO                                                               
         LHI   R1,REFMGDNE          OFFER DOES NOT EXIST                        
         BRAS  RE,SNDERROR                                                      
         J     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
*========================================================                       
* SPTDIR COMMANDS                                                               
*========================================================                       
         SPACE 1                                                                
ADDDIR   DS    0H                                                               
         BRAS  R1,GODIR                                                         
         DC    AL4(DMADD)                                                       
         DC    AL4(SPTDIR)                                                      
*                                                                               
HIGH     MVC   KEYSAVE,KEY                                                      
         CLI   GBYACT,0                                                         
         JE    HIGH10                                                           
         CLI   GBYACT,GBYHIGH                                                   
         JNE   *+2                                                              
         BRAS  R1,GOGETBUY                                                      
*                                                                               
HIGH10   BRAS  R1,GODIR                                                         
         DC    AL4(DMRDHI)                                                      
         DC    AL4(SPTDIR)                                                      
*                                                                               
HIGHREP  MVC   KEYSAVE,KEY                                                      
         BRAS  R1,GODIR                                                         
         DC    AL4(DMRDHI)                                                      
         DC    AL4(REPDIR)                                                      
*                                                                               
SEQ      CLI   GBYACT,0                                                         
         JE    SEQ10                                                            
         CLI   GBYACT,GBYSEQ                                                    
         JNE   *+2                                                              
         BRAS  R1,GOGETBUY                                                      
*                                                                               
SEQ10    BRAS  R1,GODIR                                                         
         DC    AL4(DMRSEQ)                                                      
         DC    AL4(SPTDIR)                                                      
*                                                                               
SEQREP   BRAS  R1,GODIR                                                         
         DC    AL4(DMRSEQ)                                                      
         DC    AL4(REPDIR)                                                      
*                                                                               
WRITE    BRAS  R1,GODIR                                                         
         DC    AL4(DMWRT)                                                       
         DC    AL4(SPTDIR)                                                      
*                                                                               
HIGHCT   MVC   KEYSAVE,KEY                                                      
         BRAS  R1,GODIR                                                         
         DC    AL4(DMRDHI)                                                      
         DC    AL4(CTFILE)                                                      
*                                                                               
HIGHGD   MVC   KEYSAVE,KEY                                                      
         BRAS  R1,GODIR                                                         
         DC    AL4(DMRDHI)                                                      
         DC    AL4(GENDRCT)                                                     
*                                                                               
WRTCT    BRAS  R1,GODIR                                                         
         DC    AL4(DMWRT)                                                       
         DC    AL4(CTFILE)                                                      
*                                                                               
ADDCT    BRAS  R1,GODIR                                                         
         DC    AL4(DMADD)                                                       
         DC    AL4(CTFILE)                                                      
*                                                                               
XHIGH    MVC   KEYSAVE,KEY                                                      
         BRAS  R1,GODIR                                                         
         DC    AL4(DMRDHI)                                                      
         DC    AL4(XSPDIR)                                                      
*                                                                               
XSEQ     BRAS  R1,GODIR                                                         
         DC    AL4(DMRSEQ)                                                      
         DC    AL4(XSPDIR)                                                      
*                                                                               
XWRITE   BRAS  R1,GODIR                                                         
         DC    AL4(DMWRT)                                                       
         DC    AL4(XSPDIR)                                                      
*                                                                               
ADDXKEY  BRAS  R1,GODIR                                                         
         DC    AL4(DMADD)                                                       
         DC    AL4(XSPDIR)                                                      
*                                                                               
GODIR    NTR1  BASE=*,LABEL=*                                                   
         ICM   RE,15,0(R1)         COMMAND                                      
         A     RE,RELO                                                          
         ST    RE,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
*                                                                               
         ICM   RE,15,4(R1)         FILE NAME                                    
         A     RE,RELO                                                          
         ST    RE,DMCB+4                                                        
         CLC   =C'CTFILE',0(RE)    ARE WE LOOKING AT THE CONTROL FILE           
         BNE   GODIR10                                                          
         LA    RE,KEY                                                           
         ST    RE,DMCB+8                                                        
         L     RE,AIO                                                           
         ST    RE,DMCB+12                                                       
         B     GODIR20                                                          
*                                                                               
GODIR10  LA    RE,KEYSAVE                                                       
         ST    RE,DMCB+8                                                        
         LA    RE,KEY                                                           
         ST    RE,DMCB+12                                                       
*                                                                               
GODIR20  GOTO1 VDATAMGR,DMCB                                                    
         TM    8(R1),X'FD'                                                      
         JNZ   *+2                                                              
         MVI   DMINBTS,0           RESET                                        
         XIT1                                                                   
         LTORG                                                                  
         SPACE 1                                                                
*========================================================                       
* FILE COMMANDS                                                                 
*========================================================                       
         SPACE 1                                                                
GET      CLI   GBYACT,0                                                         
         JE    GET10                                                            
         CLI   GBYACT,GBYGET                                                    
         JNE   *+2                                                              
         BRAS  R1,GOGETBUY                                                      
*                                                                               
GET10    BRAS  R1,GOFILE                                                        
         DC    AL4(GETREC)                                                      
         DC    AL4(SPTFILE)                                                     
         DC    H'14'               DSPL TO DISK ADDRESS                         
         SPACE 1                                                                
GETREP   BRAS  R1,GOFILE                                                        
         DC    AL4(GETREC)                                                      
         DC    AL4(REPFILE)                                                     
         DC    H'28'               DSPL TO DISK ADDRESS                         
*                                                                               
GETGFL   BRAS  R1,GOFILE                                                        
         DC    AL4(GETREC)                                                      
         DC    AL4(GENFILE)                                                     
         DC    H'36'               DSPL TO DISK ADDRESS                         
*                                                                               
PUT      CLI   GBYACT,0                                                         
         JE    PUT10                                                            
         CLI   GBYACT,GBYPUT                                                    
         JNE   *+2                                                              
         BRAS  R1,GOGETBUY                                                      
*                                                                               
PUT10    BRAS  R1,GOFILE                                                        
         DC    AL4(PUTREC)                                                      
         DC    AL4(SPTFILE)                                                     
         DC    H'14'                                                            
         SPACE 1                                                                
PUTREP   BRAS  R1,GOFILE                                                        
         DC    AL4(PUTREC)                                                      
         DC    AL4(REPFILE)                                                     
         DC    H'28'               DSPL TO DISK ADDRESS                         
*                                                                               
PUTGFL   BRAS  R1,GOFILE                                                        
         DC    AL4(PUTREC)                                                      
         DC    AL4(GENFILE)                                                     
         DC    H'36'               DSPL TO DISK ADDRESS                         
*                                                                               
ADD      BRAS  R1,GOFILE                                                        
         DC    AL4(ADDREC)                                                      
         DC    AL4(SPTFILE)                                                     
         DC    H'14'                                                            
*                                                                               
ADDGFL   BRAS  R1,GOFILE                                                        
         DC    AL4(ADDREC)                                                      
         DC    AL4(GENFILE)                                                     
         DC    H'36'               DSPL TO DISK ADDRESS                         
*                                                                               
XGET     BRAS  R1,GOFILE                                                        
         DC    AL4(GETREC)                                                      
         DC    AL4(XSPFIL)                                                      
         DC    H'36'               DSPL TO DISK ADDRESS                         
*                                                                               
XPUT     BRAS  R1,GOFILE                                                        
         DC    AL4(PUTREC)                                                      
         DC    AL4(XSPFIL)                                                      
         DC    H'36'                                                            
*                                                                               
XADD     BRAS  R1,GOFILE                                                        
         DC    AL4(ADDREC)                                                      
         DC    AL4(XSPFIL)                                                      
         DC    H'36'                                                            
*                                                                               
GOFILE   NTR1  BASE=*,LABEL=*                                                   
         ICM   RE,15,0(R1)                                                      
         A     RE,RELO                                                          
         ST    RE,DMCB             SET COMMAND ADDRESS                          
         MVC   DMCB(1),DMINBTS                                                  
*                                                                               
         ICM   RE,15,4(R1)                                                      
         A     RE,RELO                                                          
         ST    RE,DMCB+4           SET FILENAME ADDRESS                         
*                                                                               
         LA    RE,KEY                                                           
         AH    RE,8(R1)            GET DSPL OF DISK ADDRESS IN KEY              
         ST    RE,DMCB+8                                                        
*                                                                               
         MVC   DMCB+12(4),AIO                                                   
*                                                                               
         LA    RE,DMWORK                                                        
         ST    RE,DMCB+16                                                       
*                                                                               
         GOTO1 VDATAMGR,DMCB                                                    
         TM    8(R1),X'02'         DELETED RECORD?                              
         BNZ   GOFILX              YEAH, WE DON'T WANNA DIE                     
         TM    8(R1),X'FD'         TEST ALL BUT DELETED                         
         JNZ   *+2                                                              
*                                                                               
GOFILX   MVI   DMINBTS,0           RESET                                        
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
GOGETBUY NTR1  BASE=*,LABEL=*                                                   
         XC    GETBLK+1(L'GETBLK-1),GETBLK+1                                    
*                                                                               
         MVC   GBYCOMF,SRPARMSD.SRQACOMF                                        
*                                                                               
         CLI   GBYACT,GBYINIT                                                   
         BNE   GOGETB10                                                         
         MVC   GBYAGY,AGENCY                                                    
         B     GOGETB50                                                         
*                                                                               
GOGETB10 MVC   GBYDMIN,DMINBTS                                                  
         MVI   GBYDMOUT,X'FF'                                                   
*                                                                               
         CLI   GBYACT,GBYHIGH                                                   
         BE    GOGETB20                                                         
         CLI   GBYACT,GBYSEQ                                                    
         BNE   GOGETB30                                                         
GOGETB20 LAY   R1,KEY                                                           
         ST    R1,GBYKEYIN                                                      
         ST    R1,GBYKEYOT                                                      
         MVC   GBY1OR2,SV1OR2                                                   
         B     GOGETB50                                                         
*                                                                               
GOGETB30 CLI   GBYACT,GBYGET                                                    
         BE    GOGETB40                                                         
         CLI   GBYACT,GBYPUT                                                    
         BNE   GOGETB50                                                         
GOGETB40 LA    RE,KEY+14                                                        
         ST    RE,GBYDA                                                         
         MVC   GBYIOA,AIO                                                       
         LA    RE,DMWORK                                                        
         ST    RE,GBYDMWRK                                                      
         XC    IOBRDLST,IOBRDLST                                                
         LA    RE,IOBRDLST                                                      
         ST    RE,GBYPRDL                                                       
         MVC   GBY1OR2,SV1OR2                                                   
*                                                                               
GOGETB50 GOTOR AGETBUY,GETBLK                                                   
         MVI   GBYACT,0            RESET                                        
         MVI   DMINBTS,0           RESET                                        
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
DAREOBJD DSECT                                                                  
DOBJTID  DS    CL6                 TRANSMISSION ID                              
DOBJRIDN DS    XL1                 RETURN ID NUMBER                             
DOBJNXT  DS    0C                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
WORKD    DSECT                                                                  
DUB      DS    D                                                                
*                                                                               
RELO     DS    A                                                                
SAVERD   DS    A                                                                
EDICTFL  DS    A                   A(EDICT FILE)                                
DAFILT   DS    A                   DISK ADDRESS FILTER                          
DSKAD    DS    A                   LAST PAGE'S LAST DISK ADDRESS                
AIO      DS    A                                                                
AIO1     DS    A                   A(IOAREA #1)                                 
AIO2     DS    A                   A(IOAREA #2)                                 
ASPLAREA DS    A                   A(SPOOL AREA)                                
AWRKRIOA DS    A                   A(IO AREA USED BY EDICT)                     
AWRKRBUF DS    A                   A(WORKER BUFFER AREA)                        
AHUGEBLK DS    A                   A(HUGE BLOCK)                                
*                                                                               
VADDAY   DS    V                                                                
VDATCON  DS    V                                                                
VGETFACT DS    V                                                                
VHELLO   DS    V                                                                
VHEXIN   DS    V                                                                
VHEXOUT  DS    V                                                                
VLOCKET  DS    V                                                                
VRECUP   DS    V                                                                
VCLUNPK  DS    V                                                                
VMSPACK  DS    V                                                                
VMSUNPK  DS    V                                                                
VSWITCH  DS    V                                                                
*                                                                               
ASPOOL   DS    A                                                                
ASTAPACK DS    A                                                                
AREPFACS DS    A                                                                
AGETDARE DS    A                                                                
AGETBUY  DS    A                                                                
*                                                                               
SRPARMS  DS    8F                  SERVICE REQUEST PARAMETERS                   
EDCTFDSK DS    F                   EDICTFIL DISK ADDRESS   TTTTBB00             
EDCTFDSP DS    F                   EDICTFIL DISP ADDRESS   DD999999             
DMCB     DS    6F                                                               
FULL     DS    F                                                                
SVORDDA  DS    A                   SAVED DISK ADDRESS OF ORDER                  
SVMKNDA  DS    A                   SAVED DISK ADDRESS OF MKGD NOTICE            
*                                                                               
DMINBTS  DS    X                                                                
SVSTAT   DS    X                                                                
THISISMG DS    C                   FOR SVSTAT, IS THIS A MAKEGOOD ?             
EDCTFRPT DS    H                   EDICTFIL PHYSICAL RECORDS PER TRACK          
EDCTFTPD DS    H                   EDICTFIL TRACKS PER DAY                      
EDCTFATQ EQU   40                     ON ADV                                    
EDCTFRTQ EQU   40                     ON REP                                    
EDCTFTTQ EQU   2                      ON TEST                                   
EDCTFMTQ EQU   2                      ON MEL                                    
EDCTRPBQ DS    H                   EDICTFIL LOGICAL RECORDS PER BLOCK           
EDCTLRCL DS    H                   EDICTFIL LOGICAL RECORD LENGTH               
EDCTFLST DS    H                   LAST TRACK FOR GIVEN DAY                     
FRSTTRK  DS    H                   FIRST TRAC FOR GIVEN DAY                     
RECLEN   DS    H                                                                
TERMNUM  DS    H                   TERMINAL NUMBER                              
USERNUM  DS    H                   USER ID NUMBER                               
TRNNUM   DS    H                   TRANSACTION COUNT                            
HALF     DS    H                                                                
DATADISP DS    H                                                                
*                                                                               
AGENCY   DS    XL2                 AGENCY POWER CODE                            
SIGNON2H DS    XL2                 2 BYTE HEX AGENCY ID                         
USERID   DS    CL10                CONTROL USER ID                              
REP      DS    CL10                REP 'FROM ID'                                
POWERCDE DS    CL2                 POWER CODE FOR REP READ                      
BAGYMD   DS    XL1                 BINARY AGENCY/MEDIA                          
BCLT     DS    XL2                 BINARY CLIENT CODE                           
BEST     DS    XL1                 BINARY ESTIMATE                              
BMKTSTA  DS    XL5                 BINARY MARKET STATION                        
BPRD     DS    XL1                                                              
BINORDER DS    0XL4                ORDER NUMBER (FF COMPLEMENT)                 
BINORDDT DS    XL2                   (YEAR-90)*1000 + JULIAN DAY                
BINORDSQ DS    XL2                   SEQUENCE NUMBER (0-9999)                   
POLORDER DS    XL4                 POL ORDER NUMBER (FF COMPLEMENT)             
SVAPRF07 DS    XL1                 SAVED COPY OF APROF+07                       
SVCPRF00 DS    XL1                 SAVED COPY OF CPROF+00                       
SVCOFFC  DS    XL1                 CLIENT OFFICE                                
CONTNUMB DS    CL8                 CONTRACT NUMBER                              
         DS    0F                                                               
PACKOF4B DS    PL4                 PACKED NUMBER OF 4 BYTES                     
JDTTODAY DS    PL4                 JULIAN DATE OF TODAY                         
JDTPRIOR DS    PL4                 JULIAN DATE OF A PRIOR DAY                   
ORDRDATE DS    PL4                 JULIAN DATE BASED OFF ORDER #                
SVDATTIM DS    CL5                 SAVED DATE AND TIME OF MG CANCEL             
QREPCON  DS    CL8                 COPY OF REP CONTRACT NUMBER                  
QRETURN  DS    CL16                COPY OF RETURN TO SENDER DATA                
*                                                                               
QMED     DS    CL1                 EBCDIC MEDIA                                 
QBUYER   DS    CL3                        BUYER CODE                            
QCLT     DS    CL3                        CLIENT                                
QPRD1    DS    CL3                        PRODUCT 1                             
QPRD2    DS    CL3                        PRODUCT 2                             
QEST1    DS    CL3                        ESTIMATE 1                            
QSTA     DS    CL8                        STATION                               
QMGGROUP DS    CL3                        MAKEGOOD GROUP CODE                   
QMNUMCD  DS    CL2                        MGE MAKEGOOD CODE (INTO BUY)          
QCSHTRDE DS    CL1                        CASH OR TRADE                         
QFLTNUM  DS    CL2                        FLIGHT NUMBER                         
*                                                                               
ESTPW    DS    CL3                 PW PERCENTAGE                                
ESTCOST2 DS    XL4                 COST2 PERCENTAGE                             
ESTLOKYM DS    XL2                 BYTE 0 - YEAR, BYTE 1 MONTH                  
*                                                 X'80' - PREVIOUS              
*                                                 X'40' - SUBSEQUENT            
*                                                                               
THESYSID DS    XL1                 SAVED SYSTEM ID                              
WHCHEDCT DS    XL1                 WHICH EDICT FILE (A)DV OR (R)EP              
*                                                                               
RECSKIP  DS    XL1                 # OF RECORDS TO BUMP INTO BLOCK              
RECNUM   DS    XL1                 # OF RECORDS INTO BLOCK                      
SPTSENUM DS    XL1                 SPOT SYSTEM SENUM                            
BYTE     DS    C                                                                
*                                                                               
PIGPRD   DS    XL1                 PIGGYBACK PRODUCT BINARY CODE                
PIGEST   DS    XL1                 PIGGYBACK ESTIMATE                           
*                                                                               
MISCFLG1 DS    XL1                 VARIOUS BIT FLAGS FOR X'11' ELEM             
MF1XMTUP EQU   X'80'                -  XMT HAS BEEN UPDATED MUST PUTREC         
MF1NOXMT EQU   X'40'                -  NO TRANSMISSION ELEMENT FOUND            
*                                                                               
MISCFLG2 DS    XL1                 VARIOUS BIT FLAGS FOR X'12' ELEM             
MF2DIDNM EQU   X'80'                -  DELIVERY NOTICE ID NUMBER FOUND          
MF2CNFCM EQU   X'40'                -  WE GOT A CONFIRM WITH COMMENT            
*                                                                               
MISCFLG3 DS    XL1                 VARIOUS BIT FLAGS                            
MF3RADIO EQU   X'80'                -  RADIO EDICT TRANSMISSION                 
MF3AMEND EQU   X'40'                -  AMENDED STATUS                           
MF3INIOA EQU   X'20'                -  TRANSMISSION IN IOAREA                   
MF3SALPR EQU   X'08'                -  SALESPERSON REASSIGNED                   
MF3RECNF EQU   X'04'                -  SALESPERSON RECORD NOT FOUND             
MF3OK2CF EQU   X'02'                -  OKAY TO CONFIRM W/O ORDAPP               
MF3SPNKD EQU   X'01'                -  SPANKED                                  
*                                                                               
BITFLAG1 DS    XL1                 VARIOUS BIT FLAGS                            
BF1SKPTB EQU   X'80'                - SKIP TRACK/BLOCK                          
BF1SKIPB EQU   X'40'                - SKIP RECORD BUMP                          
BF1FRSTR EQU   X'20'                - FIRST RECORD IN BLOCK                     
BF1DPEND EQU   X'10'                - DARE RECORD PENDING                       
BF1NWCOM EQU   X'08'                - NEED TO ADD REP COMMENT RECORD            
BF1IGRCM EQU   X'04'                - IGNORE REST OF REP COMMENTS               
BF1PSSWD EQU   X'02'                - PASSWORD REQUIRED                         
BF1YSDAY EQU   X'01'                - USING PRIOR BUSINESS DAY'S INFO           
*                                                                               
BITFLAG2 DS    XL1                                                              
BF2ELCKD EQU   X'80'               ESTIMATE STATUS IS HOLD OR LOCKED            
BF2VARCN EQU   X'40'               THE VAR ORDER IS CONFIRMED                   
BF2ALLPD EQU   X'20'               ALL BRANDS ARE MARKED CONFIRM PENDNG         
BF2CNFCM EQU   X'10'               WE HAVE CONFIRM WITH COMMENTS                
BF2MGSEQ EQU   X'08'               WE HAVE MG SEQ NUMBER PROBLEM                
BF2PWLCK EQU   X'04'               PW BUY LOCKED                                
BF2SPNDG EQU   X'02'               ORDER IS UNDER SEND PENDING                  
BF2NWURL EQU   X'01'               NEED TO ADD A NEW URL RECORD                 
*                                                                               
BITFLAG3 DS    XL1               FLAGS FOR X'12' ELEMS                          
BF3SPNDG EQU   X'80'             - ORDER IS UNDER SEND PENDING                  
BF3RCLCF EQU   X'40'             - ORDER IS IN QRCLCONF, CHECK QSNTPNDG         
BF3CNFMD EQU   X'20'             - ORDER HAS BEEN CONFIRMED                     
BF3RCLRJ EQU   X'10'             - ORDER IS IN QRCLREJD, CHECK QSNTPNDG         
BF3RJCTD EQU   X'08'             - ORDER HAS BEEN REJECTED                      
BF3DLVRD EQU   X'04'             - ORDER WAS DELIVERED                          
BF3RCLNG EQU   X'02'             - ORDER IS BEING RECALLED                      
*                                                                               
DOBJNUMB DS    XL1                 DARE OBJECT NUMBER                           
DOBJDLNQ EQU   1                   DARE DELIVERY NOTIFICATION                   
DOBJOAPQ EQU   2                   DARE ORDER APPROVAL                          
DOBJORJQ EQU   3                   DARE ORDER REJECTION                         
DOBJOCMQ EQU   4                   DARE ORDER REJECTION COMMENT                 
DOBJOTRQ EQU   5                   DARE ORDER TRAILER                           
DOBJOCFQ EQU   6                   DARE ORDER CONFIRMATION                      
DOBJOLNQ EQU   7                   DARE ORDER LINE NUMBER EQUIVALENTS           
DOBJERRQ EQU   8                   DARE ERROR NOTIFICATION                      
DOBJARCQ EQU   9                   DARE AGENCY RECALL                           
DOBJORAQ EQU   10                  DARE ORDER RECALL ACKNOWLEDGEMENT            
DOBJMOKQ EQU   11                  MAKEGOOD CONFIRMATION                        
DOBJMCNQ EQU   12                  MAKEGOOD CANCELLATION                        
DOBJDFXQ EQU   13                  DARE FAX DELIVERY NOTIFICATION               
DOBJCFXQ EQU   14                  DARE FAX CANCELLATION                        
DOBJMKGQ EQU   15                  MAKEGOOD HEADER                              
DOBJEFXQ EQU   16                  DARE FAX ERROR                               
DOBJSALE EQU   17                  SALESPERSON REASSIGNMENT                     
DOBJURL  EQU   18                  URL CONFIRMATION                             
DOBJMO1  EQU   19                  MEDIA OCEAN LINE                             
DOBJRDRQ EQU   20                  AGYHDR, AGYCAN, MKGAPP, & MKGREJ             
*                                                                               
BTODAY   DS    XL3                 TODAY'S DATE IN BINARY (YMD)                 
DATENUM  DS    X                                                                
PRIORDAT DS    XL3                 YESTERDAY'S OR SOME PRIOR DAY'S DATE         
SYSNAME  DS    CL3                 3 CHR FACPAK NAME                            
SYSN1    DS    CL1                 1 CHR FACPAK NAME                            
WRKFILNO DS    XL2                 WORK FILE NUMBER                             
WRKRECNO DS    F                   WORKER FILE RECORD NUMBER                    
AFACIDT  DS    A                   A(FACIDTAB) TABLE                            
*                                                                               
WORK     DS    XL64                                                             
KEY      DS    XL64                                                             
KEYSAVE  DS    XL64                                                             
DKEY     DS    XL64                KEY OF LAST DA RECORD ADDED                  
HEADER   DS    XL32                                                             
DMWORK   DS    12D                                                              
*                                                                               
FILENAME DS    CL7                 DMGR FILENAME                                
LASTFILE DS    CL1                 PREVIOUS FILE NUMBER                         
DALINK   DS    CL4                 DISK ADDRESS OF LAST ADDREC                  
DDA      DS    CL4                 DISK ADDRESS OF GETREC                       
DALAST   DS    CL1                 LAST DA FILE ADDED TO                        
RACTN    DS    CL1                 RECORD TYPE (COPY CHNG ADD)                  
LASTACTN DS    CL1                 LAST TYPE (COPY CHNG ADD)                    
DLNFRID  DS    XL2                 DELIVERY NOTICE SENDER ID NUM                
ROUTNGCD DS    CL5                 ROUTING CODE                                 
ELCDLO   DS    XL1                                                              
ELCDHI   DS    XL1                                                              
ELCODE   DS    XL1                                                              
REVISION DS    XL1                 REVISION NUMBER                              
ORDAUDTR DS    X                   ORDER AUDIT TRAIL ACTION                     
REPORDTP DS    XL1                 REP ORDER TYPE                               
COPYQ    EQU   X'01'                                                            
CHANGEQ  EQU   X'02'                                                            
ADDQ     EQU   X'03'                                                            
NUMSPOTS DS    XL1                                                              
*                                                                               
TRDEMTHD DS    CL1                 TRADE METHOD                                 
TRDEDATA DS    CL8                 DATA TO DETERMINE TRADE                      
*                                                                               
VERSION# DS    XL2                                                              
RECCOUNT DS    F                   RECORD COUNT                                 
*                                                                               
DEMSAVE  DS    XL6                 DEMO EXIT SAVE AREA FOR DEMPROS              
*                                                                               
SAVEBUFF DS    0XL12                                                            
SBVPQTAB DS    A                   A(PQTAB)                                     
SBVPQFIL DS    A                   A(PQFILE DTF)                                
SBSAVE   DS    A                   DISPLACEMENT TO START OF SAVE AREA           
*                                                                               
SAVESIN  DS    F                                                                
NEXTSIN  DS    F                                                                
QSIN     DS    CL6                                                              
PRTQID   DS    CL8                                                              
*                                                                               
PROFDAR  DS    CL16                DAR PROFILE                                  
PDARONHD EQU   PROFDAR+0            - ON-HOLD IF MKGD OKAYED BY REP?            
*                                                                               
PROFOM   DS    CL16                OM PROFILE                                   
POMUSEOM EQU   PROFOM               - USES ORDER MANAGER?                       
POMAUCFM EQU   PROFOM+2             - PARITAL CONFIRM WORKFLOW                  
POMMGREP EQU   PROFOM+15            - CREATE MKGD TRANSACTION REPORT?           
*                                                                               
REMUSER  DS    CL3                                                              
IOBRDLST DS    XL128                                                            
*                                                                               
ACURELEM DS    F                   ADDRESS OF CURRENT ELEMENT                   
ELEM     DS    CL256                                                            
SVDOSPEL DS    XL(DOSPLNQ2)        SAVED DOSP ELEMENT                           
DOSTELEM DS    XL(DOSTLNQ2)        NEW STATUS ELEMENT                           
DOSAELEM DS    XL(DOSALNQ2)        NEW SALESPERSON REASSIGNMENT ELEMENT         
*                                                                               
SV1OR2   DS    X                                                                
GBWORK   DS    XL44                D/A LINKED FILES WORK AREA                   
         DS    0A                  ALIGNMENT                                    
GETBLK   DS    XL64                FOR SPGETBUYD                                
         ORG   GETBLK                                                           
       ++INCLUDE SPGETBUYD                                                      
         ORG   GETBLK+L'GETBLK                                                  
*                                                                               
IOA1     DS    6000C               I/O AREA                                     
IOA2     DS    6000C               I/O AREA                                     
*                                                                               
SPULAREA DS    XL3200                                                           
*                                                                               
WRKRIOA  DS    14336C              BYTES 0-1   = LENGTH OF RECORD               
*                                  BYTE  2     = L(DATA)+1                      
*                                  BYTES 3-??  = DATA                           
WRECQLNQ EQU   *-WRKRIOA                                                        
*                                                                               
HUGEBLCK DS    18432C              HUGE OUTPUT BLOCK                            
WORKX    EQU   *                                                                
*                                                                               
REPDKEYD DSECT                     DSECT TO USE OVER WRKRIOA IN AGYRCL          
RKEYDA   DS    XL4                 DISK ADDRESS OF AGENCY ORDER RECORD          
ROLDKEYS DS    XL800               TO REBUILD REP EDI PASSIVE KEYS              
RNEWKEYS DS    XL800                                                            
RKEYIO   DS    XL4000                                                           
         EJECT                                                                  
       ++INCLUDE DMWRKRD                                                        
       ++INCLUDE DMWRKRK                                                        
       ++INCLUDE FADSECTS                                                       
       ++INCLUDE FAPQPL                                                         
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE CTGENDARE                                                      
       ++INCLUDE GEGENDARE                                                      
       ++INCLUDE CTGENRAD                                                       
       ++INCLUDE DMGREQUS                                                       
       ++INCLUDE DMPRTQD                                                        
       ++INCLUDE DMPRTQS                                                        
*PREFIX=SR                                                                      
       ++INCLUDE DMPRTQK                                                        
*PREFIX=                                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DMFILTABD                                                      
       ++INCLUDE TASYSWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
RQHHDRD  DSECT                                                                  
       ++INCLUDE DMREQHDRA                                                      
       ++INCLUDE DMWRKFL                                                        
*********INCLUDE DMWRKFK                                                        
       ++INCLUDE DDEDICTFIL                                                     
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE SPTRPAT                                                        
       ++INCLUDE SPTRSHIP                                                       
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SRDARFFD                                                       
         EJECT                                                                  
RCONRECD DSECT                                                                  
       ++INCLUDE REGENCON                                                       
         EJECT                                                                  
RDARRECD DSECT                                                                  
       ++INCLUDE REGENDAR                                                       
         EJECT                                                                  
RSTARECD DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
         EJECT                                                                  
RMKGRECD DSECT                                                                  
       ++INCLUDE REGENMKG                                                       
         EJECT                                                                  
       ++INCLUDE REPFACSQ                                                       
RAG2RECD DSECT                                                                  
       ++INCLUDE REGENAGY2                                                      
         EJECT                                                                  
       ++INCLUDE GEGENSPSAL                                                     
         EJECT                                                                  
       ++INCLUDE SPDARDARED                                                     
         EJECT                                                                  
       ++INCLUDE SPDARMKGDD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENDRORD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENDRMKN                                                     
         EJECT                                                                  
       ++INCLUDE SPGENDRMKO                                                     
         EJECT                                                                  
       ++INCLUDE SPGENWIPW                                                      
         EJECT                                                                  
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
       ++INCLUDE SPSYSFAC                                                       
         EJECT                                                                  
       ++INCLUDE DDDARETABD                                                     
         EJECT                                                                  
       ++INCLUDE FACIDTABD                                                      
         EJECT                                                                  
       ++INCLUDE DDGETDARED                                                     
         EJECT                                                                  
ORDD     DSECT                                                                  
ORDSTAT  DS    XL1                 ORDER OR MG STATUS                           
ORDCOL   DS    XL1                 COLOR CODE                                   
ORDTYP   DS    XL1                 SECONDARY TYPE COMPARE                       
ORDTCMTS EQU   X'80'               WITH COMMENTS                                
ORDTNCMT EQU   X'40'               NO COMMENTS                                  
ORDLNQ   EQU   *-ORDD              LENGTH OF EACH ENTRY                         
*                                                                               
LAYOUTD  DSECT                                                                  
LAYMED   DS    CL1                 FIELDS MADE UP IN THE BUY SCREEN             
LAYBYR   DS    CL3                                                              
LAYCLT   DS    CL3                                                              
LAYPRD   DS    CL3                                                              
LAYEST   DS    CL3                                                              
LAYSTA   DS    CL8                                                              
*                                  EXTRA FIELDS NEEDED FOR MGEACC               
LAYFLT   DS    CL2                 - FLIGHT NUMBER                              
LAYGRPCD DS    CL3                 - MAKEGOOD GROUP CODE (FROM REP)             
LAYCORT  DS    CL1                 - CASH OR TRADE                              
LAYPRD1  DS    CL3                 - PRODUCT IF POL IN BUY HEADER               
LAYPRD2  DS    CL3                 - PIGGYBACK PRODUCT                          
LAYEND   EQU   *                                                                
*                                                                               
LAYOUT2D DSECT                                                                  
LAY2MED  DS    CL1                 FIELDS MADE UP IN THE DARE SCREEN            
LAY2BYR  DS    CL3                                                              
LAY2MTHD DS    CL1                 - CASH OR TRADE                              
LAY2CLT  DS    CL3                                                              
LAY2PRDS DS    CL7                                                              
LAY2EST  DS    CL6                                                              
LAY2STA  DS    CL8                                                              
LAY2OORD DS    CL8                                                              
LAY2END  EQU   *                                                                
*                                                                               
BUYINFD  DSECT                                                                  
STRTIM   DS    XL2                                                              
ENDTIM   DS    XL2                                                              
SPTLEN   DS    XL1                                                              
SPTCST   DS    XL3                                                              
SPTDAT   DS    XL2                                                              
LINNO    DS    XL2                                                              
BYINFLN  EQU   *-STRTIM                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029SRDAR00   06/10/20'                                      
         END                                                                    
