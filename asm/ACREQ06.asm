*          DATA SET ACREQ06    AT LEVEL 159 AS OF 11/14/18                      
*PHASE T60406A                                                                  
*INCLUDE ACJAX                                                                  
*INCLUDE ACJOBCOL                                                               
         TITLE 'ACREQ06 - REQUEST - VALIDATE JOB IS BILLABLE'                   
*----------------------------------------------------------------------         
* GHOA 159 14MAR18 SPEC-18341 JOB NEXT CYCLE RESET TO 1                         
*----------------------------------------------------------------------         
T60406   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWSX-LWS,T60406,RA,RR=R5,CLEAR=YES                               
         USING LWS,RC              RC=A(LOCAL WORKING STORAGE)                  
         L     R9,0(R1)                                                         
         USING GWS,R9              R9=A(GLOBAL WORKING STORAGE)                 
         LA    R8,RCARDS                                                        
         USING ACQD,R8             R8=A(REQUEST CARDS)                          
         L     R7,ASAVE                                                         
         USING TWAD,R3             R7=A(TWA)                                    
         ST    R5,RELO                                                          
*                                                                               
         LA    RF,LWS                                                           
         AHI   RF,IO3-LWS                                                       
         ST    RF,AIO3                                                          
         LA    RF,LWS                                                           
         AHI   RF,IO4-LWS                                                       
         ST    RF,AIO4                                                          
         LA    RF,LWS                                                           
         AHI   RF,MEDR-LWS                                                      
         ST    RF,AMEDR                                                         
         LA    RF,LWS                                                           
         AHI   RF,COLOUT-LWS                                                    
         ST    RF,ACOLOUT                                                       
         LA    RF,LWS                                                           
         AHI   RF,OPVTAB-LWS                                                    
         ST    RF,AOPVTAB                                                       
         LA    RF,LWS                                                           
         AHI   RF,ESWC-LWS                                                      
         ST    RF,AESWC                                                         
         LA    RF,LWS                                                           
         L     RE,=AL4(RETLB-LWS)                                               
         AR    RF,RE                                                            
         ST    RF,ARETLB                                                        
         LA    RF,LWS                                                           
         L     RE,=AL4(USRF-LWS)                                                
         AR    RF,RE                                                            
         ST    RF,AUSRF                                                         
*                                                                               
         ICM   R0,14,=X'D9000A'                                                 
         LA    R2,PHASES                                                        
         LA    R4,VJOBBER                                                       
*                                                                               
INIT03   ICM   R0,1,0(R2)          LOAD JOBBER, ETC                             
         GOTO1 CALLOV,DMCB,0,(R0),0                                             
         MVC   0(4,R4),0(R1)                                                    
         LA    R2,1(R2)                                                         
         LA    R4,4(R4)                                                         
         CLI   0(R2),X'FF'                                                      
         BNE   INIT03                                                           
         EJECT                                                                  
***********************************************************************         
* TEST OK TO BILL JOB                                                 *         
***********************************************************************         
GETJOB   MVC   KEY,SPACES          READ JOB                                     
         MVC   KEY(15),ACQCPY                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,ACCOUNT,KEY,AIO3                             
         CLI   DMCB+8,0                                                         
         BE    *+6                 NO RECORD                                    
         DC    H'0'                                                             
         MVC   MEDIA,ACQCPY+9      SAVE                                         
*                                                                               
         USING JOBEL,R2                                                         
         L     R2,AIO3                                                          
         MVI   ELCODE,JOBELQ       GET LOCK ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   GETJOB05            NOT JOB, GOTO GETJOB05 LIKE ORIGINAL         
         TM    JOBBIST,JOBB2BIL+JOBB3BIL         NEXT CYCLE = 3?                
         BZ    GETJOB05                                                         
         MVI   JOBBIST,0           RESET BACK TO 1                              
*                                                                               
         GOTO1 DATAMGR,DMCB,DMWRT,ACCOUNT,AIO3,AIO3                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETJOB05 LA    R6,GOBDATA                                                       
         USING GOBLOCKD,R6                                                      
         LA    RF,GOXBLOCK                                                      
         STCM  RF,15,GOAEXT                                                     
         MVC   GOADM,DATAMGR                                                    
         MVC   GOSELCUL,ACQCPY     SET JOB DETAIL                               
         MVC   GOSELCLI(3),ACQACT                                               
         OC    GOSELCLI,SPACES                                                  
         MVC   GOSELPRO(3),ACQACT+3                                             
         OC    GOSELPRO,SPACES                                                  
         MVC   GOSELJOB,ACQACT+6                                                
         OC    GOSELJOB,SPACES                                                  
         CLI   GOSELJOB,C' '                                                    
         BNH   GETJOB08                                                         
         GOTO1 VGETOPT,DMCB,GOBDATA                                             
*                                                                               
GETJOB08 LA    R5,JOBDATA          INIT AJAX                                    
         USING JXBLKD,R5                                                        
         MVC   JXACOMF,ACOMFACS    COMFACS                                      
         MVC   JXAGETOP,VGETOPT    GETOPT                                       
         MVC   JXAJOBR,VJOBBER     JOBBER                                       
*                                                                               
         L     RF,=V(ACJOBCOL)                                                  
         A     RF,RELO                                                          
         STCM  RF,15,JXAJOBC       JOB COL                                      
         LA    RF,GOBDATA                                                       
         STCM  RF,15,JXAGOBK       GOBLOCK                                      
         LA    RF,COLIST                                                        
         STCM  RF,15,JXACOLS       COLUMN LIST AREA                             
         L     RF,ACOLOUT                                                       
         STCM  RF,15,JXACOLTB      COLUMN OUTPUT TABLE                          
         LHI   RF,L'COLOUT                                                      
         STCM  RF,15,JXLCOLTB      LENGTH OF COLUMN OUTPUT TABLE                
         L     RF,AOPVTAB                                                       
         STCM  RF,15,JXAOPVTB      OPERAND VALUE TABLE                          
         LHI   RF,L'OPVTAB                                                      
         STCM  RF,15,JXLOPVTB      LENGTH OF OPERAND VALUE TABLE                
         L     RF,AIO3                                                          
         STCM  RF,15,JXAJOB                                                     
*                                                                               
         L     RF,AESWC            ESTIMATE WORK CODES                          
         STCM  RF,15,JXAEWC                                                     
         LHI   RF,L'ESWC                                                        
         STCM  RF,15,JXLEWC                                                     
         L     RF,ARETLB           RETAIL BUFFER                                
         STCM  RF,15,JXARETLB                                                   
         LHI   RF,L'RETLB                                                       
         STCM  RF,15,JXLRETLB                                                   
         L     RF,AUSRF            USER FIELD TABLE                             
         STCM  RF,15,JXAUSRF                                                    
         LHI   RF,L'USRF                                                        
         STCM  RF,15,JXLUSRF                                                    
         LA    RF,ACQD                                                          
         STCM  RF,15,JXACQ                                                      
*                                                                               
         MVC   JXWCFLT,ACQTRNF     WORKCODE FILTER                              
         MVI   JXXRSTA,JXXRSEX     ONLY FLAG BILLING PROBLEMS                   
         OI    JXOPTS,JXOPTRN      READ TRANSACTIONS                            
         MVI   JXACTN,JXAPACF      PROCESS ACCOUNT FIRST                        
         MVI   JXMODE,JXMREQS      SET REQUEST MODE                             
*                                                                               
         CLI   ACQACT,C' '                                                      
         BH    *+12                                                             
         MVI   BYTE,CLI1Q                                                       
         B     GETJOB29                                                         
         CLC   ACQPROG,=C'22'                                                   
         BE    GETJOB10                                                         
         CLI   ACQACT+3,C' '                                                    
         BH    *+12                                                             
         MVI   BYTE,PRD1Q                                                       
         B     GETJOB29                                                         
         CLI   ACQACT+6,C' '                                                    
         BH    GETJOB10                                                         
         MVI   BYTE,JOBQ                                                        
         B     GETJOB29                                                         
*                                                                               
GETJOB10 CLC   ACQPROG,=C'23'      TEST REVERSAL                                
         BE    *+14                YES,                                         
         CLC   ACQPROG,=C'24'                                                   
         BNE   GETJOB20                                                         
         OI    JXRROPT,JXRROUNB                                                 
         MVC   JXRRNUM,ACQSEL                                                   
         GOTO1 DATCON,DMCB,(0,ACQEND),(2,JXRRDT2)                               
*                                                                               
GETJOB20 CLI   ACQACT+6,C' '                                                    
         BNH   GETJOB40                                                         
         L     RF,=V(ACJAX)                                                     
         A     RF,RELO                                                          
         GOTO1 (RF),JXBLKD         TEST JOB IS BILLABLE                         
         CLI   JXPAERR,0           TEST POSTING ERROR                           
         BNE   EMPAERR                                                          
         OC    JXERRS,JXERRS       TEST ANY ERROR                               
         BZ    GETJOB40            NO, GET BILL NUMBER                          
         MVC   FERN,JXERRS         YES,  SET ERROR MESSAGE                      
         LHI   R1,AE$CBOS          TEST CYCLE OUT OF SEQUENCE                   
         CLM   R1,3,JXERRS                                                      
*        BNE   GETJOB30            NO,                                          
         BNE   XIT                 NO,                                          
         MVI   BYTE,CYCMQ                                                       
         CLC   ACQAPPL+6(2),SPACES TEST MOA INPUT                               
         BNE   GETFLD              YES,                                         
GETJOB29 LHI   R1,AE$MISIF         SET MISSING INPUT                            
         STCM  R1,3,FERN                                                        
         B     GETFLD                                                           
*                                                                               
GETJOB30 CLI   JXBLCODE,JXBLCLNT   TEST CLIENT BILL                             
         BNE   GETJOB40                                                         
         LA    RF,AE$CBTJI         SET 'CLIENT BILL TYPE NOT ALLOWED'           
         STCM  RF,3,FERN                                                        
         B     XIT                                                              
*                                                                               
GETJOB40 CLC   ACQPROG,=C'22'      TEST DRAFT                                   
         BE    GETJOBX                                                          
         CLC   ACQPROG,=C'24'                                                   
         BE    GETJOBX                                                          
*                                                                               
         LA    RF,JXRTLBLN                                                      
         CLC   ACQPROG,=C'23'                                                   
         BNE   *+8                                                              
         LA    RF,JXRTLNUM                                                      
         XR    R0,R0                                                            
         ICM   R0,1,0(RF)          NUMBER OF BILLS                              
         BNZ   *+8                                                              
         AHI   R0,1                MUST BE AT LEAST ONE                         
         CVD   R0,DUB                                                           
         ZAP   NUMBILS,DUB                                                      
*                                                                               
         LA    R2,WORK             BUILD A LOCK ELEMENT                         
         XC    WORK,WORK                                                        
         USING LGLELD,R2                                                        
         MVI   LGLEL,LGLELQ                                                     
         MVI   LGLLN,LGLLNQ                                                     
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   LGLDATE,FADATEB                                                  
         MVC   LGLTIME,FATIME                                                   
         MVC   LGLLUID,FASYM                                                    
         OI    LGLSTAT,LGLSLOCK                                                 
         DROP  R1                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,DMREAD,ACCOUNT,AIO3,AIO3                            
         CLI   DMCB+8,0                                                         
         BE    *+6                 NO RECORD                                    
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO3                                                          
         MVI   ELCODE,LGLELQ       GET LOCK ELEMENT                             
         BAS   RE,GETEL                                                         
         BE    GETJOB50                                                         
         GOTOR HELLO,DMCB,(C'P',ACCOUNT),AIO3,WORK,0                            
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     GETJOB70                                                         
*                                                                               
GETJOB50 TM    LGLSTAT,LGLSLOCK    TEST ACCOUNT LOCKED                          
         BO    EMACTLK                                                          
         MVC   LGLDATE(LGLLNQ-2),WORK+2                                         
*                                                                               
GETJOB70 GOTO1 DATAMGR,DMCB,DMWRT,ACCOUNT,AIO3,AIO3                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETJOBX  DS    0H                                                               
         CLC   ACQPROG,=C'22'      TEST DRAFT                                   
         BE    GTBNUMX                                                          
         B     GETMED                                                           
         DROP  R2                                                               
*                                                                               
GETFLD   LA    R1,LREQMAP          GET CURSOR TO FIELD                          
GETFLD2  CLI   0(R1),LREQMAPX                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R1),BYTE                                                     
         BE    GETFLD3                                                          
         LA    R1,3(R1)                                                         
         B     GETFLD2                                                          
GETFLD3  XR    R0,R0                                                            
         ICM   R0,3,1(R1)                                                       
         AR    R0,R3                                                            
         ST    R0,FADR                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET MEDIA RECORD/ELEMENT                                            *         
***********************************************************************         
GETMED   MVC   KEY,SPACES          READ MEDIA                                   
         LA    R2,KEY                                                           
         USING PMDRECD,R2                                                       
         MVI   PMDKTYP,PMDKTYPQ                                                 
         MVC   PMDKCPY,ACQCPY                                                   
         MVC   PMDKMED,MEDIA       MEDIA                                        
         GOTO1 DATAMGR,DMCB,DMREAD,ACCOUNT,KEY,AMEDR                            
         CLI   DMCB+8,0                                                         
         BE    *+6                 NO RECORD                                    
         DC    H'0'                                                             
         L     R2,AMEDR                                                         
         MVI   ELCODE,PMDELQ       GET MEDIA ELEMENT                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R2,APMDEL                                                        
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET PROFILE                                                         *         
***********************************************************************         
         XC    PROFREC,PROFREC                                                  
         XC    PROFDATA,PROFDATA                                                
         LA    R4,PROFDATA         READ PROFILE RECORD                          
         USING PROFKD,R4                                                        
         MVI   PROFKSYS,C'A'         ACCOUNT SYSTEM                             
         MVC   PROFKPGM+1(2),ACQPROG PROGRAM                                    
         MVC   PROFKAGY,TWAAGY     ALPHA ID                                     
         GOTO1 GETPROF,DMCB,PROFKEY,PROFREC,DATAMGR                             
         MVC   OPTMONTH,PROFREC+14                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* GET BILL NUMBER                                                     *         
***********************************************************************         
GTBNUM   DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(10,RCDATE)                                    
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,TODAY0)                                
         MVC   BILDT0,TODAY0       BILL DATE IS TODAY (YYMMDD)                  
         CLC   ACQSTART,SPACES                                                  
         BE    *+10                                                             
         MVC   BILDT0,ACQSTART     UNLESS THEY SPECIFY                          
*                                                                               
         CLC   ACQPROG,=C'22'      TEST DRAFT                                   
         BE    GTBNUMX                                                          
         CLC   ACQPROG,=C'24'                                                   
         BE    GTBNUMX                                                          
         CLC   ACQPROG,=C'23'      TEST REVERSAL                                
         BNE   GTBNUM1                                                          
         CLI   ACQOPT1,C'Y'        TEST USE SAME NUMBER                         
         BNE   GTBNUM1                                                          
         MVC   ACQSRTAR(6),ACQSEL                                               
         B     GTBNUMX                                                          
*                                                                               
GTBNUM1  MVC   MNTH,RCDATE         DEFAULT MONTH IS RUN DATE                    
         SR    R1,R1                                                            
         ICM   R1,1,OPTMONTH       COMPANY PROFILE OPTION                       
         BZ    GTBNUM3                                                          
         CHI   R1,12                                                            
         BH    GTBNUM3                                                          
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(3),DUB+6(2)    GET CHARARCTER MONTH NUMBER                  
         CLC   WORK+1(2),BILDTMM   IF PROFILE MONTH NOT HIGHER                  
         BL    GTBNUM3             THAN RUN DATE- USE RUN DATE                  
         CLC   WORK+1(2),=C'12'    IF PROFILE IS FOR DECEMBER                   
         BNE   *+14                                                             
         CLC   BILDTMM,=C'01'      AND DATE IS JANUARY                          
         BE    GTBNUM3             USE RUN MONTH                                
         MVC   MNTH,WORK+1         MONTH FROM PROFILE                           
*                                                                               
GTBNUM3  DS    0H                                                               
         MVC   KEY,SPACES          READ LEDGER                                  
         MVC   KEY(3),ACQCPY                                                    
         GOTO1 DATAMGR,DMCB,DMREAD,ACCOUNT,KEY,AIO3                             
         CLI   DMCB+8,0                                                         
         BE    *+6                 NO RECORD                                    
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         MVI   ELCODE,PMDELQ                                                    
         BAS   RE,GETEL            TEST AGENCY LEVEL ELEMENT                    
         BNE   GTBNUM5             NOT AT AGENCY                                
         OI    WRIT,WRITLDG        SET WRITE LEDGER RECORD                      
         B     GTBNUM19                                                         
*                                                                               
GTBNUM5  MVC   KEY,SPACES          READ CLIENT RECORD                           
         MVC   KEY(6),ACQCPY                                                    
         GOTO1 DATAMGR,DMCB,DMREAD,ACCOUNT,KEY,AIO3                             
         CLI   DMCB+8,0                                                         
         BE    *+6                 NO RECORD                                    
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         MVI   ELCODE,PMDELQ                                                    
         BAS   RE,GETEL            TEST CLIENT LEVEL ELEMENT                    
         BE    GTBNUM11            YES,                                         
*                                                                               
         L     R2,AIO3                                                          
         MVI   ELCODE,NUMELQ                                                    
         USING NUMELD,R2                                                        
         BAS   RE,GETEL            TEST CLIENT NUMBER ELEMENT                   
         B     *+8                                                              
GTBNUM9  BAS   RE,NEXTEL                                                        
         BNE   GTBNUM13                                                         
         CLI   NUMLN,NUMLN2Q                                                    
         BL    GTBNUM9                                                          
         CLC   NUMTYPE,MEDIA                                                    
         BNE   GTBNUM9                                                          
*                                                                               
GTBNUM11 OI    WRIT,WRITLVA        SET WRITE LEVEL A RECORD                     
         B     GTBNUM19                                                         
*                                                                               
GTBNUM13 L     R2,APMDEL           USE MEDIA LEVEL                              
         OI    WRIT,WRITMED        SET WRITE MEDIA RECORD                       
         B     GTBNUM19                                                         
*                                                                               
GTBNUM15 MVC   BILNUM,=C'000001'                                                
         B     GTBNUM25                                                         
*                                                                               
         USING PMDELD,R2                                                        
GTBNUM19 CLI   PMDEL,PMDELQ                                                     
         BNE   GTBNUM23                                                         
         CLC   MNTH,PMDLBILL           SAME MONTH                               
         BE    *+16                    YES,                                     
         MVC   PMDLBILL(2),MNTH        SET NEW MONTH                            
         MVC   PMDLBILL+2(4),PMDRBILL  AND RESET VALUE                          
         MVC   PMDFBILL,PMDLBILL       SAVE START NUMBER                        
*                                                                               
         MVC   BILNUM,PMDLBILL                                                  
         PACK  FULL,PMDLBILL+2(4)                                               
         AP    FULL,NUMBILS                                                     
         UNPK  PMDLBILL+2(4),FULL                                               
         OI    PMDLBILL+5,X'F0'                                                 
         B     GTBNUM25                                                         
*                                                                               
         USING NUMELD,R2                                                        
GTBNUM23 CLI   NUMEL,NUMELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   R4,15,APMDEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING PMDELD,R4                                                        
         CLC   MNTH,PMDLBILL           SAME MONTH                               
         BE    *+16                    YES,                                     
         MVC   NUMAFT(2),MNTH      RESET MONTH                                  
         MVC   NUMAFT+2(4),PMDRBILL                                             
         MVC   BILNUM,NUMAFT                                                    
         PACK  FULL,NUMAFT                                                      
*        ZAP   FULL,NUMAFT                                                      
         AP    FULL,NUMBILS                                                     
         UNPK  NUMAFT,FULL                                                      
         OI    NUMAFT+5,X'F0'                                                   
*                                                                               
GTBNUM25 MVC   ACQSRTAR(6),BILNUM  SET BILL NUMBER IN REQUEST                   
         CLI   WRIT,0              TEST NEED TO UPDATE RECORD                   
         BE    GTBNUMX             NO,                                          
         ICM   R4,15,AIO3                                                       
         TM    WRIT,WRITMED                                                     
         BZ    *+8                                                              
         ICM   R4,15,AMEDR                                                      
         MVC   KEY,0(R4)                                                        
         GOTO1 DATAMGR,DMCB,DMREAD,ACCOUNT,KEY,AIO4                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,DMWRT,ACCOUNT,(R4),(R4)                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GTBNUMX  DS    0H                                                               
         MVC   FERN,=AL2(FF)                                                    
         B     XIT                                                              
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* OTHER ROUTINES                                                      *         
***********************************************************************         
         GETEL R2,DATADISP,ELCODE                                               
*                                                                               
EMACTLK  LA    R0,AE$ACTLK                                                      
EMX      STCM  R0,3,FERN                                                        
         B     XIT                                                              
*                                                                               
EMPAERR  MVC   HALF,JXERRS         SET ERROR MESSAGE                            
         MVI   BYTE,C'E'                                                        
         XC    WORK,WORK                                                        
         MVI   WORK,14                                                          
         MVC   WORK+1(14),JXPAERR+1                                             
*                                                                               
         LA    R1,GTBK                                                          
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,HALF        MESSAGE NUMBER                               
         MVI   GTMSYS,6                                                         
         MVC   GTMTYP,BYTE         'I' OR 'E'                                   
         LA    R0,WORK+1                                                        
         STCM  R0,7,GTATXT                                                      
         MVC   GTLTXT,WORK                                                      
         GOTO1 GETTXT,(R1)         RESOLVE MESSAGE                              
         MVC   FERN,=AL2(FE)       SET MESSAGE ALREADY SUPPLIED                 
*                                                                               
XIT      XIT1  ,                                                                
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DATA CONSTANTS                                                      *         
***********************************************************************         
PHASES   DS    0X                                                               
         DC    AL1(QJOBBER)                                                     
         DC    AL1(QGETOPT)                                                     
         DC    X'FF'                                                            
*                                                                               
DATADISP DC    H'49'                                                            
ACCOUNT  DC    CL8'ACCFIL  '                                                    
DMREAD   DC    CL8'DMREAD  '                                                    
DMWRT    DC    CL8'DMWRT   '                                                    
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
*                                                                               
LWS      DSECT                                                                  
RELO     DS    F                                                                
AIO3     DS    A                                                                
AIO4     DS    A                                                                
AMEDR    DS    A                   A(MEDIA RECORD)                              
*                                                                               
ACOLOUT  DS    A                                                                
AOPVTAB  DS    A                                                                
AESWC    DS    A                                                                
AESWCB   DS    A                                                                
ARETLB   DS    A                                                                
AUSRF    DS    A                                                                
*                                                                               
VJOBBER  DS    A                   V(JOBBER)                                    
VGETOPT  DS    A                   V(GETOPT)                                    
*                                                                               
OPTMONTH DS    XL1                                                              
MEDIA    DS    CL1                                                              
MNTH     DS    CL2                                                              
BILNUM   DS    CL6                                                              
BILDT0   DS    CL6                 BILL DATE (YYMMDD)                           
         ORG   BILDT0                                                           
BILDTYY  DS    CL2                                                              
BILDTMM  DS    CL2                                                              
BILDTDD  DS    CL2                                                              
*                                                                               
RCDATE   DS    CL8                 MM/DD/YY                                     
TODAY0   DS    CL6                 YYMMDD                                       
*                                                                               
APMDEL   DS    A                   MEDIA ELEMENT ON MEDIA RECORD                
*                                                                               
WRIT     DS    XL1                                                              
WRITLDG  EQU   X'80'                                                            
WRITLVA  EQU   X'40'                                                            
WRITMED  EQU   X'20'                                                            
*                                                                               
NUMBILS  DS    PL2                                                              
*                                                                               
PROFDATA DS    CL16                DATA TO PASS TO GETPROF                      
PROFREC  DS    CL16                PROFILE                                      
*                                                                               
WORK     DS    CL255                                                            
GTBK     DS    XL(L'GTBLOCK)                                                    
*                                                                               
COLIST   DS    XL400               COLUMN LIST AREA                             
*                                                                               
JOBDATA  DS    XL(JXLNQ)                                                        
GOBDATA  DS    XL(GOLNQ)                                                        
*                                                                               
IO3      DS    XL2000                                                           
IO4      DS    XL2000                                                           
MEDR     DS    XL2000              MEDIA RECORD                                 
*                                                                               
COLOUT   DS    XL10000             COLUMN OUTPUT TABLE                          
OPVTAB   DS    XL12000             OPERAND VALUE TABLE                          
ESWC     DS    XL5000                                                           
RETLB    DS    XL20000                                                          
USRF     DS    XL1000                                                           
LWSX     EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
*              GETPROFS KEY DSECT                                     *         
***********************************************************************         
*                                                                               
PROFKD   DSECT                                                                  
PROFKEY  DS    0CL16                                                            
PROFKSYS DS    CL1                                                              
PROFKPGM DS    CL3                                                              
         DS    CL1                                                              
PROFKUNL DS    CL2                                                              
PROFKACC DS    CL3                                                              
PROFKAST DS    CL1                                                              
PROFKOFF DS    CL1                                                              
PROFKAGY DS    CL2                                                              
PROFKOFC DS    CL2                 NEW OFFICE                                   
                                                                                
       ++INCLUDE ACREQWORK                                                      
*                                                                               
JXBLKD   DSECT                                                                  
* ACJAXD                                                                        
         PRINT OFF                                                              
       ++INCLUDE ACJAXD                                                         
         PRINT ON                                                               
                                                                                
GOBLOCKD DSECT                                                                  
* ACGOBLOCK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGOBLOCK                                                      
         PRINT ON                                                               
* ACGOXBLOCK                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGOXBLOCK                                                     
         PRINT ON                                                               
GOLNQ    EQU   *-GOBLOCKD                                                       
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'159ACREQ06   11/14/18'                                      
         END                                                                    
