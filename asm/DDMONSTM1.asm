*          DATA SET DDMONSTM1  AT LEVEL 124 AS OF 09/21/18                      
*PHASE MONSOMA                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE NUMVAL                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE COVAIL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRTQCPY                                                                
         EJECT                                                                  
***********************************************************************         
*  TITLE:        MONSOON -- ATTACH SOON REQUESTS (WITH MONSTER)       *         
*                                                                     *         
*  COMMENTS:     WORKS IN TANDEM WITH DDMONSTER.                      *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- WORK                                           *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- PRINT QUEUE TABLE                              *         
*                R7 -- S P A R E                                      *         
*                R8 -- WORK                                           *         
*                R9 -- COMMON STORAGE AREA (2ND BASE)                 *         
*                RA -- CPRINT                                         *         
*                RB -- PROGRAM BASE                                   *         
*                RC -- COMMON STORAGE AREA                            *         
*                RD -- REGISTER SAVE AREA                             *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
***********************************************************************         
         TITLE 'MONSOON - ATTACH SOON REQUESTS / CALLED BY MONSTER'             
         EJECT                                                                  
MONSOON  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*MONSOON,ARDCHAIN                                              
         ENTRY UTL                 FOR DATAMGR                                  
         ENTRY SSB                                                              
*                                                                               
         L     RC,ACOMMON          COMMON STORAGE AREA                          
         LR    R9,RC                                                            
         AHI   R9,4096                                                          
         USING COMMWORK,RC,R9                                                   
*                                                                               
         L     RA,VCPRINT                                                       
         USING DPRINT,RA                                                        
         USING CIDATAD,R6                                                       
*                                                                               
         LINK  EP=FWRGETJI,PARAM=(JOBID)  GET JOB ID                            
*                                                                               
         MVC   MNSOONID,JOBID      EXTRACT STEPNAME                             
         MVC   MNSOONID,MNSOONID+1 ***TEMPORARY TEST**                          
*                                                                               
         BRAS  RE,INITIAL          INITIALIZE                                   
*                                                                               
         MVI   WORK,C'I'                                                        
         LA    R1,WORK                                                          
         BRAS  RE,EVENT            REPORT INITIALISED                           
*                                                                               
GETTRANS TIMEUSED STORADR=TCPUS,LINKAGE=SYSTEM,CPU=MIC                          
         LA    R2,ATBGTRN_STRUCTURE                                             
         BRAS  RE,CALLAPPC         GET MULTI-TRANS TRANSACTION                  
*                                                                               
         ICM   R0,15,RETURN_CODE                                                
         BZ    GTRN02                                                           
         CHI   R0,8                ANYTHING MORE TO DO?                         
         BE    GOODBYE                                                          
         CHI   R0,28                                                            
         BE    GOODBYE                                                          
         DC    H'0'                                                             
*                                                                               
GTRN02   BRAS  RE,BLDPQTAB         BUILD TABLE OF PQ INFO (ONCE ONLY)           
*                                                                               
         BC    0,GJID                                                           
         OI    *-3,X'F0'           ** SELF-MODIFYING CODE **                    
*                                  ONLY GET THE JOB NAME ONCE                   
*                                                                               
         EXTRACT FULL,'S',FIELDS=(ASID)                                         
         MVC   HALF,FULL+2                                                      
         L     R3,FULL             MVS JOB ACCOUNTING INFORMATION               
         LOCASCB ASID=(R3)         PUTS A(ASCB) INTO R1                         
         ST    R1,AASCB            SAVE A(ASCB)                                 
         L     R3,ASCBASSB-ASCB(R1) R3 = A(ASSB)                                
*                                                                               
         SAM31                                                                  
         L     R3,ASSBJSAB-ASSB(R3) R3 = A(JSAB)                                
         USING JSAB,R3                                                          
         MVC   JOBNAM,JSABJBNM     GET JOB NAME                                 
         SAM24                                                                  
         DROP  R3                                                               
*                                                                               
         MVC   P(15),=C'THIS JOB NAME: '                                        
         MVC   P+15(L'JOBNAM),JOBNAM                                            
         MVC   P+15+L'JOBNAM+2(6),=CL6'ASID: '                                  
         LA    R3,P+15+L'JOBNAM+8                                               
         GOTO1 VHEXOUT,DMCB,HALF,(R3),2,0                                       
*                                                                               
         MVC   JOBINAME,JOBNAM     SAVE JOBNAME                                 
         MVC   JOBIASID,0(R3)      SAVE ASID                                    
*                                                                               
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
GJID     LINK  EP=FWRGETJI,PARAM=(THISJID) GET THIS TRANSACTION JOB ID          
         GOTO1 VPRINTER                                                         
         MVC   P(25),=C'THIS TRANSACTION JOB ID: '                              
         MVC   P+25(L'THISJID),THISJID                                          
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         MVC   JOBIJID,THISJID     SAVE JOB ID                                  
*                                                                               
GCONV    BRAS  RE,GETCONV          GET APPC/MVS CONVERSATION                    
*                                                                               
DOATTCH  BRAS  RE,ATTACH           ATTACH THE JCL (IF NECESSARY)                
         BRAS  RE,DUPPQ            DUPLICATE PQ REP FOR CD-ROM                  
*                                                                               
         MVC   SEND_TYPE,ATB_SEND_AND_PREP_TO_RECEIVE                           
         CLI   TCBTMFLG,YES                                                     
         BE    *+12                                                             
         CLI   ETTMFLG,YES                                                      
         BNE   *+10                                                             
         MVC   SEND_TYPE,ATB_SEND_AND_CONFIRM                                   
*                                                                               
         CLI   RECYCLE,YES                                                      
         BNE   *+14                                                             
         MVC   BUFFER(32),=C'MONSOON RECYCLED                '                  
         B     NOABEND                                                          
*                                                                               
         MVC   BUFFER(32),=C'SOON REQUEST COMPLETE           '                  
         CLI   ABENDFLG,YES        ABEND?                                       
         BNE   NOABEND                                                          
         MVC   BUFFER+22(7),=C'W/ABEND'                                         
         CLI   TMOUTFLG,YES        ABEND DUE TO TIME OUT BY MONSOON?            
         BNE   NOABEND                                                          
         MVC   BUFFER+22(10),=C'W/TIME OUT'                                     
*                                                                               
NOABEND  DS    0H                                                               
         MVC   BUFFER+32(60),ERRMSG                                             
         MVC   BUFFER+92(4),AVOIDLDS                                            
         MVC   BUFFER+96(4),TOTALLDS                                            
         MVC   BUFFER+100(1),ABENDFLG                                           
         MVC   BUFFER+101(1),TCBTMFLG                                           
         MVC   BUFFER+102(1),ETTMFLG                                            
         MVC   BUFFER+103(1),UPDFLG                                             
         LA    R5,104                                                           
         BRAS  RE,LINXMT                                                        
*&&UK                                                                           
         ICM   R3,15,AJCLNTRY      ANY JCL ENTRY                                
         BZ    NOABEND1                                                         
         CLI   ABENDFLG,YES        ABEND?                                       
         BE    NOABEND1                                                         
         CLI   TMOUTFLG,YES        ABEND DUE TO TIME OUT BY MONSOON?            
         BE    NOABEND1                                                         
         L     RF,VSSB                                                          
         LAM   AR3,AR3,SSOTBLET-SSOOFF(RF)                                      
         SAC   512                                                              
         XC    0(8,R3),0(R3)       GOOD COMPLETION SO CLEAR JCL                 
         SAC   0                                                                
*&&                                                                             
NOABEND1 CLI   TCBTMFLG,YES                                                     
         BE    *+12                                                             
         CLI   ETTMFLG,YES                                                      
         BNE   RCWT                                                             
*                                                  SEND LONG SOON REC           
         MVC   SEND_TYPE,ATB_SEND_AND_PREP_TO_RECEIVE                           
         LHI   R5,BIGBUFLQ                         BIG BUFFER LENGTH            
         MVC   ATBSEND_STRUCTURE+40(4),ABIGBUF     CHANGE BUFFER ADR            
         BRAS  RE,LINXMT                                                        
         MVC   ATBSEND_STRUCTURE+40(4),=A(BUFFER)  RESTORE OLD ONE              
*                                                                               
RCWT     EQU   *                                                                
*                                                                               
         MVI   WORK,C'W'                                                        
         LA    R1,WORK                                                          
         BRAS  RE,EVENT                                                         
*                                                                               
         MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
         LA    R2,ATBRCVW_STRUCTURE                                             
         BRAS  RE,CALLAPPC         RECEIVE AND WAIT                             
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         CLC   STATUS_RECEIVED,ATB_CONFIRM_DEALLOC_RECEIVED                     
         BE    *+6                                                              
         DC    H'0'                MONSTER SHOULD HAVE DONE DEALLOC/CFM         
*                                                                               
         LA    R2,ATBCFMD_STRUCTURE                                             
         BRAS  RE,CALLAPPC         ISSUE CONFIRMATION                           
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         CLI   RECYCLE,YES         RECYCLE MONSOON                              
         BE    GOODBYE             YES, DON'T GET ANY MORE TRANSACTION          
*&&US                                                                           
         CLI   ABENDFLG,YES        ABEND?                                       
         BE    GOODBYE             YES, DON'T GET ANY MORE TRANSACTION          
*&&                                                                             
*                                                                               
         LA    R2,ATBRTRN_STRUCTURE                                             
         BRAS  RE,CALLAPPC         RETURN MULTI-TRANS TRANSACTION               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VISGENQ,DMCB,C'TASK'                                             
         GOTO1 VSHMUSS,DMCB,(0,=CL8'CLEANUP'),(0,=CL8'PRTQ'),0,0                
         B     GETTRANS                                                         
*                                                                               
GOODBYE  BRAS  RE,SHUTDOWN         WRAP UP                                      
*                                                                               
         MVC   FUNCTION,=F'1'      CHECK FOR # OUTSTANDING APPC CALL            
         LA    R2,ATBAMR1_STRUCTURE                                             
         BRAS  RE,CALLAPPC                                                      
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,ASYNCHRONOUS_NUMBER                                        
         BZ    GBYE030                                                          
*                                                                               
         MVC   FUNCTION,=F'2'      CLEANUP TRANSACTION PROGRAM                  
         LA    R2,ATBAMR1_STRUCTURE                                             
         BRAS  RE,CALLAPPC                                                      
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    GBYE020                                                          
         CLC   RETURN_CODE,=F'4'                                                
         BE    GBYE020                                                          
         DC    H'0'                                                             
GBYE020  CLI   TRACEFLG,YES                                                     
         BNE   *+14                                                             
         MVC   P(17),=CL17'TPCLEANEDUP'                                         
         BRAS  RE,PRNT                                                          
*                                                                               
GBYE030  CLI   TRACEFLG,YES                                                     
         BNE   *+14                                                             
         MVC   P(17),=CL17'GOODBYE    '                                         
         BRAS  RE,PRNT                                                          
*                                                                               
         MVI   WORK,C'G'           REPORT ON SHUTDOWN EVENT                     
         LA    R1,WORK                                                          
         BRAS  RE,EVENT                                                         
*                                                                               
         XBASE                                                                  
*                                                                               
ACOMMON  DC    A(COMMWORK)                                                      
ARDCHAIN DC    A(R13CHAIN)                                                      
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WRAP UP                                                             *         
***********************************************************************         
SHUTDOWN NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VENQDEQ,DMCB,(C'D',=C'ALL')                                      
         B     EXITOK                                                           
         DROP RB                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE                                                          *         
***********************************************************************         
INITIAL  NTR1  BASE=*,LABEL=*                                                   
         MVC   TITLE(7),=C'MONSOON'                                             
         MVC   P(17),=CL17'INITIALIZE'                                          
         BRAS  RE,PRNT                                                          
*                                                                               
         L     R2,PSATOLD-PSA(,0)  TCB ADDRESS                                  
         GOTO1 VHEXOUT,DMCB,(R2),P+30,4,0                                       
*                                                                               
         CLI   TRACEFLG,YES                                                     
         BNE   *+14                                                             
         MVC   P(17),=CL17'TCBADDRESS'                                          
         BRAS  RE,PRNT                                                          
*                                                                               
         EXTRACT DUB,FIELDS=TIOT   FIND DDNAME=TSTPQDD0, SET DSPACE=T           
*                                                                               
         L     R5,DUB                                                           
         AHI   R5,24                                                            
         XR    RF,RF                                                            
INIT02   CLI   0(R5),0             TEST FOR END OF TIOT TABLE                   
         BE    INIT04                                                           
         CLC   =C'DSPACE',4(R5)    SEE IF //DSPACEX CARD                        
         BNE   INIT02A                                                          
         MVC   BYTE,10(R5)         MOVE IN THE DSPACE VALUE                     
         B     INIT03                                                           
*                                                                               
INIT02A  CLC   =C'TSTPQDD0',4(R5)  TEST DDNAME=TSTPQDD0                         
         BNE   INIT02B                                                          
         MVI   BYTE,C'T'           SET DSPACE=T                                 
         B     INIT03                                                           
                                                                                
INIT02B  CLC   =C'CSCPQDD0',4(R5)  TEST DDNAME=CSCPQDD0                         
         BNE   INIT02C                                                          
         MVI   BYTE,C'C'           SET DSPACE=C                                 
         B     INIT03                                                           
                                                                                
INIT02C  CLC   =C'FQAPQDD0',4(R5)  TEST DDNAME=FQAPQDD0                         
         BNE   INIT02D                                                          
         MVI   BYTE,C'Q'           SET DSPACE=Q                                 
         B     INIT03                                                           
                                                                                
INIT02D  CLC   =C'BARPQDD0',4(R5)  TEST DDNAME=BARPQDD0                         
         BNE   INIT02X                                                          
         MVI   BYTE,C'B'           SET DSPACE=B                                 
         B     INIT03                                                           
                                                                                
INIT02X  IC    RF,0(R5)            GET LENGTH OF THIS ENTRY                     
         BXH   R5,RF,INIT02                                                     
*                                                                               
INIT03   L     RF,VSSB             SET DSPACE                                   
         MVC   SSODSPAC-SSOOFF(1,RF),BYTE                                       
*                                                                               
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),=CL8'DDSIO' DEFAULT                                      
         CLI   BYTE,C'T'           DSPACE=T?                                    
         BNE   *+10                                                             
*&&US*&& MVC   0(8,RF),=CL8'DDSION'  USE DDSION FOR TST                         
*&&UK*&& MVC   0(8,RF),=CL8'DDSIO'   USE DDSIO FOR TIME BEING                   
*                                                                               
INIT04   BRAS  RE,LOADMODS         LOAD CORE-RESIDENT PHASES                    
*                                                                               
         L     R1,X'10'(,0)        POINT TO CPU ID                              
         L     R1,X'C4'(R1)                                                     
         LA    R1,X'10'(R1)        A(FOUR CHARACTER CPU ID)                     
         ST    R1,ACPUID                                                        
         MVC   P(8),=C'CPU ID: '                                                
         MVC   P+8(4),0(R1)                                                     
*                                                                               
         LA    R1,CPUTABLE         LIST OF CPU IDS                              
INIT06   CLI   0(R1),X'FF'         END OF LIST?                                 
         BE    INIT08              YES -- DON'T ADJUST TCB PERCENTAGE           
         CLC   0(4,R1),P+8         MATCH ON CPU ID?                             
         BE    *+12                YES, R1 = A(CPU ID)                          
         AHI   R1,L'CPUTABLE       BUMP TO NEXT CPU ID                          
         B     INIT06                                                           
         MVC   TCBPCT,4(R1)        SAVE TCB ADJUSTMENT PERCENTAGE               
*                                                                               
INIT08   GOTO1 VPRINTER                                                         
*                                                                               
         BLDL  0,ENTRYPTS          BUILD LIST OF APPC/MVS ENTRY PTS             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                BAD RETURN FROM BLDL MACRO                   
*                                                                               
         LOAD  DE=ATBCFMD                                                       
         ST    R0,ATBCFMD_STRUCTURE                                             
         LOAD  DE=ATBEES3                                                       
         ST    R0,ATBEES3_STRUCTURE                                             
         LOAD  DE=ATBGETC                                                       
         ST    R0,ATBGETC_STRUCTURE                                             
         LOAD  DE=ATBGTA2                                                       
         ST    R0,ATBGTA2_STRUCTURE                                             
         LOAD  DE=ATBRCVW                                                       
         ST    R0,ATBRCVW_STRUCTURE                                             
         LOAD  DE=ATBGTRN                                                       
         ST    R0,ATBGTRN_STRUCTURE                                             
         LOAD  DE=ATBSEND                                                       
         ST    R0,ATBSEND_STRUCTURE                                             
         LOAD  DE=ATBRTRN                                                       
         ST    R0,ATBRTRN_STRUCTURE                                             
         LOAD  DE=ATBAMR1                                                       
         ST    R0,ATBAMR1_STRUCTURE                                             
         LOAD  DE=ATBSERR                                                       
         ST    R0,ATBSERR_STRUCTURE                                             
*                                                                               
         L     RF,AUTL                                                          
         MVI   4(RF),X'0A'                                                      
         GOTO1 VDMGR,DMCB,(0,DMOPEN),CONTROL,CTFLIST,AIO,0                      
*                                                                               
         XC    DMCB(24),DMCB       GET JOB TABLE HEADER FROM TABS               
         MVC   DMCB(4),=AL4(DTJOB)                                              
         OI    DMCB,X'20'                                                       
         GOTO1 VLOCKSPC,DMCB                                                    
         ICM   RF,15,4(R1)                                                      
         JZ    *+2                                                              
         MVC   DSPJTAB,0(RF)       SAVE JOBTAB HEADER FROM DSPACE               
*                                                                               
JOB      USING DMSPACED,DSPJTAB                                                 
         NI    JOB.DSPTFRST,X'3F'                                               
         BRAS  RE,ARSOFF                                                        
         DROP  JOB                                                              
*                                                                               
         XC    DMCB(24),DMCB       GET CLASS TABLE HEADER FROM TABS             
         MVC   DMCB(4),=AL4(DTDCLASS)                                           
         OI    DMCB,X'20'                                                       
         GOTO1 VLOCKSPC,DMCB                                                    
         ICM   RF,15,4(R1)                                                      
         JZ    *+2                                                              
         MVC   DSPCTAB,0(RF)        SAVE JOBTAB HEADER FROM DSPACE              
*                                                                               
CLS      USING DMSPACED,DSPCTAB                                                 
         NI    CLS.DSPTFRST,X'3F'                                               
         BRAS  RE,ARSOFF                                                        
         B     EXITOK                                                           
         DROP  CLS                                                              
*                                                                               
CPUTABLE DS    0CL6                CPU ID TABLE                                 
*                                  FIRST 4 BYTES = CPU ID                       
*                                  NEXT 2 BYTES = TCB %AGE ADJUSTMENT           
*&&UK*&& DC    X'FF'                                                            
*&&US                                                                           
         DC    C'SYF ',AL2(100)                                                 
         DC    C'SYG ',AL2(100)                                                 
         DC    C'SYH ',AL2(100)                                                 
         DC    C'SYI ',AL2(50)                                                  
         DC    C'SYJ ',AL2(50)     ADDED 2/23/98                                
         DC    X'FF'               FROE SAYS SYJ MIGHT GET LESS LATER           
*&&                                                                             
         DROP RB                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD THE PRINT QUEUE TABLE ON THE FIRST CALL ONLY                  *         
***********************************************************************         
BLDPQTAB NTR1  BASE=*,LABEL=*                                                   
         BC    0,EXITOK            *** SELF-MODIFYING CODE ***                  
         MVI   *-3,X'F0'                                                        
*                                                                               
         GOTO1 VDMGR,DMCB,(0,GLIST),PRTQUE,AIO,0,CXREC                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO                                                           
         L     R4,32(R4)           SAVE A(PRINT QUEUE LIST)                     
*                                                                               
         XR    R3,R3                                                            
         IC    R3,0(R4)            NUMBER OF PRINT QUEUES                       
         L     R6,ACITABLE         A(PRINT QUEUE TABLE)                         
*                                                                               
BP10     LA    R4,8(R4)            BUMP TO NEXT PRINT QUEUE                     
         XC    0(CITBLLNQ,R6),0(R6)                                             
         MVC   CFPQENUM,4(R4)      PRINT QUEUE EXTERNAL FILE NUMBER             
         MVC   PRTQUE+4(1),1(R4)   CONSTRUCT C'PRTQ#'                           
         GOTO1 VDMGR,DMCB,(0,BUFF),PRTQUE,0,0,CXREC                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CIDATA,CXREC+12     CIDATA IS HERE                               
         L     RF,VENQDEQ          V(DMENQDEQ)                                  
         ST    RF,CIENQDEQ                                                      
         L     RF,VISGENQ          V(DMISGENQ)                                  
         ST    RF,FIWENQ                                                        
         LA    RF,L'PQINDEX        LENGTH OF PQ KEY                             
         STH   RF,CINDXLN                                                       
         L     RF,CXREC+8          DISPLACEMENT TO PQ SAVE AREA                 
         LA    RF,CXREC(RF)                                                     
         ST    RF,APQSAVE          A(PQ SAVE AREA)                              
         LA    R6,CITBLLNQ(R6)                                                  
         BCT   R3,BP10             LOOK AT ALL PRINT QUEUES                     
*                                                                               
         MVC   P(35),=C'PRTQU    # CI''S    #INDX    TRKS/CI'                   
         GOTO1 VPRINTER                                                         
         MVC   P(35),=C'-----    ------    -----    -------'                    
         GOTO1 VPRINTER                                                         
*                                                                               
         L     R6,ACITABLE                                                      
BP20     MVC   P(5),CFPQID         PRINT QUEUE NAME                             
         EDIT  CICITOT,(5,P+10)    TOTAL # OF PART 1 CONTROL INTERVALS          
         EDIT  CICINDX,(3,P+21)    NUMBER OF INDEX CONTROL INTERVALS            
         EDIT  CITRKS,(3,P+32)     NO. OF TRACKS/CI                             
*                                                                               
         GOTO1 VPRINTER            PRINT INFO FOR ONE PRINT QUEUE               
         AHI   R6,CITBLLNQ                                                      
         CLI   0(R6),X'FF'         ANY MORE PRINT QUEUES?                       
         BNE   BP20                                                             
*                                                                               
         GOTO1 VPRINTER            SKIP A LINE                                  
         B     EXITOK                                                           
         DROP RB                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD A BUNCH OF CORE-RESIDENT (E.G., T00A) PHASES, AND BUILD A LIST *         
* OF THEIR ADDRESSES.  THE ADDRESS OF THIS LIST WILL BE PASSED VIA A  *         
* CONTROL CARD TO MASTER.                                             *         
***********************************************************************         
LOADMODS NTR1  BASE=*,LABEL=*                                                   
         L     R3,ACORETAB         A(CORE-RESIDENT PHASE TABLE)                 
*                                                                               
LM10     CLI   0(R3),X'FF'         END OF TABLE?                                
         BE    EXITOK              YES                                          
*                                                                               
         MVC   DUB,SPACES          PHASE NAMES MUST BE BLANK-PADDED             
         GOTO1 VHEXOUT,DMCB,1(R3),DUB,3,0                                       
         MVI   DUB,C'T'            PHASE NAMES ALL BEGIN WITH C'T'              
*                                                                               
         GOTO1 VLOADER,DMCB,DUB,0,0                                             
         ICM   RF,15,DMCB+4        A(PHASE)                                     
         BZ    LM20                PHASE NOT FOUND                              
         ST    RF,4(R3)            PUT A(PHASE) IN TABLE                        
*                                                                               
         TM    0(R3),X'80'         SHOULD WE GO TO THIS ROUTINE NOW?            
         BZ    LM20                                                             
         SR    R1,R1               YES -- CLEAR R1 SO ROUTINE. . .              
         BASR  RE,RF               . . . KNOWS WE CAME FROM MONSOON             
*                                                                               
LM20     LA    R3,8(R3)            BUMP TO NEXT ENTRY IN TABLE                  
         B     LM10                                                             
         DROP RB                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET A CONVERSATION WITH MONSTER, AND GET PQ INFO FOR SOON REQUEST   *         
***********************************************************************         
GETCONV  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,ATBGETC_STRUCTURE                                             
         BRAS  RE,CALLAPPC         GET A CONVERSATION WITH MONSTER              
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   TRACEFLG,YES                                                     
         BNE   *+8                                                              
         BRAS  RE,PRNTATTB         PRINT CONVERSATION ATTRIBUTES                
*                                                                               
         MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
         LA    R2,ATBRCVW_STRUCTURE                                             
         BRAS  RE,CALLAPPC         RECEIVE AND WAIT                             
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         CLC   STATUS_RECEIVED,ATB_CONFIRM_RECEIVED                             
         BE    *+6                                                              
         DC    H'0'                MONSTER SHOULD HAVE DONE CONFIRM             
*                                                                               
         LA    R2,ATBCFMD_STRUCTURE                                             
         BRAS  RE,CALLAPPC         ISSUE CONFIRMATION                           
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
         LA    R2,ATBRCVW_STRUCTURE                                             
         BRAS  RE,CALLAPPC         RECEIVE AND WAIT                             
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         MVC   OPMS20,SPACES                                                    
*                                                                               
         MVI   TRACEFLG,NO                                                      
         MVI   RECYCLE,NO          ASSUME THIS IS NOT A RECYCLE CALL            
         CLC   =C'RECYCLE',BUFFER                                               
         BNE   GETCNV03                                                         
         MVC   OPMS20(7),=C'RECYCLE'                                            
         MVI   RECYCLE,YES                                                      
         MVI   TRACEFLG,YES                                                     
         B     GETCNV10                                                         
*                                                                               
         PUSH  USING                                                            
         USING MONSD,BUFFER                                                     
GETCNV03 CLC   MNSCMD,=CL8'*HEADER*'                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   USERID,MNSUSER                                                   
         PACK  DUB,MNSPQ#                                                       
         CVB   R0,DUB                                                           
         STCM  R0,3,PQUSERID                                                    
         MVC   PQSUB,MNSPQSUB                                                   
         PACK  DUB,MNSPQSEQ                                                     
         CVB   R0,DUB                                                           
         STCM  R0,3,PQREFNO                                                     
*                                                                               
         MVC   OPMS20+00(07),=CL07'USERID='                                     
         LA    RF,OPMS20+7                                                      
         MVC   0(L'MNSUSER,RF),MNSUSER                                          
         AHI   RF,L'MNSUSER+1                                                   
*                                                                               
         MVC   0(06,RF),=CL06'PQKEY='                                           
         AHI   RF,6                                                             
         MVC   0(L'MNSPQ#,RF),MNSPQ#                                            
         AHI   RF,L'MNSPQ#                                                      
         MVI   0(RF),C','                                                       
         MVC   1(L'MNSPQSUB,RF),MNSPQSUB                                        
         AHI   RF,L'MNSPQSUB+1                                                  
         MVI   0(RF),C','                                                       
         MVC   1(L'MNSPQSEQ,RF),MNSPQSEQ                                        
*                                                                               
         GOTO1 VHEXIN,DMCB,MNSRTIME,PQTIME,6,0                                  
         GOTO1 VHEXIN,DMCB,MNSAJOB,AJOBNTRY,8,0                                 
*                                                                               
         MVC   PQNUM,MNSPRTQ                                                    
         MVC   PQPRTY,MNSPRI                                                    
         MVC   PQCLAS,MNSCLS                                                    
         MVC   PQFACPAK,MNSFAC                                                  
         MVC   TRACEFLG,MNSTRC                                                  
         MVC   JESMSG,MNSJES                                                    
*&&US*&& MVC   QTYPE,MNSQTYPE                                                   
*                                                                               
*AT THIS POINT, IT IS TOO LATE FOR SETTING DDSIO FOR MONSOON                    
*WILL GO BACK TO FIX IT VIA READING SOME PARMS, 8/12/04, YYUN                   
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),MNSDDSIO    DDSIO USED BY MONSOON                        
         MVC   DDSIOMOD,MNSDDSIO   DDSIO USED BY ATTACHED CONTROLLER            
*                                                                               
         CLC   =C'NONE',MNSREQT                                                 
         BNE   *+14                                                             
         MVC   REQTIMER,=C'NONE'                                                
         B     GETCNV05                                                         
*                                                                               
         PACK  DUB,MNSREQT                                                      
         CVB   R0,DUB                                                           
         MHI   R0,6000             CONVERT TO HUNDREDTHS/SECOND                 
         ST    R0,REQTIMER                                                      
*                                                                               
GETCNV05 MVC   DSPACE,MNSDSPC      PASS DSPACE CARD TO MASTER                   
         MVC   DUMPWARN,MNSDMPW                                                 
*                                                                               
         CLC   MNSMONS,SPACES      SPECIFIC MONSOON ID PASSED THROUGH?          
         BNH   *+10                NO                                           
         MVC   MNSOONID,MNSMONS                                                 
         GOTO1 VHEXIN,DMCB,MNSTCBT,MAXTCBTM,8,0                                 
         GOTO1 (RF),(R1),MNSET,FULL,8,0                                         
         ICM   R0,15,FULL          ET LIMIT IN SEC                              
         MHI   R0,100                                                           
         ST    R0,MAXETTM          ET LIMIT IN 0.01 SEC                         
         MVC   LIMITFLG,MNSLIM     SET LIMIT FLAG                               
*                                                                               
         CLI   TRACEFLG,YES                                                     
         BNE   GETCNV10                                                         
*                                                                               
         L     R2,RECEIVE_LENGTH                                                
         GOTO1 VPRNTBL,DMCB,=C'HEADER RECORD RECEIVED',                +        
               BUFFER,C'DUMP',(R2),=C'1D'                                       
*                                                                               
GETCNV10 LA    R2,ATBCFMD_STRUCTURE                                             
         BRAS  RE,CALLAPPC         ISSUE CONFIRMATION                           
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         XC    BUFFER,BUFFER                                                    
         MVC   SEND_TYPE,ATB_SEND_AND_CONFIRM                                   
         MVC   BUFFER(L'JOBID),JOBID                                            
         MVC   BUFFER+9(L'THISJID),THISJID                                      
         MVC   BUFFER+18(L'JOBINAME),JOBINAME                                   
         MVC   BUFFER+27(L'JOBIASID),JOBIASID                                   
         MVC   BUFFER+32(L'OPMS20),OPMS20                                       
         LA    R5,L'OPMS20+L'JOBID+L'THISJID+L'JOBINAME+L'JOBIASID+4            
         BRAS  RE,LINXMT                                                        
         B     EXIT                                                             
         POP   USING                                                            
         DROP RB                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ON ENTRY, R5 = LENGTH OF DATA                                       *         
***********************************************************************         
LINXMT   NTR1  BASE=*,LABEL=*                                                   
         ST    R5,SEND_LENGTH                                                   
*                                                                               
         LA    RE,=C'BUFFER DATA'                                               
         ST    RE,DMCB                                                          
         MVI   DMCB,11                                                          
         CLC   SEND_TYPE,ATB_BUFFER_DATA                                        
         BE    LINX10                                                           
         LA    RE,=C'SEND AND CONFIRM'                                          
         ST    RE,DMCB                                                          
         MVI   DMCB,16                                                          
         CLC   SEND_TYPE,ATB_SEND_AND_CONFIRM                                   
         BE    LINX10                                                           
         LA    RE,=C'SEND AND PREPARE TO RECEIVE'                               
         ST    RE,DMCB                                                          
         MVI   DMCB,27                                                          
         CLC   SEND_TYPE,ATB_SEND_AND_PREP_TO_RECEIVE                           
         BE    LINX10                                                           
         DC    H'0'                UNKNOWN SEND TYPE                            
*                                                                               
LINX10   CLI   TRACEFLG,YES                                                     
         BNE   LINX20                                                           
*                                                                               
         L     R4,ATBSEND_STRUCTURE+40       A(BUFFER/BIGBUF)                   
         GOTO1 VPRNTBL,DMCB,,(R4),C'DUMP',(R5),=C'1D'                           
*                                                                               
LINX20   LA    R2,ATBSEND_STRUCTURE                                             
         BRAS  RE,CALLAPPC                                                      
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    EXITOK              RETURN CODE WAS OK                           
         DC    H'0'                                                             
         DROP RB                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PRINT DETAILS ABOUT THE CURRENT STATE OF THE LU6.2 CONVERSATION     *         
***********************************************************************         
PRNTATTB NTR1  BASE=*,LABEL=*                                                   
         MVC   P(17),=CL17'*****************'                                   
         BRAS  RE,PRNT                                                          
         MVC   P(17),=CL17'GETATTRIBUTES'                                       
         BRAS  RE,PRNT                                                          
         MVC   P(17),=CL17'*****************'                                   
         BRAS  RE,PRNT                                                          
*                                                                               
         LA    R2,ATBGTA2_STRUCTURE                                             
         BRAS  RE,CALLAPPC         GET CONVERSATION ATTRIBUTES                  
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+6                 RETURN CODE WAS OK                           
         DC    H'0'                                                             
*                                                                               
         MVC   P(17),=CL17'CONVERSATION_ID'                                     
         GOTO1 VHEXOUT,DMCB,CONVERSATION_ID,P+30,8,=C'TOG'                      
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   P(17),=CL17'PARTNER_LU_NAME'                                     
         MVC   P+30(17),PARTNER_LU_NAME                                         
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   P(17),=CL17'MODE_NAME'                                           
         MVC   P+30(8),MODE_NAME                                                
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   P(17),=CL17'SYNC_LEVEL'                                          
         MVC   P+30(4),=C'NONE'                                                 
         CLC   SYNC_LEVEL,ATB_NONE                                              
         BE    PRATT02                                                          
         MVC   P+30(7),=C'CONFIRM'                                              
         CLC   SYNC_LEVEL,ATB_CONFIRM                                           
         BE    PRATT02                                                          
         DC    H'0'                UNKNOWN SYNC LEVEL                           
*                                                                               
PRATT02  BRAS  RE,PRNT                                                          
*                                                                               
         MVC   P(17),=CL17'TP_NAME_LENGTH'                                      
         EDIT  TP_NAME_LENGTH,(5,P+30),ALIGN=LEFT,ZERO=NOBLANK                  
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   P(17),=CL17'TP_NAME'                                             
         MVC   P+30(64),TP_NAME                                                 
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   P(17),=CL17'LOCAL_LU_NAME'                                       
         MVC   P+30(8),LOCAL_LU_NAME                                            
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   P(17),=CL17'CONVERSATION_TYPE'                                   
         MVC   P+30(5),=C'BASIC'                                                
         CLC   CONVERSATION_TYPE,ATB_BASIC_CONVERSATION                         
         BE    PRATT04                                                          
         MVC   P+30(6),=C'MAPPED'                                               
         CLC   CONVERSATION_TYPE,ATB_MAPPED_CONVERSATION                        
         BE    PRATT04                                                          
         DC    H'0'                UNKNOWN CONVERSATION TYPE                    
*                                                                               
PRATT04  BRAS  RE,PRNT                                                          
*                                                                               
         MVC   P(17),=CL17'USER_ID'                                             
         MVC   P+30(10),USER_ID                                                 
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   P(17),=CL17'PROFILE'                                             
         MVC   P+30(10),PROFILE                                                 
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   P(17),=CL17'USER_TOKEN'                                          
         MVC   P+30(80),USER_TOKEN                                              
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   P(17),=CL17'CONVERSATION_STATE'                                  
         MVC   P+30(4),=C'SEND'                                                 
         CLC   CONVERSATION_STATE,ATB_SEND_STATE                                
         BE    PRATT06                                                          
         MVC   P+30(7),=C'RECEIVE'                                              
         CLC   CONVERSATION_STATE,ATB_RECEIVE_STATE                             
         BE    PRATT06                                                          
         MVC   P+30(7),=C'CONFIRM'                                              
         CLC   CONVERSATION_STATE,ATB_CONFIRM_STATE                             
         BE    PRATT06                                                          
         MVC   P+30(10),=C'INITIALIZE'                                          
         CLC   CONVERSATION_STATE,ATB_INITIALIZE_STATE                          
         BE    PRATT06                                                          
         MVC   P+30(12),=C'SEND PENDING'                                        
         CLC   CONVERSATION_STATE,ATB_SEND_PENDING_STATE                        
         BE    PRATT06                                                          
         MVC   P+30(12),=C'CONFIRM SEND'                                        
         CLC   CONVERSATION_STATE,ATB_CONFIRM_SEND_STATE                        
         BE    PRATT06                                                          
         MVC   P+30(18),=C'CONFIRM DEALLOCATE'                                  
         CLC   CONVERSATION_STATE,ATB_CONFIRM_DEALLOCATE_STATE                  
         BE    PRATT06                                                          
         DC    H'0'                UNKNOWN CONVERSATION STATE                   
*                                                                               
PRATT06  BRAS  RE,PRNT                                                          
         MVC   P(17),=CL17'*****************'                                   
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
         DROP RB                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPON ENTRY,  R2 POINTS TO PARAMETER STRUCTURE                       *         
* UPON RETURN, RETURN_CODE CONTAINS THE APPC/MVS RETURN CODE          *         
***********************************************************************         
CALLAPPC NTR1  BASE=*,LABEL=*                                                   
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_ECB                                  
         TM    20(R2),X'80'                                                     
         BZ    *+10                                                             
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
*                                                                               
         MVC   P(16),=C'ABOUT TO PERFORM'                                       
         MVC   P+30(16),4(R2)      PRINT ROUTINE NAME                           
         BRAS  RE,PRNT                                                          
*                                                                               
         SAM31                                                                  
         L     RF,0(R2)            RF = A(APPC/MVS ROUTINE)                     
         LA    R3,24(R2)           R3 = A(PARAMETER LIST)                       
         CALL  (15),MF=(E,(R3))                                                 
         SAM24                                                                  
*                                                                               
         TM    20(R2),X'80'        SYNCHRONOUS CALL?                            
         BO    CALL20              YES, SO IT'S DONE                            
*                                                                               
         CLC   RETURN_CODE,ATB_OK                                               
         BE    CALL10              APPC/MVS CALL WAS ACCEPTED                   
*                                                                               
         CLI   TRACEFLG,YES                                                     
         BNE   CALL20                                                           
         MVC   P(17),=CL17'**BAD_APPC_CALL**'                                   
         MVC   P+30(16),4(R2)      PRINT NAME OF APPC/MVS ROUTINE               
         MVC   P+50(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+64),ALIGN=LEFT                                  
         BRAS  RE,PRNT                                                          
         B     CALL20              APPC/MVS CALL NOT ACCEPTED                   
*                                                                               
CALL10   WAIT  1,ECB=APPCECB       WAIT FOR COMPLETION                          
*                                                                               
         MVC   RETURN_CODE,APPCECB                                              
         MVI   RETURN_CODE,0       CLEAR HIGH-ORDER BYTE                        
         XC    APPCECB,APPCECB                                                  
*                                                                               
CALL20   CLI   TRACEFLG,YES        '*' MEANS IT'S AN APPC/MVS CALL              
         BNE   CALL22                                                           
*                                                                               
         MVI   P,C'*'              '*' MEANS IT'S AN APPC/MVS CALL              
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODE           
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         BRAS  RE,PRNT                                                          
*                                                                               
CALL22   OC    RETURN_CODE,RETURN_CODE                                          
         BZ    CALLX                                                            
         TM    20(R2),X'40'        CAN CALL ERROR_EXTRACT NOW?                  
         BZ    CALLX                                                            
         BRAS  RE,APPCERR          YES                                          
*                                                                               
CALLX    B     EXITOK                                                           
         DROP RB                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PERFORM APPC ERROR_EXTRACT FUNCTION.                                          
***********************************************************************         
APPCERR  NTR1  BASE=*,LABEL=*                                                   
         SAM31                                                                  
         LA    R2,ATBEES3_STRUCTURE                                             
         L     RF,0(R2)            RF = A(APPC/MVS ROUTINE)                     
         LA    R3,24(R2)           R3 = A(PARAMETER LIST)                       
         CALL  (15),MF=(E,(R3))                                                 
         SAM24                                                                  
*                                                                               
         CLI   TRACEFLG,YES                                                     
         BNE   APPCE02                                                          
         MVI   P,C'*'              '*' MEANS IT'S AN APPC/MVS CALL              
         MVC   P+1(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODE           
         MVC   P+30(14),=C'RETURN CODE = '                                      
         L     R0,RETURN_CODE_ERROR_EXTRACT                                     
         EDIT  (R0),(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                            
         BRAS  RE,PRNT                                                          
*                                                                               
APPCE02  ICM   R0,15,RETURN_CODE_ERROR_EXTRACT                                  
         BNZ   EXITOK              BAD RETURN CODE, SO NO ERROR INFO            
*                                                                               
         CLI   TRACEFLG,YES                                                     
         BNE   APPCE04                                                          
         MVC   P(17),=CL17'ERROR_EXTRACT'                                       
         MVC   P+30(37),=C'APPC SERVICE XXXXXXXX, REASON_CODE = '               
         MVC   P+43(8),SERVICE_NAME                                             
         L     R0,SERVICE_REASON_CODE                                           
         EDIT  (R0),(5,P+67),ALIGN=LEFT,ZERO=NOBLANK                            
         BRAS  RE,PRNT                                                          
*                                                                               
APPCE04  ICM   R3,15,MESSAGE_TEXT_LENGTH                                        
         BZ    APPCE08             NOTHING IN FIELD                             
         LA    R4,MESSAGE_TEXT                                                  
*                                                                               
APPCE06  LR    R1,R3                                                            
         CHI   R1,132                                                           
         BL    *+8                                                              
         LHI   R1,132                                                           
         BCTR  R1,0                                                             
         EX    R1,APPCMVC                                                       
         GOTO1 VPRINTER                                                         
         AHI   R1,1                                                             
         AR    R4,R1                                                            
         SR    R3,R1                                                            
         BP    APPCE06                                                          
*                                                                               
APPCE08  ICM   R3,15,ERROR_LOG_INFORMATION_LENGTH                               
         BZ    EXITOK                                                           
         LA    R4,ERROR_LOG_INFORMATION                                         
*                                                                               
APPCE10  LR    R1,R3                                                            
         CHI   R1,132                                                           
         BL    *+8                                                              
         LHI   R1,132                                                           
         BCTR  R1,0                                                             
         EX    R1,APPCMVC                                                       
         GOTO1 VPRINTER                                                         
         AHI   R1,1                                                             
         AR    R4,R1                                                            
         SR    R3,R1                                                            
         BP    APPCE10                                                          
         B     EXITOK                                                           
*                                                                               
APPCMVC  MVC   P(0),0(R4)                                                       
*                                                                               
APPCEX   B     EXITOK                                                           
         DROP RB                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* READ THE PRINT QUEUE FOR THE REQUEST.                               *         
* TURN ON THE JOB FLAG AND ATTACH IT.                                 *         
***********************************************************************         
ATTACH   NTR1  BASE=*,LABEL=*                                                   
         LR    R8,RB                                                            
         AHI   R8,4096                                                          
         USING ATTACH+4096,R8                                                   
         XC    AVOIDLDS,AVOIDLDS   CLEAR LOAD COUNTERS                          
         XC    TOTALLDS,TOTALLDS                                                
         MVI   ABENDFLG,NO         ASSUME TASK DID NOT ABEND                    
         MVI   TMOUTFLG,NO         ASSUME MONSOON DIDN'T TIME OUT               
         MVI   ETTMFLG,NO          ASSUME ET TIME NOT EXCESS                    
         MVI   TCBTMFLG,NO         ASSUME TCB TIME NOT EXCESS                   
         MVI   ATTCHFLG,NO         ASSUME JOB NOT ATTACH YET                    
         MVC   ERRMSG,SPACES                                                    
         MVI   UPDFLG,NO                                                        
         MVI   COMSCORE,0          NOT A COMSCORE REQUEST                       
*                                                                               
         CLI   RECYCLE,YES         JUST A RECYCLE CALL FROM MONSTER             
         BE    ATTX                DON'T ATTACH ANYTHING                        
*                                                                               
         XC    TOTETTM,TOTETTM                                                  
         XC    TOTTCBTM,TOTTCBTM                                                
*                                                                               
         GOTO1 VSHMUSS,DMCB,(0,=CL8'ATTACH'),(0,=CL8'PRTQ'),0,0                 
         ICM   R1,15,DMCB+8                                                     
         JZ    *+2                                                              
         STCM  R1,15,FIWSHA        Get address of shared memory table           
*                                                                               
         L     R6,ACITABLE         FIND PQ TABLE ENTRY                          
ATT02    CLC   PQNUM,CFPQID+4                                                   
         BE    ATT03                                                            
         AHI   R6,CITBLLNQ                                                      
         CLI   0(R6),X'FF'                                                      
         BNE   ATT02                                                            
         DC    H'0'                INVALID PQ NUMBER                            
*                                                                               
ATT03    MVC   OPMS1,SPACES        FORMAT THE OPERATOR MESSAGE                  
         MVC   OPMS1+8(8),USERID                                                
         MVI   OPMS1+16,C','                                                    
         MVI   OPMS1+20,C','                                                    
         EDIT  PQREFNO,(5,OPMS1+21)                                             
*                                                                               
         SAM31                                                                  
         MVC   FIWRES,SPACES                                                    
         MVC   FIWRES(L'CFPQID),CFPQID                                          
         BRAS  RE,FIRSET           Set values for this print queue              
*                                                                               
         XC    FIWREF,FIWREF                                                    
         MVC   FIWREF+2(2),PQREFNO                                              
         BRAS  RE,FIRRN                                                         
         BRAS  RE,FIRN1            IS THIS A VALID REPORT NUMBER                
         JNE   *+2                                                              
         BRAS  RE,FIRRC            REPORT NUMBER TO CI ADDRESS                  
         MVC   CIADDR,FIWCIA                                                    
*                                                                               
         BRAS  RE,FIRSLOCK         SHARED LOCK FOR THE PRINT QUEUE              
         BRAS  RE,FIRRLOCK         EXCLUSIVE LOCK FOR THE REPORT                
*                                                                               
         L     R4,FIWNDA                                                        
         LA    R4,SI1NDX-SI1PAR(R4) R4=A(INDEX)                                 
*                                                                               
         USING PQRECD,R4                                                        
         CLC   PQUSERID,PQSRCID    IS IT THE SAME REPORT?                       
         BNE   ATT06                                                            
         CLC   PQREFNO,PQREPNO                                                  
         BNE   ATT06                                                            
*                                                                               
*??      CLC   PQTIME,PQAGELT                                                   
*??      BNE   ATT06               NO  -- IT'S BEEN PURGED                      
*                                                                               
         TM    PQATTB,PQATJOBI     DOES REPORT STILL CONTAIN JCL?               
         BZ    ATT06               NOT ANY MORE -- IT COMPLETED                 
         TM    PQATTB,PQATJOBO     IS THE REPORT CURRENTLY RUNNING?             
         BZ    ATT08               NO                                           
*                                                                               
ATT06    BRAS  RE,FIRRUNLK         UNLOCK THE REPORT                            
         BRAS  RE,FIRSUNLK                                                      
         SAM24                                                                  
*                                                                               
         MVC   ATTMS1+16(18),OPMS1+8 UUUUUUUU,XXX,NNNNN                         
         WTO   TEXT=ATTMS1H                                                     
*                                                                               
         BRAS  RE,ARSOFF           FLAG JOB AS BAD                              
         BRAS  RE,LOCKJOB                                                       
*                                                                               
         L     RF,VSSB                                                          
         LAM   AR3,AR3,SSOTBLET-SSOOFF(RF)                                      
         ICM   R3,15,AJOBNTRY                                                   
         BZ    ATT90                                                            
         SAC   512                                                              
         USING TBJOBTAB,R3                                                      
         CLC   PQREPKEY,TBJPQKEY   IS IT THE SAME REPORT?                       
         BNE   *+8                 If not the right one don't flag              
         MVI   TBJSTAT,TBJERROR                                                 
         B     ATT90                                                            
         DROP  R3                                                               
*                                                                               
ATT08    OI    PQATTB,PQATJOBO     TURN ON 'JOB OUTPUT' FLAG                    
         MVC   SVPQTYPE,PQTYPE     SAVE THE PQTYPE                              
*                                                                               
         SAM24                                                                  
         MVC   CIADDR,FIWCIA                                                    
         CLC   CIADDR,=X'00010100'                                              
         JE    *+2                                                              
         GOTO1 VDMGR,DMCB,(0,DMREAD),CFPQID,CIADDR,CXREC                        
*                                                                               
         LA    R4,CXREC            A(FIRST CONTROL INTERVAL)                    
         OI    PQATTB,PQATJOBO     TURN ON 'JOB OUTPUT' FLAG                    
*                                                                               
         GOTO1 VDMGR,DMCB,(0,DMWRT),CFPQID,CIADDR,CXREC                         
         CLI   DMCB+8,0            ANY ERROR ON THE DMWRT?                      
         JNE   *+2                 NO                                           
*                                                                               
         SAM31                                                                  
         BRAS  RE,FIRRUNLK         UNLOCK THE REPORT                            
         BRAS  RE,FIRSUNLK         REMOVE THE SHARED LOCK                       
         SAM24                                                                  
                                                                                
*----------------------------------------------------------------------         
* MVC "ATTCH" TO TBJMONS, SO RCVR JOB WON'T DELETE THIS JOBTAB ENTRY            
*----------------------------------------------------------------------         
         BRAS  RE,ARSOFF                                                        
         BRAS  RE,LOCKJOB                                                       
         L     RF,VSSB                                                          
         LAM   AR3,AR3,SSOTBLET-SSOOFF(RF)                                      
         ICM   R3,15,AJOBNTRY                                                   
         BZ    ATT11                                                            
         SAC   512                                                              
         USING TBJOBTAB,R3                                                      
         CLC   PQREPKEY,TBJPQKEY   IS IT THE SAME REPORT?                       
         BNE   *+10                                                             
         MVC   TBJMONS,=CL7'ATTACH'                                             
*                                                                               
ATT11    BRAS  RE,ARSOFF                                                        
         BRAS  RE,UNLKJOB                                                       
         DROP  R3                                                               
                                                                                
*----------------------------------------------------------------------         
* WRITE JCL TO DATASPACE BUFFER                                                 
*----------------------------------------------------------------------         
*&&UK*&& BRAS  RE,JCLTODSP         KEEP THIS FOR RERUNS                         
                                                                                
*----------------------------------------------------------------------         
* BY THIS POINT, THE PRINT QUEUE HAS BEEN DEQUEUED                              
*----------------------------------------------------------------------         
         MVC   OPMS1+17(3),PQSUB   NOW WE KNOW THE SUB-ID                       
*                                                                               
         SAM31 ,                                                                
         XC    FIWREF,FIWREF                                                    
         MVC   FIWREF+2(2),PQREFNO                                              
         BRAS  RE,FIRRN                                                         
         BRAS  RE,FIRN1            IS THIS A VALID REPORT NUMBER                
         JNE   *+2                                                              
         BRAS  RE,FIRRC            REPORT NUMBER TO CI ADDRESS                  
         MVC   CIADDR,FIWCIA                                                    
         SAM24 ,                                                                
*                                                                               
         L     RF,APQSAVE                                                       
         USING SKBUFFD,RF                                                       
         XC    SKBCTRL,SKBCTRL     START READING FROM SCRATCH                   
         XC    SKFCTRL,SKFCTRL                                                  
         XC    SKXCTRL,SKXCTRL                                                  
         MVC   SKLABEL,=C'*PQSAVE*'                                             
         MVC   SKINTNO,CFPQINUM    INTERNAL FILE NUMBER                         
         MVC   SKEXTNO,CFPQENUM    EXTERNAL FILE NUMBER                         
         MVC   SKFSTCI,CIADDR                                                   
         DROP  RF                                                               
*                                                                               
         BRAS  RE,WRTJCLFL         WRITE JCL FILE                               
ATT12    BRAS  RE,EXTJCLFL         EXTRACT DATA (TMPFILE -> JCLBUF)             
         BNE   ATT14               BAD JCL                                      
         BRAS  RE,GETJCL           EXTRACT DATA (JCLBUF -> SYSIN)               
         BE    ATT16               JCL IS OK                                    
*                                                                               
ATT14    MVC   OPMS1(7),=C'BAD*JCL'                                             
         WTO   TEXT=OPMS1H                                                      
         MVC   P+11(L'OPMS1),OPMS1                                              
         GOTO1 VPRINTER                                                         
         B     ATT66               JCL ERROR                                    
*                                                                               
ATT16    L     R0,AJCLBUFF        CLEAR JCLBUF FOR 2ND STEP INPUT               
         LHI   R1,JCLBUFLQ                                                      
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
***********************************************************************         
******CODE TO TRACK MEMORY LEAK IF ANY*********************************         
***********************************************************************         
*&&DO                                                                           
         OC    MEMORY0,MEMORY0                                                  
         BNZ   MEM0X                                                            
         MVC   P+25(13),=C'Initially,   '                     ***               
         MVC   P+44(12),=C'MEMORY LEFT='                      ***               
         GOTO1 VCOVAIL,DMCB,C'LOOK'                           ***               
         CLC   DMCB+4,=F'0'                                   ***               
         BNE   *+6                                            ***               
         DC    H'0'                                           ***               
         L     RF,DMCB+8                                      ***               
         ST    RF,MEMORY0                                     ***               
         EDIT  (RF),(8,P+56),ZERO=NOBLANK,FILL=0              ***               
         GOTO1 VPRINTER                                       ***               
MEM0X    DS    0H                                                               
***********************************************************************         
******CODE TO TRACK MEMORY LEAK IF ANY*********************************         
***********************************************************************         
*                                                                               
***********************************************************************         
******CODE TO TRACK MEMORY LEAK IF ANY*********************************         
***********************************************************************         
         MVC   P+25(13),=C'BEFORE ATTACH'                     ***               
         MVC   P+44(12),=C'MEMORY LEFT='                      ***               
         GOTO1 VCOVAIL,DMCB,C'LOOK'                           ***               
         CLC   DMCB+4,=F'0'                                   ***               
         BNE   *+6                                            ***               
         DC    H'0'                                           ***               
         L     RF,DMCB+8                                      ***               
         ST    RF,MEMORY1                                     ***               
         EDIT  (RF),(8,P+56),ZERO=NOBLANK,FILL=0              ***               
         MVC   P+72(8),=C'MEMORY0='                           ***               
         L     RF,MEMORY0                                     ***               
         EDIT  (RF),(8,P+80),ZERO=NOBLANK,FILL=0              ***               
         GOTO1 VPRINTER                                       ***               
*&&                                                                             
***********************************************************************         
******CODE TO TRACK MEMORY LEAK IF ANY*********************************         
***********************************************************************         
*                                                                               
*                                                                               
         MVC   P(17),=CL17'ATTACH'                                              
         MVC   P+30(12),=C'REPORT ID:  '                                        
         MVC   P+42(18),OPMS1+8    UUUUUUUU,XXX,NNNNN                           
         MVC   P+65(8),ATTCHPGM    NAME OF ATTACHED PROGRAM                     
         MVC   P+75(10),=C'DIRECT ID='                                          
         MVC   P+85(18),DIRECTID   DIRECT CARD ID=                              
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   OPMS1(7),=C'RUNNING'                                             
         CLI   JESMSG,YES          ARE WE DISPLAYING CONSOLE MESSAGES?          
         BNE   ATT18               NO                                           
         WTO   TEXT=OPMS1H                                                      
*                                                                               
ATT18    L     R1,AASCB            A(ASCB)                                      
         L     R0,ASCBEJST-ASCB(R1)                                             
         ST    R0,STRTTMTC                                                      
*                                                                               
         TIME  BIN                                                              
         ST    R0,STRTTMRL         STARTING EXECUTION TIME                      
*                                                                               
ATT20    BRAS  RE,ARSOFF                                                        
         BRAS  RE,LOCKJOB                                                       
         L     RF,VSSB                                                          
         LAM   AR3,AR3,SSOTBLET-SSOOFF(RF)                                      
         ICM   R3,15,AJOBNTRY      GET A(JOBTAB HEADER)                         
         BZ    ATT22                                                            
         SAC   512                                                              
         USING TBJOBTAB,R3                                                      
         CLC   TBJPQKEY,PQKEY      IS IT THE SAME REPORT?                       
         BE    ATT28                                                            
*                                                                               
ATT22    BRAS  RE,ARSOFF           NOT FOUND - SEND AN EMAIL TO TEAM            
         BRAS  RE,UNLKJOB                                                       
         XC    WORK,WORK                                                        
*&&US*&& MVC   WORK(27),=CL27'AUTONOTE*US-MF_FAC_NOTIFY: '                      
*&&UK*&& MVC   WORK(27),=CL27'AUTONOTE*UK-MF_FAC_NOTIFY: '                      
         MVC   WORK+27(10),=CL10'REPORT='                                       
         MVC   WORK+37(18),OPMS1+8    UUUUUUUU,XXX,NNNNN                        
         MVC   WORK+55(21),=CL21' NOT FOUND IN DSPACE!'                         
         GOTO1 VDMGR,DMCB,=C'OPMSG',(76,WORK)                                   
         B     ATT30                                                            
*                                                                               
ATT28    EQU   *                                                                
*        MVC   TBJMONS,MNSOONID    SET MONSOON NAME                             
         MVC   TBJMONS(3),JOBINAME+5  LAST 3 CHAR IN JOBNAME (MONS#???)         
         MVC   TBJMONS+3(4),JOBIASID  4 DIGIT ASID                              
         XC    SUBTIME,SUBTIME                                                  
         MVC   SUBTIME+1(3),TBJSTIME                                            
         L     R1,STRTTMRL                                                      
         STCM  R1,7,TBJRTIME       SET RUN TIME                                 
         MVC   REQSPP,TBJMVSID+4                                                
         MVC   MVSID,TBJMVSID                                                   
         BRAS  RE,ARSOFF                                                        
         BRAS  RE,UNLKJOB                                                       
         DROP  R3                                                               
*                                                                               
ATT30    MVI   WORK,C'A'           ATTACH CALL                                  
         MVC   WORK+1(8),OPMS1+8                                                
         MVC   WORK+9(3),OPMS1+17                                               
         MVC   WORK+12(5),OPMS1+21                                              
         MVC   WORK+17(8),ATTCHPGM                                              
         LA    R1,WORK                                                          
         BRAS  RE,EVENT            REPORT ON THIS EVENT                         
*                                                                               
         BRAS  RE,ARSOFF                                                        
         XC    WORK,WORK                                                        
         MVI   WORK+1,10           L'PARAMS WITHOUT LENGTH ITSELF               
         BRAS  RE,CONVPQT                                                       
         MVC   WORK+2(3),DUB       SUBMIT TIME                                  
         MVC   WORK+5(1),CFPQINUM  PRINT QUEUE NUMBER                           
         MVC   WORK+6(2),PQCLAS    CLASS                                        
*                                                                               
         L     R1,AASCB            A(ASCB)                                      
         MVC   WORK+8(2),ASCBASID-ASCB(R1)  ASID                                
         MVC   WORK+10(1),PQPRTY   PRIORITY                                     
         MVC   WORK+11(1),PQFACPAK FACPAK ID                                    
*                                                                               
*        LA    R1,WORK             R1 = A(PARAMETERS TO SVC)                    
*        LA    R0,14                                                            
*        LNR   R0,R0               R0 = -14                                     
*        SVC   247                 FOR JOB ACCOUNTING                           
*                                                                               
         XC    AVOIDLD,AVOIDLD     CLEAR LOAD COUNTERS IN MONSPARM              
         XC    TOTALLD,TOTALLD                                                  
*                                                                               
         GOTO1 VISGENQ,DMCB,C'TASK',0                                           
*                                                                               
         LA    R3,ATTCHPGM                                                      
         XC    TASKECB,TASKECB                                                  
*&&US*&& ATTACH EPLOC=(R3),ECB=TASKECB,SZERO=NO                                 
*&&UK*&& ATTACH EPLOC=(R3),ECB=TASKECB,SZERO=NO,ALCOPY=YES                      
         ST    R1,TASKTCB          SAVE TCB                                     
         OC    TASKTCB,TASKTCB                                                  
         BNZ   *+6                                                              
         DC    H'0'                IF TCB IS ZERO, THEN ATTACH FAILED           
         MVI   ATTCHFLG,YES        REMEMBER WE ATTACHED THE JOB                 
*                                                                               
ATT32    XC    TIMERECB,TIMERECB                                                
         CLC   REQTIMER,=C'NONE'                                                
         BE    ATT34               DON'T SET THE TIMER                          
         STIMERM SET,ID=STIMERID,BINTVL=REQTIMER,EXIT=TIMERXIT                  
         LTR   RF,RF                                                            
         BZ    ATT34                                                            
         DC    H'0'                                                             
*                                                                               
ATT34    WAIT  1,ECBLIST=ECBLST    WAIT FOR REQ COMPLETION OR OPERATOR          
*                                                                               
         TM    TIMERECB,X'40'      DID THE TIMER POP?                           
         BZ    ATT36                                                            
*                                                                               
         CLI   UPDATIVE,YES        YES - UPDATIVE SOON?                         
         BNE   *+12                                                             
         BRAS  RE,PUTWARN          YES - PUT WARNING MESSAGE TO CONSOLE         
         B     ATT32               START TIMER AGAIN, THEN WAIT                 
*                                                                               
         BRAS  RE,CHKKILL          CHECK FOR KILL REQUEST                       
         BL    ATT35A                                                           
*                                                                               
         L     R1,AASCB            A(ASCB)                                      
         L     R0,ASCBEJST-ASCB(R1)                                             
         S     R0,STRTTMTC         R0 = ELAPSED TCB TIME                        
         ST    R0,WORK+4           SET CPU FOR EVEN CALL                        
         AHI   R0,-2               ALLOW TWO SECOND GRACE PERIOD                
         CH    R0,TCBTIME          HAS THE JOB TIMED OUT?                       
         BH    ATT35                                                            
         BRAS  RE,PUTWARN          NO - PUT WARNING MESSAGE TO CONSOLE          
*                                                                               
         MVI   WORK,C'P'           TIMER POP EVENT                              
         LA    R1,WORK                                                          
         BRAS  RE,EVENT                                                         
         BRAS  RE,ARSOFF                                                        
         B     ATT32               START TIMER AGAIN, THEN WAIT                 
*                                                                               
ATT35    MVC   ATTMS2+16(18),OPMS1+8 UUUUUUUU,XXX,NNNNN                         
         WTO   TEXT=ATTMS2H                                                     
         MVC   P+11(L'ATTMS2),ATTMS2                                            
         GOTO1 VPRINTER                                                         
         MVI   TMOUTFLG,YES        TIMEOUT DUE TO TIMER                         
         B     ATT38                                                            
*                                                                               
ATT35A   MVC   ATTMS4+16(18),OPMS1+8 UUUUUUUU,XXX,NNNNN                         
         WTO   TEXT=ATTMS4H                                                     
         MVC   P+11(L'ATTMS4),ATTMS4                                            
         GOTO1 VPRINTER                                                         
         MVI   TMOUTFLG,YES        TIMEOUT DUE TO CANCEL                        
         B     ATT38                                                            
*                                                                               
ATT36    TM    TASKECB,X'40'       DID THE REQUEST COMPLETE?                    
         BO    *+6                                                              
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
*                                                                               
*&&UK*&& BRAS  RE,NOKILL           REMOVE ANY KILL IF COMPLETED OK              
*                                                                               
         CLC   REQTIMER,=C'NONE'   YES -- WAS TIMER SET?                        
         BE    ATT38               NO, SO DON'T CANCEL                          
         STIMERM CANCEL,ID=STIMERID                                             
         LTR   RF,RF                                                            
         BZ    ATT38                                                            
         DC    H'0'                UNSUCCESSFUL TIMER CANCEL                    
*                                                                               
ATT38    MVC   OPMS1(7),=C'ENDED  '                                             
*                                                                               
         BRAS  RE,ARSOFF                                                        
         SAM31                                                                  
         L     R0,AJTCB           SAVE COPY OF JOBSTEP TCB                      
         LHI   R1,TCBMNLEN                                                      
         L     RE,TASKTCB                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         DETACH TASKTCB            DETACH THE TASK                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL DETACH                          
*                                                                               
         MVI   WORK,C'D'           DETACH EVENT                                 
         LA    R1,WORK                                                          
         BRAS  RE,EVENT                                                         
         BRAS  RE,ARSOFF                                                        
*                                                                               
         CLI   KILLFLG,YES         TASK WAS KILLED?                             
         BE    ATT40               YES - DON'T WAIT FOR IT                      
         TM    TASKECB,X'40'       DID REQUEST COMPLETE?                        
         BO    ATT40               YES                                          
         WAIT  ECB=TASKECB         WAIT FOR REQUEST COMPLETION                  
*                                                                               
ATT40    XC    WORK,WORK                                                        
         MVI   WORK+1,4            L'PARAMS WITHOUT LENGTH ITSELF               
         MVC   WORK+2(4),TASKECB   TASK ECB UPON COMPLETION                     
         NI    WORK+2,X'FF'-X'C0'  TURN OFF HIGH-ORDER TWO BITS OF ECB          
*                                                                               
*        LA    R1,WORK             R1 = A(PARAMETERS TO SVC)                    
*        LA    R0,15                                                            
*        LNR   R0,R0               R0 = -15                                     
*        SVC   247                 FOR JOB ACCOUNTING                           
*                                                                               
         OC    TASKECB+1(3),TASKECB+1                                           
         BZ    ATT42               NO SYSTEM OR USER ABEND                      
         GOTO1 VENQDEQ,DMCB,(C'D',=C'ALL')                                      
*                                                                               
         LA    R2,FULL                                                          
         EXTRACT (2),'S',FIELDS=(ASID)                                          
         L     R2,FULL                                                          
         LOCASCB ASID=(R2)         GET ASCB ADDRESS INTO R1                     
         L     R2,ASCBASSB-ASCB(R1) R5 = A(ASSB)                                
         SAM31                                                                  
         L     R1,ASSBJSAB-ASSB(R2) R2 = A(JSAB)                                
         USING JSAB,R1                                                          
         MVC   WORK,JSABJBID       JOBID (E.G., JOB12345)                       
         PACK  DUB,WORK+3(5)                                                    
         CVB   R1,DUB                                                           
         STCM  R1,3,WORK+8         CONVERT TO JOBNO                             
         LA    R2,FULL             GET JOB NAME INTO MVSNAME                    
         EXTRACT (2),FIELDS=TIOT                                                
         L     R2,FULL                                                          
         MVC   WORK(8),0(R2)                                                    
         SAM24 ,                   SWITCH BACK TO 24 BT MODE                    
         DROP  R1                                                               
*                                                                               
ATT42    MVC   P+30(4),=C'ECB='                                                 
         GOTO1 VHEXOUT,DMCB,TASKECB,P+34,4,0                                    
*                                                                               
         TIME  BIN                                                              
         S     R0,STRTTMRL         R0 = ELAPSED REAL TIME IN 0.01 SEC           
         BNM   *+8                                                              
         A     R0,=A(24*3600*100)  ADD 24HRS, JUST WENT PAST MIDNIGHT           
         ST    R0,FULL2            SAVE ET FOR LATER CHECK                      
*&&DO                                                                           
         BRAS  RE,ARSOFF                                                        
         L     RF,VSSB                                                          
         LAM   AR3,AR3,SSOTBLET-SSOOFF(RF)                                      
         ICM   R3,15,AJOBNTRY                                                   
         BZ    ATT44                                                            
         SAC   512                 DIP INTO THE DATASPACE                       
         USING TBJOBTAB,R3                                                      
*??      STCM  R0,7,TBJETIME       SET ELAPSED                                  
         DROP  R3                                                               
*&&                                                                             
ATT44    BRAS  RE,ARSOFF                                                        
         BRAS  RE,PRTTIME1         PUTS FORMATTED TIME IN WORK                  
         MVC   P+58(3),=C'ET='                                                  
         MVC   P+61(8),WORK                                                     
*                                                                               
         L     R1,AASCB            A(ASCB)                                      
         L     R0,ASCBEJST-ASCB(R1)                                             
         S     R0,STRTTMTC         R0 = ELAPSED TCB TIME IN SEC                 
         ST    R0,FULL             SAVE TCB TIME FOR LATER CHECK                
         BRAS  RE,PRTTIME2         PUTS FORMATTED TIME IN WORK                  
         MVC   P+44(4),=C'TCB='                                                 
         MVC   P+48(8),WORK                                                     
*                                                                               
         L     RE,TOTETTM                                                       
         A     RE,FULL2                                                         
         ST    RE,TOTETTM                                                       
         L     RE,TOTTCBTM                                                      
         A     RE,FULL                                                          
         ST    RE,TOTTCBTM                                                      
*                                                                               
         MVC   P+71(31),=C'LOADS AVOIDED: XXXX OUT OF XXXX'                     
         L     R1,AVOIDLD          # LOADS AVOIDED DUE TO MONSOON               
         EDIT  (R1),(4,P+86),ZERO=NOBLANK,ALIGN=LEFT                            
         L     R1,TOTALLD          # LOADS REQUESTED FOR THIS REQUEST           
         EDIT  (R1),(4,P+98),ZERO=NOBLANK,ALIGN=LEFT                            
         MVC   P(17),=CL17'COMPLETED'                                           
         BRAS  RE,PRNT                                                          
*                                                                               
***********************************************************************         
******CODE TO TRACK MEMORY LEAK IF ANY*********************************         
***********************************************************************         
*&&DO                                                                           
         MVC   P+25(12),=C'AFTER DETACH'                   ***                  
         MVC   P+44(12),=C'MEMORY LEFT='                   ***                  
         GOTO1 VCOVAIL,DMCB,C'LOOK'                        ***                  
         CLC   DMCB+4,=F'0'                                ***                  
         BNE   *+6                                         ***                  
         DC    H'0'                                        ***                  
         L     RF,DMCB+8                                   ***                  
         ST    RF,MEMORY2                                  ***                  
         EDIT  (RF),(8,P+56),ZERO=NOBLANK,FILL=0           ***                  
         MVC   P+72(8),=C'MEMORY0='                        ***                  
         L     RF,MEMORY0                                  ***                  
         EDIT  (RF),(8,P+80),ZERO=NOBLANK,FILL=0           ***                  
         GOTO1 VPRINTER                                    ***                  
         CLC   MEMORY0,MEMORY1                             ***                  
         BH    ATT44M                                      ***                  
         CLC   MEMORY1,MEMORY2                             ***                  
         BNH   ATT44X                                      ***                  
ATT44M   DS    0H                                          ***                  
         CLC   =C'MONS#TST',JOBNAM                         ***                  
         BE    ATT44T                                      ***                  
         CLC   =C'MONS#UP',JOBNAM                          ***                  
         BNE   ATT44X                                      ***                  
ATT44T   CLC   MEMMAIL#,=F'10'                             ***                  
         BH    ATT44X                                      ***                  
         L     RF,MEMMAIL#                                 ***                  
         AHI   RF,1                                        ***                  
         ST    RF,MEMMAIL#                                 ***                  
         MVC   ATTMS51,OPMS1+8    UUUUUUUU,XXX,NNNNN       ***                  
         MVC   ATTMS52,ATTCHPGM                            ***                  
         WTO   TEXT=ATTMS5H                                ***                  
ATT44X   DS    0H                                          ***                  
*&&                                                                             
***********************************************************************         
******CODE TO TRACK MEMORY LEAK IF ANY*********************************         
***********************************************************************         
*                                                                               
*                                                                               
*                                  ADD UP LOAD COUNTERS                         
         L     RE,AVOIDLDS                                                      
         A     RE,AVOIDLD                                                       
         ST    RE,AVOIDLDS                                                      
         L     RE,TOTALLDS                                                      
         A     RE,TOTALLD                                                       
         ST    RE,TOTALLDS                                                      
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,7,TASKECB+1      RE = 00SSSUUU (ABEND CODES)                  
         BNZ   ATT46               TASK ABENDED                                 
*                                                                               
         TM    REQFLAGS,REQ2STEP   REQUEST HAS 2ND EXEC STEP?                   
         BNO   ATT58               NO MORE EXEC STEP                            
*                                                                               
         MVC   P(17),=CL17'CONTINUE_NXT_STEP'                                   
         BRAS  RE,PRNT                                                          
         BRAS  RE,NXTSTEP          GET NEXT STEP JCLFILE                        
         BE    ATT12               NEXT EXEC STEP                               
         B     ATT14               BAD JCL ERROR                                
*                                                                               
ATT46    MVI   ABENDFLG,YES        SOON REQUEST COMPLETED W/ABEND               
         SR    RF,RF                                                            
         SRDL  RE,12               RE = SYSTEM ABEND CODE                       
         SRL   RF,20               RF = USER ABEND CODE                         
         MVC   P+11(34),=C'*** ERROR ***   ABEND = SXXX UXXXX'                  
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+41(4),DUB         PUT USER ABEND CODE IN PRINT LINE            
         STH   RE,HALF                                                          
         GOTO1 VHEXOUT,DMCB,HALF,FULL,2,0                                       
         MVC   P+36(3),FULL+1      PUT SYSTEM ABEND CODE IN PRINT LINE          
         MVC   ERRMSG(18),OPMS1+8  SAVE ERROR MSG RETURN TO MONSTER             
         MVC   ERRMSG+19(34),P+11                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         OC    USRPSW,USRPSW       DID MASTER RETURN A VALID PSW?               
         BZ    ATT58               NO -- DON'T BOTHER PRINTING PSW+REGS         
*                                                                               
         MVC   P(4),=C'PSW='       PSW AT ABEND                                 
         GOTO1 VHEXOUT,DMCB,USRPSW,P+5,4,0                                      
         GOTO1 (RF),(R1),USRPSW+4,P+14,4,0                                      
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(6),=C'GR 0-7'                                                  
         LA    R3,USRREGS          REGISTERS R0 - R7 AT ABEND                   
ATT48    LA    R0,8                                                             
         LA    R7,P+8              START OF PRINT LINE                          
ATT50    GOTO1 VHEXOUT,DMCB,(R3),(R7),4,0                                       
         LA    R3,4(R3)            BUMP TO NEXT REGISTER                        
         LA    R7,9(R7)            BUMP TO NEXT PRINT LINE POSITION             
         BCT   R0,ATT50                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         BC    0,*+18              ** SELF-MODIFYING CODE **                    
         MVI   *-3,X'F0'           NOW DO R8 - RF                               
         MVC   P(6),=C'GR 8-F'                                                  
         B     ATT48                                                            
         MVI   *-17,0              ** SELF-MODIFYING CODE **                    
*                                                                               
         CLI   DUMPWARN,YES        WARN ON DUPLICATE DUMPS?                     
         BNE   ATT58               NO                                           
         CLC   SAVEPSW,USRPSW      PSW SAME AS LAST TIME?                       
         BNE   ATT56               NO                                           
         CLC   SAVERB,USRRB        RB SAME AS LAST TIME?                        
         BNE   ATT56               NO                                           
*                                                                               
         LA    RE,ABENDCDS         TABLE OF ABEND CODES                         
ATT52    CLC   =X'FFFFFF',0(RE)                                                 
         BE    ATT54               NOT FOUND: WARN OPERATOR TO RESTART          
         CLC   0(L'ABENDCDS,RE),TASKECB+1                                       
         BE    ATT56               MATCH FOUND: NO RESTART NEEDED               
         LA    RE,L'ABENDCDS(,RE)                                               
         B     ATT52               NEXT TABLE ENTRY                             
*                                                                               
ATT54    WTO   TEXT=ATTMS3H                                                     
         MVC   P+11(L'ATTMS3),ATTMS3                                            
         GOTO1 VPRINTER                                                         
*                                                                               
ATT56    MVC   SAVEPSW,USRPSW      REMEMBER LAST PSW AND REGISTERS. . .         
         MVC   SAVEREGS,USRREGS    . . . AT ABEND                               
*                                                                               
ATT58    L     RE,TOTETTM                                                       
         C     RE,MAXETTM          EXCESS MAX ET TIME ALLOWED?                  
         BL    *+8                                                              
         MVI   ETTMFLG,YES         ET TIME EXCESS                               
*&&US                                                                           
         L     RE,TOTTCBTM                                                      
         C     RE,MAXTCBTM         EXCESS MAX TCB TIME ALLOWED?                 
         BL    *+16                                                             
         MVI   TCBTMFLG,YES        SET FLAG FOR EXCESSING TCB TIME              
         BRAS  RE,LONGSOON                                                      
         B     *+16                                                             
*                                                                               
         CLI   ETTMFLG,YES         ET TIME EXCESS?                              
         BNE   *+8                                                              
         BRAS  RE,LONGSOON                                                      
*&&                                                                             
         CLI   TRACEFLG,YES        PRINT DETAILED TRACE?                        
         BNE   ATT60               NO                                           
         GOTO1 VPRINTER            SKIP A LINE                                  
*                                                                               
ATT60    CLI   JESMSG,YES          ARE WE DISPLAYING CONSOLE MESSAGES?          
         BNE   ATT62               NO                                           
         WTO   TEXT=OPMS1H                                                      
*                                                                               
ATT62    SAM31                                                                  
         XC    FIWREF,FIWREF                                                    
         MVC   FIWREF+2(2),PQREFNO                                              
         BRAS  RE,FIRRN                                                         
         BRAS  RE,FIRN1            IS THIS A VALID REPORT NUMBER                
         JNE   *+2                                                              
         BRAS  RE,FIRRC            REPORT NUMBER TO CI ADDRESS                  
         MVC   CIADDR,FIWCIA                                                    
*                                                                               
         L     R4,FIWNDA                                                        
         LA    R4,SI1NDX-SI1PAR(R4) A(INDEX)                                    
*                                                                               
         CLI   UPDATIVE,YES        UPDATIVE SOON?                               
         BNE   ATT64                                                            
         TM    PQTYPE,PQTYUPDT     UPDATIVE BIT IS ON IN PQ INDEX?              
         BO    ATT64                                                            
*                                                                               
         MVI   UPDFLG,YES          PQ INDEX AND JCL DON'T MATCH!                
         CLC   =C'SI2',REQSPP                                                   
         BE    *+14                                                             
         CLC   =C'NI2',REQSPP                                                   
         BNE   *+8                                                              
         MVI   UPDFLG,NO           TELL MONSTER, NOT PRINT THIS MSG             
*                                                                               
         MVC   ERRMSG(18),OPMS1+8  SAVE ERROR MSG RETURN TO MONSTER             
         MVC   ERRMSG+19(4),=CL4'UPD*'                                          
         MVC   ERRMSG+23(3),REQSPP                                              
         CLI   ABENDFLG,YES        ABEND?                                       
         BNE   ATT64                                                            
         MVC   ERRMSG+26(8),=CL8' * ERR *'                                      
*                                                                               
ATT64    TM    PQATTB,PQATJOBI     IS REPORT 'SUBMITTED' OR 'RUNNING'?          
         BNZ   ATT66               YES - MARK IT ERROR AND VISIBLE              
*                                                                               
         CLI   FORCEVIS,YES                                                     
         BNE   ATT90                                                            
         MVI   FORCEVIS,NO                                                      
         MVI   BYTE2,0             MARK IT VISIBLE BUT NO ERROR                 
         B     ATT74                                                            
                                                                                
*----------------------------------------------------------------------         
* IF A JCL ERROR WAS FOUND, OR IF THE REQUEST COMPLETED                         
* BUT WAS NOT IN READY OR ERROR STATUS, THEN                                    
* ENQUEUE THE PRINT QUEUE ONCE MORE, AND SET THE ERROR FLAG                     
* IN THE PRINT QUEUE INDEX AND IN THE FIRST CONTROL INTERVAL.                   
*----------------------------------------------------------------------         
ATT66    MVI   BYTE2,PQATERR       ERROR BIT ON                                 
         CLI   COMSCORE,1          IS IT COMSCORE PHASE 1?                      
         BNE   ATT74               NO                                           
         MVI   BYTE2,0             RESET                                        
         BRAS  RE,LOCKJOB          LOCK JOB TABLE                               
         L     RF,VSSB                                                          
         LAM   AR3,AR3,SSOTBLET-SSOOFF(RF)                                      
         ICM   R3,15,AJOBNTRY                                                   
         BZ    ATT67                                                            
*                                                                               
         SAC   512                                                              
         USING TBJOBTAB,R3                                                      
         CLC   PQREPKEY,TBJPQKEY   IS IT THE SAME REPORT?                       
         BE    ATT68               If not the right one don't flag              
*                                                                               
ATT67    DC    0H                  Just for now                                 
         STAR  LABEL=Y,CLEAR=Y,ARS=OFF                                          
         BRAS  RE,FINDJOB          R3 = A(Job found)                            
         REAR  ARS=ON                                                           
         BNE   ATT90               That ain't good                              
         ST    R3,AJOBNTRY         Change Job Entry                             
*                                                                               
ATT68    OI    TBJSTAT,TBJHOLD     Hold for now                                 
         DROP  R3                                                               
         BRAS  RE,ARSOFF                                                        
         BRAS  RE,UNLKJOB                                                       
*                                                                               
ATT74    BRAS  RE,FIRSLOCK                                                      
         BRAS  RE,FIRRLOCK                                                      
*                                                                               
         CLI   COMSCORE,1          If ComScore phase 1 put on hold              
         BNE   ATT76                                                            
         OI    PQSTAT,PQSTHO       Put PQ on HOLD                               
         B     ATT78                                                            
*                                                                               
ATT76    OC    PQATTB,BYTE2        SET FLAG, RESET JCL FLAG                     
         OI    PQATTB,PQATJOBO         SET JOB OUTPUT                           
         NI    PQATTB,X'FF'-PQATJOBI                                            
         NI    PQSTAT,X'FF'-PQSTIN     TRUN OFF INVISIBLE BIT                   
*                                                                               
ATT78    L     RF,APQSAVE                                                       
         USING SKBUFFD,RF                                                       
         XC    SKFCTRL,SKFCTRL     START READING FROM SCRATCH                   
         XC    SKXCTRL,SKXCTRL                                                  
         MVC   SKFSTCI(2),FIWCIA   USE D/A FROM BEFORE                          
         MVI   SKFSTCI+2,1         ALWAYS START WITH BLOCK 1                    
         DROP  RF                                                               
*                                                                               
         XC    R,R                                                              
         MVI   R+4,C'L'            SILLY PARAMETER FOR RANDOM READ              
         GOTO1 VDMGR,DMCB,(0,RANDOM),CFPQID,0,R,CXREC                           
         CLI   DMCB+8,0            ANY DMGR ERRORS?                             
         BE    ATT80                                                            
         BRAS  RE,FIRRUNLK                                                      
         BRAS  RE,FIRSUNLK                                                      
         DC    H'0'                                                             
*                                                                               
ATT80    LA    R4,CXREC            A(FIRST CONTROL INTERVAL)                    
         CLI   COMSCORE,1                                                       
         BNE   ATT82                                                            
         OI    PQSTAT,PQSTHO       Put PQ on HOLD                               
         B     ATT84                                                            
*                                                                               
ATT82    OC    PQATTB,BYTE2        SET FLAGS                                    
         OI    PQATTB,PQATJOBO     SET JOB OUTPUT                               
         NI    PQATTB,255-PQATJOBI RESET JCL FLAG                               
         NI    PQSTAT,255-PQSTIN   TURN OFF INVISIBLE BIT                       
*                                                                               
ATT84    SAM24                                                                  
         MVC   CIADDR,FIWCIA                                                    
         CLC   CIADDR,=X'00010100' Don't write to first track                   
         JE    *+2                                                              
         GOTO1 VDMGR,DMCB,(0,DMWRT),CFPQID,CIADDR,CXREC                         
         SAM31                                                                  
         BRAS  RE,FIRRUNLK         DEQUEUE THE REPORT AND RELEASE SHARE         
         BRAS  RE,FIRSUNLK                                                      
         CLI   DMCB+8,0            ANY ERROR ON THE DMWRT?                      
         JNE   *+2                                                              
*                                                                               
ATT90    SAM24                                                                  
         BRAS  RE,ARSOFF                                                        
         BRAS  RE,UNLKJOB          UNLOCK incase left on                        
         MVC   OPMS1,SPACES                                                     
         MVC   OPMS1(22),=C'NO REQUEST RUNNING NOW'                             
         GOTO1 VSHMUSS,DMCB,(0,=CL8'DETACH'),(0,=CL8'PRTQ'),0,0                 
*                                                                               
ATTX     GOTO1 VPRINTER            SKIP A LINE                                  
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
ATTMS1H  DC    AL2(L'ATTMS1)                                                    
ATTMS1   DC C'*** WARNING *** UUUUUUUU,XXX,NNNNN IGNORED - CHECK IT'            
ATTMS2H  DC    AL2(L'ATTMS2)                                                    
ATTMS2   DC C'*** WARNING *** UUUUUUUU,XXX,NNNNN  TIMED OUT BY MONSOON'         
ATTMS3H  DC    AL2(L'ATTMS3)                                                    
ATTMS3   DC C'*** WARNING *** DUPLICATE SOON DUMP: CONSIDER RECYCLING'          
ATTMS4H  DC    AL2(L'ATTMS4)                                                    
ATTMS4   DC C'*** WARNING *** UUUUUUUU,XXX,NNNNN  CANCELLED '                   
ATTMS5H  DC    AL2(L'ATTMS5+L'ATTMS51+L'ATTMS52)                                
ATTMS5   DC C'AUTONOTE*US-MF_FAC_NOTIFY:MEMORY LEAKED! '                        
ATTMS51  DC C'UUUUUUUU,XXX,NNNNN '                                              
ATTMS52  DC C'CCCCCCCC'                                                         
                                                                                
         DROP  RB,R8                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* SHARED MEMORY ROUTINES                                                        
***********************************************************************         
       ++INCLUDE DDSHFIR                                                        
         EJECT                                                                  
                                                                                
***********************************************************************         
* THE TIMER EXIT ROUTINE.  IT POSTS THE TIMERECB                      *         
***********************************************************************         
TIMERXIT SAVE  (14,12),,*                                                       
         LR    RB,RF                                                            
         USING TIMERXIT,RB                                                      
         POST  TIMERECB                                                         
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
*                                                                               
TIMERECB DC    F'0'                ECB OF ATTACHED TASK TIMER                   
         EJECT                                                                  
***********************************************************************         
* Find JOB that to see if it moved on us                                        
***********************************************************************         
CLS      USING DMSPACED,DSPCTAB                                                 
FINDJOB  NTR1  BASE=*,LABEL=*                                                   
         MVI   FOUNDJOB,NO                                                      
         BRAS  RE,LOCKCLS                                                       
         L     RF,VSSB                                                          
         LAM   AR2,AR2,SSOTBLET-SSOOFF(RF)                                      
         CPYA  AR3,AR2                                                          
         ICM   R2,15,CLS.DSPTFRST                                               
         ICM   R4,15,CLS.DSPTEND                                                
         SAC   512                                                              
         DROP  CLS                                                              
*                                                                               
         USING JCLASSD,R2                                                       
FJOB08   CLC   JCCLASS,PQCLAS      Match on class                               
         BNE   FJOB10                                                           
         CLC   JCTYPE,QTYPE        Queue type                                   
         BE    FJOB12                                                           
FJOB10   AHI   R2,JCLASSL          Next class                                   
         CR    R2,R4                                                            
         BL    FJOB08                                                           
         DC    H'0'                Have no class                                
*                                                                               
         USING TBJOBTAB,R3                                                      
FJOB12   ICM   R3,15,JCFRUN        First running job                            
         BZ    FJOBXIT             Not Found                                    
FJOB20   CLC   TBJPQKEY,PQREPKEY   Match on PQ key                              
         BNE   FJOB26                                                           
         MVI   FOUNDJOB,YES                                                     
         B     FJOBXIT                                                          
*                                                                               
FJOB26   ICM   R3,15,TBJNXT        Next report                                  
         BNZ   FJOB20                                                           
         XR    R3,R3                                                            
         DROP  R2                                                               
*                                                                               
FJOBXIT  BRAS  RE,ARSOFF                                                        
         BRAS  RE,UNLKCLS                                                       
         BRAS  RE,FJMSG                                                         
         CLI   FOUNDJOB,YES        Set equal if reacquired job                  
         XIT1  REGS=(R3)                                                        
         EJECT                                                                  
***********************************************************************         
* Notify us that we searched for job                                            
* Display PQ job and old location and new location address id DS.               
***********************************************************************         
         USING MONSD,BUFFER                                                     
FJMSG    NTR1                                                                   
         MVC   FJUSERID,MNSUSER    USERID=                                      
         MVC   FJPQ#,MNSPQ#        PQKEY=#####,CCC,######                       
         MVC   FJSUBID,MNSPQSUB                                                 
         MVC   FJREP#,MNSPQSEQ                                                  
*                                                                               
         MVC   FJINFO,=CL10'FOUND'                                              
         LTR   R3,R3                                                            
         BNZ   *+10                                                             
         MVC   FJINFO,=CL10'NOT FOUND'                                          
*                                                                               
         L     R4,AJOBNTRY         Original Job Entry address                   
         CVD   R4,DUB                                                           
         OI    DUB+L'DUB,X'0F'                                                  
         UNPK  FJOLDLOC,DUB                                                     
*                                                                               
         CVD   R3,DUB                                                           
         OI    DUB+L'DUB,X'0F'                                                  
         UNPK  FJNEWLOC,DUB                                                     
*                                                                               
         WTO   TEXT=FJMSG1H                                                     
         J     EXITOK                                                           
***********************************************************************         
* Message block to print when found or not found                                
***********************************************************************         
FJMSG1H  DC    AL2(FJMSGLNQ)                                                    
FJMSG1   DC    C'USERID='   '                                                   
FJUSERID DC    CL10' '                                                          
         DC    C' PQKEY='                                                       
FJPQ#    DC    CL5' '                                                           
         DC    C','                                                             
FJSUBID  DC    CL3' '                                                           
         DC    C','                                                             
FJREP#   DC    CL5' '                                                           
         DC    C' A(OLD)='                                                      
FJOLDLOC DC    C' '                                                             
         DC    C' A(NEW)='                                                      
FJNEWLOC DC    C' '                                                             
         DC    C'->'                                                            
FJINFO   DC    CL10' '                                                          
FJMSGLNQ EQU   *-FJMSG1            Length of message                            
                                                                                
         DROP  RB                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* REQUEST IS NOT IN JCLFILE YET                                       *         
*    1) COPY PQ JCL BOOK TO TMPFILE AND CHECK FOR 2ND STEP            *         
*    2) IF THERE IS A 2ND STEP,                                       *         
*          I)  COPY TMPFILE TO JCLFILE ONLY                           *         
*          II) SET REQ2STEP ON                                        *         
*    3) IF THERE IS NO 2ND STEP,                                      *         
*          I)  COPY TMPFILE TO JCLFILE, AND JCLBUF IN CORE            *         
*          II) SET REQ2STEP OFF                                       *         
***********************************************************************         
WRTJCLFL NTR1  BASE=*,LABEL=*                                                   
         MVC   AJCLBUF,AJCLBUFF                                                 
         NI    REQFLAGS,X'FF'-REQ2STEP   ASSUME ONLY 1 STEP                     
*                                                                               
         OPEN  (TMPFILE,(OUTPUT))  WRITE ALL CARDS TO TMPFILE                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WJCL02   GOTO1 VDMGR,DMCB,(0,=C'READ'),CFPQID,0,R,CXREC                         
         CLI   DMCB+8,0                                                         
         BNE   WJCL06              NO MORE RECORDS                              
*                                                                               
         TM    REQFLAGS,REQ2STEP   ALREADY GET THE 2ND JCLFILE?                 
         BO    WJCL04              YES                                          
*                                                                               
         LA    RF,R+2              SKIP PAST THE OPTIONAL NAME FIELD...         
         LA    RF,1(RF)            AND LOCATE THE OPERATION FIELD               
         CLI   0(RF),C' '                                                       
         BNE   *-8                                                              
         LA    RF,1(RF)                                                         
         CLI   0(RF),C' '                                                       
         BE    *-8                                                              
*                                                                               
         CLC   =C'EXEC',0(RF)      IS THIS THE EXEC CARD?                       
         BNE   WJCL04                                                           
         CLI   4(RF),C' '                                                       
         BNH   WJCL04                                                           
         OI    REQFLAGS,REQ2STEP   REQUEST HAS A 2ND EXEC STEP                  
*                                                                               
WJCL04   CLC   =C'FORCEVISIBLE',R+1                                             
         BNE   *+12                                                             
         MVI   FORCEVIS,YES                                                     
         B     WJCL02              DON'T COPY FORCEVISIBLE CARD                 
         PUT   TMPFILE,R+1                                                      
         B     WJCL02                                                           
*                                                                               
WJCL06   CLOSE (TMPFILE,)                                                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (JCLFILE,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                  COPY TMPFILE TO JCLFILE                      
         LA    R1,WJCL12           EODAD                                        
         LA    RF,TMPFILE          A(DCB) FOR TMPFILE                           
         ST    R1,DCBEODAD-IHADCB(RF)                                           
*                                                                               
         OPEN  (TMPFILE,(INPUT))                                                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    REQFLAGS,REQ2STEP   HAS 2ND JCLFILE?                             
         BO    WJCL10              YES - DON'T COPY TO JCLBUF                   
*                                                                               
         L     R0,AJCLBUFF         CLEAR JCLBUF                                 
         LHI   R1,JCLBUFLQ                                                      
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R3,AJCLBUFF                                                      
WJCL08   GET   TMPFILE,CARD                                                     
         PUT   JCLFILE,CARD                                                     
         CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                JCLBUF IS TOO SMALL!                         
         MVC   0(L'JCLBUF,R3),CARD    COPY CARD TO JCLBUF                       
         AHI   R3,L'JCLBUF                                                      
         B     WJCL08                                                           
*                                                                               
WJCL10   GET   TMPFILE,CARD                                                     
         PUT   JCLFILE,CARD                                                     
         B     WJCL10                                                           
*                                                                               
WJCL12   CLOSE (TMPFILE)                                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLOSE (JCLFILE)                                                        
         LTR   RF,RF                                                            
         BZ    EXITOK                                                           
         DC    H'0'                                                             
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SPLIT MULTI STEPS SYSIN CARDS INTO JCLFILE2 AND JCLBUF              *         
* EXIT:  JCLBUF - THIS STEP SYSIN CARDS                               *         
*        JCLFILE2 - REST STEPS SYSIN CARDS                            *         
***********************************************************************         
EXTJCLFL NTR1  BASE=*,LABEL=*                                                   
         TM    REQFLAGS,REQ2STEP   MAY HAS 2ND JCLFILE?                         
         BNO   EXITOK              NO - SYSIN CARDS ALREADY IN JCLBUF           
*                                                                               
         L     R0,AJCLBUFF                                                      
         LHI   R1,JCLBUFLQ                                                      
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R3,AJCLBUFF                                                      
         NI    REQFLAGS,X'FF'-REQ2STEP   ASSUME ONLY 1 STEP                     
*                                                                               
         OPEN  (TMPFILE,(INPUT))                                                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
EJ10     LA    R1,EJ100            EODAD                                        
         LA    RF,TMPFILE          A(DCB) FOR TMPFILE                           
         ST    R1,DCBEODAD-IHADCB(RF)                                           
*                                                                               
EJ20     GET   TMPFILE,CARD                                                     
*                                                                               
         TM    REQFLAGS,REQ2STEP   ALREADY GET THE 2ND JCLFILE?                 
         BO    EJ30                YES                                          
*                                                                               
         LA    RF,CARD+1           SKIP PAST THE OPTIONAL NAME FIELD...         
         LA    RF,1(RF)            AND LOCATE THE OPERATION FIELD               
         CLI   0(RF),C' '                                                       
         BNE   *-8                                                              
         LA    RF,1(RF)                                                         
         CLI   0(RF),C' '                                                       
         BE    *-8                                                              
*                                                                               
         CLC   =C'EXEC',0(RF)      IS THIS THE EXEC CARD?                       
         BNE   EJ30                                                             
         CLI   4(RF),C' '                                                       
         BNH   EJ30                                                             
         MVC   BYTE,4(RF)          SAVE STEP#                                   
         MVI   4(RF),C' '          REPLACE 'EXEC#' BY 'EXEC '                   
         B     EJ50                FROM NOW, WRITE TO JCLFILE2                  
*                                                                               
EJ30     CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                JCLBUF IS TOO SMALL!                         
         MVC   0(L'JCLBUF,R3),CARD    COPY CARD TO JCLBUF                       
         AHI   R3,L'JCLBUF                                                      
         B     EJ20                                                             
*                                                                               
EJ50     OPEN  (JCLFILE2,(OUTPUT))                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    REQFLAGS,REQ2STEP   REQUEST HAS A 2ND EXEC STEP                  
         PUT   JCLFILE2,CARD       EXEC CARD                                    
*                                                                               
EJ60     LA    R1,EJ120            EODAD                                        
         LA    RF,TMPFILE          A(DCB) FOR TMPFILE                           
         ST    R1,DCBEODAD-IHADCB(RF)                                           
*                                  1ST STEP SYSIN CARD NOT READ YET             
EJ70     GET   TMPFILE,CARD                                                     
*                                                                               
         CLC   =C'//SYSIN',CARD                                                 
         BNE   EJ80                                                             
         CLC   BYTE,CARD+7         MATCHED STEP#?                               
         BNE   EJ80                NO                                           
         MVI   CARD+7,C' '         REPLACE 'SYSIN#' BY 'SYSIN '                 
         PUT   JCLFILE2,CARD                                                    
         B     EJ90                                                             
*                                                                               
EJ80     PUT   JCLFILE2,CARD                                                    
         B     EJ70                                                             
*                                                                               
EJ90     GET   TMPFILE,CARD                                                     
*                                                                               
         PUT   JCLFILE2,CARD                                                    
         CLC   =C'**',CARD         END OF SYSIN#?                               
         BNE   EJ90                NO - NEXT REC                                
*                                                                               
         CLOSE (JCLFILE2,)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     EJ10                CONTINUE TO READ FOR 1ST JCLFILE             
*                                                                               
EJ100    CLOSE (TMPFILE,)                                                       
         LTR   RF,RF                                                            
         BZ    EXITOK                                                           
         DC    H'0'                                                             
*                                                                               
EJ120    CLOSE (TMPFILE,)                                                       
         LTR   RF,RF                                                            
         BZ    EXITL                                                            
         DC    H'0'                                                             
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* JCLFILE2 - JCL OF STEPS LEFT TO BE RUN                              *         
* JCLBUF   - REQUEST CARDS RETURN BY THE CONTROLLER IN PREVIOUS STEP  *         
*                                                                     *         
* MERGE JCLFILE2 AND JCLBUF INTO TMPFILE                              *         
* SET REQ2STEP ON, SO EXTJCLFL WILL TRY TO SPLIT JCLS FOR MULTI STEPS *         
***********************************************************************         
NXTSTEP  NTR1  BASE=*,LABEL=*                                                   
         OI    REQFLAGS,REQ2STEP   MAY HAVE 2ND STEP                            
         OI    AJCLBUF,X'80'       CONTINUE FLAG ON FOR NEXT CONTROLLER         
*                                                                               
         LA    R1,NS100            EODAD                                        
         LA    RF,JCLFILE2         A(DCB) FOR JCLFILE2                          
         ST    R1,DCBEODAD-IHADCB(RF)                                           
*                                                                               
         OPEN  (JCLFILE2,(INPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (TMPFILE,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
NS10     GET   JCLFILE2,CARD                                                    
         PUT   TMPFILE,CARD                                                     
         CLC   =C'//SYSIN ',CARD                                                
         BNE   NS10                                                             
*                                                                               
NS20     GET   JCLFILE2,CARD                                                    
         CLC   =C'**',CARD                                                      
         BE    NS30                                                             
         PUT   TMPFILE,CARD                                                     
         B     NS20                                                             
*                                                                               
NS30     L     R3,AJCLBUFF                                                      
NS35     CLI   0(R3),X'FF'                                                      
         BE    NS40                END OF REQUEST CARDS                         
         OC    CARD,CARD                                                        
         BZ    NS40                END OF REQUEST CARDS                         
         MVC   CARD,0(R3)                                                       
         PUT   TMPFILE,CARD                                                     
         AHI   R3,L'JCLBUF                                                      
         B     NS35                                                             
*                                                                               
NS40     MVC   CARD,=CL80'**'                                                   
         PUT   TMPFILE,CARD                                                     
*                                                                               
         CLOSE (JCLFILE2,)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLOSE (TMPFILE,)                                                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     NSOKX                                                            
*                                                                               
NS100    CLOSE (JCLFILE2,)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLOSE (TMPFILE,)                                                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
NSBADX   B     EXITL               SET CC NOT EQUAL                             
NSOKX    B     EXITOK              SET CC EQUAL                                 
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOCATE THE EXEC CARD, AND FIGURE OUT WHICH PROGRAM TO ATTACH.       *         
* ALSO, LOCATE THE TIME PARAMETER ON THE EXEC CARD AND CREATE AN      *         
* "RTPTCB=" CARD.  THEN CREATE A SYSIN FILE WITH ALL CONTROL AND      *         
* REQUEST CARDS.                                                      *         
***********************************************************************         
GETJCL   NTR1  BASE=*,LABEL=*                                                   
         XC    SYPGM,SYPGM                                                      
         OPEN  (SYSIN,OUTPUT)      WRITE ALL SYSIN CARDS TO SYSIN FILE          
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         L     R4,AJCLBUFF                                                      
         MVC   AGENCY,=C'??'       ASSUME NO AGENCY CODE                        
*                                                                               
         MVC   OPMS1+27(14),=C'*NO EXEC CARD*'                                  
GJ40     BRAS  R7,GETJCLR                                                       
*                                                                               
         CLC   =C'$$MONSOON',R     NEW-STYLE "JCL"?                             
         BNE   GJ45                                                             
*                                                                               
GJ42     BRAS  R7,GETJCLR                                                       
*                                                                               
         CLC   =C'EXEC=',R                                                      
         BNE   *+14                                                             
         MVC   ATTCHPGM,R+5        SAVE PROGRAM NAME                            
         B     GJ42                                                             
*                                                                               
         CLC   =C'TIME=',R                                                      
         BNE   *+20                                                             
         MVC   RTPTCB+7(73),SPACES                                              
         MVC   RTPTCB+7(6),R+5     SAVE ATTACH TIME                             
         B     GJ42                                                             
*                                                                               
         CLC   =C'$$CONTROLLER',R                                               
         BE    GJ142                                                            
         B     GJ42                                                             
*                                                                               
GJ45     CLC   =C'//',R            NO -- IS THIS A JCL STATEMENT?               
         BNE   GJ40                NO -- READ THE NEXT CARD                     
*                                                                               
         CLC   =C'JOB',R+11                                                     
         BNE   *+10                                                             
         MVC   SYPGM,R+6                                                        
*                                                                               
         LA    RF,R+1              SKIP PAST THE OPTIONAL NAME FIELD...         
         LA    RF,1(RF)            AND LOCATE THE OPERATION FIELD               
         CLI   0(RF),C' '                                                       
         BNE   *-8                                                              
         LA    RF,1(RF)                                                         
         CLI   0(RF),C' '                                                       
         BE    *-8                                                              
         CLC   =C'EXEC ',0(RF)     IS THIS THE EXEC CARD?                       
         BNE   GJ40                NO -- READ THE NEXT CARD                     
*                                                                               
         MVC   AGENCY,R+2          SAVE AGENCY CODE                             
*                                                                               
         LA    RF,4(RF)                                                         
         LA    RF,1(RF)            FIGURE OUT WHAT TO ATTACH                    
         CLI   0(RF),C' '                                                       
         BE    *-8                                                              
*                                                                               
         MVI   BYTE,0              ASSUME PROCEDURE NAME WILL BE FOUND          
         CLC   =C'PGM=',0(RF)      IS THE LOAD MODULE NAMED?                    
         BNE   *+16                                                             
         LA    RF,4(RF)            YES -- POINT RF TO LOAD MODULE NAME          
         MVI   BYTE,X'FF'          A PROGRAM NAME WAS FOUND                     
         B     GJ60                                                             
*                                                                               
         CLC   =C'PROC=',0(RF)     IS A PROCEDURE NAMED?                        
         BNE   *+8                 NO -- RF ALREADY POINTS TO PROCEDURE         
         LA    RF,5(RF)            YES -- POINT RF TO PROCEDURE NAME...         
         ST    RF,FULL             ... AND SAVE IT                              
*                                                                               
GJ50     CLI   0(RF),C' '          END OF CARD?                                 
         BNE   *+12                                                             
         L     RF,FULL             YES -- RESTORE RF                            
         B     GJ60                                                             
         CLI   0(RF),C','          END OF NAME?                                 
         BE    *+12                YES                                          
         LA    RF,1(RF)                                                         
         BNE   GJ50                                                             
*                                                                               
         CLC   =C',PROG=',0(RF)    IS OVERRIDE PROGRAM (&PROG) GIVEN?           
         BE    *+12                                                             
         L     RF,FULL             RESTORE RF                                   
         B     GJ60                                                             
         LA    RF,6(RF)            YES -- POINT RF TO THE NAME                  
         MVI   BYTE,X'FF'          A PROGRAM NAME WAS FOUND                     
*                                                                               
GJ60     MVC   ATTCHPGM,SPACES     SYMBOL MUST BE BLANK-PADDED                  
         LA    R1,ATTCHPGM                                                      
GJ70     MVC   0(1,R1),0(RF)       PUT SYMBOL INTO WORK AREA                    
         LA    RF,1(RF)                                                         
         CLI   0(RF),C' '                                                       
         BE    *+20                END OF CARD                                  
         CLI   0(RF),C','                                                       
         BE    *+12                END OF NAME                                  
         LA    R1,1(R1)                                                         
         B     GJ70                                                             
*                                                                               
         CLI   BYTE,X'FF'          WAS A PROGRAM NAME FOUND?                    
         BE    GJ90                YES -- IT'S NOT A PROCEDURE                  
*                                                                               
         LA    R2,CONTRLRS         LIST OF CONTROLLERS                          
GJ80     CLI   0(R2),X'FF'         END OF LIST?                                 
         BNE   *+14                                                             
         MVC   OPMS1+27(14),=C'*UNKNOWN PROC*'                                  
         B     GJBADX              YES -- UNKNOWN CONTROLLER                    
         ZIC   RE,8(R2)            NUMBER OF SIGNIFICANT CHARACTERS - 1         
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),ATTCHPGM                                                 
         BE    *+12                WE HAVE A MATCH, R2 = A(CONTROLLER)          
         LA    R2,L'CONTRLRS(R2)   BUMP TO NEXT CONTROLLER                      
         B     GJ80                                                             
*                                                                               
         CLC   =C'REPORTER',0(R2)  DOES PROC=REPORTER                           
         BNE   *+14                                                             
         MVC   ATTCHPGM,=C'REPORTR ' YES, OVERRIDE WITH 7-CHAR PHASE            
         B     *+10                                                             
         MVC   ATTCHPGM,0(R2)      SAVE PROGRAM NAME                            
*                                                                               
GJ90     MVC   RTPTCB+7(73),SPACES CLEAR OUT THE TIME                           
*                                                                               
GJ100    CLI   0(RF),C' '          END OF CARD?                                 
         BE    GJ130                                                            
         CLC   =C',TIME=',0(RF)    IS THIS THE TIME PARAMETER?                  
         BE    *+12                                                             
         LA    RF,1(RF)                                                         
         B     GJ100                                                            
*                                                                               
         CLI   6(RF),C'('          IS TIME GIVEN IN MINUTES ONLY?               
         BE    GJ110               NO                                           
         LA    R2,6(RF)            R2 = A(NUMBER OF MINUTES)                    
         BAS   RE,CONVTIME                                                      
         L     R1,DMCB+4                                                        
         CHI   R1,1440             TIME=1440 ? (UNLIMITED?)                     
         BE    GJ130               YES -- IGNORE THIS (NOT ALLOWED)             
         MHI   R1,60               R1 = NUMBER OF SECONDS                       
         B     GJ120                                                            
*                                                                               
GJ110    LA    R2,7(RF)            R2 = A(NUMBER OF MINUTES)                    
         BAS   RE,CONVTIME                                                      
         L     R1,DMCB+4                                                        
         MHI   R1,60               R1 = NUMBER OF SECONDS                       
         L     R2,DMCB             A(NEXT CHARACTER)                            
         CLI   0(R2),C')'          ANY SECONDS?                                 
         BE    GJ120               NO                                           
         CLI   0(R2),C','                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,1(R2)                                                         
         BAS   RE,CONVTIME                                                      
         L     R2,DMCB             A(NEXT CHARACTER)                            
         CLI   0(R2),C')'          ANY SECONDS?                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         A     R1,DMCB+4           ADD SECONDS TO MINUTES                       
*                                                                               
GJ120    EDIT  (R1),(6,RTPTCB+7),ALIGN=LEFT                                     
*                                                                               
* 1) CREATE ALL 4 LIMIT CONTROL CARD WHEN LIMIT=N                               
* 2) SUPPRESS ALL OTHER LIMIT CONTROL WHEN LIMIT=N                              
*                                                                               
GJ130    DS    0H                                                               
*                                                                               
         MVC   OPMS1+27(15),=C'*NO SYSIN CARD*'                                 
GJ140    BRAS  R7,GETJCLR          SYSIN CARD NOT FOUND                         
         CLC   =C'//SYSIN ',R                                                   
         BNE   GJ140               IT IS NOT THE SYSIN CARD                     
*                                                                               
GJ142    MVC   CARD,SPACES         THIS MUST BE *FIRST* CONTROL CARD            
         MVC   CARD(9),=C'NOCONTROL' NEVER PRINT CONTROL CARDS                  
         PUT   SYSIN,CARD          PUT NOCONTROL CARD TO SYSIN FILE             
         CLI   TRACEFLG,YES        PRINT DETAILED TRACE?                        
         BNE   GJ143               NO                                           
         MVC   P(80),CARD                                                       
         GOTO1 VPRINTER                                                         
*                                                                               
GJ143    CLI   DSPACE,C' '         DSPACE= CARD PRESENT?                        
         BE    GJ145                                                            
         MVC   CARD,SPACES         YES -- GENERATE A DSPACE= CARD               
         MVC   CARD(7),=C'DSPACE='                                              
         MVC   CARD+7(1),DSPACE                                                 
         PUT   SYSIN,CARD          PUT DSPACE= CARD TO SYSIN FILE               
*                                                                               
         CLI   TRACEFLG,YES        PRINT DETAILED TRACE?                        
         BNE   GJ145               NO                                           
         MVC   P(80),CARD                                                       
         GOTO1 VPRINTER                                                         
*                                                                               
GJ145    XC    TCBTIME,TCBTIME                                                  
         CLI   RTPTCB+7,C' '       WAS A TIME PARAMETER FOUND?                  
         BE    GJ150                                                            
         LA    R2,RTPTCB           YES -- ADJUST IT IF NECESSARY                
         BRAS  RE,TCBCHNGE                                                      
*                                                                               
         CLI   LIMITFLG,NO         OFF-PEAK MODE (LIMIT=N) ?                    
         BE    GJ150               YES - SKIP JOB'S RTPTCB= CARD                
*                                                                               
         PUT   SYSIN,RTPTCB        PUT RTPTCB CARD TO SYSIN FILE                
         CLI   TRACEFLG,YES        PRINT DETAILED TRACE?                        
         BNE   GJ150               NO                                           
         MVC   P(80),RTPTCB                                                     
         GOTO1 VPRINTER                                                         
*                                                                               
GJ150    MVC   CARD,SPACES                                                      
         MVC   CARD(6),=C'DDSIO='  ALWAYS GENERATE A DDSIO= CARD                
         MVC   CARD+6(8),DDSIOMOD                                               
*        CLI   DSPACE,C'T'         DSPACE=T, TST SYSTEM?                        
*        BNE   *+10                NO                                           
*        MVC   CARD+6(8),=CL8'DDSIONF'   OTHERWISE, HARD CODE DDSIONF           
         PUT   SYSIN,CARD          PUT DDSIO= CARD TO SYSIN FILE                
         CLI   TRACEFLG,YES        PRINT DETAILED TRACE?                        
         BNE   GJ152               NO                                           
         MVC   P(80),CARD                                                       
         GOTO1 VPRINTER                                                         
*                                                                               
GJ152    MVC   CARD,SPACES                                                      
         MVC   CARD(8),=C'CORETAB='  ALWAYS GENERATE A CORETAB= CARD            
         GOTO1 VHEXOUT,DMCB,=A(CORETAB),CARD+8,4,0                              
         PUT   SYSIN,CARD          PUT CORETAB= CARD TO SYSIN FILE              
         CLI   TRACEFLG,YES        PRINT DETAILED TRACE?                        
         BNE   GJ160               NO                                           
         MVC   P(80),CARD                                                       
         GOTO1 VPRINTER                                                         
*                                                                               
GJ160    MVC   CARD,SPACES                                                      
         MVC   CARD(5),=C'SOON='   ALWAYS GENERATE A SOON= CARD                 
         MVC   CARD+5(2),PQCLAS    CLASS                                        
         MVI   CARD+7,C','                                                      
         MVC   CARD+8(1),PQPRTY    PRIORITY                                     
         MVI   CARD+9,C','                                                      
         MVC   CARD+10(1),PQFACPAK FACPAK ID                                    
         MVI   CARD+11,C','                                                     
         MVC   CARD+12(1),QTYPE    QTYPE:S/M/L                                  
         MVI   CARD+13,C','                                                     
         GOTO1 VHEXOUT,DMCB,PQTIME,CARD+14,3,0                                  
         PUT   SYSIN,CARD          PUT SOON= CARD TO SYSIN FILE                 
         CLI   TRACEFLG,YES        PRINT DETAILED TRACE?                        
         BNE   GJ250               NO                                           
         MVC   P(80),CARD                                                       
         GOTO1 VPRINTER                                                         
*                                                                               
GJ250    CLI   LIMITFLG,NO         OFF-PEAK MODE (LIMIT=N) ?                    
         BNE   GJ300               NO                                           
*                                                                               
*&&UK*&& B     GJ300               NOT FOR UK                                   
*                                                                               
         MVC   CARD,SPACES                                                      
         MVC   CARD(11),=C'RUN=OFFPEAK'                                         
         PUT   SYSIN,CARD          PUT 'RUN=OFFPEAK' CARD TO SYSIN FILE         
         CLI   TRACEFLG,YES        PRINT DETAILED TRACE?                        
         BNE   GJ260               NO                                           
         MVC   P(80),CARD                                                       
         GOTO1 VPRINTER                                                         
*                                  OFFPEAK TCB TIME LIMIT                       
GJ260    MVC   CARD,SPACES                                                      
         MVC   CARD(7),=CL7'RTPTCB='                                            
         MVC   CARD+7(4),=CL4'3600'                                             
         LA    R2,CARD             YES -- ADJUST IT IF NECESSARY                
         BRAS  RE,TCBCHNGE                                                      
         PUT   SYSIN,CARD                                                       
         CLI   TRACEFLG,YES        PRINT DETAILED TRACE?                        
         BNE   GJ270               NO                                           
         MVC   P(80),CARD                                                       
         GOTO1 VPRINTER                                                         
*                                  OFFPEAK PAGES TIME LIMIT                     
GJ270    MVC   CARD,SPACES                                                      
         MVC   CARD(9),=CL9'RTPPAGES='                                          
         MVC   CARD+9(5),=CL5'10000'       INCRE FROM 2000 TO 10000             
         PUT   SYSIN,CARD                                                       
         CLI   TRACEFLG,YES        PRINT DETAILED TRACE?                        
         BNE   GJ280               NO                                           
         MVC   P(80),CARD                                                       
         GOTO1 VPRINTER                                                         
*                                  OFFPEAK IO LIMIT                             
GJ280    MVC   CARD,SPACES                                                      
         MVC   CARD(8),=CL8'RTPEXCP='                                           
         MVC   CARD+8(7),=CL7'1000000'     INCRE FROM 150,000 TO 1 MIL          
         PUT   SYSIN,CARD                                                       
         CLI   TRACEFLG,YES        PRINT DETAILED TRACE?                        
         BNE   GJ290               NO                                           
         MVC   P(80),CARD                                                       
         GOTO1 VPRINTER                                                         
*                                  OFFPEAK CPU TIME LIMIT                       
GJ290    MVC   CARD,SPACES                                                      
         MVC   CARD(7),=CL7'RTPCPU='                                            
         MVI   CARD+7,C'6'                                                      
*                                                                               
         BRAS  RE,ARSOFF                                                        
         L     RF,VSSB                                                          
         LAM   AR3,AR3,SSOTBLET-SSOOFF(RF)                                      
         ICM   R3,15,AJOBNTRY      GET A(JOBTAB HEADER)                         
         BZ    GJ295                                                            
         SAC   512                                                              
         USING TBJOBTAB,R3                                                      
         CLC   =C'SI2',TBJMVSID+4                                               
         BE    GJ294                                                            
         CLC   =C'NI2',TBJMVSID+4                                               
         BNE   GJ295                                                            
GJ294    MVC   CARD+7(2),=C'30'                                                 
         DROP  R3                                                               
*                                                                               
GJ295    BRAS  RE,ARSOFF                                                        
*                                                                               
         PUT   SYSIN,CARD                                                       
         CLI   TRACEFLG,YES        PRINT DETAILED TRACE?                        
         BNE   GJ300               NO                                           
         MVC   P(80),CARD                                                       
         GOTO1 VPRINTER                                                         
*                                                                               
*                                                                               
GJ300    XC    DIRECTID,DIRECTID                                                
         MVI   UPDATIVE,NO         ASSUME NOT AN UPDATIVE SOON                  
*                                                                               
         MVC   OPMS1+27(15),=C'*NO "**" (EOF)*'                                 
GJ310    BRAS  R7,GETJCLR                                                       
         CLC   =C'**',R            END OF CARDS?                                
         BE    GJ400               YES                                          
*                                                                               
         CLI   LIMITFLG,NO         OFF-PEAK MODE (LIMIT=N) ?                    
         BNE   GJ310X              NO                                           
*                                                                               
*&&UK*&& B     GJ310X              NOT FOR UK                                   
*                                  YES - SKIP RUN-TIME CONTROL CARDS            
         CLC   =CL7'RTPTCB=',R                                                  
         BE    GJ310                                                            
         CLC   =CL9'RTPPAGES=',R                                                
         BE    GJ310                                                            
         CLC   =CL8'RTPEXCP=',R                                                 
         BE    GJ310                                                            
         CLC   =CL7'RTPCPU=',R                                                  
         BE    GJ310                                                            
GJ310X   DS    0H                                                               
*                                                                               
*&&UK                                                                           
         MVC   WORK(80),R          IGNORE BLANK REQUEST/CONTROL CARDS           
         OC    WORK(80),SPACES                                                  
         CLC   WORK(80),SPACES                                                  
         BE    GJ310                                                            
*&&                                                                             
         CLC   =C'DIRECT=**',R     APPEND TO SAME PQ REPORT?                    
         BNE   GJ320               NO                                           
         TM    AJCLBUF,X'80'       1ST STEP?                                    
         BNO   GJ320               YES                                          
         MVC   R(L'SVDTCARD),SVDTCARD   LOAD PREV STEP'S DIRECT CARD            
         MVC   R+51(4),=C'0001'    CODE FOR APPEND                              
         B     GJ350                                                            
*                                                                               
GJ320    CLC   =C'DIRECT=',R       IS THIS THE DIRECT= CARD?                    
         BNE   GJ330                                                            
         CLC   =C'ID=',R+34        YES -- DO WE HAVE THE PQ INFO HERE?          
         BE    *+14                                                             
         MVC   OPMS1+27(15),=C'*NO DIRECT ID=*'                                 
         B     GJBADX              NO -- BUT ALL SOON REQS MUST HAVE IT         
         MVC   DIRECTID,R+37       SAVE PQ INFO                                 
         MVC   SVDTCARD,R          SAVE DIRECT CARD FOR NEXT STEP               
         B     GJ350                                                            
*                                                                               
GJ330    CLC   =C'RTPTCB=',R       IS THIS AN RTPTCB CARD?                      
         BNE   *+16                                                             
         LA    R2,R                YES -- ADJUST IT IF NECESSARY                
         BRAS  RE,TCBCHNGE                                                      
         B     GJ350                                                            
*                                                                               
         CLC   =C'RECOVER=W',R     IS THIS AN UPDATIVE SOON?                    
         BNE   GJ340                                                            
*                                                                               
*WHEN ALL UPDATIVE SOON SET THE PQ TYPE CORRECTLY, UNCOMMENT THIS               
*        TM    SVPQTYPE,PQTYUPDT   PQ SAY THIS IS UPDATIVE SOON?                
*        BNO   GJ310               NO - DON'T WRITE THIS CARD TO SYSIN          
*                                                                               
         MVI   UPDATIVE,YES        YES                                          
         B     GJ350                                                            
*                                                                               
GJ340    CLC   =C'LOGO=',R         IS THIS A LOGO CARD?                         
         BNE   GJ345                                                            
         CLC   =C'ORIGIN=',R+33    IS THIS AN ORIGIN CARD?                      
         BNE   GJ345                                                            
         PACK  DUB,R+40(5)                                                      
         CVB   RE,DUB                                                           
         STCM  RE,3,ORGUSERN       SAVE USER ID #                               
         MVC   ORGJOBNM,R+19       SAVE JOB NAME                                
         B     GJ350                                                            
*                                                                               
GJ345    CLC   =C'COMSCORE=1',R                                                 
         BNE   GJ350                                                            
         MVI   COMSCORE,1                                                       
                                                                                
GJ350    PUT   SYSIN,R             PUT REQUEST CARD TO SYSIN FILE               
         CLI   TRACEFLG,YES        PRINT DETAILED TRACE?                        
         BNE   GJ310               NO                                           
         MVC   P(80),R                                                          
         GOTO1 VPRINTER                                                         
         B     GJ310                                                            
*                                                                               
GJ400    MVI   R,C'/'              REPLACE '**' WITH '/*' TO DENOTE EOF         
         PUT   SYSIN,R             PUT FINAL CARD TO SYSIN FILE                 
         CLI   TRACEFLG,YES        PRINT DETAILED TRACE?                        
         BNE   GJ410               NO                                           
         MVC   P(80),R                                                          
         GOTO1 VPRINTER                                                         
*                                                                               
GJ410    OC    TCBTIME,TCBTIME     WAS RTPTCB CARD GENERATED?                   
         BNZ   GJ500                                                            
*                                                                               
*&&UK*&& MVC   TCBTIME,=X'7FFF'    SET TCBTIME TO HIGH VALUES                   
*&&UK*&& B     GJ500                                                            
*                                                                               
         MVC   OPMS1+27(15),=C'*NO TIME GIVEN*'                                 
         B     GJBADX              NO                                           
*                                                                               
GJ500    MVC   OPMS1+27(15),SPACES NO ERRORS FOUND                              
*                                                                               
         CLOSE (SYSIN)                                                          
         LTR   RF,RF                                                            
         BZ    EXITOK              SET CC EQUAL                                 
         DC    H'0'                UNSUCCESSFUL CLOSE                           
*                                                                               
GJBADX   CLOSE (SYSIN)                                                          
         LTR   RF,RF                                                            
         BZ    EXITL               SET CC NOT EQUAL                             
         DC    H'0'                UNSUCCESSFUL CLOSE                           
*                                                                               
GETJCLR  CLI   0(R4),X'FF'                                                      
         BE    GJBADX              END OF JCLBUF BEFORE GOT ALL INFO            
         MVC   R(80),0(R4)                                                      
         AHI   R4,L'JCLBUF                                                      
         BR    R7                                                               
         EJECT                                                                  
CONTRLRS DS    0CL9                CONTROLLER TABLE                             
*                                  FIRST 8 BYTES IS CONTROLLER NAME             
*                                  NINTH BYTE IS NO. CHARACTERS MINUS 1         
*                                                                               
*&&US                                                                           
         DC    C'SPOOF   ',AL1(4)  SPOOFXXX                                     
         DC    C'SPONSOR ',AL1(6)  SPONSOR                                      
         DC    C'REPORTER',AL1(7)  REPORTER                                     
         DC    C'REPORTR ',AL1(6)  REPORTER (NEW 7-CHAR PANAPT SUPPORT)         
         DC    C'PPG     ',AL1(2)  PPG                                          
         DC    C'MONACC  ',AL1(5)  MONACC                                       
*&&                                                                             
*&&UK                                                                           
         DC    C'SPOOF   ',AL1(4)  SPOOF                                        
         DC    C'MONACC  ',AL1(5)  MONACC                                       
         DC    C'MERLIN  ',AL1(5)  MERLIN                                       
*&&                                                                             
         DC    X'FF'                                                            
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* R2 = A(NUMBER)                                                      *         
***********************************************************************         
CONVTIME NTR1  BASE=*,LABEL=*                                                   
         XC    DUB,DUB                                                          
         LA    RF,DUB                                                           
CT10     CLI   0(R2),C'0'          IS CHARACTER NUMERIC?                        
         BL    CT20                                                             
         CLI   0(R2),C'9'                                                       
         BH    CT20                                                             
         MVC   0(1,RF),0(R2)       YES -- SAVE IT                               
         LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
         B     CT10                                                             
*                                                                               
CT20     GOTO1 VNUMVAL,DMCB,DUB,(2,0)                                           
         CLI   DMCB,0              DID WE GET VALUE?                            
         BE    *+10                                                             
         XC    DMCB+4(4),DMCB+4    NO -- RETURN ZERO                            
         ST    R2,DMCB             A(STRING END)                                
         B     EXIT                                                             
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* AN RTPTCB= CARD IS ABOUT TO BE WRITTEN TO SYSIN.  HOWEVER, THE                
* TIME ON THE CARD MAY NEED TO BE ADJUSTED DEPENDING UPON WHICH CPU             
* IS RUNNING THIS MONSOON.  IF SO, CHANGE THE CARD AND RETURN.                  
* UPON ENTRY, R2 = A(RTPTCB CARD).                                              
***********************************************************************         
TCBCHNGE NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VNUMVAL,DMCB,7(R2),(2,0)                                         
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R0,DMCB+4           TCB TIME ON CARD                             
*                                                                               
         MH    R0,TCBPCT           (TCB TIME) X (&AGE ADJUSTMENT). . .          
         SRDL  R0,32               . . . DIVIDED BY 100. . .                    
         D     R0,=F'100'          . . . GIVES NEW TCB TIME IN R1               
         LTR   R0,R0               WAS THERE A REMAINDER?                       
         BZ    *+8                                                              
         LA    R1,1(R1)            YES -- ALWAYS ROUND UP                       
         STH   R1,TCBTIME          SAVE THE TCB TIME ALLOWED                    
         EDIT  (R1),(6,7(R2)),ALIGN=LEFT                                        
         B     EXITOK                                                           
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PUT WARNING MESSAGE ON CONSOLE                                      *         
***********************************************************************         
PUTWARN  NTR1  BASE=*,LABEL=*                                                   
         TIME  BIN                                                              
         S     R0,STRTTMRL         R0 = ELAPSED REAL TIME                       
         BNM   *+8                                                              
         A     R0,=A(24*3600*100)  ADD 24HRS, JUST WENT PAST MIDNIGHT           
         BRAS  RE,PRTTIME1         PUTS FORMATTED TIME IN WORK                  
         MVC   OPMS4+53(8),WORK                                                 
*                                                                               
         L     R1,AASCB            A(ASCB)                                      
         L     R0,ASCBEJST-ASCB(R1)                                             
         S     R0,STRTTMTC         R0 = ELAPSED TCB TIME                        
         BRAS  RE,PRTTIME2         PUTS FORMATTED TIME IN WORK                  
         MVC   OPMS4+40(8),WORK                                                 
*                                                                               
         MVC   OPMS4+16(18),OPMS1+8  UUUUUUUU,XXX,NNNNN                         
*                                                                               
         MVC   OPMS4+63(16),SPACES TELL OPERATOR IF IT'S UPDATIVE               
         CLI   UPDATIVE,YES                                                     
         BNE   *+10                                                             
         MVC   OPMS4+63(16),=C'*** UPDATIVE ***'                                
*                                                                               
         WTO   TEXT=OPMS4H                                                      
         MVC   P(L'OPMS4),OPMS4                                                 
         GOTO1 VPRINTER                                                         
         B     EXITOK                                                           
*                                                                               
OPMS4H   DC    AL2(80)                                                          
OPMS4    DC    CL80' '                                                          
         ORG   OPMS4                                                            
         DC    C'*** WARNING *** UUUUUUUU,XXX,NNNNN  TCB=HH.MM.SS  ET=H+        
               H.MM.SS'                                                         
         ORG                                                                    
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONVERT THE BINARY TIME (1/100TH SEC UNITS)IN PQTIME                *         
* TO X'HHMMSS' IN DUB(3)                                              *         
***********************************************************************         
CONVPQT  NTR1  BASE=*,LABEL=*                                                   
         SR    R0,R0                                                            
         ICM   R0,7,PQTIME         BINARY UNIT TIME IN FIRST 3 BYTES            
         SRDL  R0,32                                                            
         D     R0,=F'360000'                                                    
         STC   R1,DUB              HH                                           
         SRDL  R0,32                                                            
         D     R0,=F'6000'                                                      
         STC   R1,DUB+1            MM                                           
         SRDL  R0,32                                                            
         D     R0,=F'100'                                                       
         STC   R1,DUB+2            SS                                           
         B     EXITOK                                                           
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FORMAT TIME FOR PRINTOUT                                            *         
* NTRY: R0     = TIME IN 1/100S UNITS                                 *         
***********************************************************************         
PRTTIME1 NTR1  BASE=*,LABEL=*                                                   
         SRDL  R0,32               CONVERT SEC*100 TO SEC                       
         D     R0,=F'100'                                                       
         SR    R0,R0               LEAVES SECONDS IN R1                         
         D     R0,=F'3600'         GIVES HOURS IN R1                            
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(2),DUB                                                      
         MVI   WORK+2,C':'                                                      
         SRDL  R0,32                                                            
         D     R0,=F'60'           GIVES MINUTES IN R1/SECS IN R0               
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+3(2),DUB                                                    
         MVI   WORK+5,C':'                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+6(2),DUB                                                    
         B     EXITOK                                                           
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FORMAT TIME FOR PRINTOUT                                            *         
* NTRY: R0     = TIME IN 1S UNITS                                     *         
***********************************************************************         
PRTTIME2 NTR1  BASE=*,LABEL=*                                                   
         SRDL  R0,32                                                            
         D     R0,=F'3600'         GIVES HOURS IN R1                            
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(2),DUB                                                      
         MVI   WORK+2,C':'                                                      
         SRDL  R0,32                                                            
         D     R0,=F'60'           GIVES MINUTES IN R1/SECS IN R0               
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+3(2),DUB                                                    
         MVI   WORK+5,C':'                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+6(2),DUB                                                    
         B     EXITOK                                                           
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FORMAT TIME FOR PRINTOUT                                            *         
* NTRY:  DUB      = TOD TIME                                          *         
* EXIT:  WORK(12) = FORMATTED OUTPUT FIELD                            *         
***********************************************************************         
PRTTIME3 NTR1  BASE=*,LABEL=*                                                   
         STCKCONV STCKVAL=DUB,CONVVAL=WORK,TIMETYPE=DEC                         
*                                                                               
         MVI   WORK+6,X'0F'        HHMMSSTHMIJU+X'0F'                           
         ZAP   DUB,WORK(7)                                                      
         UNPK  WORK+20(15),DUB     C'HHMMSSTHMIJU000'                           
*                                                                               
         MVC   WORK(20),SPACES                                                  
         MVC   WORK+00(2),WORK+20  HH                                           
         MVI   WORK+02,C':'                                                     
         MVC   WORK+03(2),WORK+22  MM                                           
         MVI   WORK+05,C':'                                                     
         MVC   WORK+06(2),WORK+24  SS                                           
         MVI   WORK+08,C'.'                                                     
         MVC   WORK+09(3),WORK+26  THM                                          
         B     EXITOK                                                           
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* OUTPUT TRACE INFORMATION FOR LONG RUNNING SOON JOB                  *         
***********************************************************************         
*&&US                                                                           
LONGSOON NTR1  BASE=*,LABEL=*                                                   
         L     R0,ABIGBUF                                                       
         LHI   R1,BIGBUFLQ                                                      
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         OPEN  (SYSIN,INPUT)                                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK,WORK                                                        
         LA    R2,WORK                                                          
         USING FKEYD,R2                                                         
*                                                                               
         MVC   FKEYD(2),=C'**'                                                  
         MVC   FAGENCY,AGENCY                                                   
         MVC   FJOBUSER,USERID                                                  
         MVC   FJBSYPGM,SYPGM                                                   
         MVC   FATTPGM,ATTCHPGM                                                 
         GOTO1 VDATCON,DMCB,(5,0),(11,FDATE)                                    
         MVI   FDATE+L'FDATE,C'|'                                               
         TIME  DEC                                                              
         ST    R0,PRNTDUB                                                       
         MVI   PRNTDUB+4,X'0F'                                                  
         UNPK  PRNTTIME,PRNTDUB(5)                                              
         MVC   FHOUR,PRNTTIME                                                   
         MVI   FHOUR+L'FHOUR,C':'                                               
         MVC   FMIN,PRNTTIME+2                                                  
         MVI   FMIN+L'FMIN,C':'                                                 
         MVC   FSEC,PRNTTIME+4                                                  
         DROP  R2                                                               
*                                                                               
         L     R3,ABIGBUF                                                       
         MVC   0(L'BIGBUF,R3),WORK                                              
*                                                                               
LS02     LA    R3,L'BIGBUF(R3)                                                  
         C     R3,=A(EOBIGBUF)     END OF BIG BUFFER                            
         BNL   LS08                IGNORE THE REST CONTROL CARDS                
         GET   SYSIN,0(R3)                                                      
         B     LS02                GET NEXT LINE                                
*                                                                               
LS04     ICM   RF,15,SUBTIME                                                    
         CLI   SUBTIME+1,X'FF'     DISPLAY TIME REBUILT BY RERUN?               
         BNE   LS05                                                             
*                                                                               
         XR    RE,RE                                                            
         NILF  GRF,X'0000FFFF'                                                  
         SLL   RF,2                *4                                           
         D     RE,=F'3'            /3                                           
         MHI   RF,100                                                           
*                                                                               
LS05     ST    RF,SUBTIME                                                       
         L     R0,STRTTMRL                                                      
         S     R0,SUBTIME          WAIT TIME                                    
         BRAS  RE,PRTTIME1                                                      
         MVC   LSSTIME,WORK                                                     
*                                                                               
         L     R0,TOTETTM          ELAPSED TIME                                 
         BRAS  RE,PRTTIME1                                                      
         MVC   LSETIME,WORK                                                     
*                                                                               
         L     R0,TOTTCBTM         TCB TIME                                     
         BRAS  RE,PRTTIME2                                                      
         MVC   LSTTIME,WORK                                                     
*                                                                               
         TIMEUSED STORADR=TCPUE,LINKAGE=SYSTEM,CPU=MIC                          
*                                                                               
         LM    RE,RF,TCPUE                                                      
         SRDL  RE,12               CONVERT TO MICROSECONDS                      
         STM   RE,RF,TCPUE                                                      
*                                                                               
         LM    RE,RF,TCPUS                                                      
         SRDL  RE,12               CONVERT TO MICROSECONDS                      
         STM   RE,RF,TCPUS                                                      
*                                                                               
         LM    RE,RF,TCPUE                                                      
         LM    R0,R1,TCPUS                                                      
         SLR   RF,R1                                                            
         BNM   *+10                                                             
         LPR   RF,1                                                             
         AL    R0,=A(1)                                                         
         ST    RF,DUB+4                                                         
         SLR   RE,R0                                                            
         ST    RE,DUB                                                           
*                                                                               
         BRAS  RE,PRTTIME3         MONSTER CPU TIME                             
         MVC   LSMCPU,WORK                                                      
*                                                                               
         XC    FULL,FULL                                                        
         TIME  BIN                                                              
         STCM  R0,7,FULL+1                                                      
         L     R0,FULL                                                          
         S     R0,SUBTIME                                                       
         BRAS  RE,PRTTIME1                                                      
         MVC   LSTOTT,WORK                                                      
*                                                                               
LS06     L     RF,=A(JTCB)                                                      
         MVC   DUB,TCBTTIME-TCB(RF)                                             
                                                                                
         BRAS  RE,PRTTIME3                                                      
         MVC   LSJCPU,WORK                                                      
*                                                                               
         MVC   0(80,R3),JOBLINE    SAVE JOBINFO TO BUFFER                       
*                                                                               
LS08     CLOSE (SYSIN)                                                          
         LTR   RF,RF                                                            
         BZ    EXITOK                                                           
         DC    H'0'                                                             
*                                                                               
JOBLINE  DS    0XL80                                                            
         DC    C'WT='              WAIT TIME                                    
LSSTIME  DS    CL8' '                                                           
         DC    C',RT='             RUN TIME                                     
LSETIME  DS    CL8' '                                                           
         DC    C',TT='             TCB TIME                                     
LSTTIME  DS    CL8' '                                                           
         DC    C',MC='             MONSOON CPU                                  
LSMCPU   DS    CL12' '                                                          
         DC    C',JC='             JOBSTEP CPU                                  
LSJCPU   DS    CL12' '                                                          
         DC    C',TT='             TOTAL ELAPSED TIME                           
LSTOTT   DC    CL8' '                                                           
         ORG   JOBLINE+L'JOBLINE                                                
         LTORG                                                                  
         DROP  RB                                                               
*&&                                                                             
*&&UK                                                                           
LS04     DC    H'0'                DUMMY FOR UK                                 
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
DUPPQ    NTR1  BASE=*,LABEL=*                                                   
         CLI   ATTCHFLG,NO                                                      
         BE    DPQX                JOB DIDN'T RUN, DON'T COPY PQ                
         CLI   ABENDFLG,NO                                                      
         BNE   DPQX                JOB FAILED, DON'T COPY PQ                    
*                                                                               
         CLC   ORGSYPGM,=C'SB1'    ONLY COPY PQ REP FOR SB1 FOR NOW             
         BE    DUPPQ10                                                          
         CLC   ORGSYPGM,=C'NBU'    ELSE FOR NBU                                 
         BNE   DPQX                                                             
*                                                                               
DUPPQ10  XC    KEY,KEY             READ THE OUTPUT PROFILE                      
         LA    RE,KEY                                                           
         USING CTPREC,RE                                                        
         MVI   CTPKTYP,CTPKTYPQ                                                 
         MVC   CTPKSYS,ORGSYS                                                   
         CLI   ORGSYS,C'N'         USE 'S' FOR NET SYSTEM                       
         BNE   *+8                                                              
         MVI   CTPKSYS,C'S'                                                     
         MVC   CTPKPROG,ORGPGM                                                  
         MVC   CTPKORIG,ORGUSERN                                                
         DROP  RE                                                               
*                                                                               
DUPPQ20  GOTO1 VDMGR,DMCB,(0,DMREAD),CTFILE,KEY,A(IO),0                         
         CLI   DMCB+8,0                                                         
         BNE   DPQX                NO PROFILE RECORD                            
*                                                                               
         L     RE,AIO              FIND OUTPUT ID TYPE CODE X'42' ELEM          
         LA    RE,28(RE)                                                        
DUPPQ40  CLI   0(RE),0                                                          
         BE    DUPPQ60             NOT FOUND                                    
         CLI   0(RE),X'42'                                                      
         BE    DUPPQ80                                                          
         SR    R0,R0                                                            
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     DUPPQ40                                                          
*                                                                               
DUPPQ60  LA    RF,KEY                                                           
         USING CTPREC,RF                                                        
         OC    CTPKORIG,CTPKORIG                                                
         BZ    DPQX                NO OUTPUT ID TYPE CODE ELEM                  
         XC    CTPKORIG,CTPKORIG                                                
         B     DUPPQ20             CHECK FOR DEFAULT VALUE                      
         DROP  RF                                                               
*                                                                               
         USING CTOCOD,RE                                                        
DUPPQ80  CLI   CTOCOTYP,C'P'       PERMANENT TYPE?                              
         BNE   DPQX                NO - EXIT                                    
         CLI   CTOCODE+1,C'$'      CD-ROM OUTPUT TYPE?                          
         BNE   DPQX                NO - EXIT                                    
         DROP  RE                                                               
*                                                                               
*                                  READ THE USERID RECORD                       
         XC    KEY,KEY             BUILD ID RECORD KEY                          
         LA    RE,KEY                                                           
         USING CTIREC,RE                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID(2),=C'C$'                                                 
         MVC   CTIKID+2(8),USERID                                               
         DROP  RE                                                               
*                                                                               
         GOTO1 VDMGR,DMCB,(0,DMREAD),CTFILE,KEY,A(IO),0                         
         CLI   DMCB+8,0                                                         
         BNE   DPQX                NO ID RECORD                                 
*                                                                               
         L     RE,AIO                                                           
         LA    RE,28(RE)           FIND ID ELEMENT (X'02')                      
DUPPQ90  CLI   0(RE),0                                                          
         BE    DPQX                NO FOUND                                     
         CLI   0(RE),X'02'                                                      
         BE    DUPPQ100                                                         
         SR    R0,R0                                                            
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     DUPPQ90                                                          
*                                                                               
         USING CTDSCD,RE                                                        
DUPPQ100 MVC   INPQ,PQREPKEY                                                    
         XC    OUTPQ,OUTPQ                                                      
         LA    RF,OUTPQ                                                         
         USING PQPLD,RF                                                         
         MVC   QLSRCID,CTDSC       USERID NUM FOR "C$" USERID                   
         MVI   QLCLASS,C'$'                                                     
         DROP  RF,RE                                                            
*                                                                               
         MVC   P(10),=CL10'REPORT:'                                             
         MVC   P+10(7),INPQ                   INPUT PQ KEY                      
*                                                                               
         GOTO1 =V(PRTQCPY),DMCB,COMFACS,INPQ,0,OUTPQ,0,X'80000000'              
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P+20(10),=CL10'COPIED TO '                                       
         MVC   P+32(7),INPQ                   RETURNED PQ KEY                   
         GOTO1 VPRINTER                                                         
*                                                                               
*READ ORIGINAL PQ REPORT AND OVERRIDE THE DESC WITH COPIED PQ REP KEY           
*                                                                               
         XC    INDEX,INDEX         CLEAR ALL INDEX FIELDS                       
         LA    R2,INDEX                                                         
         USING UKRECD,R2                                                        
         MVC   UKKEY,PQREPKEY                                                   
         MVC   PQFILE,=CL8'PRTQUE'                                              
         GOTO1 VDMGR,DMCB,(0,GFILE),PQFILE,INDEX,0,CXREC                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PQFILE,UKUSRINF                                                  
         DROP  R2                                                               
*                                                                               
         XC    OUTPQ,OUTPQ                                                      
         GOTO1 VDMGR,DMCB,(0,=C'INDEX'),PQFILE,INDEX,OUTPQ,CXREC                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    OUTPQ,OUTPQ                                                      
         MVI   OUTPQ+4,C'L'       SILLY PARAMETER FOR RANDOM READ               
         GOTO1 VDMGR,DMCB,(0,RANDOM),PQFILE,INDEX,OUTPQ,CXREC 02649             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,INPQ                                                          
         LA    R3,OUTPQ                                                         
         USING UKRECD,R2                                                        
         USING PQPLD,R3                                                         
         GOTO1 VHEXOUT,DMCB,UKSRCID,QLDESC,2,=C'TOG'                            
         MVC   QLDESC+4(L'UKSUBID),UKSUBID                                      
         GOTO1 VHEXOUT,DMCB,UKREPNO,QLDESC+7,2,=C'TOG'                          
         DROP  R2,R3                                                            
         GOTO1 VDMGR,DMCB,(0,=C'DESC'),PQFILE,INDEX,OUTPQ,CXREC                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* READ THE COPIED PQ REP AND OVERRIDE THE DESC WITH JOBNAME                     
*                                                                               
         XC    INDEX,INDEX         CLEAR ALL INDEX FIELDS                       
         LA    R2,INDEX                                                         
         USING UKRECD,R2                                                        
         MVC   UKKEY,INPQ                                                       
         MVC   PQFILE,=CL8'PRTQUE'                                              
         GOTO1 VDMGR,DMCB,(0,GFILE),PQFILE,INDEX,0,CXREC                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PQFILE,UKUSRINF                                                  
         DROP  R2                                                               
*                                                                               
         XC    OUTPQ,OUTPQ                                                      
         GOTO1 VDMGR,DMCB,(0,=C'INDEX'),PQFILE,INDEX,OUTPQ,CXREC                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,OUTPQ                                                         
         USING PQPLD,RE                                                         
         MVC   QLDESC(L'ORGJOBNM),ORGJOBNM                                      
         DROP  RE                                                               
         GOTO1 VDMGR,DMCB,(0,=C'DESC'),PQFILE,INDEX,OUTPQ,CXREC                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,OUTPQ            SET LIVE RETAIN = 255 HRS                    
         USING PQPLD,RE                                                         
         MVC   QLRETNL,=X'00FF'                                                 
         DROP  RE                                                               
         GOTO1 VDMGR,DMCB,(0,=C'RELIVE'),PQFILE,INDEX,OUTPQ,CXREC 02700         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,OUTPQ            SET DEAD RETAIN = 255 HRS                    
         USING PQPLD,RE                                                         
         MVC   QLRETND,=X'00FF'                                                 
         DROP  RE                                                               
         GOTO1 VDMGR,DMCB,(0,=C'REDEAD'),PQFILE,INDEX,OUTPQ,CXREC 02709         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DPQX     B     EXITOK                                                           
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DURING TIMER POP CHECK FOR KILL COMMAND                             *         
***********************************************************************         
CHKKILL  NTR1  BASE=*,LABEL=*                                                   
         MVI   KILLFLG,NO                                                       
         BRAS  RE,LOCKJOB                                                       
         L     RF,VSSB                                                          
         LAM   AR3,AR3,SSOTBLET-SSOOFF(RF)                                      
         SAC   512                                                              
         L     R3,AJOBNTRY                                                      
         USING TBJNTRY,R3                                                       
         TM    TBJSTAT,TBJKILL                                                  
         BZ    CHKKX                                                            
*??      CLC   PQREPKEY,TBJPQKEY   IS IT THE SAME REPORT?                       
*??      BNE   CHKKX                                                            
*                                                                               
*        OI    TBJSTAT,TBJHOLD+TBJERROR                                         
         NI    TBJSTAT,X'FF'-TBJKILL                                            
         OI    TBJSTAT,TBJERROR                                                 
         MVI   KILLFLG,YES                                                      
         BRAS  RE,ARSOFF                                                        
         BRAS  RE,UNLKJOB                                                       
         B     EXITL                                                            
*                                                                               
CHKKX    BRAS  RE,ARSOFF                                                        
         BRAS  RE,UNLKJOB                                                       
         B     EXITOK                                                           
         DROP  R3                                                               
*                                                                               
NOKILL   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,LOCKJOB                                                       
         L     RF,VSSB                                                          
         LAM   AR3,AR3,SSOTBLET-SSOOFF(RF)                                      
         SAC   512                                                              
         ICM   R3,15,AJOBNTRY                                                   
         BZ    NOKKX                                                            
         USING TBJNTRY,R3                                                       
         TM    TBJSTAT,TBJKILL                                                  
         BZ    NOKKX                                                            
*??      CLC   PQREPKEY,TBJPQKEY   IS IT THE SAME REPORT?                       
*??      BNE   NOKKX                                                            
*                                                                               
         NI    TBJSTAT,255-TBJKILL                                              
         B     NOKKX                                                            
*                                                                               
NOKKX    BRAS  RE,ARSOFF                                                        
         BRAS  RE,UNLKJOB                                                       
         B     EXITOK                                                           
         DROP  R3                                                               
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* REPORT ON CURRENT STATUS OF MONSOON                                 *         
***********************************************************************         
EVENT    NTR1  BASE=*,LABEL=*                                                   
         CLI   0(R1),C'I'          INIT CALL                                    
         BNE   EVE010                                                           
*                                                                               
INIT010  LA    R2,FULL                                                          
         EXTRACT (2),'S',FIELDS=(ASID)                                          
         MVC   MVSASID,FULL+2                                                   
         SAM31                                                                  
         LA    R2,MVSJOB                                                        
         IAZXJSAB READ,WORKID=(R2) ACCOMODATE ASCH ENVIRONMENTS                 
         PACK  DUB,MVSJOB+3(5)                                                  
         CVB   R1,DUB                                                           
         STCM  R1,3,MVSJNUM        CONVERT TO JOBNO                             
         SAM24 ,                   SWITCH BACK TO 24 BT MODE                    
*                                                                               
         LA    R2,FULL             GET JOB NAME INTO MVSNAME                    
         EXTRACT (2),FIELDS=TIOT                                                
         L     R2,FULL                                                          
         MVC   MVSNAME,0(R2)                                                    
*                                                                               
         L     R1,X'10'(,0)        GET CPU ID INTO CPU                          
         L     R1,X'C4'(R1)        R1=A(SMCA) FROM CVT                          
         MVC   MVSCPU,16(R1)       CPU IS C'XXXX' FROM SMCASID                  
*                                                                               
         XC    DMCB(24),DMCB       GET SOON TABLE                               
         MVC   DMCB(4),=AL4(DTDSOON)                                            
         OI    DMCB,X'20'                                                       
         GOTO1 VLOCKSPC,DMCB                                                    
         ICM   RF,15,4(R1)         RF = COPY OF DSPACE 64 BYTES                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,7,61(RF)         A(DTDSOON)_                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ICM   R4,15,12(RF)        A(END)                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,VSSB                                                          
         LAM   AR3,AR3,SSOTBLET-SSOOFF(RF)                                      
         SAC   512                                                              
*                                                                               
         USING MONSOOND,R3                                                      
         LR    RF,R3               Save R3                                      
INIT011  CR    R3,R4               DON'T RUN OFF THE END                        
         BNL   INIT012             Not found, so find slot                      
         CLC   MVSCPUA,MONSCPU     SAME CPU/ASID AS ONE ALREADY                 
         BE    INIT015             REUSE - IT MUST HAVE DIED                    
         LA    R3,MONSNLGQ(,R3)    POINT TO FIRST / NEXT ENTRY                  
         B     INIT011                                                          
*                                                                               
INIT012  L     R0,=F'-1'           Set FFS to grab a free slot                  
         LR    R3,RF               Restore R3                                   
INIT014  CR    R3,R4               DON'T RUN OFF THE END                        
         BNL   EVEX                No slot available                            
         SR    RE,RE               Set for CS instr.                            
         CS    RE,R0,0(R3)         Try to lock it                               
         BE    INIT015             Got it                                       
         LA    R3,MONSNLGQ(,R3)    Someone has it so try next entry             
         B     INIT014                                                          
*                                                                               
INIT015  ST    R3,AMONSTAB         FILL IN INIT DETAIL                          
         MVC   MONSNAME,MVSNAME                                                 
         MVC   MONSNUM,MVSJNUM                                                  
         MVC   MONSCPU,MVSCPU                                                   
         MVC   MONSASID,MVSASID                                                 
         MVC   MONSJNUM,MVSJOB                                                  
         MVI   MONSACT,C'I'                                                     
         SAC   0                                                                
         TIME  BIN                 SET MONS START TIME                          
         SAC   512                                                              
         ST    R0,MONSTIME                                                      
         ST    R0,MONSLAST         LAST EVENT                                   
         XC    MONSCPUT,MONSCPUT                                                
         SAC   0                                                                
         B     EVEX                                                             
*                                                                               
EVE010   OC    AMONSTAB,AMONSTAB   MUST HAVE AN ENTRY                           
         BZ    EVEX                                                             
*                                                                               
         CLI   0(R1),C'A'          ATTACH EVENT                                 
         BNE   EVE020                                                           
         ST    R1,FULL                                                          
*                                                                               
         LH    R4,MVSASID          GET THE ASID                                 
         LOCASCB ASID=(R4)                                                      
         LR    R5,R1                                                            
         LTR   RF,RF                                                            
         BNZ   EVE015              ERROR FROM MACRO                             
*                                                                               
         USING ASCB,R5                                                          
         CLC   ASCBASCB,=C'ASCB'                                                
         BNE   EVE015              NOT AN ASCB                                  
         LM    R0,R1,ASCBEJST                                                   
         SRDL  R0,12               CONVERT TO MICROSEC IN R1                    
         D     R0,=F'10000'                                                     
         LR    RF,R1                                                            
         LM    R0,R1,ASCBSRBT                                                   
         SRDL  R0,12               CONVERT TO MICROSEC IN R1                    
         D     R0,=F'10000'                                                     
         AR    R1,RF               ADD CPU AND SRB TIME                         
*                                                                               
EVE015   L     RF,VSSB                                                          
         LAM   AR3,AR3,SSOTBLET-SSOOFF(RF)                                      
         SAC   512                                                              
         L     R3,AMONSTAB         FILL IN ATTACH DETAIL                        
         ST    R1,MONSCPUT                                                      
         L     R1,FULL                                                          
         MVI   MONSACT,C'R'                                                     
         MVC   MONSUSER,1(R1)                                                   
         MVC   MONSREP,9(R1)                                                    
         MVC   MONSRNUM,12(R1)                                                  
         MVC   MONSPROG,17(R1)                                                  
         MVC   MONSPOPT,REQTIMER                                                
         MVC   MONSECB,ECBLST      SAVE A(ECB)                                  
         MVC   MONSMVSJ,MVSID                                                   
         SAC   0                                                                
         TIME  BIN                 SET MONS ATTACH TIME                         
         SAC   512                                                              
         ST    R0,MONSSTIM         ATTACH                                       
         ST    R0,MONSLAST         LAST EVENT                                   
         ST    R0,MONSPTIM         POP                                          
         SAC   0                                                                
         B     EVEX                                                             
*                                                                               
EVE020   CLI   0(R1),C'P'          POP EVENT                                    
         BNE   EVE030                                                           
         TIME  BIN                                                              
         L     RF,VSSB                                                          
         LAM   AR3,AR3,SSOTBLET-SSOOFF(RF)                                      
         SAC   512                                                              
         L     R3,AMONSTAB                                                      
         ST    R0,MONSPTIM         SET TIME OF LAST POP                         
         SAC   0                                                                
         B     EVEX                                                             
*                                                                               
EVE030   CLI   0(R1),C'D'          DETACH EVENT                                 
         BNE   EVE040                                                           
         L     RF,VSSB                                                          
         LAM   AR3,AR3,SSOTBLET-SSOOFF(RF)                                      
         SAC   512                                                              
         L     R3,AMONSTAB                                                      
         XC    MONSJOBS,MONSJOBS   XC JOB INFO                                  
         SAC   0                                                                
         B     EVEX                                                             
*                                                                               
EVE040   CLI   0(R1),C'W'          WAIT EVENT                                   
         BNE   EVE050                                                           
         L     RF,VSSB                                                          
         LAM   AR3,AR3,SSOTBLET-SSOOFF(RF)                                      
         SAC   512                                                              
         L     R3,AMONSTAB                                                      
         MVI   MONSACT,C'W'                                                     
         SAC   0                                                                
         TIME  BIN                                                              
         SAC   512                                                              
         ST    R0,MONSLAST         LAST EVENT                                   
         SAC   0                                                                
         B     EVEX                                                             
*                                                                               
EVE050   CLI   0(R1),C'G'          GOODBYE                                      
         BNE   EVEX                                                             
         L     RF,VSSB                                                          
         LAM   AR3,AR3,SSOTBLET-SSOOFF(RF)                                      
         SAC   512                                                              
         L     R3,AMONSTAB                                                      
         XC    MONSTASK,MONSTASK   XC EVERYTHING                                
         XC    MONSJOBS,MONSJOBS                                                
         SAC   0                                                                
         B     EVEX                                                             
*                                                                               
EVEX     B     EXITOK                                                           
*                                                                               
MVSALL   DC    0CL10               MVS JOBNAME / NUMBER                         
MVSNAME  DC    CL8'        '       MVS JOB NAME                                 
MVSJNUM  DC    XL2'0000'           NUMERIC PART OF JOBID                        
*                                                                               
MVSCPUA  DS    0CL6                                                             
MVSCPU   DC    CL4'0000'           CPU ID                                       
MVSASID  DC    XL2'00'             ADDRESS SPACE ID                             
MVSJOB   DC    CL8'JOB00000'       JOB ID                                       
*                                                                               
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WRITE JCL TO DATASPACE TO USE IN RERUNS                             *         
***********************************************************************         
JCLTODSP NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    DMCB(24),DMCB       GET JOB TABLE HEADER FROM TABS               
         MVC   DMCB(4),=AL4(DTMONS)                                             
         GOTO1 VLOCKSPC,DMCB                                                    
*                                                                               
         L     R3,=AL4(DTMONS)                                                  
         NILF  GR3,X'00007FFF'                                                  
         SLL   R3,6                                                             
         L     RF,VSSB                                                          
         LAM   AR3,AR3,SSOTBLET-SSOOFF(RF)                                      
         SAC   512                                                              
         USING DMSPACED,R3                                                      
*                                                                               
         SR    R6,R6                                                            
         ICM   R6,15,DSPTFRST      R6=A(TABLE)                                  
         NILF  GR6,X'7FFFFFFF'                                                  
*                                                                               
         ICM   R4,15,DSPTEND       R4=A(END)                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   BYTE,0              USE BYTE TO FLAG SECOND PASS                 
*                                                                               
         ICM   RE,15,DSPUSER       RE=A(CURRENT)                                
         BNZ   *+6                                                              
         LR    RE,R6               OF USE TOP OF TABLE                          
*                                                                               
         LR    R5,R3               SAVE ENTRY IN R5                             
         LR    R3,RE               WORK ON TABLE NOW                            
*                                                                               
         USING PQRECD,R3                                                        
JDSP010  CLI   BYTE,2              THIRD PASS GRAB ANYTHING                     
         BE    JDSP020                                                          
         OC    PQKEY,PQKEY         FREE ENTRY                                   
         BZ    JDSP020                                                          
         TM    PQATTB,PQATERR      SKIP ERROR REPORTS                           
         BO    JDSPNXT                                                          
         TM    PQATTB,PQATJOBI     SKIP RUNNING REPORTS                         
         BO    JDSPNXT                                                          
*                                                                               
JDSP020  CPYA  ARE,AR3             COPY REPORT JCL TO BUFFER                    
         LR    RE,R3                                                            
         LA    R0,CXREC                                                         
         LHI   R1,4096                                                          
         LHI   RF,4096                                                          
         MVCL  RE,R0                                                            
         CPYA  ARE,AR0                                                          
         B     JDSP090                                                          
*                                                                               
JDSPNXT  AHI   R3,4096             TRY NEXT                                     
         CR    R3,R4                                                            
         BL    JDSP010                                                          
*                                                                               
         CLI   BYTE,0              FIRST PASS                                   
         BE    JDSP050                                                          
*                                                                               
         CLI   BYTE,2              IF THIRD PASS FAILS GIVE UP                  
         BE    JDSPXXX                                                          
*                                                                               
         LR    R3,R6               ROUND TO START                               
         MVI   BYTE,2              THIRD PASS                                   
         B     JDSP010                                                          
*                                                                               
JDSP050  LR    R3,R6               ROUND TO START                               
         MVI   BYTE,1              SECOND PASS                                  
         B     JDSP010                                                          
*                                                                               
JDSP090  LR    RF,R3                                                            
         ST    RF,AJCLNTRY                                                      
         AHI   RF,4096             NEXT ENTRY                                   
*                                                                               
         CR    RF,R4               IF > MAX THEN START OF TABLE                 
         BL    *+6                                                              
         LR    RF,R6                                                            
*                                                                               
         LR    R3,R5               SAVE NEW CURRENT ENTRY                       
         USING DMSPACED,R3                                                      
         STCM  RF,15,DSPUSER                                                    
         DROP  R3                                                               
*                                                                               
JDSPXXX  SAC   0                                                                
         XC    DMCB(24),DMCB       GET JOB TABLE HEADER FROM TABS               
         MVC   DMCB(4),=AL4(DTMONS)                                             
         OI    DMCB,X'10'                                                       
         GOTO1 VLOCKSPC,DMCB                                                    
*                                                                               
         B     EXITOK                                                           
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT A PRINT LINE                                      *         
***********************************************************************         
PRNT     NTR1  BASE=*,LABEL=*                                                   
         TIME  DEC                                                              
         ST    R0,PRNTDUB                                                       
         MVI   PRNTDUB+4,X'0F'                                                  
         UNPK  PRNTTIME,PRNTDUB(5)                                              
         MVC   P+18(2),PRNTTIME                                                 
         MVI   P+20,C':'                                                        
         MVC   P+21(2),PRNTTIME+2                                               
         MVI   P+23,C':'                                                        
         MVC   P+24(2),PRNTTIME+4                                               
         MVI   P+26,C'.'                                                        
         MVC   P+27(2),PRNTTIME+6                                               
         GOTO1 VPRINTER                                                         
         B     EXITOK                                                           
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* Relative code RC = GLOBAL area                                                
***********************************************************************         
LOCKJOB  CLI   LOCKEDJB,YES        Job table locked?                            
         BER   RE                  Yes                                          
         LR    R0,RE                                                            
         XC    DMCB(24),DMCB       JOB TABLE                                    
         LAY   RF,DTJOB            X'8005'                                      
         ST    RF,DMCB                                                          
         GOTOR VLOCKSPC,DMCB                                                    
         ICM   RF,15,4(R1)         LOCK JOB TABLE                               
         MVI   LOCKEDJB,YES                                                     
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
UNLKJOB  CLI   LOCKEDJB,NO         Locked ?                                     
         BER   RE                  Not locked                                   
         LR    R0,RE                                                            
         XC    DMCB(24),DMCB       JOB TABLE                                    
         LAY   RF,DTJOB            X'8005'                                      
         ST    RF,DMCB                                                          
         OI    DMCB,X'10'                                                       
         GOTOR VLOCKSPC,DMCB                                                    
         MVI   LOCKEDJB,NO         Set to not locked                            
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
LOCKCLS  CLI   LOCKEDCL,YES        Class table locked?                          
         BER   RE                  YES                                          
         LR    R0,RE                                                            
         XC    DMCB(24),DMCB       CLASS TABLE                                  
         LAY   RF,DTDCLASS         X'8027'                                      
         ST    RF,DMCB                                                          
         GOTOR VLOCKSPC,DMCB                                                    
         ICM   RF,15,4(R1)         LOCK CLASS TABLE                             
         MVI   LOCKEDCL,YES                                                     
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
UNLKCLS  CLI   LOCKEDCL,NO         Locked class table?                          
         BER   RE                  No, so don't unlock                          
         LR    R0,RE                                                            
         XC    DMCB(24),DMCB       CLASS TABLE                                  
         LAY   RF,DTDCLASS         X'8027'                                      
         ST    RF,DMCB                                                          
         OI    DMCB,X'10'                                                       
         GOTOR VLOCKSPC,DMCB                                                    
         MVI   LOCKEDCL,NO         Unlocked                                     
         LR    RE,R0                                                            
         BR    RE                                                               
***********************************************************************         
* COMMON STORAGE AREA                                                 *         
***********************************************************************         
         DS    0D                                                               
COMMWORK DC    CL8'*COMMON*'       COMMON STORAGE AREA                          
*                                                                               
EXITOK   CR    RB,RB                                                            
         J     EXIT                                                             
*                                                                               
EXITL    CLI   *,255                                                            
         J     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
ARSOFF   SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* LTIERAL POOL                                                        *         
***********************************************************************         
         EJECT                                                                  
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
*                                                                               
ARZERO   DC    16F'0'                                                           
VCPRINT  DC    V(CPRINT)                                                        
VDATCON  DC    V(DATCON)                                                        
VLOCKSPC DC    V(LOCKSPC)                                                       
VHEXIN   DC    V(HEXIN)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VCOVAIL  DC    V(COVAIL)                                                        
VLOADER  DC    V(LOADER)                                                        
VPRINTER DC    V(PRINTER)                                                       
VENQDEQ  DC    V(DMENQDEQ)                                                      
VISGENQ  DC    V(DMISGENQ)                                                      
VNUMVAL  DC    V(NUMVAL)                                                        
VDMGR    DC    V(DATAMGR)                                                       
VPRNTBL  DC    V(PRNTBL)                                                        
VSHMUSS  DC    V(DMSHMUSS)                                                      
*                                                                               
AUTL     DC    A(UTL)                                                           
ABIGBUF  DC    A(BIGBUF)                                                        
AJCLBUFF DC    A(JCLBUF)                                                        
VSSB     DC    A(SSB)                                                           
SVRE     DS    A                                                                
*                                                                               
DMOPEN   DC    CL8'DMOPEN  '                                                    
DMREAD   DC    CL8'DMREAD  '                                                    
DMWRT    DC    CL8'DMWRT   '                                                    
GLIST    DC    CL8'GLIST   '                                                    
GFILE    DC    CL8'GFILE   '                                                    
BUFF     DC    CL8'BUFFER  '                                                    
CONTROL  DC    CL8'CONTROL '                                                    
CTFILE   DC    CL8'CTFILE  '                                                    
RANDOM   DC    CL8'RANDOM  '                                                    
*                                                                               
CTFLIST  DC    C'NCTFILE '         CTFILE OPEN LIST                             
         DC    C'NGENDIR '                                                      
         DC    C'NGENFIL '                                                      
         DC    C'X'                                                             
*                                                                               
         DS    0F                                                               
DSPJTAB  DC    XL64'00'                                                         
DSPCTAB  DC    XL64'00'                                                         
*                                                                               
         EJECT                                                                  
**                                                                              
         ATBSERV                                                                
*                                                                               
JCLFILE  DCB   DDNAME=JCLFILE,MACRF=(GM,PM),DSORG=PS,RECFM=FB,LRECL=80,+        
               BLKSIZE=800                                                      
JCLFILE2 DCB   DDNAME=JCLFILE2,MACRF=(GM,PM),DSORG=PS,RECFM=FB,        +        
               LRECL=80,BLKSIZE=800                                             
TMPFILE  DCB   DDNAME=TMPFILE,MACRF=(GM,PM),DSORG=PS,RECFM=FB,LRECL=80,+        
               BLKSIZE=800                                                      
SYSIN    DCB   DDNAME=SYSIN,MACRF=(GM,PM),DSORG=PS,RECFM=FB,LRECL=80,  +        
               BLKSIZE=800,EODAD=LS04                                           
*                                                                               
COMFACS  DS    0F                                                               
         DC    V(DATAMGR)                                                       
*                                                                               
LOCKEDJB DC    AL1(NO)                                                          
LOCKEDCL DC    AL1(NO)                                                          
INPQ     DS    CL7                                                              
KEY      DS    CL(L'CTPKEY)                                                     
OUTPQ    DS    CL133                                                            
INDEX    DS    CL40                                                             
PQFILE   DS    CL8                                                              
SYPGM    DS    CL3                                                              
ECBLST   DC    A(TIMERECB)         A(TASK TIMER ECB)                            
         DC    X'80',AL3(TASKECB)  A(ATTACHED TASK ECB)                         
*                                                                               
ELCODE   DC    X'02'               ID ELEMENT                                   
         EJECT                                                                  
*                                                                               
DMWORK   DS    12D                                                              
DMCB     DS    6F                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
PRNTDUB  DS    D                   FOR PRNT MACRO                               
*                                                                               
TCPUS    DS    D                   START ELAPSED CPU TIME                       
TCPUE    DS    D                   END ELAPSED CPU TIME                         
*                                                                               
FULL     DS    F                                                                
FULL2    DS    F                                                                
MEMORY0  DC    F'0'                                                             
MEMORY1  DS    F                                                                
MEMORY2  DS    F                                                                
MEMMAIL# DC    F'0'                                                             
TOTTACC  DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
THREE    DS    XL3                                                              
PRNTTIME DS    CL9                 FOR PRNT MACRO                               
WORK     DS    CL80                                                             
WORK2    DS    CL80                                                             
THISJID  DS    CL8                 THIS TRANSACTION JOB ID                      
*                                                                               
JOBINFO  DS    CL80                THIS JOB INFORMATION                         
         ORG   JOBINFO                                                          
         DC    C'JOB NAME: '                                                    
JOBINAME DS    CL(L'JOBNAM)                                                     
         DC    C'ASID: '                                                        
JOBIASID DS    CL4                                                              
         DC    C'JOB ID: '                                                      
JOBIJID  DS    CL8                                                              
         ORG                                                                    
*                                                                               
AJTCB    DC    A(JTCB)                                                          
AMONSTAB DS    A                                                                
*                                                                               
ACITABLE DC    A(CITABLE)          A(PRINT QUEUE CI TABLE)                      
ACORETAB DC    A(CORETAB)          A(CORE-RESIDENT MODULE LIST)                 
AIO      DC    A(IO)                                                            
APQSAVE  DS    A                   A(PQ BUFFER SAVE AREA)                       
AASCB    DS    A                   A(ASCB) FROM EXTRACT                         
ACPUID   DS    A                   A(FOUR CHARACTER CPU ID)                     
AJOBNTRY DS    F                   A(JOBTAB ENTRY IN DSPACE)                    
AJCLNTRY DS    F                   A(JCL ENTRY IN DSPACE)                       
*                                                                               
USERID   DS    CL10                EBCDIC USERID OF SOON REQUESTOR              
PQREPKEY DS    0XL7                                                             
PQUSERID DS    XL2                 HEX USERID OF SOON REQUEST                   
PQSUB    DS    CL3                 SUB-ID                                       
PQREFNO  DS    XL2                 REFERENCE NUMBER                             
PQTIME   DS    XL3                 JOB SUBMIT TIME IN 1/100 SEC UNITS           
PQNUM    DS    C                   PRINT QUEUE (EBCDIC)                         
PQCLAS   DS    CL2                 CLASS                                        
PQPRTY   DS    C                   PRIORITY                                     
PQFACPAK DS    C                   FACPAK ID                                    
QTYPE    DS    C                   QTYPE: SHORT(S),MEDIUM(M),LONG(L)            
*                                                                               
TASKTCB  DS    F                   TCB OF ATTACHED TASK                         
TASKECB  DC    F'0'                ECB OF ATTACHED TASK                         
STRTTMTC DS    F                   STARTING TCB TIME OF ATTACHED TASK           
STRTTMRL DS    F                   STARTING REAL TIME OF ATTACHED TASK          
SUBTIME  DS    F                   SUBMIT TIME OF ORIGINAL JOB                  
REQTIMER DS    F                   INTERVAL TO WAIT BETWEEN WARNINGS            
TCBPCT   DC    H'100'              PERCENTAGE ADJUSTMENT ON TCB TIME            
TCBTIME  DS    H                   TCB TIME ALLOWED FOR THIS REQUEST            
APPCECB  DC    F'0'                ECB FOR APPC/MVS CALLS WITH MONSTER          
*                                                                               
AVOIDLDS DS    F                   TOTAL # OF LOADS AVOIDED IN THIS REQ         
TOTALLDS DS    F                   TOTAL # OF LOAD REQUESTS IN THIS REQ         
*                                                                               
SAVEPSW  DC    XL8'00'             SAVED PSW (FROM SDWA IN DDMASTER)            
SAVEREGS DS    0XL64                                                            
SAVER0   DC    F'0'                SAVED R0 (FROM SDWA IN DDMASTER)             
SAVER1   DC    F'0'                SAVED R1 (FROM SDWA IN DDMASTER)             
SAVER2   DC    F'0'                SAVED R2 (FROM SDWA IN DDMASTER)             
SAVER3   DC    F'0'                SAVED R3 (FROM SDWA IN DDMASTER)             
SAVER4   DC    F'0'                SAVED R4 (FROM SDWA IN DDMASTER)             
SAVER5   DC    F'0'                SAVED R5 (FROM SDWA IN DDMASTER)             
SAVER6   DC    F'0'                SAVED R6 (FROM SDWA IN DDMASTER)             
SAVER7   DC    F'0'                SAVED R7 (FROM SDWA IN DDMASTER)             
SAVER8   DC    F'0'                SAVED R8 (FROM SDWA IN DDMASTER)             
SAVER9   DC    F'0'                SAVED R9 (FROM SDWA IN DDMASTER)             
SAVERA   DC    F'0'                SAVED RA (FROM SDWA IN DDMASTER)             
SAVERB   DC    F'0'                SAVED RB (FROM SDWA IN DDMASTER)             
SAVERC   DC    F'0'                SAVED RC (FROM SDWA IN DDMASTER)             
SAVERD   DC    F'0'                SAVED RD (FROM SDWA IN DDMASTER)             
SAVERE   DC    F'0'                SAVED RE (FROM SDWA IN DDMASTER)             
SAVERF   DC    F'0'                SAVED RF (FROM SDWA IN DDMASTER)             
*                                                                               
ABENDCDS DS    0XL3                ABEND CODES WHICH DO NOT REQUIRE...          
*                                   ...A RESTART OF MONSOON                     
         DC    AL3(666)            OPERATOR STOP COMMAND                        
         DC    AL3(667)            RUN-TIME PARAMETER EXCEEDED                  
         DC    AL3(669)            BAD PATCH CARD                               
         DC    AL3(670)            BAD CPUID OR CONCURRENT FILE UPDATE          
         DC    AL3(899)            FRED ROE SPECIAL                             
         DC    X'222000'           OPERATOR CANCEL                              
         DC    X'13E000'           TIMED OUT BY MONSOON                         
         DC    X'FFFFFF'           EOT MARKER                                   
*                                                                               
ORGUSERN DS    XL2                 ORIGIN USERID NUMBER                         
ORGJOBNM DS    0CL7                ORIGIN JOB NAME                              
ORGAGYPW DS    CL4                 ORIGIN AGENCY POWER CODE                     
ORGSYPGM DS    0CL3                ORIGIN SYSTEM AND PROGRAM                    
ORGSYS   DS    CL1                 ORIGIN SYSTEM                                
ORGPGM   DS    CL2                 ORIGIN PROGRAM                               
*                                                                               
REQFLAGS DS    X                   VARIOUS FLAGS                                
REQ2STEP EQU   X'10'               REPORT CONTAIN 2ND EXEC STEP                 
*                                                                               
AGENCY   DS    CL2                 AGENCY CODE                                  
TOTTCBTM DS    F                   TOTAL TCB TIME USED                          
TOTETTM  DS    F                   TOTAL ET  TIME USED                          
MAXTCBTM DS    F                   MAX TCB TIME ALLOWED IN SEC                  
MAXETTM  DS    F                   MAX ET  TIME ALLOWED IN 0.01 SEC             
TCBTMFLG DC    AL1(NO)             SOON REQUEST EXCESS TCB TIME LIMIT           
ETTMFLG  DC    AL1(NO)             SOON REQUEST EXCESS ET  TIME LIMIT           
UPDFLG   DC    AL1(NO)             'Y' = UPDATIVE SOON W/ PQ UPD BIT ON         
ABENDFLG DC    AL1(NO)             SOON REQUEST COMPLETED W/ABEND               
TMOUTFLG DC    AL1(NO)             SOON REQUEST COMPLETED W/TIME OUT            
ATTCHFLG DC    AL1(NO)             JOB NOT ATTACH YET                           
RECYCLE  DC    AL1(NO)             RECYCLE THIS MONSOON                         
KILLFLG  DC    AL1(NO)             TASK WAS KILLED                              
FOUNDJOB DS    AL1(NO)             Lost job - reacquired?                       
COMSCORE DC    X'00'               COMSCORE=1 REQUEST                           
SVPQTYPE DS    C                   SAVE THE PQ TYPE                             
ERRMSG   DS    CL60                ERROR MSG RETURN TO MONSTER                  
REQSPP   DS    CL3                 REQUEST'S SYSTEM+PROGRAM (SPP)               
ATTCHPGM DS    CL8                 NAME OF PROGRAM TO ATTACH                    
SVDTCARD DS    CL80                SAVED PREVIOUS STEP DIRECT CARD              
DIRECTID DS    CL18                DIRECT CARD ID=                              
TRACEFLG DC    AL1(YES)            'Y' = PRINT DETAILED TRACE                   
JESMSG   DS    C                   'Y' = DO CONSOLE MSGS FOR START/END          
UPDATIVE DS    C                   'Y' = RECOVER=W SPECIFIED (UPDATIVE)         
LIMITFLG DC    AL1(YES)            'N' = OFF-PEAK MODE                          
STIMERID DS    XL4                 FOR TIMER POPS                               
PRTQUE   DC    C'PRTQU'            PRTQ NAME                                    
OPMS1H   DC    AL2(L'OPMS1)                                                     
OPMS1    DC    CL42'NO REQUEST RUNNING NOW'                                     
OPMS20   DS    CL42                                                             
CARD     DS    CL80                FOR INPUT/OUTPUT OF CONTROL CARDS            
RTPTCB   DC    CL80'RTPTCB='       RTPTCB= CARD FOR MASTER SYSIN                
DDSIOMOD DS    CL8                 DDSIO LOAD MODULE NAME                       
MNSOONID DS    CL8                 IDENTIFIES THE MONSOON IN DATASPACE          
DSPACE   DC    C' '                DSPACE PARAMETER FOR MASTER                  
FORCEVIS DC    AL1(NO)             FORCE VISIBLE WHEN JOB IS DONE               
DUMPWARN DC    AL1(YES)            'N' = NO WARNING FOR DUPLICATE DUMPS         
MVSID    DS    CL8                                                              
         DS    XL4                 PQ RECORD LENGTH                             
R        DS    CL150               PQ RECORD                                    
*                                                                               
       ++INCLUDE DDSHFIW           Shared Memory Working Storage                
         EJECT                                                                  
                                                                                
***********************************************************************         
* APPC/MVS CALL PARAMETERS                                                      
***********************************************************************         
CONVERSATION_TYPE              DS    F                                          
PARTNER_LU_NAME                DC    CL17' '                                    
LOCAL_LU_NAME                  DC    CL8' '                                     
MODE_NAME                      DC    CL8' '                                     
TP_NAME_LENGTH                 DS    F                                          
TP_NAME                        DC    CL64' '                                    
RETURN_CONTROL                 DS    F                                          
SYNC_LEVEL                     DS    F                                          
CONVERSATION_CORRELATOR        DS    CL8                                        
USER_ID                        DC    CL10' '                                    
PASSWORD                       DC    CL10' '                                    
PROFILE                        DC    CL10' '                                    
USER_TOKEN                     DC    XL80'00'                                   
CONVERSATION_ID                DS    CL8                                        
LUW_ID                         DS    XL26                                       
CONVERSATION_STATE             DS    F                                          
NOTIFY_TYPE                    DS    F                                          
                               DC    A(APPCECB) MUST FOLLOW NOTIFY TYPE         
RETURN_CODE                    DS    F                                          
FILL                           DS    F                                          
ERROR_DIRECTION                DS    F                                          
SEND_TYPE                      DS    F                                          
SEND_LENGTH                    DS    F                                          
SEND_ACCESS_TOKEN              DC    F'0'                                       
RECEIVE_LENGTH                 DS    F                                          
RECEIVE_ACCESS_TOKEN           DC    F'0'                                       
REQUEST_TO_SEND_VALUE          DS    F                                          
STATUS_RECEIVED                DS    F                                          
DATA_RECEIVED                  DS    F                                          
REQUEST_TO_SEND_RECEIVED       DS    F                                          
FUNCTION                       DC    F'2'   ASYNC_MANAGER CLEANUP               
ASYNCHRONOUS_NUMBER            DS    F                                          
BUFFER                         DS    CL256                                      
SERVICE_NAME                   DS    CL8                                        
SERVICE_REASON_CODE            DS    F                                          
MESSAGE_TEXT_LENGTH            DS    F                                          
MESSAGE_TEXT                   DS    CL256                                      
ERROR_LOG_PRODUCT_SET_ID_LENGTH DS   F                                          
ERROR_LOG_PRODUCT_SET_ID       DS    CL256                                      
ERROR_LOG_INFORMATION_LENGTH   DS    F                                          
ERROR_LOG_INFORMATION          DS    CL512                                      
REASON_CODE_ERROR_EXTRACT      DS    F                                          
RETURN_CODE_ERROR_EXTRACT      DS    F                                          
*                                                                               
**********************************************************************          
* PARAMETER LISTS FOR APPC/MVS CALLS                                            
*                                                                               
*  F    A(ROUTINE)                                                              
*  CL16 EBCDIC ROUTINE NAME                                                     
*  XL1  FLAGS                                                                   
*       X'80': ROUTINE IS SYNCHRONOUS ONLY                                      
*       X'40': ROUTINE CAN BE FOLLOWED BY ERROR_EXTRACT CALL                    
*  XL3  SPARE                                                                   
*  PARAMETERS (STANDARD IBM FORMAT)                                             
*                                                                               
**********************************************************************          
ATBGETC_STRUCTURE         DS    A                                               
                          DC    CL16'GET_CONVERSATION'                          
                          DC    X'C0'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(CONVERSATION_TYPE)                    
                          DC    X'00',AL3(PARTNER_LU_NAME)                      
                          DC    X'00',AL3(MODE_NAME)                            
                          DC    X'00',AL3(SYNC_LEVEL)                           
                          DC    X'00',AL3(CONVERSATION_CORRELATOR)              
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBCFMD_STRUCTURE         DS    A                                               
                          DC    CL16'CONFIRMED'                                 
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBRCVW_STRUCTURE         DS    A                                               
                          DC    CL16'RECEIVE_AND_WAIT'                          
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(FILL)                                 
                          DC    X'00',AL3(RECEIVE_LENGTH)                       
                          DC    X'00',AL3(RECEIVE_ACCESS_TOKEN)                 
                          DC    X'00',AL3(BUFFER)                               
                          DC    X'00',AL3(STATUS_RECEIVED)                      
                          DC    X'00',AL3(DATA_RECEIVED)                        
                          DC    X'00',AL3(REQUEST_TO_SEND_RECEIVED)             
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBGTA2_STRUCTURE         DS    A                                               
                          DC    CL16'GET_ATTRIBUTES'                            
                          DC    X'C0'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(PARTNER_LU_NAME)                      
                          DC    X'00',AL3(MODE_NAME)                            
                          DC    X'00',AL3(SYNC_LEVEL)                           
                          DC    X'00',AL3(CONVERSATION_CORRELATOR)              
                          DC    X'00',AL3(LUW_ID)                               
                          DC    X'00',AL3(TP_NAME_LENGTH)                       
                          DC    X'00',AL3(TP_NAME)                              
                          DC    X'00',AL3(LOCAL_LU_NAME)                        
                          DC    X'00',AL3(CONVERSATION_TYPE)                    
                          DC    X'00',AL3(USER_ID)                              
                          DC    X'00',AL3(PROFILE)                              
                          DC    X'00',AL3(USER_TOKEN)                           
                          DC    X'00',AL3(CONVERSATION_STATE)                   
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBSERR_STRUCTURE         DS    A                                               
                          DC    CL16'SEND_ERROR'                                
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(REQUEST_TO_SEND_RECEIVED)             
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'00',AL3(ERROR_DIRECTION)                      
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBGTRN_STRUCTURE         DS    A                                               
                          DC    CL16'GET_TRANSACTION'                           
                          DC    X'80'                                           
                          DS    XL3                                             
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBRTRN_STRUCTURE         DS    A                                               
                          DC    CL16'RTN_TRANSACTION'                           
                          DC    X'80'                                           
                          DS    XL3                                             
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBAMR1_STRUCTURE         DS    A                                               
                          DC    CL16'ASYNC_MANAGER'                             
                          DC    X'80'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(FUNCTION)                             
                          DC    X'00',AL3(ASYNCHRONOUS_NUMBER)                  
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBSEND_STRUCTURE         DS    A                                               
                          DC    CL16'SEND_DATA'                                 
                          DC    X'40'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(SEND_TYPE)                            
                          DC    X'00',AL3(SEND_LENGTH)                          
                          DC    X'00',AL3(SEND_ACCESS_TOKEN)                    
                          DC    X'00',AL3(BUFFER)                               
                          DC    X'00',AL3(REQUEST_TO_SEND_VALUE)                
                          DC    X'00',AL3(NOTIFY_TYPE)                          
                          DC    X'80',AL3(RETURN_CODE)                          
*                                                                               
ATBEES3_STRUCTURE         DS    A                                               
                          DC    CL16'ERROR_EXTRACT'                             
                          DC    X'80'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(CONVERSATION_ID)                      
                          DC    X'00',AL3(SERVICE_NAME)                         
                          DC    X'00',AL3(SERVICE_REASON_CODE)                  
                          DC    X'00',AL3(MESSAGE_TEXT_LENGTH)                  
                          DC    X'00',AL3(MESSAGE_TEXT)                         
                          DC X'00',AL3(ERROR_LOG_PRODUCT_SET_ID_LENGTH)         
                          DC    X'00',AL3(ERROR_LOG_PRODUCT_SET_ID)             
                          DC    X'00',AL3(ERROR_LOG_INFORMATION_LENGTH)         
                          DC    X'00',AL3(ERROR_LOG_INFORMATION)                
                          DC    X'00',AL3(REASON_CODE_ERROR_EXTRACT)            
                          DC    X'80',AL3(RETURN_CODE_ERROR_EXTRACT)            
         EJECT                                                                  
ENTRYPTS DS    0F                                                               
         DC    Y((ENTRYLSQ-ENTRYSTQ)/60)   NUMBER OF TABLE ENTRIES              
         DC    H'60'                       MUST REMAIN AS 60                    
ENTRYSTQ EQU   *                                                                
*                                                                               
ATBAMR1  DC    CL8'ATBAMR1'                                                     
         DC    XL52'00'                                                         
ATBCFMD  DC    CL8'ATBCFMD'                                                     
         DC    XL52'00'                                                         
ATBEES3  DC    CL8'ATBEES3'                                                     
         DC    XL52'00'                                                         
ATBGETC  DC    CL8'ATBGETC'                                                     
         DC    XL52'00'                                                         
ATBGTA2  DC    CL8'ATBGTA2'                                                     
         DC    XL52'00'                                                         
ATBGTRN  DC    CL8'ATBGTRN'                                                     
         DC    XL52'00'                                                         
ATBRCVW  DC    CL8'ATBRCVW'                                                     
         DC    XL52'00'                                                         
ATBRTRN  DC    CL8'ATBRTRN'                                                     
         DC    XL52'00'                                                         
ATBSEND  DC    CL8'ATBSEND'                                                     
         DC    XL52'00'                                                         
ATBSERR  DC    CL8'ATBSERR'                                                     
         DC    XL52'00'                                                         
ENTRYLSQ EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* CORE-RESIDENT PHASE TABLE                                           *         
***********************************************************************         
         ORG   MONSOON+(((*-MONSOON+15)/16)*16)                                 
         DC    CL16'*CORETAB*CORETAB'                                           
CORETAB  DS    0X                                                               
*                                                                               
*  TABLE FORMAT:                                                                
*    BYTE  1:     FLAGS                                                         
*                 X'FF' = END OF TABLE                                          
*                 X'80' = CALL THIS ROUTINE AT INITIALIZATION                   
*    BYTES 2..4:  PHASE NAME (E.G., 000A30 = T00A30 = GENCON)                   
*    BYTES 5..8:  A(PHASE)                                                      
*                                                                               
*&&US                                                                           
***      DC    X'00000A',AL1(QBOOKVAL),A(0)                                     
***      DC    X'00000A',AL1(QCENTER),A(0)                                      
***      DC    X'00000A',AL1(QCHOPPER),A(0)                                     
***      DC    X'00000A',AL1(QDAYVAL),A(0)                                      
***      DC    X'00000A',AL1(QDEMCON),A(0)                                      
***      DC    X'00000A',AL1(QDEMEX),A(0)                                       
***      DC    X'00000A',AL1(QDEMOTAB),A(0)                                     
***      DC    X'00000A',AL1(QDEMVAL),A(0)                                      
***      DC    X'00000A',AL1(QREDEMUP),A(0)                                     
***      DC    X'00000A',AL1(QINVEDIT),A(0)                                     
***      DC    X'00000A',AL1(QPAVCOND),A(0)                                     
***      DC    X'00000A',AL1(QPAVEXPL),A(0)                                     
***      DC    X'00000A',AL1(QSPOOL),A(0)                                       
***      DC    X'00000A',AL1(QSQUASH),A(0)                                      
***      DC    X'00000A',AL1(QTIMVAL),A(0)                                      
***      DC    X'00000A',AL1(QUNDAY),A(0)                                       
***      DC    X'00000A',AL1(QUNDRLIN),A(0)                                     
***      DC    X'00000A',AL1(QUNTIME),A(0)                                      
***      DC    X'00000A',AL1(QXSORT),A(0)                                       
***      DC    X'00000A',AL1(QUPVAL),A(0)                                       
***      DC    X'00000A',AL1(QCLPACK),A(0)                                      
***      DC    X'00000A',AL1(QCLUNPK),A(0)                                      
***      DC    X'00000A',AL1(QNETUNIV),A(0)                                     
***      DC    X'00000A',AL1(QNETWEEK),A(0)                                     
***      DC    X'00000A',AL1(QNETGTSP),A(0)                                     
***      DC    X'00000A',AL1(QNEUTIL),A(0)                                      
***      DC    X'00000A',AL1(QPARSNIP),A(0)                                     
***      DC    X'00000A',AL1(QMSPACK),A(0)                                      
***      DC    X'00000A',AL1(QMSUNPK),A(0)                                      
*********DC    X'00000A',AL1(QSTAPACK),A(0)                                     
***      DC    X'00000A',AL1(QBLDMGA),A(0)                                      
***      DC    X'00000A',AL1(QGETBROD),A(0)                                     
***      DC    X'00000A',AL1(QNETUNBK),A(0)                                     
***      DC    X'00000A',AL1(QGETDEM1),A(0)                                     
***      DC    X'00000A',AL1(QGETDEM2),A(0)                                     
***      DC    X'00000A',AL1(QSPDEMUP),A(0)                                     
***      DC    X'00000A',AL1(QNETSPVL),A(0)                                     
***      DC    X'00000A',AL1(QSPOON),A(0)                                       
***      DC    X'00000A',AL1(QDEFINE),A(0)                                      
***      DC    X'00000A',AL1(QNETIO),A(0)                                       
***      DC    X'00000A',AL1(QNETVALU),A(0)                                     
*********DC    X'00000A',AL1(QGENCON),A(0)                                      
***      DC    X'00000A',AL1(QDICTCON),A(0)                                     
***      DC    X'00000A',AL1(QGETNUN),A(0)                                      
***      DC    X'00000A',AL1(QGETHUT),A(0)                                      
***      DC    X'00000A',AL1(QNETGOAL),A(0)                                     
***      DC    X'00000A',AL1(QDRIVAL),A(0)                                      
***      DC    X'00000A',AL1(QGENPRG),A(0)                                      
***      DC    X'00000A',AL1(QOFFICER),A(0)                                     
***      DC    X'00000A',AL1(QDRONE),A(0)                                       
***      DC    X'00000A',AL1(QBUFFOON),A(0)                                     
***      DC    X'00000A',AL1(QMEDGEN),A(0)                                      
***      DC    X'00000A',AL1(QSPACNVL),A(0)                                     
***      DC    X'00000A',AL1(QSPOTIO),A(0)                                      
***      DC    X'00000A',AL1(QSPOTDRV),A(0)                                     
***      DC    X'00000A',AL1(QRANSID),A(0)                                      
***      DC    X'00000A',AL1(QSPOTBUY),A(0)                                     
***      DC    X'00000A',AL1(QSPOTBK),A(0)                                      
***      DC    X'00000A',AL1(QNEWRIGN),A(0)                                     
*********DC    X'00000A',AL1(QPRNTIO),A(0) BOBY SAYS NOT RE-ENTRANT             
***      DC    X'00000A',AL1(QQSORT),A(0)                                       
***      DC    X'00000A',AL1(QSPOTGL),A(0)                                      
***      DC    X'00000A',AL1(QSPWRIGN),A(0)                                     
***      DC    X'00000A',AL1(QPODDRIV),A(0)                                     
***      DC    X'00000A',AL1(QPDWRIGN),A(0)                                     
***      DC    X'00000A',AL1(QTSAR),A(0)                                        
***      DC    X'00000A',AL1(QOFFAL),A(0)                                       
***      DC    X'00000A',AL1(QADDTRN),A(0)                                      
***      DC    X'00000A',AL1(QNODIO),A(0)                                       
***      DC    X'00000A',AL1(QEDITOR),A(0)                                      
***      DC    X'00000A',AL1(QMOBILE),A(0)                                      
***      DC    X'00000A',AL1(QACCGEN),A(0)                                      
***      DC    X'00000A',AL1(QPROGEN),A(0)                                      
***      DC    X'00000A',AL1(QGETOPT),A(0)                                      
***      DC    X'00000A',AL1(QJOBBER),A(0)                                      
***      DC    X'00000A',AL1(QTASYSIO),A(0)                                     
***      DC    X'00000A',AL1(QTASYSVL),A(0)                                     
***      DC    X'00000A',AL1(QTASYSTB),A(0)                                     
***      DC    X'00000A',AL1(QTAREPGN),A(0)                                     
***      DC    X'00000A',AL1(QDROOL),A(0)                                       
***      DC    X'00000A',AL1(QTASYSCA),A(0)                                     
***      DC    X'00000A',AL1(QTACONCU),A(0)                                     
***      DC    X'00000A',AL1(QDDISP),A(0)                                       
***      DC    X'00000A',AL1(QDEMOVAL),A(0)                                     
***      DC    X'00000A',AL1(QDEMOMTH),A(0)                                     
***      DC    X'00000A',AL1(QDEMEL),A(0)                                       
***      DC    X'00000A',AL1(QDEMAINT),A(0)                                     
***      DC    X'00000A',AL1(QDEMAND),A(0)                                      
***      DC    X'00000A',AL1(QDEMAND1),A(0)                                     
***      DC    X'80000A',AL1(QDEMADDR),A(0)                                     
***      DC    X'00000A',AL1(QDEMOUT),A(0)                                      
***      DC    X'00000A',AL1(QDEMOCON),A(0)                                     
***      DC    X'00000A',AL1(QGENERAL),A(0)                                     
***      DC    X'00000A',AL1(QREPORT),A(0)                                      
***      DC    X'00000A',AL1(QGETIDS),A(0)                                      
***      DC    X'00000A',AL1(QPERVAL),A(0)                                      
***      DC    X'00000A',AL1(QGENIDS),A(0)                                      
***      DC    X'00000A',AL1(QGETRATE),A(0)                                     
*&&                                                                             
* DDMONSPARM MUST FOLLOW THE LAST CORERES TABLE ENTRY                           
       ++INCLUDE DDMONSPARM                                                     
         EJECT                                                                  
***********************************************************************         
* PRINT QUEUE BUFFER                                                  *         
***********************************************************************         
         ORG   MONSOON+(((*-MONSOON+15)/16)*16)                                 
         DC    CL16'*CXREC***CXREC**'                                           
CXREC    DC    14336X'00'          PRINT QUEUE BUFFER                           
         EJECT                                                                  
***********************************************************************         
* BUFFER FOR LONG SOON RECORD - MUST BE SAME AS FOR MONSTER           *         
***********************************************************************         
         ORG   MONSOON+(((*-MONSOON+15)/16)*16)                                 
         DC    CL16'*BIGBUF**BIGBUF*'                                           
BIGBUF   DS    (MAXRECBQ)CL80      BUFFER FOR LONG SOON REC                     
EOBIGBUF EQU   *                                                                
BIGBUFLQ EQU   *-BIGBUF                                                         
MAXRECBQ EQU   50                                                               
                                                                                
JTCB     DS    XL512                                                            
JTCBL    EQU   *-JTCB                                                           
         EJECT                                                                  
***********************************************************************         
* JCL BUFFER                                                          *         
***********************************************************************         
         ORG   MONSOON+(((*-MONSOON+15)/16)*16)                                 
         DC    CL16'*JCLBUF**JCLBUF*'                                           
JCLBUF   DS    (MAXRECQ)CL80       JCL BUFFER                                   
JCLBUFLQ EQU   *-JCLBUF                                                         
         DC    X'FF'                                                            
MAXRECQ  EQU   100                                                              
         EJECT                                                                  
***********************************************************************         
* I/O AREA                                                            *         
***********************************************************************         
         ORG   MONSOON+(((*-MONSOON+15)/16)*16)                                 
         DC    CL16'*I/O*I/O*I/O*I/O'                                           
IO       DS    2048X                                                            
         EJECT                                                                  
***********************************************************************         
* CI TABLE                                                            *         
***********************************************************************         
         DS    0D                                                               
         DC    C'CITABLE*'                                                      
CITABLE  DC    (MAXPQS*CITBLLNQ)X'FF'                                           
         DC    X'FF'               END OF TABLE MARKER                          
MAXPQS   EQU   16                  MAXIMUM OF 16 PRINT QUEUES                   
         EJECT                                                                  
***********************************************************************         
* DDSIO SUPPORT AREAS - SSB AND UTL - ALIGNED FOR EASY PERUSAL        *         
***********************************************************************         
         ORG   MONSOON+(((*-MONSOON+15)/16)*16)                                 
         DC    CL16'*SSB*SSB*SSB*SSB'                                           
       ++INCLUDE FASSBOFF                                                       
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
*                                                                               
         ORG   MONSOON+(((*-MONSOON+15)/16)*16)                                 
         DC    CL16'*UTL*UTL*UTL*UTL'                                           
UTL      DC    F'0',X'0A',X'251'                                                
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE CHAIN                                               *         
***********************************************************************         
         ORG   MONSOON+(((*-MONSOON+15)/16)*16)                                 
         DC    CL16'*RDCHAIN*RDCHAIN'                                           
R13CHAIN DS    10000D                                                           
         DC    CL16'*END OF RD BLK**'                                           
         DC    CL16'*END OF RD BLK**'                                           
         EJECT                                                                  
***********************************************************************         
* *********************************************************************         
***********************************************************************         
CIDATAD  DSECT                                                                  
CFPQENUM DS    X                   PRINT QUEUE EXTERNAL FILE NUMBER             
         DS    XL7                 SPARE                                        
       ++INCLUDE DMPRTQW                                                        
CITBLLNQ EQU   *-CIDATAD                                                        
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
* DMPRTQD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQD                                                        
         PRINT ON                                                               
* DMPRTQK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQK                                                        
         PRINT ON                                                               
* DMPRTQL                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQL                                                        
         PRINT ON                                                               
* DMPRTQS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQS                                                        
         PRINT ON                                                               
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
* FATABSJOB                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSJOB                                                      
         PRINT ON                                                               
* FATABSDEQU                                                                    
         PRINT OFF                                                              
       ++INCLUDE FATABSDEQU                                                     
         PRINT ON                                                               
* DDMONSBUFF                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDMONSBUFF                                                     
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDSHFID                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSHFID                                                        
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* IBM DSECTS                                                          *         
***********************************************************************         
TIOT     DSECT                                                                  
         IEFTIOT1                                                               
         EJECT                                                                  
*                                                                               
         IHAASCB                                                                
         EJECT                                                                  
*                                                                               
         IKJTCB                                                                 
         EJECT                                                                  
*                                                                               
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
*                                                                               
         IHAPSA                                                                 
         EJECT                                                                  
*                                                                               
         IHAASSB                                                                
         EJECT                                                                  
*                                                                               
         IHASTCB                                                                
         EJECT                                                                  
*                                                                               
         IAZJSAB                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'124DDMONSTM1 09/21/18'                                      
         END                                                                    
