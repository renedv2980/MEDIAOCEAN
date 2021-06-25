*          DATA SET DDMITMSAC  AT LEVEL 035 AS OF 07/17/13                      
*PHASE MITMSACA                                                                 
*SETOPT  PARM(REUS=RENT)                                                        
*INCLUDE DDWTO                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE ACRECTYP                                                               
*&&      SET   NOP=N                                                            
         IEABRCX DEFINE                                                         
         IEABRCX DISABLE                                                        
         TITLE 'USS MAN IN THE MIDDLE '                                         
         PRINT NOGEN                                                            
         REQUS                                                                  
MITMSRV  RSECT                                                                  
*        BASED ON NBASE ALLOWING WORK AREA AS PARM SO REENTRANT                 
         STM   RE,RC,12(RD)        SAVE MVS REGS                                
         BASR  RB,0                ESTABLISH BASE RB                            
         LA    RB,0(,RB)                                                        
         AHI   RB,-6                                                            
         USING MITMSRV,RB                                                       
         J     *+16                                                             
         DC    XL4'0'              ALIGN WITH NBASE                             
         DC    CL8'*MITMSR*'                                                    
         XR    RC,RC                                                            
         ICM   RC,7,5(R1)          PARAMETER IS ATTACH BLOCK                    
         USING ATTD,RC                                                          
         LAY   RE,ATTWORK+((ATTWORKL+7)/8)*8 SAVE CHAIN ROOT                    
         LA    RF,16(,RE)          CALLING SAVE AREA                            
         ST    RF,8(,RD)           FORWARD PTR FOR CALLER'S SAVE                
         ST    RD,4(,RF)           BACK PTR                                     
         LR    RD,RF               CALLING SAVE AREA                            
         MVC   0(4,RE),=C'RSRV'    SET UP ROOT STUB AS DDS STANDARD             
         XC    4(4,RE),4(RE)                                                    
         ST    RD,8(,RE)                                                        
         ST    R1,12(,RE)                                                       
*        END NBASE                                                              
         SAM31                                                                  
         J     START               GOTO START OF CODE                           
*                                                                               
$$DATA   LOCTR ,                   DATA LOCATION CLOSE TO RB                    
*                                                                               
*************************************************************                   
* RB = BASE1 LITERALS AND CONSTANTS                         *                   
* RA = RESERVED                                             *                   
* R9 = LOCAL BASE                                           *                   
* CONSTANTS & LITERALS DEFINE LATER USING $$DATA LOCTR      *                   
*************************************************************                   
*                                                                               
$$CODE   LOCTR ,                   CODE AFTER DATA                              
*                                                                               
***********************************************************************         
* MAIN CODE AREA STARTS AT RB+4096                                    *         
***********************************************************************         
*                                                                               
START    L     R1,=A(IOL-ATTD)     ADDRESS OUT OF RANGE WORK AREAS              
         AR    R1,RC                                                            
         ST    R1,AIOL                                                          
         L     R1,=A(BUFFER-ATTD)                                               
         AR    R1,RC                                                            
         ST    R1,ABUFFER                                                       
*                                                                               
         BRAS  RE,INIT             DO INITIALISATION (MITM,USS AND MQ)          
         JNE   XTASK                                                            
*                                                                               
         NI    ATTFLAGS,255-ATTFINI TELL CALLER WE ARE ACTIVE                   
*                                                                               
         BRAS  RE,MAIN             MAIN LOOP                                    
*                                                                               
XTASK    XR    R1,R1               CANCEL ANY TIMERS                            
         BRAS  RE,SETWAIT                                                       
*                                                                               
         GOTOR LOGIEND                                                          
*                                                                               
         LA    R2,PRINTDCB                                                      
         CLOSE ((R2)),MF=(E,OPLIST)                                             
*                                                                               
         LA    R2,XMLOUT                                                        
         CLOSE ((R2)),MF=(E,OPLIST)                                             
*                                                                               
         NI    ATTFLAGS,255-ATTFGO TELL CALLER WE FINISHED                      
*                                                                               
         XBASE RC=0                                                             
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS                                                         *         
***********************************************************************         
         SPACE 1                                                                
EXITL    LARL  R0,EXITL            SET CC LOW                                   
         CR    RB,R0                                                            
         J     EXIT                                                             
*                                                                               
EXITH    LTR   RB,RB               SET CC HIGH                                  
         J     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         J     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
EXITR1   XIT1  REGS=(R1)                                                        
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1                                                                   
*                                                                               
         CLI   ATTRSTRT,0          ARE WE BEING RESTARTED?                      
         JNE   INIT030             YES, STORAGE WILL BE INTACT                  
*                                                                               
         CLI   ATTDSPCE,C'A'       DSPACE=A                                     
         JE    INIT010                                                          
         MVC   QMGR,QMGRT          NO SET TEST DEFAULTS                         
         MVC   QUEUE,QUEUET                                                     
         MVC   QLABEL,QLABELA                                                   
         J     INIT020                                                          
*                                                                               
INIT010  MVC   QMGR,QMGRA          SET PROD DEFAULTS                            
         MVC   QUEUE,QUEUEA                                                     
         MVC   QLABEL,QLABELA                                                   
*                                                                               
INIT020  GOTOR DATETIME,ATTTLATT   SET FIRST AND LATEST TIMES                   
         OC    ATTTFATT,ATTTFATT                                                
         JNZ   *+10                                                             
         MVC   ATTTFATT,ATTTLATT                                                
*                                                                               
         XC    UNXQKEY,UNXQKEY     USS QUEUE KEY (0,DSPACE,0,SE)                
         MVI   UNXQTYPE,UNXQTY00                                                
         MVC   UNXQDSPC,ATTDSPCE                                                
         MVC   UNXQSE#+1(1),ATTSE                                               
*                                                                               
         LA    R1,MSGBUFF                                                       
         ST    R1,AMSGBUFF                                                      
*                                                                               
         XC    OPLIST,OPLIST       SINGLE ENTRY OPEN/CLOS LIST                  
         OI    OPLIST,X'80'        NEED TERMINATOR ON 1ST ENTRY                 
*                                                                               
INIT030  LARL  RE,PRTDCB                                                        
         MVC   PRINTDCB(L'PRINTDCB),0(RE) COPY SYSPRINT DCB                     
         MVC   PRINTDCB+43(5),ATTSYSN MODIFY DD NAME TO PRTXXXXX                
         LA    R1,PRINTDCB                                                      
         BRAS  RE,ALLOCPRN             WILL TRUNCATE ATTSYSN TO 5 CHARS         
         LA    R2,PRINTDCB                                                      
         OPEN  ((R2),OUTPUT),MF=(E,OPLIST)                                      
*                                                                               
         CLI   ATTTRACE,C'D'                                                    
         JNE   INIT040                                                          
         LARL  RE,PRTDCB                                                        
         MVC   XMLOUT(L'XMLOUT),0(RE) COPY SYSPRINT DCB                         
         MVC   XMLOUT+40(8),=C'XMLOUT  '                                        
         LA    R1,XMLOUT                                                        
         BRAS  RE,ALLOCPRN             WILL TRUNCATE ATTSYSN TO 5 CHARS         
         LA    R2,XMLOUT                                                        
         OPEN  ((R2),OUTPUT),MF=(E,OPLIST)                                      
*                                                                               
INIT040  MVI   NOTIME,C'N'                                                      
         MVI   PCC,C' '                                                         
         LARL  RE,SPACES                                                        
         MVC   PLINE,0(RE)                                                      
*                                                                               
         CLI   ATTRSTRT,0          ARE WE BEING RESTARTED?                      
         JE    *+12                                                             
         BRAS  RE,LOGRST           SE= RESTARTED                                
         J     *+8                                                              
         BRAS  RE,LOGSTR           SE= STARTED                                  
*                                                                               
         BRAS  RE,SCANPARM         LOOK FOR OVERRIDE PARMS                      
*                                                                               
         BRAS  RE,INITUSS          INITIALISE USS QUEUES                        
         BRAS  RE,INITMQ           INITIALISE MQ QUEUES                         
         JNE   EXIT                EXIT WITH CC                                 
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SCAN PARMETER CARDS                                                 *         
***********************************************************************         
         SPACE 1                                                                
SCANPARM NTR1                                                                   
*                                                                               
         L     R1,ATTPSCAN                                                      
         USING SCANPARMD,R1                                                     
         LH    R0,SCANPARMD_NUMENTS                                             
         LLC   RE,SCANPARMD_LEFTLEN                                             
         LLC   RF,SCANPARMD_RIGHTLEN                                            
         LA    RF,12(RE,RF)                                                     
         LA    R2,SCANPARMD_BLOCK                                               
         USING SCANBLKD,R2                                                      
*                                                                               
SCANP010 LARL  R3,CARDTAB                                                       
SCANP011 CLC   SC1STFLD(8),0(R3)                                                
         JE    SCANP050                                                         
         LA    R3,16(R3)                                                        
         CLI   0(R3),0                                                          
         JNE   SCANP011                                                         
*                                                                               
SCANP020 AR    R2,RF                                                            
         JCT   R0,SCANP010                                                      
         J     SCANPARX                                                         
*                                                                               
SCANP050 L     RE,8(R3)                                                         
         BR    RE                                                               
*                                                                               
         DS    0F                                                               
CARDTAB  DC    C'QMGR    ',AL4(SCANQMGR),AL4(0)                                 
         DC    C'QLABEL  ',AL4(SCANLABL),AL4(0)                                 
         DC    C'QUEUE   ',AL4(SCANQUEU),AL4(0)                                 
         DC    X'0000'                                                          
*                                                                               
SCANQMGR MVC   QMGR,SC2NDFLD                                                    
         J     SCANP020                                                         
SCANLABL MVC   QLABEL,SC2NDFLD                                                  
         J     SCANP020                                                         
SCANQUEU MVC   QUEUE,SC2NDFLD                                                   
         J     SCANP020                                                         
*                                                                               
SCANPARX J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MAIN PROGRAM CONTROL                                                *         
***********************************************************************         
         SPACE 1                                                                
MAIN     NTR1                                                                   
         BRAS  RE,OPENMQ           OPEN MQ CONNECTION                           
*                                                                               
MAIN02   BRAS  RE,QWAIT            WAIT FOR USS QUEUE                           
         JNE   MAIN990             DISCONNECT REQUIRED OR REQUESTED             
*                                                                               
TEMPO2   BRAS  RE,LOGDMP           LOG IF REQUIRED                              
*                                                                               
         GOTOR DATETIME,ATTTLRU    SET FIRST AND LATEST TIMES                   
         OC    ATTTFRU,ATTTFRU                                                  
         JNZ   *+10                                                             
         MVC   ATTTFRU,ATTTLRU                                                  
         LA    RF,ATTNRECU         COUNT USS RECORDS READ                       
         BRAS  RE,ADD1                                                          
         L     R0,USSLEN           COUNT USS BYTES READ                         
         LA    RF,ATTNBYTU                                                      
         BRAS  RE,ADDBYTES                                                      
*                                                                               
         SAM31                                                                  
         BRAS  RE,STRUCTUR         GET STRUCTURE DETAIL                         
         JNE   MAIN02              REJECTED ALREADY. COMPANY FLAG OFF           
*                                                                               
         BRAS  RE,FILTER           DO WE WANT THIS RECORD                       
         JNE   MAIN02                                                           
         SAM24                                                                  
*                                                                               
         BRAS  RE,THREAD           DO THREAD RELATED ACTIONS                    
         JNE   MAIN02                                                           
*                                                                               
         XC    ACHANGE,ACHANGE     SET NO CHANGE RECORD                         
*                                                                               
MAIN800  EQU   *                                                                
*        BRAS  RE,LOGDMP           LOG IF REQUIRED                              
         BRAS  RE,CONVERT          CONVERT TO OUTPUT DATA                       
*                                                                               
         BRAS  RE,SENDMQ           GOT USS QUEUE ITEM TO SEND                   
*                                                                               
         OC    ACHANGE,ACHANGE     DO WE HAVE A CHANGE TO DO TOO                
         JNZ   MAIN800                                                          
*                                                                               
         J     MAIN02                                                           
*                                                                               
MAIN990  BRAS  RE,CLOSMQ           CLOSE MQ CONNECTION                          
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET RECORD STRUCTURE                                                *         
***********************************************************************         
         SPACE 1                                                                
STRUCTUR NTR1                                                                   
*                                                                               
         XC    RECBLOCK,RECBLOCK   CLEAR OLD STRUCTURE                          
*                                                                               
         L     R2,AMSGBUFF                                                      
         LA    R2,4(,R2)           POINT TO 3CHR ID                             
         CLC   0(3,R2),=C'RCV'                                                  
         JNE   STRUCTY             ALL EOTS AND RBK PASS                        
*                                                                               
         LA    R2,7(,R2)           POINT TO RCVRHDR                             
         LA    R2,L'RECVHDR(,R2)   POINT TO KEY                                 
*                                                                               
         GOTO1 VACRECTY,DMCB,(C'D',(R2))                                        
         MVC   RECTYPE,0(R1)                                                    
         MVC   RECCOMP,1(R1)       SAVE COMPANY AND RECORD TYPE                 
*                                                                               
         L     R8,ATT31STO         GET 31 BIT PARMS                             
         USING ATT31_BLKD,R8                                                    
         L     R8,ATT31_ACC_COMPANY_TABLE                                       
*                                                                               
         USING CPYTABHD,R8         INDEX INTO COMPANY TABLE                     
         LLC   R1,RECCOMP          COMPANY NUMBER                               
         SLL   R1,2                X4                                           
         LA    R8,CPYTABH_INDEX(R1)                                             
*        LA    R8,CPYTABH_EYE(R1)                                               
         L     R8,0(R8)                                                         
         USING CPYTLEN,R8                                                       
         CLC   CPYCODE,RECCOMP                                                  
*NOP*    JNE   DEATH12             MUST MATCH                                   
         JNE   STRUCTX                                                          
*                                                                               
         CLI   ATTDSPCE,C'Q'       DSPACE=Q THEN ALL COMPANIES                  
         JE    *+12                                                             
         TM    CPYFLAGS,CPYFLAGS_RESOURCE_MANAGEMENT                            
         JZ    STRUCTN                                                          
*                                                                               
         MVC   RECALPHA,CPYAALPH                                                
         MVC   RECFLAGS,CPYFLAGS                                                
*                                                                               
         XC    RECSTRUC,RECSTRUC                                                
         LA    R8,CPYLEDGR         UP TO 20 LEDGERS IN TABLE                    
         LA    R0,20                                                            
         USING CPYLEDGR,R8                                                      
*                                                                               
STRUC010 CLI   RECTYPE,ACRTAEXT    JOBEXT                                       
         JE    STRUC011                                                         
         CLC   CPYUNITC(2),1(R2)   FIND THIS UNIT LEDGER ENTRY                  
         JE    STRUC020                                                         
         J     *+14                                                             
STRUC011 CLC   CPYUNITC(2),27(R2)  FIND THIS UNIT LEDGER ENTRY                  
         JE    STRUC020                                                         
         LA    R8,CPYLEDNQ(,R8)                                                 
         JCT   R0,STRUC010                                                      
         J     STRUCTY             NOT FOUND                                    
*                                                                               
STRUC020 MVC   RECSTRUC,CPYLEDST   GOT LEDGER STRUCTURE                         
*                                                                               
         MVC   RECSTRF,=C'BBBB'    INIT ALL BLANK                               
         LLC   R1,RECSTRUC                                                      
         BCTR  R1,0                                                             
         CLC   3(0,R2),SPACES      IS CLIENT SPACES                             
         EXRL  R1,*-6                                                           
         JE    *+8                                                              
         MVI   RECSTRF,C'N'        NON SPACE                                    
*                                                                               
         LLC   R1,RECSTRUC+1                                                    
         LLC   RF,RECSTRUC                                                      
         SR    R1,RF                                                            
         BCTR  R1,0                                                             
         AR    RF,R2                                                            
         CLC   3(0,RF),SPACES      IS PROD SPACES                               
         EXRL  R1,*-6                                                           
         JE    *+8                                                              
         MVI   RECSTRF+1,C'N'      NON SPACE                                    
*                                                                               
         LLC   R1,RECSTRUC+2                                                    
         LLC   RF,RECSTRUC+1                                                    
         SR    R1,RF                                                            
         BCTR  R1,0                                                             
         AR    RF,R2                                                            
         CLC   3(0,RF),SPACES      IS JOB SPACES                                
         EXRL  R1,*-6                                                           
         JE    *+8                                                              
         MVI   RECSTRF+2,C'N'      NON SPACE                                    
*                                                                               
         XR    R1,R1               LEVEL 4                                      
         ICM   R1,1,RECSTRUC+3                                                  
         JZ    STRUCTY                                                          
         LLC   RF,RECSTRUC+2                                                    
         SR    R1,RF                                                            
         BCTR  R1,0                                                             
         AR    RF,R2                                                            
         CLC   3(0,RF),SPACES                                                   
         EXRL  R1,*-6                                                           
         JE    *+8                                                              
         MVI   RECSTRF+2,C'N'      NON SPACE                                    
*                                                                               
STRUCTY  J     EXITOK                                                           
*                                                                               
STRUCTN  J     EXITL                                                            
*                                                                               
STRUCTX  BRAS  RE,LOGBAD           LOG BAD COMPANY CODE                         
         J     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER RECORDS FROM USS                                             *         
***********************************************************************         
         SPACE 1                                                                
FILTER   NTR1                                                                   
         LARL  R9,FILTER                                                        
         USING FILTER,R9                                                        
*                                                                               
         L     R2,AMSGBUFF                                                      
         LA    R2,4(,R2)           POINT TO 3CHR ID                             
         CLC   0(3,R2),=C'RCV'                                                  
         JNE   FILTERY             ALL EOTS AND RBK PASS                        
*                                                                               
         LA    R2,7(,R2)           POINT TO RCVRHDR                             
         LA    R2,L'RECVHDR(,R2)   POINT TO KEY                                 
*                                                                               
         CLI   RECTYPE,ACRTACTH    ACCOUNT HIGH                                 
         JNE   FILT100                                                          
*                                                                               
         CLC   ACTKUNT-ACTKEY(2,R2),=C'SJ'                                      
         JE    FILT030                                                          
         CLC   ACTKUNT-ACTKEY(2,R2),=C'1R'                                      
         JE    FILT040                                                          
         J     FILTERX                                                          
*                                                                               
FILT030  CLC   RECSTRF,=C'NBBB'    CLIENT RECORD                                
         JNE   FILT031                                                          
         MVC   RECTYPET,RTCLIENT   CLIENT                                       
         J     FILTERY                                                          
*                                                                               
FILT031  CLC   RECSTRF,=C'NNBB'    PRODUCT RECORD                               
         JNE   FILT032                                                          
         MVC   RECTYPET,RTPROD     PRODUCT                                      
         J     FILTERY                                                          
*                                                                               
FILT032  J     FILTERX                                                          
*                                                                               
FILT040  CLC   RECSTRF,=C'NBBB'    OFFICE RECORD                                
         JNE   FILT041                                                          
         MVC   RECTYPET,RTOFFICE   OFFICE                                       
         J     FILTERY                                                          
*                                                                               
FILT041  CLC   RECSTRF,=C'NNBB'    DEPT RECORD                                  
         JNE   FILT042                                                          
         MVC   RECTYPET,RTDEPT     DEPT                                         
         J     FILTERY                                                          
*                                                                               
FILT042  CLC   RECSTRF,=C'NNNB'    SUB DEPT RECORD                              
         JNE   FILT042                                                          
         MVC   RECTYPET,RTSUBDEP   SUB DEPT                                     
         J     FILTERY                                                          
*                                                                               
FILT043  J     FILTERX                                                          
*                                                                               
*        JOB AND NON CLIENT FILTER                                              
*                                                                               
FILT100  CLI   RECTYPE,ACRTACTL    ACCOUNT LOW (NON CLI OR JOB)                 
         JNE   FILT200                                                          
*                                                                               
         CLC   ACTKUNT-ACTKEY(2,R2),=C'SJ'                                      
         JE    FILT110                                                          
         CLC   ACTKUNT-ACTKEY(2,R2),=C'1N'                                      
         JE    FILT120                                                          
         J     FILTERX                                                          
*                                                                               
FILT110  MVC   RECTYPET,RTJOB      JOB                                          
         J     FILTERY                                                          
FILT120  MVC   RECTYPET,RTNONCLI   NONCLI                                       
         J     FILTERY                                                          
*                                                                               
FILT200  CLI   RECTYPE,ACRTAEXT    JOB EXTENSION                                
         JNE   FILT300                                                          
         CLC   AEXKULC-AEXKEY(2,R2),=C'SJ'                                      
         JNE   FILTERX                                                          
         MVC   RECTYPET,RTJOBEXT   JOB EXTENSION                                
         J     FILTERY                                                          
*                                                                               
FILT300  CLI   RECTYPE,ACRTWCO     WORK CODE                                    
         JNE   FILT400                                                          
         CLC   WCOKUNT-WCOKEY(2,R2),=C'SJ'                                      
         JNE   FILTERX                                                          
         MVC   RECTYPET,RTWORKCO                                                
         J     FILTERY                                                          
*                                                                               
FILT400  CLI   RECTYPE,ACRTCAS     PERIOD                                       
         JNE   FILT500                                                          
         MVC   RECTYPET,RTPERIOD                                                
         J     FILTERY                                                          
*                                                                               
FILT500  CLI   RECTYPE,ACRTPMD     MEDIA                                        
         JNE   FILT600                                                          
         MVC   RECTYPET,RTMEDIA                                                 
         J     FILTERY                                                          
*                                                                               
*                                                                               
FILT600  CLI   RECTYPE,ACRTPER     PERSON RECORD                                
         JNE   FILT700                                                          
         MVC   RECTYPET,RTRESOUR   RESOURCE                                     
         J     FILTERY                                                          
*                                                                               
FILT700  J     FILTERX                                                          
*                                                                               
FILTERY  J     EXITOK              KEEP                                         
*                                                                               
FILTERX  J     EXITL               DISCARD                                      
RQCLIENT EQU   001                                                              
RQPROD   EQU   002                                                              
RQOFFICE EQU   003                                                              
RQDEPT   EQU   004                                                              
RQSUBDEP EQU   005                                                              
RQJOB    EQU   006                                                              
RQRESOUR EQU   007                                                              
RQNONCLI EQU   008                                                              
RQJOBEXT EQU   009                                                              
RQWORKCO EQU   010                                                              
RQPERIOD EQU   011                                                              
RQMEDIA  EQU   012                                                              
*                                                                               
RTCLIENT DC    AL1(001),C'RecordClient    ' CLIENT                              
RTPROD   DC    AL1(002),C'RecordProduct   ' PRODUCT                             
RTOFFICE DC    AL1(003),C'RecordOffice    ' OFFICE                              
RTDEPT   DC    AL1(004),C'RecordDept      ' DEPT                                
RTSUBDEP DC    AL1(005),C'RecordSubDept   ' SUB DEPT                            
RTJOB    DC    AL1(006),C'RecordJob       ' JOB                                 
RTRESOUR DC    AL1(007),C'RecordResource  ' RESOURCE                            
RTNONCLI DC    AL1(008),C'RecordNonClient ' NON CLIENT                          
RTJOBEXT DC    AL1(009),C'RecordJobExt    ' JOB EXTENSION                       
RTWORKCO DC    AL1(010),C'RecordWorkCode  ' WORK CODE                           
RTPERIOD DC    AL1(011),C'RecordPeriod    ' PERIOD                              
RTMEDIA  DC    AL1(012),C'RecordMedia     ' MEDIA                               
         DROP  R9                                                               
         EJECT                                                                  
***********************************************************************         
* THREAD RELATED ACTIONS. SAVE ACTIVE GIN AND REPORT ON CMT OR RBK    *         
***********************************************************************         
         SPACE 1                                                                
THREAD   NTR1                                                                   
*                                                                               
         L     R2,AMSGBUFF                                                      
         LA    R3,4(,R2)           RCV CMT RBK                                  
         MVC   RECGIN,3(R3)                                                     
*                                                                               
         CLC   0(3,R3),=C'RCV'     RCV CHECK TABLE                              
         JE    THR005                                                           
         CLC   0(3,R3),=C'EOT'     EOT CLOSE THREAD                             
         JE    THR100                                                           
         CLC   0(3,R3),=C'RBK'     RBK CLOSE THREAD                             
         JE    THR100                                                           
         J     EXITOK              IGNORE ANYTHING ELSE                         
*                                                                               
THR005   LA    R4,11(,R2)          RECOVERY HEADER                              
         LR    R5,R3                                                            
         XR    RE,RE                                                            
         ICM   RE,3,3(R3)          REC LEN                                      
         BCTR  RE,0                                                             
         AR    R5,RE                                                            
         LLC   RE,0(R5)                                                         
         BCTR  RE,0                                                             
         SR    R5,RE               RECOVERY EXTENSION                           
         MVC   RECVHDR,0(R4)          COPY HEADER                               
         MVC   RECVEXT(RXLENQ),0(R5)  COPY EXTENSION                            
*                                                                               
         MVC   RECGIN,RGIN                                                      
THR010   LA    R2,GINTABL          FIND OR INSERT INTO GIN TABLE                
         LA    R0,32                                                            
         SR    RE,RE                                                            
*                                                                               
THR020   CLC   0(8,R2),RECGIN      GOT IT ALREADY                               
         JE    EXITOK                                                           
         CLC   0(8,R2),NULLS                                                    
         JNE   *+6                                                              
         LR    RE,R2               SAVE FREE ENTRY                              
         LA    R2,8(,R2)                                                        
         JCT   R0,THR020                                                        
*                                                                               
         LTR   RE,RE               GET FREE ONE                                 
         JZ    DEATH11                                                          
         MVC   0(8,RE),RECGIN      SAVE GIN                                     
         J     EXITOK                                                           
*                                                                               
THR100   LA    R2,GINTABL          FIND IN GIN                                  
         LA    R0,32                                                            
*                                                                               
THR120   CLC   0(8,R2),RECGIN      GOT IT                                       
         JE    THR130                                                           
         LA    R2,8(R2)                                                         
         JCT   R0,THR120                                                        
         J     EXITL               NOT THERE                                    
*                                                                               
THR130   XC    0(8,R2),0(R2)       CLEAR IT AND PASS ON                         
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* COPY RECORDS FROM USS MSGBUFF TO MQ BUFFER                          *         
***********************************************************************         
         SPACE 1                                                                
COPY     NTR1                                                                   
         L     R2,AMSGBUFF                                                      
         L     R1,USSLEN                                                        
         ST    R1,BUFFLEN                                                       
         L     RE,ABUFFER                                                       
         LR    RF,R1                                                            
         LA    R0,4(R2)                                                         
         MVCL  RE,R0                                                            
         L     RE,ABUFFER                                                       
         MVC   0(2,RE),RECTYPE                                                  
*                                                                               
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* CONVERT RECORDS TO XML                                              *         
***********************************************************************         
         SPACE 1                                                                
CONVERT  NTR1                                                                   
*                                                                               
         L     R2,AMSGBUFF                                                      
         LA    R2,4(,R2)           POINT TO RCV                                 
*                                                                               
CONVINIT L     R3,ABUFFER                                                       
         MVC   0(16,R3),QLABEL                                                  
*                                                                               
*DEBUG   MVC   0(16,R3),=C'RMOR************'                                    
*DEBUG   MVC   4(4,R3),RECSTRUC    DEBUG                                        
*DEBUG   MVC   8(4,R3),RECSTRF                                                  
*                                                                               
         LA    R3,16(,R3)                                                       
*                                                                               
         CLC   0(3,R2),=C'RCV'     IS IT AN RCV                                 
         JE    CONV010                                                          
*                                                                               
         MVC   0(11,R3),=C'<Record??? '                                         
         MVC   7(3,R3),0(R2)                                                    
         NC    8(2,R3),=X'BFBF'    Lower case                                   
         LA    R3,11(,R3)                                                       
         J     CONV020                                                          
*                                                                               
CONV010  MVC   0(1,R3),=C'<'                                                    
         MVC   1(16,R3),RECTYPE2                                                
         MVI   17(R3),C' '                                                      
         GOTOR XMLSPEC,PARMS,1(R3),16                                           
         LR    R3,R1                                                            
         MVI   0(R3),C' '                                                       
         LA    R3,1(,R3)                                                        
*                                                                               
CONV020  MVC   0(23,R3),=C'Gin="????????????????" '                             
         GOTOR VHEXOUT,PARMS,RECGIN,5(R3),8                                     
         LA    R3,23(,R3)                                                       
         CLC   0(3,R2),=C'RCV'     IS IT AN RCV                                 
         JNE   CONV025                                                          
*                                                                               
         MVC   0(34,R3),=C'TimeStamp="yyyymmdd hh:mm:ss.000" '                  
         GOTO1 VDATCON,PARMS,(3,RDATE),(20,11(R3))                              
*                                                                               
         MVC   FULL,RTIMEMS                                                     
         BRAS  RE,TIMEOUT                                                       
         MVC   20(12,R3),WORK1                                                  
         LA    R3,34(,R3)                                                       
         J     CONV030                                                          
*                                                                               
CONV025  MVC   0(2,R3),=C'/>'                                                   
         LA    R3,2(,R3)                                                        
         J     CONVERTZ                                                         
*                                                                               
CONV030  LA    R2,5(,R2)           POINT TO RECLEN                              
*                                                                               
         MVC   SAVSTAT,=C'**'                                                   
*                                                                               
         CLI   RRECTY,RRECTADD     ADD HAS ONLY ONE RECORD                      
         JE    CONV035                                                          
*                                                                               
         MVI   SAVSTAT,C'N'                                                     
         LR    RF,R2               RF=RECLEN                                    
         LA    RF,L'RECVHDR+42+4(,RF)                                           
         TM    0(RF),X'80'         COPY STATUS                                  
         JZ    *+8                                                              
         MVI   SAVSTAT,C'D'                                                     
*                                                                               
         MVI   SAVSTAT+1,C'N'                                                   
         SR    RE,RE                                                            
         ICM   RE,3,0(R2)                                                       
         AR    RE,R2               RE=RECLEN                                    
         LA    RE,L'RECVHDR+42+4(,RE)                                           
         TM    0(RE),X'80'         CHANGE STATUS                                
         JZ    *+8                                                              
         MVI   SAVSTAT+1,C'D'                                                   
*                                                                               
         OC    ACHANGE,ACHANGE     IS THS SECOND PASS FOR CHANGE                
         JZ    CONV031                                                          
         L     R2,ACHANGE          YES SO POINT TO IT                           
         XC    ACHANGE,ACHANGE     ONLY DO ONCE                                 
         J     CONV035                                                          
*                                                                               
CONV031  SR    RF,RF                                                            
         ICM   RF,3,0(R2)          MUST BE COPY CHANGE SO SKIP COPY             
         AR    RF,R2                                                            
*                                                                               
*NOP     ST    RF,ACHANGE          USE TO GET COPY RECORD TOO                   
*NOP     J     CONV035                                                          
*                                                                               
         LR    R2,RF               GO STRAIGHT TO CHANGE                        
         J     CONV035                                                          
*                                                                               
CONV035  LA    R2,2(,R2)           SKIP RECLEN                                  
         MVC   RECVHDR,0(R2)                                                    
         LA    R2,L'RECVHDR(,R2)   POINT TO KEY                                 
*                                                                               
CONV040  MVC   0(17,R3),=C'Action=          '                                   
         CLI   RRECTY,RRECTADD                                                  
         JE    CONV044                                                          
*                                                                               
         CLI   RRECTY,RRECTCPY                                                  
         JE    CONV041                                                          
*                                                                               
         CLC   SAVSTAT(2),=C'ND'                                                
         JNE   *+14                                                             
         MVC   7(8,R3),=C'"Delete"'                                             
         J     CONV042                                                          
*                                                                               
         CLC   SAVSTAT(2),=C'DN'   DEL TO N USE ADD                             
         JE    CONV044                                                          
         MVC   7(8,R3),=C'"Change"'                                             
CONV042  LA    R3,16(,R3)                                                       
         J     CONV045                                                          
*                                                                               
CONV041  MVC   7(6,R3),=C'"Copy"'                                               
         LA    R3,14(,R3)                                                       
         J     CONV045                                                          
*                                                                               
CONV044  MVC   7(5,R3),=C'"Add"'                                                
         LA    R3,13(,R3)                                                       
*                                                                               
CONV045  MVC   0(12,R3),=C'Agency="    '                                        
         MVC   8(2,R3),RECALPHA                                                 
         GOTOR XMLSPEC,PARMS,8(R3),2                                            
         LR    R3,R1                                                            
         MVI   0(R3),C'"'                                                       
         LA    R3,2(,R3)                                                        
*                                                                               
         LLC   R1,RECTYPE1         INDEX TO HANDLER                             
         SLL   R1,2                                                             
         LARL  RF,CONVNDX                                                       
         L     RF,0(R1,RF)                                                      
         BR    RF                  GOTO HANDLER - RETURNS TO CONVERTX           
*                                                                               
CONVNDX  DC    A(0)                                                             
         DC    A(CONV050)          CLIENT    1                                  
         DC    A(CONV051)          PRODUCT   2                                  
         DC    A(CONV500)          OFFICE    3                                  
         DC    A(CONV600)          DEPT      4                                  
         DC    A(CONV700)          SUBDEPT   5                                  
         DC    A(CONV100)          JOB       6                                  
         DC    A(CONV400)          RESOURC   7                                  
         DC    A(CONV150)          NONCLI    8                                  
         DC    A(CONV200)          JOBEXT    9                                  
         DC    A(CONV300)          WORKCOD   10                                 
         DC    A(CONV800)          PERIOD    11                                 
         DC    A(CONV900)          MEDIA     12                                 
*                                                                               
CONVERTX MVC   0(30,R3),SPACES                                                  
         MVC   0(2,R3),=C'</'                                                   
         MVC   2(16,R3),RECTYPE2                                                
         GOTOR XMLSPEC,PARMS,2(R3),16                                           
         LR    R3,R1                                                            
         MVI   0(R3),C'>'                                                       
         LA    R3,1(,R3)                                                        
*                                                                               
CONVERTZ LR    RE,R3               CALCULATE LEN AND EXIT                       
         S     RE,ABUFFER                                                       
         ST    RE,BUFFLEN                                                       
         J     EXITOK                                                           
         EJECT                                                                  
***************************************************                             
*        EDIT TUS IN FULL TO WORK HH:MM:SS.SS     *                             
***************************************************                             
         SPACE 1                                                                
TIMEOUT  ST    RE,SAVERE                                                        
         LARL  R9,TIMEOUT                                                       
         USING TIMEOUT,R9                                                       
         MVC   WORK1(12),=C'00:00:00.000'                                       
         SR    RE,RE                                                            
         L     RF,FULL                                                          
         D     RE,MSHOUR                                                        
         EDIT  (RF),(2,WORK1),FILL=0      HRS                                   
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,MSMINUTE                                                      
         EDIT  (RF),(2,WORK1+3),FILL=0    MINS                                  
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,MSSECOND                                                      
         EDIT  (RF),(2,WORK1+6),FILL=0    SECS                                  
         LR    RF,RE                                                            
         EDIT  (RF),(3,WORK1+9),FILL=0    1000/SEC                              
         L     RE,SAVERE                                                        
         BR    RE                                                               
MSHOUR   DC    F'3600000'                                                       
MSMINUTE DC    F'60000'                                                         
MSSECOND DC    F'1000'                                                          
         DROP  R9                                                               
         EJECT                                                                  
***********************************************************************         
* CLIENT AND PRODUCT RECORDS                                          *         
***********************************************************************         
         SPACE 1                                                                
CONV050  GOTOR CLIPROJB,PARMS,ACTKACT-ACTKEY(R2),(R3),1                         
         LR    R3,R1                                                            
         J     CONV052                                                          
*                                                                               
CONV051  GOTOR CLIPROJB,PARMS,ACTKACT-ACTKEY(R2),(R3),2                         
         LR    R3,R1                                                            
*                                                                               
CONV052  CLC   SAVSTAT(2),=C'ND'                                                
         JE    CONVERTX                                                         
*                                                                               
         GOTOR NAMEEL              DO NAME                                      
         GOTOR OFFICEL             DO OFFICE                                    
*                                                                               
CONV090  J     CONVERTX                                                         
         EJECT                                                                  
***********************************************************************         
* JOB RECORDS                                                         *         
***********************************************************************         
         SPACE 1                                                                
CONV100  GOTOR CLIPROJB,PARMS,ACTKACT-ACTKEY(R2),(R3),3                         
         LR    R3,R1                                                            
*                                                                               
         CLC   SAVSTAT(2),=C'ND'                                                
         JE    CONVERTX                                                         
*                                                                               
         GOTOR NAMEEL              DO NAME                                      
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('RSTELQ',(R2)),0                      
         CLI   12(R1),0                                                         
         JNE   CONV110                                                          
         L     RE,12(R1)                                                        
*                                                                               
         TM    RSTSTAT1-RSTELD(RE),RSTSACIL                                     
         JZ    *+14                                                             
         MVC   0(12,R3),=C'<IsLocked />'                                        
         LA    R3,12(,R3)                                                       
*                                                                               
         TM    RSTSTAT1-RSTELD(RE),RSTSACIC                                     
         JZ    *+14                                                             
         MVC   0(12,R3),=C'<IsClosed />'                                        
         LA    R3,12(,R3)                                                       
*                                                                               
         TM    RSTLSTAT-RSTELD(RE),RSTLSESQ                                     
         JZ    *+14                                                             
         MVC   0(18,R3),=C'<IsEstimateLock />'                                  
         LA    R3,18(,R3)                                                       
*                                                                               
         TM    RSTLSTAT-RSTELD(RE),RSTLSTIQ                                     
         JZ    *+14                                                             
         MVC   0(19,R3),=C'<IsTimesheetLock />'                                 
         LA    R3,19(,R3)                                                       
*                                                                               
         TM    RSTSTAT5-RSTELD(RE),RSTSNOTS                                     
         JZ    *+14                                                             
         MVC   0(19,R3),=C'<IsTimesheetExcl />'                                 
         LA    R3,19(,R3)                                                       
*                                                                               
         TM    RSTSTAT7-RSTELD(RE),RSTSTHRP                                     
         JZ    *+14                                                             
         MVC   0(14,R3),=C'<Is3rdParty />'                                      
         LA    R3,14(,R3)                                                       
*                                                                               
CONV110  GOTO1 VHELLO,DMCB,(C'G',ACCMST),('JOBELQ',(R2)),0                      
         CLI   12(R1),0                                                         
         JNE   CONV130                                                          
         L     R4,12(R1)                                                        
*                                                                               
         TM    JOBSTA1-JOBELD(R4),JOBSXJOB                                      
         JZ    *+14                                                             
         MVC   0(17,R3),=C'<IsExpensesJob />'                                   
         LA    R3,17(,R3)                                                       
*                                                                               
         CLC   JOBODATE-JOBELD(L'JOBODATE,R4),NULLS                             
         JZ    CONV120                                                          
         MVC   0(10,R3),=C'<DateOpen>'                                          
         LA    R3,10(,R3)                                                       
         GOTO1 VDATCON,PARMS,(1,JOBODATE-JOBELD(R4)),(20,0(R3))                 
         LA    R3,8(,R3)                                                        
         MVC   0(11,R3),=C'</DateOpen>'                                         
         LA    R3,11(,R3)                                                       
*                                                                               
CONV120  CLC   JOBCDATE-JOBELD(L'JOBCDATE,R4),NULLS                             
         JZ    CONV130                                                          
         MVC   0(11,R3),=C'<DateClose>'                                         
         LA    R3,11(,R3)                                                       
         GOTO1 VDATCON,PARMS,(1,JOBCDATE-JOBELD(R4)),(20,0(R3))                 
         LA    R3,8(,R3)                                                        
         MVC   0(12,R3),=C'</DateClose>'                                        
         LA    R3,12(,R3)                                                       
*                                                                               
CONV130  MVI   WORK,SPATMJOB                                                    
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('SPAELQ',(R2)),(1,WORK)               
         CLI   12(R1),0                                                         
         JNZ   CONV140                                                          
         L     RF,12(R1)                                                        
*                                                                               
         MVC   0(11,R3),=C'<MasterJob>'                                         
         LA    R3,11(,R3)                                                       
         GOTOR CLIPROEL,PARMS,SPAAACT-SPAELD(RF),(R3)                           
         LR    R3,R1                                                            
         MVC   0(12,R3),=C'</MasterJob>'                                        
         LA    R3,12(,R3)                                                       
*                                                                               
CONV140  MVC   0(10,R3),=C'<Approval>'                                          
         LA    R3,10(,R3)                                                       
         TM    ACTRSTAT-ACTKEY(R2),ACTSDRFT                                     
         JO    CONV145                                                          
         MVI   0(R3),C'3'                                                       
         LA    R3,1(,R3)                                                        
         J     CONV149                                                          
*                                                                               
CONV145  TM    JOBSTA2-JOBELD(R4),JOBSREJ                                       
         JNO   CONV146                                                          
         MVI   0(R3),C'2'                                                       
         LA    R3,1(,R3)                                                        
         J     CONV149                                                          
*                                                                               
CONV146  MVI   0(R3),C'1'                                                       
         LA    R3,1(,R3)                                                        
*                                                                               
CONV149  MVC   0(11,R3),=C'</Approval>'                                         
         LA    R3,11(,R3)                                                       
*                                                                               
         TM    JOBSTA2-JOBELD(R4),JOBSMST                                       
         JZ    *+14                                                             
         MVC   0(12,R3),=C'<IsMaster />'                                        
         LA    R3,12(,R3)                                                       
*                                                                               
         J     CONVERTX                                                         
         EJECT                                                                  
***********************************************************************         
* NON CLIENT                                                          *         
***********************************************************************         
         SPACE 1                                                                
CONV150  MVC   0(24,R3),=C'NonClient="............ '                            
         MVC   11(12,R3),ACTKULA-ACTKEY(R2)   COPY IN ACTKULA FROM KEY          
         GOTOR XMLSPEC,PARMS,11(R3),12                                          
         LR    R3,R1                                                            
         MVC   0(2,R3),=C'">'                                                   
         LA    R3,2(,R3)                                                        
*                                                                               
         CLC   SAVSTAT(2),=C'ND'                                                
         JE    CONVERTX                                                         
*                                                                               
         GOTOR NAMEEL              DO NAME                                      
*                                                                               
         GOTOR OFFICEL             DO OFFICE                                    
*                                                                               
         J     CONVERTX                                                         
         EJECT                                                                  
***********************************************************************         
* JOB EXTENSION                                                       *         
***********************************************************************         
         SPACE 1                                                                
CONV200  GOTOR CLIPROJB,PARMS,AEXKACC-AEXKEY(R2),(R3),3                         
         LR    R3,R1                                                            
*                                                                               
         CLC   SAVSTAT(2),=C'ND'                                                
         JE    CONVERTX                                                         
*                                                                               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('JLDELQ',(R2)),0                      
         CLI   12(R1),0                                                         
         JNE   CONVERTX                                                         
         L     R4,12(R1)                                                        
*                                                                               
         USING JLDELD,R4                                                        
CONV210  MVC   0(13,R3),=C'<Description>'                                       
         LA    R3,13(,R3)                                                       
         LLC   RF,JLDLN                                                         
         SHI   RF,JLDLNQ                                                        
         MVC   0(0,R3),JLDDESC                                                  
         EXRL  RF,*-6                                                           
         GOTOR XMLSPEC,PARMS,0(R3),(RF)                                         
         LR    R3,R1                                                            
         MVC   0(14,R3),=C'</Description>'                                      
         LA    R3,14(,R3)                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,JLDLN                                                       
         JZ    CONVERTX                                                         
         AR    R4,R0                                                            
         CLI   JLDEL,JLDELQ                                                     
         JE    CONV210                                                          
*                                                                               
         J     CONVERTX                                                         
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* WORK CODE                                                           *         
***********************************************************************         
         SPACE 1                                                                
CONV300  MVC   0(13,R3),=C'WorkCode=".. '                                       
         MVC   10(2,R3),WCOKWRK-WCOKEY(R2) CODE FROM KEY                        
         GOTOR XMLSPEC,PARMS,10(R3),2                                           
         LR    R3,R1                                                            
         MVC   0(2,R3),=C'">'                                                   
         LA    R3,2(,R3)                                                        
*                                                                               
         CLC   SAVSTAT(2),=C'ND'                                                
         JE    CONVERTX                                                         
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('WCOELQ',(R2)),0                      
         CLI   12(R1),0                                                         
         JNE   CONVERTX                                                         
         L     R4,12(R1)                                                        
*                                                                               
         MVC   0(6,R3),=C'<Name>'                                               
         LA    R3,6(,R3)                                                        
         MVC   0(15,R3),WCODESC-WCOEL(R4)                                       
         GOTOR XMLSPEC,PARMS,0(R3),15                                           
         LR    R3,R1                                                            
         MVC   0(14,R3),=C'</Name>'                                             
         LA    R3,7(,R3)                                                        
*                                                                               
*&&UK*&& TM    WCOSTAT3-WCOEL(R4),WCOKTHRD                                      
*&&US*&& TM    WCOSTAT2-WCOEL(R4),WCOSTHRD                                      
         JZ    *+14                                                             
         MVC   0(14,R3),=C'<Is3rdParty />'                                      
         LA    R3,14(,R3)                                                       
*                                                                               
         J     CONVERTX                                                         
         EJECT                                                                  
***********************************************************************         
* RESOURCE??                                                          *         
***********************************************************************         
         SPACE 1                                                                
CONV400  MVC   0(17,R3),=C'Person="........ '                                   
         MVC   8(8,R3),PERKCODE-PERKEY(R2)                                      
         GOTOR XMLSPEC,PARMS,8(R3),8                                            
         LR    R3,R1                                                            
         MVC   0(2,R3),=C'">'                                                   
         LA    R3,2(,R3)                                                        
*                                                                               
         CLC   SAVSTAT(2),=C'ND'                                                
         JE    CONVERTX                                                         
*                                                                               
         MVI   WORK,GPNTFST                                                     
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('LOCELQ',(R2)),0                      
         CLI   12(R1),0                                                         
         JNE   CONV410                                                          
         L     R4,12(R1)                                                        
*                                                                               
CONV405  CLC   LOCEND-LOCELD(L'LOCEND,R4),NULLS                                 
         JE    CONV407                                                          
         ST    R4,FULL                                                          
         XR    R0,R0               NEXT ELEMENT                                 
         ICM   R0,1,1(R4)                                                       
         JZ    CONV410                                                          
         AR    R4,R0                                                            
         CLI   0(R4),LOCELQ        ANOTHER LOCEL                                
         JE    CONV405                                                          
*                                                                               
CONV406  L     R4,FULL                                                          
*                                                                               
CONV407  MVC   0(8,R3),=C'<Office>'                                             
         LA    R3,8(,R3)                                                        
         MVC   0(2,R3),LOCOFF-LOCELD(R4)                                        
         GOTOR XMLSPEC,PARMS,0(R3),2                                            
         LR    R3,R1                                                            
         MVC   0(9,R3),=C'</Office>'                                            
         LA    R3,9(,R3)                                                        
*                                                                               
         MVC   0(6,R3),=C'<Dept>'                                               
         LA    R3,6(,R3)                                                        
         MVC   0(6,R3),LOCDEPT-LOCELD(R4)                                       
         GOTOR XMLSPEC,PARMS,0(R3),6                                            
         LR    R3,R1                                                            
         MVC   0(7,R3),=C'</Dept>'                                              
         LA    R3,7(,R3)                                                        
*                                                                               
         MVC   0(9,R3),=C'<SubDept>'                                            
         LA    R3,9(,R3)                                                        
         MVC   0(6,R3),LOCSUB-LOCELD(R4)                                        
         GOTOR XMLSPEC,PARMS,0(R3),6                                            
         LR    R3,R1                                                            
         MVC   0(10,R3),=C'</SubDept>'                                          
         LA    R3,10(,R3)                                                       
*                                                                               
CONV410  MVI   WORK,GPNTFST                                                     
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('GPNELQ',(R2)),(1,WORK)               
         CLI   12(R1),0                                                         
         JNE   CONV415                                                          
         L     R4,12(R1)                                                        
*                                                                               
         USING GPNELD,R4                                                        
         MVC   0(11,R3),=C'<NameFirst>'                                         
         LA    R3,11(,R3)                                                       
         LLC   RF,GPNLN                                                         
         SHI   RF,GPNLNQ                                                        
         MVC   0(0,R3),GPNLNAM                                                  
         EXRL  RF,*-6                                                           
         GOTOR XMLSPEC,PARMS,0(R3),(RF)                                         
         LR    R3,R1                                                            
         MVC   0(12,R3),=C'</NameFirst>'                                        
         LA    R3,12(,R3)                                                       
*                                                                               
CONV415  MVI   WORK,GPNTLST                                                     
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('GPNELQ',(R2)),(1,WORK)               
         CLI   12(R1),0                                                         
         JNE   CONV425                                                          
         L     R4,12(R1)                                                        
*                                                                               
         USING GPNELD,R4                                                        
CONV420  MVC   0(10,R3),=C'<NameLast>'                                          
         LA    R3,10(,R3)                                                       
         LLC   RF,GPNLN                                                         
         SHI   RF,GPNLNQ                                                        
         MVC   0(0,R3),GPNLNAM                                                  
         EXRL  RF,*-6                                                           
         GOTOR XMLSPEC,PARMS,0(R3),(RF)                                         
         LR    R3,R1                                                            
         MVC   0(11,R3),=C'</NameLast>'                                         
         LA    R3,11(,R3)                                                       
*                                                                               
CONV425  GOTO1 VHELLO,DMCB,(C'G',ACCMST),('EMPELQ',(R2)),0                      
         CLI   12(R1),0                                                         
         JNE   CONV440                                                          
         L     R4,12(R1)                                                        
*                                                                               
         CLC   EMPHIR-EMPELD(L'EMPHIR,R4),NULLS                                 
         JZ    CONV430                                                          
         MVC   0(11,R3),=C'<DateStart>'                                         
         LA    R3,11(,R3)                                                       
         GOTO1 VDATCON,PARMS,(1,EMPHIR-EMPELD(R4)),(20,0(R3))                   
         LA    R3,8(,R3)                                                        
         MVC   0(12,R3),=C'</DateStart>'                                        
         LA    R3,12(,R3)                                                       
*                                                                               
CONV430  CLC   EMPTRM-EMPELD(L'EMPTRM,R4),NULLS                                 
         JZ    CONV435                                                          
         MVC   0(09,R3),=C'<DateEnd>'                                           
         LA    R3,09(,R3)                                                       
         GOTO1 VDATCON,PARMS,(1,EMPTRM-EMPELD(R4)),(20,0(R3))                   
         LA    R3,8(,R3)                                                        
         MVC   0(10,R3),=C'</DateEnd>'                                          
         LA    R3,10(,R3)                                                       
*                                                                               
CONV435  TM    EMPSTAT-EMPEL(R4),EMPSTHRD                                       
         JZ    *+14                                                             
         MVC   0(14,R3),=C'<Is3rdParty />'                                      
         LA    R3,14(,R3)                                                       
*                                                                               
CONV440  J     CONVERTX                                                         
         EJECT                                                                  
***********************************************************************         
* OFFICE                                                              *         
***********************************************************************         
         SPACE 1                                                                
CONV500  GOTOR OFFDEPT,PARMS,ACTKACT-ACTKEY(R2),(R3),1                          
         LR    R3,R1                                                            
*                                                                               
         CLC   SAVSTAT(2),=C'ND'                                                
         JE    CONVERTX                                                         
*                                                                               
         GOTOR NAMEEL              DO NAME                                      
*                                                                               
CONV590  J     CONVERTX                                                         
         EJECT                                                                  
***********************************************************************         
* DEPT                                                                *         
***********************************************************************         
         SPACE 1                                                                
CONV600  GOTOR OFFDEPT,PARMS,ACTKACT-ACTKEY(R2),(R3),2                          
         LR    R3,R1                                                            
*                                                                               
         CLC   SAVSTAT(2),=C'ND'                                                
         JE    CONVERTX                                                         
*                                                                               
         GOTOR NAMEEL              DO NAME                                      
*                                                                               
CONV690  J     CONVERTX                                                         
         EJECT                                                                  
***********************************************************************         
* SUB DEPT                                                            *         
***********************************************************************         
         SPACE 1                                                                
CONV700  GOTOR OFFDEPT,PARMS,ACTKACT-ACTKEY(R2),(R3),3                          
         LR    R3,R1                                                            
*                                                                               
         CLC   SAVSTAT(2),=C'ND'                                                
         JE    CONVERTX                                                         
*                                                                               
         GOTOR NAMEEL              DO NAME                                      
*                                                                               
CONV790  J     CONVERTX                                                         
         EJECT                                                                  
***********************************************************************         
* PERIOD                                                              *         
***********************************************************************         
         SPACE 1                                                                
CONV800  MVC   0(28,R3),=C'Office="  " MOAEnd="YYYYMM" '                        
         MVC   28(19,R3),=C'MOAStart="YYYYMM"> '                                
*                                                                               
         MVC   8(2,R3),CASKOFC-CASKEY(R2)                                       
*                                                                               
         MVC   FULL(2),CASKEMOA-CASRECD(R2)                                     
         MVI   FULL+2,1                                                         
         GOTO1 VDATCON,PARMS,(1,FULL),(20,DUB)                                  
         MVC   20(6,R3),DUB                                                     
*                                                                               
         MVC   FULL(2),CASKSMOA-CASRECD(R2)                                     
         MVI   FULL+2,1                                                         
         GOTO1 VDATCON,PARMS,(1,FULL),(20,DUB)                                  
         MVC   38(6,R3),DUB                                                     
         LA    R3,47(,R3)                                                       
*                                                                               
         CLC   SAVSTAT(2),=C'ND'                                                
         JE    CONVERTX                                                         
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('TMPELQ',(R2)),0                      
         CLI   12(R1),0                                                         
         JNE   CONVERTX                                                         
         L     R4,12(R1)                                                        
*                                                                               
CONV810  MVC   0(20,R3),=C'<Period Mnth="YYMM" '                                
         MVC   20(41,R3),=C'Num="nn" Start="YYYYMMDD" End="YYYYMMDD">'          
*                                                                               
         MVC   FULL(2),TMPMTH-TMPELD(R4)                                        
         MVI   FULL+2,1                                                         
         GOTO1 VDATCON,PARMS,(1,FULL),(20,WORK)                                 
         MVC   14(4,R3),WORK+2                                                  
         EDIT  (B1,TMPNUMB-TMPELD(R4)),(2,25(R3)),FILL=0                        
         GOTO1 VDATCON,PARMS,(1,TMPSTART-TMPELD(R4)),(20,36(R3))                
         GOTO1 VDATCON,PARMS,(1,TMPEND-TMPELD(R4)),(20,51(R3))                  
         LA    R3,61(,R3)                                                       
         MVC   0(9,R3),=C'</Period>'                                            
         LA    R3,9(,R3)                                                        
*                                                                               
         XR    R0,R0               NEXT ELEMENT                                 
         ICM   R0,1,1(R4)                                                       
         JZ    CONV890                                                          
         AR    R4,R0                                                            
         CLI   0(R4),TMPELQ        ANOTHER TEMPEL                               
         JE    CONV810                                                          
*                                                                               
CONV890  J     CONVERTX                                                         
         EJECT                                                                  
***********************************************************************         
* MEDIA                                                               *         
***********************************************************************         
         SPACE 1                                                                
CONV900  MVC   0(10,R3),=C'Media=" ">'                                          
         MVC   7(1,R3),PMDKMED-PMDKEY(R2)                                       
         LA    R3,10(,R3)                                                       
*                                                                               
         CLC   SAVSTAT(2),=C'ND'                                                
         JE    CONVERTX                                                         
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('PMDELQ',(R2)),0                      
         CLI   12(R1),0                                                         
         JNE   CONVERTX                                                         
         L     R4,12(R1)                                                        
*                                                                               
         MVC   0(6,R3),=C'<Name>'                                               
         LA    R3,6(,R3)                                                        
         MVC   0(12,R3),PMDDESC-PMDEL(R4)                                       
         MVC   12(2,R3),SPACES                                                  
*                                                                               
         GOTOR XMLSPEC,PARMS,0(R3),12                                           
         LR    R3,R1                                                            
         MVC   0(7,R3),=C'</Name>'                                              
         LA    R3,7(,R3)                                                        
*                                                                               
CONV990  J     CONVERTX                                                         
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO PROCESS SPECIAL XML CHARACTERS AT P1 FOR LEN P2       *         
***********************************************************************         
         SPACE 1                                                                
XMLSPEC  NTR1                                                                   
         XC    WORK(64),WORK                                                    
*                                                                               
         L     R2,0(R1)            R2=ADDRESS                                   
         ST    R2,FULL                                                          
         L     R3,4(R1)            LENGTH                                       
*                                                                               
         AR    R2,R3               POINT R2 TO END                              
XMLS010  BCTR  R2,0                                                             
         C     R2,FULL             MUST NOT HIT START                           
         JL    DEATH13                                                          
         CLI   0(R2),C' '          BACK UP TO NON SPACE                         
         JNH   XMLS010                                                          
         LA    R3,1(,R2)           R3=NEW END POINT                             
         L     R2,FULL                                                          
         LA    RF,WORK                                                          
         XC    HALF,HALF           NO LENGTH ADJUST                             
*                                                                               
XMLS020  CLI   0(R2),X'80'                                                      
         JNL   XMLS060             > 80 CHARS ARE ALL OK                        
*                                                                               
         LARL  R1,SPECTAB          < 80 MAY BE SPECIAL CHRS                     
XMLS051  CLC   0(1,R2),0(R1)                                                    
         JE    XMLS080                                                          
         LLC   RE,1(R1)                                                         
         LA    R1,3(RE,R1)         NEXT SPECTAB ENTRY                           
         CLI   0(R1),X'FF'                                                      
         JNE   XMLS051                                                          
         J     XMLS060             NOT IN TABLE SO WRITE IT                     
*                                                                               
XMLS060  MVC   0(1,RF),0(R2)       COPY CHR                                     
         LA    RF,1(,RF)                                                        
XMLS061  LA    R2,1(,R2)                                                        
         CR    R2,R3               END POINT TEST                               
         JL    XMLS020                                                          
         J     XMLS090             DONE                                         
*                                                                               
XMLS080  LLC   RE,1(R1)            SPECIAL CHR GET LEN-1                        
         LR    R0,RE                                                            
         AH    R0,HALF                                                          
         STH   R0,HALF             ACCUMULATE LENGTH ADJUST                     
         MVC   0(0,RF),2(R1)       COPY SPECIAL TEXT                            
         EXRL  RE,*-6                                                           
         LA    RF,1(RE,RF)                                                      
         J     XMLS061                                                          
*                                                                               
XMLS090  L     R2,FULL             R2 BACK TO START                             
         LA    R3,WORK                                                          
         SR    RF,R3               RF=LEN OF OUTPUT                             
*                                                                               
         LARL  RE,OUTUKUS                                                       
         EXRL  RF,*+10                                                          
         J     *+10                                                             
         TR    WORK(0),0(RE)       TRANSLATE OUT ANY BAD CHRS                   
*                                                                               
         EXRL  RF,*+10                                                          
         J     *+10                                                             
         MVC   0(0,R2),WORK        MOVE TO OUTPUT                               
*                                                                               
         AR    RF,R2                                                            
         LR    R1,RF                                                            
*                                                                               
XMLS100  J     EXITR1                                                           
*                                                                               
XMLSPECX J     EXITR1                                                           
*                                                                               
SPECTAB  DC    C'<',AL1(3),C'&&lt;'                                             
         DC    C'>',AL1(3),C'&&gt;'                                             
         DC   C'&&',AL1(4),C'&&amp;'                                            
         DC   C'''',AL1(5),C'&&apos;'                                           
         DC    C'"',AL1(5),C'&&quot;'                                           
         DC    X'FF'                                                            
*                                                                               
* VALID OUTPUT CHARACTERS FOR UK AND USA                                        
*                                                                               
OUTUKUS  DC    XL16'40404040404040404040404040404040'  00-0F                    
         DC    XL16'40404040404040404040404040404040'  10-1F                    
         DC    XL16'40404040404040404040404040404040'  20-2F                    
         DC    XL16'40404040404040404040404040404040'  30-3F                    
         DC    XL16'404040404040404040404A4B4C4D4E4F'  40-4F                    
         DC    XL16'504040404040404040405A5B5C5D5E5F'  50-5F                    
         DC    XL16'606140404040404040406A6B6C6D6E6F'  60-6F                    
         DC    XL16'404040404040404040797A7B7C7D7E7F'  70-7F                    
         DC    XL16'40818283848586878889404040404040'  80-8F                    
         DC    XL16'4091929394959697989940404040409F'  90-9F                    
         DC    XL16'40A1A2A3A4A5A6A7A8A9404040404040'  A0-AF                    
         DC    XL16'40404040404040404040404040404040'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C9404040404040'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D9404040404040'  D0-DF                    
         DC    XL16'E040E2E3E4E5E6E7E8E9404040404040'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F9404040404040'  F0-FF                    
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO FORMAT CLIENT  PRODUCT JOB FROM CL12 AND RECSTRUC     *         
***********************************************************************         
         SPACE 1                                                                
CLIPROJB NTR1                                                                   
         L     R2,0(R1)            P1 = CL12 KEY                                
         L     R3,4(R1)            P2 = CURRENT OUTPUT POINTER                  
         L     R4,8(R1)            P3 = 1 CLI, 2 CLI/PRO, 3 CLI/PRO/JOB         
*                                                                               
         MVC   0(21,R3),=C'Client="             '                               
         MVC   8(12,R3),0(R2)      CLIENT FROM KEY                              
         GOTOR XMLSPEC,PARMS,8(R3),12                                           
         LLC   R1,RECSTRUC                                                      
         LA    R1,8(,R1)                                                        
         AR    R3,R1                                                            
         MVC   0(2,R3),=C'" '                                                   
         LA    R3,2(,R3)                                                        
         CHI   R4,1                1 CLIENT ONLY                                
         JE    CLIP990                                                          
*                                                                               
         MVC   0(22,R3),=C'Product="             '                              
         LLC   R1,RECSTRUC                                                      
         LA    R1,0(R2,R1)                                                      
         MVC   9(12,R3),0(R1)      PRODUCT FROM KEY                             
         GOTOR XMLSPEC,PARMS,9(R3),12                                           
         LLC   R1,RECSTRUC+1                                                    
         LLC   RF,RECSTRUC+0                                                    
         SR    R1,RF                                                            
         LA    R1,9(,R1)                                                        
         AR    R3,R1                                                            
         MVC   0(2,R3),=C'" '                                                   
         LA    R3,2(,R3)                                                        
         CHI   R4,2                2 CLIENT PROD                                
         JE    CLIP990                                                          
*                                                                               
         MVC   0(18,R3),=C'Job="             '                                  
         LLC   R1,RECSTRUC+1                                                    
         LA    R1,0(R2,R1)                                                      
         MVC   5(12,R3),0(R1)      JOB FROM KEY                                 
         GOTOR XMLSPEC,PARMS,5(R3),12                                           
         LLC   R1,RECSTRUC+2                                                    
         LLC   RF,RECSTRUC+1                                                    
         SR    R1,RF                                                            
         LA    R1,5(,R1)                                                        
         AR    R3,R1                                                            
         MVC   0(2,R3),=C'" '                                                   
         LA    R3,2(,R3)                                                        
         CHI   R4,3                3 CLIENT PROD JOB                            
         JE    CLIP990                                                          
*                                                                               
         DC    H'0'                                                             
*                                                                               
CLIP990  BCTR  R3,0                                                             
         MVI   0(R3),C'>'                                                       
         LA    R3,1(R3)                                                         
         LR    R1,R3                                                            
*                                                                               
CLIPX    J     EXITR1                                                           
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO FORMAT <AS ELEMENTS> CLIENT PRODUCT JOB FROM CL12     *         
***********************************************************************         
         SPACE 1                                                                
CLIPROEL NTR1                                                                   
         L     R2,0(R1)            P1 = CL12 KEY                                
         L     R3,4(R1)            P2 = CURRENT OUTPUT POINTER                  
         L     R4,8(R1)            P3 = 1 CLI, 2 CLI/PRO, 3 CLI/PRO/JOB         
*                                                                               
         MVC   0(8,R3),=C'<Client>'                                             
         MVC   8(12,R3),0(R2)      CLIENT FROM KEY                              
         GOTOR XMLSPEC,PARMS,8(R3),12                                           
         LLC   R1,RECSTRUC                                                      
         LA    R1,8(,R1)                                                        
         AR    R3,R1                                                            
         MVC   0(9,R3),=C'</Client>'                                            
         LA    R3,9(,R3)                                                        
*                                                                               
         MVC   0(9,R3),=C'<Product>'                                            
         LLC   R1,RECSTRUC                                                      
         LA    R1,0(R2,R1)                                                      
         MVC   9(12,R3),0(R1)      PRODUCT FROM KEY                             
         GOTOR XMLSPEC,PARMS,9(R3),12                                           
         LLC   R1,RECSTRUC+1                                                    
         LLC   RF,RECSTRUC+0                                                    
         SR    R1,RF                                                            
         LA    R1,9(,R1)                                                        
         AR    R3,R1                                                            
         MVC   0(10,R3),=C'</Product>'                                          
         LA    R3,10(,R3)                                                       
*                                                                               
         MVC   0(5,R3),=C'<Job>'                                                
         LLC   R1,RECSTRUC+1                                                    
         LA    R1,0(R2,R1)                                                      
         MVC   5(12,R3),0(R1)      JOB FROM KEY                                 
         GOTOR XMLSPEC,PARMS,5(R3),12                                           
         LLC   R1,RECSTRUC+2                                                    
         LLC   RF,RECSTRUC+1                                                    
         SR    R1,RF                                                            
         LA    R1,5(,R1)                                                        
         AR    R3,R1                                                            
         MVC   0(6,R3),=C'</Job>'                                               
         LA    R3,6(,R3)                                                        
         LR    R1,R3                                                            
*                                                                               
         J     EXITR1                                                           
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO FORMAT CLIENT  PRODUCT JOB FROM CL12 AND RECSTRUC     *         
***********************************************************************         
         SPACE 1                                                                
OFFDEPT  NTR1                                                                   
         L     R2,0(R1)            P1 = CL12 KEY                                
         L     R3,4(R1)            P2 = CURRENT OUTPUT POINTER                  
         L     R4,8(R1)            P3 = 1 CLI, 2 CLI/PRO, 3 CLI/PRO/JOB         
*                                                                               
         MVC   0(21,R3),=C'Office="             '                               
         MVC   8(12,R3),0(R2)      CLIENT FROM KEY                              
         GOTOR XMLSPEC,PARMS,8(R3),12                                           
         LLC   R1,RECSTRUC                                                      
         LA    R1,8(,R1)                                                        
         AR    R3,R1                                                            
         MVC   0(2,R3),=C'" '                                                   
         LA    R3,2(,R3)                                                        
         CHI   R4,1                1 OFFICE ONLY                                
         JE    OFF990                                                           
*                                                                               
         MVC   0(19,R3),=C'Dept="             '                                 
         LLC   R1,RECSTRUC                                                      
         LA    R1,0(R2,R1)                                                      
         MVC   6(12,R3),0(R1)      DEPT FROM KEY                                
         GOTOR XMLSPEC,PARMS,6(R3),12                                           
         LLC   R1,RECSTRUC+1                                                    
         LLC   RF,RECSTRUC+0                                                    
         SR    R1,RF                                                            
         LA    R1,6(,R1)                                                        
         AR    R3,R1                                                            
         MVC   0(2,R3),=C'" '                                                   
         LA    R3,2(,R3)                                                        
         CHI   R4,2                2 OFFICE DEPT                                
         JE    OFF990                                                           
*                                                                               
         MVC   0(23,R3),=C'SubDept="             '                              
         LLC   R1,RECSTRUC+1                                                    
         LA    R1,0(R2,R1)                                                      
         MVC   9(12,R3),0(R1)      SUB FROM KEY                                 
         GOTOR XMLSPEC,PARMS,9(R3),12                                           
         LLC   R1,RECSTRUC+2                                                    
         LLC   RF,RECSTRUC+1                                                    
         SR    R1,RF                                                            
         LA    R1,9(,R1)                                                        
         AR    R3,R1                                                            
         MVC   0(2,R3),=C'" '                                                   
         LA    R3,2(,R3)                                                        
         CHI   R4,3                3 OFFICE DEPT SUB                            
         JE    OFF990                                                           
*                                                                               
         MVC   0(18,R3),=C'Person="          '                                  
         LLC   R1,RECSTRUC+2                                                    
         LA    R1,0(R2,R1)                                                      
         MVC   8(12,R3),0(R1)      PERSON FROM KEY                              
         GOTOR XMLSPEC,PARMS,8(R3),12                                           
         LLC   R1,RECSTRUC+3                                                    
         LLC   RF,RECSTRUC+2                                                    
         SR    R1,RF                                                            
         LA    R1,8(,R1)                                                        
         AR    R3,R1                                                            
         MVC   0(2,R3),=C'" '                                                   
         LA    R3,2(,R3)                                                        
*                                                                               
OFF990   BCTR  R3,0                                                             
         MVI   0(R3),C'>'                                                       
         LA    R3,1(R3)                                                         
         LR    R1,R3                                                            
*                                                                               
OFFX     J     EXITR1                                                           
         EJECT                                                                  
***********************************************************************         
* COMMON ELEMENT SUBROUTIONES - NOTE NO NTR XIT JUST SAVE RE          *         
***********************************************************************         
         SPACE 1                                                                
NAMEEL   ST    RE,SAVERE                                                        
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('NAMELQ',(R2)),0                      
         CLI   12(R1),0                                                         
         JNE   NAMEELX                                                          
         L     RE,12(R1)                                                        
*                                                                               
         MVC   0(6,R3),=C'<Name>'                                               
         LA    R3,6(,R3)                                                        
         MVC   0(36,R3),NAMEREC-NAMELD(RE)                                      
         LLC   RF,NAMLN-NAMELD(RE)                                              
         SHI   RF,NAMLN1Q                                                       
         GOTOR XMLSPEC,PARMS,0(R3),(RF)                                         
         LR    R3,R1                                                            
         MVC   0(7,R3),=C'</Name>'                                              
         LA    R3,7(,R3)                                                        
NAMEELX  L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
OFFICEL  ST    RE,SAVERE                                                        
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',ACCMST),('PPRELQ',(R2)),0                      
         CLI   12(R1),0                                                         
         JNE   OFFICELX                                                         
         L     RE,12(R1)                                                        
*                                                                               
         CLC   PPRGAOFF-PPRELD(2,RE),SPACES                                     
         JZ    CONV090                                                          
         MVC   0(8,R3),=C'<Office>'                                             
         LA    R3,8(,R3)                                                        
         MVC   0(2,R3),PPRGAOFF-PPRELD(RE)                                      
         LA    R3,2(,R3)                                                        
         MVC   0(9,R3),=C'</Office>'                                            
         LA    R3,9(,R3)                                                        
OFFICELX L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
***********************************************************************         
* SUBROUTINE TO GET NEXT ITEM OFF QUEUE OR WAIT FOR ACTION            *         
***********************************************************************         
         SPACE 1                                                                
QWAIT    NTR1                                                                   
*                                                                               
         TM    ATTECBO,POSTED      TERMINATE FROM CALLER ECB                    
         JO    EXITL               POSTED, EXIT                                 
*                                                                               
         BRAS  RE,LOGNEXT          LOG ABOUT TO LOOK FOR ACTION                 
*                                                                               
QWA02    BRAS  RE,USSRCV           GET A USS MESSAGE                            
         JZ    EXITOK              RETURN IF WE HAVE A MESSAGE TO SEND          
*                                                                               
         LA    R1,2*100            SET 2 SECOND WAIT                            
         BRAS  RE,SETWAIT                                                       
*                                                                               
         LA    R2,PARMS                                                         
         LA    R1,ATTECBO                                                       
         ST    R1,0(,R2)                                                        
         LA    R1,TIMERECB                                                      
         ST    R1,4(,R2)                                                        
         OI    4(R2),X'80'                                                      
QWA04    WAIT  ECBLIST=(R2)                                                     
         TM    ATTECBO,POSTED                                                   
         JO    EXITL                                                            
         TM    TIMERECB,POSTED                                                  
         JO    QWA02                                                            
         J     QWA04                                                            
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO SEND A USS MESSAGE TO MQ                              *         
***********************************************************************         
         SPACE 1                                                                
SENDMQ   NTR1                                                                   
*                                                                               
         CLI   ATTTRACE,C'D'                                                    
         JNE   *+8                                                              
         BRAS  RE,LOGPUT                                                        
*                                                                               
         L     R1,BUFFLEN                                                       
         GOTOR LOGMQS                                                           
         BRAS  RE,PUTTOMQ          PUT IT TO MQ                                 
         BRAS  RE,COMMITMQ                                                      
*                                                                               
         NI    ATTFLAGS,255-ATTFSEND CLEAR SEND PENDING                         
*                                                                               
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* PUT MESSAGE TO LOG                                                  *         
***********************************************************************         
         SPACE 1                                                                
LOGPUT   NTR1                                                                   
         L     R2,ABUFFER                                                       
         LA    R2,16(,R2)          FROM FIRST <                                 
         L     R4,BUFFLEN                                                       
         SHI   R4,16               REMOVE 16 BYTES FROM LEN                     
         XR    R5,R5               INITIAL INDENT 0                             
*                                                                               
LOGP005  LA    R3,CARD             TO CARD                                      
         MVC   CARD,SPACES                                                      
         LR    R1,R5               INDENT                                       
         SLL   R1,2                *4                                           
         AR    R3,R1               R3=NEW START                                 
         LR    R0,R1               R0=INITIAL LINE LEN                          
*                                                                               
LOGP010  MVC   0(1,R3),0(R2)       COPY 1 CHR TO CARD                           
         JCT   R4,LOGP015                                                       
         J     LOGP050             END OF BUFFER                                
*                                                                               
LOGP015  LA    R3,1(,R3)           NEXT CHR                                     
         LA    R2,1(,R2)                                                        
         AHI   R0,1                                                             
         CHI   R0,166-1            OVER 166 CHRS                                
         JH    LOGP050                                                          
*                                                                               
LOGP020  CLC   0(2,R2),=C'/>'      CHECK FOR />                                 
         JNE   LOGP025                                                          
         BCTR  R5,0                DOWN A LEVEL                                 
*                                                                               
LOGP025  CLI   0(R2),C'<'          NEXT UNTIL <                                 
         JNE   LOGP010                                                          
         CLC   0(2,R2),=C'</'      UNLESS </                                    
         JNE   LOGP030                                                          
*                                                                               
         CLI   BYTE,C'C'           CLOSE FOUND                                  
         JNE   LOGP040                                                          
         MVI   BYTE,C'2'           2ND CLOSE FOUND                              
         J     LOGP050                                                          
*                                                                               
LOGP040  BCTR  R5,0                BACK UP ONE                                  
         MVI   BYTE,C'C'                                                        
         J     LOGP010                                                          
*                                                                               
LOGP030  CLI   BYTE,C'2'           UNLESS FOLLOWING 2ND CLOSE4                  
         JE    *+8                                                              
         AHI   R5,1                INCREASE INDENT UP ONE LEVEL                 
         MVI   BYTE,C'O'                                                        
*                                                                               
*NOP050  PUT   XMLOUT,CARD         PUT TO XMLOUT                                
*                                                                               
LOGP050  MVC   P,CARD              PUT TO PRINT                                 
         BRAS  RE,PRNT                                                          
*                                                                               
         LTR   R4,R4                                                            
         JNZ   LOGP005             CONTINUE                                     
*                                                                               
LOGP990  J     EXITOK                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISE USS                                                      *         
***********************************************************************         
         SPACE 1                                                                
INITUSS  NTR1                                                                   
         SAM31                                                                  
*                                                                               
         LHI   R2,3                                                             
         LA    R0,BPX1QGT                                                       
         LOAD  EPLOC=(0)                                                        
         LTR   R3,RF                                                            
         JNZ   DEATH03                                                          
         ST    R0,ABPX1QGT                                                      
*                                                                               
         LHI   R2,4                                                             
         LA    R0,BPX1QRC                                                       
         LOAD  EPLOC=(0)                                                        
         LTR   R3,RF                                                            
         JNZ   DEATH03                                                          
         ST    R0,ABPX1QRC                                                      
*                                                                               
         LHI   R2,5                                                             
         LA    R0,BPX1QSN                                                       
         LOAD  EPLOC=(0)                                                        
         LTR   R3,RF                                                            
         JNZ   DEATH03                                                          
         ST    R0,ABPX1QSN                                                      
         J     INUS04                                                           
*                                                                               
INUS04   MVI   S_TYPE,IPC_CREAT    MAY ALREADY EXIST                            
         MVI   S_MODE1,0           NOT USED                                     
         MVI   S_MODE2,S_IRUSR     ALL READ AND WRITE PERMISSION                
         MVI   S_MODE3,S_IWUSR+S_IRGRP+S_IWGRP+S_IROTH+S_IWOTH                  
*                                                                               
         XC    H_MQIN(8),H_MQIN                                                 
*                                                                               
         L     RF,ABPX1QGT                                                      
         CALL  (15),(UNXQKEY,S_MODE,RV,RCV,RSN),VL,                    +        
               MF=(E,PLIST),LINKINST=BASR                                       
         ICM   RF,15,RV                                                         
         ST    RF,H_MQIN                                                        
         JNM   EXITOK                                                           
*                                                                               
         GOTOR LOGFAIL,BPX1QGT                                                  
         J     EXITL               CANNOT GET ADDRESS OF QUEUE                  
         EJECT                                                                  
***********************************************************************         
* RECEIVE FROM A QUEUE                                                *         
***********************************************************************         
         SPACE                                                                  
USSRCV   NTR1                                                                   
         SAM31                                                                  
         L     RF,ABPX1QRC                                                      
         CALL  (15),                                                   +        
               (H_MQIN,                                                +        
               AMSGBUFF,                                               +        
               =A(0),                                                  +        
               =A(MSGDATAL),                                           +        
               =A(0),                                                  +        
               =A(MSG_NOERROR+IPC_NOWAIT),                             +        
               RV,RCV,RSN),                                            +        
               VL,MF=(E,PLIST),LINKINST=BASR                                    
*                                                                               
         ICM   RF,15,RV            LENGTH HERE                                  
         JNM   URCV02                                                           
*                                                                               
         CLC   RCV,=A(X'0473')     ENOMSG                                       
         JE    EXITL                                                            
         GOTOR LOGFAIL,BPX1QRC                                                  
         J     EXITL                                                            
*                                                                               
URCV02   OI    ATTFLAGS,ATTFSEND   INDICATE SEND PENDING                        
*                                                                               
         ST    RF,USSLEN                                                        
         LR    R1,RF                                                            
         GOTOR LOGRCV                                                           
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET A TIMER FOR NUMBER OF HUNDREDTHS OF A SECOND IN R1              *         
* IF R1=0, JUST CANCEL TIMER                                          *         
***********************************************************************         
         SPACE 1                                                                
SETWAIT  NTR1                                                                   
         LARL  R9,SETWAIT                                                       
         USING SETWAIT,R9                                                       
*                                                                               
         LR    R4,R1               R4=SECS/100                                  
*                                                                               
         LA    R2,STIMERID                                                      
*                                                                               
         OC    0(L'STIMERID,R2),0(R2) STIMER SET UP?                            
         JZ    SWT02               NO                                           
*                                                                               
         LA    R3,FULL                                                          
         STIMERM TEST,ID=(R2),TU=(R3),MF=(E,PLIST2)                             
         ICM   R0,15,FULL                                                       
         JZ    SWT02               NO CURRENT TIMER ACTIVE                      
*                                                                               
         STIMERM CANCEL,ID=(R2),MF=(E,PLIST2)                                   
*                                                                               
SWT02    XC    TIMERECB,TIMERECB   CLEAR ECB                                    
         LTR   R4,R4               TEST INTERVAL GIVEN                          
         JZ    EXITOK              NO, EXIT                                     
*                                                                               
         ST    R4,WAITSECS                                                      
         ST    RC,FULL                                                          
         STIMERM SET,ID=(R2),BINTVL=WAITSECS,EXIT=TIMERXIT,PARM=FULL,  X        
               MF=(E,PLIST2)                                                    
         LTR   RF,RF                                                            
         JNZ   DEATH04                                                          
         J     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* THE TIMER EXIT ROUTINE.  IT POSTS THE TIMERECB.                     *         
***********************************************************************         
         SPACE 1                                                                
TIMERXIT SAVE  (14,12),,*                                                       
         LARL  R2,TIMERXIT                                                      
         USING TIMERXIT,R2                                                      
*                                                                               
         L     RC,4(,R1)           PARM VALUE FROM STIMERM MACRO                
         POST  TIMERECB                                                         
*                                                                               
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
         DROP  R2                                                               
         DROP  R9                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO INIT MQ - BUILD PARMS IN WORKING STORAGE FOR RE-ENT   *         
***********************************************************************         
         SPACE 1                                                                
INITMQ   NTR1                                                                   
*                                                                               
         MVC   ENTRYPTN,=Y((ENTRYLSQ-ENTRYSTQ)/60)                              
         MVC   ENTRYPTL,=H'60'                                                  
         MVC   CSQBCLOS(8),=CL8'CSQBCLOS'                                       
         MVC   CSQBCOMM(8),=CL8'CSQBCOMM'                                       
         MVC   CSQBCONN(8),=CL8'CSQBCONN'                                       
         MVC   CSQBDISC(8),=CL8'CSQBDISC'                                       
         MVC   CSQBGET(8),=CL8'CSQBGET'                                         
         MVC   CSQBOPEN(8),=CL8'CSQBOPEN'                                       
         MVC   CSQBPUT(8),=CL8'CSQBPUT'                                         
*                                                                               
         BLDL  0,ENTRYPTS          BUILD LIST OF APPC/MVS ENTRY PTS             
         LTR   RF,RF                                                            
         JNZ   DEATH09             BAD RETURN FROM BLDL MACRO                   
*                                                                               
         LOAD  DE=CSQBCONN         LOAD MQ ROUTINES                             
         ST    R0,AMQBCONN                                                      
         LOAD  DE=CSQBOPEN                                                      
         ST    R0,AMQBOPEN                                                      
         LOAD  DE=CSQBPUT                                                       
         ST    R0,AMQBPUTN                                                      
         LOAD  DE=CSQBGET                                                       
         ST    R0,AMQBGETN                                                      
         LOAD  DE=CSQBCOMM                                                      
         ST    R0,AMQBCOMM                                                      
         LOAD  DE=CSQBCLOS                                                      
         ST    R0,AMQBCLOS                                                      
         LOAD  DE=CSQBDISC                                                      
         ST    R0,AMQBDISC                                                      
*                                  SET UP IBM PARAMETER LISTS                   
         MVC   CONNEYE,=C'MQ_CONNECT      '                                     
         MVC   OPENEYE,=C'MQ_OPEN         '                                     
         MVC   PUTNEYE,=C'MQ_PUT          '                                     
         MVC   COMMEYE,=C'MQ_COMMIT       '                                     
         MVC   CLOSEYE,=C'MQ_CLOSE        '                                     
         MVC   DISCEYE,=C'MQ_DISCONNECT   '                                     
*                                                                               
         L     R1,ABUFFER          SET A(BUFFER)                                
         ST    R1,PUTNABUF                                                      
         ST    R1,GETNABUF                                                      
*                                                                               
         LA    R1,QMGR             QMGR                                         
         ST    R1,CONNQMGR                                                      
*                                                                               
         LA    R1,HCONN            SET A(CONNECTION HANDLE)                     
         ST    R1,CONNCONN                                                      
         ST    R1,OPENCONN                                                      
         ST    R1,PUTNCONN                                                      
         ST    R1,GETNCONN                                                      
         ST    R1,COMMCONN                                                      
         ST    R1,CLOSCONN                                                      
         ST    R1,DISCCONN                                                      
*                                                                               
         LA    R1,HOBJ             SET A(OBJECT HANDLE)                         
         ST    R1,OPENHOBJ                                                      
         ST    R1,PUTNHOBJ                                                      
         ST    R1,GETNHOBJ                                                      
         ST    R1,CLOSHOBJ                                                      
*                                                                               
         LA    R1,OPENOPT          SET A(OPEN OPTIONS)                          
         ST    R1,OPENOPOP                                                      
*                                                                               
         LA    R1,CLOSEOPT         SET A(CLOSE OPTIONS)                         
         ST    R1,CLOSCLOP                                                      
*                                                                               
         LA    R1,OBJDESC          SET A(OBJECT DESC)                           
         ST    R1,OPENDESC                                                      
*                                                                               
         LA    R1,MSGDESC          SET A(MSG DESC)                              
         ST    R1,GETNDESC                                                      
         ST    R1,PUTNDESC                                                      
*                                                                               
         LA    R1,COMPCODE         SET A(COMPCODE)                              
         ST    R1,CONNCOMP                                                      
         ST    R1,OPENCOMP                                                      
         ST    R1,OPENCOMP                                                      
         ST    R1,PUTNCOMP                                                      
         ST    R1,GETNCOMP                                                      
         ST    R1,COMMCOMP                                                      
         ST    R1,CLOSCOMP                                                      
         ST    R1,CLOSCOMP                                                      
         ST    R1,DISCCOMP                                                      
*                                                                               
         LA    R1,REASON           SET REASON AS LAST PARM                      
         ST    R1,CONNREAS                                                      
         OI    CONNREAS,X'80'      WITH X'80' FLAGS                             
         ST    R1,OPENREAS                                                      
         OI    OPENREAS,X'80'                                                   
         ST    R1,PUTNREAS                                                      
         OI    PUTNREAS,X'80'                                                   
         ST    R1,GETNREAS                                                      
         OI    GETNREAS,X'80'                                                   
         ST    R1,COMMREAS                                                      
         OI    COMMREAS,X'80'                                                   
         ST    R1,CLOSREAS                                                      
         OI    CLOSREAS,X'80'                                                   
         ST    R1,DISCREAS                                                      
         OI    DISCREAS,X'80'                                                   
*                                                                               
         LA    R1,BUFFLEN          SET A(BUFFLEN)                               
         ST    R1,PUTNBLEN                                                      
         ST    RE,GETNBLEN                                                      
*                                                                               
         LA    R1,DATALEN          SET A(DATALEN)                               
         ST    RE,GETNDLEN                                                      
*                                                                               
*                                  COPY GET AND PUT OPTS TO DSECT               
*                                                                               
         MVC   PUTOPTS(PUTOPTSX-PUTOPTSC),PUTOPTSC                              
         MVC   GETOPTS(GETOPTSX-GETOPTSC),GETOPTSC                              
         MVC   OBJDESC(256),OBJDESCC                                            
         MVC   OBJDESC+256(OBJDESCX-OBJDESCC-256),OBJDESCC+256                  
         MVC   MSGDESC(256),MSGDESCC                                            
         MVC   MSGDESC+256(MSGDESCX-MSGDESCC-256),MSGDESCC+256                  
*                                                                               
         LA    R1,PUTOPTS          SET A(PUT OPTIONS)                           
         ST    R1,PUTNPTOP                                                      
*                                                                               
         LA    R1,GETOPTS          SET A(GET OPTIONS)                           
         ST    R1,GETNGTOP                                                      
*                                                                               
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* OPEN MQ DESTINATION (BROKER)                                        *         
***********************************************************************         
*                                                                               
OPENMQ   NTR1                                                                   
*                                                                               
         LA    R2,AMQBCONN                                                      
         BRAS  RE,CALLMQ           CONNECT TO MQ QUEUE MANAGER                  
         JNE   DEATH01                                                          
*                                                                               
         MVC   OBJDESC_OBJECTTYPE,=A(MQOT_Q)  OBJECT IS A QUEUE                 
*                                                                               
         MVC   OBJDESC_OBJECTNAME,QUEUE  INPUT QUEUE NAME                       
*                                                                               
         LA    RF,MQOO_OUTPUT                                                   
         ST    RF,OPENOPT                                                       
*                                                                               
         LA    R2,AMQBOPEN                                                      
         BRAS  RE,CALLMQ           OPEN QUEUE                                   
         JNE   DEATH02                                                          
*                                                                               
         MVC   MSGDESC_CORRELID,QCORID                                          
         MVC   MSGDESC_MSGID,QMSGID                                             
*                                                                               
OPENMQX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CLOSE MQ DESTINATION                                                *         
***********************************************************************         
*                                                                               
CLOSMQ   NTR1                                                                   
*                                                                               
CLOSMQ1  LA    RF,MQCO_NONE                                                     
         ST    RF,CLOSEOPT                                                      
*                                                                               
         LA    R2,AMQBCLOS                                                      
         BRAS  RE,CALLMQ           CLOSE QUEUE                                  
         JNE   DEATH05                                                          
*                                                                               
         LA    R2,AMQBDISC                                                      
         BRAS  RE,CALLMQ           DISCONNECT FROM MQ QUEUE MANAGER             
         JNE   DEATH06                                                          
*                                                                               
CLOSMQX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PUT SINGLE MESSAGE TO MQ                                            *         
***********************************************************************         
*                                                                               
PUTTOMQ  NTR1                                                                   
*                                                                               
         LA    RF,MQPMO_SYNCPOINT                                               
         ICM   RE,15,=AL4(MQPMO_FAIL_IF_QUIESCING)                              
         AR    RF,RE                                                            
         ST    RF,GETOPTS_OPTIONS                                               
*                                                                               
         L     R1,COUNTER          COUNTER TO MUST COMMIT POINT                 
         AHI   R1,1                                                             
         ST    R1,COUNTER                                                       
         L     R1,SEQN             SEQUENCE COUNTER                             
         AHI   R1,1                                                             
         ST    R1,SEQN                                                          
         CLC   COUNTER,=F'10000'   MUST COMMIT AT 10000 MESSAGES                
         JL    PUT050                                                           
         LA    R2,AMQBCOMM                                                      
         BRAS  RE,CALLMQ           COMMIT                                       
         LA    R1,1                                                             
         ST    R1,COUNTER          SET COUNT TO 1                               
*                                                                               
         MVC   MSGDESC_CORRELID,QCORID                                          
         MVC   MSGDESC_MSGID,QMSGID                                             
*                                                                               
PUT050   LA    R2,AMQBPUTN                                                      
         BRAS  RE,CALLMQ           PUT THE MESSAGE TO THE MQ QUEUE              
         JNE   DEATH07                                                          
PUTTOMQX J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* COMMIT MQ MESSAGES                                                  *         
***********************************************************************         
*                                                                               
*                                                                               
COMMITMQ NTR1                                                                   
         LA    R2,AMQBCOMM                                                      
         BRAS  RE,CALLMQ           COMMIT                                       
         JNE   DEATH08                                                          
COMMITX  J     EXITOK                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
* CALL MQ                                                             *         
***********************************************************************         
*                                                                               
CALLMQ   NTR1                                                                   
*                                                                               
* UPON ENTRY,  R2 POINTS TO PARAMETER STRUCTURE                                 
* UPON RETURN, COMPCODE CONTAINS THE MQ COMPLETION CODE                         
*              REASON CONTAINS THE MQ REASON CODE                               
*                                                                               
         SAM31                     SWITCH TO 31-BIT MODE                        
*                                                                               
         L     RF,0(R2)            RF = A(MQ ROUTINE)                           
         LA    R3,24(R2)           R3 = A(PARAMETER LIST)                       
         CALL  (15),MF=(E,(R3))                                                 
*                                                                               
         SAM24                     SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         MVC   P+0(16),4(R2)       PRINT ROUTINE NAME AND RETURN CODES          
         MVC   P+17(8),=C'COMP. OK'                                             
         CLC   COMPCODE,=A(MQCC_OK)                                             
         JE    CALLMQ50                                                         
         MVC   P+17(8),=C'WARNING!'                                             
         CLC   COMPCODE,=A(MQCC_WARNING)                                        
         JE    CALLMQ10                                                         
         MVC   P+17(8),=C'*FAILED!'                                             
         CLC   COMPCODE,=A(MQCC_FAILED)                                         
         JE    CALLMQ10                                                         
         J     DEATH10                                                          
*                                                                               
CALLMQ10 EQU   *                                                                
         MVC   P+26(7),=C'REASON='                                              
         EDIT  REASON,(5,P+34),ZERO=NOBLANK                                     
         MVI   P+39,C':'                                                        
*                                                                               
         L     RF,=A(MQ_REASON_CODE_TABLE)                                      
CALLMQ20 CLI   0(RF),X'FF'         END OF TABLE?                                
         JNE   *+14                NO                                           
         MVC   P+41(22),=C'*** UNKNOWN REASON ***'                              
         J     CALLMQ30            REASON CODE NOT IN TABLE                     
*                                                                               
         CLC   REASON,0(RF)        FIND MATCH ON REASON CODE                    
         JE    *+12                GOT IT                                       
         LA    RF,28(RF)           BUMP TO NEXT TABLE ENTRY                     
         J     CALLMQ20                                                         
*                                                                               
         MVC   P+41(24),4(RF)                                                   
*                                                                               
CALLMQ30 CLC   REASON,=F'2033'     2033 IS JUST EOF SO OK                       
         JE    CALLMQ40            BUT MUST EXIT NEQ                            
*                                                                               
         BRAS  RE,PRNT                                                          
*                                                                               
CALLMQ40 CLC   COMPCODE,=A(MQCC_OK)                                             
*                                                                               
CALLMQ50 XIT1                                                                   
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* MQ API CONSTANTS                                                    *         
***********************************************************************         
                                                                                
         CMQA LIST=YES                                                          
                                                                                
         EJECT                                                                  
***********************************************************************         
* LOG ERRORS AND TRACES                                               *         
***********************************************************************         
         SPACE 1                                                                
LOGSTR   LARL  RF,TXTSTR                                                        
         J     LOGGER                                                           
LOGRST   LARL  RF,TXTRST                                                        
         J     LOGGER                                                           
LOGIEND  LARL  RF,TXTIEND                                                       
         J     LOGGER                                                           
LOGNEXT  LARL  RF,TXTNEXT                                                       
         J     LOGGER                                                           
LOGBAD   LARL  RF,TXTBAD                                                        
         J     LOGGER                                                           
LOGRCV   LARL  RF,TXTRCVQ                                                       
         J     LOGGER                                                           
LOGFAIL  LARL  RF,TXTFAIL                                                       
         J     LOGGER                                                           
LOGMQS   LARL  RF,TXTMQS                                                        
         J     LOGGER                                                           
LOGDMP   LARL  RF,TXTDMP                                                        
         J     LOGGER                                                           
*                                                                               
LOGGER   ST    RE,SAVERE                                                        
         ST    R1,SAVEPRM                                                       
*                                                                               
         MVC   P(80),SPACES                                                     
         LLC   R1,0(RF)                                                         
         BCTR  R1,0                                                             
         EXRL  R1,*+10                                                          
         J     *+10                                                             
         MVC   P(0),3(RF)          COPY MESSAGE TO WORK                         
*                                                                               
         CLI   ATTTRACE,C'D'       DETAILED TRACE                               
         JE    LOGGER1                                                          
         TM    1(RF),X'40'         DETAILED LOG ONLY                            
         JNZ   LOGGERX                                                          
         CLI   ATTTRACE,C'Y'       TRACE                                        
         JE    LOGGER1                                                          
         TM    1(RF),X'80'         TRACE LOG ONLY                               
         JNZ   LOGGERX                                                          
*                                                                               
LOGGER1  LLC   R1,2(RF)            GET NUMERIC ROUTINE VALUE                    
         SLL   R1,2                                                             
         LARL  RE,LOGROUTS                                                      
         L     RE,0(R1,RE)                                                      
         BR    RE                                                               
*                                                                               
LOGROUTS DC    A(LOGGERW)          00                                           
         DC    A(LOGGERS)          01                                           
         DC    A(LOGGERR)          02                                           
         DC    A(LOGGERE)          03                                           
         DC    A(LOGGEDP)          04                                           
*                                                                               
LOGGERS  MVC   P+3(2),ATTSEC                                                    
         J     LOGGERW                                                          
*                                                                               
LOGGERR  EDIT  (B4,SAVEPRM),(4,P)                                               
         J     LOGGERW                                                          
*                                                                               
LOGGERE  L     RF,4(,R2)                                                        
         MVC   P(8),0(RF)                                                       
         L     R1,4(,RD)                                                        
         L     R1,12(,R1)          RE ON ENTRY TO LOGERR                        
         L     R0,=A(MITMSRV)                                                   
         SR    R1,R0                                                            
         ST    R1,FULL                                                          
         GOTOR VHEXOUT,PARMS2,FULL,P+31,4,0                                     
         GOTOR (RF),(R1),RV,P+43,4                                              
         GOTOR (RF),(R1),RCV,P+56,4                                             
         GOTOR (RF),(R1),RSN,P+69,4                                             
         J     LOGGERW                                                          
*                                                                               
LOGGEDP  L     R2,AMSGBUFF                                                      
*                                                                               
         CLC   4(3,R2),=C'RCV'                                                  
         JE    LOGGEDPY                                                         
         MVC   P(3),4(R2)                                                       
         MVC   DUB,7(R2)                                                        
         GOTOR VHEXOUT,PARMS2,DUB,P+3,8,0                                       
         GOTOR PRNT                                                             
         J     LOGGEDPX                                                         
*                                                                               
LOGGEDPY MVC   P(3),4(R2)                                                       
         XR    R1,R1                                                            
         ICM   R1,3,7(R2)                                                       
         AR    R1,R2                                                            
         SHI   R1,10                                                            
         MVC   DUB,0(R1)                                                        
         GOTOR VHEXOUT,PARMS2,DUB,P+3,8,0                                       
         GOTOR PRNT                                                             
         J     LOGGEDPX                                                         
*                                                                               
         CLC   4(3,R2),=C'RCV'     RCV RECORD                                   
         JE    *+12                                                             
         L     R1,=F'15'           OR FORCE 15 BYTES                            
         J     *+10                                                             
         XR    R1,R1                                                            
         ICM   R1,3,7(R2)          XXXXRCVLLLL                                  
         LR    R3,R2                                                            
         AR    R3,R1                                                            
*                                                                               
LOGGEDPX L     R1,USSLEN                                                        
         LA    R1,4(R1)            PLUS 4 FOR 00000001                          
         LR    R3,R2                                                            
         AR    R3,R1                                                            
         GOTOR PRNT                                                             
*                                                                               
LOGGEDP0 LA    R4,P+0              DO OFFSET                                    
         LA    R5,P+42                                                          
         LR    RF,R2                                                            
         S     RF,AMSGBUFF                                                      
         ST    RF,FULL                                                          
         GOTOR VHEXOUT,PARMS2,FULL+2,(R4),2,0                                   
         LA    R4,5(,R4)                                                        
*                                                                               
LOGGEDP1 GOTOR VHEXOUT,PARMS2,(R2),(R4),4                                       
         MVC   0(4,R5),0(R2)                                                    
         LA    R4,9(,R4)                                                        
         LA    R5,4(,R5)                                                        
         LA    R2,4(,R2)                                                        
         LA    RE,P+41                                                          
         CR    R4,RE               OFF PAGE                                     
         JNL   LOGGEDP2                                                         
*                                                                               
         LR    RF,R3               WORK OUT HOW MANY LEFT                       
         SR    RF,R2                                                            
         CHI   RF,4                MORE THAN 4                                  
         JNL   LOGGEDP1                                                         
         LTR   R1,RF                                                            
         JNP   LOGGERW                                                          
         SHI   R1,1                                                             
         MVC   0(0,R5),0(R2)                                                    
         EXRL  R1,*-6                                                           
         GOTOR VHEXOUT,PARMS2,(R2),(R4),(RF)                                    
         J     LOGGERW                                                          
*                                                                               
LOGGEDP2 GOTOR PRNT                                                             
         LA    R4,P+0                                                           
         J     LOGGEDP0                                                         
*                                                                               
LOGGERW  GOTOR PRNT                                                             
         J     LOGGERX                                                          
*                                                                               
*        MESSAGE TAB AL1(LEN),X'FLG',AL1(ROUT),C'MESSAGE'                       
*                                                                               
         DS    0H                                                               
TXTSTR   DC    AL1(L'TXTSTRT),X'00',AL1(1)                                      
TXTSTRT  DC    C'SE=XX INITIALISING'                                            
         DS    0H                                                               
TXTRST   DC    AL1(L'TXTRSTT),X'00',AL1(1)                                      
TXTRSTT  DC    C'SE=XX RESTARTING'                                              
         DS    0H                                                               
TXTIEND  DC    AL1(L'TXTIENDT),X'00',AL1(0)                                     
TXTIENDT DC    C'ENDING'                                                        
         DS    0H                                                               
TXTNEXT  DC    AL1(L'TXTNEXTT),X'80',AL1(0)                                     
TXTNEXTT DC    C'CHECKING FOR INPUT'                                            
         DS    0H                                                               
TXTBAD   DC    AL1(L'TXTNEXTT),X'80',AL1(0)                                     
TXTBADT  DC    C'BAD COMPANY CODE  '                                            
         DS    0H                                                               
TXTRCVQ  DC    AL1(L'TXTRCVQT),X'80',AL1(2)                                     
TXTRCVQT DC    C'NNNN BYTES RECEIVED FROM QUEUE'                                
         DS    0H                                                               
TXTFAIL  DC    AL1(L'TXTFAILT+L'TXTFAIL2),X'00',AL1(3)                          
TXTFAILT DC    X'02',C'         COMMAND FAILED OFFSET=???????? '                
TXTFAIL2 DC    C'RV=???????? RCV=???????? RSN=????????'                         
         DS    0H                                                               
TXTMQS   DC    AL1(L'TXTMQST),X'80',AL1(2)                                      
TXTMQST  DC    C'NNNN BYTES SENT TO MQ '                                        
         DS    0H                                                               
TXTDMP   DC    AL1(L'TXTDMPT),X'40',AL1(4)                                      
TXTDMPT  DC    C'--------RCVR --------'                                         
         DS    0H                                                               
                                                                                
LOGGERX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT PLINE TO A PRINT LINE                             *         
***********************************************************************         
         SPACE 1                                                                
PRNT     NTR1                                                                   
         CLI   NOTIME,C'Y'                                                      
         JE    PRNT02                                                           
*                                                                               
         GOTOR DATETIME,PRNTDUB          PRNTDUB=X'YYYYMMDDHHMMSSTT'            
*                                                                               
         MVC   PRNTDUB(1),PRNTDUB+3      PRNTDUB=DDYYMMDDHHMMSSTT               
         MVI   PRNTDUB+1,X'0F'                   DD0FMM..                       
         UNPK  DATEWORK(3),PRNTDUB(2)    DATEWORK=C'DD0'                        
         MVC   PLINDAY,DATEWORK          PLINDAY=C'DD'                          
*                                                                               
         MVC   PRNTDUB(4),PRNTDUB+4      PRNTDUB=HHMMSSTTHHMMSSTT               
         MVI   PRNTDUB+4,X'0F'                   HHMMSSTT0F......               
         UNPK  DATEWORK(9),PRNTDUB(5)    DATEWORK=C'HHMMSSTT0'                  
         MVC   PLINTIME+00(2),DATEWORK   PLINTIME=C'HH'                         
         MVI   PLINTIME+02,C':'                       :                         
         MVC   PLINTIME+03(2),DATEWORK+2               MM                       
         MVI   PLINTIME+05,C':'                          :                      
         MVC   PLINTIME+06(2),DATEWORK+4                  SS                    
         MVI   PLINTIME+08,C'.'                             .                   
         MVC   PLINTIME+09(2),DATEWORK+6                     TT                 
*                                                                               
PRNT02   MVC   PLINSYS,ATTSYSN     WILL TRUNCATE TO 5 CHARS                     
         PUT   PRINTDCB,PCC                                                     
         MVI   PCC,C' '                                                         
         LARL  RE,SPACES                                                        
         MVC   PLINE,0(RE)                                                      
         MVI   NOTIME,C'N'                                                      
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* GET DATE/TIME INTO 0(8,R1) AS PWOS YYYYMMDDHHMMSSTT                 *         
***********************************************************************         
         SPACE 1                                                                
DATETIME NTR1                                                                   
         LR    R2,R1                                                            
         TIME  DEC,DATEWORK,LINKAGE=SYSTEM,DATETYPE=YYYYMMDD,          X        
               MF=(E,PARMS2)                                                    
         LAM   ARE,AR1,ARZERO      MACRO CAN CLOBBER ANY OF THESE               
*                                                                               
*        ABOVE MACRO RETURNS (BOTH AS PWOS):                                    
*        TIME IN DATEWORK+0(8) AS X'HHMMSSMMMMMM0000' (MILLISECONDS),           
*                            E.G. X'1043561285570000'                           
*        DATE IN DATEWORK+8(4) AS X'YYYYMMDD',                                  
*                            E.G. X'20100812'                                   
*                                                                               
         MVC   0(4,R2),DATEWORK+8  0(R1)=YYYYMMDD........                       
         MVC   4(4,R2),DATEWORK+0        YYYYMMDDHHMMSSTT                       
*                                                                               
         J     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DYNAMICALLY ALLOCATE PRINT FILE                                     *         
***********************************************************************         
         SPACE 1                                                                
ALLOCPRN NTR1                                                                   
         LR    RF,R1                                                            
*                                                                               
         LA    R1,TXTDD            DDNAME TEXT                                  
         ST    R1,ATXTDD                                                        
         MVC   0(6,R1),=X'0001 0001 0008' DDNAME CODE, COUNT, LENGTH            
         MVC   6(8,R1),40(RF)      DDNAME                                       
*                                                                               
         LA    R1,TXTSYSO          SYSOUT TEXT                                  
         ST    R1,ATXTSYSO                                                      
         MVC   0(4,R1),=X'0018 0000' SYSOUT CODE, COUNT (0=DFLT CLASS)          
*                                                                               
         OI    ATXTSYSO,X'80'      END OF LIST                                  
*                                                                               
         XC    RBLK,RBLK           DYNALLOC REQUEST BLOCK                       
         MVI   RBLKLEN,20                                                       
         MVI   RBLKVERB,1          ALLOCATE BY DSN VERB CODE                    
         LA    R1,ATXT                                                          
         ST    R1,RBLKATXT         POINTER TO TEXT LIST                         
         LA    R1,RBLK                                                          
         ST    R1,ARBLK                                                         
         OI    ARBLK,X'80'                                                      
         LA    R1,ARBLK                                                         
         DYNALLOC                                                               
         LTR   RF,RF               TEST FOR ERRORS                              
         JZ    EXIT                                                             
         CLC   RBLKERR,=X'0410'    DDNAME UNAVAILABLE USUALLY MEANS JCL         
         JE    EXIT                ALREADY INCLUDES A DD CARD                   
         DC    H'0'                DIE IF ANYTHING ELSE                         
         EJECT                                                                  
***********************************************************************         
* SUNDRY SUBROUTINES                                                  *         
***********************************************************************         
         SPACE 1                                                                
ADD1     ST    R1,DUB              INCREMENT WORD AT 0(RF) BY 1                 
         L     R1,0(,RF)                                                        
         LA    R1,1(,R1)                                                        
         ST    R1,0(,RF)                                                        
         L     R1,DUB                                                           
         BR    RE                                                               
         SPACE 1                                                                
ADDBYTES STM   R0,R1,DUB           INCREMENT BYTES AT 0(RF) BY R0               
         AH    R0,4(,RF)           ADD H'BYTES' TO R0 VALUE                     
         SRDA  R0,10               DIVIDE BY 1024                               
         SRL   R1,32-10            LINE UP SHIFTED OUT REMAINDER                
         STH   R1,4(,RF)           STORE NEW F'BYTES' (ALWAYS<1024)             
         A     R0,0(,RF)           ADD F'K'                                     
         ST    R0,0(,RF)           STORE NEW F'K'                               
         LM    R0,R1,DUB                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ERROR TRAPS                                                         *         
***********************************************************************         
*                                                                               
DEATH01  DC    H'00'               MQ CONNECT FAILED                            
DEATH02  DC    H'00'               MQ OPEN FAILED                               
DEATH03  DC    H'00'               ERROR DOING USS LOADS CAN'T CONTINUE         
DEATH04  DC    H'00'               WHY CAN'T WE SET THE TIMER?                  
DEATH05  DC    H'00'               MQ CLOSE FAILED                              
DEATH06  DC    H'00'               MQ DISCONNECT FAILED                         
DEATH07  DC    H'00'               MQ PUT FAILED                                
DEATH08  DC    H'00'               MQ COMMIT FAILED                             
DEATH09  DC    H'00'               BAD RETURN FROM BLDL MACRO                   
DEATH10  DC    H'00'               UNKNOWN MQ COMPLETION CODE                   
DEATH11  DC    H'00'               GIN TABLE FULL                               
DEATH12  DC    H'00'               COMPANY CODE DOES NOT MATCH                  
DEATH13  DC    H'00'               BAD COMPRESSION                              
*                                                                               
       ++INCLUDE DDMQREASON                                                     
         EJECT                                                                  
*                                                                               
$$DATA   LOCTR ,                                                                
*                                                                               
***********************************************************************         
* MQ AREAS                                                            *         
***********************************************************************         
         SPACE 1                                                                
QMGRA    DC    CL48'MQ1L'                                                       
QMGRT    DC    CL48'MQTL'                                                       
QUEUET   DC    CL48'LDDS.BROKER.TEST.LOCALQ'                                    
QUEUEA   DC    CL48'LDDS.BROKER.LOCALQ'                                         
QLABELA  DC    CL16'RESOURCEMGR*****'                                           
*                                                                               
QCORID   DC    XL(L'MSGDESC_CORRELID)'00'                                       
QMSGID   DC    XL(L'MSGDESC_MSGID)'00'                                          
*                                                                               
         ORG                                                                    
*                                                                               
*                                  TEMPLATES FOR WORKING COPIES                 
*                                                                               
PUTOPTSC CMQPMOA LIST=YES,DSECT=NO PUT MESSAGE OPTIONS                          
PUTOPTSX EQU   *                                                                
GETOPTSC CMQGMOA LIST=YES,DSECT=NO GET MESSAGE OPTIONS                          
GETOPTSX EQU   *                                                                
*                                                                               
OBJDESCC CMQODA  LIST=YES,DSECT=NO OBJECT DESCRIPTOR                            
OBJDESCX EQU   *                                                                
MSGDESCC CMQMDA  LIST=YES,DSECT=NO MESSAGE DESCRIPTOR                           
MSGDESCX EQU   *                                                                
         ORG     MSGDESCC_FORMAT                                                
         DC      CL8'MQSTR   '     MQFMT_STRING  FOR DATA FORMAT                
         ORG                                                                    
*                                                                               
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
BPX1QGT  DC    CL8'BPX1QGT '                                                    
BPX1QRC  DC    CL8'BPX1QRC '                                                    
BPX1QSN  DC    CL8'BPX1QSN '                                                    
*                                                                               
VHEXOUT  DC    V(HEXOUT)                                                        
VDATCON  DC    V(DATCON)                                                        
VACRECTY DC    V(ACRECTYP)                                                      
VHELLO   DC    V(HELLO)                                                         
*                                                                               
ARZERO   DC    16F'0'                                                           
*                                                                               
SPACES   DC    166C' '                                                          
NULLS    DC    16X'00'                                                          
*                                                                               
ACCMST   DC    CL8'ACCMST'                                                      
*                                                                               
PRTDCB   DCB   DSORG=PS,MACRF=PM,DDNAME=PRTXXXXX,RECFM=FBA,LRECL=(166)          
PRTDCBLQ EQU   *-PRTDCB                                                         
*                                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
$$CODE   LOCTR ,                                                                
***********************************************************************         
* CONTEXT WORK AREA                                                   *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE DDMITMATTD                                                     
         SPACE 1                                                                
ATTD     DSECT                                                                  
         ORG   ATTWORK             DEFINE OUR OWN STORAGE                       
                                                                                
***********************************************************************         
* PARAMETER LISTS FOR MQSERIES CALLS                                  *         
* F    A(ROUTINE)                                                     *         
* CL16 EBCDIC ROUTINE NAME                                            *         
* XL1  FLAGS                                                          *         
* XL3  SPARE                                                          *         
* PARAMETERS (STANDARD IBM FORMAT)                                    *         
***********************************************************************         
         SPACE 1                                                                
ENTRYPTS DS    0F                                                               
ENTRYPTN DS    H                   NUMBER OF TABLE ENTRIES                      
ENTRYPTL DS    H                   MUST REMAIN AS 60                            
ENTRYSTQ EQU   *                                                                
*                                                                               
CSQBCLOS DS    CL60                AREAS FOR BLDL                               
CSQBCOMM DS    CL60                                                             
CSQBCONN DS    CL60                                                             
CSQBDISC DS    CL60                                                             
CSQBGET  DS    CL60                                                             
CSQBOPEN DS    CL60                                                             
CSQBPUT  DS    CL60                                                             
*                                                                               
ENTRYLSQ EQU   *                                                                
         SPACE 1                                                                
AMQBCONN DS    A                   STRUCTURE                                    
CONNEYE  DS    CL16                MQ_CONNECT                                   
         DS    X                                                                
         DS    XL3                                                              
CONNQMGR DS    XL1,AL3             QMGR                                         
CONNCONN DS    XL1,AL3             HCONN                                        
CONNCOMP DS    XL1,AL3             COMPCODE                                     
CONNREAS DS    XL1,AL3             REASON                                       
*                                                                               
AMQBOPEN DS    A                   STRUCTURE                                    
OPENEYE  DS    CL16                MQ_OPEN                                      
         DS    X                                                                
         DS    XL3                                                              
OPENCONN DS    XL1,AL3             HCONN                                        
OPENDESC DS    XL1,AL3             OBJDESC                                      
OPENOPOP DS    XL1,AL3             OPENOPT                                      
OPENHOBJ DS    XL1,AL3             HOBJ                                         
OPENCOMP DS    XL1,AL3             COMPCODE                                     
OPENREAS DS    XL1,AL3             REASON                                       
*                                                                               
AMQBPUTN DS    A                   STRUCTURE                                    
PUTNEYE  DS    CL16                MQ_PUT                                       
         DS    X                                                                
         DS    XL3                                                              
PUTNCONN DS    XL1,AL3             HCONN                                        
PUTNHOBJ DS    XL1,AL3             HOBJ                                         
PUTNDESC DS    XL1,AL3             MSGDESC                                      
PUTNPTOP DS    XL1,AL3             PUTOPTS                                      
PUTNBLEN DS    XL1,AL3             BUFFLEN                                      
PUTNABUF DS    XL1,AL3                                                          
PUTNCOMP DS    XL1,AL3             COMPCODE                                     
PUTNREAS DS    XL1,AL3             REASON                                       
*                                                                               
AMQBGETN DS    A                   STRUCTURE                                    
         DS    CL16                MQ_GET                                       
         DS    X                                                                
         DS    XL3                                                              
GETNCONN DS    XL1,AL3             HCONN                                        
GETNHOBJ DS    XL1,AL3             HOBJ                                         
GETNDESC DS    XL1,AL3             MSGDESC                                      
GETNGTOP DS    XL1,AL3             GETOPTS                                      
GETNBLEN DS    XL1,AL3             BUFFLEN                                      
GETNABUF DS    XL1,AL3                                                          
GETNDLEN DS    XL1,AL3             DATALEN                                      
GETNCOMP DS    XL1,AL3             COMPCODE                                     
GETNREAS DS    XL1,AL3             REASON                                       
*                                                                               
AMQBCOMM DS    A                   STRUCTURE                                    
COMMEYE  DS    CL16                MQ_COMMIT                                    
         DS    X                                                                
         DS    XL3                                                              
COMMCONN DS    XL1,AL3             HCONN                                        
COMMCOMP DS    XL1,AL3             COMPCODE                                     
COMMREAS DS    XL1,AL3             REASON                                       
*                                                                               
AMQBCLOS DS    A                   STRUCTURE                                    
CLOSEYE  DS    CL16                MQ_CLOSE                                     
         DS    X                                                                
         DS    XL3                                                              
CLOSCONN DS    XL1,AL3             HCONN                                        
CLOSHOBJ DS    XL1,AL3             HOBJ                                         
CLOSCLOP DS    XL1,AL3             CLOSEOPT                                     
CLOSCOMP DS    XL1,AL3             COMPCODE                                     
CLOSREAS DS    XL1,AL3             REASON                                       
*                                                                               
AMQBDISC DS    A                   STRUCTURE                                    
DISCEYE  DS    CL16                MQ_DISCONNECT                                
         DS    X                                                                
         DS    XL3                                                              
DISCCONN DS    XL1,AL3             HCONN                                        
DISCCOMP DS    XL1,AL3             COMPCODE                                     
DISCREAS DS    XL1,AL3             REASON                                       
*                                                                               
***********************************************************************         
* MQSERIES CALL PARAMETERS                                            *         
***********************************************************************         
*                                                                               
HCONN    DS    F                   MQ QMGR CONNECTION HANDLE                    
HOBJ     DS    F                   OBJECT HANDLE                                
OPENOPT  DS    F                   MQOPEN OPTIONS                               
CLOSEOPT DS    F                   MQCLOSE OPTIONS                              
COMPCODE DS    F                   COMPLETION CODE                              
REASON   DS    F                   QUALIFIES COMPLETION CODE                    
BUFFLEN  DS    F                   LENGTH OF MQBUFFER AREA                      
DATALEN  DS    F                   LENGTH OF THE MESSAGE                        
*                                                                               
*                                  LOCAL COPIES OF OPTIONS ETC...               
*                                                                               
PUTOPTS  CMQPMOA LIST=YES,DSECT=NO PUT MESSAGE OPTIONS                          
GETOPTS  CMQGMOA LIST=YES,DSECT=NO GET MESSAGE OPTIONS                          
*                                                                               
OBJDESC  CMQODA  DSECT=NO,LIST=YES OBJECT DESCRIPTOR                            
MSGDESC  CMQMDA  DSECT=NO,LIST=YES MESSAGE DESCRIPTOR                           
*                                                                               
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
*                                                                               
QMGR     DS    CL48                                                             
QUEUE    DS    CL48                                                             
QLABEL   DS    CL16                                                             
         SPACE 1                                                                
         EJECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
FULL2    DS    F                                                                
*                                                                               
SAVERE   DS    A                                                                
SAVELOG  DS    A                                                                
SAVEPRM  DS    A                                                                
*                                                                               
DMCB     DS    6F                                                               
PARMS    DS    6F                                                               
PARMS2   DS    6F                                                               
*                                                                               
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
*                                                                               
SAVSTAT  DS    CL2                                                              
*                                                                               
COUNTER  DS    F                                                                
*                                                                               
ABPX1QGT DS    A                                                                
ABPX1QRC DS    A                                                                
ABPX1QSN DS    A                                                                
*                                                                               
PLIST    DS    16A                                                              
PLIST2   DS    16A                                                              
OPLIST   DS    A                   SINGLE ENTRY OPEN/CLOSE LIST                 
*                                                                               
RV       DS    F                   RETURNED VALUE                               
RCV      DS    F                   RETURN CODE                                  
RSN      DS    F                   REASON CODE                                  
*                                                                               
SEQN     DS    F                                                                
*                                                                               
AMSGBUFF DS    A                                                                
ACHANGE  DS    A                                                                
*                                                                               
AIOL     DS    A                                                                
ABUFFER  DS    A                                                                
*                                                                               
WAITSECS DS    A                                                                
*                                                                               
ARBLK    DS    A                   DYNALLOC REQUEST BLOCK POINTER               
*                                                                               
RBLK     DS    0XL20               DYNALLOC REQUEST BLOCK                       
RBLKLEN  DS    XL1                                                              
RBLKVERB DS    XL1                                                              
RBLKFLG1 DS    XL2                                                              
RBLKERR  DS    XL2                                                              
RBLKINFO DS    XL2                                                              
RBLKATXT DS    A                                                                
         DS    XL4                                                              
RBLKFLAG DS    XL4                                                              
*                                                                               
ATXT     DS    0A                  DYNALLOC TEXT POINTERS                       
ATXTDD   DS    A                                                                
ATXTSYSO DS    A                                                                
*                                                                               
TXTDD    DS    XL2,XL2,XL2,CL8     DYNALLOC DDNAME TEXT       (0001)            
TXTSYSO  DS    XL2,XL2             DYNALLOC SYSOUT CLASS TEXT (0018)            
*                                                                               
TIMERECB DS    F                                                                
STIMERID DS    XL4                                                              
*                                                                               
H_MQIN   DS    F,F                 HANDLE TO MESSAGE QUEUE                      
USSLEN   DS    F                   LENGTH OF MESSAGE FROM USS                   
*                                                                               
       ++INCLUDE DMUSSKEY          USS MESSAGE QUEUE ID                         
*                                                                               
         PRINT GEN                                                              
         BPXYMODE DSECT=NO                                                      
         PRINT NOGEN                                                            
*                                                                               
PRINTDCB DS    XL(PRTDCBLQ)        SYSPRINT DCB                                 
XMLOUT   DS    XL(PRTDCBLQ)        XMLOUT                                       
DATEWORK DS    CL16                                                             
PRNTDUB  DS    D                   FOR PRNT ROUTINE ONLY                        
*                                                                               
NOTIME   DS    C                                                                
PCC      DS    C                                                                
PLINE    DS    0CL166                                                           
PLINDAY  DS    CL2                 DD                                           
         DS    C                                                                
PLINTIME DS    CL11                HH:MM:SS.HH                                  
         DS    C                                                                
PLINSYS  DS    CL5                 SE:XX / SYSYY                                
         DS    C                                                                
P        DS    CL132                                                            
         DS    CL13                                                             
*                                                                               
POVER    DS    CL166               DON'T OVERFLOW INTO GIN TABLE                
*                                                                               
GINTABL  DS    32CL8               ACTIVE GINS                                  
GINTABLX EQU   *                                                                
*                                                                               
RECBLOCK DS    0CL(RECBLKX-RECTYPE)                                             
RECTYPE  DS    X                   FROM ACRECTYP                                
RECCOMP  DS    X                                                                
RECALPHA DS    CL2                 ALPHA FROM TABLE                             
RECFLAGS DS    C                                                                
RECSTRUC DS    XL4                 LEDGER STRUCTURE                             
RECSTRF  DS    XL4                 LEDGER FLAGS BBBB/NNNN                       
RECGIN   DS    CL8                 GIN FROM RECORD                              
*                                                                               
RECTYPET DS    0CL17               nn,Name                                      
RECTYPE1 DS    XL1                 NUM RECORD TYPE                              
RECTYPE2 DS    CL16                CHR RECTYPE RecordType                       
*                                                                               
RECBLKX  EQU   *                                                                
*                                                                               
       ++INCLUDE DMRCVRHDR         COPY HEADER                                  
       ++INCLUDE DMRCVREXT         COPY EXTENSION                               
*                                                                               
WORK     DS    XL256                                                            
WORK1    DS    XL64                                                             
CARD     DS    CL166                                                            
         DS    0D                                                               
*                                                                               
MSGDATAL EQU   14000                                                            
MSGBUFF  DS    A                                                                
MSGDATA  DS    XL(MSGDATAL+20)     USS MESSAGE                                  
*                                                                               
IOL      DS    XL4                                                              
BUFFER   DS    CL4096                                                           
*                                                                               
         SPACE 1                                                                
ATTWORKL EQU   *-ATTWORK           REST IS DDS STANDARD RD CHAIN                
*                                                                               
POSTED   EQU   X'40'                                                            
         EJECT                                                                  
***********************************************************************         
* USEFUL IBM DSECTS AND EQUATES                                       *         
***********************************************************************         
         SPACE 1                                                                
         PRINT GEN                                                              
         BPXYSHM                                                                
         BPXYSEM                                                                
         BPXYIPCP                                                               
         BPXYFTYP DSECT=NO                                                      
         BPXYOPNF DSECT=NO                                                      
         BPXYMSG                                                                
         PRINT NOGEN                                                            
         SPACE 1                                                                
         PRINT GEN                                                              
         DSECT                                                                  
         IEZCIB                                                                 
         IEZCOM                                                                 
         SPACE 1                                                                
* IHAASCB                                                                       
         PRINT OFF                                                              
         IHAASCB                                                                
         PRINT ON                                                               
* IHASDWA                                                                       
         PRINT OFF                                                              
         IHASDWA                                                                
         PRINT ON                                                               
*                                                                               
         BPXYSOCK                                                               
         BPXYAIO                                                                
         BPXYMSGF                                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
***********************************************************************         
* DDS DSECTS                                                          *         
***********************************************************************         
*DDSCANBLKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035DDMITMSAC 07/17/13'                                      
         END                                                                    
