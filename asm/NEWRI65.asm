*          DATA SET NEWRI65    AT LEVEL 117 AS OF 12/12/19                      
*PHASE T32065A                                                                  
*INCLUDE RECUP                                                                  
         TITLE 'T32065 - NETWORK/TALENT TRANSFER'                               
*----------------------------------------------------------------------         
* GHOA 115 15MAR18 SPSUG-1239  ELIM NEED AGENCY STATUSES: NX, SX, CX            
*----------------------------------------------------------------------         
T32065   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NETT**,RR=R2                                                 
         LA    R6,2048(RB)         NOTE RB,R6,RA BASE REGISTERS                 
         LA    R6,2048(R6)                                                      
         LA    RA,2048(R6)                                                      
         LA    RA,2048(RA)                                                      
         USING T32065,RB,R6,RA                                                  
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R5,ATWA                                                          
         USING T320FFD,R5                                                       
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         MVC   NBACLI,ANETWS1      ANETWS1=CLIENT REC I/O AREA                  
*                                                                               
         L     R7,ANETWS2          ANETWS2=WORKING STORAGE                      
         USING WORKD,R7                                                         
         AHI   R7,500                                                           
         XC    IOWRKND,IOWRKND                                                  
         MVC   AMYIO,ANETWS4       ANETWS4 = MY I/O AREA                        
         OI    NBINDS4,NBI4TRAF    CLIENT TRAFFIC OFFICE FILTERING              
         LA    R1,HEADSPC                                                       
         ST    R1,SPECS                                                         
         LA    R1,MYHEAD                                                        
         ST    R1,HEADHOOK                                                      
* SAVE NETWORK SYSTEM NUMBER FROM UTL                                           
         L     RF,TWAMASTC          POINT TO MASTC                              
         L     RF,MCUTL-MASTD(RF)   POINT TO UTL                                
         MVC   SVNETSYS,4(RF)       SAVE NET SYSTEM NUMBER                      
*                                                                               
         CLI   TWAFIRST,0          IF FIRST REQUEST                             
         BNE   *+12                                                             
         MVI   FRSTLAST,C'Y'       REQUEST RUNFRST/RUNLAST                      
         MVI   TWAFIRST,2          GET RUNLAST INDICATOR                        
*                                                                               
         CLI   TWAFIRST,X'FF'      THIS IS EQUIVALENT TO RUNLAST                
         JE    RP4                                                              
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   RP2                                                              
         L     R1,=A(NETLIST)                                                   
         ST    R1,NBANBUFF                                                      
         OI    NBINDS7,NBI7NTIO    EXPANDED BUFFER                              
         L     R1,=A(NTILIST)                                                   
         ST    R1,NBCNVNTI                                                      
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'   TRPACK                                  
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FE'                                                      
         BNE   *+6                                                              
         DC    H'00'                                                            
         MVC   ADTRPACK,0(R1)       SAVE ADDRESS OF TRPACK                      
*                                                                               
         BAS   RE,REPMOD                                                        
         BRAS  RE,CLOSWRKR                                                      
         B     XIT                                                              
*                                                                               
RP2      CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
*                                                                               
         L     R1,=A(XPRDMSK)      NEEDED IF REQUESTING PRODUCT GROUPS          
         CLI   OFFLINE,C'Y'        OFFLINE ONLY                                 
         BNE   *+8                                                              
         ST    R1,NBADRPRG                                                      
*                                                                               
         BAS   RE,EDITMOD                                                       
         B     XIT                                                              
                                                                                
*===============================================================                
* IGNORE RUNLAST SINCE FILES CLOSED WHEN TWAFIRST=X'FF'                         
*===============================================================                
                                                                                
RP4      CLI   MODE,RUNLAST                                                     
         JE    XIT                                                              
         TM    WHEN,X'20'          TEST RUNNING SOON                            
         BO    XIT                                                              
*                                                                               
* NOW ADD A REQUEST TO SEND THE FILE TO ER                                      
*                                                                               
         OPEN  (TR21REQ,(OUTPUT))                                               
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         L     RE,=A(REQCARD)                                                   
         MVC   2(2,RE),AGENCY     SET AGENCY CODE IN REQ                        
* INSERT REQUESTING ID IN REQUEST CARD                                          
         L     RF,TWAMASTC                                                      
         SR    R0,R0                                                            
         ICM   R0,3,MCORIGID-MASTD(RF)                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         L     RE,=A(REQCDID)                                                   
         UNPK  7(5,RE),DUB                                                      
*                                                                               
         L     R1,=A(TR21REQ)                                                   
         L     R0,=A(REQCARD)                                                   
         PUT   (1),(0)            PUT A REQUEST FOR TA99                        
*                                                                               
         CLOSE TR21REQ                                                          
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         BRAS  RE,AUDITWRK         SAVE WRKR FILES IN DATASET                   
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              EDIT ROUTINES                                                    
         SPACE                                                                  
EDITMOD  NTR1                                                                   
*                                                                               
EDITM1   MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
*                                                                               
         MVI   WRITEMC,C'N'                                                     
         CLI   OFFLINE,C'Y'                                                     
         BNE   EDIT00                                                           
         L     R1,TWAMASTC                                                      
         USING MCBLOCK,R1                                                       
         CLI   MCWRITE,C'Y'        WRITE=NO IN JCL                              
         BNE   *+8                                                              
         MVI   WRITEMC,C'Y'                                                     
*                                                                               
EDIT00   LA    R2,SPLTALH               TALENT AGENCY                           
         CLI   5(R2),0                                                          
         BE    EDINV                                                            
         OC    SPLTAL,SPACES                                                    
         CLI   OFFLINE,C'Y'             ONLINE ONLY                             
         BE    EDT10                                                            
         CLC   TWAAGY,=C'SJ'            SKIP TALENT AGY VALIDATION              
         BE    EDT10                    FOR SJR                                 
         BAS   RE,READTAL                                                       
         CLI   BYTE,X'FF'                                                       
         BE    ERRID                                                            
         CLI   BYTE,X'FE'                                                       
         BE    AUTHERR                                                          
EDT10    MVC   TALAGY,SPLTAL                                                    
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+8                                                              
         BRAS  RE,READTLOF              READ TALENT AGY RECORD OFFLINE          
         B     VAL01                                                            
         EJECT                                                                  
*                                                                               
         SPACE 1                                                                
* ON-LINE VALIDATION OF TALENT AGENCY FIELD                                     
*                                                                               
* VALIDATE SPLTAL FIELD BY READING FOR ITS TALENT AGENCY REC                    
* AND MATCHING ITS NETWORK USERID ELEM TO REQUESTING AGENCY'S ID                
* TO SEE IF REQ AGY IS ALLOWED ACCESS TO SPLTAL AGY                             
*                                                                               
READTAL  NTR1                                                                   
         BAS   RE,SWTAL        SWITCH TO TALENT SYSTEM                          
         BE    RDT20                                                            
         DC    H'0'                                                             
         SPACE 1                                                                
*                                                                               
RDT20    XC    MYKEY,MYKEY          READ FOR TALENT AGENCY RECORD               
         LA    R1,MYKEY                                                         
         USING TLAYD,R1                                                         
         MVI   TLAYCD,TLAYCDQ                                                   
         MVC   TLAYAGY,SPLTAL       SCREEN TAL AGY FIELD                        
         MVC   MYKEYSV,MYKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=CL8'TALDIR',MYKEY,MYKEY,0               
         CLC   MYKEY(32),MYKEYSV                                                
         BE    *+12                                                             
         MVI   BYTE,X'FF'          IF NOT FOUND SET BYTE                        
         B     RDT40               GET OUT                                      
         L     R2,AMYIO            YES/GET THE RECORD                           
         LA    R3,MYKEY+34                                                      
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'TALFILE ',(R3),(R2),MYDMWRK           
*                                                                               
* - READ CONTROL REC OF REQUESTING AGENCY TO GET AGENCY'S FULL ID               
         DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING CTIKEY,R3                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,TWAORIG           SET ORIGIN ID NUMBER                   
         MVC   AIO,NBAIO                                                        
         MVI   USEIO,C'Y'                                                       
         MVC   FILENAME,=CL8'CTFILE'                                            
         GOTO1 HIGH                                                             
         MVI   ELCODE,2                                                         
         MVC   HALF,NBDTADSP                                                    
         MVC   NBDTADSP,=H'28'                                                  
         L     R2,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST EXIST/ELSE BOMB                         
         ZIC   R1,1(R2)                                                         
         A     R1,=F'-3'                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),2(R2)              SET AGENCY ID TO WORK                 
         XC    FILENAME,FILENAME                                                
         MVI   USEIO,0                                                          
*                                                                               
* - RETURN TO TALENT AGENCY REC AND MATCH WITH REQUESTING AGENCY'S ID           
         L     R2,AMYIO            POINT R2 TO TALENT RECORD                    
         MVI   ELCODE,X'2A'        FIND AGY ELEMENT                             
         MVC   NBDTADSP,=H'40'                                                  
*&&DO                              SPSUG-1239                                   
         BAS   RE,GETEL                                                         
         BNE   RDT33                                                            
*                                                                               
         USING TAAYEL,R2                                                        
         TM    TAAYSTA5,TAAYNETI   AGENCY ALLOWED                               
         BO    RDT40                                                            
*                                                                               
* - RETURN TO TALENT AGENCY REC AND MATCH WITH REQUESTING AGENCY'S ID           
RDT33    L     R2,AMYIO            POINT R2 TO TALENT RECORD                    
         MVI   ELCODE,X'16'        FIND NETWORK USERID LIMIT ELEM               
         MVC   NBDTADSP,=H'40'                                                  
         BAS   RE,GETEL                                                         
         BE    *+12                                                             
         MVI   BYTE,X'FE'          NOT FOUND / INVALID ID                       
         B     RDT40                                                            
         MVC   NBDTADSP,HALF       RESET NBDTADSP                               
*                                                                               
         USING TANLEL,R2                                                        
         ZIC   R3,TANLNUID         GET NUMBER OF USERIDS                        
         N     R3,=X'0000007F'     STRIP HOB                                    
         LA    R4,6                                                             
         TM    TANLNUID,TANLSPID   ARE WE USING 10 INSTEAD?                     
         BZ    *+8                                                              
         LA    R4,10                                                            
*                                                                               
         LA    R2,TANLUID          POINT TO USERID                              
RDT35    LR    RF,R4               IS IT VALID USERID                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),WORK        IS IT VALID USERID                           
         BE    RDT40               YES                                          
         AR    R2,R4               NO/TRY AGAIN                                 
         BCT   R3,RDT35                                                         
         MVI   BYTE,X'FE'          INVALID ID                                   
*&&                                                                             
RDT40    MVC   SYSDIR,SVSYSDIR     RESTORE CALLER'S FILES, ETC.                 
         MVC   SYSFIL,SVSYSFIL                                                  
         BAS   RE,SWBACK           SWITCH BACK TO ORIGINAL SYSTEM               
         BE    *+6                                                              
         DC    H'0'                                                             
RDT50    NETGO NVSETUNT,DMCB                                                    
         MVI   NBFUNCT,NBFRDHI                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO CONTROL SYSTEM SWITCHING                             
         SPACE 2                                                                
SWTAL    DS    0H                                                               
         MVI   DMCB,TAL1SE         SET TO SWITCH TO TAL1                        
         B     SWSYS                                                            
         SPACE 2                                                                
SWBACK   DS    0H                                                               
         MVC   DMCB(1),SVSYS       SET TO SWITCH BACK TO ORIG. SYSTEM           
         B     SWSYS                                                            
         SPACE 2                                                                
SWSYS    NTR1                                                                   
         CLI   SVSYS,TAL1SE        DON'T BOTHER IF ORIG. SYS IS TALENT          
         BE    SWX                                                              
         CLI   SVSYS,TAL2SE                                                     
         BE    SWX                                                              
         MVC   DMCB+1(3),=X'FFFFFF'                                             
         PRINT GEN                                                              
         GOTO1 SWITCH,DMCB,,0                                                   
         PRINT NOGEN                                                            
         CLI   4(R1),0             RETURN CC                                    
         BE    *+6                                                              
         DC    H'0'                SWITCH ERROR                                 
SWX      XIT1                                                                   
*                                                                               
TAL1SE   EQU   X'10'                                                            
TAL2SE   EQU   X'20'                                                            
*                                                                               
         EJECT                                                                  
* SCREEN FIELD VALIDATION ROUTINES                                              
*                                                                               
VAL01    MVI   FTERMFLG,0          FOLLOWING FIELDS ARE OPTIONAL                
         MVI   SVOPTSW,0                                                        
         LA    R2,SPLCLTH          CLIENT                                       
         NETGO NVCLIALL,DMCB                                                    
*                                                                               
         LA    R2,SPLPRDH          PRODUCT                                      
         NETGO NVPRDALL,DMCB                                                    
*                                                                               
         MVI   NBSELEST,0          FUDGE ESTIMATE                               
*                                                                               
         LA    R2,SPLNETH               NETWORK                                 
         NETGO NVNETALL,DMCB                                                    
         MVI   DOCABLE,0                                                        
         CLC   =C'ALL,C',SPLNET    ALL,C                                        
         BNE   VAL03                                                            
**       CLI   OFFLINE,C'Y'                                                     
**       BE    *+12                                                             
**       CLI   1(R5),C'*'          DDS TERMINALS ONLY                           
**       BNE   EDINV                                                            
         MVI   DOCABLE,C'Y'                                                     
*                                                                               
VAL03    LA    R2,SPLCMLH               COMMERCIAL FILTER                       
         CLC   =C'ALL',SPLCML                                                   
         BE    VAL10                                                            
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   COMMFLT,SPLCML                                                   
*                                                                               
VAL10    MVI   FTERMFLG,1          FOLLOWING FIELDS ARE REQUIRED                
         LA    R2,SPLRSTRH              START DATE                              
         BAS   RE,VALRFP                                                        
         BE    VAL15                                                            
*                                                                               
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
VAL15    LA    R2,SPLRENDH              END DATE                                
         BAS   RE,VALRFP                                                        
         BE    VAL18                                                            
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
VAL18    LA    R2,SPLTSTH          TEST RUN   ACCEPT Y/N/P                      
         MVI   UPDATE,C'N'                                                      
         CLI   8(R2),C'Y'          IS IT TEST RUN                               
         BE    VAL30               YES                                          
*                                                                               
**       CLI   DOCABLE,C'Y'        IF CABLE                                     
**       BE    EDINV               MUST BE TEST RUN                             
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         CLI   8(R2),C'N'          IS IT TEST RUN                               
         BNE   VAL25               NO/UPDATE FILE                               
         CLC   TWAAGY,=C'SJ'       IF AGENCY IS SJR                             
         BNE   VAL30                                                            
         BRAS  RE,CKSYS            AND IF DSPACE = A,                           
         BE    EDINV               DO NOT ALLOW TEST RUN = NO                   
         B     VAL30                                                            
*                                                                               
VAL25    MVI   UPDATE,C'P'         POST ONLY                                    
         CLI   8(R2),C'P'          CREATE WORKERFILE/NOT MARK UNITS             
         BNE   EDINV                                                            
*                                                                               
VAL30    CLI   UPDATE,C'N'         UPDATE=NO                                    
         BE    *+12                                                             
         CLI   TWAWHEN,2           IF SOON                                      
         BE    EDINV                                                            
         MVI   MARKED,0             DEFAULT ONLY UNMARKED COMMERCIALS           
         MVI   UNMARK,0                                                         
         LA    R2,SPLOPTH           OPTIONS                                     
         CLI   5(R2),0                                                          
         BE    EDTX                                                             
         LA    R3,MYWORK                                                        
         GOTO1 SCANNER,DMCB,(R2),(5,(R3))                                       
         ZIC   R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BZ    EDINV                                                            
*                                                                               
VAL32    CLC   12(4,R3),=C'SKED'        SKED UNITS ONLY                         
         BNE   VAL33                                                            
         MVI   NBSELTRF,C'T'       TRAFFIC UNITS ONLY                           
         B     VAL50                                                            
*                                                                               
VAL33    CLI   12(R3),C'S'         SORT OPTION                                  
         BNE   VAL35                                                            
         CLC   22(3,R3),=C'CMT'    SORT BY COMMERCIAL TITLE                     
         BNE   EDINV                                                            
         MVI   SRTTYP,C'T'         COMMERCIAL TITLE                             
         B     VAL50                                                            
*                                                                               
VAL35    DS    0H                                                               
         CLC   12(3,R3),=C'ALL'       ALL COMMERCIALS/MARKED,UNMARKED           
         BNE   VAL40                                                            
         MVI   MARKED,C'A'                                                      
         B     VAL50                                                            
*                                                                               
VAL40    DS    0H                                                               
         CLC   12(6,R3),=C'MARKED'      MARKED COMMERCIALS ONLY                 
         BNE   VAL42                                                            
         MVI   MARKED,X'80'             SET MARKED FILTER                       
         B     VAL50                                                            
*                                                                               
VAL42    CLC   12(6,R3),=C'UNMARK'      UNMARK COMMERCIALS                      
         BNE   VAL45                                                            
         MVI   UNMARK,C'Y'             SET TO UNMARK UNITS                      
         MVI   MARKED,X'80'            SET MARKED FILTER                        
         B     VAL50                                                            
*                                                                               
VAL45    DS    0H                                                               
         CLC   12(2,R3),=C'MD'                     MARKED DATE                  
         BNE   VAL46                                                            
         GOTO1 DATVAL,DMCB,(0,22(R3)),WORK                                      
         GOTO1 DATCON,DMCB,(0,WORK),(2,MARKDAT)   COMPRESSED FORMAT             
         MVI   MARKED,X'80'                       MARKED UN ITS ONLY            
         B     VAL50                                                            
*                                                                               
VAL46    DS    0H                                                               
         CLC   12(3,R3),=C'MA '     MARKING AGENCY                              
         BNE   VAL47                                                            
         MVC   MARKAGY,22(R3)      SET MARKING AGENCY                           
         CLI   OFFLINE,C'Y'                                                     
**       BE    VAL50               PXZ TEMPORARY PER KARI                       
         B     VAL50                                                            
         BAS   RE,SWTAL            VALIDATE TALENT AGENCY                       
         MVI   BYTE,0                                                           
         XC    MYKEY,MYKEY                                                      
         LA    R1,MYKEY                                                         
         USING TLAYD,R1                                                         
         MVI   TLAYCD,TLAYCDQ                                                   
         MVC   TLAYAGY,MARKAGY                                                  
         MVC   MYKEYSV,MYKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=CL8'TALDIR',MYKEY,MYKEY,0               
         CLC   MYKEY(32),MYKEYSV                                                
         BNE   ERRID                                                            
         MVC   SYSDIR,SVSYSDIR     RESTORE CALLER'S FILES, ETC.                 
         MVC   SYSFIL,SVSYSFIL                                                  
         BAS   RE,SWBACK           SWITCH BACK TO ORIGINAL SYSTEM               
         BNE   ERRID                                                            
         NETGO NVSETUNT,DMCB                                                    
         MVI   NBFUNCT,NBFRDHI                                                  
         CLI   BYTE,0                                                           
         BE    VAL50                                                            
         B     ERRID               INVALID TALENT AGENCY                        
*                                                                               
VAL47    CLC   12(5,R3),=C'TRACE'   TRACE?                                      
         BNE   VAL48                                                            
         OI    SVOPTSW,OPTRACE                                                  
*                                                                               
VAL48    DS    0H                                                               
         B     EDINV                                                            
*                                                                               
VAL50    LA    R3,32(R3)                                                        
         BCT   R0,VAL32                                                         
                                                                                
* DO NOT ALLOW 'ALL' OR 'MARKED' FOR UPDATE UNLESS DDS TERMINAL                 
         CLI   OFFLINE,C'Y'                                                     
         BE    VAL52                                                            
         CLI   UPDATE,C'N'         TEST RUN?                                    
         BE    VAL52               YES                                          
         CLI   1(R5),C'*'          DDS TERMINALS ONLY                           
         BE    VAL52               YES                                          
         CLI   MARKED,C'A'         ELSE 'ALL' NOT ALLOWED                       
         BE    EDINV                                                            
         CLI   MARKED,X'80'        ELSE 'MARKED' NOT ALLOWED                    
         BE    EDINV                                                            
*                                                                               
VAL52    CLI   UNMARK,C'Y'         IF UNMARKING                                 
         BNE   EDTX                                                             
         CLI   MARKDAT,0           NEED MD=DATE                                 
         BE    EDINV                                                            
         CLI   MARKAGY,0           NEED MA=TALAGY                               
         BE    EDINV                                                            
         B     EDTX                                                             
*                                                                               
EDTX     MVC   TITLSV,SPACES                                                    
         MVC   UNDERS,SPACES                                                    
         LA    R2,SPLTITH                                                       
         CLI   5(R2),0                                                          
         BE    EDTXX                                                            
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TITLSV(0),SPLTIT                                                 
         GOTO1 CENTER,DMCB,TITLSV,40                                            
         GOTO1 UNDERLIN,DMCB,(40,TITLSV),UNDERS                                 
*                                                                               
EDTXX    DS    0H                                                               
* - NEED TO HAVE COPIES WRITTEN TO RECOVERY FILE                                
         ICM   RF,15,TWAMASTC                                                   
         BZ    EDITXX                                                           
         L     RF,MCSSB-MASTD(RF)                                               
         OI    SSOSTAT2-SSOOFF(RF),SSOSROLC   RECOVER OFFLINE COPIES            
*                                                                               
EDITXX   LA    R2,SPLCLTH                                                       
         XIT1  REGS=(R2)                                                        
*                                                                               
*                                                                               
EDINV    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
ERRID    DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(33),=C'*** ERROR - INVALID TALENT AGENCY'                
         GOTO1 ERREX2                                                           
AUTHERR  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(38),=C'*** ERROR - UNAUTHORIZED TALENT AGENCY'           
         GOTO1 ERREX2                                                           
         EJECT                                                                  
*                                                                               
* REPORT MODE / PROCESS UNITS                                                   
*                                                                               
         DROP R5                                                                
REPMOD   NTR1                                                                   
*  SET NBPRD -> 0 IF IT'S A FUDGED PROD NO FOR OVERFLOW PROD                    
         TM    NBINDS6,NBI6X1OV    IS NBPRD FALSE?                              
         BNO   *+8                                                              
         MVI   NBPRD,0                                                          
         TM    NBINDS6,NBI6X2OV    IS NBPRD FALSE?                              
         BNO   *+8                                                              
         MVI   NBPRD2,0                                                         
         BRAS  RE,GETTN2              GET TN2 PROFILE                           
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,COMPDAT)    TODAY'S DATE COMPRESSED         
         GOTO1 (RF),(R1),,(3,TODAYB)            AND 3-BYTE BINARY               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD       INIT SORTER                   
         B     RP05                                                             
                                                                                
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,116,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=500'                                   
         EJECT                                                                  
* - SET UP TO CALL NETIO                                                        
RP05     MVI   NBDATA,C'U'                        UNITS ONLY                    
         MVI   NBSELUOP,0                         ACTUAL/ESTIMATED              
         MVI   NBUSER+13,C'N'           OVERRIDE N0 PREEMPT PROFILE             
         MVI   NBSPLOPT,0                         DONT SPLIT UNIT               
         MVI   NBESTOPT,0          DON'T READ ESTIMATED DEMOS                   
         MVI   NBACTOPT,0          OR ACTUAL DEMOS                              
         OI    NBINDS6,NBI6TAL     NETIO IS CALLED FOR TALENT                   
         MVC   SVMEDFLT,NBSELMFL   SAVE MEDIA FILTER                            
         MVI   NBSELMFL,0          CLEAR MEDIA FILTER FOR NETIO                 
*                                                                               
*        NEED AN OPTION TO PICK UP SKEDS                                        
*                                                                               
         OI    NBDMGOPT,X'08'                     PASS DELETED UNITS            
         LA    R1,RP20                                                          
         ST    R1,NBHOOK                          SET UNIT HOOK                 
*        CLI   UPDATE,C'N'                                                      
*        BE    *+8                                                              
*        BRAS  RE,OPENWRKR                                                      
RP10     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBREQLST           ..IF EOF                               
         BNE   RP10                                                             
         BAS   RE,RDSORTER               ..READ SORTER AND PRINT REPORT         
         B     XIT                                                              
         EJECT                                                                  
* - NETIO HOOKS HERE                                                            
RP20     NTR1                                                                   
         MVI   NBUPUNIT,C'N'          NO WRITE                                  
         MVI   NBNOWRIT,C'N'                                                    
                                                                                
         CLI   NBMODE,NBPROCUN        UNITS ONLY                                
         BNE   RPX                                                              
********************************************************************            
         L     R2,NBAIO                                                         
         USING NURECD,R2                                                        
*        CLI   NUKEST,165                                                       
*        BNE   RPX                                                              
*        CLC   =C'SUNEWS',NUKPROG                                               
*        BNE   RPX                                                              
*        L     R2,NBAIO                                                         
*        MVI   ELCODE,X'21'                                                     
*        USING NUCMLEL,R2                                                       
*        BAS   RE,GETEL                                                         
*        BNE   RPX                                                              
*        CLC   =C'NZVC8922',NUCML1                                              
*        BNE   RPX                                                              
*        GOTO1 =V(PRNTBL),DMCB,=C'8922',NBAIO,C'DUMP',30,=C'1D'                 
******************************************************************              
         BRAS  RE,GETNTI              SET NBNTISTA CORRECTLY                    
                                                                                
         CLC   NBSELCLI,=C'ALL'                                                 
         BNE   RP20AAA                                                          
         BRAS  RE,GETTN22             GET TN2 PROFILE FOR NEW CLIENT            
         CLC   NBACTDAT,MYSVSTR2      IF AIR DATE TOO EARLY, SKIP               
         BL    RPX                                                              
*                                                                               
RP20AAA  CLI   SVMEDFLT,0             IF FILTERING BY MEDIA,                    
         BE    *+14                                                             
         CLC   SVMEDFLT,NBSTATYP      TALENT MEDIA MUST MATCH FILTER            
         BNE   RPX                                                              
         CLI   DOCABLE,C'Y'           CABLE ONLY?                               
         BNE   RP20A                                                            
         CLC   =C'PAX ',NBACTNET    IF PAX, SKIP                                
         BE    RPX                                                              
         CLC   =C'ION ',NBACTNET    IF ION, SKIP                                
         BE    RPX                                                              
         CLC   =C'CW',NBACTNET      IF CW, SKIP                                 
         BE    RPX                                                              
*                                                                               
         LA    RE,BNMTTAB           BOUNCE/METV TABLE                           
RP20AA   CLI   0(RE),X'FF'          END OF TABLE?                               
         BE    RP20AB                                                           
         CLC   0(4,RE),NBACTNET     SKIP IF BOUNCE ,METV, ANTENNA               
         BE    RPX                                                              
         LA    RE,4(RE)                                                         
         B     RP20AA                                                           
*                                                                               
RP20AB   CLI   NBSTATYP,C'C'          MUST BE CABLE                             
         BNE   RPX                                                              
* SPSUG-1239                                                                    
*        TM    SVAYSTA5,TAAYNETC      AGENCY STATUS CX MUST BE SET              
*        BNO   RPX                                                              
         CLI   SVNUSER,C'1'           NETTAL USER MUST BE 1                     
         BNE   RPX                                                              
         B     RP20BB                                                           
*                                                                               
RP20A    CLC   =C'PAX ',NBACTNET    IF PAX                                      
         BE    RP20B                                                            
         CLC   =C'PAX ',NBNTISTA    OR NTI PAX-ACCEPT IT                        
         BE    RP20B                                                            
         CLC   =C'ION ',NBACTNET    IF ION                                      
         BE    RP20B                                                            
         CLC   =C'ION ',NBNTISTA    OR NTI ION-ACCEPT IT                        
         BE    RP20B                                                            
         CLC   =C'CW',NBACTNET      IF CW                                       
         BE    RP20B                                                            
         CLC   =C'CW',NBNTISTA      OR NTI CW-ACCEPT IT                         
         BE    RP20B                                                            
                                                                                
         LA    RE,BNMTTAB           BOUNCE/METV TABLE                           
RP20AC   CLI   0(RE),X'FF'          END OF TABLE?                               
         BE    RP20ACC                                                          
         CLC   0(4,RE),NBACTNET     IF BOUNCE OR METV ACCEPT IT                 
         BE    RP20B                                                            
         LA    RE,4(RE)                                                         
         B     RP20AC                                                           
                                                                                
RP20ACC  LA    RE,BNMTTAB           BOUNCE/METV TABLE                           
RP20AD   CLI   0(RE),X'FF'          END OF TABLE?                               
         BE    RP20AE                                                           
         CLC   0(4,RE),NBNTISTA     IF BOUNCE OR METV ACCEPT IT                 
         BE    RP20B                                                            
         LA    RE,4(RE)                                                         
         B     RP20AD                                                           
                                                                                
RP20AE   CLI   NBSTATYP,C'N'          NETWORK                                   
         BE    RP20B                                                            
         CLI   NBSTATYP,C'S'          SYNDICATION                               
         BE    RP20B                                                            
         CLI   SVNUSER,C'1'           ONLY ALLOW CABLE IF TEST AGENCY           
         BNE   RPX                                                              
* SPSUG-1239                                                                    
*        TM    SVAYSTA5,TAAYNETC      AND CX STATUS SET ON AGENCY REC           
*        BNO   RPX                                                              
         CLI   NBSTATYP,C'C'          CABLE                                     
         BE    RP20BB                                                           
         B     RPX                                                              
                                                                                
RP20B    CLI   SVNUSER,C'1'           ONLY ALLOW NETWORK/SYNDICATION            
         BE    *+12                   IF SVNUSER = 1 OR Y                       
         CLI   SVNUSER,C'Y'                                                     
         BNE   RPX                                                              
* SPSUG-1239                                                                    
*        TM    SVAYSTA5,TAAYNETI      AND NX STATUS SET ON AGENCY REC           
*        BNO   RPX                                                              
                                                                                
RP20BB   CLC   CLTSV(3),NBCLICOD      IF CHANGE OF CLIENT                       
         BE    RP21                                                             
         MVC   CLTSV(3),NBCLICOD      SAVE CLIENT CODE                          
*                                                                               
         L     R2,NBACLI                                                        
         USING CLTHDR,R2                                                        
         MVC   CLTSV+4(20),CNAME      AND CLIENT NAME                           
                                                                                
         BAS   RE,PRDLIST             GET LIST OF VALID PRODUCTS                
         L     RE,=A(PRODTBL)                                                   
         OC    0(4,RE),0(RE)          EMPTY LIST?                               
         BNZ   RP21                   NO, CONTINUE                              
         CLC   =C'ALL',NBSELCLI       ALL CLIENTS?                              
         BE    RPX                    YES, CONTINUE                             
         MVI   NBMODE,NBREQLST        NO, SET EOF FOR NETIO TO LEAVE            
         B     RPX                                                              
                                                                                
* - SET MULTIRUN FLAG                                                           
RP21     DS    0H                    SET MULTIRUN FLAG                          
         MVI   MULTIRUN,0                                                       
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'60'                                                     
         USING NUOTH,R2                                                         
         BAS   RE,GETEL                                                         
         BNE   ENDMULTI                                                         
         CLI   NUOTTYP,C'L'        MULTI RUN?                                   
         BE    *+16                                                             
         BAS   RE,NEXTEL                                                        
         BE    *-12                                                             
         B     ENDMULTI                                                         
         MVC   MULTIRUN,NUOTHER                                                 
         DROP  R2                                                               
ENDMULTI EQU   *                                                                
                                                                                
* NOW HANDLE COMMERCIAL ELEMENT                                                 
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'21'                                                     
         USING NUCMLEL,R2                                                       
         BAS   RE,GETEL                                                         
         BNE   RPX                                                              
*                                                                               
                                                                                
         TM    NUCMLFLG,X'E8'         ..IF COMMERCIAL DELETED                   
         BZ    RP21A                  ..NO                                      
         TM    NUCMLFL2,X'C0'         ..YES/IF CMML NOT SENT                    
         BZ    RPX                    ..SKIP                                    
RP21A    OC    NUCML1(16),NUCML1      ..IF DUMMY X'21' (NO CMML)                
         BZ    RPX                    ..SKIP                                    
         TM    NBKEY+20,X'C0'         SKIP IF CLOSED OUT                        
         BO    RPX                                                              
         MVI   DPMFLAG,0           CLEAR DELETE/PREEMPT/MISSED FLAG             
                                                                                
                                                                                
* - IF DELETED UNIT / OR MISSED / OR PREEMPTED                                  
         TM    NBKEY+20,X'80'      ..IF DELETED                                 
         BO    RP21B                                                            
         TM    NBUNITST,X'40'      ..OR PREEMPTED                               
         BO    RP21B                                                            
         TM    NBUNITST,X'02'      ..OR MISSED                                  
         BZ    RP22                                                             
***                                                                             
RP21B    TM    NUCMLFL2,X'12'      ,,AND COMML SENT/DELETED/SENT                
         BZ    RP21D                                                            
         CLI   MARKED,C'A'         ,,AND ALL COMMLS REQUESTED                   
         BNE   RP21C                                                            
         CLI   UPDATE,C'Y'         ,,AND UPDATING                               
         BE    RPX                 ,,SKIP                                       
         B     RP21E               ,,ELSE OK                                    
*                                                                               
RP21C    CLI   MARKED,X'80'        ,,OR IF MARKED ONLY                          
         BE    RP21E               ,,OK                                         
         BNE   RPX                                                              
***                                                                             
RP21D    TM    NUCMLFL2,X'C0'      ..AND NOT SENT TO TALENT                     
         BZ    RPX                 ..SKIP                                       
*****    CLI   UNMARK,C'Y'         ..SKIP IF UNMARKING                          
*****    BE    RPX                                                              
RP21E    MVI   DPMFLAG,C'Y'        ..SET DELETED/PREEMPT/MISSED                 
                                                                                
* - IF CMMLS MARKED AS CHANGED BUT CMMLS HAVE NOT BEEN SENT TO TALENT           
* - TURN OFF CHANGED BITS                                                       
RP22     CLI   NUCMLTFL,0          IF ANY CMMLS CHANGED                         
         BE    RP25                                                             
         TM    NUCMLFL2,X'C0'      AND IF CMMLS NOT SENT                        
         BNZ   RP25                                                             
         MVI   NUCMLTFL,0          CLEAR CMMLS CHANGED BITS                     
                                                                                
* - IF UNMARKING                                                                
* - SKIP IF UNIT PRIOR TO REQUEST DATE                                          
RP25     CLC   NBACTDAT,REQSTDAT                                                
         BNL   RP27                                                             
         CLI   UNMARK,C'Y'         REQUESTING TO UNMARK                         
         BE    XIT                                                              
*                                                                               
RP27     BAS   RE,DOCM21           PASS COMMERCIAL INFO                         
*                                                                               
RP30     DS    0H                                                               
         L     R2,NBAIO               ARE THERE MULTIPLE FEEDS                  
         MVI   ELCODE,X'23'                                                     
         BAS   RE,GETEL                                                         
         BNE   RP50                                                             
         USING NUFDCEL,R2                                                       
* - IF CMMLS MARKED AS CHANGED BUT NOT SENT TO TALENT/CLEAR BITS                
         CLI   NUFDCTFL,0          IF CMMLS CHANGED                             
         BE    RP40                                                             
         TM    NUFDCFL2,X'0C'      IF NOT SENT TO TALENT                        
         BNZ   RP40                                                             
         MVI   NUFDCTFL,0          CLEAR CMML CHANGED BITS                      
*                                                                               
RP40     BAS   RE,DOCM23                                                        
*                                                                               
RP50     CLI   UPDATE,C'Y'            UPDATE UNIT                               
         BNE   RPX                    NO                                        
         CLI   WRITEMC,C'N'           WRITE=NO IN JCL                           
         BE    RPX                    YES/EXIT/DON'T WRITE                      
         SPACE                                                                  
         MVI   NBUPUNIT,C'Y'       SET WRITE UNIT SWITCH                        
         MVI   NBNOWRIT,C'Y'                                                    
*                                                                               
RPX      CLI   UPDATE,C'Y'                                                      
         BNE   *+8                                                              
         BRAS  RE,OPENWRKR                                                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* X'21' NATIONAL ELEMENT                                                        
*                                                                               
* ELEMENT LENGTH INCREASED TO CL80 /MUST DELETE OLD                             
* HELLO BACK NEW IF DEALING WITH OLD ELEMENT                                    
         SPACE                                                                  
DOCM21   NTR1                 NATIONAL COMMERCIAL                               
*****    GOTO1 =V(PRNTBL),DMCB,=C'BEFORE',NBAIO,C'DUMP',80,=C'1D'               
*****    GOTO1 =V(PRNTBL),DMCB,=C'BEFORE',0(R2),C'DUMP',80,=C'1D'               
                                                                                
         L     RE,=A(FEEDTBL)                                                   
         XC    0(250,RE),0(RE)     CLEAR FEED TABLE                             
                                                                                
         USING NUCMLEL,R2                                                       
* - IF OLD COMMERCIAL ELEMENT, MUST CHANGE LENGTH                               
         XC    MYELEM,MYELEM                                                    
         CLI   NUCMLELN,80         ARE WE DEALING WITH NEW ELEMENT              
         BE    DC2100              YES                                          
         ZIC   R1,NUCMLELN         NO/NEED TO FORMAT IT                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYELEM(0),0(R2)                                                  
         MVI   MYELEM+1,80         SET NEW ELEMENT LENGTH                       
*- DELETE OLD ELEMENT                                                           
         GOTO1 HELLO,DMCB,(C'D',=CL8'UNTFILE'),(X'21',NBAIO),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DS    H'0'                                                             
* - AND ADD NEW ELELENT                                                         
         DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'P',=CL8'UNTFILE'),NBAIO,MYELEM                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DS    H'0'                                                             
                                                                                
DC2100   DS    0H                                                               
         CLI   DPMFLAG,C'Y'        DELETED/PREEMPT/MISS                         
         BNE   DC2100A                                                          
         TM    NUCMLFL2,NUCMLFLS   IF 1ST CMML NOT ALREADY SENT                 
         BO    DC2100A                                                          
         TM    NUCMLFL2,NUCMLSDS   OR NOT SENT/MARKED/SENT                      
         BZ    DC213               SKIP                                         
                                                                                
* - SPECIAL PROCESSING FOR PRIOR UNITS                                          
DC2100A  MVI   PRIORUNT,0                                                       
         CLC   NBACTDAT,REQSTDAT   IF UNIT PRIOR TO REQ DATE                    
         BNL   DC211                                                            
         MVI   PRIORUNT,1                                                       
         TM    NUCMLFL2,NUCMLFLS   IF CMML1 SENT                                
         BZ    DC211                                                            
******   CHECK HERE IF MUTIRUN NUMBER HAS CHANGED                               
         TM    NUCMLTFL,X'F0'      AND CMML1 CHANGED                            
*->      BZ    DC213                                                            
         BZ    DC211                   PXZ6/21/02                               
         MVC   SENTCHG,NUCMLTFL    SET CMML1 SENT/CHANGED BITS TO FLAG          
         NI    SENTCHG,X'F0'       TURN OF CMML2 CHANGED BITS                   
         B     DC212A                                                           
*                                                                               
DC211    CLI   MARKED,C'A'         DO ALL COMMERCIALS                           
         BE    DC212               YES                                          
                                                                                
         MVI   BYTE,0              NO/SET FILTER BYTE                           
         TM    NUCMLFL2,NUCMLFLS   WAS CML SENT                                 
         BZ    *+8                                                              
         MVI   BYTE,X'80'                                                       
         CLI   DPMFLAG,C'Y'        IF DELETED/PREMT/MISS                        
         BE    DC211B              PASS IT                                      
******   CHECK HERE IF MUTIRUN NUMBER HAS CHANGED                               
         CLC   MARKED,BYTE         0=ONLY UNMARKED,X'80=MARKED                  
         BNE   DC213                                                            
DC211B   CLI   MARKDAT,0           ..FILTER ON MARKED DATE                      
         BE    *+14                                                             
         CLC   MARKDAT,NUCMLT1D     CHECK TALENT TRANSFER DATE                  
         BNE   DC213                                                            
         CLI   MARKAGY,0           ..FILTER ON MARKED AGENCY                    
         BE    *+14                                                             
         CLC   MARKAGY,NUCMLT1A    MATCH TALENT TRANSFER AGENCY                 
         BNE   DC213                                                            
                                                                                
DC212    CLI   COMMFLT,0           ..COMM FILTER                                
         BE    *+14                                                             
         CLC   COMMFLT,NUCML1                                                   
         BNE   DC213                                                            
DC212A   XC    WORK(3),WORK                                                     
         MVC   BYTE,NBPRD          PASS PROD BYTE                               
*************************************                                           
***      CLI   NBACTSUB,3                                                       
***      BNE   XIT                                                              
*************************************                                           
         CLI   NUCMLPRD,0          ..IS IT COPY SPLIT RPODUCT                   
         BE    *+10                                                             
         MVC   BYTE,NUCMLPRD       ..YES/PASS PROD BYTE FROM CML ELEM           
* SEE IF 3 CHAR CODES PASSED                                                    
         CLI   NBPR1CL3,0          NEW 3 CHAR PROD CODE ?                       
         BE    *+14                                                             
         MVC   WORK(3),NBPR1CL3      YES/USE IT                                 
         MVI   BYTE,0                    FORCE LOOKUP OFF WORK                  
         CLI   NUCMPROD,0           NEW 3 CHAR COPY SPLIT PROD HERE ?           
         BE    *+14                NO                                           
         MVC   WORK(3),NUCMPROD   *YES/SET 3 CHAR PROD                          
         MVI   BYTE,0            *CLEAR BYTE TO FORCE LOOKUP OFF WORK           
*                                                                               
         BAS   RE,PRDOK            MATCH PRD AGAINST LIST OF VALID PRDS         
         BNE   DC213                                                            
         MVC   PRDCD,WORK                                                       
         MVC   PRDSV1,BYTE         1 BYTE CODE                                  
         MVC   PRDSV3,WORK         3 CHAR CODE                                  
DC212C   XC    TALFEED,TALFEED                                                  
         MVC   TALCMID,NUCML1                                                   
         MVI   ADIDFLAG,C'N'                                                    
         TM    NUCMADFL,NUCMADF1   FIRST COMMERCIAL IS AD-ID?                   
         BZ    *+8                                                              
         MVI   ADIDFLAG,C'Y'                                                    
*                                                                               
         MVC   TALLNTH,NBLEN1      LENGTH OF 1ST PIGGY                          
         CLI   NBLEN1,0                                                         
         BNE   *+10                                                             
         MVC   TALLNTH,NBLEN                                                    
         MVI   BYTE,0              USE BYTE FOR MULTIPLE CMML FLAG              
DC212D   BAS   RE,GETCMT           COMMERCIAL TITLE                             
         CLI   TITLEN,X'FF'        .IF UNIT PROD NOT = CMML PROD                
         BNE   DC212E                                                           
         CLI   BYTE,0              .CHECK ANY MORE COVERED CMMLS                
         BE    DC213               .NO                                          
         B     DC212D              .YES                                         
                                                                                
DC212E   L     RE,ATWA                                                          
         USING T320FFD,RE                                                       
         ZIC   R1,TITLEN           TITLEN=CMML LEN-1                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TALTITLE(0),MYWORK                                               
         DROP  RE                                                               
*                                                                               
         OI    NUCMLFL2,NUCMLFLS   MARK AS SENT                                 
*                                                                               
         CLI   UNMARK,C'Y'         IS IT UNMARK                                 
         BNE   DC212EE                                                          
         TM    NUCMLFL2,NUCMLSDS   IF SENT/DELETED/SENT ON                      
         BZ    *+12                                                             
         NI    NUCMLFL2,X'FF'-NUCMLSDS     TURN THAT OFF                        
         B     DC212G                                                           
         NI    NUCMLFL2,X'FF'-NUCMLFLS     ELSE TURN OFF MARKED BITS            
         B     DC212G                                                           
*                                                                               
DC212EE  CLI   DPMFLAG,C'Y'        IF DELEETE/PREMT/MISS                        
         BNE   DC212G                                                           
         OI    NUCMLFL2,NUCMLSDS           MARK SENT/DELETED/SENT               
*                                                                               
DC212G   MVC   NUCMLT1D,COMPDAT    SET TODAYS COMPRESSED DATE                   
         MVC   NUCMLT1A,TALAGY     SET MARKING AGENCY                           
*                                                                               
         TM    NUCMLFL2,NUCMLSDS   IF SENT/DELETED/SENT                         
         BO    *+8                 DON'T ADD TO FEED TABLE                      
         BAS   RE,ADDFDTBL                                                      
                                                                                
         BAS   RE,DOTELEM          SET UP TALENT ELEMENTS                       
**************************************************************                  
******** GOTO1 =V(PRNTBL),DMCB,=C'DO21E',NBAIO,C'DUMP',80,=C'1D'                
******** GOTO1 =V(PRNTBL),DMCB,=C'DO21E',0(R2),C'DUMP',80,=C'1D'                
*************************************************************                   
         CLI   BYTE,0              ANY MORE COVERED CMMLS                       
         BNE   DC212D              YES                                          
         NI    NUCMLTFL,X'0F'      TURN OFF CMML1 CHANGED BITS                  
         B     DC213                                                            
         EJECT                                                                  
                                                                                
* - 2ND COMMERCIAL OF X'21' ELEMENT                                             
DC213    DS    0H                                                               
         MVI   SENTCHG,0                                                        
         OC    NUCML2,NUCML2       IS THERE COMMERCIAL FOR 2ND PROD             
         BZ    DC21X                                                            
*                                                                               
         CLI   DPMFLAG,C'Y'        IF DELETE/PREMPT/MISS                        
         BNE   DC213A                                                           
         TM    NUCMLFL2,NUCMLFS2   ..IF SECOND CMML NOT SENT                    
         BO    DC213A                                                           
         TM    NUCMLFL2,NUCMLSD2   ..OR NOT SENT/DELETED/SENT                   
         BZ    DC21X               ..SKIP IT                                    
                                                                                
DC213A   CLI   PRIORUNT,1          IF PRIOR UNIT                                
         BNE   DC213B                                                           
         TM    NUCMLFL2,NUCMLFS2   IF CMML 2 SENT                               
         BZ    DC213B                                                           
******   CHECK HERE IF MUTIRUN NUMBER HAS CHANGED                               
         TM    NUCMLTFL,X'0F'      AND CMML 2 CHANGED                           
*->      BZ    DC21X                   PXZ6/21/02                               
         BZ    DC213B                  PXZ6/21/02                               
         MVC   SENTCHG,NUCMLTFL    SET CMML2 CHANGED BITS TO FLAG               
         NI    SENTCHG,X'0F'       TURN OFF CMML 1 CHANGED                      
         B     DC215                                                            
DC213B   CLI   COMMFLT,0                                                        
         BE    *+14                                                             
         CLC   COMMFLT,NUCML2      COMMERCIAL FILTER                            
         BNE   DC21X                                                            
                                                                                
         CLI   MARKED,C'A'         DO ALL COMMERCIALS                           
         BE    DC215               YES/DO ALL                                   
                                                                                
         MVI   BYTE,0              NO/FILTER                                    
         TM    NUCMLFL2,NUCMLFS2   WAS 2ND COML SENT NUCMLFS2=X'40'             
         BZ    *+8                                                              
         MVI   BYTE,X'80'                                                       
         CLI   DPMFLAG,C'Y'             IF DELETED/PREMPT/MISS                  
         BE    DC214                    PASS IT                                 
******   CHECK HERE IF MUTIRUN NUMBER HAS CHANGED                               
         CLC   MARKED,BYTE         0=UNMARKED X'80'=SENT                        
         BNE   DC21X                                                            
DC214    CLI   MARKDAT,0           ..FILTER ON MARKED DATE                      
         BE    *+14                                                             
         CLC   MARKDAT,NUCMLT2D     CHECK TALENT TRANSFER DATE                  
         BNE   DC21X                                                            
         CLI   MARKAGY,0           ..FILTER ON MARKED AGENCY                    
         BE    *+14                                                             
         CLC   MARKAGY,NUCMLT2A    MATCH TALENT TRANSFER AGENCY                 
         BNE   DC21X                                                            
DC215    MVC   BYTE,NBPRD2         PASS PROD BYTE                               
         CLI   BYTE,0              IF ZERO                                      
         BNE   *+10                                                             
         MVC   BYTE,NUCMLPRD       SET COPY SPLIT PROD                          
         CLI   BYTE,0              IF ZERO                                      
         BNE   *+10                                                             
         MVC   BYTE,NBPRD          PASS MAIN PRD                                
***************************************************                             
* SEE IF 3 CHAR CODES PASSED                                                    
         CLI   NBPR2CL3,0          NEW 3 CHAR PROD CODE ?                       
         BE    *+14                                                             
         MVC   WORK(3),NBPR2CL3      YES/USE IT                                 
         MVI   BYTE,0                    FORCE LOOKUP OFF WORK                  
         CLI   NUCMPROD,0           NEW 3 CHAR COPY SPLIT PROD HERE ?           
         BE    *+14                NO                                           
         MVC   WORK(3),NUCMPROD   *YES/SET 3 CHAR PROD                          
         MVI   BYTE,0            *CLEAR BYTE TO FORCE LOOKUP OFF WORK           
*                                                                               
         BAS   RE,PRDOK            MATCH PRD AGAINST LIST OF VALID PRDS         
         BNE   DC21X                                                            
         MVC   PRDCD,WORK                                                       
         MVC   PRDSV1,BYTE         1 BYTE CODE                                  
         MVC   PRDSV3,WORK         3 CHAR CODE                                  
***************************************************                             
***      BAS   RE,PRDOK            PROD VALID FOR TALAGY                        
***      BNE   DC21X                                                            
***      MVC   PRDSV1,BYTE         SAVE CURRENT VALID PROD                      
***      BAS   RE,GETPRD           AND RETURNS PROD CODE                        
***      MVC   PRDCD,WORK          IN WORK                                      
         XC    TALFEED,TALFEED                                                  
         MVC   TALCMID,NUCML2                                                   
         MVI   ADIDFLAG,C'N'                                                    
         TM    NUCMADFL,NUCMADF2   2ND COMMERCIAL IS AD-ID                      
         BZ    *+8                                                              
         MVI   ADIDFLAG,C'Y'                                                    
*                                                                               
         ZIC   R1,NBLEN            TOTAL LENGTH                                 
         ZIC   R0,NBLEN1           LENGTH OF FIRST                              
         SR    R1,R0                                                            
         STC   R1,TALLNTH                                                       
         MVI   BYTE,0              USE BYTE FOR MULTIPLE CMML FLAG              
DC217    BAS   RE,GETCMT           COMMERCIAL TITLE                             
         CLI   TITLEN,X'FF'        .IF UNIT PROD NOT = CMML PROD                
         BNE   DC217B                                                           
         CLI   BYTE,0              .CHECK ANY MORE COVERED CMML                 
         BE    XIT                 .NO                                          
         B     DC217               .YES                                         
                                                                                
DC217B   L     RE,ATWA                                                          
         USING T320FFD,RE                                                       
         ZIC   R1,TITLEN           TITLEN=CMML LEN-1                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TALTITLE(0),MYWORK                                               
         DROP  RE                                                               
*                                                                               
         OI    NUCMLFL2,NUCMLFS2   MARK AS SENT                                 
*                                                                               
         CLI   UNMARK,C'Y'                   IF UNMARK                          
         BNE   DC217C                                                           
         TM    NUCMLFL2,NUCMLSD2         AND SENT/DEL/SENT                      
         BZ    *+12                                                             
         NI    NUCMLFL2,X'FF'-NUCMLSD2   TURN THAT OFF                          
         B     DC217D                                                           
         NI    NUCMLFL2,X'FF'-NUCMLFS2    ELSE TURN OFF MARK BITS               
         B     DC217D                                                           
*                                                                               
DC217C   CLI   DPMFLAG,C'Y'                  IF DELETED/PREEMPT/MISS            
         BNE   DC217D                                                           
         OI    NUCMLFL2,NUCMLSD2             SENT/DELETED/SENT                  
*                                                                               
DC217D   MVC   NUCMLT2D,COMPDAT    MARK WITH TODAY'S COMPRESSED DATE            
         MVC   NUCMLT2A,TALAGY     MARK AGENCY                                  
*                                                                               
         TM    NUCMLFL2,NUCMLSD2   IF SENT/DELETED/SENT                         
         BO    *+8                 DON'T ADD TO FEED TABLE                      
         BAS   RE,ADDFDTBL                                                      
*                                                                               
         BAS   RE,DOTELEM          SET UP TALENT ELEMENTS                       
**************************************************************                  
******   GOTO1 =V(PRNTBL),DMCB,=C'DO21B',NBAIO,C'DUMP',30,=C'1D'                
******   GOTO1 =V(PRNTBL),DMCB,=C'DO21B',0(R2),C'DUMP',30,=C'1D'                
**************************************************************                  
         CLI   BYTE,0              ANY MORE COVERED CMMLS                       
         BNE   DC217               YES                                          
         MVI   SENTCHG,0                                                        
         NI    NUCMLTFL,X'F0'      TURN OFF CMML 2 CHANGED BITS                 
DC21X    MVI   SENTCHG,0                                                        
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
* DO UNIT X'23' MULTIPLE FEED ELEMENT                                           
* IN CASE OF PIGGY BACKS CAN BE COMMERCIALS FOR BOTH OR EITHER PROD             
DOCM23   NTR1                                                                   
         USING NUFDCEL,R2                                                       
         BAS   RE,FORMX23          CHANGE ALL OLD ELEMS TO NEW LENGTH           
                                                                                
* - FIRST COMMERCIAL                                                            
DC230    DS    0H                                                               
******************************************************************              
***      GOTO1 =V(PRNTBL),DMCB,=C'DOCML1',NUFDCML1,C'DUMP',30,=C'1D'            
**       CLC   =C'MODY0113',NUFDCML1                                            
**       BNE   DC237                                                            
**       GOTO1 =V(PRNTBL),DMCB,=C'DOCML1A',NUFDCML1,C'DUMP',30,=C'1D'           
*******************************************************************             
                                                                                
         OC    NUFDCML1,NUFDCML1    COMMERCIAL ID                               
         BZ    DC237                                                            
                                                                                
*                                                                               
         CLI   DPMFLAG,C'Y'        IF UNIT DELETED/PREMPT/MISS                  
         BE    DC230AA                                                          
         TM    NUFDCFL2,X'80'      IF FEED DELETED                              
         BO    DC230AA                                                          
         B     DC230A              NO                                           
                                                                                
*                                  YES - CHECK IF COMMERCIALS SENT              
DC230AA  TM    NUFDCFL2,NUFDCFLS   ..IF COMML NOT SENT                          
         BO    DC230A                                                           
         TM    NUFDCFL2,NUFDCSDS   ..OR COMML NOT SENT/DELETED/SENT             
         BZ    DC237               ..SKIP                                       
                                                                                
DC230A   CLI   PRIORUNT,1          IF PRIOR UNIT                                
         BNE   DC230B                                                           
         TM    NUFDCFL2,NUFDCFLS   IF CMML1 SENT                                
         BZ    DC230B                                                           
         TM    NUFDCTFL,X'F0'      AND CMML1 CHANGED                            
*->      BZ    DC232                   PXZ6/21/02                               
         BZ    DC230B                  PXZ6/21/02                               
         MVC   SENTCHG,NUFDCTFL    SET CMML1 CHANGED BITS                       
         NI    SENTCHG,X'F0'       TURN OFF CMML 2 CHANGED BITS                 
         B     DC231A                                                           
DC230B   CLI   MARKED,C'A'         DO ALL COMMERCIALS                           
         BE    DC231               YES                                          
                                                                                
         MVI   BYTE,0              NO/FILTER MARKED/UNMARKED                    
         TM    NUFDCFL2,NUFDCFLS   WAS CMML 1 SENT                              
         BZ    *+8                                                              
         MVI   BYTE,X'80'                                                       
         CLI   DPMFLAG,C'Y'        IF DELETED/PREEMPT/MISS                      
         BE    DC230C              PASS IT                                      
         CLC   MARKED,BYTE         0=ONLY UNMARKED,X'80=MARKED                  
         BNE   DC232                                                            
DC230C   CLI   MARKDAT,0           ..FILTER ON MARKED DATE                      
         BE    *+14                                                             
         CLC   MARKDAT,NUFDCT1D     CHECK TALENT TRANSFER DATE                  
         BNE   DC232                                                            
         CLI   MARKAGY,0           ..FILTER ON MARKED AGENCY                    
         BE    *+14                                                             
         CLC   MARKAGY,NUFDCT1A    MATCH TALENT TRANSFER AGENCY                 
         BNE   DC232                                                            
DC231    CLI   COMMFLT,0           COMMERCIAL FILTER                            
         BE    *+14                                                             
         CLC   COMMFLT,NUFDCML1                                                 
         BNE   DC232                                                            
DC231A   MVC   BYTE,NBPRD          PASS PROD BYTE                               
         CLI   NUFDCPRD,0          ..IF COPY SPLIT PROD                         
         BE    *+10                                                             
         MVC   BYTE,NUFDCPRD       ..PASS THAT                                  
***********************************************************                     
* SEE IF 3 CHAR CODES PASSED                                                    
         CLI   NBPR1CL3,0          NEW 3 CHAR PROD CODE ?                       
         BE    *+14                                                             
         MVC   WORK(3),NBPR1CL3      YES/USE IT                                 
         MVI   BYTE,0                    FORCE LOOKUP OFF WORK                  
         CLI   NUFDPROD,0          NEW 3 CHAR COPY SPLIT PROD HERE ?            
         BE    *+14                NO                                           
         MVC   WORK(3),NUFDPROD  *YES/SET 3 CHAR PROD                           
         MVI   BYTE,0            *CLEAR BYTE TO FORCE LOOKUP OFF WORK           
*                                                                               
         BAS   RE,PRDOK            MATCH PRD AGAINST LIST OF VALID PRDS         
         BNE   DC232                                                            
         MVC   PRDCD,WORK                                                       
         MVC   PRDSV1,BYTE         1 BYTE CODE                                  
         MVC   PRDSV3,WORK         3 CHAR CODE                                  
***********************************************************                     
*****    BAS   RE,FEEDPROD         IF MULTIPROD RETURNS PROD IN BYTE            
*****    BAS   RE,PRDOK            PROD VALID FOR TALENT AGENCY                 
****     BNE   DC232                                                            
****     MVC   PRDSV1,BYTE         SAVE CURRENT VALID PROD                      
****     BAS   RE,GETPRD           RETURNS PROD CODE                            
****     MVC   PRDCD,WORK          IN WORK                                      
         MVC   TALCMID,NUFDCML1                                                 
         MVI   ADIDFLAG,C'N'                                                    
         TM    NUFDADFL,NUFDADF1   FIRST COMMERCIAL IS AD-ID?                   
         BZ    DC231A5                                                          
         MVI   ADIDFLAG,C'Y'                                                    
         B     DC231A9                                                          
*                                                                               
DC231A5  OC    TALCMID,SPACES                                                   
DC231A9  MVC   TALFEED,NUFDCFED    FEED                                         
         MVC   TALLNTH,NBLEN1      LENGTH                                       
         CLI   NBLEN1,0                                                         
         MVC   TALLNTH,NBLEN       LENGTH                                       
         MVC   TALLNTH,NBLEN       1ST PROD LENGTH                              
         MVI   BYTE,0              USE BYTE FOR MULTIPLE CMML FLAG              
DC231B   BAS   RE,GETCMT           COMMERCIAL TITLE                             
         CLI   TITLEN,X'FF'        .IF UNIT PROD NOT = CMML PROD                
         BNE   DC231C                                                           
         CLI   BYTE,0              .CHECK IF ANY MORE COVERED CMML              
         BE    DC232               .NO                                          
         B     DC231B              .YES                                         
                                                                                
DC231C   BAS   RE,SKIPFEED         SAME PROD/LEN/COMMERCIAL ?                   
         BNE   DC231D              NO                                           
         OI    NUFDCFL2,NUFDCFLS   YES/MARK AS SENT                             
         MVC   NUFDCT1D,COMPDAT    MARK WITH TODAY'S COMPRESSED DATE            
         MVC   NUFDCT1A,=C'DUPFED' MARK WITH DUPLICATE FEED FLAG                
         B     DC232                   BUT DON'T SEND TO WRKFIL                 
*                                                                               
DC231D   L     RE,ATWA                                                          
         USING T320FFD,RE                                                       
         ZIC   R1,TITLEN           TITLEN=CMML LEN-1                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TALTITLE(0),MYWORK                                               
         DROP  RE                                                               
*                                                                               
         OI    NUFDCFL2,NUFDCFLS   MARK AS SENT                                 
*                                                                               
         CLI   UNMARK,C'Y'                                                      
         BNE   DC231G                                                           
         TM    NUFDCFL2,NUFDCSDS   IF SENT/D/SENT                               
         BZ    *+12                                                             
         NI    NUFDCFL2,X'FF'-NUFDCSDS TURN THAT OFF                            
         B     DC231H                                                           
         NI    NUFDCFL2,X'FF'-NUFDCFLS                                          
         B     DC231H                                                           
*                                                                               
DC231G   CLI   DPMFLAG,C'Y'                IF DELETE/PREEMT/MISS                
         BNE   DC231H                                                           
         OI    NUFDCFL2,NUFDCSDS            MARK SENT/DELETED/SENT              
*                                                                               
DC231H   MVC   NUFDCT1D,COMPDAT    MARK WITH TODAY'S COMPRESSED DATE            
         MVC   NUFDCT1A,TALAGY     MARK AGENCY                                  
*                                                                               
         TM    NUFDCFL2,NUFDCSDS   IF SENT/DELETED/SENT                         
         BO    *+8                 DON'T ADD TO FEED TABLE                      
         BAS   RE,ADDFDTBL                                                      
         BAS   RE,DOTELEM          SET UP TALENT ELEMENTS                       
         CLI   BYTE,0              ANY MORE COVERED CMMLS                       
         BNE   DC231B              YES                                          
         NI    NUFDCTFL,X'0F'      TURN OFF CMML 1 CHANGE BITS                  
         B     DC232               NO                                           
         EJECT                                                                  
* - 2ND COMMERCIAL                                                              
DC232    DS    0H                                                               
         MVI   SENTCHG,0                                                        
         OC    NUFDCML2,NUFDCML2   IS THERE COMMERCIAL FOR 2ND PROD             
         BZ    DC237                                                            
*                                                                               
         CLI   DPMFLAG,C'Y'        IF DELETED/PREEMT/MISS                       
         BNE   DC232A                                                           
         TM    NUFDCFL2,NUFDCFS2   IF COMML NOT SENT                            
         BO    DC232A                                                           
         TM    NUFDCFL2,NUFDCSD2   OR COMML NOT SENT/DELETED/SENT               
         BZ    DC237               SKIP                                         
                                                                                
DC232A   CLI   PRIORUNT,1          IF PRIOR UNIT                                
         BNE   DC232B                                                           
         TM    NUFDCFL2,NUFDCFS2   IF CMML2 SENT                                
         BZ    DC232B                                                           
         TM    NUFDCTFL,X'0F'      AND CMML2 CHANGED                            
*->      BZ    DC237                   PXZ6/21/02                               
         BZ    DC232B                                                           
         MVC   SENTCHG,NUFDCTFL    SET CMML 2 CHANGED BITS                      
         NI    SENTCHG,X'0F'       TURN OFF CMML 1 CHAGNED BITS                 
         B     DC234                                                            
*                                                                               
DC232B   CLI   COMMFLT,0           COMMERCIAL FILTER                            
         BE    *+14                                                             
         CLC   COMMFLT,NUFDCML2                                                 
         BNE   DC237                                                            
                                                                                
         CLI   MARKED,C'A'         DO ALL COMMERCIALS                           
         BE    DC234               YES                                          
                                                                                
         MVI   BYTE,0              NO/FILTER MARKED/UNMARKED                    
         TM    NUFDCFL2,NUFDCFS2   WAS IT SENT                                  
         BZ    *+8                                                              
         MVI   BYTE,X'80'                                                       
         CLI   DPMFLAG,C'Y'        IF DELETED/PREEMPT/MISS                      
         BE    DC232C              PASS IT                                      
         CLC   MARKED,BYTE         0=ONLY UNMARKED,X'80=MARKED                  
         BNE   DC237                                                            
DC232C   CLI   MARKDAT,0           ..FILTER ON MARKED DATE                      
         BE    *+14                                                             
         CLC   MARKDAT,NUFDCT2D     CHECK TALENT TRANSFER DATE                  
         BNE   DC237                                                            
         CLI   MARKAGY,0           ..FILTER ON MARKED AGENCY                    
         BE    *+14                                                             
         CLC   MARKAGY,NUFDCT2A    MATCH TALENT TRANSFER AGENCY                 
         BNE   DC237                                                            
DC234    MVC   BYTE,NBPRD2         PASS 2ND PROD BYTE                           
         CLI   BYTE,0                                                           
         BNE   *+10                                                             
         MVC   BYTE,NUFDCPRD       OR COPY SPLIT                                
         CLI   BYTE,0                                                           
         BNE   *+10                                                             
         MVC   BYTE,NBPRD          OR 1ST PROD                                  
**************************************************                              
***********************************************************                     
* SEE IF 3 CHAR CODES PASSED                                                    
         CLI   NBPR2CL3,0          NEW 3 CHAR PROD CODE ?                       
         BE    *+14                                                             
         MVC   WORK(3),NBPR2CL3      YES/USE IT                                 
         MVI   BYTE,0                    FORCE LOOKUP OFF WORK                  
         CLI   NUFDPROD,0          NEW 3 CHAR COPY SPLIT PROD HERE ?            
         BE    *+14                NO                                           
         MVC   WORK(3),NUFDPROD  *YES/SET 3 CHAR PROD                           
         MVI   BYTE,0            *CLEAR BYTE TO FORCE LOOKUP OFF WORK           
*                                                                               
         BAS   RE,PRDOK            MATCH PRD AGAINST LIST OF VALID PRDS         
         BNE   DC237                                                            
         MVC   PRDCD,WORK                                                       
         MVC   PRDSV1,BYTE         1 BYTE CODE                                  
         MVC   PRDSV3,WORK         3 CHAR CODE                                  
***********************************************************                     
**************************************************                              
***      BAS   RE,FEEDPROD         RETURNS PROD IN BYTE IF MULTIPRODS           
****     MVC   PRDSV1,BYTE         SAVE CURRENT VALID PROD                      
****     BAS   RE,GETPRD           RETURNS PROD CODE                            
****     BAS   RE,PRDOK            PROD VALID FOR TAL AGENCY                    
****     BNE   DC237                                                            
****     MVC   PRDCD,WORK          IN WORK                                      
         MVC   TALCMID,NUFDCML2    COMMERCIAL ID                                
         MVI   ADIDFLAG,C'N'                                                    
         TM    NUFDADFL,NUFDADF2   2ND COMMERCIAL IS AD-ID?                     
         BZ    *+8                                                              
         MVI   ADIDFLAG,C'Y'                                                    
*                                                                               
         MVC   TALFEED,NUFDCFED    FEED                                         
         ZIC   R1,NBLEN                                                         
         ZIC   R0,NBLEN1                                                        
         SR    R1,R0                                                            
         STC   R1,TALLNTH                                                       
         MVI   BYTE,0              USE BYTE FOR MULTIPLE CMML FLAG              
DC236    BAS   RE,GETCMT           COMMERCIAL TITLE                             
         CLI   TITLEN,X'FF'        .IF UNIT PROD NOT = CMML PROD                
         BNE   DC236B                                                           
         CLI   BYTE,0              .CHECK ANY MORE COVERED CMML                 
         BE    DC237               .NO                                          
         B     DC236               .YES                                         
                                                                                
DC236B   BAS   RE,SKIPFEED         SAME PRD AND CMML FEED?                      
         BNE   DC236C              NO                                           
         OI    NUFDCFL2,NUFDCFS2   YES/ MARK AS SENT                            
         MVC   NUFDCT2D,COMPDAT    MARK WITH TODAY'S COMPRESSED DATE            
         MVC   NUFDCT2A,=C'DUPFED' MARK WITH DUPLICATE FEED FLG                 
         B     DC237                    BUT SKIP IT                             
*                                                                               
DC236C   L     RE,ATWA                                                          
         USING T320FFD,RE                                                       
         ZIC   R1,TITLEN           TITLEN=CMML LEN-1                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TALTITLE(0),MYWORK                                               
         DROP  RE                                                               
*                                                                               
         OI    NUFDCFL2,NUFDCFS2   MARK AS SENT                                 
*                                                                               
         CLI   UNMARK,C'Y'                                                      
         BNE   DC236G                                                           
         TM    NUFDCFL2,NUFDCSD2    IS IT SDS                                   
         BZ    *+12                                                             
         NI    NUFDCFL2,X'FF'-NUFDCSD2 THEN TURN THAT OFF                       
         B     DC236H                                                           
         NI    NUFDCFL2,X'FF'-NUFDCFS2 ELSE TURN OFF MARK BITS                  
         B     DC236H                                                           
*                                                                               
DC236G   CLI   DPMFLAG,C'Y'              IF DELETE/PREEMPT/MISS                 
         BNE   DC236H                                                           
         OI    NUFDCFL2,NUFDCSD2         MARK AS SENT/DELETED/SENT              
*                                                                               
DC236H   MVC   NUFDCT2D,COMPDAT    MARK WITH TODAY'S COMPRESSED DATE            
         MVC   NUFDCT2A,TALAGY     MARK AGENCY                                  
*                                                                               
         BAS   RE,ADDFDTBL         ADD TO FEED TABLE                            
         BAS   RE,DOTELEM          SET UP TALENT ELEMENTS                       
         CLI   BYTE,0              ANY MORE COVERED CMMLS                       
         BNE   DC236               YES                                          
         NI    NUFDCTFL,X'F0'      TURN OFF CMML 2 BITS                         
*                                                                               
DC237    MVI   ELCODE,X'23'        ARE THERE MORE FEED ELEMENTS                 
         BAS   RE,NEXTEL                                                        
         BE    DC230                                                            
         B     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
*                                                                               
* FORMAT ALL X'23' ELEMENTS TO NEW ELEMENT LENGTH OF 80                         
*                                                                               
FORMX23  NTR1                                                                   
         SPACE                                                                  
FORM10   CLI   1(R2),NUFDCELN      IF NEW ELEMENT/SKIP                          
         BE    FORM30                                                           
         XC    MYELEM,MYELEM                                                    
         ZIC   R1,1(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYELEM(0),0(R2)                                                  
         MVI   MYELEM+1,NUFDCELN   SET NEW LENGTH                               
* - DELETE OLD ELEMENT                                                          
         GOTO1 =V(RECUP),DMCB,(C'U',NBAIO),0(R2),0                              
         DS    0H                                                               
* - NOW ADD NEW ELEMENT                                                         
         GOTO1 HELLO,DMCB,(C'P',=CL8'UNTFILE'),NBAIO,MYELEM                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,NBAIO         START FROM TOP / LOOK FOR OLD ELEMS             
         BAS   RE,GETEL                                                         
         B     *+8                                                              
FORM30   BAS   RE,NEXTEL                                                        
         BE    FORM10                                                           
         L     R2,NBAIO         EOF/GET 1ST X'23' ELEM AND RETURN               
         BAS   RE,GETEL                                                         
         BE    XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
* SET UP TALENT ELEMENTS IN MYWORK AND PASS TO SORTER                           
* TALENT FIELDS FILLED BY DOCM21 OR DOCM23 ROUTINES                             
*                                                                               
DOTELEM  NTR1                                                                   
         LA    RE,MYWORK                                                        
         LA    RF,400                                                           
         XCEF                                                                   
         LA    R3,MYWORK                                                        
         USING SORTD,R3                                                         
         SPACE                                                                  
* - SET UP SORT KEY                                                             
         SPACE                                                                  
         MVC   SCLI,CLTSV          CLIENT CODE                                  
         MVC   SPRD,PRDCD          PRODUCT CODE                                 
         MVC   SCOM(8),TALCMID     COMMERCIAL ID                                
                                                                                
         L     RE,ATWA                                                          
         USING T320FFD,RE                                                       
         CLI   SRTTYP,C'T'         ..OPTION TO USE COMMERCIAL TITLE             
         BNE   *+10                                                             
         MVC   SCOM,TALTITLE       ..YES/USE TITLE                              
         DROP  RE                                                               
                                                                                
         MVC   SAMPRD(3),NBACTAM       SAVE AGY/MED,CLIENT                      
         MVC   SAMPRD+3(3),PRDCD       AND PROD CODE FOR PROD NAME READ         
         MVC   SAMDAT,NBACTDAT                                                  
         MVC   SAMDATNM,NBACTSUB                                                
         MVC   SDFEED,TALFEED                                                   
         MVC   SDADID,TALADID                                                   
         MVC   SDCOMID,TALCMID    COMMERCIAL ID                                 
         SPACE                                                                  
* - COMMERCIAL ID ELEMENT                                                       
         SPACE                                                                  
         LA    R3,SDATA            BUMP TO DATA PORTION OF SORT RECORD          
         USING TANXD,R3                                                         
         MVI   TANXEL,TANXELQ      SET ELEMENT CODE                             
         MVI   TANXLEN,TANXLNQ     SET LENGTH                                   
         MVC   TANXAGY,TALAGY      REQUESTED TALENT AGENCY                      
         MVC   TANXNCID,TALCMID    COMMERCIAL ID                                
         MVC   TANXSEC,TALLNTH     LENGTH                                       
         OC    TALADID,TALADID     WAS THERE AN AD-ID                           
         BZ    DOT03                                                            
         MVC   TANXADID,TALADID    AD-ID                                        
         MVI   TANXLEN,TANXLN2Q    NEW LENGTH                                   
         CLI   PCKFLAG,C'Y'        IF COMMERCIAL IS PACKED,                     
         BNE   DOT03                                                            
         OI    TANXSTAT,TANXPACK   SET STATUS FOR PACKED COMM                   
DOT03    MVI   TANXMED,TANXMTV     MEDIA = TV FOR HOLD RECORDS                  
                                                                                
* - UNMARKING                                                                   
         CLI   UNMARK,C'Y'                                                      
         BNE   *+8                                                              
         OI    TANXCCDE,TANXCUNM   UNMARK FLAG                                  
* - DELETED UNIT                                                                
         TM    NBKEY+20,X'C0'      DELETED                                      
         BZ    *+8                                                              
         OI    TANXCCDE,X'10'      SET NOT AIRED BIT                            
         TM    NBUNITST,X'40'      PREEMPTED                                    
         BZ    *+8                                                              
         OI    TANXCCDE,X'10'      SET NOT AIRED                                
         TM    NBUNITST,X'02'      MISSED                                       
         BZ    *+8                                                              
         OI    TANXCCDE,X'10'      SET NOT AIRED                                
***      TM    NBUNITST,X'01'      MAKE-GOOD                                    
***      BZ    *+8                                                              
***      OI    TANXCCDE,X'40'      SET AS MAKE GOOD                             
                                                                                
* - IF CMML PREVIOUSLY SENT AND THEN UNIT CHANGED                               
         CLI   SENTCHG,0           REASON FOR CHANGE 0=NO CHANGE                
         BE    DOT05                                                            
         TM    SENTCHG,X'88'                                                    
         BZ    *+8                                                              
         OI    TANXCCDE,8          LENGTH CHANGED                               
         TM    SENTCHG,X'44'                                                    
         BZ    *+8                                                              
         OI    TANXCCDE,4          PRODUCT CHANGED                              
         TM    SENTCHG,X'22'                                                    
         BZ    *+8                                                              
         OI    TANXCCDE,2          DATE CHANGED                                 
         TM    SENTCHG,X'11'                                                    
         BZ    *+8                                                              
         OI    TANXCCDE,1          CMML CHANGED                                 
         CLI   TANXCCDE,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
* - CLIENT CODE ELEMENT                                                         
         SPACE                                                                  
DOT05    ZIC   R1,TANXLEN          BUMP TO NEXT ELEM                            
         AR    R3,R1                                                            
*                                                                               
         USING TACTD,R3                                                         
         MVI   TACTEL,TACTELQ      CLIENT ELEMENT                               
         MVI   TACTLEN,TACTLNQ                                                  
         MVC   TACTCLI(3),CLTSV    SET CLIENT CODE                              
         OC    TACTCLI,SPACES                                                   
         SPACE                                                                  
* - PRODUCT CODE ELEMENT                                                        
         SPACE                                                                  
         CLI   PRDCD,0             IF UNALLOCATED                               
         BNE   *+6                                                              
         DC    H'00'               TRAP TO FIND MISSING PROD CODES              
***      BE    DOT10               SKIP PROD CODE EELEMENT                      
         ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         USING TAPRD,R3                                                         
         MVI   TAPREL,TAPRELQ      PRODUCT CODE ELEMENT                         
         MVI   TAPRLEN,TAPRLNQ                                                  
         MVC   TAPRPRD(3),PRDCD    SET PROD CODE                                
         OC    TAPRPRD,SPACES                                                   
         SPACE                                                                  
* - CLIENT NAME ELEMENT (VARIABLE LENGTH)                                       
         SPACE                                                                  
DOT10    ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         USING TAFND,R3                                                         
         MVI   TAFNEL,TAFNELQ      SET ELEM CODE                                
         MVI   TAFNTYPE,C'C'       SET TYPE (CLIENT)                            
         MVC   WORK(20),CLTSV+4    GET CLIENT NAME                              
         LA    R1,19               AND DROP END BLANKS                          
         LA    R2,WORK+19                                                       
DOT20    CLI   0(R2),X'40'                                                      
         BH    DOT25                                                            
         BCTR  R1,0                                                             
         BCTR  R2,0                                                             
         B     DOT20                                                            
DOT25    EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TAFNNAME(0),WORK                                                 
         LA    R1,4(R1)            ADD FOR CORRECT ELEMENT LENGTH               
         STC   R1,TAFNLEN                                                       
         AR    R3,R1               BUMP TO NEXT ELEM                            
         SPACE                                                                  
* - FREE FORM NAME ELEMENT                                                      
         SPACE                                                                  
         MVI   TAFNEL,TAFNELQ      SET ELEM CODE                                
         MVI   TAFNTYPE,C'T'       SET TYPE (TITLE)                             
                                                                                
         L     RE,ATWA                                                          
         USING T320FFD,RE                                                       
         ZIC   R1,TITLEN           TITLE LENGTH MINUS 1                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TAFNNAME(0),TALTITLE                                             
         DROP  RE                                                               
                                                                                
         LA    R1,4(R1)            ADD FOR CORRECT ELEMENT LENGTH               
         STC   R1,TAFNLEN                                                       
         AR    R3,R1               BUMP ELEMENT LENGTH                          
                                                                                
* - NETWORK/CLASS A PROGRAM DETAILS ELEMENT                                     
                                                                                
         USING TANPD,R3                                                         
* HANDLE FLAT RATE PROGRAMS HERE                                                
         CLC   =C'NBC ',NBACTNET     IF NBC BETWEEN 2AM-6AM FLAT RATE           
         BE    CHKFLAT                                                          
         CLC   =C'ABC ',NBACTNET     IF NBC BETWEEN 2AM-6AM FLAT RATE           
         BE    CHKFLAT                                                          
         CLC   =C'CBS ',NBACTNET     IF NBC BETWEEN 2AM-6AM FLAT RATE           
         BE    CHKFLAT                                                          
         CLC   =C'FOX ',NBACTNET     IF FOX BETWEEN 2AM-6AM FLAT RATE           
         BE    CHKFLAT                                                          
         B     NOTFLAT                                                          
CHKFLAT  CLC   NBTIME(2),=X'0257'    DOES SHOW START AFTER 559AM                
         BH    NOTFLAT               THEN NOT AFFECTED                          
         CLC   NBTIME+2(2),=X'00C9'  DOES SHOW END BEFORE 201AM                 
         BL    NOTFLAT               THEN NOT AFFECTED                          
         CLC   =C'FOX ',NBACTNET     IF FOX AND BETWEEN 2AM-6AM                 
         BNE   FLATRATE                                                         
         MVC   FULL(3),=XL3'B60401'  04/01/16                                   
         GOTO1 DATCON,DMCB,(1,FULL),(2,HALF)                                    
         CLC   NBACTDAT,HALF         USE DATE MUST BE 04/01/16 OR AFTER         
         BL    NOTFLAT               TO BE LATE NIGHT                           
         B     FLATRATE                                                         
NOTNBC   DS    0H                                                               
**       CLC   =C'ABC ',NBACTNET   IF ABC                                       
**       BNE    NOTABC                                                          
**       CLC   NBNTI,=H'30807'     AND IF WORLD NEWS TONIGHT                    
**       BE    FLATRATE                                                         
**NOTABC   CLC   =C'CBS ',NBACTNET   IF CBS                                     
**         BNE   NOTCBS                                                         
**         CLC   NBNTI,=H'31999'     AND IF UP TO THE MINUTE                    
**         BNE   NOTCBS                                                         
FLATRATE OI    TANPSTAT,TANPFLAT   SET FLAT BIT                                 
*                                                                               
NOTFLAT  DS    0H                                                               
         CLC   =C'PAX ',NBACTNET    IF PAX                                      
         BE    NFLAT10                                                          
         CLC   =C'PAX ',NBNTISTA    OR NTI PAX                                  
         BE    NFLAT10                                                          
         CLC   =C'ION ',NBACTNET    IF ION                                      
         BE    NFLAT10                                                          
         CLC   =C'ION ',NBNTISTA    OR NTI ION                                  
         BE    NFLAT10                                                          
*                                                                               
         LA    RE,BNMTTAB           BOUNCE/METV TABLE                           
NFLAT05  CLI   0(RE),X'FF'          END OF TABLE?                               
         BE    NFLAT07                                                          
         CLC   0(4,RE),NBACTNET     LOOK FOR BOUNCE OR METV                     
         BE    NFLAT10                                                          
         LA    RE,4(RE)                                                         
         B     NFLAT05                                                          
*                                                                               
NFLAT07  LA    RE,BNMTTAB           BOUNCE/METV TABLE                           
NFLAT08  CLI   0(RE),X'FF'          END OF TABLE?                               
         BE    NOTPAX                                                           
         CLC   0(4,RE),NBNTISTA     LOOK FOR BOUNCE OR METV                     
         BE    NFLAT10                                                          
         LA    RE,4(RE)                                                         
         B     NFLAT08                                                          
*                                                                               
NFLAT10  OI    TANPSTAT,X'20'      SET PAX RATE                                 
NOTPAX   DS    0H                                                               
************************************************                                
         MVI   TANPEL,TANPELQ      ELEM CODE                                    
*-->     MVI   TANPLEN,TANPLNQ2    ELEM LENGTH                                  
         MVI   TANPLEN,TANPLNQ3    .. NEW ELEM LENGTH                           
         CLC   =C'PAX ',NBACTNET    IS IT A PAX?                                
         BE    DOT28                                                            
         CLC   =C'ION ',NBACTNET    IS IT ION?                                  
         BE    DOT28                                                            
         CLC   =C'CW',NBACTNET      IS IT A CW?                                 
         BE    DOT28                                                            
*                                                                               
         LA    RE,BNMTTAB           BOUNCE/METV TABLE                           
DOT26    CLI   0(RE),X'FF'          END OF TABLE?                               
         BE    DOT27                                                            
         CLC   0(4,RE),NBACTNET     IS IT BOUNCE OR METV?                       
         BE    DOT28                                                            
         LA    RE,4(RE)                                                         
         B     DOT26                                                            
*                                                                               
DOT27    CLI   NBSTATYP,C'C'       CABLE STATION?                               
         BNE   DOT28                                                            
*-->     MVI   TANPLEN,TANPLNQN    NEW LENGTH FOR CABLE                         
         MVI   TANPLEN,TANPLNQ4    NEW LENGTH FOR CABLE                         
         BRAS  RE,GETMKTN          GET THE CABLE MARKET NAME                    
DOT28    MVC   TANPFEED,TALFEED    ..FOR FEED CODE                              
         MVI   TANPSEQ,0           SEQUENCE NUMBER                              
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(1,TANPDATE)    USE DATE                
         MVC   TANPPNME,NBPROGNM   PROGRAM NAME                                 
         OC    TANPPNME,SPACES                                                  
***      MVC   TANPNWK(4),NBACTNET                                              
         LA    R1,MYWORK                                                        
         USING SORTD,R1                                                         
         MVC   SDNTWK,NBACTNET     PASS NETWORK HERE/INSERT LATER               
*                                  INTO ELEMENT                                 
         MVC   SDSTATYP,NBSTATYP   MEDIA                                        
         CLC   =C'PAX ',NBACTNET    IS IT A PAX?                                
         BE    PAXIT                                                            
         CLC   =C'PAX ',NBNTISTA    IS IT A PAX?                                
         BE    PAXIT                                                            
         CLC   =C'ION ',NBACTNET    IS IT ION?                                  
         BE    PAXIT                                                            
         CLC   =C'ION ',NBNTISTA    IS IT ION?                                  
         BNE   *+8                                                              
PAXIT    MVI   SDSTATYP,C'X'       MAKE IT X                                    
         CLC   =C'ITN ',NBACTNET    IS IT ITN?                                  
         BE    ITNIT                                                            
         CLC   =C'ITN ',NBNTISTA    IS IT ITN?                                  
         BNE   *+12                                                             
ITNIT    MVI   SDSTATYP,C'I'       MAKE IT I                                    
         OI    TANPSTAT,TANPITN    MAKE IT I                                    
*                                                                               
         CLC   =C'PAX ',NBACTNET    IS IT A PAX?                                
         BE    DOT30                                                            
         CLC   =C'ION ',NBACTNET    IS IT ION?                                  
         BE    DOT30                                                            
*                                                                               
         CLC   =C'CW',NBACTNET    IS IT A CW?                                   
         BE    CWIT                                                             
         CLC   =C'CW',NBNTISTA    IS IT A CW?                                   
         BNE   *+12                                                             
CWIT     MVI   SDSTATYP,C'W'       MAKE IT W                                    
         B     DOT30                                                            
*                                                                               
         CLC   =C'MY',NBACTNET    IS IT A MY?                                   
         BE    MYIT                                                             
         CLC   =C'MY',NBNTISTA    IS IT A MY?                                   
         BE    MYIT                                                             
         CLC   =C'MNT',NBACTNET   IS IT MNT?                                    
         BE    MYIT                                                             
         CLC   =C'MNT',NBNTISTA   IS IT MNT?                                    
         BNE   *+12                                                             
MYIT     MVI   SDSTATYP,C'M'       MAKE IT M                                    
         B     DOT30                                                            
*                                                                               
         BRAS  RE,DIGINET         RECOGNIZE DIGINET NETWORKS                    
         BE    DOT30                                                            
*                                                                               
         BRAS  RE,UNWIRED         RECOGNIZE UNWIRED NETWORKS                    
         BNE   *+8                                                              
         OI    TANPSTAT,TANPITN   UNWIRED NETWORKS WORK LIKE ITN                
*                                                                               
         CLI   NBSTATYP,C'C'       CABLE STATION?                               
         BNE   DOT30                                                            
         MVI   TANPTYP,TANPNET     ELEMENT TYPE = NET                           
         CLC   TANPNTI,SPACES      IF CNET NOT PRESENT                          
         BH    DOT29                                                            
         MVC   TANPNTI,NBNTISTA    NTI STATION CODE                             
         OC    TANPNTI,TANPNTI     IF NTI NOT PRESENT                           
         BNZ   *+10                                                             
         MVC   TANPNTI,NBACTNET    USE 4 CHARACTER STATION CODE                 
*                                                                               
DOT29    BAS   RE,PADZERO          PAD STA CODE WITH ZEROES ON RIGHT            
*                                                                               
DOT30    MVC   SDMULTI,MULTIRUN    SET MULTIRUN TO SORT REC                     
         DROP  R1                                                               
*                                                                               
         TM    NBKEY+20,X'C0'      DELETED                                      
         BZ    *+8                                                              
         MVI   TANPAIR,TANPDEL                                                  
         TM    NBUNITST,X'40'      PREEMPTED                                    
         BZ    *+8                                                              
         MVI   TANPAIR,TANPEMPT                                                 
         TM    NBUNITST,X'02'      MISSED                                       
         BZ    *+8                                                              
         MVI   TANPAIR,TANPMISD                                                 
*                                                                               
         BRAS  RE,MEDSORT          UPDATE MEDIA SORT FIELD                      
*                                                                               
         LA    R5,1                SET TO 1 FOR BCT                             
         CLI   MULTIRUN,0          IS IT MULTIRUN UNIT?                         
         BE    DOTPUT              NO                                           
         ZIC   R5,MULTIRUN         YES-SET BCT LOOP                             
         OI    TANPSTAT,X'04'      MULTIRUN INDICATOR                           
*                                                                               
DOTPUT   GOTO1 SORTER,DMCB,=C'PUT',MYWORK    \                                  
         BCT   R5,DOTPUT                                                        
*                                                                               
DOTX     B     XIT                                                              
         EJECT                                                                  
         DROP R3                                                                
* - GET RECORDS FROM SORTER                                                     
*                                                                               
RDSORTER NTR1                                                                   
         NETGO NVSETSPT,DMCB           IN CASE WE READ FOR PRODUCT NAME         
PL00     GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R3,4(R1)                                                         
         LTR   R3,R3                                                            
         BZ    MSTX                                                             
         ST    R3,ASRTREC                                                       
         SPACE                                                                  
* - SET UP TO WRITE TO WORKER FILE                                              
         USING SORTD,R3                                                         
         CLC   SAMPRDSV,SAMPRD        DO WE NEED NEW PRODUCT NAME               
         BE    PL004                                                            
         CLI   SAMPRD+3,0          IF UNALLOCATED                               
         BNE   *+14                                                             
         XC    PRODNAME,PRODNAME   CLEAR TO ZERO                                
         B     PL001                                                            
         XC    KEY,KEY                                                          
         MVC   KEY+1(6),SAMPRD                                                  
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                SERIOUS BUG                                  
         MVC   FILENAME,=C'SPTFIL  '                                            
         MVC   AIO,NBAIO                                                        
         GOTO1 GETREC                                                           
         L     R4,NBAIO                                                         
         USING PRDHDR,R4                                                        
         MVC   PRODNAME,PNAME                                                   
*                                                                               
PL001    MVC   SAMPRDSV,SAMPRD     SAVE NEW PROD KEY INFO                       
*                                                                               
PL004    CLI   UPDATE,C'N'                    IF TEST RUN                       
         BE    PL01                           NO WORKER                         
**       CLI   UNMARK,C'Y'                    IF UNMARKING UNITS                
**       BE    PL01                           NO WORKER                         
         SPACE 2                                                                
* - SET ELEMENTS TO MYWORK CREATING PRODUCT NAME ELEMENT                        
         SPACE                                                                  
         LA    RE,MYWORK                                                        
         L     RF,=F'400'                                                       
         XCEF                                                                   
         LA    R4,MYWORK                                                        
         LA    R2,SDATA            POINT TO START OF ELEMENTS IN SORTER         
*                                                                               
         BAS   RE,MVCIT            MOVE COMMERCIAL ID ELEMENT                   
*                                                                               
         BAS   RE,MVCIT            MOVE CLIENT CODE ELEMENT                     
*                                                                               
         CLI   0(R2),X'38'         IS IT PRODUCT CODE ELEMENT                   
         BNE   *+8                 IF NOT SKIP                                  
         BAS   RE,MVCIT            MOVE PRODUCT CODE ELEMENT                    
*                                                                               
         BAS   RE,MVCIT            MOVE CLIENT NAME ELEMENT                     
         SPACE                                                                  
* - NOW INSERT PRODUCT NAME ELEMENT                                             
         OC    PRODNAME,PRODNAME   ...IF NO NAME(UNALLOC)                       
         BZ    PL008               ...SKIP THIS ELEMENT                         
         USING TAFND,R4                                                         
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNTYPE,TAFNTPRD   PRODUCT NAME                                 
         LA    R1,19               DROP END BLANKS                              
         LA    RE,PRODNAME+19                                                   
PL005    CLI   0(RE),X'40'                                                      
         BH    PL007                                                            
         BCTR  R1,0                                                             
         BCTR  RE,0                                                             
         B     PL005                                                            
PL007    EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TAFNNAME(0),PRODNAME                                             
         LA    R1,4(R1)                                                         
         STC   R1,TAFNLEN                                                       
         AR    R4,R1               BUMP TO NEXT OUT AREA IN MYWORK              
*                                                                               
PL008    BAS   RE,MVCIT            MOVE COMMERCIAL TITLE                        
         SPACE                                                                  
* - TANPNWK AT THIS POINT HAS FULL 4 CHARACTER NETWORK ID                       
* - TALENT ONLY WANTS 1ST CHARACTER/ SAVE NET ID FOR PRNINTING REPORT           
         USING TANPD,R2                                                         
***      MVC   NET4CL,TANPNWK      SAVE THE FULL 4 CHARACTERS                   
         MVC   NET4CL,SDNTWK       PASS FULL 4 CHARACTERS                       
         CLI   SDSTATYP,C'C'       CABLE STATION?                               
         BE    *+10                                                             
         MVC   TANPNWK(1),SDNTWK   PASS ONLY 1ST CHARACTER                      
*                                                                               
         MVI   TANPLFT,0           AND CLEAR FOLLOWING BYTES                    
***      XC    TANPUSEN,TANPUSEN                                                
         CLC   =C'ABC',NET4CL      .IF NOT ABC,CBS,NBC,FOX                      
         BE    PL009                                                            
         CLC   =C'CBS',NET4CL      .IF NOT ABC,CBS,NBC,FOX                      
         BE    PL009                                                            
         CLC   =C'NBC',NET4CL      .IF NOT ABC,CBS,NBC,FOX                      
         BE    PL009                                                            
         CLC   =C'FOX',NET4CL      .IF NOT ABC,CBS,NBC,FOX                      
         BE    PL009                                                            
         CLC   =C'UPN',NET4CL      .IF NOT UPN                                  
         BE    PL009                                                            
         CLC   =C'WB ',NET4CL      .IF NOT WB                                   
         BE    PL009                                                            
         CLC   =C'CW',NET4CL       IF CW                                        
         BNE   *+12                                                             
         MVI   TANPNWK,C'W'        MAKE IT 'W'                                  
         B     PL009                                                            
         CLC   =C'MNT',NET4CL      IF MNT                                       
         BE    PL008A                                                           
         CLC   =C'MY',NET4CL       OR MY                                        
         BNE   *+12                                                             
PL008A   MVI   TANPNWK,C'M'        MAKE IT 'M'                                  
         B     PL009                                                            
*                                                                               
         CLC   =C'BOUN',NET4CL     IF BOUNCE                                    
         BE    PL008B                                                           
         CLC   =C'BOU ',NET4CL                                                  
         BE    PL008B                                                           
         CLC   =C'BNCE',NET4CL                                                  
         BE    PL008B                                                           
         CLC   =C'BNCT',NET4CL                                                  
         BNE   *+12                                                             
PL008B   MVI   TANPNWK,C'B'        MAKE IT 'B'                                  
         B     PL009                                                            
*                                                                               
         CLC   =C'METV',NET4CL     IF METV                                      
         BNE   *+12                                                             
         MVI   TANPNWK,C'E'        MAKE IT 'E'                                  
         B     PL009                                                            
*                                                                               
         CLC   =C'ANTN',NET4CL     IF ANTENNA TV                                
         BNE   *+12                                                             
         MVI   TANPNWK,C'Y'        MAKE IT 'Y'                                  
         B     PL009                                                            
*                                                                               
         CLC   =C'COZI',NET4CL     IF COZITV                                    
         BNE   *+12                                                             
         MVI   TANPNWK,C'Z'        MAKE IT 'Z'                                  
         B     PL009                                                            
*                                                                               
         CLC   =C'ESCA',NET4CL     IF ESCAPE                                    
         BE    PL008C                                                           
         CLC   =C'ESCP',NET4CL                                                  
         BNE   *+12                                                             
PL008C   MVI   TANPNWK,C'P'        MAKE IT 'P'                                  
         B     PL009                                                            
*                                                                               
         CLC   =C'GET ',NET4CL     IF GETTV                                     
         BE    PL008D                                                           
         CLC   =C'GETT',NET4CL                                                  
         BE    PL008D                                                           
         CLC   =C'GETV',NET4CL                                                  
         BNE   *+12                                                             
PL008D   MVI   TANPNWK,C'G'        MAKE IT 'G'                                  
         B     PL009                                                            
*                                                                               
         CLC   =C'GRIT',NET4CL     IF GRIT                                      
         BNE   *+12                                                             
         MVI   TANPNWK,C'R'        MAKE IT 'R'                                  
         B     PL009                                                            
*                                                                               
         CLC   =C'JUST',NET4CL     IF JUSTICE                                   
         BNE   *+12                                                             
         MVI   TANPNWK,C'J'        MAKE IT 'J'                                  
         B     PL009                                                            
*                                                                               
         CLC   =C'LAFF',NET4CL     IF LAFF                                      
         BNE   *+12                                                             
         MVI   TANPNWK,C'L'        MAKE IT 'L'                                  
         B     PL009                                                            
*                                                                               
         CLC   =C'THIS',NET4CL     IF THIS                                      
         BNE   *+12                                                             
         MVI   TANPNWK,C'T'        MAKE IT 'T'                                  
         B     PL009                                                            
*                                                                               
         CLI   SDSTATYP,C'C'       CABLE STATION?                               
         BE    PL009                                                            
         MVI   TANPNWK,C'S'         .MAKE IT 'S'                                
         CLI   SDSTATYP,C'X'       IF PAX                                       
         BNE   *+8                                                              
         MVI   TANPNWK,C'X'                                                     
         CLI   SDSTATYP,C'I'       IF ITN                                       
         BNE   *+8                                                              
         MVI   TANPNWK,C'I'                                                     
         CLI   SDSTATYP,C'D'       IF ACTIVE INTERNATIONAL                      
         BNE   *+8                                                              
         MVI   TANPNWK,C'D'                                                     
         CLI   SDSTATYP,C'K'       IF CONTINUUM MEDIA                           
         BNE   *+8                                                              
         MVI   TANPNWK,C'K'                                                     
         CLI   SDSTATYP,C'Q'       IF REVSHARE                                  
         BNE   *+8                                                              
         MVI   TANPNWK,C'Q'                                                     
         CLI   SDSTATYP,C'O'       IF ICON INTERNATIONAL                        
         BNE   *+8                                                              
         MVI   TANPNWK,C'O'                                                     
         CLI   SDSTATYP,C'H'       IF CADENT                                    
         BNE   *+8                                                              
         MVI   TANPNWK,C'H'                                                     
         CLI   SDSTATYP,C'B'       IF BOUNCE                                    
         BNE   *+8                                                              
         MVI   TANPNWK,C'B'                                                     
         CLI   SDSTATYP,C'E'       IF METV                                      
         BNE   *+8                                                              
         MVI   TANPNWK,C'E'                                                     
         CLI   SDSTATYP,C'Y'       IF ANTENNA                                   
         BNE   *+8                                                              
         MVI   TANPNWK,C'Y'                                                     
         CLI   SDSTATYP,C'Z'       IF COZITV                                    
         BNE   *+8                                                              
         MVI   TANPNWK,C'Z'                                                     
         CLI   SDSTATYP,C'P'       IF ESCAPE                                    
         BNE   *+8                                                              
         MVI   TANPNWK,C'P'                                                     
         CLI   SDSTATYP,C'G'       IF GETTV                                     
         BNE   *+8                                                              
         MVI   TANPNWK,C'G'                                                     
         CLI   SDSTATYP,C'R'       IF GRIT                                      
         BNE   *+8                                                              
         MVI   TANPNWK,C'R'                                                     
         CLI   SDSTATYP,C'J'       IF JUSTICE                                   
         BNE   *+8                                                              
         MVI   TANPNWK,C'J'                                                     
         CLI   SDSTATYP,C'L'       IF LAFF                                      
         BNE   *+8                                                              
         MVI   TANPNWK,C'L'                                                     
         CLI   SDSTATYP,C'T'       IF THIS TV                                   
         BNE   *+8                                                              
         MVI   TANPNWK,C'T'                                                     
PL009    BAS   RE,MVCIT            NET/PROGRAM DETAIL                           
         DROP  R2                                                               
* - ALL ELEMENTS NOW SIT IN MYWORK                                              
***      XC    IOWRKR,IOWRKR                                                    
         LA    RE,IOWRKR                                                        
         LA    RF,300                                                           
         XCEF                                                                   
***      MVC   IOWDATA,MYWORK      MOVE ELEMENTS TO WORKER OUT AREA             
         LA    RE,IOWDATA                                                       
         LHI   RF,296                                                           
         LA    R0,MYWORK                                                        
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
* - ADD UP ELEM LENGTHS IN R1                                                   
         LA    R3,IOWDATA                                                       
         SR    R1,R1                                                            
PL0012   ZIC   R2,1(R3)                                                         
         AR    R1,R2               ADD TO TOTAL ELEM LENGTH                     
         AR    R3,R2               BUMP TO NEXT ELEMENT                         
         CLI   1(R3),0             ANY MORE ELEMENTS                            
         BNE   PL0012                                                           
         LA    R1,4(R1)            NO/ADD 4 BYTE IOWLEN HEADER                  
         STH   R1,IOWLEN       SET TOTAL ELEM LENGTH IN WORKER HEADER           
         BRAS  RE,ADDWRKR      AND ADD TO WORKER                                
         B     PL01                                                             
*                                                                               
         EJECT                                                                  
         SPACE                                                                  
* - MOVES TALENT ELEMENT FROM SORTREC TO WORKER FILE AREA                       
* - BUMPS TO NEXT TALENT ELEMENT IN SORTREC AND NEXT OUT                        
* - AREA OF WORKERFILE                                                          
*                                                                               
MVCIT    DS    0H                  R4 POINTS TO OUT AREA IN MYWORK              
         ZIC   R1,1(R2)            R2 POINTS TO ELEM IN SORTER                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R2)                                                    
         LA    R1,1(R1)                                                         
         AR    R4,R1               BUMP TO NEXT ELEM AREA IM MYWORK             
         AR    R2,R1               AND TO NEXT ELEM IN SORTER                   
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
* - NOW PUT TO PRINT LINE FROM SORTREC                                          
PL01     LA    R2,P                                                             
         USING PLINE,R2                                                         
         L     R3,ASRTREC                                                       
         USING SORTD,R3                                                         
*                                                                               
         MVC   TALADID,SDADID                                                   
         MVC   TALCMID,SDCOMID                                                  
*                                                                               
         CLC   CLTSV,SCLI          PAGE BREAK ON CHANGE OF CLIENT               
         BE    PL02                                                             
         MVC   CLTSV,SCLI                                                       
         MVI   FORCEHED,C'Y'                                                    
         B     PL02A                                                            
PL02     CLI   LINE,57             IF AT TOP OF PAGE PRINTING                   
         BNH   PL02B                                                            
PL02A    MVI   PRDSV,0             FORCE PROD PRINT                             
         MVI   NETSV,0             FORCE NET PRINT                              
         MVI   IDSV,0              FORCE ID PRINT                               
         SPACE                                                                  
PL02B    CLC   SDFEED,SPACES                                                    
         BNH   *+10                                                             
         MVC   PLFEED,SDFEED                                                    
         CLC   SPRD,PRDSV          PRODUCT                                      
         BE    PL03                                                             
         MVC   PLPRD(3),SPRD                                                    
         MVC   PLPRD+4(20),PRODNAME   PRODUCT NAME                              
         CLI   SPRD,0              IS IT UNALLOCATED                            
         BNE   *+10                                                             
         MVC   PLPRD(3),=C'***'    UNALLOCATED                                  
         MVC   PRDSV,SPRD                                                       
PL03     LA    R3,SDATA                                                         
         USING TANXD,R3                                                         
         CLI   TANXCCDE,0          IS THIS A SENT/CHANGED RECORD                
         BE    PL05                                                             
         ST    R2,FULL             SAVE PRINT LINE ADDRESS                      
         LA    R2,132(R2)          PRINT ERRORS ON NEXT LINE                    
         TM    TANXCCDE,8                                                       
         BZ    *+14                                                             
         MVC   PL6(24),=C'*** ERROR LENGTH CHANGED'                             
         LA    R2,132(R2)                                                       
         TM    TANXCCDE,4                                                       
         BZ    *+14                                                             
         MVC   PL6(25),=C'*** ERROR PRODUCT CHANGED'                            
         LA    R2,132(R2)                                                       
         TM    TANXCCDE,2                                                       
         BZ    *+14                                                             
         MVC   PL6(22),=C'*** ERROR DATE CHANGED'                               
         LA    R2,132(R2)                                                       
         TM    TANXCCDE,1                                                       
         BZ    *+14                                                             
         MVC   PL6(22),=C'*** ERROR CMML CHANGED'                               
         LA    R2,132(R2)                                                       
         TM    TANXCCDE,X'10'                                                   
         BZ    *+10                                                             
         MVC   PL6(22),=C'*** ERROR UNIT DELETED'                               
         L     R2,FULL             RESET P LINE ADDRESS                         
*                                                                               
PL05     OC    TALADID,TALADID                                                  
         BZ    PL06                                                             
         CLC   IDSV,TALADID         CHECK ADID                                  
         BE    PL10                                                             
         MVC   IDSV,TALADID                                                     
         B     PL07                                                             
PL06     OC    IDSV+8(4),SPACES     IF ADID SAVED LAST,                         
         BH    PL06A                                                            
         CLC   IDSV(L'TALCMID),TALCMID    OR CHECK ID                           
         BE    PL10                IF SAME/SKIP PRINTING                        
PL06A    XC    IDSV,IDSV                                                        
         MVC   IDSV(L'TALCMID),TALCMID        ELSE SAVE                         
PL07     XC    NAMESV,NAMESV       AND FORCE NAME PRINT                         
*                                                                               
         CLI   SRTTYP,C'T'         TITLE SORT                                   
         BE    PL08                                                             
         MVC   PLCOMID,TALADID      PUT ADID TO PRINT LINE                      
         OC    TALADID,TALADID                                                  
         BNZ   *+10                                                             
         MVC   PLCOMID(L'TANXNCID),TANXNCID   PUT ID TO PRINT LINE              
         EDIT  (B1,TANXSEC),(3,PLSEC)         PUT SEC LEN TO PRINT LINE         
         B     PL10                                                             
PL08     MVC   PLID2,TALADID                PUT ADID TO PRINT LINE              
         OC    TALADID,TALADID                                                  
         BNZ   *+10                                                             
         MVC   PLID2(L'TANXNCID),TANXNCID   PUT ID TO PRINT LINE                
         EDIT  (B1,TANXSEC),(3,PLSEC2)      PUT SEC LEN TO PRINT LINE           
*                                                                               
PL10     ZIC   R1,1(R3)                   BUMP PAST CLIENT CODE ELEM            
         AR    R3,R1                                                            
PL10A    ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         CLI   0(R3),X'26'         LOOK FOR COMMERCIAL TITLE ELEM               
         BNE   PL10A                                                            
         CLI   2(R3),C'T'                                                       
         BNE   PL10A               NO                                           
         USING TAFND,R3                                                         
         ZIC   R1,TAFNLEN          YES                                          
         S     R1,=F'4'                                                         
         STC   R1,BYTE                                                          
         EX    R1,PL15                                                          
         BE    PL20                IS IT SAME COMMERCIAL TITLE                  
         B     *+10                                                             
PL15     CLC   TAFNNAME(0),NAMESV                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NAMESV(0),TAFNNAME     SAVE NEW COMMERCIAL TITLE                 
         LA    R4,PLCMTITL           PRINT LINE TITLE AREA                      
         CLI   SRTTYP,C'T'                 ..SORT BY TITLE                      
         BNE   *+8                                                              
         LA    R4,PLTITL2                  ..YES                                
* - SET UP FOR CHOPPER                                                          
         LA    RE,TAFNNAME         A(INPUT)                                     
         ST    RE,DMCB                                                          
         LA    R1,1(R1)            FULL LEN OF NAME                             
         STC   R1,DMCB             L'INPUT                                      
         ST    R4,DMCB+4           A(OUTPUT)                                    
         MVI   DMCB+4,27           L'OUTPUT                                     
         LA    R1,4                N'LINES                                      
         ST    R1,DMCB+8                                                        
         MVI   DMCB+8,132          L'PRINT LINE                                 
         GOTO1 CHOPPER,DMCB                                                     
         ZIC   R1,BYTE             RETURN NAME LENGTH                           
*                                                                               
PL20     DS    0H                                                               
         LA    R1,4(R1)            RESTORE VARIABLE ELEM LENGTH                 
         AR    R3,R1               AND BUMP TO NEXT ELEM                        
         CLI   0(R3),X'8A'         EXPECTS NET/PROG ELEM                        
         BNE   PL50                                                             
         USING TANPD,R3                                                         
         GOTO1 DATCON,DMCB,(1,TANPDATE),(5,PLDATE)                              
         MVC   PLPROG,TANPPNME                                                  
PL30     DS    0H                                                               
         L     R1,ASRTREC                                                       
         USING SORTD,R1                                                         
         MVI   PLDATNM,C'-'                                                     
         EDIT  (B1,SAMDATNM),(3,PLDATNM+1)                                      
         CLC   NETSV,SDNTWK        IS IT SAME NETWORK                           
         BE    PL40                                                             
         MVC   NETSV,SDNTWK                                                     
         MVC   PLNET,SDNTWK                                                     
         MVC   PLTYP,SDSTATYP                                                   
PL40     CLI   SDMULTI,0           MULTI RUN?                                   
         BE    PL50                                                             
         MVC   PLTYP+2(2),=C'R='                                                
         EDIT  (B1,SDMULTI),(2,PLTYP+4)                                         
PL50     GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PL00                                                             
MSTX     B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
* EXPECTS -  COMMERCIAL ID IN TALCMID                                           
* RETURNS  - COMMERCIAL TITLE IN WORK/ LENGTH OF TITLE-1 IN TITLEN              
* IF COVERED (DUMMY) COMMERCIAL GOES TO SPECIAL PROCESSING                      
*                                                                               
GETCMT   NTR1                                                                   
         MVI   PCKFLAG,C'N'                                                     
         XC    TALADID,TALADID                                                  
         CLI   BYTE,1              ARE WE IN MIDDLE OF COVERED CMMLS            
         BE    COVERCML            YES                                          
         XC    MYWORK(100),MYWORK                                               
         MVC   DTADSPSV,NBDTADSP                SAVE CURRENT NBDTADSP           
         MVC   NBDTADSP,=H'24'                                                  
         XC    MYKEY,MYKEY                                                      
         MVC   MYKEY(2),=X'0A21'                                                
*                                                                               
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   *+8                                                              
         MVI   MYKEY+1,X'C1'       PASSIVE FOR AD-ID                            
*                                                                               
         MVC   MYKEY+2(1),NBACTAM                                               
         MVC   MYKEY+3(2),NBACTCLI                                              
         MVC   MYKEY+5(8),TALCMID                                               
         BAS   RE,RDTHI            TRAFFIC                                      
         CLC   MYKEYSV(13),MYKEY                                                
         BE    CMT05                                                            
         SR    R1,R1                                                            
         B     CMT20                                                            
CMT05    BAS   RE,GTTREC           TRAFFIC                                      
         L     R2,AMYIO                                                         
         USING CMLRECD,R2                                                       
         MVC   TALCMID,CMLKCML                                                  
*                                                                               
         TM    CMLRSTAT,CMLKSTA_PCKD     IS COMMERCIAL PACKED?                  
         BZ    *+8                                                              
         MVI   PCKFLAG,C'Y'                                                     
*                                                                               
         L     R2,AMYIO                                                         
         USING CMLADIEL,R2                                                      
         MVI   ELCODE,X'A0'        IS IT AN AD-ID COMMERCIAL?                   
         BAS   RE,GETEL                                                         
         BNE   CMT07                                                            
         MVC   TALADID,CMLADID                                                  
*                                                                               
CMT07    L     R2,AMYIO                                                         
         USING CMLRECD,R2                                                       
         MVI   ELCODE,X'60'        IS IT A COVER COMMERCIAL                     
         BAS   RE,GETEL                                                         
         BE    COVERCML            YES/GO DO COVER PROCESSING                   
         L     R2,AMYIO                                                         
         CLI   CMLTALEX,C'Y'       EXCLUDE COMML/NEVER SEND                     
         BNE   *+12                                                             
         MVI   TITLEN,X'FF'        YES/NEVER SEND IT                            
         B     XIT                                                              
         MVC   MYWORK(15),CMLTITLE       COMMERCIAL TITLE                       
         MVC   TALLNTH,CMLSLN            COMMERCIAL LENGTH                      
         LA    R1,14                     LENGTH OF CMML-1                       
         LA    R3,MYWORK                                                        
         MVI   ELCODE,X'30'        CHECK FOR COMMERCIAL DESCRIP ELEM            
         USING CMLDSCEL,R2                                                      
         BAS   RE,GETEL                                                         
         BNE   CMT20               NO DESC ELEM                                 
CMT10    MVC   0(24,R3),CMLDSC     YES/MOVE IN DESC                             
         LA    R3,25(R3)                                                        
         BAS   RE,NEXTEL           NEXT CMML ELEM                               
         BE    CMT10               YES                                          
* - MAKE SURE BLANKS ARE BLANKS    NO                                           
         LA    R3,MYWORK                                                        
         LA    R4,100                                                           
         OI    0(R3),X'40'                                                      
         LA    R3,1(R3)                                                         
         BCT   R4,*-8                                                           
         GOTO1 SQUASHER,DMCB,MYWORK,100                                         
         L     R1,DMCB+4           GET NEW LENGTH OF SQUASHED DATA              
         BCTR  R1,0                                                             
CMT20    STC   R1,TITLEN                                                        
         C     R1,=F'99'           MAX TITLEN =100                              
         BNH   *+6                                                              
         DC    H'0'                HOW COME?                                    
         MVI   NBFUNCT,NBFRDHI                                                  
         MVC   NBDTADSP,DTADSPSV                                                
         B     XIT                                                              
         EJECT                                                                  
* - COVER COMMERCIAL                                                            
* - R2 = '60' ELEMENTS POINT TO 'REAL' COMMERCIALS                              
*                                                                               
COVERCML DS    0H                                                               
         CLI   BYTE,0              ARE WE IN MIDDLE OF COVERED CMMLS            
         BE    COVM0                                                            
         L     R2,ACOVCML          YES/GET CURRENT ELEMENT ADDRESS              
*                                      OF REAL CMML IN DUMMY CMML REC           
COVM0    MVI   BYTE,1              SET FLAG TO MULTIPLE                         
         BAS   RE,GETCVCML         PROCESS CMML                                 
         MVI   ELCODE,X'60'                                                     
         BAS   RE,NEXTEL           ARE THERE ANY MORE                           
         BE    COVM5                                                            
         MVI   BYTE,0              LAST COMMERCIAL                              
         MVI   NBFUNCT,NBFRDHI                                                  
         MVC   NBDTADSP,DTADSPSV   RESET UNIT'S DTADSP                          
         MVC   AMYIO,ANETWS4       AND RESET AMYIO                              
COVM5    ST    R2,ACOVCML          SAVE ELEMENT ADDRESS                         
         B     XIT                                                              
         SPACE                                                                  
GETCVCML NTR1                                                                   
         USING CMLACTEL,R2                                                      
         XC    MYWORK(100),MYWORK                                               
         XC    MYKEY,MYKEY                                                      
         MVC   MYKEY(2),=X'0A21'                                                
         MVC   MYKEY+2(1),NBACTAM                                               
         MVC   MYKEY+3(2),NBACTCLI                                              
         MVC   MYKEY+5(8),CMLACTID       REAL CMML ID                           
         MVC   TALCMID,CMLACTID          REAL CMML ID                           
         MVI   PCKFLAG,C'N'                                                     
         CLI   CMLACTLN,CMLACTL2         IF NEW LENGTH ELEMENT,                 
         BL    COV03                                                            
         MVC   TALADID,CMLACTCM          SAVE 12 CHAR FIELD IN AD-ID            
         MVI   MYKEY+1,X'C1'             READ PASSIVE KEY INSTEAD               
         GOTO1 ADTRPACK,DMCB,(C'P',CMLACTCM),MYKEY+5                            
         MVC   TALCMID,MYKEY+5           SAVE PACKED COMM ID                    
         MVI   PCKFLAG,C'Y'              AND SET PACKED FLAG                    
COV03    BAS   RE,RDTHI                                                         
         CLC   MYKEYSV(13),MYKEY                                                
         BE    COV05                                                            
         SR    R1,R1                                                            
         B     COV20                                                            
COV05    L     R1,ANETWS4          ANETWS4 HAS DUMMY CMML RECORD                
         A     R1,=F'1000'         ANETWS4+1000 GETS REAL CMML RECORD           
         ST    R1,AMYIO                                                         
         BAS   RE,GTTREC                                                        
         L     R2,AMYIO                                                         
         USING CMLRECD,R2                                                       
         MVI   ELCODE,X'60'        IS IT A COVER COMMERCIAL                     
         BAS   RE,GETEL                                                         
         BNE   *+6                                                              
         DC    H'0'                CANT HAVE COVER OF COVER                     
* - CHECK IF CMML PROD AND UNIT PROD MATCH                                      
         L     R2,AMYIO                                                         
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         USING CMLPRDEL,R2                                                      
         BNE   COV5A               SEE IF WE HAVE 3 CHAR PRODS                  
         CLI   CMLPRDS,X'FF'       ALL PRODS VALID                              
         BE    COV7                                                             
         ZIC   R1,CMLPRDLN         LENGTH OF ELEMENT                            
         BCTR  R1,0                MINUS ID                                     
         BCTR  R1,0                MINUS LENGTH                                 
         LA    R2,CMLPRDS                                                       
         CLC   PRDSV1,0(R2)                                                     
         BE    COV7                                                             
         LA    R2,1(R2)                                                         
         BCT   R1,*-14                                                          
         B     COV5A               SEE IF 3 CHAR MATCH                          
COV6     MVI   TITLEN,X'FF'        NO/SET NO-MATCH FLAG                         
         B     XIT                                                              
********************************************************                        
* - CHECK IF 3 CHAR CMML PROD AND 3 CHAR UNIT PROD MATCH                        
COV5A    L     R2,AMYIO                                                         
         MVI   ELCODE,X'29'                                                     
         BAS   RE,GETEL                                                         
         USING CMLMPREL,R2                                                      
         BNE   COV6A                                                            
         CLI   CMLMPRS,X'FF'       ALL PRODS VALID                              
         BE    COV7                                                             
         ZIC   R1,CMLMPRLN         LENGTH OF ELEMENT                            
         BCTR  R1,0                MINUS ID                                     
         BCTR  R1,0                MINUS LENGTH                                 
         LA    R2,CMLMPRS                                                       
         CLC   PRDSV3,0(R2)                                                     
         BE    COV7                                                             
         LA    R2,3(R2)                                                         
         BCT   R1,*-14                                                          
COV6A    MVI   TITLEN,X'FF'        NO/SET NO-MATCH FLAG                         
         B     XIT                                                              
*********************************************************                       
*                                                                               
COV7     L     R2,AMYIO                                                         
         A     R2,=F'24'                                                        
COV7B    CLI   0(R2),X'10'                                                      
         BE    COV8                                                             
         ZIC   R1,1(R2)                                                         
         LTR   R1,R1                                                            
         BZ    COV6                CANT FIND ELEMENT/BAD REC SKIP               
         AR    R2,R1                                                            
         B     COV7B                                                            
         USING CMLDTAEL,R2                                                      
COV8     CLI   CMLTALEX,C'Y'       NEVER TRANSFER COMMERCIAL                    
         BE    COV6                NEVER/SKIPIT                                 
         MVC   MYWORK(15),CMLTITLE       COMMERCIAL TITLE                       
         MVC   TALLNTH,CMLSLN         ** TAKE LENGTH FROM COVERED CMML          
         LA    R1,14                     LENGTH OF CMML-1                       
         LA    R3,MYWORK                                                        
         L     R2,AMYIO                                                         
         MVI   ELCODE,X'30'        CHECK FOR COMMERCIAL DESCRIP ELEM            
         USING CMLDSCEL,R2                                                      
         BAS   RE,GETEL                                                         
         BNE   COV20               NO DESC ELEM                                 
COV10    MVC   0(24,R3),CMLDSC     YES/MOVE IN DESC                             
         LA    R3,25(R3)                                                        
         BAS   RE,NEXTEL           NEXT CMML ELEM                               
         BE    COV10               YES                                          
* - MAKE SURE BLANKS ARE BLANKS    NO                                           
         LA    R3,MYWORK                                                        
         LA    R4,100                                                           
         OI    0(R3),X'40'                                                      
         LA    R3,1(R3)                                                         
         BCT   R4,*-8                                                           
         GOTO1 SQUASHER,DMCB,MYWORK,100                                         
         L     R1,DMCB+4           GET NEW LENGTH OF SQUASHED DATA              
         BCTR  R1,0                                                             
COV20    STC   R1,TITLEN                                                        
         C     R1,=F'99'           MAX TITLEN =100                              
         BNH   *+6                                                              
         DC    H'0'                HOW COME?                                    
         B     XIT                                                              
         EJECT                                                                  
* RETURNS 3 BYTE PROD CODE IN WORK/ EXPECTS 1 BYTE PRD IN BYTE                  
GETPRD   NTR1                                                                   
         CLI   BYTE,0              IS IT UNALLOCATED                            
         BE    GP15                                                             
         L     R2,NBACLI                                                        
         USING CLTHDR,R2                                                        
         LA    R2,CLIST                                                         
         DROP  R2                                                               
         LA    R1,220                                                           
         MVI   WORK+5,0            SET FLAG                                     
GP10     CLC   BYTE,3(R2)                                                       
         BE    GP20                                                             
         CLI   3(R2),0                                                          
         BE    GP15                                                             
         LA    R2,4(R2)                                                         
         BCT   R1,GP10                                                          
         CLI   WORK+5,2            EXTENDED LIST DONE?                          
         BE    GP15                YES                                          
         MVI   WORK+5,2                                                         
         LA    R1,35                                                            
*                                                                               
         L     R2,NBACLI                                                        
         USING CLTHDR,R2                                                        
         LA    R2,CLIST2                                                        
         B     GP10                                                             
*                                                                               
GP15     XC    WORK(3),WORK                                                     
         B     *+10                                                             
GP20     MVC   WORK(3),0(R2)                                                    
         B     XIT                                                              
         DROP  R2                                                               
         SPACE 2                                                                
* THIS ROUTINE FILLS PRODTBL WITH LIST OF PRODS VALID FOR TAL AGENCY            
*  - PRODUCT RECORD CARRIES VALID TALENT AGENCY CODE                            
*  - MATCH THIS CODE AGAINST REQUESTED TALENT AGENCY                            
*  - THIS ROUTINE USED TO FILL PRODTABLE FROM CLIENT RECORD                     
*  - NOW IT READ NEW PROD PASSIVE POINTERS                                      
PRDLIST  NTR1                                                                   
         L     RE,=A(PRODTBL)                                                   
         LA    RF,L'PRODTBL                                                     
         XCEF                                                                   
         L     R4,=A(PRODTBL)                                                   
*                                                                               
         LA    R3,MYKEY             R3 -> MYKEY                                 
         USING PLSTTYPE,R3                                                      
         XC    POLTAGY,POLTAGY                                                  
         XC    MYKEY,MYKEY          GET POL TALENT AGY                          
         MVC   PLSTTYPE(2),=X'0DF1'   PASSIVE POINTER                           
         MVC   PLSTAM,NBACTAM                                                   
         MVC   PLSTCLT,NBACTCLI                                                 
         MVI   PLSTXFF,X'FF'                                                    
         MVC   PLSTPRD,=C'POL'                                                  
         MVC   MYKEYSV,MYKEY                                                    
         GOTO1 RDHI                                                             
         CLC   MYKEY(8),MYKEYSV                                                 
         BNE   PRDLX                                                            
         GOTO1 GTREC                                                            
         L     R3,AMYIO            R3 -> TO RECORD                              
         OC    PTALAGY,PTALAGY                                                  
         BZ    *+10                                                             
         MVC   POLTAGY,PTALAGY                                                  
*                                                                               
         LA    R3,MYKEY             R3 -> MYKEY                                 
         XC    MYKEY,MYKEY                                                      
         MVC   PLSTTYPE(2),=X'0DF1'   PASSIVE POINTER                           
         MVC   PLSTAM,NBACTAM                                                   
         MVC   PLSTCLT,NBACTCLI                                                 
         MVC   MYKEYSV,MYKEY                                                    
         GOTO1 RDHI                                                             
PRDL15   CLC   MYKEY(5),MYKEYSV                                                 
         BNE   PRDLX                                                            
         GOTO1 GTREC                                                            
         L     R3,AMYIO            R3 -> TO RECORD                              
         L     RE,ATWA                                                          
         USING T320FFD,RE                                                       
**NO-OP  ZIC   R1,SPLTALH+5                                                     
**JAN11  BCTR  R1,0                                                             
**       EX    R1,*+8                                                           
**       B     *+10                                                             
**NO-OP  CLC   SPLTAL(0),PTALAGY   IF TALENT AGENCY CODES MATCH                 
         OC    PTALAGY,PTALAGY     TEST PRODUCT HAS TALENT AGENCY               
         BNZ   *+10                                                             
         MVC   PTALAGY,POLTAGY     SET POL TALENT AGENCY                        
         CLC   =C'N/A',PTALAGY     TEST TAL AGY = N/A                           
         BE    PRDL20                                                           
*                                                                               
         OC    SPLTAL,SPACES                                                    
         OC    PTALAGY,SPACES                                                   
         CLC   SPLTAL,PTALAGY      IF TALENT AGENCY CODES MATCH                 
         BNE   PRDL20                                                           
         DROP  RE                                                               
*                                                                               
         LA    R3,MYKEY            R3 -> BACK TO KEY                            
         MVC   0(1,R4),PLSTBPRD+1 SET PRODUCT TO LIST                           
         MVC   1(3,R4),PLSTPRD    SET PRODUCT TO LIST                           
         LA    R4,4(R4)                                                         
PRDL20   GOTO1 RDSEQ                                                            
         B     PRDL15                                                           
PRDLX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
* BYTE HAS 1 CHAR PROD / WORK HAS 3 CHAR PROD (IF IT'S THERE)                   
* TEST AGAINST 1 IF THERE -                                                     
* RETURN 3 CHAR IN WORK IN CASE TESTED OFF 1 CHAR                               
PRDOK    NTR1                                                                   
         L     R2,=A(PRODTBL)                                                   
PRDOK5   CLI   BYTE,0                                                           
         BE    PRDOK5B                                                          
         CLC   BYTE,0(R2)          MATCH AGAINST 1 CHAR CODE                    
         BE    PRDOKX             PROD VALID CC=                                
         BNE   PRDOK10                                                          
PRDOK5B  OC    NBSELPRD,NBSELPRD   DO WE HAVE A PRODUCT FILTER?                 
         BZ    PRDOK7                                                           
         CLC   NBSELPRD,=C'ALL'                                                 
         BE    PRDOK7                                                           
         CLC   NBSELPRD,WORK                                                    
         BNE   PRDOK20                                                          
PRDOK7   CLC   WORK(3),1(R2)       MATCH 3 CHAR CODE                            
         BE    PRDOKXX                                                          
PRDOK10  LA    R2,4(R2)                                                         
         CLC   0(2,R2),=X'0000'                                                 
         BNE   PRDOK5                                                           
PRDOK20  LTR   RE,RE              NOT VALID CC NOT =                            
         B     PRDOKXX                                                          
PRDOKX   MVC   WORK(3),1(R2)                                                    
PRDOKXX  B     XIT                                                              
         EJECT                                                                  
* MY DATAMGR INTERFACE FOR SPOT FILE                                            
*                                                                               
RDHI     NTR1                                                                   
         MVC   COMAND,=CL8'DMRDHI'                                              
         LA    R2,MYKEY                                                         
         MVC   MYKEYSV,MYKEY                                                    
         MVC   FILE,=C'SPTDIR  '                                                
         B     DIRALL                                                           
*                                                                               
RDTHI    NTR1                                                                   
         MVC   COMAND,=CL8'DMRDHI'                                              
         LA    R2,MYKEY                                                         
         MVC   MYKEYSV,MYKEY                                                    
         MVC   FILE,=C'TRFDIR  '                                                
         B     DIRALL                                                           
RDSEQ    NTR1                                                                   
         LA    R2,MYKEY                                                         
         MVC   MYKEYSV,MYKEY                                                    
         MVC   COMAND,=CL8'DMRSEQ'                                              
         MVC   FILE,=C'SPTDIR  '                                                
         B     DIRALL                                                           
*                                                                               
DIRALL   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,COMAND,FILE,MYKEY,MYKEY,0                           
         B     XIT                                                              
*                                                                               
GTREC    NTR1                                                                   
         LA    R2,MYKEY+14                                                      
         L     R3,AMYIO                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE ',(R2),(R3),MYDMWRK           
         B     XIT                                                              
*                                                                               
GTTREC   NTR1                                                                   
         LA    R2,MYKEY+14                                                      
         L     R3,AMYIO                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'TRFFILE ',(R2),(R3),MYDMWRK           
         B     XIT                                                              
*                                                                               
         GETEL (R2),NBDTADSP,ELCODE                                             
*                                                                               
         EJECT                                                                  
*                                                                               
HEADSPC  SSPEC H1,1,REQUESTOR                                                   
***      SSPEC H1,46,C'NETWORK/TALENT TRANSFER'                                 
***      SSPEC H2,46,C'________________________'                                
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,46,PERIOD                                                     
         SSPEC H4,99,REPORT                                                     
         SSPEC H5,99,RUN                                                        
         SSPEC H6,120,PAGE                                                      
         DC    X'00'                                                            
         SPACE 2                                                                
*                                                                               
MYHEAD   NTR1                                                                   
         LA    R4,ATWA                                                          
         USING T320FFD,R4                                                       
                                                                                
         MVC   H1+46(23),=C'NETWORK/TALENT TRANSFER'                            
         MVC   H2+46(23),=C'-----------------------'                            
         CLC   TITLSV,SPACES                                                    
         BE    OKHEAD                                                           
         MVC   H1+46(40),TITLSV                                                 
         MVC   H2+46(40),UNDERS                                                 
OKHEAD   MVC   H2(6),=C'TALENT'                                                 
         MVC   H2+10(6),SPLTAL                                                  
         MVC   H3(6),=C'CLIENT'                                                 
         MVC   H3+10(24),CLTSV                                                  
         CLI   UPDATE,C'Y'         MARK FILE                                    
         BE    MYH10                                                            
         MVC   H5+49(16),=C'*** TEST RUN ***'                                   
         CLI   UPDATE,C'N'         TEST                                         
         BE    MYH10                                                            
         MVC   H5+45(23),=C'*** TEST/WORKER RUN ***'                            
MYH10    DS    0H                                                               
         LA    R2,H10                                                           
         USING PLINE,R2                                                         
         MVC   PLPRD+4(7),=C'PRODUCT'                                           
         MVC   PLPRD+132(4),=C'CODE'                                            
         MVC   PLPRD+143(4),=C'NAME'                                            
         CLI   SRTTYP,C'T'         SORT BY COM TITLE                            
         BNE   MYH12                                                            
         MVC   PLID2(15),=CL15'   COMMERCIAL '                                  
         MVC   PLID2+135(2),=C'ID'                                              
***      MVC   PLADID2+135(5),=C'AD-ID'                                         
         MVC   PLSEC2,=C'LEN'                                                   
         MVC   PLTITL2,=C'         COMMERCIAL        '                          
         MVC   PLTITL2+144(5),=C'TITLE'                                         
         B     MYH13                                                            
MYH12    MVC   PLCOMID(15),=CL15'   COMMERCIAL '                                
         MVC   PLCOMID+135(2),=C'ID'                                            
***      MVC   PLADID+135(5),=C'AD-ID'                                          
         MVC   PLFEED,=C'FEED'                                                  
         MVC   PLFEED+132(4),=C'CODE'                                           
         MVC   PLSEC,=C'LEN'                                                    
         MVC   PLCMTITL,=C'      COMMERCIAL           '                         
         MVC   PLCMTITL+140(5),=C'TITLE'                                        
MYH13    MVC   PLDATE+2(4),=C'DATE'                                             
         MVC   PLPROG+4(7),=C'PROGRAM'                                          
         MVC   PLNET,=C'NTWK'                                                   
*                                                                               
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         LTR   R1,R1               IS ABOX ZEROS                                
         BZ    XIT                 YES/ ON-LINE SKIP BOXES                      
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         SPACE                                                                  
         DROP  R2                                                               
         LA    R2,BOXCOLS                                                       
         USING PLINE,R2                                                         
         MVI   PL1,C'L'                                                         
         MVI   PLEND,C'R'                                                       
         SPACE                                                                  
         LA    R1,BOXROWS                                                       
         LA    R1,8(R1)                                                         
         MVI   0(R1),C'T'                                                       
         LA    R1,3(R1)                                                         
         MVI   0(R1),C'M'                                                       
         LA    R1,47(R1)                                                        
         MVI   0(R1),C'B'                                                       
         SPACE                                                                  
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* FOR SAME PRODUCT/LENGTH AND COMMERCIAL DO NOT REPORT MULTIPLE FEEDS           
*                                                                               
SKIPFEED NTR1                                                                   
         USING NUFDCEL,R2                                                       
         L     R1,=A(FEEDTBL)                                                   
SKIP10   CLI   PRDSV1,0            IF SAME PRODUCT                              
         BE    *+14                                                             
         CLC   PRDSV1,0(R1)        IF SAME PRODUCT                              
         BNE   SKIP20                                                           
         CLC   PRDSV3,1(R1)        IF SAME 3 CHAR PROD                          
         BNE   SKIP20                                                           
         CLC   TALLNTH,4(R1)       AND SAME LENGTH                              
         BNE   SKIP20                                                           
         CLC   TALCMID,5(R1)       AND SAME COMMERCIAL                          
         BE    SKIPYES             SKIP THIS FEED                               
SKIP20   LA    R1,13(R1)                                                        
         CLI   0(R1),0             CHECK 1 CHAR PROD                            
         BNE   SKIP10                                                           
         CLI   1(R1),0             CHECK 3 CHAR PROD                            
         BE    SKIPNO              EOF                                          
         B     SKIP10                                                           
*                                                                               
YES      DS    0H                                                               
SKIPYES  SR    RE,RE                                                            
*                                                                               
NO       DS    0H                                                               
SKIPNO   LTR   RE,RE                                                            
         XIT1                                                                   
         DROP  R2                                                               
                                                                                
                                                                                
* ADD FEED COMMERCIALS TO FEED TABLE                                            
*                                                                               
* PRD(CL1)/LEN(CL1)/COMMERCIAL(CL8)                                             
*                                                                               
ADDFDTBL NTR1                                                                   
         LA    RE,20               MAX OF 20 ENTRIES                            
         L     R1,=A(FEEDTBL)                                                   
ADDF10   CLI   0(R1),0                                                          
         BE    ADDFEED                                                          
         LA    R1,10(R1)                                                        
         BCT   RE,ADDF10                                                        
         DC    H'0'                EXPAND TABLE                                 
ADDFEED  MVC   0(1,R1),PRDSV1      PRODUCT 1 BYTE                               
         MVC   1(3,R1),PRDSV3      PRODUCT 3 CHAR                               
         MVC   4(1,R1),TALLNTH     LENGTH                                       
         MVC   5(8,R1),TALCMID     COMMERCIAL                                   
                                                                                
*************************************************************                   
**       LR    R5,R1                                                            
**       GOTO1 =V(PRNTBL),DMCB,=C'ADDF',0(R5),C'DUMP',10,=C'1D'                 
**       GOTO1 =V(PRNTBL),DMCB,=C'ADDF',0(R2),C'DUMP',30,=C'1D'                 
**************************************************************                  
         B     XIT                                                              
                                                                                
         EJECT                                                                  
* IF MULT PRODS, USE FEED POSITION NUMBER TO INDEX INTO NBPRDLST                
* TO GET PRODUCT THAT GOES WITH THIS FEED                                       
* R2 -> FEED ELEMENT                                                            
FEEDPROD NTR1                                                                   
         USING NUFDCEL,R2                                                       
         CLI   NBPRDNO,0           MULTIPRODS?                                  
         BE    FDPX                NO                                           
*                                  YES                                          
         LA    R1,NBPRDLST         LIST OF PRODUCTS (FROM X'14 ELEM)            
         ZIC   RE,NUFDPPOS         PROD POSITION NUMBER FROM FEED ELEM          
                                                                                
         C     RE,=F'0'            IF NO PROD POSITION INDICATOR                
         BE    FDPX                GO WITH WHAT WE HAVE                         
                                                                                
         BCTR  RE,0                                                             
         AR    R1,RE                                                            
         MVC   BYTE,0(R1)          SET PROD CODE FROM NBPRDLST                  
*                                                                               
FDPX     XIT1                                                                   
         EJECT                                                                  
*=====================================================================          
* PAD RIGHT SIDE OF STATION CODE WITH ZEROES - GET RID OF SPACES                
* R3 --> TANP ELEMENT                                                           
*=====================================================================          
*                                                                               
         USING TANPD,R3                                                         
PADZERO  NTR1                                                                   
         LA    R1,TANPNTI                                                       
         LA    R0,4                                                             
PZERO10  CLI   0(R1),X'40'       CHARACTER OR SPACE?                            
         BNH   PZERO20                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,PZERO10                                                       
         B     PZEROX                                                           
PZERO20  MVI   0(R1),0           ONCE WE FIND A SPACE, PAD RIGHT                
         LA    R1,1(R1)          WITH ZEROES                                    
         BCT   R0,PZERO20                                                       
PZEROX   XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
*&&DO                                                                           
*=====================================================================          
* EXPECTS -  COMMERCIAL ID IN TALCMID                                           
* RETURNS  - COMMERCIAL AD-ID IN TALADID                                        
*=====================================================================          
*                                                                               
GETADID  NTR1                                                                   
         XC    TALADID,TALADID                                                  
         XC    MYWORK(100),MYWORK                                               
         MVC   DTADSPSV,NBDTADSP                SAVE CURRENT NBDTADSP           
         MVC   NBDTADSP,=H'24'                                                  
         XC    MYKEY,MYKEY                                                      
         MVC   MYKEY(2),=X'0A21'                                                
         MVC   MYKEY+2(1),NBACTAM                                               
         MVC   MYKEY+3(2),NBACTCLI                                              
         MVC   MYKEY+5(8),TALCMID                                               
         BAS   RE,RDTHI            TRAFFIC                                      
         CLC   MYKEYSV(13),MYKEY                                                
         BE    ADID05                                                           
         SR    R1,R1                                                            
         B     ADID20                                                           
ADID05   BAS   RE,GTTREC           TRAFFIC                                      
         L     R2,AMYIO                                                         
         USING CMLADIEL,R2                                                      
         MVI   ELCODE,X'A0'        IS IT AN AD-ID COMMERCIAL?                   
         BAS   RE,GETEL                                                         
         BNE   ADID20                                                           
         MVC   TALADID,CMLADID                                                  
*                                                                               
ADID20   MVI   NBFUNCT,NBFRDHI                                                  
         MVC   NBDTADSP,DTADSPSV                                                
         B     XIT                                                              
         EJECT                                                                  
*&&                                                                             
*======================================================================         
* - RFP SYMBOLIC NAME VALIDATION                                                
*  R2 POINTS TO FIELD HEADER                                                    
*======================================================================         
VALRFP   NTR1                                                                   
         OC    ARFPBLK,ARFPBLK     IF IN RFP                                    
         BZ    NO                                                               
         CLI   OFFLINE,C'Y'        AND ONLINE                                   
         BE    NO                                                               
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0                                                          
         BE    NO                                                               
*        TM    4(R2),X'20'         ALREADY VALIDATED                            
*        BO    YES                                                              
                                                                                
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING QRFPD,R3                                                         
         MVI   QRFPMODE,QRFPSYMB   SYMBOLIC NAME VALIDATION                     
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   QRFPWORK(0),8(R2)   PASS SYMBOLIC NAME                           
         OC    QRFPWORK,=X'40404040404040404040'                                
         GOTO1 RFP,DMCB,(R3)                                                    
         OC    QRFPWORK,QRFPWORK   ERROR                                        
         BZ    NO                                                               
         MVC   8(L'QRFPESC,R2),QRFPWORK                                         
         MVI   5(R2),8             SET LENGTH OF EXPLODED DATA                  
         MVI   11(R2),8            PASS LENGTH OF EXPLODED DATA                 
         OI    4(R2),X'20'         SET VALIDATED BIT                            
         B     YES                                                              
*                                                                               
         DROP  R3                                                               
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        BOUNCE, METV, ANTENNA TV TABLE                                         
*                                                                               
BNMTTAB  DC    C'BOUN',C'BOU ',C'BNCE',C'BNCT',C'METV',C'ANTN'                  
         DC    C'COZI',C'ESCA',C'ESCP',C'GET ',C'GETT',C'GETV'                  
         DC    C'GRIT',C'JUST',C'LAFF',C'THIS'                                  
         DC    X'FF'                                                            
*                                                                               
NETLIST  DC    4000X'00'                                                        
         DS    2000X'00'                                                        
         DC    C'**PROD**'                                                      
PRODTBL  DS    CL2004           500 PRODS X (1 BYTE/3CHAR)                      
         DC    C'**FEED**'                                                      
FEEDTBL  DS    CL325               MAX OF 25 FEED TBL ENTRIES                   
NTILIST  DC    4000X'00'                                                        
XPRDMSK  DC    2000X'00'           500 (1 BYTE BINARY + 3 CHAR CODE)            
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
* READ TN2 PROFILE TO GET NUMBER OF DAYS PREVIOUS TO REQUEST DATE     *         
* READ TN2 PROFILE TO GET ABSOLUTE EARLIEST UNIT START DATE           *         
* IF REQ START IS BEFORE ABSOLUTE START,USE ABSOLUTE START            *         
***********************************************************************         
GETTN2   NTR1  BASE=*,LABEL=*                                                   
         MVC   MYREQST,NBSELSTR     SAVE REQ START                              
         GOTO1 DATCON,DMCB,(0,NBSELSTR),(2,REQSTDAT)   SAVE REQ START           
*                                                                               
         XC    MYKEY,MYKEY                CHECK FOR SPECIFIC PROFILES           
         MVI   MYKEY,X'A2'                LOWER CASE 'S'                        
         MVC   MYKEY+1(3),=C'TN2'                                               
         MVC   MYKEY+4(2),NBEFFAGY                                              
         MVC   MYKEY+6(1),NBSELMFL                                              
         CLI   NBSELMFL,0                                                       
         BNE   *+10                                                             
         MVC   MYKEY+6(1),NBUSRMED                                              
         CLC   NBSELCLI,=C'ALL'           IF RUNNING FOR ALL CLIENTS,           
         BE    *+10                       LEAVE CLIENT BLANK                    
         MVC   MYKEY+7(3),NBCLICOD                                              
         GOTO1 NBGTPROF,DMCB,MYKEY,MYWORK,NBDM                                  
                                                                                
         MVC   SVNUSER,MYWORK+4        NETTAL USER Y/N/1                        
*                                                                               
*&&DO                                                                           
         CLI   UPDATE,C'N'             IF TEST RUN = N,                         
         BE    GTN202                                                           
         CLI   DOCABLE,C'Y'            BUT DOING CABLE ONLY,                    
         BNE   GTN202                                                           
* SPSUG-1239                                                                    
*        TM    SVAYSTA5,TAAYNETC       IF AGENCY STATUS CX SET,                 
*        BNO   GTN201                                                           
         CLI   SVNUSER,C'1'            NETTAL USER MUST BE 1                    
         BE    GTN202                                                           
GTN201   MVI   UPDATE,C'N'             DO NOT MARK FILE IF NUSER=Y,N            
*&&                                                                             
*                                                                               
GTN202   CLC   NBSELCLI,=C'ALL'        IF RUNNING FOR ALL CLIENTS,              
         BNE   GTN202A                                                          
         MVI   MYWORK+1,X'5A'          GO BACK 90 DAYS PRIOR                    
         B     GTN203                                                           
                                                                                
GTN202A  MVC   WORK(2),MYWORK+2        YR/MONTH ABSOLUTE START                  
         CLI   WORK,0                                                           
         BE    GTN203                                                           
         MVI   WORK+2,1                FUDGE DAY                                
         GOTO1 DATCON,DMCB,(3,WORK),(0,WORK)  YYMMDD ABSOLUTE START             
                                                                                
GTN203   LLC   R1,MYWORK+1          GET NUMBER OF DAYS BACK 0-255               
         LNR   R1,R1                                                            
         STCM  R1,15,DMCB+8                                                     
         MVC   MYWORK(6),NBSELSTR                                               
         GOTO1 ADDAY,DMCB,MYWORK,NBSELSTR                                       
         CLC   NBSELCLI,=C'ALL'        IF RUNNING FOR ALL CLIENTS,              
         BE    GTN2X                   IGNORE ABSOLUTE START FOR NOW            
                                                                                
* - CHK NBSELSTR NOT BEFORE TN2 PROFILE ABSOLUTE START                          
         CLC   NBSELSTR,WORK      REQ START DATE BEFORE ABSOLUTE START          
         BNL   *+10                                                             
         MVC   NBSELSTR,WORK      USE ABSOLUTE START                            
GTN2X    J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READ TN2 PROFILE TO GET NUMBER OF DAYS PREVIOUS TO REQUEST DATE     *         
* READ TN2 PROFILE TO GET ABSOLUTE EARLIEST UNIT START DATE           *         
* IF REQ START IS BEFORE ABSOLUTE START,USE ABSOLUTE START            *         
* FOR ALL CLIENTS ONLY.  SAVE ADJUSTED START TO FILTER AIR DATE       *         
***********************************************************************         
GETTN22  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    MYKEY,MYKEY                CHECK FOR SPECIFIC PROFILES           
         MVI   MYKEY,X'A2'                LOWER CASE 'S'                        
         MVC   MYKEY+1(3),=C'TN2'                                               
         MVC   MYKEY+4(2),NBEFFAGY                                              
         MVC   MYKEY+6(1),NBSELMFL                                              
         CLI   NBSELMFL,0                                                       
         BNE   *+10                                                             
         MVC   MYKEY+6(1),NBUSRMED                                              
         MVC   MYKEY+7(3),NBCLICOD                                              
         GOTO1 NBGTPROF,DMCB,MYKEY,MYWORK,NBDM                                  
                                                                                
         MVC   SVNUSER,MYWORK+4        NETTAL USER Y/N/1                        
*                                                                               
         MVC   WORK(2),MYWORK+2        YR/MONTH ABSOLUTE START                  
         CLI   WORK,0                                                           
         BE    GTN2203                                                          
         MVI   WORK+2,1                FUDGE DAY                                
         GOTO1 DATCON,DMCB,(3,WORK),(0,WORK)  YYMMDD ABSOLUTE START             
                                                                                
GTN2203  LLC   R1,MYWORK+1          GET NUMBER OF DAYS BACK 0-255               
         LNR   R1,R1                                                            
         STCM  R1,15,DMCB+8                                                     
         MVC   MYWORK(6),NBSELSTR                                               
         CLC   NBSELCLI,=C'ALL'        IF RUNNING FOR ALL CLIENTS,              
         BNE   GTN2210                                                          
         MVC   MYWORK(6),MYREQST       USE SAVED REQUEST START                  
GTN2210  GOTO1 ADDAY,DMCB,MYWORK,MYSVSTR                                        
* - CHK NBSELSTR NOT BEFORE TN2 PROFILE ABSOLUTE START                          
         CLC   MYSVSTR,WORK       REQ START DATE BEFORE ABSOLUTE START          
         BNL   *+10                                                             
         MVC   MYSVSTR,WORK      USE ABSOLUTE START                             
                                                                                
         GOTO1 DATCON,DMCB,(0,MYSVSTR),(2,MYSVSTR2)   SAVE ADJ START            
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        IF SYSTEM IS ADV4 RETURN CC EQ                                         
*        AGENCY SJR CANNOT HAVE TEST RUN = N ON ADV                             
*                                                                               
CKSYS    NTR1  BASE=*,LABEL=*                                                   
         CLI   OFFLINE,C'Y'        ONLINE ONLY                                  
         JE    NO                                                               
         USING FACTSD,RF                                                        
         GOTO1 GETFACT,DMCB,0      GET DSPACE ONLINE                            
         L     RF,DMCB                                                          
         CLI   FASYSID,1           IF DSPACE IS TEST,                           
         JE    NO                  ALLOW TEST RUN = N                           
         CLI   FASYSID,15          OR IF DSPACE IS FQA,                         
         JE    NO                  ALLOW TEST RUN = N                           
         CLI   FASYSID,11          OR IF DSPACE IS CSC,                         
         JE    NO                  ALLOW TEST RUN = N                           
         J     YES                 OTHERWISE DO NOT ALLOW                       
         DROP  RF                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        RECOGNIZE DIGINET NETWORKS                                             
*                                                                               
         USING SORTD,R1                                                         
DIGINET  NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'BOUN',NBACTNET  IS IT BOUNCE NETWORK?                         
         BE    BNIT                                                             
         CLC   =C'BOUN',NBNTISTA  IS IT BOUNCE NETWORK?                         
         BE    BNIT                                                             
         CLC   =C'BOU ',NBACTNET  IS IT BOU?                                    
         BE    BNIT                                                             
         CLC   =C'BOU ',NBNTISTA  IS IT BOU?                                    
         BE    BNIT                                                             
         CLC   =C'BNCE',NBACTNET  IS IT BNCE?                                   
         BE    BNIT                                                             
         CLC   =C'BNCE',NBNTISTA  IS IT BNCE?                                   
         BE    BNIT                                                             
         CLC   =C'BNCT',NBACTNET  IS IT BNCT?                                   
         BE    BNIT                                                             
         CLC   =C'BNCT',NBNTISTA  IS IT BNCT?                                   
         BNE   *+12                                                             
BNIT     MVI   SDSTATYP,C'B'       MAKE IT B                                    
         J     YES                                                              
*                                                                               
         CLC   =C'METV',NBACTNET  IS IT METV?                                   
         BE    MEIT                                                             
         CLC   =C'METV',NBNTISTA  IS IT METV?                                   
         BNE   *+12                                                             
MEIT     MVI   SDSTATYP,C'E'       MAKE IT E                                    
         J     YES                                                              
*                                                                               
         CLC   =C'ANTN',NBACTNET  IS IT ANTENNA TV?                             
         BE    ANIT                                                             
         CLC   =C'ANTN',NBNTISTA  IS IT ANTENNA TV?                             
         BNE   *+12                                                             
ANIT     MVI   SDSTATYP,C'Y'       MAKE IT Y                                    
         J     YES                                                              
*                                                                               
         CLC   =C'COZI',NBACTNET  IS IT COZI TV?                                
         BE    CZIT                                                             
         CLC   =C'COZI',NBNTISTA  IS IT COZI TV?                                
         BNE   *+12                                                             
CZIT     MVI   SDSTATYP,C'Z'       MAKE IT Z                                    
         J     YES                                                              
*                                                                               
         CLC   =C'ESCA',NBACTNET  IS IT ESCAPE?                                 
         BE    EPIT                                                             
         CLC   =C'ESCA',NBNTISTA  IS IT ESCAPE?                                 
         BE    EPIT                                                             
         CLC   =C'ESCP',NBACTNET  IS IT ESCAPE?                                 
         BE    EPIT                                                             
         CLC   =C'ESCP',NBNTISTA  IS IT ESCAPE?                                 
         BNE   *+12                                                             
EPIT     MVI   SDSTATYP,C'P'       MAKE IT P                                    
         J     YES                                                              
*                                                                               
         CLC   =C'GET ',NBACTNET  IS IT GETTV?                                  
         BE    GEIT                                                             
         CLC   =C'GET ',NBNTISTA  IS IT GETTV?                                  
         BE    GEIT                                                             
         CLC   =C'GETT',NBACTNET  IS IT GETTV?                                  
         BE    GEIT                                                             
         CLC   =C'GETT',NBNTISTA  IS IT GETTV?                                  
         BE    GEIT                                                             
         CLC   =C'GETV',NBACTNET  IS IT GETTV?                                  
         BE    GEIT                                                             
         CLC   =C'GETV',NBNTISTA  IS IT GETTV?                                  
         BNE   *+12                                                             
GEIT     MVI   SDSTATYP,C'G'       MAKE IT G                                    
         J     YES                                                              
*                                                                               
         CLC   =C'GRIT',NBACTNET  IS IT GRIT?                                   
         BE    GRIT                                                             
         CLC   =C'GRIT',NBNTISTA  IS IT GRIT?                                   
         BNE   *+12                                                             
GRIT     MVI   SDSTATYP,C'R'       MAKE IT R                                    
         J     YES                                                              
*                                                                               
         CLC   =C'JUST',NBACTNET  IS IT JUSTICE NETWORK?                        
         BE    JSIT                                                             
         CLC   =C'JUST',NBNTISTA  IS IT JUSTICE NETWORK?                        
         BNE   *+12                                                             
JSIT     MVI   SDSTATYP,C'J'       MAKE IT J                                    
         J     YES                                                              
*                                                                               
         CLC   =C'LAFF',NBACTNET  IS IT LAFF?                                   
         BE    LFIT                                                             
         CLC   =C'LAFF',NBNTISTA  IS IT LAFF?                                   
         BNE   *+12                                                             
LFIT     MVI   SDSTATYP,C'L'       MAKE IT L                                    
         J     YES                                                              
*                                                                               
         CLC   =C'THIS',NBACTNET  IS IT THIS TV?                                
         BE    THIT                                                             
         CLC   =C'THIS',NBNTISTA  IS IT THIS TV?                                
         BNE   *+12                                                             
THIT     MVI   SDSTATYP,C'T'       MAKE IT T                                    
         J     YES                                                              
*                                                                               
         J     NO                                                               
         DROP  R1                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        RECOGNIZE UNWIRED NETWORKS                                             
*                                                                               
         USING SORTD,R1                                                         
UNWIRED  NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'ACTI',NBACTNET  IS IT ACTIVE INTERNATIONAL?                   
         BE    ACIT                                                             
         CLC   =C'ACTI',NBNTISTA  IS IT ACTIVE INTERNATIONAL?                   
         BNE   *+12                                                             
ACIT     MVI   SDSTATYP,C'D'      MAKE IT D                                     
         J     YES                                                              
*                                                                               
         CLC   =C'CNTM',NBACTNET  IS IT CONTINUUM MEDIA?                        
         BE    COIT                                                             
         CLC   =C'CNTM',NBNTISTA  IS IT CONTINUUM MEDIA?                        
         BNE   *+12                                                             
COIT     MVI   SDSTATYP,C'K'      MAKE IT K                                     
         J     YES                                                              
*                                                                               
         CLC   =C'REV ',NBACTNET  IS IT REVSHARE?                               
         BE    REIT                                                             
         CLC   =C'REV ',NBNTISTA  IS IT REVSHARE?                               
         BNE   *+12                                                             
REIT     MVI   SDSTATYP,C'Q'      MAKE IT Q                                     
         J     YES                                                              
*                                                                               
         CLC   =C'ICON',NBACTNET  IS IT ICON INTERNATIONAL?                     
         BE    ICIT                                                             
         CLC   =C'ICON',NBNTISTA  IS IT ICON INTERNATIONAL?                     
         BNE   *+12                                                             
ICIT     MVI   SDSTATYP,C'O'      MAKE IT O                                     
         J     YES                                                              
*                                                                               
         CLC   =C'CDNT',NBACTNET  IS IT CADENT?                                 
         BE    CDIT                                                             
         CLC   =C'CDNT',NBNTISTA  IS IT CADENT?                                 
         BNE   *+12                                                             
CDIT     MVI   SDSTATYP,C'H'      MAKE IT H                                     
         J     YES                                                              
*                                                                               
         J     NO                                                               
         DROP  R1                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        SORT MEDIA TYPE FIELD                                                  
*        NETWORK = 1, CABLE = 2, SYNDICATION = 3, EVERYTHING ELSE = 4           
*                                                                               
         USING SORTD,R1                                                         
MEDSORT  NTR1  BASE=*,LABEL=*                                                   
         MVI   SDSTASRT,1          NETWORK                                      
         CLI   SDSTATYP,C'N'                                                    
         JE    XIT                                                              
                                                                                
         MVI   SDSTASRT,2          CABLE                                        
         CLI   SDSTATYP,C'C'                                                    
         JE    XIT                                                              
                                                                                
         MVI   SDSTASRT,3          SYNDICATION                                  
         CLI   SDSTATYP,C'S'                                                    
         JE    XIT                                                              
                                                                                
         MVI   SDSTASRT,4          EVERYTHING ELSE                              
         J     XIT                                                              
         DROP  R1                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*===============================================================                
*  WORKER INTERFACE                                                             
*===============================================================                
         SPACE                                                                  
OPENWRKR NTR1  BASE=*,LABEL=*                                                   
         TM    WRKRFLAG,WRKROPEN   WORKER FILE ALREADY OPEN?                    
         JO    XIT                                                              
         XC    ID,ID                                                            
         LA    RE,IOWRKR                                                        
         LA    RF,300                                                           
         XCEF                                                                   
         LA    R3,ID                                                            
         USING UKRECD,R3                                                        
         L     RF,ATWA                                                          
         USING T320FFD,RF                                                       
         MVC   UKUSRID,TWAORIG                                                  
         MVC   UKSYSPRG,=C'NE1'                                                 
         GOTO1 DATCON,DMCB,(5,0),(1,COUNTER)       TODAY'S DATE                 
         MVC   UKDAY,COUNTER+2                                                  
         MVI   UKCLASS,C'T'        CLASS                                        
         OI    UKFLAG,X'01'                    ALLOW DUPLICATE KEYS             
         OI    UKFLAG,X'10'                    RETENTION DAYS                   
         MVC   COMMAND,=CL8'OPEN'                                               
*                                                                               
         LA    R1,IOWRKR+28                                                     
         USING WKRECD,R1                                                        
         MVC   WKRETN,=H'7'                                                     
         DROP  R1,RF                                                            
*                                                                               
         LA    R3,IOWRKR                                                        
         L     R4,=A(WRKRBUFF)                                                  
         GOTO1 WORKER,DMCB,COMMAND,(R4),ID,(R3)                                 
         OI    WRKRFLAG,WRKROPEN                                                
         XIT1                                                                   
         LTORG                                                                  
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
ADDWRKR  NTR1  BASE=*,LABEL=*                                                   
         MVC   COMMAND,=CL8'ADD'                                                
         LA    R3,IOWRKR                                                        
         L     R4,=A(WRKRBUFF)                                                  
         GOTO1 WORKER,DMCB,COMMAND,(R4),ID,(R3)                                 
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
CLOSWRKR NTR1  BASE=*,LABEL=*                                                   
         TM    WRKRFLAG,WRKROPEN   HAS WORKER FILE BEEN OPENED?                 
         JZ    XIT                                                              
         MVC   COMMAND,=CL6'CLOSE'                                              
         LA    R3,IOWRKR                                                        
         L     R4,=A(WRKRBUFF)                                                  
         GOTO1 WORKER,DMCB,COMMAND,(R4),ID,(R3)                                 
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   COMMAND,=CL8'RETAIN'                                             
*                                                                               
         LA    R1,IOWRKR+28                                                     
         USING WKRECD,R1                                                        
         MVC   WKRETN,=H'7'                                                     
         DROP  R1                                                               
*                                                                               
         LA    R3,IOWRKR                                                        
         L     R4,=A(WRKRBUFF)                                                  
         GOTO1 WORKER,DMCB,COMMAND,(R4),ID,(R3)                                 
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* SAVE COPY OF WORKER FILE TO MVS DATASET                                       
*=============================================================                  
                                                                                
AUDITWRK NTR1  BASE=*,LABEL=*                                                   
         L     R7,ANETWS2          ANETWS2=WORKING STORAGE                      
         AHI   R7,500                                                           
         GOTO1 DATCON,DMCB,(5,0),(3,TODAYB) TODAY'S DATE 3-BYTE BINARY          
         GOTO1 DATCON,DMCB,(5,0),(1,TODAYP) TODAY'S DATE 3-BYTE PACKED          
*                                                                               
         L     R1,ATWA                                                          
         L     R1,TWAMASTC-T320FFD(R1)  POINT TO MASTC                          
         USING MCBLOCK,R1                                                       
         MVI   WRITEMC,C'N'                                                     
         CLI   MCWRITE,C'Y'        IF WRITE=YES, SET FLAG                       
         BNE   *+8                                                              
         MVI   WRITEMC,C'Y'                                                     
         DROP  R1                                                               
*                                                                               
         BAS   RE,RDFRSTW          READ FIRST WORKER FILE                       
         JNE   XIT                 IF NO WORKER FILES, SKIP AUDIT               
*                                                                               
         BAS   RE,OPENMVS          OPEN MVS CLONE DATASET                       
         BAS   RE,PUTMVS           READ WRKR FILES AND PUT TO DATASET           
         BRAS  RE,CLOSMVS          CLOSE MVS CLONE DATASET                      
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*==================================================================             
* READ FIRST WORKER FILE.  IF NONE EXIST, RETURN CCNEQ.                         
*==================================================================             
RDFRSTW  NTR1                                                                   
         L     R4,=A(WRKBUFF)  SET A(WORKER FILE BUFFER)                        
         ST    R4,AWRKBUFF                                                      
*                                                                               
         XC    INDEX,INDEX                                                      
         XC    WRKNM,WRKNM                                                      
         MVC   WRKFILEN,=CL8'WKFILE'  SET OLD WORKER FILE                       
         MVC   WRKNM(3),=C'NE1'  SET TO READ NET WRKR FILES                     
*                                                                               
         BRAS  RE,GTWFILE      GET FIRST/(NEXT) WORKER FILE                     
         JNE   NO                                                               
*                                                                               
         J     YES                                                              
*                                                                               
         EJECT                                                                  
*==================================================================             
* DYNAMICALLY ALLOCATE MVS SEQUENTIAL DATASET                                   
*                                                                               
* SYSTEM RECORD ON TAL FILE WILL BE ADDED TO POINT TO THIS FILE                 
*==================================================================             
                                                                                
OPENMVS  NTR1                                                                   
         USING SSOOFF,R3                                                        
         GOTO1 DATAMGR,DMCB,=C'SSBAD'                                           
         ICM   R3,15,4(R1)         =A(SSB)                                      
         CLC   0(2,R3),=H'00'                                                   
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   BYTE,SSODSPAC-SSOOFF(R3)                                         
         DROP  R3                                                               
*                                                                               
         USING MYDSND,R3                                                        
         LA    R3,FILEDSN                                                       
         MVC   FILEDSN,MYDSN       DEFAULT TO ADV                               
         CLI   BYTE,C'A'                                                        
         JE    OPENW1                                                           
*                                                                               
         MVC   FILEDSN,MYDSN2                                                   
         CLI   BYTE,C'C'                                                        
         JNE   *+10                                                             
         MVC   FILEDSN(3),=C'CSC'                                               
         CLI   BYTE,C'Q'                                                        
         JNE   *+10                                                             
         MVC   FILEDSN(3),=C'FQA'                                               
         CLI   BYTE,C'T'                                                        
         JNE   *+10                                                             
         MVC   FILEDSN(3),=C'TST'                                               
*                                                                               
OPENW1   L     RF,ATWA                                                          
         MVC   MYDSNAGY(2),TWAAGY-T320FFD(RF)  SET ORIGINATING AGY              
*                                                                               
         L     RF,TWAMASTC-T320FFD(RF)                                          
         USING MASTD,RF                                                         
         CLI   MCTSTRUN,X'FF'                                                   
         BNE   *+8                                                              
         MVI   MYDSNTST,C'2'       SET TEST FILENUM IN NAME                     
         DROP  RF                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(3,BTODAY),(X'20',MYDSNDAT)                          
*                                                                               
         TIME  DEC                 R0=HHMMSSHH                                  
         ST    R0,FULL                                                          
         GOTO1 HEXOUT,DMCB,FULL,DUB,4,=C'TOG'                                   
         MVC   MYDSNT1(6),DUB       MOVE HHMMSS                                 
         DROP  R3                                                               
*                                                                               
         L     RF,ATWA                                                          
         L     RF,TWADCONS-T320FFD(RF)                                          
         USING TWADCOND,RF                                                      
         MVC   PRNTBL,TPRNTBL                                                   
         DROP  RF                                                               
         L     RF,TDYNALLO-TWADCOND(RF)                                         
         GOTO1 (RF),DMCB,(X'80',=CL8'FILEOUT'),(X'87',ALLOCS),         X        
               (X'80',FILEDSN)  <=== UNALLOCATE AT CLOSE!                       
*                                                                               
         OPEN  (FILEOUT,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ID,ID                                                            
         LA    R3,ID                                                            
         USING UKRECD,R3                                                        
         L     RF,ATWA                                                          
         USING T320FFD,RF                                                       
         MVC   UKUSRID,TWAORIG                                                  
         MVC   UKSYSPRG,=C'NE1'                                                 
         GOTO1 DATCON,DMCB,(5,0),(1,COUNTER)       TODAY'S DATE                 
         MVC   UKDAY,COUNTER+2                                                  
         MVI   UKCLASS,C'T'        CLASS                                        
         OI    UKFLAG,X'01'                    ALLOW DUPLICATE KEYS             
         OI    UKFLAG,X'10'                    RETENTION DAYS                   
         DROP  R3                                                               
*                                                                               
         MVC   IOWDATA(L'UKINDEX),ID      MOVE ID TO IO BUFFER                  
         L     RF,ATWA                                                          
         L     RF,TWAMASTC-T320FFD(RF)                                          
*        MVC   IOWDATA+20(10),MCUSERID-MASTD(RF)                                
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING CTIKEY,R3                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,MCORIGID-MASTD(RF)    SET ORIGIN ID NUMBER               
         MVC   AIO,NBAIO                                                        
         MVI   USEIO,C'Y'                                                       
         MVC   FILENAME,=CL8'CTFILE'                                            
         GOTO1 HIGH                                                             
         MVI   ELCODE,2                                                         
         MVC   HALF,NBDTADSP                                                    
         MVC   NBDTADSP,=H'28'                                                  
         L     R2,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST EXIST/ELSE BOMB                         
         ZIC   R1,1(R2)                                                         
         A     R1,=F'-3'                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IOWDATA+20(0),2(R2) SET AGENCY ID TO WORK                        
*                                                                               
         LA    RE,34               SET REC LEN                                  
         SLL   RE,16                                                            
         ST    RE,IOWLEN                                                        
*                                                                               
         L     R1,=A(FILEOUT)                                                   
         LA    R0,IOWRKR                                                        
         PUT   (1),(0)             PUT WRKR HDR AS FIRST FILE REC               
         J     XIT                                                              
*                                                                               
ALLOCS   DC    AL3(20),AL3(10)     PRIMARY/SECONDARY ALLOCATIONS                
         EJECT                                                                  
*=============================================================                  
* READ WORKER FILES AND PUT TO MVS DATASET                                      
*=============================================================                  
                                                                                
PUTMVS   NTR1                                                                   
*                                                                               
         L     R4,=A(WRKBUFF)  SET A(WORKER FILE BUFFER)                        
         ST    R4,AWRKBUFF                                                      
*                                                                               
         XC    INDEX,INDEX                                                      
         XC    WRKNM,WRKNM                                                      
         MVC   WRKFILEN,=CL8'WKFILE'  SET OLD WORKER FILE                       
         MVC   WRKNM(3),=C'NE1'  SET TO READ NET WRKR FILES                     
*                                                                               
PMVS10   BAS   RE,GTWFILE      GET FIRST/(NEXT) WORKER FILE                     
         BNE   PMVSX                                                            
*                                                                               
         BAS   RE,WKREAD       READ FIRST WORKER FILE RECORD                    
         BNE   PMVS10                                                           
         BAS   RE,PROCWF       READ WRKR FILE, SAVE TO DATASET                  
         BAS   RE,CLOSE        CLOSE WORKER FILE                                
         BAS   RE,INDRERD      RE-READ THIS INDEX RECORD                        
         B     PMVS10          LOOP FOR NEXT WORKER FILE                        
*                                                                               
PMVSX    J     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET A WORKER FILE                                     
         SPACE 1                                                                
GTWFILE  NTR1                                                                   
         XC    RECORD,RECORD                                                    
         LA    R2,INDEX                                                         
         USING UKRECD,R2                                                        
*                                                                               
GTWFL20  BAS   RE,INDEXRD          GET INDEX OF FIRST/NEXT FILE                 
         JNE   NO                                                               
*                                                                               
         L     RF,ATWA                                                          
         USING T320FFD,RF                                                       
         TM    UKSTAT,X'08'        WORKER FILE MUST NOT BE ON KEEP              
         BO    GTWFL20                                                          
         CLC   UKUSRID,TWAORIG                                                  
         BNE   GTWFL20                                                          
         DROP  RF                                                               
*                                                                               
         CLC   UKSYSPRG,WRKNM      WORKER FILE NAME MUST MATCH                  
         BNE   GTWFL20                                                          
         CLI   UKCLASS,C'T'        MUST BE CLASS T                              
         BNE   GTWFL20                                                          
         CLC   UKDAY,TODAYP+2      MUST BE TODAY'S DATE                         
         BNE   GTWFL20                                                          
*                                                                               
         LA    R3,L'INDEX                                                       
         GOTO1 MYTRACE,DMCB,=C'GOOD INDEX',INDEX,(R3)                           
         J     YES                                                              
         EJECT                                                                  
*                                                                               
*              WORKER FILE I/O ROUTINES                                         
*                                                                               
         SPACE 3                                                                
*              ROUTINE TO READ A RECORD IN THE CURRENT WORKER FILE              
*                                  SAME ROUTINE FOR OLD & NEW STYLE             
WKREAD   NTR1                                                                   
         LA    RE,WREC             CLEAR WORKER FILE RECORD AREA                
         LH    RF,=AL2(L'WREC)                                                  
         XCEFL                                                                  
*                                                                               
         MVC   COMMAND2,=CL8'READ'                                              
         GOTO1 DATAMGR,DMCB,COMMAND2,WRKFILEN,INDEX,WREC,AWRKBUFF               
         CLI   DMCB+8,0            IF RECORD FOUND                              
         JNE   NO                                                               
*                                                                               
         LH    R2,WREC             LENGTH OF RECORD                             
         SH    R2,=H'4'                                                         
         GOTO1 MYTRACE,DMCB,=C'READ WORKER RECORD',WREC+4,(R2)                  
         J     YES                                                              
         SPACE 2                                                                
*              ROUTINE TO TRY TO FIND A WORKER FILE                             
*                                                                               
INDEXRD  NTR1                                                                   
         MVC   COMMAND2,=CL8'INDEX'                                             
INDRD10  GOTO1 DATAMGR,DMCB,COMMAND2,WRKFILEN,INDEX,WREC,AWRKBUFF               
         CLI   8(R1),0             IF NO ERROR CONDITION CODE                   
         JNE   NO                                                               
*                                                                               
         LA    R3,L'INDEX                                                       
         GOTO1 MYTRACE,DMCB,=C'POSSIBLE INDEX',INDEX,(R3)                       
         J     YES                                                              
         EJECT                                                                  
*              ROUTINE REREADS INDEX OF CURRENT FILE                            
         SPACE 1                                                                
INDRERD  NTR1                                                                   
         MVC   COMMAND2,=CL8'INDEX'                                             
         GOTO1 DATAMGR,DMCB,COMMAND2,WRKFILEN,INDEX,WREC,AWRKBUFF               
*                                                                               
         LA    R3,L'INDEX                                                       
         GOTO1 MYTRACE,DMCB,=C'REREAD OF INDEX',INDEX,(R3)                      
         J     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS CURRENT WORKER FILE                           
*              STAL LAYOUT - ONE HDR REC FOLW'D BY ONE OR MORE DTL RECS         
*                            DTL REC CONTAINS ALL WSP ELEMENTS                  
         SPACE 1                                                                
PROCWF   NTR1                                                                   
         B     *+12                                                             
*                                                                               
PROCWF5  BAS   RE,WKREAD           READ NEXT WORKER FILE RECORD                 
         JNE   PROCWFX                                                          
*                                                                               
         LA    RE,IOWRKR                                                        
         LHI   RF,300                                                           
         LA    R0,WREC                                                          
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         L     R1,=A(FILEOUT)                                                   
         LA    R0,IOWRKR                                                        
         PUT   (1),(0)             PUT TO DATASET                               
         B     PROCWF5                                                          
*                                                                               
* INSERT AN EOF RECORD BETWEEN WORKER FILE COPIES                               
                                                                                
PROCWFX  MVC   IOWDATA(2),=C'/*'                                                
         LA    RE,6                SET REC LEN                                  
         SLL   RE,16                                                            
         ST    RE,IOWLEN                                                        
         L     R1,=A(FILEOUT)                                                   
         LA    R0,IOWRKR                                                        
         PUT   (1),(0)                                                          
*                                                                               
         J     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CLOSE THE CURRENT WORKER FILE (NETWORK ONLY)          
         SPACE 1                                                                
CLOSE    LA    R3,RECORD                                                        
         MVC   COMMAND2,=CL8'CLOSE'                                             
         B     GO                                                               
*                                                                               
GO       NTR1                                                                   
         GOTO1 DATAMGR,DMCB,COMMAND2,WRKFILEN,INDEX,(R3),AWRKBUFF               
         CLI   DMCB+8,0                                                         
         J     XIT                                                              
         EJECT                                                                  
*              SET INFO FOR TRACE ROUTINE                                       
         SPACE                                                                  
MYTRACE  NTR1                                                                   
         TM    SVOPTSW,OPTRACE     IF TRACE IS ON                               
         JNO   XIT                                                              
         L     R2,0(R1)            A(LITERAL)                                   
         L     RF,8(R1)            SET LENGTH OF RECORD                         
         L     R4,4(R1)            A(RECORD)                                    
         GOTO1 PRNTBL,DMCB,(R2),(R4),C'DUMP',(RF),=C'2D'                        
         J     XIT                                                              
         LTORG                                                                  
         SPACE 1                                                                
*=============================================================                  
* CLOSE MVS DATASET THAT COPIES WORKER FILES                                    
*=============================================================                  
                                                                                
CLOSMVS  NTR1  BASE=*,LABEL=*                                                   
         CLOSE FILEOUT                                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         FREEPOOL FILEOUT                                                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*===============================================================                
* ADD A SYSTEM RECORD TO THE TALENT FILE FOR THIS MVS DATASET                   
* (1) CHANGE THE UTL TO POINT TO THE TALENT SYSTEM                              
* (2) IF ASSIGNED DATASPACE IS TST, WRITE TO TAL2                               
*     OTHERWISE, WRITE TO TAL1                                                  
*===============================================================                
                                                                                
*============================================================                   
* OPEN THE TALENT FILE FOR UPDATE IF NOT ALREADY OPEN                           
*============================================================                   
                                                                                
CLOSW0   L     RF,ATWA                                                          
         L     RF,TWAMASTC-T320FFD(RF)  POINT TO MASTC                          
*                                                                               
         L     RE,MCSSB-MASTD(RF)       POINT TO SSB                            
         LA    RE,SSODSPAC-SSOOFF(RE)   POINT TO DATASPACE ID                   
*                                                                               
         L     RF,MCUTL-MASTD(RF)  POINT TO UTL                                 
         MVI   4(RF),X'20'         SENUM FOR TAL2                               
         CLI   0(RE),C'T'          TEST RUNNING ON TST                          
         BE    *+8                                                              
         MVI   4(RF),X'10'         ELSE SET SENUM FOR TAL1                      
*                                                                               
         LA    R1,SVTALSYS                                                      
         LA    R0,8                                                             
*                                                                               
CLOSW2   CLI   0(R1),0             TEST EOL                                     
         BE    CLOSW4                                                           
         CLC   0(1,R1),4(RF)       TEST SYSTEM ALREADY OPEN                     
         BE    CLOSW6                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,CLOSW2                                                        
         DC    H'0'                                                             
*                                                                               
CLOSW4   MVC   0(1,R1),4(RF)       MOVE SYSNUM TO OPEN LIST                     
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'TALENT',TALFLIST,AIO1                 
                                                                                
* CALCULATE TRANSFER PICKUP DATE (NEXT BUSINESS DAY)                            
                                                                                
CLOSW6   LA    R1,DMCB             NORMAL CALL FOR PQ RETAIN                    
         USING GETRETD,R1                                                       
         XC    GRDCB,GRDCB         CLEAR CONTROL BLOCK                          
         MVC   GRDHRS,=H'24'       SET 24 HOURS RETAIN PERIOD                   
         MVC   GRDIDYMD,TODAYB     SET YMD BINARY INPUT DATE                    
         MVC   GRDITHM,=X'0800'    SET HM BINARY INPUT TIME                     
         MVI   GRDFLAG,GRDFTAL+GRDFHLOK                                         
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGETRET-COMFACSD(RF)                                          
         GOTO1 (RF),(R1)                                                        
         CLI   GRDRETC,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
K        USING TLSYD,KEY                                                        
         XC    KEY,KEY                                                          
         MVI   K.TLSYCD,TLSYCDQ                                                 
         MVI   K.TLSYTYPE,TLSYSNFN    TYPE=SPOT/NET TRANSFER                    
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(3),GRDODYMD                                                  
         GOTO1 DATCON,DMCB,(3,DUB),(1,DUB+4)                                    
         MVC   K.TLSYSNDT,DUB+4                                                 
         DROP  R1,K                                                             
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMRDHI'),=C'TALDIR',KEYSAVE,KEY           
*                                                                               
         CLC   KEY(5),KEYSAVE      TEST ANY RECORDS THIS DATE                   
         BNE   CLOS14              NO                                           
*                                                                               
CLOS12   MVC   KEYSAVE,KEY         SAVE HIGH SEQNUM SO FAR                      
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 (RF),(R1),(X'80',=C'DMRSEQ')                                     
         CLC   KEY(5),KEYSAVE                                                   
         BE    CLOS12                                                           
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE LAST KEY                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),=C'TALFIL',KEY+34,AIO1, X        
               DMWORK                                                           
         B     CLOS16                                                           
*                                                                               
CLOS14   L     RE,AIO1             BUILD NEW RECORD                             
         USING TLSYD,RE                                                         
*                                                                               
         XC    0(256,RE),0(RE)                                                  
         MVC   0(32,RE),KEYSAVE                                                 
*                                                                               
         LLC   RF,TLSYSEQ          GET LAST SEQNUM USED                         
         LA    RF,1(RF)                                                         
         STC   RF,TLSYSEQ                                                       
*                                                                               
         LA    RF,TLSYELEM-TLSYD   LENGTH TO FIRST ELEM                         
         STCM  RF,3,TLSYLEN                                                     
         DROP  RE                                                               
*                                                                               
CLOS16   LA    RE,ELEM                                                          
         USING TACMD,RE                                                         
*                                                                               
         MVI   TACMEL,TACMELQ                                                   
         MVI   1(RE),L'FILEDSN+3                                                
         MVI   TACMTYPE,TACMTYPF   SET TYPE=FILENAME                            
         MVC   TACMCOMM(L'FILEDSN),FILEDSN                                      
         DROP  RE                                                               
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CHELLO-COMFACSD(RF) GET HELLO ADDRESS                         
         GOTO1 (RF),DMCB,(C'P',=C'TALFIL'),AIO1,ELEM,(0,=C'ADD=END')            
*                                                                               
         CLI   12(R1),0                                                         
         BE    CLOS20                                                           
*                                                                               
         CLI   12(R1),5            TEST REC TOO LONG                            
         BE    CLOS14              GO BUILD NEW RECORD                          
         DC    H'0'                DIE ON ANY OTHER ERROR                       
*                                                                               
CLOS20   CLI   WRITEMC,C'Y'        IF WRITE=NO, SKIP PUT/ADD                    
         BNE   CLOS25                                                           
*                                                                               
         LA    RE,=C'PUTREC'                                                    
         L     RF,AIO1                                                          
         CLC   KEY(32),0(RF)       TEST KEY MATCHES REC                         
         BE    *+8                                                              
         LA    RE,=C'ADDREC'                                                    
         ST    RE,DMCB                                                          
         MVI   DMCB,X'80'          SET READ FOR UPDATE                          
*                                                                               
         GOTO1 DATAMGR,DMCB,,=C'TALFIL',KEY+34,AIO1,DMWORK                      
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
CLOS25   BRAS  RE,CLOSETAL         CLOSE TALENT FILES IF OFFLINE                
*                                                                               
         L     RF,ATWA                                                          
         L     RF,TWAMASTC-T320FFD(RF)                                          
         L     RF,MCUTL-MASTD(RF)  POINT TO UTL                                 
         MVC   4(1,RF),SVNETSYS    RESTORE NETWORK SYSTEM NUMBER                
*                                                                               
CLOSWX   J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*==================================================================             
* CLOSE ALL OPEN TALENT SYSTEMS AT RUNLAST                                      
*==================================================================             
                                                                                
CLOSETAL NTR1  BASE=*,LABEL=*                                                   
         LA    R4,SVTALSYS                                                      
         LA    R5,8                                                             
*                                                                               
CLOSETA2 CLI   0(R4),0                                                          
         BE    CLOSETAX                                                         
*                                                                               
         L     RF,ATWA                                                          
         L     RF,TWAMASTC-T320FFD(RF) POINT TO MASTC                           
         L     RF,MCUTL-MASTD(RF)      POINT TO UTL                             
         MVC   4(1,RF),0(R4)           MOVE SYSTEM NUMBER                       
*                                                                               
         L     R0,=A(TALFLIST)                                                  
         GOTO1 DATAMGR,DMCB,=C'DMCLSE',=C'TALENT',(R0),AIO1                     
*                                                                               
         LA    R4,1(R4)                                                         
         BCT   R5,CLOSETA2                                                      
*                                                                               
CLOSETAX J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
TALFLIST DC    CL8'UTALDIR '                                                    
         DC    CL8'UTALFIL '                                                    
         DC    CL8'UTALRCV '                                                    
         DC    C'X'                                                             
*                                                                               
MYDSN    DC    CL35'TALDISK.TA0T1SJ.D100521.THHMMSS'                            
MYDSN2   DC    CL35'   .TAL.TA0T1SJ.D100521.THHMMSS'                            
FILEDSN  DS    CL35                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=VB,LRECL=512,MACRF=PM              
*                                                                               
         DS    0D                  WORKER FILE RECORD                           
         DC    C'**WREC**'                                                      
WREC     DS    CL4096                                                           
         DS    0D                                                               
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'**WRK1**'                                                      
WRKBUFF  DS    14336C                                                           
         EJECT                                                                  
WRKRBUFF DS    0D                                                               
         DC    4500X'00'                                                        
         EJECT                                                                  
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* ROUTINE READS TALENT AGENCY RECORD WHILE RUNNING OFFLINE                      
*=====================================================================          
                                                                                
READTLOF NTR1  BASE=*,LABEL=*                                                   
         MVI   SVAYSTA5,0                                                       
         BRAS  RE,OPENTAL       OPEN TALENT FILE                                
*                                                                               
         XC    MYKEY,MYKEY      READ FOR TALENT AGENCY RECORD                   
         LA    R1,MYKEY                                                         
         USING TLAYD,R1                                                         
         MVI   TLAYCD,TLAYCDQ                                                   
         MVC   TLAYAGY,TALAGY       SCREEN TAL AGY FIELD                        
         MVC   MYKEYSV,MYKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=CL8'TALDIR',MYKEY,MYKEY,0               
         CLC   MYKEY(32),MYKEYSV                                                
         BE    *+12                                                             
         MVI   BYTE,X'FF'          IF NOT FOUND SET BYTE                        
         B     RTLOFX              GET OUT                                      
         L     R2,AMYIO            YES/GET THE RECORD                           
         LA    R3,MYKEY+34                                                      
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'TALFILE ',(R3),(R2),MYDMWRK           
*                                                                               
         L     R2,AMYIO            POINT R2 TO TALENT RECORD                    
         MVI   ELCODE,TAAYELQ      FIND AGY ELEMENT                             
         MVC   NBDTADSP,=H'40'                                                  
         BRAS  RE,GETEL                                                         
         BNE   RTLOFX                                                           
         USING TAAYEL,R2                                                        
         MVC   SVAYSTA5,TAAYSTA5   SAVE AGENCY STATUS 5                         
         DROP  R2                                                               
RTLOFX   BRAS  RE,SWNET            SWITCH BACK TO NET FILE                      
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* GET TALENT SE NUMBER FOR NEW AGENCY                                           
* AND OPEN TALENT FILES                                                         
* ALL SYSTEMS ARE OPENED FOR UPDATE TO AVOID OPENING MORE THAN ONCE             
*=====================================================================          
                                                                                
OPENTAL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'                                            
         IC    R0,USEIO                                                         
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING CT5KEY,R2                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,=C'DS'     USE ALPHA ID FOR TPNY                        
         CLC   NBSELAGY,=C'SJ'     UNLESS RUNNING WITH SJR                      
         BNE   *+10                                                             
         MVC   CT5KALPH,NBSELAGY   SET AGENCY ALPHA FOR SJR                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         STC   R0,USEIO                                                         
         XC    FILENAME,FILENAME                                                
         L     R2,AIO                                                           
         LA    R2,CT5DATA                                                       
         SR    R0,R0                                                            
         DROP  R2                                                               
*                                                                               
OPTAL30  CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R2),X'21'         SYSTEM ELEMENT?                              
         BNE   *+12                 NO                                          
         CLI   2(R2),X'07'         TALENT SYSTEM?                               
         BE    *+14                 YES                                         
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     OPTAL30                                                          
*                                                                               
         L     R3,ATWA                                                          
         L     R3,TWAMASTC-TWATASK(R3)     POINT TO MASTC                       
         ICM   R3,15,MCUTL-MASTD(R3)       POINT TO UTL                         
         ST    R3,NBUTL                                                         
         BNZ   *+6                                                              
         DC    H'00'                                                            
         MVC   4(1,R3),3(R2)       MOVE TALENT SYS NUMBER                       
*                                                                               
         LA    R1,SVTALSYS         MAKE A NOTE IN OPEN FILES LIST               
         LA    R0,8                                                             
*                                                                               
OPTAL32  CLI   0(R1),0                                                          
         BE    OPTAL34                                                          
OPTAL33  CLC   0(1,R1),3(R2)       TEST ALREADY OPEN                            
         BE    OPTAL36                                                          
         LA    R1,1(R1)                                                         
         B     OPTAL33                                                          
*                                                                               
OPTAL34  MVC   0(1,R1),3(R2)     SAVE THIS SYSTEM NUMBER                        
         L     R4,AIO3           WORK AREA FOR OPEN                             
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'TALENT',TALLIST,(R4)                    
*                                                                               
OPTAL36  XC    FILENAME,FILENAME                                                
         MVI   DATADISP+1,40                                                    
         MVC   LKEY,=H'32'                                                      
         MVC   LSTATUS,=H'2'                                                    
         MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
*                                                                               
         XIT1                                                                   
*                                                                               
TALLIST  DC    CL8'UTALFIL'                                                     
         DC    CL8'UTALDIR'                                                     
         DC    CL8'UTALRCV'                                                     
         DC    CL10'X'                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
* ROUTINE SWITCHES BACK TO NET FILE                                             
*=====================================================================          
SWNET    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,NBUTL                                                         
         MVC   4(1,R3),SVNETSYS    RESET UTL+4 WITH SYSTEM NUMBER               
*                                                                               
         XC    FILENAME,FILENAME                                                
         MVI   LKEY+1,13                                                        
         MVC   LSTATUS,=H'1'                                                    
         MVI   DATADISP+1,24                                                    
         MVC   SYSDIR,=CL8'UNTDIR'                                              
         MVC   SYSFIL,=CL8'UNTFIL'                                              
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*======================================================================         
* - GET NTI CODE FROM MASTER RECORD, STORE STATION AND NTI IN TABLE             
* - SET NBNTISTA WITH THE CORRECT NTI CODE FROM THE MASTER RECORD               
* - BUG IN NETIO IS NOT RETURNING NBNTISTA CORRECTLY, SO THIS FIXES IT          
*======================================================================         
                                                                                
         USING TANPD,R3                                                         
GETNTI   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,STALIST                                                       
         USING STALISTD,R2                                                      
GNTI10   CLI   0(R2),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'00'          TABLE IS FULL                                     
         CLI   0(R2),0        NO MATCHES, NEED TO ADD TO TABLE                  
         BE    GNTI20                                                           
         CLC   NBACTNET,STACODE                                                 
         BE    GNTI50                                                           
         LA    R2,STALLNQ(R2)                                                   
         B     GNTI10                                                           
*                                                                               
GNTI20   NETGO NVSETSTA        SET UP FOR STATION FILE                          
*                                                                               
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         LA    R4,KEY                                                           
         USING STAREC,R4                                                        
         MVI   STAKTYPE,STAKTYPQ STATION/MASTER RECORD                          
         MVC   STAKMED,NBSELMED                                                 
         MVC   STAKCALL,NBACTNET                                                
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,NBSELAGY  AGENCY                                         
         MVC   STAKCLT(6),=6C'0'                                                
*                                                                               
         L     R4,AMYIO                                                         
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,(R4)                     
         CLC   KEY(15),0(R4)                                                    
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         LA    R2,STALIST                                                       
GNTI30   CLI   0(R2),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'00'               TABLE IS FULL                                
         CLI   0(R2),0                                                          
         BE    GNTI40                                                           
         LA    R2,STALLNQ(R2)                                                   
         B     GNTI30                                                           
GNTI40   MVC   STACODE,NBACTNET    STATION                                      
         MVC   STATAL,STALTYP      TALENT MEDIA TYPE                            
         CLC   SNTISTA,SPACES      DON'T SAVE NTI IF ONLY SPACES                
         BNH   *+10                                                             
         MVC   STANTI,SNTISTA      NTI STATION CODE                             
*                                                                               
GNTI50   MVC   NBNTISTA,STANTI     PUT NTI CODE IN NBNTISTA                     
         MVC   NBSTATYP,STATAL     PUT TALENT MEDIA TYPE IN NBSTATYP            
*                                                                               
         XIT1                                                                   
         DROP  R3,R4                                                            
         LTORG                                                                  
         EJECT                                                                  
         DC    C'**NTIS**'                                                      
STALIST  DC    10000X'00'                                                       
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*======================================================================         
* - GET CABLE MARKET NAME FROM MARKET RECORD. STORE NAME IN TABLE               
*======================================================================         
         USING TANPD,R3                                                         
GETMKTN  NTR1  BASE=*,LABEL=*                                                   
         XC    SVCNET,SVCNET                                                    
         LA    R2,MKTNLIST                                                      
         USING MKTND,R2                                                         
GMKTN10  CLI   0(R2),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'00'          TABLE IS FULL                                     
         CLI   0(R2),0        NO MATCHES, NEED TO ADD TO TABLE                  
         BE    GMKTN20                                                          
         CLC   NBACTNET,MKTNCOD                                                 
         BE    GMKTN50                                                          
         LA    R2,MKTNLNQ(R2)                                                   
         B     GMKTN10                                                          
*                                                                               
GMKTN20  NETGO NVSETSTA        SET UP FOR STATION FILE                          
*                                                                               
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         LA    R4,KEY                                                           
         USING STAREC,R4                                                        
         MVI   STAKTYPE,STAKTYPQ STATION/MASTER RECORD                          
         MVC   STAKMED,NBSELMED                                                 
         MVC   STAKCALL,NBACTNET                                                
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,NBSELAGY  AGENCY                                         
         MVC   STAKCLT(6),=6C'0'                                                
*                                                                               
         L     R4,AMYIO                                                         
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,(R4)                     
         CLC   KEY(15),0(R4)                                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   FULL,SMKT                                                        
         MVC   SVCNET,SLSTCNET                                                  
         DROP  R4                                                               
*                                                                               
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         LA    R4,KEY                                                           
         USING MKTREC,R4                                                        
         MVI   MKTKTYPE,MKTKTYPQ MARKET RECORD                                  
         MVC   MKTKMED,NBSELMED                                                 
         MVC   MKTKMKT,FULL      MARKET CODE                                    
         MVC   MKTKAGY,NBSELAGY  AGENCY                                         
         MVC   MKTKFILL,=7C'0'                                                  
*                                                                               
         L     R4,AMYIO                                                         
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,(R4)                     
         CLC   KEY(15),0(R4)                                                    
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         LA    R2,MKTNLIST                                                      
GMKTN30  CLI   0(R2),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'00'               TABLE IS FULL                                
         CLI   0(R2),0                                                          
         BE    GMKTN40                                                          
         LA    R2,MKTNLNQ(R2)                                                   
         B     GMKTN30                                                          
GMKTN40  MVC   MKTNCOD,NBACTNET    STATION                                      
         MVC   MKTNNAM,MKTNAME     MARKET NAME                                  
         OC    MKTNNAM,SPACES                                                   
         MVC   MKTCNET,SVCNET      CNET CODE                                    
*                                                                               
GMKTN50  MVC   TANPMKTN,MKTNNAM    PUT MARKET NAME IN TANP ELEMENT              
         MVC   TANPNTI,MKTCNET     PUT CNET NAME IN TANP ELEMENT                
*                                                                               
GMKTNX   XIT1                                                                   
         DROP  R3,R4                                                            
         LTORG                                                                  
         EJECT                                                                  
         DC    C'**MKTN**'                                                      
MKTNLIST DC    10000X'00'                                                       
         DC    X'FF'                                                            
         LTORG                                                                  
*                                                                               
REQCARD  DC    CL80' '                                                          
         ORG   REQCARD                                                          
         DC    C'EQXX 123456 '                                                  
         DC    C'0204ERMQ 0303REP 05**OV,DDS 09**NE1 '                          
REQCDID  DC    C'1008ID=12345*'                                                 
         ORG                                                                    
*                                                                               
TR21REQ  DCB   DDNAME=TR21REQ,DSORG=PS,RECFM=FB,BLKSIZE=1600,          X        
               LRECL=80,MACRF=PM                                                
         EJECT                                                                  
*  DSECT TO COVER MKTNLIST                                                      
MKTND    DSECT                                                                  
MKTNCOD  DS    CL4                                                              
MKTCNET  DS    CL4                                                              
MKTNNAM  DS    CL15                                                             
MKTNLNQ  EQU   *-MKTND                                                          
         EJECT                                                                  
*        DSECT TO COVER STALIST                                                 
STALISTD DSECT                                                                  
STACODE  DS    CL4                                                              
STANTI   DS    CL4                                                              
STATAL   DS    CL1                 TALENT MEDIA TYPE                            
STALLNQ  EQU   *-STALISTD                                                       
         EJECT                                                                  
*                                                                               
WORKD    DSECT                                                                  
         DS    0D                                                               
MYDMWRK  DS    CL96                                                             
COUNTER  DS    F                                                                
AMYIO    DS    A                                                                
ASRTREC  DS    A                                                                
ACOVCML  DS    A                 X'60' ELEM ADDRESS IN COVER CMML               
ADTRPACK DS    A                 TRPACK                                         
AWRKBUFF DS    A                 A(WORKER FILE BUFFER)                          
PRNTBL   DS    A                 A(PRNTBL)                                      
DTADSPSV DS    CL2                                                              
REQSTDAT DS    CL2               REQUESTED START DATE                           
PRIORUNT DS    CL1               UNIT PRIOR TO REQUET DATE                      
SENTCHG  DS    CL1               1=UNIT ALREADY SENT/LATER CMML CHANGED         
DOCABLE  DS    CL1                 CABLE OPTION                                 
MULTIRUN DS    XL1               MULTIPLE RUN UNIT                              
NEWMULTI DS    XL1               IF NOT=0 THIS OVERRIDES MULTIRUN               
*                                ONLY FILLED IN IF UNIT SENT AND                
*                                THEN MULTIRUN FLAG CHANGED                     
*                                                                               
ADIDFLAG DS    XL1               ADID FLAG FROM NUCMADFL                        
SVNETSYS DS    X                 SYSNUM OF NETWORK SYSTEM OPENED                
SVTALUPD DS    X                 SYSNUM OF TAL SYSTEM OPEN FOR UPDATE           
         DS    0D                                                               
SVTALSYS DS    XL8                                                              
MYELEM   DS    CL100                                                            
MYWORK   DS    CL400                                                            
*                                                                               
         ORG   MYWORK                                                           
WRKNM    DS    CL4                 WRK FILE SYS/PROGRAM/SUB-PROGRAM             
INDEX    DS    CL42                WORKER FILE INDEX                            
COMMAND2 DS    CL8                 WORKER FILE COMMAND                          
WRKFILEN DS    CL8                 WORKER FILE FILE                             
RECORD   DS    CL150                                                            
         ORG                                                                    
*                                                                               
MYKEY    DS    CL40                                                             
MYKEYSV  DS    CL40                                                             
COMAND   DS    CL8                                                              
FILE     DS    CL8                                                              
ID       DS    CL16                                                             
IOWRKR   DS    CL300                                                            
         ORG   IOWRKR                                                           
IOWLEN   DS    CL4                 LENGTH OF WORKER RECORD                      
IOWDATA  DS    CL296               DATA                                         
IOWRKND  DS    CL2                 SET TO ZERO                                  
*                                                                               
POLTAGY  DS    CL6                                                              
TALAGY   DS    CL6                                                              
TALCMID  DS    CL8                                                              
TALFEED  DS    CL4                                                              
TALLNTH  DS    CL1                                                              
TALDATE  DS    CL3                                                              
TALADID  DS    CL12                                                             
TITLEN   DS    CL1                                                              
*                                                                               
SRTTYP   DS    CL1                 C'T'=COMMERCIAL TITLE SORT                   
UPDATE   DS    CL1                                                              
WRITEMC  DS    CL1                 SET FROM DDMASTC                             
MARKED   DS    CL1                 COMMERCIAL FILTER=ALL,UN/MARKED              
MARKDAT  DS    XL2                 COMMERCIAL FILTER DATE MARKED                
MARKAGY  DS    CL6                 COMMERCIAL FILTER MARKE TALENT AGY           
COMPDAT  DS    XL2                 TODAY'S COMPRESSED DATE                      
PRDCD    DS    CL3                                                              
UNMARK   DS    CL1                 UNMARK FLAG                                  
TODAYB   DS    XL3                                                              
TODAYP   DS    XL3                 TODAY PACKED                                 
*                                                                               
CLTSV    DS    CL24                                                             
TITLSV   DS    CL40                                                             
UNDERS   DS    CL40                                                             
IDSV     DS    CL12                                                             
SECSV    DS    CL1                                                              
NAMESV   DS    CL100                                                            
DATSV    DS    CL3                                                              
PROGSV   DS    CL15                                                             
NETSV    DS    CL4                                                              
PRDSV    DS    CL3                                                              
PRDSV1   DS    CL1                                                              
PRDSV3   DS    CL1                                                              
COMMFLT  DS    CL8                                                              
SAMPRDSV DS    CL6                                                              
PRODNAME DS    CL20                                                             
NET4CL   DS    CL4                                                              
DPMFLAG  DS    CL1                 Y=DELETED OR PREEMPTED OR MISSED             
SVNUSER  DS    CL1                 NETTAL USER Y/N/1  - FROM PROFILE            
SVAYSTA5 DS    CL1                 SAVED AGENCY STATUS 5                        
SVCNET   DS    CL4                 SAVED CNET CODE FROM MASTER RECORD           
PCKFLAG  DS    CL1                 Y=COMMERCIAL IS PACKED                       
*                                                                               
SVOPTSW  DS    XL1                                                              
OPTRACE  EQU   X'80'               TRACE                                        
*                                                                               
SVMEDFLT DS    XL1                 SAVED MEDIA FILTER                           
WRKRFLAG DS    XL1                                                              
WRKROPEN EQU   X'80'               WORKER FILE HAS BEEN OPENED                  
MYREQST  DS    CL6                 MY SAVED REQUEST START DATE                  
MYSVSTR  DS    CL6                 MY SAVED START DATE                          
MYSVSTR2 DS    CL2                 MY SAVED START DATE                          
WRKLENE  EQU   *-WORKD              * 1500 = MAX ROOM FOR STORAGE               
*                                                                               
         EJECT                                                                  
*                                                                               
PLINE    DSECT              PRINT LINE DSECT                                    
         DS    CL6                                                              
PL1      DS    CL2                                                              
PLPRD    DS    CL24                PRODUCT                                      
PL2      DS    CL2                                                              
PLCOMID  DS    CL12                COMMERCIAL ID                                
         DS    CL2                                                              
SPARE    DS    CL8                                                              
         DS    CL2                                                              
PLFEED   DS    CL4                 FEED CODE                                    
PL3      DS    CL2                                                              
PLSEC    DS    CL3                 LENGTH                                       
PL4      DS    CL2                                                              
PLCMTITL DS    CL27                COMMERCIAL TITLE                             
         ORG   PLCOMID                                                          
*                            ALTERNATE SORT PRINTING                            
PLTITL2  DS    CL27                COMMERCIAL TITLE                             
PL32     DS    CL2                                                              
PLSEC2   DS    CL3                 LENGTH                                       
PL42     DS    CL2                                                              
PLID2    DS    CL12                COMMERCIAL ID                                
         DS    CL2                                                              
SPARE2   DS    CL8                                                              
*                                                                               
PL5      DS    CL2                                                              
PLDATE   DS    CL8                 DATE                                         
PLDATNM  DS    CL4                 DATNUM                                       
PL6      DS    CL2                                                              
PLPROG   DS    CL15                PROGRAM                                      
PL7      DS    CL2                                                              
PLNET    DS    CL4                 NETWORK                                      
         DS    CL1                 NETWORK                                      
PLTYP    DS    CL1                                                              
PLEND    DS    CL1                                                              
*                                                                               
*                                                                               
*                                                                               
SORTD    DSECT                                                                  
SORTREC  DS    0CL500                                                           
SCLI     DS    CL24                CLIENT                                       
SPRD     DS    CL3                 PROD                                         
SCOM     DS    CL72                COMMERCIAL ID OR NAME (3X24)                 
SDSTASRT DS    CL1                 STA TYPE SORT (N=1,C=2,S=3,OTHER=4)          
SDSTATYP DS    CL1                 STATION TYPE (MEDIA)                         
SDNTWK   DS    CL4                                                              
SAMPRD   DS    CL6                 A/M,CLI,PRD                                  
SAMDAT   DS    CL2                 NBACTDAT                                     
SAMDATNM DS    CL1                 NBACTSUB                                     
         DS    CL1                 SPARE                                        
SDATA    DS    CL300                                                            
SDMULTI  DS    CL1                 MULTI RUN                                    
SDFEED   DS    CL4                                                              
SDADID   DS    CL12                                                             
SDCOMID  DS    CL8                 COMM ID (PACKED IF ADID)                     
         DS    CL60                SPARE                                        
*                                                                               
*                                                                               
*                                                                               
MYDSND   DSECT                                                                  
         DS    CL12                                                             
MYDSNTST DS    CL1                 MYDSN+8,1                                    
MYDSNAGY DS    CL2                 MYDSN+9,2                                    
         DS    CL2                                                              
MYDSNDAT DS    CL6                 MYDSN+13,6                                   
         DS    CL2                                                              
MYDSNT1  DS    CL4                 MYDSN+21,4                                   
         DS    CL2                                                              
MYDSNT2  DS    CL4                 MYDSN+27,4                                   
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NETINCLN                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SPTRCMML                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DMWRKRK                                                        
       ++INCLUDE DMWRKRD                                                        
       ++INCLUDE DDMASTD                                                        
SSBD   DSECT                                                                    
       ++INCLUDE FASSBOFF                                                       
         EJECT                                                                  
         PRINT ON                                                               
* RFP INCLUDES                                                                  
       ++INCLUDE GEGENRFPD                                                      
       ++INCLUDE NEDDEQUS                                                       
*                                                                               
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIDCD                                                       
TALTITLE DS    CL100                                                            
*                                                                               
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE DDGETRETD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'117NEWRI65   12/12/19'                                      
         END                                                                    
