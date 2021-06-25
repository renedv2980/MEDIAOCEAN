*          DATA SET SRNTQ00    AT LEVEL 001 AS OF 07/13/15                      
*PHASE T18200A                                                                  
*INCLUDE CLPACK                                                                 
         TITLE 'SRNTQ00 - NET/MQ INTERFACE - INITIALIZATION'                    
***********************************************************************         
*        INITIALIZATION - COPY MQ MESSAGE TO TWA                      *         
***********************************************************************         
SRNTQ00  CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKL,**$NMQ**,CLEAR=YES,RR=RE                                   
         USING WORKD,RC            RC=A(W/S)                                    
         ST    RE,RELO                                                          
         ST    RD,SYSRD            SAVE INCOMING RD                             
         MVC   SRPARMS,0(R1)                                                    
*                                                                               
         USING SRPARMD,R1                                                       
         L     R9,SRQAUTL                                                       
         USING UTLD,R9             R9=A(UTL)                                    
         ST    R9,AUTL                                                          
*                                                                               
         L     RA,SRPATWA          RA=A(TWA)                                    
         USING TWAD,RA                                                          
*                                                                               
*        FIND VSSB FOR TRACING OPTION                                           
*                                                                               
         L     RF,SRQASYSF                                                      
         USING SYSFACD,RF                                                       
         ST    RF,ASYSFACS                                                      
*                                                                               
         L     RE,VSSB                                                          
         ST    RE,ASSB                                                          
*                                                                               
         USING SSBD,RE                                                          
         MVC   SVMQION,SSBMQION    TRACE OPTION                                 
         DROP  RF                                                               
*                                                                               
*        GET TBUFFER ADDRESS                                                    
*                                                                               
         L     R1,SRPAUTL                                                       
         ICM   R9,15,TBUFF-UTLD(R1)                                             
         ST    R9,ATBUFF           SAVE TBUFF ADDRESS                           
*                                                                               
         MVC   BYTE,TSYS-UTLD(R1)                                               
*                                                                               
         LR    R1,R9               GET TBUFF LENGTH                             
         SHI   R1,2                                                             
         SR    RF,RF                                                            
         LH    RF,0(R1)            TBUFF LENGTH                                 
*                                                                               
         LR    R1,R9               A(TBUFF)                                     
         AR    R1,RF               ==> END OF BUFFER                            
         MVC   0(32,R1),=32X'00'   SET NULL FIELD AT END OF BUFFER              
*                                                                               
         L     RF,=V(CLPACK)                                                    
         A     RF,RELO                                                          
         ST    RF,VCLPACK                                                       
*                                                                               
         BRAS  RE,SYSINIT          INITIALZE                                    
*                                                                               
***********************************************************************         
*        ANALIZE MQ CALL                                              *         
***********************************************************************         
         SPACE 2                                                                
MQCALL   DS    0H                                                               
         USING EZMQMSGD,R9         ESTABLISH MQ REPLY                           
*                                                                               
         CLC   SVMQION,=F'2'       SKIP IF NO TRACING                           
         BNE   MQTR1                                                            
*                                                                               
         GOTOR VDMGR,DMCB,=C'OPMSG',(70,EZMQMSGD)                               
         GOTOR VDMGR,DMCB,=C'OPMSG',(70,EZMQMSGD+70)                            
*                                                                               
MQTR1    DS    0H                                                               
*                                                                               
         BRAS  RE,NETMQ               GO HANDLE                                 
*                                                                               
MQCALLX  B     EXIT                                                             
EXIT     XIT1                                                                   
         LTORG                                                                  
***********************************************************************         
*        HANDLE MQ MESSAGES FROM NET SERVER                           *         
***********************************************************************         
NETMQ    NTR1  BASE=*,LABEL=*                                                   
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
         USING EZMQMSGD,R9         ESTABLISH MQ REPLY                           
*                                                                               
         MVI   QTYP,WIOKRCDQ       ASSUME EIO MESSAGE                           
*                                                                               
         USING NTMQD,R9            ESTABLISH NET MQ DATA                        
         MVC   QAGY,NTMQAGY        SAVE AGENCY ALPHA                            
*                                                                               
         GOTOR SETDMGR             SET DATAMANAGER TO CORRECT FILES             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH AGENCY RECORD KEY                  
         USING AGYHDRD,R4                                                       
*                                                                               
         MVI   AGYKTYPE,AGYKTYPQ                                                
         MVC   AGYKAGY,QAGY        MOVE IN AGENCY                               
*                                                                               
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
         GOTOR VDMGR,DMCB,=CL8'DMRDHI',=C'SPTDIR ',KEY,KEY,DMWORK               
         CLC   KEY(13),KEYSAVE     RECORD MUST BE FOUND                         
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         GOTOR VDMGR,DMCB,=CL8'GETREC',=CL8'SPTFILE',KEY+14,AIO1,DMWORK         
         CLI   8(R1),0             CHECK FOR ERRORS                             
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R4,AIO1                                                          
         LA    R4,AGYEL            1ST ELEMENT                                  
*                                                                               
NT20     DS    0H                                                               
         CLI   0(R4),0             END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         CLI   0(R4),X'02'         X'02' ELEMENT?                               
         BNE   NT30                                                             
         CLI   2(R4),C'N'                                                       
         BNE   NT30                                                             
*                                                                               
         MVC   AGYHEX,3(R4)        HEX AGENCY CODE                              
         B     NT40                                                             
*                                                                               
NT30     ZIC   RF,1(R4)                                                         
         AR    R4,RF                                                            
         B     NT20                                                             
         DROP  R4                                                               
*                                                                               
NT40     LA    RE,3                GET LENGTH OF # OF INPUTS                    
         CLI   NTMQNUM,X'40'                                                    
         BH    NT50                                                             
         SHI   RE,1                                                             
         CLI   NTMQNUM+1,X'40'                                                  
         BH    NT50                                                             
         SHI   RE,1                                                             
         CLI   NTMQNUM+2,X'40'                                                  
         BH    *+6                                                              
         DC    H'00'                                                            
*                                                                               
NT50     DS    0H                                                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,NTMQNUM                                                      
         CVB   RE,DUB                                                           
         STC   RE,NUMCLTS                                                       
*                                                                               
         LA    R3,NTMQDATA         POINT TO DATA BEING PASSED BY NETMQ          
         ZIC   R5,NUMCLTS                                                       
*                                                                               
NTLOOP   DS    0H                                                               
         USING DATAD,R3            ESTABLISH INCOMING DATA                      
         MVI   DATASTAT,C'Y'       DEFAULT TO YES                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH AGENCY RECORD KEY                  
         USING CLTHDRD,R4                                                       
*                                                                               
         MVC   CKEYAM,AGYHEX                                                    
         GOTO1 VCLPACK,DMCB,DATACLI,CKEYCLT                                     
*                                                                               
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
         GOTOR VDMGR,DMCB,=CL8'DMRDHI',=C'SPTDIR ',KEY,KEY,DMWORK               
         CLC   KEY(13),KEYSAVE     RECORD MUST BE FOUND                         
         BE    NTIDCONT                                                         
*                                                                               
         MVI   DATASTAT,C'N'       CLIENT FAILED                                
*                                                                               
NTIDCONT DS    0H                                                               
         LA    R3,DATAENLN(R3)     BUMP TO NEXT ENTRY                           
         BCT   R5,NTLOOP                                                        
*                                                                               
NTIDDONE DS    0H                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   VMQIO,CMQIO                                                      
         DROP  RF                                                               
*                                                                               
         ZIC   R5,NUMCLTS          # OF ENTRIES                                 
         MH    R5,=H'4'            4 BYTES PER ENTRY                            
         AHI   R5,L'NTMQAGY        AGENCY ALPHA                                 
         AHI   R5,L'NTMQNUM        # OF ENTRIES                                 
         AHI   R5,L'NTMQIO#        OUTPUT QUEUE ID                              
*                                                                               
         LA    R4,NTMQIO#          OUTPUT AREA                                  
         GOTOR VMQIO,DMCB,=CL8'PUT',(R4),(R5),0,0,DUB                           
*                                                                               
NTMQX    DS    0H                                                               
         J     EXIT                                                             
         LTORG                                                                  
         DROP                                                                   
***********************************************************************         
*        INITIALIZE SYSTEM DEPENDENT VARIABLES                        *         
***********************************************************************         
SYSINIT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
*        SET UP ADDRESSES TO INCLUDED MODULES                                   
*                                                                               
         LA    R2,SYSV             ADDRS OF  INCLUDED ROUTINES                  
         LA    R3,SYSVCON          VCONS OF  INCLUDED ROUTINES                  
         LA    R4,NVTYPES          NUMBER OF INCLUDED ROUTINES                  
         LTR   R4,R4               SKIP IF NONE                                 
         BZ    SINVCOND                                                         
*                                                                               
SINVCONL DS    0H                                                               
         L     R1,0(R3)            GET V(INCLUDED ROUTINE)                      
         A     R1,RELO             RELOCATE ADDRESS                             
         ST    R1,0(R2)            SAVE ADDRESS                                 
*                                                                               
SINVCONC DS    0H                                                               
         LA    R2,4(R2)            BUMP TO NEXT ADDRESS STORAGE                 
         LA    R3,4(R3)            BUMP TO NEXT V-CON                           
         BCT   R4,SINVCONC         LOOP THROUGH LISTS                           
*                                                                               
SINVCOND DS    0H                                                               
         LA    R2,VCOMMON          COMMON ENTRY POINT                           
         SR    R3,R3               INIT ROUTINE ID                              
         LA    R4,SYSCOMM          START OF ADDRESS AVEAREA                     
         LA    R5,VCOUNT           SET NUMBER OF ROUTINES                       
*                                                                               
SINCOMLP DS    0H                                                               
         ST    R2,0(R4)            SET ENTRY POINT                              
         STC   R3,0(R4)            SET ROUTINE ID                               
*                                                                               
SINCOMCN DS    0H                                                               
         LA    R3,4(R3)            BUMP ROUTINE ID                              
         LA    R4,4(R4)            BUMP TO NEXT ADDRESS SAVEAREA                
         BCT   R5,SINCOMLP         NEXT ROUTINER5,                              
*                                                                               
SINCOMDN DS    0H                                                               
         XC    DMCB,DMCB                                                        
*                                                                               
         L     R1,SRPACOM          SAVE ADDRESS                                 
         USING COMFACSD,R1                                                      
*                                                                               
         ST    R1,ACOMFACS         SAVE COMFACS  ADDRESS                        
         MVC   VRECUP,CRECUP       SAVE RECUP    ADDRESS                        
         MVC   VDMGR,CDATAMGR      SAVE DATA MGR ADDRESS                        
         MVC   VPERVAL,CPERVAL     SAVE PERVAL   ADDRESS                        
         MVC   VDATCON,CDATCON     SAVE DATCON   ADDRESS                        
         MVC   VSWITCH,CSWITCH     SAVE SWITCH   ADDRESS                        
         MVC   AJESMAIL,CJESMAIL   SAVE JESMAIL  ADDRESS                        
         MVC   VGETTXT,CGETTXT     SAVE GETTXT   ADDRESS                        
*                                                                               
         L     RF,CCALLOV          A(CALLOV)                                    
         DROP  R1                                                               
*                                                                               
SINCORDN DS    0H                                                               
         LHI   RF,IOAREA1-WORKD   SET IOAREA ADDRESSES                          
         AR    RF,RC                                                            
         ST    RF,AIO1                                                          
         AHI   RF,4096                                                          
         ST    RF,AIO2                                                          
         AHI   RF,4096                                                          
         ST    RF,AIO3                                                          
*                                                                               
         MVC   AIO,AIO1            SET DEFAULT IOAREA ADDRESS                   
*                                                                               
         MVC   QAGY,TWAAGY         SAVE AGENCY APLPHA                           
*                                                                               
         MVI   SPACES,C' '         FILL IN SPACES FIELD                         
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
SYSINITX DS    0H                                                               
         J     EXIT                                                             
         LTORG                                                                  
         DROP                                                                   
***********************************************************************         
*        CONSTANTS TABLES, ETC                                        *         
***********************************************************************         
SYSVCON  DS    0F                  INCLUDED MODULES                             
*                                                                               
NVTYPES  EQU   (*-SYSVCON)/4       NUMBER OF INCLUDED MODULES                   
*                                                                               
***********************************************************************         
*        COMMON ENTRY POINT FOR GENERAL SYSTEM ROUTINES               *         
***********************************************************************         
VCOMMON  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
*                                                                               
         SRL   RF,24               SHIFT ROUTINE ID TO RIGHT NYBBLE             
         L     RF,VBRANCH(RF)      GET A(ROUTINE)                               
         A     RF,RELO             RELOCATE ADDRESS                             
*                                                                               
         BASR  RE,RF               GO TO ROUTINE                                
*                                                                               
VCOMMONX DS    0H                                                               
         J     EXIT                RETURN TO CALLER                             
*                                                                               
*        COMMON ROUTINE ADDRESSES                                               
*                                                                               
VBRANCH  DS    0D                  ALIGNMENT                                    
         DC    A(VSETDMGR)         SET DATAMANAGER FILES                        
*                                                                               
VCOUNT   EQU   (*-VBRANCH)/4                                                    
*                                                                               
         LTORG                                                                  
         DROP                                                                   
***********************************************************************         
*        FIND SE NUMBER AND SWITCH TO THOSE FILES FOR DMGR CALLS      *         
*ENTRY   QAGY   =  2 CH ALPHA FOR AGENCY                              *         
*EXIT    PROGRAM HAS BEEN SWITCHED TO READ CORRECT PRINT FILES        *         
***********************************************************************         
VSETDMGR NTR1  BASE=*,LABEL=*                                                   
         USING TWAD,RA             ESTABLISH TWA                                
         USING WORKD,RC            ESTABLISH WORKING STORAGE                    
         USING NTMQD,R9            ESTABLISH NET MQ DATA                        
*                                                                               
*        READ SYSTEM ACCESS RECORD FOR PASSED AGENCY                            
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH CT5REC (SYSTEM ACCESS REC)         
         USING CT5REC,R4                                                        
*                                                                               
         MVI   CT5KTYP,CT5KTYPQ    RECORD ID                                    
         MVC   CT5KALPH,QAGY       SET AGENCY CODE                              
*                                                                               
         GOTO1 VDMGR,DMCB,=CL8'DMREAD',=CL8'CTFILE',KEY,AIO1,0                  
         CLI   8(R1),0             NO ERRORS ALLOWED                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO1             POINT TO FOUND RECORD                        
*                                                                               
         LA    R6,CT5DATA          POINT TO FIRST ELEMENT IN RECORD             
         USING CTSYSD,R6           ESTABLISH SYS AUTH ELEMENT                   
         SR    R0,R0                                                            
*                                                                               
CTAUTHLP DS    0H                                                               
         CLI   CTSYSEL,0           MUST FIND AUTH ELEMENT                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   CTSYSEL,CTSYSELQ    FIND SYSTEM AUTH ELEMENT                     
         BNE   CTAUTHCN                                                         
*                                                                               
         CLC   CTSYSNUM,BYTE       MATCH ON SYSTEM                              
         BE    CTAUTHFD                                                         
*                                                                               
CTAUTHCN DS    0H                                                               
         IC    R0,CTSYSLEN         BUMP TO NEXT ELEMENT                         
         AR    R6,R0                                                            
         B     CTAUTHLP                                                         
*                                                                               
CTAUTHFD DS    0H                                                               
         MVC   NETSE,CTSYSSE       SAVE PRINT SE NUMBER                         
*                                                                               
*        SWITCH TO NET FILE                                                     
*                                                                               
         MVC   DMCB(1),NETSE       SET FOR PRINT SYSTEM                         
         MVC   DMCB+1(3),=3X'FF'   INDICATE HARD SWITCH                         
*                                                                               
         GOTO1 VSWITCH,DMCB,,0     SWITCH TO NET SYSTEM                         
         CLI   4(R1),0                                                          
         BNE   SETDERR1               CAN'T SWITCH                              
*                                                                               
SETDMGRX DS    0H                                                               
         J     EXIT                                                             
*                                                                               
SETDERR1 DS    0H                  SYSTEM UNAVAILABLE                           
         MVC   PERROR,=X'FE01'     SET ERROR NUMBER                             
         DC    H'00'                                                            
         LTORG                                                                  
         DROP                                                                   
***********************************************************************         
*                                                                     *         
*        EDICT RETURNED MQ MESSAGE                                    *         
*                                                                     *         
***********************************************************************         
EZMQMSGD DSECT                     DATA RETURNED BY EDICT VIA MQ                
EZMLABEL DS    CL6                 PRTADB                                       
         DS    CL2                 SPARE                                        
EZMUSER  DS    CL10                REPORT ID - WIO,123                          
EZMSTAT  DS    CL1                 D FOR DELIVERY, C FOR CANCEL                 
EZMDATE  DS    CL5                 MMMDD                                        
EZMTIME  DS    CL4                 HHMM                                         
EZMAPPL  DS    CL58                APPLICATION INFO FROM TRN CARD               
EZMEMSG  DS    CL24                ERROR MESSAGE                                
EZDEST   DS    CL16                DESTINATION                                  
EZMQMSGL EQU   *-EZMQMSGD                                                       
*                                                                               
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
WORKD    DSECT                                                                  
BYTE     DS    XL1                                                              
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF2    DS    H                                                                
SYSRD    DS    F                   INCOMING RD SAVEAREA                         
*                                                                               
SRPARMS  DS    0XL32               SERVICE REQUEST PARAMETER LIST               
SRPASYS  DS    A                   A(SYSFACS)                                   
SRPATIA  DS    A                   A(TIA)                                       
SRPAUTL  DS    A                   A(UTL ENTRY)                                 
SRPACOM  DS    A                   A(COMFACS)                                   
SRPASEL  DS    A                   A(SELIST ENTRY)                              
SRPATWA  DS    A                   A(TWA)                                       
SRPAMAP  DS    A                   A(PHASE MAP)                                 
SRPATIOB DS    A                   A(TRANSLATOR I/O BLOCK)                      
*                                                                               
RELO     DS    A                                                                
*                                                                               
DMCB     DS    6F                  DATAMGR CONTROL BLOCK                        
DMWORK   DS    12D                 DATAMGR WORKAREA                             
*                                                                               
KEY      DS    XL32                KEY                                          
KEYSAVE  DS    XL32                                                             
*                                                                               
AIO      DS    A                   A(CURRENT IOAREA)                            
AIO1     DS    A                   A(IOAREA 1)                                  
AIO2     DS    A                   A(IOAREA 2)                                  
AIO3     DS    A                   A(IOAREA 3)                                  
*                                                                               
WORK     DS    XL256               WORKAREA                                     
*                                                                               
ELEMENT  DS    XL256               ELEMENT WORKAREA                             
*                                                                               
ELCODE   DS    XL1                 WORKAREA FOR GETEL                           
*                                                                               
ERROR    DS    XL1                 1-BYTE ERROR CODE                            
PERROR   DS    XL2                 2-BYTE ERROR CODE                            
*                                                                               
NUMCLTS  DS    XL1                 NUMBER OF CLIENTS                            
AGYHEX   DS    XL1                 HEX AGENCY                                   
CURCLT   DS    XL2                 CLIENT                                       
VCLPACK  DS    V                                                                
VMQIO    DS    V                                                                
*                                                                               
***********************************************************************         
*        NET IO SYSTEM WORKING STORAGE                                *         
***********************************************************************         
SYSV     DS    0D                  EXTERNAL ADDRESSES                           
VRECUP   DS    V                                                                
VDMGR    DS    V                                                                
VPERVAL  DS    V                                                                
VDATCON  DS    V                                                                
VSWITCH  DS    V                                                                
AJESMAIL DS    V                                                                
ACOMFACS DS    V                                                                
VGETTXT  DS    V                                                                
         DS    10V                 CAREFUL IF YOU CHANGE THIS                   
*                                                                               
SYSCOMM  DS    0A                  COMMON FACILITIES FOR SYSTEM                 
SETDMGR  DS    V                   SET DATAMGR TO PRINT FILES                   
         DS    71V                 CAREFUL IF YOU CHANGE THIS                   
*                                                                               
COREFACS DS    0A                  CORE-RESIDENT PHASES                         
VMINIO   DS    V                   MINIO                                        
VPUBVAL  DS    V                   PUBVAL                                       
VPUBEDIT DS    V                   PUBEDIT                                      
         DS    20V                 CAREFUL IF YOU CHANGE THIS                   
*                                                                               
ATBUFF   DS    A                   A(T BUFFER)                                  
ASYSFACS DS    A                                                                
ASSB     DS    A                                                                
AUTL     DS    A                                                                
*                                                                               
*        MINIO CONTROL BLOCK                                                    
*                                                                               
MNBLKCB  DS    XL(MINBLKL)         MINIO CONTROL BLOCK                          
         DS    0D                  ALIGNMENT                                    
MNRTAB   DS    CL256               MINIO RECORD TABLE                           
MNELEM   DS    CL256               MINIO ELEMENT AREA                           
*                                                                               
*                                                                               
         DS    0D                  ALIGNMENT                                    
WIOPARMS DS    XL24                PARAMETER LIST                               
GETTXTCB DS    XL(L'GTBLOCK)       GETTXT WORKAREA                              
*                                                                               
* FIELD VALIDATION STORAGE                                                      
*                                                                               
USEIONUM DS    X                   INDICATOR FOR WHICH AIO TO USE               
*                                                                               
NETSE    DS    XL1                 SE NUMBER FOR PRINT FILES                    
*                                                                               
SVMQION  DS    F                   TRACE SWITCH                                 
*                                                                               
         DS    CL45                SPARE                                        
*                                                                               
SPACES   DS    XL132                                                            
*                                                                               
NTTLEN   DS    PL4                 TOTAL LENGTH OF NETIO DATA                   
*                                                                               
* EXTRACT AREAS                                                                 
*                                                                               
*        THESE VALUES NEED TO BE MAINTAINED WITH THE CURRENT                    
*        VALUES                                                                 
*                                                                               
         DS    0D                  ALIGNMENT                                    
QAGY     DS    CL2                 AGENCY ALPHA                                 
QTYP     DS    XL1                 MINIO RECORD CODE FOR EIO OR ESR             
QMED     DS    CL1                 MEDIA CODE                                   
QCLT     DS    CL3                 CLIENT CODE                                  
QPRD     DS    CL3                 PRODUCT CODE                                 
QEST     DS    CL3                 ESTIMATE RANGE START                         
QESTEND  DS    CL3                 ESTIMATE RANGE END                           
QPUB     DS    XL6                 PUB NUMBER                                   
QDIV     DS    CL3                 DIVISION CODE                                
QREG     DS    CL3                 REGION CODE                                  
QDST     DS    CL3                 DISTRICT CODE                                
QREP     DS    CL4                 REP      CODE                                
QSTART   DS    CL6                 YYMMDD START DATE                            
QEND     DS    CL6                 YYMMDD END DATE                              
QPER     DS    XL6                 PERIOD START-END - BINARY                    
QRDATE   DS    XL3                 INSORD RUN DATE                              
QSTAT    DS    CL1                 STATUS                                       
QSTADATE DS    XL3                 STATUS DATE - BINARY                         
QSTATIME DS    XL3                 STATUS TIME - BINARY - HH:MM:SS              
QIPADDR  DS    CL16                IPADDRESS                                    
QUSERID  DS    CL10                SIGN ON USER ID                              
QPERSID  DS    CL10                PERSON ID                                    
QADCODE  DS    CL6                                                              
QIOKEY   DS    CL32                CURRENT MINIO MASTER KEY                     
*                                                                               
QIO#     DS    0XL4                WEB IO SERIAL NUMBER                         
QIO#IOYR DS    XL1                 WEB IO SERIAL NUMBER - YEAR                  
QIO#IOSQ DS    XL3                 WEB IO SERIAL NUMBER - SEQ #                 
*                                                                               
QREV#    DS    XL1                 WEB IO REVISION NUMBER                       
QFGP#    DS    XL1                 FAX GROUP NUMBER                             
QFSQN    DS    XL2                 FAX SEQUENCE NUMBER                          
*                                                                               
QIO#SHRT DS    CL16                IO# - SHORT FORM                             
*                                  MYYCCC###-###                                
*                                  CCC - CLIENT CODE                            
*                                                                               
QIO#EXP  DS    CL18                EXPANDED IO#                                 
*                                  M-YYCCC0000-REV000                           
*                                                                               
QSTEW    DS    XL1                 STEWARDSHIP OPTION                           
         DS    XL7                                                              
QDSQN    DS    XL2                 DETAIL SEQUENCE NUMBER                       
QDISK    DS    XL4                 DISK ADDR OF MINIO MASTER REC                
*                                                                               
QSSQN    DS    XL2                 STATUS SEQUENCE NUMBER                       
*                                                                               
QFXTYP   DS    XL1                 FAX TYPE - FYI?                              
*                                                                               
QGRPID   DS    XL12                GROUP ID                                     
         DS    XL35                SPARE                                        
*                                                                               
QACTCHGS DS    0XL6                CHANGES INDICATORS                           
QACTCHG1 DS    X                                                                
QACTADD  EQU   X'80'               IO HEADER ADDED                              
QACTDEL  EQU   X'40'               IO HEADER DELETED                            
QACTRST  EQU   X'20'               IO RESTORED                                  
QACTSTAT EQU   X'10'               TOTAL TYPE  G/N                              
QACTVCCL EQU   X'08'               VENDOR CONTACT LIST                          
QACTACCL EQU   X'04'               AGENCY CONTACT LIST                          
*                                                                               
QACTCHG2 DS    X                                                                
QACTCHG3 DS    X                                                                
QACTCHG4 DS    X                                                                
QACTCHG5 DS    X                                                                
QACTCHG6 DS    X                                                                
*                                                                               
         DS    XL48                SPARE                                        
*                                                                               
*        SCHEMA RECORD FIELDS                                                   
*                                                                               
         DS    0D                  ALIGNMENT                                    
QSCHEMA  DS    0X                  SCHEMA DATA                                  
QACCTM   DS    XL3                 ACCESS   TIMEOUT PERIOD                      
QACTTM   DS    XL3                 ACTIVATE TIMEOUT PERIOD                      
QPERTYP  DS    CL1                 IO PERIOD TYPE                               
QACTDT   DS    XL3                 ACTIVATION DATE - BINARY                     
QIO#DEP  DS    XL1                 IO # DEPENDENCIES                            
         DS    XL23                SPARE                                        
QSCHEMAL EQU   *-QSCHEMA           LENGTH OF SCHEMA DATA                        
*                                                                               
QRECORD  DS    XL8                 RECORD TYPE BEING PROCESSED                  
         DS    XL48                SPARE                                        
*                                                                               
MEDNM    DS    CL10                MEDIA NAME                                   
CLTNM    DS    CL20                CLIENT NAME                                  
PRDNM    DS    CL20                PRODUCT NAME                                 
ESTNM    DS    CL20                ESTIMATE NAME                                
PUBNM    DS    CL20                PUB NAME                                     
REPNM    DS    CL20                REP NAME                                     
DDCOMSW  DS    CL1                 C'Y' - COMMENTS TO BE ADDED                  
*                                  C'C' - COMMENTS TO BE ADDED NOW              
*                                  C'D' - COMMENTS ADDED                        
         DS    XL119               SPARE                                        
*                                                                               
SVSECAGY DS    XL2                 SECURITY AGENCY                              
SVWIOPID DS    XL2                 PERSONAL ID                                  
SVWIOID  DS    CL8                 PERSON   ID IN CHARACTER                     
*                                                                               
SVKEY    DS    XL32                USE WHEN BREAKING READ SEQUENCE              
SVPROF   DS    CL16                                                             
SVUSER   DS    CL66                SAVE AREA FOR AGYNAME/ADDR                   
*                                                                               
SVCPROF  DS    CL32                                                             
SVCLTOFC DS    CL1                 CLIENT OFFICE SAVEAREA                       
*                                                                               
SVAGYPF  DS    CL30                AGENCY PROFILE                               
SVACTELM DS    XL256               ACTIVITY ELEMENT BUILD AREA                  
SVACHGS  DS    0XL6                ACTIVITY ACCUMULATORS                        
SVACH1   DS    XL1                 ACTIVITY ACCUMULATOR 1                       
SVACH2   DS    XL1                 ACTIVITY ACCUMULATOR 2                       
SVACH3   DS    XL1                 ACTIVITY ACCUMULATOR 3                       
SVACH4   DS    XL1                 ACTIVITY ACCUMULATOR 4                       
SVACH5   DS    XL1                 ACTIVITY ACCUMULATOR 5                       
SVACH6   DS    XL1                 ACTIVITY ACCUMULATOR 6                       
*                                                                               
SVSTAELM DS    XL256               STATUS ELEMENT SAVEAREA                      
SVURLELM DS    0XL256              URL    ELEMENT SAVEAREA                      
SVFAXELM DS    XL256               FAX    ELEMENT SAVEAREA                      
*                                                                               
*        JESMAIL PARAMTER BLOCK                                                 
*                                                                               
SMTPC    DS    6A                  PARAMTER BLOCK FOR JES MAIL                  
*                                                                               
*        E-MAIL FIELDS                                                          
*                                                                               
EMLFLDS  DS    0D                                                               
EMLTOADR DS    CL60                TO: E-MAIL ADDRESS                           
EMLTOEND DS    XL1                 X'FF' END OF LIST                            
EMLSUBJ  DS    CL70                SUBJECT                                      
EMLMSG   DS    10CL80              MESSAGE                                      
         ORG   EMLMSG                                                           
EMLLIN1  DS    CL80                  LINE 1                                     
EMLLIN2  DS    CL80                  LINE 2                                     
EMLLIN3  DS    CL80                  LINE 3                                     
EMLLIN4  DS    CL80                  LINE 4                                     
EMLLIN5  DS    CL80                  LINE 5                                     
EMLLIN6  DS    CL80                  LINE 6                                     
EMLLIN7  DS    CL80                  LINE 7                                     
EMLLIN8  DS    CL80                  LINE 8                                     
EMLLIN9  DS    CL80                  LINE 9                                     
EMLLIN10 DS    CL80                  LINE 10                                    
         DS    XL1                 POSSIBLE EOL MARKER                          
DUMMY    DS    CL60                                                             
*                                                                               
         ORG                                                                    
SYSSPARE EQU   *                                                                
         DS    CL(6000-(*-WORKD))                                               
*                                                                               
IOAREA1  DS    XL4096              I/O AREAS                                    
IOAREA2  DS    XL4096                                                           
IOAREA3  DS    XL4096                                                           
*                                                                               
WORKL    EQU   *-WORKD                                                          
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - NTMQD'                           
***********************************************************************         
*                                                                     *         
*        LAYOUT OF DATA FROM NTMQ                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
NTMQD    DSECT                                                                  
NTMQ     DS    CL8'NETCLIST'       NETPAK IDENTIFIER                            
*                                                                               
NTMQIO#  DS    CL16                MQ OUTPUT QUEUE HEADER                       
NTMQAGY  DS    CL2                 AGENCY                                       
NTMQNUM  DS    CL3                 # OF ENTRIES                                 
NTMQDATA DS    0C                  START OF DATA                                
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - DATAD'                           
***********************************************************************         
*                                                                     *         
*        LAYOUT OF DATA FROM WEBIO                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DATAD    DSECT                                                                  
DATACLI  DS    CL3                 CLIENT                                       
DATASTAT DS    CL1                 'Y' - VALID CLIENT, 'N' - INVALID            
DATAENLN EQU   *-DATACLI                                                        
*                                                                               
         TITLE 'SRPMQ00 - PRINT/MQ INTERFACE - DSECTS'                          
***********************************************************************         
*                                                                     *         
*        VARIOUS DSECTS                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
** DDCOMFACS                                                                    
       ++INCLUDE DDCOMFACS                                                      
** PPEDICT                                                                      
       ++INCLUDE PPEDICT                                                        
** DDCOREQUS                                                                    
       ++INCLUDE DDCOREQUS                                                      
** PPERREQUS                                                                    
       ++INCLUDE PPERREQUS                                                      
** FAFACTS                                                                      
       ++INCLUDE FAFACTS                                                        
** DDGLOBEQUS                                                                   
       ++INCLUDE DDGLOBEQUS                                                     
** DDOFFICED                                                                    
       ++INCLUDE DDOFFICED                                                      
** PRGENFILE                                                                    
       ++INCLUDE PRGENFILE                                                      
** DDMINBLK                                                                     
       ++INCLUDE DDMINBLK                                                       
** FAGETTXTD                                                                    
       ++INCLUDE FAGETTXTD                                                      
** FASECRETD                                                                    
       ++INCLUDE FASECRETD                                                      
** FAJESMAILD                                                                   
       ++INCLUDE FAJESMAILD                                                     
** DDPERVALD                                                                    
       ++INCLUDE DDPERVALD                                                      
** PPMAPEQUS                                                                    
       ++INCLUDE PPMAPEQUS                                                      
** CTGENFILE                                                                    
       ++INCLUDE CTGENFILE                                                      
** SEACSFILE                                                                    
       ++INCLUDE SEACSFILE                                                      
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SRNTQ00   07/13/15'                                      
         END                                                                    
