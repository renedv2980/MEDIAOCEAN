*          DATA SET CTMAD23    AT LEVEL 012 AS OF 07/08/04                      
*PHASE TA0C23A,*                                                                
         TITLE 'TA0C23 - $MAD PRESTO ORDER RESERVATION'                         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* NOTE: IF YOU MAKE ANY CHANGES TO THIS PROGRAM, PLEASE CHECK IF THEY           
*       SHOULD GO IN PRESTO'S VERSION (ACPRF03)                                 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
TA0C23   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0C23,RA                                                      
*                                                                               
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
         USING USUKD,R8            US/UK APPLICATION COMMON STORAGE             
         USING OVERD,R9            OVERLAY SAVED STORAGE                        
*                                                                               
         L     RC,ACONTD           RC = A(CONTROLLER COMMON STORAGE)            
         L     R9,AOVER            R9 = A(OVERLAY SAVED STORAGE)                
*                                                                               
         ST    RD,SAVEDRD          SAVE STACK POINTER FOR RETURNING             
         EJECT                                                                  
* MAIN DRIVER - CALLS THE THREE SUBROUTINES THAT HANDLE EACH OF THE             
* THREE MODES THAT THE CONTROLLER CALLS AN OVERLAY WITH (START, MIDDLE,         
* AND END).                                                                     
*                                                                               
MAIN     DS    0H                                                               
         BAS   RE,INIT             INITIALIZE OVERLAY                           
*                                                                               
         CLI   OVERMODE,C'S'       IF MODE IS START                             
         BNE   M10                                                              
         BAS   RE,PROCSTRT         THEN CALL PROCSTRT                           
         B     MX                                                               
*                                                                               
M10      CLI   OVERMODE,C'M'       ELSE IF MODE IS MIDDLE                       
         BNE   M20                                                              
         BAS   RE,PROCMID          THEN CALL PROCMID                            
         B     MX                                                               
*                                                                               
M20      BAS   RE,PROCEND          ELSE CALL PROCEND                            
*                                                                               
MX       B     EXIT                                                             
         EJECT                                                                  
* THIS ROUTINE INITIALIZES VARIABLES IN THIS OVERLAY THAT NEED                  
* INITIALIZATION.                                                               
*                                                                               
INIT     NTR1  ,                                                                
*                                                                               
*        INITIALIZE LOCAL WORKING STORAGE                                       
*                                                                               
         GOTO1 SETSYS,DMCB,=C'ACCOUNT',=CL8'ACCDIR',=CL8'ACCMST'                
         BE    INIT10                                                           
* IF THERE WAS AN ERROR, CHECK IF ERROR SWITCHING SYSTEMS                       
* IF ERROR SWITCHING SYSTEMS, CHANGE TO FILE READ-ONLY                          
         CLC   MDACTION,=Y(ERSWITCH)                                            
         BNE   EXIT                                                             
         XC    MDACTION,MDACTION                                                
         B     ERRUPRO                                                          
*                                                                               
INIT10   MVI   LENKEY,L'ACCKEY                                                  
         MVI   DISPDSKA,ACCKDA-ACCRECD                                          
*                                  DON'T ALLOW UPLOAD TO READ ONLY FILE         
         GOTO1 GETFACT,DMCB,(X'80',BYTE),(SYSNUM,F#SEIND)                       
         TM    BYTE,SEIRONLY                                                    
         BO    ERRUPRO                                                          
*                                                                               
*        SET UP GLOBAL VALUES FOR THE COMPANY/UNIT/LEDGER.                      
*                                                                               
         L     RF,AUTL             FIND COMPANY IN UTL                          
         USING UTLD,RF                                                          
         MVC   CUL(1),TAGYB                                                     
         MVC   CTRY,TCTRY                                                       
         DROP  RF                                                               
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES+0                                    
*                                                                               
         LA    R6,BIGKEY                                                        
         USING CPYRECD,R6                                                       
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,CUL         READ COMPANY RECORD                          
         GOTO1 HIGH                                                             
         CLC   CPYKEY(CPYKEND),KEYSAVE                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
         LA    R3,CPYELQ                                                        
         GOTO1 GETELEM,DMCB,(R3),0                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CPYELD,R6                                                        
         MVC   CUL+1(2),CPYPROD                                                 
         DROP  R6                                                               
*                                                                               
INX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES THE START MODE.  EVERYTHING WILL HAPPEN HERE           
* BECAUSE WE NEED ONLY ONE TRANSACTION TO READ THE TEMPFILE AND WRITE           
* THE ORDER RESERVATION RECORD.                                                 
*                                                                               
PROCSTRT NTR1  ,                                                                
*                                                                               
         BAS   RE,VALREQ                                                        
         BAS   RE,BLDKEY           BUILD ORDER RESERVATION KEY                  
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         LA    R6,BIGKEY                                                        
         USING OBRRECD,R6                                                       
         CLC   OBRKEY(OBRKEND),KEYSAVE COMPARE SIGNIFICANT DATA                 
         BNE   PS05                                                             
*                                                                               
         GOTO1 GETREC              GET THE RESERVATION                          
         BAS   RE,TESTDUP          TEST FOR DUPLICATE REQUEST                   
         BE    PS25                SAME SERVER MAKING EXACT REQUEST-OK          
         B     ERRDUP              ANOTHER SERVER MAKING DUP REQUEST            
*                                                                               
PS05     GOTO1 BLDOKEY,CONTROL     READ ORDER CONTROL RECORD                    
         MVI   RDUPDATE,C'Y'       TO FORCE ORDER PROGRAM TO WAIT               
         GOTO1 READ                UNTIL THIS REQUEST COMPLETES                 
*                                                                               
         BAS   RE,BLDKEY                                                        
         XC    OBRKFRST,OBRKFRST   CLEAR FIRST NUMBER                           
         MVC   OBRKLAST,SVORDSTR   PLUG START NUMBER INTO LAST IN KEY           
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   OBRKEY(OBRKLAST-OBRKEY),KEYSAVE TEST SAME AGENCY                 
         BNE   PS10                NO-CAN'T HAVE OVERLAP                        
         CLC   OBRKFRST,SVORDEND   TEST IF FIRST > END OF REQUEST               
         BH    PS10                NO OVERLAP                                   
*                                                                               
         MVC   OVLPFRST,OBRKFRST   SAVE FIRST AND LAST NUMBER                   
         MVC   OVLPLAST,OBRKLAST   OF RESERVATION CAUSING OVERLAP               
         B     ERROVER                                                          
*                                                                               
PS10     GOTO1 BLDOKEY,SVORDSTR    BUILD AN ORDER KEY W/ REQUEST START          
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         LA    R6,BIGKEY                                                        
         USING ORDRECD,R6                                                       
         CLC   ORDKEY(ORDKORD-ORDKEY),KEYSAVE  TEST SAME COMPANY                
         BNE   PS20                NO-ITS OK, NO OVERLAP                        
*                                                                               
         CLC   ORDKORD,SVORDEND    TEST FIRST NUMBER VS. END                    
         BH    PS20                PAST END-ITS OK                              
         CLC   ORDKORD,SVORDSTR    IS ORDER RETURNED WITHIN RANGE               
         BL    PS20                                                             
*                                                                               
         MVC   ORDNUM,ORDKORD      SAVE ORDER NUMBER                            
         B     ERRORD                                                           
*                                                                               
PS20     BAS   RE,ADDORES          ADD ORDER RESERVATION                        
*                                                                               
PS25     GOTO1 PUTITEM,DMCB,A(ITEOD),0                                          
         BNE   EXIT                                                             
*                                                                               
         MVI   MDLAST,C'Y'                                                      
*                                                                               
PSX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES MIDDLE MODE.  IT DOES NOTHING HERE.                    
*                                                                               
PROCMID  NTR1                                                                   
*                                                                               
PMX      B     XIT                                                              
         SPACE 3                                                                
* THIS ROUTINE PROCESSES THE END MODE.  IT CURRENTLY DOES NOTHING.              
*                                                                               
PROCEND  NTR1                                                                   
*                                                                               
PEX      B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
* VALIDATE REQUEST, SET VARIBLES IN WORKING STORAGE                             
*---------------------------------------------------------------------          
*                                                                               
VALREQ   NTR1                                                                   
         GOTO1 GETITEM             GET FIRST ITEM - FILTER OBJECT               
         BNE   EXIT                                                             
*                                  MUST BE ORDER RESEVERATION REQUEST           
         CLC   TYPENUM,=A(ITPRORRQ)                                             
         BNE   ERRROBJ                                                          
*                                                                               
         L     R2,ADATA                                                         
         USING PROROBJD,R2                                                      
         CLC   PRORSTR,SPACES                                                   
         BNH   ERRFMT                                                           
         CLC   PROREND,SPACES                                                   
         BNH   ERRFMT                                                           
         MVC   SVPERSON,PRORPERS   SAVE PERSON                                  
         OC    SVPERSON,SPACES     AND SPACE PAD IT                             
*                                                                               
         MVC   SVSERVER,SPACES     GET ORIGINATING SQL SERVER                   
         MVC   SVSERVER(8),PRORSERV                                             
         L     RE,DATALEN                                                       
         CH    RE,=Y(PRORSERV-PROROBJD+8)  TEST FOR 8/16 BYTE SERVER ID         
         BNH   *+10                                                             
         MVC   SVSERVER,PRORSERV   ITS 16 BYTES                                 
         OC    SVSERVER,SPACES                                                  
*                                                                               
* MAKE SURE THE START AND END NUMBERS CONTAIN ALL 6 DIGITS AND                  
* THAT IF THE START NUMBER BEGINS WITH A LETTER, THE END NUMBER                 
* BEGINS WITH THE SAME ONE                                                      
*                                                                               
VALREQ10 LA    R0,L'PRORSTR                                                     
         LA    R1,PRORSTR          R1=A(START NUMBER)                           
         BAS   RE,CHKNUM                                                        
         BNE   ERRFMT                                                           
*                                                                               
         LA    R0,L'PROREND                                                     
         LA    R1,PROREND                                                       
         BAS   RE,CHKNUM                                                        
         BNE   ERRFMT                                                           
*                                                                               
VALREQ20 CLI   PRORSTR,C'0'                                                     
         BL    VALREQ25            START NUMBER BEGINS WITH A LETTER            
         CLI   PROREND,C'0'        START BEGINS WITH A NUMBER - TEST            
         BL    ERRSEQ              END NUMBER ALSO BEGINS WITH NUMBER           
         B     VALREQ30                                                         
*                                                                               
VALREQ25 CLC   PRORSTR(1),PROREND  TEST SAME LETTER STARTS BOTH                 
         BNE   ERRSEQ                                                           
*                                                                               
VALREQ30 CLC   PRORSTR,PROREND                                                  
         BH    ERRSEQ              OUT OF SEQUENCE START/END                    
*                                                                               
VALREQ40 MVC   SVORDSTR,PRORSTR                                                 
         MVC   SVORDEND,PROREND                                                 
*                                                                               
VALREQX  B     XIT                                                              
         DROP  R2                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO CHECK THE FORMAT OF AN ORDER NUMBER                            
* AT ENTRY, R1=A(NUMBER), R0=L'NUMBER                                           
* AT EXIT, CC=EQ FOR OK, CC=NEQ FOR ERROR                                       
*                                                                               
CHKNUM   CLI   0(R1),C'A'                                                       
         BL    CHKNUMR                                                          
         CLI   0(R1),C'I'                                                       
         BNH   CHKNUM2                                                          
*                                                                               
         CLI   0(R1),C'J'                                                       
         BL    CHKNUMR                                                          
         CLI   0(R1),C'R'                                                       
         BNH   CHKNUM2                                                          
*                                                                               
         CLI   0(R1),C'S'                                                       
         BL    CHKNUMR                                                          
         CLI   0(R1),C'Z'                                                       
         BNH   CHKNUM2                                                          
*                                                                               
         CLI   0(R1),C'0'                                                       
         BL    CHKNUMR                                                          
         CLI   0(R1),C'9'                                                       
         BH    CHKNUMR                                                          
*                                                                               
CHKNUM2  LA    R1,1(R1)                                                         
         BCTR  R0,0                                                             
*                                                                               
CHKNUM4  CLI   0(R1),C'0'                                                       
         BL    CHKNUMR                                                          
         CLI   0(R1),C'9'                                                       
         BH    CHKNUMR                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,CHKNUM4                                                       
*                                                                               
         CLI   *+1,0               FORCE CC=EQ                                  
         BR    RE                                                               
*                                                                               
CHKNUMR  CLI   *+0,0               FORCE CC=NEQ                                 
         BR    RE                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
* BUILD ORDER RESERVATION KEY                                                   
*---------------------------------------------------------------------          
*                                                                               
BLDKEY   NTR1                                                                   
         USING OBRRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         XC    OBRKEY,OBRKEY                                                    
         MVI   OBRKTYP,OBRKTYPQ                                                 
         MVC   OBRKCPY,CUL                                                      
         MVC   OBRKLAST,SVORDEND                                                
         MVC   OBRKFRST,SVORDSTR                                                
         B     XIT                                                              
         SPACE 2                                                                
*---------------------------------------------------------------------          
* TEST FOR DUPLICATE RESERVATION REQUEST                                        
* AT ENTRY, AIO = AIO1 = A(RESERVATION)                                         
* RETURNS CC=EQ FOR SAME SERVER REQUESTING DUPLICATE                            
*         CC=NEQ FOR DIFFERENT SERVER REQUESTING DUPLICATE - REJECT             
*---------------------------------------------------------------------          
*                                                                               
TESTDUP  NTR1  ,                                                                
         MVC   DUPDATA,SPACES                                                   
         GOTO1 GETELEM,DMCB,PACELQ,0                                            
         BNE   TESTDUP2                                                         
         USING PACELD,R6                                                        
         MVC   DUPPERS,PACPERS     EXTRACT PERSON                               
*                                                                               
TESTDUP2 MVI   BYTE,FFTTSQLI       SEARCH FOR SQL ID EL                         
         GOTO1 SRCHGET,DMCB,FFTELQ,(1,BYTE)                                     
         BNE   TESTDUP4                                                         
         USING FFTELD,R6                                                        
         ZIC   R1,FFTDLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     TESTDUP4                                                         
         MVC   DUPSERV(0),FFTDATA                                               
*                                                                               
TESTDUP4 CLC   SVSERVER,DUPSERV    TEST SAME SERVER MAKING REQUEST              
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
* ADD ORDER RESERVATION RECORD                                                  
*---------------------------------------------------------------------          
*                                                                               
ADDORES  NTR1  ,                                                                
         L     R6,AIO                                                           
         USING OBRRECD,R6                                                       
*                                                                               
         LR    RE,R6               CLEAR IO AREA                                
         LA    RF,2000                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         XC    OBRKEY,OBRKEY       RECORD IS KEY ONLY                           
         MVI   OBRKTYP,OBRKTYPQ                                                 
         MVC   OBRKCPY,CUL                                                      
         MVC   OBRKLAST,SVORDEND                                                
         MVC   OBRKFRST,SVORDSTR                                                
         MVC   OBRRLEN,=Y(OBRRFST-OBRRECD+1)                                    
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT          BUILD A PERSON ELEMENT                       
         USING PACELD,R6                                                        
         MVI   PACEL,PACELQ                                                     
         MVI   PACLN,PACLNQ                                                     
         MVC   PACPERS,SVPERSON                                                 
         GOTO1 DATCON,DMCB,(5,0),(1,PACDATE)                                    
         GOTO1 ADDELEM,DMCB,PACELD                                              
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         USING FFTELD,R6                                                        
         MVI   FFTEL,FFTELQ        BUILD A FREE FORM ELEMENT                    
         MVI   FFTTYPE,FFTTSQLI    WITH THE ORIGINATING SQL SERVER ID           
         MVC   FFTDATA(L'SVSERVER),SVSERVER                                     
         LA    R1,L'SVSERVER                                                    
         LA    RE,SVSERVER+L'SVSERVER-1                                         
         CLI   0(RE),C' '          FIND LAST SIGNIFICANT BYTE                   
         BH    *+10                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
*                                                                               
         STC   R1,FFTDLEN                                                       
         LA    R1,FFTDATA-FFTELD(R1) COMPUTE ELEMENT LENGTH                     
         STC   R1,FFTLN                                                         
         GOTO1 ADDELEM,DMCB,FFTELD                                              
*                                                                               
         GOTO1 ADDREC                                                           
*                                                                               
ADDORESX B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
* BUILD ORDER KEY FOR THE ORDER NUMBER AT 0(R1)                                 
*---------------------------------------------------------------------          
*                                                                               
BLDOKEY  NTR1  ,                                                                
         USING ORDRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,CUL                                                      
         MVC   ORDKORD,0(R1)                                                    
         B     XIT                                                              
         EJECT                                                                  
* EXIT AND ERROR ROUTINES                                                       
*                                                                               
*        RESERVATION REQUEST OVERLAPS ANOTHER RESERVATION                       
*        PASS BACK FIRST AND LAST NUMBER OF OVERLAPPING RESERVATION             
*                                                                               
ERROVER  MVC   APPLERR,=Y(PREROROV)                                             
         GOTO1 PUTITEM,DMCB,A(ITGENSTR),L'OVLPDATA,OVLPDATA                     
         BNE   EXIT                                                             
         B     ERROBJ                                                           
*                               FOUND ORDER WITHIN REQUEST RANGE                
ERRORD   MVC   APPLERR,=Y(PREROROR)                                             
         GOTO1 PUTITEM,DMCB,A(ITGENSTR),L'ORDNUM,ORDNUM                         
         BNE   EXIT                                                             
         B     ERROBJ                                                           
*                               DUPLICATE ORDER RESERVATION REQUEST             
ERRDUP   MVC   APPLERR,=Y(PRERORDU)                                             
         GOTO1 PUTITEM,DMCB,A(ITGENSTR),L'DUPDATA,DUPDATA                       
         BNE   EXIT                                                             
         B     ERROBJ                                                           
*                                                                               
*                                  FORMAT ERROR IN RESERVATION REQUEST          
ERRFMT   MVC   APPLERR,=Y(PRERFMT)                                              
         B     ERROBJ                                                           
*                                  INVALID REQUEST OBJECT                       
ERRROBJ  MVC   APPLERR,=Y(PRERROBJ)                                             
         B     ERROBJ                                                           
*                                  INVALID REQUEST SEQUENCE                     
ERRSEQ   MVC   APPLERR,=Y(PRERORSE)                                             
         B     ERROBJ                                                           
*                                  UPLOAD TO READ ONLY FILE                     
ERRUPRO  MVC   APPLERR,=Y(PRERUPRO)                                             
         B     ERROBJ                                                           
*                                  RETURN APPL ERRORS IN ERR OBJECT             
ERROBJ   GOTO1 HEXOUT,DMCB,APPLERR,FULL,2                                       
         GOTO1 PUTITEM,DMCB,A(ITGENERR),4,FULL                                  
         BNE   EXIT                                                             
         B     SETMDLST                                                         
*                                                                               
SETMDLST MVI   MDLAST,C'Y'         SET LAST FRAME INDICATOR                     
*                                                                               
EXIT     L     RD,SAVEDRD          RESTORE RD                                   
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
CONTROL  DC    C'000000'           ORDER CONTROL RECORD NUMBER                  
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* OVERLAY WORKING STORAGE                                                       
*                                                                               
OVERD    DSECT                                                                  
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
SVORDSTR DS    CL6                 START ORDER TO RESERVE                       
SVORDEND DS    CL6                 HIGHEST NUMBER TO RESERVE                    
SVPERSON DS    CL8                 PERSON MAKING REQUEST                        
SVSERVER DS    CL16                ORIGINATING SQL SERVER                       
*                                                                               
CTRY     DS    XL1                 COUNTRY FROM UTL                             
*                                                                               
OVLPDATA DS    0CL12                                                            
OVLPFRST DS    CL6                 OVERLAP FIRST NUMBER                         
OVLPLAST DS    CL6                 OVERLAP LAST NUMBER                          
ORDNUM   DS    CL6                 ORDER NUMBER                                 
DUPDATA  DS    0CL24                                                            
DUPPERS  DS    CL8                 DUPLICATE RES PERSON                         
DUPSERV  DS    CL16                DUPLICATE SQL SERVER ID                      
*                                                                               
SPACES   DS    CL128               BLANKS                                       
         EJECT                                                                  
* CTMADWORKD                                                                    
       ++INCLUDE CTMADWORKD                                                     
         EJECT                                                                  
* CTMADEQUS                                                                     
       ++INCLUDE CTMADEQUS                                                      
         EJECT                                                                  
* CTMADDSECT                                                                    
       ++INCLUDE CTMADDSECT                                                     
         EJECT                                                                  
* CTMADPREST                                                                    
       ++INCLUDE CTMADPREST                                                     
         EJECT                                                                  
* ACGENFILE                                                                     
* ACPRESTOQ                                                                     
* FAFACTS                                                                       
* FASELIST                                                                      
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACPRESTOQ                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012CTMAD23   07/08/04'                                      
         END                                                                    
