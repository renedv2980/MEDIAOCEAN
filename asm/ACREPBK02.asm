*          DATA SET ACREPBK02  AT LEVEL 001 AS OF 11/12/17                      
*PHASE ACBK02A                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
                                                                                
ACBK02   TITLE 'READ COMPANY RECORD AND BANK DETAIL RECORD AND REPORT '         
                                                                                
***********************************************************************         
*                                                                     *         
*  AUTHOR :-   NEERAJ MALIK   [ NMAL ]                                *         
*  DATE   :-   15 MARCH 2017                                          *         
*                                                                     *         
* READ COMPANY RECORD AND BANK DETAIL RECORD AND CREATE A REPORT      *         
* WITH ALL BANK RECORD DETAIL FOR EVERY COMPANY RECORD                *         
*                                                                     *         
* EXTRACT BELOW DETAIL FROM COMPANY RECORD                            *         
*                                                                               
* COMPANY LOGO , COMPANY NAME                                         *         
*                                                                     *         
* EXTRACT BELOW DETAIL FROM BANK DETAIL RECORD FOR COMPANY CODE       *         
* THERE IS POSSIBLITY TO HAVE MULTIPLE BANK RECORD FOR SAME CODE      *         
*                                                                     *         
*  BANK/BRANCH CODE                                                   *         
*  TYPE OF TRANMISSION                                                *         
*  ADVANTIS USER ID                                                   *         
*  ADVANTIS ACCOUNT                                                   *         
*  MESSAGE CLASS                                                      *         
*  TRANSMISSION KEY                                                   *         
*  DATASET NAME                                                       *         
*  FORMAT KEY                                                         *         
*  STATUS ATTRIBUTE FOR BKSTAT                                        *         
*  TRANSMISSION TYPE BKTRNTY (EITHER MQ OR EDICT)                     *         
*                                                                     *         
* IF ANY OF THE DETAILS ARE NOT AVAILABLE ON BANK RECORD              *         
* THAN WE NEED TO READ GENDIR/FIL TO FETCH BANK DETAIL RECORD AND SEE *         
* IF WE CAN FIND ANY DETAILS THERE                                    *         
*    IF  GENDIR/FIL FILE HAVE ANY OF THE DETAIL THEN THAT WILL BE     *         
*    POPULATED ON REPORT WITH PARANTHESIS, TO IDENTIFY THAT THEY ARE  *         
*    DEFAULT VALUES AND NOT FROM ACC FILE.                            *         
*                                                                     *         
***********************************************************************         
                                                                                
ACBK02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACBK**,R8,CLEAR=Y                                            
*                                                                               
         L     RA,0(,R1)                                                        
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING WORKD,RC            RA=A(WORK AREA SAVED AREA)                   
*                                                                               
         CLI   MODE,RUNLAST                                                     
         JE    RUNL                                                             
         J     EXIT                                                             
                                                                                
EXITL    CLI   *,FF                SET CC=LOW                                   
         J     EXIT                                                             
EXITH    CLI   *,0                 SET CC=HIGH                                  
         J     EXIT                                                             
EXITE    CR    RB,RB                                                            
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* LAST FOR RUN                                                        *         
* READ COMPANY RECORD AND BANK DETAIL REOCRD                          *         
* AND GENERATE A REPORT                                               *         
***********************************************************************         
         SPACE 1                                                                
RUNL     XR    RF,RF                                                            
         LH    RF,=Y(IOAREA-WORKD)                                              
         LA    RE,WORKD                                                         
         AR    RF,RE                                                            
         ST    RF,AIOAREA                                                       
*                                                                               
         LH    RF,=Y(IOAREA2-WORKD)                                             
         AR    RF,RE                                                            
         ST    RF,AIOAREA2                                                      
*                                                                               
         MVI   BYTE,0                                                           
*                                                                               
         LA    R6,P                                                             
         USING PLINED,R6           PRINT LINE DSECT                             
         MVC   PRTLINE,HEADER1     HEADER LINE  -1                              
         GOTOR ACREPORT            WRITE HEADER -1                              
         MVC   PRTLINE,HEADER2     HEADER LINE  -2                              
         GOTOR ACREPORT            WRITE HEADER -2                              
*                                                                               
         MVI   SCPYCD,SCPY         MOVE START VALUE FOR COMPANY CODE            
*                                                                               
RUNL02   EQU   *                                                                
         BAS   RE,CMPYCALL         CALL TO READ COMPANY RECORD                  
         BNZ   EXIT                NO MORE COMPANY RECORD                       
*                                                                               
         BAS   RE,BANKCALL         CALL TO READ BANK DETAIL RECORD              
         B     RUNL02              CALL NEXT COMPANY RECORD                     
*                                                                               
         EJECT                                                                  
**                                                                              
***********************************************************************         
* READ COMPANY RECORD, AND FIND IF VALIED COMPANY CODE THEN EXTARCT   *         
* COMPANY DETAILS. SUCH AS                                                      
* COMPANY ALPHA ID                                                              
***********************************************************************         
*                                                                               
CMPYCALL NTR1                                                                   
*                                                                               
         XC    LALPHA,LALPHA         CLEAR ALPHA CODE                           
*                                                                               
         LA    R2,IOKEY                                                         
         USING CPYRECD,R2            COMPANY RECORD DSECT                       
CMPY001  XR    R5,R5                                                            
         LLC   R5,SCPYCD             LOAD COMPANY CODE                          
         AHI   R5,1                  ADD  ONE FOR NEXT COMPANY CODE             
         STC   R5,SCPYCD             STORE COMPANY CODE FOR NXT LOOP            
         CLI   SCPYCD,ECPY           CHECK IF WE REACH END OF CODE              
         BH    CMPYEND                                                          
         MVI   CPYKEY,C' '           MOVE SPACE ON FIRST BYTE                   
         MVC   CPYKEY+1(L'CPYKEY-1),CPYKEY  CLEAR KEY WITH SPACES               
         MVC   CPYKCPY,SCPYCD        COMPANY CODE FOR READ                      
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,CPYKEY,CPYKEY,0                       
*                                                                               
         BNE   CMPY001               NOT EQUAL, CHECK FOR NEXT CODE             
*                                                                               
         MVC   SVDA,CPYKDA            MOVE DISK ADDRESS FOR MST CALL            
         GOTOR DATAMGR,DMCB,(X'00',GETREC),ACCMST,SVDA,AIOAREA,DMWORK           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIOAREA                                                       
         LA    R3,CPYRFST             LOAD FIRST ELEMENT ADDRESS IN R3          
CMPY002  EQU   *                                                                
         CLI   0(R3),X'00'                                                      
         BE    CMPYOVR                                                          
         CLI   0(R3),CPYELQ           CHECK IF X'10' ELEMENT                    
         BE    CMPY10                                                           
         XR    R5,R5                                                            
         LLC   R5,1(R3)                                                         
         AR    R3,R5                                                            
         B     CMPY002                                                          
         USING CPYELD,R3                                                        
CMPY10   MVC   LALPHA,CPYALPHA        COMPANY ALPHA ID                          
         B     CMPYOVR                                                          
*                                                                               
CMPYEND  EQU   *                                                                
         LTR   RC,RC                                                            
         B     CMPYCALX                                                         
*                                                                               
CMPYOVR  EQU   *                                                                
         XR    R5,R5                     | SET CONDITION CODE AS ZERO           
         LTR   R5,R5                     |                                      
CMPYCALX XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* READ BANK DETAIL RECORD FROM ACC FILE FOR THE COMPANY CODE RECORD   *         
* WE ALREDY READ. IF BANK DETAI RECORD IS AVILABLE THEN WE WILL                 
* WRITE COMPANY REOCRD FIRST AND THEN BANK DETAIL RECORDS ON PRINT LINE         
*                                                                               
***********************************************************************         
*                                                                               
BANKCALL NTR1                                                                   
*                                                                               
         LA    R6,P                  LOAD PRINT LINE ADDRESS                    
         USING PLINED,R6             PRINT LINE DSECT                           
         LA    R2,IOKEY                                                         
         USING BNKRECD,R2            COMPANY RECORD DSECT                       
         XC    BNKKEY,BNKKEY         CLEAR BANK RECORD KEY                      
         MVI   BNKTYP,BNKTYPQ        BANK RECORD TYPE  X'2D'                    
         MVI   BNKSUB,BNKSUBQ        BANK RECORD SUB-TYPE  X'08'                
         MVC   BNKCPY,SCPYCD         COMPANY CODE                               
*                                                                               
BANKHI   GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,BNKKEY,BNKKEY,0                       
         TM    DMCB+8,X'80'      |  CHECK FOR END OF RECORD                     
         BNZ   BANKCALX          |  IF END THEN COME OUT OF LOOP                
         CLI   DMCB+8,X'00'         IF READ HIGH WAS SUCCESS                    
         BE    BANK001                                                          
         DC    H'0'                                                             
*                                                                               
BANKSEQ  EQU   *                                                                
         LA    R2,IOKEY                                                         
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,BNKKEY,BNKKEY,0                       
         TM    DMCB+8,X'80'      |  CHECK FOR END OF RECORD                     
         BNZ   BANKCALX          |  IF END THEN COME OUT OF LOOP                
         CLI   DMCB+8,X'00'         IF SEQUENTIAL READ WAS SUCCESS              
         BE    BANK001                                                          
         DC    H'0'                 ABEND IN ANY OTHER CASE                     
*                                                                               
BANK001  EQU   *                                                                
         CLI   BNKTYP,BNKTYPQ      BANK RECORD TYPE CHANGED                     
         BNE   BANK007             EXIT BANK READ LOOP                          
         CLI   BNKSUB,BNKSUBQ      BANK RECORD SUB-TYPE CHANGED                 
         BNE   BANK007             EXIT BANK READ LOOP                          
         CLC   BNKCPY,SCPYCD       CHECK COMPANY CODE IS CHANGED ?              
         BNE   BANK007             EXIT BANK READ LOOP                          
*                                                                               
BANK002  EQU   *                                                                
         GOTO1 HEXOUT,DMCB,BNKCPY,PBNKCPY,L'BNKCPY   COMPANY CODE               
         MVC   PALPHA,LALPHA       MOVE COMPANY ALPHA ID                        
         LA    R5,TRANS            LOAD TRANSMISSION TABLE                      
*                                                                               
BANK003  CLI   0(R5),FF            CHECK END OF TABLE                           
         BE    BANK004             MOVE DEFAULT AS XXXXXXX NO ENTRY             
         CLC   0(1,R5),BNKETYP                                                  
         BE    BANK004                                                          
         LA    R5,TRANSL(R5)                                                    
         B     BANK003                                                          
BANK004  MVC   PBNKETYP,1(R5)           MOVE TRNAS TYPE DESCRIPTION             
*                                                                               
         MVC   PBNKBANK,BNKBANK    BANK CODE     | FROM KEY PART                
         MVI   PBNKBRNB,C'B'       'B' TO MAKE BRANCH CODE ALPHA                
         MVC   PBNKBRNC,BNKBRNCH   BRANCH CODE   |                              
*                                                                               
         MVC   SVDA,BNKKDA         MOVE DISK ADDRESS FOR MST CALL               
         GOTOR DATAMGR,DMCB,(X'00',GETREC),ACCMST,SVDA,AIOAREA,DMWORK           
         BE    *+6                READ FULL RECORD FOR BANK                     
         DC    H'0'                                                             
*                                                                               
         L     R2,AIOAREA         LOAD ADDRESS FOR BANK RECORD                  
         LA    R3,BNKRFST         POINT TO FIRST ELEMENT IN BANK RECORD         
BANK005  EQU   *                                                                
         CLI   0(R3),X'00'                                                      
         BE    BANK007            X'2E' ELEMENT WAS NOT FOUND IN RECORD         
         CLI   0(R3),BNKELQ       X'2E' ELEMENT                                 
         BE    BANK2E                                                           
         XR    R5,R5                                                            
         LLC   R5,1(R3)                                                         
         AR    R3,R5                                                            
         B     BANK005                                                          
*                                                                               
         USING BNKELD,R3          MAP TO ELEMENT X'2E' BANK INFORMATION         
BANK2E   EQU   *                                                                
         MVC   PBKADVID,BKADVID   ADVANTIS USER ID                              
         MVC   PBKADVAC,BKADVAC   ADVANTIS ACCOUNT                              
         MVC   PBKMSGCL,BKMSGCL   MESSAGE CLASS                                 
         MVC   PBKTRNKY,BKTRNKY   TRANSMISSION KEY                              
         MVC   PBKDSNM,BKDSNM     DATASET NAME                                  
         MVC   PBKFMKY,BKFMKY     FORMAT KEY                                    
         GOTO1 HEXOUT,DMCB,BKSTAT,PBKSTAT,L'BKSTAT  STATUS ATTRIBUTE            
*                                                                               
         CLI   BKTRNTY,BKTEDT       IS IT A EDI TRANSACTION                     
         BNE   *+14                                                             
         MVC   PBKTRNTY,EDICT       TRANSMISSION TYPE EDICT                     
         B     BANK006                                                          
         CLI   BKTRNTY,BKTMQ        IS IT A MQ  TRANSACTION                     
         BNE   *+10                                                             
         MVC   PBKTRNTY,MQM         TRANSMISSION TYPE MQ                        
*                                                                               
         MVC   SAVKEY,BNKKEY        SAVE KEY BEFORE LEAVING FOR GEN             
         BAS   RE,GENREAD           READ GENDIR/FIL FILES FOR BANK              
         MVC   BNKKEY,SAVKEY        RESTORE KEY AND READ ACCFILE                
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,BNKKEY,BNKKEY,0                       
         BE    BANK006              SHOULD BE A SUCCESSFUL READ                 
         DC    H'0'                 SOMETHING WRONG -- ABEND                    
*                                                                               
BANK006  EQU   *                                                                
         MVI   PFIL,FF              FILTER FOR RECORDS IN REPORT                
         MVI   PBPIP01,PIPE         CREATE PIPE DELIMITER                       
         MVI   PBPIP02,PIPE         CREATE PIPE DELIMITER                       
         MVI   PBPIP03,PIPE         CREATE PIPE DELIMITER                       
         MVI   PBPIP04,PIPE         CREATE PIPE DELIMITER                       
         MVI   PBPIP05,PIPE         CREATE PIPE DELIMITER                       
         MVI   PBPIP06,PIPE         CREATE PIPE DELIMITER                       
         MVI   PBPIP07,PIPE         CREATE PIPE DELIMITER                       
         MVI   PBPIP08,PIPE         CREATE PIPE DELIMITER                       
         MVI   PBPIP09,PIPE         CREATE PIPE DELIMITER                       
         MVI   PBPIP10,PIPE         CREATE PIPE DELIMITER                       
         MVI   PBPIP11,PIPE         CREATE PIPE DELIMITER                       
         MVI   PBPIP12,PIPE         CREATE PIPE DELIMITER                       
         MVI   PBPIP13,PIPE         CREATE PIPE DELIMITER                       
         GOTOR ACREPORT             PRINT REPORT FOR BANK DETAILS               
         B     BANKSEQ              READ NEXT SEQ BANK RECORD                   
*                                                                               
BANK007  EQU   *                                                                
         MVI   PRTLINE,C' '          MOVE ' ' SPACE ON FIRST BYTE               
         MVC   PRTLINE+1(L'PRTLINE-1),PRTLINE  SET SPAC LINE FOR PRINT          
         GOTOR ACREPORT              PRINT EMPTY LINE                           
*                                                                               
BANKCALX XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ROUTINE TO GET BANK RECORD FROM CONTROL/NFILE                       *         
***********************************************************************         
GENREAD  NTR1                                                                   
*                                                                               
         XC    LBATTKEY(LBATQ),LBATTKEY  CLEAR COMPLETE BLOCK OF DATA           
*                                                                               
         L     R2,AIOAREA2                                                      
         USING BANKRECD,R2               COMPANY RECORD DSECT                   
         XC    BANKEY,BANKEY             CLEAR BANK RECORD KEY                  
         MVI   BANKTYP,BANKTYPQ          BANK RECORD TYPE  'B'                  
         MVI   BANKSUB,BANKSGQ           BANK RECORD SUB-TYPE  'G'              
         MVC   BANKCDE,PBNKBANK          BANK MAIN CODE                         
         MVC   BANKHUB,PBNKHUB           BANK HUB CODE                          
         CLC   PBNKBRN,SPACES            IF BRANCH CODE IS SPACE                
         BE    *+10                                                             
         MVC   BANKBRN,PBNKBRN           BANK BRANCH CODE                       
*                                                                               
         L     R3,ADMASTC              | SAVE SE VALUE AND SET IT TO            
         USING MASTD,R3                | X'0A' FOR READING GENDIR               
         L     R5,MCUTL                | AND GENFIL FILES                       
         MVC   SAVSE,4(R5)             |                                        
         MVI   4(R5),X'0A'             |                                        
*                                                                               
         XC    DMCB,DMCB                                                        
         GOTO1 DATAMGR,DMCB,(X'00',DMREAD),GENDIR,BANKEY,BANKEY                 
         TM    DMCB+8,FF                                                        
         BZ    *+8                                                              
         B     GEN_END      NOT ABENDING RATHER GOING OUT OF LOOP               
*        DC    H'00'                                                            
*                                                                               
         MVC   SVDA,BANKDA                                                      
         GOTOR DATAMGR,DMCB,(X'00',GETREC),GENFIL,SVDA,BANKEY,BANKEY            
         BE    *+6                READ FULL RECORD FOR BANK                     
         DC    H'0'                                                             
*                                                                               
         L     R3,AIOAREA2                                                      
         AHI   R3,BANFIRST                                                      
GEN002   DS    0H                                                               
         CLI   0(R3),0                                                          
         BE    GEN004                                                           
         CLI   0(R3),BAGELQ             X'5B' ELEMENT PRESENT                   
         BE    GEN5B                                                            
         CLI   0(R3),BATELQ             X'5E' ELEMENT PRESENT                   
         BE    GEN5E                                                            
GEN003   XR    R5,R5                                                            
         LLC   R5,1(R3)                                                         
         AR    R3,R5                                                            
         B     GEN002                                                           
*                                                                               
         USING BAGELD,R3                                                        
GEN5B    DS    0H                                                               
         MVC   LBAGRNO,BAGRNO           ROUTING NUMBER                          
         MVC   LBAGFORM,BAGFORM         FORMAT KEY                              
         B     GEN003                                                           
         USING BATELD,R3                                                        
GEN5E    DS    0H                                                               
         MVC   LBATTKEY,BATTKEY         TRANSMISSION KEY                        
         MVC   LBATUSER,BATUSER         ADVANTIS USER ID                        
         MVC   LBATACCN,BATACCN         ADVANTIS ACCOUNT                        
         MVC   LBATCLAS,BATCLAS         MESSAGE CLASS                           
         MVC   LBATTTYP,BATTTYP         TRANSMISSION TYPE (EDICT OR MQ)         
         B     GEN003                                                           
GEN004   DS    0H                                                               
         CLC   PBKADVID,SPACES          ADVANTIS USER ID                        
         BNE   GEN005                   CHECK AND MOVE                          
         CLC   LBATUSER,SPACES                                                  
         BNH   GEN005                                                           
         MVI   PBKADVI1,PARL                                                    
         MVC   PBKADVID,LBATUSER         MOVE ADVANTIS USER ID FROM GEN         
         MVI   PBKADVI2,PARR                                                    
GEN005   CLC   PBKADVAC,SPACES           ADVANTIS ACCOUNT                       
         BNE   GEN006                    CHECK AND MOVE                         
         CLC   LBATACCN,SPACES                                                  
         BNH   GEN006                                                           
         MVI   PBKADVA1,PARL                                                    
         MVC   PBKADVAC,LBATACCN         MOVE ADVANTIS ACC FROM GEN             
         MVI   PBKADVA2,PARR                                                    
GEN006   CLC   PBKMSGCL,SPACES           MESSAGE CLASS                          
         BNE   GEN007                    CHECK AND MOVE                         
         CLC   LBATCLAS,SPACES                                                  
         BNH   GEN007                                                           
         MVI   PBKMSGC1,PARL                                                    
         MVC   PBKMSGCL,LBATCLAS         MOVE MESSAGE CLASS FROM GEN            
         MVI   PBKMSGC2,PARR                                                    
GEN007   CLC   PBKTRNKY,SPACES           TRANSMISSION KEY                       
         BNE   GEN008                    CHECK AND MOVE                         
         CLC   LBATTKEY,SPACES                                                  
         BNH   GEN008                                                           
         MVI   PBKTRNK1,PARL                                                    
         MVC   PBKTRNKY,LBATTKEY         MOVE TRANS KEY FROM GEN                
         MVI   PBKTRNK2,PARR                                                    
GEN008   CLC   PBKFMKY,SPACES            FORMAT KEY                             
         BNE   GEN009                    CHECK AND MOVE                         
         CLC   LBAGFORM,SPACES                                                  
         BNH   GEN009                                                           
         MVI   PBKFMKY1,PARL                                                    
         MVC   PBKFMKY,LBAGFORM          MOVE FORMAT KEY FROM GEN               
         MVI   PBKFMKY2,PARR                                                    
GEN009   CLC   PBKTRNTY,SPACES           FORMAT KEY                             
         BNE   GEN010                    CHECK AND MOVE                         
         CLI   LBATTTYP,BATTTEDT         CHECK FOR EDT                          
         BNE   *+10                                                             
         MVC   PBKTRNTY,EDICT            MOVE VALUEF ROM GEN                    
         CLI   LBATTTYP,BATTTMQ          CHECK FOR EDT                          
         BNE   *+10                                                             
         MVC   PBKTRNTY,MQM              MOVE MQ VALUE FOE TRAMSMISSON          
         CLC   PBKTRNTY,SPACES           CHECK IF ANY VALUE IS MOVED            
         BE    GEN010                                                           
         MVI   PBKTRNT1,PARL             MOVE PARANTHESIS FOR DEFAULT           
         MVI   PBKTRNT2,PARR             MOVE PARANTHESIS FOR DEFAULT           
GEN010   CLC   LBAGRNO,SPACES            CHECK FOR ROUTING NUMBER               
         BNH   GEN_END                                                          
         MVI   PBAGRNO1,PARL             MOVE PARANTHESIS FOR DEFAULT           
         MVI   PBAGRNOR,C'R'             PREFIX WITH 'R' FOR FILTER             
         MVC   PBAGRNO,LBAGRNO           MOVE ROUTING NUMBER FROM GEN           
         MVI   PBAGRNO2,PARR             MOVE PARANTHESIS FOR DEFAULT           
*                                                                               
GEN_END  EQU   *                                                                
         L     R3,ADMASTC                                                       
         USING MASTD,R3                                                         
         L     R5,MCUTL                                                         
         MVC   4(1,R5),SAVSE                                                    
*                                                                               
GENREADX XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
SCPY     EQU   X'3F'           START RANGE OF COMPANY CODE                      
ECPY     EQU   X'FE'           END RANGE OF COMPANY CODE                        
K        EQU   1024                                                             
YESQ     EQU   C'Y'                                                             
NOQ      EQU   C'N'                                                             
FF       EQU   X'FF'                                                            
PARL     EQU   C'('                                                             
PARR     EQU   C')'                                                             
PIPE     EQU   C'|'                                                             
MAXRECLN EQU   2000                                                             
GETREC   DC    C'GETREC  '                                                      
GENDIR   DC    C'GENDIR  '                                                      
GENFIL   DC    C'GENFIL  '                                                      
ACCDIR   DC    C'ACCDIR  '                                                      
ACCMST   DC    C'ACCMST  '                                                      
ADDREC   DC    C'ADDREC  '                                                      
PUTREC   DC    C'PUTREC  '                                                      
WRITE    DC    C'DMWRT   '                                                      
WARNING  DC    C'**ELEMENTS IN DOESNT MATCH OUT!!**'                            
EDICT    DC    CL5'EDICT'                                                       
MQM      DC    CL5'MQ'                                                          
*****         PLNQ                                                              
HEADER1  DC CL(PLNQ)'§ COMPANY|ALPHA|BANK |BRANCH|EDI |ADVANTS|ADVANTS|-        
               MESSAGE|TRANSMISSION|DATASET|FORMAT|STATUS|TRANSMISSION -        
               |ROUTING| '                                                      
*****                                                                           
HEADER2  DC CL(PLNQ)'§ CODE|ID |CODE |CODE|TYPE|USER ID|ACCOUNT        -        
               |CLASS|KEY|NAME|KEY|BYTE|TYPE|NUMBER|   '                        
*****                                                                           
*                                                                               
TRANS    DC    X'00',C'DEFAULT'                                                 
TRANSL   EQU   *-TRANS                                                          
         DC    X'01',C'EFT    '                                                 
         DC    X'02',C'POSPAY '                                                 
         DC    X'03',C'ACR    '                                                 
         DC    X'04',C'820    '                                                 
         DC    X'FF',C'XXXXXXX'                                                 
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                                                             
IOAREA3  DS    CL2000                                                           
IOLNQ    EQU   *-IO                                                             
*                                                                               
***********************************************************************         
* ADDRESS CONSTANTS                                                   *         
***********************************************************************         
VHEXOUT  DC    V(HEXOUT)                                                        
*                                                                               
***********************************************************************         
WORKD    DSECT                                                                  
MSG      DS    CL20         MESSAGE FOR DUMP                                    
*                                                                               
AIOAREA  DS    A            ADDRESS OF IO AREA                                  
AIOAREA2 DS    A            ADDRESS OF IO AREA                                  
SVDA     DS    XL4          SAVED DISK ADDRESS                                  
*                                                                               
SCPYCD   DS    XL1                                                              
SAVKEY   DS    XL42                                                             
*                                                                               
LBATTKEY DS    CL8            TRANSMISSION KEY                                  
LBATUSER DS    CL7            ADVANTIS USER ID                                  
LBATACCN DS    CL4            ADVANTIS USER ACCOUNT                             
LBATCLAS DS    CL8            MESSAGE CLASS                                     
LBATTTYP DS    XL1            TRANSMISSION TYPE (EDICT OR MQ)                   
LBAGRNO  DS    CL9            ROUTING NUMBER                                    
LBAGFORM DS    CL10           FORMAT KEY                                        
LBATQ    EQU   *-LBATTKEY     LENGTH OF VARIABLES FROM GEN FILE                 
*                                                                               
SAVSE    DS    XL1                 SAVE SE CODE                                 
LALPHA   DS    CL2            COMPANY ALPHA CODE                                
*                                                                               
         DS    0H                                                               
IOAREA   DS    XL(2*K)                                                          
IOAREA2  DS    XL(2*K)                                                          
         DS    XL2                                                              
WORKX    EQU   *-WORKD                                                          
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DSECT FOR PRINT LINE                                                *         
***********************************************************************         
         SPACE 1                                                                
PLINED   DSECT                                                                  
PRTLINE  DS    0CL130                                                           
PFIL     DS    CL1                 FOR FILTERING RECORD IN JOB                  
         DS    CL1                                                              
PBNKCPY  DS    CL2                 COMPANY CODE                                 
         DS    CL1                                                              
PBPIP01  DS    CL1                                                              
PALPHA   DS    CL2                 COMPANY ALPHA ID                             
PBPIP02  DS    CL1                                                              
PBNKBANK DS    CL3                 BANK CODE                                    
PBPIP03  DS    CL1                                                              
PBNKBRNB DS    CL1                 TO MAKE BRANCH CODE ALPHA FOR SHEET          
PBNKBRNC DS   0CL7                 HUB + BRANCH CODE                            
PBNKHUB  DS    CL3                 HUB CODE                                     
PBNKBRN  DS    CL4                 BRANCH CODE                                  
PBPIP04  DS    CL1                                                              
PBNKETYP DS    CL7                 TYPE OF TRANSMISSION                         
PBPIP05  DS    CL1                                                              
PBKADVI1 DS    CL1                 '(' FOR DEFAULT VALUE                        
PBKADVID DS    CL7                 ADVANTIS USER ID                             
PBKADVI2 DS    CL1                 ')' FOR DEFAULT VALUE                        
PBPIP06  DS    CL1                                                              
PBKADVA1 DS    CL1                 '(' FOR DEFAULT VALUE                        
PBKADVAC DS    CL4                 ADVANTIS ACCOUNT                             
PBKADVA2 DS    CL1                 ')' FOR DEFAULT VALUE                        
PBPIP07  DS    CL1                                                              
PBKMSGC1 DS    CL1                 '(' FOR DEFAULT VALUE                        
PBKMSGCL DS    CL8                 MESSAGE CLASS                                
PBKMSGC2 DS    CL1                 ')' FOR DEFAULT VALUE                        
PBPIP08  DS    CL1                                                              
PBKTRNK1 DS    CL1                 '(' FOR DEFAULT VALUE                        
PBKTRNKY DS    CL8                 TRANSMISSION KEY                             
PBKTRNK2 DS    CL1                 ')' FOR DEFAULT VALUE                        
PBPIP09  DS    CL1                                                              
PBKDSNM  DS    CL15                DATA SET NAME                                
PBPIP10  DS    CL1                                                              
PBKFMKY1 DS    CL1                 '(' FOR DEFAULT VALUE                        
PBKFMKY  DS    CL10                FORMAT KEY                                   
PBKFMKY2 DS    CL1                 ')' FOR DEFAULT VALUE                        
PBPIP11  DS    CL1                                                              
PBKSTAT  DS    CL2                 STATUS BYTE                                  
PBPIP12  DS    CL1                                                              
PBKTRNT1 DS    CL1                 '(' FOR DEFAULT VALUE                        
PBKTRNTY DS    CL5                 TRANSMISSION TYPE EDI OR MQ                  
PBKTRNT2 DS    CL1                 ')' FOR DEFAULT VALUE                        
PBPIP13  DS    CL1                                                              
PBAGRNO1 DS    CL1                 '(' FOR DEFAULT VALUE                        
PBAGRNOR DS    CL1                 'R' FOR ROUTING NUMBER DENOTION              
PBAGRNO  DS    CL9                 ROUTING NUMBER                               
PBAGRNO2 DS    CL1                 ')' FOR DEFAULT VALUE                        
         DS    CL9                                                              
PLNQ     EQU   *-PRTLINE           LENGTH OF DSECT                              
         EJECT                                                                  
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
* DDTSARD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDTSARD                                                        
         PRINT ON                                                               
* GEGENBNK                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEGENBNK                                                       
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACREPBK02 11/12/17'                                      
         END                                                                    
