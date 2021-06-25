*          DATA SET SPEZF19    AT LEVEL 003 AS OF 05/01/02                      
*          DATA SET SPEZF19    AT LEVEL 023 AS OF 11/18/98                      
*PHASE T23019A                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE QSORT                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: T23019 - EZ EXPRESS TRACE - READS FILE FROM EZLOAD          *         
*                                                                     *         
*  OUTPUTS: OFFLINE REPORT OF MISSING BATCHES THAT ARE ON BILLING FILE*         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 - WORK REG                                              *         
*          R4 - WORK REG                                              *         
*          R5 - WORK REG                                              *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER, EZBLOCK         *         
*          R7 - SECOND BASE                                           *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPEZF00-T23000)         *         
*                  - IN AIO FROM HYPER CONTROLLER (T00A30)            *         
*             AIO2 - TABLE OF LINKED IDS AND INVOICE CTS FOR %        *         
*             AIO3 - TABLE OF LINKED IDS FOR COMBINED BILLING         *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*  LEV  2    MAY29/01 FIX DUMMY                                       *         
*                                                                     *         
***********************************************************************         
         TITLE 'T23019 - EZ EXPRESS BILLING TRACE'                              
T23019   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T23019**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO                                                          
         ST    RC,SVRC                                                          
         MVC   AIO,AIO1                                                         
         MVI   QMED,C'T'                                                        
         MVI   IOOPT,C'Y'          USER DOING ALL I/O                           
         SPACE                                                                  
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    INVAL                                                            
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    INVAL                                                            
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    INVAL                                                            
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    LIST                                                             
         CLI   MODE,RECADD         ADD RECORD                                   
         BE    INVAL                                                            
         CLI   MODE,RECDEL         DEL RECORD                                   
         BE    INVAL                                                            
         SPACE                                                                  
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*   VKEY - VALIDATE KEY                                               *         
***********************************************************************         
         SPACE                                                                  
VKEY     CLI   ACTNUM,18          THIS HAD BETTER BE BILLING                    
         BE    *+12                                                             
         CLI   ACTNUM,ACTSEL                                                    
         BNE   INVAL                                                            
         SPACE                                                                  
         CLI   OFFLINE,C'Y'                                                     
         BE    *+12                                                             
         CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
         BNE   INVAL                                                            
         SPACE                                                                  
         GOTO1 VALIMED                                                          
         SPACE                                                                  
         LA    R2,LINUSRH          USER                                         
         XC    RUID,RUID                                                        
         XC    RUIDNUM,RUIDNUM                                                  
         CLI   5(R2),0                                                          
         BE    VK100                                                            
         SPACE                                                                  
         CLI   5(R2),3                                                          
         BNE   VK040                                                            
         SPACE                                                                  
         CLC   =C'ALL',8(R2)                                                    
         BE    VK100                                                            
         SPACE                                                                  
* VALIDATE USER HERE                                                            
         SPACE                                                                  
VK040    DS    0H                                                               
         CLI   1(R4),8                                                          
         BH    USIDLER                                                          
         MVC   RUID,8(R2)                                                       
         OC    RUID,SPACES                                                      
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(10),SPACES                                                
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   KEY+15(0),8(R2)                                                  
         L     R4,AIO1                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD ',=C'CTFILE ',KEY,(R4)                    
         CLI   8(R1),0                                                          
         BNE   USERIDBD                                                         
         USING CTIKEY,R4                                                        
         SPACE                                                                  
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVC   FUIDNUM,2(R6)       BINARY USER ID (ORIGIN)                      
         DROP  R4                                                               
         SPACE                                                                  
VK100    LA    R2,LINSTAH          STATION                                      
         XC    RQSTA,RQSTA                                                      
         CLI   5(R2),0                                                          
         BE    VK200                                                            
         SPACE                                                                  
         OC    RUID,RUID           CAN'T HAVE BOTH AGENCY AND STATION           
         BNZ   BOTHERR                                                          
         SPACE                                                                  
         CLI   5(R2),3                                                          
         BNE   VK140                                                            
         SPACE                                                                  
         CLC   =C'ALL',8(R2)                                                    
         BE    VK200                                                            
         SPACE                                                                  
VK140    GOTO1 VALISTA                                                          
         SPACE                                                                  
         MVC   RQSTA,8(R2)                                                      
         OC    RQSTA,SPACES                                                     
         SPACE                                                                  
VK200    LA    R2,LINBMOH          BILLING MONTH                                
         XC    RQDTES,RQDTES                                                    
         CLI   5(R2),0             IF NO DATE                                   
         BE    MISSERR                                                          
         SPACE                                                                  
         GOTO1 DATVAL,DMCB,(0,FHDRLEN(R2)),WORK                                 
         ICM   R3,15,DMCB                                                       
         BNZ   BADATE                                                           
         SPACE                                                                  
         GOTO1 (RF),(R1),(2,FHDRLEN(R2)),WORK                                   
         ICM   R3,15,DMCB                                                       
         BZ    BADATE                                                           
         SPACE                                                                  
         MVC   WORK+4(2),=C'01'                                                 
         MVC   RQDTSTR,WORK                                                     
         SPACE                                                                  
         MVC   INVMON(1),WORK+1    SAVE BILLING YEAR FOR INVOICE #              
         MVC   INVMON+1(1),WORK+3  SAVE BILLING MONTH FOR INVOICE #             
         CLI   WORK+2,C'0'                                                      
         BE    VK220                                                            
         IC    RE,INVMON+1                                                      
         LA    RE,1(,RE)                                                        
         STC   RE,INVMON+1                                                      
         NI    INVMON+1,X'CF'                                                   
         SPACE                                                                  
VK220    LA    R0,1                                                             
         LA    R3,40                                                            
         MVC   WORK+6(4),WORK                                                   
         MVC   WORK+10(2),=C'25'                                                
VK240    GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         SPACE                                                                  
         CLC   WORK(4),WORK+6                                                   
         BNE   VK260                                                            
         MVC   WORK(6),WORK+6                                                   
         B     VK240                                                            
         SPACE                                                                  
VK260    MVC   RQDTEND,WORK                                                     
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(0,RQDTSTR),(6,BILLMON)                              
*                                  FILTERS LINE                                 
         OI    4(R2),X'20'                                                      
         SPACE                                                                  
         LA    R2,LINIDTH          INVOICE DATE                                 
         CLI   5(R2),0             IF NO DATE                                   
         BE    VK300                USE TODAY                                   
         SPACE                                                                  
         GOTO1 DATVAL,DMCB,(2,FHDRLEN(R2)),WORK                                 
         ICM   R3,15,DMCB                                                       
         BNZ   BADATE                                                           
         SPACE                                                                  
         GOTO1 (RF),(R1),(0,FHDRLEN(R2)),WORK                                   
         ICM   R3,15,DMCB                                                       
         BZ    BADATE                                                           
         SPACE                                                                  
         GOTO1 DATCON,(R1),(0,WORK),(5,INVCDATE)                                
         B     VK400                                                            
         SPACE                                                                  
VK300    DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(5,INVCDATE)                                   
         GOTO1 (RF),(R1),(5,0),(0,WORK)                                         
         ZAP   INVNUM,=P'0'                                                     
         SPACE                                                                  
VK400    MVC   TODAY(2),WORK+2     MONTH                                        
         MVC   TODAY+2(2),WORK+4   DAY                                          
         MVC   TODAY+4(2),WORK     YEAR                                         
         L     R3,=A(HEADTE)                                                    
         GOTO1 DATCON,DMCB,WORK,(5,(R3))                                        
         SPACE                                                                  
         LA    R2,LINFTRH          FILTERS                                      
         GOTO1 =A(VFTR),RR=RELO                                                 
         SPACE                                                                  
VKXIT    XC    KEY,KEY                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LIST - OPEN EZI.BILL.MTD FILE, ON A MATCH WITH REQUEST, SEE IF STILL*         
*          ON WORKER FILE - ALSO CK MOVE RECS, AND SEE IF ON FILE.    *         
***********************************************************************         
         SPACE                                                                  
LIST     DS    0H                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVI   USEIO,C'Y'          SET USER WILL DO ALL I/O                     
         SPACE                                                                  
         ZAP   TMOVREC,=P'0'                                                    
         ZAP   UMOVREC,=P'0'                                                    
         ZAP   CABYPASS,=P'0'                                                   
         SPACE                                                                  
         LA    R0,NCTRS                                                         
         LA    R1,CTRS                                                          
INIT010  ZAP   0(5,R1),=P'0'                                                    
         LA    R1,5(,R1)                                                        
         BCT   R0,INIT010                                                       
         SPACE                                                                  
         MVC   EZBILL-8(8),=C'*EZBILL*'                                         
         SPACE                                                                  
         OPEN  (EZBILLF,(INPUT))                                                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         LA    R0,(EZBILLED-EZBILD)+1+8                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         L     R1,=A(RECCARD)                                                   
         UNPK  21(3,R1),DUB                                                     
         MVI   FIRSTSW,0                SET FIRST TIME                          
         SPACE                                                                  
         GOTO1 =V(SORTER),DMCB,A(SORTCARD),A(RECCARD)                           
         SPACE                                                                  
         ICM   RF,15,UTL                                                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   SAVSYS,4(RF)                                                     
         MVI   4(RF),CONSECTL                                                   
CONSECTL EQU   X'0A'                                                            
         SPACE                                                                  
         L     R4,AIO1                                                          
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'CONTROL',A(FILIST),(R4)               
         L     RE,VADUMMY                                                       
         ST    RE,ABILTAB                                                       
         LH    RF,=AL2(BILTABLN*L'BILENT)                                       
         LR    R1,RE                                                            
         AR    R1,RF                END OF BILLING TABLE                        
         XCEF                                                                   
BILTABLN EQU   (20000/L'BILENT)                                                 
         SPACE                                                                  
         LR    RE,R1               START OF BATCH MOVE TABLE                    
         ST    RE,AMOVTAB                                                       
         L     RF,=A(MOVTABLN*L'MOVENT)                                         
         XCEF                                                                   
MOVTABLN EQU   (102000/L'MOVENT)    1000 ENTRIES                                
         SPACE                                                                  
* READ BATCH MOVE RECORD FROM GENDIR/GENFIL                                     
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING EZMKEY,R4                                                        
         MVC   EZMKID,=C'ZM'                                                    
         MVC   KEYSAVE,KEY                                                      
         SPACE                                                                  
         MVI   DATADISP+1,42                                                    
         MVI   LKEY+1,32                                                        
         SPACE                                                                  
         LA    R2,MOVTABLN                                                      
         L     R3,AMOVTAB                                                       
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',KEYSAVE,KEY                   
         SPACE                                                                  
         USING MOVTABD,R3                                                       
         SPACE                                                                  
         SR    R5,R5                                                            
         SPACE                                                                  
LIST010  CLC   KEY(2),KEYSAVE                                                   
         BNE   LIST035                                                          
         SPACE                                                                  
         AP    TMOVREC,=P'1'                                                    
         SPACE                                                                  
         MVC   HALF,EZMKDTP        BATCH DATE                                   
         XC    HALF,=X'FFFF'                                                    
         GOTO1 DATCON,DMCB,(2,HALF),(0,DUB)                                     
         SPACE                                                                  
         CLC   RQDTSTR,DUB         THIS IN REQUEST RANGE                        
         BH    LIST030               NO                                         
         CLC   RQDTEND,DUB         THIS IN REQUEST RANGE                        
         BL    LIST030               NO                                         
         SPACE                                                                  
         AP    UMOVREC,=P'1'                                                    
         SPACE                                                                  
         LA    R5,1(,R5)                                                        
         SPACE                                                                  
         MVC   MOVBDTE,DUB                                                      
         MVC   MOVINV,EZMKINV      INVOICE NUMBER                               
         MVC   MOVCALL,EZMKSTA     4 CALL LETTERS, MEDIA                        
         CLI   MOVCALL+3,C'.'                                                   
         BNE   *+8                                                              
         MVI   MOVCALL+3,C' '                                                   
         SPACE                                                                  
         L     R6,AIO1                                                          
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'GENFIL',KEY+36,(R6),DMWORK            
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING EZMDTAEL,R6                                                      
         SPACE                                                                  
         MVC   MOVMEDIA,EZMMEDIA   SAVE MEDIA                                   
         MVC   MOVNET,EZMNET            NETWORK                                 
         MVC   MOVMOS,EZMMOS            MONTH OF SERVICE                        
         MVC   MOVOAGY,EZMFAGY          OLD AGY                                 
         OC    MOVOAGY,SPACES                                                   
         MVC   MOVNAGY,EZMTAGY          NEW AGY                                 
         MVC   MOVSRCE,EZMSRCE          SOURCE                                  
         MVC   MOVDATE,EZMDATE                                                  
         MVC   MOVTIME,EZMTIME                                                  
         SPACE                                                                  
         OC    MOVCOM,SPACES                                                    
         SPACE                                                                  
         LA    R3,MOVNEXT                                                       
         BCT   R2,LIST030                                                       
         DROP  R3                                                               
         DC    H'0'                                                             
         SPACE                                                                  
LIST030  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'GENDIR',KEYSAVE,KEY                   
         B     LIST010                                                          
         SPACE                                                                  
LIST035  GOTO1 =V(QSORT),DMCB,AMOVTAB,(R5),L'MOVENT,10,1                        
         CVD   R5,DUB                                                           
         CP    DUB,UMOVREC                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
LIST100  L     R1,=A(EZBILLF)                                                   
         GET   (1),EZBILLI                                                      
         AP    RINVCT,=P'1'                                                     
         SPACE                                                                  
* SEE IF THIS IS IN REQUESTED PERIOD *                                          
         SPACE                                                                  
         CLC   RQDTSTR,EZBTODAY                                                 
         BH    LIST100                                                          
         CLC   RQDTEND,EZBTODAY                                                 
         BL    LIST100                                                          
         SPACE                                                                  
         OC    EZBCALL,SPACES                                                   
         SPACE                                                                  
* SEE IF MOVED TO A NEW AGENCY                                                  
         SPACE                                                                  
LIST104  LA    R2,MOVTABLN                                                      
         L     R3,AMOVTAB                                                       
         SPACE                                                                  
         USING MOVTABD,R3                                                       
         SPACE                                                                  
LIST105  DS    0H                                                               
         CLI   MOVOAGY,0           END OF TABLE                                 
         BE    LIST115                                                          
         CLI   MOVUSED,1           USED ?                                       
         BE    LIST110             YES, BYPASS                                  
         SPACE                                                                  
         CLC   MOVINV,EZBINVNO     SAME INVOICE ?                               
         BNE   LIST110                                                          
         SPACE                                                                  
         CLC   MOVBDTE,EZBTODAY    SAME BATCH DATE ?                            
         BNE   LIST110                                                          
         SPACE                                                                  
         MVC   WORK(L'MOVCOM),EZBAGY                                            
         OC    WORK(L'MOVCOM),SPACES                                            
*        OC    EZBAGY(L'MOVCOM),SPACES                                          
         SPACE                                                                  
         CLC   MOVCOM(12),WORK        SAME AGY/SRC                              
         BNE   LIST110                                                          
         SPACE                                                                  
         CLC   MOVCOM+13(13),WORK+13  SAME CALL/NET/MOS ?                       
         BNE   LIST110                                                          
         SPACE                                                                  
         MVC   EZBAGY,MOVNAGY      MOVE NEW AGENCY                              
         OC    EZBAGY,SPACES                                                    
         MVI   MOVUSED,1           TURN ON USED FLAG                            
         B     LIST104             SEE IF MOVED AGAIN                           
         SPACE                                                                  
LIST110  LA    R3,MOVNEXT                                                       
         BCT   R2,LIST105                                                       
         DROP  R3                                                               
         SPACE                                                                  
LIST115  MVC   EZBORGMD,EZBCALL+4  SAVE ORIG MEDIA                              
         SPACE                                                                  
*        CLI   EZBMEDIA,C'R'       CONVERT RADIO                                
*        BE    LIST120                                                          
*        CLI   EZBMEDIA,C'X'       CONVERT NETWORK RADIO                        
*        BE    LIST120                                                          
         CLI   EZBCALL+4,C'N'      FORCE TO NET                                 
*        BE    LIST122                                                          
         BE    LIST120                                                          
         CLI   EZBCALL+4,C'S'      FORCE TO NET                                 
*        BE    LIST122                                                          
         BE    LIST120                                                          
         CLI   EZBCALL+4,C'C'      FORCE TO NET                                 
         BE    LIST122                                                          
         CLI   EZBCALL+4,C'T'      FORCE TO TV                                  
         BE    LIST120                                                          
         CLI   EZBCALL+4,C' '      FORCE TO TV                                  
         BE    LIST120                                                          
         CLI   EZBCALL+4,0         FORCE TO TV                                  
         BE    LIST120                                                          
         CLI   EZBCALL+4,C'X'      FORCE NETWORK RADIO TO TV                    
         BE    LIST120                                                          
         CLI   EZBCALL+4,C'A'      FORCE RADIO TO TV                            
         BE    LIST120                                                          
         CLI   EZBCALL+4,C'F'      FORCE RADIO TO TV                            
         BE    LIST120                                                          
         CLI   EZBCALL+4,C'R'      FORCE RADIO TO TV                            
         BE    LIST120                                                          
         MVC   P(91),EZBAGY                                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   P+EZBCALL+4-EZBAGY,C'*'                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LIST100                                                          
         DC    H'0'                                                             
*        BNE   LIST130                                                          
         SPACE                                                                  
LIST120  MVI   EZBMEDIA,C'T'                                                    
         B     LIST130                                                          
LIST122  MVI   EZBMEDIA,C'N'                                                    
         SPACE                                                                  
LIST130  OC    RUID,RUID           SPECIAL REQUEST FOR 1 AGENCY                 
         BZ    LIST140                                                          
         CLC   EZBAGY,RUID                                                      
         BNE   LIST100                                                          
         SPACE                                                                  
LIST140  OC    RQSTA,RQSTA         SPECIAL REQUEST FOR 1 STATION                
         BZ    LIST150                                                          
         CLC   EZBCALL,RQSTA                                                    
         BNE   LIST100                                                          
         SPACE                                                                  
LIST150  OC    FTRMOS,FTRMOS       FILTERING ON MONTH OF SERVICE                
         BZ    LIST154                                                          
         CLC   EZBMOS,FTRMOS                                                    
         BNE   LIST100                                                          
         SPACE                                                                  
LIST154  DS    0H                                                               
         LA    R0,CABNOBCT                                                      
         L     R1,=A(CABNOBIL)                                                  
         A     R1,RELO                                                          
LIST160  CLC   0(5,R1),EZBCALL     THIS EXEMPT CALL LETTERS                     
         BE    LIST164                                                          
         LA    R1,L'CABNOBIL(,R1)                                               
         BCT   R0,LIST160                                                       
         B     LIST166                                                          
         SPACE                                                                  
LIST164  AP    CABYPASS,=P'1'                                                   
         AP    5(3,R1),=P'1'                                                    
         B     LIST100                                                          
         SPACE                                                                  
LIST166  DS    0H                                                               
         AP    UINVCT,=P'1'                                                     
         SPACE                                                                  
         OC    RQSTA,RQSTA         SPECIAL REQUEST FOR 1 STATION                
         BNZ   LIST170              YES, NO AGENCY RECS                         
         CLI   AGYSTASW,C'S'       SPECIAL REQUEST FOR STATIONS ONLY            
         BE    LIST170              YES, NO AGENCY RECS                         
         SPACE                                                                  
* PUT OUT AN AGENCY BILLING RECORD                                              
         SPACE                                                                  
         MVI   EZBILLT,C'A'        AGENCY BILLING                               
         MVC   EZBILLS,EZBAGY                                                   
         SPACE                                                                  
         BAS   RE,CKBILL           GO CHECK IF BILLABLE & LINKED                
         BNE   LIST170              NOT BILLABLE                                
         SPACE                                                                  
         GOTO1 =V(SORTER),DMCB,=C'PUT',EZBILL                                   
         AP    SRECCT,=P'1'                                                     
         SPACE                                                                  
         CLI   FTRTRACE,C'Y'       TRACE                                        
         BNE   LIST170                                                          
         SPACE                                                                  
         L     R4,HEADHOOK                                                      
         L     R5,SPECS                                                         
         SR    R0,R0                                                            
         ST    R0,SPECS                                                         
         L     R0,HEADHOOK                                                      
         MVC   P,EZBILL                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         ST    R4,HEADHOOK                                                      
         ST    R5,SPECS                                                         
         SPACE                                                                  
LIST170  OC    RUID,RUID           SPECIAL REQUEST FOR 1 AGENCY                 
         BNZ   LIST174              YES, NO STATION RECS                        
         CLI   AGYSTASW,C'A'       SPECIAL REQUEST FOR AGENCIES ONLY            
         BE    LIST174              YES, NO STATION RECS                        
         SPACE                                                                  
* PUT OUT A STATION BILLING RECORD                                              
         SPACE                                                                  
         MVI   EZBILLT,C'S'        STATION BILLING                              
         MVC   EZBILLS,SPACES                                                   
         MVC   EZBILLS(5),EZBCALL                                               
         SPACE                                                                  
         BAS   RE,CKBILL           GO CHECK IF BILLABLE & LINKED                
         BNE   LIST174              NOT BILLABLE                                
         SPACE                                                                  
         GOTO1 =V(SORTER),DMCB,=C'PUT',EZBILL                                   
         AP    SRECCT,=P'1'                                                     
         SPACE                                                                  
         LA    RE,STATABLN                                                      
         L     RF,=A(STATABLE)                                                  
         OC    0(5,RF),0(RF)                                                    
         BZ    *+24                                                             
         CLC   0(5,RF),EZBCALL                                                  
         BE    *+20                                                             
         LA    RF,8(RF)                                                         
         BCT   RE,*-24                                                          
         DC    H'0'                                                             
         MVC   0(5,RF),EZBCALL                                                  
         AP    5(3,RF),=P'1'                                                    
         SPACE                                                                  
         CLI   FTRTRACE,C'Y'       TRACE                                        
         BNE   LIST174                                                          
         SPACE                                                                  
         L     R4,HEADHOOK                                                      
         L     R5,SPECS                                                         
         SR    R0,R0                                                            
         ST    R0,SPECS                                                         
         L     R0,HEADHOOK                                                      
         MVC   P,EZBILL                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         ST    R4,HEADHOOK                                                      
         ST    R5,SPECS                                                         
LIST174  B     LIST100                                                          
         SPACE                                                                  
* END OF FILE FOR BILLING *                                                     
         SPACE                                                                  
LIST190  L     R2,=A(EZBILLF)                                                   
         CLOSE ((R2),)                                                          
         FREEPOOL (R2)                                                          
         SPACE                                                                  
         CP    SRECCT,=P'0'                                                     
         BE    LIST210                                                          
         SPACE                                                                  
         GOTO1 =A(NEXTSET)                                                      
         SPACE                                                                  
LIST200  GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         SPACE                                                                  
         L     R2,4(R1)            GET REC ADDRESS                              
         ST    R2,SORTRECA                                                      
         LTR   R2,R2                                                            
         BNZ   LIST230                                                          
         SPACE                                                                  
* E-O-F -- SET SVREC TO X'FF'                                                   
         SPACE                                                                  
         CLI   FIRSTSW,0           TEST FIRST TIME                              
         BNE   LIST220              NO                                          
         SPACE                                                                  
         CP    UINVCT,=P'0'        ANY RECS READ                                
         BH    LIST220              YES, ALL MUST HAVE BEEN ZERO                
         SPACE                                                                  
LIST210  MVC   P2(20),=C'* NO INPUT RECORDS *'                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         L     R2,=A(EZWRKT)                                                    
         CLOSE ((R2),)                                                          
         FREEPOOL (R2)                                                          
         SPACE                                                                  
         L     R2,=A(EZSOLM)                                                    
         CLOSE ((R2),)                                                          
         FREEPOOL (R2)                                                          
         SPACE                                                                  
         L     R2,=A(EZSOLT)                                                    
         CLOSE ((R2),)                                                          
         FREEPOOL (R2)                                                          
         SPACE                                                                  
         L     R2,=A(EZREG)                                                     
         CLOSE ((R2),)                                                          
         FREEPOOL (R2)                                                          
         SPACE                                                                  
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         SPACE                                                                  
         ICM   RF,15,UTL                                                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   4(1,RF),SAVSYS                                                   
         SPACE                                                                  
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(20),=C'* NO INPUT RECORDS *'                             
         LA    R2,CONRECH                                                       
         GOTO1 ERREX2                                                           
         SPACE                                                                  
LIST220  MVI   FIRSTSW,X'FF'                                                    
         B     LIST232                                                          
         SPACE                                                                  
LIST230  CLC   EZBILLT(9),0(R2)    SAME TYPE AND LINKED AGENCY                  
         BNE   LIST232                                                          
         SPACE                                                                  
* CHECK FOR DUPLICATES                                                          
         SPACE                                                                  
         CLC   EZBAGY,EZBAGY-EZBILLT(R2)                                        
         BNE   LIST500              NO, JUST ADD TO COUNTERS                    
         SPACE                                                                  
         CLC   EZBCALL,EZBCALL-EZBILLT(R2) SAME CALL LETTERS                    
         BNE   LIST500                      NO, JUST ADD TO COUNTERS            
         SPACE                                                                  
         CLC   EZBMOS,EZBMOS-EZBILLT(R2)   SAME MONTH OF SERVICE                
         BNE   LIST500                      NO, JUST ADD TO COUNTERS            
         SPACE                                                                  
         CLC   EZBINVNO,EZBINVNO-EZBILLT(R2)                                    
         BNE   LIST500              NO, JUST ADD TO COUNTERS                    
         SPACE                                                                  
         AP    DUPCT,=P'1'         TOTAL DUPLICATES                             
         AP    DUPCTB,=P'1'        BILL                                         
         SPACE                                                                  
         CLI   FTRDUP,C'Y'         TRACE DUPS                                   
         BE    LIST231                                                          
         CLI   FTRTRACE,C'Y'       TRACE                                        
         BNE   LIST200                                                          
         SPACE                                                                  
LIST231  L     R4,HEADHOOK                                                      
         L     R5,SPECS                                                         
         SR    R0,R0                                                            
         ST    R0,SPECS                                                         
         ST    R0,HEADHOOK                                                      
         SPACE                                                                  
         MVC   P+1(3),=C'DUP'                                                   
         MVC   P+5(100),EZBILL-EZBILLT(R2)                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVC   P+5(100),EZBILL                                                  
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         ST    R4,HEADHOOK                                                      
         ST    R5,SPECS                                                         
         SPACE                                                                  
         B     LIST200              BYPASS                                      
         SPACE                                                                  
LIST232  CLI   FIRSTSW,0           FIRST TIME THRU                              
         BE    LIST400              YES                                         
         SPACE                                                                  
         ZAP   WORKCT1,=P'0'                                                    
         ZAP   WORKCT2,=P'0'                                                    
         ZAP   WORKCT3,=P'0'                                                    
         ZAP   WORKCT4,=P'0'                                                    
         ZAP   WORKCH1,=P'0'                                                    
         ZAP   WORKCH2,=P'0'                                                    
         ZAP   WORKCH3,=P'0'                                                    
         ZAP   WORKCH4,=P'0'                                                    
         SPACE                                                                  
         AP    INVNUM,=P'1'        ADD TO OUR INVOICE NUMBER                    
         MVI   BILLSW,C'N'                                                      
         MVI   SVBILL,C'N'                                                      
         MVI   SVRATE,C'1'                                                      
         MVC   SVFRMT,=C'A '                                                    
         CLI   EZBILLT,C'R'        THIS A LINKED STATION                        
         BE    *+12                                                             
         CLI   EZBILLT,C'S'        THIS A STATION                               
         BNE   *+10                                                             
         MVC   SVFRMT,=C'ST'                                                    
         SPACE                                                                  
         XC    REGREC,REGREC       CLEAR REGISTER RECORD AREA                   
         MVC   REGTYP,EZBILLT                                                   
         MVC   REGID,EZBILLT+1                                                  
         MVC   REGSOL,SVCSTC                                                    
         MVC   REGINVNO,INVNUM                                                  
         SPACE                                                                  
* READ BILLING RECORD FROM GENDIR/FIL                                           
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING EZBKEY,R4                                                        
         MVC   EZBKID,=C'ZB'                                                    
         MVC   EZBKTYP,EZBILLT                                                  
         SPACE                                                                  
         CLI   EZBILLT,C'R'        THIS A LINKED STATION                        
         BNE   *+8                                                              
         MVI   EZBKTYP,C'S'                                                     
         SPACE                                                                  
         CLI   EZBILLT,C'L'        THIS A LINKED AGENCY                         
         BNE   *+8                                                              
         MVI   EZBKTYP,C'A'                                                     
         SPACE                                                                  
         MVC   EZBKIDN,EZBILLS                                                  
         MVC   KEYSAVE,KEY                                                      
         MVC   DATADISP,=H'42'                                                  
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',KEYSAVE,KEY                   
         SPACE                                                                  
         CLC   KEY(32),KEYSAVE                                                  
         BNE   LIST240                                                          
         SPACE                                                                  
         L     R6,AIO1                                                          
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'GENFIL',KEY+36,(R6),DMWORK            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVI   BILLSW,C'Y'                                                      
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING EZBDTAEL,R6                                                      
         L     R1,=A(SVATTN)                                                    
         MVC   0(30,R1),EZBATTN                                                 
         MVC   30(30,R1),EZBNAME                                                
         MVC   60(92,R1),EZBADR1     92                                         
*        MVC   SVATTN,EZBATTN        30                                         
*        MVC   SVNAME,EZBNAME        30                                         
*        MVC   SVADR1,EZBADR1        30                                         
*        MVC   SVADR2,EZBADR2        30                                         
*        MVC   SVCITY,EZBCITY        20                                         
*        MVC   SVST,EZBST            10                                         
*        MVC   SVZIP,EZBZIP           2                                         
         MVC   SVBILL,EZBILL                                                    
         MVC   SVBILLS,EZBILLS                                                  
         MVC   SVRATE,EZBRATE                                                   
         MVC   SVFRMT,EZBFRMT                                                   
         MVC   SVINAC,EZBINAC                                                   
         MVC   SVCSTC,EZBCSTC                                                   
         SPACE                                                                  
         OC    SVCSTC,SVCSTC                                                    
         BNZ   LIST238                                                          
         SPACE                                                                  
         MVC   SVCSTC(4),=C'4650'                                               
         SPACE                                                                  
LIST238  MVC   SVARCD,EZBARCD                                                   
         SPACE                                                                  
         OC    SVARCD,SVARCD                                                    
         BNZ   LIST240                                                          
         SPACE                                                                  
         MVC   SVARCD(4),=C'1170'                                               
*                                                                               
LIST240  CLI   EZBILLT,C'A'        AGENCY                                       
         BE    *+12                                                             
         CLI   EZBILLT,C'L'        LINKED AGENCY                                
         BNE   LIST250                                                          
         GOTO1 =A(AGYC)            CALC AGENCY CHARGES                          
         B     LIST305                                                          
         SPACE                                                                  
LIST250  CLI   EZBILLT,C'R'        LINKED STATION BILLING                       
         BE    *+14                                                             
         CLI   EZBILLT,C'S'        STATION BILLING                              
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
         LA    R0,SCHGTBLN                                                      
         L     R1,=A(SCHGTBL)                                                   
         SPACE                                                                  
         ZAP   WORKCT,INVCT                                                     
         LA    R2,WORKCT1                                                       
         SPACE                                                                  
LIST254  CP    WORKCT,0(3,R1)                                                   
         BNH   LIST256                                                          
         AP    INVCHRG,6(4,R1)                                                  
         SP    WORKCT,0(3,R1)                                                   
         ZAP   0(4,R2),0(3,R1)                                                  
         MVC   4(4,R2),6(R1)                                                    
         LA    R1,10(,R1)                                                       
         LA    R2,8(,R2)                                                        
         BCT   R0,LIST254                                                       
         DC    H'0'                                                             
LIST256  ZAP   DUB,3(3,R1)                                                      
         ZAP   0(4,R2),WORKCT                                                   
         CVB   R0,DUB                                                           
         ZAP   DUB,WORKCT                                                       
         CVB   RF,DUB                                                           
         SR    RE,RE                                                            
         MR    RE,R0                                                            
         CVD   RF,DUB                                                           
         AP    INVCHRG,DUB                                                      
         AP    4(4,R2),DUB                                                      
         SPACE                                                                  
         AP    SINVCT,INVCT                                                     
         AP    SINVSPT,INVSPT                                                   
         AP    SINVDOL,INVDOL                                                   
         AP    SINVCHRG,INVCHRG                                                 
         SPACE                                                                  
         LA    R0,12                                                            
         LA    R1,MEDCTS                                                        
LIST260  LA    RE,12                                                            
         LA    RF,SMEDCTS                                                       
LIST264  CLC   0(1,R1),0(RF)       SAME MEDIA                                   
         BE    LIST268                                                          
         CLI   0(RF),0                                                          
         BE    LIST266                                                          
         LA    RF,20(,RF)                                                       
         BCT   RE,LIST264                                                       
         DC    H'0'                                                             
LIST266  MVC   0(1,RF),0(R1)                                                    
         SPACE                                                                  
LIST268  AP    1(4,RF),1(4,R1)                                                  
         AP    5(4,RF),5(4,R1)                                                  
         AP    9(6,RF),9(6,R1)                                                  
         AP    15(5,RF),15(5,R1)                                                
         LA    R1,20(,R1)                                                       
         CLI   0(R1),0                                                          
         BE    *+8                                                              
         BCT   R0,LIST260                                                       
         SPACE                                                                  
         LA    R0,12                                                            
         LA    R1,MEDCTS                                                        
LIST280  LA    RE,12                                                            
         LA    RF,TMEDCTS                                                       
LIST284  CLC   0(1,R1),0(RF)       SAME MEDIA                                   
         BE    LIST286                                                          
         CLI   0(RF),0                                                          
         BE    LIST286                                                          
         LA    RF,20(,RF)                                                       
         BCT   RE,LIST284                                                       
         DC    H'0'                                                             
LIST286  MVC   0(1,RF),0(R1)                                                    
         AP    1(4,RF),1(4,R1)     INVOICES                                     
         AP    5(4,RF),5(4,R1)     SPOTS                                        
         AP    9(6,RF),9(6,R1)     GROSS DOLLARS                                
         AP    15(5,RF),15(5,R1)   CHARGES                                      
         LA    R1,20(,R1)                                                       
         CLI   0(R1),0                                                          
         BE    *+8                                                              
         BCT   R0,LIST280                                                       
         SPACE                                                                  
         AP    TINVCT,INVCT                                                     
         AP    TINVSPT,INVSPT                                                   
         AP    TINVDOL,INVDOL                                                   
         AP    TINVCHRG,INVCHRG                                                 
         SPACE                                                                  
         MVI   FORCEHED,C'Y'                                                    
         SPACE                                                                  
LIST296  DS   0H                                                                
         SPACE                                                                  
         LA    R2,WORKCT1                                                       
         LA    R3,4                                                             
         ZAP   INVCHRG,=P'0'                                                    
LIST298  DS   0H                                                                
         AP    INVCHRG,4(4,R2)                                                  
         SPACE                                                                  
         LA    R2,8(,R2)                                                        
         BCT   R3,LIST298                                                       
         SPACE                                                                  
         LA    R2,WORKCT1                                                       
         LA    R3,3                                                             
         L     R4,=A(SRATETBL)                                                  
         SPACE                                                                  
         ZAP   INVCHRG,=P'0'                                                    
LIST300  DS   0H                                                                
         MVC   PCOUNT+5(11),0(R4)                                               
         MVC   PINVCHG+5(4),11(R4)                                              
         EDIT  (P4,0(R2)),(6,PINVS),0,COMMAS=YES                                
         EDIT  (P4,4(R2)),(9,PCHRG),2,COMMAS=YES,FLOAT=$                        
         SPACE                                                                  
         AP    INVCHRG,4(4,R2)                                                  
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         LA    R2,8(,R2)                                                        
         LA    R4,15(,R4)                                                       
         BCT   R3,LIST300                                                       
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
LIST305  MVC   PINVS-2(8),DASHES                                                
         MVC   PCHRG-2(11),DASHES                                               
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVC   PINVCHG(13),=C'INVOICE TOTAL'                                    
         EDIT  INVCT,(6,PINVS),0,COMMAS=YES                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVC   PCHRG-25(22),=C'PLEASE PAY THIS AMOUNT'                          
         EDIT  INVCHRG,(9,PCHRG),2,COMMAS=YES,FLOAT=$                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVC   PCHRG(9),=C'_________'                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVI   SPACING,2                                                        
         MVC   PCHRG(9),=C'_________'                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         L     R2,AIO2                                                          
         LA    R3,2000/20                                                       
         OC    0(12,R2),0(R2)      ANY LINKED TO PRINT                          
         BZ    LIST340                                                          
         SPACE                                                                  
         MVC   PINVCHG(9),=C'ID/OFFICE'                                         
         MVC   PINVS-2(8),=C'INVOICES'                                          
         MVC   PCHRG-13(10),=C'% OF TOTAL'                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVC   PINVCHG(9),DASHES                                                
         MVC   PINVS-2(8),DASHES                                                
         MVC   PCHRG-13(11),DASHES                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
LIST310  MVC   PINVCHG+1(8),0(R2)                                               
         ZAP   ELEM(12),8(4,R2)                                                 
         MP    ELEM(12),=P'1000'                                                
         DP    ELEM(12),INVCT                                                   
         EDIT  (P4,8(R2)),(6,PINVS),0,COMMAS=YES                                
         SPACE                                                                  
         EDIT  (P8,ELEM),(5,PCHRG-8),1                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,20(,R2)                                                       
         OC    0(12,R2),0(R2)      ANY LINKED TO PRINT                          
         BZ    LIST330                                                          
         BCT   R3,LIST310                                                       
         SPACE                                                                  
LIST330  MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
LIST340  MVC   PINVCHG(27),=C'PLEASE REMIT TO E I EXPRESS'                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVC   PINVCHG+16(17),=C'P.O. BOX 9189 GPO'                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVC   PINVCHG+16(23),=C'NEW YORK, NY 10087-9189'                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
* PUT TO EZREG FOR REGISTER PRINTING LATER                                      
         SPACE                                                                  
         ZAP   REGINV,INVCT                                                     
         ZAP   REGINVT,INVCTT                                                   
         ZAP   REGINVR,INVCTR                                                   
         ZAP   REGINVC,INVCTC                                                   
         ZAP   REGINVX,INVCTX                                                   
         ZAP   REGSPT,INVSPT                                                    
         ZAP   REGDOL,INVDOL                                                    
         ZAP   REGDUE,INVCHRG                                                   
         ZAP   REGDUPCT,DUPCTB                                                  
         ZAP   DUPCTB,=P'0'                                                     
         SPACE                                                                  
         L     R1,=A(EZREG)                                                     
         PUT   (1),REGREC                                                       
         SPACE                                                                  
* PUT OUT MASTER BILLING TO EZSOL FOR ACCOUNTING SYSTEM TRANSFER                
         SPACE                                                                  
         LA    R2,SOLMAS                                                        
         L     R3,=A(MASTAB)                                                    
         SPACE                                                                  
         BAS   RE,BLDSOL                                                        
         SPACE                                                                  
         L     R1,=A(EZSOLM)                                                    
         PUT   (1),SOLMAS-4                                                     
         AP    EZSOLMCT,=P'1'                                                   
         SPACE                                                                  
* PUT OUT INVOICE TO EZSOL FOR ACCOUNTING SYSTEM TRANSFER                       
         SPACE                                                                  
         UNPK  DUB,INVCHRG                                                      
         OI    DUB+7,X'F0'                                                      
         MVC   INVAMT(6),DUB                                                    
         MVI   INVAMT+6,C'.'                                                    
         MVC   INVAMT+7(2),DUB+6                                                
         LA    R2,SOLINV                                                        
         L     R3,=A(INVTAB)                                                    
         BAS   RE,BLDSOL                                                        
         SPACE                                                                  
         LH    R1,SOLINV-4                                                      
         SH    R1,=H'2'                                                         
         STH   R1,SOLINV-4                                                      
         SPACE                                                                  
         L     R1,=A(EZWRKT)                                                    
         PUT   (1),SOLINV-4                                                     
         AP    EZSOLTCT,=P'1'                                                   
         SPACE                                                                  
         LA    R2,SOLINV                                                        
         L     R3,=A(TRNTAB)                                                    
         BAS   RE,BLDSOL                                                        
         SPACE                                                                  
         L     R1,=A(EZWRKT)                                                    
         PUT   (1),SOLINV-4                                                     
         AP    EZSOLTCT,=P'1'                                                   
         SPACE                                                                  
         B     LIST370                                                          
         SPACE                                                                  
* ADD TO BYPASSED CHARGES *                                                     
         SPACE                                                                  
LIST350  AP    BYPASSCT,=P'1'                                                   
         AP    BYPASSCH,INVCHRG                                                 
         SPACE                                                                  
LIST370  CLI   FIRSTSW,X'FF'       EOF?                                         
         BE    LIST600              YES                                         
         SPACE                                                                  
LIST400  MVI   FIRSTSW,1           SET NOT FIRST TIME THRU                      
         SPACE                                                                  
         LA    R0,12                                                            
         LA    R1,MEDCTS                                                        
LIST410  MVI   0(R1),0                                                          
         ZAP   1(4,R1),=P'0'                                                    
         ZAP   5(4,R1),=P'0'                                                    
         ZAP   9(6,R1),=P'0'                                                    
         ZAP   15(5,R1),=P'0'                                                   
         LA    R1,20(,R1)                                                       
         BCT   R0,LIST410                                                       
         SPACE                                                                  
         ZAP   INVCT,=P'0'                                                      
         ZAP   INVCTT,=P'0'                                                     
         ZAP   INVCTR,=P'0'                                                     
         ZAP   INVCTC,=P'0'                                                     
         ZAP   INVCTX,=P'0'                                                     
         ZAP   INVSPT,=P'0'                                                     
         ZAP   INVDOL,=P'0'                                                     
         ZAP   INVCHRG,=P'0'                                                    
         SPACE                                                                  
         L     RE,AIO2             CLEAR COMBINED BILLING AREA                  
         LA    RF,2000                                                          
         XCEF                                                                   
         SPACE                                                                  
* MOVE DATA FROM SORT TO EZBILL AREA *                                          
         SPACE                                                                  
LIST500  LA    R0,EZBILL                                                        
         L     R2,SORTRECA                                                      
         LA    R1,(EZBILLED-EZBILD)+1+8                                         
         LR    R3,R1                                                            
         MVCL  (R0),(R2)                                                        
         SPACE                                                                  
         LA    R0,12                                                            
         LA    R1,MEDCTS                                                        
         SPACE                                                                  
         MVC   BYTE,EZBMEDIA                                                    
         SPACE                                                                  
         BAS   RE,SEPCT                                                         
         SPACE                                                                  
         CLI   BYTE,C'R'           FORCE MEDIA R TO MEDIA T                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*        BNE   *+12                                                             
*        MVI   BYTE,C'T'                                                        
*        B     LIST510                                                          
         SPACE                                                                  
         CLI   BYTE,C'X'           FORCE MEDIA X TO MEDIA T                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*        BNE   *+12                                                             
*        MVI   BYTE,C'T'                                                        
*        B     LIST510                                                          
         SPACE                                                                  
         CLI   EZBCALL+4,C'C'      THIS REALLY CABLE                            
         BNE   LIST510                                                          
         MVI   BYTE,C'C'                                                        
         SPACE                                                                  
LIST510  CLC   BYTE,0(R1)                                                       
         BE    LIST530                                                          
         CLI   0(R1),0                                                          
         BE    LIST520                                                          
         LA    R1,20(,R1)                                                       
         BCT   R0,LIST510                                                       
         DC    H'0'                                                             
         SPACE                                                                  
LIST520  MVC   0(1,R1),BYTE                                                     
         SPACE                                                                  
LIST530  AP    INVCT,=P'1'                                                      
         AP    1(4,R1),=P'1'                                                    
         PACK  DUB,EZBNSPTS                                                     
         AP    INVSPT,DUB                                                       
         AP    5(4,R1),DUB                                                      
         PACK  DUB,EZBGDOL                                                      
         AP    INVDOL,DUB                                                       
         AP    9(6,R1),DUB                                                      
         SPACE                                                                  
         CLI   EZBILLT,C'L'        LINKED AGENCY BILLING                        
         BE    LIST540                                                          
         CLI   EZBILLT,C'R'        LINKED STATION BILLING                       
         BNE   LIST200              GET NEXT REC                                
         SPACE                                                                  
LIST540  BAS   RE,ADLINK           GO ADD TO LINKED CTRS                        
         SPACE                                                                  
         B     LIST200             GET NEXT REC                                 
         SPACE                                                                  
LIST600  GOTO1 =V(SORTER),DMCB,=C'END'                                          
         SPACE                                                                  
         L     R2,=A(EZWRKT)                                                    
         CLOSE ((R2),)                                                          
         FREEPOOL (R2)                                                          
         SPACE                                                                  
         L     R2,=A(EZWRKTI)                                                   
         OPEN  ((R2),(INPUT))                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         LA    R4,1                                                             
         L     R1,=A(EZWRKTI)                                                   
         GET   (1),SOLINV-4                                                     
         SPACE                                                                  
         LH    R1,SOLINV-4                                                      
         SH    R1,=H'2'                                                         
         STH   R1,SOLINV-4                                                      
         SPACE                                                                  
         L     R1,=A(EZSOLT)                                                    
         PUT   (1),SOLINV-4                                                     
         SPACE                                                                  
         ZAP   DUB,SINVCHRG                                                     
         AP    DUB,AINVCHRG                                                     
         XC    SOLINV(256),SOLINV                                               
         MVI   SOLINV-3,4+18                                                    
         MVI   SOLINV,C','                                                      
         MVI   SOLINV+1,C'"'                                                    
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(12),DUB                                                     
         MVC   SOLINV+2(10),WORK                                                
         MVI   SOLINV+12,C'.'                                                   
         MVC   SOLINV+13(2),WORK+10                                             
         MVI   SOLINV+15,C'"'                                                   
         MVC   SOLINV+16(2),=X'0D25'                                            
         L     R1,=A(EZSOLT)                                                    
         PUT   (1),SOLINV-4                                                     
         SPACE                                                                  
LIST620  L     R1,=A(EZWRKTI)                                                   
         GET   (1),SOLINV-4                                                     
         L     R1,=A(EZSOLT)                                                    
         PUT   (1),SOLINV-4                                                     
         LA    R4,1(,R4)                                                        
         B     LIST620                                                          
         SPACE                                                                  
LIST640  LA    R1,1                                                             
         CR    R4,R1               MUST HAVE MORE THAN 1 RECORD                 
         BH    *+6                                                              
         DC    H'0'                                                             
         CVD   R4,DUB                                                           
         CP    DUB,EZSOLTCT                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =A(PRTOT)                                                        
         B     EXIT                                                             
         EJECT                                                                  
* CT ORIGINAL MEDIAS  *                                                         
         SPACE                                                                  
SEPCT    DS    0H                                                               
         CLI   EZBORGMD,C'N'                                                    
         BE    SEPCT06                                                          
         CLI   EZBORGMD,C'T'                                                    
         BNE   SEPCT10                                                          
SEPCT06  AP    INVCTT,=P'1'                                                     
         B     SEPCTX                                                           
SEPCT10  CLI   EZBORGMD,C'A'                                                    
         BE    SEPCT20                                                          
         CLI   EZBORGMD,C'F'                                                    
         BE    SEPCT20                                                          
         CLI   EZBORGMD,C'R'                                                    
         BNE   SEPCT30                                                          
SEPCT20  AP    INVCTR,=P'1'                                                     
         B     SEPCTX                                                           
SEPCT30  CLI   EZBORGMD,C'C'                                                    
         BNE   SEPCT40                                                          
         AP    INVCTC,=P'1'                                                     
         B     SEPCTX                                                           
SEPCT40  CLI   EZBORGMD,C'X'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         AP    INVCTX,=P'1'                                                     
SEPCTX   BR    RE                                                               
         EJECT                                                                  
* BUILD RECS FOR SOL ACCTG SYSTEM *                                             
         SPACE                                                                  
BLDSOL   NTR1                                                                   
         SPACE                                                                  
         ST    R2,FULL                                                          
         XC    0(256,R2),0(R2)                                                  
SOL10    CLI   0(R3),0                                                          
         BE    SOL20                                                            
         ZIC   R4,0(R3)                                                         
         BCTR  R4,0                                                             
         ICM   R5,7,1(R3)                                                       
         MVI   0(R2),C'"'                                                       
         EX    R4,SOLMVC                                                        
         LA    RF,2(R2,R4)                                                      
         SPACE                                                                  
SOL14    CLI   0(RF),C' '                                                       
         BH    SOL16                                                            
         BCT   RF,SOL14                                                         
SOL16    LA    R2,1(,RF)                                                        
         MVI   0(R2),C'"'                                                       
         LA    R2,1(,R2)                                                        
SOL20    MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         LA    R3,4(,R3)                                                        
         CLI   0(R3),255                                                        
         BNE   SOL10                                                            
         MVC   0(2,R2),=X'0D25'                                                 
         LA    R2,2+4(,R2)                                                      
         L     R1,FULL                                                          
         SR    R2,R1                                                            
         SH    R1,=H'4'                                                         
         XC    2(2,R1),2(R1)                                                    
         STH   R2,0(R1)            SAVE LENGTH                                  
         B     EXIT                                                             
SOLMVC   MVC   1(0,R2),0(R5)                                                    
         EJECT                                                                  
* CHECK FOR LINKED IDS FOR COMBINED BILLING *                                   
         SPACE                                                                  
CKBILL   NTR1                                                                   
         SPACE                                                                  
         LA    R2,BILTABLN                                                      
         L     R3,ABILTAB                                                       
         USING BILTABD,R3                                                       
CKB010   CLI   BILTYP,0            END OF TABLE                                 
         BE    CKB030                                                           
         CLC   EZBILLT(9),BILTYP                                                
         BE    CKB020                                                           
         LA    R3,BILNEXT                                                       
         BCT   R2,CKB010                                                        
         DC    H'0'                                                             
CKB020   CLI   BILABL,C'Y'         BILLABLE                                     
         BNE   CKB030                                                           
         OC    BILNK,BILNK         IS THERE A LINK FIELD                        
         BZ    CKBX                                                             
         MVC   EZBILLS,BILNK                                                    
         SPACE                                                                  
         CLI   EZBILLT,C'A'        AGENCY BILLING                               
         BNE   CKB050                                                           
         MVI   EZBILLT,C'L'        LINKED AGENCY BILLING                        
         CR    RB,RB                                                            
         B     CKBX                                                             
         SPACE                                                                  
CKB030   DS    0H                                                               
         CLI   EZBILLT,C'S'        TEMP-PRODUCE BILLS FOR ALL STATIONS          
         BC    0,CKBX                                                           
         SPACE                                                                  
         CLI   EZBILLT,C'A'        TEMP-PRODUCE BILLS FOR ALL AGENCIES          
         BC    0,CKBX                                                           
         SPACE                                                                  
         CLI   EZBILLT,C'S'        TEMP-PRODUCE BILLS FOR ALL STATIONS          
         BE    CKB048                                                           
         SPACE                                                                  
         LA    R0,NOBILLCT                                                      
         L     R1,=A(NOBILLS)                                                   
CKB040   OC    0(10,R1),0(R1)      EMPTY ENTRY                                  
         BZ    CKB044                                                           
         CLC   EZBILLS,0(R1)                                                    
         BE    CKB046                                                           
         LA    R1,10(,R1)                                                       
         BCT   R0,CKB040                                                        
         DC    H'0'                                                             
CKB044   MVC   0(8,R1),EZBILLS                                                  
         SPACE                                                                  
CKB046   LH    RF,8(R1)            ADD TO INVOICE COUNT                         
         LA    RF,1(,RF)                                                        
         STH   RF,8(R1)                                                         
         SPACE                                                                  
CKB048   DS   0H                                                                
         LTR   RB,RB               SET CC UNEQ                                  
         B     CKBX                                                             
CKB050   DS   0H                                                                
         MVI   EZBILLT,C'R'        LINKED STATION BILLING                       
         CR    RB,RB                                                            
CKBX     B     EXIT                                                             
         DROP  R3                                                               
         SPACE 2                                                                
* ADD TO LINKED INVOICE COUNTS *                                                
         SPACE                                                                  
ADLINK   NTR1                                                                   
         SPACE                                                                  
         LA    R2,2000/20                                                       
         L     R3,AIO2                                                          
ADL010   OC    0(12,R3),0(R3)       END OF TABLE                                
         BZ    ADL050                                                           
         CLI   EZBILLT,C'L'                                                     
         BE    ADL014                                                           
         CLI   EZBILLT,C'R'                                                     
         BE    ADL016                                                           
         DC    H'0'                                                             
ADL014   CLC   EZBAGY,0(R3)                                                     
         BE    ADL080                                                           
         B     ADL020                                                           
ADL016   CLC   EZBCALL,0(R3)                                                    
         BE    ADL080                                                           
         SPACE                                                                  
ADL020   LA    R3,20(,R3)                                                       
         BCT   R2,ADL010                                                        
         DC    H'0'                                                             
ADL050   CLI   EZBILLT,C'L'                                                     
         BE    ADL054                                                           
         CLI   EZBILLT,C'R'                                                     
         BE    ADL056                                                           
         DC    H'0'                                                             
ADL054   MVC   0(L'EZBAGY,R3),EZBAGY                                            
         B     ADL060                                                           
ADL056   MVC   0(L'EZBCALL,R3),EZBCALL                                          
         SPACE                                                                  
ADL060   ZAP   8(4,R3),=P'0'                                                    
         SPACE                                                                  
ADL080   AP    8(4,R3),=P'1'                                                    
         SPACE                                                                  
ADLX     B     EXIT                                                             
         SPACE                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
BOTHERR  L     R1,=A(BOTHMS)                                                    
         B     ERREXIT2                                                         
USERIDBD L     R1,=A(INVUIDMS)                                                  
         B     ERREXIT2                                                         
USIDLER  L     R1,=A(UIDLENMS)                                                  
         SPACE                                                                  
ERREXIT2 GOTO1 ERREX2                                                           
         SPACE                                                                  
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
NUMERR   MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
BADATE   MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
INVAL    MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         SPACE                                                                  
TRAPERR  GOTO1 ERREX                                                            
         SPACE 3                                                                
         LTORG                                                                  
DASHES   DC    11C'-'                                                           
         SPACE                                                                  
INVAMT   DS    CL9                                                              
         SPACE                                                                  
TODAY    DS    CL6                                                              
SVARCD   DS    CL6                 SAVED ACCTS REC CODE                         
SOLINVNO DS    CL6                                                              
SVBILLS  DS    CL8                                                              
         SPACE                                                                  
SVATTN   DS    CL30                                                             
SVNAME   DS    CL30                                                             
SVADR1   DS    CL30                                                             
SVADR2   DS    CL30                                                             
SVCITY   DS    CL20                                                             
SVST     DS    CL2                                                              
SVZIP    DS    CL10                                                             
         DS    0H                                                               
HEADER   DC    AL2(HEADEND-HEADERA+4)                                           
         DC    H'0'                                                             
HEADERA  DC    C'"EIX","EIXBILL '                                               
HEADTE   DC    C'MONDA/YR HH:MM:SS"'                                            
         DC    X'0D25'                                                          
HEADEND  EQU   *                                                                
         SPACE                                                                  
* STATION RATE TABLE                                                            
         SPACE                                                                  
SRATETBL DC    C'1-500      ',C'1.00'                                           
         DC    C'501-1000   ',C' .75'                                           
         DC    C'1001 -     ',C' .50'                                           
         SPACE                                                                  
* STATION          COUNT   CHARGE      TOTAL AT                                 
*                           EACH       THIS RATE                                
         SPACE                                                                  
SCHGTBL  DC    PL3'00500',PL3'100',PL4'050000'                                  
         DC    PL3'00500',PL3'075',PL4'035000'                                  
         DC    PL3'99999',PL3'050',PL4'0'                                       
SCHGTBLN EQU   (*-SCHGTBL)/5                                                    
         SPACE 2                                                                
*                FLD   FIELD                                                    
*                LEN   ADDRESS                                                  
INVTAB   DC    AL1(6),AL3(SVBILLS)  1                                           
         DC    AL1(2),AL3(IN)       2                                           
         DC    AL1(6),AL3(SOLINVNO) 3 INVOICE #                                 
         DC    AL1(6),AL3(TODAY)    4                                           
         DC    AL1(1),AL3(ZERO)     5 TERMS                                     
         DC    AL1(0),AL3(0)        6 DISC DATE                                 
         DC    AL1(0),AL3(0)        7 DUE DATE                                  
         DC    AL1(L'EIX),AL3(EIX)  8 DESC (MAX 15)                             
         DC    AL1(9),AL3(INVAMT)   9 INVOICE AMOUNT                            
         DC    AL1(0),AL3(0)       10 DISCOUNT AMOUNT                           
         DC    X'FF'                                                            
TRNTAB   DC    AL1(0),AL3(0)        1                                           
         DC    AL1(0),AL3(0)        2                                           
         DC    AL1(0),AL3(0)        3                                           
         DC    AL1(0),AL3(0)        4                                           
         DC    AL1(0),AL3(0)        5                                           
         DC    AL1(0),AL3(0)        6                                           
         DC    AL1(0),AL3(0)        7                                           
         DC    AL1(0),AL3(0)        8                                           
         DC    AL1(0),AL3(0)        9                                           
         DC    AL1(0),AL3(0)       10                                           
         DC    AL1(0),AL3(0)       11                                           
         DC    AL1(9),AL3(INVAMT)  12 INVOICE AMOUNT                            
         DC    X'FF'                                                            
MASTAB   DC    AL1(1),AL3(ADDALPHA)  A01                                        
         DC    AL1(6),AL3(SVBILLS)   A02                                        
         DC    AL1(1),AL3(ADDALPHA)  A03  STATUS = A - ACTIVE                   
         DC    AL1(6),AL3(SVBILLS)   A04  BILLING                               
         DC    AL1(30),AL3(SVNAME)   A05  NAME                                  
         DC    AL1(30),AL3(SVATTN)   A06  ATTENTION                             
         DC    AL1(8),AL3(SVBILLS)   B01  SALUTATION - HOLDS AGYORIG            
         DC    AL1(30),AL3(SVADR1)   B02  ADDR 1                                
         DC    AL1(30),AL3(SVADR2)   C01  ADDR 2                                
         DC    AL1(20),AL3(SVCITY)   C02  CITY                                  
         DC    AL1(2),AL3(SVST)      C03  STATE                                 
         DC    AL1(10),AL3(SVZIP)    C04  ZIP                                   
         DC    AL1(0),AL3(0)         C05  TEL                                   
         DC    AL1(0),AL3(0)         D01                                        
         DC    AL1(0),AL3(0)         D02                                        
         DC    AL1(0),AL3(0)         D03                                        
         DC    AL1(6),AL3(SVBILLS)   D04 SHIPPING ADDRESS ID                    
         DC    AL1(0),AL3(0)         D05                                        
         DC    AL1(0),AL3(0)         E01                                        
         DC    AL1(0),AL3(0)         E02                                        
         DC    AL1(0),AL3(0)         F01                                        
         DC    AL1(0),AL3(0)         F02                                        
         DC    AL1(0),AL3(0)         G01                                        
         DC    AL1(0),AL3(0)         G02                                        
         DC    AL1(0),AL3(0)         GO3                                        
         DC    AL1(0),AL3(0)         G04                                        
         DC    AL1(0),AL3(0)         G05                                        
         DC    AL1(0),AL3(0)         G06                                        
         DC    AL1(0),AL3(0)         H01                                        
         DC    AL1(1),AL3(CUSTCLS)   H02 CUST CLASS (T)                         
         DC    AL1(1),AL3(OPNALPHA)  H03                                        
         DC    AL1(1),AL3(STCYONE)   H04                                        
         DC    AL1(4),AL3(SVARCD)    H05                                        
         DC    X'FF'                                                            
         SPACE                                                                  
* TABLE OF BYPASSED CABLE STATIONS                                              
         SPACE                                                                  
         DS   0D                                                                
CABNOBIL DS   0CL8                                                              
         DC    CL5'AEN C',PL3'0'                                                
         DC    CL5'BET C',PL3'0'                                                
         DC    CL5'CMT C',PL3'0'                                                
         DC    CL5'CNBCC',PL3'0'                                                
         DC    CL5'ENT C',PL3'0'                                                
         DC    CL5'EOP C',PL3'0'                                                
         DC    CL5'ESPAC',PL3'0'                                                
         DC    CL5'ESPCC',PL3'0'                                                
         DC    CL5'ESPEC',PL3'0'                                                
         DC    CL5'ESPGC',PL3'0'                                                
         DC    CL5'ESPHC',PL3'0'                                                
         DC    CL5'ESPNC',PL3'0'                                                
         DC    CL5'FAM C',PL3'0'                                                
         DC    CL5'FIT C',PL3'0'                                                
         DC    CL5'HGTVC',PL3'0'                                                
         DC    CL5'MSNBC',PL3'0'                                                
         DC    CL5'MTV C',PL3'0'                                                
         DC    CL5'NAN C',PL3'0'                                                
         DC    CL5'NIC C',PL3'0'                                                
         DC    CL5'SCI C',PL3'0'                                                
         DC    CL5'THC C',PL3'0'                                                
         DC    CL5'TNN C',PL3'0'                                                
         DC    CL5'TVFNC',PL3'0'                                                
         DC    CL5'USANC',PL3'0'                                                
         DC    CL5'VH1 C',PL3'0'                                                
         DC    CL5'WGN C',PL3'0'                                                
CABNOBCT EQU   (*-CABNOBIL)/8                                                   
         SPACE                                                                  
         DS   0D                                                                
STATABLE DC 4000XL8'000000000000000F'                                           
STATABLN EQU   4000                                                             
         SPACE                                                                  
         DS   0D                                                                
NOBILLS  DC 100XL10'00'            8 AGENCY SIGNON ID, 2 CTS                    
NOBILLCT EQU   100                                                              
         SPACE                                                                  
IN       DC    C'IN'                                                            
ADDALPHA DC    C'A'                                                             
OPNALPHA DC    C'O'                                                             
CUSTCLS  DC    C'T'                                                             
STCYONE  DC    C'1'                                                             
EIX      DC    C'EIX BILLING'                                                   
ZERO     DC    C'0'                                                             
         SPACE                                                                  
         DS    0D                                                               
FILIST   DC    CL8'NGENDIR'                                                     
         DC    CL8'NGENFIL'                                                     
         DC    CL10'X '                                                         
*                                ID   MED/CALL MOS    ADV NAM INVNO             
SORTCARD DC    CL80'SORT FIELDS=(1,17,A,22,6,A,32,4,A,42,25,A,92,10,A),C        
               FORMAT=BI,WORK=1 '                                               
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH='                                      
         DS    0D                                                               
EZBILLF  DCB   DDNAME=EZBILL,                                          C        
               DSORG=PS,                                               C        
               EODAD=LIST190,                                          C        
               LRECL=512,                                              C        
               RECFM=FB,                                               C        
               MACRF=GM                                                         
         SPACE                                                                  
         SPACE                                                                  
         DC    AL1(L'BOTHMS-1)                                                  
BOTHMS   DC    C'CAN ONLY REQUEST STATION OR AGENCY, NOT BOTH *'                
         DC    AL1(L'INVUIDMS-1)                                                
INVUIDMS DC    C'INVALID USER ID *'                                             
         DC    AL1(L'CCLENMS-1)                                                 
CCLENMS  DC    C'CLIENT CODE MUST BE 2 OR 3 CHARACTERS *'                       
         DC    AL1(L'PCLENMS-1)                                                 
PCLENMS  DC    C'PRODUCT CODE MUST BE 2 OR 3 CHARACTERS *'                      
         DC    AL1(L'FTRHELP-1)                                                 
FTRHELP  DC    CL60'FILTERS=CC/CN/PC/PN/INV/SOURCE/BATCH/MEDIA/MOS *'           
         DC    AL1(L'UIDLENMS-1)                                                
UIDLENMS DC    C'* ERROR * USERID CAN''T BE MORE THAN 8 CHARACTERS *'           
         DC    AL1(L'SRCLENMS-1)                                                
SRCLENMS DC    C'* ERROR * SOURCE CAN''T BE MORE THAN 4 CHARACTERS *'           
         DC    AL1(L'MOSERMS-1)                                                 
MOSERMS  DC    C'* ERROR * ENTER MOS MO/YR OR MONYR *'                          
         DC    AL1(L'AGYSTAMS-1)                                                
AGYSTAMS DC    C'* ERROR * CAN NOT ENTER BOTH AGENCY && STATION *'              
         SPACE                                                                  
HEADING  SSPEC H6,3,C'TO -'                                                     
         SSPEC H6,40,C'I N V O I C E'                                           
         SSPEC H7,40,C'-------------'                                           
*         SSPEC H1,53,C'FROM - EI-EXPRESS'                                      
*         SSPEC H2,60,C'115 W 18TH ST NY, NY 10011'                             
         SSPEC H9,53,C'DUE UPON RECEIPT'                                        
         SSPEC H10,53,C'INVOICE #'                                              
         SSPEC H11,53,C'INVOICE DATE'                                           
         SSPEC H14,9,C'FOR ELECTRONIC INVOICES RECEIVED FOR'                    
*         SSPEC H13,9,C'RATE CARD'                                              
*         SSPEC H14,9,C'---------'                                              
*         SSPEC H13,39,C'INVOICES'                                              
*         SSPEC H14,39,C'--------'                                              
*         SSPEC H13,66,C'AMOUNT DUE'                                            
*         SSPEC H14,66,C'----------'                                            
         DC    X'00'                                                            
         SPACE                                                                  
HEADINGR SSPEC H1,30,C'EIX BILLING REGISTER'                                    
         SSPEC H2,30,C'--------------------'                                    
         SSPEC H1,60,C'EI-EXPRESS'                                              
         SSPEC H2,60,C'115 W 18TH ST NY, NY 10011'                              
         SSPEC H3,10,C'FOR ELECTRONIC INVOICES RECEIVED IN'                     
         SSPEC H3,60,C'INVOICE DATE'                                            
         SSPEC H8,2,C'TYP'                                                      
         SSPEC H9,2,C'---'                                                      
         SSPEC H8,6,C'INVOICE'                                                  
         SSPEC H9,6,C'-------'                                                  
         SSPEC H8,16,C'ID'                                                      
         SSPEC H9,16,C'--------'                                                
         SSPEC H8,26,C'NAME'                                                    
         SSPEC H9,26,C'--------'                                                
*        SSPEC H8,72,C'SPOTS'                                                   
*        SSPEC H9,72,C'-----'                                                   
         SSPEC H8,57,C'# INV'                                                   
         SSPEC H9,57,C'-----'                                                   
         SSPEC H8,63,C'T INV'                                                   
         SSPEC H9,63,C'-----'                                                   
         SSPEC H8,69,C'R INV'                                                   
         SSPEC H9,69,C'-----'                                                   
         SSPEC H8,75,C'C INV'                                                   
         SSPEC H9,75,C'-----'                                                   
         SSPEC H8,81,C'X INV'                                                   
         SSPEC H9,81,C'-----'                                                   
         SSPEC H8,95,C'GROSS'                                                   
         SSPEC H9,95,C'-----'                                                   
         SSPEC H8,106,C'CHARGE'                                                 
         SSPEC H9,106,C'------'                                                 
         SSPEC H8,114,C'DUP'                                                    
         SSPEC H9,114,C'---'                                                    
         DC    X'00'                                                            
MIDLINE1 DC    C'RATE CARD                      INVOICES               C        
                  AMOUNT DUE'                                                   
MIDLINE2 DC    C'---------                      --------               C        
                  ----------'                                                   
         DROP  R7,RB,RC                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE FILTERS (IF ANY)                                           *         
***********************************************************************         
         SPACE                                                                  
         DS    0H                                                               
VFTR     NMOD1 0,**+VFTR*                                                       
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
         XC    FILTERS,FILTERS                                                  
         SPACE                                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VFTRX               NO                                           
         SPACE                                                                  
         CLI   8(R2),C'?'          HELP                                         
         BE    VFTRHLP             YES                                          
         CLI   5(R2),4                                                          
         BNH   VFTR02                                                           
         LA    R1,3                                                             
         B     VFTR04                                                           
VFTR02   ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
VFTR04   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL4'HELP'                                               
         BE    VFTRHLP                                                          
         LA    R0,25               NON-STANDARD LENGTH                          
         MVI   BYTE,1                                                           
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
         GOTO1 SCANNER,DMCB,((R0),(R2)),(5,(R4)),0                              
         CLI   4(R1),0                                                          
         BE    MISSERRA             SCANNER DIDN'T FIND ANYTHING                
         SR    R3,R3                                                            
         ICM   R3,1,DMCB+4         GET NUMBER OF BLOCKS                         
VFTR10   ZIC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         LA    R5,12(R1,R4)        POINT TO LAST CHAR FOUND                     
         CLI   0(R5),C'+'          PLUS                                         
         BE    VFTR12               YES, SAVE IT                                
         CLI   0(R5),C'-'          MINUS                                        
         BNE   VFTR20               NO, NETHER                                  
VFTR12   MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
VFTR20   EX    R1,VFTRCLCA         TRACE                                        
         BNE   VFTR24                                                           
         MVI   FTRTRACE,C'Y'                                                    
         B     VFTR90                                                           
         SPACE                                                                  
VFTR24   EX    R1,VFTRCLCE         DUPLICATE                                    
         BNE   VFTR30                                                           
         MVI   FTRDUP,C'Y'                                                      
         B     VFTR90                                                           
         SPACE                                                                  
VFTR30   EX    R1,VFTRCLCB         PRODUCT NAME (PN)                            
         BNE   VFTR40                                                           
         MVC   FTRPRDN,22(R4)                                                   
         ZIC   R0,1(R4)                                                         
         BCTR  R0,0                                                             
         STC   R0,FTRPRDNL                                                      
         B     VFTR90                                                           
         SPACE                                                                  
VFTR40   EX    R1,VFTRCLCC         CLIENT NAME (CN)                             
         BNE   VFTR46                                                           
         MVC   FTRCLTN,22(R4)                                                   
         ZIC   R0,1(R4)                                                         
         BCTR  R0,0                                                             
         STC   R0,FTRCLTNL                                                      
         B     VFTR90                                                           
         SPACE                                                                  
VFTR46   EX    R1,VFTRCLCD         DATES                                        
         BNE   VFTR50                                                           
         SPACE                                                                  
         GOTO1 DATVAL,DMCB,(0,22(R4)),RQDTSTR                                   
         ICM   R6,15,DMCB                                                       
         BZ    BDFTRDT                                                          
         SPACE                                                                  
         CLM   R6,1,1(R4)          ONLY 1 DATE ENTERED                          
         BE    BDFTRDT              YES, ERROR                                  
         LA    R6,1+22(R6,R4)                                                   
         SPACE                                                                  
         GOTO1 (RF),(R1),(0,(R6)),RQDTEND                                       
         OC    DMCB,DMCB                                                        
         BZ    BDFTRDT                                                          
         SPACE                                                                  
         CLC   RQDTSTR,RQDTEND                                                  
         BH    BDFTRDT                                                          
         B     VFTR90                                                           
         SPACE                                                                  
VFTR50   EX    R1,VFTRCLCJ         SOURCE                                       
         BNE   VFTR60                                                           
         CLI   1(R4),4                                                          
         BH    SRCLENER                                                         
         MVC   FTRSRCE,22(R4)                                                   
         B     VFTR90                                                           
         SPACE                                                                  
VFTR60   EX    R1,VFTRCLCM         MEDIA                                        
         BNE   VFTR70                                                           
         LA    R5,22(,R4)                                                       
         CLI   0(R5),C'T'          TV                                           
         BE    VFTR68                                                           
         CLI   0(R5),C'R'          RADIO                                        
         BE    VFTR68                                                           
         CLI   0(R5),C'N'          NETWORK                                      
         BE    VFTR68                                                           
         CLI   0(R5),C'X'          NETWORK RADIO                                
         BNE   VFTRMDER                                                         
VFTR68   MVC   FTRMEDIA,0(R5)                                                   
         B     VFTR90                                                           
         SPACE                                                                  
VFTR70   EX    R1,VFTRCLCU         USER ID                                      
         BNE   VFTR80                                                           
         CLI   OFFLINE,C'Y'                                                     
         BE    *+12                                                             
         CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
         BNE   VFTRHLP                                                          
         CLI   1(R4),8                                                          
         BH    USIDLNER                                                         
         MVC   FUID,22(R4)                                                      
         SPACE                                                                  
         MVI   FUIDNUM,X'FF'       ALL IDS                                      
         CLC   FUID(3),=C'ALL'                                                  
         BE    VFTR90                                                           
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(10),SPACES                                                
         ZIC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   KEY+15(0),22(R4)                                                 
         L     R4,AIO1                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD ',=C'CTFILE ',KEY,(R4)                    
         CLI   8(R1),0                                                          
         BNE   USERIDER                                                         
         USING CTIKEY,R4           BUILD ID RECORD KEY                          
         SPACE                                                                  
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,VFNEXTEL                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVC   FUIDNUM,2(R6)       BINARY USER ID (ORIGIN)                      
         B     VFTR90                                                           
         DROP  R4                                                               
         SPACE                                                                  
VFTR80   EX    R1,VFTRCLCP         MOS - MONTH OF SERVICE                       
         BNE   VFTR84                                                           
         LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),WORK                                        
         ICM   RE,15,DMCB          WAS MO/DA/YR DATE VALID                      
         BNZ   MOSERR               YES, ERROR                                  
         SPACE                                                                  
         GOTO1 (RF),(R1),(2,(R5)),WORK                                          
         ICM   RE,15,DMCB          WAS MO/YR DATE VALID                         
         BZ    MOSERR               NO, ERROR                                   
         GOTO1 DATCON,(R1),(0,WORK),(0,FTRMOS)                                  
         B     VFTR90                                                           
         SPACE                                                                  
VFTR84   EX    R1,VFTRCLCG         AGENCY - AGENCY BILLING ONLY                 
         BNE   VFTR86                                                           
         CLI   AGYSTASW,C'S'       MUST NOT BE MORE THAN 1 REQUEST              
         BE    AGYSTAER                                                         
         MVI   AGYSTASW,C'A'                                                    
         B     VFTR90                                                           
         SPACE                                                                  
VFTR86   EX    R1,VFTRCLCS         STATION - STATION BILLING ONLY               
         BNE   VFTRERR                                                          
         CLI   AGYSTASW,C'A'       MUST NOT BE MORE THAN 1 REQUEST              
         BE    AGYSTAER                                                         
         MVI   AGYSTASW,C'S'                                                    
         SPACE                                                                  
VFTR90   ZIC   RE,BYTE             UP FIELD NUMBER CTR                          
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
         LA    R4,47(R4)           NOTE- NON-STANDARD LENGTH                    
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,VFTR10           FOR NUMBER OF BLOCKS FOUND                   
VFTRX    XIT1                                                                   
         SPACE                                                                  
         EJECT                                                                  
VFGETEL  LA    R6,24(R6)           POINT TO FIRST ELEMENT                       
         B     VFNEXT2                                                          
         SPACE                                                                  
VFNEXTEL CLI   0(R6),0                                                          
         BE    VFNEXTX                                                          
         SR    RF,RF                                                            
         ICM   RF,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,RF                                                            
VFNEXT2  CLC   ELCODE,0(R6)                                                     
         BER   RE                                                               
         B     VFNEXTEL                                                         
         SPACE                                                                  
VFNEXTX  LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
VFTRCLCA CLC   12(0,R4),=CL6'TRACE '   TRACE (PRINT BILLING LINES)              
VFTRCLCB CLC   12(0,R4),=CL3'PN'       PRODUCT NAME                             
VFTRCLCC CLC   12(0,R4),=CL3'CN'       CLIENT NAME                              
VFTRCLCD CLC   12(0,R4),=CL5'DATE '                                             
VFTRCLCE CLC   12(0,R4),=CL10'DUPLICATE '                                       
VFTRCLCG CLC   12(0,R4),=CL7'AGENCY'                                            
VFTRCLCJ CLC   12(0,R4),=CL7'SOURCE'                                            
VFTRCLCS CLC   12(0,R4),=CL8'STATION'                                           
VFTRCLCM CLC   12(0,R4),=CL6'MEDIA'                                             
VFTRCLCP CLC   12(0,R4),=CL4'MOS'                                               
VFTRCLCU CLC   12(0,R4),=CL5'USER'                                              
         SPACE                                                                  
USERIDER L     R1,=A(INVUIDMS)                                                  
         B     VFTRERX                                                          
USIDLNER L     R1,=A(UIDLENMS)                                                  
         B     VFTRERX                                                          
VFTRMDER L     R1,=A(VFTRMDMS)                                                  
         B     VFTRERX                                                          
VFTRERRC L     R1,=A(VFTRMS)                                                    
         B     VFTRERX                                                          
CCLENER  L     R1,=A(CCLENMS)                                                   
         B     VFTRERX                                                          
PCLENER  L     R1,=A(PCLENMS)                                                   
         B     VFTRERX                                                          
SRCLENER L     R1,=A(SRCLENMS)                                                  
         B     VFTRERX                                                          
MOSERR   L     R1,=A(MOSERMS)                                                   
         B     VFTRERX                                                          
AGYSTAER L     R1,=A(AGYSTAMS)                                                  
         B     VFTRERX                                                          
BDFTRDT  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD+33(17),=C'ENTER STR-END DATE'                            
         B     VFTRERR1                                                         
VFTRHLP  L     R1,=A(FTRHELP)                                                   
         SPACE                                                                  
VFTRERX  XC    CONHEAD,CONHEAD                                                  
         A     R1,RELO                                                          
         BCTR  R1,0                                                             
         CLI   0(R1),L'CONHEAD-1                                                
         BNH   *+6                                                              
         DC    H'0'                                                             
         ZIC   RF,0(R1)                                                         
         EX    RF,*+8                                                           
         B     VFTRERY                                                          
         MVC   CONHEAD(0),1(R1)                                                 
VFTRERR  XC    CONHEAD,CONHEAD                                                  
         MVI   CONHEAD+33,C'*'                                                  
VFTRERR1 OI    BYTE,X'F0'                                                       
         MVC   CONHEAD(30),=C'* ERROR * INVALID FILTER FIELD'                   
         MVC   CONHEAD+31(1),BYTE                                               
         SPACE                                                                  
VFTRERY  GOTO1 ERREX2                                                           
MISSERRA MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
         DC    AL1(L'VFTRMS-1)                                                  
VFTRMS   DC    C'CAN''T USE CONVERT/UNCOVERT TOGETHER *'                        
         DC    AL1(L'VFTRMDMS-1)                                                
VFTRMDMS DC    C'VALID MEDIAS - T, R, N, X *'                                   
         DROP  RB,RC                                                            
         EJECT                                                                  
***********************************************************************         
*   HEADHOOK ROUTINE FOR BILLS                                        *         
***********************************************************************         
         SPACE                                                                  
         DS    0H                                                               
HDHK     NMOD1 0,**HDHK**                                                       
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
         MVC   H11+65(8),INVCDATE                                               
         MVC   H14+45(6),BILLMON                                                
         MVI   H14+52,C'('                                                      
         GOTO1 DATCON,DMCB,RQDTSTR,(5,H14+53)                                   
         MVI   H14+61,C'-'                                                      
         GOTO1 (RF),(R1),RQDTEND,(5,H14+62)                                     
         MVI   H14+70,C')'                                                      
         SPACE                                                                  
         CLI   FIRSTSW,X'0F'       END OF FILE                                  
         BNE   HDHK10               NO                                          
         SPACE                                                                  
         MVC   H6+38(15),=C' BILLING TOTALS'                                    
         MVC   H7+38(15),=C' --------------'                                    
         MVC   H6+53(5),SPACES                                                  
         MVC   H9+40(5),SPACES                                                  
         MVC   H10+8(30),SPACES                                                 
         B     HDHK50                                                           
         SPACE                                                                  
HDHK10   DS   0H                                                                
         MVC   H10+66(2),INVMON                                                 
         MVI   H10+68,C'-'                                                      
         OI    INVNUM+2,X'0F'                                                   
         UNPK  H10+69(4),INVNUM                                                 
         L     R2,=A(SOLINVNO)                                                  
         MVC   0(2,R2),INVMON                                                   
         MVC   2(4,R2),H10+69                                                   
         SPACE                                                                  
         LA    R2,H6+8                                                          
         SPACE                                                                  
         CLI   EZBILLT,C'A'        AGENCY BILL                                  
         BE    HDHK12                                                           
         SPACE                                                                  
         CLI   EZBILLT,C'L'        LINKED AGENCY BILL                           
         BNE   HDHK20                                                           
         SPACE                                                                  
* PRINT AGENCY HEADINGS                                                         
         SPACE                                                                  
HDHK12   CLI   BILLSW,C'Y'                                                      
         BE    HDHK40                                                           
         SPACE                                                                  
* IF BILLSW IS SET TO N, THIS IS A SPECIAL RUN TO BILL ALL AGY/STA              
* AND WE USE BILLING REC AS SOURCE FOR ADDRESS AND ALL INFO                     
         SPACE                                                                  
         LA    R3,EZBAGYNM                                                      
         MVC   REGNAME,EZBAGYNM                                                 
         SPACE                                                                  
         LA    R0,5                                                             
         LR    R1,R3                                                            
         SPACE                                                                  
         OC    0(30,R1),SPACES                                                  
         LA    R1,30(,R1)                                                       
         BCT   R0,*-10                                                          
         SPACE                                                                  
         LA    R0,5                                                             
HDHK14   CLC   0(30,R3),SPACES                                                  
         BE    HDHK16                                                           
         MVC   0(30,R2),0(R3)                                                   
         LA    R2,132(,R2)                                                      
         SPACE                                                                  
HDHK16   LA    R3,30(,R3)                                                       
         BCT   R0,HDHK14                                                        
         SPACE                                                                  
         LA    R2,132(,R2)                                                      
         MVC   0(6,R2),=C'AGENCY'                                               
         SPACE                                                                  
         MVC   8(8,R2),EZBAGY                                                   
         SPACE                                                                  
         B     HDHK50                                                           
         SPACE                                                                  
* PRINT STATION HEADINGS                                                        
         SPACE                                                                  
HDHK20   CLI   EZBILLT,C'R'        LINKED STATION BILL                          
         BE    HDHK22                                                           
         CLI   EZBILLT,C'S'        STATION BILL                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
HDHK22   CLI   BILLSW,C'Y'                                                      
         BE    HDHK40                                                           
         SPACE                                                                  
         LA    R3,EZBSTADR                                                      
         MVC   REGNAME,EZBSTADR                                                 
         SPACE                                                                  
         LA    R0,5                                                             
         LR    R1,R3                                                            
         SPACE                                                                  
         OC    0(30,R1),SPACES                                                  
         LA    R1,30(,R1)                                                       
         BCT   R0,*-10                                                          
         SPACE                                                                  
         LA    R0,5                                                             
HDHK24   CLC   0(30,R3),SPACES                                                  
         BE    HDHK26                                                           
         MVC   0(30,R2),0(R3)                                                   
         LA    R2,132(,R2)                                                      
         SPACE                                                                  
HDHK26   LA    R3,30(,R3)                                                       
         BCT   R0,HDHK24                                                        
         MVC   0(7,R2),=C'STATION'                                              
         MVC   8(5,R2),EZBCALL                                                  
         SPACE                                                                  
         B     HDHK50                                                           
         SPACE                                                                  
HDHK40   LA    R0,4                                                             
         L     R3,=A(SVATTN)                                                    
         L     RF,=A(SVNAME)                                                    
         MVC   REGNAME,0(RF)                                                    
         SPACE                                                                  
         LR    R1,R3                                                            
         SPACE                                                                  
         OC    0(L'SVATTN,R1),SPACES                                            
         LA    R1,30(,R1)                                                       
         BCT   R0,*-10                                                          
         SPACE                                                                  
         LA    R0,4                                                             
         B     HDHK44                                                           
         SPACE                                                                  
HDHK42   CLC   0(L'SVNAME,R3),SPACES                                            
         BE    HDHK46                                                           
         SPACE                                                                  
HDHK44   MVC   0(L'SVNAME,R2),0(R3)                                             
         LA    R2,132(,R2)                                                      
HDHK46   LA    R3,L'SVNAME(,R3)                                                 
         BCT   R0,HDHK42                                                        
         L     RF,=A(SVCITY)                                                    
         MVC   0(L'SVCITY,R2),0(RF)                                             
         LA    R1,L'SVCITY(,R2)                                                 
HDHK48   CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,HDHK48                                                        
         MVI   1(R1),C','                                                       
         L     RF,=A(SVST)                                                      
         MVC   3(2,R1),0(RF)                                                    
         L     RF,=A(SVZIP)                                                     
         MVC   6(L'SVZIP,R1),0(RF)                                              
         LA    R2,264(,R2)                                                      
         MVC   0(6,R2),=C'AGENCY'                                               
         MVC   8(8,R2),EZBILLS                                                  
         SPACE                                                                  
         CLI   EZBILLT,C'A'        AGENCY BILL                                  
         BE    HDHK50                                                           
         CLI   EZBILLT,C'L'        LINKED AGENCY BILL                           
         BE    HDHK50                                                           
         SPACE                                                                  
         MVC   0(7,R2),=C'STATION'                                              
         MVC   8(5,R2),EZBCALL                                                  
         MVC   13(3,R2),SPACES                                                  
         SPACE                                                                  
HDHK50   CLI   FTRMOS,0            TEST HAVE MONTH OF SERVICE FILTER            
         BNH   HDHK60                                                           
         MVC   H11+53(6),FTRMOS                                                 
         SPACE                                                                  
HDHK60   L     R1,=A(MIDLINE1)                                                  
         MVC   MID1+8(L'MIDLINE1),0(R1)                                         
         MVC   MID2+8(L'MIDLINE2),L'MIDLINE1(R1)                                
         SPACE                                                                  
HDHKX    XIT1                                                                   
         SPACE                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
***********************************************************************         
*   HEADHOOK ROUTINE FOR REGISTER                                     *         
***********************************************************************         
         SPACE                                                                  
         DS    0H                                                               
HDHKR    NMOD1 0,**HDHKR*                                                       
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
         MVC   MID1,SPACES                                                      
         MVC   MID1,SPACES                                                      
         MVC   H3+45(6),BILLMON                                                 
         MVC   H3+72(8),INVCDATE                                                
         SPACE                                                                  
         MVI   H5+31,C'('                                                       
         GOTO1 DATCON,DMCB,RQDTSTR,(5,H5+32)                                    
         MVI   H5+41,C'-'                                                       
         GOTO1 (RF),(R1),RQDTEND,(5,H5+43)                                      
         MVI   H5+51,C')'                                                       
         SPACE                                                                  
         CLI   FTRMOS,0            TEST HAVE MONTH OF SERVICE FILTER            
         BNH   HDHKRX                                                           
         MVC   H3+35(16),=C'MONTH OF SERVICE'                                   
         MVC   H3+53(6),FTRMOS                                                  
         SPACE                                                                  
HDHKRX   XIT1                                                                   
         SPACE                                                                  
         LTORG                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
* PRINT TOTALS BY MEDIA *                                                       
         SPACE                                                                  
         DS    0H                                                               
PMEDCT   NMOD1 0,**PMEDCT                                                       
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
         SPACE                                                                  
         CLI   0(R2),0             ANY COUNTS AT ALL                            
         BE    PMEDCTX              NO                                          
         SPACE                                                                  
         MVC   P+9(5),=C'SPOTS'                                                 
         MVC   P+32(5),=C'GROSS'                                                
         MVC   PINVS(8),=C'INVOICES'                                            
         MVC   PCHRG+6(7),=C'CHARGES'                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         SR    R4,R4               CT OF MEDIAS                                 
         LR    RE,R2                                                            
         LA    RF,12                                                            
PMEDCT10 CLI   0(RE),0             CT HOW MANY MEDIAS                           
         BE    PMEDCT20                                                         
         LA    R4,1(,R4)                                                        
         LA    RE,20(,RE)                                                       
         BCT   RF,PMEDCT10                                                      
         SPACE                                                                  
PMEDCT20 STH   R4,MEDCT                                                         
         CH    R4,=H'1'            IF ONLY ONE, NO SORT                         
         BE    PMEDCT30                                                         
         SPACE                                                                  
         GOTO1 XSORT,DMCB,(R2),(R4),20,1,0                                      
         SPACE                                                                  
PMEDCT30 MVC   P+2(1),0(R2)                                                     
         EDIT  (P4,1(R2)),(7,PINVS),COMMAS=YES                                  
         EDIT  (P4,5(R2)),(7,P+7),COMMAS=YES                                    
         EDIT  (P6,9(R2)),(17,P+20),COMMAS=YES,2                                
         SPACE                                                                  
         EDIT  (P5,15(R2)),(13,PCHRG),COMMAS=YES,2                              
         SPACE                                                                  
         MVI   SPACING,2                                                        
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,20(,R2)                                                       
         BCT   R4,PMEDCT30                                                      
PMEDCTX  XIT1                                                                   
         SPACE                                                                  
         LTORG                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
* CLOSE EZBILL, FORMAT COUNTERS, OPEN NEXT FILES                                
         SPACE                                                                  
         DS    0H                                                               
NEXTSET  NMOD1 0,**NEXTST                                                       
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
         SPACE                                                                  
         LM    R0,R1,=A(HEADING,HDHK)                                           
         ST    R0,SPECS                                                         
         ST    R1,HEADHOOK                                                      
         SPACE                                                                  
         ZAP   INVCT,=P'0'                                                      
         ZAP   INVCTT,=P'0'                                                     
         ZAP   INVCTR,=P'0'                                                     
         ZAP   INVCTC,=P'0'                                                     
         ZAP   INVCTX,=P'0'                                                     
         ZAP   INVSPT,=P'0'                                                     
         ZAP   INVDOL,=P'0'                                                     
         ZAP   INVCHRG,=P'0'                                                    
         SPACE                                                                  
         ZAP   AINVCT,=P'0'                                                     
         ZAP   AINVSPT,=P'0'                                                    
         ZAP   AINVDOL,=P'0'                                                    
         ZAP   AINVCHRG,=P'0'                                                   
         SPACE                                                                  
         ZAP   SINVCT,=P'0'                                                     
         ZAP   SINVSPT,=P'0'                                                    
         ZAP   SINVDOL,=P'0'                                                    
         ZAP   SINVCHRG,=P'0'                                                   
         SPACE                                                                  
         ZAP   TINVCT,=P'0'                                                     
         ZAP   TINVSPT,=P'0'                                                    
         ZAP   TINVDOL,=P'0'                                                    
         ZAP   TINVCHRG,=P'0'                                                   
         ZAP   BYPASSCT,=P'0'                                                   
         ZAP   BYPASSCH,=P'0'                                                   
         SPACE                                                                  
         LA    R1,AMEDCTS                                                       
         BAS   RE,FMTCT                                                         
         SPACE                                                                  
         LA    R1,SMEDCTS                                                       
         BAS   RE,FMTCT                                                         
         SPACE                                                                  
         LA    R1,TMEDCTS                                                       
         BAS   RE,FMTCT                                                         
         SPACE                                                                  
         XC    EZBILLT(9),EZBILLT                                               
         SPACE                                                                  
         XIT1                                                                   
         SPACE                                                                  
FMTCT    LA    R0,12                                                            
FMTCT10  ZAP   0(5,R1),=P'0'                                                    
         ZAP   5(4,R1),=P'0'                                                    
         ZAP   9(6,R1),=P'0'                                                    
         ZAP   15(5,R1),=P'0'                                                   
         LA    R1,20(,R1)                                                       
         BCT   R0,FMTCT10                                                       
         BR    RE                                                               
         DROP  RB,RC                                                            
         EJECT                                                                  
* DO CALC AND ADDS FOR AGENCY BILLING *                                         
         SPACE                                                                  
         DS    0H                                                               
AGYC     NMOD1 0,**AGYC**                                                       
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
         SPACE                                                                  
         MVI   FORCEHED,C'Y'                                                    
         SPACE                                                                  
         LA    R0,12                                                            
         LA    R1,MEDCTS                                                        
         SR    R2,R2                                                            
AGYC10   CLI   0(R1),0                                                          
         BE    AGYC14                                                           
         LA    R1,20(,R1)                                                       
         LA    R2,1(,R2)                                                        
         BCT   R0,AGYC10                                                        
         SPACE                                                                  
AGYC14   CH    R2,=H'1'                                                         
         BE    AGYC16                                                           
         SPACE                                                                  
         GOTO1 XSORT,DMCB,MEDCTS,(R2),20,1,0                                    
         SPACE                                                                  
AGYC16   LA    R5,MEDCTS                                                        
         USING MEDCTD,R5                                                        
         CLI   MEDMED,C'C'          THIS CABLE                                  
         BNE   AGYC60                                                           
         SPACE                                                                  
         LA    R0,CCHGTBLN                                                      
         LA    R1,CCHGTBL                                                       
         SPACE                                                                  
         ZAP   WORKCT1,=P'0'                                                    
         ZAP   WORKCT2,=P'0'                                                    
         ZAP   WORKCT3,=P'0'                                                    
         ZAP   WORKCT4,=P'0'                                                    
         ZAP   WORKCH1,=P'0'                                                    
         ZAP   WORKCH2,=P'0'                                                    
         ZAP   WORKCH3,=P'0'                                                    
         ZAP   WORKCH4,=P'0'                                                    
         SPACE                                                                  
         LA    R2,WORKCT1                                                       
         ZAP   WORKCT,MEDINV                                                    
         SPACE                                                                  
AGYC30   CP    WORKCT,0(3,R1)                                                   
         BNH   AGYC34                                                           
         SP    WORKCT,0(3,R1)                                                   
         ZAP   0(4,R2),0(3,R1)                                                  
         MVC   4(4,R2),6(R1)                                                    
         LA    R1,10(,R1)                                                       
         LA    R2,8(,R2)                                                        
         BCT   R0,AGYC30                                                        
         DC    H'0'                                                             
AGYC34   ZAP   DUB,3(3,R1)                                                      
         ZAP   0(4,R2),WORKCT                                                   
         CVB   R0,DUB                                                           
         ZAP   DUB,WORKCT                                                       
         CVB   RF,DUB                                                           
         SR    RE,RE                                                            
         MR    RE,R0                                                            
         CVD   RF,DUB                                                           
         AP    4(4,R2),DUB                                                      
         SPACE                                                                  
         LA    R2,WORKCT1                                                       
         LA    R3,1                                                             
         LA    R4,CRATETBL                                                      
         SPACE                                                                  
AGYC40   DS   0H                                                                
         MVC   PCOUNT+5(11),0(R4)                                               
         MVC   PINVCHG+5(4),11(R4)                                              
         EDIT  (P4,0(R2)),(6,PINVS),0,COMMAS=YES                                
         EDIT  (P4,4(R2)),(9,PCHRG),2,COMMAS=YES,FLOAT=$                        
         SPACE                                                                  
         AP    MEDCHG,4(4,R2)                                                   
         AP    INVCHRG,4(4,R2)                                                  
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         LA    R2,8(,R2)                                                        
         LA    R4,15(,R4)                                                       
         BCT   R3,AGYC40                                                        
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVC   PINVS-2(8),DASHESA                                               
         MVC   PCHRG-2(11),DASHESA                                              
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVC   PINVCHG(11),=C'CABLE TOTAL'                                      
         EDIT  MEDINV,(6,PINVS),0,COMMAS=YES                                    
         EDIT  MEDCHG,(9,PCHRG),2,COMMAS=YES,FLOAT=$                            
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         LA    R5,20(,R5)                                                       
         SPACE                                                                  
         CLI   MEDMED,0             THIS END OF TABLE                           
         BE    AGYC90                                                           
         SPACE                                                                  
AGYC60   CLI   MEDMED,C'T'          THIS TV                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         ZAP   WORKCT1,=P'0'                                                    
         ZAP   WORKCT2,=P'0'                                                    
         ZAP   WORKCT3,=P'0'                                                    
         ZAP   WORKCT4,=P'0'                                                    
         ZAP   WORKCH1,=P'0'                                                    
         ZAP   WORKCH2,=P'0'                                                    
         ZAP   WORKCH3,=P'0'                                                    
         ZAP   WORKCH4,=P'0'                                                    
         SPACE                                                                  
         LA    R0,ACHGTBLN                                                      
         LA    R1,ACHGTBL                                                       
         LA    R2,WORKCT1                                                       
         ZAP   WORKCT,MEDINV                                                    
         SPACE                                                                  
AGYC70   CP    WORKCT,0(3,R1)                                                   
         BNH   AGYC74                                                           
         SP    WORKCT,0(3,R1)                                                   
         ZAP   0(4,R2),0(3,R1)                                                  
         MVC   4(4,R2),6(R1)                                                    
         LA    R1,10(,R1)                                                       
         LA    R2,8(,R2)                                                        
         BCT   R0,AGYC70                                                        
         DC    H'0'                                                             
AGYC74   ZAP   DUB,3(3,R1)                                                      
         ZAP   0(4,R2),WORKCT                                                   
         CVB   R0,DUB                                                           
         ZAP   DUB,WORKCT                                                       
         CVB   RF,DUB                                                           
         SR    RE,RE                                                            
         MR    RE,R0                                                            
         CVD   RF,DUB                                                           
         AP    4(4,R2),DUB                                                      
         SPACE                                                                  
         LA    R2,WORKCT1                                                       
         LA    R3,4                                                             
         LA    R4,ARATETBL                                                      
         SPACE                                                                  
AGYC80   DS   0H                                                                
         MVC   PCOUNT+5(11),0(R4)                                               
         MVC   PINVCHG+5(4),11(R4)                                              
         EDIT  (P4,0(R2)),(6,PINVS),0,COMMAS=YES                                
         EDIT  (P4,4(R2)),(9,PCHRG),2,COMMAS=YES,FLOAT=$                        
         SPACE                                                                  
         AP    INVCHRG,4(4,R2)                                                  
         AP    MEDCHG,4(4,R2)                                                   
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         LA    R2,8(,R2)                                                        
         LA    R4,15(,R4)                                                       
         BCT   R3,AGYC80                                                        
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVC   PINVS-2(8),DASHESA                                               
         MVC   PCHRG-2(11),DASHESA                                              
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVC   PINVCHG(8),=C'TV TOTAL'                                          
         EDIT  MEDINV,(6,PINVS),0,COMMAS=YES                                    
         EDIT  MEDCHG,(9,PCHRG),2,COMMAS=YES,FLOAT=$                            
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
AGYC90   LA    R0,12                                                            
         LA    R1,MEDCTS                                                        
AGYC92   LA    RE,12                                                            
         LA    RF,AMEDCTS                                                       
AGYC94   CLC   0(1,R1),0(RF)       SAME MEDIA                                   
         BE    AGYC96                                                           
         CLI   0(RF),0                                                          
         BE    AGYC96                                                           
         LA    RF,20(,RF)                                                       
         BCT   RE,AGYC94                                                        
         DC    H'0'                                                             
AGYC96   MVC   0(1,RF),0(R1)                                                    
         AP    1(4,RF),1(4,R1)                                                  
         AP    5(4,RF),5(4,R1)                                                  
         AP    9(6,RF),9(6,R1)                                                  
         AP    15(5,RF),15(5,R1)                                                
         LA    R1,20(,R1)                                                       
         CLI   0(R1),0                                                          
         BE    *+8                                                              
         BCT   R0,AGYC92                                                        
         SPACE                                                                  
         AP   AINVCT,INVCT                                                      
         AP   AINVSPT,INVSPT                                                    
         AP   AINVDOL,INVDOL                                                    
         AP   AINVCHRG,INVCHRG                                                  
         SPACE                                                                  
AGYCX    XIT1                                                                   
         LTORG                                                                  
DASHESA  DC    11C'-'                                                           
         SPACE                                                                  
* AGENCY RATE TABLE                                                             
         SPACE                                                                  
ARATETBL DC    C'1-1000     ',C'1.00'                                           
         DC    C'1001-4000  ',C' .67'                                           
         DC    C'4001-10,000',C' .50'                                           
         DC    C'10,001 +   ',C' .33'                                           
         SPACE                                                                  
* CABLE RATE TABLE                                                              
         SPACE                                                                  
CRATETBL DC    C'1 +        ',C'1.00'                                           
         SPACE                                                                  
* AGENCY           COUNT   CHARGE      TOTAL AT                                 
*                           EACH       THIS RATE                                
         SPACE                                                                  
ACHGTBL  DC    PL3'01000',PL3'100',PL4'100000'                                  
         DC    PL3'03000',PL3'067',PL4'201000'                                  
         DC    PL3'06000',PL3'050',PL4'300000'                                  
         DC    PL3'99999',PL3'033',PL4'0'                                       
         SPACE                                                                  
ACHGTBLN EQU   (*-ACHGTBL)/5                                                    
         SPACE                                                                  
* CABLE            COUNT   CHARGE      TOTAL AT                                 
*                           EACH       THIS RATE                                
         SPACE                                                                  
CCHGTBL  DC    PL3'99999',PL3'100',PL4'0'                                       
CCHGTBLN EQU   (*-CCHGTBL)/5                                                    
         SPACE                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
* DO CALC AND ADDS FOR AGENCY BILLING *                                         
         SPACE                                                                  
         DS    0H                                                               
PRTOT    NMOD1 0,*PRTOT**                                                       
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
         SPACE                                                                  
         CLOSE ((R2),)                                                          
         FREEPOOL (R2)                                                          
         SPACE                                                                  
         MVI   FORCEHED,C'Y'                                                    
         L     R2,=A(EZSOLM)                                                    
         CLOSE ((R2),)                                                          
         FREEPOOL (R2)                                                          
         SPACE                                                                  
         L     R2,=A(EZSOLT)                                                    
         CLOSE ((R2),)                                                          
         FREEPOOL (R2)                                                          
         MVI   FIRSTSW,X'0F'       END OF PRINT                                 
         SPACE                                                                  
         MVC   P(15),=C'BILLING RECORDS'                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVC   P(4),=C'READ'                                                    
         EDIT  RINVCT,(5,P+5)                                                   
         MVC   P+11(7),=C'RECORDS'                                              
         MVC   P+25(9),=C'PROCESSED'                                            
         EDIT  UINVCT,(5,P+35)                                                  
         MVC   P+41(7),=C'RECORDS'                                              
         MVC   P+60(8),=C'BYPASSED'                                             
         EDIT  DUPCT,(5,P+70)                                                   
         MVC   P+77(9),=C'DUPLICATE'                                            
         MVC   P+87(7),=C'RECORDS'                                              
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVC   P(20),=C'INVOICE MOVE RECORDS'                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVC   P+1(5),=C'READ='                                                 
         EDIT  TMOVREC,(6,P+7),COMMAS=YES,ALIGN=LEFT                            
         MVC   P+25(7),=C'TABLED='                                              
         EDIT  UMOVREC,(6,P+33),COMMAS=YES,ALIGN=LEFT                           
         MVC   P+40(5),=C'USED='                                                
         SPACE                                                                  
         LA    R2,MOVTABLN                                                      
         L     R3,AMOVTAB                                                       
         SR    R4,R4               UNUSED                                       
         SR    R5,R5               TOTAL                                        
         SPACE                                                                  
         USING MOVTABD,R3                                                       
         SPACE                                                                  
PRTOT20  DS    0H                                                               
         CLI   MOVOAGY,0           END OF TABLE                                 
         BE    PRTOT30                                                          
         CLI   MOVUSED,1           USED ?                                       
         BE    PRTOT24              YES, BYPASS                                 
         LA    R4,1(,R4)                                                        
         SPACE                                                                  
PRTOT24  LA    R5,1(,R5)                                                        
         SPACE                                                                  
         LA    R3,MOVNEXT                                                       
         BCT   R2,PRTOT20                                                       
         DC    H'0'                                                             
         SPACE                                                                  
PRTOT30  CVD   R5,DUB                                                           
         CP    DUB,UMOVREC                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R5,R4                                                            
         EDIT  (R5),(6,P+46),COMMAS=YES,ALIGN=LEFT                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         LTR   R4,R4                                                            
         BZ    PRTOT50                                                          
         SPACE                                                                  
         LA    R2,MOVTABLN                                                      
         L     R3,AMOVTAB                                                       
         SPACE                                                                  
PRTOT40  DS    0H                                                               
         CLI   MOVOAGY,0           END OF TABLE                                 
         BE    PRTOT50                                                          
         CLI   MOVUSED,1           USED ?                                       
         BE    PRTOT44              YES, BYPASS                                 
         SPACE                                                                  
         MVC   P+3(4),=C'FROM'                                                  
         MVC   P+8(8),MOVOAGY                                                   
         MVC   P+20(2),=C'TO'                                                   
         MVC   P+23(8),MOVNAGY                                                  
         MVC   P+35(3),=C'STA'                                                  
         MVC   P+39(5),MOVCALL                                                  
         MVC   P+50(3),=C'INV'                                                  
         MVC   P+54(10),MOVINV                                                  
         MVC   P+70(3),=C'BDT'                                                  
         MVC   P+74(6),MOVBDTE                                                  
         MVC   P+85(3),=C'MOS'                                                  
         MVC   P+89(4),MOVMOS                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+20(3),=C'MDT'                                                  
         GOTO1 DATCON,DMCB,(2,MOVDATE),(5,P+24)                                 
         MVC   P+35(3),=C'SRC'                                                  
         MVC   P+39(4),MOVSRCE                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
PRTOT44  LA    R3,MOVNEXT                                                       
         BCT   R2,PRTOT40                                                       
         DC    H'0'                                                             
         DROP  R3                                                               
PRTOT50  DS   0H                                                                
         EDIT  EZSOLMCT,(5,P+35)                                                
         MVC   P+41(9),=C'SOL MASTR'                                            
         SPACE                                                                  
         EDIT  EZSOLTCT,(5,P+70)                                                
         MVC   P+77(9),=C'SOL TRANS'                                            
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
* PRINT OUT BYPASSED TOTALS *                                                   
         SPACE                                                                  
         MVC   P+5(17),=C'BYPASSED INVOICES'                                    
         MVI   SPACING,2                                                        
         EDIT  BYPASSCT,(6,P+22),COMMAS=YES,ZERO=NOBLANK                        
         EDIT  BYPASSCH,(13,P+30),2,COMMAS=YES,ZERO=NOBLANK                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVC   P+2(8),=C'AGENCIES'                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         LA    R2,AMEDCTS                                                       
         SPACE                                                                  
         GOTO1 =A(PMEDCT)          PRINT COUNTS BY MEDIA                        
         SPACE                                                                  
         EDIT  AINVCT,(7,PINVS),COMMAS=YES                                      
         SPACE                                                                  
         EDIT  AINVSPT,(7,P+7),COMMAS=YES                                       
         SPACE                                                                  
         EDIT  AINVDOL,(17,P+20),2,COMMAS=YES,ZERO=NOBLANK                      
         SPACE                                                                  
         EDIT  AINVCHRG,(13,PCHRG),2,COMMAS=YES,ZERO=NOBLANK                    
         SPACE                                                                  
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVC   P+2(8),=C'STATIONS'                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,SMEDCTS                                                       
         SPACE                                                                  
         GOTO1 =A(PMEDCT)          PRINT COUNTS BY MEDIA                        
         SPACE                                                                  
         EDIT  SINVCT,(7,PINVS),COMMAS=YES                                      
         SPACE                                                                  
         EDIT  SINVSPT,(7,P+7),COMMAS=YES                                       
         SPACE                                                                  
         EDIT  SINVDOL,(17,P+20),2,COMMAS=YES,ZERO=NOBLANK                      
         SPACE                                                                  
         EDIT  SINVCHRG,(13,PCHRG),2,COMMAS=YES,ZERO=NOBLANK                    
         SPACE                                                                  
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVC   P+2(5),=C'TOTAL'                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         LA    R2,TMEDCTS                                                       
         SPACE                                                                  
         GOTO1 =A(PMEDCT)          PRINT COUNTS BY MEDIA                        
         SPACE                                                                  
         EDIT  TINVCT,(7,PINVS),COMMAS=YES                                      
         SPACE                                                                  
         EDIT  TINVSPT,(7,P+7),COMMAS=YES                                       
         SPACE                                                                  
         EDIT  TINVDOL,(17,P+20),2,COMMAS=YES,ZERO=NOBLANK                      
         SPACE                                                                  
         EDIT  TINVCHRG,(13,PCHRG),2,COMMAS=YES,ZERO=NOBLANK                    
         SPACE                                                                  
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         ICM   RF,15,UTL                                                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   4(1,RF),SAVSYS                                                   
         SPACE                                                                  
         L     R2,=A(EZREG)                                                     
         CLOSE ((R2),)                                                          
         FREEPOOL (R2)                                                          
         SPACE                                                                  
         LM    R0,R1,=A(HEADINGR,HDHKR)                                         
         ST    R0,SPECS                                                         
         ST    R1,HEADHOOK                                                      
         SPACE                                                                  
         MVI   FORCEHED,C'Y'                                                    
         SPACE                                                                  
         OPEN  (EZREGR,(INPUT))                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         ZAP   TREGINV,=P'0'                                                    
         ZAP   TREGINVT,=P'0'                                                   
         ZAP   TREGINVR,=P'0'                                                   
         ZAP   TREGINVC,=P'0'                                                   
         ZAP   TREGINVX,=P'0'                                                   
         ZAP   TREGSPT,=P'0'                                                    
         ZAP   TREGDOL,=P'0'                                                    
         ZAP   TREGDUE,=P'0'                                                    
         ZAP   TREGDUP,=P'0'                                                    
         ZAP   TREGINVS,=P'0'                                                   
         SPACE                                                                  
LIST700  L     R1,=A(EZREGR)                                                    
         GET   (1),REGREC                                                       
         SPACE                                                                  
         MVC   PREGTYP,REGTYP                                                   
         MVC   PREGINVN(2),INVMON                                               
         MVI   PREGINVN+2,C'-'                                                  
         OI    REGINVNO+2,X'0F'                                                 
         UNPK  PREGINVN+3(4),REGINVNO                                           
         MVC   PREGID,REGID                                                     
         SPACE                                                                  
*        MVC   PREGSOL,REGSOL                                                   
         SPACE                                                                  
         MVC   PREGNAM(L'REGNAME),REGNAME                                       
*        EDIT  REGSPT,(10,PREGSPT),COMMAS=YES                                   
         EDIT  REGINV,(5,PREGIVC)                                               
         EDIT  REGINVT,(5,PREGIVCT)                                             
         EDIT  REGINVR,(5,PREGIVCR)                                             
         EDIT  REGINVC,(5,PREGIVCC)                                             
         EDIT  REGINVX,(4,PREGIVCX)                                             
         EDIT  REGDOL,(14,PREGDL),2,COMMAS=YES                                  
         EDIT  REGDUE,(11,PREGDUE),2,COMMAS=YES                                 
         EDIT  REGDUPCT,(4,PREGDUP-1)                                           
         AP    TREGSPT,REGSPT                                                   
         AP    TREGINV,REGINV                                                   
         AP    TREGINVT,REGINVT                                                 
         AP    TREGINVR,REGINVR                                                 
         AP    TREGINVC,REGINVC                                                 
         AP    TREGINVX,REGINVX                                                 
         AP    TREGSPT,REGSPT                                                   
         AP    TREGDOL,REGDOL                                                   
         AP    TREGDUE,REGDUE                                                   
         AP    TREGDUP,REGDUPCT                                                 
         AP    TREGINVS,=P'1'                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LIST700                                                          
         SPACE                                                                  
LIST800  GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVC   P1+1(5),=C'TOTAL'                                                
         EDIT  TREGINVS,(7,PREGINVN+1),COMMAS=YES                               
         MVC   PREGINVN+9(8),=C'INVOICES'                                       
*        EDIT  TREGSPT,(10,PREGSPT),COMMAS=YES                                  
*        EDIT  TREGDOL,(16,PREGDL-2),2,COMMAS=YES                               
         MVC   WORK(18),=X'4020206B2020206B2020206B2020214B2020'                
         ED    WORK(18),TREGDOL+1                                               
         MVC   PREGDL-3(17),WORK+1                                              
         EDIT  TREGINV,(6,PREGIVC-1)                                            
         EDIT  TREGINVR,(6,PREGIVCR-1)                                          
         EDIT  TREGDUE,(11,PREGDUE),2,COMMAS=YES                                
         EDIT  TREGDUP,(4,PREGDUP-1)                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         EDIT  TREGINVT,(6,PREGIVCT-1)                                          
         EDIT  TREGINVC,(6,PREGIVCC-1)                                          
         EDIT  TREGINVX,(5,PREGIVCX)                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         L     R2,=A(EZREGR)                                                    
         CLOSE ((R2),)                                                          
         FREEPOOL (R2)                                                          
         SPACE                                                                  
         LA    R2,NOBILLCT                                                      
         L     R3,=A(NOBILLS)                                                   
         SR    R0,R0                                                            
         LR    RE,R2                                                            
         LR    RF,R3                                                            
         SR    R5,R5                                                            
LIST810  DS   0H                                                                
         OC    0(10,RF),0(RF)                                                   
         BZ    LIST812                                                          
         LA    RF,10(,RF)                                                       
         BCTR  R0,0                                                             
         BCT   RE,LIST810                                                       
         DC    H'0'                                                             
LIST812  DS   0H                                                                
         LPR   R2,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(QSORT),DMCB,(R3),(R2),10,8,0                                  
         SPACE                                                                  
         MVC   P+3(25),=C'AGENCIES FOUND NOT BILLED'                            
LIST814  OC    0(10,R3),0(R3)      EMPTY ENTRY                                  
         BZ    LIST816                                                          
         MVC   P+40(8),0(R3)                                                    
         LH    R4,8(R3)                                                         
         AR    R5,R4                                                            
         EDIT  (R4),(6,P+50),COMMAS=YES                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         LA    R3,10(,R3)                                                       
         BCT   R2,LIST814                                                       
         SPACE                                                                  
LIST816  DS   0H                                                                
         SPACE                                                                  
         MVC   P+39(7),=C'*TOTAL*'                                              
         EDIT  (R5),(6,P+50),COMMAS=YES                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,CABNOBCT                                                      
         L     R3,=A(CABNOBIL)                                                  
         MVC   P+3(33),=C'BYPASSED CABLE (NO BILL) STATIONS'                    
LIST820  CP    5(3,R3),=P'0'                                                    
         BE    LIST824                                                          
         MVC   P+40(5),0(R3)                                                    
         EDIT  (P3,5(R3)),(6,P+50),COMMAS=YES                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
LIST824  LA    R3,8(,R3)                                                        
         SPACE                                                                  
         BCT   R2,LIST820                                                       
         SPACE                                                                  
         MVC   P+39(7),=C'*TOTAL*'                                              
         EDIT  (P4,CABYPASS),(6,P+50),COMMAS=YES                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         LA    R4,STATABLN                                                      
         L     R5,=A(STATABLE)                                                  
         OC    0(5,R5),0(R5)                                                    
         BZ    LISTX                                                            
         SR    R0,R0                                                            
         LR    RE,R4                                                            
         LR    RF,R5                                                            
LIST830  DS   0H                                                                
         OC    0(5,RF),0(RF)                                                    
         BZ    LIST834                                                          
         LA    RF,8(,RF)                                                        
         BCTR  R0,0                                                             
         BCT   RE,LIST830                                                       
         DC    H'0'                                                             
LIST834  DS   0H                                                                
         LPR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(QSORT),DMCB,(R5),(R0),8,5,0                                   
         SPACE                                                                  
         MVC   P+3(25),=C'STATIONS FOUND AND BILLED'                            
LIST840  DS   0H                                                                
         MVC   P+30(5),0(R5)                                                    
         EDIT  (P3,5(R5)),(6,P+40),COMMAS=YES                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R5,8(,R5)                                                        
         BCT   R0,LIST840                                                       
LISTX    XIT1                                                                   
         LTORG                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* SPEZFFFD                                                                      
* EZBLOCK                                                                       
* CTGENFILE                                                                     
       ++INCLUDE SPGENEZ                                                        
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPEZFFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
* SPEZFE3D                                                                      
       ++INCLUDE SPEZFE3D                                                       
         PRINT OFF                                                              
         EJECT                                                                  
* DDGENTWA                                                                      
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
* DMWRKRD                                                                       
       ++INCLUDE DMWRKRD                                                        
         EJECT                                                                  
* DMWRKRK                                                                       
       ++INCLUDE DMWRKRK                                                        
         EJECT                                                                  
* SPGENSTA                                                                      
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
* CTGENFILE                                                                     
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
* SPEZBILL                                                                      
       ++INCLUDE SPEZBILL                                                       
         EJECT                                                                  
* SPEZMOV                                                                       
       ++INCLUDE SPEZMOV                                                        
         EJECT                                                                  
*SPEZFWORKD                                                                     
       ++INCLUDE SPEZFWORKD                                                     
         PRINT ON                                                               
* DSECT FOR THIS PROGRAM *                                                      
         SPACE                                                                  
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
RELO     DS    A                                                                
SVRC     DS    A                                                                
SORTRECA DS    A                                                                
ABILTAB  DS    A                                                                
AMOVTAB  DS    A                                                                
MEDCT    DS    H                                                                
TMOVREC  DS    PL4                 TOTAL MOVE RECS READ                         
UMOVREC  DS    PL4                 MOVE RECS USED                               
CABYPASS DS    PL4                                                              
         SPACE                                                                  
         SPACE                                                                  
* REGISTER RECORD                                                               
         SPACE                                                                  
REGREC   DS    0CL(REGEND-REGTYP)                                               
REGTYP   DS    CL1                                                              
REGID    DS    CL8                                                              
REGSOL   DS    CL6                                                              
REGINV   DS    PL4                                                              
REGINVT  DS    PL4                                                              
REGINVR  DS    PL4                                                              
REGINVC  DS    PL4                                                              
REGINVX  DS    PL4                                                              
REGSPT   DS    PL4                                                              
REGDOL   DS    PL6                                                              
REGDUE   DS    PL8                                                              
REGNAME  DS    CL30                                                             
REGINVNO DS    PL3                                                              
REGDUPCT DS    PL3                                                              
REGEND   EQU   *                                                                
         SPACE                                                                  
TREGINV  DS    PL6                                                              
TREGINVT DS    PL6                                                              
TREGINVR DS    PL6                                                              
TREGINVC DS    PL6                                                              
TREGINVX DS    PL6                                                              
TREGSPT  DS    PL6                                                              
TREGDOL  DS    PL8                                                              
TREGDUE  DS    PL8                                                              
TREGDUP  DS    PL4                                                              
TREGINVS DS    PL4                                                              
         SPACE                                                                  
RQSTA    DS    CL5                 REQUESTED STATION                            
RQDTES   DS   0CL12                          PERIOD DATES                       
RQDTSTR  DS    CL6                                  DATE START                  
RQDTEND  DS    CL6                                  DATE END                    
         SPACE                                                                  
BILLMON  DS    CL6                 MON/YR                                       
INVCDATE DS    CL8                 MONDA/YR                                     
INVMON   DS    CL2                 NUMERIC YEAR, MONTH 1-9, A, B, C             
INVNUM   DS    PL3                                                              
         SPACE                                                                  
RUID     DS    CL8                           USER                               
RUIDNUM  DS    XL2                                                              
         SPACE                                                                  
PRNTSTA  DS    CL7                 CURRENT BATCH STATION                        
         SPACE                                                                  
BILLSW   DS    CL1                                                              
SVBILL   DS    CL1                                                              
SVRATE   DS    CL1                                                              
SVFRMT   DS    CL2                                                              
         SPACE                                                                  
FILTERS  DS    0CL(FILTERND-FUID)                                               
FUID     DS    CL8                                                              
FUIDNUM  DS    XL2                 USER ID (DDS TERMS ONLY)                     
FTRTRACE DS    CL1                 TRACE                                        
FTRDUP   DS    CL1                 PRINT DUPLICATES                             
FTRCLTN  DS    CL25                EASI CLIENT NAME                             
FTRCLTNL DS    XL1                                  LENGTH                      
FTRPRDN  DS    CL25                EASI PRODUCT NAME                            
FTRPRDNL DS    XL1                                  LENGTH                      
FTRSRCE  DS    CL4                                                              
FTRMOS   DS    CL6                 MONTH OF SERVICE DATE                        
FTRMEDIA DS    CL1                 MEDIA                                        
AGYSTASW DS    CL1                 ONLY BILL AGENCIES/STATIONS                  
HOLDSIGN DS    CL1                                                              
FILTERND EQU   *                                                                
         SPACE                                                                  
CTRS     DS    0PL5                                                             
RINVCT   DS    PL5                 BILLING INV RECS READ                        
UINVCT   DS    PL5                 BILLING INV RECS USED                        
SRECCT   DS    PL5                 BILLING INV RECS TO SORT                     
         SPACE                                                                  
DUPCT    DS    PL5                 TOTAL DUPLICATE BILLING RECORDS              
DUPCTB   DS    PL5                 DUPLICATE BILLING RECORDS FOR BILL           
         SPACE                                                                  
NCTRS    EQU   (*-CTRS)/5                                                       
         SPACE                                                                  
FIRSTSW  DS    CL1                                                              
SAVSYS   DS    XL1                 SAVE SYSTEM FROM UTL                         
         SPACE                                                                  
WORKCT   DS    PL4                                                              
WORKCT1  DS    PL4                                                              
WORKCH1  DS    PL4                                                              
WORKCT2  DS    PL4                                                              
WORKCH2  DS    PL4                                                              
WORKCT3  DS    PL4                                                              
WORKCH3  DS    PL4                                                              
WORKCT4  DS    PL4                                                              
WORKCH4  DS    PL4                                                              
         SPACE                                                                  
INVCT    DS    PL4                                                              
INVCTT   DS    PL4                                                              
INVCTR   DS    PL4                                                              
INVCTC   DS    PL4                                                              
INVCTX   DS    PL4                                                              
INVSPT   DS    PL4                                                              
INVDOL   DS    PL6                                                              
INVCHRG  DS    PL4                                                              
*                                                                               
* FORMAT IS 1 MEDIA, 4 INV CT, 4 SPOT CT, 6 GROSS DOL, 5 CHARGES                
*                                                                               
MEDCTS   DS    CL240               12 SETS OF CTS                               
         SPACE                                                                  
AINVCT   DS    PL4                                                              
AINVSPT  DS    PL4                                                              
AINVDOL  DS    PL8                                                              
AINVCHRG DS    PL5                                                              
AMEDCTS  DS    CL240               12 SETS OF CTS                               
         SPACE                                                                  
SINVCT   DS    PL4                                                              
SINVSPT  DS    PL4                                                              
SINVDOL  DS    PL8                                                              
SINVCHRG DS    PL5                                                              
SMEDCTS  DS    CL240               12 SETS OF CTS                               
         SPACE                                                                  
TINVCT   DS    PL4                                                              
TINVSPT  DS    PL4                                                              
TINVDOL  DS    PL8                                                              
TINVCHRG DS    PL5                                                              
TMEDCTS  DS    CL240               12 SETS OF CTS                               
         SPACE                                                                  
BYPASSCT DS    PL3                                                              
BYPASSCH DS    PL8                                                              
         SPACE                                                                  
SVINAC   DS    CL6                 SAVED INCOME ACCT                            
SVCSTC   DS    CL6                                                              
         EJECT                                                                  
* EZBILL RECORDS CREATED BY EZLOAD                                              
         SPACE                                                                  
         DS    CL8                                                              
EZBILL   DS   0CL521                                                            
EZBILLT  DS    CL1                                                              
EZBILLS  DS    CL8                                                              
EZBILLI  DS    CL(EZBILLED-EZBILD)                                              
         DS    CL(L'EZBILL-(L'EZBILLT+L'EZBILLS+L'EZBILLI))                     
         SPACE 3                                                                
         ORG   EZBILLI                                                          
EZBILD   EQU   *                                                                
EZBAGY   DS    CL8                  1  -   8                                    
EZBSRC   DS    CL4                  9  -  12                                    
EZBMEDIA DS    CL1                 13  -  13                                    
EZBCALL  DS    CL5                 14  -  18                                    
EZBNET   DS    CL4                 19  -  22                                    
EZBMOS   DS    CL4                 23  -  26                                    
EZBTODAY DS    CL6                 27  -  32                                    
EZBADVNM DS    CL25                33  -  57                                    
EZBPRDNM DS    CL25                58  -  82                                    
EZBINVNO DS    CL10                83  -  92                                    
EZBNSPTS DS    CL5                 93  -  97                                    
EZBGDOL  DS    CL11                98  - 108                                    
EZBNDOL  DS    CL11               109  - 119                                    
EZBAGYNM DS    CL30               120  - 149                                    
EZBAGYAD DS    CL120              150  - 269                                    
EZBSTADR DS    CL150              270  - 419                                    
         SPACE                                                                  
EZBORGMD DS    CL1                 ADDED FIELD - ORIG MEDIA                     
EZBILLED EQU   *                                                                
         DS    F                                                                
SOLMAS   DS    CL256                                                            
         DS    F                                                                
SOLINV   DS    CL256                                                            
         SPACE 3                                                                
* BILLING TABLE BUILT FROM ZB BILLING RECORDS                                   
         SPACE                                                                  
BILTABD  DSECT                                                                  
BILENT   DS   0CL20                A=AGENCY, S=STATION                          
BILTYP   DS    CL1                 A=AGENCY, S=STATION                          
BILID    DS    CL8                 BILL ID-AGY SIGNON ID/STATION CAL            
BILNK    DS    CL8                 LINK FIELD                                   
BILABL   DS    CL1                 BILLABLE Y OR N                              
BILFMT   DS    CL2                 BILL FORMAT - FUTURE USE                     
BILNEXT  DS    0CL1                                                             
         SPACE 3                                                                
* MOVE TABLE BUILT FROM ZM BATCH MOVE RECORDS                                   
         SPACE                                                                  
MOVTABD  DSECT                                                                  
MOVENT   DS   0CL(MOVNEXT-MOVBDTE)                                              
MOVBDTE  DS    CL6                 BATCH DATE                                   
MOVDATE  DS    CL2                 MOVE DATE                                    
MOVTIME  DS    CL2                 MOVE TIME                                    
MOVCOM   DS   0CL26                                                             
MOVOAGY  DS    CL8                 OLD AGENCY                                   
MOVSRCE  DS    CL4                 SOURCE                                       
MOVMEDIA DS    CL1                 MEDIA                                        
MOVCALL  DS    CL5                 CALL LETTERS                                 
MOVNET   DS    CL4                 NETWORK                                      
MOVMOS   DS    CL4                 MONTH OF SERVICE                             
MOVINV   DS    CL10                INVOICE NUMBER                               
MOVNAGY  DS    CL8                 NEW AGENCY                                   
MOVUSED  DS    CL1                 1=USED, 0=AVAILABLE FOR USE                  
MOVNEXT  EQU   *                                                                
         SPACE 3                                                                
* MEDIA COUNTS DSECT                                                            
         SPACE                                                                  
MEDCTD   DSECT                                                                  
MEDMED   DS    CL1                                                              
MEDINV   DS    PL4                                                              
MEDSPT   DS    PL4                                                              
MEDDOL   DS    PL6                                                              
MEDCHG   DS    PL5                                                              
         SPACE 3                                                                
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL3                                                              
PCOUNT   DS    CL11                                                             
         DS    CL4                                                              
PINVCHG  DS    CL4                                                              
         DS    CL19                                                             
PINVS    DS    CL6                 TOTAL INVOICES                               
         DS    CL19                                                             
PCHRG    DS    CL13                                                             
         ORG   P                                                                
         DS    CL2                                                              
PREGTYP  DS    CL1                                                              
         DS    CL2                                                              
PREGINVN DS    CL8                                                              
         DS    CL2                                                              
PREGID   DS    CL8                                                              
         DS    CL2                                                              
PREGNAM  DS    CL30                                                             
*        DS    CL1                                                              
*REGSPT  DS    CL10                                                             
         DS    CL1                                                              
PREGIVC  DS    CL5                                                              
         DS    CL1                                                              
PREGIVCT DS    CL5                                                              
         DS    CL1                                                              
PREGIVCR DS    CL5                                                              
         DS    CL1                                                              
PREGIVCC DS    CL5                                                              
         DS    CL1                                                              
PREGIVCX DS    CL4                                                              
         DS    CL1                                                              
PREGDL   DS    CL14                                                             
         DS    CL1                                                              
PREGDUE  DS    CL11                                                             
         DS    CL2                                                              
PREGDUP  DS    CL3                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPEZF19   05/01/02'                                      
         END                                                                    
