*          DATA SET ACCLB0AB   AT LEVEL 017 AS OF 12/22/99                      
*PHASE T6210AB                                                                  
*&&      SET   NOP=N                                                            
CLB0A    TITLE '- BILL PROGRAM - EDIT SCREEN'                                   
CLB0A    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CLBA**,CLEAR=YES,RR=RE                                       
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     RC,AOVERWRK         RC=A(LOCAL WORKING STORAGE)                  
         USING EWORKD,RC                                                        
         ST    RE,RELO             SAVE RELOCATION FACTOR                       
*                                                                               
         TM    BCINDS2,BCINTRS                                                  
         BZ    *+8                                                              
         MVI   TWASCRN,0                                                        
*        MVI   CSQRTN,1                                                         
*        TM    BCINDS2,BCIXITS                                                  
*        BZ    *+8                                                              
*        MVI   TWASCRN,0                                                        
         BAS   RE,INIT             INITIALIZE VALUES                            
*                                                                               
         CLI   TWASCRN,S#BILEDT    TEST FIRST TIME                              
         BE    MAIN02                                                           
         GOTO1 AFIRST                                                           
         B     MAINX                                                            
*                                                                               
MAIN02   DS    0H                                                               
         CLI   BCPFKEY,PFKADDP     ADD PARAGRAPH?                               
         BNE   MAIN04                                                           
         GOTO1 AADDPARA                                                         
         B     MAINX                                                            
*                                                                               
MAIN04   MVI   PRMODE,PRCHANGQ     CHANGE PARAGRAPH TYPE/DESCRIPTION            
         GOTO1 APARAREC                                                         
         BNE   MAINX                                                            
*                                                                               
         GOTO1 AVALOPTS            VALIDATE COMMAND LINE                        
         BNE   MAINX                                                            
         GOTO1 AEDTSCRN            EDIT SCREEN                                  
         BNE   MAINX                                                            
         GOTO1 ACMDVAL             DEAL WITH LINE COMMANDS                      
         BNE   MAINX                                                            
*                                                                               
         TM    EDTPARAH+FHIID,FHIIVA PREVIOUSLY VALIDATED?                      
         BO    MAIN06                                                           
         TM    ESEBFLG,BLKCOUTQ    CANNOT CHNG PARA IF BLOCK INCOMPLETE         
         BO    MAIN06                                                           
         GOTO1 AGPARA,0            READ AND DISPLAY CURRENT PARAGRAPH           
         BNE   MAINX                                                            
         MVI   ESSTLIN,1           SET START LINE TO BEGINING OF PARA           
         MVI   ESCOL#1,1           SET START COLUMN TO FAR LEFT                 
         GOTO1 ADISP               DISPLAY SCREEN                               
         GOTO1 ASETMSG             SET MESSAGE                                  
         B     MAINX                                                            
*                                                                               
MAIN06   CLI   ESCMND,CMNDELQ      DELETE PARAGRAPH?                            
         BNE   MAIN08                                                           
         GOTO1 ADELPARA            DELETE PARAGRAPH RECORD                      
*                                                                               
MAIN08   GOTO1 ASCROLL             DEAL WITH SCROLLING                          
*                                                                               
         GOTO1 AFCTXT              FIND/CHANGE TEXT                             
         BNE   MAINX                                                            
*                                                                               
         CLI   ESLACTV,0           NO ACTIVE LINE RECORDS?                      
         BNE   MAIN10                                                           
         OC    ESEINSB,ESEINSB     ANY INSERTS?                                 
         BNZ   MAIN10                                                           
         MVI   LRMODE,LRADDQ       ADD A BLANK LINE RECORD                      
         MVC   LRPARA,ESPARA       PARAGRAPH INDEX                              
         MVI   LRLINE,1                                                         
         GOTO1 ALINEREC,0          ADD/REACTIVATE/CHANGE LINE                   
         MVI   PRMODE,PRCHANGQ                                                  
         GOTO1 APARAREC            CHANGE PARAGRAPH RECORD IF NECESSARY         
*                                                                               
MAIN10   GOTO1 ADISP               DISPLAY SCREEN                               
         GOTO1 ASETMSG             SET MESSAGE                                  
*                                                                               
MAINX    DS    0H                                                               
         TM    BCINDS1,BCIXERR                                                  
         BO    EXIT                                                             
         TM    EWINDS,EWITOT       TEST TOTALS NEED UPDATING                    
         BZ    MAINX2                                                           
         GOTO1 AUPDTOT,BOPARM,ESPARA                                            
*                                                                               
MAINX2   GOTO1 ASAVOWS,BOPARM,(C'S',L'ESAVE),ESAVE                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALIZE WORKING STORAGE ETC.                                     *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
*                                                                               
         LA    R0,AROUTN           R0=(NUMBER OF ROUTINES)                      
         LA    RF,AREROUT          RF=A(RELOCATED ADDRESSES)                    
         XR    RE,RE                                                            
         LA    R1,ROUT                                                          
INIT02   ST    R1,0(RF)                                                         
         STC   RE,0(RF)                                                         
         LA    RE,1(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,INIT02                                                        
*                                                                               
         LA    R0,ATABN            R0=(NUMBER OF TABLES)                        
         LA    RF,ARETAB           RF=A(RELOCATED ADDRESSES)                    
         LA    R1,ATAB             R1=A(UNREOLOCATED ADDRESSES)                 
INIT03   L     RE,0(R1)                                                         
         A     RE,RELO                                                          
         ST    RE,0(RF)                                                         
         LA    RF,4(RF)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,INIT03                                                        
*                                                                               
         LH    R1,=Y(LC@NRTV-TWAD)                                              
         LA    R1,TWAD(R1)                                                      
         MVC   MY@NRTV,0(R1)                                                    
*                                                                               
         PUSH  USING                                                            
         USING TRNRECD,BOWORK1                                                  
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   TRNKACT,BCJOBCOD                                                 
         L     RE,AGOPBLK                                                       
         MVC   GOABEXT-GOBLOCKD(L'GOABEXT,RE),AJOBBLK                           
         GOTO1 AGETOPT,BOPARM,TRNKEY                                            
         POP   USING                                                            
*                                                                               
         SR    RF,RF                                                            
         IC    RF,CULANG                                                        
         SLL   RF,2                                                             
         L     RE,ATRUPTAB                                                      
         AR    RE,RF                                                            
         ICM   RF,15,0(RE)                                                      
         LA    RF,CLB0A(RF)                                                     
         ST    RF,ATRUP            RF=A(TRANSLATE TO UPPER CASE TAB)            
         L     R3,ALEDCTAB         R3=A(LINE EDIT TABE)                         
         USING LEDTABD,R3                                                       
         LA    RF,LEHLNQ(R3)                                                    
         ST    RF,ALENTRY          DEFAULT TO UK IF NO ENTRY FOUND              
INIT06   CLI   LEHLANG,EOT         END OF TABLE?                                
         BE    INIT10                                                           
         CLC   CULANG,LEHLANG      MATCH ON LANGUAGE                            
         BE    INIT08                                                           
         SR    RF,RF                                                            
         IC    RF,LEHLEN           RF=L'(LANGUAGE ENTRY)                        
         AR    R3,RF                                                            
         B     INIT06                                                           
INIT08   LA    R3,LEHLNQ(R3)                                                    
         ST    R3,ALENTRY          R3=A(LINE EDIT COMMAND ENTRIES)              
         DROP  R3                                                               
*                                                                               
INIT10   OI    BASOPTH+FHATD,FHATLC SET TO LOWER CASE EACH TIME THROUGH         
         OI    BASOPTH+FHOID,FHOITR                                             
*                                                                               
         CLI   TWASCRN,S#BILEDT    TEST FIRST TIME                              
         BNE   INIT20              NO - RESTORE SAVE AREA                       
         GOTO1 ASAVOWS,BOPARM,(C'R',L'ESAVE),ESAVE                              
         ICM   R1,15,ESATWA        RELOCATE SAVED TWA ADDRESSESS                
         BZ    INIT14                                                           
         S     R1,ATWA                                                          
         LA    R0,ESARELON                                                      
         LA    RF,ESARELO                                                       
INIT12   L     RE,0(RF)                                                         
         SR    RE,R1                                                            
         ST    RE,0(RF)                                                         
         LA    RF,L'ESARELO(RF)                                                 
         BCT   R0,INIT12                                                        
INIT14   MVC   ESATWA,ATWA                                                      
*                                                                               
INIT20   L     RF,AINP             RF=A(TIOB)                                   
         USING TIOBD,RF                                                         
         MVC   ESCURD,TIOBCURD     SAVE DISPLACEMENT TO CURSOR POSITION         
         XC    TIOBCURD,TIOBCURD                                                
         DROP  RF                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BRANCH TO ROUTINE                                                   *         
***********************************************************************         
         SPACE 1                                                                
ROUT     DS    0H                                                               
         NTR1  ,                                                                
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         L     R8,AROUT(RF)                                                     
         A     R8,RELO                                                          
         BR    R8                                                               
         SPACE 1                                                                
AROUT    DS    0F                  * ROUTINES *                                 
         DC    A(FIRST)            FIRST TIME IN                                
         DC    A(DISP)             DISPLAY SCREEN                               
         DC    A(GBILL)            READ/ADD BILL RECORD                         
         DC    A(GPARA)            READ AND DISPLAY PARAGRAGPH RECORD           
         DC    A(SETINI)           SET INITIAL VALUES                           
         DC    A(TYPVAL)           FORMAT TYPE VALIDATION                       
         DC    A(TYPSET)           SET FORMAT TYPE INFORMATION                  
         DC    A(TYPDIS)           DISPLAY FORMAT TYPE NAME                     
         DC    A(ADDPARA)          ADD NEW PARAGRAPH RECORD                     
         DC    A(DELPARA)          DELETE PARAGRAPH RECORD OR ALL LINES         
         DC    A(EDTSCRN)          EDIT SCREEN LINES                            
         DC    A(DISPARA)          DISPLAY PARAGRAPH DATA                       
         DC    A(DISHEAD)          DISPLAY HEADER LINES                         
         DC    A(DISSCRN)          DISPLAY SCREEN LINES                         
         DC    A(SETMSG)           SET MESSAGE                                  
         DC    A(CMDDIS)           DISPLAY LINE COMMANDS                        
         DC    A(CMDVAL)           DEAL WITH LINE COMMANDS                      
         DC    A(CHKCONF)          CHECK CONFLICTS / BUILD BLOCK LISTS          
         DC    A(CHKPSIZ)          CHECK PARAGRAPH SIZE                         
         DC    A(DEL)              DELETE LINES FROM A PARAGRAPH                
         DC    A(CMLN)             COPY/MOVE LINES OF A PARAGRAPH               
         DC    A(REPT)             REPEAT LINES WITHIN A PARAGRAPH              
         DC    A(ADDS)             ADD SUB-TOTAL LINE WITHIN A PARA             
         DC    A(ADDT)             ADD TOTAL LINE WITHIN A PARAGRAPH            
         DC    A(INBVAL)           INITIALIZE BLOCK VALUES FOR SCREEN           
         DC    A(VALOPTS)          VALIDATE OPTIONS FIELD                       
         DC    A(SCROLL)           VERTICAL/HORIZONTAL SCROLLING                
         DC    A(LINEREC)          CHANGE/ADD A LINE RECORD                     
         DC    A(PARAREC)          CHANGE/ADD A PARAGRAPH RECORD                
         DC    A(BILLREC)          CHANGE A BILL RECORD                         
         DC    A(DEFCURS)          SET DEFAULT CURSOR POSITION                  
         DC    A(ALTBLKV)          ALTER BLOCKS IF PREV LINE COMMAND            
         DC    A(FCTXT)            FIND/CHANGE TEXT STRING                      
         DC    A(CMI)              COPY/MOVE INDEXES                            
         DC    A(SETLC)            SET LINE/COLUMN AND CURSOR POSITION          
         DC    A(CHGTXT)           CHANGE MATCHED STRING                        
         DC    A(COMBLD)           BUILD STANDARD COMMENT TABLE                 
         DC    A(COMPRC)           PROCESS STANDARD COMMENT TABLE               
         DC    A(CLRTWA)           CLEAR TWA                                    
         DC    A(BLDTOP)           BUILD TOP LINE OF SCREEN                     
         DC    A(BLDEND)           BUILD END-OF-PARAGRAPH LINE                  
         DC    A(BLDFOOT)          BUILD FOOTLINE                               
         DC    A(SETLAD)           SET DATA NEW LINE SCREEN ADDRESSES           
         DC    A(INITX)            INITIALIZE NEW TX TO TEXT DEFAULT            
         DC    A(GETTX)            GET TXD FROM LINE RECORD                     
         DC    A(PUTTX)            PUT TXD INTO LINE RECORD                     
         DC    A(BLDTX)            BUILD TXD LINE ONTO SCREEN                   
         DC    A(DISTX)            DISPLAY TXD ONTO TEXT LINE                   
         DC    A(VALTX)            VALIDEATE TXD TEXT LINE                      
         DC    A(INITSAR)          INITIALIZE FOR TSAR RECORDS                  
         DC    A(SAVTX)            SAVE TXD ON TSAR RECORD                      
         DC    A(RESTX)            GET TXD FROM TSAR RECORD                     
         DC    A(FMTTX)            FORMAT NUMBERS                               
         DC    A(GETNUM)           RETURN NUMTABD ENTRY                         
         DC    A(DISNUM)           DISPLAY NUMBER                               
         DC    A(VALNUM)           VALIDATE NUMBER                              
         DC    A(UPDTOT)           UPDATE PARAGRPAH TOTALS                      
         DC    A(UPDSCR)           UPDATE TOTAL LINES ON SCREEN                 
         DC    A(ADDTOT)           ADD ONE TXD NUMBERS TO ANOTHER               
         DC    A(PUTTOT)           PUT ONE TXD NUMBERS INTO ANOTHER             
         DC    A(CLCTOT)           COMPARE TXD TOTALS                           
         DC    A(DISAMT)           DISPLAY NET/COMM BILL AMOUNTS                
         DC    A(INSLAY)           INSERT ENTRY INTO LAYOUTD TABLE              
         DC    A(SCRLAY)           FORMAT LAYOUT TABLE FOR SCREEN               
AROUTN   EQU   (*-AROUT)/L'AROUT                                                
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS                                                         *         
***********************************************************************         
         SPACE 1                                                                
EXITH    CLI   *,0                 SET CC=HIGH                                  
         B     EXIT                                                             
EXITN    DS    0H                                                               
EXITL    CLI   *,FF                SET CC=LOW                                   
         B     EXIT                                                             
EXITY    CR    RB,RB               SET CC=EQUAL                                 
*                                                                               
EXIT     XIT1  ,                                                                
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO CREATE SPACE IN W/S FOR NEW TXD                          *         
*                                                                     *         
* NTRY: R1 = X, REGISTER NUMBER TO HOLD A(TXD)                        *         
* EXIT: RX = A(TXD)                                                   *         
***********************************************************************         
         SPACE 1                                                                
MAKETX   L     RF,4(RD)            CREATE W/S SPACE FOR TXD                     
         SLL   R1,4                                                             
         EX    R1,MAKETXEX                                                      
         LA    RD,TXL(RD)                                                       
         ST    RD,8(RF)                                                         
         ST    RF,4(RD)                                                         
         BR    RE                                                               
MAKETXEX LR    R0,RD                                                            
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO COPY ONE TXD INTO ANOTHER                                *         
*                                                                     *         
* NTRY: P1 = A(DESTINATION TXD)                                       *         
*       P2 = A(SOURCE TXD)                                            *         
***********************************************************************         
         SPACE 1                                                                
MVCTX    L     RF,4(R1)                                                         
         L     R1,0(R1)                                                         
         MVC   0(TXL-256,R1),0(RF)                                              
         MVC   256(TXL-256,R1),256(RF)                                          
         BR    RE                                                               
         SPACE 1                                                                
***********************************************************************         
* SPACE FOR LITERAL POOL                                              *         
***********************************************************************         
         SPACE 1                                                                
LTORG    DS    0H                                                               
         ORG   CLB0A+X'0A00'                                                    
LTORGX   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* FIRST TIME IN                                                       *         
*                                                                     *         
* EXIT: CC = NOT EQUAL IF ERROR                                       *         
***********************************************************************         
         SPACE 1                                                                
FIRST    DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         MVI   ESPIND,ESPIDFT      DEFAULT TO DRAFT BILL                        
         SR    RE,RE               TEST WHETHER WE CAME FROM LIST               
         IC    RE,TWASESNL                                                      
         SLL   RE,1                                                             
         LA    RE,TWASESRA-L'TWASESRA(RE)                                       
         CLI   CSACT-CSRECACT(RE),ACTLST                                        
         BNE   FIRST04             NOT FROM LIST - MUST BE DRAFT                
         L     RF,ALSVALS                                                       
         USING LSVALSD,RF                                                       
         USING TLSTD,LSTLST                                                     
         USING PBRPAS,TLBPAS                                                    
         CLI   PBRPTYP,PBRPTYPQ                                                 
         BNE   FIRST04                                                          
         CLI   PBRPSUB,PBRPPASQ                                                 
         BNE   FIRST02                                                          
         CLI   PBRPIND,PBRPILVE                                                 
         BNE   FIRST02                                                          
         MVI   ESPIND,ESPILVE      BILL IS LIVE                                 
FIRST02  TM    PBRKSTA2,PBRSAUTR                                                
         BZ    FIRST04                                                          
         OI    ESPIND,ESPIAUR      BILL IS AUTO REVERSAL                        
         DROP  RF                                                               
*                                                                               
FIRST04  XC    BASOPT,BASOPT       CLEAR OPTIONS FIELD                          
         OI    BASOPTH+FHOID,FHOITR                                             
         MVI   BCPFKEY,0           INITIALISE PFKEY                             
         GOTO1 AOVRSCR,BCDMCB,('S#BILEDT',BASOLAYH) LOAD EDIT SCREEN            
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    ESPIND,ESPILVE+ESPIAUR                                           
         BZ    *+8                                                              
         OI    EDTTYPEH+FHATD,FHATPR                                            
*                                                                               
         CLI   CSACT,ACTFFRM       FIRST TIME FOR FREE-FORMAT                   
         BNE   FIRST10                                                          
         GOTO1 AGBILL              ADD BILL RECORD                              
         BNE   FIRSTN                                                           
         MVI   PRMODE,PRADDQ                                                    
         GOTO1 APARAREC            ADD A PARAGRAPH RECORD                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   LRMODE,LRADDQ       ADD A BLANK LINE RECORD                      
         MVC   LRPARA,ESPARA       PARAGRAPH INDEX                              
         MVI   LRLINE,1                                                         
         GOTO1 ALINEREC,0          ADD/REACTIVATE/CHANGE LINE                   
         MVI   PRMODE,PRCHANGQ                                                  
         GOTO1 APARAREC   ??       CHANGE PARAGRAPH RECORD IF NECESSARY         
         BE    FIRSTY                                                           
         DC    H'0'                                                             
*                                                                               
FIRST10  CLI   CSACT,ACTAFRM       ACTION AUTO-FORMAT                           
         BNE   FIRST20                                                          
         GOTO1 VCOLY,BOPARM,('O#BILFRM',0),0,0                                  
         L     RF,0(R1)                                                         
         BASR  RE,RF                                                            
         BE    FIRST20                                                          
         OI    BCINDS1,BCIXERR                                                  
         B     FIRSTN                                                           
*                                                                               
FIRST20  OC    CSBILNUM,CSBILNUM   ENSURE BILL NUMBER IS SET                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGBILL              GET BILL REC                                 
         BNE   FIRSTN                                                           
         XR    R1,R1                                                            
         CLI   CSPAROFF,0                                                       
         BE    *+8                                                              
         LA    R1,CSPAROFF                                                      
         GOTO1 AGPARA              READ AND DISPLAY CURRENT PARAGRAPH           
         BNE   FIRSTN                                                           
*                                                                               
FIRSTY   MVC   EDTBILN,CSBILNUM    DISPLAY BILL NUMBER                          
         OI    EDTBILNH+FHOID,FHOITR                                            
         CLI   CSACT,ACTEDT                                                     
         BE    FIRSTY2                                                          
         MVI   BOHALF1,RECBIL      SET ACTION TO EDIT                           
         MVI   BOHALF1+1,ACTEDT                                                 
         GOTO1 ARECACT,BOHALF1                                                  
*                                                                               
FIRSTY2  DS    0H                                                               
         GOTO1 AINITSAR                                                         
         GOTO1 ADISP               DISPLAY SCREEN                               
         GOTO1 ASETMSG             SET MESSAGE                                  
         B     EXITY                                                            
*                                                                               
FIRSTN   DS    0H                                                               
         TM    BCINDS1,BCIXERR                                                  
         BZ    FIRSTN02                                                         
         GOTO1 AXITSES                                                          
         GOTO1 ARECACT,CSREC                                                    
         B     EXITN                                                            
*                                                                               
FIRSTN02 GOTO1 AINITSAR                                                         
         B     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY SCREEN                                                      *         
***********************************************************************         
         SPACE 1                                                                
DISP     DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         GOTO1 ACLRTWA,EDTOLAYH                                                 
         GOTO1 ADISPARA            DISPLAY PARAGRAPH DATA                       
         GOTO1 ADISHEAD            DISPLAY HEADER LINE                          
         GOTO1 ADISSCRN            DISPLAY SCREEN                               
         GOTO1 ACMDDIS             DISPLAY LINE COMMANDS                        
         GOTO1 ABLDFOOT            BUILD FOOTLINE                               
         GOTO1 ADEFCURS            DEAL WITH DEFAULT CURSOR POSITION            
*                                                                               
         LA    R2,TWASCR                                                        
         XR    RE,RE                                                            
         LA    RF,EDTOLAYH-1                                                    
         USING FHD,R2                                                           
DISP02   ICM   RE,1,FHLN                                                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         OI    FHOI,FHOITR                                                      
         BXLE  R2,RE,DISP02                                                     
         DROP  R2                                                               
*                                                                               
DISPLAYX B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ BILL RECORD (ACTION EDIT)                                      *         
* ADD BILL RECORD (ACTION ADD)                                        *         
***********************************************************************         
         SPACE 1                                                                
GBILL    DS    0H                                                               
         USING *,R8                                                             
         LA    R2,EDTBILNH         R2=A(BILL NUMBER FIELD HEADER)               
         USING FHD,R2                                                           
         ST    R2,FVADDR                                                        
         CLI   CSACT,ACTFFRM       ACTION ADD?                                  
         BE    GBIL10                                                           
         MVC   EDTBILN,CSBILNUM                                                 
         OI    EDTBILNH+FHOID,FHOITR                                            
         B     GBIL70                                                           
*                                                                               
GBIL10   LA    R3,IOKEY            R3=A(KEY OF PRODUCTION LEDGER REC)           
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN      COMPANY                                      
         MVC   ACTKUNT(L'BCCPYPRD),BCCPYPRD PRODUCTION LEDGER                   
         GOTO1 AIO,IO2+IOACCMST+IORDUP                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO2             R3=A(PRODUCTION LEDGER RECORD)               
         LA    R3,ACTRFST          R3=A(FIRST ELEMENT ON RECORD)                
         USING SCIELD,R3                                                        
GBIL20   CLI   SCIEL,EOR           END OF RECORD                                
         BE    GBIL40                                                           
         CLI   SCIEL,SCIELQ        SUBSIDIARY CASH ELEMENT?                     
         BE    GBIL50                                                           
*                                                                               
GBIL30   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,SCILN                                                         
         AR    R3,R0                                                            
         B     GBIL20                                                           
*                                  ADD A DRAFT BILL NUMBER TO LEDGER            
GBIL40   LA    R3,BOELEM           R3=A(ELEMENT WORK AREA)                      
         USING SCIELD,R3                                                        
         XC    SCIEL(SCILN1Q),SCIEL                                             
         MVI   SCIEL,SCIELQ        SUBSIDIARY CASH ELEMENT                      
         MVI   SCITYPE,SCITDBNO    DRAFT BILL NUMBER ELEMENT                    
         MVI   SCILN,SCILN1Q       SET LENGTH                                   
         ZAP   SCIAMNT,=P'0'       INITIALISE DRAFT BILL SEQUENCE               
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO2,(R3),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   R3,15,16(R1)        R3=A(NEW ELEMENT)                            
         B     *+12                                                             
*                                                                               
GBIL50   CLI   SCITYPE,SCITDBNO    DRAFT BILL NUMER SEQ TYPE?                   
         BNE   GBIL30                                                           
GBIL60   AP    SCIAMNT,=P'1'       INCREMENT DRAFT BILL NUMBER SEQUENCE         
         UNPK  BCDUB,SCIAMNT                                                    
         MVC   CSBILNUM,BCDUB+(L'BCDUB-L'CSBILNUM)                              
         OI    CSBILNUM+L'CSBILNUM-1,X'F0' UPDATE SAVED BILL NUMBER             
*                                                                               
GBIL70   LA    R4,IOKEY            R4=A(KEY FOR PRODUCTION BILL RECORD)         
         USING PBRRECD,R4                                                       
         XC    PBRPAS,PBRPAS                                                    
         MVI   PBRPTYP,PBRPTYPQ    RECORD TYPE                                  
         MVC   PBRPCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRPSUB,PBRPPASQ    PASSIVE RECORD                               
         MVC   PBRPBLNO,CSBILNUM   BILL NUMBER                                  
         MVI   PBRPIND,PBRPIDFT    ASSUME DRAFT                                 
         TM    ESPIND,ESPILVE                                                   
         BZ    *+8                                                              
         MVI   PBRPIND,PBRPILVE    SET LIVE                                     
         GOTO1 AIO,IO1+IOACCDIR+IOHIGH GET BILL RECORD                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PBRPAS(PBRPUSER-PBRPAS),IOKEYSAV RECORD FOUND?                   
         BNE   GBIL80                                                           
         CLI   CSACT,ACTFFRM       ACTION ADD?                                  
         BNE   GBIL90                                                           
         CP    SCIAMNT,=P'999999'  MAX BILL NUMBER REACHED?                     
         BNE   GBIL60                                                           
         ZAP   SCIAMNT,=P'0'       RESET BILL NUMBER                            
         B     GBIL60                                                           
         DROP  R3                                                               
*                                                                               
GBIL80   MVC   FVMSGNO,=AL2(AE$RECNF) RECORD NOT FOUND                          
         CLI   CSACT,ACTFFRM       ACTION ADD?                                  
         BNE   GBILERRX                                                         
         B     GBIL120                                                          
*                                                                               
GBIL90   GOTO1 AIO,IO1+IOACCMST+IOGET GET BILL RECORD                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO1             R4=A(BILL RECORD)                            
         MVI   ESCOL#1,1           SET FIRST DISPLAY COLUMN TO START            
         MVI   ESSTLIN,1           SET FIRST DISPLAY LINE TO START              
         MVC   ESJOB,PBRKJOB       SAVE JOB                                     
         MVC   ESSEQ,PBRKSEQ       SAVE SEQUENCE NUMBER (COMPLEMENT)            
*                                                                               
         LA    R4,PBRRFST          R4=A(FIRST ELEMENT)                          
         USING BLHELD,R4                                                        
GBIL100  CLI   BLHEL,EOR           END OF RECORD?                               
         BE    GBIL140                                                          
         CLI   BLHEL,BLHELQ        BILL HEADER RECORD?                          
         BE    GBIL106                                                          
         CLI   BLHEL,NDXELQ        INDEX ELEMENT?                               
         BE    GBIL110                                                          
*                                                                               
GBIL105  SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,BLHLN                                                         
         AR    R4,R0                                                            
         B     GBIL100                                                          
*                                                                               
         USING BLHELD,R4           BILL HEADER RECORD                           
GBIL106  MVC   CSFORMAT,BLHFORM    BILL FORMAT                                  
         MVC   CSFMLANG,BLHLANG    BILL FORMAT LANGUAGE                         
         B     GBIL105                                                          
         DROP  R4                                                               
*                                                                               
         USING NDXELD,R4           INDEX ELEMENT                                
GBIL110  MVC   ESPHIGH,NDXHIGH     HIGHEST PARAGRAPH NUMBER                     
         MVC   ESPACTV,NDXACTV     HIGHEST ACTIVE PARAGRAPH NUMBER              
         SR    RF,RF                                                            
         IC    RF,NDXLN            RF=L'(INDEX ELEMENT)                         
         SH    RF,=Y(NDXINDX+1-NDXELD)                                          
         EX    RF,*+4              RF=(X LENGTH OF INDEX LIST)                  
         MVC   ESPLST(0),NDXINDX   SAVE PARAGRAPH INDEX LIST                    
         B     GBIL105                                                          
         DROP  R4                                                               
*                                                                               
GBIL120  MVC   ESJOB,BCJOBCOD      DISPLAY JOB CODE                             
         GOTO1 AIO,IO2+IOACCMST+IOPUTREC PUT BACK LEDGER RECORD                 
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,IOKEY                                                         
         USING PBRRECD,R4                                                       
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ                                                 
         MVC   PBRKCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRKSUB,PBRKACTQ    ACTIVE RECORD                                
         MVC   PBRKJOB,BCJOBCOD    JOB CODE                                     
         GOTO1 AIO,IO1+IOACCDIR+IOHID GET BILL RECORD                           
         LH    RF,=X'FFFF'         COMP SEQ OF FIRST BILL RECORD                
         CLC   PBRKEY(PBRKSEQ-PBRKEY),IOKEYSAV RECORD FOUND?                    
         BNE   GBIL130                                                          
         CLI   PBRKPARA,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    RF,RF                                                            
         ICM   RF,3,PBRKSEQ        GET SEQUENCE OF LATEST BILL NUMBER           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  RF,0                DECREMENT TO GET SEQ FOR NEW BILL            
GBIL130  L     R4,AIO1             R4=A(IOAREA FOR NEW BILL RECORD)             
         MVC   PBRKEY,IOKEYSAV     RESTORE KEY                                  
         STCM  RF,3,PBRKSEQ        OVERWRITE SEQUENCE                           
         STCM  RF,3,ESSEQ          SAVE SEQUENCE                                
         LA    RF,PBRRFST          RF=A(FIRST ELEMENT)                          
         MVI   0(RF),0             SET END OF RECORD MARKER                     
         LA    RF,1(RF)                                                         
         SR    RF,R4               RF=L'(NEW RECORD)                            
         STCM  RF,3,PBRRLEN        SET LENGTH OF NEW RECORD                     
         LA    R4,BOELEM           R4=A(ELEMENT WORK AREA)                      
         USING BLHELD,R4                                                        
         XC    BLHEL(BLHLNQ),BLHEL                                              
         MVI   BLHEL,BLHELQ        BILL HEADER ELEMENT                          
         MVI   BLHLN,BLHLNQ                                                     
         MVC   BLHJOB,BCJOBCOD     SET JOB CODE                                 
         MVC   BLHBLNO,CSBILNUM    SET BILL NUMBER                              
         MVC   BLHUSER,CUUSER                                                   
         MVC   BLHPERS,CUPASS                                                   
         MVC   BLHCRED,ASCDAT      CREATED DATE                                 
         L     RF,AGOPBLK                                                       
         L     RF,GOABEXT-GOBLOCK(RF)                                           
         USING GOBBLOCK,RF                                                      
         ZAP   BLHDSC,GODSCPCT     DISCOUNT                                     
         ZAP   BLHSCH,GOSRGPCT     SURCHARGE                                    
         DROP  RF                                                               
         MVC   BLHFORM,CSFORMAT    GET BILLING FORMAT                           
         MVC   BLHLANG,CSFMLANG    GET BILLING FORMAT LANGUAGE                  
         MVC   BLHCUR,CSBILCUR     SET CURRENCY CODE IN BILL                    
         CLC   CSBILCUR,CSCPYCUR   TEST AGENCY PRIMARY CURRENCY                 
         BE    GBIL132                                                          
         CLC   BCCPYSEC,CSBILCUR   TEST AGENCY SECOND CURRENCY                  
         BE    GBIL132                                                          
         MVC   BLHRVAL,CSEXCVAL    FOREIGN CURRECNY - SET EXCHG RATE            
GBIL132  GOTO1 VDATCON,BCDMCB,(2,BCTODAYC),(0,BOWORK1)                          
         XR    RF,RF               SET BILL EXPIRY DATE                         
         ICM   RF,1,P#RETDAY       GET NON-STANDARD RETENTION DAYS              
         BNZ   *+8                                                              
         LA    RF,7                DEFAULT IS 7                                 
         GOTO1 VADDAY,BCDMCB,BOWORK1,BOWORK1+6,(RF)                             
         GOTO1 VDATCON,BCDMCB,(0,BOWORK1+6),(2,BLHEXPD)                         
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO1,(R4),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NDXELD,R4                                                        
         XC    NDXEL(NDXLNQ),NDXEL                                              
         MVI   NDXEL,NDXELQ        INDEX ELEMENT                                
         MVI   NDXLN,NDXLNQ                                                     
         MVI   NDXHIGH,1           SET HIGHEST EXISTING PARA                    
         MVI   NDXACTV,1           SET HIGHEST ACTIVE PARA                      
         MVI   NDXINDX,1           SET FOR PARAGRAPH 1                          
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO1,(R4),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IO1+IOACCMST+IOADDREC ADD NEW BILL RECORD                    
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
         LA    R4,IOKEY                                                         
         USING PBRRECD,R4                                                       
         XC    IOKEY,IOKEY                                                      
         MVI   PBRPTYP,PBRPTYPQ    RECORD TYPE                                  
         MVC   PBRPCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRPSUB,PBRPPASQ    PASSIVE RECORD                               
         MVC   PBRPBLNO,CSBILNUM   BILL NUMBER                                  
         MVI   PBRPIND,PBRPIDFT    DRAFT BILL                                   
         MVC   PBRPUSER,CUUSER     USER-ID                                      
         MVC   PBRPJOB,ESJOB       JOB                                          
         MVC   PBRPCRED,ASCDAT     CREATED DATE                                 
         MVC   PBRPFORM,CSFORMAT   GET BILLING FORMAT                           
         MVC   PBRPPERS,CUPASS     PERSON CODE                                  
         MVC   PBRKDA,IODA         DISK ADDRESS                                 
         GOTO1 AIO,IO1+IOACCDIR+IOADDREC ADD RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
         XC    ESPVALS(ESPLNQ),ESPVALS     INIT CURRENT PARA VALUES             
         MVI   ESPOFST,0                                                        
         MVI   ESPARA,0                                                         
*                                                                               
GBIL140  LA    R2,EDTJOBH          R2=A(JOB FIELD HEADER)                       
         USING FHD,R2                                                           
         MVC   EDTJOB,ESJOB        DISPLAY JOB CODE                             
         OI    FHOI,FHOITR                                                      
         LA    R2,EDTBILNH                                                      
         MVC   EDTBILN,CSBILNUM    DISPLAY BILL NUMBER                          
         MVI   ESCOL#1,1           SET FIRST DISPLAY COLUMN TO START            
         MVI   ESSTLIN,1           SET FIRST DISPLAY LINE TO START              
         L     R2,ESATXT1          ?? POINTLISS DOES NOT YET EXIST              
         S     R2,ATWA             R2=(DISP TO TEXT LINE FIELD HEADER)          
         L     RF,AINP             RF=A(TIOB)                                   
         USING TIOBD,RF                                                         
         MVI   TIOBCURI,0          SET CURSOR INDEX TO START OF FIELD           
         STCM  R2,3,TIOBCURD       OVERRIDE DEFAULT CURSOR POSITION             
         DROP  RF                                                               
*                                                                               
         GOTO1 ASETINI                                                          
         BNE   GBILERRX                                                         
*                                                                               
GBILX    B     EXITY               CC EQUAL FOR OK EXIT                         
*                                                                               
GBILERRX B     EXITN               CC UNEQUAL FOR ERROR EXIT                    
         EJECT                                                                  
***********************************************************************         
* READ AND DISPLAY PARAGRAPH RECORD                                   *         
* NTRY- R1=A(BYTE CONTAINING PARAGRAPH NUMBER)                        *         
*       R1=(BINARY ZEROS) IF PARAGRAPH SCREEN FIELD TO BE USED        *         
***********************************************************************         
         SPACE 1                                                                
GPARA    DS    0H                                                               
         USING *,R8                                                             
         LTR   R1,R1               ADDRESS OF PARA SUPPLIED BY CALLER?          
         BZ    GPAR02                                                           
         XR    R3,R3                                                            
         ICM   R3,1,0(R1)          R3=(PARAGRAPH NUMBER TO BE READ)             
         BNZ   GPAR10                                                           
         DC    H'0'                                                             
*                                                                               
GPAR02   LA    R0,L'EDTPARA                                                     
         LA    RF,EDTPARA                                                       
         CLI   0(RF),C' '          FIND FIRST NON-SPACE CHARACTER               
         BH    GPAR04                                                           
         LA    RF,1(RF)                                                         
         BCT   R0,*-12                                                          
         B     GPAR08                                                           
GPAR04   CLI   0(RF),C' '          FIND NEXT FIRST SPACE                        
         BNH   GPAR06                                                           
         LA    RF,1(RF)                                                         
         BCT   R0,GPAR04                                                        
         B     GPAR08                                                           
GPAR06   MVI   0(RF),C' '          SET REST OF FIELD TO SPACES                  
         LA    RF,1(RF)            ( SO "1 OF 2" BECOMES "1" )                  
         BCT   R0,GPAR06                                                        
*                                                                               
GPAR08   GOTO1 AFVAL,EDTPARAH                                                   
         BE    *+12                                                             
         LA    R3,1                DEFAULT TO PARAGRAPH 1 IF NO INPUT           
         B     GPAR10                                                           
*                                                                               
         TM    FVIIND,FVINUM       TEST FIELD NUMERIC                           
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$NONIF)                                           
         B     GPARERRX                                                         
         L     R3,BCFULL           BCFULL=NUMBER ENTERED                        
*                                                                               
GPAR10   MVC   EDTPARA,SPACES                                                   
         CURED (R3),(L'EDTPARA,EDTPARA),0,DMCB=BCDMCB,ALIGN=LEFT                
         OI    EDTPARAH+FHOID,FHOITR                                            
*                                                                               
         CLM   R3,1,ESPACTV        PARAGRAPH NOT ACTIVE?                        
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$PNFND) PARAGRAPH NOT FOUND                       
         B     EXITN                                                            
         STC   R3,ESPOFST          SAVE PARAGRAPH OFFEST                        
*                                                                               
         LA    RE,ESPLST-1(R3)     RE=A(PARAGRAPH INDEX)                        
K        USING PBRRECD,IOKEY                                                    
         XC    K.PBRKEY,K.PBRKEY                                                
         MVI   K.PBRKTYP,PBRKTYPQ  RECORD TYPE                                  
         MVC   K.PBRKCPY,CUABIN      COMPANY CODE                               
         MVI   K.PBRKSUB,PBRKACTQ    ACTIVE RECORD                              
         MVC   K.PBRKJOB,ESJOB       JOB                                        
         MVC   K.PBRKSEQ,ESSEQ       SEQUENCE# (COMPLEMENT)                     
         MVC   K.PBRKPARA,0(RE)      PARAGRAPH#                                 
         GOTO1 AIO,IO1+IOACCDIR+IOREAD                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$PNFND) PARAGRAPH NOT FOUND                       
         B     EXITN                                                            
*                                                                               
         GOTO1 AIO,IO1+IOACCMST+IOGET                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO1             RF=A(PRODUCTION PARA RECORD)                 
         OI    EDTPARAH+FHIID,FHIIVA                                            
         MVC   ESPARA,K.PBRKPARA   SAVE PARAGRAPH NUMBER                        
         DROP  K                                                                
*                                                                               
         L     R3,AIO1                                                          
         LA    R3,PBRRFST-PBRRECD(R3)                                           
         USING PGHELD,R3                                                        
GPAR12   CLI   PGHEL,EOR           END OF RECORD?                               
         BE    GPAR30                                                           
         CLI   PGHEL,PGHELQ        PARAGRAPH HEADER ELEMENT?                    
         BE    GPAR16                                                           
         CLI   PGHEL,NDXELQ        INDEX ELEMENT?                               
         BE    GPAR18                                                           
         CLI   PGHEL,FFTELQ        FREE FORM TEXT ELEMENT?                      
         BE    GPAR20                                                           
*                                                                               
GPAR14   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,PGHLN                                                         
         AR    R3,R0                                                            
         B     GPAR12                                                           
*                                  PARAGRAPH HEADER ELEMENT                     
GPAR16   DS    0H                                                               
*                                                                               
         CLI   PGHHTYP,0                                                        
         BNE   *+8                                                              
         MVI   PGHHTYP,1                                                        
         GOTO1 ATYPSET,BOPARM,PGHHTYP                                           
         ZAP   ESPARNET,PGHNET     SET PARAGRAPH NET/COMMISSION                 
         ZAP   ESPARCOM,PGHCOM                                                  
         OC    PGHOALLC,PGHOALLC                                                
         BNZ   *+10                                                             
         ZAP   PGHOALLC,BCPZERO                                                 
         ZAP   ESPAROVR,PGHOALLC   ??                                           
         MVC   ESPARTAX,PGHTAX     ??                                           
         MVC   ESPARTY2,PGHHTYP2   ??                                           
         B     GPAR14                                                           
*                                                                               
         USING NDXELD,R3           INDEX ELEMENT                                
GPAR18   MVC   ESLHIGH,NDXHIGH     HIGHEST LINE NUMBER                          
         MVC   ESLACTV,NDXACTV     HIGHEST ACTIVE LINE NUMBER                   
         SR    RF,RF                                                            
         IC    RF,NDXLN            RF=L'(INDEX ELEMENT)                         
         SH    RF,=Y(NDXINDX+1-NDXELD)                                          
         EX    RF,*+4              RF=(X LENGTH OF INDEX LIST)                  
         MVC   ESLLST(0),NDXINDX   SAVE LINE INDEX LIST                         
         B     GPAR14                                                           
*                                                                               
         USING FFTELD,R3                                                        
GPAR20   CLI   FFTTYPE,FFTTPGHC    PARAGRAPH HEADER COMMENT?                    
         BNE   GPAR14                                                           
         MVC   EDTDESC,SPACES      DISPLAY HEADER COMMENT                       
         XR    RF,RF                                                            
         ICM   RF,1,FFTDLEN        RF=L'(HEADER COMMENT)                        
         BZ    GPAR14                                                           
         CH    RF,=Y(L'EDTDESC)    TRUNCATE DESCRIPTION IF TOO LONG             
         BNH   *+8                                                              
         LA    RF,L'EDTDESC                                                     
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   EDTDESC(0),FFTDATA  DISPLAY HEADER COMMENT                       
         OI    EDTDESCH+FHOID,FHOITR                                            
         OI    EDTDESCH+FHIID,FHIIVA                                            
         B     GPAR14                                                           
         DROP  R3                                                               
*                                                                               
GPAR30   DS    0H                                                               
         B     EXITY               CC EQUAL FOR OK EXIT                         
*                                                                               
GPARERRX LA    RF,EDTPARAH         CC UNEQUAL FOR ERROR EXIT                    
         ST    RF,FVADDR                                                        
         B     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET INITIAL VALUES                                       *         
*                                                                     *         
* NTRY: IO1 = BILL RECORD                                             *         
***********************************************************************         
         SPACE 1                                                                
SETINI   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         LA    R2,IOKEY                                                         
         USING PBCRECD,R2          R2=A(CONTROL RECORD)                         
         XC    PBCKEY,PBCKEY                                                    
         MVI   PBCKTYP,PBCKTYPQ                                                 
         MVC   PBCKCPY,CUABIN                                                   
         MVI   PBCKSUB,PBCKCONQ                                                 
         MVC   PBCKFMT,CSFORMAT    FORMAT NUMBER                                
         MVC   PBCKLANG,CSFMLANG   FORMAT LANGUAGE                              
         GOTO1 AIO,IOACCDIR+IOREAD+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
         GOTO1 AIO,IOACCMST+IOGET+IO2                                           
         GOTO1 AFMTBLK,BOPARM,('FBGET',AIO3),AIO2                               
         L     R2,AIO3                                                          
         USING FBLKD,R2                                                         
         USING BOFELD,FBBOFEL                                                   
         MVC   ESREPWD,BOFMAXWD    MAX LINE WIDTH                               
         DROP  R2                                                               
*                                                                               
         XC    ESTXTLAY,ESTXTLAY   SET UP ESTXTLAY                              
L        USING LAYOUTD,ESTXTLAY                                                 
         MVI   L.LAYCOLF,1                                                      
         MVC   L.LAYCOLN,ESREPWD                                                
         DROP  L                                                                
*                                                                               
         LA    R2,IOKEY                                                         
         USING PBSRECD,R2                                                       
         XC    PBSKEY,PBSKEY                                                    
         MVI   PBSKTYP,PBSKTYPQ                                                 
         MVC   PBSKCPY,CUABIN                                                   
         MVI   PBSKSUB,PBSKDEFQ                                                 
         MVC   PBSKFMT,CSFORMAT    FORMAT NUMBER                                
         XC    BOWORK1,BOWORK1     BOWORK1 = LIST OF VALID SECTIONS             
         LA    R3,BOWORK1                                                       
         LA    R1,IOACCDIR+IOHIGH                                               
         B     *+8                                                              
SINI02   LA    R1,IOACCDIR+IOSEQ                                                
         GOTO1 AIO                                                              
         BNE   SINI04                                                           
         CLC   PBSKEY(PBSKSEC-PBSKEY),IOKEYSAV                                  
         BNE   SINI04                                                           
         CLC   PBSKLANG,CSFMLANG                                                
         BNE   SINI02                                                           
         MVC   0(L'PBSKSEC,R3),PBSKSEC                                          
         LA    R3,L'PBSKSEC(R3)                                                 
         B     SINI02                                                           
         DROP  R2                                                               
*                                                                               
SINI04   ZAP   ESALLNET,BCPZERO    SET ALLOCATED NET/COMMISSION                 
         ZAP   ESALLCOM,BCPZERO                                                 
         MVC   IODAOVER,BCJOBDA                                                 
         GOTO1 AIO,IOACCMST+IOGET+IO2                                           
         L     R3,AIO2                                                          
         LA    R3,ACTRFST-ACTRECD(R3)                                           
         USING SCIELD,R3                                                        
         XR    RF,RF                                                            
SINI08   CLI   SCIEL,0                                                          
         BE    SINI10                                                           
         CLI   SCIEL,SCIELQ                                                     
         BNE   *+12                                                             
         CLI   SCITYPE,SCITCBAP                                                 
         BE    *+12                                                             
         IC    RF,SCILN                                                         
         BXH   R3,RF,SINI08                                                     
         ZAP   ESALLNET,SCIAMNT                                                 
         ZAP   ESALLCOM,SCIADMN                                                 
         DROP  R3                                                               
*                                                                               
SINI10   ZAP   ESBILNET,BCPZERO                                                 
         ZAP   ESBILCOM,BCPZERO                                                 
*                                                                               
         L     R4,AIO1                                                          
         PUSH  USING                                                            
         USING PBRRECD,IOKEY                                                    
         MVC   PBRKEY,0(R4)                                                     
         MVI   PBRKPARA,1                                                       
         LA    R4,PBRRFST-PBRRECD(R4)                                           
         USING SCIELD,R4           R4=A(CASH DISCOUNT SCIEL)                    
         XR    RF,RF                                                            
SINI12   CLI   SCIEL,0                                                          
         BE    SINI20                                                           
         CLI   SCIEL,SCIELQ                                                     
         BNE   *+12                                                             
         CLI   SCITYPE,SCITCBDC                                                 
         BE    SINI14                                                           
         IC    RF,SCILN                                                         
         BXH   R4,RF,SINI12                                                     
SINI14   AP    ESALLNET,SCIAMNT    ADD CASH DISCOUNT TO ALLOC NET               
*                                                                               
SINI20   L     R4,AIO1                                                          
         LA    R4,PBRRFST-PBRRECD(R4)                                           
         USING NDXELD,R4           R4=A(INDEX ELEMENT)                          
         XR    RF,RF                                                            
         CLI   NDXEL,NDXELQ                                                     
         BE    *+12                                                             
         IC    RF,NDXLN                                                         
         BXH   R4,RF,*-12                                                       
         CLI   NDXACTV,0                                                        
         BE    EXIT                                                             
*                                                                               
         LA    R1,IOHIGH+IOACCDIR+IO2                                           
         B     *+8                                                              
SINI22   LA    R1,IOSEQ+IOACCDIR+IO2                                            
         GOTO1 AIO                                                              
         BNE   SINI30                                                           
         CLC   PBRKEY(PBRKPARA-PBRKEY),IOKEYSAV                                 
         BNE   SINI30                                                           
         CLI   PBRKLINE,0                                                       
         BNE   SINI22                                                           
         XR    R0,R0               TEST PARAGRAPH IS ACTIVE                     
         IC    R0,NDXACTV                                                       
         LA    RF,NDXINDX                                                       
         CLC   PBRKPARA,0(RF)                                                   
         BE    SINI24                                                           
         LA    RF,1(RF)                                                         
         BCT   R0,*-14                                                          
         B     SINI22                                                           
*                                                                               
SINI24   GOTO1 AIO,IOGET+IOACCMST+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO2                                                          
         LA    R1,PBRRFST-PBRRECD(R1)                                           
         USING PGHELD,R1                                                        
         XR    RF,RF                                                            
         CLI   PGHEL,PGHELQ                                                     
         BE    *+12                                                             
         IC    RF,PGHLN                                                         
         BXH   R1,RF,*-12                                                       
         AP    ESBILNET,PGHNET     UPDATE AMOUNTS                               
         AP    ESBILCOM,PGHCOM                                                  
         LA    R3,BOWORK1                                                       
SINI26   CLC   PGHHTYP,0(R3)       CHECK PARAGRAPH TYPE IS VALID                
         BE    SINI28                                                           
         CLI   0(R3),0                                                          
         BE    *+12                                                             
         LA    R3,L'PGHHTYP(R3)                                                 
         B     SINI26                                                           
         OI    BCINDS1,BCIXERR                                                  
         MVC   FVMSGNO,=AL2(AE$IBLFM)                                           
         B     EXITN                                                            
         DROP  R1                                                               
SINI28   DS    0H                                                               
         B     SINI22                                                           
         DROP  R4                                                               
*                                                                               
SINI30   GOTO1 ADISAMT                                                          
         POP   USING                                                            
*                                                                               
SETINIX  B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE PARAGRAPH TYPE                                  *         
*                                                                     *         
* NTRY: EDTTYPE = FIELD TO BE VALIDATED                               *         
* EXIT: TYPSET CALLED TO SET SECTION TYPE INFO                        *         
***********************************************************************         
         SPACE 1                                                                
TYPVAL   DS    0H                                                               
         USING *,R8                                                             
         CLI   ESSECT,0                                                         
         BNE   *+8                                                              
         NI    EDTTYPEH+FHIID,FF-FHIIVA                                         
*                                  TEST FIELD ALREADY VALIDATED                 
         TM    EDTTYPEH+FHIID,FHIIVA                                            
         BO    EXITY                                                            
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,EDTTYPEH                                                   
         BE    TVAL02                                                           
         CLI   ESSECT,0                                                         
         BE    EXITN                                                            
         GOTO1 ATYPDIS             RE-DISPLAY CURRENT TYPE                      
         B     EXITY                                                            
*                                                                               
TVAL02   CP    ESPARNET,BCPZERO    TEST PARAGRAPH IS ZERO                       
         BNE   *+14                                                             
         CP    ESPARCOM,BCPZERO                                                 
         BE    TVAL10                                                           
         CLI   ESSECT,0                                                         
         BE    TVAL10                                                           
         GOTO1 ATYPDIS             RE-DISPLAY CURRENT TYPE                      
         MVC   FVMSGNO,=AL2(AE$TYPNZ)                                           
         B     EXITN                                                            
*                                                                               
K        USING PBSRECD,IOKEY                                                    
TVAL10   XC    K.PBSKEY,K.PBSKEY   SECTION DEFINITION KEY                       
         MVI   K.PBSKTYP,PBSKTYPQ                                               
         MVC   K.PBSKCPY,CUABIN                                                 
         MVI   K.PBSKSUB,PBSKDEFQ                                               
         MVC   K.PBSKFMT,CSFORMAT                                               
         MVI   BOBYTE1,0           BOBYTE1 = TYPE MATCHED                       
*                                  READ THRU ACCDIR RECORDS                     
         LA    R1,IOHIGH+IOACCDIR+IO3                                           
         B     *+8                                                              
TVAL12   LA    R1,IOSEQ+IOACCDIR+IO3                                            
         GOTO1 AIO                                                              
         BNE   TVAL20                                                           
         CLC   K.PBSKEY(PBSKSEC-PBSKEY),IOKEYSAV                                
         BNE   TVAL20                                                           
         CLC   K.PBSKLANG,CSFMLANG                                              
         BNE   TVAL12                                                           
*                                                                               
         GOTO1 AIO,IOGET+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO3                                                          
         LA    R3,PBSRFST-PBSRECD(R3)                                           
         USING FFTELD,R3           FIND SECTION NAME                            
         XR    RF,RF                                                            
TVAL14   CLI   FFTEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    RF,FFTLN                                                         
         CLI   FFTEL,FFTELQ                                                     
         BNE   *+12                                                             
         CLI   FFTTYPE,FFTTBSNM                                                 
         BE    *+8                                                              
         BXH   R3,RF,TVAL14                                                     
         MVC   BOWORK1,BCSPACES                                                 
         SH    RF,=Y(FFTDATA-FFTELD+1)                                          
         EX    RF,*+4                                                           
         MVC   BOWORK1(0),FFTDATA                                               
         MVC   BOWORK2,BOWORK1                                                  
         L     R1,ATRUP            TRANSLATE TO UPPER CASE                      
         TR    BOWORK1,0(R1)                                                    
*                                                                               
         IC    RE,FVXLEN           COMPARE NAME TO INPUT                        
         EX    RE,*+8                                                           
         BNE   TVAL16                                                           
         CLC   BOWORK1(0),FVIFLD                                                
         MVC   BOBYTE1,K.PBSKSEC                                                
         B     TVAL30                                                           
*                                                                               
TVAL16   CLI   FVILEN,3            FIND MATCH FOR 1ST 3 CHARS                   
         BL    TVAL18                                                           
         CLI   BOBYTE1,FF          TEST ALREADY TOO MANY FOUND                  
         BE    TVAL18                                                           
         CLC   BOWORK1(3),FVIFLD                                                
         BNE   TVAL18                                                           
         CLI   BOBYTE1,0                                                        
         BE    *+12                                                             
         MVI   BOBYTE1,FF                                                       
         B     TVAL18                                                           
         MVC   BOBYTE1,K.PBSKSEC                                                
         MVC   BOELEM(L'BOWORK2),BOWORK2                                        
         DROP  R3,K                                                             
*                                                                               
TVAL18   DS    0H                                                               
         B     TVAL12                                                           
*                                                                               
TVAL20   CLI   BOBYTE1,0                                                        
         BE    TYPVALN                                                          
         CLI   BOBYTE1,FF                                                       
         BE    TYPVALN                                                          
         MVC   BOWORK2,BOELEM 3 LETTER MATCH FOUND - RESTORE NAME               
*                                                                               
TVAL30   MVC   EDTTYPE,BOWORK2                                                  
         OI    EDTTYPEH+FHIID,FHIIVA                                            
         OI    EDTTYPEH+FHOID,FHOITR                                            
         GOTO1 ATYPSET,BOPARM,(1,BOBYTE1)                                       
*                                                                               
         CLI   ESLACTV,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 MAKETX,R7                                                        
         USING TXD,R7                                                           
K        USING PBRRECD,IOKEY       RE-FORMAT NUMERICAL LINES                    
         XC    K.PBRKEY,K.PBRKEY                                                
         MVI   K.PBRKTYP,PBRKTYPQ                                               
         MVC   K.PBRKCPY,CUABIN                                                 
         MVI   K.PBRKSUB,PBRKACTQ                                               
         MVC   K.PBRKJOB,BCJOBCOD                                               
         MVC   K.PBRKSEQ,ESSEQ                                                  
         MVC   K.PBRKPARA,ESPARA                                                
         MVI   K.PBRKLINE,1                                                     
         LA    R1,IOHIGH+IOACCDIR+IO3                                           
         B     *+8                                                              
TVAL32   LA    R1,IOSEQ+IOACCDIR+IO3                                            
         GOTO1 AIO                                                              
         CLC   K.PBRKEY(PBRKLINE-PBRRECD),IOKEYSAV                              
         BNE   TVAL40                                                           
         CLI   K.PBRKLSTA,PBRLTXT TEXT ONLY LINE - FINE                         
         BE    TVAL32                                                           
*                                                                               
         XR    R0,R0               TEST LINE IS ACTIVE                          
         IC    R0,ESLACTV                                                       
         LA    RF,ESLLST                                                        
TVAL34   CLC   K.PBRKLINE,0(RF)                                                 
         BE    TVAL36                                                           
         LA    RF,1(RF)                                                         
         BCT   R0,TVAL34                                                        
         B     TVAL32                                                           
*                                                                               
TVAL36   GOTO1 AIO,IOGETRUP+IOACCMST+IO3                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGETTX,BOPARM,TXD,AIO3                                           
         MVC   TXNUM,ESNUMTX       SET LAYOUT TO NUMBER LAYOUT                  
         MVC   TXLINLAY,ESNUMLAY                                                
         TM    TXLSTA,PBRLTOT+PBRLSUB TEST TOTAL LINE                           
         BZ    TVAL38                                                           
         MVC   TXNUM,ESTOTTX                                                    
         MVC   TXLINLAY,ESTOTLAY                                                
TVAL38   GOTO1 AFMTTX,BOPARM,TXD   CLEAR NUMBER FIELDS                          
         GOTO1 APUTTX,BOPARM,TXD,AIO3                                           
         GOTO1 AIO,IOPUT+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         B     TVAL32                                                           
         DROP  R7,K                                                             
*                                                                               
TVAL40   DS    0H                                                               
         B     EXITY                                                            
*                                                                               
TYPVALN  MVC   FVXTRA,FVIFLD                                                    
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET VALUES FOR PARAGRAPH TYPE                            *         
*                                                                     *         
* NTRY: P1 BYTE 0 = ZERO TO READ AND DISPLAY SECTION NAME             *         
*             1-3 = A(SECTION TYPE)                                   *         
***********************************************************************         
         SPACE 1                                                                
TYPSET   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,7,1(R1)                                                       
         CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   ESSECT,0(RF)        TEST CHANGE IN TYPE                          
         BE    EXITY                                                            
         MVC   ESSECT,0(RF)                                                     
         CLI   0(R1),0             TEST SET VALUES ONLY                         
         BNE   TSET02                                                           
         GOTO1 ATYPDIS             DISPLAY FORMAT NAME                          
*                                                                               
TSET02   DS    0H                                                               
         XC    ESCOLLST,ESCOLLST   RESET COLUMN LIST                            
         XC    ESNUMLAY,ESNUMLAY   RESET NUMERICAL LINE                         
         MVI   ESNUMTX,0                                                        
         MVC   ESNUMLAY(L'ESTXTLAY),ESTXTLAY                                    
         XC    ESTOTLAY,ESTOTLAY   RESET TOTAL LINE                             
         MVI   ESTOTTX,0                                                        
         MVC   ESTOTLAY(L'ESTXTLAY),ESTXTLAY                                    
         MVC   ESHEAD1,SPACES      RESET HEADLINE                               
         MVC   ESHEAD2,SPACES                                                   
*                                                                               
K        USING PBCRECD,IOKEY       READ THRU FORMAT RECORD(S)                   
         XC    K.PBCKEY,K.PBCKEY                                                
         MVI   K.PBCKTYP,PBCKTYPQ                                               
         MVC   K.PBCKCPY,CUABIN                                                 
         MVI   K.PBCKSUB,PBCKCONQ                                               
         MVC   K.PBCKFMT,CSFORMAT                                               
         MVC   K.PBCKLANG,CSFMLANG                                              
         LA    R1,IOHIGH+IOACCDIR+IO3                                           
         B     *+8                                                              
TSET12   LA    R1,IOSEQ+IOACCDIR+IO3                                            
         GOTO1 AIO                                                              
         BNE   TSET30                                                           
         CLC   K.PBCKEY(PBCKSEQ-PBCKEY),IOKEYSAV                                
         BNE   TSET30                                                           
         GOTO1 AIO,IOGET+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  K                                                                
*                                                                               
         L     R1,AIO3                                                          
         LA    R1,PBCRFST-PBCRECD(R1)                                           
         USING BLFELD,R1                                                        
TSET14   CLI   BLFEL,0                                                          
         BE    TSET20                                                           
         CLI   BLFEL,BLFELQ                                                     
         BNE   TSET18                                                           
         CLC   BLFSECT,ESSECT                                                   
         BNE   TSET18                                                           
*                                                                               
         GOTO1 TYPCOL,(R1)         INSERT INTO ESCOLLST                         
         GOTO1 TYPHEAD,(R1)        SET HEADLINE                                 
         GOTO1 TYPLAY,(R1)         SET LAYOUTS                                  
*                                                                               
TSET18   XR    RF,RF                                                            
         IC    RF,BLFLN                                                         
         BXH   R1,RF,TSET14                                                     
         DROP  R1                                                               
*                                                                               
TSET20   DS    0H                  READ NEXT RECORD                             
         B     TSET12                                                           
*                                                                               
TSET30   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INSERT BLFCOLN IN ESCOLLST LIST                                     *         
*                                                                     *         
* NTRY: R1 = A(BLFELD)                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING BLFELD,R1                                                        
TYPCOL   MVC   BOWORK1(L'ESCOLLST),ESCOLLST                                     
         LA    R0,L'ESCOLLST                                                    
         LA    RF,BOWORK1                                                       
TCOL02   CLI   0(RF),0             INSERT BLFCOLN IN ESCOLLST                   
         BE    TCOL04                                                           
         CLC   BLFCOLF,0(RF)                                                    
         BL    TCOL04                                                           
         LA    RF,1(RF)                                                         
         BCT   R0,TCOL02                                                        
         DC    H'0'                                                             
TCOL04   MVC   BOWORK2(L'ESCOLLST),0(RF)                                        
         MVC   0(1,RF),BLFCOLF                                                  
         MVC   1(L'ESCOLLST,RF),BOWORK2                                         
         MVC   ESCOLLST,BOWORK1                                                 
         BR    RE                                                               
         DROP  R1                                                               
         SPACE 1                                                                
***********************************************************************         
* SET HEADLINE FROM BLFELD                                            *         
*                                                                     *         
* NTRY: R1 = A(BLFELD)                                                *         
***********************************************************************         
         SPACE 1                                                                
TYPHEAD  NTR1  ,                                                                
         LR    R3,R1                                                            
         USING BLFELD,R3                                                        
         XR    R4,R4                                                            
         GOTO1 AGETNUM,BOPARM,BLFFLD TEST NUMERICAL FIELD                       
         BNE   THEAD02                                                          
         L     RF,0(R1)                                                         
         CLC   BLFFLD,NUMFLD-NUMTABD(RF)  TEST NOT A TOTAL FIELD                
         BNE   THEAD02                                                          
         LA    R4,X'80'            TELL AFMTHED TO LEFT-ALIGN HEADLINE          
*                                                                               
THEAD02  LA    RF,1(R4)                                                         
         GOTO1 AFMTHED,BOPARM,((RF),BLFELD)                                     
         BNE   THEAD04                                                          
         LM    RE,RF,0(R1)                                                      
         LA    RE,ESHEAD1(RE)                                                   
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),BOELEM                                                   
THEAD04  LA    RF,2(R4)                                                         
         GOTO1 AFMTHED,BOPARM,((RF),BLFELD)                                     
         BNE   TYPHEADX                                                         
         LM    RE,RF,0(R1)                                                      
         LA    RE,ESHEAD2(RE)                                                   
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),BOELEM                                                   
*                                                                               
TYPHEADX B     EXIT                                                             
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* SET LAYOUT FORM BLFELD                                              *         
*                                                                     *         
* NTRY: R1 = A(BLFELD)                                                *         
***********************************************************************         
         SPACE 1                                                                
TYPLAY   NTR1  ,                                                                
         LR    R3,R1                                                            
         USING BLFELD,R3                                                        
*                                                                               
         GOTO1 AGETNUM,BOPARM,BLFFLD                                            
         BNE   EXIT                TEST FIELD IS NUMERICAL                      
         L     R4,0(R1)                                                         
         USING NUMTABD,R4          R4=A(NUMTAB ENTRY)                           
*                                  INSERT INTO NUMBER LINE                      
         MVC   BOBYTE1,NUMTX       IGNORE IF AMOUNT ALREADY IN LINE             
         NC    BOBYTE1,ESNUMTX                                                  
         BNZ   TLAY02                                                           
         CLC   BLFFLD,NUMFLD2      IGNORE IF ONLY FOR TOTAL LINE                
         BE    TLAY02                                                           
         OC    ESNUMTX,NUMTX                                                    
         GOTO1 AINSLAY,BOPARM,ESNUMLAY,NUMFLD,BLFCOLF,BLFCOLN                   
*                                                                               
TLAY02   DS    0H                  INSERT INTO TOTAL LINE                       
         CLI   BLFFLD,BLFFRATQ     YES - IGNORE RATE FIELD                      
         BE    TYPLAYX                                                          
         OC    ESTOTTX,NUMTX                                                    
         GOTO1 AINSLAY,BOPARM,ESTOTLAY,NUMFLD,BLFCOLF,BLFCOLN                   
*                                                                               
TYPLAYX  B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO REFRESH FORMAT TYPE DISPLAY                              *         
***********************************************************************         
         SPACE 1                                                                
TYPDIS   DS    0H                                                               
         USING *,R8                                                             
K        USING PBSRECD,IOKEY                                                    
         XC    K.PBSKEY,K.PBSKEY   SECTION DEFINITION KEY                       
         MVI   K.PBSKTYP,PBSKTYPQ                                               
         MVC   K.PBSKCPY,CUABIN                                                 
         MVI   K.PBSKSUB,PBSKDEFQ                                               
         MVC   K.PBSKFMT,CSFORMAT                                               
         MVC   K.PBSKSEC,ESSECT                                                 
         CLI   K.PBSKSEC,0                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   K.PBSKLANG,CSFMLANG                                              
         GOTO1 AIO,IOREAD+IOACCDIR+IO3                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGET+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO3                                                          
         LA    R3,PBSRFST-PBSRECD(R3)                                           
         USING FFTELD,R3           FIND SECTION NAME                            
         XR    RF,RF                                                            
TDIS02   CLI   FFTEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    RF,FFTLN                                                         
         CLI   FFTEL,FFTELQ                                                     
         BNE   *+12                                                             
         CLI   FFTTYPE,FFTTBSNM                                                 
         BE    *+8                                                              
         BXH   R3,RF,TDIS02                                                     
         MVC   BOWORK1,BCSPACES                                                 
         SH    RF,=Y(FFTDATA-FFTELD+1)                                          
         EX    RF,*+4                                                           
         MVC   BOWORK1(0),FFTDATA                                               
         MVC   EDTTYPE,BOWORK1                                                  
         OI    EDTTYPEH+FHIID,FHIIVA                                            
         OI    EDTTYPEH+FHOID,FHOITR                                            
         DROP  R3,K                                                             
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ADD A NEW PARAGRAPH                                                 *         
***********************************************************************         
         SPACE 1                                                                
ADDPARA  DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         CLI   ESPIND,ESPIDFT      TEST DRAFT (AND NOT AUTOREV)                 
         BE    ADDPAR02                                                         
         MVC   FVMSGNO,=AL2(AE$CAPLB)                                           
         LA    RE,BASOPTH                                                       
         ST    RE,FVADDR                                                        
         B     EXITN                                                            
*                                                                               
ADDPAR02 MVI   PRMODE,PRADDQ                                                    
         GOTO1 APARAREC                                                         
         BNE   EXITN                                                            
*                                                                               
         GOTO1 ABILLREC            CHANGE BILL RECORD                           
         MVI   LRMODE,LRADDQ       ADD A BLANK LINE RECORD                      
         MVC   LRPARA,ESPARA       PARAGRAPH INDEX                              
         MVI   LRLINE,1                                                         
         GOTO1 ALINEREC,0          ADD/REACTIVATE/CHANGE LINE                   
         MVI   PRMODE,PRCHANGQ                                                  
         GOTO1 APARAREC  ??        CHANGE PARAGRAPH RECORD IF NECESSARY         
*                                                                               
         GOTO1 ADISP               DISPLAY SCREEN                               
         GOTO1 ASETMSG             SET MESSAGE                                  
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* DELETE A PARAGRAPH RECORD OR ALL LINES IN THE PARAGRAPH             *         
***********************************************************************         
         SPACE 1                                                                
DELPARA  DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         TM    ESDPAR,ESDALLQ      DELETE ALL LINES?                            
         BNO   DELPAR10                                                         
         MVI   ESSTLIN,1                                                        
         MVI   ESLACTV,0           SET NUMBER OF ACTIVE LINES TO ZERO           
         MVI   LRMODE,LRADDQ       ADD A BLANK LINE RECORD                      
         MVI   LRLINE,1            FIRST LINE                                   
         MVC   LRPARA,ESPARA       PARAGRAPH INDEX                              
         GOTO1 ALINEREC,0                                                       
         OI    LINEFLAG,LCHANGEQ   SET LINE INDEX CHANGED                       
         MVI   PRMODE,PRCHANGQ                                                  
         GOTO1 APARAREC            CHANGE PARAGRAPH RECORD                      
         BE    DELPARX                                                          
         DC    H'0'                                                             
*                                  DELETE WHOLE PARAGRAPH                       
DELPAR10 MVI   CMPMODE,CMPMOVEQ    MOVE MODE                                    
         MVC   CMPTO,ESPHIGH       MOVE TO OFFSET                               
         MVC   CMPFROM1,ESPOFST    MOVE FROM OFFSET 1 (START)                   
         MVC   CMPFROM2,ESPOFST    MOVE FROM OFFSET 2 (END)                     
         LA    RF,ESPLST           RF=A(PARAGRAPH INDEX LIST)                   
         ST    RF,CMPAILST                                                      
         MVI   CMPILLN,L'ESPLST    LENGTH OF INDEX LIST                         
         GOTO1 ACMI                MOVE PARA INDEX TO END OF LIST               
         SR    RF,RF                                                            
         IC    RF,ESPACTV          REDUCE NUMBER OF ACTIVE PARAGRAPHS           
         SH    RF,=H'1'                                                         
         BNM   *+8                                                              
         LA    RF,0                                                             
         STC   RF,ESPACTV                                                       
         SP    ESBILNET,ESPARNET   UPDATE BILL NET/COMMISSION TOTALS            
         SP    ESBILCOM,ESPARCOM                                                
         GOTO1 ADISAMT                                                          
         CLI   ESPACTV,0           LAST PARAGRAPH DELETED?                      
         BNE   DELPAR20                                                         
         MVI   PRMODE,PRADDQ       ADD A BLANK PARAGRAPH                        
         GOTO1 APARAREC                                                         
DELPAR20 GOTO1 ABILLREC                                                         
*                                                                               
         CLC   ESPOFST,ESPACTV     FIRST PARAGRAPH DELETED?                     
         BNH   *+8                                                              
         MVI   ESPOFST,1                                                        
*                                                                               
         LA    R1,ESPOFST          R1=A(PARAGRAPH# REQUIRED)                    
         GOTO1 AGPARA                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DELPARX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* EDIT SCREEN LINES                                                   *         
***********************************************************************         
         SPACE 1                                                                
EDTSCRN  DS    0H                                                               
         USING *,R8                                                             
         XC    SCVALS(SCVALSL),SCVALS                                           
         MVI   AUTOINS,0           INIT AUTOMATIC INSERT LINE                   
         XC    UNUSEINS,UNUSEINS   INIT NUMBER OF UNUSED INSERTS                
         MVC   NXTLOFF,ESSTLIN     SET NEXT LINE NUMBER TO START LINE           
         MVI   TSARLIN#,0                                                       
         XR    R0,R0                                                            
         IC    R0,ESNUMLIN         R0=(NUMBER OF TEXT LINES)                    
*                                                                               
         GOTO1 MAKETX,R7                                                        
         USING TXD,R7                                                           
EDTS10   GOTO1 ARESTX,BOPARM,TXD                                                
         GOTO1 AVALTX,BOPARM,TXD                                                
         BNE   EDTSN                                                            
         GOTO1 ACOMBLD,TXD         SEARCH FOR STANDARD COMMENTS                 
         BNE   EDTSN                                                            
*                                                                               
         L     R3,TXANUM                                                        
         USING LNUMFLDD,R3                                                      
         TM    TXINDS,TXIITH       FIELD INPUT THIS TIME?                       
         BO    EDTS20                                                           
         CLI   LNUMFLD,INSERTQ     INSERT LINE?                                 
         BNE   EDTS30                                                           
         LA    RE,UNUSEINS         RE=A(LIST OF UNUSED INSERTS)                 
         CLI   0(RE),EOT           END OF LIST?                                 
         BE    *+12                                                             
         LA    RE,1(RE)            BUMP TO NEXT UNUSED INSERT NUMBER            
         B     *-12                                                             
         MVC   0(L'NXTLOFF,RE),NXTLOFF SAVE CURRENT UNUSED INS LINE             
         B     EDTS40                                                           
*                                                                               
EDTS20   MVI   LRMODE,LRCHANGE     CHANGE LINE RECORD                           
         CLI   LNUMFLD,INSERTQ     INSERT LINE?                                 
         BNE   *+8                                                              
         MVI   LRMODE,LRADDQ       ADD/REACTIVATE LINE RECORD                   
         MVC   LRLINE,NXTLOFF      LINE NUMBER TO BE ADDED/CHANGED              
         MVC   LRPARA,ESPARA       PARAGRAPH INDEX                              
*                                                                               
         GOTO1 ALINEREC,TXD        ADD/REACTIVATE/CHANGE LINE                   
*                                                                               
EDTS30   DS    0H                                                               
         SR    RF,RF               BUMP LINE NUMBER                             
         IC    RF,NXTLOFF                                                       
         LA    RF,1(RF)                                                         
         STC   RF,NXTLOFF                                                       
         CLI   LNUMFLD,INSERTQ     INSERT LINE?                                 
         BNE   EDTS40                                                           
         BCTR  RF,0                                                             
         STC   RF,AUTOINS          SET FOR AUTO INSERT                          
         MVI   AUTOBLKI,BLKITXT                                                 
         CLI   TXNUM,0                                                          
         BE    *+8                                                              
         MVI   AUTOBLKI,BLKINUM                                                 
         MVC   LNUMFLD,SPACES      CLEAR DOTS OFF LINE NUMBER                   
         DROP  R3                                                               
*                                                                               
EDTS40   DS    0H                                                               
         BCT   R0,EDTS10                                                        
         DROP  R7                                                               
*                                                                               
EDTSY    MVI   PRMODE,PRCHANGQ                                                  
         GOTO1 APARAREC            CHANGE PARAGRAPH RECORD IF NECESSARY         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXITY                                                            
*                                                                               
EDTSN    DS    0H                                                               
         MVC   EDMSGNO,FVMSGNO     SAVE ERROR DETAILS                           
         MVC   EDXTRA,FVXTRA                                                    
         MVC   EDERRNDX,FVERRNDX                                                
         MVC   EDCURS,FVCURS                                                    
         MVC   EDADDR,FVADDR                                                    
         MVI   PRMODE,PRCHANGQ                                                  
         GOTO1 APARAREC            CHANGE PARAGRAPH RECORD IF NECESSARY         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FVMSGNO,EDMSGNO     RESTORE ERROR DETAILS                        
         MVC   FVXTRA,EDXTRA                                                    
         MVC   FVERRNDX,EDERRNDX                                                
         MVC   FVCURS,EDCURS                                                    
         MVC   FVADDR,EDADDR                                                    
         L     RF,AINP                                                          
         USING TIOBD,RF                                                         
         XC    TIOBCURD,TIOBCURD                                                
         DROP  RF                                                               
         B     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY PARAGRAPH DATA                                              *         
***********************************************************************         
         SPACE 1                                                                
DISPARA  DS    0H                                                               
         USING *,R8                                                             
         MVC   EDTPARA,SPACES                                                   
         CLI   ESPACTV,0           NO ACTIVE PARAGRAPHS?                        
         BE    DISP10                                                           
         SR    RF,RF                                                            
         IC    RF,ESPOFST          RF=(CURRENT PARAGRAPH NUMBER)                
         LA    R3,EDTPARA          R3=A(PARAGRAPH FIELD)                        
         CURED (RF),(3,0(R3)),0,DMCB=BCDMCB,ALIGN=LEFT                          
         AR    R3,R0               BUMP R3 BY LENGTH OF PARA NUMBER             
         LH    RE,=Y(LC@OF-TWAD)                                                
         LA    RE,TWAD(RE)                                                      
         MVC   1(L'LC@OF,R3),0(RE)                                              
         LA    R3,L'LC@OF(R3)                                                   
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         SR    RF,RF                                                            
         IC    RF,ESPACTV          RF=(NUMBER OF ACTIVE PARAGRAPHS)             
         CURED (RF),(3,2(R3)),0,DMCB=BCDMCB,ALIGN=LEFT                          
         OI    EDTPARAH+FHIID,FHIIVA                                            
DISP10   OI    EDTPARAH+FHOID,FHOITR                                            
*                                                                               
         MVC   EDTLTXT,SPACES                                                   
         LH    RE,=AL2(LC@LINE-TWAD) LINE                                       
         LA    RE,TWAD(RE)                                                      
         MVC   EDTLTXT(L'LC@LINE),0(RE)                                         
         CLC   ESSTLIN,ESLACTV     START LINE IS LAST LINE?                     
         BNL   DISP20                                                           
         LH    RE,=AL2(LC@LINES-TWAD) LINES                                     
         LA    RE,TWAD(RE)                                                      
         MVC   EDTLTXT(L'LC@LINES),0(RE)                                        
DISP20   OI    EDTLTXTH+FHOID,FHOITR                                            
*                                                                               
         MVC   EDTLNUM,SPACES                                                   
         CLI   ESLACTV,0           NO ACTIVE LINE RECORDS?                      
         BE    DISP40                                                           
         CLC   ESSTLIN,ESLACTV     START LINE IS LAST LINE?                     
         BNH   *+10                                                             
         MVC   ESSTLIN,ESLACTV     START LINE IS LAST LINE?                     
         SR    RF,RF                                                            
         IC    RF,ESSTLIN          RF=(TEXT START LINE NUMBER)                  
         LA    R3,EDTLNUM          R3=A(LINE NUMBER FIELD)                      
         CURED (RF),(3,0(R3)),0,DMCB=BCDMCB,ALIGN=LEFT                          
         AR    R3,R0               BUMP UP BY LENGTH OF LINE NUMBER             
         SR    R4,R4                                                            
         IC    R4,ESLACTV          R4=(NUMBER OF ACTIVE LINE RECORDS)           
         CLC   ESSTLIN,ESLACTV     START LINE IS LAST LINE?                     
         BE    DISP30                                                           
         MVI   0(R3),C'-'                                                       
         SR    RF,RF                                                            
         IC    RF,ESSTLIN                                                       
         LA    RF,MAXSLINQ-1(RF)                                                
         CR    RF,R4                                                            
         BNH   *+6                                                              
         LR    RF,R4               RF=(TEXT END LINE NUMBER)                    
         LA    R3,1(R3)                                                         
         CURED (RF),(3,0(R3)),0,DMCB=BCDMCB,ALIGN=LEFT                          
         AR    R3,R0                                                            
DISP30   LH    RE,=Y(LC@OF-TWAD)                                                
         LA    RE,TWAD(RE)                                                      
         MVC   1(L'LC@OF,R3),0(RE)                                              
         LA    R3,L'LC@OF(R3)                                                   
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         CURED (R4),(3,2(R3)),0,DMCB=BCDMCB,ALIGN=LEFT ACTIVE LINES             
DISP40   OI    EDTLNUMH+FHOID,FHOITR                                            
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY HEADER LINES                                                *         
***********************************************************************         
         SPACE 1                                                                
DISHEAD  DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         CLI   ESCMND,CMNCOLQ      COLUMN COMMAND ENTERED?                      
         BNE   DISH10                                                           
         CLI   ESCPAR,0            YES/NO PARAMETER SUPPLIED?                   
         BNE   *+12                                                             
         XI    ESCLFLG,ESCLRLQ     FLIP THE BITS IF NO PARAMETER                
         B     DISH10                                                           
         TM    ESCPAR,ESCYESQ      YES PARAMETER ENTERD?                        
         BO    *+12                                                             
         NI    ESCLFLG,X'FF'-ESCLRLQ                                            
         B     DISH10                                                           
         OI    ESCLFLG,ESCLRLQ                                                  
*                                                                               
DISH10   GOTO1 ABLDTOP             ??                                           
         L     R2,ESAHEAD1                                                      
         USING TOPBANH,R2                                                       
         XR    RF,RF                                                            
         IC    RF,ESCOL#1                                                       
         LA    RF,ESHEAD1-1(RF)                                                 
         IC    RE,ES#COL                                                        
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   TOPBAN(0),0(RF)                                                  
*                                                                               
         L     R2,ESATOP                                                        
         USING TOPD,R2             R2=A(TOP PARAGRAPH HEADLINE)                 
         MVC   TOPBAN,SPACES       COPY HEADER INTO BANNER                      
         XR    RF,RF                                                            
         IC    RF,ESCOL#1                                                       
         LA    RF,ESHEAD2-1(RF)                                                 
         IC    RE,ES#COL                                                        
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   TOPBAN(0),0(RF)                                                  
*                                                                               
DISHX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY SCREEN LINES                                                *         
***********************************************************************         
         SPACE 1                                                                
DISSCRN  DS    0H                                                               
         USING *,R8                                                             
         OI    BASSCRH+FHOID,FHOITR XMIT FLD SO TIOB UPDATED                    
         MVC   NXTLOFF,ESSTLIN     SET NEXT LINE NUMBER TO START LINE           
         MVI   TSARLIN#,0                                                       
         LA    R0,MAXSLINQ         R0=(NUMBER OF TEXT LINES)                    
         TM    ESCLFLG,ESCLRLQ                                                  
         BZ    *+6                                                              
         BCTR  R0,0                LESS 1 IF RULER DISPLAYED                    
         LA    R4,ESEINSB          R4=A(LIST OF INSERT BLOCK VALUES)            
         USING BLKVALD,R4                                                       
         NI    EWINDS,FF-EWIDINS                                                
         NI    ESINDS,FF-ESIDEND                                                
         GOTO1 MAKETX,R7                                                        
         USING TXD,R7                                                           
*                                                                               
DISS10   OC    BLKSTRT(BLKLNQ),BLKSTRT ANY OUTSTANDING INSERT?                  
         BZ    DISS40                                                           
         SR    R3,R3               INSERT LINES REQUESTED HERE?                 
         IC    R3,BLKSTRT                                                       
         LA    R3,1(R3)            R3=(LINE NUMBER FOR INSERT START)            
         CLM   R3,1,NXTLOFF        MATCH ON CURRENT LINE NUMBER?                
         BNE   DISS40                                                           
*                                                                               
         SR    R6,R6                                                            
         IC    R6,BLKLEN           R6=(NUMBER OF LINES TO BE INSERTED)          
         CR    R0,R6               NUM TO BE INSERTED > LEFT ON SCREEN?         
         BNL   *+6                                                              
         LR    R6,R0               SET TO NUM OF LINES LEFT ON SCREEN           
         STC   R6,ALTBDVAL         NUMBER OF INSERT                             
         STC   R3,ALTBSTRT         START LINE                                   
         MVI   ALTBEND,X'FF'       END LINE                                     
         MVC   ALTBCTYP,=AL2(LENITXQ) COMMAND TYPE                              
         MVI   ALTBSTAT,0                                                       
         GOTO1 AALTBLKV            ALTER ANY BLOCK COMMANDS AFFECTED            
         MVC   BINDS,BLKINDS                                                    
         LA    R4,BLKLNQ(R4)       BUMP TO NEXT INSERT BLOCK ENTRY              
*                                                                               
DISS30   DS    0H                                                               
         XR    RF,RF                                                            
         TM    BINDS,BLKINUM       TEST INSERTING TEXT/NUMERICAL LINE           
         BZ    *+8                                                              
         LA    RF,PBRLNUM                                                       
         GOTO1 AINITX,BOPARM,((RF),TXD)                                         
         OI    TXINDS,TXIINS                                                    
         GOTO1 ABLDTX,BOPARM,TXD                                                
         TM    EWINDS,EWIDINS                                                   
         BO    DISS32                                                           
         OI    EWINDS,EWIDINS                                                   
         L     RF,AINP             RF=A(TIOB)                                   
         USING TIOBD,RF                                                         
         L     RE,TXATXT                                                        
         S     RE,ATWA             RE=(DISPLACEMENT TO FIELD HEADER)            
         STCM  RE,3,TIOBCURD       OVERRIDE DEFAULT CURSOR CONTROL              
         MVI   TIOBCURI,0          SET CURSOR AT START OF FIELD                 
         DROP  RF                                                               
*                                                                               
DISS32   GOTO1 ASAVTX,BOPARM,TXD                                                
         BCT   R0,*+8                                                           
         B     DISSCRNX            SCREEN FULL?                                 
         BCT   R6,DISS30                                                        
*                                                                               
DISS40   CLC   NXTLOFF,ESLACTV     ALL TEXT LINES DISPLAYED?                    
         BNH   DISS50                                                           
         CLC   SLAD,=Y((ROWS#Q-1)*COLS#Q)                                       
         BE    DISSCRNX            TEST ALREADY REACHED LAST LINE               
         GOTO1 ABLDEND             BUILD END-OF-PARAGRAPH                       
         OI    ESINDS,ESIDEND                                                   
         B     DISSCRNX                                                         
*                                                                               
DISS50   DS    0H                                                               
         LA    RF,IOKEY            RF=A(KEY FOR PRODUCTION BILL RECORD)         
         USING PBRRECD,RF                                                       
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ    RECORD TYPE                                  
         MVC   PBRKCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRKSUB,PBRKACTQ    ACTIVE RECORD                                
         MVC   PBRKJOB,ESJOB       JOB                                          
         MVC   PBRKSEQ,ESSEQ       SEQUENCE# (COMPLEMENT)                       
         MVC   PBRKPARA,ESPARA     PARAGRAPH#                                   
         SR    RE,RE                                                            
         IC    RE,NXTLOFF          OFFSET TO LINE INDEX                         
         BCTR  RE,0                                                             
         LA    RE,ESLLST(RE)       RE=A(CORRESPONDING INDEX FOR LINE)           
         MVC   PBRKLINE,0(RE)      LINE#                                        
         GOTO1 AIO,IO1+IOACCMST+IOREAD GET LINE NUMBER RECORD                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGETTX,BOPARM,TXD,AIO1                                           
         GOTO1 ABLDTX,BOPARM,TXD                                                
         GOTO1 ADISTX,BOPARM,TXD                                                
         GOTO1 ASAVTX,BOPARM,TXD                                                
*                                                                               
         SR    RF,RF               BUMP LINE NUMBER                             
         IC    RF,NXTLOFF                                                       
         LA    RF,1(RF)                                                         
         STC   RF,NXTLOFF                                                       
         BCT   R0,DISS10                                                        
         DROP  R7                                                               
         DROP  R4                                                               
*                                                                               
DISSCRNX MVC   FVADDR,ESATXT1      FVADDR=A(SCREEN TEXT LINE FIELD)             
         XC    ESEINSB,ESEINSB     CLEAR OUT INSERTS                            
         MVC   ESNUMLIN,TSARLIN#   SET NO. OF LINES ON SCREEN                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SET MESSAGE                                                         *         
***********************************************************************         
         SPACE 1                                                                
SETMSG   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         TM    ESFCFLG,ESFCTLQ     LINE TOO LONG FOR AMEND?                     
         BNO   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$LLONG)                                           
         B     SETMSGX                                                          
*                                                                               
         MVI   FVOMTYP,GTMINF                                                   
         CLI   BCPFKEY,PFKFINDQ    FIND PFKEY?                                  
         BE    SMSG02                                                           
         CLI   BCPFKEY,PFKCHGQ     CHANGE PFKEY?                                
         BE    SMSG02                                                           
         CLI   ESCMND,CMNFNDQ      FIND COMMAND?                                
         BE    SMSG02                                                           
         CLI   ESCMND,CMNCHGQ      CHANGE COMMAND                               
         BNE   SMSG12                                                           
SMSG02   TM    ESFCFLG,ESFCMTQ     MATCH FOUND?                                 
         BNO   SMSG06                                                           
         TM    ESFCPAR,ESFCALQ     ALL PARAMETER USED?                          
         BO    SMSG04                                                           
         MVC   FVMSGNO,=AL2(AI$CHFD) CHARS FOUND                                
         TM    ESFCFLG,ESFCFNQ                                                  
         BO    SMSG08                                                           
         MVC   FVMSGNO,=AL2(AI$CHCH) CHARS CHANGED                              
         B     SMSG08                                                           
SMSG04   MVC   FVMSGNO,=AL2(AI$ACHFD) N CHARS FOUND                             
         TM    ESFCFLG,ESFCFNQ     FIND COMMAND?                                
         BO    *+10                                                             
         MVC   FVMSGNO,=AL2(AI$ACHCH) N CHARS CHANGED                           
         XC    FVPARMS,FVPARMS                                                  
         LA    R2,FVPARMS          R2=A(MSG PARAMETERS)                         
         SR    RF,RF                                                            
         ICM   RF,3,ESFCCNT        NUMBER OF MATCHES                            
         CURED (RF),(5,1(R2)),0,DMCB=BCDMCB,ALIGN=LEFT                          
         AH    R0,=H'1'                                                         
         STC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         NI    ESFCPAR,FF-ESFCALQ SWITCH OF ALL PARAMETER                       
         B     SMSG10                                                           
*                                  MATCH NOT FOUND                              
SMSG06   L     RF,AINP             RF=A(TIOB)                                   
         USING TIOBD,RF                                                         
         MVC   TIOBCURD,ESCURD     OVERRIDE DEFAULT CURSOR CONTROL              
         L     RE,ESACOM1                                                       
         S     RE,ATWA                                                          
         CH    RE,TIOBCURD         CURSOR ON COMMAND LINE?                      
         BNE   *+8                                                              
         MVI   TIOBCURI,0          SET TO BEGINING OF FIELD                     
         DROP  RF                                                               
         TM    ESFCFLG,ESFCBTQ                                                  
         BNO   *+14                                                             
         MVC   FVMSGNO,=AL2(AI$BTRCH) BOTTOM OF TEXT REACHED                    
         B     SETMSGX                                                          
         TM    ESFCFLG,ESFCTPQ                                                  
         BNO   *+14                                                             
         MVC   FVMSGNO,=AL2(AI$TPRCH) TOP OF TEXT REACHED                       
         B     SETMSGX                                                          
         MVC   FVMSGNO,=AL2(AI$NCHFD) NO CHARS FOUND                            
SMSG08   XC    FVPARMS,FVPARMS                                                  
         LA    R2,FVPARMS          R2=A(MESSAGE PARAMETERS)                     
SMSG10   SR    RF,RF                                                            
         IC    RF,ESFDLN           RF=L'(FIND STRING)                           
         LA    RE,3(RF)                                                         
         STC   RE,0(R2)                                                         
         MVI   1(R2),C''''         PUT STRING IN QUOTES                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   2(0,R2),ESFDTXT                                                  
         LA    R2,3(R2,RF)                                                      
         MVI   0(R2),C''''                                                      
         B     SETMSGX                                                          
*                                                                               
SMSG12   MVC   FVMSGNO,=AL2(AI$EPARA) ENTER PARAGRAPH DETAILS                   
         TM    ESEBFLG,BLKCOUTQ                                                 
         BNO   *+14                                                             
         MVC   FVMSGNO,=AL2(AI$BLKIN) BLOCK COMMAND INCOMPLETE                  
         B     SETMSGX                                                          
         TM    ESEBFLG,BLKPENDQ                                                 
         BNO   *+10                                                             
         MVC   FVMSGNO,=AL2(AI$MCPEN) MOVE/COPY IS PENDING                      
SETMSGX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LINE COMMANDS                                               *         
***********************************************************************         
         SPACE 1                                                                
CMDDIS   DS    0H                                                               
         USING *,R8                                                             
         SR    RF,RF                                                            
         IC    RF,ESSTLIN          RF=(LINE NO AT START OF CURR SCREEN)         
         LA    RE,MAXSLINQ-1(RF)   RE=(LINE NO AT END OF CURR SCREEN)           
*        CLM   RE,1,ESLACTV        BEYOND NUMBER OF LINES IN PARAGRAPH?         
*        BNH   *+8                                                              
*        IC    RE,ESLACTV                                                       
         STC   RE,BCBYTE2          LINE NUMBER AT END OF SCREEN                 
*                                                                               
         L     R3,AEBLTAB          R3=A(EDIT BLOCK LIST TABLE)                  
         USING EBLTABD,R3                                                       
CDIS10   CLC   EBLCTYPE,=AL2(EOT)  END OF TABLE?                                
         BE    CDISX                                                            
         CLC   EBLCTYPE,=AL2(LENITXQ+LENINMQ) DONT RE-DISPLAY INSERTS           
         BE    CDIS80                                                           
         MVC   EHALF,EBLCTYPE                                                   
         NC    EHALF,=AL2(LENMOVEQ+LENCOPYQ) MOVE/COPY CMD?                     
         BZ    CDIS20                                                           
         CLC   ESPARA,ESECMPA      SAME PARA AS DISPLAYED?                      
         BNE   CDIS80                                                           
         B     CDIS30                                                           
CDIS20   MVC   EHALF,EBLCTYPE                                                   
         NC    EHALF,=AL2(LENAFTQ+LENBEFQ+LENOVERQ) AFTER/BEF/OVER CMD?         
         BZ    CDIS30                                                           
         CLC   ESPARA,ESETOPA      SAME PARA AS DISPLAYED?                      
         BNE   CDIS80                                                           
CDIS30   SR    R2,R2                                                            
         IC    R2,EBLNM            R2=(MAX NUMBER OF ENTRIES IN LIST)           
         SR    R1,R1                                                            
         ICM   R1,3,EBLDIS                                                      
         LA    R1,ESAVE(R1)        R1=A(BLOCK VALUES LIST)                      
         USING BLKVALD,R1                                                       
         ICM   RE,15,ALENTRY       RE=A(LINE EDIT COMMAND ENTRIES)              
         USING LEDTABD,RE                                                       
CDIS40   CLC   LENCTYPE,=AL2(EOT)  END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   LENCTYPE,EBLCTYPE   MATCH ON COMMAND TYPE                        
         BE    *+12                                                             
         LA    RE,LENLNQ(RE)       BUMP TO NEXT ENTRY                           
         B     CDIS40                                                           
         ST    RE,BCFULL           SAVE ENTRY FOR CURRENT BLOCK TYPE            
         DROP  RE                                                               
CDIS50   OC    BLKSTRT(BLKLNQ),BLKSTRT NO MORE VALUES?                          
         BZ    CDIS80                                                           
         CLC   BLKSTRT,ESSTLIN     START CMND BEFORE START OF SCREEN?           
         BL    CDIS60                                                           
         CLC   BLKSTRT,BCBYTE2                                                  
         BH    CDIS60              START CMND AFTER END OF SCREEN?              
         MVC   BCBYTE1,BLKSTRT                                                  
         BAS   RE,CMDFMT           FORMAT START COMMAND ON SCREEN               
CDIS60   CLI   BLKLEN,0            END CMND NOT NEEDED FOR NUMERIC CMND         
         BNE   CDIS70                                                           
         CLC   BLKEND,ESSTLIN      END CMND BEFORE START OF SCREEN?             
         BL    CDIS70                                                           
         CLC   BLKEND,BCBYTE2      END CMND AFTER END OF SCREEN?                
         BH    CDIS70                                                           
         MVC   BCBYTE1,BLKEND                                                   
         BAS   RE,CMDFMT           FORMAT END COMMAND ON SCREEN                 
CDIS70   LA    R1,BLKLNQ(R1)       BUMP TO NEXT BLOCK LIST VALUE                
         BCT   R2,CDIS50                                                        
CDIS80   LA    R3,EBLLNQ(R3)       BUMPT TO NEXT EDIT TABLE ENTRY               
         B     CDIS10                                                           
CDISX    B     EXIT                                                             
         DROP  R1,R3                                                            
         SPACE 1                                                                
***********************************************************************         
* FORMAT COMMAND LINE ON SCREEN                                       *         
* ON NTRY R1=A(ENTRY IN BLOCK VALUES LIST)                            *         
***********************************************************************         
         SPACE 1                                                                
CMDFMT   NTR1  ,                                                                
         LR    R3,R1                                                            
         USING BLKVALD,R3                                                       
         GOTO1 MAKETX,R7                                                        
         USING TXD,R7                                                           
         SR    R2,R2                                                            
         IC    R2,BCBYTE1          R2=(LINE NUMBER OF COMAND)                   
         SR    RF,RF                                                            
         IC    RF,ESSTLIN          RF=(LINE NUMBER OF SCREEN START)             
         SR    R2,RF                                                            
         STC   R2,TSARLIN#         ??                                           
         GOTO1 ARESTX,BOPARM,TXD                                                
         ICM   R2,15,TXACOM                                                     
         USING COMFLDD,R2          R2=A(LINE CMND FIELD HEADER)                 
         MVI   COMFLDH+FHILD,2     SET LENGTH                                   
         SR    RF,RF                                                            
         ICM   RF,15,BCFULL        RF=A(COMMAND ENTRY)                          
         MVC   COMFLD,LENCOMM-LEDTABD(RF)                                       
         CLI   BLKLEN,1            NUMERIC COMMAND HIGHER THAN 1 LINE?          
         BH    CFMT10                                                           
         CLI   BLKLEN,0            BLOCK COMMAND?                               
         BE    CFMTCX                                                           
         MVI   COMFLD+1,C' '       GET RID OF SECOND CHAR                       
         MVI   COMFLDH+FHILD,1                                                  
         B     CFMTCX                                                           
CFMT10   SR    RF,RF               DISPLAY NUMERIC PART OF COMMAND              
         IC    RF,BLKLEN                                                        
         CURED (RF),(2,COMFLD+1),0,DMCB=BCDMCB,ALIGN=LEFT                       
         AH    R0,=H'1'                                                         
         STC   R0,COMFLDH+FHILD                                                 
CFMTCX   B     EXIT                                                             
         DROP  R2,R3,R7                                                         
         EJECT                                                                  
***********************************************************************         
* DEAL WITH LINE COMMANDS                                             *         
***********************************************************************         
         SPACE 1                                                                
CMDVAL   DS    0H                                                               
         USING *,R8                                                             
         CLI   ESCMND,CMNRESQ      RESET LINE COMMANDS?                         
         BNE   CVAL02                                                           
         LA    RE,ESEVAL                                                        
         LA    RF,ESELNQ                                                        
         XR    R1,R1                                                            
         MVCL  RE,R0               INIT ALL LINE EDIT VARIABLES                 
*                                                                               
CVAL02   NI    ESEBFLG,FF-BLKPENDQ SWITCH OFF COPY/MOVE IS PENDING              
         MVI   TOTINRP,0           INIT TOTAL OF INSERT AND REPEATS             
         GOTO1 AINBVAL             INIT BLOCK VALUES ON CURRENT SCREEN          
*                                                                               
         MVC   NXTLOFF,ESSTLIN     SET NEXT LINE NUMBER TO START LINE           
         MVI   TSARLIN#,0                                                       
         XR    R0,R0                                                            
         IC    R0,ESNUMLIN         R0=(NUMBER OF TEXT LINES)                    
*                                                                               
         GOTO1 MAKETX,R7                                                        
         USING TXD,R7                                                           
         MVC   FVMSGNO,=AL2(AE$INVIF) INVALID INPUT                             
CVAL10   GOTO1 ARESTX,BOPARM,TXD                                                
         L     R2,TXACOM                                                        
         USING COMFLDD,R2                                                       
         ST    R2,FVADDR                                                        
         LA    R1,L'COMFLD         R1=L'(LINE COMMAND FIELD)                    
CVAL20   LA    RF,COMFLD-1(R1)                                                  
         CLI   0(RF),INSERTQ ??    INSERT DOT?                                  
         BNE   CVAL30                                                           
         MVI   0(RF),C' '          CLEAR OUT INSERT DOT                         
         SR    RE,RE                                                            
         ICM   RE,1,COMFLDH+FHILD  REDUCE INPUT LENGTH OF FIELD                 
         BZ    CVAL30                                                           
         BCTR  RE,0                                                             
         STCM  RE,1,COMFLDH+FHILD                                               
CVAL30   BCT   R1,CVAL20           GET PREVIOUS CHARACTER                       
         CLC   COMFLD,SPACES       ANY INPUT?                                   
         BNH   CVAL100                                                          
*                                                                               
         OC    COMFLD,SPACES       PAD OUT FIELD WITH SPACES                    
         MVI   BLEN,0              BLOCK LENGTH IF NUMERIC COMMAND              
         MVI   BFR1,0              START LINE NUMBER FOR COMMAND                
         MVI   BFR2,0              END LINE NUMBER FOR COMMAND                  
         MVI   BINDS,0                                                          
         ICM   R3,15,ALENTRY       R3=A(LINE EDIT COMMAND ENTRIES)              
         USING LEDTABD,R3                                                       
CVAL40   CLI   LENCOMM,EOT         END OF TABLE?                                
         BE    CVALERRX                                                         
         CLC   LENCOMM,COMFLD      MATCH ON COMMAND?                            
         BNE   CVAL50                                                           
         CLI   COMFLDH+FHILD,2     NON NUMERIC BLOCK COMMAND?                   
         BE    CVAL80                                                           
         B     CVAL60              ASSUME SINGLE CHAR IS NUMERIC CMND           
CVAL50   CLC   LENCOMM(1),COMFLD   MATCH ON FIRST CHAR?                         
         BNE   CVAL70                                                           
         SR    RF,RF               MUST BE FOLLOWED BY BLANKS OR NUMBER         
         IC    RF,COMFLDH+FHILD                                                 
         SH    RF,=H'1'                                                         
         BZ    CVAL60                                                           
         GOTO1 VCASHVAL,BCDMCB,(C'N',COMFLD+1),(RF)                             
         CLI   0(R1),0                                                          
         BNE   CVAL70                                                           
         ICM   RF,15,4(R1)                                                      
         BM    CVALERRX                                                         
         BNZ   *+8                                                              
CVAL60   LA    RF,1                DEFAULT TO 1 IF NO NUMERIC INPUT             
         STC   RF,BLEN             SET BLOCK LENGTH                             
         B     CVAL80                                                           
*                                                                               
CVAL70   LA    R3,LENLNQ(R3)       BUMP TO NEXT ENTRY IN TABLE                  
         B     CVAL40                                                           
*                                                                               
CVAL80   MVC   BFR1,NXTLOFF        SAVE LINE NUMBER OF COMMAND                  
         GOTO1 ACHKCONF            CHECK FOR CONFLICT ETC                       
         BNE   CVALERRX                                                         
         CLC   LENCTYPE,=AL2(LENDELQ)    DELETE COMMAND?                        
         BNE   CVAL90                                                           
         MVC   FVMSGNO,=AL2(AE$NAWTT) NOT ALLOWED                               
         TM    TXINDS,TXIINS       NOT ALLOWED ON INSERT                        
         BO    CVALERRX                                                         
         L     RF,AINP             RF=A(TIOB)                                   
         USING TIOBD,RF                                                         
         OC    TIOBCURD,TIOBCURD   DEFAULT CURSOR CONTROL OVERWRITTEN?          
         BNZ   CVAL90                                                           
         LR    RE,R2                                                            
         S     RE,ATWA                                                          
         MVI   TIOBCURI,0          SET CURSOR TO START OF FIELD                 
         STCM  RE,3,TIOBCURD       KEEP CURSOR ON SAME LINE COMMAND             
         DROP  RF                                                               
CVAL90   CLC   LENCTYPE,=AL2(LENOVERQ)   IF OVER  COMMAND                       
         BNE   CVAL95                                                           
         MVC   FVMSGNO,=AL2(AE$NAWTT) NOT ALLOWED                               
         TM    TXINDS,TXIINS       NOT ALLOWED ON INSERT                        
         BO    CVALERRX                                                         
CVAL95   GOTO1 ACHKPSIZ            CHECK SIZE OF PARA NOT EXCEEDED              
         BNE   CVALERRX                                                         
*                                                                               
CVAL100  SR    RF,RF               BUMP SCREEN LINE NUMBER                      
         IC    RF,NXTLOFF                                                       
         LA    RF,1(RF)                                                         
         STC   RF,NXTLOFF                                                       
         BCT   R0,CVAL10                                                        
         DROP  R2,R3                                                            
         DROP  R7                  ??                                           
*                                                                               
         MVC   FVMSGNO,=AL2(AE$CCONF)                                           
         MVC   FVADDR,ESECONF                                                   
         TM    ESEBFLG,BLKPCONQ                                                 
         BNO   *+12                                                             
         TM    ESEBFLG,BLKCOUTQ                                                 
         BO    CVALERRX                                                         
         L     R3,AEBLTAB          R3=A(EDIT BLOCK LIST TABLE)                  
         USING EBLTABD,R3                                                       
         XC    ESECTYP,ESECTYP     INIT SAVED COMMAND TYPE                      
CVAL110  CLC   EBLCTYPE,=AL2(EOT)  END OF TABLE                                 
         BE    CVAL120                                                          
         SR    RF,RF                                                            
         ICM   RF,3,EBLDIS         RF=(DISBLOCK LIST FOR COMMAND)               
         LA    RF,ESAVE(RF)        RF=A(BLOCK LIST FOR COMMAND)                 
         OC    0(BLKLNQ,RF),0(RF)  ANYTHING IN THE LIST?                        
         BZ    *+10                                                             
         OC    ESECTYP,EBLCTYPE    SET COMMAND IS IN USE                        
         LA    R3,EBLLNQ(R3)       BUMP TO NEXT TABLE ENTRY                     
         B     CVAL110                                                          
         DROP  R3                                                               
*                                                                               
CVAL120  LA    RF,ESEINSB          RF=A(INSERT BLOCK LIST)                      
         USING BLKVALD,RF                                                       
         OC    BLKSTRT(BLKLNQ),BLKSTRT NO INSERT COMMANDS FOUND?                
         BNZ   CVAL130                                                          
         CLI   AUTOINS,0           IS AUTO INSERT OUTSTANDING?                  
         BE    CVAL130                                                          
         CLI   ESLACTV,MAXPLINQ    PARAGRAPH FULL?                              
         BE    CVAL130                                                          
         MVC   BLKSTRT,AUTOINS     PUT AUTO INSET VALUES IN LIST                
         MVC   BLKEND,AUTOINS                                                   
         MVI   BLKLEN,1                                                         
         MVC   BLKINDS,AUTOBLKI                                                 
         DROP  RF                                                               
CVAL130  LA    R3,UNUSEINS         R3=A(UNUSED INSET LINES LIST)                
CVAL140  CLI   0(R3),EOT           END OF LIST?                                 
         BE    CVAL150                                                          
         MVI   ALTBDVAL,1          SET ALTER BLOCK VALUES BY ONE LINE           
         MVC   ALTBSTRT,0(R3)      START LINE                                   
         MVI   ALTBEND,FF          END LINE                                     
         XC    ALTBCTYP,ALTBCTYP   NO COMMAND TYPE EXCEPTIONS                   
         MVI   ALTBSTAT,ALTBSUBQ   SUBTRACT AMOUNT                              
         GOTO1 AALTBLKV            ALTER BLOCK LISTS                            
         LA    R3,1(R3)            BUMP TO NEXT UNUSED INSERT LINE NUM          
         B     CVAL140                                                          
*                                                                               
CVAL150  GOTO1 ACOMPRC             PROCESS STANDARD COMMENT TABLE               
         USING SCTABD,R3                                                        
         LA    R3,SCTAB                                                         
CVAL152  CLI   SCTABD,EOT                                                       
         BE    CVAL156                                                          
         SR    RE,RE                                                            
         ICM   RE,1,SCTNUM                                                      
         BZ    CVAL154                                                          
         BCTR  RE,0                DROP ONE LINE (ALREADY ADDED)                
         LTR   RE,RE                                                            
         BZ    CVAL154                                                          
         STC   RE,ALTBDVAL         ALTER BLOCK VALUES BY #LINES                 
         MVC   ALTBSTRT,SCTLIN     START LINE                                   
         MVI   ALTBEND,FF          END LINE                                     
         MVI   ALTBSTAT,0                                                       
         XC    ALTBCTYP,ALTBCTYP   NO COMMAND TYPE EXCEPTIONS                   
         GOTO1 AALTBLKV            ALTER BLOCK LISTS                            
CVAL154  XC    SCTABD(SCTABL),SCTABD                                            
         LA    R3,SCTABL(R3)                                                    
         B     CVAL152                                                          
         DROP  R3                                                               
*                                                                               
CVAL156  TM    ESEBFLG,BLKCOUTQ    BLOCK COMMAND OUTSTANDING?                   
         BO    CVALX                                                            
         L     R3,AEBLTAB          R3=A(EDIT BLOCK LIST TABLE)                  
         USING EBLTABD,R3                                                       
CVAL160  CLC   EBLCTYPE,=AL2(EOT)  END OF TABLE?                                
         BE    CVAL170                                                          
         SR    RF,RF                                                            
         ICM   RF,3,EBLFUNC                                                     
         BZ    CVAL162                                                          
         LA    RF,EWORKD(RF)                                                    
         LA    R0,ROUT                                                          
         CLM   R0,7,1(RF)                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,0(RF)            RF=A(EDIT FUNCTION)                          
         BASR  RE,RF               EXECUTE EDIT COMMAND                         
CVAL162  LA    R3,EBLLNQ(R3)       BUMP TO NEXT TABLE ENTRY                     
         B     CVAL160                                                          
         DROP  R3                                                               
*                                                                               
CVAL170  MVI   PRMODE,PRCHANGQ                                                  
         GOTO1 APARAREC            UPDATE PARA RECORD LINE SEQ CHANGED          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CVALX    B     EXITY                                                            
*                                                                               
CVALERRX B     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK FOR COMMAND CONFLICTS AND BUILD BLOCK VALUE LISTS             *         
* NTRY R3=A(LINE EDIT TABLE ENTRY)                                    *         
***********************************************************************         
         SPACE 1                                                                
CHKCONF  DS    0H                                                               
         USING *,R8                                                             
         USING LEDTABD,R3                                                       
         L     RF,AEBLTAB          RF=A(EDIT BLOCK LIST TABLE)                  
         USING EBLTABD,RF                                                       
CHKC10   CLC   EBLCTYPE,=AL2(EOT)  END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   LENCTYPE,EBLCTYPE   MATCH ON COMMAND                             
         BE    *+12                                                             
         LA    RF,EBLLNQ(RF)       BUMP TO NEXT ENTRY                           
         B     CHKC10                                                           
*                                                                               
         MVC   PRECCMP,EBLPRECM    SAVE COMPATIBLE PRIOR CMNDS                  
         MVC   PROCCMP,EBLPROCM    SAVE COMPATIBLE PROCEEDING COMMANDS          
         SR    R0,R0                                                            
         IC    R0,EBLNM            R0=(MAX NUMBER OF ENTRIES IN LIST)           
         SR    R2,R2                                                            
         ICM   R2,3,EBLDIS                                                      
         LA    R2,ESAVE(R2)        R2=A(BLOCK VALUE LIST)                       
         USING BLKVALD,R2                                                       
         MVC   BINDS,EBLINDS       SAVE INDICATOR VALUE                         
         DROP  RF                                                               
*                                                                               
         MVC   EHALF,LENCTYPE                                                   
         NC    EHALF,=AL2(LENCOPYQ+LENMOVEQ)                                    
         BZ    *+14                                                             
         MVC   ESECMPA,ESPARA      SAVE PARA NUMBER OF MOVE/COPY CMND           
         B     CHKC20                                                           
         MVC   EHALF,LENCTYPE                                                   
         NC    EHALF,=AL2(LENAFTQ+LENBEFQ+LENOVERQ) AFTER/BEFORE/OVER?          
         BZ    CHKC20                                                           
         MVC   ESETOPA,ESPARA      SAVE PARA INDEX                              
         MVC   ESETOPO,ESPOFST     SAVE PARA OFFSET                             
         MVC   ESETOTY,LENCTYPE    SAVE 'TO' COMMAND TYPE                       
*                                  CHECK CONFLICT WITH PRIOR COMMAND            
CHKC20   SR    RF,RF                                                            
         ICM   RF,1,BLEN           BLOCK COMMAND?                               
         BZ    CHKC30                                                           
         SR    RE,RE                                                            
         IC    RE,BFR1             RE=(START LINE NUM OF NUMERIC CMND)          
         AR    RF,RE                                                            
         BCTR  RF,0                                                             
         STC   RF,BFR2             RF=(END LINE NUM OF NUMERIC CMND)            
CHKC30   L     RF,AEBLTAB          R3=A(EDIT BLOCK LIST TABLE)                  
         USING EBLTABD,RF                                                       
CHKC40   CLC   EBLCTYPE,=AL2(EOT)  END OF TABLE?                                
         BE    CHKC120                                                          
         MVC   EHALF,EBLCTYPE                                                   
         NC    EHALF,=AL2(LENAFTQ+LENBEFQ+LENOVERQ) AFTER/BEFORE/OVER           
         BZ    *+14                                                             
         CLC   ESETOPA,ESPARA      RELEVANT TO THIS PARAGRAPH?                  
         BNE   CHKC110                                                          
         MVC   EHALF,EBLCTYPE                                                   
         NC    EHALF,=AL2(LENMOVEQ+LENCOPYQ) MOVE/COPY?                         
         BZ    *+14                                                             
         CLC   ESECMPA,ESPARA      RELEVANT TO THIS PARAGRAPH?                  
         BNE   CHKC110                                                          
         SR    RE,RE                                                            
         IC    RE,EBLNM            RE=(MAX NUMBER OF ENTRIES IN LIST)           
         SR    R1,R1                                                            
         ICM   R1,3,EBLDIS                                                      
         LA    R1,ESAVE(R1)        R1=A(BLOCK LIST OF COMMANDS)                 
         MVC   BOHALF1,PRECCMP     COMPATIBLE PRECEEDING COMMANDS               
         NC    BOHALF1,EBLCTYPE                                                 
         MVC   BOHALF2,PROCCMP     COMPATIBLE PROCEEDING COMMANDS               
         NC    BOHALF2,EBLCTYPE                                                 
*                                                                               
CHKC50   OC    0(BLKLNQ,R1),0(R1)  END OF BLOCK LIST?                           
         BZ    CHKC110                                                          
*                                  CHECK PRECEEDING COMMANDS                    
         CLC   BFR1,BLKSTRT-BLKVALD(R1) PRECEEDING COMMAND IN LIST?             
         BL    CHKC80                                                           
         OC    BOHALF1,BOHALF1     LIST COMMAND TOTALLY COMPATIBLE?             
         BNZ   CHKC100                                                          
         CLI   BLKLEN-BLKVALD(R1),0 LIST COMMAND IS BLOCK OR NUMERIC?           
         BNE   CHKC60                                                           
         MVC   EHALF,EBLCTYPE                                                   
         NC    EHALF,=AL2(LENMOVEQ+LENCOPYQ) IF BLOCK MOVE OR COPY              
         BZ    CHKC60                                                           
         MVC   EHALF,LENCTYPE                                                   
         NC    EHALF,=AL2(LENADDQ)         THEN INSERT/REPEAT EXCEPTED          
         BNZ   CHKC100                                                          
CHKC60   CLI   BLKEND-BLKVALD(R1),0 INCOMPLETE BLOCK CMND PRECEEDING?           
         BNE   CHKC70                                                           
         CLI   BLEN,0              IF NUMERIC COMMAND THEN CONFLICT             
         BNE   CHKCERRX                                                         
         B     CHKC100                                                          
CHKC70   CLC   BFR1,BLKEND-BLKVALD(R1) PRECEEDING CMND OVERLAPS CMND?           
         BNH   CHKCERRX                                                         
         B     CHKC100                                                          
*                                  CHECK PROCEEDING OFF SCREEN CMNDS            
CHKC80   OC    BOHALF2,BOHALF2     LIST COMMAND TOTALLY COMPATIBLE?             
         BNZ   CHKC100                                                          
         CLI   BLEN,0              BLOCK COMMAND?                               
         BNE   CHKC90                                                           
         TM    ESEBFLG,BLKCOUTQ    BLOCK COMMAND OUTSTANDING?                   
         BO    CHKC100                                                          
         OI    ESEBFLG,BLKPCONQ    SET POTENTIAL CONFLICT SITUATION             
         MVC   ESECONF,FVADDR      SAVE ADDRESS OF LINE CMND FIELD HDR          
         B     CHKC100                                                          
CHKC90   CLC   BFR2,BLKSTRT-BLKVALD(R1) CMND OVERLAP?                           
         BNL   CHKCERRX            COMMAND CONFLICT                             
CHKC100  LA    R1,BLKLNQ(R1)       BUMP TO NEXT BLOCK LIST VALUE                
         BCT   RE,CHKC50                                                        
CHKC110  LA    RF,EBLLNQ(RF)       BUMP TO NEXT LINE COMMAND ENTRY              
         B     CHKC40                                                           
*                                                                               
CHKC120  CLI   BLEN,0              NUMERIC OR BLOCK COMMAND?                    
         BNE   CHKC160                                                          
*                                  DEAL WITH BLOCK COMMAND                      
         OC    BLKSTRT(BLKLNQ),BLKSTRT TOTALLY FREE ENTRY?                      
         BNZ   CHKC130                                                          
         MVC   BLKSTRT,BFR1        SET START OF BLOCK                           
         MVC   BLKINDS,BINDS                                                    
         TM    ESEBFLG,BLKCOUTQ    BLOCK COMMAND OUTSTANDING?                   
         BO    CHKCERRX                                                         
         OI    ESEBFLG,BLKCOUTQ    SET BLOCK COMMAND OUTSTANDING                
         B     CHKC180                                                          
*                                                                               
CHKC130  CLI   BLKEND,0            ONLY ONE ENTRY? (HALF A BLOCK CMND)          
         BNE   CHKC140                                                          
         NI    ESEBFLG,FF-BLKCOUTQ SWITCH OFF BLOCK COMMAND OUTSTND             
         CLC   BLKSTRT,BFR1        NEW COMMAND FALLS BEFORE OR AFTER?           
         BH    *+14                                                             
         MVC   BLKEND,BFR1         AFTER                                        
         B     CHKC180                                                          
         MVC   BLKEND,BLKSTRT      IF BEFORE SWAP ROUND                         
         MVC   BLKSTRT,BFR1                                                     
         B     CHKC180                                                          
*                                  FULL BLOCK CMND ENTRY                        
CHKC140  CLC   BLKEND,BFR1         OLD END BEFORE NEW CMND?                     
         BL    CHKC170                                                          
         CLC   BLKSTRT,BFR1        OLD START AFTER NEW CMND?                    
         BH    CHKC150                                                          
         XC    BFR1,BLKEND         SWAP NEW CMND WITH OLD END                   
         XC    BLKEND,BFR1                                                      
         XC    BFR1,BLKEND                                                      
         B     CHKC170                                                          
*                                                                               
CHKC150  MVC   BCBYTE1,BFR1        SAVE NEW CMND                                
         MVC   BFR1,BLKEND         SET NEW CMND WITH OLD END                    
         MVC   BLKEND,BLKSTRT      SWAP OLD START WITH OLD END                  
         MVC   BLKSTRT,BCBYTE1     SWAP OLD START WITH NEW CMND                 
         B     CHKC170                                                          
*                                  DEAL WITH NUMERIC COMMAND                    
CHKC160  OC    BLKSTRT(BLKLNQ),BLKSTRT TOTALLY FREE ENTRY?                      
         BNZ   CHKC170                                                          
         MVC   0(BLKLNQ,R2),BFR1                                                
         B     CHKC180                                                          
*                                                                               
CHKC170  LA    R2,BLKLNQ(R2)       BUMP TO NEXT BLOCK LIST VALUE                
         BCT   R0,CHKC120                                                       
         B     CHKCERRX            BLOCK LIST FULL                              
         DROP  R2                                                               
*                                                                               
CHKC180  OC    ESECPYB,ESECPYB                                                  
         BZ    CHKC190                                                          
         OC    ESEMVEB,ESEMVEB                                                  
         BNZ   CHKCERRX                                                         
CHKC190  OC    ESEBFLN,ESEBFLN                                                  
         BZ    *+14                                                             
         OC    ESEAFLN(L'ESEAFLN+L'ESEOVLN),ESEAFLN                             
         BNZ   CHKCERRX                                                         
         OC    ESEOVLN,ESEOVLN                                                  
         BZ    *+14                                                             
         OC    ESEBFLN(L'ESEBFLN+L'ESEAFLN),ESEBFLN                             
         BNZ   CHKCERRX                                                         
*                                                                               
CHKCX    B     EXITY                                                            
*                                                                               
CHKCERRX MVC   FVMSGNO,=AL2(AE$CCONF)                                           
         B     EXITN                                                            
         DROP  RF,R3                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK SIZE/END OF PARAGRAPH NOT EXCEEEDED                           *         
* NTRY R3=A(LINE EDIT TABLE ENTRY)                                    *         
***********************************************************************         
         SPACE 1                                                                
CHKPSIZ  DS    0H                                                               
         USING *,R8                                                             
         USING LEDTABD,R3                                                       
         MVC   EHALF,LENCTYPE                                                   
         NC    EHALF,=AL2(LENAFTQ+LENBEFQ+LENOVERQ) BEFORE/AFTER CMND?          
         BZ    *+14                                                             
         MVC   ESETOAC,ESLACTV     SAVE NUM OF ACTV LINES IN 'TO' PARA          
         B     CHKP10                                                           
         MVC   EHALF,LENCTYPE                                                   
         NC    EHALF,=AL2(LENCOPYQ+LENMOVEQ) MOVE/COPY COMMAND?                 
         BZ    CHKP40                                                           
         MVC   EHALF,ESETOTY                                                    
         NC    EHALF,=AL2(LENAFTQ+LENBEFQ+LENOVERQ) HAD AFTER/BEF/OVER?         
         BZ    CHKPX                                                            
CHKP10   TM    ESEBFLG,BLKCOUTQ    ANY OUTSTANDING BLOCK COMMANDS?              
         BO    CHKPX                                                            
         USING BLKVALD,R1                                                       
         LA    R1,ESECPYB          R1=A(COPY BLOCK LIST)                        
         CLI   BLKSTRT,0           NO START VALUE FOUND?                        
         BNE   CHKP20                                                           
         LA    R1,ESEMVEB          R1=A(MOVE BLOCK LIST)                        
         CLI   BLKSTRT,0           NO START VALUE FOUND?                        
         BE    CHKPX                                                            
CHKP20   SR    RF,RF                                                            
         IC    RF,BLKEND           RF=(MOVE/COPY END LINE NUMBER)               
         SR    RE,RE                                                            
         USING SCTABD,R2                                                        
         LA    R2,SCTAB            MAY NEED TO ADJUST BLOCK END                 
         LA    R0,SCTABN                                                        
CHKP22   CLI   SCTLIN,0                                                         
         BE    CHKP26                                                           
         CLC   SCTLIN,BLKSTRT      CHECK COMMENT IN THIS BLOCK                  
         BL    CHKP24                                                           
         CLC   SCTLIN,BLKEND                                                    
         BH    CHKP24                                                           
         IC    RE,SCTNUM                                                        
         AR    RF,RE               ADD STANDARD COMMENT LINES                   
         OI    LINEFLAG,LSCIBLKQ   SET STANDARD COMMENT IN BLOCK                
CHKP24   LA    R2,SCTABL(R2)                                                    
         BCT   R0,CHKP22                                                        
CHKP26   IC    RE,BLKSTRT          RE=(MOVE/COPY START LINE NUMBER)             
         SR    RF,RE                                                            
         LA    RF,1(RF)            RF=L'(MOVE/COPY BLOCK)                       
*                                                                               
         LA    R1,ESETOLN          R1=A(FIRST 'TO' TYPE COMMAND VALUES)         
         LA    R0,L'ESETOLN/BLKLNQ R0(NUMBER OF 'TO' TYPE COMMANDS)             
         CLI   0(R1),EOT           COMMAND TYPE UNUSED?                         
         BNE   *+14                                                             
         LA    R1,BLKLNQ(R1)       BUMP TO NEXT COMMAND VALUE LIST              
         BCT   R0,*-12                                                          
         DC    H'0'                NO 'TO' COMMAND FOUND                        
         TM    LINEFLAG,LSCIBLKQ   TEST STANDARD COMMENT IN BLOCK               
         BZ    *+12                                                             
         NI    LINEFLAG,FF-LSCIBLKQ                                             
         B     CHKP30                                                           
         CLI   ESEMVEB,0           MOVE COMMAND?                                
         BE    CHKP30                                                           
         MVC   EHALF,ESETOTY                                                    
         NC    EHALF,=AL2(LENOVERQ) OVER CMND?                                  
         BNZ   CHKP30                                                           
         CLI   BLKLEN,1            NO REPEATS?                                  
         BH    CHKP30                                                           
         CLC   ESETOPA,ESECMPA     IF SAME PARAGRAPH THEN SIZE NOT CHAN         
         BE    CHKPX                                                            
CHKP30   SR    RE,RE                                                            
         IC    RE,BLKLEN           RE=(NUMBER OF REPEATS)                       
         STCM  RE,3,BCHALF                                                      
         MH    RF,BCHALF           RF=(NUMBER OF NEW LINES)                     
         SR    RE,RE                                                            
         IC    RE,ESETOAC          RE=(NUM OF ACTVE LINES IN 'TO' PARA)         
         MVC   EHALF,ESETOTY                                                    
         NC    EHALF,=AL2(LENOVERQ) OVER CMND?                                  
         BNZ   CHKP60                                                           
         B     CHKP50                                                           
CHKP40   MVC   EHALF,LENCTYPE                                                   
         NC    EHALF,=AL2(LENADDQ)         INSERT OR REPEAT?                    
         BZ    CHKPX                                                            
         SR    RF,RF                                                            
         IC    RF,BLEN             RF=(NUMBER OF NEW LINES)                     
         USING SCTABD,R2                                                        
CHKP50   LA    R2,SCTAB            MAY NEED TO ADJUST BLOCK END                 
         LA    R0,SCTABN                                                        
CHKP52   CLI   SCTLIN,0                                                         
         BE    CHKP56                                                           
         CLC   SCTLIN,BLKSTRT      CHECK COMMENT ON THIS LINE                   
         BE    CHKP54                                                           
         LA    R2,SCTABL(R2)                                                    
         BCT   R0,CHKP52                                                        
         B     CHKP56                                                           
CHKP54   SR    RE,RE                                                            
         IC    RE,SCTNUM           ADD STANDARD COMMENT LINES                   
         AR    RF,RE                                                            
         IC    RE,TOTINRP          R0=(NUM OF NEW LINS FROM PREV CMNDS)         
         AR    RF,RE                                                            
         STC   RF,TOTINRP                                                       
         IC    RE,ESLACTV          RE=(NUM OF ACTIVE LINES IN PARA)             
         AR    RF,RE               RF=(NUM OF LINES IN RESULTING PARA)          
CHKP56   MVC   FVMSGNO,=AL2(AE$MPARA) MAXIMUM PARA SIZE EXCEEDED                
         CH    RF,=Y(MAXPLINQ)                                                  
         BH    CHKPERRX                                                         
         B     CHKPX                                                            
*                                                                               
CHKP60   MVC   FVMSGNO,=AL2(AE$EPEX) END OF PARAGRAPH EXCEEDED                  
         SR    R0,R0                                                            
         IC    R0,BLKSTRT          R0=('TO' LINE FOR OVER CMND)                 
         BCTR  R0,0                RF=(LAST LINE NUMBER OF BLOCK)               
         AR    RF,R0                                                            
         CR    RF,RE               BEYOND NUMBER OF ACTIVE LINES?               
         BH    CHKPERRX                                                         
*                                                                               
CHKPX    B     EXITY                                                            
*                                                                               
CHKPERRX B     EXITN                                                            
         DROP  R1,R2,R3                                                         
         EJECT                                                                  
***********************************************************************         
* DELETE LINES FROM A PARAGRAPH                                       *         
* NTRY R3=A(LINE EDIT TABLE ENTRY)                                    *         
***********************************************************************         
         SPACE 1                                                                
DEL      DS    0H                                                               
         USING *,R8                                                             
         USING EBLTABD,R3                                                       
         CLI   ESLACTV,0                                                        
         BE    DELX                                                             
         SR    R1,R1                                                            
         ICM   R1,3,EBLDIS                                                      
         LA    R1,ESAVE(R1)        R1=A(DELETE LIST VALUES)                     
         USING BLKVALD,R1                                                       
         SR    R0,R0                                                            
         IC    R0,EBLNM            R0=(MAX NO OF ENTRIES IN LIST)               
DEL10    OC    BLKSTRT(BLKLNQ),BLKSTRT END OF LIST?                             
         BZ    DELX                                                             
         CLC   BLKEND,ESLACTV      NUMERICAL DELETE PAST PARAGRAPH END?         
         BNH   DEL20                                                            
         MVC   BLKEND,ESLACTV      SET DELETE END TO PARA END                   
         SR    RF,RF                                                            
         IC    RF,BLKEND           RECALCULATE BLOCK LENGTH                     
         SR    RE,RE                                                            
         IC    RE,BLKSTRT                                                       
         SR    RF,RE                                                            
         LA    RF,1(RF)                                                         
         STC   RF,BLKLEN                                                        
DEL20    MVI   CMPMODE,CMPMOVEQ    MOVE MODE                                    
         MVC   CMPTO,ESLHIGH       MOVE TO OFFSET                               
         MVC   CMPFROM1,BLKSTRT    MOVE FROM OFFSET 1 (START)                   
         MVC   CMPFROM2,BLKEND     MOVE FROM OFFSET 2 (END)                     
         LA    RF,ESLLST           RF=A(LINE INDEX LIST)                        
         ST    RF,CMPAILST                                                      
         MVI   CMPILLN,L'ESLLST   L'(LINE INDEX LIST)                           
         GOTO1 ACMI                                                             
         SR    RF,RF                                                            
         ICM   RF,1,BLKLEN         NUMERIC DELETE COMMAND?                      
         BNZ   DEL30                                                            
         SR    RE,RE                                                            
         IC    RE,BLKSTRT                                                       
         SR    RF,RF                                                            
         IC    RF,BLKEND                                                        
         SR    RF,RE                                                            
         LA    RF,1(RF)            RF=(NUMBER OF LINES TO BE DELETED)           
DEL30    SR    RE,RE                                                            
         IC    RE,ESLACTV                                                       
         SR    RE,RF                                                            
         STC   RE,ESLACTV          RESET NUMBER OF LINES IN PARA                
         STC   RF,ALTBDVAL                                                      
         MVC   ALTBSTRT,BLKEND                                                  
         MVI   ALTBEND,FF                                                       
         MVI   ALTBSTAT,ALTBSUBQ                                                
         XC    ALTBCTYP,ALTBCTYP                                                
         GOTO1 AALTBLKV            ADJUST ANY OTHER LINE COMMANDS               
         OI    LINEFLAG,LCHANGEQ   SET LINE INDEX LIST CHANGED                  
         LA    R1,BLKLNQ(R1)       BUMP TO NEXT LIST VALUE                      
         BCT   R0,DEL10                                                         
         DROP  R1                                                               
*                                                                               
DELX     XC    ESEDELB,ESEDELB     CLEAR OUT DELETE LIST                        
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* COPY/MOVE LINES FROM ONE PARA TO ANOTHER OR WITHIN ONE PARAGRAPH    *         
* NTRY R3=A(LINE EDIT TABLE ENTRY FOR COPY/MOVE)                      *         
***********************************************************************         
         SPACE 1                                                                
CMLN     DS    0H                                                               
         USING *,R8                                                             
         USING EBLTABD,R3                                                       
         GOTO1 MAKETX,R7                                                        
         USING TXD,R7              R7=A(AREA FOR TXD)                           
         SR    R2,R2                                                            
         ICM   R2,3,EBLDIS                                                      
         DROP  R3                                                               
         LA    R2,ESAVE(R2)        R2=A(COPY/MOVE VALUES LIST)                  
         USING BLKVALD,R2                                                       
         OC    BLKSTRT(BLKLNQ),BLKSTRT ANY COPY VALUES?                         
         BNZ   CML10                                                            
         OC    ESETOLN,ESETOLN         ANY 'TO' VALUES?                         
         BZ    CMLX                                                             
         B     *+14                                                             
*                                                                               
CML10    OC    ESETOLN,ESETOLN                                                  
         BNZ   *+12                                                             
         OI    ESEBFLG,BLKPENDQ COPY/MOVE IS PENDING                            
         B     CMLX                                                             
*                                                                               
         NI    ESEBFLG,FF-BLKPENDQ SWITCH OFF COPY/MOVE IS PENDING              
         XC    FRMLLST,FRMLLST     INIT 'FROM' LIST                             
         CLC   ESPARA,ESECMPA      CURRENT PARAGRAPH SAME AS FROM PARA?         
         BNE   CML60                                                            
         CLI   BLKLEN,0            NUMERICAL COMMAND?                           
         BE    CML20                                                            
         CLC   BLKEND,ESLACTV      IF NUMERICAL COMMAND PAST PARA END           
         BNH   CML30                                                            
         MVC   BLKEND,ESLACTV      THEN RESET WITH PARA END                     
CML20    SR    RE,RE               CALCULATE BLOCK LENGTH                       
         IC    RE,BLKEND                                                        
         SR    RF,RF                                                            
         IC    RF,BLKSTRT                                                       
         SR    RE,RF                                                            
         LA    RE,1(RE)                                                         
         STC   RE,BLKLEN                                                        
CML30    SR    R1,R1                                                            
         IC    R1,BLKSTRT                                                       
         BCTR  R1,0                                                             
         LA    R1,ESLLST(R1)       R1=A(LINE INDEX AT START OF BLOCK)           
         SR    RE,RE                                                            
         IC    RE,BLKLEN                                                        
         BCTR  RE,0                RE=X L'(MOVE/COPY BLOCK)                     
         EX    RE,*+4                                                           
         MVC   FRMLLST(0),0(R1)    SAVE 'FROM' LINE LIST                        
         MVC   EHALF,ESECTYP                                                    
         NC    EHALF,=AL2(LENMOVEQ) MOVE COMMAND?                               
         BZ    CML40                                                            
         MVI   CMPMODE,CMPMOVEQ    MOVE MODE                                    
         MVC   CMPTO,ESLHIGH       MOVE TO OFFSET                               
         MVC   CMPFROM1,BLKSTRT    MOVE FROM OFFSET 1 (START)                   
         MVC   CMPFROM2,BLKEND     MOVE FROM OFFSET 2 (END)                     
         LA    RF,ESLLST                                                        
         ST    RF,CMPAILST                                                      
         MVI   CMPILLN,L'ESLLST                                                 
         GOTO1 ACMI                                                             
         SR    R1,R1                                                            
         IC    R1,BLKLEN                                                        
         SR    RE,RE                                                            
         IC    RE,ESLACTV                                                       
         SR    RE,R1                                                            
         STC   RE,ESLACTV          ADJUST PARAGRAPH LENGTH                      
         OI    LINEFLAG,LCHANGEQ   SET LINE INDEX LIST CHANGED                  
         CLC   ESPARA,ESETOPA      CURRENT PARAGRAPH SAME AS 'TO' PARA?         
         BNE   CML50                                                            
         MVC   ALTBDVAL,BLKLEN                                                  
         MVC   ALTBSTRT,BLKSTRT                                                 
         MVI   ALTBEND,FF                                                       
         MVC   ALTBCTYP,=AL2(LENMOVEQ)                                          
         MVI   ALTBSTAT,ALTBSUBQ                                                
         GOTO1 AALTBLKV            ADJUST ANY OTHER LINE COMMANDS               
         B     CML100                                                           
CML40    CLC   ESPARA,ESETOPA      CURRENT PARAGRAPH SAME AS 'TO' PARA?         
         BE    CML100                                                           
CML50    MVI   PRMODE,PRCHANGQ                                                  
         GOTO1 APARAREC            CHANGE PARAGRAPH RECORD IF NECESSARY         
         BE    CML90                                                            
         DC    H'0'                                                             
*                                  'FROM' PARA IS NOT THE CURRENT PARA          
CML60    LA    RF,IOKEY            RF=A(KEY FOR PRODUCTION BILL RECORD)         
         USING PBRRECD,RF                                                       
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ    RECORD TYPE                                  
         MVC   PBRKCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRKSUB,PBRKACTQ    ACTIVE RECORD                                
         MVC   PBRKJOB,ESJOB       JOB                                          
         MVC   PBRKSEQ,ESSEQ       SEQUENCE# (COMPLEMENT)                       
         MVC   PBRKPARA,ESECMPA    PARAGRAPH#                                   
         GOTO1 AIO,IO1+IOACCMST+IORDUP GET PARAGRAPH RECORD                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO1             RF=A('FROM' PARA RECORD)                     
*                                                                               
         LA    R4,PBRRFST          R4=A(FIRST ELEMENT)                          
         USING NDXELD,R4                                                        
CML70    CLI   NDXEL,EOR           END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   NDXEL,NDXELQ        INDEX ELEMENT?                               
         BE    CML80                                                            
*                                                                               
         SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,NDXLN                                                         
         AR    R4,R0                                                            
         B     CML70                                                            
*                                                                               
CML80    CLI   BLKLEN,0            NUMERICAL COMMAND?                           
         BE    CML85                                                            
         CLC   BLKEND,NDXACTV      IF NUMERICAL COMMAND PAST PARA END           
         BNH   CML87                                                            
         MVC   BLKEND,NDXACTV      THEN RESET WITH PARA END                     
CML85    SR    RE,RE               CALCULATE BLOCK LENGTH                       
         IC    RE,BLKEND                                                        
         SR    R0,R0                                                            
         IC    R0,BLKSTRT                                                       
         SR    RE,R0                                                            
         LA    RE,1(RE)                                                         
         STC   RE,BLKLEN                                                        
CML87    SR    R1,R1                                                            
         IC    R1,BLKSTRT                                                       
         BCTR  R1,0                                                             
         LA    R1,NDXINDX(R1)                                                   
         SR    RE,RE                                                            
         IC    RE,BLKLEN           RE=L'(INDEX ELEMENT)                         
         BCTR  RE,0                                                             
         EX    RE,*+4              RE=(X LENGTH OF INDEX LIST)                  
         MVC   FRMLLST(0),0(R1)    SAVE FROM LINE LIST                          
*                                                                               
         MVC   EHALF,ESECTYP                                                    
         NC    EHALF,=AL2(LENMOVEQ)                                             
         BZ    CML100                                                           
         MVI   CMPMODE,CMPMOVEQ    MOVE MODE                                    
         MVC   CMPTO,NDXHIGH       MOVE TO OFFSET                               
         MVC   CMPFROM1,BLKSTRT    MOVE FROM OFFSET 1 (START)                   
         MVC   CMPFROM2,BLKEND     MOVE FROM OFFSET 2 (END)                     
         LA    RE,NDXINDX                                                       
         ST    RE,CMPAILST                                                      
         MVI   CMPILLN,(L'NDXINDX*200)                                          
         GOTO1 ACMI                                                             
         SR    R1,R1                                                            
         IC    R1,BLKLEN                                                        
         SR    RE,RE                                                            
         IC    RE,NDXACTV                                                       
         SR    RE,R1                                                            
         STC   RE,NDXACTV          ADJUST PARAGRAPH LENGTH                      
         DROP  R4                                                               
         GOTO1 AIO,IO1+IOACCMST+IOPUTREC PUT BACK A RECORD                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AUPDTOT,BOPARM,ESECMPA - UPDATE TOTALS FOR PARA                  
         B     CML100                                                           
*                                                                               
CML90    CLC   ESPARA,ESETOPA      'TO' PARA IS CURRENT PARA?                   
         BE    CML100                                                           
         MVI   ESSTLIN,FF          SET HIGH VALUE SO START IS RE-SET            
         LA    R1,ESETOPO          R1=(COPY/MOVE TO PARAGRAPH)                  
         GOTO1 AGPARA                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
CML100   SR    RF,RF                                                            
         ICM   RF,1,BLKLEN                                                      
         STCM  RF,3,BCHALF         SAVE MOVE/COPY BLOCK LENGTH                  
*                                                                               
         LA    RF,ESETOLN          RF=A('TO' VALUES LISTS)                      
         LA    R0,L'ESETOLN/BLKLNQ R0=(NUMBER OF 'TO' LISTS)                    
         CLI   0(RF),0             EMPTY LIST?                                  
         BNE   *+14                                                             
         LA    RF,BLKLNQ(RF)       BUMP TO NEXT ONE                             
         BCT   R0,*-12                                                          
         DC    H'0'                                                             
         MVC   TOLINE,BLKSTRT-BLKVALD(RF) SAVE 'TO' START LINE                  
         SR    RE,RE                                                            
         IC    RE,TOLINE                                                        
         CLI   ESLACTV,0           NO LINES IN PARA?                            
         BE    CML110                                                           
         CLC   ESETOTY,=AL2(LENAFTQ) AFTER COMMAND?                             
         BNE   CML110                                                           
         LA    RE,1(RE)            BUMP UP START LINE                           
         STC   RE,TOLINE           RE=('TO' LINE NUMBER)                        
CML110   MVC   TOREPNO,BLKLEN-BLKVALD(RF) GET NUMBER OF REPEATS                 
*                                                                               
         SR    RF,RF                                                            
         IC    RF,ESSTLIN          RF=(SCREEN START LINE)                       
         CR    RE,RF               IS 'TO' LINE ON CURRENT SCREEN?              
         BL    CML120                                                           
         LA    R1,MAXSLINQ-1(RF)   R1=(SCREEN END LINE)                         
         CR    RE,R1                                                            
         BH    CML120                                                           
*                                  'TO' LINE IS ON CURRENT SCREEN               
         SR    RE,RF                                                            
         B     CML122                                                           
*                                                                               
CML120   STC   RE,ESSTLIN                                                       
         XR    RE,RE                                                            
CML122   STC   RE,TSARLIN#                                                      
         GOTO1 ARESTX,BOPARM,TXD                                                
         L     RE,TXACOM           RE=A(LINE COMMAND FIELD HEADER)              
         L     RF,AINP                                                          
         USING TIOBD,RF                                                         
         S     RE,ATWA                                                          
         STCM  RE,3,TIOBCURD       OVERRIDE DEFAULT CURSOR CONTROL              
         MVI   TIOBCURI,0          SET CURSOR TO BEGINING OF FIELD              
         DROP  RF                                                               
*                                                                               
         LA    R3,FRMLLST          R3=A('FROM' LINE INDEX LIST)                 
         SR    R0,R0                                                            
         IC    R0,BLKLEN           R0=(LENGTH OF MOVE/COPY BLOCK)               
CML130   MVC   BCBYTE1,TOLINE      'TO' LINE NUMBER                             
         LA    RF,IOKEY            RF=A(KEY FOR PRODUCTION BILL RECORD)         
         USING PBRRECD,RF                                                       
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ    RECORD TYPE                                  
         MVC   PBRKCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRKSUB,PBRKACTQ    ACTIVE RECORD                                
         MVC   PBRKJOB,ESJOB       JOB                                          
         MVC   PBRKSEQ,ESSEQ       SEQUENCE# (COMPLEMENT)                       
         MVC   PBRKPARA,ESECMPA    PARAGRAPH#                                   
         MVC   PBRKLINE,0(R3)      LINE#                                        
         DROP  RF                                                               
         GOTO1 AIO,IO1+IOACCMST+IOREAD GET LINE NUMBER RECORD                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGETTX,BOPARM,TXD,AIO1                                           
*                                                                               
         MVI   LRMODE,LRADDQ       ADD LINE RECORD                              
         CLC   ESETOTY,=AL2(LENOVERQ) OVER WRITE COMMAND?                       
         BNE   *+8                                                              
         MVI   LRMODE,LROVRWRT     OVER WRITE RECORD                            
         MVC   LRPARA,ESPARA       PARA INDEX                                   
         SR    R6,R6                                                            
         IC    R6,TOREPNO           R6=(NUMBER OF REPEATS)                      
CML170   MVC   LRLINE,BCBYTE1       LINE NUMBER                                 
         GOTO1 ALINEREC,TXD         CHANGE/ADD LINE RECORD                      
         SR    RE,RE                                                            
         IC    RE,BCBYTE1           RE=(LINE NUMBER)                            
         SR    RF,RF                                                            
         IC    RF,BLKLEN            RF=(MOVE/COPY BLOCK LENGTH)                 
         CLC   ESETOTY,=AL2(LENOVERQ) OVER WRITE COMMAND?                       
         BE    *+10                                                             
         SR    RF,R0                                                            
         LA    RF,1(RF)                                                         
         AR    RE,RF                                                            
         STC   RE,BCBYTE1           RE=(NEXT LINE NUMBER FOR ADD/CHA)           
         BCT   R6,CML170            REPEAT LINE                                 
         LA    R3,1(R3)             R3=A(NEXT 'FROM' LINE INDEX)                
         SR    RE,RE                                                            
         IC    RE,TOLINE                                                        
         LA    RE,1(RE)                                                         
         STC   RE,TOLINE            BUMP 'TO' LINE NUMBER                       
         BCT   R0,CML130            REPEAT FOR NUMBER OF LINES IN BLOCK         
*                                                                               
         CLC   ESETOTY,=AL2(LENOVERQ) OVER WRITE?                               
         BE    CML180                                                           
         SR    RF,RF                                                            
         IC    RF,BLKLEN                                                        
         STCM  RF,3,BCHALF                                                      
         SR    RE,RE                                                            
         IC    RE,TOREPNO                                                       
         MH    RE,BCHALF                                                        
         STC   RE,ALTBDVAL                                                      
         MVC   ALTBSTRT,TOLINE                                                  
         MVI   ALTBEND,FF                                                       
         MVI   ALTBSTAT,0                                                       
         XC    ALTBCTYP,ALTBCTYP                                                
         GOTO1 AALTBLKV             ADJUST ALL OTHER LINE COMMANDS              
CML180   XC    BLKSTRT(BLKLNQ),BLKSTRT CLEAR COPY/MOVE BLOCK LISTS              
         XC    ESETOLN,ESETOLN     CLEAR 'TO' BLOCK LISTS                       
CMLX     B     EXIT                                                             
         DROP  R2,R7                                                            
         EJECT                                                                  
***********************************************************************         
* REPEAT LINES WITHIN A PARAGRAPH                                     *         
* NTRY R3=A(LINE EDIT TABLE ENTRY)                                    *         
***********************************************************************         
         SPACE 1                                                                
REPT     DS    0H                                                               
         USING *,R8                                                             
         USING EBLTABD,R3                                                       
         SR    R2,R2                                                            
         ICM   R2,3,EBLDIS                                                      
         LA    R2,ESAVE(R2)        R2=A(REPEAT VALUES LIST)                     
         USING BLKVALD,R2                                                       
         SR    R0,R0                                                            
         IC    R0,EBLNM            R0=(MAX NO OF ENTRIES IN BLOCK LIST)         
REPT10   OC    BLKSTRT(BLKLNQ),BLKSTRT ANY REPEAT VALUES?                       
         BZ    REPTX                                                            
         LA    RF,IOKEY            RF=A(LINE RECORD KEY)                        
         USING PBRRECD,RF                                                       
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ    RECORD TYPE                                  
         MVC   PBRKCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRKSUB,PBRKACTQ    ACTIVE RECORD                                
         MVC   PBRKJOB,ESJOB       JOB                                          
         MVC   PBRKSEQ,ESSEQ       SEQUENCE# (COMPLEMENT)                       
         MVC   PBRKPARA,ESPARA     PARAGRAPH#                                   
         SR    RE,RE                                                            
         IC    RE,BLKSTRT          OFFSET TO LINE INDEX                         
         BCTR  RE,0                                                             
         LA    RE,ESLLST(RE)       RE=A(CORRESPONDING INDEX FOR LINE)           
         MVC   PBRKLINE,0(RE)      LINE#                                        
         GOTO1 AIO,IO1+IOACCMST+IOREAD GET LINE NUMBER RECORD                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 MAKETX,R7                                                        
         USING TXD,R7                                                           
         GOTO1 AGETTX,BOPARM,TXD,AIO1                                           
*                                                                               
         MVI   LRMODE,LRADDQ       ADD LINE                                     
         MVC   LRLINE,BLKSTRT      LINE NUMBER                                  
         MVC   LRPARA,ESPARA       PARA NUMBER                                  
         SR    R6,R6                                                            
         IC    R6,BLKLEN                                                        
         L     RF,ALINEREC                                                      
         LA    R1,TXD                                                           
         BASR  RE,RF                                                            
         BCT   R6,*-2                                                           
*                                                                               
         MVC   ALTBDVAL,BLKLEN                                                  
         MVC   ALTBSTRT,BLKSTRT                                                 
         MVI   ALTBEND,FF                                                       
         MVI   ALTBSTAT,0                                                       
         XC    ALTBCTYP,ALTBCTYP                                                
         GOTO1 AALTBLKV            ADJUST OTHER LINE COMMANDS                   
*                                                                               
         LA    R2,BLKLNQ(R2)       BUMP TO NEXT REPEAT VALUE                    
         BCT   R0,REPT10                                                        
*                                                                               
REPTX    XC    ESEREPB,ESEREPB     CLEAR REPEAT VALUE LIST                      
         B     EXIT                                                             
         DROP  R2,R3,R7                                                         
         EJECT                                                                  
***********************************************************************         
* ADD SUB-TOTAL LINE WITHIN A PARAGRPAH                               *         
* NTRY R3=A(LINE EDIT TABLE ENTRY)                                    *         
***********************************************************************         
         SPACE 1                                                                
ADDS     DS    0H                                                               
         USING *,R8                                                             
         USING EBLTABD,R3                                                       
         SR    R2,R2                                                            
         ICM   R2,3,EBLDIS                                                      
         LA    R2,ESAVE(R2)        R2=A(SUB-TOT VALUES LIST)                    
         USING BLKVALD,R2                                                       
         SR    R0,R0                                                            
         IC    R0,EBLNM            R0=(MAX NO OF ENTRIES IN BLOCK LIST)         
ADDS10   OC    BLKSTRT(BLKLNQ),BLKSTRT ANY SUB-TOT VALUES?                      
         BZ    ADDSX                                                            
         GOTO1 MAKETX,R7                                                        
         USING TXD,R7                                                           
         GOTO1 AINITX,BOPARM,('PBRLSUB',TXD)                                    
*                                                                               
         MVI   LRMODE,LRADDQ       ADD LINE                                     
         IC    RE,BLKSTRT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,LRLINE                                                        
*        MVC   LRLINE,BLKSTRT      LINE NUMBER                                  
         MVC   LRPARA,ESPARA       PARA NUMBER                                  
         SR    R6,R6                                                            
         IC    R6,BLKLEN                                                        
         L     RF,ALINEREC                                                      
         LA    R1,TXD                                                           
         BASR  RE,RF                                                            
         BCT   R6,*-2                                                           
*                                                                               
         MVC   ALTBDVAL,BLKLEN                                                  
         MVC   ALTBSTRT,BLKSTRT                                                 
         MVI   ALTBEND,FF                                                       
         MVI   ALTBSTAT,0                                                       
         XC    ALTBCTYP,ALTBCTYP                                                
         GOTO1 AALTBLKV            ADJUST OTHER LINE COMMANDS                   
*                                                                               
         LA    R2,BLKLNQ(R2)       BUMP TO NEXT SUB-TOT VALUE                   
         BCT   R0,ADDS10                                                        
         OI    EWINDS,EWITOT       SET UPDATE PARAGRPAH TOTALS                  
*                                                                               
ADDSX    XC    ESESUBB,ESESUBB     CLEAR SUB-TOT VALUE LIST                     
         B     EXIT                                                             
         DROP  R2,R3,R7                                                         
         EJECT                                                                  
***********************************************************************         
* ADD TOTAL LINE WITHIN A PARAGRAPH                                   *         
* NTRY R3=A(LINE EDIT TABLE ENTRY)                                    *         
***********************************************************************         
         SPACE 1                                                                
ADDT     DS    0H                                                               
         USING *,R8                                                             
         USING EBLTABD,R3                                                       
         SR    R2,R2                                                            
         ICM   R2,3,EBLDIS                                                      
         LA    R2,ESAVE(R2)        R2=A(TOTAL VALUES LIST)                      
         USING BLKVALD,R2                                                       
         SR    R0,R0                                                            
         IC    R0,EBLNM            R0=(MAX NO OF ENTRIES IN BLOCK LIST)         
ADDT10   OC    BLKSTRT(BLKLNQ),BLKSTRT ANY TOTAL VALUES?                        
         BZ    ADDTX                                                            
         GOTO1 MAKETX,R7                                                        
         USING TXD,R7                                                           
         GOTO1 AINITX,BOPARM,('PBRLTOT',TXD)                                    
*                                                                               
         MVI   LRMODE,LRADDQ       ADD LINE                                     
         IC    RE,BLKSTRT                                                       
         LA    RE,1(RE)                                                         
         STC   RE,LRLINE                                                        
*        MVC   LRLINE,BLKSTRT      LINE NUMBER                                  
         MVC   LRPARA,ESPARA       PARA NUMBER                                  
         SR    R6,R6                                                            
         IC    R6,BLKLEN                                                        
         L     RF,ALINEREC                                                      
         LA    R1,TXD                                                           
         BASR  RE,RF                                                            
         BCT   R6,*-2                                                           
*                                                                               
         MVC   ALTBDVAL,BLKLEN                                                  
         MVC   ALTBSTRT,BLKSTRT                                                 
         MVI   ALTBEND,FF                                                       
         MVI   ALTBSTAT,0                                                       
         XC    ALTBCTYP,ALTBCTYP                                                
         GOTO1 AALTBLKV            ADJUST OTHER LINE COMMANDS                   
*                                                                               
         LA    R2,BLKLNQ(R2)       BUMP TO NEXT TOTAL VALUE                     
         BCT   R0,ADDT10                                                        
         OI    EWINDS,EWITOT       SET UPDATE PARAGRPAH TOTALS                  
*                                                                               
ADDTX    XC    ESETOTB,ESETOTB     CLEAR TOTAL VALUE LIST                       
         B     EXIT                                                             
         DROP  R2,R3,R7                                                         
         EJECT                                                                  
***********************************************************************         
* INITIALISE BLOCK VALUES FOR CURRENT SCREEN                          *         
* NTRY- FOLLOWING PARAMTERS MUST BE SET                               *         
* XIT- BLOCK LINE VALUES CURRENTLY ON SCREEN ARE INITIALISED          *         
***********************************************************************         
         SPACE 1                                                                
INBVAL   DS    0H                                                               
         USING *,R8                                                             
         L     R3,AEBLTAB          R3=A(EDIT BLOCK LIST TABLE)                  
         USING EBLTABD,R3                                                       
INBV10   CLC   EBLCTYPE,=AL2(EOT)  END OF TABLE?                                
         BE    INBV120                                                          
         MVC   INBLSTNM,EBLNM      SET MAX NUMBER OF ENTRIES IN LIST            
         SR    RF,RF                                                            
         ICM   RF,3,EBLDIS                                                      
         LA    RF,ESAVE(RF)        RF=A(BLOCK VALUES LIST)                      
         ST    RF,INABLST                                                       
         MVC   EHALF,EBLCTYPE                                                   
         NC    EHALF,=AL2(LENMOVEQ+LENCOPYQ) MOVE/COPY COMMAND                  
         BZ    INBV20                                                           
         CLC   ESPARA,ESECMPA      SAME PARA AS DISPLAYED?                      
         BNE   INBV100                                                          
         B     INBV30                                                           
INBV20   MVC   EHALF,EBLCTYPE                                                   
         NC    EHALF,=AL2(LENAFTQ+LENBEFQ+LENOVERQ) AFTER/BEFORE/OVER           
         BZ    INBV30                                                           
         CLC   ESPARA,ESETOPA      SAME PARA AS DISPLAYED?                      
         BNE   INBV100                                                          
*                                                                               
INBV30   SR    RF,RF                                                            
         IC    RF,ESSTLIN          RF=(LINE NO AT START OF CURR SCREEN)         
         LA    RE,MAXSLINQ(RF)                                                  
         BCTR  RE,0                RE=(LINE NO AT END OF CURR SCREEN)           
         STC   RF,BCBYTE1                                                       
         STC   RE,BCBYTE2                                                       
         SR    R2,R2                                                            
         ICM   R2,1,INBLSTNM       R2=(MAX NUM OF ENTRIES IN LIST)              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   R4,15,INABLST       R4=A(BLOCK VALUES LIST)                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING BLKVALD,R4                                                       
INBV40   OC    BLKSTRT(BLKLNQ),BLKSTRT END OF LIST?                             
         BZ    INBV100                                                          
         CLC   BCBYTE1,BLKSTRT     BLOCK LINE BEFORE START OF SCREEN?           
         BH    INBV50                                                           
         CLC   BCBYTE2,BLKSTRT     BLOCK LINE AFTER END OF SCREEN?              
         BL    INBV50                                                           
         CLI   BLKLEN,0            NUMERIC COMMAND?                             
         BE    *+14                                                             
         XC    BLKSTRT(BLKLNQ),BLKSTRT CLEAR ALL VALS FOR NUMERIC CMND          
         B     INBV80                                                           
         MVI   BLKSTRT,0           CLEAR BLOCK START                            
         CLI   BLKEND,0            EMPTY SLOT?                                  
         BNE   INBV50                                                           
         NI    ESEBFLG,FF-BLKCOUTQ SWITCH OFF BLOCK COMMAND OUTSTND             
         B     INBV80                                                           
INBV50   CLI   BLKLEN,0            NUMERIC COMMAND?                             
         BNE   INBV60                                                           
         CLC   BCBYTE1,BLKEND      BLOCK VALUE BEFORE START OF SCREEN?          
         BH    INBV60                                                           
         CLC   BCBYTE2,BLKEND      BLOCK VALUE AFTER END OF SCREEN?             
         BL    INBV60                                                           
         MVI   BLKEND,0            CLEAR OUT END VALUE                          
         B     INBV60                                                           
*                                                                               
INBV60   OC    BLKSTRT(BLKLNQ),BLKSTRT EMPTY SLOT?                              
         BZ    INBV80                                                           
*                                                                               
         CLI   BLKEND,0            END VALUE?                                   
         BE    INBV70                                                           
         CLI   BLKSTRT,0           START VALUE?                                 
         BNE   INBV90                                                           
         MVC   BLKSTRT,BLKEND      PUT END VALUE IN START SLOT                  
         MVI   BLKEND,0                                                         
INBV70   OI    ESEBFLG,BLKCOUTQ    SWITCH ON BLOCK COMMAND OUTSTND              
         B     INBV90                                                           
*                                                                               
INBV80   LR    R0,R4               R0=R4=A(BLOCK VALUE LIST ENTRY)              
         LA    RE,BLKLNQ(R4)       RE=A(NEXT BLOCK VALUE LIST ENTRY)            
         SR    RF,RF                                                            
         IC    RF,INBLSTNM         RF=(MAX NUM OF ENTRIES IN LIST)              
         MH    RF,=Y(BLKLNQ)                                                    
         A     RF,INABLST          RF=A(END OF LIST)                            
         LR    R1,RF                                                            
         SR    R1,R0               R1=L'(REMAINING LIST VALUES)                 
         SR    RF,RE               RE=L'(REMAINING LIST AFTER NXT NTRY)         
         MVCL  R0,RE               OVERWRITE EMPTY SLOT                         
         B     *+8                                                              
*                                                                               
INBV90   LA    R4,BLKLNQ(R4)                                                    
         BCT   R2,INBV40           BUMP TO NEXT BLOCK VALUE                     
         DROP  R4                                                               
INBV100  L     RF,INABLST          RF=A(BLOCK VALUE LIST)                       
         CLI   0(RF),EOT           END OF TABLE                                 
         BNE   INBV110                                                          
         MVC   BCHALF,ESECTYP      SWITCH OFF SAVED COMMAND TYPE                
         NC    BCHALF,EBLCTYPE                                                  
         BZ    *+10                                                             
         XC    ESECTYP,EBLCTYPE                                                 
         MVC   BCHALF,ESETOTY      SWITCH OF 'TO' COMMAND TYPE                  
         NC    BCHALF,EBLCTYPE                                                  
         BZ    INBV110                                                          
         XC    ESETOTY,EBLCTYPE                                                 
INBV110  LA    R3,EBLLNQ(R3)       BUMP TO NEXT LINE COMMAND ENTRY              
         B     INBV10                                                           
         DROP  R3                                                               
INBV120  OC    ESETOTY,ESETOTY     NO 'TO' TYPE COMMAND?                        
         BNZ   *+10                                                             
         XC    ESETOLN(ESETOLQ),ESETOLN INIT 'TO' VALUES                        
         MVC   EHALF,ESECTYP                                                    
         NC    EHALF,=AL2(LENCOPYQ+LENMOVEQ) NO MOVE/COPY COMMANDS?             
         BNZ   *+8                                                              
         MVI   ESECMPA,0           INIT MOVE/COPY PARAGRAPH NUMBER              
INBVX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DEAL WITH COMMAND LINE                                              *         
***********************************************************************         
         SPACE 1                                                                
VALOPTS  DS    0H                                                               
         USING *,R8                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF) INVALID INPUT FIELD                       
         MVI   VOPTFLAG,0          INITIALISE COMMAND FLAG                      
         MVI   ESCMND,0            INITIALISE COMMAND TYPE                      
         LA    R2,BASOPTH          R2=A(OPTION LINE FIELD HEADER)               
         USING FHD,R2                                                           
         TM    FHII,FHIIVA         PREVIOUSLY VALIDATED?                        
         BO    VOPTX                                                            
         ST    R2,FVADDR                                                        
         CLI   FHIL,0              ANY INPUT?                                   
         BE    VOPT310                                                          
         LA    R0,WORKLST          CLEAR AREA USED FOR SCANNING                 
         LH    R1,=Y(L'WORKLST)                                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    FSVAL,FSVAL         INIT FREE FORM TEXT STRING VALS              
         MVC   VOPTLNH,BASOPTH     INIT COMMAND LINE                            
         XC    VOPTLN,VOPTLN       INIT COMMAND LINE                            
         SR    RF,RF                                                            
         IC    RF,FHIL             R0=L'(COMMAND LINE INPUT)                    
         LR    R0,RF                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   VOPTLN(0),BASOPT    GET COMMAND LINE                             
         SR    RF,RF                                                            
VOPT10   LA    RE,VOPTLN(RF)       RE=A(NEXT INPUT CHAR)                        
         CLI   0(RE),C''''         SINGLE DINK?                                 
         BE    VOPT20                                                           
         CLI   0(RE),C'"'          DOUBLE DINK?                                 
         BNE   VOPT80                                                           
VOPT20   CLI   FSVALST1,0          START DINK 1 UNUSED?                         
         BNE   VOPT30                                                           
         STC   RF,FSVALST1         SET START DINK 1                             
         MVC   FSVALTP1,0(RE)      SAVE DINK TYPE                               
         B     VOPT80                                                           
VOPT30   CLC   FSVALTP1,0(RE)      MATCH ON DINK TYPE?                          
         BE    VOPT40                                                           
         CLI   FSVALEN1,0          END DINK 1 UNUSED?                           
         BE    VOPT80                                                           
         B     VOPT50                                                           
VOPT40   CLI   FSVALEN1,0          END DINK 1 UNUSED?                           
         BNE   VOPT50                                                           
         STC   RF,FSVALEN1         SET END DINK 1                               
         B     VOPT80                                                           
VOPT50   CLI   FSVALST2,0          START DINK 2 UNUSED?                         
         BNE   VOPT60                                                           
         STC   RF,FSVALST2         SET DINK 2                                   
         MVC   FSVALTP2,0(RE)      SAVE DINK TYPE                               
         B     VOPT80                                                           
VOPT60   CLC   FSVALTP2,0(RE)      MATCH ON DINK 2?                             
         BNE   VOPT80                                                           
         CLI   FSVALEN2,0          END DINK 2 UNUSED?                           
         BNE   VOPT80              EXTRA DINK                                   
         STC   RF,FSVALEN2                                                      
*                                                                               
VOPT80   LA    RF,1(RF)            BUMP TO NEXT INPUT CHARACTER                 
         BCT   R0,VOPT10                                                        
*                                                                               
         CLI   FSVALEN1,0          ENSURE COMPLETE PAIR OF DINKS                
         BNE   *+14                                                             
         XC    FSVALST1(L'FSVALST1+L'FSVALEN1+L'FSVALTP1),FSVALST1              
         B     VOPT90                                                           
         CLI   FSVALEN2,0          ENSURE COMPLETE PAIR OF DINKS                
         BNE   VOPT90                                                           
         XC    FSVALST2(L'FSVALST2+L'FSVALEN2+L'FSVALTP2),FSVALST2              
*                                                                               
VOPT90   SR    R0,R0                                                            
         IC    R0,FHIL             R0=L'(COMMAND LINE INPUT)                    
         SR    RF,RF                                                            
VOPT100  LA    RE,VOPTLN(RF)       RE=(NEXT INPUT CHAR)                         
         CLI   0(RE),C' '          BLANK                                        
         BH    VOPT120                                                          
         CLI   1(RE),C' '          FOLLOWED BY NON BLANK?                       
         BNH   VOPT120                                                          
         OC    FSVALST1(L'FSVALST1+L'FSVALEN1),FSVALST1                         
         BZ    VOPT110                                                          
         CLM   RF,1,FSVALST1       IF SURROUNDED BY DINKS IGNORE                
         BL    VOPT110                                                          
         CLM   RF,1,FSVALEN1                                                    
         BNH   VOPT120                                                          
         OC    FSVALST2(L'FSVALST2+L'FSVALEN2),FSVALST2                         
         BZ    VOPT110                                                          
         CLM   RF,1,FSVALST2                                                    
         BL    VOPT110                                                          
         CLM   RF,1,FSVALEN2                                                    
         BNH   VOPT120                                                          
VOPT110  MVI   0(RE),C','          REPLACE BLANK WITH COMMA                     
VOPT120  LA    RF,1(RF)            BUMP TO NEXT CHAR                            
         BCT   R0,VOPT100                                                       
         DROP  R2                                                               
*                                                                               
         MVC   FVMSGNO,=AL2(AE$FLDTL) INPUT FIELD TOO LONG                      
         LA    RF,VOPTLNH                                                       
         GOTO1 VSCANNER,BCDMCB,(0,(RF)),(0,WORKLST),C',=  '                     
         CLI   4(R1),0                                                          
         BE    VOPTERRX            INPUT IS TOO LONG                            
*                                                                               
         LA    R3,WORKLST          R3=A(SCAN BLOCK)                             
         USING SCANBLKD,R3                                                      
         CLI   4(R1),1             IF MORE THAN ONE ENTRY                       
         BE    *+8                                                              
         MVI   FVINDX,1            SET FIELD INDEX                              
         SR    R0,R0                                                            
         IC    R0,4(R1)                                                         
         STC   R0,CMNSTNO          R0=(NUMBER OF ENTRIES IN SCAN BLOCK)         
*                                                                               
         L     R1,ATRUP            GET TRANSLATE TABLE FOR LANG                 
         SR    RE,RE                                                            
         ICM   RE,1,SC1STLEN       RE=L'(INPUT COMMAND)                         
         BZ    VOPTERRX                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         TR    SC1STFLD(0),0(R1)   CONVERT TO UPPER CASE                        
         L     R2,ACMNDTAB         R2=A(COMMAND TABLE)                          
         USING CMNTABD,R2                                                       
         MVC   FVMSGNO,=AL2(AE$CMDNR) COMMAND NOT RECOGNISED                    
VOPT130  CLI   CMNMTYPE,EOT        END OF TABLE?                                
         BE    VOPTERRX                                                         
         SR    RF,RF                                                            
         ICM   RF,3,CMNWORD                                                     
         LA    RF,TWAD(RF)         RF=A(COMMAND WORD)                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SC1STFLD(0),0(RF)   MATCH ON COMMAND?                            
         BE    *+12                                                             
         LA    R2,CMNLNQ(R2)       BUMP TO NEXT COMMAND ENTRY                   
         B     VOPT130                                                          
*                                                                               
         MVC   ESCMND,CMNMTYPE     SAVE COMMAND TYPE                            
         TM    CMNSTAT,CMNSPREQ    COMMAND REQUIRES PARAMETETERS?               
         BNO   VOPT140                                                          
         MVC   FVMSGNO,=AL2(AE$TFPRM) TOO FEW PARAMETERS                        
         CLC   CMNSTNO,CMNMIN      ENSURE THAT MINIMUM NUMBER ENTERED           
         BL    VOPTERRX                                                         
         MVC   FVMSGNO,=AL2(AE$PARNR) PARAMETER NOT RECOGNISED                  
         B     VOPT150                                                          
VOPT140  MVC   FVMSGNO,=AL2(AE$PARNR) PARAMETER NOT RECOGNISED                  
         XR    RF,RF                                                            
         ICM   RF,3,CMNPTAB        PARAMETERS ALLOWED WITH COMMAND?             
         BNZ   VOPT150                                                          
         CLI   CMNSTNO,1           ENSURE NO PARAMETERS ENTERED                 
         BH    VOPTERRX                                                         
         CLI   SC2NDLEN,0                                                       
         BNE   VOPTERRX                                                         
         B     VOPT310                                                          
VOPT150  SR    RF,RF                                                            
         ICM   RF,3,CMNPFLAG                                                    
         LA    RF,ESAVE(RF)        RF=A(PARAMETER FLAG FOR COMMAND)             
         MVI   0(RF),0             INITIALISE FLAG                              
         MVI   CMNFFNO,0           INIT FREE FORM TEXT STRING COUNT             
         MVI   CMNNUMNO,0          INITIALISE NUMERICAL INPUT COUNT             
         CLI   ESCMND,CMNCHGQ      CHANGE COMMAND ENTERED?                      
         BE    *+12                                                             
         CLI   ESCMND,CMNFNDQ      FIND COMMAND ENTERED?                        
         BNE   *+8                                                              
         MVI   ESFCCOL,0           INITIALISE COLUMN PARAMETER                  
         CLI   SC2NDLEN,0          IF SECOND FIELD ENTERED VIA '='              
         BE    VOPT300             TREAT IT AS SEPERATE INPUT                   
         SR    RE,RE                                                            
         IC    RE,FVINDX                                                        
         LA    RE,1(RE)            BUMP FIELD INDEX                             
         STC   RE,FVINDX                                                        
         MVC   SC1STLEN,SC2NDLEN   OVERWRITE FIRST INPUT                        
         MVI   SC2NDLEN,0                                                       
         MVC   SC1STFLD,SC2NDFLD                                                
VOPT160  MVC   FVMSGNO,=AL2(AE$PARNR) PARAMETER NOT RECOGNISED                  
         CLI   SC2NDLEN,0                                                       
         BNE   VOPTERRX            PARAMETER NOT RECOGNISED                     
         L     R1,ATRUP            GET TRANSLATE TABLE FOR LANG                 
         SR    RE,RE                                                            
         IC    RE,SC1STLEN         RE=L'(PARAMETER)                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BCWORK(0),SC1STFLD  SAVE PARAMETER                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         TR    SC1STFLD(0),0(R1)   CONVERT PARAMETER TO UPPER CASE              
         SR    R4,R4                                                            
         ICM   R4,3,CMNPTAB                                                     
         LA    R4,CLB0A(R4)        R4=A(PARAMETER TABLE FOR COMMAND)            
         USING CPARTABD,R4                                                      
VOPT170  CLI   CPARTYPE,EOT        END OF PARAMETER TABLE?                      
         BNE   VOPT180             PARAMETER NOT RECOGNISED                     
         TM    VOPTFLAG,VOPTDUPQ   DUPLICATE PARAMETER?                         
         BNO   VOPTERRX                                                         
         MVC   FVMSGNO,=AL2(AE$DUPRM) DUPLICATE PARAMETER                       
         MVC   FVINDX,SVFVINDX                                                  
         B     VOPTERRX                                                         
VOPT180  TM    CPARSTAT,CPARSDFQ   DRAFT BILL ONLY PARAMETER?                   
         BZ    *+12                                                             
         CLI   ESPIND,ESPIDFT      TEST BILL IS DRAFT (AND NOT AUTOREV)         
         BNE   VOPT260                                                          
         TM    CPARSTAT,CPARSFFQ   FREE FORM TEXT PARAMETER?                    
         BNO   VOPT220                                                          
         CLC   CMNFFNO,CPARTYPE    ALREADY SET THIS PARAMETER?                  
         BNL   VOPT260                                                          
         MVC   CMNFFNO,CPARTYPE    UPDATE FREE FORM TEXT COUNT                  
         SR    RF,RF                                                            
         ICM   RF,3,CPARWORD                                                    
         LA    RF,ESAVE(RF)        RF=A(FREE FORM TEXT AREA)                    
         SR    RE,RE                                                            
         IC    RE,SC1STLEN                                                      
         SH    RE,=H'1'            RE=X L'(FREE FORM TEXT                       
         BNZ   VOPT190                                                          
         CLI   SC1STFLD,C'*'       USE PREVIOUS TEXT STRING?                    
         BNE   VOPT190                                                          
         MVC   FVMSGNO,=AL2(AE$ENTXT) ENTER TEXT STRING                         
         CLI   0(RF),0                                                          
         BE    VOPTERRX                                                         
         B     VOPT300                                                          
VOPT190  CLI   SC1STFLD,C''''      START WITH DINKS?                            
         BE    *+12                                                             
         CLI   SC1STFLD,C'"'                                                    
         BNE   VOPT200                                                          
         MVC   BCBYTE1,SC1STFLD    SAVE DINK                                    
         LA    R1,SC1STFLD(RE)                                                  
         CLC   BCBYTE1,0(R1)       IS LAST CHAR OF PARAM MATCHING DINK?         
         BNE   VOPT200                                                          
         LA    R1,SC1STFLD+1       R1=A(FREE FORM TEXT IN UPPER CASE)           
         CLI   CPARTYPE,CPARFFUQ   UPPER CASE TEXT TYPE?                        
         BE    *+8                                                              
         LA    R1,BCWORK+1         R1=A(FREE FORM TEXT IN ORIG CASE)            
         SH    RE,=H'2'            AND REDUCE LENGTH ACCORDINGLY                
         B     VOPT210                                                          
VOPT200  LA    R1,SC1STFLD         R1=A(FREE FORM TEXT)                         
         CLI   CPARTYPE,CPARFFUQ   UPPER CASE TEXT TYPE?                        
         BE    *+8                                                              
         LA    R1,BCWORK           R1=A(FREE FORM TEXT IN ORIG CASE)            
VOPT210  EX    RE,*+4                                                           
         MVC   1(0,RF),0(R1)       SAVE FREE FORM TEXT                          
         LA    RE,1(RE)                                                         
         STC   RE,0(RF)            SAVE LENGTH OF FREE FORM TEXT                
         B     VOPT300                                                          
VOPT220  TM    CPARSTAT,CPARSNMQ   NUMERIC PARAMETER?                           
         BNO   VOPT230                                                          
         TM    SC1STVAL,SCNUMQ     TEST INPUT IS NUMERIC                        
         BNO   VOPT260                                                          
         CLC   CMNNUMNO,CPARTYPE   ALREADY SET THIS PARAMETER?                  
         BNL   VOPT260                                                          
         MVC   CMNNUMNO,CPARTYPE   UPDATE NUMERICAL INPUT COUNT                 
         SR    RF,RF                                                            
         ICM   RF,3,CPARWORD                                                    
         LA    RF,ESAVE(RF)        RF=A(NUMERICAL INPUT SAVE AREA)              
         SR    RE,RE                                                            
         IC    RE,SC1STLEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  BCDUB,SC1STFLD(0)                                                
         CVB   RE,BCDUB                                                         
         STC   RE,0(RF)            SAVE NUMERICAL INPUT                         
         B     VOPT300                                                          
VOPT230  SR    RF,RF                                                            
         ICM   RF,3,CPARWORD                                                    
         LA    RF,TWAD(RF)         RF=A(PARAMETER WORD)                         
         CLI   ESCMND,CMNCHGQ      CHANGE COMMAND?                              
         BE    *+12                                                             
         CLI   ESCMND,CMNFNDQ      FIND COMMAND?                                
         BNE   VOPT240                                                          
         LA    RE,7                SET RE TO FILL LENGTH OF PARAM WORD          
         LA    R1,0(RE,RF)                                                      
         CLI   0(RE),C' '                                                       
         BH    VOPT250                                                          
         BCT   RE,*-12                                                          
         DC    H'0'                                                             
VOPT240  SR    RE,RE                                                            
         ICM   RE,1,SC1STLEN       RE=L'(INPUT PARAMETER)                       
         BZ    VOPTERRX                                                         
         BCTR  RE,0                                                             
VOPT250  EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SC1STFLD(0),0(RF)   MATCH FOUND ON PARAMETER?                    
         BE    *+12                                                             
VOPT260  LA    R4,CPARLNQ(R4)      BUMP TO NEXT PARAMETER ENTRY                 
         B     VOPT170                                                          
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,CMNPFLAG                                                    
         LA    RF,ESAVE(RF)        RF=A(PARAMETER FLAG)                         
         LA    RE,CPARINCM         RE=A(LIST OF INCOMPATIBLE PARAMS)            
VOPT270  MVC   BCBYTE1,0(RF)                                                    
         CLI   0(RE),EOT           END OF LIST?                                 
         BE    VOPT280                                                          
         MVC   FVMSGNO,=AL2(AE$INPAR) INCOMPATIBLE PARAMETERS                   
         NC    BCBYTE1,0(RE)                                                    
         BNZ   VOPTERRX                                                         
         LA    RE,1(RE)            BUMP TO NEXT ENTRY                           
         B     VOPT270                                                          
VOPT280  NC    BCBYTE1,CPARTYPE                                                 
         BZ    VOPT290             TEST FOR DUPLICATE                           
         OI    VOPTFLAG,VOPTDUPQ   SET POSSIBLE DUPLICATE FLAG ON               
         MVC   SVFVINDX,FVINDX                                                  
         B     VOPT260                                                          
VOPT290  OC    0(L'CMNPFLAG,RF),CPARTYPE SET PARAMETER TYPE                     
*                                                                               
VOPT300  SR    RF,RF               BUMP UP FIELD INDEX                          
         IC    RF,FVINDX                                                        
         LA    RF,1(RF)                                                         
         STC   RF,FVINDX                                                        
         LA    R3,SCBLKLQ(R3)      BUMP TO NEXT ENTRY IN SCAN BLOCK             
         BCT   R0,VOPT160                                                       
         DROP  R2,R3,R4                                                         
*                                                                               
VOPT310  LA    R2,BASOPTH          R2=A(COMMAND LINE FIELD HEADER)              
         USING FHD,R2                                                           
         MVC   FHDA(L'BASOPT),SPACES                                            
         OI    FHOI,FHOITR         TRANSMIT FIELD                               
         OI    FHII,FHIIVA         SET VALIDATED THIS TIME ON                   
         MVI   FVINDX,0                                                         
         DROP  R2                                                               
*                                                                               
VOPTX    B     EXITY                                                            
*                                                                               
VOPTERRX B     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* DEAL WITH BOTH VERTICAL AND HORIZONTAL SCROLLING                    *         
***********************************************************************         
         SPACE 1                                                                
SCROLL   DS    0H                                                               
         USING *,R8                                                             
         TM    BCINDS1,BCIANYPF    HAS A PFKEY BEEN PRESSED?                    
         BO    SCRL20                                                           
         TM    ESINDS,ESIDEND      END OF PARA LINE ON SCREEN?                  
         BO    SCRLX                                                            
         SR    R3,R3                                                            
         IC    R3,ESSTLIN          R3=(NUMBER OF START LINE ON SCREEN)          
         XR    RF,RF                                                            
         IC    RF,ESNUMLIN                                                      
         AR    RF,R3                                                            
         BCTR  RF,0                RF=(NUMBER OF LAST LINE ON SCREEN)           
         CLM   RF,1,ESEINSB        INSERT LINE?                                 
         BE    SCRL10                                                           
         CLM   RF,1,ESLACTV        END OF PARA BEFORE LAST SCREEN LINE?         
         BH    SCRLX                                                            
         L     RE,AINP             RE=A(TIOB)                                   
         SR    R1,R1                                                            
         ICM   R1,3,TIOBCURS-TIOBD(RE) R1=(ABSOLUTE CURSOR ADDRESS)             
         CH    R1,=Y(COLS#Q*(ROWS#Q-2)) CURSOR ON LAST TEXT LINE?               
         BL    SCRLX                                                            
         CH    R1,=Y(COLS#Q*(ROWS#Q-1))                                         
         BNL   SCRLX                                                            
*                                                                               
SCRL10   LA    R3,1(R3)            SCROLL DOWN BY ONE LINE                      
         STCM  R3,1,ESSTLIN                                                     
         B     SCRLX                                                            
*                                                                               
SCRL20   CLI   BCPFKEY,PFKBKWDQ    BACKWARDS?                                   
         BE    SCRL50                                                           
         CLI   BCPFKEY,PFKFRWDQ    FORWARDS?                                    
         BE    SCRL50                                                           
         CLI   BCPFKEY,PFKLEFTQ    LEFT?                                        
         BE    SCRL50                                                           
         CLI   BCPFKEY,PFKRGHTQ    RIGHT?                                       
         BE    SCRL50                                                           
*                                                                               
         CLI   BCPFKEY,PFKNEXTP    NEXT PARAGRAPH?                              
         BNE   SCRL30                                                           
         TM    ESEBFLG,BLKCOUTQ    NOT ALLOWED IF OUTSTANDING BLOCK CMD         
         BO    SCRLX                                                            
         CLC   ESPOFST,ESPACTV     LAST PARAGRAPH?                              
         BE    SCRLX                                                            
         SR    RF,RF                                                            
         IC    RF,ESPOFST                                                       
         LA    RF,1(RF)            BUMP TO NEXT PARAGRAPH                       
         B     SCRL40                                                           
*                                                                               
SCRL30   CLI   BCPFKEY,PFKPREVP    PREVIOUS PARAGRAPH?                          
         BNE   SCRLX                                                            
         TM    ESEBFLG,BLKCOUTQ    NOT ALLOWED IF OUTSTANDING BLOCK CMD         
         BO    SCRLX                                                            
         CLI   ESPOFST,1           FIRST PARAGRAPH?                             
         BE    SCRLX                                                            
         SR    RF,RF                                                            
         IC    RF,ESPOFST                                                       
         BCTR  RF,0                BUMP TO PREVIOUS PARAGRAPH                   
SCRL40   STC   RF,ESPOFST                                                       
         BCTR  RF,0                                                             
         LA    RF,ESPLST(RF)                                                    
         MVC   ESPARA,0(RF)        GET PARAGRAPH INDEX                          
         LA    R3,1                                                             
         LA    R1,ESPOFST          R1=A(PARAGRAPH# REQUIRED)                    
         GOTO1 AGPARA                                                           
         BE    SCRL160                                                          
         DC    H'0'                                                             
*                                                                               
SCRL50   LA    RF,MAXSLINQ         RF=(MAX NUM OF TEXT LINES ON SCREEN)         
         TM    BCSCROLL,PFKIHORZ   HORIZONTAL SCROLLING?                        
         BNO   *+8                                                              
         LA    RF,MAXSCOLQ         RF=(NUMBER OF TEXT COLS ON SCREEN)           
         TM    BCSCRNUM,PFKIPAGE   PAGE?                                        
         BO    SCRL80                                                           
         SRL   RF,1                                                             
         TM    BCSCRNUM,PFKIHALF   HALF?                                        
         BO    SCRL80                                                           
         TM    BCSCRNUM,PFKIMAXN   MAX?                                         
         BNO   SCRL70                                                           
         LA    R3,1                R3=(FIRST LINE/PARAGRAPH)                    
         TM    BCSCROLL,PFKIHORZ   HORIZONTAL SCROLLING?                        
         BO    SCRL60                                                           
         TM    BCSCROLL,PFKIUPDN   UP TO TOP OF TEXT?                           
         BO    SCRL160                                                          
         B     SCRL140                                                          
SCRL60   TM    BCSCROLL,PFKIUPDN   UP TO LEFT MOST COLUMN?                      
         BO    SCRL200                                                          
         B     SCRL170                                                          
*                                                                               
SCRL70   SR    RF,RF                                                            
         ICM   RF,1,BCSCRNUM       SCROLL AMOUNT?                               
         BZ    SCRLX               PFKEY NOT A SCROLL PFKEY                     
*                                                                               
SCRL80   SR    R3,R3                                                            
         TM    BCSCROLL,PFKIHORZ   HORIZONTAL SCROLLING?                        
         BO    SCRL120                                                          
         IC    R3,ESSTLIN          R3=(START LINE NUM IN CURRENT DISP)          
         TM    BCSCROLL,PFKIUPDN   UP?                                          
         BO    SCRL110                                                          
         AR    R3,RF               R3=(NEXT START LINE NUMBER)                  
         CLC   ESPOFST,ESPACTV     LAST PARAGRAPH?                              
         BNE   SCRL90                                                           
         LA    RF,MAXSLINQ-1(R3)                                                
         CLM   RF,1,ESLACTV                                                     
         BH    SCRL100                                                          
         B     SCRL160                                                          
*                                                                               
SCRL90   CLM   R3,1,ESLACTV        BEYOND THE LAST ACTIVE LINE?                 
         BNH   SCRL160                                                          
         TM    ESEBFLG,BLKCOUTQ                                                 
         BO    SCRLX                                                            
         SR    RF,RF                                                            
         IC    RF,ESPOFST                                                       
         LA    RF,1(RF)            BUMP TO NEXT PARAGRAPH                       
         STC   RF,ESPOFST                                                       
         BCTR  RF,0                                                             
         LA    RF,ESPLST(RF)       RF=A(NEXT PARAGRAPH INDEX)                   
         MVC   ESPARA,0(RF)                                                     
         LA    R3,1                                                             
         LA    R1,ESPOFST          R1=A(PARAGRAPH# REQUIRED)                    
         GOTO1 AGPARA                                                           
         BE    SCRL160                                                          
         DC    H'0'                                                             
*                                                                               
SCRL100  LR    R1,R3                                                            
         LA    R1,MAXSLINQ(R1)     R1=(NEXT LAST LINE NUMBER)                   
         CLM   R1,1,ESLACTV        BEYOND THE LAST ACTIVE LINE?                 
         BH    SCRL140                                                          
         B     SCRL160                                                          
*                                                                               
SCRL110  CH    R3,=H'1'                                                         
         BE    *+14                                                             
         SR    R3,RF               R3=(NEXT START LINE NUMBER)                  
         BP    SCRL160                                                          
         B     SCRL150                                                          
*                                                                               
         CLI   ESPOFST,1           FIRST PARAGRAPH?                             
         BE    SCRL150                                                          
         TM    ESEBFLG,BLKCOUTQ                                                 
         BO    SCRL150                                                          
         SR    RF,RF                                                            
         IC    RF,ESPOFST                                                       
         BCTR  RF,0                                                             
         STC   RF,ESPOFST                                                       
         BCTR  RF,0                                                             
         LA    RF,ESPLST(RF)                                                    
         MVC   ESPARA,0(RF)                                                     
         LA    R1,ESPOFST          R1=A(PARAGRAPH# REQUIRED)                    
         GOTO1 AGPARA                                                           
         SR    R3,R3                                                            
         IC    R3,ESLACTV                                                       
         SH    R3,=Y(MAXSLINQ-2)                                                
         BNP   SCRL150                                                          
         B     SCRL160                                                          
*                                                                               
SCRL120  IC    R3,ESCOL#1          R3=(1ST COLUMN NUM IN CURRENT DISP)          
         TM    BCSCROLL,PFKIUPDN   UP?                                          
         BO    SCRL130                                                          
         AR    R3,RF               R3=(NEXT LEFTMOST COLUMN)                    
         CLM   R3,1,ESREPWD        BEYOND THE LAST POSSIBLE COLUMN?             
         BNH   *+8                                                              
         IC    R3,ESREPWD                                                       
         B     SCRL200                                                          
*                                                                               
SCRL130  SR    R3,RF               R3=(NEXT LEFTMOST COLUMN)                    
         B     SCRL180                                                          
*                                                                               
SCRL140  SR    R3,R3                                                            
         IC    R3,ESLACTV          NUMBER OF LINES IN PARAGRAPH                 
         SH    R3,=Y(MAXSLINQ-2)                                                
         LTR   R3,R3               BEYOND START OF DISPLAY?                     
         BP    *+8                                                              
SCRL150  LA    R3,1                                                             
SCRL160  STC   R3,ESSTLIN          SAVE NEXT START LINE NUMBER                  
         B     SCRL300                                                          
*                                                                               
SCRL170  SR    R3,R3                                                            
         ICM   R3,1,ESREPWD                                                     
         SH    R3,=Y(MAXSCOLQ-1)                                                
         BP    *+8                                                              
         LA    R3,1                                                             
         B     SCRL200                                                          
SCRL180  LTR   R3,R3               BEYOND START OF DISPLAY?                     
         BP    SCRL200                                                          
         LA    R3,1                                                             
*                                                                               
SCRL200  STC   R3,ESCOL#1          SAVE NEXT START COLUMN NUMBER                
         TM    BCSCRNUM,PFKIPAGE+PFKIHALF TEST SCROLL BY HALF/PAGE              
         BZ    SCRL300                                                          
         CLI   BCPFKEY,PFKRGHTQ    TEST SCROLLING RIGHT                         
         BNE   SCRL250                                                          
         LA    R2,ESCOLLST         FIND FIELD START BEFORE NEW LEFT             
         LA    RF,1                                                             
SCRL202  CLI   0(R2),0                                                          
         BE    SCRL210                                                          
         CLC   ESCOL#1,0(R2)                                                    
         BL    SCRL210                                                          
         IC    RF,0(R2)                                                         
         LA    R2,1(R2)                                                         
         B     SCRL202                                                          
SCRL210  XR    RE,RE                                                            
         IC    RE,ESCOL#1                                                       
         SR    RE,RF                                                            
         CH    RE,=Y(MAXSCOLQ/2-1) TEST WITHIN LAST HALF PAGE                   
         BH    *+8                                                              
         STC   RF,ESCOL#1                                                       
         B     SCRL300                                                          
*                                                                               
SCRL250  CLI   BCPFKEY,PFKLEFTQ    TEST SCROLLING LEFT                          
         BNE   SCRL300                                                          
         LA    R2,ESCOLLST         FIND FIELD START AFTER NEW LEFT              
SCRL252  CLI   0(R2),0                                                          
         BE    SCRL300                                                          
         CLC   ESCOL#1,0(R2)                                                    
         BNH   SCRL260                                                          
         LA    R2,1(R2)                                                         
         B     SCRL252                                                          
SCRL260  XR    RF,RF                                                            
         IC    RF,0(R2)                                                         
         XR    RE,RE                                                            
         IC    RE,ESCOL#1                                                       
         SR    RF,RE                                                            
         CH    RF,=Y(MAXSCOLQ/2-1) TEST WITHIN LAST HALF PAGE                   
         BH    *+10                                                             
         MVC   ESCOL#1,0(R2)                                                    
*                                                                               
SCRL300  L     RE,ESATXT1                                                       
         S     RE,ATWA                                                          
         L     RF,AINP             RF=A(TIOB)                                   
         USING TIOBD,RF                                                         
         MVI   TIOBCURI,0          OVERRIDE DEFAULT CURSOR CONTROL              
         STCM  RE,3,TIOBCURD                                                    
         DROP  RF                                                               
SCRLX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CHANGE/ADD A LINE RECORD                                            *         
* NTRY- FOLLOWING PARAMTERS MUST BE SET                               *         
* R1 = A(NEW TXD ENTRY) OR 0 TO USE BLANK                             *         
*                                                                     *         
* LRECPARM DS    0X                COPY/MOVE ROUTINE PARMS            *         
* LRMODE   DS    X                                                    *         
* LRADDQ   EQU   X'80'             ADD A RECORD (OR CHANGE UNACTIVE)  *         
* LRCHANGE EQU   X'40'             CHANGE AN EXISTING RECORD (ACTIVE) *         
* LROVRWRT EQU   X'20'             OVER WRITE LINE (ACTIVE)           *         
* LRMERGE  EQU   X'10'             MERGE LINE, IN WHICH CASE          *         
* LRLINE   DS    X                 LOGICAL LINE NUMBER                *         
* LRPARA   DS    X                 PARAGRAPH OF LINE                  *         
* LRATX    DS    A                 A(TXD TO BE ADDED/CHANGED)         *         
* LRCOLF   DS    X                 STANDART COMMENT COLUMN START      *         
***********************************************************************         
         SPACE 1                                                                
LINEREC  DS    0H                                                               
         USING *,R8                                                             
         LTR   R7,R1                                                            
         BNZ   LINER02                                                          
N        USING TXD,R7              R7=A(NEW TXD)                                
         GOTO1 MAKETX,R7                                                        
         GOTO1 AINITX,BOPARM,('PBRLNUM',N.TXD)                                  
*                                                                               
LINER02  GOTO1 MAKETX,R6                                                        
O        USING TXD,R6              R6=A(TXD FOR EXISTING RECORD)                
*                                                                               
         MVI   RECMODE,RECACTQ     SET ACTIVE RECORD MODE                       
         CLI   LRMODE,LRADDQ       NEW LINE                                     
         BE    LINER10                                                          
         CLI   LRMODE,LRMERGE      ACTIVE LINE RECORD TO BE MERGED?             
         BE    LINER20                                                          
         CLI   LRMODE,LRCHANGE     ACTIVE LINE RECORD TO BE CHANGED?            
         BE    LINER20                                                          
         CLI   LRMODE,LROVRWRT     ACTIVE LINE REC TO BE OVER-WRITTEN?          
         BE    LINER20                                                          
         DC    H'0'                                                             
*                                                                               
LINER10  SR    RF,RF                                                            
         IC    RF,ESLACTV          RF=(NUMBER OF ACTIVE LINES IN PARA)          
         LA    RF,1(RF)            INCREMENT                                    
         CLC   ESLHIGH,ESLACTV     ARE ALL EXISTING LINE RECS ACTIVE?           
         STC   RF,ESLACTV          UPDATE ACTIVE LINE NUMBER                    
         BE    *+12                                                             
         MVI   RECMODE,RECUACTQ    UNACTIVE LINE RECORD TO BE USED              
         B     LINER12                                                          
         STC   RF,ESLHIGH          UPDATE NUMBER OF LINE RECORDS                
         LA    RE,ESLLST(RF)                                                    
         BCTR  RE,0                RE=A(CORRESPONDING INDEX FOR LINE)           
         STC   RF,0(RE)            ATTATCH AT END OF LIST                       
         MVI   RECMODE,RECADDQ     LINE RECORD REQUIRES ADDING                  
LINER12  MVI   CMPMODE,CMPMOVEQ    MOVE COMMAND                                 
         OI    CMPMODE,CMPBEFRQ    BEFORE OFFSET                                
         MVC   CMPTO,LRLINE        OFFSET FOR LINE NUMBER                       
         MVC   CMPFROM1,ESLACTV    START OF BLOCK                               
         MVC   CMPFROM2,ESLACTV    END OF BLOCK                                 
         LA    RF,ESLLST                                                        
         STCM  RF,15,CMPAILST      RF=A(LINE INDEX LIST)                        
         LA    RF,L'ESLLST                                                      
         STCM  RF,1,CMPILLN        RF=L'(LINE INDEX LIST)                       
         GOTO1 ACMI                MOVE NEW LINE TO CORRECT POSITION            
         OI    LINEFLAG,LCHANGEQ   INDICATE LINE INDEX LIST CHANGED             
*                                                                               
LINER20  LA    RF,IOKEY            RF=A(KEY FOR PRODUCTION LINE RECORD)         
         USING PBRRECD,RF                                                       
         CLI   RECMODE,RECADDQ     ADDING A NEW RECORD                          
         BNE   *+8                                                              
         L     RF,AIO1             RF=A(IOAREA FOR NEW LINE RECORD)             
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ    RECORD TYPE                                  
         MVC   PBRKCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRKSUB,PBRKACTQ    ACTIVE RECORD                                
         MVC   PBRKJOB,ESJOB       JOB                                          
         MVC   PBRKSEQ,ESSEQ       SEQUENCE# (COMPLEMENT)                       
         MVC   PBRKPARA,LRPARA     PARAGRAPH#                                   
         SR    RE,RE                                                            
         IC    RE,LRLINE           OFFSET TO LINE INDEX                         
         BCTR  RE,0                                                             
         LA    RE,ESLLST(RE)       RE=A(CORRESPONDING INDEX FOR LINE)           
         MVC   PBRKLINE,0(RE)      LINE#                                        
         CLI   PBRKLINE,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         DROP  RF                                                               
         MVI   BOELEM,C' '                                                      
         MVC   BOELEM+1(L'BOELEM-1),BOELEM INIT ELEMENT STORAGE                 
         CLI   RECMODE,RECADDQ     ADDING A RECORD?                             
         BE    LINER30                                                          
         GOTO1 AIO,IO1+IOACCMST+IORDUP GET TEXT LINE RECORD                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGETTX,BOPARM,O.TXD,AIO1,0                                       
*                                                                               
LINER30  CLI   LRMODE,LROVRWRT     TEST OVER                                    
         BNE   LINER40                                                          
         LA    R3,O.TXLINLAY                                                    
         USING LAYOUTD,R3                                                       
LINER32  CLI   LAYOUTD,EOT                                                      
         BE    LINER38                                                          
         CLI   LAYFLD,0            ENSURE NON-NUMERICAL FIELD                   
         BNE   LINER36                                                          
         XR    RE,RE                                                            
         IC    RE,LAYCOLN                                                       
         LA    RF,N.TXDATA-1(RE)   RF=A(N.TEXT)                                 
         LA    RE,O.TXDATA-1(RE)   RE=A(O.TEXT)                                 
         XR    R0,R0                                                            
         IC    R0,LAYCOLF          R0=WIDTH                                     
*                                                                               
LINER34  CLI   0(RE),C' '                                                       
         BNE   *+0                                                              
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,LINER34                                                       
*                                                                               
LINER36  LA    R3,LAYOUTL(R3)                                                   
         B     LINER32                                                          
*                                                                               
LINER38  GOTO1 MVCTX,BOPARM,N.TXD,O.TXD                                         
         B     LINER50                                                          
*                                                                               
LINER40  CLI   LRMODE,LRMERGE      TEST MERGE ACTIVE LINE                       
         BNE   LINER50                                                          
         XR    RF,RF                                                            
         IC    RF,LRCOLF                                                        
         LA    R1,O.TXDATA-1(RF)                                                
         LA    R2,N.TXDATA-1(RF)                                                
         LA    RE,L'TXDATA                                                      
         SR    RE,RF                                                            
         EX    RE,*+4                                                           
         MVC   0(0,R1),0(R2)                                                    
         GOTO1 MVCTX,BOPARM,N.TXD,O.TXD                                         
         LA    RE,N.TXAMT                                                       
         LA    R0,TXAMTN                                                        
LINER42  CP    0(L'TXAMT,RE),BCPZERO                                            
         BNE   LINER44                                                          
         LA    RE,L'TXAMT(RE)                                                   
         BCT   R0,LINER42                                                       
         B     LINER50                                                          
LINER44  GOTO1 AFMTTX,BOPARM,N.TXD IN CASE COMMENT OVERRIDES NUMBERS            
*                                                                               
LINER50  L     R2,AIO1                                                          
         USING PBRRECD,R2                                                       
         MVC   BODUB1,PBRRSTA      SAVE INITIAL RECORD STATUS                   
         GOTO1 APUTTX,BOPARM,N.TXD,PBRRECD                                      
         CLI   RECMODE,RECADDQ     ADDING RECORD?                               
         BNE   LINER52                                                          
         GOTO1 AIO,IO1+IOACCMST+IOADDREC                                        
         BE    LINERX                                                           
         DC    H'0'                                                             
*                                                                               
LINER52  GOTO1 AIO,IO1+IOACCMST+IOPUTREC                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    N.TXLSTA,PBRLNUM    TEST  NUMERICAL LINE                         
         BZ    LINER54                                                          
         GOTO1 ACLCTOT,BOPARM,O.TXD,N.TXD                                       
         BE    LINER54                                                          
         OI    EWINDS,EWITOT       SET UPDATE PARAGRPAH TOTALS                  
*                                                                               
LINER54  CLC   PBRRSTA,BODUB1      TEST CHANGE IN STATUS AREA                   
         BE    LINERX                                                           
K        USING PBRRECD,IOKEY                                                    
         MVC   K.PBRKEY,PBRKEY                                                  
         GOTO1 AIO,IORDUP+IOACCDIR                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   K.PBRKSTA,PBRRSTA                                                
         GOTO1 AIO,IOWRITE+IOACCDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  K,R2                                                             
*                                                                               
LINERX   B     EXIT                                                             
         SPACE 1                                                                
         DROP  N,O                                                              
         EJECT                                                                  
***********************************************************************         
* CHANGE/ADD A PARAGRAPH RECORD                                       *         
* NTRY- FOLLOWING PARAMTERS MUST BE SET                               *         
* PRECPARM DS    0X                COPY/MOVE ROUTINE PARMS            *         
* PRMODE   DS    X                                                    *         
* PRADDQ   EQU   X'80'             ADD A RECORD (OR CHANGE UNACTIVE)  *         
* PRCHANGQ EQU   X'40'             CHANGE AN EXISTING RECORD (ACTIVE) *         
***********************************************************************         
         SPACE 1                                                                
PARAREC  DS    0H                                                               
         USING *,R8                                                             
         MVI   PARAFLAG,0          INIT PARAGRAPH FLAG                          
         MVI   RECMODE,RECACTQ     SET ACTIVE RECORD MODE                       
*                                                                               
         CLI   PRMODE,PRADDQ       TEST ADD RECORD                              
         BE    PARAR02                                                          
         CLI   PRMODE,PRCHANGQ     TEST CHANGE RECORD                           
         BE    PARAR10                                                          
         DC    H'0'                                                             
*                                                                               
PARAR02  CLI   ESPACTV,MAXPLINQ    MAXIMUM NUMBER OF PARAGRAPHS?                
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$MBILL) MAX BILL SIZE IS 200 PARAGRAPHS           
         B     PARARERX                                                         
         XR    RF,RF                                                            
         IC    RF,ESPACTV          RF=(NUMBER OF ACTIVE PARA RECORDS)           
         LA    RF,1(RF)            INCREMENT COUNT                              
         STC   RF,ESPOFST          UPDATE CURRENT PARA NUMBER/OFFSET            
         CLC   ESPHIGH,ESPACTV     HIGHEST PARA INDEX IS ACTIVE?                
         STC   RF,ESPACTV          UPDATE ACTIVE PARA COUNT                     
         BE    PARAR04                                                          
         MVI   RECMODE,RECUACTQ    UNACTIVE PARA RECORD TO BE USED              
         LA    RE,ESPLST-1(RF)     RE=A(CORRESPONDING INDEX FOR PARA)           
         MVC   ESPARA,0(RE)                                                     
         B     PARAR06                                                          
PARAR04  STC   RF,ESPHIGH          UPDATE NUMBER OF PARA RECORDS COUNT          
         LA    RE,ESPLST(RF)                                                    
         BCTR  RE,0                RE=A(CORRESPONDING INDEX FOR PARA)           
         STC   RF,0(RE)            ATTATCH AT END OF LIST                       
         MVI   RECMODE,RECADDQ     PARA RECORD REQUIRES ADDING                  
         STC   RF,ESPARA                                                        
PARAR06  DS    0H                                                               
         ZAP   ESPARNET,BCPZERO                                                 
         ZAP   ESPARCOM,BCPZERO                                                 
         ZAP   ESPAROVR,BCPZERO                                                 
         MVI   ESPARTAX,0          ??                                           
         MVI   ESPARTY2,0          ??                                           
*        MVC   EDTDESC,BCSPACES    RESET DESCRIPTION TO SPACES                  
*        OI    EDTDESCH+FHOID,FHOITR                                            
         CLI   ESSECT,0            TEST PARAGRAPH TEST ALREADY SET              
         BNE   PARAR20                                                          
         MVI   BOBYTE1,1           NO - SET TO 1                                
         GOTO1 ATYPSET,BOPARM,BOBYTE1                                           
         B     PARAR20                                                          
*                                                                               
PARAR10  TM    LINEFLAG,LCHANGEQ   LINE INDEX LIST CHANGED?                     
         BO    PARAR12                                                          
         TM    EDTTYPEH+FHIID,FHIIVA TYPE INPUT                                 
         BZ    PARAR12                                                          
         TM    EDTDESCH+FHIID,FHIIVA DESCRIPTION CHANGED                        
         BZ    PARAR12                                                          
         B     EXITY               NO CHANGES TO MAKE                           
PARAR12  NI    LINEFLAG,FF-LCHANGEQ                                             
         GOTO1 ATYPVAL             VALIDATE TYPE                                
         BNE   EXITN                                                            
*                                                                               
PARAR20  L     R2,AIO1             R2=A(IOAREA FOR RECORD)                      
         USING PBRRECD,R2                                                       
         XC    PBRRECD(PBRRFST-PBRRECD),PBRRECD                                 
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ    RECORD TYPE                                  
         MVC   PBRKCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRKSUB,PBRKACTQ    ACTIVE RECORD                                
         MVC   PBRKJOB,ESJOB       JOB                                          
         MVC   PBRKSEQ,ESSEQ       SEQUENCE# (COMPLEMENT)                       
         MVC   PBRKPARA,ESPARA     PARAGRAPH#                                   
*                                                                               
         CLI   RECMODE,RECADDQ     ADDING A RECORD?                             
         BNE   PARAR22                                                          
         MVC   PBRRLEN,=AL2(PBRRFST-PBRRECD+1)                                  
         MVI   PBRRFST,0           INITIALIZE RECORD TO BE EMPTY                
         B     PARAR30                                                          
*                                                                               
PARAR22  MVC   IOKEY(L'PBRKEY),PBRKEY GET PARAGRAPH RECORD                      
         GOTO1 AIO,IOREAD+IOACCDIR+IO1                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGET+IOACCMST+IO1+IOLOCK                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PARAR30  DS    0H                  PGHELD                                       
         LA    R3,PBRRFST                                                       
         USING PGHELD,R3                                                        
         XR    RF,RF                                                            
PARAR32  CLI   PGHEL,0             FIND OR ADD NEW PGHELD                       
         BE    PARAR34                                                          
         CLI   PGHEL,PGHELQ                                                     
         BE    PARAR36                                                          
         IC    RF,PGHLN                                                         
         BXH   R3,RF,PARAR32                                                    
PARAR34  LA    R3,BOELEM                                                        
         XC    PGHELD(PGHLNQ),PGHELD                                            
         MVI   PGHEL,PGHELQ                                                     
         MVI   PGHLN,PGHLNQ                                                     
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),PBRRECD,PGHELD,ADDCODE               
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,16(R1)                                                        
*                                                                               
PARAR36  ZAP   PGHNET,ESPARNET                                                  
         ZAP   PGHCOM,ESPARCOM                                                  
         ZAP   PGHOALLC,ESPAROVR                                                
         MVC   PGHTAX,ESPARTAX                                                  
         MVC   PGHHTYP,ESSECT                                                   
         CLI   ESSECT,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   PGHHTYP2,ESPARTY2                                                
         DROP  R3                                                               
*                                                                               
PARAR40  DS    0H                  NDXELD                                       
         LA    R3,PBRRFST                                                       
         USING NDXELD,R3                                                        
         XR    RF,RF                                                            
PARAR42  CLI   NDXEL,0             FIND OR ADD NEW NDXELD                       
         BE    PARAR46                                                          
         CLI   NDXEL,NDXELQ                                                     
         BNE   PARAR44                                                          
         CLI   NDXLN,NDXLNQ                                                     
         BE    PARAR50                                                          
         DC    H'0'                                                             
PARAR44  IC    RF,NDXLN                                                         
         BXH   R3,RF,PARAR42                                                    
PARAR46  LA    R3,BOELEM                                                        
         XC    NDXELD(NDXLNQ),NDXELD                                            
         MVI   NDXEL,NDXELQ                                                     
         MVI   NDXLN,NDXLNQ                                                     
         XC    ESLVALS(ESLLNQ),ESLVALS   RESET SAVED VALUES                     
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),PBRRECD,NDXELD,ADDCODE               
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PARAR59                                                          
*                                                                               
PARAR50  CLI   PRMODE,PRADDQ       ACTIVE RECORD TO BE RE-USED?                 
         BNE   PARAR52                                                          
         MVI   NDXACTV,0                                                        
         MVC   ESLHIGH,NDXHIGH                                                  
         MVI   ESLACTV,0                                                        
         MVC   ESLLST,NDXINDX                                                   
         B     PARAR59                                                          
*                                                                               
PARAR52  CLC   ESLHIGH,NDXHIGH     TEST ANY CHANGES TO NDXELD                   
         BNE   PARAR54                                                          
         CLC   ESLACTV,NDXACTV                                                  
         BNE   PARAR54                                                          
         CLC   ESLLST,NDXINDX                                                   
         BNE   PARAR54                                                          
         B     PARAR59                                                          
*                                                                               
PARAR54  CLI   PRMODE,PRCHANGQ     MUST BE CHANGING                             
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    EWINDS,EWITOT       SET UPDATE TOTALS                            
         MVC   NDXHIGH,ESLHIGH                                                  
         MVC   NDXACTV,ESLACTV                                                  
         MVC   NDXINDX(L'ESLLST),ESLLST                                         
*                                                                               
PARAR59  DS    0H                                                               
         DROP  R3                                                               
*                                                                               
PARAR60  DS    0H                  FFTELD                                       
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('FFTELQ',PBRRECD),0,0               
         CLI   12(R1),0            DELETE EXISTING FFTELD                       
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    BOELEM,BOELEM                                                    
         PUSH  USING                                                            
         USING FFTELD,BOELEM                                                    
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTTYPE,FFTTPGHC                                                 
         GOTO1 AFVAL,EDTDESCH                                                   
         MVC   FFTDATA(L'EDTDESC),FVIFLD                                        
         MVI   FFTDLEN,L'EDTDESC                                                
         MVI   FFTLN,L'EDTDESC+FFTDATA-FFTELD                                   
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),PBRRECD,FFTELD,ADDCODE               
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         POP   USING                                                            
*                                                                               
         LA    R1,IO1+IOACCMST+IOPUTREC                                         
         CLI   RECMODE,RECADDQ                                                  
         BNE   *+8                                                              
         LA    R1,IO1+IOACCMST+IOADDREC                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   PRMODE,PRADDQ       ACTIVE LINE RECORD TO BE RE-USED?            
         BNE   PARAR70                                                          
         LA    RF,ESEINSB          RF=A(INSERT BLOCK VALUES LIST)               
         USING BLKVALD,RF                                                       
         MVI   BLKSTRT,0           SET TO INSERT WHOLE SCREEN                   
         MVI   BLKEND,MAXSLINQ                                                  
         MVI   BLKLEN,MAXSLINQ                                                  
         MVI   BLKINDS,BLKINUM                                                  
         DROP  RF                                                               
         MVI   ESSTLIN,1           INIT START LINE                              
         MVI   ESCOL#1,1           INIT START COLUMN                            
         B     PARARX                                                           
*                                                                               
PARAR70  TM    PARAFLAG,PERRORQ    ERROR FLAG ON?                               
         BO    PARARERX                                                         
*                                                                               
PARARX   B     EXITY                                                            
*                                                                               
PARARERX B     EXITN                                                            
         SPACE 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* CHANGE A BILL RECORD                                                *         
***********************************************************************         
         SPACE 1                                                                
BILLREC  DS    0H                                                               
         USING *,R8                                                             
         LA    R3,IOKEY            R3=A(KEY FOR PRODUCTION BILL RECORD)         
         USING PBRRECD,R3                                                       
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ    RECORD TYPE                                  
         MVC   PBRKCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRKSUB,PBRKACTQ    ACTIVE RECORD                                
         MVC   PBRKJOB,ESJOB       JOB                                          
         MVC   PBRKSEQ,ESSEQ       SEQUENCE# (COMPLEMENT)                       
         GOTO1 AIO,IO1+IOACCMST+IORDUP GET BILL RECORD                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO1             R3=A(BILL RECORD)                            
         LA    R3,PBRRFST          R3=A(FIRST ELEMENT)                          
         USING NDXELD,R3                                                        
BILLR10  CLI   NDXEL,EOR           END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   NDXEL,NDXELQ        INDEX ELEMENT?                               
         BE    BILLR20                                                          
*                                                                               
         SR    RE,RE               BUMP TO NEXT ELEMENT                         
         IC    RE,NDXLN                                                         
         AR    R3,RE                                                            
         B     BILLR10                                                          
*                                                                               
         USING NDXELD,R3           INDEX ELEMENT                                
BILLR20  SR    RF,RF                                                            
         IC    RF,NDXHIGH          RF=(NUM OF LINE RECS ON OLD PARA)            
         SR    RE,RE                                                            
         IC    RE,ESPHIGH          RE=(NUM OF LINE RECS ON NEW PARA)            
         CR    RF,RE                                                            
         BH    *+6                                                              
         LR    RF,RE                                                            
         BCTR  RF,0                RF=(X LENGTH OF LONGEST INDX LIST)           
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   NDXINDX(0),ESPLST   ANY CHANGE                                   
         BNE   *+14                                                             
         CLC   NDXHIGH(L'NDXHIGH+L'NDXACTV),ESPHIGH CHANGED?                    
         BE    BILLR40                                                          
         CLI   NDXLN,NDXLNQ                                                     
         BNE   BILLR30                                                          
         MVC   NDXINDX(L'ESPLST),ESPLST       SAVE LINE INDEX LIST              
         MVC   NDXHIGH(L'NDXHIGH+L'NDXACTV),ESPHIGH SET HIGH AND ACTV           
         CLI   NDXACTV,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     BILLR40                                                          
*                                                                               
BILLR30  MVI   NDXEL,FF            MARK FOR DELETION                            
         GOTO1 VHELLO,BCDMCB,(C'D',ACCMST),('FF',AIO1),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,BOELEM           R3=A(ELEMENT WORK AREA)                      
         XC    BOELEM,BOELEM                                                    
         MVI   NDXEL,NDXELQ        INDEX ELEMENT                                
         MVI   NDXLN,NDXLNQ        SET ELEMENT LENGTH                           
         MVC   NDXINDX(L'ESPLST),ESPLST SAVED PARA INDEX LIST                   
         MVC   NDXHIGH(L'NDXHIGH+L'NDXACTV),ESPHIGH SET HIGH AND ACTV           
         CLI   NDXACTV,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 VHELLO,BCDMCB,(C'P',ACCMST),AIO1,(R3),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
BILLR40  GOTO1 AIO,IO1+IOACCMST+IOPUTREC PUT BACK A RECORD                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BILLRX   B     EXITY                                                            
*                                                                               
BILLRERX B     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* SET DEFAULT CURSOR POSTION IF NO OVERRIDE HAS TAKEN PLACE           *         
***********************************************************************         
         SPACE 1                                                                
DEFCURS  DS    0H                                                               
         USING *,R8                                                             
         L     RF,AINP             RF=A(TIOB)                                   
         USING TIOBD,RF                                                         
         TM    BCINDS2,BCINTRS     TEST FIRST TIME IN                           
         BO    DEFC15              YES - PUT CURSOR ON COMMAND LINE             
*                                                                               
DEFC02   SR    RE,RE                                                            
         ICM   RE,3,TIOBCURS       RE=ABSOLUTE A(CURSOR ON SCREEN)              
         CH    RE,=Y(LINE#1*COLS#Q) CURSOR BEFORE 1ST TEXT LINE?                
         BL    DEFC10                                                           
         OC    TIOBCURD,TIOBCURD   CURSOR POSITION OVERRIDEN?                   
         BNZ   DEFCX                                                            
         CH    RE,=Y(LINE#X*COLS#Q) CURSOR AFTER LAST TEXT LINE?                
         BNL   *+8                                                              
         LA    RE,COLS#Q(RE)       RE=ABSOLUTE A(NEXT ROW, SAME COLUMN)         
         SR    R2,R2                                                            
         LR    R3,RE               R3=RE                                        
         LA    R1,COLS#Q           R1=L(SCREEN LINE)                            
         STCM  R1,15,BCFULL                                                     
*                                                                               
         D     R2,BCFULL                                                        
         CH    R2,=Y(1+L'COMFLD)   CURSOR IN LINE COMMAND COL?                  
         BH    *+8                                                              
         BCTR  R2,0                                                             
         SR    RE,R2               SET TO FIRST COLUMN ON COMMAND LINE          
         STCM  RE,3,TIOBCURS       SAVE ABSOLUTE CURSOR ADDRESS                 
         B     DEFCX                                                            
*                                                                               
DEFC10   TM    ESFCFLG,ESFCMTQ     MATCH FOUND?                                 
         BO    DEFCX                                                            
         LA    RE,BASOPTH          RE=A(OPTION FIELD HEADER)                    
         S     RE,ATWA             RE=(DISPLACEMENT TO OPTION FIELD HD)         
         CLM   RE,3,ESCURD         IS CURSOR CURRENTLY ON THIS LINE?            
         BNE   *+12                                                             
         STCM  RE,3,TIOBCURD                                                    
         B     DEFC20                                                           
*                                                                               
DEFC15   L     RE,ESACOM1                                                       
         S     RE,ATWA             RE=(DISPLACEMENT TO COMMAND HEADER)          
         STCM  RE,3,TIOBCURD                                                    
DEFC20   MVI   TIOBCURI,0          SET TO START OF FIELD                        
DEFCX    OI    TIOBINDS,TIOBSETC   APPLICATION SET CURSOR POSITION              
         B     EXIT                                                             
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* ALTER BLOCK VALUES IF AFFECTED BY PREVIOUS LINE COMMAND             *         
* ON ENTRY FOLLOWING PARAMETERS ARE REQUIRED-                         *         
* ALTPARMS DS    0X                                                   *         
* ALTBSTRT DS    X                   DISP TO 1ST AFFECTED BLOCK VALUE *         
* ALTBEND  DS    X                   DISP TO LST AFFECTED BLOCK VALUE *         
* ALTBDVAL DS    X                   DISPLACEMENT VALUE               *         
* ALTBCTYP DS    XL2                 EXCEPT CMND TYPE LIST SPECIFIED  *         
* ALTBSTAT DS    X                   STATUS BYTE                      *         
* ALTBSUBQ EQU   X'80'               SUBTRACT DISP VAL (DEFAULT ADD)  *         
***********************************************************************         
         SPACE 1                                                                
ALTBLKV  DS    0H                                                               
         USING *,R8                                                             
         L     R3,AEBLTAB          R3=A(EDIT BLOCK LIST TABLE)                  
         USING EBLTABD,R3                                                       
ALTB10   CLC   EBLCTYPE,=AL2(EOT)  END OF TABLE?                                
         BE    ALTBX                                                            
         CLC   EBLCTYPE,ALTBCTYP   EXCEPTION COMMAND SPECIFIED?                 
         BE    ALTB70                                                           
         MVC   EHALF,EBLCTYPE                                                   
         NC    EHALF,=AL2(LENMOVEQ+LENCOPYQ)  MOVE/COPY CMD?                    
         BZ    ALTB20                                                           
         CLC   ESPARA,ESECMPA      SAME PARA AS DISPLAYED?                      
         BNE   ALTB70                                                           
         B     ALTB30                                                           
ALTB20   MVC   EHALF,EBLCTYPE                                                   
         NC    EHALF,=AL2(LENAFTQ+LENBEFQ+LENOVERQ) AFTER/BEFORE/OVER           
         BZ    ALTB30                                                           
         CLC   ESPARA,ESETOPA      SAME PARA AS DISPLAYED?                      
         BNE   ALTB70                                                           
ALTB30   SR    R0,R0                                                            
         IC    R0,EBLNM            R0=(MAX NUMBER OF ENTRIES IN LIST)           
         SR    R1,R1                                                            
         ICM   R1,3,EBLDIS                                                      
         LA    R1,ESAVE(R1)        R1=A(BLOCK LIST)                             
         USING BLKVALD,R1                                                       
         SR    RF,RF                                                            
         IC    RF,ALTBDVAL         RF=(DISPLACEMENT VALUE)                      
         TM    ALTBSTAT,ALTBSUBQ   SUBTRACT DISPLACEMENT?                       
         BNO   *+6                                                              
         LNR   RF,RF               SET TO NEGATIVE VALUE                        
ALTB40   OC    BLKSTRT(BLKLNQ),BLKSTRT NO MORE VALUES?                          
         BZ    ALTB70                                                           
         CLC   BLKSTRT,ALTBSTRT    BLOCK START IN AFFECTED AREA?                
         BL    ALTB50                                                           
         CLC   BLKSTRT,ALTBEND                                                  
         BH    ALTB50                                                           
         SR    RE,RE                                                            
         IC    RE,BLKSTRT                                                       
         AR    RE,RF                                                            
         BP    *+8                                                              
         LA    RE,1                                                             
         STC   RE,BLKSTRT                                                       
*        B     ALTB55                                                           
ALTB50   CLC   BLKEND,ALTBSTRT     BLOCK END IN AFFECTED AREA?                  
         BL    ALTB60                                                           
         CLC   BLKEND,ALTBEND                                                   
         BH    ALTB60                                                           
         SR    RE,RE                                                            
         IC    RE,BLKEND                                                        
         AR    RE,RF                                                            
         BP    *+8                                                              
         LA    RE,1                                                             
         STC   RE,BLKEND                                                        
ALTB55   CLI   BLKLEN,0            IF NUMERIC COMMAND RECALC LENGTH             
         BE    ALTB60                                                           
         SR    R2,R2                                                            
         IC    R2,BLKEND                                                        
         SR    R4,R4                                                            
         IC    R4,BLKSTRT                                                       
         SR    R2,R4                                                            
         LA    R2,1(R2)                                                         
         STC   R2,BLKLEN                                                        
ALTB60   LA    R1,BLKLNQ(R1)       BUMP TO NEXT BLOCK LIST VALUE                
         BCT   R0,ALTB40                                                        
ALTB70   LA    R3,EBLLNQ(R3)       BUMP TO NEXT EDIT TABLE ENTRY                
         B     ALTB10                                                           
ALTBX    B     EXIT                                                             
         DROP  R1,R3                                                            
         EJECT                                                                  
***********************************************************************         
* COPY/MOVE INDEXES                                                   *         
* NTRY- FOLLOWING PARAMTERS MUST BE SET                               *         
* CMPARM   DS    0X                COPY/MOVE ROUTINE PARMS            *         
* CMPMODE  DS    X                 MODE BYTE                          *         
* CMPCOPYQ EQU   X'80'             COPY MODE                          *         
* CMPMOVEQ EQU   X'40'             MOVE MODE                          *         
* CMPBEFRQ EQU   X'20'             BEFORE 'TO' POS (DEFAULT AFTER)    *         
* CMPTO    DS    X                 COPY/MOVE TO LINE                  *         
* CMPFROM1 DS    X                 COPY/MOVE FROM LINE 1 (START)      *         
* CMPFROM2 DS    X                 COPY/MOVE FROM LINE 2 (END)        *         
* CMPAILST DS    A                 A(PARAGRAPH/LINE INDEX LIST)       *         
* CMLNQ    EQU   *-CMPARM                                             *         
* XIT-     ORIGINAL INDEX STRING OVERWRITTEN BY MANIPULATED STRING    *         
***********************************************************************         
         SPACE 1                                                                
CMI      DS    0H                                                               
         USING *,R8                                                             
         LA    R0,WORKLST          INIT WORKER INDEX LIST                       
         LH    R1,=Y(L'WORKLST)                                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         ICM   R2,15,CMPAILST      R2=A(PARAGRAPH/LINE INDEX LIST)              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    RF,RF                                                            
         IC    RF,CMPTO            RF=(LINE NUMBER OF 'TO' POSITION)            
         TM    CMPMODE,CMPBEFRQ    COPY/MOVE BEFORE 'TO' POSITION?              
         BNO   CMI10                                                            
         SH    RF,=H'1'            REDUCE 'TO' LINE NUMBER                      
         STCM  RF,1,CMPTO                                                       
         BZ    CMI20                                                            
CMI10    LR    RE,RF               RE=RF                                        
         BCTR  RE,0                RE=X L'(STRING UP TO 'TO' POSITION)          
         EX    RE,*+4                                                           
         MVC   WORKLST(0),0(R2)    GET STRING UP TO 'TO' POSITION               
*                                                                               
CMI20    LA    R3,WORKLST(RF)      R3=A('TO' POSITION IN NEW LIST)              
         SR    RF,RF                                                            
         IC    RF,CMPFROM1         RF=(COPY/MOVE BLOCK START LINE)              
         SR    RE,RE                                                            
         IC    RE,CMPFROM2         RE=A(COPY/MOVE BLOCK END LINE)               
         SR    RE,RF               RF=(X LENGTH OF BLOCK)                       
         BNM   *+6                                                              
         DC    H'0'                                                             
         BCTR  RF,0                                                             
         LA    R2,0(RF,R2)         R2=A(FIRST INDEX IN COPY/MOVE BLOCK)         
         EX    RE,*+4                                                           
         MVC   0(0,R3),0(R2)       GET COPY/MOVE BLOCK                          
*                                                                               
         LA    R3,1(RE,R3)         R3=A(NEXT FREE INDEX IN NEW LIST)            
         ICM   R2,15,CMPAILST                                                   
         SR    RF,RF                                                            
         IC    RF,CMPTO            RF=(TO LINE NUMBER)                          
         SR    RE,RE                                                            
         IC    RE,CMPILLN          RE=L'(INDEX LIST)                            
         SR    RE,RF                                                            
         BZ    CMI30                                                            
         BCTR  RE,0                RE=(X LEN OF REMAINING INDEX STRING)         
         LA    R2,0(RF,R2)         R2=A(1ST REMAING INDEX STRING)               
         EX    RE,*+4                                                           
         MVC   0(0,R3),0(R2)       GET REMAINING STRING                         
*                                                                               
CMI30    TM    CMPMODE,CMPCOPYQ    IF NOT COPY MODE REMOVE ORIG BLOCK           
         BO    CMI40                                                            
         SR    RF,RF                                                            
         IC    RF,CMPFROM1         RF=(START OF BLOCK LINE NUMBER)              
         SR    R2,R2                                                            
         IC    R2,CMPFROM2         R2=(END OF BLOCK LINE NUMBER)                
         SR    R2,RF                                                            
         LA    R2,1(R2)            R2=L'(MOVE BLOCK)                            
         CLC   CMPFROM1,CMPTO      ORIG MOVE BLOCK BEFORE 'TO' LINE NO?         
         BNH   *+6                                                              
         AR    RF,R2               ADJUST LINE NUMBER FOR NEW BLOCK             
         LA    R0,WORKLST(RF)      R0=A(1ST INDX IN ORIG MOVE BLOCK)            
         BCTR  R0,0                                                             
         LR    RE,R0               RE=A(1ST INDX CHAR AFTER MOVE BLOCK)         
         AR    RE,R2                                                            
         LH    R1,=Y(L'WORKLST)                                                 
         LA    R1,WORKLST(R1)                                                   
         LR    RF,R1                                                            
         SR    R1,R0               R1=L'(MOVE TO)                               
         SR    RF,RE               RF=L'(MOVE FROM)                             
         MVCL  R0,RE               OVER WRITE ORIGINAL MOVE BLOCK               
*                                                                               
CMI40    ICM   R2,15,CMPAILST                                                   
         SR    RE,RE                                                            
         IC    RE,CMPILLN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R2),WORKLST     OVER WRITE OLD STRING                        
*                                                                               
CMIX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FIND/CHANGE TEXT STRING                                             *         
***********************************************************************         
         SPACE 1                                                                
FCTXT    DS    0H                                                               
         USING *,R8                                                             
         CLI   ESCMND,CMNFNDQ      NEW FIND COMMAND ENTERED?                    
         BNE   *+12                                                             
         MVI   ESFCFLG,ESFCFNQ     SET FIND MODE FLAG ON                        
         B     FCT10                                                            
         CLI   ESCMND,CMNCHGQ      NEW CHANGE COMMAND ENTERED                   
         BNE   FCT20                                                            
         MVI   ESFCFLG,ESFCCHQ     SET CHANGE MODE FLAG ON                      
FCT10    XC    ESFCCNT,ESFCCNT     INIT NUMBER OF MATCHES FOUND                 
         B     FCT60                                                            
FCT20    LA    R2,BASOPTH          R2=A(COMMAND LINE FIELD HEADER)              
         ST    R2,FVADDR                                                        
         NI    ESFCFLG,FF-(ESFCMTQ+ESFCTLQ) MATCH/TOO LONG OFF                  
         CLI   BCPFKEY,PFKFINDQ    FIND PFKEY?                                  
         BNE   FCT30                                                            
         MVC   FVMSGNO,=AL2(AE$ENTFD) ENTER FIND COMMAND                        
         CLI   ESFDLN,0            FIND STRING BEEN ENTERED?                    
         BE    FCTERRX                                                          
         OI    ESFCFLG,ESFCFNQ     SET FIND MODE FLAG ON                        
         NI    ESFCFLG,FF-ESFCCHQ SWITCH OFF CHANGE MODE                        
         B     FCT40                                                            
FCT30    CLI   BCPFKEY,PFKCHGQ     CHANGE PFKEY?                                
         BE    *+12                                                             
         NI    ESFCFLG,FF-(ESFCFNQ+ESFCCHQ) SWITCH OFF FIND/CHNGE               
         B     FCTX                                                             
         MVC   FVMSGNO,=AL2(AE$ENTCH) ENTER CHANGE COMMAND                      
         CLI   ESCHLN,0            CHANGE STRING BEEN ENTERED?                  
         BE    FCTERRX                                                          
         OI    ESFCFLG,ESFCCHQ     SET CHANGE MODE FLAG ON                      
         NI    ESFCFLG,FF-ESFCFNQ SWITCH OFF FIND MODE                          
FCT40    CLC   ESFCPOF,ESPOFST     SAME PARA AS PREVIOUS TIME THROUGH?          
         BNE   FCT50                                                            
         CLC   ESFCSTL,ESSTLIN     SAME START LINE AS PREVIOUS TIME?            
         BNE   FCT50                                                            
         CLC   ESFCSTC,ESCOL#1     SAME START COLUMN AS PREVIOUS TIME?          
         BNE   FCT50                                                            
         L     RF,AINP             RF=A(TIOB)                                   
         USING TIOBD,RF                                                         
         CLC   TIOBCURS,=Y(LINE#1*COLS#Q) CURSOR BEFORE TEXT AREA?              
         BL    FCT60                                                            
         CLC   ESFCCRS,TIOBCURS-TIOBD(RF) CURSOR POS SAME AS PREVIOUS?          
         BE    FCT60                                                            
FCT50    NI    ESFCFLG,FF-(ESFCTPQ+ESFCBTQ) SWITCH OF TOP/BOT                   
FCT60    MVC   ESFCSTL,ESSTLIN     SAVE CURRENT SCREEN START LINE               
         MVC   ESFCSTC,ESCOL#1     SAVE CURRENT SCREEN START COL                
         MVC   ESFCENC,ESCOL#X     SAVE CURRENT SCREEN END COL                  
         MVC   ESFCPOF,ESPOFST     SAVE CURRENT SCREEN PARA OFFSET              
         MVC   ESFCPRA,ESPARA      SAVE CURRENT SCREEN PARA INDEX               
         L     RF,AINP                                                          
         MVC   ESFCCRS,TIOBCURS    SAVE CURRENT SCREEN ABSOLUTE A(CURS)         
         DROP  RF                                                               
*                                                                               
         TM    ESFCPAR,ESFCPVQ     PREVIOUS PARAMETER SET?                      
         BO    FCT70                                                            
         TM    ESFCPAR,ESFCLSQ     LAST PARAMETER SET?                          
         BO    FCT70                                                            
         TM    ESFCFLG,ESFCTPQ     TOP OF TEXT PREVIOUSLY REACHED?              
         BNO   FCT110                                                           
FCT70    LA    R1,ESPACTV          R1=A(LAST ACTIVE PARA INDEX)                 
         GOTO1 AGPARA              GET LAST PARAGRAPH                           
         MVC   ESFCLTL,ESLACTV     SET LAST LINE NUMBER                         
         MVC   ESFCSCP,ESPOFST     SET SEARCH START TO LAST PARA                
         MVC   ESFCSCL,ESLACTV     SET SEARCH START LINE TO LAST LINE           
         MVC   ESFCSCC,ESREPWD     SET SEARCH START COL TO LAST COL             
         TM    ESFCPAR,ESFCLSQ     LAST PARAMETER SET?                          
         BNO   FCT80                                                            
         NI    ESFCPAR,FF-ESFCLSQ SWITCH OFF LAST PARAMETER                     
         OI    ESFCPAR,ESFCPVQ     AND REPLACE WITH PREVIOUS PARAM              
         B     FCT100                                                           
FCT80    TM    ESFCFLG,ESFCTPQ     PREVIOUS PARAMETER SET?                      
         BO    FCT100                                                           
         CLI   ESFCPOF,1           CURR SCREEN IS FIRST PARA?                   
         BNE   FCT90                                                            
         CLI   ESFCSTL,1           CURR SCREEN STARTS AT 1ST TEXT LINE?         
         BNE   FCT90                                                            
         L     RF,AINP             RF=A(TIOB)                                   
         CLC   =Y(LINE#1*COLS#Q),TIOBCURS-TIOBD(RF) CURS BEFORE TEXT?           
         BNL   FCT100                                                           
FCT90    LA    R1,ESFCPOF          R1=A(CURRENT PARA OFFSET)                    
         GOTO1 AGPARA              GET PARAGRAPH                                
         B     FCT130                                                           
*                                                                               
FCT100   NI    ESFCFLG,FF-ESFCTPQ SWITCH OF TOP FLAG                            
         SR    R2,R2                                                            
         IC    R2,ESLACTV                                                       
         LA    R2,ESLLST-1(R2)     R2=A(INDEX OF LAST LIN IN LAST PARA)         
         B     FCT170                                                           
*                                                                               
FCT110   TM    ESFCPAR,ESFCALQ     ALL PARAMETER SET?                           
         BO    FCT120                                                           
         TM    ESFCPAR,ESFCFSQ     FIRST PARAMETER SET?                         
         BO    FCT120                                                           
         TM    ESFCFLG,ESFCBTQ     BOTTOM OF TEXT PREVIOUSLY REACHED?           
         BNO   FCT130                                                           
FCT120   NI    ESFCFLG,FF-ESFCBTQ SWITCH OFF BOTTOM                             
         NI    ESFCPAR,FF-ESFCFSQ       SWITCH OFF FIRST PARAM                  
         MVI   ESFCSCP,1           SET SEARCH START PARA TO FIRST PARA          
         MVI   ESFCSCL,1           SET SEARCH START LINE TO FIRST LINE          
         MVI   ESFCSCC,1           SET SEARCH START COL TO FIRST COL            
         LA    R1,ESFCSCP                                                       
         GOTO1 AGPARA              GET FIRST PARAGRAPH                          
         LA    R2,ESLLST           R2=A(INDEX OF 1ST LINE IN 1ST PARA)          
         B     FCT170                                                           
*                                                                               
FCT130   MVC   ESFCSCP,ESPOFST     SET SEARCH START PARA TO CURR PARA           
         MVC   ESFCSCL,ESSTLIN     SET SEARCH STRT LIN TO CURR STRT LIN         
         MVI   ESFCSCC,1           SET SEARCH START COL TO 1ST COL              
         SR    RF,RF                                                            
         IC    RF,ESSTLIN                                                       
         LA    R2,ESLLST-1(RF)     R2=A(INDEX OF CURRENT START LINE)            
         L     RF,AINP             RF=A(TIOB)                                   
         USING TIOBD,RF                                                         
         SR    RE,RE                                                            
         ICM   RE,3,TIOBCURS       RE=ABSOLUTE A(CURSOR ON SCREEN)              
         CH    RE,=Y(LINE#1*COLS#Q) CURSOR ABOVE TEXT AREA?                     
         BNL   FCT150                                                           
         TM    ESFCPAR,ESFCPVQ     PREVIOUS PARAMETER SET?                      
         BO    FCT410                                                           
         B     FCT170                                                           
FCT150   CH    RE,=Y(LINE#X*COLS#Q) CURSOR BEYOND TEXT AREA?                    
         BL    *+12                                                             
         LA    R2,MAXSLINQ(R2)     R2=A(INDX OF LIN AFTER CURR END LIN)         
         B     FCT160                                                           
         OI    ESFCFLG,ESFCIGQ     SET IGNORE FIRST MATCH FLAG ON               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,TIOBCURS       R1=(DISPLACEMENT TO FIELD WITH CURS)         
         DROP  RF                                                               
         LH    RF,=Y(LINE#1*COLS#Q) RF=(DISPLACEMENT TO START LINE)             
         SR    R1,RF                                                            
         LA    RF,COLS#Q           RF=L'(PHYSICAL SCREEN LINE)                  
         STCM  RF,15,BCFULL                                                     
*                                                                               
         D     R0,BCFULL                                                        
         STC   R1,ESFCSCL          R1=(SEARCH START LINE NUMBER)                
         AR    R2,R1               R2=A(INDEX OF SEARCH START LINE)             
         SH    R0,=Y(1+L'COMFLD)   SUBTRACT LINE CMND FIELD FROM COL NO         
         BP    *+8                                                              
         LA    R0,1                                                             
         SR    RF,RF                                                            
         IC    RF,ESFCSTC                                                       
         BCTR  RF,0                                                             
         AR    R0,RF               ADJUST SEARCH START COLUMN                   
         STC   R0,BCBYTE1                                                       
         STC   R0,ESFCSCC                                                       
*                                  ENSURE SEACH START NOT BEYOND PARA           
FCT160   SR    RF,RF                                                            
         IC    RF,ESLACTV          RF=(NUMBER OF ACTIVE LINE RECORDS)           
         LA    RF,ESLLST-1(RF)                                                  
         CR    R2,RF               ENSURE WE HAVE ACTIVE RECORD                 
         BNH   FCT170                                                           
         TM    ESFCPAR,ESFCPVQ     PREVIOUS PARAMETER SET?                      
         BNO   FCT450                                                           
         LR    R2,RF               R2=A(INDEX OF SEARCH START LINE)             
         LR    RE,R2                                                            
         LA    R1,ESLLST-1                                                      
         SR    RE,R1                                                            
         STC   RE,ESFCSCL          SET START LINE/COL FOR PREV SEARCH           
         MVC   ESFCSCC,ESREPWD                                                  
*                                                                               
FCT170   LA    RF,IOKEY            RF=A(KEY FOR LINE RECORD)                    
         USING PBRRECD,RF                                                       
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ    RECORD TYPE                                  
         MVC   PBRKCPY,CUABIN      COMPANY CODE                                 
         MVI   PBRKSUB,PBRKACTQ    ACTIVE RECORD                                
         MVC   PBRKJOB,ESJOB       JOB                                          
         MVC   PBRKSEQ,ESSEQ       SEQUENCE# (COMPLEMENT)                       
         MVC   PBRKPARA,ESPARA     PARAGRAPH#                                   
         MVC   PBRKLINE,0(R2)      LINE#                                        
         DROP  RF                                                               
         GOTO1 AIO,IO1+IOACCMST+IOREAD GET LINE RECORD                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 MAKETX,R7                                                        
         USING TXD,R7                                                           
         GOTO1 AGETTX,BOPARM,TXD,AIO1                                           
         L     R1,ATRUP            GET TRANSLATE TABLE FOR LANG                 
         MVC   WORKLST(L'TXDATA),TXDATA SAVE ORIGINAL LINE                      
         TR    TXDATA,0(R1)        CONVERT 1ST SAVED LINE TO UPPER CASE         
         TM    ESFCPAR,ESFCPVQ     PREVIOUS PARAMETER SET?                      
         BNO   FCT210                                                           
         CLC   ESPOFST,ESPACTV     LAST PARAGRAPH?                              
         BNE   FCT210                                                           
         CLC   ESFCLTL,0(R2)       LAST LINE OF PARAGRAPH?                      
         BNE   FCT210                                                           
         MVC   ESFCLTC,ESREPWD     GET LAST COLUMN POSITION OF TEXT             
*                                                                               
FCT210   TM    ESFCFLG,ESFCIGQ     IGNORE FIRST MATCH TEST FLAG ON?             
         BNO   FCT240                                                           
         NI    ESFCFLG,FF-ESFCIGQ                                               
         CLI   ESFCCOL,0           COLUMN PARAMETER SET?                        
         BE    FCT230                                                           
         TM    ESFCPAR,ESFCPVQ     PREVIOUS PARAMETER SET?                      
         BNO   FCT220                                                           
         CLC   ESFCCOL,BCBYTE1     IF COL PARAMETER >= CURSOR COL               
         BNL   FCT410              THEN IGNORE LINE                             
         B     FCT250                                                           
FCT220   CLC   ESFCCOL,BCBYTE1     IF COL PARAMETER <= CURSOR COL               
         BNH   FCT430              THEN IGNORE LINE                             
         B     FCT250                                                           
FCT230   LA    R4,TXDATA           R4=A(FIRST CHAR IN UPPER CASE LINE)          
         SR    RE,RE                                                            
         IC    RE,BCBYTE1          RE=(COL NUM OF CURSOR)                       
         BCTR  RE,0                                                             
         AR    R4,RE               R4=A(CHAR AT CURSOR POSITION)                
         TM    ESFCFLG,ESFCCHQ     IF CHANGE MODE DONT IGNORE                   
         BNO   FCT390                                                           
         B     FCT280                                                           
FCT240   CLI   ESFCCOL,0           COLUMN PARAMETER SET?                        
         BE    FCT260                                                           
FCT250   SR    R4,R4                                                            
         IC    R4,ESFCCOL                                                       
         LA    R4,TXDATA-1(R4)     R4=A(CHAR AT COLUMN PARAMETER)               
         B     FCT280                                                           
FCT260   TM    ESFCPAR,ESFCPVQ     PREVIOUS PARAMETER IN USE?                   
         BNO   FCT270                                                           
         LA    R4,TXDATA+L'TXDATA R4=A(LAST CHAR IN LINE + 1)                   
         SR    R1,R1                                                            
         IC    R1,ESFDLN           R1=L'(FIND STRING)                           
         SR    R4,R1               R4=A(LAST POSS MATCH IN TEXT LINE)           
         B     *+8                                                              
FCT270   LA    R4,TXDATA           R4=A(FIRST POSS MATCH IN TEXT LINE)          
*                                                                               
FCT280   SR    R1,R1                                                            
         IC    R1,ESFDLN           R1=L'(FIND STRING)                           
         TM    ESFCPAR,ESFCWDQ     WORD PARAMETER SET?                          
         BO    FCT290                                                           
         TM    ESFCPAR,ESFCPFQ     PREFIX PARAMETER SET?                        
         BO    FCT290                                                           
         TM    ESFCPAR,ESFCSFQ     SUFFIX PARAMETER SET?                        
         BNO   FCT340                                                           
FCT290   LR    RE,R4                                                            
         BCTR  RE,0                RE=A(CHAR BEFORE MATCH STRING)               
         LA    R0,TXDATA           R0=A(FIRST CHAR)                             
         CR    R0,R4               IF START OF LINE THE START OF WORD           
         BE    FCT300                                                           
         CLI   0(RE),C'A'          IF CHAR NOT ALPHA NUMERIC                    
         BL    FCT300                                                           
         CLI   0(RE),C'9'          THEN EITHER WORD OR PREFIX                   
         BNH   FCT310                                                           
FCT300   TM    ESFCPAR,ESFCSFQ     SUFFIX PARAMETER?                            
         BO    FCT390                                                           
         B     *+12                                                             
FCT310   TM    ESFCPAR,ESFCSFQ                                                  
         BNO   FCT390                                                           
         LA    RE,0(R4,R1)         RE=A(LAST CHAR IN MATCH TEST)                
         AH    R0,=Y(L'TXDATA)                                                  
         CR    RE,R0               IF LAST CHAR                                 
         BNL   FCT320                                                           
         CLI   0(RE),C'A'          IF CHAR NOT ALPHA NUMERIC                    
         BL    FCT320                                                           
         CLI   0(RE),C'9'          THEN EITHER WORD OR SUFFIX                   
         BNH   FCT330                                                           
FCT320   TM    ESFCPAR,ESFCPFQ     PREFIX PARAMETER?                            
         BO    FCT390                                                           
         B     *+12                                                             
FCT330   TM    ESFCPAR,ESFCPFQ                                                  
         BNO   FCT390                                                           
FCT340   BCTR  R1,0                R1=X L'(FIND STRING)                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),ESFDTXT     MATCH ON TEXT LINE?                          
         BNE   FCT390                                                           
*                                                                               
         TM    ESFCFLG,ESFCCHQ     CHANGE MODE FLAG ON?                         
         BNO   FCT350                                                           
         LA    RE,ESLLST           RE=A(SAVED LINE INDEX LIST)                  
         LR    R1,R2                                                            
         SR    R1,RE                                                            
         LA    R1,1(R1)                                                         
         STC   R1,CHGLINOF         R1=(LINE NUMBER OF MATCH)                    
         LA    RE,TXDATA           RE=A(UPPER CASE LINE)                        
         LR    R1,R4               R1=A(FIRST CHAR OF MATCH)                    
         SR    R1,RE               R1=(COL NUMBER OF MATCH)                     
         LA    R1,1(R1)                                                         
         STC   R1,CHGCOLOF                                                      
         LA    RE,TXD              RE=A(ORIGINAL LINE)                          
         ST    RE,CHGATX                                                        
         GOTO1 ACHGTXT             CHANGE THE TEXT                              
         BE    *+12                                                             
         OI    ESFCFLG,ESFCTLQ     SET LINE AMENDED                             
         B     FCT355                                                           
         OI    ESFCFLG,ESFCAMQ     SET LINE AMENDED                             
         TM    ESFCPAR,ESFCALQ     ALL PARAMETER IN USE?                        
         BNO   FCT360                                                           
         MVC   TXDATA,WORKLST      UPDATE UPPERCASE LINE                        
         L     R1,ATRUP            GET TRANSLATE TABLE FOR LANG                 
         TR    TXDATA,0(R1)        CONVERT TO UPPER CASE                        
         B     *+12                                                             
*                                                                               
FCT350   TM    ESFCPAR,ESFCALQ     ALL PARAMETER SET?                           
         BNO   FCT360                                                           
         SR    RE,RE                                                            
         ICM   RE,3,ESFCCNT                                                     
         LA    RE,1(RE)                                                         
         STCM  RE,3,ESFCCNT        INCREMENT MATCH COUNT                        
         TM    ESFCFLG,ESFCMTQ     PREVIOUS MATCH?                              
         BO    FCT390                                                           
FCT355   MVC   ESFCSTL,ESSTLIN     SAVE CURRENT SCREEN START LINE               
         MVC   ESFCSTC,ESCOL#1     SAVE CURRENT SCREEN START COL                
         MVC   ESFCENC,ESCOL#X     SAVE CURRENT SCREEN END COL                  
         MVC   ESFCPOF,ESPOFST     SAVE CURRENT SCREEN PARA OFFSET              
         MVC   ESFCPRA,ESPARA      SAVE CURRENT SCREEN PARA INDEX               
FCT360   GOTO1 ASETLC,BOPARM,(R2),(R4),TXD SET NEXT START LINE AND COL          
         TM    ESFCFLG,ESFCTLQ     LINE TOO LONG FOR AMEND?                     
         BO    FCT510                                                           
         OI    ESFCFLG,ESFCMTQ     SET MATCH FOUND FLAG ON                      
*                                                                               
FCT370   TM    ESFCPAR,ESFCALQ                                                  
         BNO   FCTX                                                             
*                                                                               
FCT390   CLI   ESFCCOL,0           COLUMN PARAMETER SET?                        
         BNE   FCT410                                                           
         SR    R1,R1                                                            
         IC    R1,ESFDLN           R1=L'(FIND STRING)                           
         TM    ESFCPAR,ESFCPVQ     PREVIOUS PARAMETER SET?                      
         BNO   FCT400                                                           
         BCTR  R4,0                BUMP TO PREVIOUS CHAR IN LINE                
         LA    R0,TXDATA           R0=(START OF LINE)                           
         CR    R4,R0               IF START OF LINE REACHED                     
         BL    FCT410              THEN GET PREVIOUS LINE                       
         B     FCT280                                                           
*                                                                               
FCT400   SR    R1,R1                                                            
         IC    R1,ESFDLN           R1=L'(FIND STRING)                           
         LA    R4,1(R4)            BUMP TO NEXT CHAR IN LINE                    
         LA    RE,0(R1,R4)                                                      
         LA    R0,TXDATA                                                        
         SR    RE,R0                                                            
         CLM   RE,1,=AL1(L'TXDATA) END OF LINE REACHED?                         
         BNH   FCT280                                                           
*                                                                               
FCT410   TM    ESFCFLG,ESFCAMQ     LINE AMENDED?                                
         BNO   FCT420                                                           
         NI    ESFCFLG,FF-ESFCAMQ                                               
         MVI   LRMODE,LRCHANGE     CHANGE RECORD                                
         LA    RE,ESLLST                                                        
         LR    R1,R2                                                            
         SR    R1,RE                                                            
         LA    R1,1(R1)                                                         
         STC   R1,LRLINE           R1=(LINE NUMBER)                             
         MVC   LRPARA,ESPARA       PARAGRAPH NUMBER                             
         MVC   TXDATA,WORKLST      ??                                           
         GOTO1 ALINEREC,TXD                                                     
         DROP  R7                                                               
*                                                                               
FCT420   TM    ESFCPAR,ESFCPVQ     PREVIOUS PARAMETER?                          
         BNO   FCT430                                                           
         SH    R2,=H'1'            BUMP TO PREV LINE INDEX                      
         LA    RE,ESLLST           RE=A(LINE INDEX LIST)                        
         CR    R2,RE                                                            
         BL    FCT440              GET PREVIOUS PARA                            
         B     FCT170                                                           
*                                                                               
FCT430   LA    R2,1(R2)            BUMP TO NEXT LINE INDEX                      
         SR    RE,RE                                                            
         IC    RE,ESLACTV                                                       
         LA    RE,ESLLST-1(RE)                                                  
         CR    R2,RE               IF NO MORE LINES GET NEXT PARAGRAPH          
         BH    FCT450                                                           
         B     FCT170                                                           
*                                  GET PREVIOUS PARA                            
FCT440   SR    RF,RF                                                            
         IC    RF,ESPOFST                                                       
         CH    RF,=H'1'            FIRST PARA?                                  
         BE    FCT490                                                           
         BCTR  RF,0                                                             
         B     FCT460                                                           
*                                  GET NEXT PARA                                
FCT450   SR    RF,RF                                                            
         IC    RF,ESPOFST                                                       
         CLM   RF,1,ESPACTV        LAST PARA?                                   
         BE    FCT470                                                           
         LA    RF,1(RF)                                                         
FCT460   STC   RF,ESPOFST                                                       
         LA    R1,ESPOFST                                                       
         GOTO1 AGPARA              GET PARAGRAPH                                
         LA    R2,ESLLST           R2=A(FIRST LINE INDEX)                       
         TM    ESFCPAR,ESFCPVQ     PREVIOUS PARAMETER SET?                      
         BNO   FCT170                                                           
         SR    R2,R2                                                            
         IC    R2,ESLACTV                                                       
         LA    R2,ESLLST-1(R2)     R2=A(LAST LINE INDEX)                        
         B     FCT170                                                           
*                                                                               
FCT470   CLI   ESFCSCP,1           IF SEARCH STARTED AT PARAGRAPH 1             
         BNE   FCT480                                                           
         CLI   ESFCSCL,1           AND LINE 1                                   
         BNE   FCT480                                                           
         CLI   ESFCSCC,1           AND COLUMN 1                                 
         BE    FCT510              THEN WHOLE OF TEXT SCANNED                   
FCT480   OI    ESFCFLG,ESFCBTQ     ELSE SET BOTTOM OF TEXT REACHED              
         B     FCT510                                                           
*                                                                               
FCT490   CLC   ESFCSCP,ESPACTV     IF SEARCH STARTED  AT LAST PARA              
         BNE   FCT500                                                           
         CLC   ESFCSCL,ESFCLTL     AND LAST LINE                                
         BNE   FCT500                                                           
         CLC   ESFCSCC,ESREPWD     AND LAST COLUMN                              
         BE    FCT510              THEN WHOLE OF TEXT SCANNED                   
FCT500   OI    ESFCFLG,ESFCTPQ     ELSE SET TOP OF TEXT REACHED                 
*                                                                               
FCT510   CLC   ESPOFST,ESFCPOF     SAME PARA AS PREVIOUS DISPLAY?               
         BNE   FCT520                                                           
         TM    ESFCFLG,ESFCTLQ     LINE TOO LONG FOR AMEND?                     
         BO    FCT530                                                           
         B     FCTX                                                             
FCT520   MVC   ESPOFST,ESFCPOF                                                  
         LA    R1,ESFCPOF                                                       
         GOTO1 AGPARA              REDISPLAY START PARA                         
FCT530   MVC   ESSTLIN,ESFCSTL     RESET LINE/COLUMN STARTS                     
         MVC   ESCOL#1,ESFCSTC                                                  
         MVC   ESCOL#X,ESFCENC                                                  
*                                                                               
FCTX     B     EXITY                                                            
*                                                                               
FCTERRX  B     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* SET LINE NUMBER, COLUMN NUMBER AND CURSOR POSITION FOR NEXT DISPLAY *         
*                                                                     *         
* NTRY: P1 = A(INDEX OF LINE CONTAINING MATCH)                        *         
*       P2 = TEXT POSTION IN TXD                                      *         
*       P3 = TXD                                                      *         
***********************************************************************         
         SPACE 1                                                                
SETLC    DS    0H                                                               
         USING *,R8                                                             
         L     R2,0(R1)                                                         
         L     R4,4(R1)                                                         
         L     R6,8(R1)                                                         
         USING TXD,R6                                                           
*                                  SET LINE NUMBER FOR NEXT DISP                
         LA    RE,ESLLST-1         RE=A(SAVED LINE INDX LST - 1)                
         SR    R2,RE               R2=(LINE NUMBER OF MATCH)                    
         CLC   ESPARA,ESFCPRA      SAME PARAGRAPH AS PREV DISP?                 
         BNE   SETLC10                                                          
         CLM   R2,1,ESFCSTL        MATCHED LINE BEFORE CURRENT SCREEN?          
         BL    SETLC10                                                          
         SR    RE,RE                                                            
         IC    RE,ESFCSTL                                                       
         LA    RE,MAXSLINQ-1(RE)                                                
         CR    R2,RE               MATCHED LINE AFTER CURRENT SCREEN?           
         BH    SETLC10                                                          
         MVC   ESSTLIN,ESFCSTL     NEXT START LINE SAME AS PREVIOUS             
         SR    R0,R0                                                            
         IC    R0,ESSTLIN                                                       
         SR    R2,R0               R2=(SCREEN LINE NUMBER)                      
         B     SETLC30                                                          
*                                                                               
SETLC10  CH    R2,=H'1'            START OF TEXT?                               
         BE    SETLC20                                                          
         BCTR  R2,0                DISPLAY FROM LINE ABOVE MATCH LINE           
         STC   R2,ESSTLIN                                                       
         LA    R2,1                                                             
         B     SETLC30                                                          
SETLC20  STC   R2,ESSTLIN                                                       
         XR    R2,R2                                                            
SETLC30  STC   R2,TSARLIN#                                                      
         GOTO1 MAKETX,R7                                                        
         GOTO1 ARESTX,BOPARM,(R7)                                               
         L     R2,(TXATXT-TXD)(R7)                                              
         S     R2,ATWA                                                          
         L     RE,AINP             RE=A(TIOB)                                   
         STCM  R2,3,TIOBCURD-TIOBD(RE) SAVE DISPLACEMENT TO TEXT FIELD          
*                                  SET COLUMN NUMBER FOR NEXT DISP              
         TM    ESFCFLG,ESFCTLQ     LINE TOO LONG TO CHANGE?                     
         BNO   *+10                                                             
         XR    R0,R0               SET CURSOR AT START OF LINE                  
         B     SETLC80                                                          
         SR    R1,R1                                                            
         IC    R1,ESFDLN           R1=L'(FIND STRING)                           
         TM    ESFCFLG,ESFCCHQ     CHANGE MODE?                                 
         BNO   *+12                                                             
         IC    R1,ESCHLN                                                        
         LA    R1,1(R1)            R1=L'(CHANGE STRING + 1)                     
         LA    RE,TXDATA           RE=A(UPPER CASE TEXT LINE)                   
         SR    R4,RE                                                            
         LA    R4,1(R4)            R4=(START COLUMN OF MATCHED STRING)          
         LR    R0,R4               R0=R4                                        
         CLC   ESPARA,ESFCPRA      SAME PARAGRAPH AS PREV DISP?                 
         BNE   SETLC40                                                          
         CLM   R4,1,ESFCSTC        IF MATCHED STRING WITHIN CURR COLS           
         BL    SETLC40                                                          
         SR    RE,RE                                                            
         IC    RE,ESFCSTC                                                       
         LA    RE,MAXSCOLQ(RE)     ??                                           
         LA    R4,0(R4,R1)                                                      
         CR    R4,RE                                                            
         BH    SETLC50                                                          
         MVC   ESCOL#1,ESFCSTC     DO NOT CHANGE START COLUMN                   
         MVC   ESCOL#X,ESFCENC     DO NOT CHANGE END COLUMN                     
         SR    R4,R4                                                            
         IC    R4,ESCOL#1          R4=(START COLUMN)                            
         B     SETLC60                                                          
SETLC40  LA    R4,0(R4,R1)                                                      
SETLC50  CLM   R4,1,ESREPWD        MATCHED/CHANGED STRING WILL FIT?             
         BNH   *+12                                                             
         IC    R4,ESREPWD                                                       
         LA    R4,1(R4)                                                         
         SH    R4,=Y(MAXSCOLQ) ??  FIND START COLUMN                            
         BP    *+8                                                              
         LA    R4,1                                                             
         STC   R4,ESCOL#1                                                       
SETLC60  SR    R0,R4                                                            
         TM    ESFCFLG,ESFCCHQ     IF CHNGE CURS GOES AFTER MATCH               
         BNO   SETLC80                                                          
         TM    ESFCPAR,ESFCPVQ     PREVIOUS PARAMETER USED?                     
         BNO   SETLC70                                                          
         SH    R0,=H'1'                                                         
         BNM   SETLC80                                                          
         SR    R0,R0                                                            
         B     SETLC80                                                          
SETLC70  AR    R0,R1                                                            
         BCTR  R0,0                                                             
         CH    R0,=Y(MAXSCOLQ-1)   ??                                           
         BNH   *+8                                                              
         LA    R0,L'MAXSCOLQ-1     ??                                           
SETLC80  L     RE,AINP             RE=A(TIOB)                                   
         STC   R0,TIOBCURI-TIOBD(RE) SET FIELD INDEX OF CURSOR                  
SETLCX   B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* CHANGE MATCHED STRING                                               *         
* NTRY- FOLLOWING PARAMTERS MUST BE SET                               *         
* CHGPARMS DS    0X                CHANGE ROUTINE PARAMETERS          *         
* CHGATX   DS    A                 ADDRESS OF ZYZD                    *         
* CHGLINOF DS    X                 OFFSET TO LINE                     *         
* CHGCOLOF DS    X                 OFFSET TO COL                      *         
* ESFDLN   MUST CONTAIN LENGTH OF MATCHED STRING                      *         
* ESCHLN   MUST CONTAIN LENGTH OF NEW STRING                          *         
* ESCHTXT  MUST CONTAIN NEW STRING                                    *         
***********************************************************************         
         SPACE 1                                                                
CHGTXT   DS    0H                                                               
         USING *,R8                                                             
         L     R6,CHGATX                                                        
F        USING TXD,R6              ??                                           
         GOTO1 MAKETX,R7                                                        
T        USING TXD,R7              ??                                           
         GOTO1 MVCTX,BOPARM,T.TXD,F.TXD                                         
         MVC   T.TXDATA,SPACES     ??                                           
         LA    R2,F.TXDATA         R2=A(LINE TO BE CHANGED)                     
         CLC   ESCHLN,ESFDLN       CHANGE STRING LEN > FIND STRING LEN?         
         BNH   CHGT10                                                           
         XR    RE,RE                                                            
         IC    RE,ESREPWD                                                       
         BCTR  RE,0                                                             
         LA    RF,0(RE,R2)         RF=A(LAST CHAR IN LINE)                      
         CLI   0(RF),C' '          NON SPACE CHAR FOUND?                        
         BH    *+8                                                              
         BCT   RE,*-12                                                          
         LA    RE,1(RE)            RE=L'(LINE TO LAST NON SPACE CHAR)           
         SR    R1,R1                                                            
         IC    R1,ESCHLN           ADD CHANGE STRING LENGTH                     
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,ESFDLN           SUBTRACT MATCHED STRING LENGTH               
         SR    RE,R1                                                            
         CLM   RE,1,ESREPWD                                                     
         BH    CHGTERRX            NEW LINE WOULD BE TOO LONG                   
*                                                                               
CHGT10   LA    R3,T.TXDATA         R3=A(AREA TO BE USED FOR NEW LINE)           
         SR    RF,RF                                                            
         ICM   RF,1,CHGCOLOF       RF=L'(LINE BEFORE MATCH STRING)              
         SH    RF,=H'2'                                                         
         BM    CHGT20                                                           
         EX    RF,*+4                                                           
         MVC   0(0,R3),0(R2)       GET LINE UPTO MATCH                          
         LA    R3,1(R3,RF)         R3=A(NEXT FREE CHAR IN NEW LINE)             
         LA    R2,1(R2,RF)                                                      
CHGT20   SR    RF,RF                                                            
         IC    RF,ESCHLN           RF=L'(CHANGE TEXT)                           
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R3),ESCHTXT     GET CHANGE STRING                            
         LA    R3,1(R3,RF)         R3=A(NEXT FREE CHAR IN NEW LINE)             
         SR    RE,RE                                                            
         IC    RE,ESFDLN           RF=L'(CHANGE TEXT)                           
         LA    R2,0(R2,RE)         R2=A(LINE AFTER MATCH)                       
         XR    RF,RF                                                            
         IC    RF,ESREPWD                                                       
         LA    RF,T.TXDATA(RF)                                                  
         SR    RF,R3               RF=(MAX REMAINING LEN FOR NEW LINE)          
         BZ    CHGT30                                                           
         XR    RE,RE                                                            
         IC    RE,ESREPWD                                                       
         LA    RE,F.TXDATA(RE)                                                  
         SR    RE,R2               RE=(REMAINING LEN FOR OLD LINE)              
         BZ    CHGT30                                                           
         CR    RE,RF                                                            
         BNL   *+6                                                              
         LR    RF,RE                                                            
         EX    RF,*+4                                                           
         MVC   0(0,R3),0(R2)       GET REST OF LINE                             
CHGT30   DS    0H                                                               
         TM    ESFCPAR,ESFCALQ     ALL PARAMETER IN USE?                        
         BO    CHGT40                                                           
         MVI   LRMODE,LRCHANGE     CHANGE RECORD                                
         MVC   LRLINE,CHGLINOF     LINE NUMBER                                  
         MVC   LRPARA,ESPARA       PARAGRAPH                                    
         GOTO1 ALINEREC,T.TXD                                                   
         B     CHGTX                                                            
CHGT40   DS    0H               UPDATE ORIGINAL LINE                            
         GOTO1 MVCTX,BOPARM,F.TXD,T.TXD ?? THIS IS SHITE                        
*                                                                               
CHGTX    B     EXITY                                                            
*                                                                               
CHGTERRX B     EXITN                                                            
         DROP  F,T                                                              
         EJECT                                                                  
***********************************************************************         
* SEARCH FOR STANDARD COMMENT WITHIN TEXT                             *         
*                                                                     *         
* NTRY: R1 = A(TXD)                                                   *         
***********************************************************************         
         SPACE 1                                                                
COMBLD   DS    0H                                                               
         USING *,R8                                                             
         LR    R7,R1                                                            
         USING TXD,R7                                                           
         TM    SC1IND,SC1IRCRS     TEST RECURSIVE CALL                          
         BO    COMBLD20                                                         
*                                                                               
         MVC   BCDUB,SPACES        CHECK FOR STANDARD COMMENT REQUIRED          
         MVC   BCDUB(1),MY@NRTV                                                 
         MVI   BCDUB+1,C'='        N=                                           
         MVI   BCDUB+2,C'+'                                                     
         MVI   BCDUB+3,C'='        +=                                           
         L     R1,ATRUP            R1=A(UPPER CASE TRANSLATE TABLE)             
         LA    R2,TXLINLAY                                                      
         USING LAYOUTD,R2                                                       
COMBLD02 CLI   LAYOUTD,EOT                                                      
         BE    EXITY               EOT - NONE FOUND                             
         CLI   LAYFLD,0            TEST NOT NUMERICAL FIELD                     
         BNE   COMBLD08                                                         
         XR    RF,RF                                                            
         IC    RF,LAYCOLF                                                       
         LA    RF,TXDATA-1(RF)                                                  
         XR    RE,RE                                                            
         IC    RE,LAYCOLN                                                       
         SH    RE,=Y(2)                                                         
         BNP   COMBLD08                                                         
COMBLD04 MVC   BCHALF,0(RF)                                                     
         TR    BCHALF(1),0(R1)                                                  
         CLC   BCHALF,BCDUB                                                     
         BE    COMBLD10                                                         
         CLC   BCHALF,BCDUB+2                                                   
         BE    COMBLD10                                                         
         LA    RF,1(RF)                                                         
         BCT   RE,COMBLD04                                                      
*                                                                               
COMBLD08 LA    R2,LAYOUTL(R2)                                                   
         B     COMBLD02                                                         
*                                                                               
COMBLD10 DS    0H                  STANDART COMMENT IS REQUIRED                 
         MVC   SCCODE,SPACES                                                    
         MVC   SCCODE,2(RF)                                                     
         TR    SCCODE,0(R1)                                                     
         LA    RE,TXDATA-1                                                      
         SR    RF,RE                                                            
         STC   RF,SCCOLF           SET STANDARD COMMENT COLUMN START            
         XR    RE,RE                                                            
         IC    RE,LAYCOLF                                                       
         SR    RF,RE                                                            
         IC    RE,LAYCOLN                                                       
         SR    RE,RF                                                            
         STC   RE,SCCOLN           SET COLUMNS AVAIALBLE FOR COMMENT            
         DROP  R2                                                               
*                                                                               
         LA    R3,SCTAB            FIND NEXT TABLE POSITION                     
         USING SCTABD,R3                                                        
         LA    R0,SCTABN                                                        
         CLI   SCTLIN,EOT                                                       
         BE    COMBLD20                                                         
         LA    R3,SCTABL(R3)                                                    
         BCT   R0,*-12                                                          
         MVC   FVMSGNO,=AL2(AE$TMIDT)                                           
         B     COMBLDE                                                          
*                                                                               
         USING SCMRECD,R2                                                       
COMBLD20 LA    R2,IOKEY            R2=A(COMMENT RECORD KEY)                     
         XC    SCMKEY,SCMKEY                                                    
         MVI   SCMKTYP,SCMKTYPQ    RECORD TYPE                                  
         MVC   SCMKCPY,CUABIN      COMPANY CODE                                 
         MVC   SCMKCODE,SPACES                                                  
         LA    RE,SCCODE+L'SCMKCODE-1                                           
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         LA    R1,SCCODE                                                        
         SR    RE,R1                                                            
         BM    COMBLD28                                                         
         LA    RF,L'SCMKCODE-1                                                  
         SR    RF,RE                                                            
         LA    RF,SCMKCODE(RF)                                                  
         EX    RE,*+4                                                           
         MVC   0(0,RF),SCCODE      STANDARD COMMENT NUMBER                      
COMBLD28 GOTO1 AIO,IO2+IOACCMST+IOREAD                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$RECNF)                                           
         B     COMBLDE                                                          
         MVC   FVXTRA,SPACES                                                    
         L     R2,AIO2             R2=A(STANDARD COMMENT RECORD)                
         TM    SC1IND,SC1IRCRS     TEST RECURSIVE CALL                          
         BO    COMBLD32            DON'T SET TABLE VALUES                       
         MVC   SCTCOD,SCMKCODE     SET CODE FROM KEY                            
         MVC   SCTCOLF,SCCOLF                                                   
         SR    RE,RE                                                            
         IC    RE,NXTLOFF          TAKE KNOWN START LINE                        
         SR    RF,RF                                                            
         IC    RF,SCLTOT           ADD TOTAL LINES ADDED THIS TIME              
         AR    RF,RE                                                            
         STC   RF,SCTLIN           SET SCREEN LINE NUMBER                       
COMBLD32 LA    R2,SCMRFST          R2=A(FIRST ELEMENT)                          
*                                                                               
         USING SCMELD,R2                                                        
COMBLD36 CLI   SCMEL,EOR                                                        
         BE    COMBLD52                                                         
         CLI   SCMEL,SCMELQ        TEST STANDARD COMMENT                        
         BE    COMBLD40                                                         
COMBLD38 SR    R0,R0                                                            
         IC    R0,SCMLN                                                         
         AR    R2,R0                                                            
         B     COMBLD36                                                         
*                                                                               
COMBLD40 TM    SCMTYPE,SCMTPRAD    TEST EMBEDDED CODE                           
         BO    COMBLD42                                                         
         MVC   BCHALF(1),MY@NRTV                                                
         MVI   BCHALF+1,C'='                                                    
         CLC   BCHALF,SCMCODE      CODE IS N=123456                             
         BNE   COMBLD48                                                         
COMBLD42 TM    SC1IND,SC1IRCRS     TEST RECURSING                               
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMLSC)                                           
         B     COMBLDE                                                          
         L     R0,AIO3             SAVE CURRENT COMMENT RECORD                  
         L     RE,AIO2                                                          
         LA    R1,IOAREALN                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVC   BCDUB,SPACES                                                     
         SR    R1,R1                                                            
         IC    R1,SCMLN                                                         
         LH    R0,=Y(SCMLN1Q+1)                                                 
         TM    SCMTYPE,SCMTPRAD    TEST EMBEDDED CODE                           
         BO    *+8                                                              
         LH    R0,=Y(SCMLN1Q+3)                                                 
         SR    R1,R0                                                            
         BM    COMBLD44                                                         
         CH    R1,=Y(L'BCDUB-1)                                                 
         BNH   *+8                                                              
         LH    R1,=Y(L'BCDUB-1)                                                 
         LA    RF,SCMCODE                                                       
         TM    SCMTYPE,SCMTPRAD    TEST EMBEDDED CODE                           
         BO    *+8                                                              
         LA    RF,SCMCODE+2                                                     
         EX    R1,*+4                                                           
         MVC   BCDUB(0),0(RF)                                                   
COMBLD44 OI    SC1IND,SC1IRCRS     SET RECURSING                                
         MVC   SCCODE,BCDUB                                                     
         GOTO1 ACOMBLD,TXD         RECURSE TO READ FOR THIS CODE                
         BNE   EXITN               MESSAGE WILL BE SET                          
         NI    SC1IND,FF-(SC1IRCRS)                                             
         L     R0,AIO2                                                          
         L     RE,AIO3                                                          
         LA    R1,IOAREALN                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE               RESTORE SAVED RECORD                         
         B     COMBLD38            CONTINUE LOOKING FOR ELEMENTS                
*                                                                               
COMBLD48 CLI   SCTNUM,FF           TEST TOO MANY COMMENT LINES                  
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMIDT)                                           
         B     COMBLDE                                                          
         SR    R0,R0                                                            
         IC    R0,SCMLN                                                         
         SH    R0,=Y(SCMLN1Q)                                                   
         SR    RF,RF               TEST COMMENT WILL FIT ON LINE                
         IC    RF,SCCOLN                                                        
         CR    R0,RF                                                            
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(GE$LONG)                                            
         B     COMBLDE                                                          
         SR    RF,RF                                                            
         IC    RF,SCTNUM           BUMP NUMBER OF COMMENT LINES                 
         LA    RF,1(RF)                                                         
         STC   RF,SCTNUM           BUMP BLOCK TOTAL IN TABLE                    
         B     COMBLD38                                                         
*                                                                               
COMBLD52 TM    SC1IND,SC1IRCRS     TEST RECURSING                               
         BO    COMBLDY                                                          
         SR    RF,RF                                                            
         ICM   RF,1,SCTNUM         TOTAL LINES IN THIS BLOCK                    
         BZ    COMBLDY                                                          
         BCTR  RF,0                FIRST LINE ALREADY EXISTS                    
         SR    R0,R0                                                            
         IC    R0,SCLTOT           TOTAL LINES ADDED SO FAR                     
         AR    RF,R0                                                            
         STC   RF,SCLTOT           UPDATE TOTAL LINES ADDED SO FAR              
*                                                                               
COMBLDY  B     EXITY                                                            
*                                                                               
COMBLDE  LA    R0,SCTAB            ERROR PROCEDURE                              
         LA    R1,SCTABN*SCTABL                                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR STANDARD COMMENT TABLE                 
*                                                                               
         MVC   FVXTRA(1),MY@NRTV                                                
         MVI   FVXTRA+1,C'='                                                    
         MVC   FVXTRA+2(L'SCMKCODE),SCCODE                                      
         MVC   FVADDR,TXATXT                                                    
         MVI   FVCURS,0                                                         
*                                                                               
         LA    R2,TXSCRLAY         FIND FIELD FOR CURSOR                        
         USING LAYOUTD,R2                                                       
         XR    R1,R1                                                            
         IC    R1,SCCOLF                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
COMBLDE2 CLI   LAYOUTD,EOT                                                      
         BE    COMBLDEX                                                         
         IC    RF,SCCOLF                                                        
         IC    RE,LAYCOLF                                                       
         SR    RF,RE                                                            
         BM    COMBLDEX                                                         
         CLM   RF,1,LAYCOLN                                                     
         BNH   *+12                                                             
         LA    R2,LAYOUTL(R2)                                                   
         B     COMBLDE2                                                         
*                                                                               
         STC   RF,FVERRNDX         SET DISPLACEMENT WITHIN FIELD                
         XR    RF,RF                                                            
         IC    RF,LAYFHD                                                        
         A     RF,TXATXT                                                        
         NI    FHIID(RF),FF-FHIIVA                                              
         OI    FHOID(RF),FHOITR                                                 
         ST    RF,FVADDR                                                        
         DROP  R2                                                               
*                                                                               
COMBLDEX B     EXITN                                                            
         SPACE 1                                                                
         DROP  R3,R7                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS A STANDARD COMMENT RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
COMPRC   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         TM    SC1IND,SC1IRCRS     TEST RECURSIVE CALL                          
         BO    COMPRC04                                                         
         GOTO1 MAKETX,R7           CREATE NEW TEXT LINE                         
         USING TXD,R7                                                           
         LA    R3,SCTAB            R3=A(STANDARD COMMENT TABLE)                 
         USING SCTABD,R3                                                        
         MVC   LRPARA,ESPARA       SET PARAGRAPH                                
         MVI   LRMODE,LRMERGE      FIRST LINE IS A MERGE                        
*                                                                               
COMPRC02 CLI   SCTABD,EOR                                                       
         BE    COMPRCX                                                          
         MVC   LRLINE,SCTLIN       SCREEN LINE NUMBER                           
         LA    R1,SCTCOD                                                        
*                                                                               
         USING SCMRECD,R2                                                       
COMPRC04 LA    R2,IOKEY            R2=A(COMMENT RECORD KEY)                     
         XC    SCMKEY,SCMKEY                                                    
         MVI   SCMKTYP,SCMKTYPQ    RECORD TYPE                                  
         MVC   SCMKCPY,CUABIN      COMPANY CODE                                 
         MVC   SCMKCODE,0(R1)                                                   
         GOTO1 AIO,IO2+IOACCMST+IOREAD                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2             R2=A(STANDARD COMMENT RECORD)                
         LA    R2,SCMRFST          R2=A(FIRST ELEMENT)                          
*                                                                               
         USING SCMELD,R2                                                        
COMPRC08 CLI   SCMEL,EOR                                                        
         BE    COMPRC24                                                         
         CLI   SCMEL,SCMELQ        TEST STANDARD COMMENT                        
         BE    COMPRC12                                                         
COMPRC10 SR    R0,R0                                                            
         IC    R0,SCMLN                                                         
         AR    R2,R0                                                            
         B     COMPRC08                                                         
*                                                                               
COMPRC12 TM    SCMTYPE,SCMTPRAD    TEST EMBEDDED CODE                           
         BO    COMPRC14                                                         
         MVC   BCHALF(1),MY@NRTV                                                
         MVI   BCHALF+1,C'='                                                    
         CLC   BCHALF,SCMCODE      CODE IS N=123456                             
         BNE   COMPRC20                                                         
COMPRC14 TM    SC1IND,SC1IRCRS     TEST RECURSING                               
         BZ    *+6                                                              
         DC    H'0'                TOO MANY LEVELS OF EMBEDDING                 
         L     R0,AIO3             SAVE CURRENT COMMENT RECORD                  
         L     RE,AIO2                                                          
         LA    R1,IOAREALN                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVC   BCDUB,SPACES                                                     
         SR    RE,RE                                                            
         IC    RE,SCMLN                                                         
         LH    R0,=Y(SCMLN1Q+1)                                                 
         TM    SCMTYPE,SCMTPRAD    TEST EMBEDDED CODE                           
         BO    *+8                                                              
         LH    R0,=Y(SCMLN1Q+3)                                                 
         SR    RE,R0                                                            
         BM    COMPRC16                                                         
         CH    RE,=Y(L'BCDUB-1)                                                 
         BNH   *+8                                                              
         LH    RE,=Y(L'BCDUB-1)                                                 
         LH    RF,=Y(L'SCMKCODE-1)                                              
         SR    RF,RE                                                            
         BNM   *+6                                                              
         SR    RF,RF                                                            
         LA    RF,BCDUB(RF)                                                     
         LA    R1,SCMCODE                                                       
         TM    SCMTYPE,SCMTPRAD    TEST EMBEDDED CODE                           
         BO    *+8                                                              
         LA    R1,SCMCODE+2                                                     
         EX    RE,*+4                                                           
         MVC   0(0,RF),0(R1)       STANDARD COMMENT NUMBER                      
COMPRC16 OI    SC1IND,SC1IRCRS     SET RECURSING                                
         GOTO1 ACOMPRC,BCDUB       RECURSE TO READ FOR THIS CODE                
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    SC1IND,FF-(SC1IRCRS)                                             
         L     R0,AIO2                                                          
         L     RE,AIO3                                                          
         LA    R1,IOAREALN                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE               RESTORE SAVED RECORD                         
         B     COMPRC10            CONTINUE LOOKING FOR ELEMENTS                
*                                                                               
COMPRC20 GOTO1 AINITX,BOPARM,TXD   INITIALIZE NEW TEXT LINE                     
*                                                                               
         SR    RE,RE                                                            
         IC    RE,SCTCOLF                                                       
         LA    RF,TXDATA-1(RE)                                                  
         SR    R1,R1                                                            
         IC    R1,SCMLN                                                         
         SH    R1,=Y(SCMLN1Q)                                                   
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,RF),SCMCODE     COPY COMMENT INTO TEXT LINE                  
*                                                                               
         MVC   LRCOLF,SCTCOLF                                                   
         GOTO1 ALINEREC,TXD                                                     
*                                                                               
         MVI   LRMODE,LRADDQ       SUBSEQUENT LINES ARE ADDS                    
         SR    RF,RF                                                            
         IC    RF,LRLINE                                                        
         LA    RF,1(RF)                                                         
         STC   RF,LRLINE           SET NEXT LINE                                
         B     COMPRC10            CONTINUE LOOKING FOR ELEMENTS                
*                                                                               
COMPRC24 TM    SC1IND,SC1IRCRS     TEST RECURSING                               
         BO    COMPRCX                                                          
         LA    R3,SCTABL(R3)                                                    
         MVI   LRMODE,LRMERGE      NEXT ENTRY FIRST LINE IS A MERGE             
         B     COMPRC02                                                         
*                                                                               
COMPRCX  B     EXITY                                                            
         SPACE 1                                                                
         DROP  R3,R7                                                            
         EJECT                                                                  
***********************************************************************         
* ROUITNE TO CLEAR TWA                                                *         
*                                                                     *         
* NTRY: R1 = A(POINT IN TWA)                                          *         
***********************************************************************         
         SPACE 1                                                                
CLRTWA   DS    0H                                                               
         USING *,R8                                                             
         LR    RE,R1                                                            
         LA    RF,OSVALS+OSVALSL   ??                                           
         SR    RF,RE                                                            
         XR    R1,R1                                                            
         XR    R0,R0                                                            
         MVCL  RE,R0                                                            
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO BUILD TOP LINE OF SCREEN                                 *         
***********************************************************************         
         SPACE 1                                                                
BLDTOP   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         IC    RF,ESCOL#1          SET LAST SCREEN COLUMN NUMBER                
         LA    RE,MAXSCOLQ-1(RF)                                                
         CLM   RE,1,ESREPWD                                                     
         BNH   *+8                                                              
         IC    RE,ESREPWD                                                       
         STC   RE,ESCOL#X                                                       
         SR    RE,RF                                                            
         LA    RE,1(RE)                                                         
         STC   RE,ES#COL           SET NUMBER OF COLUMNS ON LINE                
*                                                                               
         GOTO1 ASETLAD             INITITILAIZE                                 
*                                                                               
         TM    ESCLFLG,ESCLRLQ     TEST SHOW RULER                              
         BZ    BTOP02                                                           
         L     R2,SLALINE                                                       
         USING TOPBANH,R2                                                       
         MVI   TOPBANH+FHLND,L'TOPBANH+L'TOPBAN                                 
         MVI   TOPBANH+FHATD,FHATLC+FHATPR+FHATHI                               
         MVI   TOPBANH+FHADD+1,5                                                
         MVI   TOPBANH+FHOID,FHOITR                                             
         MVC   TOPBAN,SPACES                                                    
         XR    RF,RF                                                            
         IC    RF,ESCOL#1                                                       
         LA    RF,RULER-1(RF)                                                   
         IC    RE,ES#COL                                                        
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   TOPBAN(0),0(RF)                                                  
         GOTO1 ASETLAD                                                          
         DROP  R2                                                               
*                                                                               
BTOP02   L     R2,SLALINE                                                       
         ST    R2,ESAHEAD1                                                      
         USING TOPBANH,R2                                                       
         MVI   TOPBANH+FHLND,L'TOPBANH+L'TOPBAN                                 
         MVI   TOPBANH+FHATD,FHATLC+FHATPR+FHATHI                               
         MVI   TOPBANH+FHADD+1,5                                                
         MVI   TOPBANH+FHOID,FHOITR                                             
         MVC   TOPBAN,SPACES                                                    
         GOTO1 ASETLAD                                                          
         DROP  R2                                                               
*                                                                               
         L     R2,SLALINE                                                       
         ST    R2,ESATOP                                                        
         USING TOPD,R2                                                          
         LH    R4,=Y(BSDICT-TWAD)                                               
         LA    R4,TWAD(R4)                                                      
         USING BSDICT,R4                                                        
*                                                                               
         MVI   TOPACTH+FHLND,L'TOPACTH+L'TOPACT                                 
         MVI   TOPACTH+FHATD,FHATLC+FHATPR+FHATHI                               
         MVI   TOPACTH+FHADD+1,1                                                
         MVI   TOPACTH+FHOID,FHOITR                                             
         MVC   TOPACT,LC@ACT                                                    
*                                                                               
         MVI   TOPBANH+FHLND,L'TOPBANH+L'TOPBAN                                 
         MVI   TOPBANH+FHATD,FHATLC+FHATPR+FHATHI                               
         MVI   TOPBANH+FHADD+1,5                                                
         MVI   TOPBANH+FHOID,FHOITR                                             
         MVC   TOPBAN,SPACES                                                    
*                                                                               
         MVI   TOPNUMH+FHLND,L'TOPNUMH+L'TOPNUM                                 
         MVI   TOPNUMH+FHATD,FHATLC+FHATPR+FHATHI                               
         MVI   TOPNUMH+FHADD+1,75                                               
         MVI   TOPNUMH+FHOID,FHOITR                                             
         MVC   TOPNUM,LC4LINE                                                   
*                                                                               
         GOTO1 ASETLAD                                                          
*                                                                               
         B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD END-OF-PARAGRAPH LINE                              *         
***********************************************************************         
         SPACE 1                                                                
BLDEND   DS    0H                                                               
         USING *,R8                                                             
         L     R2,SLALINE                                                       
         USING TOPBANH,R2                                                       
         MVI   TOPBANH+FHLND,L'TOPBANH+L'TOPBAN                                 
         MVI   TOPBANH+FHATD,FHATHI+FHATLC+FHATPR                               
         MVI   TOPBANH+FHADD+1,5                                                
         MVI   TOPBANH+FHOID,FHOITR                                             
*                                                                               
         XR    RE,RE                                                            
         IC    RE,ESREPWD          SET TO DASHES                                
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   BOELEM,DASHES                                                    
         SRL   RE,1                YES - SET TO -----END OF PARA-----           
         LR    R0,RE                                                            
         SRL   RE,1                                                             
         AR    RE,R0                                                            
         SRL   RE,1                                                             
         SH    RE,=Y(L'LC@ENPAR/2)                                              
         LA    R3,BOELEM(RE)                                                    
         LH    RE,=Y(LC@ENPAR-TWAD)                                             
         LA    RE,TWAD(RE)                                                      
         MVC   0(L'LC@ENPAR,R3),0(RE)                                           
*                                                                               
         MVC   TOPBAN,SPACES       COPY HEADER INTO BANNER                      
         XR    RF,RF                                                            
         IC    RF,ESCOL#1                                                       
         LA    RF,BOELEM-1(RF)                                                  
         IC    RE,ES#COL                                                        
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   TOPBAN(0),0(RF)                                                  
*                                                                               
         CLI   ESCOL#1,1           LEFT MOST COLUMN DISPLAYED?                  
         BE    *+10                                                             
         MVC   TOPBAN(L'LHSMORE),LHSMORE                                        
         CLC   ESCOL#X,ESREPWD     RIGHT MOST COLUMN DISPLAYED                  
         BE    *+10                                                             
         MVC   TOPBAN+L'TOPBAN-L'RHSMORE(L'RHSMORE),RHSMORE                     
*                                                                               
         GOTO1 ASETLAD                                                          
*                                                                               
         TM    ESFCFLG,ESFCFNQ+ESFCCHQ FIND/CHANGE MODE?                        
         BNZ   BLDENDX                                                          
         LA    RE,TOPBANH ??       RE=A(SCREEN LINE AFTER END OF TEXT)          
         S     RE,ATWA   ??                                                     
         DROP  R2                                                               
         CLM   RE,3,ESCURD         WILL CURSOR BE ON PROTECTED SCREEN?          
         BH    BEND12                                                           
         L     RE,ESACOMX          SET CURS TO LAST SCREEN LINE                 
         S     RE,ATWA                                                          
         L     RF,AINP                                                          
         MVI   TIOBCURI-TIOBD(RF),0                                             
         B     BEND16                                                           
BEND12   L     RE,ESANUMX                                                       
         S     RE,ATWA             RE=D(LINE NUM FLD ON LAST WITH TEXT)         
         CLM   RE,3,ESCURD         CURSOR ON LAST LINE NUMBER FIELD?            
         BNH   BEND14                                                           
         L     RE,ESATXTX                                                       
         S     RE,ATWA                                                          
         CLM   RE,3,ESCURD         CURSOR ON LAST TEXT LINE FIELD?              
         BNH   BEND14                                                           
         L     RE,ESACOMX                                                       
         S     RE,ATWA                                                          
         CLM   RE,3,ESCURD         CURSOR ON LAST LINE COMMAND FIELD?           
         BH    *+12                                                             
BEND14   L     RF,AINP                                                          
BEND16   STCM  RE,3,TIOBCURD-TIOBD(RF) OVERRIDE DEFAULT CURSOR CONTROL          
*                                                                               
BLDENDX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD FOOTLINE                                           *         
***********************************************************************         
         SPACE 1                                                                
BLDFOOT  DS    0H                                                               
         USING *,R8                                                             
         L     R2,SLALINE          R2=A(LINE)                                   
         USING FHD,R2                                                           
*                                                                               
         MVI   FHLN,FHDAD+79+FHDAD                                              
         MVI   FHAT,FHATHI+FHATXH+FHATLC+FHATPR                                 
         MVI   FHAD+1,+1                                                        
         MVI   FHOI,FHOITR                                                      
         MVI   FHDA+79,FF                                                       
         LA    R2,FHDAD+79+FHDAD(R2)                                            
         MVC   FHD(L'EOS),EOS                                                   
*                                                                               
         MVC   SLAD,=AL2(ROWS#Q*COLS#Q-COLS#Q)                                  
         GOTO1 ASETLAD                                                          
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET DATA SCREEN ADDRESS FOR NEW LINE                     *         
*                                                                     *         
* NTRY: SLALINE = A(TWA LINE)                                         *         
*          SLAD = DATA SCREEN ADDRESS                                 *         
***********************************************************************         
         SPACE 1                                                                
SETLAD   DS    0H                                                               
         USING *,R8                                                             
         ICM   R1,15,SLALINE                                                    
         BNZ   *+14                                                             
         LA    R1,EDTOLAYH                                                      
         MVC   SLAD,=Y(COLS#Q*5)                                                
         USING FHD,R1                                                           
*                                                                               
         LH    R3,SLAD                                                          
         XR    RF,RF                                                            
SLAD02   ICM   RF,1,FHLN                                                        
         BZ    SLAD04                                                           
         LH    RE,FHAD                                                          
         AR    RE,R3                                                            
         STH   RE,FHAD                                                          
         BXH   R1,RF,SLAD02                                                     
*                                                                               
SLAD04   ST    R1,SLALINE                                                       
         LA    R3,COLS#Q(R3)                                                    
         STH   R3,SLAD                                                          
*                                                                               
         B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INITIALZE NEW TX TO TEXT DEFAULT                         *         
*                                                                     *         
* NTRY: P1 BYTE 0 = TXLSTA VALUE                                      *         
*             1-3 = A(TXD)                                            *         
***********************************************************************         
         SPACE 1                                                                
INITX    DS    0H                                                               
         USING *,R8                                                             
         XR    R3,R3                                                            
         IC    R3,0(R1)                                                         
         XR    R2,R2                                                            
         ICM   R2,7,1(R1)                                                       
         USING TXD,R2                                                           
*                                                                               
         LA    RE,TXD              CLEAR TXD                                    
         LA    RF,TXL                                                           
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         STC   R3,TXLSTA                                                        
*                                                                               
         MVC   TXDATA,SPACES       SPACERIZE TEXT                               
         ZAP   TXAMT,BCPZERO       ZEROISE AMOUNTS                              
         MVC   TXAMT+L'TXAMT(TXAMTS-L'TXAMT),TXAMT                              
*                                                                               
         CLI   TXLSTA,0            TEST FREE-FORM TEXT LINE                     
         BE    ITXT02                                                           
         TM    TXLSTA,PBRLNUM      TEST NUMERICAL LINE                          
         BO    ITXT04                                                           
         TM    TXLSTA,PBRLTOT      TEST TOTAL LINE                              
         BO    ITXT06                                                           
         TM    TXLSTA,PBRLSUB      TEST SUB-TOTAL LINE                          
         BO    ITXT08                                                           
         DC    H'0'                                                             
*                                                                               
ITXT02   MVC   TXLINLAY(L'ESTXTLAY),ESTXTLAY                                    
         B     EXIT                                                             
*                                                                               
ITXT04   MVC   TXNUM,ESNUMTX                                                    
         MVC   TXLINLAY,ESNUMLAY                                                
         B     EXIT                                                             
*                                                                               
ITXT06   MVC   TXNUM,ESTOTTX                                                    
         MVC   TXLINLAY,ESTOTLAY                                                
         B     EXIT                                                             
*                                                                               
ITXT08   MVC   TXNUM,ESTOTTX                                                    
         MVC   TXLINLAY,ESTOTLAY                                                
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INITIALZE TXD FROM LINE RECORD                           *         
*                                                                     *         
* NTRY: P1 = A(TXD)                                                   *         
*       P2 = A(LINE RECORD)                                           *         
***********************************************************************         
         SPACE 1                                                                
GETTX    DS    0H                                                               
         USING *,R8                                                             
         LM    R2,R3,0(R1)                                                      
         USING TXD,R2                                                           
         USING PBRRECD,R3                                                       
         GOTO1 AINITX,BOPARM,TXD                                                
         MVC   TXLINE#,PBRKLINE                                                 
         MVC   TXLSTA,PBRRLSTA     COPY LINE STATUS                             
         XC    TXLINLAY,TXLINLAY                                                
         MVC   TXLINLAY(L'ESTXTLAY),ESTXTLAY                                    
         LA    R6,PBRRFST                                                       
*                                                                               
GTX02    CLI   0(R6),0                                                          
         BE    GTX10                                                            
*                                                                               
         USING FFTELD,R6                                                        
         CLI   FFTEL,FFTELQ                                                     
         BNE   GTX04                                                            
         CLI   FFTTYPE,FFTTBLIN                                                 
         BNE   GTX08                                                            
         IC    RE,FFTDLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   TXDATA(0),FFTDATA                                                
         B     GTX08                                                            
*                                                                               
         USING FWTELD,R6                                                        
GTX04    CLI   FWTEL,FWTELQ                                                     
         BNE   GTX08                                                            
         GOTO1 AGETNUM,BOPARM,FWTFLD                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R7,0(R1)                                                         
         USING NUMTABD,R7                                                       
         LH    RF,NUMAMT                                                        
         LA    RF,TXD(RF)                                                       
         ZAP   0(L'TXAMT,RF),FWTAMT                                             
         CLI   FWTCOLF,0           COLUMN ZERO MEANS FIELD NOT ON LINE          
         BE    GTX08                                                            
         OC    TXNUM,NUMTX                                                      
         GOTO1 AINSLAY,BOPARM,TXLINLAY,FWTFLD,FWTCOLF,FWTCOLN                   
         DROP  R6,R7                                                            
*                                                                               
GTX08    XR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         BXH   R6,RF,GTX02                                                      
*                                                                               
GTX10    DS    0H                                                               
*                                                                               
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT TXD INTO LINE RECORD                                 *         
*                                                                     *         
* NTRY: P1 = A(TXD)                                                   *         
*       P2 = A(LINE RECORD)                                           *         
***********************************************************************         
         SPACE 1                                                                
PUTTX    DS    0H                                                               
         USING *,R8                                                             
         LM    R2,R3,0(R1)                                                      
         USING TXD,R2                                                           
         USING PBRRECD,R3                                                       
         CLI   PBRKLINE,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   PBRRLEN,=AL2(PBRRFST+1-PBRRECD)                                  
         MVI   PBRRFST,0                                                        
         MVC   PBRRLSTA,TXLSTA     COPY LINE STATUS                             
*                                                                               
         PUSH  USING                                                            
         USING FFTELD,BOELEM                                                    
         XC    FFTELD(FFTLN1Q),FFTELD                                           
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTTYPE,FFTTBLIN                                                 
         IC    RE,ESREPWD                                                       
         STC   RE,FFTDLEN                                                       
         EX    RE,*+4                                                           
         MVC   FFTDATA(0),TXDATA                                                
         LA    RE,FFTLN1Q+1(RE)                                                 
         STC   RE,FFTLN                                                         
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),PBRRECD,FFTELD                       
         POP   USING                                                            
*                                                                               
         CLI   TXNUM,0             TEST NUMBERS ON LINE                         
         BE    PTX20                                                            
         PUSH  USING                                                            
         USING FWTELD,BOELEM                                                    
         XC    FWTELD(FWTLNQ),FWTELD                                            
         MVI   FWTEL,FWTELQ                                                     
         MVI   FWTLN,FWTLNQ                                                     
         LA    R4,TXLINLAY                                                      
         USING LAYOUTD,R4                                                       
*                                                                               
PTX02    CLI   LAYOUTD,EOT         ADD AMOUNTS ON LINE                          
         BE    PTX10                                                            
         CLI   LAYFLD,0                                                         
         BE    PTX08                                                            
         MVC   FWTFLD,LAYFLD                                                    
         MVC   FWTCOLF,LAYCOLF                                                  
         MVC   FWTCOLN,LAYCOLN                                                  
         GOTO1 AGETNUM,BOPARM,LAYFLD                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,0(R1)                                                         
         LH    RF,NUMAMT-NUMTABD(R1)                                            
         LA    RF,TXD(RF)                                                       
         ZAP   FWTAMT,0(L'TXAMT,RF)                                             
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),PBRRECD,FWTELD                       
PTX08    LA    R4,LAYOUTL(R4)                                                   
         B     PTX02                                                            
         DROP  R4                                                               
*                                                                               
PTX10    TM    TXLSTA,PBRLTOT+PBRLSUB                                           
         BNZ   PTX20               TEST TOTAL LINE                              
         L     R4,ANUMTAB          NO - ADD OTHER REQUIRED AMOUNTS              
         USING NUMTABD,R4                                                       
         MVI   FWTCOLF,0                                                        
         MVI   FWTCOLN,0                                                        
PTX12    CLI   NUMTABD,EOT                                                      
         BE    PTX20                                                            
         MVC   BOBYTE1,TXNUM       TEST FWTEL ALREADY ADDED FOR AMOUNT          
         NC    BOBYTE1,NUMTX                                                    
         BNZ   PTX18                                                            
         MVC   BOBYTE1,TXNUM       TEST ADD FWTEL...                            
         NC    BOBYTE1,NUMMASK     ...EVEN THOUGH NOT DISPLAYED                 
         BZ    PTX18                                                            
         MVC   FWTFLD,NUMFLD                                                    
         LH    RF,NUMAMT                                                        
         LA    RF,TXD(RF)                                                       
         ZAP   FWTAMT,0(L'TXAMT,RF)                                             
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),PBRRECD,FWTELD                       
PTX18    LA    R4,NUMTABL(R4)                                                   
         B     PTX12                                                            
         POP   USING                                                            
*                                                                               
PTX20    B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD TXD LINE ONTO SCREEN                               *         
*                                                                     *         
* NTRY: P1 = A(TXD)                                                   *         
***********************************************************************         
         SPACE 1                                                                
BLDTX    DS    0H                                                               
         USING *,R8                                                             
         L     R2,0(R1)                                                         
         USING TXD,R2                                                           
*                                  FORMAT LAYOUT FOR SCREEN                     
         GOTO1 ASCRLAY,BOPARM,TXD                                               
*                                                                               
         L     R6,SLALINE                                                       
         USING COMFLDD,R6          BUILD COMMAND LINE                           
         ST    R6,TXACOM                                                        
         MVI   COMFLDH+FHLND,COMFLDL                                            
         MVI   COMFLDH+FHATD,FHATHI+FHATXH                                      
         MVI   COMFLDH+FHADD+1,1                                                
         MVI   COMFLDH+FHOID,FHOITR                                             
         MVI   COMFLDX,1                                                        
         MVC   COMFLD,SPACES                                                    
         TM    TXINDS,TXIINS                                                    
         BZ    *+10                                                             
         MVC   COMFLD,INSDOTS                                                   
         LA    R6,COMFLDL(R6)                                                   
*                                                                               
         ST    R6,TXATXT                                                        
         USING FHD,R6              BUILD TEXT FIELDS                            
         LA    R4,TXSCRLAY                                                      
         USING LAYOUTD,R4                                                       
         XR    R0,R0                                                            
         IC    R0,ESCOL#1                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
*                                                                               
BTX04    CLI   LAYOUTD,EOT                                                      
         BE    BTX10                                                            
*                                                                               
         IC    RF,LAYCOLN                                                       
         LA    RE,FHDAD(RF)                                                     
         STC   RE,FHLN                                                          
         MVI   FHAT,FHATLC                                                      
         CLI   LAYFLD,0                                                         
         BE    *+8                                                              
         OI    FHAT,FHATHI         HIGHLIGHT AMOUNT FIELDS                      
         TM    LAYINDS,LAYIPR                                                   
         BZ    *+12                                                             
         OI    FHAT,FHATPR                                                      
         NI    FHAT,FF-FHATHI                                                   
         MVI   FHOI,FHOITR                                                      
         EX    RF,*+4                                                           
         MVC   FHDA(0),SPACES                                                   
         OI    FHII,FHIIVA                                                      
         IC    RE,LAYCOLF                                                       
         SR    RE,R0                                                            
         LA    RE,5(RE)                                                         
         STC   RE,FHAD+1                                                        
         LA    R6,FHDA(RF)                                                      
*                                                                               
         LA    R4,LAYOUTL(R4)                                                   
         B     BTX04                                                            
         DROP  R4                                                               
*                                                                               
         USING LNUMFLDD,R6         BUILD NUMBER FIELD                           
BTX10    ST    R6,TXANUM                                                        
         MVI   LNUMFLDH+FHLND,LNUMFLDL                                          
         MVI   LNUMFLDH+FHATD,FHATPR                                            
         MVI   LNUMFLDH+FHADD+1,76                                              
         MVI   LNUMFLDH+FHOID,FHOITR                                            
         MVC   LNUMFLD(L'LNUMFLD-1),INSDOTS                                     
         TM    TXINDS,TXIINS                                                    
         BO    BTX12                                                            
         TM    TXLSTA,PBRLTOT+PBRLSUB                                           
         BNZ   *+8                                                              
         OI    LNUMFLDH+FHATD,FHATHI                                            
         XR    RE,RE                                                            
         IC    RE,NXTLOFF                                                       
         CVD   RE,BODUB1                                                        
         UNPK  LNUMFLD(L'LNUMFLD-1),BODUB1                                      
         OI    LNUMFLD+L'LNUMFLD-2,C'0'                                         
*                                                                               
BTX12    MVI   LNUMFLD+L'LNUMFLD-1,C' '                                         
         TM    TXLSTA,PBRLTOT+PBRLSUB                                           
         BZ    BTX14                                                            
         MVI   LNUMFLD+L'LNUMFLD-1,C'T'                                         
         TM    TXLSTA,PBRLSUB                                                   
         BZ    *+8                                                              
         MVI   LNUMFLD+L'LNUMFLD-1,C'S'                                         
         B     BTX20                                                            
BTX14    CLI   TXNUM,0                                                          
         BE    BTX20                                                            
         MVI   LNUMFLD+L'LNUMFLD-1,C'#'                                         
         DROP  R6                                                               
*                                                                               
BTX20    GOTO1 ASETLAD                                                          
*                                                                               
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY TXD ONTO TEXT LINE                               *         
*                                                                     *         
* NTRY: P1 = A(TXD)                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISTX    DS    0H                                                               
         USING *,R8                                                             
         L     R2,0(R1)                                                         
         USING TXD,R2                                                           
*                                                                               
         LA    R4,TXSCRLAY                                                      
         USING LAYOUTD,R4                                                       
*                                                                               
         L     R6,TXATXT                                                        
         USING FHD,R6                                                           
         XR    RF,RF                                                            
*                                                                               
DTX02    CLI   LAYOUTD,EOT                                                      
         BE    DISTXX                                                           
         XR    R3,R3                                                            
         IC    R3,LAYCOLF                                                       
         LA    R3,TXDATA-1(R3)                                                  
         IC    RF,LAYCOLN                                                       
         STC   RF,FHIL                                                          
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   FHDA(0),0(R3)                                                    
         OI    FHII,FHIIVA                                                      
*                                                                               
         CLI   LAYFLD,0            TEST NUMERICAL INPUT FIELD                   
         BE    DTX08                                                            
         TM    LAYINDS,LAYIPR                                                   
         BO    DTX08                                                            
         EX    RF,*+8                                                           
         BE    DTX08                                                            
         CLC   FHDA(0),BCSPACES    TEST EMPTY FIELD                             
         LA    RE,FHDA(RF)         LEFT JUSTIFY FIELD                           
         BCTR  RF,0                                                             
DTX06    CLI   FHDA,C' '                                                        
         BNE   DTX08                                                            
         EX    RF,*+12                                                          
         MVI   0(RE),C' '                                                       
         B     DTX06                                                            
         MVC   FHDA(0),FHDA+1                                                   
*                                                                               
DTX08    LA    R4,LAYOUTL(R4)                                                   
         IC    RF,FHLN                                                          
         BXH   R6,RF,DTX02                                                      
*                                                                               
DISTXX   B     EXIT                                                             
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE TXD TEXT LINE                                   *         
*                                                                     *         
* NTRY: P1 = A(TXD)                                                   *         
***********************************************************************         
         SPACE 1                                                                
VALTX    DS    0H                                                               
         USING *,R8                                                             
         L     R2,0(R1)                                                         
         USING TXD,R2                                                           
*                                                                               
         LA    R4,TXSCRLAY                                                      
         USING LAYOUTD,R4                                                       
*                                                                               
         L     R6,TXATXT                                                        
         USING FHD,R6                                                           
         NI    TXINDS,FF-TXIITH                                                 
         MVI   TXNINP,0                                                         
*                                                                               
VTX02    CLI   LAYOUTD,EOT                                                      
         BE    VTX10                                                            
         TM    FHII,FHIIVA         TEST INPUT THIS TIME                         
         BZ    *+12                                                             
         TM    FHII,FHIITH                                                      
         BZ    VTX08                                                            
         OI    TXINDS,TXIITH                                                    
         XR    R3,R3                                                            
         IC    R3,LAYCOLF                                                       
         LA    R3,TXDATA-1(R3)                                                  
         IC    RF,LAYCOLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R3),SPACES                                                   
         ICM   RF,1,FHIL                                                        
         BZ    VTX04                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R3),FHDA                                                     
*                                                                               
VTX04    CLI   LAYFLD,0                                                         
         BE    VTX08                                                            
         GOTO1 AGETNUM,BOPARM,LAYFLD                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)            SET NUMBER INPUT TO                          
         OC    TXNINP,NUMTX-NUMTABD(RF)                                         
*                                                                               
VTX08    LA    R4,LAYOUTL(R4)                                                   
         XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         BXH   R6,RF,VTX02                                                      
         DROP  R4,R6                                                            
*                                                                               
VTX10    CLI   TXNINP,0            TEST FOR ANY NUMBERS INPUT                   
         BE    VTX30                                                            
         L     R3,ANUMTAB          VALIDATE NUMBERS IN ORDER OF TABLE           
         USING NUMTABD,R3                                                       
VTX12    CLI   NUMTABD,EOT                                                      
         BE    VTX20                                                            
         MVC   BOBYTE1,TXNINP      TEST THIS NUMBER INPUT                       
         NC    BOBYTE1,NUMTX                                                    
         BZ    VTX18                                                            
         GOTO1 AVALNUM,BOPARM,TXD,NUMTABD                                       
         BNE   EXITN                                                            
VTX18    LA    R3,NUMTABL(R3)                                                   
         B     VTX12                                                            
         DROP  R3                                                               
*                                                                               
VTX20    GOTO1 AFMTTX,BOPARM,TXD                                                
*                                                                               
VTX30    GOTO1 ADISTX,BOPARM,TXD                                                
*                                                                               
VALTXX   B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INITIALIZE FOR TSAR RECORDS                              *         
***********************************************************************         
         SPACE 1                                                                
INITSAR  DS    0H                                                               
         USING *,R8                                                             
         L     R7,ALSVALS                                                       
         USING LSVALSD,R7                                                       
         USING TLSTD,LSTLST                                                     
         GOTO1 MAKETX,R6                                                        
         USING TXD,R6                                                           
*                                                                               
ITSAR02  CLC   BCTSHIGH,CSPSRECN                                                
         BE    ITSAR04                                                          
         BH    *+6                                                              
         DC    H'0'                                                             
         MVC   TLNUM,BCTSHIGH                                                   
         GOTO1 ATSARIO,TSAGET                                                   
         GOTO1 (RF),TSADEL                                                      
         B     ITSAR02                                                          
*                                                                               
ITSAR04  DS    0H                                                               
         XC    TLKEY,TLKEY         THIS SHOULDN'T BE NECESSARY                  
         MVC   TLKSES,TWASESNL     BUT OCCASIONALLY IS                          
         GOTO1 ATSARIO,TSARDH                                                   
         BL    ITSAR06                                                          
         CLC   TLKSES,TWASESNL                                                  
         BNE   ITSAR06                                                          
         DC    H'0'                                                             
*        GOTO1 ATSARIO,TSADEL                                                   
*        B     ITSAR04                                                          
*                                                                               
ITSAR06  DS    0H                                                               
         GOTO1 AINITX,BOPARM,TXD                                                
*                                                                               
         XC    TLKEY,TLKEY                                                      
         MVC   TLKSES,TWASESNL                                                  
         LA    RE,TLDATA                                                        
         LA    RF,TXL                                                           
         LA    R0,TXD                                                           
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         MVC   TLRLEN,=AL2(TXL+TLDATA-TLREC)                                    
         LH    R3,CSPSRECN                                                      
         LA    R0,MAXSLINQ                                                      
         LA    R2,1                                                             
ITSAR08  STCM  R2,3,TLKSEQ                                                      
         GOTO1 ATSARIO,TSAADD                                                   
         LA    R3,1(R3)                                                         
         CLM   R3,3,TLNUM                                                       
         BE    ITSAR10                                                          
         DC    H'0'                                                             
*                                                                               
ITSAR10  LA    R2,1(R2)                                                         
         BCT   R0,ITSAR08                                                       
*                                                                               
         LH    RE,CSPSRECN                                                      
         LA    RE,MAXSLINQ(RE)                                                  
         CH    RE,CSHIRECN                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R7,R6                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SAVE TXD ON TSAR RECORD                                  *         
*                                                                     *         
* NTRY: TSARLIN# = SCREEN LINE #                                      *         
*             P1 = A(TXD)                                             *         
* EXIT: TSARLIN# = TSARLIN# + 1                                       *         
***********************************************************************         
         SPACE 1                                                                
SAVTX    DS    0H                                                               
         USING *,R8                                                             
         L     R7,ALSVALS                                                       
         USING LSVALSD,R7                                                       
         USING TLSTD,LSTLST                                                     
         L     R3,0(R1)                                                         
         USING TXD,R3                                                           
*                                                                               
         CLI   TSARLIN#,0          TEST FIRST LINE                              
         BNE   STX02                                                            
         MVC   ESACOM1,TXACOM      YES SET A(FIRST LINE FIELDS)                 
         MVC   ESATXT1,TXATXT                                                   
         MVC   ESANUM1,TXANUM                                                   
         B     STX04                                                            
*                                                                               
STX02    CLC   TXACOM,ESACOMX      TEST LAST LINE                               
         BNH   STX06                                                            
*                                                                               
STX04    MVC   ESACOMX,TXACOM      SET A(LAST LINE FIELDS)                      
         MVC   ESATXTX,TXATXT                                                   
         MVC   ESANUMX,TXANUM                                                   
*                                                                               
STX06    MVC   TXTWA,ATWA                                                       
         XR    RE,RE                                                            
         IC    RE,TSARLIN#                                                      
         LA    RF,ESSCRLST(RE)                                                  
         MVC   0(1,RF),TXLINE#                                                  
         LA    RE,1(RE)                                                         
         STC   RE,TSARLIN#                                                      
         XC    TLKEY,TLKEY                                                      
         MVC   TLKSES,TWASESNL                                                  
         STCM  RE,3,TLKSEQ                                                      
         AH    RE,CSPSRECN                                                      
         STH   RE,TLNUM                                                         
         LA    RE,TLDATA                                                        
         LA    RF,TXL                                                           
         LA    R0,TXD                                                           
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         GOTO1 ATSARIO,TSAPUT                                                   
*                                                                               
         B     EXIT                                                             
         DROP  R3,R7                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET TXD FROM TSAR RECORD                                 *         
*                                                                     *         
* NTRY: TSARLIN# = SCREEN LINE #                                      *         
*             P1 = A(TXD)                                             *         
* EXIT: TSARLIN# = TSARLIN# + 1                                       *         
***********************************************************************         
         SPACE 1                                                                
RESTX    DS    0H                                                               
         USING *,R8                                                             
         L     R7,ALSVALS                                                       
         USING LSVALSD,R7                                                       
         USING TLSTD,LSTLST                                                     
         L     R3,0(R1)                                                         
         USING TXD,R3                                                           
*                                                                               
         XR    RE,RE                                                            
         IC    RE,TSARLIN#                                                      
         LA    RE,1(RE)                                                         
         STC   RE,TSARLIN#                                                      
         AH    RE,CSPSRECN                                                      
         STH   RE,TLNUM                                                         
         GOTO1 ATSARIO,TSAGET                                                   
         LA    RE,TLDATA                                                        
         LA    RF,TXL                                                           
         LA    R0,TXD                                                           
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         L     RE,ATWA                                                          
         S     RE,TXTWA                                                         
         L     RF,TXACOM                                                        
         AR    RF,RE                                                            
         ST    RF,TXACOM                                                        
         L     RF,TXATXT                                                        
         AR    RF,RE                                                            
         ST    RF,TXATXT                                                        
         L     RF,TXANUM                                                        
         AR    RF,RE                                                            
         ST    RF,TXANUM                                                        
*                                                                               
         B     EXIT                                                             
         DROP  R3,R7                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT NUMBERS                                           *         
*                                                                     *         
* NTRY: P1 = A(TXD)                                                   *         
***********************************************************************         
         SPACE 1                                                                
FMTTX    DS    0H                                                               
         USING *,R8                                                             
         L     R2,0(R1)                                                         
         USING TXD,R2                                                           
*                                                                               
         LA    R3,TXLINLAY                                                      
         USING LAYOUTD,R3                                                       
*                                                                               
FTX02    CLI   LAYOUTD,EOT                                                      
         BE    FMTTXX                                                           
         CLI   LAYFLD,0                                                         
         BE    FTX08                                                            
         GOTO1 ADISNUM,BOPARM,TXD,LAYOUTD                                       
FTX08    LA    R3,LAYOUTL(R3)                                                   
         B     FTX02                                                            
*                                                                               
FMTTXX   B     EXIT                                                             
         SPACE 1                                                                
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO RETURN NUMTABD ENTRY                                     *         
*                                                                     *         
* NTRY: P1 = A(BLFFLD)                                                *         
* EXIT: P1 = A(NUMTAB ENTRY)                                          *         
***********************************************************************         
         SPACE 1                                                                
GETNUM   DS    0H                                                               
         USING *,R8                                                             
         L     RF,0(R1)                                                         
         L     R2,ANUMTAB                                                       
         USING NUMTABD,R2                                                       
GNUM02   CLC   NUMFLD,0(RF)                                                     
         BE    GNUM10                                                           
         CLI   NUMFLD2,0                                                        
         BE    *+14                                                             
         CLC   NUMFLD2,0(RF)                                                    
         BE    GNUM10                                                           
         LA    R2,NUMTABL(R2)                                                   
         CLI   NUMTABD,EOT                                                      
         BNE   GNUM02                                                           
         XC    0(4,R1),0(R1)                                                    
         B     EXITN                                                            
GNUM10   ST    R2,0(R1)                                                         
         B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY NUMBER                                           *         
*                                                                     *         
* NTRY: P1 = A(TXD)                                                   *         
*       P2 = A(LAYOUTD) ENTRY                                         *         
***********************************************************************         
         SPACE 1                                                                
DISNUM   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         LM    R2,R3,0(R1)                                                      
         USING TXD,R2                                                           
         USING LAYOUTD,R3                                                       
         GOTO1 AGETNUM,BOPARM,LAYFLD                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,0(R1)                                                         
         USING NUMTABD,R4                                                       
*                                                                               
         XR    R6,R6                                                            
         IC    R6,LAYCOLN          R6=L(NUMBER)                                 
         XR    RE,RE                                                            
         IC    RE,LAYCOLF                                                       
         LA    R7,TXDATA-1(RE)     R7=A(NUMBER)                                 
*                                                                               
         CLI   LAYCOLF,1           ENSURE SPACES EITHER SIDE OF NUMBER          
         BE    DNUM02                                                           
         LR    R1,R7                                                            
         BCTR  R1,0                                                             
         MVI   0(R1),C' '                                                       
DNUM02   AR    RE,R6                                                            
         CLM   RE,1,ESREPWD                                                     
         BH    DNUM04                                                           
         LA    RE,TXDATA-1(RE)                                                  
         MVI   0(RE),C' '                                                       
*                                                                               
DNUM04   LH    RF,NUMDIS                                                        
         LA    RF,CLB0A(RF)                                                     
         BASR  RE,RF                                                            
*                                                                               
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY HOURS                                                       *         
***********************************************************************         
         SPACE 1                                                                
DISHRS   NTR1  ,                                                                
         CURED (P8,TXHRS),((R6),(R7)),2,MINUS=YES,DMCB=BODMCB,         *        
               ZERO=BLANK,COMMAS=YES                                            
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY RATE                                                        *         
***********************************************************************         
         SPACE 1                                                                
DISRAT   NTR1  ,                                                                
         CURED (P8,TXRAT),((R6),(R7)),2,DMCB=BODMCB,MINUS=YES,         *        
               ZERO=BLANK,COMMAS=YES                                            
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY COMMISSION RATE                                             *         
***********************************************************************         
         SPACE 1                                                                
DISCMR   NTR1  ,                                                                
         CURED (P8,TXCMR),((R6),(R7)),2,DMCB=BODMCB,MINUS=YES,         *        
               ZERO=BLANK,COMMAS=YES                                            
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY NET                                                         *         
***********************************************************************         
         SPACE 1                                                                
DISNET   NTR1  ,                                                                
         CURED (P8,TXNET),((R6),(R7)),CSCURBIL,DMCB=BODMCB,MINUS=YES,  *        
               ZERO=NOBLANK,COMMAS=YES                                          
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY COMMISSION                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISCOM   NTR1  ,                                                                
         CURED (P8,TXCOM),((R6),(R7)),CSCURBIL,DMCB=BODMCB,MINUS=YES,  *        
               ZERO=NOBLANK,COMMAS=YES                                          
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY GROSS                                                       *         
***********************************************************************         
         SPACE 1                                                                
DISGRS   NTR1  ,                                                                
         CURED (P8,TXGRS),((R6),(R7)),CSCURBIL,DMCB=BODMCB,MINUS=YES,  *        
               ZERO=NOBLANK,COMMAS=YES                                          
         B     EXIT                                                             
         SPACE 1                                                                
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE NUMBER                                          *         
*                                                                     *         
* NTRY: P1 = A(TXD)                                                   *         
*       P2 = A(NMTABD)                                                *         
***********************************************************************         
         SPACE 1                                                                
VALNUM   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         LM    R2,R3,0(R1)                                                      
         USING TXD,R2                                                           
         USING NUMTABD,R3                                                       
*                                                                               
         LA    R4,TXSCRLAY         R4=A(LAYOUT ENTRY)                           
         USING LAYOUTD,R4                                                       
VNUM02   CLC   LAYFLD,NUMFLD                                                    
         BE    VNUM04                                                           
         LA    R4,LAYOUTL(R4)                                                   
         CLI   LAYOUTD,EOT                                                      
         BNE   VNUM02                                                           
         DC    H'0'                                                             
*                                                                               
VNUM04   XR    R6,R6                                                            
         IC    R6,LAYCOLN          R6=L(NUMBER)                                 
         XR    R7,R7                                                            
         IC    R7,LAYCOLF                                                       
         LA    R7,TXDATA-1(R7)     R7=A(NUMBER)                                 
         LA    R1,0(R6,R7)                                                      
         BCTR  R1,0                                                             
         CLI   0(R1),C' '          TEST ANY INPUT                               
         BH    *+12                                                             
         BCT   R6,*-10                                                          
         B     EXITY                                                            
*                                                                               
         LH    RF,NUMVAL                                                        
         LA    RF,CLB0A(RF)                                                     
         BASR  RE,RF                                                            
         BE    EXITY                                                            
*                                                                               
VALNUMN  MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         XR    RF,RF                                                            
         IC    RF,LAYFHD                                                        
         A     RF,TXATXT                                                        
         ST    RF,FVADDR                                                        
         B     EXITN                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE HOURS                                                      *         
***********************************************************************         
         SPACE 1                                                                
VALHRS   NTR1  ,                                                                
         GOTO1 VCASHVAL,BOPARM,(X'82',(R7)),(X'40',(R6))                        
         CLI   0(R1),0                                                          
         BNE   EXITN                                                            
         ZAP   TXHRS,4(8,R1)                                                    
         CP    TXHRS,BCPZERO       TEST HOURS IS NOW ZERO                       
         BNE   VHRS02                                                           
         ZAP   TXRAT,BCPZERO       YES - SET RATE TO ZERO                       
         B     EXITY                     AND LEAVE NET ALONE                    
*                                                                               
VHRS02   TM    TXNINP,TXNRAT       TEST RATE INPUT                              
         BO    VHRS04                                                           
         CP    TXRAT,BCPZERO       TEST RATE IS ZERO                            
         BNE   VHRS04                                                           
         BAS   RE,RNH                                                           
         B     EXITY                                                            
*                                                                               
VHRS04   BAS   RE,NHR                                                           
         BAS   RE,GNC                                                           
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE RATE                                                       *         
***********************************************************************         
         SPACE 1                                                                
VALRAT   NTR1  ,                                                                
         GOTO1 VCASHVAL,BOPARM,(X'82',(R7)),(X'40',(R6))                        
         CLI   0(R1),0                                                          
         BNE   EXITN                                                            
         ZAP   TXRAT,4(8,R1)                                                    
         CP    TXRAT,BCPZERO       TEST RATE IS ZERO                            
         BNE   VRAT02                                                           
         ZAP   TXHRS,BCPZERO       YES - SET HOURS TO ZERO                      
         B     EXITY                     AND LEAVE NET ALONE                    
*                                                                               
VRAT02   TM    TXNINP,TXNHRS       TEST HOURS INPUT                             
         BO    VRAT04                                                           
         CP    TXHRS,BCPZERO       TEST HOURS IS ZERO                           
         BNE   VRAT04                                                           
         BAS   RE,HNR                                                           
         B     EXITY                                                            
*                                                                               
VRAT04   BAS   RE,NHR                                                           
         BAS   RE,GNC                                                           
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE COMMISSION RATE                                            *         
***********************************************************************         
         SPACE 1                                                                
VALCMR   NTR1  ,                   COMM RATE=COMM / NET                         
         ZAP   BOPL81(16),TXCOM                                                 
         SRP   BOPL81(16),5,0                                                   
         DP    BOPL81(16),TXNET                                                 
         SRP   BOPL81,64-3,5                                                    
         ZAP   TXCMR,BOPL81                                                     
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE NET                                                        *         
***********************************************************************         
         SPACE 1                                                                
VALNET   NTR1  ,                                                                
         TM    TXNINP,TXNHRS+TXNRAT                                             
         BO    EXITY               IGNORE INPUT IF HOURS & RATE INPUT           
*                                                                               
         XR    RF,RF                                                            
         IC    RF,CSCURBIL+(CURTDECP-CURTABD)                                   
         LA    RF,X'80'(RF)                                                     
         GOTO1 VCASHVAL,BOPARM,((RF),(R7)),(X'40',(R6))                         
         CLI   0(R1),0                                                          
         BNE   EXITN                                                            
         ZAP   TXNET,4(8,R1)                                                    
*                                                                               
         TM    TXNUM,TXNHRS+TXNRAT     TEST HOURS/RATE ON LINE                  
         BZ    VALNETY                                                          
         CP    TXNET,BCPZERO       TEST NET SET TO ZERO                         
         BNE   VNET02                                                           
         ZAP   TXHRS,BCPZERO       YES - SET HOURS AND RATE TO ZERO             
         ZAP   TXRAT,BCPZERO                                                    
         B     EXITY                                                            
VNET02   TM    TXNINP,TXNHRS       TEST HOURS INPUT                             
         BZ    *+12                                                             
         BAS   RE,RNH              YES - CALCULATE RATE                         
         B     EXITY                                                            
         BAS   RE,HNR              NO - CALCULATE HOURS                         
*                                                                               
VALNETY  BAS   RE,GNC              CALCULATE GROSS                              
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE COMMISSION                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALCOM   NTR1  ,                                                                
         XR    RF,RF                                                            
         IC    RF,CSCURBIL+(CURTDECP-CURTABD)                                   
         LA    RF,X'80'(RF)                                                     
         GOTO1 VCASHVAL,BOPARM,((RF),(R7)),(X'40',(R6))                         
         CLI   0(R1),0                                                          
         BNE   EXITN                                                            
         ZAP   TXCOM,4(8,R1)                                                    
         BAS   RE,GNC                                                           
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE GROSS                                                      *         
***********************************************************************         
         SPACE 1                                                                
VALGRS   NTR1  ,                                                                
*                                                                               
         TM    TXNINP,TXNCOM       TEST COMMISSION INPUT                        
         BZ    VGRS02                                                           
         TM    TXNINP,TXNNET+TXNRAT+TXNHRS TEST NET/HOURS/RATE INPUT            
         BNZ   EXITY               YES - IGNORE GROSS INPUT                     
*                                                                               
VGRS02   XR    RF,RF                                                            
         IC    RF,CSCURBIL+(CURTDECP-CURTABD)                                   
         LA    RF,X'80'(RF)                                                     
         GOTO1 VCASHVAL,BOPARM,((RF),(R7)),(X'40',(R6))                         
         CLI   0(R1),0                                                          
         BNE   EXITN                                                            
         ZAP   TXGRS,4(8,R1)                                                    
*                                                                               
         TM    TXNINP,TXNCOM       TEST COM ENTERED                             
         BO    VGRSNET             YES - NET=GROSS-COM                          
         TM    TXNINP,TXNNET       TEST NET ENTERED                             
         BO    VGRSCOM             YES - COM=GROSS-NET                          
         TM    TXNINP,TXNHRS+TXNRAT TEST HOURS/RATE ENTERED                     
         BZ    VGRS04                                                           
         TM    TXNUM,TXNNET+TXNCOM YES - TEST NET/COM ON LINE                   
         BNZ   VGRSCOM                    YES - COM=GROSS-NET                   
         B     VGRSNET                     NO - NET=GROSS-COM                   
*                                                                               
VGRS04   TM    TXNUM,TXNNET+TXNCOM TEST NET/COMM ON LINE                        
         BO    VGRSNET             BOTH - CHANGE NET                            
         BZ    VGRSNET             NEITHER - CHANGE NET                         
         TM    TXNUM,TXNNET                                                     
         BO    VGRSCOM             NET ONLY - CHANGE COMMISSION                 
         B     VGRSNET             COMMISSION ONLY - CHANGE NET                 
*                                                                               
VGRSNET  BAS   RE,NGC              ADJUST NET AMOUNT                            
         TM    TXNUM,TXNHRS+TXNRAT     TEST HOURS/RATE ON LINE                  
         BZ    EXITY                                                            
         CP    TXNET,BCPZERO       TEST NET NOW ZERO                            
         BNE   VGRSNET2                                                         
         ZAP   TXRAT,BCPZERO       YES - SET RATE/HOURS TO ZERO                 
         ZAP   TXHRS,BCPZERO                                                    
         B     EXITY                                                            
VGRSNET2 TM    TXNINP,TXNHRS       TEST HOURS INPUT                             
         BZ    *+12                                                             
         BAS   RE,RNH              YES - CALCULATE RATE                         
         B     EXITY                                                            
         BAS   RE,HNR              NO - CALCULATE HOURS                         
         B     EXITY                                                            
*                                                                               
VGRSCOM  BAS   RE,CGN              ADJUST COMMISSION AMOUNT                     
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* FORMULAE                                                            *         
***********************************************************************         
         SPACE 1                                                                
NHR      DS    0H                  NET = HOURS * RATE                           
         ZAP   BOPL81(16),TXHRS                                                 
         MP    BOPL81(16),TXRAT                                                 
         SRP   BOPL81(16),64-2,5                                                
         ZAP   TXNET,BOPL81(16)                                                 
         BR    RE                                                               
*                                                                               
GNC      DS    0H                  GROSS = NET + COMMISSION                     
         ZAP   TXGRS,TXNET                                                      
         AP    TXGRS,TXCOM                                                      
         BR    RE                                                               
*                                                                               
HNR      DS    0H                  HOURS = NET / RATE                           
         CP    TXRAT,BCPZERO                                                    
         BNE   *+12                                                             
         ZAP   TXHRS,BCPZERO                                                    
         BR    RE                                                               
         ZAP   BOPL81(16),TXNET                                                 
         SRP   BOPL81(16),5,0                                                   
         DP    BOPL81(16),TXRAT                                                 
         SRP   BOPL81,64-3,5                                                    
         ZAP   TXHRS,BOPL81                                                     
         BR    RE                                                               
*                                                                               
RNH      DS    0H                  RATE = NET / HOURS                           
         CP    TXHRS,BCPZERO                                                    
         BNE   *+12                                                             
         ZAP   TXRAT,BCPZERO                                                    
         BR    RE                                                               
         ZAP   BOPL81(16),TXNET                                                 
         SRP   BOPL81(16),5,0                                                   
         DP    BOPL81(16),TXHRS                                                 
         SRP   BOPL81,64-3,5                                                    
         ZAP   TXRAT,BOPL81                                                     
         BR    RE                                                               
*                                                                               
NGC      DS    0H                  NET = GROSS - COMMISSION                     
         ZAP   TXNET,TXGRS                                                      
         SP    TXNET,TXCOM                                                      
         BR    RE                                                               
*                                                                               
CGN      DS    0H                  COMMISSION = GROSS - NET                     
         ZAP   TXCOM,TXGRS                                                      
         SP    TXCOM,TXNET                                                      
         BR    RE                                                               
         SPACE 1                                                                
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* UPDATE THE TOTALS ELEMENT                                           *         
*                                                                     *         
* NTRY: P1 = A(PARAGRAPH NUMBER)                                      *         
***********************************************************************         
         SPACE 1                                                                
UPDTOT   DS    0H                                                               
         USING *,R8                                                             
         PUSH  USING                                                            
         L     RF,0(R1)                                                         
         MVC   UTPARA,0(RF)        SAVE PARAGRAPH NUMBER                        
         GOTO1 MAKETX,R7                                                        
I        USING TXD,R7              R7=A(ITEM TXD)                               
         GOTO1 MAKETX,R6                                                        
S        USING TXD,R6              R6=A(SUB-TOTAL TXD)                          
         GOTO1 AINITX,BOPARM,S.TXD                                              
         GOTO1 MAKETX,R5                                                        
T        USING TXD,R5              R5=A(TOAL TXD)                               
         GOTO1 AINITX,BOPARM,T.TXD                                              
*                                                                               
K        USING PBRRECD,IOKEY                                                    
         XC    K.PBRKEY,K.PBRKEY                                                
         MVI   K.PBRKTYP,PBRKTYPQ                                               
         MVC   K.PBRKCPY,CUABIN                                                 
         MVI   K.PBRKSUB,PBRKACTQ                                               
         MVC   K.PBRKJOB,BCJOBCOD                                               
         MVC   K.PBRKSEQ,ESSEQ                                                  
         MVC   K.PBRKPARA,UTPARA                                                
         GOTO1 AIO,IOREAD+IOACCDIR+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGETRUP+IOACCMST+IO2                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO2                                                          
         LA    R3,PBRRFST-PBRRECD(R3)                                           
         USING NDXELD,R3                                                        
         XR    RF,RF                                                            
         CLI   NDXEL,NDXELQ                                                     
         BE    *+12                                                             
         IC    RF,NDXLN                                                         
         BXH   R3,RF,*-12                                                       
         CLI   NDXACTV,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RE,AIO3                                                          
         LA    RF,SEQTABN*SEQTABL                                               
         XR    R1,R1                                                            
         MVCL  RE,R0               CLEAR IO3 FOR SEQUENCE TABLE                 
*                                                                               
UTOT02   GOTO1 AIO,IOACCDIR+IOSEQ+IO1                                           
         BNE   UTOT10                                                           
         CLC   K.PBRKEY(PBRKLINE-PBRKEY),IOKEYSAV                               
         BNE   UTOT10                                                           
         CLI   K.PBRKLSTA,PBRLTXT TEST TEXT LINE ONLY                           
         BE    UTOT02                                                           
*                                                                               
         XR    R0,R0               TEST LINE IS CURRENTLY ACTIVE                
         IC    R0,NDXACTV                                                       
         LA    RF,NDXINDX                                                       
         CLC   K.PBRKLINE,0(RF)                                                 
         BE    UTOT04                                                           
         LA    RF,1(RF)                                                         
         BCT   R0,*-14                                                          
         B     UTOT02                                                           
*                                                                               
UTOT04   DS    0H                                                               
         LA    RE,NDXINDX                                                       
         SR    RF,RE                                                            
         MH    RF,=Y(SEQTABL)                                                   
         A     RF,AIO3                                                          
         USING SEQTABD,RF                                                       
         MVC   SEQLSTA,K.PBRKLSTA  LINE STATUS                                  
         MVC   SEQLDA,K.PBRKDA     DISK ADDRESS                                 
         DROP  RF                                                               
         B     UTOT02                                                           
         DROP  K                                                                
*                                                                               
UTOT10   L     R2,AIO1                                                          
         USING PBRRECD,R2                                                       
*                                                                               
         L     R4,AIO3                                                          
         USING SEQTABD,R4                                                       
         LA    R0,SEQTABN                                                       
UTOT12   CLI   SEQLSTA,0                                                        
         BE    UTOT28                                                           
*                                                                               
         CLI   SEQLSTA,PBRLNUM     TEST NUMBER LINE                             
         BE    UTOT14                                                           
         CLI   SEQLSTA,PBRLSUB     TEST SUB-TOTAL LINE                          
         BE    UTOT16                                                           
         CLI   SEQLSTA,PBRLTOT     TEST TOTAL LINE                              
         BE    UTOT18                                                           
         DC    H'0'                                                             
*                                                                               
UTOT14   MVC   IODAOVER,SEQLDA                                                  
         GOTO1 AIO,IOACCMST+IOGET+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGETTX,BOPARM,I.TXD,PBRRECD                                      
         GOTO1 AADDTOT,BOPARM,S.TXD,I.TXD                                       
         GOTO1 AADDTOT,BOPARM,T.TXD,I.TXD                                       
         B     UTOT28                                                           
*                                                                               
UTOT16   MVC   IODAOVER,SEQLDA                                                  
         GOTO1 AIO,IOACCMST+IOGETRUP+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGETTX,BOPARM,I.TXD,PBRRECD                                      
         GOTO1 APUTTOT,BOPARM,S.TXD,I.TXD                                       
         BE    UTOT17                                                           
         GOTO1 AFMTTX,BOPARM,I.TXD                                              
         GOTO1 APUTTX,BOPARM,I.TXD,PBRRECD                                      
         GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   ESPARA,PBRKPARA     TEST PARAGRAPH IS ON SCREEN                  
         BNE   UTOT17                                                           
         GOTO1 AUPDSCR,BOPARM,I.TXD,PBRKLINE                                    
UTOT17   GOTO1 AINITX,BOPARM,S.TXD    RESET SUB-TOTALS                          
         B     UTOT28                                                           
*                                                                               
UTOT18   MVC   IODAOVER,SEQLDA                                                  
         GOTO1 AIO,IOACCMST+IOGETRUP+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGETTX,BOPARM,I.TXD,PBRRECD                                      
         GOTO1 APUTTOT,BOPARM,T.TXD,I.TXD                                       
         BE    UTOT28                                                           
         GOTO1 AFMTTX,BOPARM,I.TXD                                              
         GOTO1 APUTTX,BOPARM,I.TXD,PBRRECD                                      
         GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   ESPARA,PBRKPARA     TEST PARAGRAPH IS ON SCREEN                  
         BNE   UTOT28                                                           
         GOTO1 AUPDSCR,BOPARM,I.TXD,PBRKLINE                                    
*                                                                               
UTOT28   LA    R4,SEQTABL(R4)                                                   
         BCT   R0,UTOT12                                                        
         DROP  R4                                                               
*                                                                               
UTOT30   DS    0H                                                               
         L     R3,AIO2             UPDATE PARAGRAPH TOTALS                      
         LA    R3,PBRRFST-PBRRECD(R3)                                           
         USING PGHELD,R3                                                        
         XR    RF,RF                                                            
         CLI   PGHEL,PGHELQ                                                     
         BE    *+12                                                             
         IC    RF,PGHLN                                                         
         BXH   R3,RF,*-12                                                       
         CP    PGHNET,T.TXNET      TEST CHANGE IN TOTALS                        
         BNE   *+14                                                             
         CP    PGHCOM,T.TXCOM                                                   
         BE    UTOT34                                                           
         OI    BCAPINDS,BCAPIBAU   SET AMOUNT CHNGED TO CALLING OVERLAY         
         SP    ESBILNET,PGHNET                                                  
         SP    ESBILCOM,PGHCOM                                                  
         ZAP   PGHNET,T.TXNET                                                   
         ZAP   PGHCOM,T.TXCOM                                                   
         AP    ESBILNET,PGHNET                                                  
         AP    ESBILCOM,PGHCOM                                                  
         CLC   ESPARA,PBRKPARA     UPDATE SAVED CURRENT PARA TOTALS             
         BNE   UTOT32                                                           
         ZAP   ESPARNET,PGHNET                                                  
         ZAP   ESPARCOM,PGHCOM                                                  
UTOT32   GOTO1 AIO,IOPUT+IOACCMST+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
UTOT34   GOTO1 ADISAMT                                                          
*                                                                               
         B     EXITY                                                            
         POP   USING                                                            
         SPACE 1                                                                
SEQTABD  DSECT                     SEQUENCE TABLE                               
SEQLSTA  DS    XL1                 LINE STATUS                                  
SEQLDA   DS    AL4                 LINE RECORD D/A                              
SEQTABL  EQU   *-SEQTABD                                                        
SEQTABN  EQU   200                                                              
CLB0A    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO UPDATE TOTAL LINES ON SCREEN                             *         
*                                                                     *         
* NTRY: P1 = A(TXD OF UPDATED TOTALS)                                 *         
*       P2 = A(LINE NUMBER OF TOTAL)                                  *         
***********************************************************************         
         SPACE 1                                                                
UPDSCR   DS    0H                                                               
         USING *,R8                                                             
         LM    R2,R3,0(R1)         R3=A(LINE NUMBER)                            
T        USING TXD,R2              R2=A(TXD OF TOTAL)                           
         GOTO1 MAKETX,R7                                                        
S        USING TXD,R7              R7=A(TXD SAVED FOR SCREEN)                   
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,1,ESNUMLIN                                                    
         BZ    EXIT                                                             
         LA    R4,ESSCRLST                                                      
         XR    R5,R5                                                            
*                                                                               
USCR02   CLC   0(1,R4),0(R3)                                                    
         BNE   USCR08                                                           
         STC   R5,TSARLIN#                                                      
         GOTO1 ARESTX,BOPARM,S.TXD                                              
         MVC   S.TXDATA,T.TXDATA                                                
         MVC   S.TXAMT(TXAMTS),T.TXAMT                                          
         GOTO1 ADISTX,BOPARM,S.TXD                                              
         STC   R5,TSARLIN#                                                      
         GOTO1 ASAVTX,BOPARM,S.TXD                                              
         B     EXIT                                                             
*                                                                               
USCR08   LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         BCT   R0,USCR02                                                        
*                                                                               
USCR10   B     EXIT                                                             
         DROP  S,T                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD ONE TXD TO ANOTHER                                   *         
*                                                                     *         
* NTRY: P1 = A(TXD) TO BE ADDED TO                                    *         
*       P2 = A(TXD) TO BE ADDED FROM                                  *         
***********************************************************************         
         SPACE 1                                                                
ADDTOT   DS    0H                                                               
         USING *,R8                                                             
         LM    R2,R3,0(R1)                                                      
T        USING TXD,R2                                                           
I        USING TXD,R3                                                           
         AP    T.TXNET,I.TXNET                                                  
         AP    T.TXCOM,I.TXCOM                                                  
         AP    T.TXGRS,I.TXGRS                                                  
         AP    T.TXHRS,I.TXHRS                                                  
         B     EXIT                                                             
         DROP  I,T                                                              
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO PUT TXD TOTALS INTO ANOTHER TXD                          *         
*                                                                     *         
* NTRY: P1 = A(TXD OF TOTALS TO BE COPIED FROM)                       *         
*       P2 = A(TXD OF TOTALS TO BE COPIED TO)                         *         
* EXIT: CC = EQUAL IF NO CHANGE IN TOTALS                             *         
***********************************************************************         
         SPACE 1                                                                
PUTTOT   DS    0H                                                               
         USING *,R8                                                             
         LM    R2,R3,0(R1)                                                      
         LA    RE,TXAMT-TXD(R3)                                                 
         LA    RF,TXAMT-TXD(R2)                                                 
         LA    R0,TXAMTN                                                        
         MVI   BOBYTE1,0                                                        
*                                                                               
PTOT02   CP    0(L'TXAMT,RE),0(L'TXAMT,RF)                                      
         BE    *+14                                                             
         MVI   BOBYTE1,1                                                        
         ZAP   0(L'TXAMT,RE),0(L'TXAMT,RF)                                      
         LA    RE,L'TXAMT(RE)                                                   
         LA    RF,L'TXAMT(RF)                                                   
         BCT   R0,PTOT02                                                        
*                                                                               
         CLI   BOBYTE1,0                                                        
         B     EXIT                CC = EQUAL IF NO CHANGE                      
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO COMPARE TX TOTALS                                        *         
*                                                                     *         
* NTRY: P1 = A(1ST TXD)                                               *         
*       P2 = A(2ND TXD)                                               *         
* EXIT: CC = EQUAL TOTALS THE SAME                                    *         
***********************************************************************         
         SPACE 1                                                                
CLCTOT   DS    0H                                                               
         USING *,R8                                                             
         LM    R2,R3,0(R1)                                                      
         LA    RE,TXAMT-TXD(R3)                                                 
         LA    RF,TXAMT-TXD(R2)                                                 
         LA    R0,TXAMTN                                                        
*                                                                               
CTOT02   CP    0(L'TXAMT,RE),0(L'TXAMT,RF)                                      
         BNE   EXIT                CC = NOT EQUAL IF  CHANGE                    
         LA    RE,L'TXAMT(RE)                                                   
         LA    RF,L'TXAMT(RF)                                                   
         BCT   R0,CTOT02                                                        
*                                                                               
         B     EXITY               CC = EQUAL IF NO CHANGE                      
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY NET/COMMISSION BILL AMOUNTS                      *         
***********************************************************************         
         SPACE 1                                                                
DISAMT   DS    0H                                                               
         USING *,R8                                                             
*                                                                               
         LA    R3,BOWORK1                                                       
         MVC   BOWORK1,BCSPACES                                                 
         CURED (P8,ESBILNET),(L'EDTNET,(R3)),CSCURBIL,MINUS=YES,       *        
               DMCB=BCDMCB,ALIGN=LEFT                                           
         TM    ESPIND,ESPILVE                                                   
         BO    DAMT02                                                           
         ZAP   BODUB1,ESALLNET                                                  
         SP    BODUB1,ESBILNET                                                  
         BZ    DAMT02                                                           
         AR    R3,R0                                                            
         LA    R3,1(R3)                                                         
         CURED (P8,BODUB1),(L'EDTNET,(R3)),CSCURBIL,MINUS=YES,         *        
               DMCB=BCDMCB,ALIGN=LEFT,BRACKET=YES                               
         CP    BODUB1,BCPZERO                                                   
         BL    DAMT02                                                           
         AR    R3,R0                                                            
         BCTR  R3,0                                                             
         MVC   1(1,R3),0(R3)                                                    
         MVI   0(R3),C'+'                                                       
DAMT02   MVC   EDTNET,BOWORK1                                                   
         OI    EDTNETH+FHOID,FHOITR                                             
*                                                                               
         LA    R3,BOWORK1                                                       
         MVC   BOWORK1,BCSPACES                                                 
         CURED (P8,ESBILCOM),(L'EDTCOMN,(R3)),CSCURBIL,MINUS=YES,      *        
               DMCB=BCDMCB,ALIGN=LEFT                                           
         TM    ESPIND,ESPILVE                                                   
         BO    DAMT04                                                           
         ZAP   BODUB1,ESALLCOM                                                  
         SP    BODUB1,ESBILCOM                                                  
         BZ    DAMT04                                                           
         AR    R3,R0                                                            
         LA    R3,1(R3)                                                         
         CURED (P8,BODUB1),(L'EDTCOMN,(R3)),CSCURBIL,MINUS=YES,        *        
               DMCB=BCDMCB,ALIGN=LEFT,BRACKET=YES                               
         CP    BODUB1,BCPZERO                                                   
         BL    DAMT04                                                           
         AR    R3,R0                                                            
         BCTR  R3,0                                                             
         MVC   1(1,R3),0(R3)                                                    
         MVI   0(R3),C'+'                                                       
DAMT04   MVC   EDTCOMN,BOWORK1                                                  
         OI    EDTCOMNH+FHOID,FHOITR                                            
*                                                                               
DISAMTX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INSERT ENTRY INTO LAYOUTD TABLE                          *         
*                                                                     *         
* NTRY: P1 = A(CURRENT LAYOUT TABLE)                                  *         
*       P2 = A(BLFFLD EQUATE)                                         *         
*       P2 = A(BLFCOLF VALUE)                                         *         
*       P2 = A(BLFCOLN VALUE)                                         *         
***********************************************************************         
         SPACE 1                                                                
INSLAY   DS    0H                                                               
         USING *,R8                                                             
         LM    R2,R4,4(R1)                                                      
         MVC   ILFLD,0(R2)                                                      
         MVC   ILCOLF,0(R3)                                                     
         MVC   ILCOLN,0(R4)                                                     
*                                                                               
         L     R6,0(R1)                                                         
O        USING LAYOUTD,R6          R6=A(OLD LAYOUT TABLE)                       
         LA    R7,BOWORK1                                                       
         XC    BOWORK1,BOWORK1                                                  
N        USING LAYOUTD,R7          R7=A(NEW LAYOUT TABLE)                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         XR    R0,R0                                                            
ILAY02   CLI   O.LAYOUTD,EOT                                                    
         BE    ILAY10                                                           
         CLC   O.LAYCOLF,ILCOLF                                                 
         BH    ILAY03                                                           
         IC    RE,O.LAYCOLF                                                     
         IC    RF,O.LAYCOLN                                                     
         AR    RE,RF                                                            
         CLM   RE,1,ILCOLF                                                      
         BNL   ILAY04                                                           
*                                                                               
ILAY03   MVC   N.LAYOUTD(LAYOUTL),O.LAYOUTD                                     
         LA    R7,LAYOUTL(R7)                                                   
         B     ILAY08                                                           
*                                                                               
ILAY04   IC    RE,ILCOLF                                                        
         IC    RF,O.LAYCOLF                                                     
         SR    RE,RF                                                            
         BZ    ILAY06                                                           
         BCT   RE,*+8                                                           
         B     ILAY06                                                           
         MVC   N.LAYCOLF,O.LAYCOLF                                              
         STC   RE,N.LAYCOLN                                                     
         LA    R7,LAYOUTL(R7)                                                   
*                                                                               
ILAY06   MVC   N.LAYCOLF,ILCOLF                                                 
         MVC   N.LAYCOLN,ILCOLN                                                 
         MVC   N.LAYFLD,ILFLD                                                   
         LA    R7,LAYOUTL(R7)                                                   
*                                                                               
         IC    RE,ILCOLF                                                        
         IC    RF,ILCOLN                                                        
         LA    R0,1(RE,RF)         R0=START OF FILED                            
         IC    RE,O.LAYCOLF                                                     
         IC    RF,O.LAYCOLN                                                     
         AR    RF,RE               RF=END OF FIELD                              
         CR    RF,R0                                                            
         BNH   ILAY08                                                           
         SR    RF,R0                                                            
         STC   R0,N.LAYCOLF                                                     
         STC   RF,N.LAYCOLN                                                     
         LA    R7,LAYOUTL(R7)                                                   
*                                                                               
ILAY08   LA    R6,LAYOUTL(R6)                                                   
         B     ILAY02                                                           
         DROP  O,N                                                              
*                                                                               
ILAY10   L     R6,0(R1)            COPY NEW INTO OLD                            
         MVC   0(LAYOUTS,R6),BOWORK1                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FORMAT LAYOUT TABLE FOR SCREEN                           *         
*                                                                     *         
* NTRY: P1 = A(TXD)                                                   *         
***********************************************************************         
         SPACE 1                                                                
SCRLAY   DS    0H                                                               
         USING *,R8                                                             
         L     R7,0(R1)                                                         
         USING TXD,R7                                                           
         LA    R2,TXLINLAY                                                      
L        USING LAYOUTD,R2          R2=A(LAYOUT FOR LINE)                        
         LA    R3,TXSCRLAY                                                      
S        USING LAYOUTD,R3          R3=A(LAYOUT FOR SCREEN)                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         XR    R1,R1                                                            
         XR    R6,R6               R6=DISP. OF FIELD ON LINE                    
*                                                                               
SLAY02   CLI   L.LAYOUTD,EOT                                                    
         BE    SLAY20                                                           
         CLC   L.LAYCOLF,ESCOL#X   TEST START FIELD > END SCREEN                
         BH    SLAY20              YES - FINISHED                               
         IC    RE,L.LAYCOLF        RE = START OF FIELD                          
         IC    RF,L.LAYCOLN        RF = LENGTH OF FIELD                         
         LA    R0,0(RE,RF)         R0 = END OF FIELD                            
         BCTR  R0,0                R0 = END OF FIELD                            
         CLM   R0,1,ESCOL#1        TEST END FIELD < START SCREEN                
         BL    SLAY18              YES- TRY NEXT FIELD                          
*                                                                               
         MVC   S.LAYOUTD(LAYOUTL),L.LAYOUTD                                     
*                                                                               
         CLM   RE,1,ESCOL#1        TEST LHS OF FIELD ON SCREEN                  
         BNL   SLAY04                                                           
         MVC   S.LAYCOLF,ESCOL#1   RESET START                                  
         IC    R1,ESCOL#1                                                       
         SR    R1,RE                                                            
         SR    RF,R1                                                            
         STC   RF,S.LAYCOLN        ADJUST LENGTH                                
*                                                                               
SLAY04   CLM   R0,1,ESCOL#X        TEST RHS OF FIELD ON SCREEN                  
         BNH   SLAY06                                                           
         IC    R1,ESCOL#X                                                       
         SR    R0,R1                                                            
         SR    RF,R0                                                            
         STC   RF,S.LAYCOLN                                                     
*                                                                               
SLAY06   CLI   S.LAYFLD,0          TEST SPECIAL FIELD                           
         BE    SLAY10                                                           
         TM    ESPIND,ESPILVE      LIVE BILL                                    
         BO    SLAY08                                                           
         TM    TXLSTA,PBRLTOT+PBRLSUB                                           
         BNZ   SLAY08              TOTAL OR SUBTOTAL                            
         CLC   S.LAYCOLN,L.LAYCOLN TEST LENGTH ADJUSTED                         
         BE    SLAY10                                                           
*                                                                               
SLAY08   OI    S.LAYINDS,LAYIPR    PROTECT FIELD                                
*                                                                               
SLAY10   STC   R6,S.LAYFHD         SAVE DISP. TO FIELD                          
         LA    R6,FHDAD(RF,R6)     SET DISP. TO NEXT FIELD                      
         LA    R3,LAYOUTL(R3)                                                   
*                                                                               
SLAY18   LA    R2,LAYOUTL(R2)                                                   
         B     SLAY02                                                           
*                                                                               
SLAY20   MVI   S.LAYOUTD,EOT                                                    
         B     EXIT                                                             
         DROP  L,S                                                              
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         ORG   LTORG                                                            
         LTORG                                                                  
         SPACE 1                                                                
ACCMST   DC    C'ACCMST'                                                        
ADDCODE  DC    C'ADD=CODE'                                                      
INSDOTS  DC    C'...'                                                           
LHSMORE  DC    C'<<<'                                                           
RHSMORE  DC    C'>>>'                                                           
DASHES   DC    132C'-'                                                          
EOS      DC    X'0001010000008000' END-OF-SCREEN DATA                           
SPACES   DC    CL256' '                                                         
RULER    DS    0CL132                                                           
         DC    C'----+----1----+----2----+----3----+----4----+----5'            
         DC    C'----+----6----+----7----+----8----+----9----+----0'            
         DC    C'----+----1----+----2----+----3--'                              
         SPACE 1                                                                
*                                                                               
ATAB     DS    0A                  A(TABLES)                                    
         DC    A(EBLTAB)                                                        
         DC    A(LEDCTAB)                                                       
         DC    A(CMNDTAB)                                                       
         DC    A(TRUPTAB)                                                       
         DC    A(NUMTAB)                                                        
ATABN    EQU   (*-ATAB)/L'ATAB                                                  
         SPACE 1                                                                
         DS    (LTORGX-*)X                                                      
         ORG                                                                    
         EJECT                                                                  
***********************************************************************         
* GENERAL EQUATES                                                     *         
***********************************************************************         
         SPACE 1                                                                
GE$LONG  EQU   101+X'FF00'         &T FIELD IS TOO LONG.                        
MAXSLINQ EQU   16                  MAX LINES ON SCREEN                          
MAXSCOLQ EQU   69                  MAXIMUM COLS ON SCREEN                       
MAXPLINQ EQU   200                 MAX LINES IN PARAGRAPH                       
LINE#1   EQU   7                   SCREEN LINE NUMBER FOR 1ST TEXT LINE         
LINE#X   EQU   22                  SCREEN LINE NUMBER FOR LST TEXT LINE         
COLS#Q   EQU   80                  NUMBER OF COLUMNS ON SCREEN                  
ROWS#Q   EQU   24                  NUMBER OF ROWS ON SCREEN                     
INSERTQ  EQU   C'.'                INSERT CHARACTER                             
PFKFINDQ EQU   1                   FIND TEXT STRING PFKEY                       
PFKCHGQ  EQU   2                   CHANGE TEXT STRING PFKEY                     
PFKADDP  EQU   3                   NEW PARAGRAPH PFKEY                          
PFKPREVP EQU   5                   PREVIOUS PARAGRAPH PFKEY                     
PFKNEXTP EQU   6                   NEXT PARAGRAPH PFKEY                         
EOR      EQU   0                                                                
FF       EQU   X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* COMMAND TABLE (FOR OPTIONS FIELD)                                   *         
***********************************************************************         
         SPACE 1                                                                
CMNTABD  DSECT                     COMMAND TABLE                                
CMNMTYPE DS    XL1                 MAIN COMMAND TYPE                            
CMNRESQ  EQU   1                   RESET                                        
CMNDELQ  EQU   2                   DELETE                                       
CMNFNDQ  EQU   3                   FIND                                         
CMNCHGQ  EQU   4                   CHANGE                                       
CMNCOLQ  EQU   5                   COLUMNS                                      
CMNWORD  DS    AL2                 COMMAND WORD                                 
CMNPTAB  DS    AL2                 PARAMETER TABLE                              
CMNMIN   DS    XL1                 MINIMUM NUMBER OF PARAMS + 1                 
CMNSTAT  DS    XL1                 STATUS BYTE                                  
CMNSPREQ EQU   X'80'               MAIN COMMAND REQUIRES PARAMETERS             
CMNSFFTQ EQU   X'40'               PARAMETERS INCLUDE FREE FORM TEXT            
CMNPFLAG DS    AL2                 DISPLACEMENT TO PARAMETER FLAG               
CMNLNQ   EQU   *-CMNTABD           LENGTH OF COMMAND TABLE ENTRY                
         SPACE 1                                                                
CLB0A    CSECT                                                                  
CMNDTAB  DS    0C                                                               
         DC    AL1(CMNRESQ),AL2(UC@RESET-TWAD),AL2(0),AL1(1)                    
         DC    AL1(0),AL2(0)                                                    
         DC    AL1(CMNDELQ),AL2(UC@DEL-TWAD),AL2(CPARDEL-CLB0A),AL1(2)          
         DC    AL1(CMNSPREQ),AL2(ESDPAR-ESAVE)                                  
         DC    AL1(CMNFNDQ),AL2(UC@FIND-TWAD),AL2(CPARFND-CLB0A),AL1(2)         
         DC    AL1(CMNSPREQ+CMNSFFTQ),AL2(ESFCPAR-ESAVE)                        
         DC    AL1(CMNCHGQ),AL2(UC@CHG-TWAD),AL2(CPARCHG-CLB0A),AL1(3)          
         DC    AL1(CMNSPREQ+CMNSFFTQ),AL2(ESFCPAR-ESAVE)                        
         DC    AL1(CMNCOLQ),AL2(UC@COLS-TWAD),AL2(CPARCOL-CLB0A),AL1(1)         
         DC    AL1(0),AL2(ESCPAR-ESAVE)                                         
         DC    AL1(EOT)                                                         
*                                                                               
CPARDEL  DC    AL1(ESDPARQ),AL2(UC@PRGRP-TWAD),AL1(CPARSDFQ)                    
         DC    AL1(ESDALLQ,0,0,0,0,0)                                           
         DC    AL1(ESDALLQ),AL2(UC@ALL-TWAD),AL1(0)                             
         DC    AL1(ESDPARQ,0,0,0,0,0)                                           
         DC    AL1(EOT)                                                         
*                                                                               
CPARCOL  DC    AL1(ESCYESQ),AL2(UC@YES-TWAD),AL1(0)                             
         DC    AL1(ESCNOQ,0,0,0,0,0)                                            
         DC    AL1(ESCNOQ),AL2(UC@NO-TWAD),AL1(0)                               
         DC    AL1(ESCYESQ,0,0,0,0,0)                                           
         DC    AL1(EOT)                                                         
*                                                                               
CPARFND  DC    AL1(ESFCALQ),AL2(UC@ALL-TWAD),AL1(0)                             
         DC    AL1(ESFCFSQ,ESFCLSQ,ESFCPVQ,0,0,0)                               
         DC    AL1(ESFCPFQ),AL2(UC@PREFX-TWAD),AL1(0)                           
         DC    AL1(ESFCSFQ,ESFCWDQ,0,0,0,0)                                     
         DC    AL1(ESFCSFQ),AL2(UC@SUFFX-TWAD),AL1(0)                           
         DC    AL1(ESFCPFQ,ESFCWDQ,0,0,0,0)                                     
         DC    AL1(ESFCPVQ),AL2(UC@PRV-TWAD),AL1(0)                             
         DC    AL1(ESFCALQ,ESFCFSQ,ESFCLSQ,0,0,0)                               
         DC    AL1(ESFCWDQ),AL2(UC@WORD-TWAD),AL1(0)                            
         DC    AL1(ESFCPFQ,ESFCSFQ,0,0,0,0)                                     
         DC    AL1(ESFCFSQ),AL2(UC@FIRST-TWAD),AL1(0)                           
         DC    AL1(ESFCLSQ,ESFCALQ,ESFCPVQ,0,0,0)                               
         DC    AL1(ESFCLSQ),AL2(UC@LAST-TWAD),AL1(0)                            
         DC    AL1(ESFCFSQ,ESFCALQ,ESFCPVQ,0,0,0)                               
         DC    AL1(CPARFFUQ),AL2(ESFDVL-ESAVE),AL1(CPARSFFQ)                    
         DC    AL1(0,0,0,0,0,0)                                                 
         DC    AL1(CPARNM1Q),AL2(ESFCCOL-ESAVE),AL1(CPARSNMQ)                   
         DC    AL1(0,0,0,0,0,0)                                                 
         DC    AL1(EOT)                                                         
*                                                                               
CPARCHG  DC    AL1(ESFCALQ),AL2(UC@ALL-TWAD),AL1(0)                             
         DC    AL1(ESFCFSQ,ESFCLSQ,ESFCPVQ,0,0,0)                               
         DC    AL1(ESFCPFQ),AL2(UC@PREFX-TWAD),AL1(0)                           
         DC    AL1(ESFCSFQ,ESFCWDQ,0,0,0,0)                                     
         DC    AL1(ESFCSFQ),AL2(UC@SUFFX-TWAD),AL1(0)                           
         DC    AL1(ESFCPFQ,ESFCWDQ,0,0,0,0)                                     
         DC    AL1(ESFCPVQ),AL2(UC@PRV-TWAD),AL1(0)                             
         DC    AL1(ESFCALQ,ESFCFSQ,ESFCLSQ,0,0,0)                               
         DC    AL1(ESFCWDQ),AL2(UC@WORD-TWAD),AL1(0)                            
         DC    AL1(ESFCPFQ,ESFCSFQ,0,0,0,0)                                     
         DC    AL1(ESFCFSQ),AL2(UC@FIRST-TWAD),AL1(0)                           
         DC    AL1(ESFCLSQ,ESFCALQ,ESFCPVQ,0,0,0)                               
         DC    AL1(ESFCLSQ),AL2(UC@LAST-TWAD),AL1(0)                            
         DC    AL1(ESFCFSQ,ESFCALQ,ESFCPVQ,0,0,0)                               
         DC    AL1(CPARFFUQ),AL2(ESFDVL-ESAVE),AL1(CPARSFFQ)                    
         DC    AL1(0,0,0,0,0,0)                                                 
         DC    AL1(CPARFFLQ),AL2(ESCHVL-ESAVE),AL1(CPARSFFQ)                    
         DC    AL1(0,0,0,0,0,0)                                                 
         DC    AL1(CPARNM1Q),AL2(ESFCCOL-ESAVE),AL1(CPARSNMQ)                   
         DC    AL1(0,0,0,0,0,0)                                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* TABLE OF EDIT BLOCK LISTS                                           *         
***********************************************************************         
         SPACE 1                                                                
EBLTABD  DSECT                     OVERLAY BLOCK/LINE INIT TABLE                
EBLCTYPE DS    XL2                 COMMAND TYPE                                 
EBLINDS  DS    X                   BLKINDS VALUE                                
EBLNM    DS    X                   MAX NUMBER OF ENTRIES IN BLOCK LIST          
EBLDIS   DS    AL2                 DISPLACEMENT TO LIST                         
EBLFUNC  DS    AL2                 DISCPLACEMENT TO EDIT FUNCTION               
EBLPRECM DS    XL2                 COMPATIBLE PRIOR CMNDS                       
EBLPROCM DS    XL2                 COMPATIBLE POST CMNDS                        
EBLLNQ   EQU   *-EBLTABD                                                        
         SPACE 1                                                                
CLB0A    CSECT                                                                  
EBLTAB   DS    0X                                                               
*                                                                               
         DC    AL2(LENITXQ)                                                     
         DC    AL1(BLKITXT,L'ESEINSB/BLKLNQ),AL2(ESEINSB-ESAVE)                 
         DC    AL2(0)                                                           
         DC    AL2(LENAFTQ+LENBEFQ+LENADDQ)                                     
         DC    X'FFFF'                                                          
*                                                                               
         DC    AL2(LENINMQ)                                                     
         DC    AL1(BLKINUM,L'ESEINSB/BLKLNQ),AL2(ESEINSB-ESAVE)                 
         DC    AL2(0)                                                           
         DC    AL2(LENAFTQ+LENBEFQ+LENADDQ)                                     
         DC    X'FFFF'                                                          
*                                                                               
         DC    AL2(LENDELQ)                                                     
         DC    AL1(0,L'ESEDELB/BLKLNQ),AL2(ESEDELB-ESAVE)                       
         DC    AL2(ADEL-EWORKD)                                                 
         DC    AL2(LENAFTQ+LENBEFQ+LENADDQ)                                     
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LENREPQ)                                                     
         DC    AL1(0,L'ESEREPB/BLKLNQ),AL2(ESEREPB-ESAVE)                       
         DC    AL2(AREPT-EWORKD)                                                
         DC    AL2(LENAFTQ+LENBEFQ+LENADDQ)                                     
         DC    X'FFFF'                                                          
*                                                                               
         DC    AL2(LENMOVEQ)                                                    
         DC    AL1(0,L'ESEMVEB/BLKLNQ),AL2(ESEMVEB-ESAVE)                       
         DC    AL2(ACMLN-EWORKD)                                                
         DC    AL2(LENAFTQ+LENBEFQ+LENADDQ)                                     
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LENCOPYQ)                                                    
         DC    AL1(0,L'ESECPYB/BLKLNQ),AL2(ESECPYB-ESAVE)                       
         DC    AL2(ACMLN-EWORKD)                                                
         DC    AL2(LENAFTQ+LENBEFQ+LENADDQ)                                     
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LENAFTQ)                                                     
         DC    AL1(0,L'ESEAFLN/BLKLNQ),AL2(ESEAFLN-ESAVE,0)                     
         DC    AL2(LENADDQ)                                                     
         DC    AL2(LENDELQ+LENMOVEQ+LENCOPYQ+LENADDQ)                           
*                                                                               
         DC    AL2(LENBEFQ)                                                     
         DC    AL1(0,L'ESEBFLN/BLKLNQ),AL2(ESEBFLN-ESAVE,0)                     
         DC    AL2(LENADDQ)                                                     
         DC    AL2(LENDELQ+LENMOVEQ+LENCOPYQ+LENADDQ)                           
*                                                                               
         DC    AL2(LENOVERQ)                                                    
         DC    AL1(0,L'ESEOVLN/BLKLNQ),AL2(ESEOVLN-ESAVE)                       
         DC    AL2(0)                                                           
         DC    AL2(LENADDQ)                                                     
         DC    AL2(0)                                                           
*                                                                               
         DC    AL2(LENSUBQ)                                                     
         DC    AL1(0,L'ESESUBB/BLKLNQ),AL2(ESESUBB-ESAVE)                       
         DC    AL2(AADDS-EWORKD)                                                
         DC    AL2(LENAFTQ+LENBEFQ+LENADDQ)                                     
         DC    X'FFFF'                                                          
*                                                                               
         DC    AL2(LENTOTQ)                                                     
         DC    AL1(0,L'ESETOTB/BLKLNQ),AL2(ESETOTB-ESAVE)                       
         DC    AL2(AADDT-EWORKD)                                                
         DC    AL2(LENAFTQ+LENBEFQ+LENADDQ)                                     
         DC    X'FFFF'                                                          
*                                                                               
EBLTABX  DC    AL2(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* LINE EDIT COMMAND TABLE                                             *         
***********************************************************************         
         SPACE 1                                                                
LEDTABD  DSECT                     OVERLAY LINE EDIT COMMAND TABLE              
LEHEAD   DS    0X                  ** TABLE HEADER **                           
LEHLANG  DS    X                   LANGUAGE CODE                                
LEHLEN   DS    XL1                 LENGTH OF ENTRY                              
LEHLNQ   EQU   *-LEHEAD            LENGTH OF HEADER                             
         ORG   LEHEAD                                                           
LENTRY   DS    0X                  ** TABLE ENTRY **                            
LENCOMM  DS    CL3                 COMMAND                                      
LENCTYPE DS    XL2                 LINE EDIT COMMAND TYPE                       
LENITXQ  EQU   X'8000'             INSERT TEXT LINE(S)                          
LENINMQ  EQU   X'4000'             INSERT NUMERICAL LINE(S)                     
LENDELQ  EQU   X'2000'             DELETE                                       
LENMOVEQ EQU   X'1000'             MOVE                                         
LENCOPYQ EQU   X'0800'             COPY                                         
LENREPQ  EQU   X'0400'             REPEAT                                       
LENAFTQ  EQU   X'0200'             AFTER                                        
LENBEFQ  EQU   X'0100'             BEFORE                                       
LENOVERQ EQU   X'0080'             OVER                                         
LENSUBQ  EQU   X'0040'             SUB-TOTAL                                    
LENTOTQ  EQU   X'0020'             TOTAL                                        
LENLNQ   EQU   *-LENTRY            LENGTH OF ENTRY                              
         SPACE 1                                                                
*                                  EQUATE FOR ADDING TYPE LINE COMMANDS         
LENADDQ  EQU   LENITXQ+LENINMQ+LENREPQ+LENSUBQ+LENTOTQ                          
         SPACE 1                                                                
CLB0A    CSECT                                                                  
LEDCTAB  DS    0X    LINE EDIT COMMAND TABLE                                    
*                                                                               
LEGBR    DC    AL1(LANGEUK,LEGBRX+2-LEGBR)                                      
         DC    C'I  ',AL2(LENITXQ)                                              
         DC    C'#  ',AL2(LENINMQ)                                              
         DC    C'DD ',AL2(LENDELQ)                                              
         DC    C'MM ',AL2(LENMOVEQ)                                             
         DC    C'CC ',AL2(LENCOPYQ)                                             
         DC    C'R  ',AL2(LENREPQ)                                              
         DC    C'A  ',AL2(LENAFTQ)                                              
         DC    C'B  ',AL2(LENBEFQ)                                              
         DC    C'O  ',AL2(LENOVERQ)                                             
         DC    C'S  ',AL2(LENSUBQ)                                              
         DC    C'T  ',AL2(LENTOTQ)                                              
LEGBRX   DC    AL2(EOT)                                                         
         SPACE 1                                                                
LEGER    DC    AL1(LANGGER,LEGERX+2-LEGER)                                      
         DC    C'I  ',AL2(LENITXQ)                                              
         DC    C'#  ',AL2(LENINMQ)                                              
         DC    C'DD ',AL2(LENDELQ)                                              
         DC    C'MM ',AL2(LENMOVEQ)                                             
         DC    C'CC ',AL2(LENCOPYQ)                                             
         DC    C'R  ',AL2(LENREPQ)                                              
         DC    C'A  ',AL2(LENAFTQ)                                              
         DC    C'B  ',AL2(LENBEFQ)                                              
         DC    C'O  ',AL2(LENOVERQ)                                             
         DC    C'S  ',AL2(LENSUBQ)                                              
         DC    C'T  ',AL2(LENTOTQ)                                              
LEGERX   DC    AL2(EOT)                                                         
         SPACE 1                                                                
LEDUT    DC    AL1(LANGDUT,LEDUTX+2-LEDUT)                                      
         DC    C'I  ',AL2(LENITXQ)                                              
         DC    C'#  ',AL2(LENINMQ)                                              
         DC    C'AA ',AL2(LENDELQ)                                              
         DC    C'VV ',AL2(LENMOVEQ)                                             
         DC    C'KK ',AL2(LENCOPYQ)                                             
         DC    C'H  ',AL2(LENREPQ)                                              
         DC    C'N  ',AL2(LENAFTQ)                                              
         DC    C'E  ',AL2(LENBEFQ)                                              
         DC    C'O  ',AL2(LENOVERQ)                                             
         DC    C'S  ',AL2(LENSUBQ)                                              
         DC    C'T  ',AL2(LENTOTQ)                                              
LEDUTX   DC    AL2(EOT)                                                         
         SPACE 1                                                                
LEUSA    DC    AL1(LANGEUS,LEUSAX+2-LEUSA)                                      
         DC    C'I  ',AL2(LENITXQ)                                              
         DC    C'#  ',AL2(LENINMQ)                                              
         DC    C'DD ',AL2(LENDELQ)                                              
         DC    C'MM ',AL2(LENMOVEQ)                                             
         DC    C'CC ',AL2(LENCOPYQ)                                             
         DC    C'R  ',AL2(LENREPQ)                                              
         DC    C'A  ',AL2(LENAFTQ)                                              
         DC    C'B  ',AL2(LENBEFQ)                                              
         DC    C'O  ',AL2(LENOVERQ)                                             
         DC    C'S  ',AL2(LENSUBQ)                                              
         DC    C'T  ',AL2(LENTOTQ)                                              
LEUSAX   DC    AL2(EOT)                                                         
         SPACE 1                                                                
         DC    AL2(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* NUMBER TYPE TABLE                                                   *         
***********************************************************************         
         SPACE 1                                                                
NUMTABD  DSECT                                                                  
NUMFLD   DS    XL1                 BLFFLD EQUATE                                
NUMFLD2  DS    XL1                 BLFFLD EQUATE FOR TOTAL LINE ONLY            
NUMTX    DS    XL1                 TXNUM EQUATE                                 
NUMMASK  DS    XL1                 MASK TO CARRY FWTEL ON RECORD                
NUMDIS   DS    H                   DISPLACEMENT TO DISPLAY ROUTINE              
NUMVAL   DS    H                   DISPLACEMENT TO VALIDATION ROUTINE           
NUMAMT   DS    H                   DISPLACEMENT TO AMT IN TXD                   
NUMTABL  EQU   *-NUMTABD                                                        
         SPACE 1                                                                
CLB0A    CSECT                                                                  
NUMTAB   DS    0X                  * NUMBERS IN ORDER OF VALIDATION *           
*                                                                               
         DC    AL1(BLFFHRSQ,0,TXNHRS,TXNRAT)                                    
         DC    AL2(DISHRS-CLB0A,VALHRS-CLB0A,TXHRS-TXD)                         
*                                                                               
         DC    AL1(BLFFRATQ,0,TXNRAT,TXNHRS)                                    
         DC    AL2(DISRAT-CLB0A,VALRAT-CLB0A,TXRAT-TXD)                         
*                                                                               
         DC    AL1(BLFFNETQ,BLFFNET2,TXNNET,FF)                                 
         DC    AL2(DISNET-CLB0A,VALNET-CLB0A,TXNET-TXD)                         
*                                                                               
         DC    AL1(BLFFCOMQ,0,TXNCOM,FF)                                        
         DC    AL2(DISCOM-CLB0A,VALCOM-CLB0A,TXCOM-TXD)                         
*                                                                               
         DC    AL1(BLFFGRSQ,BLFFGRS2,TXNGRS,0)                                  
         DC    AL2(DISGRS-CLB0A,VALGRS-CLB0A,TXGRS-TXD)                         
*                                                                               
         DC    AL1(BLFFCMRQ,0,TXNCMR,FF)                                        
         DC    AL2(DISCMR-CLB0A,VALCMR-CLB0A,TXCMR-TXD)                         
*                                                                               
NUMTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* TRANSLATE TO UPPER CASE TABLES                                      *         
***********************************************************************         
         SPACE 1                                                                
*                                  TRANSLATE TO UPPER TABLE LIST                
TRUPTAB  DC    A(TRUPUKUS-CLB0A,TRUPUKUS-CLB0A)                                 
         DC    A(TRUPUKUS-CLB0A,TRUPGER-CLB0A)                                  
         DC    A(TRUPFRA-CLB0A,TRUPUKUS-CLB0A)                                  
         DC    A(TRUPUKUS-CLB0A,TRUPDUT-CLB0A)                                  
         SPACE 1                                                                
*                   0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.                            
TRUPUKUS DC    XL16'00404040404040404040404040404040' 00-0F                     
         DC    XL16'40404040404040404040404040404040' 10-1F                     
         DC    XL16'40404040404040404040404040404040' 20-2F                     
         DC    XL16'40404040404040404040404040404040' 30-3F                     
         DC    XL16'404040404040404040404A4B4C4D4E4F' 40-4F                     
         DC    XL16'504040404040404040405A5B5C5D5E5F' 50-5F                     
         DC    XL16'606140404040404040406A6B6C6D6E6F' 60-6F                     
         DC    XL16'404040404040404040797A7B7C7D7E7F' 70-7F                     
         DC    XL16'40C1C2C3C4C5C6C7C8C940404040408F' 80-8F                     
         DC    XL16'40D1D2D3D4D5D6D7D8D9404040404040' 90-9F                     
         DC    XL16'40A1E2E3E4E5E6E7E8E940ABAC404040' A0-AF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F940BBBC4040BF' B0-BF                     
         DC    XL16'C0C1C2C3C4C5C6C7C8C940CBCC404040' C0-CF                     
         DC    XL16'D0D1D2D3D4D5D6D7D8D9404040404040' D0-DF                     
         DC    XL16'E040E2E3E4E5E6E7E8E940EBEC404040' E0-EF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F9FA4040404040' F0-FF                     
         SPACE 1                                                                
*                   0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.                            
TRUPGER  DC    XL16'00404040404040404040404040404040' 00-0F                     
         DC    XL16'40404040404040404040404040404040' 10-1F                     
         DC    XL16'40404040404040404040404040404040' 20-2F                     
         DC    XL16'40404040404040404040404040404040' 30-3F                     
         DC    XL16'404040404040404040404A4B4C4D4E4F' 40-4F                     
         DC    XL16'504040404040404040405A5B5C5D5E5F' 50-5F                     
         DC    XL16'60614040404040404040E06B6C6D6E6F' 60-6F O UMLAUT 6A         
         DC    XL16'404040404040404040797A7B7C7D7E7F' 70-7F                     
         DC    XL16'40C1C2C3C4C5C6C7C8C940404040408F' 80-8F                     
         DC    XL16'40D1D2D3D4D5D6D7D8D9404040404040' 90-9F                     
         DC    XL16'40A1E2E3E4E5E6E7E8E940ABAC404040' A0-AF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F940BBBC4040BF' B0-BF                     
         DC    XL16'4AC1C2C3C4C5C6C7C8C940CBCC404040' C0-CF A UMLAUT C0         
         DC    XL16'5AD1D2D3D4D5D6D7D8D9404040404040' D0-DF U UMLAUT D0         
         DC    XL16'E040E2E3E4E5E6E7E8E940EBEC404040' E0-EF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F9FA4040404040' F0-FF                     
         SPACE 1                                                                
*                   0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.                            
TRUPFRA  DC    XL16'00404040404040404040404040404040' 00-0F                     
         DC    XL16'40404040404040404040404040404040' 10-1F                     
         DC    XL16'40404040404040404040404040404040' 20-2F                     
         DC    XL16'40404040404040404040404040404040' 30-3F                     
         DC    XL16'404040404040404040404A4B4C4D4E4F' 40-4F                     
         DC    XL16'504040404040404040405A5B5C5D5E5F' 50-5F                     
         DC    XL16'60614040404040404040E46B6C6D6E6F' 60-6F U CIRC  6A          
         DC    XL16'404040404040404040797A7BC17D7E7F' 70-7F A GRAVE 7C          
         DC    XL16'40C1C2C3C4C5C6C7C8C940404040408F' 80-8F                     
         DC    XL16'40D1D2D3D4D5D6D7D8D9404040404040' 90-9F                     
         DC    XL16'40A1E2E3E4E5E6E7E8E940ABAC404040' A0-AF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F940BBBC4040BF' B0-BF                     
         DC    XL16'C5C1C2C3C4C5C6C7C8C940CBCC404040' C0-CF E ECUTE C0          
         DC    XL16'C5D1D2D3D4D5D6D7D8D9404040404040' D0-DF E GRAVE D0          
         DC    XL16'C340E2E3E4E5E6E7E8E940EBEC404040' E0-EF C CEDIL E0          
         DC    XL16'F0F1F2F3F4F5F6F7F8F9FA4040404040' F0-FF                     
         SPACE 1                                                                
*                   0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.                            
TRUPDUT  DC    XL16'00404040404040404040404040404040' 00-0F                     
         DC    XL16'40404040404040404040404040404040' 10-1F                     
         DC    XL16'40404040404040404040404040404040' 20-2F                     
         DC    XL16'40404040404040404040404040404040' 30-3F                     
         DC    XL16'404040404040404040404A4B4C4D4E4F' 40-4F                     
         DC    XL16'504040404040404040405A5B5C5D5E5F' 50-5F                     
         DC    XL16'606140404040404040406A6B6C6D6E6F' 60-6F                     
         DC    XL16'404040404040404040797A7B7C7D7E7F' 70-7F                     
         DC    XL16'40C1C2C3C4C5C6C7C8C940404040408F' 80-8F                     
         DC    XL16'40D1D2D3D4D5D6D7D8D9404040404040' 90-9F                     
         DC    XL16'40A1E2E3E4E5E6E7E8E940ABAC404040' A0-AF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F940BBBC4040BF' B0-BF                     
         DC    XL16'C0C1C2C3C4C5C6C7C8C940CBCC404040' C0-CF                     
         DC    XL16'D0D1D2D3D4D5D6D7D8D9404040404040' D0-DF                     
         DC    XL16'E040E2E3E4E5E6E7E8E940EBEC404040' E0-EF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F9FA4040404040' F0-FF                     
         EJECT                                                                  
***********************************************************************         
* GLOBAL W/S                                                          *         
***********************************************************************         
         SPACE 1                                                                
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
         SPACE 1                                                                
***********************************************************************         
* TERMINAL WORK AREA                                                  *         
***********************************************************************         
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBF5BD                                                      
         SPACE 1                                                                
*                                                                               
         DS    0X                                                               
         EJECT                                                                  
***********************************************************************         
* LINE COMMAND FIELD                                                  *         
***********************************************************************         
         SPACE 1                                                                
COMFLDD  DSECT                                                                  
COMFLDH  DS    XL8                 FIELD HEADER                                 
COMFLD   DS    CL3                 FIELD                                        
COMFLDX  DS    XL8                 EXTENDED FIELD HEADER                        
COMFLDL  EQU   *-COMFLDD                                                        
         SPACE 1                                                                
***********************************************************************         
* LINE NUMBER FIELD                                                   *         
***********************************************************************         
         SPACE 1                                                                
LNUMFLDD DSECT                     DSECT FOR LINE NUMBER                        
LNUMFLDH DS    XL8                 FIELD HEADER                                 
LNUMFLD  DS    CL4                 FIELD                                        
LNUMFLDL EQU   *-LNUMFLDD                                                       
         SPACE 1                                                                
***********************************************************************         
* EDIT SCREEN HEADER FIELDS                                           *         
***********************************************************************         
         SPACE 1                                                                
TOPD     DSECT                     TOP OF PARAGRAPH FIELDS                      
TOPACTH  DS    CL8                 SUB ACTION HEADER                            
TOPACT   DS    CL3                                                              
TOPBANH  DS    CL8                 LINE BANNER                                  
TOPBAN   DS    CL69                                                             
TOPNUMH  DS    CL8                 LINE NUMBER BANNER                           
TOPNUM   DS    CL4                                                              
         SPACE 1                                                                
***********************************************************************         
* BLOCK VALUE LIST                                                    *         
***********************************************************************         
         SPACE 1                                                                
BLKVALD  DSECT                                                                  
BLKSTRT  DS    X                   START OF BLOCK                               
BLKEND   DS    X                   END OF BLOCK                                 
BLKLEN   DS    X                   L'(BLK IF NUMERIC CMND ELSE ZERO)            
BLKINDS  DS    X                   INDICATOR BYTE                               
BLKITXT  EQU   X'80'               INSERTING TEXT                               
BLKINUM  EQU   X'40'               INSERTING NUMERICAL LINE                     
BLKLNQ   EQU   *-BLKVALD                                                        
         SPACE 1                                                                
***********************************************************************         
* CONFLICT TABLE                                                      *         
***********************************************************************         
         SPACE 1                                                                
CONTABD  DSECT                     CONFLICT TABLE                               
CONCTYPE DS    X                   COMMAND TYPE                                 
CONCOMP  DS    X                   COMPATIBLE PRIOR NUMERIC COMMANDS            
CONLNQ   EQU   *-CONTABD                                                        
         SPACE 1                                                                
***********************************************************************         
* COMMAND PARAMETER TABLE                                             *         
***********************************************************************         
         SPACE 1                                                                
CPARTABD DSECT                     COMMAND PARAMTERE TABLE                      
CPARTYPE DS    X                   PARAMETER TYPE                               
CPARFFUQ EQU   1                   FREE FORM TEXT PARAMETER UPPER CASE          
CPARFFLQ EQU   2                   FREE FORM TEXT PARAMETER LOWER CASE          
CPARNM1Q EQU   1                   NUMERIC PARAM 1                              
CPARWORD DS    AL2                 PARAM WRD/DISP TO FREE FRM TEXT PARA         
CPARSTAT DS    X                   STATUS BYTE                                  
CPARSFFQ EQU   X'80'               FREE FORM TEXT PARAMETER                     
CPARSNMQ EQU   X'40'               NUMERIC PARAMETER                            
CPARSDFQ EQU   X'20'               PARAMETER VALID FOR DRAFT BILLS ONLY         
CPARINCM DS    6AL1                INCOMPATIBLE PARAMETER LIST                  
CPARLNQ  EQU   *-CPARTABD          LENGTH OF COMMAND TABLE ENTRY                
         SPACE 1                                                                
***********************************************************************         
* STANDARD COMMENT TABLE                                              *         
***********************************************************************         
         SPACE 1                                                                
SCTABD   DSECT                                                                  
SCTLIN   DS    XL(L'LRLINE)        START LINE NUMBER                            
SCTNUM   DS    XL1                 NUMBER OF LINES IN COMMENT                   
SCTCOD   DS    XL(L'SCMKCODE)      STANDARD COMMENT CODE                        
SCTCOLF  DS    XL1                 COMMENT START NUMBER                         
SCTABL   EQU   *-SCTABD                                                         
SCTABN   EQU   16                  MAXIMUM TABLE ENTRIES                        
         EJECT                                                                  
***********************************************************************         
* TEXT LINE DSECT                                                     *         
***********************************************************************         
         SPACE 1                                                                
TXD      DSECT                                                                  
TXLSTA   DS    XL1                 LINE STATUS (SEE PBRKLSTA)                   
TXINDS   DS    XL1                 INDICATOR BYTE                               
TXIINS   EQU   X'80'               INSERT LINE                                  
TXIITH   EQU   X'40'               TEXT INPUT THIS TIME                         
TXNUM    DS    XL1                 WHICH NUMBERS ARE ON LINE                    
TXNNET   EQU   X'80'               NET                                          
TXNCOM   EQU   X'40'               COMMISSION                                   
TXNGRS   EQU   X'20'               GROSS                                        
TXNHRS   EQU   X'10'               HOURS                                        
TXNRAT   EQU   X'08'               RATE                                         
TXNCMR   EQU   X'04'               COMMISSION RATE                              
TXNINP   DS    XL1                 WHICH NUMBERS INPUT TO                       
TXLINE#  DS    XL1                 RECORD LINE NUMBER                           
         DS    XL3                 N/D                                          
TXDATA   DS    CL132               TEXT                                         
TXTWA    DS    A                   A(TWA WHEN FIELDS SAVED)                     
TXACOM   DS    A                   A(COMMAND FIELD)                             
TXATXT   DS    A                   A(FIRST TEXT FIELD)                          
TXANUM   DS    A                   A(LINE NUMBER FIELD)                         
*                                                                               
TXAMT    DS    0PL8                AMOUNTS                                      
TXNET    DS    PL8                 NET AMOUNT                                   
TXCOM    DS    PL8                 COMMISSION AMOUNT                            
TXGRS    DS    PL8                 GROSS AMOUNT                                 
TXHRS    DS    PL8                 HOURS AMOUNT                                 
TXRAT    DS    PL8                 RATE AMOUNT                                  
TXCMR    DS    PL8                 COMMISSION RATE                              
TXAMTS   EQU   *-TXAMT                                                          
TXAMTN   EQU   TXAMTS/L'TXAMT                                                   
*                                                                               
         DS    XL(256-(*-TXD))                                                  
TXLINLAY DS    XL(LAYOUTS)         LAYOUT FOR WHOLE LINE                        
TXSCRLAY DS    XL(LAYOUTS)         LAYOUT FOR SCREEN                            
*                                                                               
TXL      EQU   512                                                              
         DS    XL(TXL-(*-TXD))                                                  
         DS    0X                                                               
         SPACE 1                                                                
***********************************************************************         
* LAYOUT TABLE                                                        *         
***********************************************************************         
         SPACE 1                                                                
LAYOUTD  DSECT                                                                  
LAYCOLF  DS    XL1                 COLUMN NUMBER FOR START OF FIELD             
LAYCOLN  DS    XL1                 NUMBER OF COLUMNS IN FIELD                   
LAYFLD   DS    XL1                 BLFFLD OR ZERO                               
LAYINDS  DS    XL1                 INDICATOR BYTE                               
LAYIPR   EQU   X'80'               PROTECT FIELD                                
LAYFHD   DS    XL1                 DISP. TO FIELD HEADER ON LINE                
LAYOUTL  EQU   *-LAYOUTD                                                        
LAYOUTN  EQU   20                                                               
LAYOUTS  EQU   LAYOUTL*LAYOUTN+1                                                
         EJECT                                                                  
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
EWORKD   DSECT                                                                  
RELO     DS    A                   RELOCATION FACTOR                            
*                                                                               
ARETAB   DS    0A                  RELOCATED A(TABLES)                          
AEBLTAB  DS    A                                                                
ALEDCTAB DS    A                                                                
ACMNDTAB DS    A                                                                
ATRUPTAB DS    A                                                                
ANUMTAB  DS    A                                                                
ARETABN  EQU   (*-ARETAB)/L'ARETAB                                              
         DS    (ATABN-ARETABN)X    ENSURE ATABN=ARETABN                         
         DS    (ARETABN-ATABN)X                                                 
*                                                                               
EHALF    DS    H                                                                
*                                                                               
EWINDS   DS    XL1                 INDICATOR BYTE                               
EWIDINS  EQU   X'80'               FIRST INSET LINE DISPLAYED                   
EWITOT   EQU   X'40'               UPDATE TOTALS FOR PARAGRAPH                  
*                                                                               
SVFVINDX DS    X                   SAVED FVINDX                                 
EDMSGNO  DS    XL(L'FVMSGNO)       SAVED ERROR VALUES USED BY EDTSCRN           
EDADDR   DS    XL(L'FVADDR)                                                     
EDERRNDX DS    XL(L'FVERRNDX)                                                   
EDCURS   DS    XL(L'FVCURS)                                                     
EDXTRA   DS    XL(L'FVXTRA)                                                     
PARAFLAG DS    X                   PARAGRAPH FLAG                               
PERRORQ  EQU   X'80'               ERROR FOUND IN PARAGRAPH INPUT               
VOPTFLAG DS    X                   COMMAND FLAG                                 
VOPTDUPQ EQU   X'80'               DUPLICATE PARAMETER                          
VOPTLNH  DS    CL(L'BASOPTH)       COMMAND LINE DUMMY HEADER                    
VOPTLN   DS    CL(L'BASOPT)        COMMAND LINE                                 
CMNSTNO  DS    X                   NUMBER OF TEXT STRINGS ON COMM LINE          
CMNFFNO  DS    X                   NUMBER OF FREE FORM TEXT PARAMS              
CMNNUMNO DS    X                   NUMBER OF NUMERIC PARAMS                     
PRECCMP  DS    XL2                 COMPATIBLE PRECEEDING COMMANDS               
PROCCMP  DS    XL2                 COMPATIBLE PROCEEDING COMMANDS               
FSVAL    DS    0XL6                DINK INDICATORS FOR FREE FORM STRING         
FSVALST1 DS    X                   START DINK 1                                 
FSVALEN1 DS    X                   END DINK 1                                   
FSVALTP1 DS    X                   DINK TYPE 1                                  
FSVALST2 DS    X                   START DINK 2                                 
FSVALEN2 DS    X                   END DINK 2                                   
FSVALTP2 DS    X                   DINK TYPE 2                                  
TOTINRP  DS    X                   TOTAL INSERTS AND REPEATS                    
TOLINE   DS    X                   TO POSITION                                  
TOREPNO  DS    X                   NUMBER OF TIMES TO BE MOVED/COPIED           
AUTOINS  DS    X                   AUTO INSERT LINE                             
AUTOBLKI DS    X                   BLKINDS VALUE FOR AUTO INSERT                
UNUSEINS DS    XL(MAXSLINQ+1)                                                   
CHGPARMS DS    0X                  CHANGE TEXT PARMS                            
CHGCOLOF DS    X                   OFFSET TO CHANGE COLUMN                      
CHGLINOF DS    X                   OFFSET TO CHANGE LINE NUMBER                 
CHGATX   DS    A                   ADDRESS OF LINE TO BE CHANGED                
CHGLNQ   EQU   *-CHGPARMS                                                       
LRPARMS  DS    0X                                                               
LRMODE   DS    X                                                                
LRADDQ   EQU   X'80'               ADD A RECORD OR (CHANGE UNACTIVE)            
LRCHANGE EQU   X'40'               CHANGE AN EXISTING RECORD (ACTIVE)           
LROVRWRT EQU   X'20'               OVERWITE EXISTING RECORD  (ACTIVE)           
LRMERGE  EQU   X'10'               MERGE EXISTING RECORD     (ACTIVE)           
LRLINE   DS    X                   LOGICAL LINE NUMBER                          
LRPARA   DS    X                   PARAGRAPH OF LINE                            
LRCOLF   DS    X                   START COLUMN FOR MERGE                       
LRLNQ    EQU   *-LRPARMS                                                        
PRPARMS  DS    0X                                                               
PRMODE   DS    X                                                                
PRADDQ   EQU   X'80'               ADD A RECORD OR (CHANGE UNACTIVE)            
PRCHANGQ EQU   X'40'               CHANGE AN EXISTING RECORD (ACTIVE)           
PRLNQ    EQU   *-PRPARMS                                                        
BRMODE   DS    X                                                                
BRADDQ   EQU   X'80'               ADD A RECORD OR (CHANGE UNACTIVE)            
BRCHANGE EQU   X'40'               CHANGE AN EXISTING RECORD (ACTIVE)           
BRLNQ    EQU   *-PRPARMS                                                        
ALTPARMS DS    0X                                                               
ALTBSTRT DS    X                   DISP TO FIRST AFFECTED BLOCK VALUE           
ALTBEND  DS    X                   DISP TO LAST AFFECTED BLOCK VALUE            
ALTBDVAL DS    X                   DISPLACEMENT VALUE                           
ALTBCTYP DS    XL2                 EXCEPT COMMAND TYPE LIST SPECIFIED           
ALTBSTAT DS    X                   STATUS BYTE                                  
ALTBSUBQ EQU   X'80'               SUBTRACT DISP VAL (DEFAULT ADD)              
ALTLNQ   EQU   *-ALTPARMS                                                       
ATRUP    DS    A                   A(TRANSLATE TO UPPER TABLE FOR LANG)         
ABLOCK   DS    A                   A(BLOCK VALUE LIST)                          
BLOCKLN  DS    X                   L'(BLOCK VALUE LIST)                         
BFR1     DS    X                   BLOCK FROM LINE NUMBER                       
BFR2     DS    X                   BLOCK FROM LINE NUMBER                       
BLEN     DS    X                   BLOCK FROM LINE NUMBER                       
BINDS    DS    X                   BLOCK INDICATORS                             
ALENTRY  DS    A                   A(LINE EDIT COMMAND ENTRIES)                 
ACMNFFT  DS    A                   A(FREE FORM TEXT PARAM LIST)                 
WORKLST  DS    XL512               WORK AREA FOR INDEX LISTS                    
LINEFLAG DS    X                   LINE INEX INDICATOR STATUS FLAG              
LCHANGEQ EQU   X'80'               LINE INDEX LIST CHANGED                      
LSCIBLKQ EQU   X'40'               STANDARD COMMENT IN BLOCK                    
NXTLOFF  DS    X                   NEXT LINE INDEX OFFSET IN INDX LIST          
TSARLIN# DS    X                   SCREEN LINE# (USED BY RESTX/SAVTX)           
RECMODE  DS    X                   RECORD ACTION MODE                           
RECACTQ  EQU   X'80'               ACTIVE RECORD TO BE CHANGED                  
RECUACTQ EQU   X'40'               UNACTIVE RECORD TO BE USED                   
RECADDQ  EQU   X'20'               RECORD TO BE ADDED                           
CMPARM   DS    0X                  COPY/MOVE ROUTINE PARMS                      
CMPMODE  DS    X                   MODE BYTE                                    
CMPCOPYQ EQU   X'80'               COPY MODE                                    
CMPMOVEQ EQU   X'40'               MOVE MODE                                    
CMPBEFRQ EQU   X'20'               COPY/MOVE BEFORE TO POS (DEFLT AFTR)         
CMPTO    DS    X                   COPY/MOVE TO OFFSET                          
CMPFROM1 DS    X                   COPY/MOVE FROM OFFSET 1 (START)              
CMPFROM2 DS    X                   COPY/MOVE FROM OFFSET 2 (END)                
CMPAILST DS    A                   A(PARAGRAPH/LINE INDEX LIST)                 
CMPILLN  DS    X                   L'(INDEX LIST)                               
CMLNQ    EQU   *-CMPARM                                                         
INPARM   DS    0X                  INITIALISE BLOCK VALUE LIST PARMS            
INBLSTNM DS    X                   LENGTH OF BLOCK VALUE LIST                   
INABLST  DS    A                   A(BLOCK VALUE LIST)                          
FRMLLST  DS    CL200                                                            
*                                                                               
MY@NRTV  DS    CL1                                                              
*                                                                               
SCVALS   DS    0X                  STANDARD COMMENT ROUTINE VALUES              
*                                  (USED BY COMBLD AND COMPR)                   
SC1IND   DS    X                   INDICATOR BYTE - 1                           
SC1IRCRS EQU   X'01'               RECURSING FOR EMBEDDED COMMENT               
SCCOLF   DS    X                   STANDARD COMMENT COLUMN START                
SCCOLN   DS    X                   COLUMNS AVAILABLE FOR COMMENT                
SCLTOT   DS    X                   TOTAL LINES TO BE ADDED IN COMPRC            
SCCODE   DS    CL6                 LEFT ALIGNED CODE                            
SCVALSL  EQU   *-SCVALS                                                         
SCTAB    DS    (SCTABN)XL(SCTABL)  STANDARD COMMENT TABLE                       
*                                                                               
         DS    0A                                                               
SLVALS   DS    0XL6                SETLAD ROUTINE VALUES                        
SLALINE  DS    A                   A(CURRENT LINE)                              
SLAD     DS    H                   DATA SCREEN ADDRESS OF CURRENT LINE          
*                                                                               
UTPARA   DS    XL1                 UPDTOT PARAGRAPH #                           
*                                                                               
ILFLD    DS    XL1                 INSLAY FIELD NUMBER                          
ILCOLF   DS    XL1                 INSLAY COLUMN #                              
ILCOLN   DS    XL1                 INSLAY # COLUMNNS                            
*                                                                               
         DS    0A                                                               
ESAVE    DS    0XL2048             OVERLAY SAVED VALUES                         
ESATWA   DS    A                   A(TWA LAST TIME)                             
ESARELO  DS    0A                  RELOCATABLE TWA ADDRESSES                    
ESAHEAD1 DS    A                   A(HEADLINE 1)                                
ESATOP   DS    A                   A(ACT/HEADLINE2/LINE HEADING)                
ESACOM1  DS    A                   A(1ST COMMAND FIELD)                         
ESATXT1  DS    A                   A(1ST TEXT FIELD)                            
ESANUM1  DS    A                   A(1ST LINE NUMBER FIELD)                     
ESACOMX  DS    A                   A(LAST COMMAND FIELD)                        
ESATXTX  DS    A                   A(LAST TEXT FIELD)                           
ESANUMX  DS    A                   A(LAST LINE NUMBER FIELD)                    
ESARELON EQU   (*-ESARELO)/L'ESARELO                                            
*                                                                               
ESALLNET DS    PL8                 ALLOCATED NET                                
ESALLCOM DS    PL8                 ALLOCATED COMMISSION                         
ESBILNET DS    PL8                 BILL TOTAL NET                               
ESBILCOM DS    PL8                 BILL TOTAL COMMISSION                        
ESPARNET DS    PL8                 CURRENT PARAGRAPH TOTAL NET                  
ESPARCOM DS    PL8                 CURRENT PARAGRAPH TOTAL COMMISSION           
ESPAROVR DS    PL8                 PARAGRAPH OVER ALLOCATION                    
ESPARTAX DS    XL1                 PARAGRAPH TAX TYPE                           
ESPARTY2 DS    XL1                 PGHHTYP2                                     
*                                                                               
ESCURD   DS    XL(L'TIOBCURD)      DISPLACEMENT TO CURSOR                       
*                                                                               
ESEVAL   DS    0X                  LINE EDIT VALUES                             
ESECPYB  DS    XL(BLKLNQ)          COPY BLOCK LIST                              
ESEMVEB  DS    XL(BLKLNQ)          MOVE BLOCK LIST                              
ESECMPA  DS    X                   COPY/MOVE BLOCK PARAGRAPH INDEX              
ESEDELB  DS    XL(16*BLKLNQ)       DELETE BLOCK LIST                            
ESEREPB  DS    XL(16*BLKLNQ)       REPEAT BLOCK                                 
ESEINSB  DS    XL(16*BLKLNQ)       INSERT BLOCK                                 
ESESUBB  DS    XL(5*BLKLNQ)        SUB-TOTAL BLOCK                              
ESETOTB  DS    XL(BLKLNQ)          TOTAL BLOCK                                  
*                                                                               
ESETOLN  DS    0XL(3*BLKLNQ)                                                    
ESEBFLN  DS    XL(BLKLNQ)          TO LINE (BEFORE)                             
ESEAFLN  DS    XL(BLKLNQ)          TO LINE (AFTER)                              
ESEOVLN  DS    XL(BLKLNQ)          TO LINE (OVER)                               
ESETOAC  DS    X                   NUMBER OF ACTIVE LINES IN TO PARA            
ESETOTY  DS    XL2                 TO TYPE AFTER/BEFORE OR OVER                 
ESETOPA  DS    X                   TO PARAGRAPH                                 
ESETOPO  DS    X                   TO PARAGRAPH OFFSET                          
ESETOLQ  EQU   *-ESETOLN           LENGTH OF TO VALUES                          
*                                                                               
ESECTYP  DS    XL2                 EDIT COMMANDS                                
ESEBFLG  DS    X                   BLOCK FLAG                                   
BLKCOUTQ EQU   X'80'               BLOCK COMMAND OUTSTANDING                    
BLKPENDQ EQU   X'40'               COPY/MOVE IS PENDING                         
BLKPCONQ EQU   X'20'               POTENTIAL CONFLICT                           
ESECONF  DS    A                   A(POTENTIAL CONFLICT LINE)                   
ESELNQ   EQU   *-ESEVAL                                                         
*                                                                               
ESSTLIN  DS    X                   LINE NUM AT START OF CURRENT SCREEN          
ESCOL#1  DS    X                   COLM NUM AT START OF CURRENT SCREEN          
ESCOL#X  DS    X                   COLM NUM AT END OF SCREEN                    
ES#COL   DS    X                   NUMBER OF COLUMNS ON THE SCREEN              
*                                                                               
ESNUMLIN DS    X                   NUMBER OF LINES ON SCREEN                    
ESSCRLST DS    16X                 LIST OF RECORD LINE #S ON SCREEN             
*                                                                               
ESCLVAL  DS    0X                  COMMAND LINE VALUES                          
ESCMND   DS    XL1                 MAIN COMMAND LINE TYPE                       
*                                                                               
ESFCPAR  DS    XL1                 FIND/ CHANGE SUB COMMAND LINE TYPES          
ESFCALQ  EQU   X'80'               ALLQ                                         
ESFCPFQ  EQU   X'40'               PREFFIX                                      
ESFCSFQ  EQU   X'20'               SUFFIX                                       
ESFCPVQ  EQU   X'10'               PREVIOUS                                     
ESFCWDQ  EQU   X'08'               WORD                                         
ESFCFSQ  EQU   X'04'               FIRST                                        
ESFCLSQ  EQU   X'02'               LAST                                         
*                                                                               
ESCPAR   DS    XL1                 COLUMNS SUB COMMAND TYPE                     
ESCYESQ  EQU   X'80'               YES                                          
ESCNOQ   EQU   X'40'               NO                                           
*                                                                               
ESDPAR   DS    X                   DELETE SUB COMMAND TYPE                      
ESDALLQ  EQU   X'80'               ALL LINES                                    
ESDPARQ  EQU   X'40'               PARAGRAPH                                    
ESFDVL   DS    0X                                                               
ESFDLN   DS    X                                                                
ESFDTXT  DS    CL20                                                             
ESFDLNQ  EQU   *-ESFDVL                                                         
ESFCCOL  DS    X                   FIND/CHANGE COLUMN                           
ESCHVL   DS    0X                                                               
ESCHLN   DS    X                                                                
ESCHTXT  DS    CL20                                                             
ESCHLNQ  EQU   *-ESCHVL                                                         
ESCLLNQ  EQU   *-ESCLVAL                                                        
*                                                                               
ESCLFLG  DS    X                   COLUMN DISPLAY FLAG                          
ESCLRLQ  EQU   X'80'               DISP RULE LINE (DEFAULT IS 'TEXT')           
ESCLTMQ  EQU   X'40'               DISP TIME HEADLINE                           
ESCLCTQ  EQU   X'20'               DISP COST HEADLINE                           
ESFCFLG  DS    X                   COLUMN DISPLAY FLAG                          
ESFCTPQ  EQU   X'80'               TOP OF TEXT PREVIOUSLY REACHED               
ESFCBTQ  EQU   X'40'               BOTTOM OF TEXT PREVIOUSLY REACHED            
ESFCMTQ  EQU   X'20'               MATCH FOUND                                  
ESFCIGQ  EQU   X'10'               IGNORE FIRST MATCH TEST                      
ESFCFNQ  EQU   X'08'               FIND MODE                                    
ESFCCHQ  EQU   X'04'               CHANGE MODE                                  
ESFCAMQ  EQU   X'02'               LINE AMNEDED                                 
ESFCTLQ  EQU   X'01'               LINE TO LONG TO AMEND                        
ESFCCNT  DS    XL2                 NUMBER OF MATCHES FOUND                      
ESFCLST  DS    0XL2                LAST LINE/COL USED FOR PREV PARAM            
ESFCLTL  DS    X                   FIRST SEARCH PARAGRAPH OFFSET                
ESFCLTC  DS    X                   FIRST SEARCH PARAGRAPH OFFSET                
ESFCSCH  DS    0XL3                SEARCH START ATTRIBUTES                      
ESFCSCP  DS    X                   FIRST SEARCH PARAGRAPH OFFSET                
ESFCSCL  DS    X                   FIRST SEARCH LINE                            
ESFCSCC  DS    X                   FIRST SEARCH COLIMN                          
ESFCPOF  DS    XL(L'ESPOFST)       SAVE CURRENT SCREEN PARA OFFSET              
ESFCPRA  DS    XL(L'ESPARA)        SAVE CURRENT SCREEN PARA INDEX               
ESFCSTL  DS    XL(L'ESSTLIN)       SAVE CURRENT SCREEN START LINE               
ESFCSTC  DS    XL(L'ESCOL#1)       SAVE CURRENT SCREEN START COL                
ESFCENC  DS    XL(L'ESCOL#X)       SAVED CURRENT SCREEN END COL                 
ESFCCRS  DS    XL(L'TIOBCURS)      SAVE CURRENT SCREEN CURSOR ADDRESS           
ESJOB    DS    CL(L'TRNKACT)       JOB                                          
ESSEQ    DS    XL(L'PBRKSEQ)       SEQUENCE# (COMPLEMENT)                       
ESPARA   DS    XL(L'PBRKPARA)      PARAGRAPH#                                   
ESPOFST  DS    XL(L'PBRKPARA)      PARAGRAPH# OFFSET                            
*                                                                               
ESLVALS  DS    0X                  LINE VALUES                                  
ESLHIGH  DS    XL(L'NDXHIGH)       HIGHEST LINE INDEX RECORD                    
ESLACTV  DS    XL(L'NDXACTV)       HIGHEST ACTIVE LINE INDEX                    
ESLLST   DS    XL200               LINE INDEX LIST                              
ESLLNQ   EQU   *-ESLVALS           LENGTH OF LINE VALUES                        
*                                                                               
ESPVALS  DS    0X                  PARAGRAPH VALUES                             
ESPHIGH  DS    XL(L'NDXHIGH)       HIGHEST PARAGRAPH INDEX                      
ESPACTV  DS    XL(L'NDXACTV)       HIGHEST ACTIVE PARAGRAPH LINE INDEX          
ESPLST   DS    XL200               PARAGRAPH INDEX LIST                         
ESPLNQ   EQU   *-ESPVALS           LENGTH OF PARAGRAPH VALUES                   
*                                                                               
ESSECT   DS    XL1                 SECTION TYPE                                 
ESREPWD  DS    XL(L'BOFMAXWD)      MAXIMUM PAGE WIDTH OF BILL                   
ESPIND   DS    XL1                 BILL PASSIVE INDICATOR                       
ESPIDFT  EQU   X'80'               DRAFT BILL                                   
ESPILVE  EQU   X'40'               LIVE BILL                                    
ESPIAUR  EQU   X'20'               AUTOREV BILL (DRAFT/LIVE SET TOO)            
*                                                                               
ESTXTLAY DS    XL(LAYOUTL+1)       LAYOUT FOR FREE-FORM TEXT LINE               
ESNUMLAY DS    XL(LAYOUTS+1)       LAYOUT FOR NUMBER LINE                       
ESNUMTX  DS    XL1                 NUMTX MASK VALUE                             
ESTOTLAY DS    XL(LAYOUTS+1)       LAYOUT FOR TOTAL LINE                        
ESTOTTX  DS    XL1                 NUMTX MASK VALUE                             
*                                                                               
ESHEAD1  DS    CL132               CURRENT HEADLINE                             
ESHEAD2  DS    CL132               CURRENT HEADLINE                             
ESCOLLST DS    XL32                LIST OF COLUMN STARTS FOR BLFELDS            
*                                                                               
ESINDS   DS    XL1                 INDICATOR BYTE                               
ESIDEND  EQU   X'80'               END OF PARAGRAPH LINE ON SCREEN              
*                                                                               
         DS    (L'ESAVE-(*-ESAVE))X                                             
         DS    0H                                                               
         SPACE 1                                                                
*                                                                               
AREROUT  DS    0A                                                               
AFIRST   DS    A                   FIRST TIME IN                                
ADISP    DS    A                   DISPLAY SCREEN                               
AGBILL   DS    A                   READ/ADD BILL RECORD                         
AGPARA   DS    A                   READ AND DISPLAY PARAGRAGPH RECORD           
ASETINI  DS    A                   SET INITIAL VALUES                           
ATYPVAL  DS    A                   FORMAT TYPE VALIDATION                       
ATYPSET  DS    A                   SET FORMAT TYPE INFORMATION                  
ATYPDIS  DS    A                   DISPLAY FORMAT TYPE NAME                     
AADDPARA DS    A                   ADD NEW PARAGRAPH RECORD                     
ADELPARA DS    A                   DELETE PARAGRAPH RECORD OR ALL LINES         
AEDTSCRN DS    A                   EDIT SCREEN LINES                            
ADISPARA DS    A                   DISPLAY PARAGRAPH DATA                       
ADISHEAD DS    A                   DISPLAY HEADER LINES                         
ADISSCRN DS    A                   DISPLAY SCREEN LINES                         
ASETMSG  DS    A                   SET MESSAGE                                  
ACMDDIS  DS    A                   DISPLAY LINE COMMANDS                        
ACMDVAL  DS    A                   DEAL WITH LINE COMMANDS                      
ACHKCONF DS    A                   CHECK CONFLICTS / BUILD BLOCK LISTS          
ACHKPSIZ DS    A                   CHECK PARAGRAPH SIZE                         
ADEL     DS    A                   DELETE LINES FROM A PARAGRAPH                
ACMLN    DS    A                   COPY/MOVE LINES OF A PARAGRAPH               
AREPT    DS    A                   REPEAT LINES WITHIN A PARAGRAPH              
AADDS    DS    A                   ADD SUB-TOTAL LINE WITHIN A PARA             
AADDT    DS    A                   ADD TOTAL LINE WITHIN A PARAGRAPH            
AINBVAL  DS    A                   INITIALIZE BLOCK VALUES FOR SCREEN           
AVALOPTS DS    A                   DEAL WITH COMMAND LINE                       
ASCROLL  DS    A                   VERTICAL/HORIZONTAL SCROLLING                
ALINEREC DS    A                   CHANGE/ADD A LINE RECORD                     
APARAREC DS    A                   CHANGE/ADD A PARAGRAPH RECORD                
ABILLREC DS    A                   CHANGE A BILL RECORD                         
ADEFCURS DS    A                   SET DEFAULT CURSOR POSITION                  
AALTBLKV DS    A                   ALTER BLOCKS IF PREV LINE COMMAND            
AFCTXT   DS    A                   FIND/CHANGE TEXT STRING                      
ACMI     DS    A                   COPY/MOVE INDEXES                            
ASETLC   DS    A                   SET LINE/COLUMN AND CURSOR POSITION          
ACHGTXT  DS    A                   CHANGE MATCHED STRING                        
ACOMBLD  DS    A                   BUILD STANDARD COMMENT TABLE                 
ACOMPRC  DS    A                   PROCESS STANDARD COMMENT TABLE               
ACLRTWA  DS    A                   CLEAR TWA                                    
ABLDTOP  DS    A                   BUILD TOP LINE OF SCREEN                     
ABLDEND  DS    A                   BUILD END-OF-PARAGRAPH LINE                  
ABLDFOOT DS    A                   BUILD FOOTLINE                               
ASETLAD  DS    A                   SET DATA NEW LINE SCREEN ADDRESSES           
AINITX   DS    A                   INITIALIZE NEW TX TO TEXT DEFAULT            
AGETTX   DS    A                   GET TXD FROM LINE RECORD                     
APUTTX   DS    A                   PUT TXD INTO LINE RECORD                     
ABLDTX   DS    A                   BUILD TXD LINE ONTO SCREEN                   
ADISTX   DS    A                   DISPLAY TXD ONTO TEXT LINE                   
AVALTX   DS    A                   VALIDEATE TXD TEXT LINE                      
AINITSAR DS    A                   INITIALIZE FOR TSAR RECORDS                  
ASAVTX   DS    A                   SAVE TXD ON TSAR RECORD                      
ARESTX   DS    A                   GET TXD FROM TSAR RECORD                     
AFMTTX   DS    A                   FORMAT NUMBERS                               
AGETNUM  DS    A                   RETURN NUMTABD ENTRY                         
ADISNUM  DS    A                   DISPLAY NUMBER                               
AVALNUM  DS    A                   VALIDATE NUMBER                              
AUPDTOT  DS    A                   UPDATE PARAGRPAH TOTALS                      
AUPDSCR  DS    A                   UPDATE TOTAL LINES ON SCREEN                 
AADDTOT  DS    A                   ADD ONE TXD NUMBERS TO ANOTHER               
APUTTOT  DS    A                   PUT ONE TXD NUMBERS INTO ANOTHER             
ACLCTOT  DS    A                   COMPARE TXD TOTALS                           
ADISAMT  DS    A                   DISPLAY NET/COMM BILL AMOUNTS                
AINSLAY  DS    A                   INSERT ENTRY INTO LAYOUTD TABLE              
ASCRLAY  DS    A                   FORMAT LAYOUT TABLE FOR SCREEN               
AREROUTN EQU   (*-AREROUT)/L'AREROUT                                            
         DS    (AROUTN-AREROUTN)X ENSURE AROUTN=AREROUTN                        
         DS    (AREROUTN-AROUTN)X                                               
*                                                                               
         DS    0X                                                               
         DS    (OVERWRKL-(*-EWORKD))X                                           
         DS    0X                                                               
         EJECT                                                                  
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         EJECT                                                                  
         SPACE 1                                                                
*DDSCANBLKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017ACCLB0AB  12/22/99'                                      
         END                                                                    
