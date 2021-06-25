*          DATA SET ACREQ05    AT LEVEL 023 AS OF 01/22/14                      
*PHASE T60405C                                                                  
*INCLUDE ACWKSCAN                                                               
         TITLE 'ACREQ05 - HANDLE STACKED/55-SOON REQUESTS'                      
T60405   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWSX-LWS,T60405,R7,RR=R5                                         
         USING LWS,RC              RC=A(LOCAL WORKING STORAGE)                  
         ST    R5,RELO                                                          
         L     R9,0(R1)            R9=A(GLOBAL WORKING STORAGE)                 
         USING GWS,R9                                                           
         LA    R8,RCARDS                                                        
         USING ACQD,R8             R8=A(REQUEST CARDS)                          
         L     R3,ASAVE                                                         
         USING TWAD,R3             R3=A(TWA)                                    
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A66',0             GET A(SETLOCK)           
         L     RF,0(R1)                                                         
         ST    RF,ASETLOCK                                                      
*                                                                               
         L     RF,=V(ACWKSCAN)                                                  
         A     RF,RELO                                                          
         ST    RF,VCWKSCAN                                                      
*                                                                               
         MVI   WSPLDG,0                                                         
         NI    FLAG,X'FF'-(FLATFIL+WSP)                                         
*                                                                               
         MVC   FERN,=AL2(FF)                                                    
         LA    R6,BVRNAMEH                                                      
         ST    R6,FADR                                                          
         XC    BVRHDR,BVRHDR                                                    
         MVC   BVRRNAM,=CL22'STACKED CHECKS'                                    
         OI    BVRRNAMH+6,X'80'    TRANSMIT                                     
*                                                                               
         LA    R0,ACTTABN          NUMBER OF TABLE ENTRIES                      
         LA    R1,ACTTAB           BEGINNING OF ACTION TABLE                    
REQ10    CLC   OUTSTAT,0(R1)                                                    
         BE    REQ20                                                            
         LA    R1,L'ACTTAB(R1)     BUMP TO NEXT ENTRY                           
         BCT   R0,REQ10                                                         
         MVC   FERN,=AL2(INVENTRY) INVALID ENTRY                                
         B     EXIT                                                             
*                                                                               
REQ20    SR    RF,RF                                                            
         ICM   RF,15,1(R1)                                                      
         A     RF,RELO                                                          
         BASR  RE,RF               GOTO APPROPRIATE ROUTINE                     
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*              WRITE OUT A STACK REQUEST                              *         
***********************************************************************         
*                                                                               
STACK    NTR1                                                                   
         USING CHARECD,RF                                                       
         LA    RF,KEY                                                           
         XC    CHAKEY,CHAKEY                                                    
         MVI   CHAKTYP,CHAKTYPQ    X'10' - CHECK AUTHORIZATION RECORD           
         MVC   CHAKCULA,ACQCPY                                                  
         BAS   RE,GET54EL          GET SEQ#/LEDGER REC                          
         CLC   FERN,=AL2(FF)                                                    
         BNE   EXIT                                                             
         BAS   RE,CLEARIO          CLEAR IO BUFFER                              
         GOTO1 ASETREQ             SET REQUEST DETAILS                          
*                                                                               
         USING SRMRECD,R6                                                       
         L     R6,AIO1             BUILD NEW RECORD                             
         MVI   SRMKTYP,SRMKTYPQ    X'3F'                                        
         MVI   SRMKSUB,SRMKSCRQ    X'01'                                        
         MVC   SRMKCUL,ACQCPY      C/U/L                                        
         GOTO1 DATCON,DMCB,(5,SRMKDTE),(1,SRMKDTE)     TODAY'S DATE             
         MVC   SRMKUSR,TWAUSRID    USER CODE                                    
         MVC   SRMKSEQ,SEQNUM      SEQUENCE NUMBER                              
         MVC   SRMKACT,ACQACT      ACCOUNT CODE                                 
         AH    R6,DATADISP         BUMP TO BUILD FIRST ELEMENT                  
*                                                                               
         USING RQCELD,R6                                                        
         MVI   RQCEL,RQCELQ        X'CE' ELEMENT                                
         MVI   RQCLN,RQCLNQ        ELEMENT LENGTH                               
         MVI   RQCSEQ,X'01'        SEQUENCE NUMBER                              
         MVC   RQCCARD,ACQCARD1    SAVE OFF FIRST REQUEST CARD                  
         LA    R6,RQCLNQ(R6)                                                    
*                                                                               
         CLI   ACQCONT1,C'C'                                                    
         BNE   STACK200                                                         
         MVI   RQCEL,RQCELQ        X'CE' ELEMENT                                
         MVI   RQCLN,RQCLNQ        ELEMENT LENGTH                               
         MVI   RQCSEQ,X'02'        SEQUENCE NUMBER                              
         MVC   RQCCARD,ACQCARD2    SAVE OFF 2ND REQUEST CARD                    
         LA    R6,RQCLNQ(R6)                                                    
*                                                                               
         CLI   ACQCONT2,C'C'                                                    
         BNE   STACK200                                                         
         MVI   RQCEL,RQCELQ        X'CE' ELEMENT                                
         MVI   RQCLN,RQCLNQ        ELEMENT LENGTH                               
         MVI   RQCSEQ,X'03'        SEQUENCE NUMBER                              
         MVC   RQCCARD,ACQCARD3    SAVE OFF 3RD REQUEST CARD                    
         LA    R6,RQCLNQ(R6)                                                    
*                                                                               
         CLI   ACQCONT3,C'C'                                                    
         BNE   STACK200                                                         
         MVI   RQCEL,RQCELQ        X'CE' ELEMENT                                
         MVI   RQCLN,RQCLNQ        ELEMENT LENGTH                               
         MVI   RQCSEQ,X'04'        SEQUENCE NUMBER                              
         MVC   RQCCARD,ACQCARD4    SAVE OFF 4TH REQUEST CARD                    
         LA    R6,RQCLNQ(R6)                                                    
*                                                                               
STACK200 DS    0H                                                               
         MVI   0(R6),0             MARK END OF RECORD                           
         L     RF,AIO1             START OF RECORD                              
         LA    R6,1(R6)                                                         
         SR    R6,RF               GET LENGTH OF RECORD                         
         STCM  R6,3,ACCORLEN(RF)   PUT LENGTH INTO RECORD                       
*                                                                               
         USING SRMRECD,R6                                                       
         L     R6,AIO1                                                          
         MVC   KEY,SPACES          SEE IF SAME KEY ALREADY ON FILE              
         MVC   KEY(L'SRMKEY),SRMKEY                                             
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCFIL,KEY,AIO2                      
         TM    DMCB+8,X'10'        IF REC NOT FOUND, WRITE OUT NEW ONE          
         BO    STACK300                                                         
         TM    DMCB+8,X'02'        IF RECORD IS DELETED                         
         BO    STACK400            THEN REWRITE OLD RECORD W/NEW DATA           
         MVC   FERN,=AL2(SREQFIL)  ELSE CANT ADD DUPLICATE REQUESTS             
         B     EXIT                                                             
*                                                                               
STACK300 GOTO1 DATAMGR,DMCB,DMADD,ACCFIL,(R6),(R6)                              
         CLI   DMCB+8,0                                                         
         BE    STACKX                                                           
         MVC   FERN,=AL2(CANTADD)  UNSUCCESSFUL ADD                             
         B     EXIT                                                             
*                                                                               
STACK400 GOTO1 DATAMGR,DMCB,DMWRT,ACCFIL,(R6),(R6)                              
         CLI   DMCB+8,0                                                         
         BE    STACKX                                                           
         MVC   FERN,=AL2(CANTADD)  UNSUCCESSFUL ADD                             
         B     EXIT                                                             
*                                                                               
STACKX   DS    0H                                                               
         MVC   BVRHDR(L'MSG1),MSG1                                              
         MVC   FERN,=AL2(FE)       SET FOR MY MESSAGE                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              WRITE OUT A 55-RUN REQUEST                             *         
***********************************************************************         
*                                                                               
RUN      NTR1                                                                   
         USING CHARECD,RF                                                       
         LA    RF,KEY                                                           
         XC    CHAKEY,CHAKEY                                                    
         MVI   CHAKTYP,CHAKTYPQ    X'10' - CHECK AUTHORIZATION RECORD           
         MVC   CHAKCULA,ACQCPY                                                  
         BAS   RE,GET54EL                                                       
         CLC   FERN,=AL2(FF)                                                    
         BNE   EXIT                                                             
*                                                                               
         USING SRMRECD,R6                                                       
         LA    R6,KEY                                                           
         MVC   KEY,SPACES                                                       
         MVI   SRMKTYP,SRMKTYPQ    X'3F'                                        
         MVI   SRMKSUB,SRMKSCRQ    X'01'                                        
         MVC   SRMKCUL,ACQCPY      C/U/L                                        
         GOTO1 DATCON,DMCB,(5,SRMKDTE),(1,SRMKDTE)     TODAY'S DATE             
         MVC   SRMKUSR,TWAUSRID    USER CODE                                    
         MVC   SRMKSEQ,SEQNUM      SEQUENCE NUMBER                              
         SR    R1,R1                                                            
         ICM   R1,3,SEQNUM         STORE SEQUENCE NUMBER IN ACQAPPL(5)          
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ACQAPPL(5),DUB                                                   
         L     R6,AIO1                                                          
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCFIL,KEY,(R6)                              
         CLC   KEY(SRMKACT-SRMKEY),0(R6)            SAME SEQ NUMBER             
         BE    RUN300                                                           
         MVC   FERN,=AL2(NORQFND)  NO STACKED REQUESTS ON FILE                  
         B     EXIT                                                             
*                                                                               
RUN300   DS    0H                                                               
         MVI   BYTE,0                                                           
         GOTO1 GETFACT,DMCB,(X'80',BYTE),F#SEIND                                
         TM    BYTE,SEIRONLY       READ ONLY ADV FOR ACC FILE ?                 
         BNO   RUN350                                                           
         MVC   FERN,=AL2(WRNGADV)  WRONG ADV-CAN'T LOCK LEDGER                  
         B     EXIT                                                             
*                                                                               
RUN350   MVC   KEY,SPACES                                                       
         MVC   KEY(3),ACQCPY                                                    
         GOTO1 ASETLOCK,DMCB,(C'T',KEY),ACOMFACS           SETLOCK              
         CLI   DMCB+4,0                                                         
         BE    RUN400                                                           
         MVC   FERN,=AL2(LDGLOCK)                                               
         B     EXIT                                                             
*                                                                               
RUN400   DS    0H                                                               
         USING CHARECD,RF                                                       
         LA    RF,KEY                                                           
         XC    CHAKEY,CHAKEY                                                    
         MVI   CHAKTYP,CHAKTYPQ    X'10' - CHECK AUTHORIZATION RECORD           
         MVC   CHAKCULA,ACQCPY                                                  
         OI    OUTSTAT,BUMPQ                                                    
         BAS   RE,GET54EL                                                       
         NI    OUTSTAT,X'FF'-BUMPQ                                              
         CLC   FERN,=AL2(FF)                                                    
         BNE   EXIT                                                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(3),ACQCPY                                                    
         GOTO1 ASETLOCK,DMCB,(C'L',KEY),ACOMFACS           SETLOCK              
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ASETREQ                                                          
         GOTO1 =A(XSOON),DMCB,(RC),RR=RELO                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              WRITE OUT A 55-SOON REQUEST                            *         
***********************************************************************         
*                                                                               
SOON     NTR1                                                                   
         MVI   BYTE,0                                                           
         GOTO1 GETFACT,DMCB,(X'80',BYTE),F#SEIND                                
         TM    BYTE,SEIRONLY       READ ONLY ADV FOR ACC FILE ?                 
         BNO   SOON10                                                           
         MVC   FERN,=AL2(WRNGADV)  WRONG ADV-CAN'T LOCK LEDGER                  
         B     EXIT                                                             
*                                                                               
SOON10   MVC   KEY,SPACES                                                       
         MVC   KEY(3),ACQCPY                                                    
         GOTO1 ASETLOCK,DMCB,(C'T',KEY),ACOMFACS                                
         CLI   DMCB+4,0                                                         
         BE    *+14                                                             
         MVC   FERN,=AL2(LDGLOCK)                                               
         B     EXIT                                                             
*                                                                               
         USING CHARECD,RF                                                       
         LA    RF,KEY                                                           
         XC    CHAKEY,CHAKEY                                                    
         MVI   CHAKTYP,CHAKTYPQ    X'10' - CHECK AUTHORIZATION RECORD           
         MVC   CHAKCULA,ACQCPY                                                  
         OI    OUTSTAT,BUMPQ                                                    
         BAS   RE,GET54EL                                                       
         NI    OUTSTAT,X'FF'-BUMPQ                                              
         CLC   FERN,=AL2(FF)                                                    
         BNE   EXIT                                                             
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(3),ACQCPY                                                    
         GOTO1 ASETLOCK,DMCB,(C'L',KEY),ACOMFACS       SETLOCK                  
*                                                                               
         GOTO1 ASETREQ                                                          
         GOTO1 =A(XSOON),DMCB,(RC),RR=RELO                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              DISPLAY STACKED REQUESTS                               *         
***********************************************************************         
*                                                                               
DISPLAY  NTR1                      CAN ONLY DISPLAY STACKED REQUESTS            
         CLI   DISPMODE,0          MODE0=INIT FOR STACK DISPLAY SCREEN          
         BNE   DISP100                                                          
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D90604FB'    LOAD DISPLAY SCREEN                    
         GOTO1 CALLOV,DMCB,(0,BVRFRSTH)                                         
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                CANT LOAD OVERLAY                            
         LA    R4,NOWLDGH                                                       
         ST    R4,FADR             R4=A(NEXT TWA LINE NUM)                      
         TWAXC (R4),PROT=N         CLEAR SCREEN                                 
         MVI   DISPMODE,1                                                       
         MVC   BVRHDR(L'MSG3),MSG3                                              
         MVC   FERN,=AL2(FE)       SET FOR MY MESSAGE                           
         B     EXIT                                                             
*                                                                               
*              VALIDATE REQUIRED FIELDS FOR DISPLAY/CANCEL LOGIC                
*                                                                               
DISP100  DS    0H                                                               
         TM    NOWLDGH+4,X'80'     MAKE SURE FIELDS DIDNT CHANGE                
         BZ    *+8                                                              
         MVI   DISPMODE,1                                                       
         TM    NOWSTRTH+4,X'80'                                                 
         BZ    *+8                                                              
         MVI   DISPMODE,1                                                       
*                                                                               
         CLI   DISPMODE,1                                                       
         BNE   DIS600                                                           
         LA    R4,NOWFRSTH                                                      
         TWAXC (R4),PROT=Y         CLEAR SCREEN                                 
         XC    DISPSKIP,DISPSKIP   GET NUMBER OF REQUESTS TO SKIP               
         CLI   NOWSTRTH+5,0                                                     
         BE    DIS150                                                           
         TM    NOWSTRTH+4,X'08'    TEST IF FIELD IS NUMERICS OR NOT             
         BO    DIS110                                                           
         LA    R4,NOWSTRTH                                                      
         ST    R4,FADR                                                          
         MVC   FERN,=AL2(FLDNNUM)  IF NOT EXIT                                  
         B     EXIT                                                             
*                                                                               
DIS110   SR    R1,R1                                                            
         IC    R1,NOWSTRTH+5       L'INPUT                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,NOWSTRT(0)                                                   
         CVB   R1,DUB                                                           
         OR    R1,R1                                                            
         BZ    DIS150                                                           
         BCTR  R1,0                                                             
         STCM  R1,3,DISPSKIP       NUMBER OF REQUESTS TO SKIP                   
*                                                                               
DIS150   CLI   NOWLDG,C' '         LEDGER FIELD IS REQUIRED                     
         BH    DISP200                                                          
         LA    R4,NOWLDGH                                                       
         ST    R4,FADR                                                          
         MVC   FERN,=AL2(FLDMIS)                                                
         B     EXIT                                                             
*                                                                               
DISP200  DS    0H                                                               
         USING CHARECD,RF                                                       
         LA    RF,KEY                                                           
         XC    CHAKEY,CHAKEY                                                    
         MVI   CHAKTYP,CHAKTYPQ    X'10' - CHECK AUTHORIZATION RECORD           
         MVC   CHAKCPY,CMPY                                                     
         MVI   CHAKUNT,C'S'        UNIT S                                       
         MVC   CHAKLDG,NOWLDG      LEDGER                                       
         BAS   RE,GET54EL          GET CURRENT SEQUENCE NUMBER                  
         CLC   FERN,=AL2(FF)                                                    
         BNE   EXIT                                                             
*                                                                               
*              GET TOTAL NUMBER OF STACKED REQUESTS                             
*                                                                               
         XC    STACKTOT,STACKTOT   CLEAR TOTAL NUMBER OF STACKED REQS           
         BAS   RE,CLEARIO                                                       
         USING SRMRECD,R6                                                       
         L     R6,AIO1                                                          
         MVI   SRMKTYP,SRMKTYPQ    X'3F'                                        
         MVI   SRMKSUB,SRMKSCRQ    X'01'                                        
         MVC   SRMKCUL(1),CMPY                                                  
         MVI   SRMKCUL+1,C'S'      UNIT S                                       
         MVC   SRMKCUL+2(1),NOWLDG                                              
         GOTO1 DATCON,DMCB,(5,SRMKDTE),(1,SRMKDTE)     TODAY'S DATE             
         MVC   SRMKUSR,TWAUSRID    USER CODE                                    
         MVC   SRMKSEQ,SEQNUM      SEQUENCE NUMBER                              
         MVC   SAVSRM,SRMKEY                                                    
         MVC   COMMAND,DMRDHI      FIRST READ IS A RDHI                         
*                                                                               
DIS225   GOTO1 DATAMGR,DMCB,COMMAND,ACCFIL,AIO1,AIO1                            
         L     R6,AIO1                                                          
         CLC   SAVSRM(SRMKSEQ-SRMKEY+L'SRMKSEQ),0(R6)                           
         BNE   DIS250                                                           
         MVC   COMMAND,DMRSEQ      ALL READS AFTER ARE SEQUENTIAL               
         SR    R1,R1               BUMP TOTAL STACK COUNT                       
         ICM   R1,3,STACKTOT                                                    
         LA    R1,1(R1)                                                         
         STCM  R1,3,STACKTOT                                                    
         B     DIS225                                                           
*                                                                               
*              BUILD KEY FOR STACKED REQUESTS                                   
*                                                                               
DIS250   BAS   RE,CLEARIO                                                       
         USING SRMRECD,R6                                                       
         L     R6,AIO1                                                          
         MVI   SRMKTYP,SRMKTYPQ    X'3F'                                        
         MVI   SRMKSUB,SRMKSCRQ    X'01'                                        
         MVC   SRMKCUL(1),CMPY                                                  
         MVI   SRMKCUL+1,C'S'      UNIT S                                       
         MVC   SRMKCUL+2(1),NOWLDG                                              
         GOTO1 DATCON,DMCB,(5,SRMKDTE),(1,SRMKDTE)     TODAY'S DATE             
         MVC   SRMKUSR,TWAUSRID    USER CODE                                    
         MVC   SRMKSEQ,SEQNUM      SEQUENCE NUMBER                              
         MVC   SAVSRM,SRMKEY                                                    
*                                                                               
*              SKIP FIRST N STACKED REQUESTS WHERE N=DISPSKIP                   
*                                                                               
         SR    R5,R5               R5=#STACKED REQUESTS TO SKIP                 
         ICM   R5,3,DISPSKIP                                                    
         BZ    DIS400                                                           
         MVC   COMMAND,DMRDHI      FIRST READ IS A RDHI                         
*                                                                               
DIS300   L     R6,AIO1                                                          
         GOTO1 DATAMGR,DMCB,COMMAND,ACCFIL,(R6),(R6)                            
         MVC   COMMAND,DMRSEQ      ALL READS AFTER ARE SEQUENTIAL               
         CLC   SAVSRM(SRMKSEQ-SRMKEY+L'SRMKSEQ),0(R6)                           
         BE    *+14                                                             
         MVC   FERN,=AL2(NORQFND)  NO STACKED REQUESTS FOUND                    
         B     EXIT                                                             
         OR    R5,R5                                                            
         BZ    DIS400                                                           
         SH    R5,=H'1'                                                         
         B     DIS300                                                           
*                                                                               
*              1. READ STACK REQUESTS                                           
*              2. BUILD 'STACK' KEY TABLE                                       
*              3. MOVE REQ CARDS TO TWA                                         
*                                                                               
         USING SAVD,R2                                                          
DIS400   MVI   SAVCNT,0                                                         
         LA    R2,SAVTBL           R2 --->  REQ KEY TABLE                       
         LA    R4,NOWFRSTH         R4 --->  CURRENT FIELD                       
         LA    R5,DISPMAX          R5 --->  MAX NUMBER OF CARDS/SCRN            
         MVC   COMMAND,DMRDHI                                                   
*                                                                               
DIS410   L     R6,AIO1                                                          
         GOTO1 DATAMGR,DMCB,COMMAND,ACCFIL,(R6),(R6)                            
         MVC   COMMAND,DMRSEQ      NEXT READ WILL BE SEQ                        
         CLC   SAVSRM(SRMKSEQ-SRMKEY+L'SRMKSEQ),0(R6)                           
         BNE   DIS500              NO MORE REQUESTS                             
*                                                                               
         BAS   RE,LINE             MOVE REQ CARDS TO TWA - USES R4              
         USING DISPLD,R4                                                        
         LA    R4,DLNEXT           POINT TO NEXT HEADER                         
*                                                                               
         MVC   SAVKEY,0(R6)        SAVE KEY                                     
         ZIC   R1,SAVCNT           SAVE COUNT                                   
         LA    R1,1(R1)                                                         
         STC   R1,SAVCNT                                                        
         LA    R2,SAVLNQ(R2)       NEXT TABLE ENTRY                             
         BCT   R5,DIS410                                                        
*                                                                               
*              DISPLAY SCREEN                                                   
*                                                                               
DIS500   MVI   DISPMODE,2                                                       
         LA    R1,NOWFRSTH         START                                        
         LA    R0,NOWENDH          END FIELD                                    
DIS550   OI    6(R1),X'80'         TRANSMIT                                     
         CLI   0(R1),X'51'         L'HEADER(8)+L'REQ CARD(73)                   
         BNE   *+8                                                              
         OI    1(R1),X'20'         PROTECT FIELDS NOT VALID FOR INPUT           
         ZIC   R2,0(R1)            L'FIELD                                      
         AR    R1,R2                                                            
         CR    R1,R0                                                            
         BL    DIS550                                                           
         CLI   SAVCNT,0                                                         
         BNE   *+14                                                             
         MVC   FERN,=AL2(NORQFND)  NO REQUESTS FOUND                            
         B     EXIT                                                             
*                                                                               
         MVC   BVRHDR(L'MSG4),MSG4                                              
         SR    R5,R5               DISPLAY TOTAL NUMBER STACKED REQS            
         ICM   R5,3,STACKTOT                                                    
         CVD   R5,DUB                                                           
         UNPK  DUB(3),DUB+6(2)                                                  
         OI    DUB+2,X'F0'                                                      
         MVC   BVRHDR(3),DUB                                                    
*                                                                               
         SR    R5,R5               DISPLAY REQUEST NUMBERS                      
         ICM   R5,3,DISPSKIP                                                    
         AH    R5,=H'1'            GET FIRST NUMBER                             
         CVD   R5,DUB                                                           
         UNPK  DUB(3),DUB+6(2)                                                  
         OI    DUB+2,X'F0'                                                      
         MVC   BVRHDR+24(3),DUB                                                 
*                                                                               
         ZIC   R1,SAVCNT                                                        
         AR    R5,R1               GET 2ND NUMBER                               
         BCTR  R5,0                                                             
         CVD   R5,DUB                                                           
         UNPK  DUB(3),DUB+6(2)                                                  
         OI    DUB+2,X'F0'                                                      
         MVC   BVRHDR+33(3),DUB                                                 
         MVC   FERN,=AL2(FE)       SET FOR MY MESSAGE                           
         B     EXIT                                                             
*                                                                               
DIS600   DS    0H                                                               
         CLI   SAVCNT,0                                                         
         BNE   *+14                                                             
         MVC   FERN,=AL2(NORQFND)  NO REQUESTS FOUND                            
         B     EXIT                                                             
         BAS   RE,CANCEL                                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              DISPLAY A REQUEST                                      *         
***********************************************************************         
*                                                                               
*              R4 --> POINTING TO CURRENT FIELD HEADER                          
*                                                                               
LINE     NTR1                                                                   
         MVC   SAVCARD1,SPACES                                                  
         MVC   SAVCARD2,SPACES                                                  
         MVC   SAVCARD3,SPACES                                                  
         MVC   SAVCARD4,SPACES                                                  
*                                                                               
         L     R6,AIO1             BUILD NEW RECORD                             
         AH    R6,DATADISP         BUMP TO BUILD FIRST ELEMENT                  
*                                                                               
         USING RQCELD,R6                                                        
LINE100  CLI   0(R6),0             END OF RECORD                                
         BE    LINE200                                                          
         CLI   0(R6),RQCELQ        X'CD' ELEMENT                                
         BE    LINE150                                                          
LINE125  ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     LINE100                                                          
*                                                                               
LINE150  LA    R1,SAVCARD1                                                      
         SR    RF,RF                                                            
         IC    RF,RQCSEQ                                                        
         SH    RF,=H'1'                                                         
         BM    LINE125                                                          
         MH    RF,=Y(L'RQCCARD)                                                 
         AR    R1,RF                                                            
         MVC   0(L'RQCCARD,R1),RQCCARD                                          
         B     LINE125                                                          
*                                                                               
LINE200  DS    0H                                                               
         OC    SAVCARD1,SPACES                                                  
         OC    SAVCARD2,SPACES                                                  
         OC    SAVCARD3,SPACES                                                  
         OC    SAVCARD4,SPACES                                                  
         MVC   ACQCARD1,SAVCARD1   SAVE OFF REQUEST CARDS                       
         MVC   ACQCARD2,SAVCARD2                                                
         MVC   ACQCARD3,SAVCARD3                                                
         MVC   ACQCARD4,SAVCARD4                                                
*                                                                               
         USING DISPLD,R4                                                        
         MVC   DLINE,SPACES        INITIALIZE DISPLAY LINE(S)                   
         MVC   DLCOMMNT(6),ACQCOMNT                                             
         MVC   DLACTSTR(6),ACQACTST                                             
         MVC   DLACTEND(6),ACQACTND                                             
         MVC   DLMOSTRT(8),ACQMOSST                                             
         MVC   DLCONTRA+14(3),ACQCNTRA+14                                       
         MVC   DLOFFICE(2),ACQOFFFL                                             
         MVC   DLUNIT,ACQUNT                                                    
         MVC   DLLDGR,ACQLDG                                                    
         MVC   DLACNO,ACQACT                                                    
         MVC   DLLGRP,ACQTRNF                                                   
         MVC   DLBGRP,ACQBILGP                                                  
         MVC   DLFTR1,ACQACTF1                                                  
         MVC   DLFTR2,ACQACTF2                                                  
         MVC   DLFTR3,ACQACTF3                                                  
         MVC   DLFTR4,ACQACTF4                                                  
         MVC   DLFTR5,ACQACTF5                                                  
         MVC   DLMEDIA,ACQMEDFL                                                 
         MVC   DLBILTYP,ACQBILTY                                                
         MVC   DLSTRD,ACQSTART                                                  
         MVC   DLENDD,ACQEND                                                    
         MVC   DLSORT,ACQSORT                                                   
         MVC   DLSELECT,ACQSEL                                                  
         MVC   DLOPTNS,ACQOPTS                                                  
         MVC   DLNAME,ACQESTOR                                                  
         MVC   DLREV,ACQREVOP                                                   
         MVC   DLSTAT,ACQTRNST                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              CANCEL A REQUEST                                       *         
***********************************************************************         
*                                                                               
CANCEL   NTR1                                                                   
         LA    R2,SAVTBL           R2 --->  REQ KEY TABLE                       
         USING SAVD,R2                                                          
         LA    R4,NOWFRSTH         R4 --->  CURRENT FIELD                       
         USING DISPLD,R4                                                        
         ZIC   R5,SAVCNT           R5 --->  MAX NUMBER OF REQUESTS              
*                                                                               
CAN100   DS    0H                                                               
         CLI   DLCANC,C'C'         CANCEL THIS REQUEST?                         
         BNE   CAN150                                                           
         L     R6,AIO1                                                          
         MVC   0(L'SAVKEY,R6),SAVKEY                                            
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,(R6),(R6)                             
         OI    ACCOSTAT(R6),X'80'      DELETE RECORD                            
         GOTO1 DATAMGR,DMCB,DMWRT,ACCFIL,(R6),(R6)                              
         CLI   DMCB+8,0                                                         
         BE    CAN200                                                           
         MVC   FERN,=AL2(CANTCAN)  ERROR - UNABLE TO CANCEL REQUEST             
         B     EXIT                                                             
*                                                                               
CAN150   MVI   DLCANC,C' '                                                      
         OI    DLCANCH+6,X'80'                                                  
*                                                                               
CAN200   LA    R2,SAVLNQ(R2)                                                    
         LA    R4,DLNEXT           POINT TO NEXT HEADER                         
         BCT   R5,CAN100                                                        
*                                                                               
CAN300   MVI   DISPMODE,1                                                       
         MVI   SAVCNT,0                                                         
         MVC   BVRHDR(L'MSG5),MSG5                                              
         MVC   FERN,=AL2(FE)                                                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              CLEAR THE IO AREA                                      *         
***********************************************************************         
*                                                                               
CLEARIO  NTR1                                                                   
         LA    R0,SPACES                                                        
         SR    R1,R1                                                            
         L     RE,AIO1                                                          
         L     RF,=F'2000'                                                      
         MVCL  RE,R0                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              GET CURRENT SEQUENCE NUMBER & VALIDATE ID              *         
***********************************************************************         
*                                                                               
GET54EL  NTR1                                                                   
*                                                                               
*              READ FOR X'10' CHECK AUTHORIZATION RECORD                        
*                                                                               
         MVC   LKEY,KEY                                                         
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCFIL,KEY,AIO2                              
         L     R6,AIO2                                                          
         CLC   KEY(4),0(R6)                                                     
         BE    GET25                                                            
*                                                                               
         MVC   FERN,=AL2(NTONFILE) ERROR READING LEDGER RECORD                  
         B     EXIT                                                             
*                                                                               
GET25    L     R6,AIO2                                                          
         AH    R6,DATADISP         BUMP TO FIRST ELEMENT                        
*                                                                               
GET50    CLI   0(R6),X'54'         FIND X'54' ELEMENT                           
         BE    GET100                                                           
         CLI   0(R6),0                                                          
         BNE   GET75                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCFIL,AIO2,AIO2                             
         L     R6,AIO2                                                          
         CLC   LKEY(4),0(R6)                                                    
         BE    GET25                                                            
*                                                                               
         MVC   FERN,=AL2(NAUTHCKS) NOT AUTHORIZED FOR ONLINE CHECKS             
         B     EXIT                                                             
GET75    SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1               BUMP TO NEXT ELEMENT                         
         B     GET50                                                            
*                                                                               
         USING OCNELD,R6                                                        
GET100   CLC   OCNOFFID,TWAUSRID   FIND MATCHING ID                             
         BNE   GET75                                                            
         TM    OCNSTAT,OCNSSOON    TEST IF AUTHORIZED FOR SOON CHECKS           
         BO    GET125                                                           
         MVC   FERN,=AL2(NAUTHCKS) NOT AUTHORIZED FOR ONLINE CHECKS             
         B     EXIT                                                             
*                                                                               
GET125   MVC   SEQNUM,OCNSEQN      SAVE SEQUENCE NUMBER                         
         NI    FLAG,X'FF'-(FLATFIL+WSP)                                         
         TM    OCNSTAT2,OCNSDFIL   FLATFILE?                                    
         BZ    *+8                                                              
         OI    FLAG,FLATFIL                                                     
         CLI   OCNLN,OCNLN3Q                                                    
         BL    GET130                                                           
         CLI   OCNLASR,OCNWSP                                                   
         BNE   GET130                                                           
         OI    FLAG,WSP                                                         
GET130   TM    FLAG,FLATFIL+WSP                                                 
         BZ    GET140                                                           
GET135   MVC   WSPLDG,ACQCPY+2                                                  
GET140   EQU   *                                                                
         TM    OUTSTAT,RUNQ+SOONQ                                               
         BZ    EXIT                                                             
         OC    OCNDPSR,OCNDPSR     IS A SOON REGISTER PENDING?                  
         BZ    *+14                                                             
         MVC   FERN,=AL2(SREGPEND)                                              
         B     EXIT                                                             
         OC    OCNDPLR,OCNDPLR     CHECK IF A LOCAL REGISTER PENDING            
         BZ    GET150                                                           
         CLC   OCNDPLR,TODAY2      ERROR IF PENDING FROM YESTERDAY              
         BNL   *+14                                                             
         MVC   FERN,=AL2(LREGPEND)                                              
         B     EXIT                                                             
*                                                                               
GET150   TM    OUTSTAT,BUMPQ       ONLY BUMP SEQ NUM FOR STACKED REQS           
         BZ    EXIT                                                             
*                                                                               
         TM    OUTSTAT,RUNQ                                                     
         BZ    GET175                                                           
         SR    R1,R1                                                            
         ICM   R1,3,OCNSEQN                                                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,OCNSEQN                                                     
*                                                                               
GET175   DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,OCNDPSR),(2,OCNDPSR)  SET REGSTR PENDING          
         GOTO1 DATAMGR,DMCB,DMWRT,ACCFIL,AIO2,AIO2                              
         CLI   8(R1),0                                                          
         BE    EXIT                                                             
         DC    H'0'                ERROR IN WRITING BACK LEDGER RECORD          
         EJECT                                                                  
***********************************************************************         
*              CONSTANTS                                              *         
***********************************************************************         
*                                                                               
DISPMAX  EQU   5           MAX #LINES AVAILABLE FOR DISPLAY                     
DATADISP DC    H'49'                                                            
ACCFIL   DC    CL8'ACCOUNT'                                                     
DMREAD   DC    CL8'DMREAD'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMRDHI   DC    CL8'DMRDHI'                                                      
DMADD    DC    CL8'DMADD'                                                       
DMWRT    DC    CL8'DMWRT'                                                       
MSG1     DC    CL46'Stacked Request Added                         '             
MSG2     DC    CL46'Stacked Requests Submitted                    '             
MSG3     DC    CL46'Input Ledger For List Display                 '             
MSG4     DC    CL46'xxx Requests - Requests xxx Thru xxx Displayed'             
MSG5     DC    CL46'Request Cancel Status Amended                 '             
*                                                                               
ACTTAB   DS    0XL5                                                             
         DC    AL1(STCKQ),AL4(STACK)                                            
         DC    AL1(RUNQ),AL4(RUN)                                               
         DC    AL1(SOONQ),AL4(SOON)                                             
         DC    AL1(DISPQ),AL4(DISPLAY)                                          
ACTTABN  EQU   (*-ACTTAB)/L'ACTTAB                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              GENERATE A SOON REQUEST                                *         
***********************************************************************         
*                                                                               
         USING SPOOK,R6                                                         
XSOON    DS    0D                                                               
         NMOD1 0,*XSOON*,RR=R5                                                  
         L     RC,0(R1)                                                         
*                                                                               
         L     R4,DATAMGR          CHECK TO SEE IF FACWK HAS AN                 
         GOTO1 VCWKSCAN,DMCB,(R4)    AVAILABLE INDEX                            
         CLI   0(R1),0             IF ZERO, FILE IS NOT FULL                    
         BE    XSOON2                                                           
         MVC   BVRHDR,SPACES                                                    
         MVC   BVRHDR(31),=CL31'** File Full - Stop call DDS **'                
         MVC   FERN,=AL2(FE)                                                    
         B     XSOONX                                                           
*                                                                               
XSOON2   CLC   CMPY,ACQCPY         TRAP BAD REQUESTS ON THE WAY OUT             
         BE    *+6                                                              
         DC    H'0'                HEXCOMP IS CREAMED - CHECK FOR MORE          
         XC    TEMP(SPOOKXL),TEMP   BUILD SPOOK BLOCK                           
         LA    R6,TEMP                                                          
         MVC   SPOOKXT,=C'XT='                                                  
         MVC   SPOOKUID,USRID      CONNECT ID                                   
         MVC   SPOOKAGY,TWAAGY     TWO CHARACTER ID CODE                        
         MVC   SPOOKAGX,CMPY       HEXCOMP                                      
         TM    OUTSTAT,RUNQ                                                     
         BZ    *+10                                                             
         MVC   SPOOKDID,BVROUT+4   USER INITIALS (ID)                           
         TM    OUTSTAT,SOONQ                                                    
         BZ    *+10                                                             
         MVC   SPOOKDID,BVROUT+5   USER INITIALS (ID)                           
         CLI   SPOOKDID+2,C' '                                                  
         BH    *+8                                                              
         MVI   SPOOKDID+2,C'*'                                                  
         CLI   SPOOKDID+1,C' '                                                  
         BH    *+8                                                              
         MVI   SPOOKDID+1,C'*'                                                  
         MVC   SPOOKSYS,=C'AC'     ACCOUNT SYSTEM                               
         MVC   SPOOKEOD,=C'55'                                                  
         MVC   SPOOKJCL,=C'55'                                                  
         MVI   SPOOKPR2,7          SET HIGH PRIORITY FOR CHECKS                 
         MVI   SPOOKWEN,5          SET UPDATIVE SOON STATUS                     
         OI    SPOOKSTA,X'02'      SET REPORT STATUS(INVISIBLE)                 
*        CLI   WSPLDG,0                                                         
*        BE    XSOON7                                                           
         TM    FLAG,WSP                                                         
         BZ    XSOON7                                                           
         TM    OUTSTAT,RUNQ+SOONQ                                               
         BZ    XSOON7                                                           
         MVC   SPOOKDID(2),TWAAGY                                               
         MVC   SPOOKDID+2(1),WSPLDG                                             
         MVC   SPOOKFNO,=CL4'WSP ' ONLY LASERWSP SETTING                        
         TM    FLAG,FLATFIL                                                     
         BZ    *+10                                                             
         MVC   SPOOKFNO,=CL4'WFF ' BOTH LASERWSP AND FLATFILE SETTING           
                                                                                
         L     R2,AIO1                                                          
         USING CTIREC,R2           R2=A(USER-ID RECORD)                         
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,TWAUSRID                                                 
         GOTO1 DATAMGR,DMCB,=C'DMREAD  ',=C'CTFILE  ',CTIREC,CTIREC             
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO1                                                          
         USING CTDSTD,R1                                                        
         LA    R1,CTIDATA          LOCATE ORIGIN DETAILS ELEMENT                
XSOON5   CLI   CTDSTEL,0           TEST E-O-R                                   
         BE    XSOON6                                                           
         CLI   CTDSTEL,CTDSTELQ    TEST DESTINATION DETAILS ELEMENT             
         BE    XSOON5A                                                          
         SR    R0,R0                                                            
         IC    R0,CTDSTLEN                                                      
         AR    R1,R0                                                            
         B     XSOON5                                                           
                                                                                
XSOON5A  MVC   SPOOKDID(2),CTDSTLFC                                             
         OC    CTDSTLFC,CTDSTLFC                                                
         BNZ   *+10                                                             
         MVC   SPOOKDID(2),CTDSTPOW                                             
         DROP  R1,R2                                                            
                                                                                
XSOON6   EQU   *                                                                
         L     R2,AIO1                                                          
         USING CTIREC,R2           R2=A(USER-ID RECORD)                         
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKID,SPACES                                                    
         MVC   CTIKID(5),=C'LASER'                                              
         GOTO1 DATAMGR,DMCB,=C'DMREAD  ',=C'CTFILE  ',CTIREC,CTIREC             
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO1                                                          
         USING CTDSCD,R1                                                        
         LA    R1,CTIDATA                                                       
XSOON6A  CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),CTDSCELQ      ID ELEMENT                                   
         BE    XSOON6B                                                          
         SR    R0,R0                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     XSOON6A                                                          
*                                                                               
XSOON6B  MVC   REQDEST,CTDSC       FORCE DESTINATION TO LASER ID                
         DROP  R1,R2                                                            
                                                                                
XSOON7   EQU   *                                                                
         L     R4,APARM                                                         
         L     R4,16(R4)           A(COMFACS)                                   
         GOTO1 REQTWA,DMCB,(5,(R3)),REQREC+54,DATAMGR,(R4),(R6)                 
         XC    BVRHDR,BVRHDR                                                    
         MVC   BVRHDR(L'SOONMSG1),SOONMSG1                                      
         L     RE,8(R1)            GET A(PRTQUE) KEY                            
         MVC   BVRHDR+7(3),2(RE)                                                
         XR    RF,RF                                                            
         ICM   RF,3,6(RE)                                                       
         LA    R4,BVRHDR+L'SOONMSG1                                             
         EDIT  (RF),(5,(R4)),ALIGN=LEFT,WRK=TEMP+60                             
         AR    R4,R0                                                            
         MVC   0(L'SOONMSG2,R4),SOONMSG2                                        
         MVC   FERN,=AL2(FE)       SET FOR MY MESSAGE                           
*                                                                               
XSOONX   XIT1                                                                   
*                                                                               
SOONMSG1 DC    C'Checks XXX,'                                                   
SOONMSG2 DC    C' Will Be Processed SOON'                                       
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              LOCAL WORKING STORAGE                                  *         
***********************************************************************         
*                                                                               
LWS      DSECT                                                                  
RELO     DS    F                                                                
ASETLOCK DS    A                   A(SETLOCK)                                   
VCWKSCAN DS    V                   V(ACWKSCAN)                                  
DISPADR  DS    F                   A(NEXT DISP LINE ON SCR)                     
DISPSKIP DS    H                   #STACK REQUESTS TO SKIP                      
STACKTOT DS    H                   TOTAL NUMBER OF STACKED REQUESTS             
SEQNUM   DS    XL2                 SEQUENCE NUMBER                              
COMMAND  DS    CL8                                                              
SAVSRM   DS    CL42                SAVED SRM KEY                                
WSPLDG   DS    CL1                                                              
FLAG     DS    CL1                                                              
FLATFIL  EQU   X'80'                                                            
WSP      EQU   X'40'                                                            
*                                                                               
SAVCARD1 DS    CL80                SAVED REQUEST CARDS                          
SAVCARD2 DS    CL80                                                             
SAVCARD3 DS    CL80                                                             
SAVCARD4 DS    CL80                                                             
*                                                                               
LWSX     EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
*              REQUEST SAVE TABLE                                     *         
***********************************************************************         
*                                                                               
SAVD     DSECT                                                                  
SAVKEY   DS    CL42                REQUEST KEY                                  
SAVLNQ   EQU   *-SAVD              LNGTH OF EACH TABLE ENTRY                    
         EJECT                                                                  
***********************************************************************         
*              DISPLAY LINE                                           *         
***********************************************************************         
*                                                                               
DISPLD   DSECT                                                                  
DLCANCH  DS    CL8                                                              
DLCANC   DS    CL1                                                              
DLINEH   DS    CL8                                                              
DLINE    DS    0CL73                                                            
DLNUM    DS    CL2                                                              
         DS    CL1                                                              
DLCMPY   DS    CL2                                                              
DLUNIT   DS    CL1                                                              
DLLDGR   DS    CL1                                                              
DLACNO   DS    CL12                                                             
DLLGRP   DS    CL3                                                              
DLBGRP   DS    CL3                                                              
DLFTR1   DS    CL1                                                              
DLFTR2   DS    CL1                                                              
DLFTR3   DS    CL1                                                              
DLFTR4   DS    CL1                                                              
DLFTR5   DS    CL1                                                              
DLMEDIA  DS    CL1                                                              
DLBILTYP DS    CL1                                                              
DLSTRD   DS    CL6                                                              
DLENDD   DS    CL6                                                              
DLSORT   DS    CL1                                                              
         DS    CL1                                                              
DLSELECT DS    CL6                                                              
         DS    CL3                                                              
DLOPTNS  DS    CL7                                                              
DLNAME   DS    CL9                                                              
DLREV    DS    CL1                                                              
DLSTAT   DS    CL1                                                              
         ORG   DLINE+L'DLINE                                                    
DLINE2H  DS    CL8                                                              
DLINE2   DS    0CL73                                                            
         DS    CL3                                                              
DLSRTA   DS    CL7                                                              
DLMOSTRT DS    CL4                                                              
DLMOSEND DS    CL4                                                              
DLCONTRA DS    CL14                                                             
         DS    CL3                                                              
DLACTSTR DS    CL6                                                              
DLACTEND DS    CL6                                                              
         DS    CL4                                                              
DLOFFICE DS    CL2                                                              
DLCOMMNT DS    CL6                                                              
         ORG   DLINE2+L'DLINE2                                                  
DLNEXT   EQU   *                                                                
                                                                                
       ++INCLUDE ACREQWORK                                                      
* FASELIST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023ACREQ05   01/22/14'                                      
         END                                                                    
