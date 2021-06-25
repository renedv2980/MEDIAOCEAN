*          DATA SET ACLFM17S   AT LEVEL 014 AS OF 05/01/02                      
*PHASE T60317A,*                                                                
*INCLUDE SCANNER                                                                
*INCLUDE UNSCAN                                                                 
*INCLUDE ACPUTEL                                                                
*INCLUDE ACDELEL                                                                
*INCLUDE HEXIN                                                                  
         TITLE 'OFFICE CHECK NUMBER ELEMENTS'                                   
T60317   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWDX-LWD,**LFM17*,R7,RR=R5                                       
         LR    R9,RC                                                            
         USING LWD,R9                                                           
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         ST    R5,RELO                                                          
         EJECT                                                                  
* ******************************************************************            
*              BUILD KEY                                                        
* ******************************************************************            
         MVI   ERROR,X'FF'                                                      
         LR    RF,RA                                                            
         USING TWAD,RF                                                          
         MVC   ALPHAID,TWAAGY                                                   
         CLI   MODE,BUILDKEY                                                    
         BNE   DSP00                                                            
         MVC   KEY,SPACES                                                       
         CLI   TWAOFFC,C'*'        DDS TERMINAL                                 
         BE    BLD03                                                            
         OI    LOGCOMPH+1,X'20'             COMPANY                             
         OI    LOGCOMPH+6,X'80'                                                 
         OI    LOGCOMH+1,X'0C'     COMPANY CONSTANT - ZERO INTENSITY            
         OI    LOGCOMH+6,X'80'                                                  
         OI    LOGCNAMH+1,X'0C'    COMPANY NAME     - ZERO INTENSITY            
         OI    LOGCNAMH+6,X'80'                                                 
         MVC   KEY(1),COMPANY                                                   
         LA    R2,LOGCOMPH         VALIDATE COMPANY                             
         B     BLD04                                                            
*                                                                               
BLD03    DS    0H                                                               
         LA    R2,LOGCOMPH         VALIDATE COMPANY                             
         GOTO1 ANY                                                              
         MVC   KEY(1),LOGCOMP                                                   
         CLI   5(R2),1                                                          
         BE    BLD04                                                            
         GOTO1 =V(HEXIN),DMCB,LOGCOMP,KEY,2,RR=RB                               
         OC    DMCB+12(4),DMCB+12                                               
         BNZ   BLD04                                                            
         MVI   ERROR,2                                                          
         B     EXIT                                                             
*                                                                               
BLD04    TM    4(R2),X'20'         IS COMPANY STILL VALID                       
         BO    BLD05                                                            
         MVC   LOGCNAM,SPACES                                                   
         OI    LOGCNAMH+6,X'80'                                                 
         BAS   RE,CTLREAD          GET COMPANY RECORD                           
         GOTO1 NAMOUT              DISPLAY NAME                                 
         OI    4(R2),X'20'         SET COMPANY VALID                            
         MVI   ANYKEY,C'Y'                                                      
         NI    LOGLDGH+4,X'FF'-X'20'  TURNOFF LEDGER VALID                      
*                                                                               
BLD05    MVI   KEY+1,C'S'          UNIT 'S' IS ASSUMED                          
         LA    R2,LOGLDGH          VALIDATE LEDGER                              
         GOTO1 ANY                                                              
         MVC   KEY+2(1),LOGLDG                                                  
         BAS   RE,CTLREAD          GET LEDGER RECORD                            
         TM    4(R2),X'20'         IS LEDGER STILL VALID                        
         BO    BLD07                                                            
         GOTO1 NAMOUT              DISPLAY NAME                                 
         OI    4(R2),X'20'         SET LEDGER VALID                             
         MVI   ANYKEY,C'Y'                                                      
         LR    RF,RA                                                            
         USING TWAD,RF                                                          
         MVI   SCRN,0              SET FOR FIRST TIME                           
*                                                                               
BLD07    CLI   PFKEY,0             ANY PFKEY                                    
         BE    EXIT                                                             
         LR    RF,RA                                                            
         SR    R1,R1                                                            
         IC    R1,SCRN             CURRENT SCREEN NUMBER                        
         MVI   ERROR,251           WRONG PFKEY                                  
         CLI   PFKEY,5             PF5=PREVIOUS                                 
         BNE   BLD09                                                            
         LTR   R1,R1               TEST ALREADY AT FIRST                        
         BZ    *+8                                                              
         SH    R1,=H'1'            BACKUP 1                                     
         B     BLD11                                                            
*                                                                               
BLD09    CLI   PFKEY,6             PF6=NEXT                                     
         BNE   EXIT                ERROR                                        
         CH    R1,=Y(MXSCN)        TEST ALREADY A LAST                          
         BNL   *+8                                                              
         AH    R1,=H'1'            BUMP TO NEXT                                 
*                                                                               
BLD11    STC   R1,SCRN             SAVE THE SCREEN NUMBER                       
         MVI   ERROR,X'FF'                                                      
         MVI   ANYKEY,C'Y'                                                      
         B     EXIT                                                             
         EJECT                                                                  
* *********************************************************************         
* DSP00 - DISPLAY ELEMENTS                                                      
* *********************************************************************         
*                                                                               
DSP00    CLI   MODE,DSPLYREC                                                    
         BNE   CHA00                                                            
         MVC   LDGERKEY,KEY        SAVE KEY TO LEDGER RECD                      
         LA    R8,LOGIDH           R8 TO FIRST IS ENTRY                         
         USING OFD,R8                                                           
         LA    R0,MXIDS                                                         
DSP01    NI    OFIDH+1,X'FF'-X'20' UNPROTECT ID FIELDS                          
         NI    OFNOH+1,X'FF'-X'20'          NUMBER                              
         NI    OFBNKH+1,X'FF'-X'20'         BANK                                
         NI    OFFLTH+1,X'FF'-X'20'         FILTER                              
         NI    OFLSTH+1,X'FF'-X'20'         LAST                                
         NI    OFSORTH+1,X'FF'-X'20'        SORT                                
         NI    OFPOFFH+1,X'FF'-X'20'        OFFICE                              
         NI    OFSTAH+1,X'FF'-X'20'         STATUS                              
         LA    R8,OFLNQ(R8)                                                     
         BCT   R0,DSP01                                                         
         TWAXC LOGIDH,LOGENDH      CLEAR THE SCREEN                             
         LR    RF,RA                                                            
         USING TWAD,RF                                                          
         CLI   TWAOFFC,C'*'        DDS TERMINAL                                 
         BE    *+8                                                              
         BAS   RE,PRO              PROTECT ALL FILEDS IF NOT DDS                
         SR    R1,R1                                                            
         IC    R1,SCRN             CURRENT SCREEN NUMBER                        
         MH    R1,=Y(MXIDS)        X NUMBER PER SCREEN                          
         LA    R1,1(R1)            PLUS 1                                       
*                                                                               
         LA    R4,CHKIO            PT TO LEDGER RECORD                          
         BAS   RE,RDCHK            SEE IF VALID CHK FILE EXISTS                 
         CLI   CHKFLG,C'Y'         IF IT DOES IT WILL BE IN CHIO AREA           
         BNE   DISP02              NEW CHK RECD WAS NOT FOUND                   
         MVC   CHKEY,CHIO          READ CHK RECD INTO CHKIO                     
         BAS   RE,IOREAD                                                        
         BE    DISP02                                                           
         DC    H'0'                                                             
*                                                                               
DISP02   AH    R4,DATADISP                                                      
         B     DSP03A                                                           
*                                                                               
         USING OCNELD,R4                                                        
DSP03    ZIC   R0,OCNLN            SKIP THE FIRST N (R1-1) ELEMENTS             
         AR    R4,R0                                                            
DSP03A   CLI   0(R4),0             CHECK FOR END OF RECD CONDITION              
         BE    DSPX                                                             
         CLI   OCNEL,X'54'         GET OFFICE CHECK ELEMENTS                    
         BNE   DSP03                                                            
         BCT   R1,DSP03                                                         
*                                                                               
*--WE GET HERE WHEN THE APPROPRIATE '54' ELEM WAS FOUND AT END OF BCT           
*                                                                               
         LA    R0,MXIDS            SET SCREEN MAX                               
         LA    R8,LOGIDH           R8 TO FIRST IS ENTRY                         
         USING OFD,R8                                                           
*                                                                               
DSP15    MVI   CTBYTE,0            READ FOR BINARY ID NUMBER                    
         BAS   RE,GETID            GET ID RECORD                                
         MVC   OFID,WORK           ALPHA ID                                     
         OI    OFIDH+1,X'20'       PROTECT ID FIELD                             
         MVC   OFNO,OCNAFT         NUMBER                                       
         CLI   OCNNTYP,0                                                        
         BE    *+10                                                             
         MVC   OFNO(1),OCNNTYP     NUMBER TYPE                                  
         MVC   OFBNK,OCNBANKA      BANK ACCOUNT                                 
         CLC   OCNBANKU(2),=C'SC'                                               
         BE    *+10                                                             
         MVC   OFBNK,OCNBANK                                                    
         MVC   OFFLT,OCNFILT       FILTER CLIENT OR OFFICE                      
         OC    OFFLT,SPACES                                                     
         CLI   OCNLN,OCNLNQ                                                     
         BL    *+10                                                             
         MVC   OFLST,OCNLAST       LAST CHECK                                   
*                                                                               
         XR    R5,R5               USED IN STUN ROUTINE                         
         LA    R3,BLOCK            R3 = UNSCAN BLOCK                            
         LA    R6,OCNSTAT          R6 = STATUS BYTE                             
         LA    R1,STATLST          R1 = STATUS TABLE                            
         BAS   RE,STUN             UNSCAN THE STATUS LINE                       
*                                                                               
         CLI   OCNLN,OCNLN3Q                                                    
         BL    DSP31                                                            
         LA    R6,OCNSTAT2         R6 = STATUS BYTE                             
         LA    R1,STATLST2         R1 = STATUS TABLE                            
         BAS   RE,STUN             UNSCAN THE STATUS LINE                       
*                                                                               
         LA    R6,OCNLASR          R6 = STATUS BYTE                             
         LA    R1,STATLASR         R1 = STATUS TABLE                            
         BAS   RE,STUNM            UNSCAN THE STATUS LINE                       
*                                                                               
DSP20    DS    0H                                                               
         LA    RF,SORTS            DISPLAY SORT FIELD                           
*                                                                               
DSP21    CLI   0(RF),X'FF'         NO SORT OPTION                               
         BE    DSP25                                                            
         CLC   OCNSORT,0(RF)                                                    
         BE    *+12                                                             
         LA    RF,L'SORTS(RF)                                                   
         B     DSP21                                                            
         MVC   OFSORT(L'SORTS-1),1(RF)                                          
*                                                                               
DSP25    MVC   OFPOFF,OCNPOFF      POSTING OFFICE                               
         OC    OFPOFF,SPACES                                                    
         OC    OCNDPSR,OCNDPSR     TEST SOON REGISTER PENDING                   
         BZ    DSP27                                                            
         LA    R6,=C'SOON'                                                      
         LA    R1,OCNDPSR                                                       
         BAS   RE,PDTE             DISPLAY THE PENDING DATE                     
*                                                                               
DSP27    OC    OCNDPLR,OCNDPLR     TEST LOCAL REGISTER PENDING                  
         BZ    DSP29                                                            
         LA    R6,=C'LOCAL'                                                     
         LA    R1,OCNDPLR                                                       
         BAS   RE,PDTE             DISPLAY THE PENDING DATE                     
*                                                                               
DSP29    OC    OCNSEQN,OCNSEQN     STACK SEQUENCE NUMBER                        
         BZ    DSP31                                                            
         LR    R3,R5               R3 = NUMBER OF ENTRIES                       
         MH    R3,=H'20'                                                        
         LA    R3,BLOCK(R3)        R3 = NEXT AVAILABLE ENTRY                    
         MVC   0(20,R3),SPACES                                                  
         MVC   0(5,R3),=C'STACK'                                                
         SR    R1,R1                                                            
         ICM   R1,3,OCNSEQN        SEQUENCE                                     
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  10(5,R3),DUB+5(3)   SEQUENCE NUMBER TO UNSCAN BLOCK              
         LA    R5,1(R5)            INCREMENT NUMBER OF ENTRIES                  
*                                                                               
DSP31    LTR   R5,R5               UNSCAN INTO THE STATUS FIELD                 
         BZ    DSP39                                                            
         LA    RF,OFSTAH                                                        
         GOTO1 =V(UNSCAN),DMCB,((R5),BLOCK),(RF),0,RR=RB                        
*                                                                               
DSP39    SR    R1,R1                                                            
         IC    R1,OCNLN            GET NEXT ELEMENTS                            
         AR    R4,R1                                                            
         CLI   0(R4),0             END OF RECORD                                
         BE    DSPX                                                             
         CLI   OCNEL,X'54'         GET NEXT OFFICE CHECK ELEMENTS               
         BNE   DSP39                                                            
         LA    R8,OFLNQ(R8)        NEXT SCREEN ENTRY                            
         BCT   R0,DSP15                                                         
*                                                                               
DSPX     MVI   ERROR,X'FF'                                                      
         LA    R2,LOGCOMPH         COMPANY FIELD FOR INQUIRY                    
         CLI   LOGACT,C'I'                                                      
         BE    *+8                                                              
         LA    R2,LOGIDH           ID FIELD FOR CHANGE                          
         TM    LOGIDH+1,X'20'                                                   
         BNO   *+8                                                              
         LA    R2,LOGNOH                                                        
         TM    LOGCOMPH+1,X'20'                                                 
         BNO   *+8                                                              
         LA    R2,LOGLDGH                                                       
         OI    6(R2),X'40'         INSERT CURSOR                                
         B     EXIT                                                             
         EJECT                                                                  
* ********************************************************************          
* CHA00  -  CHANGE RECORD, BUILD ELEMENTS                                       
* ********************************************************************          
*                                                                               
CHA00    DS    0H                                                               
         MVC   LDGERKEY,KEY        SAVE AWAY THE LEDGER KEY                     
         LA    R2,LOGACTH                                                       
         MVI   ERROR,NOTVLACT      NO VALID ACTION                              
         LR    RF,RA                                                            
         USING TWAD,RF                                                          
         CLI   TWAOFFC,C'*'        UNLESS DDS                                   
         BNE   EXIT                                                             
         LA    R8,LOGIDH                                                        
         USING OFD,R8                                                           
         LA    R0,MXIDS                                                         
*                                                                               
CHA07    XC    ELEMENT,ELEMENT     BUILD AN ELEMENT                             
         LA    R4,ELEMENT                                                       
         USING OCNELD,R4                                                        
         MVI   OCNEL,X'54'         SET ELEMENT CODE                             
         MVI   OCNLN,OCNLN3Q       AND LENGTH                                   
         MVC   OCNBEF,OFNO         CHECK NUMBER                                 
         MVC   OCNAFT,OFNO                                                      
         TM    OFIDH+1,X'20'       IS ID PROCTECTED                             
         BO    CHA09                                                            
         CLI   OFIDH+5,0           ANY ID INPUT                                 
         BNE   CHA09               YES, VALIDATE ID                             
         MVI   ERROR,INVALID                                                    
         LA    R2,OFNOH            ANY NUMBER                                   
         CLI   OFNOH+5,0                                                        
         BE    CHA85               NO, OK TO SKIP LINE                          
         B     EXIT                ERROR                                        
*                                                                               
CHA09    LA    R2,OFIDH                                                         
         MVI   CTBYTE,1            SET FOR ALPHA ID                             
         BAS   RE,GETID            GET ID RECORD                                
         MVC   OCNOFFID,WORK       USER ID                                      
         CLC   OFNO,=C'DELETE'                                                  
         BNE   CHA10                                                            
         MVI   ERROR,0                                                          
         B     CHA11                                                            
*                                                                               
CHA10    CLI   ERROR,X'FF'         INVALID ID                                   
         BNE   EXIT                                                             
         LA    R2,OFNOH            VALIDATE CHECK NUMBER                        
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLI   OFNOH+5,6           CHECK NUMBER MUST BE 6 LONG                  
         BNE   EXIT                                                             
         MVC   WORK(5),=5X'F0'                                                  
         MVZ   WORK(5),OFNO+1                                                   
         CLC   WORK(5),=5X'F0'     2-6 MUST BE NUMERIC                          
         BNE   EXIT                                                             
         CLI   OFNO,C'0'                                                        
         BNL   CHA11                                                            
         MVC   OCNNTYP,OFNO        SPECIAL LETTER                               
         MVI   OCNBEF,C'0'                                                      
         MVI   OCNAFT,C'0'                                                      
*                                                                               
CHA11    LA    R2,OFBNKH           VALIDATE BANK                                
         GOTO1 ANY                                                              
         MVC   KEY+1(L'KEY-1),SPACES                                            
         MVC   KEY+1(2),=C'SC'                                                  
         OC    OFBNK,SPACES                                                     
         MVC   KEY+3(L'OFBNK),OFBNK                                             
         BAS   RE,CTLREAD          SEE IF BANK ACCOUNT EXISTS                   
         MVC   OCNBANK,KEY                                                      
*                                                                               
         MVI   ERROR,INVALID                                                    
         LA    R2,OFFLTH           FILTER FIELD                                 
         CLI   OFFLTH+5,0                                                       
         BE    CHA15               NO FILTER                                    
         CLI   OFFLT,C'*'          TEST OFFICE FILTER                           
         BNE   CHA13               BRANCH IF CLIENT FILTER                      
         TM    COMPSTA4,X'01'      2 BYTE OFFICE CODES                          
         BO    EXIT                OFFICE FILTER NOT ALLOWED                    
         CLI   OFFLTH+5,2                                                       
         BNE   EXIT                MUST BE AT LEAST 2                           
         MVC   OCNFILT,OFFLT       SAVE THE OFFICE FILTER                       
         OC    OCNFILT,SPACES                                                   
         B     CHA15                                                            
*                                                                               
CHA13    MVC   KEY+1(L'KEY-1),SPACES  VALIDATE CLIENT                           
         MVC   KEY+1(2),=C'SJ'                                                  
         OC    OFFLT,SPACES                                                     
         MVC   KEY+3(L'OFFLT),OFFLT                                             
         BAS   RE,CTLREAD          CLIENT MUST BE ON SJ                         
         MVC   OCNFILT,KEY+3                                                    
*                                                                               
CHA15    LA    R2,OFLSTH           VALIDATE LAST CHECK NUMBER                   
         MVI   ERROR,INVALID                                                    
         CLI   OFLSTH+5,0          NO LAST CHECK INPUT                          
         BE    CHA17                                                            
         CLI   OFLSTH+5,6                                                       
         BNE   EXIT                MUST BE 6 LONG                               
         MVC   WORK(5),=5X'F0'                                                  
         MVZ   WORK(5),OFLST                                                    
         CLC   WORK(5),=5X'F0'     2-6 MUST BE NUMERIC                          
         BNE   EXIT                                                             
         MVC   OCNLAST,OFLST                                                    
*                                                                               
CHA17    LA    R2,OFSORTH          VALIDATE SORT OPTION                         
         MVI   ERROR,INVALID                                                    
         CLI   OFSORTH+5,0         NO SORT OPTION                               
         BE    CHA21                                                            
         SR    R1,R1                                                            
         IC    R1,OFSORTH+5                                                     
         BCTR  R1,0                                                             
         LA    RF,SORTS                                                         
*                                                                               
CHA19    CLI   0(RF),X'FF'         INVALID SORT OPTION                          
         BE    EXIT                ERROR                                        
         EX    R1,*+8              INPUT FIELD VS. TABLE                        
         B     *+10                                                             
         CLC   OFSORT(0),1(RF)                                                  
         BE    *+12                                                             
         LA    RF,L'SORTS(RF)                                                   
         B     CHA19                                                            
         MVC   OCNSORT,0(RF)       CODE TO ELEMENT                              
         MVC   OFSORT(L'SORTS-1),1(RF) FULL NAME TO SCREEN                      
         OI    OFSORTH+6,X'80'                                                  
*                                                                               
CHA21    LA    R2,OFPOFFH                                                       
         GOTO1 ANY                 FIELD IS REQUIRED (AS OF 4/6/00)             
         OC    OFPOFF,SPACES       OFFICE CODE FOR CASH POSTING                 
         MVC   OCNPOFF,OFPOFF                                                   
         TM    COMPSTA4,X'01'      NEW OFFICES                                  
         BNO   CHA25                                                            
         MVC   CHKEY,SPACES                                                     
         LA    RF,CHKEY                                                         
         USING OFFRECD,RF                                                       
         MVI   OFFKTYP,OFFKTYPQ    VALIDATE OFFICE                              
         MVC   OFFKCPY,COMPANY                                                  
         MVC   OFFKOFF,OFPOFF                                                   
         MVI   ERROR,NOOFFICE                                                   
         BAS   RE,ACREAD                                                        
         BNE   EXIT                                                             
         B     CHA27                                                            
*                                                                               
CHA25    CLI   OFPOFFH+5,1         OLD OFFICES - MAX LENGTH IS 1                
         BH    EXIT                                                             
*                                                                               
CHA27    LA    R2,OFSTAH           VALIDATE STATUS LINE                         
         LA    R3,BLOCK                                                         
         MVI   ERROR,X'FF'                                                      
         BAS   RE,STED             EDIT STATUS LINE                             
         CLI   ERROR,X'FF'                                                      
         BNE   EXIT                                                             
         MVI   ERROR,INVALID                                                    
         TM    OCNSTAT2,OCNSMICR+OCNSLBLT CAN'T HAVE MICR & BLT                 
         BO    EXIT                                                             
         TM    OCNSTAT2,OCNSMICR+OCNSFTP  CAN'T HAVE MICR & FTP                 
         BO    EXIT                                                             
         TM    OCNSTAT2,OCNSLBLT+OCNSFTP  CAN'T HAVE FTP & BLT                  
         BO    EXIT                                                             
         TM    OCNSTAT,OCNSSOON+OCNSLOCL  TEST SOON OR LOCAL                    
         BNZ   CHA28                                                            
         TM    OCNSTAT2,OCNSMICR+OCNSLBLT CAN'T BE MICR OR LASER(BLT)           
         BNZ   EXIT                                                             
         B     CHA31                                                            
*                                                                               
*                                  LOCAL OR SOON                                
*                                                                               
CHA28    CLI   OCNLASR,0           CAN'T HAVE LASER(XXX)                        
         BNE   EXIT                                                             
         TM    OCNSTAT,OCNSPRTQ+OCNSDREG+OCNSSHUT DIRECT, DIREG,                
         BNZ   EXIT                               SHUTTLE                       
         EJECT                                                                  
*                                                                               
* 1. SEARCH FOR CHK RECORD INTO CHIO AREA                                       
* 2. TEST IF THERE ARE ANY '54' ELEMENTS ON LEDGER RECORD.                      
*    YES- IS THERE A '01'(NEW RECD) OUT THERE?                                  
*         A) NO- 1)CREATE IT 2)XFER ELEMENTS 3)REMOVE '54' FROM LEDGER          
*         B) YES-1)XFER ELEMENTS TO RECD- IGNORE PRE-EXISTING ELEMENTS          
*         PLACE THE NEW RECORD IN CHKIO.                                        
*    NO - IF THERE IS AN '01' RECD, READ IT INTO CHKIO                          
*       - IF RECD IS DELETED, CLEAR AWAY ALL ELEMENTS                           
* 3. PROCESS CHECK ELEMENTS                                                     
* 4. JUST BEFORE DOING AN ADDELEM,TEST IF CHKIO HAS NEW RECD (X'01')            
*    NO - CREATE NEW RECD WITH THE ELEMENT AND PLACE IN CHKIO                   
* 5. JUST BEFORE DOING A REMELEM, SEE IF THIS IS THE LAST ELEM ON RECD?         
*    YES- DO A DMWRT TO DELETE THE RECORD                                       
*                                                                               
         SPACE 2                                                                
CHA31    BAS   RE,RDCHK            READ THE CHECK FILE INTO CHIO AREA           
         CLI   CHKIO,CHAKTYPQ      CHK RECD OR LEDGR REC IN CHKIO.              
         BE    CHA34               CHECK RECD                                   
         MVI   ELCODE,X'54'        SEARCH THRU LEDGER REC FOR '54'S             
         LA    R5,CHKIO            PT TO LEDGER REC                             
         BAS   RE,GETEL            ANY '54'S ON LEDGER FILE                     
         BNE   CHA32               NO                                           
         BAS   RE,XFER             YES TRANSFER THEM                            
*                                                                               
CHA32    CLI   CHKFLG,C'Y'         IF THERE'S A CHK REC MOVE IN CHKIO           
         BNE   CHA33               NO, SET UP NEW REC FMT IN CHKIO              
         MVC   KEY,CHKEY           YES, READ IT INTO CHKIO BUFFER               
         BAS   RE,IOREAD                                                        
         BZ    CHA34                                                            
         DC    H'0'                                                             
*                                                                               
CHA33    MVC   KEY,LDGERKEY        MAKE SURE KEY HAS LEDGER RECD KEY            
         LA    R6,CHKIO            BUILD NEW RECD IN CHKIO                      
         BAS   RE,NEWREC           IF DEL/NEW REC, BUILD A NEW RECD             
         MVC   KEY,CHIO                                                         
*                                                                               
CHA34    MVC   KEY,CHKIO           USE CHECK KEY                                
         LA    R4,CHKIO            FIND ELEMENT IN THE RECORD                   
         AH    R4,DATADISP                                                      
         SR    RF,RF                                                            
         SR    R1,R1                                                            
         B     CHA38                                                            
*                                                                               
CHA37    IC    R1,OCNLN           FIND MATCHING ELEMENTS                        
         AR    R4,R1                                                            
CHA38    CLI   OCNEL,0                                                          
         BE    CHA40                                                            
         CLI   OCNEL,X'54'        FIND 54 ELEMENTS                              
         BNE   CHA37                                                            
*                                                                               
CHA39    LA    R5,ELEMENT                                                       
         CLC   OCNOFFID,OCNOFFID-OCNELD(R5) MATCH ID NUMBER                     
         BNE   CHA37                                                            
         LA    R2,OFIDH                                                         
         MVI   ERROR,DUPLCATE      DON'T ALLOW DUPLICATES                       
         TM    OFIDH+1,X'20'       IF PROTECTED IT'S OLD ID                     
         BZ    EXIT                NEW - DUPLICATE                              
         LTR   RF,RF                                                            
         BNZ   EXIT                                                             
         LR    RF,R4               SAVE FIRST MATCHING ID                       
         CLC   OFNO,=C'DELETE'                                                  
         BE    CHA41                                                            
         B     CHA37               LOOK FOR ANOTHER                             
*                                                                               
CHA40    DS    0H                                                               
         CLC   OFNO,=C'DELETE'                                                  
         BNE   CHA41                                                            
         LA    R4,CHKIO                                                         
         AH    R4,DATADISP                                                      
CHA41A   CLI   OCNEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   OCNEL,X'54'                                                      
         BNE   CHA41F                                                           
         LA    R5,ELEMENT                                                       
*                                                                               
         ZIC   R3,OFBNKH+5                                                      
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   OCNBANKA(0),OCNBANKA-OCNELD(R5)                                  
         BNE   CHA41F                                                           
*                                                                               
         CLI   OFFLTH+5,0                                                       
         BE    CHA41B                                                           
         CLC   OCNFILT,OCNFILT-OCNELD(R5)                                       
         BNE   CHA41F                                                           
*                                                                               
CHA41B   CLI   OFLSTH+5,0                                                       
         BE    CHA41J                                                           
         CLC   OCNLAST,OCNLAST-OCNELD(R5)                                       
         BE    CHA41J                                                           
*                                                                               
CHA41F   ZIC   R1,OCNLN                                                         
         AR    R4,R1                                                            
         B     CHA41A                                                           
CHA41J   LR    RF,R4                                                            
*                                                                               
CHA41    DS    0H                                                               
         LTR   RF,RF               ANY MATCHES ON FILE                          
         BZ    CHA72                                                            
                                                                                
         LA    R4,ELEMENT                                                       
         CLI   1(RF),OCNLN2Q                                                    
         BNH   CHA69                                                            
         CLC   OCNDPSR-OCNEL(L'OCNDPSR,R4),OCNDPSR-OCNEL(RF)                    
         BE    CHA50                                                            
         BH    CHA50                                                            
         MVC   OCNDPSR-OCNEL(L'OCNDPSR,R4),OCNDPSR-OCNEL(RF)                    
CHA50    CLC   OCNDPLR-OCNEL(L'OCNDPLR,R4),OCNDPLR-OCNEL(RF)                    
         BE    CHA69                                                            
         BH    CHA69                                                            
         MVC   OCNDPLR-OCNEL(L'OCNDPLR,R4),OCNDPLR-OCNEL(RF)                    
                                                                                
CHA69    MVI   0(RF),X'FF'         DELETE OLD ELEMENT                           
         GOTO1 REMELM,DMCB,(X'FF',0)                                            
CHA72    BAS   RE,GETLNGTH         RESET RECORD LENGTH                          
         LA    R1,CHKIO            SEE IF ALL THE ELEMENTS DELETED              
         CLI   ACCORFST(R1),0      END OF RECORD?                               
         BNE   CHA83               NO                                           
         OI    ACCOSTAT(R1),X'80'  MARK FOR DELETION                            
*                                                                               
CHA83    CLC   OFNO,=C'DELETE'                                                  
         BE    CHA85                                                            
         CLI   CHKFLG,C'N'         IF NO PREV FILE, CREATE IT                   
         BNE   *+12                                                             
         BAS   RE,MYADD            ADD ELEMENT TO CHKIO AND DO A DMADD          
         B     CHA85                                                            
*                                                                               
         LA    R1,CHKIO            SEE IF RECD WAS MARKED FOR DELETION          
         TM    ACCOSTAT(R1),X'80'                                               
         BNO   *+8                 NO                                           
         NI    ACCOSTAT(R1),X'7F'  UN-MARK IT                                   
         BAS   RE,ADDELM           ADD THE NEW ONE- DMWRT                       
*                                                                               
CHA85    LA    R8,OFLNQ(R8)        NEXT SCREEN ENTRY                            
         MVC   KEY,LDGERKEY        RESTORE LEDGER KEY                           
         BCT   R0,CHA07                                                         
*                                                                               
         LA    R8,LOGIDH           R8 TO FIRST IS ENTRY                         
         LA    R0,MXIDS                                                         
*                                                                               
CHA90    CLI   OFIDH+5,0           IF INPUT                                     
         BE    *+12                                                             
         OI    OFIDH+1,X'20'       PROTECT ID                                   
         OI    OFIDH+6,X'80'                                                    
         LA    R8,OFLNQ(R8)                                                     
         BCT   R0,CHA90                                                         
*                                                                               
         MVI   ERROR,90            CHECK IF ANYTHING THERE                      
         LA    R2,LOGIDH           POINT TO FIRST FIELD                         
         OI    6(R2),X'40'         INSERT CURSOR                                
         OC    CHKIO(L'CHKEY),CHKIO                                             
         BZ    EXIT                                                             
*                                                                               
         MVI   ERROR,X'FF'                                                      
         LA    R2,LOGCOMPH         COMPANY FIELD FOR INQUIRY                    
         OI    6(R2),X'40'         INSERT CURSOR                                
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'ACCOUNT',CHKIO,CHKIO                   
         CLI   DMCB+8,0            ANY ERRORS ADDING FILE?                      
         BE    *+6                                                              
         DC    H'0'                UNABLE TO DO THE WRT                         
*                                                                               
EXIT     XIT1  REGS=(R2)                                                        
         EJECT                                                                  
* ********************************************************************          
* NEWREC- BUILD KEY FOR THE NEW CHK RECD IN AREA PTD TO BY R6.                  
*         NOTE:  (R6) - PTS TO AREA TO BUILD NEW RECD IN                        
* ********************************************************************          
*                                                                               
NEWREC   NTR1                                                                   
         BAS   RE,CLEARIO          CLEAR BUILD AREA PTD TO BY R6                
         MVC   0(L'CHKEY,R6),SPACES  BLANK PAD THE KEY                          
         USING CHARECD,R6                                                       
         MVI   CHAKTYP,CHAKTYPQ                                                 
         MVC   CHAKCULA,LDGERKEY                                                
         LR    RF,R6               SAVE ADDR OF START OF RECD                   
         AH    R6,DATADISP         DISP TO 1ST ELEMENT                          
         MVI   0(R6),X'00'         NO ELEMENTS YET                              
         LA    R6,1(R6)            CALCULATE LENGTH OF RECORD                   
         SR    R6,RF               LENGTH OF RECD                               
         STCM  R6,3,ACCORLEN(RF)   PUT LENGTH IN RECD                           
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* ********************************************************************          
* RDCHK - LOOK FOR NEW CHK RECORD. IF IT EXIST, THE CONDITION CODE WILL         
*         BE SET TO '=' AND THE RECORD WILL BE IN CHIO.                         
*         NOTE: BUILD NEW KEY USING THE LEDGER RECDS KEY FROM THE               
*               CHKIO AREA, NOT FROM 'KEY' SINCE KEY MAY BE CORRUPTED.          
* ********************************************************************          
*                                                                               
RDCHK    NTR1                                                                   
         LA    R6,CHIO             PT TO CHIO AREA                              
         BAS   RE,CLEARIO          CLEAR IT                                     
         MVC   CHKEY,SPACES        CLEAR KEY AS WELL!!!                         
         MVI   CHKFLG,C'N'         CHK RECD EXISTS FLAG: INIT=NO                
         LA    RF,CHKEY            BUILD NEW KEY                                
         USING CHARECD,RF          USE CHECK RECORD DSECT                       
         MVI   CHAKTYP,CHAKTYPQ    KEY REC CODE                                 
         MVC   CHAKCULA,LDGERKEY   SEE IF SAME KEY EXISTS                       
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMREAD'),=C'ACCOUNT',CHKEY,CHIO           
         TM    DMCB+8,X'10'        REC NOT FOUND?                               
         BO    RDCHKX              NOT FOUND                                    
         MVI   CHKFLG,C'Y'         CHK RECD EXISTS                              
         TM    DMCB+8,X'02'        REC DELETED?                                 
         BNO   RDCHKX              NO                                           
         MVI   CHKFLG,C'D'         SET FLAG TO RECD MARKED FOR DELETION         
*                                                                               
RDCHKX   B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
* ********************************************************************          
* XFER - TRANSFER '54' ELEMENTS FROM THE LEDGER FILE TO THE NEW CHECK           
*        FILE.  DELETE THEM FROM THE LEDGER FILE.                               
* ********************************************************************          
         SPACE 1                                                                
XFER     NTR1                                                                   
         MVC   KEY,LDGERKEY        MAKE SURE KEY HAD LEDGER KEY                 
         LA    R6,CHIO             R6=PTS TO CHIO AREA                          
         BAS   RE,NEWREC           BUILD CHKKEY+0 ELEMENTS IN CHIO              
         AH    R6,DATADISP         ADDR OF 1ST ELEMENT                          
         USING CHARECD,R6                                                       
*                                                                               
* COPY ALL X'54' ELEMENTS FROM THE LEDGER RECD TO THE NEW CHK RECD              
*                                                                               
         LA    R5,CHKIO            PT TO LEDGER REC                             
         MVI   ELCODE,X'54'                                                     
         BAS   RE,GETEL            PT R5 TO 1ST '54' ELEM                       
         BNE   XFERX               NO '54'S EXIT                                
*                                                                               
XFER05   ZIC   R1,1(R5)            LENGTH OF ELEMENT                            
         BCTR  R1,0                DECR FOR EXMVC                               
         EXMVC R1,0(R6),0(R5)      COPY CHK ELEMT TO NEW CHK RECD               
         LA    R1,1(R1)            RESET ACTUAL LENGTH                          
         AR    R6,R1               BUMP NEW RECD PTR                            
         MVI   0(R5),X'FF'         MARK FOR DELETION                            
         BAS   RE,NEXTEL           GET NEXT '54' ELEMENT                        
         BE    XFER05              PROCESS ALL ELEMENTS                         
         MVI   0(R6),X'00'         END OF RECORD MARKER                         
         LA    R6,1(R6)                                                         
*                                                                               
         LA    R1,CHIO             CALCULATE LENGTH OF RECORD                   
         SR    R6,R1                                                            
         STCM  R6,3,ACCORLEN(R1)   SAVE LENGTH OF RECORD                        
*                                                                               
* IF RECD ALREADY EXIST, OVERWRITE IT (DMWRT) ELSE DO A DMADD                   
*                                                                               
         CLI   CHKFLG,C'N'         RECD DOES NOT EXIST, DO AN ADDREC            
         BNE   XFER20                                                           
*                                                                               
XFER10   GOTO1 DATAMGR,DMCB,=C'DMADD',=C'ACCOUNT',CHIO,CHIO                     
         CLI   DMCB+8,0            ANY ERRORS ADDING FILE?                      
         BE    XFER30              NO                                           
         DC    H'0'                UNABLE TO DO THE ADD                         
*                                                                               
XFER20   GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'ACCOUNT',CHIO,CHIO                     
         CLI   DMCB+8,0            ANY ERRORS ADDING FILE?                      
         BE    XFER30              NO                                           
         DC    H'0'                UNABLE TO DO THE WRT                         
*                                                                               
XFER30   BAS   RE,RMVELEM          REMOVE THE 'FF' ELEM FROM LEDGER             
         BE    *+6                 NO PROBLEM WRITING OUT THE RECD              
         DC    H'0'                                                             
*                                                                               
         MVC   CHKEY,CHIO          PUT NEW RECD IN CHKIO                        
         BAS   RE,IOREAD                                                        
         BZ    *+6                 READ WAS OKAY                                
         DC    H'0'                READ FAILED                                  
         MVI   CHKFLG,C'Y'         YES IT DOES, SET FLAG                        
*                                                                               
XFERX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* ********************************************************************          
* MYADD- ADD NEW ELEMENT TO RECORD IN CHKIO. DO AN ADDREC                       
* ********************************************************************          
*                                                                               
MYADD    NTR1                                                                   
         LA    R4,ELEMENT                                                       
         LA    R6,CHKIO            PT TO NEW RECD IN CHKIO                      
         USING CHARECD,R1                                                       
         NI    ACCOSTAT(R6),X'7F'  UN-MARK IT                                   
         AH    R6,DATADISP         PT TO 1ST ELEMENT POSITION                   
         ZIC   RF,OCNLN            LENGTH OF '54' ELEMENT                       
         BCTR  RF,0                                                             
         EXMVC RF,0(R6),ELEMENT                                                 
         ZIC   RF,OCNLN            RESTORE LENGTH OF ELEM                       
         AR    R6,RF               PT TO NEXT ELEM POSN                         
         MVI   0(R6),X'00'         END OF RECORD MARKER                         
         LA    R6,1(R6)                                                         
*                                                                               
         LA    RF,CHKIO            CALCULATE LENGTH OF RECORD                   
         SR    R6,RF               LENGTH OF RECORD                             
         STCM  R6,3,ACCORLEN(RF)   SAVE LENGTH OF RECD                          
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'ACCOUNT',CHKIO,CHKIO                   
         CLI   DMCB+8,0            ANY ERRORS ADDING FILE?                      
         BE    MYADDX              NO                                           
         DC    H'0'                UNABLE TO DO THE ADD                         
*                                                                               
MYADDX   B     XIT                                                              
         DROP  R1                                                               
*                                                                               
         EJECT                                                                  
* ********************************************************************          
* GETLNGTH - GET LENGTH OF RECORD IN CHKIO                                      
* ********************************************************************          
*                                                                               
GETLNGTH NTR1                                                                   
         LA    R2,CHKIO                                                         
         AH    R2,DATADISP                                                      
         SR    R3,R3                                                            
                                                                                
GETLEN2  CLI   0(R2),0                                                          
         BE    GOTEND                                                           
         IC    R3,1(R2)                                                         
         AR    R2,R3                                                            
         LTR   R3,R3                                                            
         BNZ   GETLEN2                                                          
         DC    H'0'                                                             
         DC    C'ZERO LENGTH ELEMENT'                                           
                                                                                
GOTEND   LA    R2,1(R2)                                                         
         LA    R3,CHKIO                                                         
         SR    R2,R3                                                            
         STH   R2,DUB                                                           
         MVC   CHKIO+42(2),DUB                                                  
         B     XIT                                                              
         DROP  R8                                                               
         EJECT                                                                  
* ********************************************************************          
* PROTECT ALL FIELDS FOR NON-DDS                                                
* ********************************************************************          
*                                                                               
PRO      LA    R1,LOGIDH           R8 TO FIRST IS ENTRY                         
         USING OFD,R1                                                           
         LA    R0,MXIDS                                                         
PRO1     OI    OFIDH+1,X'20'        PROTECT ID                                  
         OI    OFNOH+1,X'20'                NUMBER                              
         OI    OFBNKH+1,X'20'               BANK                                
         OI    OFFLTH+1,X'20'               FILTER                              
         OI    OFLSTH+1,X'20'               LAST                                
         OI    OFSORTH+1,X'20'              SORT                                
         OI    OFPOFFH+1,X'20'              OFFICE                              
         OI    OFSTAH+1,X'20'               STATUS                              
         LA    R1,OFLNQ(R1)                                                     
         BCT   R0,PRO1                                                          
         BR    RE                                                               
         EJECT                                                                  
* ******************************************************************            
* CLEARIO- CLEAR IO AREA PTD TO BY (R6)                                         
* ******************************************************************            
*                                                                               
CLEARIO  NTR1                                                                   
*                                                                               
         LR    RE,R6               CLEAR AREA PTD TO BY R6                      
         LA    RF,L'CHIO                                                        
         LA    R0,SPACES                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         B     XIT                                                              
         SPACE 3                                                                
* ********************************************************************          
* RMVELEM- DELETE 'FF' ELEMENTS FROM CHKIO. WRITE OUT THE UPDATED RECD          
* ********************************************************************          
*                                                                               
RMVELEM  NTR1  ,                   REMOVE FF ELEMS AND WRITE OUT RECD           
         GOTO1 REMELM,DMCB,(X'FF',0)   DELETE X'FF' ELEMENTS                    
         BAS   RE,GETLNGTH         GET LENGTH OF RECD                           
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'ACCOUNT',CHKIO,CHKIO                   
         CLI   DMCB+8,0            ANY ERRORS ADDING FILE?                      
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO ADD AN ELEMENT                                          *          
**********************************************************************          
         SPACE 1                                                                
ADDELM   NTR1                                                                   
         GOTO1 =V(ACPUTEL),DMCB,CHKIO,ELEMENT,RR=RELO                           
         BAS   RE,GETLNGTH         GET LENGTH OF NEW RECORD                     
         CLC   CHKIO+42(2),=H'2000'                                             
         BNH   ADDELMX                                                          
         LA    R2,LOGRECH                                                       
         MVI   ERROR,66                                                         
ADDELMX  B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO REMOVE AN ELEMENT                                       *          
**********************************************************************          
         SPACE 1                                                                
REMELM   NTR1                                                                   
         LA    R4,CHKIO                                                         
         AH    R4,DATADISP                                                      
         SR    R5,R5                                                            
*                                                                               
REMELM10 CLI   0(R4),0                                                          
         BE    REMELM20                                                         
         CLC   0(1,R4),0(R1)                                                    
         BNE   *+8                 REMOVE ELEMENTS THAT MATCH                   
         MVI   0(R4),X'FF'                                                      
         IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     REMELM10                                                         
*                                                                               
REMELM20 GOTO1 =V(ACDELEL),DMCB,CHKIO,RR=RELO                                   
         LA    R6,CHKIO                                                         
         AH    R6,DATADISP                                                      
*                                                                               
REMELM30 CLI   0(R6),0             FIND END OF NEW RECORD                       
         BE    REMELM40                                                         
         IC    R5,1(R6)                                                         
         AR    R6,R5                                                            
         B     REMELM30                                                         
*                                                                               
REMELM40 MVI   0(R6),0             CLEAR BALANCE                                
         CR    R6,R4                                                            
         BE    REMELMX                                                          
         LA    R6,1(R6)                                                         
         B     REMELM40                                                         
*                                                                               
REMELMX  B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
IOREAD   NTR1  ,                                                                
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',CHKEY,CHKIO                  
         CLI   8(R1),0                                                          
         B     XIT                                                              
*                                                                               
CTLREAD  NTR1  ,                   CONTROLLERS GOTO1 READ COMMAND               
         GOTO1 READ                                                             
         B     XIT                                                              
*                                                                               
ACREAD   NTR1  ,                                                                
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',CHKEY,CHIO                   
         CLI   8(R1),0                                                          
         B     XIT                                                              
*                                                                               
ACHIGH   NTR1  ,                                                                
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCOUNT',CHKEY,CHIO                   
         CLC   CHKEY,CHIO                                                       
         B     XIT                                                              
*                                                                               
ACWRT    NTR1  ,                                                                
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'ACCOUNT',CHKEY,CHIO                    
         CLI   8(R1),0                                                          
         BE    XIT                                                              
         DC    H'0'                                                             
                                                                                
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* GETID- READ CONTROL FILE FOR ID                                               
***********************************************************************         
GETID    NTR1  ,                                                                
         USING OFD,R8                                                           
         LA    R5,CNTRLIO                                                       
         USING CTIREC,R5                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         CLI   CTBYTE,0            READ FOR ALPHA                               
         BNE   GET2                                                             
         MVC   CTIKID+8(2),OCNOFFID                                             
         B     GET4                                                             
*                                                                               
GET2     MVC   CTIKID,SPACES       READ FOR NUMBER                              
         MVC   CTIKID(L'OFID),OFID                                              
         OC    CTIKID,SPACES                                                    
*                                                                               
GET4     MVC   CTSAVE,CTIKEY                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',(R5),(R5)                     
         MVI   ERROR,INVALID                                                    
         CLC   CTSAVE,CTIKEY                                                    
         BNE   XIT                                                              
         LA    R5,CTIDATA          FIND ID ELEMENT                              
         SR    R0,R0                                                            
*                                                                               
GET8     CLI   0(R5),0             NO ELEMENT                                   
         BE    XIT                                                              
         CLI   0(R5),X'02'                                                      
         BE    GET10                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     GET8                                                             
*                                                                               
GET10    MVC   WORK(7),2(R5)                                                    
         MVI   ERROR,X'FF'                                                      
         B     XIT                                                              
         EJECT                                                                  
* *********************************************************************         
* STED-  EDIT THE STATUS LINE                                                   
*        R4 = OFFICE CHECK ELEMENT                                              
*        R1 = ADDRESS                                                           
* *********************************************************************         
*                                                                               
STED     NTR1  ,                                                                
         LR    R8,R1               SAVE ADDRESS PASSED                          
         CLI   5(R2),0             SCAN INPUT LINE                              
         BE    XIT                                                              
         MVI   ERROR,INVALID                                                    
         GOTO1 =V(SCANNER),DMCB,(R2),(10,BLOCK),RR=RB                           
         XR    R0,R0                                                            
         IC    R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BZ    XIT                                                              
         LA    R3,BLOCK            MATCH ITEM IN BLOCK TO TABLE                 
         MVI   FNDX,1                                                           
*                                                                               
STED03   LA    R8,STATLST                                                       
         LA    R6,OCNSTAT                                                       
*                                                                               
STED05   CLC   12(10,R3),2(R8)     ITEM IN BLOCK - TABLE                        
         BE    STED09                                                           
         CLI   0(R8),1             ONE SIDED ENTRY                              
         BE    *+8                                                              
         LA    R8,10(R8)           NEXT TABLE ENTRY                             
         LA    R8,12(R8)                                                        
         CLI   0(R8),X'FF'         END OF TABLE                                 
         BNE   STED05              LOOK AT NEXT                                 
*                                                                               
         CLI   OCNLN,OCNLN3Q                                                    
         BL    XIT                                                              
         LA    R8,STATLST2                                                      
         LA    R6,OCNSTAT2                                                      
STED07   CLC   12(10,R3),2(R8)     ITEM IN BLOCK - TABLE                        
         BE    STED09                                                           
         CLI   0(R8),1             ONE SIDED ENTRY                              
         BE    *+8                                                              
         LA    R8,10(R8)           NEXT TABLE ENTRY                             
         LA    R8,12(R8)                                                        
         CLI   0(R8),X'FF'         END OF TABLE                                 
         BNE   STED07              LOOK AT NEXT                                 
*                                                                               
         LA    R8,STATLASR                                                      
         LA    R6,OCNLASR                                                       
STED08   CLC   12(10,R3),2(R8)     ITEM IN BLOCK - TABLE                        
         BE    STED08A                                                          
         CLI   0(R8),1             ONE SIDED ENTRY                              
         BE    *+8                                                              
         LA    R8,10(R8)           NEXT TABLE ENTRY                             
         LA    R8,12(R8)                                                        
         CLI   0(R8),X'FF'         END OF TABLE                                 
         BNE   STED08              LOOK AT NEXT                                 
         B     XIT                 IF NO TABLE ENTRY IS INVALID INPUT           
*                                                                               
STED08A  CLI   0(R8),1             ONE SIDED ENTRY                              
         BE    *+14                                                             
         CLC   22(10,R3),12(R8)    MUST MATCH RIGHT SIDE OF EQUAL               
         BNE   STED05                                                           
         CLI   0(R6),0                                                          
         BNE   XIT                                                              
         TM    OCNSTAT,OCNSSHUT    IF SHUTTLE IS ON ALSO, EXIT WITH             
         BO    XIT                 ERROR                                        
         MVC   0(1,R6),1(R8)       TURN ON BIT                                  
         B     STED90                                                           
*                                                                               
STED09   CLI   0(R8),1             ONE SIDED ENTRY                              
         BE    *+14                                                             
         CLC   22(10,R3),12(R8)    MUST MATCH RIGHT SIDE OF EQUAL               
         BNE   STED05                                                           
         OC    0(1,R6),1(R8)       TURN ON BIT                                  
         LA    R5,OCNDPSR          SOON PENDING                                 
         CLC   12(10,R3),=CL10'SOON'                                            
         BE    STED11                                                           
         LA    R5,OCNDPLR          LOCAL PENDING                                
         CLC   12(10,R3),=CL10'LOCAL'                                           
         BNE   STED13                                                           
*                                                                               
STED11   CLI   1(R3),0             LOOKING FOR SOON=12/15/91                    
         BE    STED15              NO DATE                                      
         GOTO1 DATVAL,DMCB,(0,22(R3)),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    XIT                 INVALID DATE                                 
         GOTO1 DATCON,DMCB,(0,WORK),(2,0(R5))                                   
         B     STED15                                                           
*                                                                               
STED13   CLC   12(10,R3),=CL10'STACK'  STACK NUMBER                             
         BNE   STED15                                                           
         CLI   1(R3),1             MUST ENTER STACK NUMBER                      
         BL    XIT                                                              
         CLI   1(R3),5             MAX IS 65535                                 
         BH    XIT                                                              
         SR    R1,R1                                                            
         ICM   R1,15,8(R3)         STACK NUMBER                                 
         BZ    XIT                 NOT NUMERIC                                  
         C     R1,=F'65534'                                                     
         BH    XIT                                                              
         STCM  R1,3,OCNSEQN                                                     
*                                                                               
STED15   CLC   12(3,R3),=C'FTP'    USING FTP OPTION                             
         BNE   STED90                                                           
         CLC   ALPHAID,=C'CC'      SKIP EDICT TEST FOR COKE                     
         BE    STED90                                                           
         BAS   RE,EDICT                                                         
         CLI   ERROR,X'FF'                                                      
         BNE   XIT                                                              
*                                                                               
STED90   LA    R3,32(R3)                                                        
         SR    R1,R1                                                            
         IC    R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         BCT   R0,STED03                                                        
         MVI   FNDX,0                                                           
         MVI   ERROR,X'FF'                                                      
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*              CHECK FOR EDICT RECORD IF USING FTP                   *          
**********************************************************************          
*                                                                               
EDICT    NTR1                                                                   
         MVI   CTBYTE,0            READ FOR BINARY ID NUMBER                    
         BAS   RE,GETID            GET ID RECORD                                
*                                                                               
         MVI   ERROR,X'FF'                                                      
         USING EDIKEYD,R5                                                       
         LA    R5,CNTRLIO                                                       
         XC    EDIKEY,EDIKEY                                                    
         MVI   EDIKSYS,EDIKSYSQ    X'05' - KEY SYSTEM FOR ALL SYSTEMS           
         MVI   EDITYPE,EDITYPEQ    X'07' - EDICT TYPE                           
         MVC   EDINAME,WORK                                                     
         OC    EDINAME,SPACES                                                   
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',(R5),(R5)                     
         CLI   DMCB+8,0                                                         
         BE    EXIT                                                             
         MVI   ERROR,X'FE'                                                      
         MVC   LOGHEAD,=CL60'EA#9999 EDICT RECORD NOT SET UP'                   
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
* *******************************************************************           
* STUN- UNSCAN THE STATUS LINE                                                  
*       R6 = STATUS BIT                                                         
* *******************************************************************           
*                                                                               
STUN     NTR1  ,                                                                
         LR    R8,R1               SAVE ADDRESS                                 
STUN03   MVC   WORK(1),1(R8)       BIT FROM TABLE                               
         NC    WORK(1),0(R6)       STATUS BIT                                   
         BZ    STUN05              NOT ON                                       
         CLC   2(10,R8),=CL10'MICR'                                             
         BNE   STUN04                                                           
         CLI   OCNLASR,0                                                        
         BNE   STUN05                                                           
STUN04   LA    R5,1(R5)            COUNT OUTPUT ITEMS                           
         MVC   0(20,R3),SPACES     SPACES TO BLOCK                              
         MVC   0(10,R3),2(R8)      LEFT SIDE TO BLOCK                           
         CLI   0(R8),1             ONE SIDED                                    
         BE    *+10                YES                                          
         MVC   10(10,R3),12(R8)    RIGHT SIDE TO BLOCK                          
         LA    R3,20(R3)                                                        
*                                                                               
STUN05   CLI   0(R8),1                                                          
         BE    *+8                                                              
         LA    R8,10(R8)                                                        
         LA    R8,12(R8)                                                        
         CLI   0(R8),X'FF'         END OF TABLE                                 
         BNE   STUN03                                                           
*                                                                               
         XIT1  REGS=(R3,R5)                                                     
         EJECT                                                                  
* *******************************************************************           
* STUN- UNSCAN THE STATUS LINE                                                  
*       R6 = STATUS BIT                                                         
* *******************************************************************           
*                                                                               
STUNM    NTR1  ,                                                                
         LR    R8,R1               SAVE ADDRESS                                 
STUNM03  MVC   WORK(1),1(R8)       BIT FROM TABLE                               
         CLC   WORK(1),0(R6)       STATUS BIT                                   
         BNE   STUNM05             NOT ON                                       
         LA    R5,1(R5)            COUNT OUTPUT ITEMS                           
         MVC   0(20,R3),SPACES     SPACES TO BLOCK                              
         MVC   0(10,R3),2(R8)      LEFT SIDE TO BLOCK                           
         CLI   0(R8),1             ONE SIDED                                    
         BE    *+10                YES                                          
         MVC   10(10,R3),12(R8)    RIGHT SIDE TO BLOCK                          
         LA    R3,20(R3)                                                        
*                                                                               
STUNM05  CLI   0(R8),1                                                          
         BE    *+8                                                              
         LA    R8,10(R8)                                                        
         LA    R8,12(R8)                                                        
         CLI   0(R8),X'FF'         END OF TABLE                                 
         BNE   STUNM03                                                          
*                                                                               
         XIT1  REGS=(R3,R5)                                                     
         EJECT                                                                  
* *******************************************************************           
* PDTE- FIND SOON OR LOCAL ENTRY AND FORMAT THE DATE                            
* *******************************************************************           
*                                                                               
PDTE     NTR1  ,                                                                
         LR    R8,R1               GET ADDR OF PENDING DATE                     
         LR    R0,R5               NUMBER OF ENTRIES                            
         LA    R3,BLOCK                                                         
*                                                                               
PDTE3    CLC   0(4,R3),0(R6)       FIND ENTRY FOR SOON/LOCAL                    
         BE    PDTE5                                                            
         LA    R3,20(R3)                                                        
         BCT   R0,PDTE3                                                         
         B     XIT                                                              
*                                                                               
PDTE5    GOTO1 DATCON,DMCB,(2,0(R8)),(8,10(R3))                                 
*                                                                               
XIT      XIT1                                                                   
*                                                                               
* -------------------------------------------------------------------           
         GETEL R5,DATADISP,ELCODE                                               
* -------------------------------------------------------------------           
*                                                                               
         EJECT                                                                  
* *******************************************************************           
* STATLST-     STATUS BIT TABLES                                                
*              BYTE 1  01 = ONE SIDED ENTRY                                     
*                      02 = TWO SIDED ENTRY                                     
*              BYTE 2  BIT SETTING                                              
*              BYTE 3-12  LEFT SIDE OR ENTRY                                    
*              BYTE 13-22 RIGHT SIDE OF ENTRY (OPTIONAL)                        
* *******************************************************************           
         SPACE 1                                                                
STATLST  DC    X'01',AL1(OCNSPRTQ),CL10'DIRECT'                                 
         DC    X'01',AL1(OCNSOFFR),CL10'OFFICE'                                 
         DC    X'01',AL1(OCNSPRTN),CL10'NUMBER'                                 
         DC    X'01',AL1(OCNSDREG),CL10'DIREG'                                  
         DC    X'01',AL1(OCNSAUTH),CL10'AUTH'                                   
         DC    X'01',AL1(OCNSSHUT),CL10'SHUTTLE'                                
         DC    X'01',AL1(OCNSSOON),CL10'SOON'                                   
         DC    X'01',AL1(OCNSLOCL),CL10'LOCAL'                                  
         DC    X'01',AL1(0),CL10'STACK'                                         
         DC    X'FF'                                                            
*                                                                               
STATLST2 DC    X'01',AL1(OCNSMICR),CL10'MICR'                                   
         DC    X'01',AL1(OCNSFTP),CL10'FTP'                                     
         DC    X'01',AL1(OCNSLBLT),CL10'LASER'   BOTTOM LINE TECHNOLGY          
         DC    X'FF'                                                            
*                                                                               
STATLASR DC    X'01',AL1(OCNRED),CL10'LASERRED'                                 
         DC    X'01',AL1(OCNBLU),CL10'LASERBLU'                                 
         DC    X'01',AL1(OCNGRN),CL10'LASERGRE'                                 
         DC    X'01',AL1(OCNGLD),CL10'LASERGLD'                                 
         DC    X'01',AL1(OCNBRN),CL10'LASERBRN'                                 
         DC    X'01',AL1(OCNVIO),CL10'LASERVIO'                                 
         DC    X'FF'                                                            
*                                                                               
SORTS    DS    0CL9                                                             
         DC    C'A',CL8'AMOUNT'                                                 
         DC    C'C',CL8'CODE'                                                   
         DC    C'D',CL8'DISCOUNT'                                               
         DC    C'N',CL8'NAME'                                                   
         DC    X'FF'                                                            
*                                                                               
MXIDS    EQU   6                   ID'S PER SCREEN                              
*MXSCN    EQU   2                   MAX NUMBER OF SCREENS - LESS ONE            
MXSCN    EQU   3                   MAX NUMBER OF SCREENS - LESS ONE             
         EJECT                                                                  
**********************************************************************          
* LOCAL STORAGE                                                      *          
**********************************************************************          
         SPACE 1                                                                
CHKIO    DS    CL2000                                                           
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR SCREEN LINE ENTRY                                      
OFD      DSECT                                                                  
OFIDH    DS    XL8                 HEADER FOR ID FIELD                          
OFID     DS    CL(L'LOGID)         ID FIELD                                     
OFNOH    DS    XL8                 HEADER FOR NUMBER FIELD                      
OFNO     DS    CL(L'LOGNO)         NUMBER FIELD                                 
OFBNKH   DS    XL8                 HEADER FOR BANK FIELD                        
OFBNK    DS    CL(L'LOGBANK)       BANK FIELD                                   
OFFLTH   DS    XL8                 HEADER FOR FILTER FIELD                      
OFFLT    DS    CL(L'LOGFILT)       FILTER FIELD                                 
OFLSTH   DS    XL8                 HEADER FOR LAST CHECK FIELD                  
OFLST    DS    CL(L'LOGLCHK)       LAST CHECK FIELD                             
OFSORTH  DS    XL8                 HEADER FOR SORT FIELD                        
OFSORT   DS    CL(L'LOGSORT)       SORT FIELD                                   
OFPOFFH  DS    XL8                 HEADER FOR OFFICE POSTING                    
OFPOFF   DS    CL(L'LOGPOFF)       POSTING OFFICE                               
         DS    XL8                 HEADER FOR WORD 'STATUS'                     
         DS    XL6                 STATUS                                       
OFSTAH   DS    XL8                 HEADER FOR STATUS FIELD                      
OFSTA    DS    CL(L'LOGSTAT)       STATUS FIELD                                 
OFLNQ    EQU   *-OFD                                                            
         EJECT                                                                  
*              DSECT FOR LOCAL WORKING STORAGE                                  
*                                                                               
LWD      DSECT                                                                  
RELO     DS    F                                                                
ADDLIST  DS    F                                                                
ADDSTAT  DS    F                                                                
CHKFLG   DS    C              TELLS IF A CHK REC WAS FOUND:'Y','N','D'          
ELCODE   DS    X                                                                
ALPHAID  DS    CL2                 ALPHA ID                                     
CTSAVE   DS    CL25                                                             
CTBYTE   DS    CL1                                                              
CHKEY    DS    CL42                                                             
LDGERKEY DS    CL42                                                             
CHIO     DS    CL2000                                                           
CNTRLIO  DS    CL2000                                                           
LWDX     EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         EJECT                                                                  
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFME8D                                                       
         EJECT                                                                  
         ORG   T603FFD                                                          
       ++INCLUDE FATWA                                                          
         ORG   TWAUSER                                                          
LASTSEQ  DS    CL1                 PHASE/SEQ NUMBER                             
TWAXTRA  DS    CL31                FOR THE USER                                 
         ORG   TWAXTRA                                                          
SCRN     DS    XL1                 CURRENT SCREEN NUMBER                        
         DS    XL30                                                             
         EJECT                                                                  
*        ACLFMWORK                                                              
*        ACGENBOTH                                                              
*        ACGENFILE                                                              
*        CTGENFILE                                                              
*        ACLFMEQU                                                               
*        DDLFDIND                                                               
         PRINT OFF                                                              
       ++INCLUDE ACLFMWORK                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE ACLFMEQU                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE CTGENEDICT                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014ACLFM17S  05/01/02'                                      
         END                                                                    
