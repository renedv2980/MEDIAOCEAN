*          DATA SET REREPEI02  AT LEVEL 062 AS OF 05/31/96                      
*PHASE REEI02A,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE RECUP                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE BINSRCH                                                                
*INCLUDE DMPRTQB                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'REREPEI02 (REEI02) --- KATZ BULK EDI LOADER'                    
*                                                                               
*------------------------------------------------------------------*            
*                                                                  *            
*        REREPED02  -- KATZ BULK EDI LOADER: ACCEPT A TAPE FILE,   *            
*                      CONVERT DATA TO DARE '51' RECORDS.          *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* FEB12/96 ABBEY --- ORIGINAL ENTRY                                *            
*                                                                  *            
*                                                                  *            
*------------------------------------------------------------------*            
*                                                                  *            
*        QOPTION1= Y -> DON'T CHECK CONTRACTS                                   
*        QOPTION2 =Y -> DISPLAY DUPLICATE KEYS ON ADDS FOR 51 RECS *            
*        QOPTION3 =U -> UPDATE THE RECS                            *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
REEI02   CSECT                                                                  
         NMOD1 0,**REEI**,R9,RR=R5                                              
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R5,RELO                                                          
*                                                                               
         GOTO1 =V(STXITER),DMCB,DUMPLIST                                        
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   MAINEXIT                                                         
         GOTO1 INITIAL,DMCB,(RC)                                                
*                                                                               
         L     R4,ARECAREA         SET A(TAPE RECD DELIVERY AREA)               
         USING DKHEADR1,R4                                                      
MAIN     EQU   *                                                                
         GET   INTAPE,(R4)         READ TAPE RECORD INTO RDA                    
         CLI   DKHDTYP,DKHDTYPQ    HEADER ONE RECORD                            
         BE    MAIN10                                                           
         CLI   DKHDTYP,DKH2TYPQ    HEADER TWO RECORD                            
         BE    MAIN100                                                          
         BNE   MAIN450             GET NEXT RECORD                              
*                                  EOF --> MAIN500                              
MAIN10   DS    0H                                                               
         L     R1,TOTHDR                                                        
         LA    R1,1(R1)                                                         
         ST    R1,TOTHDR                                                        
*                                                                               
         CLC   DKHDEND1,=C'960101' IS RECORD THIS YEAR?                         
         BNL   MAIN13                                                           
         L     R1,TDATLOW                                                       
         LA    R1,1(R1)                                                         
         ST    R1,TDATLOW                                                       
         GOTO1 RESPACE,DMCB,(RC)   CLEAR WORKSPACE                              
MAIN12   GET   INTAPE,(R4)         READ TAPE RECORD INTO RDA                    
         CLI   DKHDTYP,DKHDTYPQ    HEADER ONE RECORD                            
         BNE   MAIN12              READ UNTIL WE HIT NEXT HEADER ONE            
         B     MAIN10                                                           
*                                                                               
MAIN13   EQU   *                                                                
*                                                                               
*   TEST                                                                        
*        MVC   P+1(13),=C'INPUT RECORD1'                                        
*        GOTO1 REPORT                                                           
*        L     R5,ARECAREA                                                      
*        GOTO1 =V(PRNTBL),DMCB,(0,(R5)),(R5),C'DUMP',529,=C'1D'                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         LA    R5,IOAREA                                                        
         USING RDARREC,R5                                                       
         MVI   RDARKTYP,X'51'       HEADER REC/CONFIRMED DARE ORDER             
*                                                                               
         MVC   RDARKSTA(L'DKHDSTAT),DKHDSTAT                                    
         CLI   RDARKSTA+4,C' '     STATION                                      
         BNE   *+8                                                              
         MVI   RDARKSTA+4,C'T'     FORCE TV IF SPACE                            
         MVI   RDARKSTA+5,C' '     CLEAR LAST CHAR OF STATION FIELD             
*                                                                               
MAIN15   MVC   RDARKAOF,DKHDOFNM   INSERT ROUTING AGENCY OFFICE                 
*                                                                               
         GOTO1 =V(HEXIN),DMCB,DKHDREF+2,RDARKORD,8,0                            
*                                  CONVERT ORDER NUMBER TO HEXIN                
         MVI   RDARKRT,X'10'       AGENCY HEADER                                
*                                                                               
         MVI   RDARELEM,X'01'      AGENCY HEADER ELEMENT                        
         LA    RF,RDARAGLN                                                      
         STC   RF,RDARELLN         LENGTH OF AGENCY HEADER ELEM                 
         MVI   RDARCLEM,X'02'      DESCRIPTIVE ELEMENT                          
         LA    RF,RDARCLLN         2ND ELEM LENGTH                              
         STC   RF,RDARCLEN                                                      
*                                                                               
         MVI   RDARVER#,C'0'                                                    
*                                                                               
MAIN20   MVC   RDARSNDR+5(5),DKHDOFNM     SENDER ID                             
         GOTO1 DATCON,DMCB,(0,DKHDDATE),(2,RDARDATE)    COMPRESSED              
*                                                                               
         PACK  DUB(8),DKHDTIME(4)  CONVERT AND STORE TIME                       
         CVB   RF,DUB                                                           
         STCM  RF,3,RDARTIME       2 CHARS OUTPUT                               
*                                                                               
         LA    RF,DKHDESNO+5       A (LAST POSITION OF FIELD)                   
         LA    R0,6                                                             
*                                                                               
* CALCULATE LENGTH OF INPUT FIELD - COUNT TRAILING SPACES                       
*                                                                               
MAIN25   DS    0H                                                               
         CLI   0(RF),X'40'         FIELD = SPACE?                               
         BNE   MAIN35              NO - FINISHED                                
         BCTR  RF,0                BACK UP ONE POSITION                         
         BCT   R0,MAIN25           YES - GO BACK FOR NEXT                       
         DC    H'0'                NO ESTIMATE NUMBER - !!                      
*                                                                               
MAIN35   DS    0H                                                               
         LR    RF,R0               LOAD ANOTHER REGISTER                        
         BCTR  RF,0                SUBTRACT ONE FOR EX                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),DKHDESNO(0)  PACK BY LENGTH                               
         CVB   RF,DUB                                                           
         STCM  RF,7,RDAREST#       3 CHARS OUTPUT                               
*                                                                               
         GOTO1 DATCON,DMCB,(0,DKHDST1),(2,RDARESST)   FLIGHT START              
*                                                                               
         MVC   DATE,DKHDEND3                                                    
         CLC   DKHDEND3,SPACES     PUT LAST EXISTING END DATE                   
         BH    MAIN50              DO A HIATUS                                  
         MVC   DATE,DKHDEND2                                                    
         CLC   DKHDEND2,SPACES                                                  
         BH    MAIN50              DO A HIATUS                                  
         MVC   DATE,DKHDEND1                                                    
         B     MAIN60              NO NEED TO DO ANY HIATUSES                   
*                                                                               
MAIN50   DS    0H                  DO ALL AGENCY HIATUS WEEK LINES              
*                                                                               
MAIN60   DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,DATE),(2,RDARESEN)      ESTIMATE END              
*                                                                               
         XC    RDARRTS,RDARRTS     RETURN TO SENDER INFO                        
         MVI   RDARCORT,C'C'       CASH                                         
*                                                                               
         MVC   RDARCLNM+24(L'DKHDESNO),DKHDESNO                                 
         MVI   RDARCLNM+33,C'S'                                                 
         MVC   RDARPRN1(L'DKHDPRNM),DKHDPRNM                                    
         MVC   RDARTDEM(L'DKHDDEMO),DKHDDEMO                                    
*                                                                               
         MVC   RDARAGAD(5),=C'$EDI$'                                            
         MVC   RDARAGAD+5(L'DKHDREF),DKHDREF                                    
         MVC   RDARAGAD+30(L'DKHDMKT),DKHDMKT                                   
         MVC   RDARAGAD+32(L'DKHDPOB),DKHDPOB                                   
*                                                                               
         XC    RDARBUYC,RDARBUYC                                                
         MVC   RDARBUYR,DKHDBUYR                                                
*                                                                               
         MVC   RDARBTEL,DKHDBUFN   PHONE NUMBER                                 
         CLI   RDARBTEL+3,C'-'                                                  
         BE    *+12                                                             
         CLI   RDARBTEL+3,C' '                                                  
         BNE   MAIN75                                                           
         MVC   RDARBTEL+3(3),DKHDBUFN+4                                         
         MVC   RDARBTEL+6(4),DKHDBUFN+8                                         
         B     *+10                                                             
MAIN75   MVC   RDARBXTN(4),DKHDBUFN+10                                          
*                                                                               
         MVC   MKTNAM,DKHDMKNM     SAVE MARKET NAME                             
*                                                                               
         B     MAIN450                                                          
*                                                                               
MAIN100  DS    0H                                                               
*                                                                               
*   TEST                                                                        
*        MVC   P+1(13),=C'INPUT RECORD2'                                        
*        GOTO1 REPORT                                                           
*        L     R5,ARECAREA                                                      
*        GOTO1 =V(PRNTBL),DMCB,(0,(R5)),(R5),C'DUMP',529,=C'1D'                 
*        GOTO1 REPORT                                                           
*        LA    R5,IOAREA           RESET A(IOAREA)                              
*   TEST END                                                                    
*                                                                               
         USING DKHEADR2,R4                                                      
*                                                                               
         MVC   RDARSNDR(5),=C'LBED1'                                            
         MVC   RDARKAGY,=C'ED1'    INSERT ROUTING AGENCY                        
         CLC   =C'LB',DKH2AGCD     LEO BURNETT                                  
         BE    MAIN105                                                          
         MVC   RDARSNDR(5),=C'DMED2'                                            
         MVC   RDARKAGY,=C'ED2'                                                 
         CLC   =C'DMB',DKH2AGCD  DARCY                                          
         BE    MAIN105                                                          
         CLC   =C'HEXA',DKH2AGCD                                                
         BE    MAIN105                                                          
         CLC   =C'ARCY',DKH2AGCD+2                                              
         BE    MAIN105                                                          
         MVC   RDARSNDR(5),=C'MCED3'                                            
         MVC   RDARKAGY,=C'ED3'                                                 
         CLC   =C'ME',DKH2AGCD     MCANN ERIKSON                                
         BE    MAIN105                                                          
         MVC   RDARSNDR(5),=C'FCED4'                                            
         MVC   RDARKAGY,=C'ED4'                                                 
         CLC   =C'FCB',DKH2AGCD    FOOT CONE                                    
         BE    MAIN105                                                          
         BAS   RE,WRNGAGY                                                       
*---------------------------------------------------------------------*         
* WRNGAGY                                                                       
*---------------------------------------------------------------------*         
WRNGAGY  NTR1                                                                   
         MVC   P+1(20),=C'INVALID AGENCY CODE:'                                 
         MVC   P+25(L'DKH2AGCD),DKH2AGCD                                        
         GOTO1 REPORT                                                           
         DC    H'0'                                                             
         XIT1                                                                   
*                                                                               
MAIN105  GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,IOAREA,KEYSAVE,0                      
*                                                                               
         CLC   IOAREA(27),KEYSAVE  SAME RECORD ALREADY EXISTS?                  
         BNE   *+12                                                             
         BAS   RE,PRTDUP           ROUTINE - PRINTS DUPLICATE KEY               
         B     MAIN450                                                          
*                                                                               
         LA    RF,RDARAGLN+RDARCLLN+RDARCFLQ+RDAREDLQ+34                        
         STCM  RF,3,RDARLEN        RECORD LENGTH                                
*                                                                               
*   TEST                                                                        
         MVC   P+1(15),=C'ORDER DIVISION:'                                      
         MVC   P+17(1),DKH2DVCD                                                 
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         CLI   DKH2DVCD,C'S'       SELTEL?                                      
         BNE   MAIN0110            NO                                           
         MVC   RDARKREP,=C'SZ'     SELTEL                                       
         MVC   RDARRCVR(6),=C'SELTEL'     RECEIVER ID                           
         B     MAIN0150                                                         
MAIN0110 EQU   *                                                                
         CLI   DKH2DVCD,C'A'       AMERICAN?                                    
         BNE   MAIN0115            NO                                           
         MVC   RDARKREP,=C'AM'     AMERICAN                                     
         MVC   RDARRCVR(6),=C'AMERIC'     RECEIVER ID                           
         B     MAIN0150                                                         
MAIN0115 EQU   *                                                                
         CLI   DKH2DVCD,C'C'       CONTINENTAL?                                 
         BNE   MAIN0120            NO                                           
         MVC   RDARKREP,=C'CQ'     CONTINENTAL                                  
         MVC   RDARRCVR(6),=C'CONTIN'     RECEIVER ID                           
         B     MAIN0150                                                         
MAIN0120 EQU   *                                                                
         CLI   DKH2DVCD,C'I'       NATIONAL?                                    
         BNE   MAIN0125            NO                                           
         MVC   RDARKREP,=C'NK'     NATIONAL                                     
         MVC   RDARRCVR(6),=C'NATION'     RECEIVER ID                           
         B     MAIN0150                                                         
MAIN0125 EQU   *                                                                
         DC    H'0'                                                             
MAIN0150 EQU   *                                                                
         MVC   SAVEKREP,RDARKREP   SAVE THE REP CODE                            
         OC    DKH2CON,DKH2CON                                                  
         BNZ   *+12                                                             
         BAS   RE,NOCONT           SUM NUMBER SENT WITHOUT CONTRACT #           
         B     MAIN450                                                          
         CLC   DKH2CON,ZEROES                                                   
         BNE   *+12                                                             
         BAS   RE,NOCONT                                                        
         B     MAIN450                                                          
*                                  INSERT REP CONTRACT NUMBER                   
         CLI   QOPTION1,C'Y'       SKIP CONTRACT CHECK?                         
         BE    MAIN0170                                                         
         BAS   RE,CKCONT                                                        
         BNE   MAIN450                                                          
MAIN0170 EQU   *                                                                
*                                                                               
         MVC   RDARMEDI,DKH2MED    MEDIA                                        
         OC    DKH2CON,SPACES      SET SPACES IF BINARY                         
         MVC   WORK+1(L'DKH2CON),DKH2CON                                        
         MVI   WORK,X'F0'                                                       
         GOTO1 =V(HEXIN),DMCB,WORK,RDARREP#,8                                   
*                                  INSERT REP CONTRACT NUMBER                   
         MVC   RDARCLI,DKH2ADCD    CLIENT CODE                                  
         MVC   RDARPRD1,DKH2PRCD   1ST PRODUCT CODE                             
         OC    DKH2PR2,SPACES      SET SPACES IF BINARY                         
         MVC   RDARPRD2,DKH2PR2    2ND PRODUCT CODE                             
         MVC   RDARESDS,DKH2EST                                                 
*                                  INSERT REP CONTRACT NUMBER                   
         MVC   RDARPRN2(L'DKH2AGCD),DKH2AGCD                                    
         MVC   RDARPRN2+8(L'DKH2ADCD),DKH2ADCD                                  
         MVC   RDARPRN2+16(L'DKH2PRCD),DKH2PRCD                                 
         MVC   RDARPRN2+32(L'DKH2TIND),DKH2TIND                                 
         MVC   RDARPRN2+33(L'DKH2DVCD),DKH2DVCD                                 
*                                                                               
         MVC   RDARAGNM(L'DKH2CNAD),DKH2CNAD                                    
         MVC   RDARAGNM+15(L'DKH2INV),DKH2INV                                   
         MVC   RDARAGNM+30(10),DKH2BFNM                                         
*                                                                               
         MVC   RDARAGAD+35(L'DKH2BUOF),DKH2BUOF                                 
*                                                                               
         LA    R5,RDARRCCD                                                      
         DROP  R5                                                               
*                                                                               
         USING RDAREDEM,R5                                                      
         MVI   RDAREDCD,X'ED'      EDI ELEM                                     
         MVI   RDAREDLN,RDAREDLQ                                                
         MVC   RDAREDDT+1(L'DKH2BFNM),DKH2BFNM     BUYING OFFICE NAME           
         MVC   RDAREDDT+21(L'MKTNAM),MKTNAM        MARKET NAME                  
         MVC   RDAREDDT+51(L'DKH2FLNO),DKH2FLNO    FLIGHT NUMBER                
         DROP  R5                                                               
*                                                                               
         LA    R1,RDAREDLQ                                                      
         AR    R5,R1               BUMP R5                                      
*                                                                               
         USING RDARCFEM,R5         DATE TIME OF CREATION ELEM                   
         MVI   RDARCFCD,X'F1'                                                   
         MVI   RDARCFLN,RDARCFLQ                                                
         GOTO1 DATCON,DMCB,(5,DATE),(2,RDARCFDT)                                
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,12               SHIFT OFF SECONDS AND SIGN                   
         STCM  R1,3,RDARCFTM                                                    
*                                                                               
         DROP  R4,R5                                                            
         LA    R5,IOAREA                                                        
         CLC   TOTPRT,=F'20'                                                    
         BH    MAIN200                                                          
         L     R2,TOTPRT                                                        
         LA    R2,1(R2)                                                         
         ST    R2,TOTPRT                                                        
         GOTO1 =V(PRNTBL),DMCB,(0,(R5)),(R5),C'DUMP',500,=C'1D'                 
         GOTO1 REPORT                                                           
*                                                                               
MAIN200  DS    0H                                                               
*        MVC   P+1(8),=C'OPTION3:'                                              
*        MVC   P+10(L'QRECORD),QRECORD                                          
*        GOTO1 REPORT                                                           
         CLI   QOPTION3,C'U'      UPDATE RECS?                                  
         BNE   *+8                                                              
         BAS   RE,RECWRITE                                                      
*                                                                               
MAIN450  GOTO1 RESPACE,DMCB,(RC)   CLEAR WORKSPACE                              
         B     MAIN                GO BACK FOR NEXT RECORD                      
*                                                                               
MAIN500  DS    0H                                                               
         CLOSE (INTAPE,REWIND)                                                  
         BAS   RE,PRTTOT                                                        
*                                                                               
MAINEXIT XIT1                                                                   
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
         B     MAINEXIT                                                         
         EJECT                                                                  
*---------------------------------------------------------------------*         
* CKCONT  - MAKE SURE THAT THE CONTRACT EXISTS AND ADD THE 1D ELEM    *         
*---------------------------------------------------------------------*         
CKCONT   NTR1                                                                   
*                                                                               
         USING DKHEADR2,R4                                                      
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING RCONREC,R5                                                       
*                                                                               
         MVI   RCONPTYP,X'8C'      PASSIVE POINTER 1                            
         MVC   RCONPREP,SAVEKREP   INSERT SAVED REP CODE                        
         PACK  DUB+3(5),DKH2CON(7)                                              
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB+3(5)    9'S COMPLEMENT                            
         MVO   WORK(5),WORK+10(5)                                               
         MVC   RCONPCON,WORK                                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEYSAVE,0                         
         CLC   KEY(27),KEYSAVE                                                  
         BE    CKCONADD                                                         
         L     R1,TCNNTFD          CONTRACT NOT FOUND TOTAL                     
         LA    R1,1(R1)                                                         
         ST    R1,TCNNTFD                                                       
*                                                                               
         MVC   P+1(23),=C'CONTRACT DOESN''T EXIST:'                             
         GOTO1 REPORT                                                           
         GOTO1 =V(PRNTBL),DMCB,(0,(R5)),(R5),C'DUMP',27,=C'1D'                  
         GOTO1 REPORT                                                           
         B     NO                                                               
*                                                                               
CKCONADD DS    0H                  ADD 1D ELEM                                  
*                                                                               
         DROP  R5                                                               
*                                                                               
CKADD05  DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(X'80',GETREC),=C'REPFILE',KEYSAVE+28,     X        
               REC,(0,IOWORK)                                                   
         TM    DMCB+8,X'10'        REC NOT FOUN                                 
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING RCONDREL,R5                                                      
         MVI   RCONDRCD,X'1D'      DARE AGENCY ORDER ELEM                       
         MVI   RCONDRLN,RCONDRLQ                                                
         MVI   RCONDRFG,X'02'      KATZ EDI 1 SHOT CONVERSION                   
*                                                                               
         MVC   WORK(8),DKH2REF+2                                                
         OC    WORK(8),ZEROES      DEAL WITH LETTERS                            
         PACK  DUB+3(5),DKH2REF+2(8)                                            
         XC    WORK,WORK                                                        
         MVO   WORK+10(5),DUB+3                                                 
         MVC   WORK(5),WORK+10                                                  
         MVC   RCONDRLK,WORK       PWOS AGY ORDER NUMBER                        
*                                                                               
         LA    R6,REC+34                                                        
CKADD10  ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BZ    CKADD20                                                          
         AR    R6,R0                                                            
         CLI   0(R6),X'1D'                                                      
         BL    CKADD10                                                          
*                                                                               
CKADD20  DS    0H                                                               
         LA    R2,REC                                                           
         GOTO1 =V(RECUP),DMCB,(C'R',(R2)),(R5),(R6)                             
         L     R1,TCONUPD                                                       
         LA    R1,1(R1)                                                         
         ST    R1,TCONUPD                                                       
         CLI   QOPTION3,C'U'      UPDATE RECS?                                  
         BNE   YES                                                              
         GOTO1 DATAMGR,DMCB,(X'80',PUTREC),=C'REPFILE',KEY+28          X        
               REC,IOWORK                                                       
*                                                                               
         B     YES                                                              
*---------------------------------------------------------------------*         
* NOCONT# - THE CONTRACT # THEY SENT WAS ALL ZEROES OR EMPTY          *         
*---------------------------------------------------------------------*         
NOCONT   NTR1                                                                   
         L     R1,TOTNOCN#                                                      
         LA    R1,1(R1)                                                         
         ST    R1,TOTNOCN#                                                      
         MVC   P+1(26),=C'CONTRACT SENT WITHOUT CON#'                           
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     MAINEXIT                                                         
*---------------------------------------------------------------------*         
*   RECWRITE:  CHECKS FOR KEYS, DELETED RECORDS, ETC....              *         
*---------------------------------------------------------------------*         
RECWRITE NTR1                                                                   
         MVC   KEY,IOAREA          SET KEY FROM NEW RECORD                      
         GOTO1 DATAMGR,DMCB,(X'88',ADDREC),=C'REPFILE',KEY,            X        
               IOAREA,IOWORK                                                    
*                                  ATTEMPT TO ADD NEW RECORD                    
         CLI   DMCB+8,0            ERROR RETURN?                                
         BE    *+6                 NO  - SUCCESSFUL                             
         DC    H'0'                NO  - CAN'T PROCESS: DUMP                    
         L     R1,TADDS                                                         
         LA    R1,1(R1)                                                         
         ST    R1,TADDS                                                         
*                                                                               
RECW0400 DS    0H                                                               
         B     MAINEXIT                                                         
         EJECT                                                                  
*----------------------------------------------------------------*              
* IF QOPTION2=Y, PRINT KEY OF DUPLICATE RECORD WE TRIED TO ADD  *               
*----------------------------------------------------------------*              
PRTDUP   NTR1                                                                   
         CLI   QOPTION2,C'N'                                                    
         BE    PDX                                                              
         MVC   P+1(8),=C'DUP KEY:'                                              
         MVC   P+10(32),IOAREA                                                  
         GOTO1 REPORT                                                           
PDX      B     MAINEXIT                                                         
*----------------------------------------------------------------*              
* PRTTOT                                                                        
*----------------------------------------------------------------*              
PRTTOT   NTR1                                                                   
         MVC   P+1(19),=C'TOTAL HEADERS READ:'                                  
         EDIT  (4,TOTHDR),(8,P+40)                                              
         GOTO1 REPORT                                                           
         MVC   P+1(32),=C'HEADERS SENT WITH NO CONTRACT #:'                     
         EDIT  (4,TOTNOCN#),(8,P+40)                                            
         GOTO1 REPORT                                                           
         MVC   P+1(20),=C'CONTRACTS NOT FOUND:'                                 
         EDIT  (4,TCNNTFD),(8,P+40)                                             
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'CONTRACTS UPDATED WITH 1D ELEM'                       
         EDIT  (4,TCONUPD),(8,P+40)                                             
         GOTO1 REPORT                                                           
         MVC   P+1(17),=C'51 RECORDS ADDED:'                                    
         EDIT  (4,TADDS),(8,P+40)                                               
         GOTO1 REPORT                                                           
         MVC   P+1(17),=C'RECORDS < 960101:'                                    
         EDIT  (4,TDATLOW),(8,P+40)                                             
         GOTO1 REPORT                                                           
         B     MAINEXIT                                                         
*----------------------------------------------------------------*              
*  RESPACE:  RESET WORKSPACE                                     *              
*----------------------------------------------------------------*              
RESPACE  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R4,ARECAREA         SET A(INPUT RECORD)                          
         B     MAINEXIT                                                         
*                                                                               
         EJECT                                                                  
INITIAL  NTR1                                                                   
         OPEN  (INTAPE,(INPUT))                                                 
         SPACE 1                                                                
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     RF,COVAIL                                                        
         DROP  RF                                                               
         GOTO1 (RF),DMCB,C'GET',100000,100000                                   
*                                  GET 100K STORAGE SPACE                       
         OC    P2(4),P2                                                         
         BNZ   INIT0020                                                         
         DC    H'0'                                                             
INIT0020 EQU   *                                                                
         L     RF,P2               INITIALIZE THE WORK AREA                     
         XCEFL 0(RF),P3                                                         
         MVC   LBLDAREA,P3         L(ADD'L SPACE GIVEN)                         
         MVC   ARECAREA,P2         TAPE RECORD DELIVERY AREA                    
         L     RF,ARECAREA                                                      
         A     RF,=F'40000'        TAPE BUFFER AREA:                            
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
IOWORK   DS    12D                                                              
RELO     DS    F                   RELOCATION FACTOR                            
ARECAREA DS    A                   TAPE RECORD DELIVERY AREA                    
LBLDAREA DS    F                                                                
TOTNOCN# DS    F                                                                
TCNNTFD  DS    F                                                                
TOTHDR   DS    F                                                                
TDATLOW  DS    F                                                                
TOTPRT   DS    F                                                                
TADDS    DS    F                                                                
TCONUPD  DS    F                                                                
ZEROES   DC    C'0000000000000000000000000000'                                  
DATE     DS    CL6                                                              
SAVEKREP DS    CL2                                                              
MKTNAM   DS    CL30                                                             
ELEM     DS    CL256                                                            
         EJECT                                                                  
*                                                                               
ELCODE   DS    CL1                                                              
*                                                                               
IOAREA   DS    1024C                                                            
REC      DS    1024C                                                            
*                                                                               
INTAPE   DCB   DDNAME=INTAPE,DSORG=PS,RECFM=FB,LRECL=529,              X        
               BLKSIZE=10580,MACRF=GM,EODAD=MAIN500                             
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(REEI02,65000)                                                  
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*  INCLUDE REGENSTA                STATION     RECORD                           
*  INCLUDE DMPRTQL                                                              
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
RECD     DSECT                                                                  
RECORD   DS    CL1008                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENSTA          STATION     RECORD                           
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENMKT          MARKET      RECORD                           
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE DDREMOTED                                                      
         EJECT                                                                  
       ++INCLUDE REGENDAR                                                       
         EJECT                                                                  
       ++INCLUDE REGENCON                                                       
         EJECT                                                                  
       ++INCLUDE DRKZEDIIND                                                     
         EJECT                                                                  
         SPACE 4                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'062REREPEI02 05/31/96'                                      
         END                                                                    
