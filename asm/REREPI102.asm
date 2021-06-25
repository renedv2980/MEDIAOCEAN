*          DATA SET REREPI102  AT LEVEL 107 AS OF 05/01/02                      
*PHASE REI102A,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE BINSRCH                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'REREPI102A (REI102A) --- INTEREP COMPANY SPLIT'                 
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPI102A -- INTEREP COMPANY SEPARATOR                   *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* OCT31/95 (BU ) --- ORIGINAL ENTRY, BASED ON SWITCHER (REREPSW02) *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*  LIST OF DISPLAYS (REQUESTOR VALUE TRIGGER = VALUE IN BYTE:)     *            
*     QUESTOR+0   =                                                *            
*     QUESTOR+1   =                                                *            
*     QUESTOR+2   =                                                *            
*     QUESTOR+3   =                                                *            
*     QUESTOR+4   =                                                *            
*     QUESTOR+5   =                                                *            
*     QUESTOR+6   =                                                *            
*     QUESTOR+7   =                                                *            
*     QUESTOR+8   =                                                *            
*     QUESTOR+9   =                                                *            
*     QUESTOR+10  =                                                *            
*     QUESTOR+11  =                                                *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
REI102   CSECT                                                                  
         NMOD1 0,**REI2**,R9,RR=R5                                              
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BNE   MAINEXIT                                                         
         DS    0H                                                               
         GOTO1 INITIAL,DMCB,(RC)                                                
*                                                                               
         MVC   P+1(29),=C'BEGINNING CONTRACT SEPARATION'                        
         GOTO1 REPORT                                                           
*                                                                               
         LA    R2,RECTYPES         SET A(RECORD TYPE TABLE)                     
MAIN0040 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE REACHED?                        
         BE    MAIN1000            YES - DO BUY RECORDS                         
         GOTO1 RECPROC,DMCB,(R2)                                                
*                                  PROCESS THE RECORD TYPE                      
         LA    R2,LRECTYPE(R2)     BUMP TO NEXT ENTRY                           
         B     MAIN0040            GO BACK FOR NEXT                             
MAIN1000 EQU   *                                                                
         BAS   RE,BUYPROC          PROCESS THE BUY RECORDS                      
*                                     FOR ORDERS TRANSFERRED                    
         BAS   RE,DISPTOTS         DISPLAY RUN TOTALS                           
         CLOSE FILOUTA                                                          
         CLOSE FILOUTB                                                          
MAINEXIT EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         DS    0F                                                               
*                                                                               
*   RECTYPES:  TABLE OF RECORD TYPES TO BE COPIED TO NEW FILE                   
*        POS 0      =    RECORD TYPE CODE                                       
*        POS 1      =    DISPLACEMENT TO REP CODE IN RECORD                     
*        POS 2      =    'DELETE ORIGINAL' FLAG                                 
*        POS 3      =    DISPLAY/DON'T DISPLAY FLAG                             
*        POS 4-7    =    RECORD COUNTER FOR THIS TYPE                           
*        POS 8-11   =    RECORD NAME FOR DISPLAY                                
*        POS 12     =    DISPLACEMENT TO REP CODE IN OUTPUT RECORD              
*        POS 13-15  =    UNUSED AT THIS TIME                                    
*                                                                               
RECTYPES EQU   *                                                                
*              +0      +1      +2+3  +4   +8      +12     +13                   
*****    DC    XL1'01',AL1(25),C'NN',F'0',C'REP ',AL1(25),AL3(0)                
         DC    XL1'02',AL1(20),C'NN',F'0',C'STAT',AL1(20),AL3(0)                
         DC    XL1'03',AL1(23),C'NN',F'0',C'REGN',AL1(23),AL3(0)                
         DC    XL1'04',AL1(23),C'NN',F'0',C'OFF ',AL1(23),AL3(0)                
         DC    XL1'44',AL1(23),C'NN',F'0',C'OFF2',AL1(23),AL3(0)                
         DC    XL1'06',AL1(22),C'NN',F'0',C'S/P ',AL1(22),AL3(0)                
RECTCON  EQU   *                                                                
         DC    XL1'8C',AL1(21),C'YN',F'0',C'CON ',AL1(02),AL3(0)                
RECTEOM  EQU   *                                                                
         DC    XL1'18',AL1(24),C'NN',F'0',C'EOM ',AL1(24),AL3(0)                
         DC    XL1'24',AL1(24),C'NN',F'0',C'DYPT',AL1(24),AL3(0)                
         DC    XL1'26',AL1(20),C'NN',F'0',C'SDD ',AL1(20),AL3(0)                
         DC    XL1'29',AL1(11),C'NN',F'0',C'COM1',AL1(11),AL3(0)                
****>>>  DC    XL1'2C',AL1(04),C'NN',F'0',C'AUR ',AL1(04),AL3(0)                
         DC    XL1'2E',AL1(15),C'NN',F'0',C'COM2',AL1(15),AL3(0)                
         DC    XL1'32',AL1(24),C'NN',F'0',C'CTYP',AL1(24),AL3(0)                
         DC    XL1'33',AL1(17),C'NN',F'0',C'DPAN',AL1(17),AL3(0)                
         DC    XL1'34',AL1(20),C'NN',F'0',C'OFCM',AL1(20),AL3(0)                
         DC    XL1'36',AL1(17),C'NN',F'0',C'LABL',AL1(17),AL3(0)                
         DC    X'0000'                                                          
LRECTYPE EQU   RECTEOM-RECTCON                                                  
         EJECT                                                                  
*                                                                               
*   RECPROC:  CYCLE THROUGH RECORDS FOR THE KEY INDICATED.                      
*        WRITE THE NEW RECORDS TO THE FILE, DELETE THE OLD                      
*        CONTRACTS.                                                             
*                                                                               
RECPROC  NTR1                                                                   
         L     R2,0(R1)            RESET A(RECORD TYPE IN PROGRESS)             
         XC    KEY,KEY                                                          
         MVC   KEY(1),0(R2)        INSERT RECORD TYPE                           
         ZIC   RF,1(R2)            GET DISPLACEMENT                             
         LA    RE,KEY                                                           
         AR    RE,RF               DISPLACE TO REP CODE                         
         MVC   0(2,RE),=C'I8'      INSERT REP CODE                              
         GOTO1 HIGH                                                             
         B     RECP0040                                                         
RECP0020 EQU   *                                                                
         GOTO1 SEQ                                                              
RECP0040 EQU   *                                                                
         ZIC   RF,1(R2)            GET DISPLACEMENT AGAIN                       
         LA    RF,1(RF)            ADJUST FOR EX STATEMENT                      
*                                     ADD TO INCLUDE REP CODE                   
         EX    RF,RECP2000         COMPARE BY LENGTH                            
         BNE   RECP1000            KEYS NOT EQUAL THRU REP:                     
*                                     KEY TYPE FINISHED                         
         BAS   RE,GETRECRD         RETRIEVE RECORD                              
         L     RF,4(R2)            INCREMENT TYPE COUNTER                       
         LA    RF,1(RF)                                                         
         ST    RF,4(R2)            RESTORE                                      
         L     RF,GRANDTOT         INCREMENT TYPE COUNTER                       
         LA    RF,1(RF)                                                         
         ST    RF,GRANDTOT         RESTORE                                      
         CLI   RECORD,X'0C'        CONTRACT RECORD?                             
         BNE   RECP0200            NO                                           
         CLC   RCONHDRD,=X'5F0919'                                              
*                                  CREATION DATE VS 95/09/25                    
         BNL   RECP0100            9/25 OR LATER: LEAVE                         
         L     RF,CONXFER          INCREMENT TRANSFER COUNTER                   
         LA    RF,1(RF)                                                         
         ST    RF,CONXFER                                                       
         MVC   P+1(12),=C'TRANSFERRED:'                                         
         GOTO1 HEXOUT,DMCB,RCONKCON,P+15,4,=C'TOG'                              
         GOTO1 DATCON,DMCB,(3,RCONHDRD),(5,P+30)                                
         GOTO1 DATCON,DMCB,(3,RCONDATE),(5,P+40)                                
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(5,P+50)                              
         GOTO1 REPORT                                                           
         XC    WORK,WORK                                                        
         MVC   WORK+15(4),KEY+23   SET UP CONTRACT NUMBER FOR BUY               
*                                      REVERSE THE COMPLIMENT                   
         MVC   WORK+0(4),KEY+23                                                 
         PACK  WORK+0(1),WORK+18(1)                                             
         PACK  WORK+1(1),WORK+17(1)                                             
         PACK  WORK+2(1),WORK+16(1)                                             
         PACK  WORK+3(1),WORK+15(1)                                             
         L     RF,ANXTAREA         LOAD A(NEXT AVAILABLE SLOT)                  
         MVC   0(4,RF),WORK        INSERT CONTRACT INTO TABLE                   
         LA    RF,4(RF)                                                         
         ST    RF,ANXTAREA         SAVE NEXT AVAILABLE SLOT                     
****     GOTO1 HEXOUT,DMCB,RCONKCON,P+2,4,=C'TOG'                               
****     GOTO1 HEXOUT,DMCB,WORK,P+12,4,=C'TOG'                                  
****     GOTO1 REPORT                                                           
         B     RECP0200                                                         
RECP0100 EQU   *                                                                
         MVC   P+1(11),=C'RETAINED  :'                                          
         GOTO1 HEXOUT,DMCB,RCONKCON,P+15,4,=C'TOG'                              
         GOTO1 DATCON,DMCB,(3,RCONHDRD),(5,P+30)                                
         GOTO1 DATCON,DMCB,(3,RCONDATE),(5,P+40)                                
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(5,P+50)                              
         GOTO1 REPORT                                                           
         B     RECP0020            GO BACK FOR NEXT                             
RECP0200 EQU   *                                                                
         CLI   RECORD,X'0C'        CONTRACT RECORD?                             
         BNE   RECP0240            NO                                           
         CLC   CONXFER,=F'100'     ONLY PRINT FIRST 100 CONTRACTS               
         BH    RECP0280                                                         
RECP0240 EQU   *                                                                
         CLI   3(R2),C'Y'          DISPLAY THIS RECORD TYPE?                    
         BNE   RECP0280            NO  - SKIP IT                                
         GOTO1 REPORT                                                           
         MVC   P+1(12),=C'INPUT RECORD'                                         
         MVC   P+15(4),8(R2)       INSERT RECORD TYPE                           
         GOTO1 REPORT                                                           
         LA    R4,RECORD           SET RECORD ADDR                              
         ZICM  RF,RECORD+27,2      SET RECORD LENGTH                            
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
RECP0280 EQU   *                                                                
         ZIC   RF,12(R2)           GET INSERT DISPLACEMENT                      
         LA    RE,RECORD                                                        
         AR    RE,RF                                                            
         MVC   0(2,RE),=C'RM'      INSERT REPLACEMENT REP ID                    
         LA    RF,REC                                                           
         LA    R1,1000                                                          
         LA    RE,RECORD                                                        
         MOVE  ((RF),(R1)),(RE)                                                 
         MVC   REC-4(2),RECORD+27  INSERT LENGTH                                
         GOTO1 OUTRECS             ADD TO OUTPUT FILE                           
         CLI   2(R2),C'Y'          DELETE THIS RECORD TYPE?                     
         BNE   RECP0420            NO                                           
         MVC   RECORD+2(2),=C'I8'  RE-INSERT ORIGINAL REP ID                    
         OI    RECORD+29,X'01'     TURN ON 'CLOSED' BIT                         
         L     RF,REWRCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,REWRCTR                                                       
         CLC   CONXFER,=F'100'     ONLY PRINT FIRST 100 CONTRACTS               
         BH    RECP0360                                                         
*        MVC   P+1(12),=C'CON REWRITE:'                                         
*        MVC   P+15(34),RECORD                                                  
*        GOTO1 REPORT                                                           
RECP0360 EQU   *                                                                
         CLI   QOPTION1,C'U'       UPDATE RUN?                                  
         BNE   RECP0420            NO                                           
         BAS   RE,PUTRECRD         REWRITE THE RECORD 'CLOSED'                  
RECP0420 EQU   *                                                                
         CLI   RECORD,X'0C'        CONTRACT RECORD?                             
         BNE   RECP0440            NO                                           
         CLC   CONXFER,=F'100'     ONLY PRINT FIRST 100 CONTRACTS               
         BH    RECP0480                                                         
RECP0440 EQU   *                                                                
         CLI   3(R2),C'Y'          DISPLAY THIS RECORD TYPE?                    
         BNE   RECP0480            NO  - SKIP IT                                
         BAS   RE,DISPPUT          DISPLAY PUTRECORD                            
RECP0480 EQU   *                                                                
         B     RECP0020            GO BACK FOR NEXT                             
RECP1000 EQU   *                                                                
         XIT1                                                                   
RECP2000 CLC   KEY(0),KEYSAVE      KEY FOUND? THRU REP                          
         EJECT                                                                  
*                                                                               
*  BUYPROC:  ACCESS THE CONTRACT NUMBER TABLE, RETURN AND PROCESS               
*        ALL BUYS FOR CONTRACTS INVOLVED.                                       
*                                                                               
BUYPROC  NTR1                                                                   
         L     R2,ACONAREA                                                      
BUYP0010 EQU   *                                                                
         OC    0(4,R2),0(R2)       END OF TABLE?                                
         BZ    BUYP2000            YES - FINISHED                               
         L     RF,BUYCONTS         INCREMENT BUY CONTRACTS                      
         LA    RF,1(RF)                                                         
         ST    RF,BUYCONTS                                                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'0B'           INSERT RECORD TYPE                           
         MVC   KEY+16(2),=C'I8'    INSERT REP CODE                              
         MVC   KEY+18(4),0(R2)     INSERT CONTRACT NUMBER AS                    
*                                     9'S COMP, REVERSED                        
         GOTO1 HIGH                                                             
         B     BUYP0040                                                         
BUYP0020 EQU   *                                                                
         GOTO1 SEQ                                                              
BUYP0040 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME KEY THROUGH CONTRACT NUMBER?            
         BE    BUYP0080            YES - PROCESS IT                             
         LA    R2,4(R2)            NO  - BUMP TO NEXT SLOT                      
*                                     GO BACK FOR NEXT CONTRACT                 
         B     BUYP0010                                                         
BUYP0080 EQU   *                                                                
         BAS   RE,GETRECRD         RETRIEVE RECORD                              
         L     RF,GRANDTOT         INCREMENT TYPE COUNTER                       
         LA    RF,1(RF)                                                         
         ST    RF,GRANDTOT         RESTORE                                      
         L     RF,BUYXFER          INCREMENT BUY COUNTER                        
         LA    RF,1(RF)                                                         
         ST    RF,BUYXFER                                                       
         CLC   BUYXFER,=F'100'     ONLY PRINT FIRST 100 CONTRACTS               
         B     BUYP0120            DON'T DISPLAY AT ALL                         
****<<<                                                                         
         BH    BUYP0120            DON'T DISPLAY - GO BACK                      
         GOTO1 REPORT                                                           
         MVC   P+1(12),=C'INPUT RECORD'                                         
         MVC   P+15(4),=C'BUY '    INSERT RECORD TYPE                           
         GOTO1 REPORT                                                           
         LA    R4,RECORD           SET RECORD ADDR                              
         ZICM  RF,RECORD+27,2      SET RECORD LENGTH                            
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
BUYP0120 EQU   *                                                                
         MVC   RECORD+16(2),=C'RM' INSERT REPLACEMENT REP ID                    
         LA    RF,REC                                                           
         LA    R1,1000                                                          
         LA    RE,RECORD                                                        
         MOVE  ((RF),(R1)),(RE)                                                 
         MVC   REC-4(2),RECORD+27  INSERT LENGTH                                
         GOTO1 OUTRECS             ADD TO OUTPUT FILE                           
         CLC   BUYXFER,=F'100'     ONLY PRINT FIRST 100 CONTRACTS               
         B     BUYP0160            DON'T DISPLAY AT ALL                         
****<<<                                                                         
         BH    BUYP0160            DON'T DISPLAY - GO BACK                      
         BAS   RE,DISPPUT          DISPLAY PUTRECORD                            
BUYP0160 EQU   *                                                                
         MVC   RECORD+16(2),=C'I8' INSERT REPLACEMENT REP ID                    
         OI    RECORD+29,X'01'     TURN ON 'CLOSED' BIT                         
         L     RF,REWRCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,REWRCTR                                                       
         CLC   BUYXFER,=F'100'     ONLY PRINT FIRST 100 CONTRACTS               
         B     BUYP0180            DON'T DISPLAY AT ALL                         
****<<<                                                                         
         BH    BUYP0180            DON'T DISPLAY - GO BACK                      
         GOTO1 REPORT                                                           
         MVC   P+1(13),=C'CLOSED RECORD'                                        
         MVC   P+15(4),=C'BUY '    INSERT RECORD TYPE                           
         GOTO1 REPORT                                                           
         LA    R4,RECORD           SET RECORD ADDR                              
         ZICM  RF,RECORD+27,2      SET RECORD LENGTH                            
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
BUYP0180 EQU   *                                                                
         CLI   QOPTION1,C'U'       UPDATE RUN?                                  
         BNE   BUYP0200            NO                                           
         BAS   RE,PUTRECRD         REWRITE THE RECORD 'CLOSED'                  
BUYP0200 EQU   *                                                                
         B     BUYP0020            GO BACK FOR NEXT BUY                         
BUYP2000 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
******************************************************************              
*  OUTRECS :  GENERATE OUTFILE ENTRIES                           *              
******************************************************************              
*                                                                               
OUTRECS  NTR1                                                                   
*                                                                               
*   THIS ENTRY ADDS LENGTH OF CONTROL BYTE TO RECORD CONTROL                    
*                                                                               
         LH    RF,REC-4            ADD LENGTH OF 'LENGTH WORD'                  
*                                     TO RECORD CONTROL                         
         LA    RF,4(RF)                                                         
         STH   RF,REC-4            PUT IT BACK                                  
*                                                                               
         LA    R0,REC-4                                                         
         PUT   FILOUTA,(R0)        PUT RECORD TO OUTPUT                         
         LA    R0,REC-4                                                         
         PUT   FILOUTB,(R0)        PUT RECORD TO BACKUP OUTPUT                  
         L     RF,PUTCTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,PUTCTR           SAVE IT                                      
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  DISPPUT:  DISPLAY RECORD JUST 'PUT' TO OUTPUT.                *              
*                                                                *              
******************************************************************              
*                                                                               
DISPPUT  NTR1                                                                   
*                                                                               
         MVC   P+10(06),=C'OUTPUT'                                              
         GOTO1 REPORT                                                           
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
**********************************************************************          
*  INITIAL:   SET UP ORIGINAL WORKAREAS, ETC                                    
**********************************************************************          
*                                                                               
INITIAL  NTR1                                                                   
         OPEN  (FILOUTA,(OUTPUT))                                               
         OPEN  (FILOUTB,(OUTPUT))                                               
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
         MVC   ACONAREA,P2         A(CONTRACT NUMBER STORAGE AREA)              
         MVC   ANXTAREA,P2         A(NEXT AVAILABLE SLOT)                       
*                                     AT THIS TIME SIZE IS UNKNOWN              
         MVI   FORCEHED,C'Y'       FORCE FIRST PAGE BREAK                       
         LA    RF,RECORD                                                        
         ST    RF,AIOAREA          SET A(IOAREA)                                
         XIT1                                                                   
         EJECT                                                                  
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
FILOUTB  DCB   DDNAME=FILOUTB,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
         EJECT                                                                  
*                                                                               
HIGH     NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   TRACEKEY,KEY                                                     
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
         SPACE 2                                                                
SEQ      NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
         SPACE 3                                                                
GETRECRD LA    R6,GETREC                                                        
         B     LINKFILE                                                         
PUTRECRD LA    R6,PUTREC                                                        
         B     LINKFILE                                                         
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),REPFILE,KEY+28,             X        
               RECORD,(0,DMWORK)                                                
         B     DMCHECK                                                          
         SPACE 3                                                                
*        DATA MANAGER INTERFACE (CHECK ERRORS)                                  
         SPACE 1                                                                
DMCHECK  TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BO    NEXIT                                                            
         TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    EQXIT                                                            
         SPACE 1                                                                
         MVC   WORK(25),=C'*** DATA MANAGER ERROR***'                           
         GOTO1 LOGIO,WORK+48,1,(25,WORK)                                        
         MVC   WORK(25),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'                BLOW UP                                      
         SPACE 2                                                                
EQXIT    CR    RB,RB                                                            
         B     MAINEXIT                                                         
         SPACE 1                                                                
NEXIT    LTR   RB,RB                                                            
         B     MAINEXIT                                                         
         EJECT                                                                  
*              ROUTINE TO TRACE DATA MANAGER CALLS                              
         SPACE 1                                                                
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   TRACEDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         LA    R4,TRACEKEY                                                      
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACE4                                                         
         SPACE 1                                                                
DMTRACE2 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
         SPACE 1                                                                
DMTRACE4 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,TRACEDM8,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     MAINEXIT                                                         
         SPACE 2                                                                
TRACEDM8 DS    C                                                                
TRACEKEY DS    CL32                                                             
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DISPTOTS:                                                     *              
******************************************************************              
*                                                                               
DISPTOTS NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         LA    R2,RECTYPES                                                      
DTOT0020 EQU   *                                                                
         CLI   0(R2),0             END OF LIST?                                 
         BZ    DTOT0100            YES - FINISHED DISPLAY                       
         MVC   P+1(4),8(R2)        INSERT RECORD TYPE                           
         L     R3,4(R2)            LOAD COUNTER VALUE                           
         EDIT  (R3),(12,P+30),COMMAS=YES                                        
         GOTO1 REPORT                                                           
         LA    R2,LRECTYPE(R2)     BUMP TO NEXT ENTRY                           
         B     DTOT0020            GO BACK FOR NEXT                             
DTOT0100 EQU   *                                                                
         MVC   P+1(24),=C'TOTAL RECORDS PROCESSED:'                             
         EDIT  GRANDTOT,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CONTRACTS TRANSFERRED  :'                             
         EDIT  CONXFER,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'BUY CONTRACTS SCANNED  :'                             
         EDIT  BUYCONTS,(12,P+30),COMMAS=YES                                    
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'BUYS      TRANSFERRED  :'                             
         EDIT  BUYXFER,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'TOTAL RECORDS WRITTEN  :'                             
         EDIT  PUTCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'TOTAL RECORDS "CLOSED" :'                             
         EDIT  REWRCTR,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*    WORK SPACE, ETC.                                                           
*                                                                               
RELO     DS    F                   RELOCATION FACTOR                            
ACONAREA DS    A                   CONTRACT NUMBER STORAGE AREA                 
ANXTAREA DS    A                   NEXT AVAILABLE SLOT                          
LBLDAREA DS    F                                                                
PUTCTR   DS    F                                                                
CONXFER  DS    F                                                                
BUYXFER  DS    F                                                                
BUYCONTS DS    F                                                                
REWRCTR  DS    F                                                                
GRANDTOT DS    F                                                                
AIOAREA  DS    A                                                                
RECORD   DS    CL1008                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENCON          CONTRACT    RECORD                           
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENBUY          BUY         RECORD                           
         EJECT                                                                  
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL1008              RECORD FOR OUTPUT                            
*                                                                               
ELCODE   DS    CL1                                                              
         EJECT                                                                  
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
         EJECT                                                                  
*  INCLUDE REGENCON                CONTRACT    RECORD                           
*  INCLUDE REGENBUY                BUY         RECORD                           
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'107REREPI102 05/01/02'                                      
         END                                                                    
