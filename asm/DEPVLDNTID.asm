*          DATA SET DEPVLDNTID AT LEVEL 012 AS OF 05/01/02                      
*PHASE DEPLNTDA PVLDNTID                                                        
*INCLUDE ADDAY                                                                  
*INCLUDE DATVAL                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE NETWEEK                                                                
*INCLUDE SCANNER                                                                
PVLDNTID TITLE '- LOAD/DUMP EXTERN FOR NTI DIRECTORY DELETION'                  
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*                                                                               
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
*                                                                               
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
*                                                                               
* FOR INITIALIZE ONLY, MODE VALUES PASSED IN LOW ORDER NIBBLE                   
*                               X'03'=COPY (UPDATE+REPORT)                      
*                               X'02'=REPORT                                    
*                               X'01'=UPDATE                                    
*                                                                               
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
* P7=V(DATAMGR)                                                                 
*                                                                               
         SPACE 2                                                                
DMLDNTID CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,DMLDNTID                                             
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* INITIALIZE LOGIC                                                    *         
* EXTERN LOOKS FOR PARAM=WEEK1,(WEEK2),(BT(YPE)=C,I),(TEST),(NOPRINT) *         
* CARD.  WEEK CAN BE EXPRESSED AS WWYY OR AS MMMDD/YY.  THE           *         
* MODE BYTE PASSED FROM DMLDXMOD DETERMINES WHETHER 'RUN'             *         
* SWITCH IS SET.  FOR UPDATE OR COPY, RECORDS WILL BE DELETED,        *         
* FOR REPORT, ONLY A LISTING WILL APPEAR.  'TEST' FORCES REPORT       *         
* MODE AND 'NOPRINT' SUPPRESSES LISTING.                              *         
***********************************************************************         
         SPACE 1                                                                
DMXINIT  DS    0H                                                               
         MVC   MODE,4(R1)          SAVE MODE BYTE                               
         MVI   RUN,YES             SET RUN TO YES                               
         TM    MODE,X'01'          TEST FOR UPDATE                              
         BO    *+8                 YES                                          
         MVI   RUN,NO              OTHERWISE ITS REPORT ONLY                    
*                                                                               
         GOTO1 VPRINTER                                                         
         MVC   P(20),=C'NTI DIRECTORY DELETE'                                   
         GOTO1 VPRINTER                                                         
         MVI   P,C'-'                                                           
         MVC   P+1(19),P                                                        
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
DMXIN1   DS    0H                                                               
         RELOC                                                                  
         ST    RE,RELO             SAVE RELOCATION FACTOR                       
         LA    R1,MODTAB           POINT R1 AT TABLE OF VCONS                   
         LA    R0,MODULES          R0 SERVES AS COUNTER                         
         L     RF,0(R1)            A(ROUTINE)                                   
         AR    RF,RE               RELOCATE IT                                  
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)            NEXT ROUTINE                                 
         BCT   R0,*-14                                                          
         SPACE 1                                                                
DMXIN2   DS    0H                                                               
         L     R4,APARAMC          POINT R4 AT PARAMETER CARD                   
         CLC   0(80,R4),SPACES     IS IT BLANK                                  
         BE    MISERR              YES-PUT OUT ERROR MESSAGE                    
         LA    R0,MAXPARM                                                       
         GOTO1 VSCANNER,DMCB,(C'C',APARAMC),((R0),BLOCK),0                      
         CLI   4(R1),0             TEST FOR ERROR                               
         BE    PARMERR                                                          
         CLI   4(R1),MAXPARM       TEST FOR TOO MANY PARMS                      
         BH    PARMERR                                                          
         ZIC   R3,DMCB+4           NUMBER OF BLOCKS                             
         LA    R2,BLOCK                                                         
         SPACE 1                                                                
DMXIN3   DS    0H                                                               
         CLC   12(4,R2),=C'TEST'                                                
         BNE   *+12                                                             
         MVI   RUN,NO              SET RUN TO REPORT ONLY                       
         B     DMXIN4                                                           
*                                                                               
         CLC   12(7,R2),=C'NOPRINT'                                             
         BNE   *+12                                                             
         MVI   PRINT,NO            SUPPRESS PRINTING OF DELETED KEYS            
         B     DMXIN4                                                           
*                                                                               
         BAS   RE,TYPVAL           LOOK FOR BOOK TYPE                           
         BE    DMXIN4              YES-FOUND VALID ONE                          
*                                                                               
         BAS   RE,WEEKVAL          VALIDATE A WEEK EXPRESSION                   
         SPACE 1                                                                
DMXIN4   DS    0H                                                               
         LA    R2,32(R2)                                                        
         BCT   R3,DMXIN3                                                        
         OC    WEEK1,WEEK1         TEST FOR AT LEAST ONE WEEK                   
         BZ    PARMERR             NO-ITS AN ERROR                              
         B     DMXIT                                                            
         SPACE 2                                                                
* ERROR EXITS                                                                   
*                                                                               
MISERR   DS    0H                                                               
         MVC   P(30),=CL30'**MISSING PARAM CARD**'                              
         B     ERRXIT                                                           
         SPACE 1                                                                
PARMERR  DS    0H                                                               
         MVC   P(30),=CL30'**INVALID PARAM CARD**'                              
         B     ERRXIT                                                           
         SPACE 1                                                                
TYPERR   DS    0H                                                               
         MVC   P(30),=CL30'**INVALID BOOK TYPE VALUE**'                         
         B     ERRXIT                                                           
         SPACE 1                                                                
ERRXIT   DS    0H                                                               
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         MVC   P+1(L'ABORT),ABORT                                               
         GOTO1 VPRINTER                                                         
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'WARNING),WARNING                                          
         LA    R0,L'WARNING        ALERT OPERATOR TO ERROR                      
         GOTO1 VLOGIO,DMCB,1,((R0),WORK)                                        
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'DUMP),DUMP                                                
         LA    R0,L'DUMP           NOW NOTIFY OF DUMP                           
         GOTO1 (RF),(R1),1,((R0),WORK)                                          
         DC    H'0'                DUMP TO STOP LOAD                            
         EJECT                                                                  
**************************************************************                  
* PROCESS RECORD LOGIC                                       *                  
* ON FIRST TIME DELETE 'N', 'P', AND 'Q' DIRECTORY ITEMS     *                  
* FOR THE WEEK(S) SPECIFIED ON THE PARAM= CARD.              *                  
**************************************************************                  
         SPACE 2                                                                
DMXREC   DS    0H                                                               
         BC    0,DMXKEEP                                                        
         OI    *-3,X'F0'           DONE FIRST TIME ONLY                         
         L     R4,VLDDEFN                                                       
         USING LDDEFND,R4                                                       
         L     R5,LDDDTFDA         POINT AT DTF FOR DA FILE                     
         CLC   =C'PAVFIL',22(R5)   ONLY DO DELETE ON PAVFIL                     
         BE    DMXREC1             OK                                           
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'PAVMSG),PAVMSG                                            
         LA    R0,L'PAVMSG                                                      
         GOTO1 VLOGIO,DMCB,1,((R0),WORK)                                        
         DC    H'0'                BLOW UP                                      
         DROP  R4                                                               
         SPACE 1                                                                
DMXREC1  DS    0H                  FILE IS OPEN FOR COPY OR UPDATE              
         TM    MODE,X'03'          TEST FOR COPY                                
         BO    NKEY                YES                                          
         TM    MODE,X'02'          TEST FOR REPORT                              
         BNO   NKEY                OPEN DIRECTORY AND FILE FOR REPORT           
         GOTO1 VDATAMGR,DMCB,DMOPEN,DMSYS,DMSYSFLS,IOAREA                       
         B     NKEY                                                             
         EJECT                                                                  
* DELETE PASSIVE POINTERS                                                       
*                                                                               
NKEY     DS    0H                                                               
         LA    R3,1                SET WEEK COUNTER                             
         LA    R4,WEEK1                                                         
         OC    WEEK2,WEEK2         TEST FOR SECOND WEEK                         
         BZ    NKEY2                                                            
         LA    R3,2                                                             
         SPACE 1                                                                
NKEY2    DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING PNKEY,R2                                                         
         MVC   PNCODE(3),=C'NNN'                                                
         MVC   PNBTYP,BTYP         BOOK TYPE                                    
         MVC   PNBOOK,0(R4)        WEEK                                         
         BAS   RE,HIGH                                                          
         SPACE 1                                                                
NKEY3    DS    0H                                                               
         CLC   KEY(PNSTAT-PNKEY),KEYSAVE                                        
         BNE   NKEY8               NOT SAME BOOK                                
         CLI   PNSTAT+4,C'T'                                                    
         BNE   NKEY6               READ NEXT ENTRY                              
         LA    RE,NETAB                                                         
         LA    RF,NETABS                                                        
NKEY4    CLC   PNSTAT(3),0(RE)     CHECK NETWORK TABLE                          
         BE    NKEY5                                                            
         LA    RE,L'NETAB(RE)                                                   
         BCT   RF,NKEY4                                                         
         B     NKEY6               READ NEXT ENTRY                              
*                                                                               
NKEY5    OI    PNKSTAT,X'80'       TURN ON DELETE BIT                           
         BAS   RE,WRITE                                                         
         BAS   RE,DUMPKEY                                                       
         L     R1,NDELETES                                                      
         LA    R1,1(R1)                                                         
         ST    R1,NDELETES                                                      
*                                                                               
NKEY6    BAS   RE,SEQ                                                           
         B     NKEY3                                                            
         SPACE 1                                                                
NKEY8    DS    0H                                                               
         LA    R4,WEEK2                                                         
         BCT   R3,NKEY2                                                         
         B     PKEY                                                             
         EJECT                                                                  
* DELETE PROGRAM AVERAGE DIRECTORY ENTRIES                                      
*                                                                               
PKEY     DS    0H                                                               
         LA    R3,1                                                             
         LA    R4,WEEK1                                                         
         OC    WEEK2,WEEK2                                                      
         BZ    PKEY2                                                            
         LA    R3,2                                                             
         SPACE 1                                                                
PKEY2    DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING PRKEY,R2                                                         
         MVC   PRCODE(3),=C'PNN'                                                
         BAS   RE,HIGH                                                          
         CLC   KEY(3),=C'PNN'      TEST FOR NETWORK                             
         BNE   QKEY                EXIT IF NOT FOUND                            
         SPACE 1                                                                
PKEY3    DS    0H                                                               
         MVC   PRBOOK,0(R4)        SET WEEK AND CLEAR REST OF KEY               
         XC    PRSTYP(PRKSTAT-PRSTYP),PRSTYP                                    
         BAS   RE,HIGH                                                          
*                                                                               
PKEY4    CLC   KEY(PRSTYP-PRKEY),KEYSAVE TEST FOR SAME BOOK                     
         BNE   PKEY8                                                            
*                                                                               
         CLI   PRSTAT+4,C'T'                                                    
         BNE   PKEY8               READ NEXT ENTRY                              
         LA    RE,NETAB                                                         
         LA    RF,NETABS                                                        
PKEY5    CLC   PRSTAT(3),0(RE)     CHECK NETWORK TABLE                          
         BE    PKEY6                                                            
         LA    RE,L'NETAB(RE)                                                   
         BCT   RF,PKEY5                                                         
         B     PKEY8               READ NEXT ENTRY                              
*                                                                               
PKEY6    CLC   PRBTYP,BTYP         FILTER ON BOOK TYPE                          
         BE    PKEY7               YES                                          
         BAS   RE,SEQ              NO-READ NEXT RECORD                          
         B     PKEY4                                                            
*                                                                               
         EJECT                                                                  
PKEY7    OI    PRKSTAT,X'80'                                                    
         BAS   RE,WRITE                                                         
         BAS   RE,DUMPKEY                                                       
         L     R1,PDELETES                                                      
         LA    R1,1(R1)                                                         
         ST    R1,PDELETES                                                      
         SPACE 1                                                                
PKEY8    DS    0H                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         ZIC   R1,PRSTAT+4         BUMP LAST BYTE OF STATION                    
         LA    R1,1(R1)                                                         
         STC   R1,PRSTAT+4         AND CLEAR REST OF KEY                        
         XC    PRKMKT(PRKSTAT-PRKMKT),PRKMKT                                    
         BAS   RE,HIGH             READ FOR NEXT STATION                        
         CLC   KEY(3),=C'PNN'                                                   
         BE    PKEY3                                                            
         SPACE 1                                                                
         DS    0H                                                               
         LA    R4,WEEK2                                                         
         BCT   R3,PKEY2            DO SECOND WEEK                               
*                                                                               
         EJECT                                                                  
* DELETE NETWORK PROGRAM ENTRIES                                                
*                                                                               
QKEY     DS    0H                                                               
         LA    R3,1                                                             
         LA    R4,WEEK1                                                         
         OC    WEEK2,WEEK2                                                      
         BZ    QKEY2                                                            
         LA    R3,2                                                             
         SPACE 1                                                                
QKEY2    DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING PMKEY,R2                                                         
         MVC   PMCODE(3),=C'QNN'                                                
         MVC   PMBOOK,0(R4)                                                     
         BAS   RE,HIGH                                                          
         SPACE 1                                                                
QKEY3    DS    0H                                                               
         CLC   KEY(PMSTAT-PMKEY),KEYSAVE TEST FOR SAME BOOK                     
         BNE   QKEY8                                                            
         CLI   PMSTAT+4,C'T'                                                    
         BNE   QKEY6               READ NEXT ENTRY                              
         LA    RE,NETAB                                                         
         LA    RF,NETABS                                                        
QKEY4    CLC   PMSTAT(3),0(RE)     CHECK NETWORK TABLE                          
         BE    QKEY5                                                            
         LA    RE,L'NETAB(RE)                                                   
         BCT   RF,QKEY4                                                         
         B     QKEY6               READ NEXT ENTRY                              
*                                                                               
QKEY5    CLC   PMBTYP,BTYP         TEST FOR MATCH ON BOOK TYPE                  
         BNE   QKEY6               NO-READ NEXT ENTRY                           
         OI    PMKSTAT,X'80'                                                    
         BAS   RE,WRITE                                                         
         BAS   RE,DUMPKEY                                                       
         L     R1,QDELETES                                                      
         LA    R1,1(R1)                                                         
         ST    R1,QDELETES                                                      
*                                                                               
QKEY6    BAS   RE,SEQ                                                           
         B     QKEY3                                                            
         SPACE 1                                                                
QKEY8    DS    0H                                                               
         LA    R4,WEEK2                                                         
         BCT   R3,QKEY2                                                         
         B     DMXKEEP             ALL DONE                                     
         EJECT                                                                  
***************************************************************                 
* END-OF-FILE LOGIC                                           *                 
* SHOW TOTALS FOR RECORDS DELETED.                            *                 
***************************************************************                 
         SPACE 1                                                                
DMXEOF   DS    0H                                                               
         MVI   SPACING+3,C'2'      PRINT A HEADLINE FOR TOTALS                  
         GOTO1 VPRINTER                                                         
         MVI   SPACING+3,C'1'                                                   
         MVC   P(26),=C'END OF FILE SUMMARY TOTALS'                             
         GOTO1 VPRINTER                                                         
         MVI   P,C'-'                                                           
         MVC   P+1(25),P                                                        
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
         LA    R4,BUCKETS          PRINT THE BUCKETS AND DESCRIPTIONS           
         LA    R3,BUCKTAB          IN A LOOP                                    
         SPACE 1                                                                
DMXEOF1  MVC   P(20),4(R3)         DESCRIPTION                                  
         MVI   P+20,C'='                                                        
         L     R2,0(R3)            BUCKET VALUE                                 
         EDIT  (R2),(10,P+22)                                                   
         GOTO1 VPRINTER                                                         
         LA    R3,L'BUCKTAB(R3)    POINT TO NEXT BUCKET                         
         BCT   R4,DMXEOF1                                                       
*                                                                               
         GOTO1 VPRINTER                                                         
         MVC   P(14),=C'CONTROL VALUES'                                         
         MVI   P+16,C'='                                                        
         MVC   P+18(2),RUN         RUN AND PRINT                                
         GOTO1 VHEXOUT,DMCB,WEEK1,P+20,4,0                                      
*                                                                               
         CLI   BTYP,0              TEST BOOK TYPE SPECIFIED                     
         BE    DMXEOF2             NO                                           
         MVC   P+30(5),=C'BTYP='                                                
         MVC   P+35(1),BTYP                                                     
*                                                                               
DMXEOF2  GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE POSSIBLE BOOK TYPE PARM                               
*                                                                               
* ON EXIT, CC=EQ FOR VALID BOOK TYPE FOUND, CC=NEQ FOR PARM                     
* CANNOT BE BOOK TYPE                                                           
*                                                                               
TYPVAL   ST    RE,SAVEREG                                                       
         CLI   1(R2),0             TEST FOR DIVIDED FIELD                       
         BE    TYPVALN             NO-CANNOT BE BOOK TYPE                       
         CLI   0(R2),2                                                          
         BL    TYPVALN                                                          
         CLI   0(R2),5                                                          
         BH    TYPVALN                                                          
         ZIC   R1,0(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R2),=C'BTYPE'  TEST FOR BT(YPE)                             
         BNE   TYPVALN                                                          
*                                                                               
TYPVAL2  CLI   1(R2),1             TEST PARM LEN=1                              
         BNE   TYPERR                                                           
         CLI   22(R2),C'A'         TEST FOR ASCRIBED                            
         BE    TYPVAL4                                                          
         CLI   22(R2),C'C'         TEST FOR CONFORMED                           
         BE    TYPVAL4                                                          
         CLI   22(R2),C'Z'         X-RATED BOOK                                 
         BE    TYPVAL4                                                          
         CLI   22(R2),C'I'         TEST FOR INTEGRATED                          
         BNE   TYPERR                                                           
TYPVAL4  CLI   BTYP,0              TEST FOR DUPLICATE PARM                      
         BNE   PARMERR             YES                                          
         MVC   BTYP,22(R2)                                                      
*                                                                               
TYPVALY  CR    RB,RB               SET CC TO EQ                                 
         B     TYPVALX                                                          
*                                                                               
TYPVALN  LTR   RB,RB               SET CC TO NEQ                                
*                                                                               
TYPVALX  L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE POTENTIAL WEEK EXPRESSIONS (R2 POINTS TO              
* INPUT)                                                                        
*                                                                               
WEEKVAL  DS    0H                                                               
         ST    RE,SAVEREG                                                       
         GOTO1 VDATVAL,DMCB,12(R2),WORK                                         
         OC    DMCB(4),DMCB        TEST FOR ERROR                               
         BZ    WEEKVAL2            YES-TRY WWYY                                 
         GOTO1 VNETWEEK,(R1),WORK,VGETDAY,VADDAY                                
         MVC   HALF+1(1),8(R1)     WEEK                                         
         PACK  DUB,WORK(2)         YEAR                                         
         CVB   R0,DUB                                                           
         STC   R0,HALF                                                          
         B     WEEKVAL4                                                         
         SPACE 1                                                                
WEEKVAL2 DS    0H                                                               
         TM    2(R2),X'80'         TEST FOR NUMERIC DATA                        
         BZ    PARMERR                                                          
         CLI   0(R2),4             TEST FOR 4 BYTES                             
         BNE   PARMERR                                                          
         PACK  DUB,12(2,R2)        WEEK                                         
         CVB   R0,DUB                                                           
         STC   R0,HALF+1                                                        
         PACK  DUB,14(2,R2)        YEAR                                         
         CVB   R0,DUB                                                           
         STC   R0,HALF                                                          
         B     WEEKVAL4                                                         
         SPACE 1                                                                
WEEKVAL4 DS    0H                                                               
         CLI   HALF,0              CHECK YEAR VALUE                             
         BE    PARMERR                                                          
         CLI   HALF,79                                                          
         BL    PARMERR                                                          
         CLI   HALF+1,0                                                         
         BE    PARMERR                                                          
         CLI   HALF+1,53                                                        
         BH    PARMERR                                                          
         LA    R1,WEEK1                                                         
         OC    WEEK1,WEEK1         TEST IF FIRST WEEK INPUT YET                 
         BZ    *+18                NO                                           
         LA    R1,WEEK2            YES-NOW LOOK AT WEEK2                        
         OC    WEEK2,WEEK2                                                      
         BNZ   PARMERR                                                          
         MVC   0(2,R1),HALF        MOVE IN BOOK                                 
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* FILE I/O ROUTINES                                                             
*                                                                               
HIGH     NTR1                                                                   
         MVC   COMMAND,DMRDHI                                                   
         MVC   KEYSAVE,KEY                                                      
         B     IO                                                               
         SPACE 1                                                                
SEQ      NTR1                                                                   
         MVC   COMMAND,DMRSEQ                                                   
         MVC   KEYSAVE,KEY                                                      
         B     IO                                                               
         SPACE 1                                                                
IO       DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,COMMAND,PAVDIR,KEY,IOAREA                          
         LA    RE,IOAREA                                                        
         MVC   KEY,0(RE)                                                        
         B     EXIT                                                             
         SPACE 1                                                                
WRITE    NTR1                                                                   
         CLI   RUN,C'N'                                                         
         BE    EXIT                                                             
         CLI   RUN,C'Y'            TEST FOR DESTROYED BYTE                      
         BNE   EXIT                                                             
         MVC   COMMAND,DMWRT                                                    
         GOTO1 VDATAMGR,DMCB,COMMAND,PAVDIR,KEY,KEY                             
         B     EXIT                                                             
         SPACE 1                                                                
EXIT     XIT1                                                                   
         SPACE 2                                                                
* ROUTINE TO DUMP OUT KEY                                                       
*                                                                               
DUMPKEY  DS    0H                                                               
         ST    RE,SAVEREG                                                       
         CLI   PRINT,C'N'          TEST WHETHER TO PRINT                        
         BER   RE                                                               
         MVC   P+1(23),KEY                EBCDIC                                
         GOTO1 VHEXOUT,DMCB,KEY,P+26,23,0 HEX                                   
         GOTO1 VPRINTER                                                         
         BASR  RE,RF               SKIP A LINE BETWEEN KEYS                     
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
* TABLE OF EXTERNAL ROUTINE ADDRESSES                                           
*                                                                               
MODTAB   DS    0F                                                               
VADDAY   DC    V(ADDAY)                                                         
VDATVAL  DC    V(DATVAL)                                                        
VGETDAY  DC    V(GETDAY)                                                        
VHEXOUT  DC    V(HEXOUT)                                                        
VLOGIO   DC    V(LOGIO)                                                         
VNETWEEK DC    V(NETWEEK)                                                       
VSCANNER DC    V(SCANNER)                                                       
MODULES  EQU   (*-MODTAB)/4                                                     
         SPACE 1                                                                
* CONSTANTS                                                                     
*                                                                               
ABORT    DC    CL50'**DIRECTORY DELETE ABORTED DUE TO ERROR**'                  
WARNING  DC    C'**WARNING** ERROR ON PARAM= CARD'                              
DUMP     DC    C'**LOAD (DIRECTORY DELETE) IS ABOUT TO DUMP**'                  
PAVMSG   DC    C'**ERROR** CANNOT RUN PVLDNTID ON THIS FILE'                    
*                                                                               
RUN      DC    C'Y'                DEFAULT IS DELETE RECORDS                    
PRINT    DC    C'Y'                DEFAULT IS TO PRINT DELETED KEYS             
MODE     DC    X'00'               MODE BYTE PASSED FROM DMLDXMOD               
WEEK1    DC    X'0000'                                                          
WEEK2    DC    X'0000'                                                          
BTYP     DC    X'00'                                                            
PAVDIR   DC    CL8'PAVDIR'                                                      
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
DMOPEN   DC    CL8'DMOPEN'                                                      
DMSYS    DC    CL8'SPOT'                                                        
DMSYSFLS DC    C'UPAVDIR NPAVFIL X'                                             
         EJECT                                                                  
* BUCKET TABLES                                                                 
*                                                                               
         DS    0F                                                               
BUCKTAB  DS    0CL24                                                            
NDELETES DC    F'0',CL20'PASSIVE PTRS DELETED'                                  
PDELETES DC    F'0',CL20'PROG AVE DELETES'                                      
QDELETES DC    F'0',CL20'NETWORK PROG DELETES'                                  
BUCKETS  EQU   (*-BUCKTAB)/L'BUCKTAB                                            
         SPACE 1                                                                
NETAB    DS    0CL3                                                             
         DC    C'ABC'                                                           
         DC    C'CBS'                                                           
         DC    C'NBC'                                                           
         DC    C'HUT'                                                           
NETABS   EQU   (*-NETAB)/L'NETAB                                                
         SPACE 1                                                                
* EQUATES                                                                       
*                                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
MAXPARM  EQU   5                   MAX N'PARMS ON PARAM= CARD                   
         SPACE 2                                                                
* WORKING STORAGE DSECT                                                         
*                                                                               
WORKD    DSECT                                                                  
RELO     DS    A                                                                
SAVEREG  DS    A                                                                
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL28                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
VDATAMGR DS    V                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
FULL     DS    F                                                                
WORK     DS    CL64                                                             
COMMAND  DS    CL8                                                              
KEY      DS    CL23                                                             
KEYSAVE  DS    CL23                                                             
BLOCK    DS    CL(MAXPARM*32)      SCANNER BLOCK                                
IOAREA   DS    CL1024                                                           
WORKX    EQU   *                                                                
         EJECT                                                                  
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012DEPVLDNTID05/01/02'                                      
         END                                                                    
