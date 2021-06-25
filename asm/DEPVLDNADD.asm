*          DATA SET DEPVLDNADD AT LEVEL 019 AS OF 05/01/02                      
*PHASE DEPLNDDA PVLDNADD                                                        
*INCLUDE DATVAL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE SCANNER                                                                
PVLDNADD TITLE '- LOAD/DUMP EXTERN FOR NAD DIRECTORY DELETION'                  
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
DMLDNADD CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,DMLDNADD                                             
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
* EXTERN LOOKS FOR PARAM=BOOK,(TEST),(NOPRINT) CARD.                  *         
* BOOK MUST BE EXPRESSED AS MMM/YY.                                   *         
* THE MODE BYTE PASSED FROM DMLDXMOD DETERMINES WHETHER 'RUN'         *         
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
         MVC   P(20),=C'NAD DIRECTORY DELETE'                                   
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
         BAS   RE,BOOKVAL          VALIDATE BOOK EXPRESSION                     
         SPACE 1                                                                
DMXIN4   DS    0H                                                               
         LA    R2,32(R2)                                                        
         BCT   R3,DMXIN3                                                        
*                                                                               
         OC    BOOK,BOOK           TEST FOR BOOK                                
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
* FOR THE BOOK SPECIFIED ON THE PARAM= CARD.                 *                  
**************************************************************                  
         SPACE 2                                                                
DMXREC   DS    0H                                                               
         BC    0,DMXKEEP                                                        
         OI    *-3,X'F0'                                                        
***      CLI   FIRSTSW,0                                                        
***      BNE   DMXKEEP                                                          
***      MVI   FIRSTSW,1                                                        
*                                                                               
         L     R4,VLDDEFN          DONE 1ST-TIME-THRU ONLY                      
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
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING PNKEY,R2                                                         
         MVC   PNCODE(3),=C'NNN'                                                
         MVC   PNBTYP,BTYP         BOOK TYPE                                    
         MVC   PNBOOK,BOOK         BOOK                                         
         BAS   RE,HIGH                                                          
         SPACE 1                                                                
NKEY3    DS    0H                                                               
         CLC   KEY(PNSTAT-PNKEY),KEYSAVE                                        
         BNE   PKEY                NOT SAME BOOK                                
         CLI   PNSTAT+4,C'N'                                                    
         BNE   NKEY6               READ NEXT KEY                                
         LA    R3,NETAB                                                         
         LA    R4,NETABS                                                        
NKEY4    CLC   PNSTAT(3),0(R3)     CHECK NETWORK TABLE                          
         BE    NKEY5                                                            
         LA    R3,L'NETAB(R3)                                                   
         BCT   R4,NKEY4                                                         
         B     NKEY6               READ NEXT KEY                                
NKEY5    CLC   PNBTYP,BTYP                                                      
         BNE   NKEY6               READ NEXT KEY                                
*                                                                               
         OI    PNKSTAT,X'80'       TURN ON DELETE BIT                           
         BAS   RE,WRITE                                                         
         BAS   RE,DUMPKEY                                                       
         L     R1,NDELETES                                                      
         LA    R1,1(R1)                                                         
         ST    R1,NDELETES                                                      
*                                                                               
NKEY6    BAS   RE,SEQ                                                           
         B     NKEY3                                                            
         EJECT                                                                  
* DELETE PROGRAM AVERAGE DIRECTORY ENTRIES                                      
*                                                                               
PKEY     DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING PRKEY,R2                                                         
         MVC   PRCODE(3),=C'PNN'                                                
PKEY2    BAS   RE,HIGH                                                          
         CLC   KEY(3),=C'PNN'      TEST FOR NETWORK                             
         BNE   QKEY                EXIT IF NOT FOUND                            
         SPACE 1                                                                
PKEY3    DS    0H                                                               
         MVC   PRBOOK,BOOK         SET BOOK AND CLEAR REST OF KEY               
         XC    PRSTYP(PRKSTAT-PRSTYP),PRSTYP                                    
         BAS   RE,HIGH                                                          
*                                                                               
PKEY4    CLC   KEY(PRSTYP-PRKEY),KEYSAVE TEST FOR SAME BOOK                     
         BNE   PKEY7                                                            
         CLI   PRSTAT+4,C'N'                                                    
         BNE   PKEY7               READ NEXT STATION                            
         LA    R3,NETAB                                                         
         LA    R4,NETABS                                                        
PKEY5    CLC   PRSTAT(3),0(R3)     CHECK NETWORK TABLE                          
         BE    PKEY6                                                            
         LA    R3,L'NETAB(R3)                                                   
         BCT   R4,PKEY5                                                         
         B     PKEY7               READ NEXT STATION                            
PKEY6    CLC   PRBTYP,BTYP                                                      
         BNE   PKEY7               READ NEXT STATION                            
*                                                                               
         OI    PRKSTAT,X'80'                                                    
         BAS   RE,WRITE                                                         
         BAS   RE,DUMPKEY                                                       
         L     R1,PDELETES                                                      
         LA    R1,1(R1)                                                         
         ST    R1,PDELETES                                                      
         SPACE 1                                                                
PKEY7    DS    0H                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         ZIC   R1,PRSTAT+4         BUMP LAST BYTE OF STATION                    
         LA    R1,1(R1)                                                         
         STC   R1,PRSTAT+4         AND CLEAR REST OF KEY                        
         XC    PRKMKT(PRKSTAT-PRKMKT),PRKMKT                                    
         B     PKEY2               READ FOR NEXT STATION                        
*                                                                               
         EJECT                                                                  
* DELETE NETWORK PROGRAM ENTRIES                                                
*                                                                               
QKEY     DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING PMKEY,R2                                                         
         MVC   PMCODE(3),=C'QNN'                                                
         MVC   PMBTYP,BTYP         BOOK TYPE                                    
         MVC   PMBOOK,BOOK         BOOK                                         
         BAS   RE,HIGH                                                          
         SPACE 1                                                                
QKEY3    DS    0H                                                               
         CLC   KEY(PMSTAT-PMKEY),KEYSAVE TEST FOR SAME BOOK                     
         BNE   DMXKEEP             ALL DONE                                     
         CLI   PMSTAT+4,C'N'                                                    
         BNE   QKEY6               READ NEXT KEY                                
         LA    R3,NETAB                                                         
         LA    R4,NETABS                                                        
QKEY4    CLC   PMSTAT(3),0(R3)     CHECK NETWORK TABLE                          
         BE    QKEY5                                                            
         LA    R3,L'NETAB(R3)                                                   
         BCT   R4,QKEY4                                                         
         B     QKEY6               READ NEXT KEY                                
QKEY5    CLC   PMBTYP,BTYP                                                      
         BNE   QKEY6               READ NEXT KEY                                
*                                                                               
         OI    PMKSTAT,X'80'                                                    
         BAS   RE,WRITE                                                         
         BAS   RE,DUMPKEY                                                       
         L     R1,QDELETES                                                      
         LA    R1,1(R1)                                                         
         ST    R1,QDELETES                                                      
*                                                                               
QKEY6    BAS   RE,SEQ                                                           
         B     QKEY3                                                            
*                                                                               
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
*                                                                               
DMXEOF2  GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE POTENTIAL BOOK EXPRESSION (R2 POINTS TO               
* INPUT)                                                                        
*                                                                               
BOOKVAL  DS    0H                                                               
         ST    RE,SAVEREG                                                       
*                                                                               
         GOTO1 VDATVAL,DMCB,(2,12(R2)),WORK                                     
         OC    DMCB(4),DMCB        TEST FOR ERROR                               
         BZ    PARMERR                                                          
*                                                                               
         GOTO1 VDATCON,(R1),(0,WORK),(3,BOOK)                                   
         CLI   BOOK,79             CHECK YEAR VALUE                             
         BL    PARMERR                                                          
*                                                                               
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
         MVC   P+1(23),KEY               EBCDIC                                 
         GOTO1 VHEXOUT,DMCB,KEY,P+26,23,0 HEX                                   
         GOTO1 VPRINTER                                                         
         BASR  RE,RF               SKIP A LINE BETWEEN KEYS                     
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* TABLE OF EXTERNAL ROUTINE ADDRESSES                                           
*                                                                               
MODTAB   DS    0F                                                               
VDATVAL  DC    V(DATVAL)                                                        
VHEXOUT  DC    V(HEXOUT)                                                        
VLOGIO   DC    V(LOGIO)                                                         
VDATCON  DC    V(DATCON)                                                        
VSCANNER DC    V(SCANNER)                                                       
MODULES  EQU   (*-MODTAB)/4                                                     
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
ABORT    DC    CL50'**DIRECTORY DELETE ABORTED DUE TO ERROR**'                  
WARNING  DC    C'**WARNING** ERROR ON PARAM= CARD'                              
DUMP     DC    C'**LOAD (DIRECTORY DELETE) IS ABOUT TO DUMP**'                  
PAVMSG   DC    C'**ERROR** CANNOT RUN PVLDNADD ON THIS FILE'                    
*                                                                               
RUN      DC    C'Y'                DEFAULT IS DELETE RECORDS                    
PRINT    DC    C'Y'                DEFAULT IS TO PRINT DELETED KEYS             
MODE     DC    X'00'               MODE BYTE PASSED FROM DMLDXMOD               
BTYP     DC    X'00'                                                            
BOOK     DC    X'0000'                                                          
FIRSTSW  DC    X'00'               1ST-TIME-THRU SWITCH                         
*                                                                               
PAVDIR   DC    CL8'PAVDIR'                                                      
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
DMOPEN   DC    CL8'DMOPEN'                                                      
DMSYS    DC    CL8'SPOT'                                                        
DMSYSFLS DC    C'UPAVDIR NPAVFIL X'                                             
         SPACE 1                                                                
* BUCKET TABLE                                                                  
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
MAXPARM  EQU   3                   MAX N'PARMS ON PARAM= CARD                   
         SPACE 2                                                                
* WORKING STORAGE DSECT                                                         
*                                                                               
WORKD    DSECT                                                                  
RELO     DS    A                                                                
SAVEREG  DS    A                                                                
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
*                                                                               
PLIST    DS    0CL28                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
VDATAMGR DS    V                                                                
*                                                                               
HALF     DS    H                                                                
BYTE     DS    C                                                                
FULL     DS    F                                                                
WORK     DS    CL64                                                             
COMMAND  DS    CL8                                                              
KEY      DS    CL23                                                             
KEYSAVE  DS    CL23                                                             
BLOCK    DS    CL(MAXPARM*32)      SCANNER BLOCK                                
IOAREA   DS    CL1024                                                           
*                                                                               
WORKX    EQU   *                                                                
*                                                                               
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
**PAN#1  DC    CL21'019DEPVLDNADD05/01/02'                                      
         END                                                                    
