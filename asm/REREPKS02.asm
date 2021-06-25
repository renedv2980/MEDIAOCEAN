*          DATA SET REREPKS02  AT LEVEL 104 AS OF 05/01/02                      
*PHASE REKS02A,*                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE DAYPAK                                                                 
*INCLUDE TIMVAL                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE REGENBUC                                                               
*INCLUDE RECUP                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PERVERT                                                                
*INCLUDE QSORT                                                                  
         TITLE 'REREPKS02  (REKS02A) --- SUPER SWITCHER '                       
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPKS02  -- KATZ SUPER SWITCHER.                        *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* DEC09/95 (BU ) --- ORIGINAL ENTRY, BASED ON CONVERT  (REREPK802) *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*  RUN-TIME SWITCHES AND INPUT VALUES:                             *            
*      QUESTOR    =   Y   DISPLAY SORT OUTPUT                      *            
*      QUESTOR+1  =   Y   DISPLAY SORT RETURN                      *            
*      QUESTOR+2  =   Y   DISPLAY CONTRACT OUTPUT RECORDS          *            
*      QUESTOR+3  =   Y   DISPLAY KEYS AS DELETED                  *            
*      QUESTOR+4  =   Y   DISPLAY CONTRACT SWITCH                  *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*      QRECORD+20  =  REPS TO SCAN FOR SWITCH:  MAX OF 8           *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
REKS02   CSECT                                                                  
         NMOD1 0,**REKS**,R8,R9,RR=R5                                           
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
*                                                                               
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
*                                                                               
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BNE   MAIN0900                                                         
*                                                                               
MAIN0020 DS    0H                                                               
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,RUNSTRT          SAVE START TIME                              
*                                                                               
         BAS   RE,INITIAL          ESTABLISH WORK AREAS                         
         BAS   RE,TABLINIT         SET UP AGENCY SWITCH TABLE                   
         BAS   RE,CONTPROC         SWEEP THE CONTRACTS                          
         GOTO1 DISPTOTS                                                         
         CLOSE FILOUTA             CLOSE OUTPUT FILES                           
         CLOSE FILOUTB                                                          
MAIN0900 DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*   INITIALIZATIONS ....                                                        
INITIAL  NTR1                                                                   
         OPEN  (FILOUTA,(OUTPUT))                                               
         OPEN  (FILOUTB,(OUTPUT))                                               
         SPACE 1                                                                
         MVC   P+1(16),=C'ENTERING INITIAL'                                     
         GOTO1 REPORT                                                           
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     RF,COVAIL                                                        
         DROP  RF                                                               
         GOTO1 (RF),DMCB,C'GET',1000000,1000000                                 
*                                  GET 1MEG STORAGE SPACE                       
         OC    P2(4),P2                                                         
         BNZ   INIT0020                                                         
         DC    H'0'                                                             
INIT0020 EQU   *                                                                
         L     RF,P2               INITIALIZE THE WORK AREA                     
         XCEFL 0(RF),P3                                                         
         MVC   LBLDAREA,P3         L(ADD'L SPACE GIVEN)                         
         MVC   ARECAREA,P2         TAPE RECORD DELIVERY AREA                    
         L     RF,ARECAREA                                                      
         A     RF,=F'30000'        TAPE BUFFER AREA:                            
         ST    RF,AAGYAREA         SET A(AGENCY AREA)                           
         ST    RF,ANXTAGY          SET A(NEXT AGENCY)                           
         MVC   SWIREPS,SPACES      CLEAR SWITCH REPS                            
         MVC   SWIREPS(16),QRECORD+20  INSERT REPS TO BE SWITCHED               
         MVC   AGYREP,QOPTION1     SAVE REP FOR AGENCY RECORDS                  
         CLC   AGYREP,SPACES       ANY VALUE IN AGENCY REP?                     
         BNE   INIT0040            NO  - USE IT                                 
         MVC   AGYREP,=C'K3'       YES - USE KATZ                               
INIT0040 EQU   *                                                                
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*   TABLINIT:                                                                   
*                                                                               
TABLINIT NTR1                                                                   
*                                                                               
*   AGYTABLE:  READ ALL AGENCY RECORDS.  SKIP ALL NUMERIC ENTRIES.              
*        FOR EACH CORPORATE ENTRY, SEE IF THERE IS AN EQUIVALENCY               
*        CODE.  IF THERE IS, INSERT AGENCY + EQUIVALENCY INTO TABLE.            
*        SEQUENCE TABLE AS:                                                     
*             BYTES 0  -  3  =  EQUIVALENCY CODE                                
*             BYTES 4  -  5  =  OFFICE CODE (SPACE IF CORPORATE)                
*             BYTES 6  -  9  =  REPLACEMENT CODE                                
*                                                                               
AGYTABLE EQU   *                                                                
         MVC   P+1(17),=C'ENTERING AGYTABLE'                                    
         GOTO1 REPORT                                                           
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         XC    SORTREC,SORTREC                                                  
         LA    RF,RECORD3          SET A(IO AREA FOR PROCEDURE)                 
         ST    RF,AIOAREA                                                       
AGYT0020 EQU   *                                                                
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'1A'           SET RECORD TYPE                              
         GOTO1 HIGH                READ FIRST RECORD                            
         B     AGYT0060                                                         
AGYT0040 EQU   *                                                                
         GOTO1 SEQ                 READ NEXT RECORD                             
AGYT0060 EQU   *                                                                
*        MVC   P+1(10),=C'KEY INPUT='                                           
*        MVC   P+13(27),KEY                                                     
*        GOTO1 REPORT                                                           
         CLI   KEY,X'1A'           AGENCY RECORD?                               
         BNE   AGYT0200            NO  - FINISHED                               
         CLC   KEY+25(2),AGYREP    SELECTED REP'S AGENCY RECORD?                
         BNE   AGYT0040            NO  - SKIP THE RECORD                        
         LA    RF,KEY+19           CHECK KEY FOR ALL NUMERIC                    
         LA    RE,4                SET LOOP CONTROL                             
AGYT0080 EQU   *                                                                
         CLI   0(RF),C'0'          ZERO COMPARE                                 
         BL    AGYT0120            LESS THAN ZERO: USE IT                       
         CLI   0(RF),C'9'          9 COMPARE                                    
         BH    AGYT0120            MORE THAN 9: USE IT                          
         BCT   RE,AGYT0080         DO EACH POSITION                             
         B     AGYT0040            ALL NUMERIC: DON'T USE IT                    
AGYT0120 EQU   *                                                                
*        MVC   P+1(10),=C'SAVED KEY='                                           
*        MVC   P+13(10),SORTSAVE                                                
*        GOTO1 REPORT                                                           
         CLC   KEY+23(2),SPACES    ANY AGENCY OFFICE CODE?                      
         BNE   AGYT0160            NO - GO BACK FOR NEXT                        
         GOTO1 GREC                YES - READ THE RECORD                        
         OC    RAGY2EQU,RAGY2EQU   ANY EQUIVALENCY CODE?                        
         BZ    AGYT0040            NO  - GO BACK FOR NEXT                       
         MVC   SORTNUM,RAGY2EQU    INSERT EQUIV CODE                            
         MVC   SORTOFF,RAGK2AOF    INSERT OFFICE CODE                           
         MVC   SORTALFA,RAGK2AGY   INSERT NEW AGENCY CODE                       
         MVC   SORTSAVE,SORTREC                                                 
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         CLI   QUESTOR,C'Y'        DISPLAY SORT OUTPUT?                         
         BNE   AGYT0040                                                         
         MVC   P+1(09),=C'CORP OUT:'                                            
         MVC   P+12(10),SORTREC                                                 
         GOTO1 REPORT                                                           
         B     AGYT0040            GO BACK FOR NEXT                             
*                                                                               
*                1.2.3.4.5.6.7.8.9.0.1.2.3.4.5.                                 
OFFTABLE DC    C'ATBOCHDADEHOLAMNNYPHPOSESFSLTO'                                
         DC    X'0000'                                                          
         DS    0H                                                               
AGYT0160 EQU   *                                                                
         LA    RF,OFFTABLE                                                      
AGYT0180 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE?                                
         BE    AGYT0040            YES - OFFICE NOT FOUND - SKIP                
         CLC   KEY+23(2),0(RF)     OFF IN TABLE?                                
         BE    AGYT0190            YES                                          
         LA    RF,2(RF)                                                         
         B     AGYT0180            GO BACK FOR NEXT                             
AGYT0190 EQU   *                                                                
         CLC   KEY+19(4),SORTSAVE+6                                             
*                                  SAVED CORPORATE AGENCY?                      
         BNE   AGYT0040            NO  - SKIP IT                                
         MVC   SORTREC,SORTSAVE    YES                                          
         MVC   SORTOFF,KEY+23      INSERT NEW AGENCY OFFICE                     
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         CLI   QUESTOR,C'Y'        DISPLAY SORT OUTPUT?                         
         BNE   AGYT0040                                                         
         MVC   P+1(09),=C'OFFC OUT:'                                            
         MVC   P+12(10),SORTREC                                                 
         GOTO1 REPORT                                                           
         B     AGYT0040                                                         
AGYT0200 EQU   *                                                                
         L     R2,AAGYAREA                                                      
         XC    AGYCTR,AGYCTR       CLEAR AGENCY COUNTER                         
AGYT0240 EQU   *                                                                
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4           END OF FILE?                                 
         LTR   R6,R6                                                            
         BZ    AGYT0400            YES                                          
         MVC   SORTREC,0(R6)       RETRIEVE SORT RECORD                         
         MVC   0(10,R2),SORTREC    INSERT SORTREC INTO TABLE                    
         LA    R2,10(R2)           BUMP LENGTH                                  
         ST    R2,ANXTAGY          SAVE NEXT ADDRESS                            
         L     RF,AGYCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,AGYCTR                                                        
         CLI   QUESTOR+1,C'Y'      DISPLAY VALUE TO BINSRCH?                    
         BNE   AGYT0240            NO                                           
         MVC   P+1(09),=C'SORT BAK:'                                            
         MVC   P+12(10),SORTREC                                                 
         GOTO1 REPORT                                                           
         B     AGYT0240            GO BACK FOR NEXT                             
AGYT0400 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CONTPROC:  SWEEP CONTRACTS FOR EACH REP ON LIST.  SET UP AGENCY/            
*      KEY OFFICE FOR BINARY SEARCH OF AGENCY TABLE.  IF FOUND,                 
*      REPLACE AGENCY IN RECORD WITH EQUIV FROM TABLE, WRITE CONTRACT           
*      TO OUTPUT FILE, MARK ORIGINAL FOR DELETION.                              
*                                                                               
CONTPROC NTR1                                                                   
         LA    RF,RCONREC          SET IOAREA                                   
         ST    RF,AIOAREA                                                       
         LA    R2,SWIREPS                                                       
CCON0020 EQU   *                                                                
         CLC   0(2,R2),SPACES      END OF REPS?                                 
         BE    CCON1000            YES - FINISHED                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'0C'           INSERT KEY TYPE                              
         MVC   KEY+2(2),0(R2)      INSERT REP CODE                              
         GOTO1 HIGH                READ FOR THE FIRST KEY                       
         B     CCON0060                                                         
CCON0040 EQU   *                                                                
         CLC   PUTCTR2,=F'100'     ** MAX 100 O/P TEST                          
         BH    CCON0900            EXCEEDED:  TRY NEXT REP                      
         GOTO1 SEQ                 READ NEXT                                    
CCON0060 EQU   *                                                                
*        MVC   P+1(07),=C'CONKEY='                                              
*        MVC   P+10(27),KEY                                                     
*        GOTO1 REPORT                                                           
         CLI   KEY,X'0C'           CONTRACT RECORD?                             
         BNE   CCON0900            NO  - BUMP TO NEXT REP                       
         CLC   KEY+2(2),0(R2)      SAME REP?                                    
         BNE   CCON0900            NO  - BUMP TO NEXT REP                       
         L     RF,REDCTR           INCREMENT TOTAL CONTRACTS READ               
         LA    RF,1(RF)                                                         
         ST    RF,REDCTR                                                        
         MVC   DUB(4),KEY+13       SET UP SEARCH ARGUMENT:  AGENCY              
         MVC   DUB+4(2),KEY+11     OFFICE OF ORDER                              
         L     R3,AAGYAREA         SET A(AGENCY TABLE)                          
         L     R4,AGYCTR           CURRENT # OF TABLE ENTRIES                   
         GOTO1 =V(BINSRCH),DMCB,DUB,(R3),(R4),10,6,(R4),RR=RELO                 
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BNE   CCON0040            NO  - GO BACK FOR NEXT RECORD                
         ZICM  RF,DMCB+1,3         YES - GET A(TABLE ENTRY)                     
         ST    RF,ANEWAGY          SAVE A(TABLE ENTRY)                          
         GOTO1 GREC                RETRIEVE CONTRACT RECORD                     
         MVC   CONELT7F+2(4),RCONKAGY                                           
*                                  INSERT OLD AGY CODE                          
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,CONELT7F,0             
*                                  INSERT 7F ELT WITH OLD AGY CODE              
         L     RF,ANEWAGY          RELOAD A(TABLE ENTRY)                        
         MVC   RCONKAGY,6(RF)      INSERT AGENCY FROM TABLE                     
         MVC   RCONKAOF,4(RF)      INSERT AGENCY OFFICE FROM TABLE              
         CLI   QUESTOR+4,C'Y'      DISPLAY SWITCH INFO?                         
         BNE   CCON0070            NO                                           
         GOTO1 REPORT              INSERT BLANK LINE                            
         L     RF,ANEWAGY          RELOAD A(TABLE ENTRY)                        
         MVC   P+1(23),=C'SWITCH: AGY         -> '                              
         MVC   P+13(06),0(RF)      INSERT ORIGINAL AGY/OFF                      
         MVC   P+26(04),6(RF)      INSERT NEW AGENCY                            
         MVC   P+30(02),4(RF)      INSERT NEW OFFICE                            
         MVC   P+35(09),=C'FOR CON#:'                                           
         GOTO1 HEXOUT,DMCB,RCONKCON,P+45,4,=C'TOG'                              
         GOTO1 REPORT                                                           
         B     CCON0070                                                         
*                                                                               
CONELT7F DC    X'7F06000000'                                                    
*                                                                               
         DS    0F                                                               
CCON0070 EQU   *                                                                
         LA    RF,REC              PUT NEW RECORD TO TAPE                       
         LA    R1,2000             JUST IN CASE 2K'S EXIST                      
         LA    RE,RCONREC                                                       
         MOVE  ((RF),(R1)),(RE)    MOVE RECORD TO OUTPUT                        
         MVC   REC-4(2),RCONLEN    INSERT LENGTH INTO OUTPUT                    
         GOTO1 PUTRECS             GENERATE THE OUTPUT RECORDS FOR BUY          
         CLI   QUESTOR+2,C'Y'      DISPLAY OUTPUT RECORDS?                      
         BNE   CCON0200            NO                                           
         BAS   RE,DISPPUT          YES - DISPLAY OUTPUT RECORDS                 
CCON0200 EQU   *                                                                
         CLI   QUESTOR+3,C'Y'      DISPLAY DELETED KEYS?                        
         BNE   CCON0220            NO                                           
         MVC   P+1(08),=C'PRE KEY:'                                             
         MVC   P+12(34),KEY        INSERT DELETED KEY                           
         GOTO1 REPORT                                                           
CCON0220 EQU   *                                                                
         L     RF,DELCTR           INCREMENT DELETED KEYS                       
         LA    RF,1(RF)                                                         
         ST    RF,DELCTR                                                        
         OI    KEY+27,X'80'        TURN ON DELETE BIT FOR KEY                   
         CLI   QOPTION1,C'U'       UPDATE?                                      
         BNE   CCON0240            NO                                           
         GOTO1 WRITE               YES - REWRITE KEY FOR DELETION               
CCON0240 EQU   *                                                                
         CLI   QUESTOR+3,C'Y'      DISPLAY DELETED KEYS?                        
         BNE   CCON0280            NO                                           
         MVC   P+1(08),=C'DEL KEY:'                                             
         MVC   P+12(34),KEY        INSERT DELETED KEY                           
         GOTO1 REPORT                                                           
CCON0280 EQU   *                                                                
         B     CCON0040            GO BACK FOR NEXT CONTRACT                    
CCON0900 EQU   *                                                                
         XC    PUTCTR2,PUTCTR2     CLEAR COUNTER FOR LOOP                       
         LA    R2,2(R2)            BUMP TO NEXT REP IN TABLE                    
         B     CCON0020            GO BACK FOR NEXT                             
CCON1000 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DISPTOTS:                                                     *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
DISPTOTS NTR1                                                                   
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVC   P+1(24),=C'CONTRACTS     READ     :'                             
         EDIT  REDCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CONTRACTS CHANGED      :'                             
         EDIT  PUTCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'KEYS MARKED FOR DELETE :'                             
         EDIT  DELCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
DITO0400 EQU   *                                                                
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,RUNEND           SAVE END   TIME                              
         MVC   P+1(16),=C'START/END TIMES:'                                     
         GOTO1 HEXOUT,DMCB,RUNSTRT,P+20,4,=C'TOG'                               
         MVI   P+28,C'/'                                                        
         GOTO1 HEXOUT,DMCB,RUNEND,P+29,4,=C'TOG'                                
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
******************************************************************              
*  DISPPUT:  DISPLAY RECORD JUST 'PUT' TO OUTPUT.                *              
*                                                                *              
******************************************************************              
*                                                                               
DISPPUT  NTR1                                                                   
*                                                                               
DIPU0001 EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P+1(8),=C'CONTRACT'                                              
         MVC   P+10(06),=C'OUTPUT'                                              
         EDIT  PUTCTR,(7,P+20)                                                  
         GOTO1 REPORT                                                           
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
DIPU0090 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
******************************************************************              
*  PUTRECS:  GENERATE OUTFILE ENTRIES                            *              
******************************************************************              
*                                                                               
PUTRECS  NTR1                                                                   
*                                                                               
*   THIS ENTRY ADDS LENGTH OF CONTROL BYTE TO RECORD CONTROL                    
*                                                                               
*                                                                               
         LH    RF,REC-4            ADD LENGTH OF 'LENGTH WORD'                  
*                                     TO RECORD CONTROL                         
         LA    RF,4(RF)                                                         
         STH   RF,REC-4            PUT IT BACK                                  
         LA    R0,REC-4                                                         
         PUT   FILOUTA,(R0)        PUT RECORD TO OUTPUT                         
         LA    R0,REC-4                                                         
         PUT   FILOUTB,(R0)        PUT RECORD TO BACKUP OUTPUT                  
         L     RF,PUTCTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,PUTCTR           SAVE IT                                      
         L     RF,PUTCTR2          INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,PUTCTR2          SAVE IT                                      
PUTR0040 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
ARECAREA DS    A                   TAPE RECORD DELIVERY AREA                    
LBLDAREA DS    F                                                                
AAGYAREA DS    A                   AGENCY CONVERSION AREA                       
ANXTAGY  DS    A                   NEXT AGENCY AREA                             
AAGYEND  DS    A                   A(LAST ENTRY IN TABLE)                       
ANEWAGY  DS    A                   A(NEW AGENCY FOR CONTRACT)                   
AGYCTR   DS    F                                                                
CONCTR   DS    F                                                                
REDCTR   DS    F                                                                
BADCTR   DS    F                                                                
LOWCTR   DC    F'99999'            LOW DISPLAY COUNT                            
HIGHCTR  DC    F'99999'            HIGH COUNTER                                 
PUTCTR   DS    F                                                                
PUTCTR2  DS    F                                                                
DELCTR   DS    F                                                                
AIOAREA  DS    F                                                                
DATEWORK DS    CL48                DATE WORK AREA                               
COMMAND  DS    CL8                                                              
ELTBILD1 DS    CL128                                                            
ELTBILD2 DS    CL128                                                            
ELTBILD3 DS    CL128                                                            
RUNSTRT  DS    F                                                                
RUNEND   DS    F                                                                
WORK2    DS    CL256                                                            
SWIREPS  DS    CL24                                                             
AGYREP   DS    CL2                                                              
*                                                                               
FOXZEROS DC    C'0000000'                                                       
ELTAREA  DS    CL128                                                            
         DS    0F                                                               
SORTSAVE DS    CL10                                                             
SORTREC  DS    CL10                                                             
         ORG   SORTREC                                                          
SORTNUM  DS    CL4                 ORIGINAL NUMERIC AGENCY CODE                 
SORTOFF  DS    CL2                 OFFICE NUMBER                                
SORTALFA DS    CL4                 NEW ALPHANUMERIC AGENCY CODE                 
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,6,A),FORMAT=BI,WORK=1'                       
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=10'                                    
*                                                                               
         EJECT                                                                  
*                                                                               
ELCODE   DS    CL1                                                              
         SPACE 3                                                                
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
FILOUTB  DCB   DDNAME=FILOUTB,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
*                                                                               
         SPACE 3                                                                
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL2008              AREA FOR RECORD                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
*  INCLUDE REGENBUY                BUY RECORD                                   
*  INCLUDE REGENCON                CONTRACT RECORD                              
*  INCLUDE REGENSAL                SALESPERSON RECORD                           
*  INCLUDE REGENPTP                POINT PERSON RECORD                          
*  INCLUDE REGENSTA                STATION     RECORD                           
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
FILED    DSECT                                                                  
RECORD   DS    CL2048                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENCON          CONTRACT RECORD                              
         EJECT                                                                  
RECORD3  DS    CL1024                                                           
         ORG   RECORD3                                                          
       ++INCLUDE REGENAGY2         AGENCY      RECORD                           
         EJECT                                                                  
         ORG                                                                    
RECORD4  DS    CL1024                                                           
RECORD5  DS    CL1024                                                           
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE DDCOMFACSD                                                     
         EJECT                                                                  
*********************************************************************           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'104REREPKS02 05/01/02'                                      
         END                                                                    
