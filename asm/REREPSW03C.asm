*          DATA SET REREPSW03C AT LEVEL 135 AS OF 05/01/02                      
*PHASE RESW02B,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE QSORT                                                                  
*INCLUDE BINSRCH2                                                               
         TITLE 'REREPSW03C (RESW03) --- COMPANY SPLIT:  SELTEL'                 
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPSW03  -- REP COMPANY SPLIT: SELTEL/CAPITOL/REPUBLIC  *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* JAN13/98 (BU ) --- ORIGINAL ENTRY                                *            
*                                                                  *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*  LIST OF DISPLAYS (REQUESTOR VALUE TRIGGER = Y IN BYTE:)         *            
*     QUESTOR+0   =  Y  =  STATION DISPLAY                         *            
*     QUESTOR+1   =  Y  =  CONTRACT TABLE INSERTION                *            
*     QUESTOR+2   =  Y  =  CONTRACT TABLE FINAL SETUP              *            
*     QUESTOR+3   =  Y  =  SORT RETURN RECORDS                     *            
*     QUESTOR+11  =  C  =  CONTRACT RECORD                         *            
*                    B  =  BUY      RECORD                         *            
*                    +  =  CONTRACT + BUY RECORD                   *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
RESW02   CSECT                                                                  
         NMOD1 0,**RESW**,R9,R8,RR=R5                                           
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BE    SW10                                                             
SWXIT    XIT1                                                                   
         EJECT                                                                  
SW10     DS    0H                                                               
         GOTO1 INITIAL                                                          
*                                                                               
         GOTO1 CONTPROC,DMCB,(RC)  PROCESS CONTRACT RECORDS                     
*                                                                               
         GOTO1 OTHRPROC,DMCB,(RC)  PROCESS ALL OTHER RECORDS                    
*                                                                               
TESTEXIT EQU   *                                                                
         GOTO1 =A(DISPTOTS),DMCB,(RC),RECTABLE                                  
*                                  DISPLAY TOTALS FOR RUN                       
         CLOSE FILOUTA             CLOSE OUTPUT FILES                           
         CLOSE FILOUTB                                                          
         B     SWXIT               EXIT                                         
         EJECT                                                                  
INITIAL  NTR1                                                                   
         OPEN  (FILOUTA,(OUTPUT))                                               
         OPEN  (FILOUTB,(OUTPUT))                                               
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
*                                                                               
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     RF,COVAIL                                                        
         DROP  RF                                                               
         GOTO1 (RF),DMCB,C'GET',1800000,4000000                                 
         OC    P2(4),P2                                                         
         BNZ   INIT0020                                                         
         DC    H'0'                                                             
INIT0020 EQU   *                                                                
         L     RF,P2               INITIALIZE THE WORK AREA                     
         XCEFL 0(RF),P3                                                         
         MVC   LBLDAREA,P3         L(ADD'L SPACE GIVEN)                         
         MVC   ABLDAREA,P2         ADDITIONAL WORKSPACE                         
         MVC   ASTAAREA,P2         A(GROUP/STA TABLE)                           
         MVC   ANEXTSTA,P2         A(NEXT GROUP/STA SLOT)                       
         L     RF,ABLDAREA         ESTABLISH WORKSPACE SETUP                    
         A     RF,=F'8000'         8000 BYTES = 1000 ENTRIES:                   
*                                  4 CHARS:  STATION                            
*                                  2 CHARS:  GROUP                              
*                                  2 CHARS:  NEW REP CODE                       
         ST    RF,ACONAREA         ESTABLISH CONTRACT NUMBER AREA               
         ST    RF,ANEXTCON         A(NEXT CON SLOT)                             
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE FIRST PAGE BREAK                       
*                                                                               
*   LOAD STATIONS INTO TABLE, BASED ON GROUP, AND INSERT NEW REP                
*                                                                               
         L     R2,ANEXTSTA         SET A(FIRST STATION IN TABLE)                
         XC    KEY,KEY                                                          
         MVI   KEY,2               SET KEY TYPE                                 
         MVC   KEY+20(2),=C'SZ'    INSERT ORIGINAL REP CODE                     
         GOTO1 HIGHDIR                                                          
         B     INIT0060                                                         
INIT0040 EQU   *                                                                
         GOTO1 SEQDIR                                                           
INIT0060 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME KEY TYPE/REP CODE?                      
         BNE   INIT0200            NO  - FINISHED LOADING STATIONS              
         LA    R5,REC                                                           
         USING RSTAREC,R5                                                       
*                                                                               
         GOTO1 GETRECRD            RETRIEVE RECORD                              
*                                                                               
         MVC   0(4,R2),RSTAKSTA    INSERT STATION LETTERS                       
         MVC   4(2,R2),RSTAGRUP    INSERT STATION GROUP                         
         LA    R1,GRPTABLE         SET A(GROUP TABLE)                           
INIT0080 EQU   *                                                                
         CLI   0(R1),0             END OF TABLE?                                
         BNE   INIT0090            NO                                           
         MVC   P+1(19),=C'GROUP NOT IN TABLE:'                                  
         MVC   P+22(2),RSTAGRUP                                                 
         MVI   P+24,C'/'                                                        
         MVC   P+25(5),RSTAKSTA                                                 
         GOTO1 REPORT                                                           
         B     INIT0040            GO BACK FOR NEXT                             
*                                                                               
INIT0090 EQU   *                                                                
         CLC   RSTAGRUP,0(R1)      GROUP FOUND?                                 
         BE    INIT0100            YES - LOAD NEW REP CODE                      
         LA    R1,4(R1)            NO  - BUMP TO NEXT ENTRY                     
         B     INIT0080            GO BACK FOR NEXT                             
*                                                                               
         DROP  R5                                                               
*                                                                               
INIT0100 EQU   *                                                                
         MVC   6(2,R2),2(R1)       INSERT NEW REP CODE INTO TABLE               
         LA    R2,8(R2)            BUMP TO NEXT STATION IN TABLE                
         L     RF,STACTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,STACTR           INCREMENT COUNTER                            
         B     INIT0040            GO BACK FOR NEXT RECORD                      
INIT0200 EQU   *                                                                
         CLI   QUESTOR+0,C'Y'      DISPLAY STATIONS?                            
         BNE   INIT0240            NO                                           
         SR    R2,R2               SET COUNTER                                  
         L     R1,ASTAAREA         YES  - SET A(STATION LIST)                   
INIT0220 EQU   *                                                                
         CLI   0(R1),0             END OF TABLE?                                
         BE    INIT0240            YES                                          
         MVC   P+1(08),=C'STATION:'                                             
         MVC   P+10(4),0(R1)       INSERT STATION CALLS                         
         MVC   P+15(2),4(R1)       INSERT GROUP                                 
         MVC   P+18(2),6(R1)       INSERT NEW REP CODE                          
         LA    R2,1(R2)                                                         
         EDIT  (R2),(4,P+24)                                                    
         GOTO1 REPORT                                                           
         LA    R1,8(R1)            BUMP TO NEXT ENTRY                           
         B     INIT0220            GO BACK FOR NEXT                             
INIT0240 EQU   *                                                                
         XIT1                                                                   
*                                                                               
*  GRPTABLE:  SPLIT APART BY GROUP/SUBGROUP.                                    
*        CHARS 1 - 2  =  GROUP/SUBGROUP                                         
*        CHARS 3 - 4  =  NEW REP CODE                                           
*                                                                               
*    PRODUCTION TABLE                                                           
*                                                                               
*GRPTABLE DC    CL4'L CA'           SHOULD BE GONE                              
*         DC    CL4'TCCA'                                                       
*         DC    CL4'TDCA'                                                       
*         DC    CL4'THRE'                                                       
*         DC    CL4'TKRE'                                                       
*         DC    CL4'TQ??'           NO CONTRACTS ON FILE FOR THIS               
*         DC    CL4'TSCA'                                                       
*         DC    CL4'TWRE'                                                       
*         DC    CL4'TYCA'                                                       
*         DC    CL4'TZCA'           SHOULD BE GONE                              
*         DC    XL2'0000'                                                       
*                                                                               
*    TEST       TABLE                                                           
*                                                                               
GRPTABLE DC    CL4'L 5S'           SHOULD BE GONE                               
         DC    CL4'TC5S'                                                        
         DC    CL4'TD5S'                                                        
         DC    CL4'TH6S'                                                        
         DC    CL4'TK6S'                                                        
         DC    CL4'TQ??'           NO CONTRACTS ON FILE FOR THIS                
         DC    CL4'TS5S'                                                        
         DC    CL4'TW6S'                                                        
         DC    CL4'TY5S'                                                        
         DC    CL4'TZ5S'           SHOULD BE GONE                               
         DC    XL2'0000'                                                        
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DISPPUT:  DISPLAY RECORD JUST 'PUT' TO OUTPUT.                *              
*     NOTE:  SOME RECORDS ARE DISPLAYED PRIOR TO LENGTH OF REC-  *              
*        ORD FIELD HAVING CONTROL LENGTH ADDED.  THIS RESULTS    *              
*        IN A SHORT DISPLAY, CUTTING OFF LAST 4 CHARACTERS.      *              
*        4 BYTES ADDED TO ALL.  BETTER TOO MUCH THAN TOO LITTLE. *              
*                                                                *              
******************************************************************              
*                                                                               
DISPPUT  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         LA    RF,4(RF)            ADD 4 TO LENGTH OF DISPLAY                   
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
DIPU0090 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  CONTPROC:  RETRIEVE ALL CONTRACT RECORDS.  NO CONTRACT NUMBER *              
*     ADJUSTMENTS ARE REQUIRED. REPLACE S/P CODE FROM TABLE.     *              
*             REPLACE GROUP/SUBGROUP AS APPROPRIATE.             *              
*                                                                *              
******************************************************************              
*                                                                               
CONTPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,REC                                                           
         USING RCONREC,R3                                                       
CPRO0020 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'0C'           SET UP CONTRACT KEY                          
         OI    DMINBTS,X'08'       GET DELETED KEYS ALSO                        
         GOTO1 HIGHDIR             RETRIEVE FIRST KEY                           
         B     CPRO0060                                                         
CPRO0040 EQU   *                                                                
         OI    DMINBTS,X'08'       GET DELETED KEYS ALSO                        
         GOTO1 SEQDIR              RETRIEVE NEXT KEY                            
CPRO0060 EQU   *                                                                
         CLC   KEY(1),KEYSAVE      SAME RECTYPE?                                
         BNE   CPRO0600            NO  - FINISHED                               
         OI    DMINBTS,X'08'       GET DELETED RECORDS ALSO                     
         GOTO1 GETRECRD            YES - RETRIEVE RECORD                        
         CLC   RCONKREP,=C'SZ'     SELTEL ORDER?                                
         BNE   CPRO0070            NO                                           
         CLI   QUESTOR+7,C'#'      USE TEST COUNTS?                             
         BNE   CPRO0065            NO                                           
         CLC   NUMCONS,=F'10000'   **TEST CUTOFF**                              
         BE    CPRO0600            NUMBER PROCESSED:  SKIP OUT                  
CPRO0065 EQU   *                                                                
         L     RF,NUMCONS                                                       
         LA    RF,1(RF)                                                         
         ST    RF,NUMCONS          BUMP SELTEL CONTRACT I/P COUNT               
CPRO0070 EQU   *                                                                
         L     RF,TOTCONS          BUMP TOTAL CONTRACT I/P COUNT                
         LA    RF,1(RF)                                                         
         ST    RF,TOTCONS                                                       
         CLC   RCONKREP,=C'SZ'     SELTEL ORDER?                                
         BNE   CPRO0080            NO                                           
         BAS   RE,SETCONRP         YES - SET NEW REP CODE                       
*                                     AND RELEASE SORT RECORD                   
CPRO0080 EQU   *                                                                
         CLC   RCONKREP,=C'SZ'     SELTEL ORDER?                                
         BNE   CPRO0090            NO                                           
         MVC   RCONKREP,OLDREP     INSERT OLD REP (SEE NOTE THERE)              
CPRO0090 EQU   *                                                                
         MVC   REC-4(2),RCONLEN    INSERT LENGTH FOR PUT                        
         L     RF,CONCTR           BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR           SAVE IT BACK                                 
         BAS   RE,PUTRECS                                                       
         CLI   QUESTOR+11,C'C'     DISPLAY CONTRACT RECORD?                     
         BE    CPRO0100            YES                                          
         CLI   QUESTOR+11,C'+'     NO  - CONTRACT + BUY?                        
         BNE   CPRO0040            NO                                           
CPRO0100 EQU   *                                                                
         OC    NUMCONS,NUMCONS     ANY SZ CONTRACTS READ?                       
         BZ    CPRO0040            NO  - DON'T PRINT YET                        
         CLC   NUMCONS,=F'50'      DISPLAY FIRST 50 CONTRACTS ONLY              
         BH    CPRO0040            GO BACK FOR NEXT CONTRACT                    
         MVC   P+1(19),=C'   CONTRACT RECORD:'                                  
         EDIT  NUMCONS,(3,P+30)                                                 
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT,DMCB,(RC)                                                
         GOTO1 REPORT                                                           
         B     CPRO0040            GO BACK FOR NEXT CONTRACT                    
CPRO0600 EQU   *                                                                
         BAS   RE,SETTABL          LOAD TABLE WITH SORTED CON#S                 
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  SETCONRP:  FIND STATION IN TABLE.  INSERT NEW REP CODE FROM   *              
*        TABLE.                                                  *              
******************************************************************              
*                                                                               
SETCONRP NTR1                                                                   
         L     R2,ASTAAREA         SET A(STATION TABLE)                         
         L     R4,STACTR           CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,RCONKSTA,(R2),(R4),08,4,1000,RR=RELO            
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    SCON0020            YES                                          
         DC    H'0'                RECORD MUST BE FOUND                         
SCON0020 EQU   *                                                                
         ZICM  R2,DMCB+1,3         SET A(TABLE ENTRY FOUND)                     
*                                    A(REP'D STATION IN LIST)                   
         MVC   RCONKREP,6(R2)      INSERT NEW REP CODE INTO CON REC             
         CLC   RCONKREP,REPREP     REPUBLIC ORDER?                              
         BE    SCON0080            YES - DON'T TABLE                            
*                                  COMPLEMENT/REVERSE CONTRACT NUMBER           
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),RCONKCON                                              
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   WORK+0(4),WORK+15                                                
         PACK  WORK+1(1),WORK+18(1)    REVERSE THE COMPLIMENT                   
         PACK  WORK+2(1),WORK+17(1)                                             
         PACK  WORK+3(1),WORK+16(1)                                             
         PACK  WORK+4(1),WORK+15(1)                                             
         CLI   QUESTOR+1,C'Y'                                                   
         BNE   SCON0040                                                         
         MVC   P+1(10),=C'BUYLINE = '                                           
         GOTO1 HEXOUT,DMCB,RCONKCON,P+15,4,=C'TOG'                              
         GOTO1 HEXOUT,DMCB,WORK+1,P+25,4,=C'TOG'                                
         EDIT  NUMCONS,(7,P+40)                                                 
         GOTO1 REPORT                                                           
*                                                                               
SCON0040 EQU   *                                                                
         L     RF,TABLCAP          INCREMENT CAPITOL CONTRACT COUNT             
         LA    RF,1(RF)                                                         
         ST    RF,TABLCAP          SAVE IT BACK                                 
         MVC   SCON#,WORK+1        INSERT CONTRACT NUMBER INTO TABLE            
         MVC   SNEWREP,RCONKREP    INSERT NEW REP INTO SORT RECORD              
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
*                                  RELEASE SORT RECORD                          
         B     SCON0100                                                         
SCON0080 EQU   *                                                                
         L     RF,TABLREP          INCREMENT REPUBLIC CONTRACT COUNT            
         LA    RF,1(RF)                                                         
         ST    RF,TABLREP          SAVE IT BACK                                 
SCON0100 EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
*******************************************************************             
*   SETTABL:  RETRIEVE SORT RECORDS AND SET UP TABLE.             *             
*******************************************************************             
SETTABL  NTR1                                                                   
         L     R2,ANEXTCON         SET A(NEXT CON NUMBER IN TABLE)              
STAB0020 EQU   *                                                                
         BAS   RE,GETSORT          RETURN SORT RECORD                           
         CLI   SCON#,X'FF'         EOF?                                         
         BE    STAB0080            YES                                          
         MVC   0(4,R2),SCON#       INSERT CONTRACT NUMBER INTO TABLE            
         MVC   4(2,R2),SNEWREP     INSERT NEW REP INTO TABLE                    
         LA    R2,6(R2)            BUMP TO NEXT SLOT                            
         ST    R2,ANEXTCON         SAVE IT BACK                                 
         B     STAB0020            GO BACK FOR NEXT                             
STAB0080 EQU   *                                                                
         CLI   QUESTOR+2,C'Y'      DISPLAY FINAL TABLE?                         
         BNE   STAB0160            NO                                           
         L     R2,ACONAREA         YES - SET A(1ST ENTRY)                       
         LA    R3,100              DISPLAY FIRST 100 ENTRIES                    
STAB0100 EQU   *                                                                
         MVC   P+1(10),=C'CON TABLE:'                                           
         GOTO1 HEXOUT,DMCB,(R2),P+15,4,=C'TOG'                                  
         MVC   P+25(2),4(R2)                                                    
         EDIT  (R3),(4,P+30)                                                    
         GOTO1 REPORT                                                           
         LA    R2,6(R2)            BUMP TO NEXT SLOT                            
         BCT   R3,STAB0100         GO BACK FOR NEXT                             
STAB0160 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
                                                                                
*                                                                               
*   SORT RETURN AND END-OF-FILE TESTING                                         
*                                                                               
GETSORT  NTR1                                                                   
         CLI   SCON#,X'FF'         EOF REACHED?                                 
         BE    GSOR0200            YES - FINISHED                               
         MVI   SCON#,X'FF'                                                      
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         LTR   R6,R6                                                            
         BZ    GSOR0200            TEST RETURN ZERO=EOF                         
         MVC   SORTREC,0(R6)                                                    
         CLI   QUESTOR+3,C'Y'      DISPLAY SORT RETURN?                         
         BNE   GSOR0200            NO  - FINISHED                               
         CLC   RETCTR,=F'100'      DISPLAY FIRST 100 RETURNED                   
         BE    GSOR0200                                                         
         MVC   P+5(07),=C'GETSORT' **TEST**                                     
         MVC   P+15(06),SORTREC    **TEST**                                     
         GOTO1 REPORT              **TEST**                                     
         L     RF,RETCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,RETCTR                                                        
GSOR0200 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   OTHRPROC:  USE RECTABLE TO PROCESS ALL OTHER RECORDS ON FILE.               
*                                                                               
OTHRPROC NTR1                                                                   
         XC    KEY,KEY             SET KEY TO START                             
         GOTO1 HIGHDIR             READ FIRST RECORD ON FILE                    
OTHR0020 EQU   *                                                                
         GOTO1 SEQDIR                                                           
OTHR0040 EQU   *                                                                
         CLI   KEY,X'51'           KEY HIGHER THAN LAST PRIMARY?                
         BH    OTHR0800            YES - FINISHED                               
*                                                                               
         OI    DMINBTS,X'08'       SET TO GET DELETED RECORDS ALSO              
         GOTO1 GETRECRD            RETRIEVE RECORD                              
*                                                                               
         LA    R2,RECTABLE         SET A(RECORD TABLE)                          
         LA    R4,NRECTABS         CURRENT # OF TABLE ENTRIES                   
*                                     USE FOR MAX TABLE ENTRIES TOO             
         ST    R4,DMCB+20          STORE LENGTH IN PARA 6                       
         LA    RF,LRECTABL         SET L(TABLE ENTRY)                           
         ST    RF,DMCB+12          STORE LENGTH IN PARA 4                       
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,KEY,(R2),(R4),,1,,RR=RELO                       
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    OTHR0060            YES                                          
         DC    H'0'                RECORD MUST BE FOUND                         
OTHR0060 EQU   *                                                                
         ZICM  R2,DMCB+1,3         SET A(TABLE ENTRY FOUND)                     
*                                    A(RECORD TYPE IN TABLE)                    
         ZICM  RF,RTDROUTN(R2),4   SET A(ROUTINE FOR PROCESS)                   
         BASR  RE,RF               BRANCH TO ROUTINE                            
         BNZ   OTHR0040            NOT ZERO:  SKIP READ DONE                    
*                                     NEXT KEY IS ALREADY READ                  
*                                     DON'T INCREMENT RECORD COUNT              
         ZICM  RF,RTDCOUNT(R2),4   INCREMENT COUNT                              
         LA    RF,1(RF)                                                         
         STCM  RF,15,RTDCOUNT(R2)  REINSERT COUNT                               
*                                                                               
         B     OTHR0020             READ NEXT KEY                               
OTHR0800 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   RECORD TABLE:  MAIN DRIVER OF PROGRAM. BREAKDOWN OF ENTRIES:                
*        DISP  0  -  0    =    RECORD TYPE                                      
*        DISP  1  -  2    =    DISPLACEMENT TO REP CODE IN KEY                  
*        DISP  3  -  3    =    M = MASTER RECORD FOR SELTEL                     
*        DISP  4  -  7    =    RECORD TYPE COUNT                                
*        DISP  8  - 11    =    A(PROCESSING ROUTINE)                            
*        DISP 12  - 19    =    SPARE JIC                                        
*                                                                               
RTDRECTP EQU   0                   DISPLACE TO RECORD TYPE                      
RTDREPCD EQU   1                   DISPLACE TO REP CODE                         
RTDMASTR EQU   3                   DISPLACE TO MASTER INDICATOR                 
RTDCOUNT EQU   4                   DISPLACE TO RECORD TYPE COUNT                
RTDROUTN EQU   8                   DISPLACE TO A(PROCESSING ROUTINE)            
RTDSPARE EQU   12                  DISPLACE TO SPARE                            
*                                                                               
RECTABLE DS    0F                  FULL-WORD ALIGN                              
*                                                                               
         DC    X'01',AL2(25),C' ',F'0',A(GENREC),F'0',F'0'                      
LRECTABL EQU   *-RECTABLE                                                       
         DC    X'02',AL2(20),C' ',F'0',A(GENREC),F'0',F'0'                      
         DC    X'03',AL2(23),C' ',F'0',A(GENREC),F'0',F'0'                      
         DC    X'04',AL2(23),C' ',F'0',A(GENREC),F'0',F'0'                      
         DC    X'05',AL2(23),C' ',F'0',A(GENREC),F'0',F'0'                      
         DC    X'06',AL2(22),C'M',F'0',A(GENREC),F'0',F'0'                      
         DC    X'07',AL2(23),C'M',F'0',A(GENREC),F'0',F'0'                      
         DC    X'08',AL2(25),C'M',F'0',A(GENREC),F'0',F'0'                      
         DC    X'09',AL2(25),C'M',F'0',A(GENREC),F'0',F'0'                      
         DC    X'0A',AL2(25),C'M',F'0',A(GENREC),F'0',F'0'                      
         DC    X'0B',AL2(16),C' ',F'0',A(BUYREC),F'0',F'0'                      
         DC    X'0C',AL2(00),C' ',F'0',A(SKIPIT),F'0',F'0'                      
         DC    X'0D',AL2(23),C'M',F'0',A(GENREC),F'0',F'0'                      
         DC    X'0E',AL2(14),C' ',F'0',A(EDIREC),F'0',F'0'                      
         DC    X'0F',AL2(23),C'M',F'0',A(GENREC),F'0',F'0'                      
         DC    X'10',AL2(00),C' ',F'0',A(DUMPIT),F'0',F'0'                      
         DC    X'11',AL2(06),C' ',F'0',A(MKGREC),F'0',F'0'                      
         DC    X'12',AL2(10),C' ',F'0',A(INVREC),F'0',F'0'                      
         DC    X'13',AL2(16),C' ',F'0',A(BUDREC),F'0',F'0'                      
         DC    X'14',AL2(17),C' ',F'0',A(AVLREC),F'0',F'0'                      
         DC    X'15',AL2(00),C' ',F'0',A(DUMPIT),F'0',F'0'                      
         DC    X'16',AL2(14),C' ',F'0',A(PRPREC),F'0',F'0'                      
         DC    X'17',AL2(00),C' ',F'0',A(DUMPIT),F'0',F'0'                      
         DC    X'18',AL2(24),C' ',F'0',A(GENREC),F'0',F'0'                      
         DC    X'19',AL2(17),C' ',F'0',A(GENREC),F'0',F'0'                      
         DC    X'1A',AL2(25),C'M',F'0',A(GENREC),F'0',F'0'                      
         DC    X'1B',AL2(15),C' ',F'0',A(EOPRC1),F'0',F'0'                      
         DC    X'1C',AL2(15),C' ',F'0',A(EOPRC2),F'0',F'0'                      
         DC    X'1D',AL2(15),C' ',F'0',A(EOPRC3),F'0',F'0'                      
         DC    X'1E',AL2(15),C' ',F'0',A(EOPRC4),F'0',F'0'                      
         DC    X'1F',AL2(16),C' ',F'0',A(GENREC),F'0',F'0'                      
         DC    X'20',AL2(00),C' ',F'0',A(DUMPIT),F'0',F'0'                      
         DC    X'21',AL2(00),C' ',F'0',A(DUMPIT),F'0',F'0'                      
         DC    X'22',AL2(00),C' ',F'0',A(DUMPIT),F'0',F'0'                      
         DC    X'23',AL2(23),C' ',F'0',A(GENREC),F'0',F'0'                      
         DC    X'24',AL2(24),C' ',F'0',A(GENREC),F'0',F'0'                      
         DC    X'25',AL2(24),C' ',F'0',A(GENREC),F'0',F'0'                      
         DC    X'26',AL2(20),C' ',F'0',A(GENREC),F'0',F'0'                      
         DC    X'27',AL2(01),C' ',F'0',A(DUMPIT),F'0',F'0'                      
         DC    X'28',AL2(13),C' ',F'0',A(SWIREC),F'0',F'0'                      
         DC    X'29',AL2(11),C' ',F'0',A(COMREC),F'0',F'0'                      
         DC    X'2A',AL2(22),C'M',F'0',A(GENREC),F'0',F'0'                      
         DC    X'2B',AL2(21),C'M',F'0',A(GENREC),F'0',F'0'                      
         DC    X'2C',AL2(04),C' ',F'0',A(AURREC),F'0',F'0'                      
         DC    X'2D',AL2(12),C' ',F'0',A(GENREC),F'0',F'0'                      
         DC    X'2E',AL2(15),C' ',F'0',A(GENREC),F'0',F'0'                      
         DC    X'2F',AL2(01),C' ',F'0',A(GENREC),F'0',F'0'                      
         DC    X'30',AL2(00),C' ',F'0',A(DUMPIT),F'0',F'0'                      
         DC    X'31',AL2(22),C'M',F'0',A(GENREC),F'0',F'0'                      
         DC    X'32',AL2(24),C' ',F'0',A(GENREC),F'0',F'0'                      
         DC    X'33',AL2(17),C' ',F'0',A(GENREC),F'0',F'0'                      
         DC    X'34',AL2(20),C' ',F'0',A(GENREC),F'0',F'0'                      
         DC    X'35',AL2(13),C' ',F'0',A(DIRREC),F'0',F'0'                      
         DC    X'36',AL2(17),C' ',F'0',A(GENREC),F'0',F'0'                      
         DC    X'37',AL2(13),C' ',F'0',A(DIRREC),F'0',F'0'                      
*                                  GOAL RECORD USES DIR RESPONSE CODE           
         DC    X'38',AL2(19),C' ',F'0',A(SETREC),F'0',F'0'                      
         DC    X'39',AL2(13),C' ',F'0',A(GENREC),F'0',F'0'                      
         DC    X'3A',AL2(22),C'M',F'0',A(GENREC),F'0',F'0'                      
         DC    X'3B',AL2(23),C'M',F'0',A(GENREC),F'0',F'0'                      
         DC    X'3C',AL2(24),C' ',F'0',A(GENREC),F'0',F'0'                      
         DC    X'3D',AL2(23),C'M',F'0',A(GENREC),F'0',F'0'                      
         DC    X'3E',AL2(10),C' ',F'0',A(GENREC),F'0',F'0'                      
         DC    X'3F',AL2(00),C' ',F'0',A(DUMPIT),F'0',F'0'                      
         DC    X'40',AL2(00),C' ',F'0',A(DUMPIT),F'0',F'0'                      
         DC    X'41',AL2(07),C' ',F'0',A(DARREC),F'0',F'0'                      
         DC    X'42',AL2(20),C' ',F'0',A(GENREC),F'0',F'0'                      
         DC    X'43',AL2(07),C' ',F'0',A(PROREC),F'0',F'0'                      
         DC    X'44',AL2(23),C' ',F'0',A(GENREC),F'0',F'0'                      
         DC    X'45',AL2(01),C' ',F'0',A(GENREC),F'0',F'0'                      
         DC    X'46',AL2(22),C'M',F'0',A(GENREC),F'0',F'0'                      
         DC    X'47',AL2(21),C' ',F'0',A(GENREC),F'0',F'0'                      
         DC    X'48',AL2(00),C' ',F'0',A(DUMPIT),F'0',F'0'                      
         DC    X'49',AL2(00),C' ',F'0',A(DUMPIT),F'0',F'0'                      
         DC    X'4A',AL2(00),C' ',F'0',A(DUMPIT),F'0',F'0'                      
         DC    X'4B',AL2(00),C' ',F'0',A(DUMPIT),F'0',F'0'                      
         DC    X'4C',AL2(00),C' ',F'0',A(DUMPIT),F'0',F'0'                      
         DC    X'4D',AL2(00),C' ',F'0',A(DUMPIT),F'0',F'0'                      
         DC    X'4E',AL2(00),C' ',F'0',A(DUMPIT),F'0',F'0'                      
         DC    X'4F',AL2(00),C' ',F'0',A(DUMPIT),F'0',F'0'                      
         DC    X'50',AL2(00),C' ',F'0',A(DUMPIT),F'0',F'0'                      
         DC    X'51',AL2(07),C' ',F'0',A(DARREC),F'0',F'0'                      
         DC    X'52',AL2(00),C' ',F'0',A(DUMPIT),F'0',F'0'                      
LRECTABX EQU   *-RECTABLE                                                       
NRECTABS EQU   LRECTABX/LRECTABL                                                
         DC    20X'00'                                                          
         EJECT                                                                  
*                                                                               
*   OUTPUT SINGLE RECORD IF:                                                    
*        NOT SELTEL RECORD                                                      
*        SELTEL RECORD, AND MASTER RECORD - DON'T CHANGE REP CODE               
*   OUTPUT RECORD FOR CAPITOL AND REPUBLIC IF:                                  
*        SELTEL RECORD, BUT NOT MASTER RECORD                                   
*   R2 --> TABLE ENTRY                                                          
*                                                                               
GENREC   NTR1                                                                   
*                                                                               
         LA    R3,REC              SET ADDRESSABILITY:  USE RCONREC AS          
*                                     COMMON LABEL FOR LENGTH                   
         USING RCONREC,R3                                                       
*                                                                               
         LR    RF,R3               SET A(RECORD)                                
         ZICM  RE,RTDREPCD(R2),2   GET DISPLACEMENT TO REP CODE                 
         AR    RF,RE               DISPLACE TO REP CODE                         
         CLC   =C'SZ',0(RF)        SELTEL RECORD?                               
         BNE   GNRC0120            NO  - JUST OUTPUT RECORD                     
         MVC   0(2,RF),OLDREP      INSERT OLD REP CODE (SEE NOTE THERE)         
         CLI   RTDMASTR(R2),C'M'   MASTER RECORD?                               
         BE    GNRC0120            YES - PUT OUT AS 'SZ'                        
         MVC   0(2,RF),REPCAP      NO  - PUT OUT CAPITOL RECORD                 
         MVC   REC-4(2),RCONLEN    INSERT LENGTH FOR PUT                        
         ST    RF,SAVERF           SAVE A(REP CODE IN KEY)                      
         BAS   RE,PUTRECS2                                                      
         L     RF,SAVERF           RESTORE A(REP CODE IN KEY)                   
         MVC   0(2,RF),REPREP      PUT OUT REPUBLIC RECORD                      
         ZICM  RE,RTDCOUNT(R2),4   ADD 1 TO COUNT FOR REPUBLIC REC              
         LA    RE,1(RE)                                                         
         STCM  RE,15,RTDCOUNT(R2)  REPLACE COUNT                                
GNRC0120 EQU   *                                                                
         MVC   REC-4(2),RCONLEN    INSERT LENGTH FOR PUT                        
*                                                                               
         DROP  R3                                                               
*                                                                               
         BAS   RE,PUTRECS2                                                      
         B     GNRC0200            EXIT                                         
GNRC0200 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
SAVERF   DS    F                                                                
         EJECT                                                                  
*                                                                               
*                                                                               
*   BYPASS THIS RECORD TYPE BY SKIP-READING TO NEXT TYPE                        
*                                                                               
SKIPIT   NTR1                                                                   
         MVC   P+1(13),=C'SKIP: PRE   :'                                        
         MVC   P+17(27),KEY                                                     
         GOTO1 REPORT                                                           
         XC    KEY+1(26),KEY+1     CLEAR ALL BUT KEYTYPE                        
         ZIC   RF,KEY              GET RECORD TYPE                              
         LA    RF,1(RF)            BUMP TO NEXT RECORD TYPE                     
         STC   RF,KEY              REPLACE KEY TYPE                             
         MVC   P+1(13),=C'SKIP: CLEAR :'                                        
         MVC   P+17(27),KEY                                                     
         GOTO1 REPORT                                                           
         GOTO1 HIGHDIR             SKIP TO NEXT KEY TYPE                        
         MVC   P+1(13),=C'SKIP: NEXT  :'                                        
         MVC   P+17(27),KEY                                                     
         GOTO1 REPORT                                                           
         LTR   RB,RB               SET CC NOT = ZERO                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   PROCESS BUY RECORD TYPE -                                                   
*        IF NOT AN 'SZ' BUY, OUTPUT RECORD WITH NO CHANGE.                      
*        IF 'SZ' BUY, SEEK CONTRACT NUMBER IN TABLE.                            
*              IF FOUND, OUTPUT AS CAPITOL                                      
*              IF NOT FOUND, OUTPUT AS REPUBLIC                                 
*                                                                               
BUYREC   NTR1                                                                   
         LA    R3,REC              SET ADDRESSABILITY FOR BUY RECORD            
         USING RBUYREC,R3                                                       
*                                                                               
         LR    R5,R3               SET A(RECORD)                                
         ZICM  RE,RTDREPCD(R2),2   GET DISPLACEMENT TO REP CODE                 
         AR    R5,RE               DISPLACE TO REP CODE                         
         CLC   =C'SZ',0(R5)        SELTEL RECORD?                               
         BNE   BUYR0120            NO  - JUST OUTPUT RECORD                     
         MVC   0(2,R5),REPCAP      INSERT CAPITOL REP CODE                      
         L     R6,ACONAREA         SET A(CONTRACT TABLE)                        
         L     R4,TABLCAP          CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,RBUYKCON,(R6),(R4),06,4,(R4),RR=RELO            
*                                                                               
         CLI   DMCB,0              RECORD FOUND? ONLY CAPITOL IN TABLE          
         BE    BUYR0120            YES - LEAVE AS CAPITOL ORDER                 
         MVC   0(2,R5),REPREP      NO  - INSERT REPUBLIC REP CODE               
BUYR0120 EQU   *                                                                
*                                                                               
         MVC   REC-4(2),RBUYLEN    INSERT LENGTH FOR PUT                        
         BAS   RE,PUTRECS2                                                      
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*   PROCESS EDI RECORD TYPE -                                                   
*        IF NOT AN 'SZ' BUY, OUTPUT RECORD WITH NO CHANGE.                      
*        IF 'SZ' BUY, SEEK CONTRACT NUMBER IN TABLE.                            
*              CONTRACT NUMBER MUST BE COMP'ED/REVERSED FOR COMPARE             
*                  IF FOUND, OUTPUT AS CAPITOL                                  
*                  IF NOT FOUND, OUTPUT AS REPUBLIC                             
*                                                                               
EDIREC   NTR1                                                                   
         LA    R3,REC              SET ADDRESSABILITY FOR EDI RECORD            
         USING REDBKEY,R3                                                       
*                                                                               
         GOTO1 COMPREVR,DMCB,REDBKCON,WORK+16                                   
*                                                                               
         LR    R5,R3               SET A(RECORD)                                
         ZICM  RE,RTDREPCD(R2),2   GET DISPLACEMENT TO REP CODE                 
         AR    R5,RE               DISPLACE TO REP CODE                         
         CLC   =C'SZ',0(R5)        SELTEL RECORD?                               
         BNE   EDIR0120            NO  - JUST OUTPUT RECORD                     
         MVC   0(2,R5),REPCAP      INSERT CAPITOL REP CODE                      
         L     R6,ACONAREA         SET A(CONTRACT TABLE)                        
         L     R4,TABLCAP          CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,WORK+16,(R6),(R4),06,4,(R4),RR=RELO             
*                                                                               
         CLI   DMCB,0              RECORD FOUND? ONLY CAPITOL IN TABLE          
         BE    EDIR0120            YES - LEAVE AS CAPITOL ORDER                 
         MVC   0(2,R5),REPREP      NO  - INSERT REPUBLIC REP CODE               
EDIR0120 EQU   *                                                                
         MVC   REC-4(2),REDBLEN    INSERT LENGTH FOR PUT                        
         BAS   RE,PUTRECS2                                                      
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*   PROCESS MKG RECORD TYPE -                                                   
*        IF NOT AN 'SZ' MKG, OUTPUT RECORD WITH NO CHANGE.                      
*        IF 'SZ' MKG, SEEK CONTRACT NUMBER IN TABLE.                            
*              IF FOUND, OUTPUT AS CAPITOL                                      
*              IF NOT FOUND, OUTPUT AS REPUBLIC                                 
*                                                                               
MKGREC   NTR1                                                                   
         LA    R3,REC              SET ADDRESSABILITY FOR MKG RECORD            
         USING RMKGREC,R3                                                       
*                                                                               
         LR    R5,R3               SET A(RECORD)                                
         ZICM  RE,RTDREPCD(R2),2   GET DISPLACEMENT TO REP CODE                 
         AR    R5,RE               DISPLACE TO REP CODE                         
         CLC   =C'SZ',0(R5)        SELTEL RECORD?                               
         BNE   MKGR0120            NO  - JUST OUTPUT RECORD                     
         MVC   0(2,R5),REPCAP      INSERT CAPITOL REP CODE                      
         L     R6,ACONAREA         SET A(CONTRACT TABLE)                        
         L     R4,TABLCAP          CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,RMKGKCON,(R6),(R4),06,4,(R4),RR=RELO            
*                                                                               
         CLI   DMCB,0              RECORD FOUND? ONLY CAPITOL IN TABLE          
         BE    MKGR0120            YES - LEAVE AS CAPITOL ORDER                 
         MVC   0(2,R5),REPREP      NO  - INSERT REPUBLIC REP CODE               
MKGR0120 EQU   *                                                                
*                                                                               
         MVC   REC-4(2),RMKGLEN    INSERT LENGTH FOR PUT                        
         BAS   RE,PUTRECS2                                                      
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*   PROCESS INV RECORD TYPE -                                                   
*        IF NOT AN 'SZ' INV, OUTPUT RECORD WITH NO CHANGE.                      
*        IF 'SZ' INV, SEEK STATION CALLS IN TABLE.                              
*              INSERT APPROPRIATE REP CODE FOR THAT STATION                     
*                                                                               
INVREC   NTR1                                                                   
         LA    R3,REC              SET ADDRESSABILITY FOR INV RECORD            
         USING RINVREC,R3                                                       
*                                                                               
         LR    R5,R3               SET A(RECORD)                                
         ZICM  RE,RTDREPCD(R2),2   GET DISPLACEMENT TO REP CODE                 
         AR    R5,RE               DISPLACE TO REP CODE                         
         CLC   =C'SZ',0(R5)        SELTEL RECORD?                               
         BNE   INVR0120            NO  - JUST OUTPUT RECORD                     
         L     R6,ASTAAREA         SET A(STATION TABLE)                         
         L     R4,STACTR           CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,RINVKSTA,(R6),(R4),08,4,1000,RR=RELO            
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    INVR0020            YES                                          
*                                  NO  - PUT OUT RECORD FOR BOTH REPS           
         MVC   RINVKREP,REPCAP     INSERT NEW CAPITOL CODE                      
*                                                                               
         MVC   REC-4(2),RINVLEN    INSERT LENGTH FOR PUT                        
         BAS   RE,PUTRECS2                                                      
         MVC   RINVKREP,REPREP     INSERT NEW REPUBLIC CODE                     
*                                                                               
         MVC   REC-4(2),RINVLEN    INSERT LENGTH FOR PUT                        
         BAS   RE,PUTRECS2                                                      
         ZICM  RF,RTDCOUNT(R2),4   INCREMENT COUNT FOR EXTRA RECORD             
         LA    RF,1(RF)                                                         
         STCM  RF,15,RTDCOUNT(R2)  RESTORE COUNT                                
         B     INVR0160                                                         
INVR0020 EQU   *                                                                
         ZICM  R6,DMCB+1,3         SET A(TABLE ENTRY FOUND)                     
*                                    A(REP'D STATION IN LIST)                   
         MVC   RINVKREP,6(R6)      INSERT NEW REP CODE INTO INV REC             
*                                                                               
INVR0120 EQU   *                                                                
         MVC   REC-4(2),RINVLEN    INSERT LENGTH FOR PUT                        
         BAS   RE,PUTRECS2                                                      
INVR0160 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*   PROCESS BUD RECORD TYPE -                                                   
*        IF NOT AN 'SZ' BUD, OUTPUT RECORD WITH NO CHANGE.                      
*        IF 'SZ' BUD, SEEK STATION CALLS IN TABLE.                              
*              INSERT APPROPRIATE REP CODE FOR THAT STATION                     
*                                                                               
BUDREC   NTR1                                                                   
         LA    R3,REC              SET ADDRESSABILITY FOR BUD RECORD            
         USING RBUDREC,R3                                                       
*                                                                               
         LR    R5,R3               SET A(RECORD)                                
         ZICM  RE,RTDREPCD(R2),2   GET DISPLACEMENT TO REP CODE                 
         AR    R5,RE               DISPLACE TO REP CODE                         
         CLC   =C'SZ',0(R5)        SELTEL RECORD?                               
         BNE   BUDR0120            NO  - JUST OUTPUT RECORD                     
         L     R6,ASTAAREA         SET A(STATION TABLE)                         
         L     R4,STACTR           CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,RBUDKSTA,(R6),(R4),08,4,1000,RR=RELO            
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    BUDR0020            YES                                          
*                                  NO  - PUT OUT RECORD FOR BOTH REPS           
         DC    H'0'                                                             
*                                                                               
BUDR0020 EQU   *                                                                
         ZICM  R6,DMCB+1,3         SET A(TABLE ENTRY FOUND)                     
*                                    A(REP'D STATION IN LIST)                   
         MVC   RBUDKREP,6(R6)      INSERT NEW REP CODE INTO BUD REC             
*                                                                               
BUDR0120 EQU   *                                                                
         MVC   REC-4(2),RBUDLEN    INSERT LENGTH FOR PUT                        
         BAS   RE,PUTRECS2                                                      
BUDR0160 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*   PROCESS AVL RECORD TYPE -                                                   
*        IF NOT AN 'SZ' AVL, OUTPUT RECORD WITH NO CHANGE.                      
*        IF 'SZ' AVL, SEEK CONTRACT NUMBER IN TABLE.                            
*              IF FOUND, OUTPUT AS CAPITOL                                      
*              IF NOT FOUND, OUTPUT AS REPUBLIC                                 
*                                                                               
AVLREC   NTR1                                                                   
         LA    R3,REC              SET ADDRESSABILITY FOR AVL RECORD            
         USING RAVLREC,R3                                                       
*                                                                               
         LR    R5,R3               SET A(RECORD)                                
         ZICM  RE,RTDREPCD(R2),2   GET DISPLACEMENT TO REP CODE                 
         AR    R5,RE               DISPLACE TO REP CODE                         
         CLC   =C'SZ',0(R5)        SELTEL RECORD?                               
         BNE   AVLR0120            NO  - JUST OUTPUT RECORD                     
         MVC   0(2,R5),REPCAP      INSERT CAPITOL REP CODE                      
         L     R6,ACONAREA         SET A(CONTRACT TABLE)                        
         L     R4,TABLCAP          CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,RAVLKCON,(R6),(R4),06,4,(R4),RR=RELO            
*                                                                               
         CLI   DMCB,0              RECORD FOUND? ONLY CAPITOL IN TABLE          
         BE    AVLR0120            YES - LEAVE AS CAPITOL ORDER                 
         MVC   0(2,R5),REPREP      NO  - INSERT REPUBLIC REP CODE               
AVLR0120 EQU   *                                                                
*                                                                               
         MVC   REC-4(2),RAVLLEN    INSERT LENGTH FOR PUT                        
         BAS   RE,PUTRECS2                                                      
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*   PROCESS PROPOSAL RECORD TYPE -                                              
*        IF NOT AN 'SZ' PRO, OUTPUT RECORD WITH NO CHANGE.                      
*        IF 'SZ' PRO, SEEK CONTRACT NUMBER IN TABLE.                            
*              IF FOUND, OUTPUT AS CAPITOL                                      
*              IF NOT FOUND, OUTPUT AS REPUBLIC                                 
*                                                                               
PRPREC   NTR1                                                                   
         LA    R3,REC              SET ADDRESSABILITY FOR PRP RECORD            
         USING RPRPREC,R3                                                       
*                                                                               
         LR    R5,R3               SET A(RECORD)                                
         ZICM  RE,RTDREPCD(R2),2   GET DISPLACEMENT TO REP CODE                 
         AR    R5,RE               DISPLACE TO REP CODE                         
         CLC   =C'SZ',0(R5)        SELTEL RECORD?                               
         BNE   PRPR0120            NO  - JUST OUTPUT RECORD                     
         MVC   0(2,R5),REPCAP      INSERT CAPITOL REP CODE                      
         L     R6,ACONAREA         SET A(CONTRACT TABLE)                        
         L     R4,TABLCAP          CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,RPRPKCON,(R6),(R4),06,4,(R4),RR=RELO            
*                                                                               
         CLI   DMCB,0              RECORD FOUND? ONLY CAPITOL IN TABLE          
         BE    PRPR0120            YES - LEAVE AS CAPITOL ORDER                 
         MVC   0(2,R5),REPREP      NO  - INSERT REPUBLIC REP CODE               
PRPR0120 EQU   *                                                                
*                                                                               
         MVC   REC-4(2),RPRPLEN    INSERT LENGTH FOR PUT                        
         BAS   RE,PUTRECS2                                                      
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*   PROCESS EOP RECORD TYPE 1 -                                                 
*        IF NOT AN 'SZ' EOP, OUTPUT RECORD WITH NO CHANGE.                      
*        IF 'SZ' EOP, SEEK STATION CALLS IN TABLE.                              
*              INSERT APPROPRIATE REP CODE FOR THAT STATION                     
*                                                                               
EOPRC1   NTR1                                                                   
         LA    R3,REC              SET ADDRESSABILITY FOR EOP RECORD            
         USING REOPREC,R3                                                       
*                                                                               
         LR    R5,R3               SET A(RECORD)                                
         ZICM  RE,RTDREPCD(R2),2   GET DISPLACEMENT TO REP CODE                 
         AR    R5,RE               DISPLACE TO REP CODE                         
         CLC   =C'SZ',0(R5)        SELTEL RECORD?                               
         BNE   EOPR1120            NO  - JUST OUTPUT RECORD                     
         L     R6,ASTAAREA         SET A(STATION TABLE)                         
         L     R4,STACTR           CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,REOPKSTA,(R6),(R4),08,4,1000,RR=RELO            
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    EOPR1020            YES                                          
*                                  NO  - PUT OUT RECORD FOR BOTH REPS           
         DC    H'0'                                                             
*                                                                               
EOPR1020 EQU   *                                                                
         ZICM  R6,DMCB+1,3         SET A(TABLE ENTRY FOUND)                     
*                                    A(REP'D STATION IN LIST)                   
         MVC   REOPKREP,6(R6)      INSERT NEW REP CODE INTO EOP REC             
*                                                                               
EOPR1120 EQU   *                                                                
         MVC   REC-4(2),REOPLEN    INSERT LENGTH FOR PUT                        
         BAS   RE,PUTRECS2                                                      
EOPR1160 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*   PROCESS EOP RECORD TYPE 2 -                                                 
*        IF NOT AN 'SZ' EOP, OUTPUT RECORD WITH NO CHANGE.                      
*        IF 'SZ' EOP, SEEK STATION CALLS IN TABLE.                              
*              INSERT APPROPRIATE REP CODE FOR THAT STATION                     
*                                                                               
EOPRC2   NTR1                                                                   
         LA    R3,REC              SET ADDRESSABILITY FOR EOP RECORD            
         USING REOPREC,R3                                                       
*                                                                               
         LR    R5,R3               SET A(RECORD)                                
         ZICM  RE,RTDREPCD(R2),2   GET DISPLACEMENT TO REP CODE                 
         AR    R5,RE               DISPLACE TO REP CODE                         
         CLC   =C'SZ',0(R5)        SELTEL RECORD?                               
         BNE   EOPR2120            NO  - JUST OUTPUT RECORD                     
         L     R6,ASTAAREA         SET A(STATION TABLE)                         
         L     R4,STACTR           CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,REO2KSTA,(R6),(R4),08,4,1000,RR=RELO            
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    EOPR2020            YES                                          
*                                  NO  - PUT OUT RECORD FOR BOTH REPS           
         DC    H'0'                                                             
*                                                                               
EOPR2020 EQU   *                                                                
         ZICM  R6,DMCB+1,3         SET A(TABLE ENTRY FOUND)                     
*                                    A(REP'D STATION IN LIST)                   
         MVC   REOPKREP,6(R6)      INSERT NEW REP CODE INTO EOP REC             
*                                                                               
EOPR2120 EQU   *                                                                
         MVC   REC-4(2),REOPLEN    INSERT LENGTH FOR PUT                        
         BAS   RE,PUTRECS2                                                      
EOPR2160 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*   PROCESS EOP RECORD TYPE 3 -                                                 
*        IF NOT AN 'SZ' EOP, OUTPUT RECORD WITH NO CHANGE.                      
*        IF 'SZ' EOP, SEEK STATION CALLS IN TABLE.                              
*              INSERT APPROPRIATE REP CODE FOR THAT STATION                     
*                                                                               
EOPRC3   NTR1                                                                   
         LA    R3,REC              SET ADDRESSABILITY FOR EOP RECORD            
         USING REOPREC,R3                                                       
*                                                                               
         LR    R5,R3               SET A(RECORD)                                
         ZICM  RE,RTDREPCD(R2),2   GET DISPLACEMENT TO REP CODE                 
         AR    R5,RE               DISPLACE TO REP CODE                         
         CLC   =C'SZ',0(R5)        SELTEL RECORD?                               
         BNE   EOPR3120            NO  - JUST OUTPUT RECORD                     
         L     R6,ASTAAREA         SET A(STATION TABLE)                         
         L     R4,STACTR           CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,REO3KSTA,(R6),(R4),08,4,1000,RR=RELO            
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    EOPR3020            YES                                          
*                                  NO  - PUT OUT RECORD FOR BOTH REPS           
         DC    H'0'                                                             
*                                                                               
EOPR3020 EQU   *                                                                
         ZICM  R6,DMCB+1,3         SET A(TABLE ENTRY FOUND)                     
*                                    A(REP'D STATION IN LIST)                   
         MVC   REOPKREP,6(R6)      INSERT NEW REP CODE INTO EOP REC             
*                                                                               
EOPR3120 EQU   *                                                                
         MVC   REC-4(2),REOPLEN    INSERT LENGTH FOR PUT                        
         BAS   RE,PUTRECS2                                                      
EOPR3160 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*   PROCESS EOP RECORD TYPE 1 -                                                 
*        IF NOT AN 'SZ' EOP, OUTPUT RECORD WITH NO CHANGE.                      
*        IF 'SZ' EOP, SEEK STATION CALLS IN TABLE.                              
*              INSERT APPROPRIATE REP CODE FOR THAT STATION                     
*                                                                               
EOPRC4   NTR1                                                                   
         LA    R3,REC              SET ADDRESSABILITY FOR EOP RECORD            
         USING REOPREC,R3                                                       
*                                                                               
         LR    R5,R3               SET A(RECORD)                                
         ZICM  RE,RTDREPCD(R2),2   GET DISPLACEMENT TO REP CODE                 
         AR    R5,RE               DISPLACE TO REP CODE                         
         CLC   =C'SZ',0(R5)        SELTEL RECORD?                               
         BNE   EOPR4120            NO  - JUST OUTPUT RECORD                     
         L     R6,ASTAAREA         SET A(STATION TABLE)                         
         L     R4,STACTR           CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,REO4KSTA,(R6),(R4),08,4,1000,RR=RELO            
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    EOPR4020            YES                                          
*                                  NO  - PUT OUT RECORD FOR BOTH REPS           
         DC    H'0'                                                             
*                                                                               
EOPR4020 EQU   *                                                                
         ZICM  R6,DMCB+1,3         SET A(TABLE ENTRY FOUND)                     
*                                    A(REP'D STATION IN LIST)                   
         MVC   REOPKREP,6(R6)      INSERT NEW REP CODE INTO EOP REC             
*                                                                               
EOPR4120 EQU   *                                                                
         MVC   REC-4(2),REOPLEN    INSERT LENGTH FOR PUT                        
         BAS   RE,PUTRECS2                                                      
EOPR4160 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*   PROCESS SDD RECORD TYPE -                                                   
*        IF NOT AN 'SZ' SDD, OUTPUT RECORD WITH NO CHANGE.                      
*        IF 'SZ' SDD, SEEK STATION CALLS IN TABLE.                              
*              INSERT APPROPRIATE REP CODE FOR THAT STATION                     
*                                                                               
SDDREC   NTR1                                                                   
         LA    R3,REC              SET ADDRESSABILITY FOR SDD RECORD            
         USING RSDDREC,R3                                                       
*                                                                               
         LR    R5,R3               SET A(RECORD)                                
         ZICM  RE,RTDREPCD(R2),2   GET DISPLACEMENT TO REP CODE                 
         AR    R5,RE               DISPLACE TO REP CODE                         
         CLC   =C'SZ',0(R5)        SELTEL RECORD?                               
         BNE   SDDR0120            NO  - JUST OUTPUT RECORD                     
         L     R6,ASTAAREA         SET A(STATION TABLE)                         
         L     R4,STACTR           CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,RSDDKSTA,(R6),(R4),08,4,1000,RR=RELO            
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    SDDR0020            YES                                          
*                                  NO  - PUT OUT RECORD FOR BOTH REPS           
         DC    H'0'                                                             
*                                                                               
SDDR0020 EQU   *                                                                
         ZICM  R6,DMCB+1,3         SET A(TABLE ENTRY FOUND)                     
*                                    A(REP'D STATION IN LIST)                   
         MVC   RSDDKREP,6(R6)      INSERT NEW REP CODE INTO SDD REC             
*                                                                               
SDDR0120 EQU   *                                                                
         MVC   REC-4(2),RSDDLEN    INSERT LENGTH FOR PUT                        
         BAS   RE,PUTRECS2                                                      
SDDR0160 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*   PROCESS SWITCH RECORD TYPE                                                  
*        IF NOT AN 'SZ' SWD, OUTPUT RECORD WITH NO CHANGE.                      
*        IF 'SZ' SWI, DROP IT                                                   
*                                                                               
SWIREC   NTR1                                                                   
         LA    R3,REC              SET ADDRESSABILITY FOR SWI RECORD            
         USING RSWIREC,R3                                                       
*                                                                               
         LR    R5,R3               SET A(RECORD)                                
         ZICM  RE,RTDREPCD(R2),2   GET DISPLACEMENT TO REP CODE                 
         AR    R5,RE               DISPLACE TO REP CODE                         
         CLC   =C'SZ',0(R5)        SELTEL RECORD?                               
         BE    SWIR0160            YES - SKIP IT                                
SWIR0120 EQU   *                                                                
         MVC   REC-4(2),RSWILEN    INSERT LENGTH FOR PUT                        
         BAS   RE,PUTRECS2                                                      
SWIR0160 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*   PROCESS COM RECORD TYPE -                                                   
*        IF NOT AN 'SZ' COM, OUTPUT RECORD WITH NO CHANGE.                      
*        IF 'SZ' COM, SEEK STATION CALLS IN TABLE.                              
*              INSERT APPROPRIATE REP CODE FOR THAT STATION                     
*                                                                               
COMREC   NTR1                                                                   
         LA    R3,REC              SET ADDRESSABILITY FOR COM RECORD            
         USING RCOMREC,R3                                                       
*                                                                               
         LR    R5,R3               SET A(RECORD)                                
         ZICM  RE,RTDREPCD(R2),2   GET DISPLACEMENT TO REP CODE                 
         AR    R5,RE               DISPLACE TO REP CODE                         
         CLC   =C'SZ',0(R5)        SELTEL RECORD?                               
         BNE   COMR0120            NO  - JUST OUTPUT RECORD                     
         L     R6,ASTAAREA         SET A(STATION TABLE)                         
         L     R4,STACTR           CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,RCOMKSTA,(R6),(R4),08,4,1000,RR=RELO            
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    COMR0020            YES                                          
*                                  NO  - PUT OUT RECORD FOR BOTH REPS           
         DC    H'0'                                                             
*                                                                               
COMR0020 EQU   *                                                                
         ZICM  R6,DMCB+1,3         SET A(TABLE ENTRY FOUND)                     
*                                    A(REP'D STATION IN LIST)                   
         MVC   RCOMKREP,6(R6)      INSERT NEW REP CODE INTO COM REC             
*                                                                               
COMR0120 EQU   *                                                                
         MVC   REC-4(2),RCOMLEN    INSERT LENGTH FOR PUT                        
         BAS   RE,PUTRECS2                                                      
COMR0160 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*   PROCESS AUR RECORD TYPE -                                                   
*        IF NOT AN 'SZ' AUR, OUTPUT RECORD WITH NO CHANGE.                      
*        IF 'SZ' AUR, SEEK STATION CALLS IN TABLE.                              
*              INSERT APPROPRIATE REP CODE FOR THAT STATION                     
*                                                                               
AURREC   NTR1                                                                   
         LA    R3,REC              SET ADDRESSABILITY FOR AUR RECORD            
         USING RAURREC,R3                                                       
*                                                                               
         LR    R5,R3               SET A(RECORD)                                
         ZICM  RE,RTDREPCD(R2),2   GET DISPLACEMENT TO REP CODE                 
         AR    R5,RE               DISPLACE TO REP CODE                         
         CLC   =C'SZ',0(R5)        SELTEL RECORD?                               
         BNE   AURR0120            NO  - JUST OUTPUT RECORD                     
         L     R6,ASTAAREA         SET A(STATION TABLE)                         
         L     R4,STACTR           CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,RAURKSTA,(R6),(R4),08,4,1000,RR=RELO            
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    AURR0020            YES                                          
*                                  NO  - PUT OUT RECORD FOR BOTH REPS           
         DC    H'0'                                                             
*                                                                               
AURR0020 EQU   *                                                                
         ZICM  R6,DMCB+1,3         SET A(TABLE ENTRY FOUND)                     
*                                    A(REP'D STATION IN LIST)                   
         MVC   RAURKREP,6(R6)      INSERT NEW REP CODE INTO AUR REC             
*                                                                               
AURR0120 EQU   *                                                                
         MVC   REC-4(2),RAURLEN    INSERT LENGTH FOR PUT                        
         BAS   RE,PUTRECS2                                                      
AURR0160 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*   PROCESS DIR RESPONSE/GOAL RECORD TYPES - USES SAME FORMAT                   
*        IF NOT AN 'SZ' DIR, OUTPUT RECORD WITH NO CHANGE.                      
*        IF 'SZ' DIR, SEEK STATION CALLS IN TABLE.                              
*              INSERT APPROPRIATE REP CODE FOR THAT STATION                     
*                                                                               
DIRREC   NTR1                                                                   
         LA    R3,REC              SET ADDRESSABILITY FOR DIR RECORD            
         USING RDIRREC,R3                                                       
*                                                                               
         LR    R5,R3               SET A(RECORD)                                
         ZICM  RE,RTDREPCD(R2),2   GET DISPLACEMENT TO REP CODE                 
         AR    R5,RE               DISPLACE TO REP CODE                         
         CLC   =C'SZ',0(R5)        SELTEL RECORD?                               
         BNE   DIRR0120            NO  - JUST OUTPUT RECORD                     
         L     R6,ASTAAREA         SET A(STATION TABLE)                         
         L     R4,STACTR           CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,RDIRSTA,(R6),(R4),08,4,1000,RR=RELO             
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    DIRR0020            YES                                          
*                                  NO  - PUT OUT RECORD FOR BOTH REPS           
         DC    H'0'                                                             
*                                                                               
DIRR0020 EQU   *                                                                
         ZICM  R6,DMCB+1,3         SET A(TABLE ENTRY FOUND)                     
*                                    A(REP'D STATION IN LIST)                   
         MVC   RDIRREP,6(R6)       INSERT NEW REP CODE INTO DIR REC             
*                                                                               
DIRR0120 EQU   *                                                                
         MVC   REC-4(2),RDIRLEN    INSERT LENGTH FOR PUT                        
         BAS   RE,PUTRECS2                                                      
DIRR0160 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*   PROCESS SET RECORDS:                                                        
*   OUTPUT SINGLE RECORD IF:                                                    
*        NOT SELTEL RECORD                                                      
*        SELTEL RECORD, IF SET IS A MASTER SET, DON'T CHANGE REP CODE           
*   OUTPUT RECORD FOR CAPITOL AND REPUBLIC IF:                                  
*        SELTEL RECORD, BUT NOT MASTER SET                                      
*   R2 --> TABLE ENTRY                                                          
*                                                                               
SETREC   NTR1                                                                   
*                                                                               
         LA    R3,REC              SET ADDRESSABILITY                           
*                                                                               
         USING RSETREC,R3                                                       
*                                                                               
         CLC   =C'SZ',RSETKREP     SELTEL RECORD?                               
         BNE   SETR0120            NO  - JUST OUTPUT RECORD                     
         MVC   RSETKREP,OLDREP     INSERT OLD REP CODE (SEE NOTE THERE)         
         CLC   RSETKSET,=C'GS'     MASTER SET?  GROUP/SUBGROUP                  
         BE    SETR0120            YES - PUT OUT AS 'SZ'                        
         CLC   RSETKSET,=C'SP'     MASTER SET? SALESPERSON                      
         BE    SETR0120            YES - PUT OUT AS 'SZ'                        
         CLC   RSETKSET,=C'AD'     MASTER SET? ADVERTISER                       
         BE    SETR0120            YES - PUT OUT AS 'SZ'                        
         CLC   RSETKSET,=C'AG'     MASTER SET? AGENCY                           
         BE    SETR0120            YES - PUT OUT AS 'SZ'                        
         CLC   RSETKSET,=C'PP'     MASTER SET? POINT PERSON                     
         BE    SETR0120            YES - PUT OUT AS 'SZ'                        
         CLC   RSETKSET,=C'DT'     MASTER SET? DEV CONTRACT TYPE                
         BE    SETR0120            YES - PUT OUT AS 'SZ'                        
         MVC   RSETKREP,REPCAP     NO  - PUT OUT CAPITOL RECORD                 
         MVC   REC-4(2),RSETLEN    INSERT LENGTH FOR PUT                        
         BAS   RE,PUTRECS2                                                      
         MVC   RSETKREP,REPREP     PUT OUT REPUBLIC RECORD                      
         ZICM  RE,RTDCOUNT(R2),4   ADD 1 TO COUNT FOR REPUBLIC REC              
         LA    RE,1(RE)                                                         
         STCM  RE,15,RTDCOUNT(R2)  REPLACE COUNT                                
SETR0120 EQU   *                                                                
         MVC   REC-4(2),RSETLEN    INSERT LENGTH FOR PUT                        
*                                                                               
         DROP  R3                                                               
*                                                                               
         BAS   RE,PUTRECS2                                                      
         B     SETR0200            EXIT                                         
SETR0200 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   PROCESS DAR RECORD TYPE - BOTH 41 AND 51 RECORDS                            
*        IF NOT AN 'SZ' DAR, OUTPUT RECORD WITH NO CHANGE.                      
*        IF 'SZ' DAR, SEEK STATION CALLS IN TABLE.                              
*              INSERT APPROPRIATE REP CODE FOR THAT STATION                     
*                                                                               
DARREC   NTR1                                                                   
         LA    R3,REC              SET ADDRESSABILITY FOR DAR RECORD            
         USING RDARREC,R3                                                       
*                                                                               
         LR    R5,R3               SET A(RECORD)                                
         ZICM  RE,RTDREPCD(R2),2   GET DISPLACEMENT TO REP CODE                 
         AR    R5,RE               DISPLACE TO REP CODE                         
         CLC   =C'SZ',0(R5)        SELTEL RECORD?                               
         BNE   DARR0120            NO  - JUST OUTPUT RECORD                     
         L     R6,ASTAAREA         SET A(STATION TABLE)                         
         L     R4,STACTR           CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,RDARKSTA,(R6),(R4),08,4,1000,RR=RELO            
*                                                                               
         CLI   DMCB,0              RECORD FOUND?                                
         BE    DARR0020            YES                                          
*                                  NO  - PUT OUT RECORD FOR BOTH REPS           
         MVC   RDARKREP,REPCAP     INSERT NEW CAPITOL CODE                      
*                                                                               
         MVC   REC-4(2),RDARLEN    INSERT LENGTH FOR PUT                        
         BAS   RE,PUTRECS2                                                      
         MVC   RDARKREP,REPREP     INSERT NEW REPUBLIC CODE                     
*                                                                               
         MVC   REC-4(2),RDARLEN    INSERT LENGTH FOR PUT                        
         BAS   RE,PUTRECS2                                                      
         ZICM  RF,RTDCOUNT(R2),4   INCREMENT COUNT FOR EXTRA RECORD             
         LA    RF,1(RF)                                                         
         STCM  RF,15,RTDCOUNT(R2)  RESTORE COUNT                                
         B     DARR0160                                                         
DARR0020 EQU   *                                                                
         ZICM  R6,DMCB+1,3         SET A(TABLE ENTRY FOUND)                     
*                                    A(REP'D STATION IN LIST)                   
         MVC   RDARKREP,6(R6)      INSERT NEW REP CODE INTO DAR REC             
*                                                                               
DARR0120 EQU   *                                                                
         MVC   REC-4(2),RDARLEN    INSERT LENGTH FOR PUT                        
         BAS   RE,PUTRECS2                                                      
DARR0160 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*   PROCESS PRO RECORD TYPE -                                                   
*        IF NOT AN 'SZ' PRO, OUTPUT RECORD WITH NO CHANGE.                      
*        IF 'SZ' PRO, SEEK CONTRACT NUMBER IN TABLE.                            
*              CONTRACT NUMBER IS 9'S COMPLEMENT - MUST BE REVERSED             
*              IF FOUND, OUTPUT AS CAPITOL                                      
*              IF NOT FOUND, OUTPUT AS REPUBLIC                                 
*                                                                               
PROREC   NTR1                                                                   
         LA    R3,REC              SET ADDRESSABILITY FOR PRO RECORD            
         USING RPROKEY,R3                                                       
*                                                                               
         LR    R5,R3               SET A(RECORD)                                
         ZICM  RE,RTDREPCD(R2),2   GET DISPLACEMENT TO REP CODE                 
         AR    R5,RE               DISPLACE TO REP CODE                         
         CLC   =C'SZ',0(R5)        SELTEL RECORD?                               
         BNE   PROR0120            NO  - JUST OUTPUT RECORD                     
         MVC   0(2,R5),REPCAP      INSERT CAPITOL REP CODE                      
*                                                                               
         GOTO1 REVERSE,DMCB,RPROKCON,WORK+1                                     
         L     R6,ACONAREA         SET A(CONTRACT TABLE)                        
         L     R4,TABLCAP          CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,WORK+1,(R6),(R4),06,4,(R4),RR=RELO              
*                                                                               
         CLI   DMCB,0              RECORD FOUND? ONLY CAPITOL IN TABLE          
         BE    PROR0120            YES - LEAVE AS CAPITOL ORDER                 
         MVC   0(2,R5),REPREP      NO  - INSERT REPUBLIC REP CODE               
PROR0120 EQU   *                                                                
*                                                                               
         MVC   REC-4(2),RPRORLEN   INSERT LENGTH FOR PUT                        
         BAS   RE,PUTRECS2                                                      
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*   PROCESS CFC RECORD TYPE -                                                   
*        IF NOT AN 'SZ' CFC, OUTPUT RECORD WITH NO CHANGE.                      
*        IF 'SZ' BUY, SEEK CONTRACT NUMBER IN TABLE.                            
*              CONTRACT NUMBER MUST BE COMP'ED/REVERSED FOR COMPARE             
*                  IF FOUND, OUTPUT AS CAPITOL                                  
*                  IF NOT FOUND, OUTPUT AS REPUBLIC                             
*                                                                               
CFCREC   NTR1                                                                   
         LA    R3,REC              SET ADDRESSABILITY FOR CFC RECORD            
         USING RCFCREC,R3                                                       
*                                                                               
         GOTO1 COMPREVR,DMCB,RCFCKCON,WORK+16                                   
*                                                                               
         LR    R5,R3               SET A(RECORD)                                
         ZICM  RE,RTDREPCD(R2),2   GET DISPLACEMENT TO REP CODE                 
         AR    R5,RE               DISPLACE TO REP CODE                         
         CLC   =C'SZ',0(R5)        SELTEL RECORD?                               
         BNE   CFCR0120            NO  - JUST OUTPUT RECORD                     
         MVC   0(2,R5),REPCAP      INSERT CAPITOL REP CODE                      
         L     R6,ACONAREA         SET A(CONTRACT TABLE)                        
         L     R4,TABLCAP          CURRENT # OF TABLE ENTRIES                   
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,WORK+16,(R6),(R4),06,4,(R4),RR=RELO             
*                                                                               
         CLI   DMCB,0              RECORD FOUND? ONLY CAPITOL IN TABLE          
         BE    CFCR0120            YES - LEAVE AS CAPITOL ORDER                 
         MVC   0(2,R5),REPREP      NO  - INSERT REPUBLIC REP CODE               
CFCR0120 EQU   *                                                                
         MVC   REC-4(2),RCFCLEN    INSERT LENGTH FOR PUT                        
         BAS   RE,PUTRECS2                                                      
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*INSERT*                                                                        
*                                                                               
*   COMP/REVERSE CONTRACT NUMBER LOCATED AT ADDRESS IN P1                       
*   PLACE RETURN ADDRESS IN ADDRESS IN P2                                       
*                                                                               
COMPREVR NTR1                                                                   
         L     R2,0(R1)            A(CONTRACT NUMBER INPUT)                     
         L     R3,4(R1)            A(CONTRACT NUMBER OUTPUT)                    
*                                                                               
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),0(4,R2)                                               
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   WORK+0(4),WORK+15                                                
         PACK  WORK+1(1),WORK+18(1)    REVERSE THE COMPLIMENT                   
         PACK  WORK+2(1),WORK+17(1)                                             
         PACK  WORK+3(1),WORK+16(1)                                             
         PACK  WORK+4(1),WORK+15(1)                                             
         MVC   0(4,R3),WORK+1      RESET OUTPUT                                 
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   REVERSE CONTRACT NUMBER LOCATED AT ADDRESS IN P1                            
*   PLACE RETURN ADDRESS IN ADDRESS IN P2                                       
*                                                                               
REVERSE  NTR1                                                                   
         L     R2,0(R1)            A(CONTRACT NUMBER INPUT)                     
         L     R3,4(R1)            A(CONTRACT NUMBER OUTPUT)                    
*                                                                               
         MVC   WORK+0(4),0(R2)         MOVE 9'S COMP CONTRACT #                 
         MVC   WORK+15(4),0(R2)        MOVE 9'S COMP CONTRACT #                 
         PACK  WORK+1(1),WORK+18(1)    REVERSE THE COMPLIMENT                   
         PACK  WORK+2(1),WORK+17(1)                                             
         PACK  WORK+3(1),WORK+16(1)                                             
         PACK  WORK+4(1),WORK+15(1)                                             
         MVC   0(4,R3),WORK+1      RESET OUTPUT                                 
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   DUMP ON THIS RECORD TYPE, BECAUSE NO RECORDS WERE SUPPOSED TO               
*        BE ON FILE.                                                            
*                                                                               
DUMPIT   NTR1                                                                   
         DC    H'0'                DUMP ON THIS TYPE                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  PUTRECS:  GENERATE OUTFILE ENTRIES                            *              
******************************************************************              
*                                                                               
PUTRECS  NTR1                                                                   
         LH    RF,REC-4            ADD LENGTH OF 'LENGTH WORD'                  
*                                     TO RECORD CONTROL                         
         LA    RF,4(RF)                                                         
         STH   RF,REC-4            PUT IT BACK                                  
         LA    R0,REC-4                                                         
         PUT   FILOUTA,(R0)        PUT RECORD TO OUTPUT                         
         LA    R0,REC-4                                                         
         PUT   FILOUTB,(R0)        PUT RECORD TO BACKUP OUTPUT                  
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  PUTRECS2:  GENERATE OUTFILE ENTRIES                           *              
******************************************************************              
*                                                                               
PUTRECS2 NTR1                                                                   
*                                                                               
         CLI   REC,X'0B'           BUY RECORD IN PROCESS?                       
         BNE   PURE0040            NO  - CHECK NEXT TYPE                        
         LA    R3,REC                                                           
         USING RBUYREC,R3                                                       
*                                                                               
         CLC   REPCAP,RBUYKREP     CAPITOL ORDER?                               
         BE    PURE0020            YES                                          
         CLC   REPREP,RBUYKREP     REPUBLIC ORDER?                              
         BNE   PURE0720            NO  - JUST DISPLAY 1ST 20                    
*                                                                               
         DROP  R3                                                               
*                                                                               
PURE0020 EQU   *                                                                
         L     RF,BUYCTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,BUYCTR                                                        
         CLC   BUYCTR,=F'500'                                                   
*                                  DISPLAY 1ST 500 BUY RECORDS                  
         BH    PURE0800                                                         
         MVC   P+1(11),=C'RECORD TYPE'                                          
         GOTO1 HEXOUT,DMCB,REC,P+13,1,=C'TOG'                                   
         EDIT  BUYCTR,(6,P+20)                                                  
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT,DMCB,(RC)                                                
         GOTO1 REPORT                                                           
         B     PURE0800                                                         
PURE0040 EQU   *                                                                
         CLI   REC,X'12'           INV RECORD IN PROCESS?                       
         BNE   PURE0720            NO  - CHECK NEXT TYPE                        
         LA    R3,REC                                                           
         USING RINVREC,R3                                                       
*                                                                               
         CLC   REPCAP,RINVKREP     CAPITOL ORDER?                               
         BE    PURE0060            YES                                          
         CLC   REPREP,RINVKREP     REPUBLIC ORDER?                              
         BNE   PURE0720            NO  - JUST DISPLAY 1ST 20                    
*                                                                               
         DROP  R3                                                               
*                                                                               
PURE0060 EQU   *                                                                
         L     RF,INVCTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,INVCTR                                                        
         CLC   INVCTR,=F'100'                                                   
*                                  DISPLAY 1ST 100 BUY RECORDS                  
         BH    PURE0800                                                         
         MVC   P+1(11),=C'RECORD TYPE'                                          
         GOTO1 HEXOUT,DMCB,REC,P+13,1,=C'TOG'                                   
         EDIT  INVCTR,(6,P+20)                                                  
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT,DMCB,(RC)                                                
         GOTO1 REPORT                                                           
         B     PURE0800                                                         
PURE0720 EQU   *                                                                
         CLC   RTDCOUNT(4,R2),=F'20'                                            
*                                  DISPLAY 1ST 20 RECORDS OF TYPE               
         BH    PURE0800                                                         
PURE0760 EQU   *                                                                
         MVC   P+1(11),=C'RECORD TYPE'                                          
         GOTO1 HEXOUT,DMCB,(R2),P+13,1,=C'TOG'                                  
         ZICM  RF,RTDCOUNT(R2),4                                                
         EDIT  (RF),(6,P+20)                                                    
         GOTO1 REPORT                                                           
         GOTO1 DISPPUT,DMCB,(RC)                                                
         GOTO1 REPORT                                                           
PURE0800 EQU   *                                                                
         LH    RF,REC-4            ADD LENGTH OF 'LENGTH WORD'                  
*                                     TO RECORD CONTROL                         
         LA    RF,4(RF)                                                         
         STH   RF,REC-4            PUT IT BACK                                  
         LA    R0,REC-4                                                         
         PUT   FILOUTA,(R0)        PUT RECORD TO OUTPUT                         
         LA    R0,REC-4                                                         
         PUT   FILOUTB,(R0)        PUT RECORD TO BACKUP OUTPUT                  
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
HIGHDIR  NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   TRACEKEY,KEY                                                     
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
         SPACE 2                                                                
SEQDIR   NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
         SPACE 3                                                                
GETRECRD LA    R6,GETREC                                                        
         B     LINKFILE                                                         
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),REPFILE,KEY+28,             X        
               REC,(0,DMWORK)                                                   
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
         B     SWXIT                                                            
         SPACE 1                                                                
NEXIT    LTR   RB,RB                                                            
         B     SWXIT                                                            
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
         B     SWXIT                                                            
         SPACE 2                                                                
*    *** REMOVE ***                                                             
         DS    CL500               SPARE FOR ADDRESSABILITY                     
*    *** REMOVE ***                                                             
TRACEDM8 DS    C                                                                
TRACEKEY DS    CL32                                                             
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
LBLDAREA DS    F                                                                
ABLDAREA DS    A                                                                
ASTAAREA DS    A                                                                
ANEXTSTA DS    A                                                                
AKEYSREP DS    A                                                                
STACTR   DS    F                   STATION COUNTER                              
ACONAREA DS    A                                                                
ANEXTCON DS    A                                                                
NUMCONS  DS    F                                                                
TOTCONS  DS    F                                                                
NUMBUYS  DS    F                                                                
CONCTR   DS    F                                                                
RETCTR   DS    F                                                                
BUYCTR   DS    F                                                                
INVCTR   DS    F                                                                
TABLREP  DS    F                                                                
TABLCAP  DS    F                                                                
*                                                                               
*    PRODUCTION CODES:  USE IN FINAL RUN                                        
*                                                                               
*REPCAP   DC    CL2'CA'             1ST REP: CAPITOL                            
*REPREP   DC    CL2'RE'             2ND REP: REPUBLIC                           
*OLDREP   DC    CL2'SZ'             OLD REP: SELTEL                             
*                                                                               
*    TEST       CODES:  USE IN TEST RUN TO LOAD TO REPFIL9                      
*                                                                               
REPCAP   DC    CL2'5S'             1ST REP: CAPITOL                             
REPREP   DC    CL2'6S'             2ND REP: REPUBLIC                            
OLDREP   DC    CL2'4S'             OLD REP: SELTEL                              
*                                                                               
SORTREC  DS    0CL6                                                             
SCON#    DS    CL4                                                              
SNEWREP  DS    CL2                 NEW FIELD FOR SUB REP                        
*                                                                               
*                                                                               
*                                                                               
ELCODE   DS    CL1                                                              
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,4,A),FORMAT=BI,WORK=1'                       
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=6'                                     
         SPACE 3                                                                
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
FILOUTB  DCB   DDNAME=FILOUTB,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL4100              AREA FOR RECORD                              
         SPACE 2                                                                
         DS    0H                  HALFWORD ALIGNMENT NEEDED.                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   INITIALIZATIONS ....                                                        
*                                                                               
*                                                                               
******************************************************************              
*  DISPTOTS:                                                     *              
******************************************************************              
*                                                                               
DISPTOTS NMOD1 0,*TOTS*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            LOAD A(RECTABLE)                             
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVC   P+1(24),=C'CONTRACTS     PROCESSED:'                             
         EDIT  TOTCONS,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'SELTEL CONS   PROCESSED:'                             
         EDIT  NUMCONS,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'CAPITOL  CONS PROCESSED:'                             
         EDIT  TABLCAP,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'REPUBLIC CONS PROCESSED:'                             
         EDIT  TABLREP,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'TOTAL  CONS   OUTPUT   :'                             
         EDIT  CONCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
DTOT0020 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE REACHED?                        
         BE    DTOT0800            YES - FINISHED                               
         MVC   P+1(11),=C'RECORD TYPE'                                          
         GOTO1 HEXOUT,DMCB,(R2),P+13,1,=C'TOG'                                  
         ZICM  R3,RTDCOUNT(R2),4                                                
         EDIT  (R3),(10,P+20)                                                   
         LA    R2,LRECTABL(R2)                                                  
         GOTO1 REPORT                                                           
         B     DTOT0020            GO BACK FOR NEXT                             
DTOT0800 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
*  INCLUDE REGENCOM                COMMISSION RECORD                            
*  INCLUDE REGENREG                REGION RECORD                                
*  INCLUDE REGENOFF                OFFICE RECORD                                
*  INCLUDE REGENEOM                EOM RECORD                                   
*  INCLUDE REGENDPT                DAYPART RECORD                               
*  INCLUDE REGENBUD                BUDGET RECORD                                
*  INCLUDE REGENBUY                BUY RECORD                                   
*  INCLUDE REGENCON                CONTRACT RECORD                              
*  INCLUDE REGENSAL                SALESPERSON RECORD                           
*  INCLUDE REGENSTA                STATION RECORD                               
*  INCLUDE REGENSDD                                                             
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
RECD     DSECT                                                                  
RECORD   DS    CL1008                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENCOM          COMMISSION RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENREG          REGION     RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENOFF          OFFICE     RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENOBUD         OFFICE BUDGET RECORD                         
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENAVLN         AVAIL RECORD                                 
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENPRPN         PROPOSAL RECORD                              
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENEOP          EOP EQUIV RECORD                             
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSWI          SWITCH    RECORD                             
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENDIR          DIR RESP  RECORD                             
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENDAR          DARE      RECORD                             
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENOCM          OFFICE COMMENT RECORD                        
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENPGTX         PROGRAM TYPE  RECORD                         
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENLAB          LABEL  RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCMT          STANDARD COMMENT RECORD                      
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCTY          CONTRACT TYPE RECORD                         
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENRDA          RADAR RECORD                                 
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENEOM          END OF MONTH RECORD                          
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENDPTA         DAYPART RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENBUD          BUDGET RECORD                                
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENBUY          BUY RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCON          CONTRACT RECORD                              
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSAL          SALESPERSON RECORD                           
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSTA          STATION RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENTER          TERRITORY RECORD                             
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSET          SET DEFINITION                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENTYP          TYPE (30) DEFINITION                         
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENREP          REP RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENEDI          EDI BACKUP                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENINV          INVENTORY                                    
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENMKG          MAKEGOOD                                     
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCFC          CONTRACT CONFIRM COMMENT                     
         EJECT                                                                  
       ++INCLUDE REGENAUR          AUR       RECORD                             
         EJECT                                                                  
       ++INCLUDE REGENPRO          PROPOSAL  RECORD                             
         EJECT                                                                  
       ++INCLUDE REGENSDD          DAYPART DEFINITION                           
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
         SPACE 4                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'135REREPSW03C05/01/02'                                      
         END                                                                    
