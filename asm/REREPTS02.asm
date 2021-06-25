*          DATA SET REREPTS02  AT LEVEL 164 AS OF 02/01/00                      
*PHASE RETS02A,*                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE RECUP                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PERVERT                                                                
*INCLUDE XSORT                                                                  
         TITLE 'REREPTS02  (RETS02A) --- EXTRACT REP RTS BUY DATA'              
*                                                                               
********************************************************************            
*                                                                  *            
*                                                                  *            
*        REREPTS02  -- EXTRACT REP RTS BUY DATA FOR COMPARISON     *            
*                      WITH SPOT BUY DATA                          *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* AUG03/99 (BU ) --- ORIGINAL ENTRY, BASED ON SWITCHER (REREPSW02) *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*                                                                  *            
*                                                                  *            
*  LIST OF DISPLAYS (REQUESTOR VALUE TRIGGER = Y IN BYTE:)         *            
*     QUESTOR+0   =    Y   =   DISPLAY INPUT RECORDS               *            
*     QUESTOR+1   =    Y   =   DISPLAY OUTPUT RECORDS              *            
*     QUESTOR+2   =    Y   =   CUT OFF AFTER N RECORDS             *            
*     QUESTOR+3   =    Y   =   DISPLAY OUTPUT RECORDS              *            
*     QUESTOR+11  =    Y   =   DISPLAY PERFORMANCE TIME STAMP      *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
RETS02   CSECT                                                                  
         NMOD1 0,**RETS**,R8,R9,RR=R5                                           
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
         GOTO1 =A(INITIAL),DMCB,(RC),RR=Y                                       
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'8D'           SCAN KEY TYPE                                
MAIN0040 EQU   *                                                                
         GOTO1 HIGH                GET FIRST RECORD                             
         B     MAIN0100                                                         
MAIN0060 EQU   *                                                                
         MVC   KEY(27),STARTKEY    RESET KEY                                    
         GOTO1 HIGH                REREAD RECORD                                
*                                     THEN GET NEXT RECORD                      
MAIN0080 EQU   *                                                                
         GOTO1 SEQ                                                              
MAIN0100 EQU   *                                                                
*                                                                               
         CLI   KEY+16,2            SECOND KEY OF 3-SET?                         
         BNE   MAIN0120            NO  - DON'T COUNT IT                         
*                                                                               
         L     RF,CONCTR           PROCESSING COUNTERS                          
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR                                                        
         L     RF,PROCCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,PROCCTR                                                       
*                                                                               
         CLC   PROCCTR,=F'5000'    N CONTRACTS PROCESSED?                       
         BNE   MAIN0120            NO                                           
*                                                                               
         XC    PROCCTR,PROCCTR                                                  
         MVC   P+1(05),=C'CONS:'                                                
         EDIT  CONCTR,(8,P+08)                                                  
         MVC   P+19(05),=C'BUYS:'                                               
         EDIT  SORTCTR,(8,P+27)                                                 
         MVC   P+48(05),=C'KEY :'                                               
         MVC   P+56(27),KEY                                                     
         TIME  DEC                 RETRIEVE TIME: R0 AS HH:MM:SS:TT             
         ST    R0,WORK                                                          
         GOTO1 HEXOUT,DMCB,WORK,P+37,4,=C'TOG'                                  
         GOTO1 REPORT                                                           
*                                                                               
MAIN0120 EQU   *                                                                
         CLI   KEY,X'8D'           ALL KEYS PROCESSED?                          
         BNE   MAIN0300            YES - FINISHED                               
         LA    R2,REPCODES         SET A(REPCODE TABLE)                         
MAIN0140 EQU   *                                                                
         CLI   0(R2),X'FF'         END OF TABLE REACHED?                        
         BE    MAIN0160            YES - REP NOT IN TABLE                       
         CLC   KEY+1(2),0(R2)      REP CODE IN TABLE?                           
         BE    MAIN0180            YES - PROCESS RECORD                         
         LA    R2,2(R2)            NO  - BUMP TO NEXT TABLE ENTRY               
         B     MAIN0140            GO BACK FOR NEXT                             
MAIN0160 EQU   *                                                                
         ZIC   RF,KEY+2            GET LOW CHAR OF REP CODE                     
         LA    RF,1(RF)            BUMP FOR SKIP READ                           
         STC   RF,KEY+2            PUT LOW CHAR BACK                            
         B     MAIN0040            SKIP-READ PAST REP CODE                      
MAIN0180 EQU   *                                                                
         CLC   KEY+10(2),=X'C721'  END DATE < SEP1/99?                          
         BL    MAIN0080            YES - GO BACK FOR NEXT                       
         CLI   KEY+16,2            SECOND KEY OF 3-SET?                         
         BNE   MAIN0080            NO  - GO BACK AND GET NEXT                   
*                                                                               
         CLI   KEY+20,C'N'         YES - RTS CONTYPE?                           
         BE    MAIN0200            YES - PROCESS IT                             
         CLI   KEY+20,C'X'         NO  - RTS CONTYPE?                           
         BE    MAIN0200            YES - PROCESS IT                             
         CLI   RTSCTYPE,C'O'       YES - RTS CONTYPE OPTIONAL USED?             
         BE    MAIN0080            NO  - SKIP THIS CONTRACT                     
         CLC   KEY+20(1),RTSCTYPE  YES - RTS CONTYPE OPTIONAL FOUND?            
         BNE   MAIN0080            NO  - SKIP THIS CONTRACT                     
MAIN0200 EQU   *                                                                
         MVC   STARTKEY,KEY        SAVE KEY FOR RESTART                         
         XC    KEY,KEY             CLEAR KEY FOR BUYLINE READ                   
         MVI   KEY,11              INSERT RECORD TYPE                           
         MVC   KEY+16,STARTKEY+1   INSERT REP CODE                              
*                                  COMPLEMENT/REVERSE CONTRACT NUMBER           
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),STARTKEY+12(4)                                        
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   KEY+18(4),WORK+15                                                
         PACK  KEY+18(1),WORK+18(1)    REVERSE THE COMPLIMENT                   
         PACK  KEY+19(1),WORK+17(1)                                             
         PACK  KEY+20(1),WORK+16(1)                                             
         PACK  KEY+21(1),WORK+15(1)                                             
**       MVC   P+1(10),=C'BUYLINE = '                                           
**       GOTO1 HEXOUT,DMCB,STARTKEY+12,P+15,4,=C'TOG'                           
**       GOTO1 HEXOUT,DMCB,KEY+18,P+25,4,=C'TOG'                                
**       GOTO1 REPORT                                                           
**       MVC   P+1(10),=C'NEW KEY = '                                           
**       MVC   P+12(27),KEY                                                     
**       MVC   P+45(12),=C'START KEY = '                                        
**       MVC   P+60(27),STARTKEY                                                
**       GOTO1 REPORT                                                           
*                                                                               
         GOTO1 HIGH                READ FIRST BUY RECORD                        
         B     MAIN0240                                                         
MAIN0220 EQU   *                                                                
         GOTO1 SEQ                                                              
MAIN0240 EQU   *                                                                
*                                                                               
*   TEST BUY READ                                                               
*        MVC   P+01(12),=C'BUY KEY   = '                                        
*        MVC   P+20(27),KEY                                                     
*        GOTO1 REPORT                                                           
*   TEST BUY READ                                                               
*                                                                               
         CLC   KEY(22),KEYSAVE     SAME CONTRACT NUMBER?                        
         BNE   MAIN0060            NO  - BUYS FINISHED -                        
*                                     GET NEXT CONTRACT                         
         GOTO1 GREC                YES - RETRIEVE BUY RECORD                    
*                                                                               
*   TEST BUY RECORD                                                             
*        MVC   P+01(12),=C'BUY RECORD= '                                        
*        MVC   P+20(27),RBUYREC                                                 
*        GOTO1 REPORT                                                           
*   TEST BUY RECORD                                                             
*                                                                               
         LA    R2,RBUYELEM         FIND X'08' ELT                               
MAIN0260 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD REACHED?                       
         BE    MAIN0220            YES - NO X'08' - READ NEXT                   
         CLI   0(R2),8             SPOTPAK INTERFACE ELEMENT?                   
         BE    MAIN0280            YES                                          
         ZIC   RF,1(R2)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R2,RF                                                            
         B     MAIN0260            GO BACK FOR NEXT ELEMENT                     
MAIN0280 EQU   *                                                                
         USING RBUYSPEL,R2                                                      
         XC    SORTREC,SORTREC                                                  
         MVC   SCLIENT,RBUYSPCL    INSERT CLIENT                                
         MVC   SPROD,RBUYSPPD      INSERT PRODUCT                               
         MVC   SESTIMAT,RBUYSPES   INSERT ESTIMATE                              
         MVC   SBUYLN#,RBUYSPL#    INSERT SPOTPAK BUYLINE #                     
         MVC   SSTATION,RBUYSPST   INSERT STATION                               
         MVC   SCON#,STARTKEY+12   INSERT REP CONTRACT NUMBER                   
         MVC   SREPBYL#,RBUYKLIN   INSERT REP LINE NUMBER                       
         MVC   SREPCOD,RBUYKREP    INSERT REP CODE                              
         MVC   SXFRDATE,RBUYSPDT   INSERT TRANSFER DATE                         
*                                                                               
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
*                                                                               
**       MVC   P+5(07),=C'SORTGEN' **TEST**                                     
**       MVC   P+15(23),SORTREC    **TEST**                                     
**       GOTO1 REPORT              **TEST**                                     
         L     RF,SORTCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,SORTCTR          ADD TO SORT OUT COUNT                        
*                                                                               
*   TEST                                                                        
****     C     RF,=F'1000'         END ON N RECORDS                             
****     BE    MAIN0300                                                         
*   TEST END                                                                    
         B     MAIN0220            GO BACK FOR NEXT BUY RECORD                  
*                                                                               
                                                                                
MAIN0300 EQU   *                                                                
         GOTO1 GETSORT                                                          
         CLI   STYP,X'FF'          END OF FILE REACHED?                         
         BNE   MAIN0400            NO                                           
         XC    REC,REC             PUT OF END OF FILE REC                       
         MVI   SORTREC,X'FF'       FILL REC WITH FOXES                          
         MVC   SORTREC+1(31),SORTREC                                            
         MVC   REC(32),SORTREC                                                  
         GOTO1 PUTRECS                                                          
         L     RF,TOTCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,TOTCTR                                                        
         B     MAIN0800                                                         
MAIN0400 EQU   *                                                                
         XC    REC,REC                                                          
         MVC   REC(32),SORTREC                                                  
         GOTO1 PUTRECS             YES                                          
         L     RF,TOTCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,TOTCTR                                                        
         B     MAIN0300            GO BACK FOR NEXT                             
         EJECT                                                                  
***>>>                                                                          
MAIN0800 EQU   *                                                                
         GOTO1 =A(DISPTOTS),DMCB,(RC),RR=Y                                      
*                                  DISPLAY TOTALS FOR RUN                       
*                                                                               
         CLOSE FILOUTA             YES - CLOSE OUTPUT FILES                     
MAIN0820 EQU   *                                                                
*                                                                               
MAIN0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*    SORT RETURN AND END-OF-FILE TESTING                                        
*                                                                               
GETSORT  NTR1                                                                   
         CLI   STYP,X'FF'          EOF REACHED?                                 
         BE    GSOR0080            YES                                          
         MVI   STYP,X'FF'                                                       
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         LTR   R6,R6                                                            
         BZ    GSOR0080            TEST RETURN ZERO=EOF                         
         MVC   SORTREC,0(R6)                                                    
*        CLI   SORTREC+STYP2,C'1'  **TEST**                                     
*        BNE   GSOR0080            **TEST**                                     
*        MVC   P+5(07),=C'GETSORT' **TEST**                                     
*        MVC   P+15(32),SORTREC    **TEST**                                     
*        GOTO1 REPORT              **TEST**                                     
GSOR0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*   INITIALIZATIONS ....                                                        
INITIAL  NTR1                                                                   
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         OPEN  (FILOUTA,(OUTPUT))                                               
INIT0010 EQU   *                                                                
         LA    RF,RECORD                                                        
         ST    RF,AIOAREA          SET INITIAL IO AREA                          
*                                                                               
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     RF,COVAIL                                                        
         DROP  RF                                                               
         GOTO1 (RF),DMCB,C'GET',500000,500000                                   
*                                  GET .5MEG STORAGE SPACE                      
         OC    P2(4),P2                                                         
         BNZ   INIT0020                                                         
         DC    H'0'                                                             
INIT0020 EQU   *                                                                
         L     RF,P2               INITIALIZE THE WORK AREA                     
         XCEFL 0(RF),P3                                                         
         MVC   LBLDAREA,P3         L(ADD'L SPACE GIVEN)                         
         MVC   ARECAREA,P2         TAPE RECORD DELIVERY AREA                    
*                                                                               
INIT0120 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,1               SET REP REC CODE TYPE                        
         MVC   KEY+25(2),QREP      INSERT REP CODE                              
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 MUST BE ON FILE                              
         DC    H'0'                                                             
         GOTO1 GREC                RETRIEVE REP RECORD                          
*                                                                               
**GET SUBREPS                                                                   
         MVC   RTSCTYPE,RREPPROF+11                                             
*                                  SAVE OPTIONAL REP CONTRACT TYPE              
         LA    R2,REPCODES         SET A(REPCODES TABLE)                        
         CLC   RREPMAST,=X'FFFF'    IS THIS A MASTER REP?                       
         BE    INIT0140             YES --READ ALL SUB REPS INTO TABLE          
         MVC   0(2,R2),QREP        NO  - INSERT SINGLE CODE IN TABLE            
         MVC   2(2,R2),=X'FFFF'    INSERT DELIMITER                             
         B     INIT0220                                                         
*                                                                               
INIT0140 DS    0H                                                               
*                                  FOR MASTER REP:                              
         SR    R0,R0                                                            
         LA    R1,RREPELEM         GET SUB REPS FROM REP RECORD                 
INIT0160 IC    R0,1(R1)            SEARCH FOR '02' ELEMENT                      
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BNE   *+6                 MUST BE ON FILE                              
         DC    H'0'                                                             
         CLI   0(R1),2                                                          
         BNE   INIT0160                                                         
*                                                                               
*  '02' ELEMENT FOUND:  EXTRACT COUNT, STORE CODES IN TABLE                     
*                                                                               
         ZIC   RF,2(R1)           RREPSCNT: # SUB REPS FOR MASTER               
         LR    R5,RF               SAVE FOR LOOP                                
         LA    R4,10(R1)           A(SUBSID REP CODES)                          
*                                                                               
*  SORT THE CODES IN THE REP RECORD BEFORE TABLING                              
*                                                                               
         GOTO1 XSORT,DMCB,(0,0(R4)),(RF),2,2,0                                  
*                                                                               
INIT0180 EQU   *                                                                
         MVC   0(2,R2),0(R4)       MOVE CODE TO TABLE                           
         CLC   0(2,R2),=C'BF'      BANNER IS KAPUT!                             
         BNE   INIT0190                                                         
         MVC   0(2,R2),=X'0000'    OVERWRITE IT                                 
INIT0190 EQU   *                                                                
         LA    R4,2(R4)            NEXT CODE IN REP REC                         
         LA    R2,2(R2)            NEXT TABLE ENTRY                             
         BCT   R5,INIT0180         LOAD ALL CODES                               
*                                                                               
INIT0200 DS    0H                                                               
         MVC   0(2,R2),=X'FFFF'    END MARKER - FOR REP TABLE                   
**GET SUBREPS                                                                   
INIT0220 DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
*  DISPIPUT:  DISPLAY THE RECORD LOCATED AT ARECAREA.                           
*                                                                               
DISPIPUT NTR1                                                                   
         GOTO1 REPORT                                                           
         L     R4,ARECAREA         A(RECORD)                                    
         ZICM  RF,0(R4),2          SET L(INPUT RECORD)                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 REPORT                                                           
DPUT0090 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  DISPTOTS:                                                     *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
DISPTOTS NTR1                                                                   
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVC   P+1(24),=C'INPUT RECORDS READ     :'                             
         EDIT  REDCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'OUTPUT RECORDS WRITTEN :'                             
         EDIT  TOTCTR,(12,P+30),COMMAS=YES                                      
         GOTO1 REPORT                                                           
DITO0010 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DISPPUT:  DISPLAY RECORD JUST 'PUT' TO OUTPUT.                *              
*                                                                *              
******************************************************************              
*                                                                               
DISPPUT  NTR1                                                                   
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         GOTO1 REPORT                                                           
DIPU0001 EQU   *                                                                
         MVC   P+1(8),=C'AGENCY  '                                              
         CLI   REC,X'0A'           AGENCY?                                      
         BE    DIPU0020            YES                                          
         CLI   REC,X'1A'           AGENCY?                                      
         BE    DIPU0020            YES                                          
         MVC   P+1(8),=C'ADVERT  '                                              
DIPU0020 EQU   *                                                                
         MVC   P+10(06),=C'OUTPUT'                                              
         GOTO1 REPORT                                                           
         LA    R4,REC-4            A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,REC-4            GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
DIPU0090 EQU   *                                                                
         GOTO1 REPORT                                                           
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  PUTRECS:  GENERATE OUTFILE ENTRIES                            *              
******************************************************************              
*                                                                               
PUTRECS  NTR1                                                                   
***      LH    RF,REC-4            ADD LENGTH OF 'LENGTH WORD'                  
*                                     TO RECORD CONTROL                         
***      LA    RF,4(RF)                                                         
***      STH   RF,REC-4            PUT IT BACK                                  
         LA    R0,REC                                                           
         PUT   FILOUTA,(R0)        PUT RECORD TO OUTPUT                         
PUTR0040 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
AIPFIELD DS    A                   INPUT FIELD ADDRESS                          
AIPFIEL2 DS    A                   INPUT FIELD ADDRESS                          
ARECAREA DS    A                   TAPE RECORD DELIVERY AREA                    
LBLDAREA DS    F                                                                
ASTAAREA DS    A                                                                
ASTANEXT DS    A                                                                
REDCTR   DS    F                                                                
TOTCTR   DS    F                                                                
STARCTR  DS    F                                                                
DISPCTR  DS    F                                                                
CORPCTR  DS    F                                                                
DUPECTR  DS    F                                                                
CORPOVER DS    F                                                                
AIOAREA  DS    F                                                                
ENDOFREC DS    A                   END OF RECORD                                
DATEWORK DS    CL48                DATE WORK AREA                               
COMMAND  DS    CL8                                                              
CALLOV   DS    A                                                                
RUNSTRT  DS    F                                                                
RUNEND   DS    F                                                                
OLDNAME  DS    CL18                OLD AGENCY NAME                              
OLDNAM2  DS    CL20                OLD ADVERT NAME                              
KEYDA    DS    CL4                 KEY DISK ADDRESS                             
WORK2    DS    CL256                                                            
REQWORKA DS    CL128                                                            
ELTBILD1 DS    CL64                                                             
ELTBILD2 DS    CL64                                                             
ELTBILD3 DS    CL64                                                             
RUNID    DS    CL5                                                              
USERID   DS    CL4                                                              
FOXZEROS DC    C'0000000'                                                       
FOXALPHA DC    20X'C0'                                                          
REPUSE   DS    CL2                 PLUG-IN REP VALUE                            
QUOTES   DS    CL1                                                              
SAVOFFIC DS    CL2                                                              
CORPDETL DS    CL1                                                              
TCODESAV DS    CL10                                                             
REPCODES DS    CL40                AREA FOR REP CODES                           
RTSCTYPE DS    CL1                 OPTIONAL RTS CONTRACT TYPE                   
STARTKEY DS    CL27                RESTART KEY SAVE                             
SORTCTR  DS    F                   RECORDS RELEASED TO SORT                     
PROCCTR  DS    F                   COUNT OF RECS IN PROCESS                     
CONCTR   DS    F                                                                
         SPACE 1                                                                
SORTREC  DS    0CL32                                                            
STYP     DS    CL1                 JIC:  SET TO ZERO                            
SCLIENT  DS    CL3                 SPOTPAK CLIENT CODE                          
SPROD    DS    CL3                 SPOTPAK PRODUCT CODE                         
SESTIMAT DS    CL1                 SPOTPAK ESTIMATE NUMBER                      
SSTATION DS    CL5                 STATION CALL LETTERS                         
SBUYLN#  DS    CL1                 SPOTPAK BUYLINE NUMBER                       
SCON#    DS    CL4                 REP CONTRACT NUMBER                          
SREPBYL# DS    CL1                 REP BUYLINE NUMBER                           
SREPCOD  DS    CL2                 REP CODE                                     
SXFRDATE DS    CL3                 TRANSFER DATE                                
         DS    CL8                 FILLER                                       
*                                                                               
         DS    0H                                                               
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,19,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=32'                                    
*                                                                               
ELCODE   DS    CL1                                                              
         SPACE 3                                                                
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=FB,MACRF=PM,              X        
               LRECL=40,BLKSIZE=6160,BUFNO=2                                    
*                                                                               
         SPACE 3                                                                
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL40                AREA FOR RECORD                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*  INCLUDE REGENBUY                BUY RECORD                                   
*  INCLUDE REGENCON                CONTRACT RECORD                              
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
FILED    DSECT                                                                  
RECORD   DS    CL1024                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENREPA         REP        RECORD                            
         ORG   RECORD                                                           
       ++INCLUDE REGENBUY          BUY        RECORD                            
         ORG   RECORD                                                           
       ++INCLUDE REGENADV          ADVERT     RECORD                            
         ORG                                                                    
         EJECT                                                                  
RECORD2  DS    CL4096                                                           
         ORG   RECORD2                                                          
       ++INCLUDE REGENCON          CONTRACT RECORD                              
         EJECT                                                                  
         ORG                                                                    
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE DDCOMFACSD                                                     
         EJECT                                                                  
       ++INCLUDE DMREQHDRA                                                      
         EJECT                                                                  
         CSECT                                                                  
*                                                                               
*********************************************************************           
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'164REREPTS02 02/01/00'                                      
         END                                                                    
