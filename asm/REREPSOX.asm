*          DATA SET REREPSOX   AT LEVEL 045 AS OF 03/18/15                      
*PHASE RESOXA                                                                   
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE SORTER                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE DMPRTQB                                                                
*INCLUDE DATCON                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'SOX FILE RECORD TURNAROUND REPORT'                              
*********************************************************************           
* HISTORY OF CHANGES                                                *           
*********************************************************************           
* AUG10/04 (BU ) --- ORIGINAL ENTRY                                 *           
*                                                                   *           
* MAY11/05 (BU ) --- FIX LEN OF ZIP CODE FIELD                      *           
*                                                                   *           
* MAY19/05 (BU ) --- FIX PRINT QUEUE ASSIGNMENT PROBLEM             *           
*                                                                   *           
*********************************************************************           
* NOTES FOR USE OF THIS PROGRAM:                                    *           
* THIS PROGRAM IS PRIMARILY TABLE-DRIVEN.  AS SUCH, CHANGES TO IT   *           
*        INVOLVE THE MODIFICATION OF TABLES. OCCASIONALLY A NEW CASE*           
*        ARISES WHERE A SPECIAL ROUTINE WILL REQUIRE NEW CODING.    *           
*                                                                   *           
* TABLE DESCRIPTIONS / INTERACTION                                  *           
* RECTYPE:  TABLE DESCRIBES EACH RECORD TYPE FOR WHICH ANALYSIS     *           
*        IS TO BE DONE. TO ADD A NEW ENTRY:                         *           
*        1.  ENTER THE RECORD CODE                                  *           
*        2.  ENTER A ROUTINE NAME FOR RECORD CODE, SUCH AS 'XXXTYPE'*           
*        3.  RECORD CODE MUST ALSO HAVE AN ENTRY IN TABLE 'HDRTABLE'*           
*                                                                   *           
* HDRTABLE:  EACH RECTYPE ENTRY HAS AN ENTRY HERE THAT CONTAINS:    *           
*        1.  GENERAL DISPLAY INFORMATION FOR RECORD CODE            *           
*        2.  DISPLACEMENT INFO FOR REP AND CODE IN KEY              *           
*        3.  LENGTH OF CODE IN KEY                                  *           
*                                                                   *           
* XXXTYPE:  ELEMENT TABLE FOR RECORD CODE                           *           
*        1.  ENTER ELEMENTS TO BE INTERROGATED IN TABLE             *           
*            (TABLE DESCRIPTION IS PROVIDED IN CODE)                *           
*        2.  INDICATE 'SPECIAL ROUTINE', IF NEEDED.  (A SPECIAL     *           
*            ROUTINE REFINES THE PROCESSING OF THAT PARTICULAR      *           
*            ELEMENT)                                               *           
*        3.  INDICATE 'CLEAR   ROUTINE', IF NEEDED.  (A CLEAR       *           
*            ROUTINE SETS TO ZERO ANY DATA WITHIN A PARTICULAR      *           
*            ELEMENT THAT SHOULDN'T BE CONSIDERED AS 'DIFFERENT'    *           
*            WHEN COMPARING TWO ELEMENTS.  THIS ELIMINATES LISTING  *           
*            ENTRIES WHEN AN ACTUAL CHANGE HASN'T TAKEN PLACE.)     *           
*        4.  INDICATE 'DISPLAY MESSAGE NUMBER' - THIS IS THE ENTRY  *           
*            IN TABLE 'XXXMSGS', WHERE XXX = RECORD TYPE            *           
*                                                                   *           
* XXXMSGS:   CORRESPOND TO MESSAGE NUMBER FIELD IN XXXTYPE TABLE.   *           
*        MESSAGE DESCRIBES THE ELEMENT IN GENERAL TERMS             *           
*                                                                   *           
* SPECIAL ROUTINES:  REFINEMENT OF PROCESSING OF AN ELEMENT         *           
*        1.  TABLE 'LOCAL' TO SPEC RTN CAN BREAK DOWN ELEMENT INTO  *           
*            LOGICAL COMPONENTS FOR COMPARISON (IE, ADDRESS FIELD)  *           
*        2.  'OPTION' FIELDS, EITHER 'BIT' OR 'FLAG', CAN BE DE-    *           
*            FINED AND PROCESSED IN TABULAR FORM.  A TABLE OF       *           
*            CORRESPONDING POSITIONAL LABELS MAY BE ASSOCIATED      *           
*            WITH THE COMPARISON, OR THE COMPARISON ITSELF MAY BE   *           
*            DONE, AND THE FIELDS THEMSELVES DISPLAYED IF DIFFERENT.*           
*            IN THIS CASE, A BINARY OPTIONS FIELD CAN BE EXPLODED   *           
*            INTO A 'Y/N' CHARACTER STRING.                         *           
*        3.  'NUMERIC' FIELDS MUST BE NORMALIZED FOR HANDLING IN    *           
*            A COMMON FIELD FOR DISPLAY.  NUMERIC FIELDS CANNOT     *           
*            BE MORE THAN FOUR POSITIONS IN LENGTH. AS REP HAS      *           
*            NEVER REALLY DONE PACKED ARITHMETIC, THERE ARE NO      *           
*            PROVISIONS IN THIS MODULE TO HANDLE THAT TYPE OF DATA. *           
*        4.  DATES CAN BE SPECIFIED AS EITHER BINARY OR COMPRESSED. *           
*                                                                   *           
*********************************************************************           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
RESOX    CSECT                                                                  
         PRINT NOGEN                                                            
         ENTRY UTL                                                              
         NBASE 0,**RESOX*,AACESAVE                                              
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING RESOX+4096,RC                                                    
         LA    R4,2048(RC)                                                      
         LA    R4,2048(R4)                                                      
         USING RESOX+8192,R4                                                    
*                                                                               
         SPACE 1                                                                
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         SPACE 1                                                                
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(2,COMDATE)                                
*                                                                               
*        DATE CARD TO OVERRIDE TODAY'S DATE?                                    
*                                                                               
DATE0010 GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   CARD(2),=C'/*'      TEST FOR END OF FILE                         
         BE    DATE0040                                                         
         CLC   CARD(5),=C'DATE='                                                
         BNE   DATE0020            GET DATE CARD DATE IN CMPRSD FORMAT          
         GOTO1 =V(DATCON),DMCB,(4,CARD+5),(2,COMDATE)                           
         B     DATE0010                                                         
DATE0020 EQU   *                                                                
         CLC   CARD(3),=C'ID='                                                  
         BNE   DATE0025                                                         
         MVC   SAVNAME,CARD+3                                                   
         B     DATE0010                                                         
DATE0025 EQU   *                                                                
         CLC   CARD(3),=C'OV='                                                  
         BNE   DATE0030                                                         
         MVC   SAVORID,CARD+3                                                   
         B     DATE0010                                                         
DATE0030 EQU   *                                                                
         CLC   CARD(6),=C'LOCAL='                                               
         BNE   DATE0010                                                         
         MVC   SAVLOCAL,CARD+6                                                  
         B     DATE0010                                                         
         SPACE 1                                                                
DATE0040 EQU   *                                                                
*                                                                               
*   OPEN CONTROL SYSTEM TO ACCESS CONTROL FILE IDENTIFICATION                   
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CONTROL',            +        
               =C'NCTFILE X',AREC,0                                             
         XC    WORK,WORK                                                        
         MVI   WORK,C'I'           FIND CONTROL FILE ID RECORD                  
         MVC   WORK+15(10),SAVNAME LOAD AGENCY NAME                             
         OC    WORK+15(10),SPACES  SET REMAINDER TO SPACES                      
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'CTFILE',WORK,AREC                 
*                                                                               
         CLI   8(R1),0             FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                MUST BE PRESENT                              
         L     R1,AREC                                                          
         CLC   WORK(25),0(R1)      CHECK THE KEY                                
         BE    *+6                 SAME - OKAY                                  
         DC    H'0'                DIFFERS - DUMP IT OUT                        
         LA    R1,28(R1)           FIND SYS AUTHORIZATION ELEMENT               
CTRL0010 EQU   *                                                                
         CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   CTRL0020            NO                                           
         CLI   2(R1),X'08'         IS IT 'REP' SYSTEM?                          
         BE    CTRL0030            YES                                          
CTRL0020 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD                                
         BNE   CTRL0010            NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
CTRL0030 EQU   *                                                                
         MVC   SAVECUTL,UTL+4      SAVE CONTROL FILE UTL                        
         MVC   UTL+4(1),3(R1)      OVERRIDE CONTROL FILE UTL                    
*                                     WITH REP UTL CODE                         
         MVC   SAVERUTL,UTL+4      SAVE REP     FILE UTL                        
*  OPEN REP FILE                                                                
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'REP',FLIST,AREC                   
         SPACE 1                                                                
         MVC   TITLE(26),=C'SOX FILE RECORD TURNAROUND'                         
         SPACE 1                                                                
         MVI   BYTE,0                                                           
         MVI   SORTSW,C'N'                                                      
         ZAP   LINE,=P'99'         FORCE PAGE CHANGE                            
         MVI   EOFSW,C'N'                                                       
         MVC   P2,SPACES                                                        
         SPACE 1                                                                
         B     IN0                                                              
         SPACE 1                                                                
AACESAVE DC    A(ACESAVE)                                                       
VREMOTEC DC    V(REMOTEC)                                                       
         EJECT                                                                  
*              SORT RECOVERY FILE                                               
         SPACE 2                                                                
IN0      OPEN  (RECVIN,(INPUT))                                                 
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         XC    SAVREC(64),SAVREC   CLEAR 1ST PART OF SAVE AREA                  
*                                                                               
*  ACTIVITY CONCERNING REP FILE RECORDS WILL BE SENT TO SORT                    
*                                                                               
RCIN0020 EQU   *                                                                
         LA    R0,RECVHDR-4                                                     
         GET   RECVIN,(0)                                                       
         SPACE 1                                                                
         CLI   RFILTY,X'82'        REP FILE?                                    
         BNE   RCIN0020                                                         
         OC    RTRM,RTRM           ANY TERMINAL ID IN RECORD HDR?               
         BZ    RCIN0020            NO  - OFFLINE UPDATE - SKIP IT               
*                                                                               
         LA    RF,TYPTABLE         SET A(RECORD TYPE TABLE)                     
RCIN0022 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE REACHED?                        
         BE    RCIN0020            YES - RECORD TYPE NOT FOUND                  
         CLC   RKEY(2),0(RF)       KEY-TYPE IN TABLE?                           
         BE    RCIN0024            YES                                          
         LA    RF,LTYPTABL(RF)     NO  - BUMP TO NEXT TABLE ENTRY               
         B     RCIN0022            GO BACK FOR NEXT                             
RCIN0024 EQU   *                                                                
         ST    RF,ATYPTABL         SAVE A(ENTRY IN TABLE)                       
***      CLI   RRECTY,2            CHANGE?                                      
***      BNE   RCIN0020                                                         
         SPACE 1                                                                
         LA    RE,RECVHDR-4                                                     
         AH    RE,0(RE)                                                         
         XC    0(2,RE),0(RE)       PUT ZERO AT END OF RECORD                    
RCIN0040 EQU   *                                                                
*&&DO                                                                           
         GOTO1 =V(PRINTER)                                                      
         MVC   P+1(13),=C'RECORD TYPE: '                                        
         L     RF,ATYPTABL         SET A(ENTRY IN TABLE)                        
         MVC   P+14(8),DRECNAME(RF)                                             
*                                  DISPLACE TO RECORD NAME                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R2,RECVHDR-4        A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         LH    RF,RECVHDR-4        GET LENGTH OF ENTRY                          
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,(0,(R2)),(R2),C'DUMP',(RF),=C'1D'                
*&&                                                                             
*                                                                               
RCIN0100 EQU   *                                                                
         XCEFL SRTREC,1100                                                      
         MVC   SRTRDAT,RDATE       INSERT RCV DATE                              
         MVC   SRTRTIM,RTIME       INSERT RCV TIME                              
         MVC   SRTRTTYP,RRECTY     INSERT RCV TRANS TYPE                        
         L     RF,ATYPTABL         SET A(TYPE RECORD TABLE ENTRY)               
         LA    RF,DRECREP(RF)      DISPLACE TO KEY DISPLACEMENT                 
         ZIC   R3,0(RF)            GET KEY DISPLACEMENT FOR REP                 
         LA    R1,RKEY                                                          
         AR    R1,R3               SET A(REP CODE IN KEY OF RCV REC)            
         BAS   RE,MASTREP          INSERT PROPER REP CODE IN KEY                
*                                                                               
         MVC   SRTRTYPE,RKEY       INSERT RECORD TYPE                           
         MVC   SRTRKEY(27),RKEY    INSERT RECORD KEY                            
*                                                                               
*   SORT KEY HAS 34 BYTES.  ONLY 27 ARE LOADED FROM RECORD.  LOAD OF            
*        MORE CAUSES RECORD LENGTH AND CONTROL TO INFLUENCE ORDER               
*        INCORRECTLY.                                                           
*                                                                               
*   DIDN'T REALLY NEED SRTRTYPE, EXCEPT FOR EXPLICIT USE                        
*                                                                               
*                                                                               
         LA    R0,SRTDATA          A(RECEIVING FIELD)                           
         LA    R1,1100             SET RECEIVING FIELD                          
         LA    RE,RKEY             A(SENDING FIELD)                             
         ZICM  RF,RKEY+27,2        SET RECORD LENGTH, PAD CHAR                  
         LA    RF,14(RF)           INCLUDE RECV HDR EXTENSION                   
         MVCL  R0,RE                                                            
*                                                                               
*&&DO                                                                           
         MVC   P+1(13),=C'SORT RECORD: '                                        
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R2,SRTREC           A(SORT RECORD)                               
         LA    RF,1100             LENGTH OF ENTRY                              
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,(0,(R2)),(R2),C'DUMP',(RF),=C'1D'                
*&&                                                                             
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRTREC                                   
         MVI   SORTSW,C'Y'                                                      
         B     RCIN0020            GET NEXT RECOVERY RECORD                     
         SPACE 1                                                                
RCIN0280 EQU   *                                                                
         CLOSE (RECVIN,)                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
*   RECOVERY DATA OUTPUT SECTION.                                               
*                                                                               
*   NEED TO MAKE SURE THAT THE LAST RECORD IS ACCOUNTED FOR  !!                 
*                                                                               
         CLI   SORTSW,C'Y'         IF NO RECORDS,                               
         BNE   RCOT0200            GO DIRECTLY TO CLOSE SORT                    
         SPACE 1                                                                
         XCEFL SAVREC,1100                                                      
RCOT0020 GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         L     R6,DMCB+4                                                        
         LTR   R6,R6                                                            
         BNZ   RCOT0040                                                         
         MVI   EOFSW,C'Y'          SET 'END OF FILE'                            
         B     RCOT0200                                                         
         SPACE 1                                                                
RCOT0040 EQU   *                                                                
         LA    R0,SRTREC           A(RECEIVING FIELD)                           
         LA    R1,1100             SET RECEIVING FIELD                          
         LA    RE,0(R6)            A(SENDING FIELD)                             
         LA    RF,1100             SET RECORD LENGTH, PAD CHAR                  
         MVCL  R0,RE                                                            
*                                                                               
         CLC   LASTREP,SRTREP      SAME REP?                                    
         BE    RCOT0050            YES                                          
         CLC   LASTREP,=C'  '       FIRST TIME?                                 
         BNE   RCOT0045            NO                                           
         MVI   CLOSEFLG,C'N'       YES - SET 'DONT CLOSE FILE'                  
RCOT0045 EQU   *                                                                
         MVC   LASTREP,SRTREP      NO  - SAVE NEW REP                           
         GOTO1 SETQUEUE,DMCB,SRTREP                                             
RCOT0050 EQU   *                                                                
*                                                                               
*                                                                               
*&&DO                                                                           
*   DISPLAY ALL RECORDS BEING READ.                                             
         MVC   P+1(07),=C'SORTIN:'                                              
         MVC   P+12(45),SRTREC                                                  
         GOTO1 =V(PRINTER)                                                      
         LA    R2,SRTREC           A(SORT RECORD)                               
         LA    RF,1100             LENGTH OF ENTRY                              
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,(0,(R2)),(R2),C'DUMP',(RF),=C'1D'                
*&&                                                                             
         CLI   SRTRTTYP,X'03'      'ADD' RECORD?                                
         BNE   RCOT0060            NO  -                                        
         BAS   RE,SAVERECD         SAVE THIS SORT RECORD                        
         BAS   RE,DISHEADR         DISPLAY RECORD TYPE INFORMATION              
         MVC   P+45(05),=C'ADDED'                                               
         GOTO1 =V(PRINTER)         PRINT RECORD ADDED MESSAGE                   
         GOTO1 =V(PRINTER)                                                      
         B     RCOT0020            GO BACK FOR NEXT RECORD                      
*                                                                               
*   AN 'ADD' FOR A RECORD WILL ALWAYS BE THE FIRST RECORD OF A                  
*        GROUP.  IT'S TOUGH TO CHANGE A RECORD THAT DOESN'T ALREADY             
*        EXIST.  DATE AND TIME SHOULD BE SUFFICIENT TO ENSURE IT SORTS          
*        FIRST.                                                                 
*                                                                               
RCOT0060 EQU   *                                                                
         OC    SAVREC(16),SAVREC   FIRST TIME?                                  
         BNZ   RCOT0100            NO                                           
*                                  YES                                          
RCOT0080 EQU   *                   MOVE SORT REC TO SAVE AREA                   
         BAS   RE,SAVERECD         SAVE THE SORT RECORD                         
         B     RCOT0020            GO BACK AND READ NEXT RECORD                 
RCOT0100 EQU   *                                                                
         CLC   SRTREC(SRTKYLEN),SAVREC                                          
*                                  SAME KEY FOUND (45 CHARS)?                   
         BE    RCOT0020            YES - SKIP THIS ONE                          
*                                     (SAME RECORD - DON'T PROCESS)             
         CLC   SRTREC(SRTCOMP),SAVREC                                           
*                                  SAME KEY FOUND?                              
         BNE   RCOT0080            NO  - KEY BREAK                              
         CLI   SRTRTTYP,X'01'      YES - 'COPY' RECORD?                         
         BE    RCOT0020            YES - NOT 1ST COPY OF GROUP                  
*                                     THEREFORE, NOT NEEDED                     
         BAS   RE,COMPARE          NO  - DO COMPARISON FOR REPORT               
         B     RCOT0080            MOVE NEW RECORD TO SAVREC                    
*                                                                               
***>>>   GOTO1 =V(PRNTBL),DMCB,=C'SORTOUT',SRTREC,C'DUMP',1101,=C'1D'           
*                                                                               
         SPACE 1                                                                
RCOT0200 GOTO1 =V(SORTER),DMCB,=C'END'                                          
         GOTO1 =V(PRINT),DMCB,=C'CLOSE'                                         
*                                  CLOSE FINAL PRINT FILE                       
         B     EOJ                 REPORT IS DONE                               
         EJECT                                                                  
*                                                                               
*   SAVERECD: MOVE SORT RECORD TO SAVE AREA                                     
*                                                                               
SAVERECD NTR1                                                                   
         LA    R0,SAVREC           SET A(RECEIVING FIELD)                       
         LA    R1,1100             SET RECEIVING FIELD                          
         LA    RE,SRTREC           A(SENDING FIELD)                             
         LA    RF,1100             SET RECORD LENGTH, PAD CHAR                  
         MVCL  R0,RE                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   MASTREP:  FOR REP TO BE INSERTED, LOOK IN TABLES FOR A MASTER CODE.         
*        THIS IS THE LAZY WAY TO DO THIS.  TABLES ARE HARDCODED, AS             
*        KATZ IS GOING AWAY, AND INTEREP WILL PROBABLY FOLLOW.                  
*        R1  ->  REP CODE IN RECORD                                             
*                                                                               
MASTREP  NTR1                                                                   
         MVC   SRTREP,0(R1)        INSERT RECORD REP CODE                       
         LA    RF,MASTABLE         SET A(REP TABLE)                             
MREP0020 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE?                                
         BE    MREP0900            YES - FINISHED                               
         CLC   SRTREP,DSUBREP(RF)  REP FOUND IN TABLE?                          
         BE    MREP0040            YES                                          
         LA    RF,LMASTABL(RF)     NO  - BUMP TO NEXT ENTRY                     
         B     MREP0020                                                         
MREP0040 EQU   *                                                                
         MVC   SRTREP,DMASREP(RF)  INSERT MASTER REP INTO SRTREC                
MREP0900 EQU   *                                                                
         XIT1                                                                   
*                                                                               
DSUBREP  EQU   0                                                                
DMASREP  EQU   2                                                                
MASTABLE DC    C'MGIR'                                                          
LMASTABL EQU   *-MASTABLE                                                       
         DC    C'IFIR'                                                          
         DC    C'I2IR'                                                          
         DC    C'D4IR'                                                          
         DC    C'CNIR'                                                          
         DC    C'L7IR'                                                          
         DC    C'IBIR'                                                          
         DC    C'NXIR'                                                          
         DC    C'UOIR'                                                          
         DC    C'K4K3'                                                          
         DC    C'KUK3'                                                          
         DC    C'KFK3'                                                          
         DC    C'CRK3'                                                          
         DC    C'K6K3'                                                          
         DC    C'S3K3'                                                          
         DC    C'RSK3'                                                          
         DC    C'QDK3'                                                          
         DC    C'G8K3'                                                          
         DC    C'J0K3'                                                          
         DC    C'WCK3'                                                          
         DC    C'AMMR'                                                          
         DC    C'CQMR'                                                          
         DC    X'0000'                                                          
         DS    0F                                                               
         EJECT                                                                  
*                                                                               
*   COMPARE:  ROUTINE DETERMINES TYPE OF RECORD IN PROGRESS,                    
*        COMPARES FIRST RECORD (SAVREC) WITH LAST RECORD (SRTREC)               
*        AND REPORTS ON CHANGE.                                                 
*        AT END, SAVREC IS CLEARED TO INDICATE A KEY BREAK.                     
*                                                                               
COMPARE  NTR1                                                                   
         MVI   DETAILS,0           CLEAR DETAILS PRINTED FLAG                   
         BAS   RE,DISHEADR         DISPLAY RECORD TYPE INFORMATION              
         MVC   P+45(07),=C'CHANGED'                                             
         MVC   SAVEHDR,P           SAVE HEADER - DON'T PRINT YET                
         MVC   P,SPACES            CLEAR PRINT LINE                             
*&&DO                                                                           
*   TEST SHOW SAVEHDR AREA                                                      
         MVC   P+1(08),=C'SET HDR:'                                             
         MVC   P+10(96),SAVEHDR                                                 
         GOTO1 =V(PRINTER)                                                      
*   TEST SHOW SAVEHDR END                                                       
*&&                                                                             
*                                                                               
*&&DO                                                                           
         MVC   P+1(07),=C'COMP  :'                                              
         MVC   P+12(45),SAVREC                                                  
         GOTO1 =V(PRINTER)                                                      
         MVC   P+12(45),SRTREC                                                  
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
*&&                                                                             
*                                                                               
*&&DO                                                                           
         CLC   PRNTCTR,=F'5'       DISPLAY FIRST 5 INPUTS                       
         BH    TEST0020                                                         
*                                                                               
         L     RF,PRNTCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,PRNTCTR                                                       
*                                                                               
         MVC   P+1(06),=C'SAVREC'                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R2,SAVREC           A(SAVE RECORD)                               
         LA    RF,1100             LENGTH OF ENTRY                              
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,(0,(R2)),(R2),C'DUMP',(RF),=C'1D'                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P+1(06),=C'SRTREC'                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R2,SRTREC           A(SAVE RECORD)                               
         LA    RF,1100             LENGTH OF ENTRY                              
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,(0,(R2)),(R2),C'DUMP',(RF),=C'1D'                
         GOTO1 =V(PRINTER)                                                      
TEST0020 EQU   *                                                                
*&&                                                                             
         L     RF,=A(RECTYPE)      SET A(RECTYPE TABLE)                         
*                                                                               
CPAR0020 EQU   *                                                                
         CLI   DRECTYPE(RF),0      END OF TABLE REACHED?                        
         BNE   CPAR0030            NO                                           
*&&DO                                                                           
         MVC   P+1(20),=C'NO RECTYPE TAB ENTRY'                                 
         MVC   P+22(45),SRTREC                                                  
         GOTO1 =V(PRINTER)                                                      
*&&                                                                             
         B     CPAR0900            EXIT ROUTINE.  IF RECORD NOT                 
*                                     IN TABLE, SKIP IT                         
CPAR0030 EQU   *                                                                
         CLC   SRTRTYPE,DRECTYPE(RF)  RECORD TYPE FOUND IN TABLE?               
         BE    CPAR0040            YES                                          
         LA    RF,LRECTYPE(RF)     NO  - BUMP TO NEXT SLOT                      
         B     CPAR0020            GO BACK FOR NEXT                             
CPAR0040 EQU   *                                                                
         L     RE,DTYPMSGS(RF)     SET A(MESSAGE TABLE FOR RECTYPE)             
         ST    RE,ATYPMSGS         SAVE A(MSG TAB)                              
         L     R3,DTYPETAB(RF)     LOAD A(TYPE TABLE FOR RECORD)                
         ST    R3,ATABTYPE         SAVE A(TYPE TABLE ENTRY)                     
*                                                                               
*   RUN COMPARISON OLD VS NEW                                                   
*                                                                               
         LA    R5,SRTDATA-SRTREC+SAVREC                                         
*                                  SET A(OLD RECORD)                            
         LA    R7,SRTDATA          SET A(NEW RECORD)                            
         MVI   COMPTEST,C'A'       SET OLD VS NEW FLAG                          
*                                                                               
         GOTO1 CYCLEREC,DMCB,(R3),(R5),(R7)   PERFORM COMPARISON                
CPAR0900 EQU   *                                                                
         CLI   DETAILS,1           DETAILS PRINTED?                             
         BNE   CPAR0910                                                         
         GOTO1 =V(PRINTER)                                                      
CPAR0910 EQU   *                                                                
*                                  FINISHED PROCESSING KEY GROUP                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CYCLEREC:                                                                   
*        USE STATION ELEMENT TABLE TO LOOK FOR AND COMPARE ELTS                 
*        WITHIN RECORDS.                                                        
*        OLD VS NEW - COMPARE ELEMENTS THAT EXIST WITHIN BOTH RECS              
*                     DETERMINE ELEMENTS DROPPED IN NEW FROM OLD                
*        NEW VS OLD - DETERMINE ELEMENTS ADDED   IN NEW FROM OLD                
*                                                                               
*        R3   =>  ELEMENT TABLE FOR RECORD TYPE                                 
*        R5   =>  'OLD' RECORD                                                  
*        R7   =>  'NEW' RECORD                                                  
*        COMPTEST  =  'A'  - CHANGES/DELETIONS FROM OLD TO NEW                  
*        COMPTEST  =  'B'  - ADDITIONS TO NEW FROM OLD                          
*                                                                               
CYCLEREC NTR1                                                                   
         L     R3,0(R1)            SET A(ELEMENT TABLE)                         
         MVC   AOLDREC,4(R1)       SAVE A('OLD' RECORD)                         
         MVC   ANEWREC,8(R1)       SAVE A('NEW' RECORD)                         
*                                                                               
CREC0020 EQU   *                                                                
         CLI   DTABRTYP(R3),0      END OF ELEMENT TABLE?                        
         BE    CREC0900            YES - COMPARISON FINISHED                    
         L     R5,AOLDREC                                                       
OLD      USING RSTAREC,R5                                                       
         LA    R5,OLD.RSTAELEM         SET A(DESCRIPTOR ELEMENT)                
         DROP  OLD                                                              
*                                                                               
*   USING REGENSTA DSECT FOR INITIAL POSITIONING.  REST OF CODING               
*        IS GENERIC                                                             
*                                                                               
         L     R7,ANEWREC                                                       
NEW      USING RSTAREC,R7                                                       
         LA    R7,NEW.RSTAELEM         SET A(DESCRIPTOR ELEMENT)                
         DROP  NEW                                                              
         GOTO1 ELTCOMP,DMCB,(R3),(R5),(R7)                                      
         LA    R3,LELTTYPE(R3)     BUMP TO NEXT ELT ENTRY                       
*                                                                               
         B     CREC0020            GO BACK FOR NEXT ELEMENT                     
CREC0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   ELTCOMP:  A(DESCRIPTOR ELEMENTS) OF BOTH RECORDS ARE PASSED IN.             
*        CORRESPONDING ELEMENTS IN BOTH RECORDS ARE FOUND AND                   
*        COMPARED.                                                              
*        DIFFERENCES ARE FLAGGED ON THE REPORT.                                 
*             A.  ELEMENTS DIFFER                                               
*             B.  ELEMENT DELETED FROM 'NEW'                                    
*             C.  ELEMENT ADDED   TO   'NEW'                                    
*                                                                               
ELTCOMP  NTR1                                                                   
         L     R3,0(R1)            SET A(ELEMENT TABLE ENTRY)                   
*&&DO                                                                           
*   TEST                                                                        
         MVC   P+1(08),=C'ELTCOMP:'                                             
         MVC   P+10(LELTTYPE),0(R3)                                             
         GOTO1 =V(PRINTER)                                                      
*   TEST END                                                                    
*&&                                                                             
         L     R5,4(R1)            SET A('OLD' RECORD DESCRIPTOR)               
         L     R7,8(R1)            SET A('NEW' RECORD DESCRIPTOR)               
*                                                                               
*   FIND FIRST ELEMENT IN 'OLD' RECORD                                          
*                                                                               
         SR    R1,R1                                                            
         XC    AOLDADD1(8),AOLDADD1                                             
ELTC0020 EQU   *                                                                
         CLI   0(R5),0             END OF RECORD?                               
         BE    ELTC0040            END ELTS IN 'OLD' RECORD                     
         CLC   0(1,R5),DTABRTYP(R3)    RECORD ELT = TABLE ELT?                  
         BNE   ELTC0030            NO  - BUMP TO NEXT ELEMENT                   
         LA    R1,1(R1)            INCREMENT COUNTER                            
         OC    AOLDADD1,AOLDADD1   ANY ADDRESS?                                 
         BNZ   ELTC0030            YES - DON'T SAVE                             
         ST    R5,AOLDADD1         NO  - SAVE ADDR OF 1ST ELT OF TYPE           
ELTC0030 EQU   *                                                                
         ZIC   RF,1(R5)            BUMP TO NEXT ELEMENT                         
         AR    R5,RF                                                            
         B     ELTC0020            GO BACK FOR NEXT IN 'OLD' REC                
ELTC0040 EQU   *                                                                
         ST    R1,AOLDELT#         SAVE COUNT OF OLD ELTS                       
         SR    R1,R1               RESET COUNTER TO ZERO                        
ELTC0050 EQU   *                                                                
         CLI   0(R7),0             END OF RECORD?                               
         BE    ELTC0070            END ELTS IN 'NEW' RECORD                     
         CLC   0(1,R7),DTABRTYP(R3)    RECORD ELT = TABLE ELT?                  
         BNE   ELTC0060            NO  - BUMP TO NEXT ELEMENT                   
         LA    R1,1(R1)            INCREMENT COUNTER                            
         OC    ANEWADD1,ANEWADD1   ANY ADDRESS?                                 
         BNZ   ELTC0060            YES - DON'T SAVE                             
         ST    R7,ANEWADD1         NO  - SAVE ADDR OF 1ST ELT OF TYPE           
ELTC0060 EQU   *                                                                
         ZIC   RF,1(R7)            BUMP TO NEXT ELEMENT                         
         AR    R7,RF                                                            
         B     ELTC0050            GO BACK FOR NEXT IN 'OLD' REC                
ELTC0070 EQU   *                                                                
         ST    R1,ANEWELT#         SAVE COUNT OF NEW ELTS                       
*&&DO                                                                           
*   TEST                                                                        
         MVC   P+1(08),=C'ELTS   :'                                             
         EDIT  AOLDELT#,(2,P+12)                                                
         EDIT  ANEWELT#,(2,P+15)                                                
         MVC   P+20(4),AOLDADD1                                                 
         MVC   P+26(4),ANEWADD1                                                 
         GOTO1 =V(PRINTER)                                                      
*   TEST END                                                                    
*&&                                                                             
         ZICM  RF,DTABMSG#(R3),2   RETRIEVE FIELD MESSAGE NUMBER                
         BCTR  RF,0                MAKE ZERO RELATIVE                           
         SLL   RF,5                MULTIPLY BY 32                               
         L     RE,ATYPMSGS         SET A(LABEL TABLE FOR REC TYPE)              
         AR    RE,RF               DISPLACE TO PROPER LABEL MESSAGE             
         MVC   P+10(32),0(RE)      MOVE LABEL TO PRINT LINE                     
         MVI   P+43,C'-'                                                        
*&&DO                                                                           
*   TEST                                                                        
         LA    R0,1                                                             
         LNR   R0,R0                                                            
         DC    H'0'                                                             
*   TEST END                                                                    
*&&                                                                             
         CLC   AOLDELT#,ANEWELT#   SAME NUMBER OF ELEMENTS?                     
         BE    ELTC0100            YES -                                        
*                                                                               
*   ELEMENTS CONTAINING PROFILES WILL NOT BE MULTIPLES                          
*                                                                               
         BH    ELTC0080            MORE 'OLD' THAN 'NEW'                        
         MVC   P+45(23),=C'ENTRIES CHANGED/ADDED  '                             
         B     ELTC0200                                                         
ELTC0080 EQU   *                                                                
         MVC   P+45(23),=C'ENTRIES CHANGED/DELETED'                             
         B     ELTC0200                                                         
ELTC0100 EQU   *                                                                
*                                                                               
*   SAME NUMBER OF ELEMENTS                                                     
*                                                                               
                                                                                
         L     R5,AOLDADD1         SET A(1ST ELEMENT IN OLD)                    
         LTR   R5,R5               ANY COUNT IN REG?                            
         BZ    ELTC0400            NO  - EXIT RTN                               
ELTC0110 EQU   *                                                                
         L     R7,ANEWADD1         SET A(1ST ELEMENT IN NEW)                    
         L     R0,AOLDELT#         SET COUNT OF ELEMENTS                        
ELTC0120 EQU   *                                                                
*&&DO                                                                           
*   TEST                                                                        
         ZIC   RF,1(R5)            SET L(OLD ELT)                               
         BCTR  RF,0                BACK OFF 1 FOR EX                            
         MVC   P+1(08),=C'OLD>NEW:'                                             
         EX    RF,ELTC012A         MOVE ELT FROM OLDREC                         
         GOTO1 =V(PRINTER)                                                      
         ZIC   RF,1(R5)            SET L(OLD ELT)                               
         BCTR  RF,0                BACK OFF 1 FOR EX                            
         EX    RF,ELTC012B         MOVE ELT FROM NEWREC                         
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
         B     ELTC0130                                                         
ELTC012A MVC   P+12(0),0(R5)                                                    
ELTC012B MVC   P+12(0),0(R7)                                                    
ELTC0130 EQU   *                                                                
*   TEST END                                                                    
*&&                                                                             
         L     RF,DCLRRTN(R3)      CHECK FOR CLEARANCE ROUTINE                  
         LTR   RF,RF               ANY ROUTINE?                                 
         BZ    ELTC0135            NO                                           
         GOTO1 (RF),DMCB,(R5),(R7) YES - BRANCH TO SPECIAL CLEAR                
ELTC0135 EQU   *                                                                
         ZIC   RF,1(R5)            SET L(OLD ELT)                               
         BCTR  RF,0                BACK OFF 1 FOR EX                            
*                                                                               
         EX    RF,ELTC0160         COMPARE BY L(OLD ELT)                        
         BNE   ELTC0180            NOT EQUAL:  MESSAGE                          
         BCT   R0,ELTC0140         DO ALL ELEMENTS                              
         B     ELTC0400            FINISHED                                     
ELTC0140 EQU   *                                                                
         ZIC   RF,1(R5)            BUMP TO NEXT OLD ELEMENT                     
         AR    R5,RF                                                            
         ZIC   RF,1(R7)            BUMP TO NEXT NEW ELEMENT                     
         AR    R7,RF                                                            
         BCT   R0,ELTC0120         GO BACK FOR NEXT COMPARISON                  
         B     ELTC0400            NO ELEMENTS LEFT: FINISHED                   
ELTC0160 CLC   0(0,R5),0(R7)       COMPARE ELEMENTS BY LNGTH                    
ELTC0180 EQU   *                                                                
*&&DO                                                                           
*   TEST SHOW SAVEHDR AREA                                                      
         MVC   P+1(08),=C'SAVEHDR:'                                             
         MVC   P+10(96),SAVEHDR                                                 
         GOTO1 =V(PRINTER)                                                      
*   TEST SHOW SAVEHDR END                                                       
*&&                                                                             
         CLC   SAVEHDR,SPACES      ANYTHING IN HEADER LINE?                     
         BNH   ELTC0190            NO                                           
         MVC   SAVEHDR2,P          YES - SAVE P-LINE #1                         
         MVC   P,SAVEHDR           LOAD RECORD HEADER FOR DISPLAY               
         GOTO1 =V(PRINTER)         DISPLAY RECORD HEADER                        
         MVC   SAVEHDR,SPACES      CLEAR RECORD HEADER                          
         MVC   P,SAVEHDR2          RELOAD LINE TO BE PRINTED                    
ELTC0190 EQU   *                                                                
         MVI   DETAILS,1           SET DETAILS PRINTED                          
         MVC   P+45(15),=C'ENTRIES CHANGED'                                     
         GOTO1 =V(PRINTER)                                                      
         CLI   DTABSPEC(R3),C'Y'   SPECIAL PROCESSING FOR THIS ELT?             
         BNE   ELTC0400            NO                                           
         L     RF,DTABRTN(R3)                                                   
         GOTO1 (RF),DMCB,(R5),(R7)                                              
         B     ELTC0400                                                         
ELTC0200 EQU   *                                                                
         CLC   SAVEHDR,SPACES      ANYTHING IN HEADER LINE?                     
         BNH   ELTC0220            NO                                           
         MVC   SAVEHDR2,P          YES - SAVE P-LINE #1                         
         MVC   P,SAVEHDR           LOAD RECORD HEADER FOR DISPLAY               
         GOTO1 =V(PRINTER) DISPLAY RECORD HEADER                                
         MVC   SAVEHDR,SPACES      CLEAR RECORD HEADER                          
         MVC   P,SAVEHDR2          RELOAD LINE TO BE PRINTED                    
ELTC0220 EQU   *                                                                
         MVI   DETAILS,1           SET DETAILS PRINTED                          
         GOTO1 =V(PRINTER)         PRINT DETAIL LINE                            
ELTC0400 EQU   *                                                                
         CLI   DETAILS,0           HEADER DETAILS PRINTED?                      
         BE    ELTC0420            NO  - LEAVE HEADER                           
         MVC   SAVEHDR,SPACES      CLEAR RECORD HEADER IN CASE NO               
*                                     DETAILS WERE PRINTED                      
ELTC0420 EQU   *                                                                
         MVC   P,SPACES            CLEAR PRINT LINE IN CASE LINE                
*                                     WAS NOT PRINTED                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   DISHEADR:  DESCRIPTIVE INFORMATION CONCERNING RECORD IS                     
*        DISPLAYED.  RECORD IS IN 'SAVREC'                                      
*                                                                               
DISHEADR NTR1                                                                   
         L     RF,=A(HDRTABLE)     SET A(DESCRIPTIVE TABLE)                     
DISH0020 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE REACHED?                        
         BNE   DISH0030            NO  -                                        
*                                                                               
         MVC   P+1(17),=C'REC NOT IN TABLE:'                                    
         MVC   P+20(64),SAVREC                                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         B     DISH0900            YES - RECORD NOT IN TABLE                    
DISH0030 EQU   *                                                                
         CLC   SAVRTYPE,DHDRTYPE(RF)                                            
*                                  NO  - SAVREC TYPE = TABLE TYPE?              
         BE    DISH0040            YES - PROCESS IT                             
         LA    RF,LHDRTABL(RF)     NO  - BUMP TO NEXT ENTRY                     
         B     DISH0020            GO BACK FOR NEXT                             
DISH0040 EQU   *                                                                
         MVC   P+04(16),DHDRLABL(RF)                                            
*                                  MOVE REC TYPE DESCRIP TO PRINT               
         LA    R3,SAVDATA          SET A(DATA IN RECV RECD)                     
         ZIC   RE,DHDRREP(RF)      GET DISPLACEMENT TO REP IN KEY               
         AR    RE,R3                                                            
         MVC   P+22(07),=C'FOR REP'                                             
         MVC   P+30(2),0(RE)       INSERT REP                                   
         ZIC   RE,DHDRKTYP(RF)     GET DISPLACEMENT TO CODE IN KEY              
         AR    RE,R3                                                            
         ZIC   R1,DHDRKLEN(RF)     GET LENGTH OF MOVE                           
         BCTR  R1,0                BACK OFF 1 FOR MOVE                          
         LA    R6,P+04             GET A(REC TYPE DESC ON PLINE)                
         LA    R0,16                                                            
DISH0060 EQU   *                                                                
         CLI   0(R6),C'>'          INSERTION POINT FOR CODE?                    
         BE    DISH0080            YES                                          
         LA    R6,1(R6)                                                         
         BCT   R0,DISH0060         GO BACK FOR NEXT                             
         DC    H'0'                MUST BE INDICATOR FOR CODE                   
DISH0080 EQU   *                                                                
         EX    R1,DISH0990         MOVE BY LENGTH                               
         MVC   P+55(2),=C'BY'                                                   
         LA    RF,SRTDATA                                                       
         ZICM  RE,27(RF),2         RETRIEVE RECORD DATA LENGTH                  
         AR    RF,RE               SET A(END OF RECORD)                         
         MVC   P+58(8),6(RF)       INSERT LUID FROM RCV HDR EXTEN               
         MVC   P+67(2),=C'ON'                                                   
         GOTO1 =V(DATCON),DMCB,(3,SRTRDAT),(5,P+70)                             
         MVC   P+79(2),=C'AT'                                                   
         GOTO1 =V(HEXOUT),DMCB,SRTRTIM,WORK+20,4,=C'TOG'                        
         MVC   P+82(2),WORK+21     INSERT HOUR                                  
         MVC   P+85(2),WORK+23     INSERT MINUTE                                
         MVC   P+88(2),WORK+25     INSERT SECOND                                
         MVI   P+84,C':'                                                        
         MVI   P+87,C':'                                                        
DISH0900 EQU   *                                                                
         XIT1                                                                   
DISH0990 EQU   *                                                                
         MVC   0(0,R6),0(RE)       MOVE CODE BY LENGTH                          
         EJECT                                                                  
EOJ      XBASE                                                                  
         EJECT                                                                  
*                                                                               
*   REP     X'01' ELEMENT: SPECIAL PROCESSING ROUTINE                           
*        P1  =  A(OLD ELEMENT)                                                  
*        P2  =  A(NEW ELEMENT)                                                  
*                                                                               
*                                                                               
REPDESC  NTR1                                                                   
         L     R5,0(R1)            SET A(OLD ELEMENT)                           
         L     R7,4(R1)            SET A(NEW ELEMENT)                           
         L     R2,=A(REP01ELT)     SET A(ELEMENT COMPARE TABLE)                 
         GOTO1 ELTDETLS,DMCB,(R2),(R5),(R7)                                     
*                                  PROCESS ELEMENT DETAILS                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   REP     X'04' ELEMENT: SPECIAL PROCESSING ROUTINE                           
*        P1  =  A(OLD ELEMENT)                                                  
*        P2  =  A(NEW ELEMENT)                                                  
*                                                                               
*                                                                               
REPPROF  NTR1                                                                   
         L     R5,0(R1)            SET A(OLD ELEMENT)                           
         L     R7,4(R1)            SET A(NEW ELEMENT)                           
         L     R2,=A(REP04ELT)     SET A(ELEMENT COMPARE TABLE)                 
         GOTO1 ELTDETLS,DMCB,(R2),(R5),(R7)                                     
*                                  PROCESS ELEMENT DETAILS                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   STATION X'01' ELEMENT: SPECIAL PROCESSING ROUTINE                           
*        P1  =  A(OLD ELEMENT)                                                  
*        P2  =  A(NEW ELEMENT)                                                  
*                                                                               
*                                                                               
STADESC  NTR1                                                                   
         L     R5,0(R1)            SET A(OLD ELEMENT)                           
         L     R7,4(R1)            SET A(NEW ELEMENT)                           
         L     R2,=A(STA01ELT)     SET A(ELEMENT COMPARE TABLE)                 
         GOTO1 ELTDETLS,DMCB,(R2),(R5),(R7)                                     
*                                  PROCESS ELEMENT DETAILS                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
*   XTRADESC:  SPECIAL CODE TO INTERROGATE, DISPLAY CHANGES TO                  
*        STATION'S EXTRA DESCRIPTION ELEMENT                                    
*                                                                               
XTRADESC NTR1                                                                   
         L     R5,0(R1)            SET A(OLD ELEMENT)                           
         L     R7,4(R1)            SET A(NEW ELEMENT)                           
         L     R2,=A(XTR08ELT)     SET A(ELEMENT COMPARE TABLE)                 
         GOTO1 ELTDETLS,DMCB,(R2),(R5),(R7)                                     
*                                  PROCESS ELEMENT DETAILS                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   STATION X'05' ELEMENT: SPECIAL PROCESSING ROUTINE                           
*        P1  =  A(OLD ELEMENT)                                                  
*        P2  =  A(NEW ELEMENT)                                                  
*                                                                               
*                                                                               
STARECV  NTR1                                                                   
         L     R5,0(R1)            SET A(OLD ELEMENT)                           
         L     R7,4(R1)            SET A(NEW ELEMENT)                           
         L     R2,=A(STA05ELT)     SET A(ELEMENT COMPARE TABLE)                 
         GOTO1 ELTDETLS,DMCB,(R2),(R5),(R7)                                     
*                                  PROCESS ELEMENT DETAILS                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
*   STATION X'06' ELEMENT: SPECIAL PROCESSING ROUTINE                           
*        P1  =  A(OLD ELEMENT)                                                  
*        P2  =  A(NEW ELEMENT)                                                  
*                                                                               
*                                                                               
STASIGN  NTR1                                                                   
         L     R5,0(R1)            SET A(OLD ELEMENT)                           
         L     R7,4(R1)            SET A(NEW ELEMENT)                           
         L     R2,=A(STA06ELT)     SET A(ELEMENT COMPARE TABLE)                 
         GOTO1 ELTDETLS,DMCB,(R2),(R5),(R7)                                     
*                                  PROCESS ELEMENT DETAILS                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
*   OFFICE  X'01' ELEMENT: SPECIAL PROCESSING ROUTINE                           
*        P1  =  A(OLD ELEMENT)                                                  
*        P2  =  A(NEW ELEMENT)                                                  
*                                                                               
*                                                                               
OFFDESC  NTR1                                                                   
         L     R5,0(R1)            SET A(OLD ELEMENT)                           
         L     R7,4(R1)            SET A(NEW ELEMENT)                           
         L     R2,=A(OFF01ELT)     SET A(ELEMENT COMPARE TABLE)                 
         GOTO1 ELTDETLS,DMCB,(R2),(R5),(R7)                                     
*                                  PROCESS ELEMENT DETAILS                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   S/P     X'01' ELEMENT: SPECIAL PROCESSING ROUTINE                           
*        P1  =  A(OLD ELEMENT)                                                  
*        P2  =  A(NEW ELEMENT)                                                  
*                                                                               
*                                                                               
SALDESC  NTR1                                                                   
         L     R5,0(R1)            SET A(OLD ELEMENT)                           
         L     R7,4(R1)            SET A(NEW ELEMENT)                           
         L     R2,=A(SAL01ELT)     SET A(ELEMENT COMPARE TABLE)                 
         GOTO1 ELTDETLS,DMCB,(R2),(R5),(R7)                                     
*                                  PROCESS ELEMENT DETAILS                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   S/P2    X'01' ELEMENT: SPECIAL PROCESSING ROUTINE                           
*        P1  =  A(OLD ELEMENT)                                                  
*        P2  =  A(NEW ELEMENT)                                                  
*                                                                               
*                                                                               
SAL2DESC NTR1                                                                   
         L     R5,0(R1)            SET A(OLD ELEMENT)                           
         L     R7,4(R1)            SET A(NEW ELEMENT)                           
         L     R2,=A(SP201ELT)     SET A(ELEMENT COMPARE TABLE)                 
         GOTO1 ELTDETLS,DMCB,(R2),(R5),(R7)                                     
*                                  PROCESS ELEMENT DETAILS                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   AGENCY  X'01' ELEMENT: SPECIAL PROCESSING ROUTINE                           
*                                                                               
AGYDESC  NTR1                                                                   
         L     R5,0(R1)            SET A(OLD ELEMENT)                           
         L     R7,4(R1)            SET A(NEW ELEMENT)                           
         L     R2,=A(AGY01ELT)     SET A(ELEMENT COMPARE TABLE)                 
         GOTO1 ELTDETLS,DMCB,(R2),(R5),(R7)                                     
*                                  PROCESS ELEMENT DETAILS                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   AGENCY2 X'10' ELEMENT: SPECIAL PROCESSING ROUTINE                           
*                                                                               
AGY2FAX  NTR1                                                                   
         L     R5,0(R1)            SET A(OLD ELEMENT)                           
         L     R7,4(R1)            SET A(NEW ELEMENT)                           
         L     R2,=A(AG210ELT)     SET A(ELEMENT COMPARE TABLE)                 
         GOTO1 ELTDETLS,DMCB,(R2),(R5),(R7)                                     
*                                  PROCESS ELEMENT DETAILS                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   AGENCY2 X'1F' ELEMENT: SPECIAL PROCESSING ROUTINE                           
*                                                                               
AGY2DESC NTR1                                                                   
         L     R5,0(R1)            SET A(OLD ELEMENT)                           
         L     R7,4(R1)            SET A(NEW ELEMENT)                           
         L     R2,=A(AG21FELT)     SET A(ELEMENT COMPARE TABLE)                 
         GOTO1 ELTDETLS,DMCB,(R2),(R5),(R7)                                     
*                                  PROCESS ELEMENT DETAILS                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   AGENCY2 X'20' ELEMENT: SPECIAL PROCESSING ROUTINE                           
*                                                                               
AGY2EXAD NTR1                                                                   
         L     R5,0(R1)            SET A(OLD ELEMENT)                           
         L     R7,4(R1)            SET A(NEW ELEMENT)                           
         L     R2,=A(AG220ELT)     SET A(ELEMENT COMPARE TABLE)                 
         GOTO1 ELTDETLS,DMCB,(R2),(R5),(R7)                                     
*                                  PROCESS ELEMENT DETAILS                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   ADVERT  X'01' ELEMENT: SPECIAL PROCESSING ROUTINE                           
*                                                                               
ADVDESC  NTR1                                                                   
         L     R5,0(R1)            SET A(OLD ELEMENT)                           
         L     R7,4(R1)            SET A(NEW ELEMENT)                           
         L     R2,=A(ADV01ELT)     SET A(ELEMENT COMPARE TABLE)                 
         GOTO1 ELTDETLS,DMCB,(R2),(R5),(R7)                                     
*                                  PROCESS ELEMENT DETAILS                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   PROD    X'01' ELEMENT: SPECIAL PROCESSING ROUTINE                           
*                                                                               
PRDDESC  NTR1                                                                   
         L     R5,0(R1)            SET A(OLD ELEMENT)                           
         L     R7,4(R1)            SET A(NEW ELEMENT)                           
*&&DO                                                                           
*   TEST DISPLAY                                                                
         MVC   P+1(04),=C'OLD:'                                                 
         MVC   P+5(64),0(R5)                                                    
         GOTO1 =V(PRINTER)                                                      
         MVC   P+1(04),=C'NEW:'                                                 
         MVC   P+5(64),0(R7)                                                    
         GOTO1 =V(PRINTER)                                                      
*   TEST DISPLAY END                                                            
*&&                                                                             
         L     R2,=A(PRD01ELT)     SET A(ELEMENT COMPARE TABLE)                 
         GOTO1 ELTDETLS,DMCB,(R2),(R5),(R7)                                     
*                                  PROCESS ELEMENT DETAILS                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   PR0D    X'01' ELEMENT: SPECIAL CLEARANCE  ROUTINE                           
*                                                                               
PRD01CLR NTR1                                                                   
         L     R5,0(R1)            SET A(OLD ELEMENT)                           
         L     R7,4(R1)            SET A(NEW ELEMENT)                           
*&&DO                                                                           
*   TEST DISPLAY                                                                
         MVC   P+1(04),=C'OLD:'                                                 
         MVC   P+5(64),0(R5)                                                    
         GOTO1 =V(PRINTER)                                                      
         MVC   P+1(04),=C'NEW:'                                                 
         MVC   P+5(64),0(R7)                                                    
         GOTO1 =V(PRINTER)                                                      
*   TEST DISPLAY END                                                            
*&&                                                                             
         L     R2,=A(PCL01ELT)     SET A(ELEMENT CLEAR TABLE)                   
         GOTO1 ELTCLEAR,DMCB,(R2),(R5),(R7)                                     
*                                  PROCESS ELEMENT DETAILS                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   PROD    X'02' ELEMENT: SPECIAL PROCESSING ROUTINE                           
*                                                                               
PRDNETC  NTR1                                                                   
         L     R5,0(R1)            SET A(OLD ELEMENT)                           
         L     R7,4(R1)            SET A(NEW ELEMENT)                           
         L     R2,=A(PRD02ELT)     SET A(ELEMENT COMPARE TABLE)                 
         GOTO1 ELTDETLS,DMCB,(R2),(R5),(R7)                                     
*                                  PROCESS ELEMENT DETAILS                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   PROD    X'03' ELEMENT: SPECIAL PROCESSING ROUTINE                           
*                                                                               
PRDSPTPK NTR1                                                                   
         L     R5,0(R1)            SET A(OLD ELEMENT)                           
         L     R7,4(R1)            SET A(NEW ELEMENT)                           
         L     R2,=A(PRD03ELT)     SET A(ELEMENT COMPARE TABLE)                 
         GOTO1 ELTDETLS,DMCB,(R2),(R5),(R7)                                     
*                                  PROCESS ELEMENT DETAILS                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   PROD    X'04' ELEMENT: SPECIAL PROCESSING ROUTINE                           
*                                                                               
PRDVALS  NTR1                                                                   
         L     R5,0(R1)            SET A(OLD ELEMENT)                           
         L     R7,4(R1)            SET A(NEW ELEMENT)                           
         L     R2,=A(PRD04ELT)     SET A(ELEMENT COMPARE TABLE)                 
         GOTO1 ELTDETLS,DMCB,(R2),(R5),(R7)                                     
*                                  PROCESS ELEMENT DETAILS                      
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   ELTDETLS:  ROUTINE USES INDICATED TABLE TO DETERMINE                        
*        WHETHER SIGNIFICANT PIECES OF DATA WITHIN THE ELEMENT                  
*        HAVE CHANGED.                                                          
*        P1  ->  R2  =  A(ELEMENT TABLE)                                        
*        P2  ->  R5  =  A(OLD ELEMENT)                                          
*        P3  ->  R7  =  A(NEW ELEMENT)                                          
*                                                                               
ELTDETLS NTR1                                                                   
         L     R2,0(R1)            SET A(ELEMENT TABLE)                         
         L     R5,4(R1)            SET A(OLD ELEMENT)                           
         L     R7,8(R1)            SET A(NEW ELEMENT)                           
ELTD0020 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    ELTD0900            YES -                                        
         LR    R3,R5               SET A(OLD ELEMENT)                           
         LR    R9,R7               SET A(NEW ELEMENT)                           
         ZIC   RE,SPRDISP(R2)      GET DISPLACEMENT TO DATA                     
         AR    R3,RE               DISPLACE TO OLD DATA                         
         AR    R9,RE               DISPLACE TO NEW DATA                         
         ZIC   RF,SPRLEN(R2)       GET LENGTH OF COMPARE                        
         BCTR  RF,0                BACK OFF 1 FOR EX                            
         EX    RF,ELTD0990         COMPARE FIELDS                               
         BNE   ELTD0040            NOT EQUAL:  DISPLAY                          
ELTD0030 EQU   *                                                                
         LA    R2,LSTA01EL(R2)     EQUAL:  BUMP TO NEXT DATA ITEM               
         B     ELTD0020            GO BACK AND CHECK IT                         
ELTD0040 EQU   *                                                                
         MVC   P+12(12),SPRFNAME(R2)                                            
*&&DO                              PLACE FIELD NAME ON LINE                     
         CLI   SPRINDCT(R2),C'O'   OPTION    FIELD?                             
         BNE   ELTD0042            NO  -                                        
         CLI   SPROPT(R2),X'FF'    YES - DISPLAY BY LENGTH?                     
         BE    ELTD0050            YES - DON'T SET NEXT FIELDS                  
*&&                                                                             
ELTD0042 EQU   *                                                                
         MVC   P+25(3),=C'OLD'                                                  
         MVC   P2+25(3),=C'NEW'                                                 
ELTD0050 EQU   *                                                                
         CLI   SPRINDCT(R2),C'D'   DATE FIELD?                                  
         BE    ELTD0080            YES - CONVERT DATE                           
         CLI   SPRINDCT(R2),C'C'   NO  - CHARACTER FIELD?                       
         BE    ELTD0060            YES                                          
         CLI   SPRINDCT(R2),C'N'   NO  - NUMERIC   FIELD?                       
         BE    ELTD0160            YES                                          
         CLI   SPRINDCT(R2),C'O'   NO  - OPTION    FIELD?                       
         BE    ELTD0200            YES                                          
         DC    H'0'                UNRECOGNIZED FIELD                           
ELTD0060 EQU   *                                                                
         EX    RF,ELTD0980         MOVE OLD FIELD TO DISPLAY                    
         EX    RF,ELTD0970         MOVE NEW FIELD TO DISPLAY                    
         B     ELTD0120                                                         
ELTD0080 EQU   *                                                                
         ZIC   RF,SPRLEN(R2)       GET FIELD LENGTH                             
         BCTR  RF,0                BACK OFF 1 FOR EX                            
         XC    WORK,WORK           CLEAR WORK AREA                              
         EX    RF,ELTD0960         MOVE OLD DATE TO WORKAREA                    
         EX    RF,ELTD0950         MOVE NEW DATE TO WORKAREA                    
         CLI   SPRDTYPE(R2),C'B'   BINARY DATES?                                
         BNE   ELTD0100            NO  - COMPRESSED                             
         GOTO1 =V(DATCON),DMCB,(3,WORK+32),(5,P+29)                             
         GOTO1 =V(DATCON),DMCB,(3,WORK+44),(5,P2+29)                            
         B     ELTD0120                                                         
ELTD0100 EQU   *                                                                
         GOTO1 =V(DATCON),DMCB,(2,WORK+32),(5,P+29)                             
         GOTO1 =V(DATCON),DMCB,(2,WORK+44),(5,P2+29)                            
ELTD0120 EQU   *                                                                
         GOTO1 =V(PRINTER)                                                      
         MVC   P,P2                LOAD SECOND LINE OF COMPARISON               
         MVC   P2,SPACES           CLEAR STORAGE SPACE FOR LINE2                
         GOTO1 =V(PRINTER)                                                      
         B     ELTD0030                                                         
ELTD0160 EQU   *                                                                
*                                                                               
*   NUMERIC FIELD HANDLING.                                                     
*        1.  NUMERIC FIELDS ARE A MAXIMUM OF FOUR CHARACTERS LONG               
*        2.  AS FIELDS VARY, IT IS NECESSARY TO INSERT EACH INTO                
*            A COMMON FIELD TO FACILITATE EDITING                               
*        3.  THE LENGTH OF THE FIELD WILL SERVE AS AN INDICATOR OF              
*            ITS DISPLACEMENT INTO THE COMMON FIELD.                            
*            A.  A 1-POS FIELD WILL BE INSERTED INTO THE 4TH POS                
*                OF THE COMMON FIELD                                            
*            B.  A 2-POS FIELD WILL BE INSERTED INTO THE 3RD POS                
*                OF THE COMMON FIELD                                            
*            C.  A 3-POS FIELD WILL BE INSERTED INTO THE 2ND POS                
*                OF THE COMMON FIELD                                            
*            D.  A 4-POS FIELD WILL BE INSERTED INTO THE 1ST POS                
*                OF THE COMMON FIELD                                            
*        4.  ALL EDITING WILL BE DONE USING THE 4-POSITION FIELD                
*                                                                               
         ZIC   RF,SPRLEN(R2)       GET FIELD LENGTH                             
         BCTR  RF,0                BACK OFF 1 FOR EX                            
         LA    R1,3                SET MAX DISPLACE INTO WORK FIELD             
         SR    R1,RF               SUBTRACT SIZE OF FIELD                       
         XC    WORK,WORK           CLEAR WORK AREA                              
         LA    RE,WORK+44          SET A(OLD BIN#)                              
         AR    RE,R1               ADD IN DISPLACEMENT FOR A                    
*                                     FIELD OF THIS LENGTH                      
         EX    RF,ELTD0940         MOVE OLD BIN# TO WORKAREA                    
         LA    RE,WORK+54          SET A(NEW BIN#)                              
         AR    RE,R1               ADD IN DISPLACEMENT FOR A                    
*                                     FIELD OF THIS LENGTH                      
         EX    RF,ELTD0930         MOVE NEW BIN# TO WORKAREA                    
         EDIT  (4,WORK+44),(8,P+29)                                             
         EDIT  (4,WORK+54),(8,P2+29)                                            
         B     ELTD0120                                                         
ELTD0200 EQU   *                                                                
         BAS   RE,OPTCYCLE         DISPLAY DIFFERING OPTIONS                    
         GOTO1 =V(PRINTER)                                                      
         MVC   P2,SPACES           CLEAR STORAGE SPACE FOR LINE2                
         B     ELTD0030                                                         
ELTD0900 EQU   *                                                                
         XIT1                                                                   
ELTD0930 EQU   *                                                                
         MVC   0(0,RE),0(R9)       MOVE BINARY #  BY LENGTH FOR                 
*                                     NEW NUMERIC FIELD  - RIGHT                
*                                     JUSTIFY IN FLD BY LENGTH                  
ELTD0940 EQU   *                                                                
         MVC   0(0,RE),0(R3)       MOVE BINARY #  BY LENGTH FOR                 
*                                     OLD NUMERIC FIELD  - RIGHT                
*                                     JUSTIFY IN FLD BY LENGTH                  
ELTD0950 EQU   *                                                                
         MVC   WORK+44(0),0(R9)    MOVE NEW DATE  BY LENGTH                     
ELTD0960 EQU   *                                                                
         MVC   WORK+32(0),0(R3)    MOVE OLD DATE  BY LENGTH                     
ELTD0970 EQU   *                                                                
         MVC   P2+29(0),0(R9)      MOVE NEW FIELD BY LENGTH                     
ELTD0980 EQU   *                                                                
         MVC   P+29(0),0(R3)       MOVE OLD FIELD BY LENGTH                     
ELTD0990 EQU   *                                                                
         CLC   0(0,R3),0(R9)       COMPARE FIELDS BY LENGTH                     
         EJECT                                                                  
*                                                                               
*   ELTCLEAR:  ROUTINE USES INDICATED TABLE TO CLEAR PORTIONS                   
*        OF AN ELEMENT PRIOR TO COMPARISON, TO ENSURE THAT                      
*        NON-SIGNIFICANT DATA WITHIN THE ELEMENT IS IGNORED.                    
*        P1  ->  R2  =  A(ELEMENT TABLE)                                        
*        P2  ->  R5  =  A(OLD ELEMENT)                                          
*        P3  ->  R7  =  A(NEW ELEMENT)                                          
*                                                                               
ELTCLEAR NTR1                                                                   
         L     R2,0(R1)            SET A(ELEMENT TABLE)                         
         L     R5,4(R1)            SET A(OLD ELEMENT)                           
         L     R7,8(R1)            SET A(NEW ELEMENT)                           
ELCL0020 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    ELCL0900            YES -                                        
         LR    R3,R5               SET A(OLD ELEMENT)                           
         LR    R9,R7               SET A(NEW ELEMENT)                           
         ZIC   RE,SPRDISP(R2)      GET DISPLACEMENT TO DATA                     
         AR    R3,RE               DISPLACE TO OLD DATA                         
         AR    R9,RE               DISPLACE TO NEW DATA                         
         ZIC   RF,SPRLEN(R2)       GET LENGTH OF COMPARE                        
         BCTR  RF,0                BACK OFF 1 FOR EX                            
         EX    RF,ELCL0990         CLEAR OLD FIELD                              
         EX    RF,ELCL0980         CLEAR NEW FIELD                              
         LA    R2,LSTA01EL(R2)     EQUAL:  BUMP TO NEXT DATA ITEM               
         B     ELCL0020            GO BACK AND CHECK IT                         
ELCL0900 EQU   *                                                                
         XIT1                                                                   
ELCL0980 EQU   *                                                                
         XC    0(0,R9),0(R9)       CLEAR NEW FIELD BY LENGTH                    
ELCL0990 EQU   *                                                                
         XC    0(0,R3),0(R3)       CLEAR OLD FIELD BY LENGTH                    
*                                                                               
*   OPTCYCLE:  COMPARE OPTION FIELD IDENTIFIED WITHIN AN ELEMENT                
*        R3  =  A(OLD RECORD OPTION FIELD)                                      
*        R9  =  A(NEW RECORD OPTION FIELD)                                      
*        R2  =  A(ELEMENT ENTRY IN LIST)                                        
*                                                                               
OPTCYCLE NTR1                                                                   
         ZIC   R8,SPRLEN(R2)       SET LENGTH OF COMPARE (LOOP CTRL)            
         L     R6,SPROPT(R2)       SET A(OPTION DETAIL MSGS)                    
         BCTR  R8,0                BACK OFF 1 FOR EX                            
*                                                                               
         EX    R8,OPTC0990         COMPARE BY LENGTH                            
         BE    OPTC0900            EQUAL: NO CHANGE.  EXIT RTN                  
         CLI   SPROPT(R2),X'FF'    DISPLAY BY LENGTH ONLY?                      
         BE    OPTC0200            YES -                                        
         CLI   SPRDTYPE(R2),C'C'   CHARACTER-BASED OPTIONS?                     
         BNE   OPTC0100            NO  - BIT-BASED                              
         ZIC   R8,SPRLEN(R2)       YES - SET LENGTH OF COMPARE                  
OPTC0040 EQU   *                                                                
         CLC   0(1,R3),0(R9)       COMPARE BYTE-BY-BYTE                         
         BE    OPTC0060            BYTES EQUAL                                  
         MVC   P+29(LOPTDESC),0(R6)                                             
         MVC   P+29+LOPTDESC+3(04),=C'FROM'                                     
         MVC   P+29+LOPTDESC+3+6(1),0(R3)                                       
         MVC   P+29+LOPTDESC+11(02),=C'TO'                                      
         MVC   P+29+LOPTDESC+11+3(1),0(R9)                                      
         GOTO1 =V(PRINTER)                                                      
OPTC0060 EQU   *                                                                
         LA    R3,1(R3)            BUMP TO NEXT OPTION                          
         LA    R9,1(R9)            DITTO                                        
         LA    R6,LOPTDESC(R6)     BUMP TO NEXT OPTION DESCRIPTION              
         BCT   R8,OPTC0040         GO BACK FOR NEXT OPTION                      
         B     OPTC0900            FINISHED - EXIT ROUTINE                      
OPTC0100 EQU   *                                                                
         ZIC   R8,SPRLEN(R2)       SET LENGTH OF COMPARE                        
OPTC0120 EQU   *                                                                
         BAS   RE,SHIFTREG         LOOP THROUGH OPTION BYTE                     
         LA    R3,1(R3)            BUMP TO NEXT OLD BYTE                        
         LA    R9,1(R9)            BUMP TO NEXT NEW BYTE                        
         BCT   R8,OPTC0120         GO BACK FOR NEXT OPTION BYTE                 
*                                  ALL BYTES DONE                               
         B     OPTC0900                                                         
OPTC0200 EQU   *                                                                
*&&DO                                                                           
         MVC   P+1(31),=C'OPTIONS: DISPLAY BY LENGTH ONLY'                      
         GOTO1 =V(PRINTER)                                                      
*&&                                                                             
         CLI   SPREXPLD(R2),X'FF'  EXPLODE FIELD FROM BITS TO BYTES?            
         BE    OPTC0240            YES                                          
*                                                                               
         ZIC   R8,SPRLEN(R2)       NO  - SET LENGTH OF DISPLAY                  
         BCTR  R8,0                BACK OFF 1 FOR EX                            
         MVC   P+33(3),=C'OLD'                                                  
         MVC   P2+33(3),=C'NEW'                                                 
         EX    R8,OPTC0980         MOVE OLD BY LENGTH                           
         EX    R8,OPTC0970         MOVE NEW BY LENGTH                           
         GOTO1 =V(PRINTER)                                                      
         MVC   P,P2                LOAD SECOND LINE OF COMPARISON               
         MVC   P2,SPACES           CLEAR STORAGE SPACE FOR LINE2                
         GOTO1 =V(PRINTER)                                                      
         B     OPTC0900                                                         
OPTC0240 EQU   *                                                                
         BAS   RE,EXPLDREG         LOOP THROUGH OPTION BYTE, EXPLODE            
***      LA    R3,1(R3)            BUMP TO NEXT OLD BYTE                        
***      LA    R9,1(R9)            BUMP TO NEXT NEW BYTE                        
***      BCT   R0,OPTC0240         GO BACK FOR NEXT OPTION BYTE                 
*                                  ALL BYTES DONE                               
         GOTO1 =V(PRINTER)                                                      
         MVC   P,P2                LOAD SECOND LINE OF COMPARISON               
         MVC   P2,SPACES           CLEAR STORAGE SPACE FOR LINE2                
         GOTO1 =V(PRINTER)                                                      
         B     OPTC0900                                                         
OPTC0900 EQU   *                                                                
         XIT1                                                                   
OPTC0970 EQU   *                                                                
         MVC   P2+29(0),0(R9)      MOVE NEW FIELD TO PRINT                      
OPTC0980 EQU   *                                                                
         MVC   P+29(0),0(R3)       MOVE OLD FIELD TO PRINT                      
OPTC0990 EQU   *                                                                
         CLC   0(0,R3),0(R9)       COMPARE OLD / NEW OPT FIELDS                 
         EJECT                                                                  
*                                                                               
*   SHIFTREG:  COMPARES TWO BYTES, BIT BY BIT.  IF DIFFERENT,                   
*        DISPLAYS APPROPRIATE OPTION MESSAGE                                    
*        R3 ->  OLD OPTION BYTE                                                 
*        R9 ->  NEW OPTION BYTE                                                 
*        LOOP IS FOR 8 BITS                                                     
*                                                                               
SHIFTREG NTR1                                                                   
         LA    R0,8                SET LOOP FOR 8 BITS                          
         SR    R2,R2               CLEAR SHIFT REGISTERS                        
         SR    R8,R8                                                            
*&&DO                                                                           
*   TEST 1                                                                      
         MVC   P+1(03),=C'T1:'                                                  
         MVC   P+4(4),0(R3)                                                     
         MVI   P+8,C'/'                                                         
         MVC   P+9(4),0(R9)                                                     
         GOTO1 =V(PRINTER)                                                      
*   TEST 1 END                                                                  
*&&                                                                             
         ZIC   RF,0(R3)            LOAD OLD BYTE TO R3                          
         LR    R3,RF                                                            
         ZIC   RF,0(R9)            LOAD NEW BYTE TO R9                          
         LR    R9,RF                                                            
*&&DO                                                                           
*   TEST 2                                                                      
         MVC   P+1(03),=C'T2:'                                                  
         STCM  R3,15,P+4                                                        
         MVI   P+8,C'/'                                                         
         STCM  R9,15,P+9                                                        
         GOTO1 =V(PRINTER)                                                      
*   TEST 2 END                                                                  
*&&                                                                             
         SLL   R3,24               SHIFT TO HIGH ORDER POSITION                 
         SLL   R9,24               SHIFT TO HIGH ORDER POSITION                 
*&&DO                                                                           
*   TEST 3                                                                      
         MVC   P+1(03),=C'T3:'                                                  
         STCM  R3,15,P+4                                                        
         MVI   P+8,C'/'                                                         
         STCM  R9,15,P+9                                                        
         GOTO1 =V(PRINTER)                                                      
*   TEST 3 END                                                                  
*&&                                                                             
SREG0020 EQU   *                                                                
         SLDL  R2,1                SHIFT HIGH ORDER BIT OUT OF R3               
         SLDL  R8,1                SHIFT HIGH ORDER BIT OUT OF R9               
         CR    R2,R8               BITS EQUAL?                                  
         BE    SREG0040            YES - NO MESSAGE                             
         MVC   P+29(LOPTDESC),0(R6)                                             
         MVC   P+29+LOPTDESC+3(04),=C'FROM'                                     
         MVC   P+29+LOPTDESC+11(02),=C'TO'                                      
         MVI   P+29+LOPTDESC+3+6,C'N'                                           
         MVI   P+29+LOPTDESC+11+4,C'Y'                                          
         LTR   R2,R2               ANY VALUE IN R2?                             
         BZ    SREG0030                                                         
         MVI   P+29+LOPTDESC+3+6,C'Y'                                           
         MVI   P+29+LOPTDESC+11+4,C'N'                                          
SREG0030 EQU   *                                                                
         GOTO1 =V(PRINTER)                                                      
SREG0040 EQU   *                                                                
         SR    R2,R2               CLEAR BOTH REGISTERS                         
         SR    R8,R8                                                            
         LA    R6,LOPTDESC(R6)     BUMP TO NEXT ERROR MESSAGE                   
         BCT   R0,SREG0020         GO BACK FOR NEXT BIT                         
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   EXPLDREG:  EXPLODES TWO FIELDS FROM BITS TO N/Y INDICATORS.                 
*        R3 ->  OLD OPTION BYTE                                                 
*        R9 ->  NEW OPTION BYTE                                                 
*        SPRLEN(R2) = LENGTH OF FIELDS BEING DISPLAYED                          
*                                                                               
EXPLDREG NTR1                                                                   
         ST    R6,SAVER6           SAVE A(TABLE ENTRY)                          
         LA    R5,P+29             SET A(OLD PRINT FIELD)                       
         LA    R6,P2+29            SET A(NEW PRINT FIELD)                       
         ZIC   R1,SPRLEN(R2)       SET OUTER LOOP                               
XREG0010 EQU   *                                                                
         LA    R0,8                SET INNER LOOP FOR 8 BITS                    
         SR    R2,R2               CLEAR SHIFT REGISTERS                        
         SR    R8,R8                                                            
*&&DO                                                                           
*   TEST 1                                                                      
         MVC   P+1(03),=C'T1:'                                                  
         MVC   P+4(4),0(R3)                                                     
         MVI   P+8,C'/'                                                         
         MVC   P+9(4),0(R9)                                                     
         GOTO1 =V(PRINTER)                                                      
*   TEST 1 END                                                                  
*&&                                                                             
         ZIC   RF,0(R3)            LOAD OLD BYTE TO R3                          
         LR    R3,RF                                                            
         ZIC   RF,0(R9)            LOAD NEW BYTE TO R9                          
         LR    R9,RF                                                            
*&&DO                                                                           
*   TEST 2                                                                      
         MVC   P+1(03),=C'T2:'                                                  
         STCM  R3,15,P+4                                                        
         MVI   P+8,C'/'                                                         
         STCM  R9,15,P+9                                                        
         GOTO1 =V(PRINTER)                                                      
*   TEST 2 END                                                                  
*&&                                                                             
         SLL   R3,24               SHIFT TO HIGH ORDER POSITION                 
         SLL   R9,24               SHIFT TO HIGH ORDER POSITION                 
*&&DO                                                                           
*   TEST 3                                                                      
         MVC   P+1(03),=C'T3:'                                                  
         STCM  R3,15,P+4                                                        
         MVI   P+8,C'/'                                                         
         STCM  R9,15,P+9                                                        
         GOTO1 =V(PRINTER)                                                      
*   TEST 3 END                                                                  
*&&                                                                             
XREG0020 EQU   *                                                                
         SLDL  R2,1                SHIFT HIGH ORDER BIT OUT OF R3               
         SLDL  R8,1                SHIFT HIGH ORDER BIT OUT OF R9               
         MVI   0(R5),C'N'          SET OLD PRINT POS TO 'NO'                    
         LTR   R2,R2               BIT ON?                                      
         BZ    XREG0022            NO                                           
         MVI   0(R5),C'Y'          SET OLD PRINT POS TO 'YES'                   
XREG0022 EQU   *                                                                
         MVI   0(R6),C'N'          SET NEW PRINT POS TO 'NO'                    
         LTR   R8,R8               BIT ON?                                      
         BZ    XREG0024            NO                                           
         MVI   0(R6),C'Y'          SET NEW PRINT POS TO 'YES'                   
XREG0024 EQU   *                                                                
         SR    R2,R2               CLEAR BOTH REGISTERS                         
         SR    R8,R8                                                            
         LA    R5,1(R5)            INCREMENT 0LD PRINT POSITION                 
         LA    R6,1(R6)            INCREMENT NEW PRINT POSITION                 
         BCT   R0,XREG0020         GO BACK FOR NEXT BIT                         
         LA    R3,1(R3)            BUMP TO NEXT OLD BYTE                        
         LA    R9,1(R9)            BUMP TO NEXT NEW BYTE                        
         BCT   R1,XREG0010         GO BACK FOR NEXT BYTE                        
*&&DO                                                                           
*   TEST 4                                                                      
         MVC   P+1(06),=C'T4: P='                                               
         GOTO1 =V(PRINTER)                                                      
         MVC   P,P2                                                             
         GOTO1 =V(PRINTER)                                                      
*   TEST 4 END                                                                  
*&&                                                                             
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        SETQUEUE --- OPEN THE PRINT QUEUE FOR THIS AGENCY                      
*                                                                               
SETQUEUE NTR1                                                                   
*                                                                               
         L     R2,0(R1)         GET PASSED AGY CODE                             
         L     R3,=A(QUEUETAB)     SET A(QUEUE TABLE)                           
SQUE0060 EQU   *                                                                
         CLI   0(R3),X'00'                                                      
         BE    SQUE0160            NO ENTRY: DEFAULT TO SJR                     
         CLC   0(2,R2),0(R3)                                                    
         BE    SQUE0080                                                         
         LA    R3,LQUEUTAB(R3)                                                  
         B     SQUE0060                                                         
SQUE0080 EQU   *                                                                
         MVC   WORK(6),2(R3)                                                    
*                                                                               
*   TEST DISPLAY OUTPUT QUEUE ID                                                
         CLI   SAVORID,C'Y'        PRINT QUEUE OVERRIDE?                        
         BNE   SQUE0100            NO  - DON'T DISPLAY ORIGINAL QUEUE           
         MVC   P+1(15),=C'PRINT QUEUE TO:'                                      
         MVC   P+16(6),WORK                                                     
         GOTO1 =V(PRINTER)                                                      
*   TEST DISPLAY END                                                            
*                                                                               
*                                                                               
SQUE0100 EQU   *                                                                
         L     R5,AREC                                                          
         XC    0(128,R5),0(R5)                                                  
         MVI   0(R5),C'I'                                                       
         MVC   15(10,R5),SPACES                                                 
         MVC   15(6,R5),WORK                                                    
         MVC   UTL+4(1),SAVECUTL   SET UTL FOR CONTROL FILE                     
         GOTO1 =V(DATAMGR),DMCB,=C'DMREAD',=C'CTFILE',(R5),(R5),0               
         MVC   UTL+4(1),SAVERUTL   RESET UTL FOR REP FILE                       
         CLI   DMCB+8,0                                                         
         BNE   SQUE0160            NO ID RECORD FOUND                           
*&&DO                                                                           
*   TEST                                                                        
         MVC   P+1(12),=C'REC FOUND  :'                                         
         MVC   P+13(96),0(R5)                                                   
         GOTO1 =V(PRINTER)                                                      
*   TEST END                                                                    
*&&                                                                             
*                                                                               
         LA    R5,28(R5)           SET A(1ST ELEMENT IN RECORD)                 
*                                                                               
*&&DO                                                                           
*   TEST                                                                        
         MVC   P+1(12),=C'1ST ELEMENT:'                                         
         MVC   P+13(96),0(R5)                                                   
         GOTO1 =V(PRINTER)                                                      
*   TEST END                                                                    
*&&                                                                             
SQUE0120 CLI   0(R5),0                                                          
         BE    SQUE0160                                                         
*&&DO                                                                           
*   TEST                                                                        
         MVC   P+1(12),=C'NEXT ELEMNT:'                                         
         MVC   P+13(96),0(R5)                                                   
         GOTO1 =V(PRINTER)                                                      
*   TEST END                                                                    
*&&                                                                             
         CLI   0(R5),X'02'                                                      
         BNE   SQUE0140                                                         
         MVC   SAVID,2(R5)                                                      
*&&DO                                                                           
*   TEST                                                                        
         MVC   P+1(06),=C'SAVID='                                               
         MVC   P+07(2),SAVID                                                    
         GOTO1 =V(PRINTER)                                                      
*   TEST END                                                                    
*&&                                                                             
         CLI   SAVORID,C'Y'        PRINT QUEUE OVERRIDE?                        
         BE    SQUE0160            YES - FORCE O/P TO SJR PRINT QUEUE           
         B     SQUE0180                                                         
*                                                                               
SQUE0140 SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     SQUE0120                                                         
*                                                                               
SQUE0160 MVC   SAVID,=H'17'        DEFAULT TO SJR                               
*                                                                               
SQUE0180 EQU   *                                                                
         CLI   SAVLOCAL,C'Y'       LOCAL PRINT ONLY?                            
         BE    SQUE0200            YES                                          
         BAS   RE,REMOTE           NO  - CLOSE OLD, OPEN NEW FILE               
*                                                                               
SQUE0200 EQU   *                                                                
         B     TMD1GOOD                                                         
SQUE0220 EQU   *                                                                
*                                                                               
*&&DO                                                                           
         MVC   RUNERROR(L'NOQUEUE),NOQUEUE                                      
*                                                                               
*   TEST DISPLAY                                                                
         MVC   P(14),=C'BAD PRINTQUEUE'                                         
         GOTO1 REPORT                                                           
*   TEST DISPLAY END                                                            
*                                                                               
         B     TMD1BAD                                                          
*&&                                                                             
*                                                                               
         SPACE 2                                                                
TMD1GOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TMD1EXIT                                                         
TMD1BAD  EQU   *                                                                
         LA    R0,1                                                             
TMD1EXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
*    READ RECORDS                                                               
HIGH     LA    RF,=C'DMRDHI'                                                    
         MVC   KEYSAVE,KEY                                                      
         B     LINKDIR                                                          
         SPACE 2                                                                
SEQ      LA    RF,=C'DMRSEQ'                                                    
         MVC   KEYSAVE,KEY                                                      
         B     LINKDIR                                                          
         SPACE 2                                                                
LINKDIR  NTR1                                                                   
         ST    RF,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 =V(DATAMGR),DMCB,,=C'REPDIR',KEY,KEY,0                           
         B     DMCHECK                                                          
         EJECT                                                                  
*              DATA MANAGER INTERFACE (FILE GETS)                               
         SPACE 3                                                                
GETOFF   LA    RF,ROFFREC                                                       
         B     LINKFILE                                                         
GETMAN   LA    RF,RSALREC                                                       
         B     LINKFILE                                                         
GETADV   LA    RF,RADVREC                                                       
         B     LINKFILE                                                         
GETPRD   LA    RF,RPRDREC                                                       
         B     LINKFILE                                                         
GETAGY   LA    RF,RAGYREC                                                       
         B     LINKFILE                                                         
GETSTA   LA    RF,RSTAREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         LR    R2,RF                                                            
         GOTO1 =V(DATAMGR),DMCB,(DMINBTS,=C'GETREC'),=C'REPFILE',      X        
               KEY+28,(R2),(0,DMWORK)                                           
         B     DMCHECK                                                          
         SPACE 4                                                                
DMCHECK  TM    DMCB+8,X'FF'                                                     
         BZ    XIT                                                              
         DC    H'0'                DATA MANAGER ERROR                           
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              SET UP FOR REMOTE PRINT QUEUE                                    
         SPACE 2                                                                
REMOTE   NTR1                                                                   
         ZAP   LINE,=P'99'         FORCE PAGE CHANGE                            
         CLI   CLOSEFLG,C'N'       DON'T CLOSE ON FIRST TIME                    
         BNE   REMOT2                                                           
         MVI   CLOSEFLG,C'Y'       CHANGE FLAG                                  
         B     REMOT5                                                           
REMOT2   EQU   *                                                                
         GOTO1 =V(PRINT),DMCB,=C'CLOSE'                                         
REMOT5   L     R2,VREMOTEC                                                      
         USING REMOTED,R2                                                       
         MVC   REMOTAOP,=V(PQOPEN)                                              
         MVC   REMOTABF,=V(PQBUFF)                                              
         MVC   REMOTADM,=V(DATAMGR)                                             
         MVC   REMOTDST,SAVID                                                   
         MVC   REMOTKEY(11),SPACES                                              
         MVC   REMOTSYS(3),=C'SOX'                                              
         MVI   REMOTCPY,C'1'                                                    
         MVI   REMOTLPP,68                                                      
         MVI   REMOTCLS,C'K'                                                    
         MVC   REMOTJID,=C'SOX'    SOX TURNAROUND REPORT                        
*                                                                               
         XIT1                                                                   
         SPACE 1                                                                
         DROP  R2                                                               
         EJECT                                                                  
DUMPLIST DS    0F                                                               
         DC    A(RESOX,65000)                                                   
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         SPACE 2                                                                
DMCB     DS    6F                                                               
WORK     DS    CL64                                                             
DUB      DS    D                                                                
ATABTYPE DS    A                                                                
ATYPTABL DS    A                                                                
AOLDREC  DS    A                                                                
ANEWREC  DS    A                                                                
ATYPMSGS DS    A                                                                
AOLDELT# DS    F                   ELEMENT COUNT IN 'OLD' RECORD                
ANEWELT# DS    F                   ELEMENT COUNT IN 'NEW' RECORD                
AOLDADD1 DS    F                   A(FIRST OLD ELEMENT) OF THIS TYPE            
ANEWADD1 DS    F                   A(FIRST NEW ELEMENT) OF THIS TYPE            
PRNTCTR  DS    F                                                                
SAVER6   DS    F                                                                
*                                                                               
SAVEHDR  DS    CL132                                                            
SAVEHDR2 DS    CL132                                                            
*                                                                               
KEY      DS    XL32                                                             
KEYSAVE  DS    XL32                                                             
DMWORK   DS    XL96                                                             
MYSEQ    DS    H                                                                
CARD     DS    CL80                                                             
EOFSW    DS    CL1                                                              
SORTSW   DS    CL1                                                              
CLOSEFLG DS    CL1                                                              
LASTREP  DC    CL2'  '                                                          
SAVWORK6 DS    CL2                                                              
BYTE     DS    X                   X'80'-GET RECEIVING ID                       
*                                  X'40'-CLOSE PRINT QUEUE                      
SAVNAME  DS    CL10                AGENCY NAME STORAGE FROM CARD I/P            
SAVLOCAL DS    CL1                                                              
SAVORID  DS    CL1                                                              
SAVUTL   DS    CL1                                                              
SAVECUTL DS    CL1                                                              
SAVERUTL DS    CL1                                                              
SAVSEQ2  DS    CL1                                                              
SAVID    DS    CL2                                                              
SAVRID   DS    CL2                                                              
*                                                                               
*    NOT ALL RECORD TYPES WILL BE REPORTED.  TO INCLUDE A RECORD                
*        TYPE, ACTIVATE ITS ENTRY IN THE FOLLOWING TABLE, AND                   
*        INSERT THE APPROPRIATE INFORMATIONAL DISPLAY CODE FOR                  
*        THAT TYPE IN THE CODING STRUCTURE.                                     
*                                                                               
DRECTYP  EQU   0                   DISP TO RECORD TYPE                          
DRECNAME EQU   2                   DISP TO RECORD NAME FOR DISPLAY              
DRECREP  EQU   10                  DISP TO REP CODE DISPLACEMENT                
*                                                                               
*                                  NOTE:  THE LABELS IN THE DISPLACE-           
*        ------------------>>      MENT MAY NOT BE CORRECT.  I DIDN'T           
*                                  LOOK UP ALL THE RECORD DSECTS                
*                                                                               
TYPTABLE EQU   *                                                                
         DC    X'0100',C'REP     ',AL1(RREPKREP-RREPREC)                        
LTYPTABL EQU   *-TYPTABLE                                                       
         DC    X'0200',C'STATION ',AL1(RSTAKREP-RSTAREC)                        
*        DC    X'0300',C'REGION  ',AL1(RRGNKREP-RRGNREC)                        
         DC    X'0400',C'OFFICE  ',AL1(ROFFKREP-ROFFREC)                        
*        DC    X'4400',C'OFFICE2 ',AL1(ROFFKREP-ROFFREC)                        
*        DC    X'0500',C'TEAM    ',AL1(RTEMKREP-RTEMREC)                        
         DC    X'0600',C'S/P     ',AL1(RSALKREP-RSALREC)                        
*        DC    X'4600',C'S/P2    ',AL1(RSALKREP-RSALREC)                        
*        DC    X'0700',C'GROUP   ',AL1(RGRPKREP-RGRPREC)                        
         DC    X'0800',C'ADVERT  ',AL1(RADVKREP-RADVREC)                        
         DC    X'0900',C'PRODUCT ',AL1(RPRDKREP-RPRDREC)                        
         DC    X'0A00',C'AGENCY  ',AL1(RAGYKREP-RAGYREC)                        
*        DC    X'1A00',C'AGENCY2 ',AL1(RAG2KREP-RAG2REC)                        
*        DC    X'0D00',C'CLASS   ',AL1(RCLSKREP-RCLSREC)                        
*        DC    X'0F00',C'CATEGORY',AL1(RCTGKREP-RCTGREC)                        
*        DC    X'1300',C'BUDGET  ',AL1(RBUDKREP-RBUDREC)                        
*        DC    X'1501',C'PAR SECY',AL1(RPARKREP-RPARREC)                        
*        DC    X'1502',C'SEC DEF ',AL1(RSDFKREP-RSDFREC)                        
*        DC    X'1503',C'IBKLST  ',AL1(RIBKKREP-RIBKREC)                        
*        DC    X'1504',C'SBUD+ST ',AL1(RSBDKREP-RSBDREC)                        
*        DC    X'1505',C'SBUD-ST ',AL1(RSBDKREP-RSBDREC)                        
*        DC    X'1800',C'EOM     ',AL1(REOMKREP-REOMREC)                        
*        DC    X'1900',C'OBUD    ',AL1(ROBDKREP-ROBDREC)                        
*        DC    X'2000',C'ALTCAL  ',AL1(RACLKREP-RACLREC)                        
*        DC    X'2400',C'DAYPART ',AL1(RDPTKREP-RDPTREC)                        
*        DC    X'2600',C'SDD     ',AL1(RSDDKREP-RSDDREC)                        
*        DC    X'2900',C'COMMIS  ',AL1(RCOMKREP-RCOMREC)                        
*        DC    X'2A00',C'OWNER   ',AL1(ROWNKREP-ROWNREC)                        
*        DC    X'2B00',C'MARKET  ',AL1(RMKTKREP-RMKTREC)                        
*        DC    X'2E00',C'STD CMT ',AL1(RSCMKREP-RSCMREC)                        
*        DC    X'3000',C'TYPE    ',AL1(RTYPKREP-RTYPREC)                        
*        DC    X'3100',C'PTPERSON',AL1(RPTPKREP-RPTPREC)                        
*        DC    X'3200',C'CONTYPE ',AL1(RCTYKREP-RCTYREC)                        
*        DC    X'3400',C'OFF CMT ',AL1(ROCMKREP-ROCMREC)                        
*        DC    X'3A00',C'DEV S/P ',AL1(RDSPKREP-RDSPREC)                        
*        DC    X'3B00',C'DEV CTYP',AL1(RDCTKREP-RDCTREC)                        
*        DC    X'3D00',C'TERRITOR',AL1(RTERKREP-RTERREC)                        
         DC    X'0000',C'DELIMTER'                                              
         DS    0H                                                               
COMPTEST DS    CL1                                                              
DETAILS  DS    CL1                                                              
*                                                                               
DMINBTS  DS    X                                                                
FLIST    DS    0H                                                               
         DC    CL8'NREPFILE'                                                    
         DC    CL8' REPDIR '                                                    
         DC    CL8'X       '                                                    
UTL      DC    F'0',X'0A'          FOR CONTROL SYSTEM                           
AREC     DC    A(REC)                                                           
COMDATE  DS    XL2                 TODAY'S DATE - COMPRESSED                    
         SPACE 2                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,45,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=1100'                                  
*                                                                               
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,LRECL=4200,             X        
               MACRF=(GM,),EODAD=RCIN0280                                       
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*SORTREC'                                                      
SRTREC   DS    0XL1100             SORTREC                                      
SRTKEY   DS    0XL45               SORTKEY                                      
*                                                                               
SRTREP   DS    CL2                 REP                                          
SRTRTYPE DS    XL1                 RECORD TYPE (STA, AGY, ETC)                  
SRTRKEY  DS    CL34                KEY OF RECORD                                
SRTCOMP  EQU   *-SRTREC            HIGH ORDER KEY COMPARISON                    
SRTRDAT  DS    XL03                DATE OF ENTRY (3 CHAR BIN)                   
SRTRTIM  DS    XL04                TIME OF ENTRY                                
SRTRTTYP DS    XL01                TRANS TYPE (1=COPY,2=CHG,3=ADD)              
SRTKYLEN EQU   *-SRTREC            SORT KEY LENGTH                              
*                                                                               
SRTDATA  DS    CL1024              DATA RECORD                                  
         DS    CL31                SPARE                                        
SRTRECX  EQU   *                                                                
SRTLEN   EQU   *-SRTREC            SORT LENGTH                                  
*                                                                               
         SPACE 3                                                                
         DS    0D                                                               
         DC    C'*SAVEREC'                                                      
SAVREC   DS    0XL1100             SORTREC                                      
SAVKEY   DS    0XL45               SORTKEY                                      
*                                                                               
SAVREP   DS    CL2                 REP                                          
SAVRTYPE DS    XL1                 RECORD TYPE (STA, AGY, ETC)                  
SAVRKEY  DS    CL34                KEY OF RECORD                                
SAVCOMP  EQU   *-SAVREC            HIGH ORDER KEY COMPARISON                    
SAVRDAT  DS    XL03                DATE OF ENTRY (3 CHAR BIN)                   
SAVRTIM  DS    XL04                TIME OF ENTRY                                
SAVRTTYP DS    XL01                TRANS TYPE (1=COPY,2=CHG,3=ADD)              
SAVKYLEN EQU   *-SAVREC            SORT KEY LENGTH                              
*                                                                               
SAVDATA  DS    CL1024              DATA RECORD                                  
         DS    CL31                SPARE                                        
P2       DS    CL132                                                            
         EJECT                                                                  
       ++INCLUDE RERISKTAB                                                      
         SPACE 3                                                                
       ++INCLUDE REGENREPA                                                      
         SPACE 3                                                                
       ++INCLUDE REGENOFF                                                       
         SPACE 3                                                                
       ++INCLUDE REGENSTA                                                       
         SPACE 3                                                                
       ++INCLUDE REGENSAL                                                       
         SPACE 3                                                                
       ++INCLUDE REGENSAL2                                                      
         SPACE 3                                                                
       ++INCLUDE REGENADV                                                       
         SPACE 3                                                                
       ++INCLUDE REGENPRD                                                       
         SPACE 3                                                                
       ++INCLUDE REGENAGY                                                       
         SPACE 3                                                                
       ++INCLUDE REGENAGY2                                                      
         EJECT                                                                  
         DS    F                   LENGTH OF RECOVERY RECORD                    
         SPACE 1                                                                
       ++INCLUDE DMRCVRHDR                                                      
         SPACE 2                                                                
RKEY     DS    0XL27                                                            
REC      DS    4200X                                                            
         SPACE 1                                                                
ACESAVE  DS    3000D                                                            
         SPACE 4                                                                
       ++INCLUDE DDREMOTED                                                      
         SPACE 4                                                                
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
         SPACE 3                                                                
RESOX    CSECT                                                                  
*   FLDLABLS                                                                    
*        EACH RECORD TYPE HAS ITS OWN MESSAGES BY ELEMENT TYPE                  
*                                                                               
*   LABELS FOR ELEMENT CHANGES                                                  
*        EACH ENTRY IS 32 BYTES LONG                                            
*        EACH LABEL IS REFERENCED BY A NUMBER IN THE RECORD                     
*             TYPE ELEMENT TABLE                                                
*        THIS MESSAGE WILL BE DISPLAYED WHEN A CHANGE IS FOUND                  
*             FOR THAT RECORD TYPE / ELEMENT NUMBER                             
*                                                                               
REPMSGS  DC    C'REP    : DESCRIPTOR ELEMENT     ' 1                            
         DC    C'REP    : X/02/      ELEMENT     ' 2                            
         DC    C'REP    : X/03/      ELEMENT     ' 3                            
         DC    C'REP    : PROFILES SCREEN ARRAYS ' 4                            
         DS    0F                                                               
STAMSGS  DC    C'STATION: DESCRIPTOR ELEMENT     ' 1                            
         DC    C'STATION: OTHER STATIONS         ' 2                            
         DC    C'STATION: RECEIVING SIGNON       ' 3                            
         DC    C'STATION: SIGNON IDS             ' 4                            
         DC    C'STATION: PROFILES/OPTIONS       ' 5                            
         DC    C'STATION: FORMER/NEW REP         ' 6                            
         DC    C'STATION: JOIN/LEAVE DATES       ' 7                            
         DC    C'STATION: ID NUMBER              ' 8                            
         DC    C'STATION: DEMO MARKET            ' 9                            
         DC    C'STATION: PERSONAL ID NUMBERS    ' 10                           
         DC    C'STATION: CLOSEOUT INFORMATION   ' 11                           
         DC    C'STATION: EMAIL ADDRESSES        ' 12                           
         DC    C'STATION: ETRANS VENDOR          ' 13                           
         DS    0F                                                               
OFFMSGS  DC    C'OFFICE : DESCRIPTOR ELEMENT     ' 1                            
         DS    0F                                                               
SALMSGS  DC    C'SALESPN: DESCRIPTOR ELEMENT     ' 1                            
         DS    0F                                                               
SAL2MSGS DC    C'SAL2SPN: PAY S/P CODE ELEMENT   ' 1                            
         DC    C'SAL2SPN: EMAIL ADDRESS ELEMENT  ' 2                            
         DC    C'SAL2SPN: S/A EMAIL ADDR ELEMENT ' 3                            
         DC    C'SAL2SPN: MO S/ASSIST NAME ELEMT ' 4                            
         DS    0F                                                               
ADVMSGS  DC    C'ADVERT : DESCRIPTOR ELEMENT     ' 1                            
         DC    C'ADVERT : ?????????? ELEMENT     ' 2                            
         DC    C'ADVERT : OFFICE     ELEMENT     ' 3                            
         DS    0F                                                               
PRDMSGS  DC    C'PRODUCT: DESCRIPTOR ELEMENT     ' 1                            
         DC    C'PRODUCT: NETWORK CONTRACT INFO  ' 2                            
         DC    C'PRODUCT: SPOTPAK INTRFACE INFO  ' 3                            
         DC    C'PRODUCT: AGY/FLT DATE VALIDATION' 4                            
         DS    0F                                                               
AGYMSGS  DC    C'AGENCY : DESCRIPTOR ELEMENT     ' 1                            
         DC    C'AGENCY : ?????????? ELEMENT     ' 2                            
         DS    0F                                                               
AGY2MSGS DC    C'AGENCY2: AGENCY FAX ELEMENT     ' 1                            
         DC    C'AGENCY2: DESCRIPTOR ELEMENT     ' 2                            
         DC    C'AGENCY2: EXPANDED ADDRESS       ' 3                            
         DC    C'AGENCY2: TAKEOVER DATA          ' 4                            
         DC    C'AGENCY2: AGENCY COMMENT         ' 5                            
         DC    C'AGENCY2: OFFICE     ELEMENT     ' 6                            
         DS    0F                                                               
         EJECT                                                                  
*                                                                               
*   TABLE FOR DESCRIPTIVE INFORMATION CONCERNING RECORD TYPES                   
*                                                                               
*        BYTE  1       =   RECORD TYPE                                          
*        BYTES 2  - 17 =   RECORD DESCRIPTION                                   
*                          ">" INDICATES FIRST POSITION TO INSERT               
*                              CODE FROM RECORD.  DON'T USE THIS                
*                              CHARACTER IN THIS FIELD FOR ANY                  
*                              OTHER PURPOSE.                                   
*        BYTE  18      =   DISPLACEMENT TO REP CODE IN RECORD                   
*        BYTE  19      =   DISPLACEMENT TO RECORD CODE IN RECORD                
*        BYTE  20      =   LENGTH OF RECORD CODE IN KEY                         
*        BYTES 21 - 32 =   SPARE                                                
*                                                                               
DHDRTYPE EQU   0                                                                
DHDRLABL EQU   1                                                                
DHDRREP  EQU   17                                                               
DHDRKTYP EQU   18                                                               
DHDRKLEN EQU   19                                                               
DHDRSPAR EQU   20                                                               
HDRTABLE DC    X'01',C'REP     >       '                                        
         DC    AL1(RREPKREP-RREPREC),AL1(RREPKREP-RREPREC)                      
         DC    X'02',12X'00'                                                    
LHDRTABL EQU   *-HDRTABLE                                                       
*                                                                               
         DC    X'02',C'STATION >       '                                        
         DC    AL1(RSTAKREP-RSTAREC),AL1(RSTAKSTA-RSTAREC)                      
         DC    X'05',12X'00'                                                    
*                                                                               
         DC    X'04',C'OFFICE  >       '                                        
         DC    AL1(ROFFKREP-ROFFREC),AL1(ROFFKOFF-ROFFREC)                      
         DC    X'02',12X'00'                                                    
*                                                                               
         DC    X'06',C'SALESPERSON >   '                                        
         DC    AL1(RSALKREP-RSALREC),AL1(RSALKSAL-RSALREC)                      
         DC    X'03',12X'00'                                                    
*                                                                               
         DC    X'08',C'ADVERTISER >    '                                        
         DC    AL1(RADVKREP-RADVREC),AL1(RADVKADV-RADVREC)                      
         DC    X'04',12X'00'                                                    
*                                                                               
         DC    X'09',C'PRODUCT >       '                                        
         DC    AL1(RPRDKREP-RPRDREC),AL1(RPRDKADV-RPRDREC)                      
         DC    X'07',12X'00'                                                    
*                                                                               
         DC    X'0A',C'AGENCY >        '                                        
         DC    AL1(RAGYKREP-RAGYREC),AL1(RAGYKAGY-RAGYREC)                      
         DC    X'06',12X'00'                                                    
*                                                                               
         DC    X'1A',C'AGNCY2 >        '                                        
         DC    AL1(RAGYKREP-RAGYREC),AL1(RAGYKAGY-RAGYREC)                      
         DC    X'06',12X'00'                                                    
*                                                                               
         DC    X'0000'             DELIMITER                                    
         DS    0F                                                               
*   RECTYPE TABLE:                                                              
*        FOR EACH TYPE OF RECORD FOUND ON THE RECOVERY FILE, THERE              
*        MAY BE AN ENTRY IN THIS TABLE DETAILING THE PROCESSING                 
*        TABLE TO ASSIGN TO THE COMPARISON OF THAT RECORD TYPE.                 
*                                                                               
*        IF THE TYPE OF RECORD IS NOT FOUND IN THIS TABLE, THE                  
*        RECORDS ARE TO BE SKIPPED / NOT COMPARED.                              
*                                                                               
DRECTYPE EQU   0                   DISPLACEMENT TO RECORD TYPE                  
DRECSPAR EQU   1                   DISPLACEMENT TO SPARE                        
*                                     FOR ALIGNMENT                             
DTYPETAB EQU   4                   DISPLACEMENT TO A(RECORD TYPE TABLE)         
DTYPMSGS EQU   8                   DISPLACEMENT TO A(RECORD TYPE MSGS)          
         DC    C'RECTYPE '                                                      
         DS    0F                                                               
RECTYPE  DC    X'01',AL3(0),A(REPTYPE),A(REPMSGS)                               
LRECTYPE EQU   *-RECTYPE                                                        
         DC    X'02',AL3(0),A(STATYPE),A(STAMSGS)                               
         DC    X'04',AL3(0),A(OFFTYPE),A(OFFMSGS)                               
         DC    X'06',AL3(0),A(SALTYPE),A(SALMSGS)                               
         DC    X'08',AL3(0),A(ADVTYPE),A(ADVMSGS)                               
         DC    X'09',AL3(0),A(PRDTYPE),A(PRDMSGS)                               
         DC    X'0A',AL3(0),A(AGYTYPE),A(AGYMSGS)                               
         DC    X'1A',AL3(0),A(AGY2TYPE),A(AGY2MSGS)                             
         DC    X'46',AL3(0),A(SAL2TYPE),A(SAL2MSGS)                             
         DC    X'00'                  DELIMITER                                 
         DS    0F                                                               
         EJECT                                                                  
*                                                                               
*   ***TYPE TABLE:                                                              
*        NOTE:  THIS TABLE MUST ALWAYS BEGIN ON A FULL WORD BOUNDARY.           
*        NOTE THAT IT CONTAINS THE ADDRESS OF A SPECIAL PROCESSING              
*        ROUTINE.  THAT ENTRY ALSO ***GENERATES A FULL-WORD ALIGNED             
*        ENTRY  !!  WATCH ALIGNMENT  !!                                         
*                                                                               
*        FOR EACH SPECIFIC RECORD TYPE FOUND IN THE 'RECTYPE' TABLE,            
*        THERE WILL BE A TABLE DESCRIBING THE PROCESSING OF THAT                
*        RECORD'S ELEMENTS.                                                     
*                                                                               
*        TABLE LAYOUT:                                                          
*              BYTE  1      =  ELEMENT CODE                                     
*              BYTE  2      =  Y = OPTIONAL ELEMENT                             
*                              M = MANDATORY ELEMENT                            
*              BYTE  3      =  M = MULTIPLE ELEMENTS MAY EXIST                  
*                                  DEFAULT = 1 ELEMENT ONLY                     
*              BYTE  4      =  Y = SPECIAL PROCESSING FOR ELEMENT               
*              BYTE  5 -  6 =  NUMBER OF MESSAGE LABEL                          
*                                  SEE XXXMSGS TABLE FOR RECT TYPE              
*              BYTE  7 -  8 =  SPARE (INSERTED FOR ALIGNMENT OF                 
*                                  ADDRESS FIELD)                               
*              BYTE  9 - 12 =  A(SPECIAL PROCESSING ROUTINE)                    
*              BYTE 13 - 16 =  A(SPECIAL CLEARING ROUTINE)                      
*                                                                               
DTABRTYP EQU   0                   DISPLACEMENT TO RECORD TYPE FIELD            
DTABOPT  EQU   1                   DISPLACEMENT TO OPTIONAL FLAG                
DTABMULT EQU   2                   DISPLACEMENT TO MULTIPLE FLAG                
DTABSPEC EQU   3                   DISPLACEMENT TO SPECL PROCESS FLAG           
DTABMSG# EQU   4                   DISPLACEMENT TO MESSAGE NUMBER               
DTABSPAR EQU   6                   DISPLACEMENT TO SPARE                        
DTABRTN  EQU   8                   DISPLACEMENT TO A(SPECL PROC RTN)            
DCLRRTN  EQU   12                  DISPLACEMENT TO A(SPECL CLEAR RTN)           
*                                                                               
REPTYPE  DC    X'01',C'M',C'N',C'Y',AL2(001),AL2(0),A(REPDESC),A(0)             
LELTTYPE EQU   *-REPTYPE                                                        
         DC    X'02',C'Y',C'M',C'N',AL2(002),AL2(0),A(0),A(0)                   
         DC    X'03',C'Y',C'M',C'N',AL2(003),AL2(0),A(0),A(0)                   
         DC    X'04',C'M',C'N',C'Y',AL2(004),AL2(0),A(REPPROF),A(0)             
         DC    X'000000'                                                        
         DS    0F                  ALIGNMENT                                    
         EJECT                                                                  
STATYPE  DC    X'01',C'M',C'N',C'Y',AL2(001),AL2(0),A(STADESC),A(0)             
         DC    X'02',C'Y',C'M',C'N',AL2(002),AL2(0),A(0),A(0)                   
**       DC    X'03',C'Y',C'N',C'N',AL2(000),AL2(0),A(0),A(0)                   
**       DC    X'04',C'Y',C'M',C'N',AL2(000),AL2(0),A(0),A(0)                   
         DC    X'05',C'Y',C'N',C'Y',AL2(003),AL2(0),A(STARECV),A(0)             
         DC    X'06',C'Y',C'M',C'Y',AL2(004),AL2(0),A(STASIGN),A(0)             
**       DC    X'07',C'Y',C'M',C'N',AL2(000),AL2(0),A(0),A(0)                   
         DC    X'08',C'M',C'N',C'Y',AL2(005),AL2(0),A(XTRADESC),A(0)            
**       DC    X'09',C'Y',C'M',C'N',AL2(000),AL2(0),A(0),A(0)                   
**       DC    X'0A',C'Y',C'M',C'N',AL2(000),AL2(0),A(0),A(0)                   
**       DC    X'0B',C'Y',C'M',C'N',AL2(000),AL2(0),A(0),A(0)                   
         DC    X'0C',C'Y',C'N',C'N',AL2(006),AL2(0),A(0),A(0)                   
         DC    X'0D',C'M',C'N',C'N',AL2(007),AL2(0),A(0),A(0)                   
         DC    X'0F',C'Y',C'N',C'N',AL2(008),AL2(0),A(0),A(0)                   
**       DC    X'10',C'Y',C'N',C'N',AL2(000),AL2(0),A(0),A(0)                   
         DC    X'11',C'Y',C'N',C'N',AL2(009),AL2(0),A(0),A(0)                   
         DC    X'13',C'Y',C'M',C'N',AL2(010),AL2(0),A(0),A(0)                   
         DC    X'23',C'M',C'N',C'N',AL2(011),AL2(0),A(0),A(0)                   
         DC    X'25',C'Y',C'M',C'N',AL2(012),AL2(0),A(0),A(0)                   
**       DC    X'48',C'Y',C'M',C'N',AL2(000),AL2(0),A(0),A(0)                   
**       DC    X'49',C'Y',C'M',C'N',AL2(000),AL2(0),A(0),A(0)                   
         DC    X'2C',C'Y',C'N',C'N',AL2(013),AL2(0),A(0),A(0)                   
         DC    X'000000'                                                        
         DS    0F                  ALIGNMENT                                    
         EJECT                                                                  
OFFTYPE  DC    X'01',C'M',C'N',C'Y',AL2(001),AL2(0),A(OFFDESC),A(0)             
         DC    X'000000'                                                        
         DS    0F                  ALIGNMENT                                    
         EJECT                                                                  
SALTYPE  DC    X'01',C'M',C'N',C'Y',AL2(001),AL2(0),A(SALDESC),A(0)             
         DC    X'000000'                                                        
         DS    0F                  ALIGNMENT                                    
         EJECT                                                                  
SAL2TYPE DC    X'01',C'M',C'N',C'Y',AL2(001),AL2(0),A(SAL2DESC),A(0)            
         DC    X'20',C'Y',C'M',C'N',AL2(002),AL2(0),A(0),A(0)                   
         DC    X'21',C'Y',C'M',C'N',AL2(003),AL2(0),A(0),A(0)                   
         DC    X'22',C'Y',C'M',C'N',AL2(004),AL2(0),A(0),A(0)                   
         DC    X'000000'                                                        
         DS    0F                  ALIGNMENT                                    
         EJECT                                                                  
ADVTYPE  DC    X'01',C'M',C'N',C'Y',AL2(001),AL2(0),A(ADVDESC),A(0)             
         DC    X'20',C'M',C'N',C'N',AL2(002),AL2(0),A(0),A(0)                   
         DC    X'50',C'Y',C'N',C'N',AL2(003),AL2(0),A(0),A(0)                   
         DC    X'000000'                                                        
         DS    0F                  ALIGNMENT                                    
         EJECT                                                                  
AGYTYPE  DC    X'01',C'M',C'N',C'Y',AL2(001),AL2(0),A(AGYDESC),A(0)             
         DC    X'02',C'Y',C'M',C'N',AL2(002),AL2(0),A(0),A(0)                   
         DC    X'000000'                                                        
         DS    0F                  ALIGNMENT                                    
AGY2TYPE DC    X'10',C'M',C'N',C'N',AL2(001),AL2(0),A(AGY2FAX),A(0)             
         DC    X'1F',C'M',C'N',C'Y',AL2(002),AL2(0),A(AGY2DESC),A(0)            
         DC    X'20',C'M',C'N',C'N',AL2(003),AL2(0),A(AGY2EXAD),A(0)            
         DC    X'30',C'Y',C'N',C'N',AL2(004),AL2(0),A(0),A(0)                   
         DC    X'40',C'Y',C'M',C'N',AL2(005),AL2(0),A(0),A(0)                   
         DC    X'50',C'Y',C'M',C'N',AL2(006),AL2(0),A(0),A(0)                   
         DC    X'000000'                                                        
         DS    0F                  ALIGNMENT                                    
PRDTYPE  DC    X'01',C'M',C'N',C'Y',AL2(001),AL2(0),A(PRDDESC)                  
         DC    A(PRD01CLR)                                                      
         DC    X'02',C'Y',C'N',C'N',AL2(002),AL2(0),A(PRDNETC),A(0)             
         DC    X'03',C'Y',C'N',C'N',AL2(003),AL2(0),A(PRDSPTPK),A(0)            
         DC    X'04',C'Y',C'N',C'N',AL2(004),AL2(0),A(PRDVALS),A(0)             
         DC    X'000000'                                                        
         DS    0F                  ALIGNMENT                                    
         EJECT                                                                  
*                                                                               
*   OPTION DESCRIPTION TABLES.  SET UP ONE FOR EACH ENTRY IN AN                 
*        ELEMENT LIST IN WHICH AN OPTION MUST BE COMPARED/EXPLODED.             
*                                                                               
REPOPT01 DC    C'PAPERWORK COUNTS'                                              
LOPTDESC EQU   *-REPOPT01                                                       
         DC    C'ATHENA ENABLED  '                                              
         DC    C'INV DEF=DIS/ADV '                                              
         DC    C'FRIDAY TESTING  '                                              
         DC    C'DAY=6A->559A    '                                              
         DC    C'CONTRACT -> NSI '                                              
         DC    C'ALT SPL SCREEN  '                                              
         DC    C'INV 2-LINE SCRN '                                              
         DC    C'DIR RESP CONTYPE'                                              
         DC    C'PAID PGM CONTYPE'                                              
         DC    C'UPLOAD ACTUALS  '                                              
         DC    C'OPTL RTS CONTYPE'                                              
         DC    C'NRGON AS-AT DATE'                                              
         DC    C'RTS: SUMMARY REP'                                              
         DC    C'BUDGET OPTIONS  '                                              
         DC    C'BACKBILLING OPT:'                                              
         DC    C'S/P BUDGET OPT  '                                              
         DC    C'SPOOL OPTION    '                                              
         DC    C'UNUSED          '                                              
         DC    C'PROPOSER CONTYPE'                                              
         DC    C'PROP: DECIMAL UP'                                              
         DC    C'PROP: DECIMAL DN'                                              
         DC    C'R10: SHOW $0    '                                              
         DC    C'R/F MODEL       '                                              
         DC    C'RXTRACT: PND/FC '                                              
         DC    C'UNUSED          '                                              
         DC    C'UNUSED          '                                              
         DC    C'DAILY PACING OPT'                                              
         DC    C'UNUSED          '                                              
         DS    0F                                                               
*                                                                               
XTROP108 DC    C'NO CFD WORKSHTS '                                              
         DC    C'SPACING OPTION  '                                              
         DC    C'#DS BUYLNS ALLWD'                                              
         DC    C'EASI AGENCY     '                                              
         DC    C'DARE REV OVERIDE'                                              
         DC    C'UNI STATUS      '                                              
         DC    C'PRINT CODES     '                                              
         DC    C'FORCE SPL < SEND'                                              
         DC    C'CHECK EOP CODES '                                              
         DS    0F                                                               
*                                                                               
XTROP208 DC    C'STD CMT REQUIRED'                                              
         DC    C'OVERRIDE PRF#31 '                                              
         DC    C'ALT CALENDAR STN'                                              
         DC    C'NON COMPETIT STN'                                              
         DC    C'OVERRIDE PRF#35 '                                              
         DC    C'BLOCK TAKEOVER  '                                              
         DC    C'FAX ADD"L WRKSHT'                                              
         DC    C'BIAS EC STN:    '                                              
         DS    0F                                                               
*                                                                               
XTROP308 DC    C'SECTION CDS REQD'                                              
         DC    C'SHOW PATTERN/NOT'                                              
         DC    C'EC CHANGES VERSN'                                              
         DC    C'ENCODA OVERRIDE '                                              
         DC    C'CONFIRMS VIA WEB'                                              
         DC    C'DO NOT FAX ORDER'                                              
         DC    C'EXCLUDE DEMO CTG'                                              
         DC    C'HFS FOR RMP O/P '                                              
         DS    0F                                                               
*                                                                               
XTROP408 DC    C'EC FROM REP SIDE'                                              
         DC    C'UNUSED          '                                              
         DC    C'UNUSED          '                                              
         DC    C'UNUSED          '                                              
         DC    C'UNUSED          '                                              
         DC    C'UNUSED          '                                              
         DC    C'UNUSED          '                                              
         DC    C'UNUSED          '                                              
         DS    0F                                                               
AGY1P101 DC    C'EASI AGENCY     '                                              
         DC    C'EASILINK AGY CPY'                                              
         DC    C'CNF GEN WKSHEET '                                              
         DC    C'DARE: NO CONTRCT'                                              
         DC    C'UNUSED          '                                              
         DC    C'UNUSED          '                                              
         DC    C'UNUSED          '                                              
         DC    C'UNUSED          '                                              
         DC    C'UNUSED          '                                              
         DC    C'FOOTE/LEO BURNET'                                              
         DS    0F                                                               
AGY1P102 DC    C'USE EXPANDED ADD'                                              
         DC    C'UNUSED          '                                              
         DC    C'AGY=BUYING SERVC'                                              
         DC    C'NBC VIA DOWNLOAD'                                              
         DC    C'CORP FROM AGY/OF'                                              
         DC    C'CONTRCT FOR CORP'                                              
         DC    C'CNTRCT DON"T USE'                                              
         DC    C'SWTCH DON"T DROP'                                              
         DS    0F                                                               
         EJECT                                                                  
*                                                                               
*   THIS IS THE DISTRIBUTION TABLE THAT IS USED IN REP TO SPOT XFER.            
*        IT MAY BE INCOMPLETE.  IT MAY BE INACCURATE.                           
*        ANYTHING NOT FINDING A MATCH IN THE TABLE WILL BE ROUTED TO            
*        SJR                                                                    
*        SEVERAL ENTRIES HAVE BEEN ADDED FOR INITIAL STARTUP.  ALL              
*        ARE DATED 3/17/05                                                      
*                                                                               
QUEUETAB EQU   *                                                                
         DC    CL2'IR',CL6'IRFL'   ROUTE PER P. GEORGE 7/25/97                  
LQUEUTAB EQU   *-QUEUETAB                                                       
***      DC    CL2'IR',CL6'IRNY'                                                
         DC    CL2'BL',CL6'BLRNY'                                               
         DC    CL2'PV',CL6'PETNY'                                               
         DC    CL2'K3',CL6'KRGDN ' ROUTE PER D. VALERIOTI 9/25/97               
***      DC    CL2'K3',CL6'KRGNY '                                              
         DC    CL2'K9',CL6'RADNY'                                               
         DC    CL2'RT',CL6'TRANY'                                               
         DC    CL2'KH',CL6'REPDEM'                                              
         DC    CL2'JS',CL6'JSNY'                                                
         DC    CL2'SJ',CL6'SJR'                                                 
         DC    CL2'V8',CL6'PMCNY'                                               
         DC    CL2'NP',CL6'NPTVNY'                                              
         DC    CL2'F8',CL6'FSNLAU'                                              
         DC    CL2'MR',CL6'KTVNY'    MAR7/02                                    
         DC    CL2'RA',CL6'RMRNY '   FEB/02 - REGIONAL MARKET RADIO             
         DC    CL2'2S',CL6'LERLA '   SEP/04 - LOTUS ENTRAVISION                 
         DC    CL2'FB',CL6'FSSNY '   ADDED:  BILL 3/17/05                       
         DC    CL2'UV',CL6'UNNY  '   ADDED:  BILL 3/17/05                       
         DC    CL2'NB',CL6'NBCNY '   ADDED:  BILL 3/17/05                       
         DC    CL2'CB',CL6'CBSNY '   ADDED:  BILL 3/17/05                       
         DC    CL2'B1',CL6'TELNY '   ADDED:  BILL 3/17/05                       
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
*   RELOCATION OF ELEMENT DECONSTRUCTION TABLES.                                
*                                                                               
*                                                                               
*   ALL SPECIAL PROCESSING TABLES ARE BUILT TO THE FOLLOWING                    
*       SPECIFICATIONS.  ONLY ONE SET OF DOCUMENTATION AND                      
*       TABLE DISPLACEMENTS WILL BE USED.                                       
*                                                                               
*   ELEMENT DATA COMPARISON TABLE                                               
*        BYTES  0   -  11  =  FIELD NAME                                        
*              12   -      =  DISPLACEMENT IN ELEMENT                           
*              13   -  1   =  INDICATOR                                         
*                             D  = DATE (DATCON NEEDED)                         
*                             C  = CHARACTER (STRAIGHT DISPLAY)                 
*                             O  = OPTION FIELD                                 
*                             N  = NUMERIC (DISPLAY W/EDIT)                     
*              14   -      =  DATE TYPE, IF 13 = D                              
*                             C  = 2-BYTE COMPRESSED                            
*                             B  = BINARY                                       
*                          =  OPTION TYPE, IF 13 = O                            
*                             C  = BYTE REPRESENTS ONE OPTION                   
*                             B  = BIT-LEVEL: BYTE REPS 8 OPTIONS               
*              15   -      =  FIELD LENGTH                                      
*              16   -  19  =  A(OPTION DEFINITION TABLE)                        
*                             16 = X'FF': DISPLAY BY LEN ONLY                   
*                             17 = X'FF': EXPLODE BINARY TO DISPLAY             
*                                  (16 MUST BE X'FF')                           
*              20   -  23  =  SPARE                                             
*                                                                               
SPRFNAME EQU   0                                                                
SPRDISP  EQU   12                                                               
SPRINDCT EQU   13                                                               
SPRDTYPE EQU   14                                                               
SPRLEN   EQU   15                                                               
SPROPT   EQU   16                                                               
SPREXPLD EQU   17                                                               
SPRSPARE EQU   20                                                               
*                                                                               
REP01ELT DC    C'REP NAME    ',AL1(RREPNAME-RREPELEM),C'C',C' '                 
         DC    AL1(33),8X'00'                                                   
         DC    C'REP ADDRESS ',AL1(RREPADDR-RREPELEM),C'C',C' '                 
         DC    AL1(33),8X'00'                                                   
         DC    C'REP PROFILES',AL1(RREPPROF-RREPELEM),C'O',C'C'                 
         DC    AL1(30),A(REPOPT01),4X'00'                                       
         DC    X'0000'                                                          
         DS    0F                                                               
*   THIS ELEMENT REQUIRES SOME EXPLANATION, AS ITS FIELDS ARE SET               
*        UP SLIGHTLY DIFFERENT THAN OTHER ELEMENTS.                             
*        THE ELEMENT CONSISTS OF A SERIES OF BIT ARRAYS, EACH OF                
*        WHICH REPRESENT A SET OF PROFILES FOR ONE PROGRAM                      
*        THE DSECT FOR THE ELEMENT DOES NOT CONTAIN INDIVIDUAL                  
*        LABELS FOR EACH PROFILE FIELD.                                         
*        EACH PROFILE IS THEREFORE INSERTED AS A SEPARATE ENTRY                 
*        IN THE TABLE.  THIS PROVIDES A PLACE TO INSERT A SEPARATE              
*        LABEL FOR EACH PROFILE.  THE DISPLACEMENT OF EACH FIELD                
*        IS BASICALLY HARD-CODED INTO EACH ENTRY.                               
*                                                                               
REP04ELT DC    C'CONTRACT PRF',AL1(06),C'O',C'B'                                
         DC    AL1(08),X'FF',X'FF',2X'00',4X'00'                                
         DC    C'FILE     PRF',AL1(16),C'O',C'B'                                
         DC    AL1(08),X'FF',X'FF',2X'00',4X'00'                                
         DC    C'INFO     PRF',AL1(26),C'O',C'B'                                
         DC    AL1(08),X'FF',X'FF',2X'00',4X'00'                                
         DC    C'INVOICE  PRF',AL1(36),C'O',C'B'                                
         DC    AL1(08),X'FF',X'FF',2X'00',4X'00'                                
         DC    C'REQUEST  PRF',AL1(46),C'O',C'B'                                
         DC    AL1(08),X'FF',X'FF',2X'00',4X'00'                                
         DC    C'RMP      PRF',AL1(56),C'O',C'B'                                
         DC    AL1(08),X'FF',X'FF',2X'00',4X'00'                                
         DC    C'RIS      PRF',AL1(66),C'O',C'B'                                
         DC    AL1(08),X'FF',X'FF',2X'00',4X'00'                                
         DC    C'RRGON    PRF',AL1(76),C'O',C'B'                                
         DC    AL1(08),X'FF',X'FF',2X'00',4X'00'                                
         DC    C'SFM      PRF',AL1(86),C'O',C'B'                                
         DC    AL1(08),X'FF',X'FF',2X'00',4X'00'                                
         DC    C'SIN      PRF',AL1(96),C'O',C'B'                                
         DC    AL1(08),X'FF',X'FF',2X'00',4X'00'                                
         DC    C'SEL WKSH PRF',AL1(106),C'O',C'B'                               
         DC    AL1(08),X'FF',X'FF',2X'00',4X'00'                                
         DC    C'DARE     PRF',AL1(116),C'O',C'B'                               
         DC    AL1(08),X'FF',X'FF',2X'00',4X'00'                                
         DC    C'SECURITY PRF',AL1(126),C'O',C'B'                               
         DC    AL1(08),X'FF',X'FF',2X'00',4X'00'                                
         DC    C'EZPOST   PRF',AL1(136),C'O',C'B'                               
         DC    AL1(08),X'FF',X'FF',2X'00',4X'00'                                
         DC    C'SOM      PRF',AL1(146),C'O',C'B'                               
         DC    AL1(08),X'FF',X'FF',2X'00',4X'00'                                
         DC    X'0000'                                                          
         DS    0F                                                               
STA01ELT DC    C'JOIN DATE   ',AL1(RSTASTRT-RSTAELEM),C'D',C'B'                 
         DC    X'03',8X'00'                                                     
LSTA01EL EQU   *-STA01ELT                                                       
         DC    C'LEAVE DATE  ',AL1(RSTAEND-RSTAELEM),C'D',C'B'                  
         DC    X'03',8X'00'                                                     
         DC    C'AFFILIATION ',AL1(RSTAAFFL-RSTAELEM),C'C',C' '                 
         DC    X'03',8X'00'                                                     
         DC    C'CLOSE DATE  ',AL1(RSTACLDT-RSTAELEM),C'D',C'B'                 
         DC    X'02',8X'00'                                                     
         DC    C'TRAFFIC SYST',AL1(RSTATRAF-RSTAELEM),C'C',C' '                 
         DC    X'01',8X'00'                                                     
         DC    C'OWNER       ',AL1(RSTAOWN-RSTAELEM),C'C',C' '                  
         DC    X'03',8X'00'                                                     
         DC    X'0000'                                                          
         DS    0F                                                               
XTR08ELT DC    C'XTRA DESCRIP',AL1(RSTAOPTS-RSTAXXEL),C'O',C'C'                 
         DC    AL1(09),A(XTROP108),4X'00'                                       
         DC    C'XTRA DESCR#2',AL1(RSTAOPTA-RSTAXXEL),C'O',C'B'                 
         DC    AL1(01),A(XTROP208),4X'00'                                       
         DC    C'XTRA DESCR#3',AL1(RSTAOPTB-RSTAXXEL),C'O',C'B'                 
         DC    AL1(01),A(XTROP308),4X'00'                                       
         DC    C'XTRA DESCR#4',AL1(RSTAOPTC-RSTAXXEL),C'O',C'B'                 
         DC    AL1(01),A(XTROP408),4X'00'                                       
         DC    C'OFF DEST CDE',AL1(RSTAORDS-RSTAXXEL),C'C',C' '                 
         DC    AL1(08),8X'00'                                                   
         DC    C'OFF DEST ID ',AL1(RSTAORID-RSTAXXEL),C'C',C' '                 
         DC    AL1(02),8X'00'                                                   
         DC    C'FAX NUMBER  ',AL1(RSTAOFAX-RSTAXXEL),C'C',C' '                 
         DC    AL1(13),8X'00'                                                   
         DC    C'INTERFACE CD',AL1(RSTAOSI-RSTAXXEL),C'C',C' '                  
         DC    AL1(10),8X'00'                                                   
         DC    X'0000'                                                          
         DS    0F                                                               
STA05ELT DC    C'RECVG SIGNON',AL1(RSTARSO-RSTAXEL),C'C',C' '                   
         DC    X'08',8X'00'                                                     
LSTA05EL EQU   *-STA05ELT                                                       
         DC    C'RECVG ID    ',AL1(RSTARID-RSTAXEL),C'C',C' '                   
         DC    X'02',8X'00'                                                     
         DC    X'0000'                                                          
         DS    0F                                                               
STA06ELT DC    C'SIGN ON CODE',AL1(RSTASO-RSTASOEL),C'C',C' '                   
         DC    X'08',8X'00'                                                     
LSTA06EL EQU   *-STA06ELT                                                       
         DC    C'SIGNON ID   ',AL1(RSTASID-RSTASOEL),C'C',C' '                  
         DC    X'02',8X'00'                                                     
         DC    X'0000'                                                          
         DS    0F                                                               
OFF01ELT DC    C'OFFICE NAME ',AL1(ROFFNAME-ROFFELEM),C'C',C' '                 
         DC    AL1(20),8X'00'                                                   
         DC    C'OFFC ADDR 1 ',AL1(ROFFADD1-ROFFELEM),C'C',C' '                 
         DC    AL1(20),8X'00'                                                   
         DC    C'OFFC ADDR 2 ',AL1(ROFFADD2-ROFFELEM),C'C',C' '                 
         DC    AL1(18),8X'00'                                                   
         DC    C'OFFICE STATE',AL1(ROFFSTT-ROFFELEM),C'C',C' '                  
         DC    AL1(02),8X'00'                                                   
         DC    X'0000'                                                          
         DS    0F                                                               
SAL01ELT DC    C'S/P NAME    ',AL1(RSALNAME-RSALELEM),C'C',C' '                 
         DC    AL1(20),8X'00'                                                   
         DC    C'TELEPHONE # ',AL1(RSALTEL-RSALELEM),C'C',C' '                  
         DC    AL1(12),8X'00'                                                   
         DC    C'TEAM        ',AL1(RSALTEAM-RSALELEM),C'C',C' '                 
         DC    AL1(02),8X'00'                                                   
         DC    C'S/P OFFICE  ',AL1(RSALOFF-RSALELEM),C'C',C' '                  
         DC    AL1(02),8X'00'                                                   
         DC    X'0000'                                                          
         DS    0F                                                               
SP201ELT DC    C'PAY S/P CODE',AL1(RSA2PSAL-RSA2ELEM),C'C',C' '                 
         DC    AL1(03),8X'00'                                                   
         DC    X'0000'                                                          
         DS    0F                                                               
ADV01ELT DC    C'ADVERT NAME ',AL1(RADVNAME-RADVELEM),C'C',C' '                 
         DC    AL1(20),8X'00'                                                   
         DC    C'ADVERT CITY ',AL1(RADVCITY-RADVELEM),C'C',C' '                 
         DC    AL1(20),8X'00'                                                   
         DC    C'ADVERT CLASS',AL1(RADVCLSS-RADVELEM),C'C',C' '                 
         DC    AL1(02),8X'00'                                                   
         DC    C'ADVERT CATGY',AL1(RADVCATG-RADVELEM),C'C',C' '                 
         DC    AL1(02),8X'00'                                                   
         DC    X'0000'                                                          
         DS    0F                                                               
PRD01ELT DC    C'PRODUCT NAME',AL1(RPRDNAME-RPRDELEM),C'C',C' '                 
         DC    AL1(20),8X'00'                                                   
         DC    C'PRODCT CLASS',AL1(RPRDCLSS-RPRDELEM),C'C',C' '                 
         DC    AL1(02),8X'00'                                                   
         DC    C'PRODCT CATGY',AL1(RPRDCATG-RPRDELEM),C'C',C' '                 
         DC    AL1(02),8X'00'                                                   
         DC    C'PRODCT NET# ',AL1(RPRDNET#-RPRDELEM),C'C',C' '                 
         DC    AL1(02),8X'00'                                                   
         DC    C'LAST UPDATED',AL1(RPRDUPD-RPRDELEM),C'D',C'C'                  
         DC    AL1(02),8X'00'                                                   
**       DC    C'PRD LOCKOUT ',AL1(RPRDLOCK-RPRDELEM),C'N',C' '                 
**       DC    AL1(02),8X'00'                                                   
         DC    C'PRD CONTYPE1',AL1(RPRDCTY1-RPRDELEM),C'C',C' '                 
         DC    AL1(01),8X'00'                                                   
         DC    C'PRD CONTYPE2',AL1(RPRDCTY2-RPRDELEM),C'C',C' '                 
         DC    AL1(01),8X'00'                                                   
         DC    C'PRD CONTYPE3',AL1(RPRDCTY3-RPRDELEM),C'C',C' '                 
         DC    AL1(01),8X'00'                                                   
         DC    C'PRD CONTYPE4',AL1(RPRDCTY4-RPRDELEM),C'C',C' '                 
         DC    AL1(01),8X'00'                                                   
         DC    X'0000'                                                          
         DS    0F                                                               
PRD02ELT DC    C'NETCON DESCR',AL1(RPRDNDES-RPRDNELM),C'C',C' '                 
         DC    AL1(20),8X'00'                                                   
         DC    C'NET POINTPER',AL1(RPRDNPNT-RPRDNELM),C'C',C' '                 
         DC    AL1(03),8X'00'                                                   
         DC    X'0000'                                                          
         DS    0F                                                               
PRD03ELT DC    C'SPAK CLIENT ',AL1(RPRDSPCL-RPRDSPOT),C'C',C' '                 
         DC    AL1(03),8X'00'                                                   
         DC    C'SPAK PRODUCT',AL1(RPRDSPP1-RPRDSPOT),C'C',C' '                 
         DC    AL1(03),8X'00'                                                   
         DC    C'SPAK PIGGY  ',AL1(RPRDSPP2-RPRDSPOT),C'C',C' '                 
         DC    AL1(03),8X'00'                                                   
         DC    C'SPAK EST #  ',AL1(RPRDSPES-RPRDSPOT),C'N',C' '                 
         DC    AL1(01),8X'00'                                                   
         DC    X'0000'                                                          
         DS    0F                                                               
PRD04ELT DC    C'VAL: AGENCY ',AL1(RPRDAGAG-RPRDAGFL),C'C',C' '                 
         DC    AL1(06),8X'00'                                                   
         DC    C'VAL: DT FROM',AL1(RPRDAGDF-RPRDAGFL),C'D',C'B'                 
         DC    AL1(03),8X'00'                                                   
         DC    C'VAL: DT TO  ',AL1(RPRDAGDT-RPRDAGFL),C'D',C'B'                 
         DC    AL1(03),8X'00'                                                   
         DC    X'0000'                                                          
         DS    0F                                                               
AGY01ELT DC    C'AGENCY NAME ',AL1(RAGYNAM1-RAGYELEM),C'C',C' '                 
         DC    AL1(20),8X'00'                                                   
         DC    C'AGENCY NAME2',AL1(RAGYNAM2-RAGYELEM),C'C',C' '                 
         DC    AL1(33),8X'00'                                                   
         DC    C'AGENCY ADDR1',AL1(RAGYADD1-RAGYELEM),C'C',C' '                 
         DC    AL1(20),8X'00'                                                   
         DC    C'AGENCY ADDR2',AL1(RAGYADD2-RAGYELEM),C'C',C' '                 
         DC    AL1(20),8X'00'                                                   
         DC    C'AGENCY STATE',AL1(RAGYSTAT-RAGYELEM),C'C',C' '                 
         DC    AL1(02),8X'00'                                                   
         DC    C'AGENCY ZIP  ',AL1(RAGYZIP-RAGYELEM),C'C',C' '                  
         DC    AL1(10),8X'00'                                                   
         DC    C'AGENCY PROFS',AL1(RAGYPROS-RAGYELEM),C'O',C'C'                 
         DC    AL1(10),A(AGY1P101),4X'00'                                       
         DC    C'AGENCY RISK ',AL1(RAGYRISK-RAGYELEM),C'C',C' '                 
         DC    AL1(01),8X'00'                                                   
         DC    C'AGENCY LIAB ',AL1(RAGYRISK-RAGYELEM),C'C',C' '                 
         DC    AL1(01),8X'00'                                                   
         DC    C'AGENCY FLAGS',AL1(RAGYFLAG-RAGYELEM),C'O',C'B'                 
         DC    AL1(01),A(AGY1P102),4X'00'                                       
         DC    X'0000'                                                          
         DS    0F                                                               
AG210ELT DC    C'EZL FAX NUM ',AL1(RAGY2FAX-RAGY2FXE),C'C',C' '                 
         DC    AL1(10),8X'00'                                                   
         DC    C'AGY PHONE # ',AL1(RAGY2FON-RAGY2FXE),C'C',C' '                 
         DC    AL1(10),8X'00'                                                   
         DC    C'DARE EQUIVS ',AL1(RAGY2DAR-RAGY2FXE),C'C',C' '                 
         DC    AL1(20),8X'00'                                                   
         DS    0F                                                               
AG21FELT DC    C'AGENCY NAME ',AL1(RAG2NAM1-RAG2ELEM),C'C',C' '                 
         DC    AL1(20),8X'00'                                                   
         DC    C'AGENCY NAME2',AL1(RAG2NAM2-RAG2ELEM),C'C',C' '                 
         DC    AL1(33),8X'00'                                                   
         DC    C'AGENCY PROFS',AL1(RAG2PROS-RAG2ELEM),C'O',C'C'                 
         DC    AL1(10),A(AGY1P101),4X'00'                                       
         DC    C'AGENCY RISK ',AL1(RAG2RISK-RAG2ELEM),C'C',C' '                 
         DC    AL1(01),8X'00'                                                   
         DC    C'AGENCY LIAB ',AL1(RAG2RISK-RAG2ELEM),C'C',C' '                 
         DC    AL1(01),8X'00'                                                   
         DC    C'AGENCY FLAGS',AL1(RAG2FLAG-RAG2ELEM),C'O',C'B'                 
         DC    AL1(01),A(AGY1P102),4X'00'                                       
         DC    X'0000'                                                          
         DS    0F                                                               
AG220ELT DC    C'AG EXP NAME1',AL1(RAGY2AD1-RAGY2AE1),C'C',C' '                 
         DC    AL1(34),8X'00'                                                   
         DC    C'AG EXP NAME2',AL1(RAGY2AD2-RAGY2AE1),C'C',C' '                 
         DC    AL1(34),8X'00'                                                   
         DC    C'AG EXP CITY ',AL1(RAGY2CTY-RAGY2AE1),C'C',C' '                 
         DC    AL1(20),8X'00'                                                   
         DC    C'AG EXP STATE',AL1(RAGY2STE-RAGY2AE1),C'C',C' '                 
         DC    AL1(02),8X'00'                                                   
         DC    C'AG EXP ZIP  ',AL1(RAGY2ZIP-RAGY2AE1),C'C',C' '                 
         DC    AL1(10),8X'00'                                                   
         DS    0F                                                               
*                                                                               
*   CLEARANCE ROUTINE ELEMENT DESCRIPTIONS                                      
*       DURING THE COMPARISON OF AN OLD AND A NEW ELEMENT, THERE                
*       ARE CASES WHERE THE ELEMENT CONTAINS INFORMATION THAT                   
*       SHOULD NOT BE CONSIDERED DURING THE COMPARISON.                         
*       A CASE IN POINT IS THE 'PRODUCT LOCKOUT COUNT' WHICH IS                 
*           CHANGED BY THE CONTRACT PROGRAM AS A FUNCTION OF                    
*           NORMAL PROCESSING.                                                  
*       THE CLEAR ROUTINE DESCRIBES THE DISPLACEMENT AND LENGTH                 
*           OF FIELDS IN AN ELEMENT THAT ARE TO BE CLEARED TO                   
*           BINARY ZERO IN BOTH THE OLD AND THE NEW ELEMENTS                    
*           TO EFFECTIVELY REMOVE THEM FROM THE COMPARISON.                     
*       THE ENTRY DESCRIPTION IS THE SAME AS THE ELEMENT COMPARISON             
*           TABLE.  SIGNIFICANT FIELDS ARE THE DISPLACEMENT AND                 
*           THE FIELD LENGTH. SAME EQUATES ARE USED.                            
*                                                                               
PCL01ELT DC    C'PRODUCT LOCK',AL1(RPRDLOCK-RPRDELEM),C'C',C' '                 
         DC    AL1(02),8X'00'                                                   
         DC    X'0000'                                                          
         DS    0F                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045REREPSOX  03/18/15'                                      
         END                                                                    
