*          DATA SET REREPSALE  AT LEVEL 154 AS OF 05/01/02                      
*PHASE REST02B,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE HEXIN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE XSORT                                                                  
*INCLUDE SCANNER                                                                
         TITLE 'REREPSTID - INTEREP STATION ID UPLOAD'                          
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPSTID -- INSERT STATION ID/FREQUENCY                  *            
*                     FOR ALL REPS/STATIONS IN SPREADSHEET         *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* FEB13/01 (BU ) --- ORIGINAL ENTRY                                *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*  LIST OF DISPLAYS (REQUESTOR VALUE TRIGGER = Y IN BYTE:)         *            
*     QUESTOR     Y = DISPLAY STATION TABLE                        *            
*     QUESTOR+1   Y = STATION RECORD PRE AND POST                  *            
*     QUESTOR+2   Y = DISPLAY SCANNER RESULTS                      *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
REST02   CSECT                                                                  
         NMOD1 0,**ST02**,R8,R9,RR=R5                                           
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BE    SW10                                                             
         XIT1                                                                   
         EJECT                                                                  
SW10     DS    0H                                                               
         OPEN  (INTAPE1,(INPUT))   TAPE1:  STATION LIST                         
*                                                                               
         GOTO1 INITIAL,DMCB,(RC)                                                
*                                                                               
         GOTO1 LOADTABL,DMCB,(RC)                                               
*                                                                               
         CLOSE (INTAPE1,REWIND)                                                 
*                                                                               
         GOTO1 FILEPROC,DMCB,(RC)  PROCESS BULK OF FILE                         
*                                                                               
         GOTO1 DISPTOTS,DMCB,(RC)  DISPLAY TOTALS FOR RUN                       
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P+1(31),=C'***MAIN PROCESSING COMPLETED***'                      
         GOTO1 REPORT                                                           
*                                                                               
         XIT1                      EXIT                                         
         EJECT                                                                  
******************************************************************              
*  FILEPROC:  PROCESS EACH ENTRY IN THE STATION LIST             *              
*        RETRIEVE EACH STATION RECORD, AND INSERT THE STATION ID *              
*        AND FREQUENCY.                                          *              
******************************************************************              
FILEPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XC    KEY,KEY                                                          
         MVI   KEY,1               RETRIEVE REP RECORD                          
         MVC   KEY+25(2),RCREPFL                                                
         GOTO1 HIGHDIR                                                          
         GOTO1 GETRECRD                                                         
         LA    R5,REC                                                           
         USING RREPREC,R5                                                       
         MVC   P+1(07),=C'REP IS:'                                              
         MVC   P+10(33),RREPNAME                                                
         GOTO1 REPORT                                                           
         DROP  R5                                                               
*                                                                               
         L     R5,ASTAAREA         SET A(STATION LIST)                          
FPRO0020 EQU   *                                                                
         CLI   0(R5),X'FF'         END OF TABLE REACHED?                        
         BE    FPRO0900            YES - JOB FINISHED                           
         TM    DSTATUS(R5),X'80'   'DON'T PROCESS' TURNED ON?                   
         BNO   FPRO0030            NO  - PROCESS THIS ONE                       
         MVC   P+1(14),=C'NOT PROCESSED:'                                       
         MVC   P+16(PRTTABLN),0(R5)                                             
         GOTO1 REPORT                                                           
         L     RF,SKIPCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,SKIPCTR                                                       
         B     FPRO0200                                                         
FPRO0030 EQU   *                                                                
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,2               INSERT STATION KEY TYPE                      
         MVC   KEY+20(2),DREPCODE(R5)                                           
*                                  INSERT REP CODE                              
         MVC   KEY+22(5),DSTATION(R5)                                           
*                                  INSERT STATION CALL LETTERS                  
         GOTO1 HIGHDIR                                                          
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    FPRO0040            YES                                          
         MVC   P+1(20),=C'STATION NOT ON FILE:'                                 
         MVC   P+23(PRTTABLN),0(R5)                                             
         MVC   P+23+PRTTABLN+3(27),KEYSAVE                                      
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         L     RF,SKIPCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,SKIPCTR                                                       
         B     FPRO0200            SKIP PROCESSING THIS TABLE ENTRY             
FPRO0040 EQU   *                                                                
         GOTO1 GETRECRD                                                         
         LA    R7,REC                                                           
         USING RSTAREC,R7                                                       
*                                                                               
*   TEST                                                                        
         CLI   QUESTOR+1,C'Y'      DISPLAY RECORD?                              
         BNE   TEST0010                                                         
         MVC   P+1(21),=C'STATION RECORD: PRE :'                                
         GOTO1 REPORT                                                           
         LA    R4,RSTAREC          A(CONTRACT RECORD)                           
         SR    RF,RF                                                            
         ICM   RF,3,RSTALEN                                                     
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
TEST0010 EQU   *                                                                
*   TEST PRNTBL END                                                             
*                                                                               
         OC    DSTAFREQ(4,R5),DSTAFREQ(R5)                                      
*                                  ANYTHING IN FREQUENCY FIELD?                 
         BZ    FPRO0050            NO                                           
         MVC   RSTACHAN,DSTAFREQ+2(R5)                                          
*                                  ONLY STORE 2 CHARACTERS                      
FPRO0050 EQU   *                                                                
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'0F'        LOOK FOR X'0F' ELT                           
         BAS   RE,GETEL                                                         
         BNE   FPRO0060            NOT FOUND                                    
         USING RSTAINEL,R6                                                      
         MVC   RSTAINID,DSTATID#(R5)                                            
*                                  INSERT NEW STATION ID #                      
         DROP  R6                                                               
         B     FPRO0080                                                         
FPRO0060 EQU   *                                                                
         MVC   NEWSTAID,DSTATID#(R5)                                            
*                                  INSERT NEW STATION ID #                      
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RSTAREC,NEW0FELT,0             
*                                  INSERT NEW ID ELEMENT                        
         B     FPRO0080                                                         
*                                                                               
*                0                   1                                          
*                0.1.2.3.4.5.6.7.8.9.0.1.                                       
NEW0FELT DC    X'0F0C00000000000000000000'                                      
         ORG   NEW0FELT                                                         
         DS    XL2                                                              
NEWSTAID DS    XL4                                                              
         DS    XL6                                                              
         DS    0H                                                               
FPRO0080 EQU   *                                                                
*                                                                               
*   TEST                                                                        
         CLI   QUESTOR+1,C'Y'      DISPLAY RECORD?                              
         BNE   TEST0020                                                         
         MVC   P+1(21),=C'STATION RECORD: POST:'                                
         GOTO1 REPORT                                                           
         LA    R4,RSTAREC          A(CONTRACT RECORD)                           
         SR    RF,RF                                                            
         ICM   RF,3,RSTALEN                                                     
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
TEST0020 EQU   *                                                                
*   TEST PRNTBL END                                                             
*                                                                               
                                                                                
         CLI   QOPTION2,C'U'       UPDATE FILE?                                 
         BNE   FPRO0100            NO                                           
         GOTO1 PREC                                                             
FPRO0100 EQU   *                                                                
FPRO0200 EQU   *                                                                
         LA    R5,STATABLN(R5)     BUMP TO NEXT TABLE ENTRY                     
         B     FPRO0020                                                         
FPRO0900 EQU   *                                                                
         MVC   P+1(21),=C'UPDATE PASS COMPLETED'                                
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
* DISPLAY TOTALS                                                                
******************************************************************              
DISPTOTS NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVC   P+1(30),=C'STATIONS      PROCESSED      :'                       
         EDIT  STACTR,(12,P+36),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'STATIONS      SKIPPED        :'                       
         EDIT  SKIPCTR,(12,P+36),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
******************************************************************              
*   INITIALIZATIONS ....                                                        
*                                                                               
*   NEED TO LOAD UP A TABLE OF ALL EXISTING STATIONS/REPS ON FILE               
*        TO DETERMINE IF A STATION RECORD MUST BE GENERATED.                    
*                                                                               
*                                                                               
*                                                                               
******************************************************************              
INITIAL  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         MVC   P+1(23),=C'FILE WRITING IS ENABLED'                              
         CLI   QOPTION2,C'U'       UPDATE RUN?                                  
         BE    INIT0010                                                         
         MVC   P+1(24),=C'FILE WRITING IS DISABLED'                             
         MVC   PREC(2),=X'0000'    FORCE DUMP                                   
INIT0010 EQU   *                                                                
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         SPACE 1                                                                
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     RF,COVAIL                                                        
         DROP  RF                                                               
         GOTO1 (RF),DMCB,C'GET',800000,800000                                   
         OC    P2(4),P2                                                         
         BNZ   INIT0020                                                         
         MVC   P+1(30),=C'CANNOT GET SPACE: COVAIL ABORT'                       
         GOTO1 REPORT                                                           
         DC    H'0'                                                             
INIT0020 EQU   *                                                                
         L     RF,P2               INITIALIZE THE WORK AREA                     
         XCEFL 0(RF),P3                                                         
         MVC   LBLDAREA,P3         L(ADD'L SPACE GIVEN)                         
         MVI   RCSUBPRG,2          SET HEADING FLAG                             
         MVC   ABLDAREA,P2         ADDITIONAL WORKSPACE                         
         MVC   ASTAAREA,P2         A(STATION 1 TABLE)                           
         MVC   ANEXTSTA,P2         A(NEXT STATION SLOT)                         
         L     RF,ABLDAREA         ESTABLISH WORKSPACE SETUP                    
         A     RF,=F'300000'       400K FOR STATION 1 STORAGE                   
*                                    4,000 ENTRIES * 100 BYTES/ENTRY            
*                                  +0    20 BYTES REP NAME                      
*                                  +20   08 BYTES STATION ID NUMBER             
*                                  +28   05 BYTES STATION CALL LETTERS          
*                                  +33   04 BYTES STATION FREQ                  
*                                  +37   25 BYTES STATION CITY NAME             
         ST    RF,AIO2             A(IOAREA #2)                                 
*                                                                               
         A     RF,=F'4000'         4K FOR ALTERNATE IO AREA                     
         ST    RF,ATAPEREC         A(TAPE RECORD READ AREA)                     
*                                                                               
         A     RF,=F'4000'         4K FOR TAPE READ AREA                        
         ST    RF,AWORKBLK         A(WORK BLOCK FOR SCANNER)                    
*                                                                               
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE FIRST PAGE BREAK                       
                                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
******************************************************************              
*   LOAD TABLE VALUES                                                           
******************************************************************              
*                                                                               
*   SCANNER DISPLACEMENT EQUATES: TO BEGINNING OF SCANNER WORK FIELD            
*                                                                               
REPNAME  EQU   0                                                                
STATID#  EQU   32                                                               
STATIONX EQU   64                  DON'T USE THIS FIELD: NOT CORRECT            
STATION  EQU   96                                                               
STAFREQ  EQU   128                                                              
STACITY  EQU   160                                                              
*                                                                               
*                                                                               
*   STATION SLOT DISPLACEMENT EQUATES: SLOTS IN TABLE ASTAAREA                  
*                                                                               
DREPNAME EQU   0                   D(REP NAME)                                  
DREPCODE EQU   18                  D(REP POWER CODE)                            
DSTATID# EQU   20                  D(STATION ID NUMBER)                         
DSTATION EQU   28                  D(STATION CALL LETTERS)                      
DSTAFREQ EQU   33                  D(STATION FREQUENCY)                         
DSTATUS  EQU   38                  D(STATUS FLAGS)                              
*                                  X'80' - ERROR: DON'T UPDATE                  
DSTACITY EQU   40                  D(STATION CITY)                              
*                                                                               
*                                                                               
LOADTABL NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         LA    RF,REC              SET A(RECORD)                                
         ST    RF,AREC                                                          
*                                                                               
         L     R5,ATAPEREC         SET A(TAPE INPUT AREA)                       
LDIR0020 EQU   *                                                                
         L     R7,ANEXTSTA         SET A(NEXT STATION SLOT)                     
         XC    0(250,R5),0(R5)     CLEAR INPUT AREA                             
         GET   INTAPE1,(R5)        READ STATION TAPE RECORD INTO RDA            
*                                                                               
*   TEST COUNTER END                                                            
*                                                                               
**       CLC   STACTR,=F'10'       PROCESS FIRST N FOR TEST                     
**       BH    LDIR0400                                                         
*                                                                               
         L     RF,STATOTAL                                                      
         LA    RF,1(RF)                                                         
         ST    RF,STATOTAL                                                      
*                                     TAPE WILL BE PARSED ON ',' SEPS           
*        GOTO1 REPORT                                                           
*        MVC   P+1(13),=C'INPUT RECORD:'                                        
*        MVC   P+20(80),0(R5)                                                   
*        GOTO1 REPORT                                                           
*                                                                               
         XC    MYWORK,MYWORK       CLEAR WORK SPACE                             
         ZICM  RF,0(R5),2          GET LENGTH OF INPUT RECORD                   
         BCTR  RF,0                SUBTRACT 1 FOR MOVE                          
         EX    RF,LDIRMOV1         MOVE BY LENGTH                               
         B     LDIR0040                                                         
LDIRMOV1 MVC   MYWORK+8(0),4(R5)                                                
LDIR0040 EQU   *                                                                
         ZICM  RF,0(R5),2          LOAD LENGTH OF RECORD                        
         SH    RF,=H'4'            SUBTRACT CONTROL LENGTH                      
         STC   RF,MYWORK+5                                                      
*        MVC   P+1(15),=C'MYWORK  RECORD:'                                      
*        MVC   P+20(80),MYWORK                                                  
*        GOTO1 REPORT                                                           
*                                                                               
LDIR0060 EQU   *                                                                
         L     RF,AWORKBLK         SET A(WORKSPACE)                             
         XC    0(256,RF),0(RF)     CLEAR WORK SPACE                             
         GOTO1 =V(SCANNER),DMCB,MYWORK,(5,AWORKBLK),C',=,='                     
                                                                                
         CLI   DMCB+4,0                                                         
         BNE   LDIR0080                                                         
         DC    H'0'                                                             
*                                                                               
LDIR0080 EQU   *                                                                
*                                                                               
*   TEST PRNTBL                                                                 
         CLI   QUESTOR+2,C'Y'      DISPLAY SCAN RESULT?                         
         BNE   TEST0030                                                         
         MVC   P+1(12),=C'SCANNER OKAY'                                         
         GOTO1 REPORT                                                           
         L     R4,AWORKBLK         A(SCANNER OUTPUT)                            
         LA    RF,256                                                           
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
TEST0030 EQU   *                                                                
*   TEST PRNTBL END                                                             
*                                                                               
         L     RF,AWORKBLK         SET A(WORKSPACE)                             
         LA    RF,256(RF)                                                       
         ST    RF,AWRKBLK2         SET SECOND WORKBLOCK                         
         XC    0(256,RF),0(RF)     CLEAR WORK SPACE                             
         LA    RF,256(RF)                                                       
         ST    RF,AWRKBLK3         SET THIRD WORKBLOCK                          
         XC    0(256,RF),0(RF)     CLEAR WORK SPACE                             
         L     RE,AWORKBLK         SET A(WORKSPACE)                             
         XC    MYWORK2,MYWORK2                                                  
         MVC   MYWORK2+8(12),STATION+12(RE)   SET STATION                       
         MVC   MYWORK2+5(01),STATION(RE)      SET LENGTH                        
*                                  SPLIT STATION/MEDIA INPUT                    
         GOTO1 =V(SCANNER),DMCB,MYWORK2,AWRKBLK2,C',=-='                        
                                                                                
         CLI   DMCB+4,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                                                                               
*   TEST PRNTBL                                                                 
         CLI   QUESTOR+2,C'Y'      DISPLAY SCAN RESULT?                         
         BNE   TEST0040                                                         
         MVC   P+1(12),=C'STASCAN OKAY'                                         
         GOTO1 REPORT                                                           
         L     R4,AWRKBLK2         A(SCANNER OUTPUT)                            
         LA    RF,256                                                           
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
TEST0040 EQU   *                                                                
***      DC    H'0'                                                             
*   TEST PRNTBL END                                                             
*                                                                               
         L     R2,AWORKBLK         SET A(WORKSPACE)                             
         L     R4,AWRKBLK2                                                      
         BAS   RE,TREPNAME         TEST/LOAD REPNAME TO TABLE                   
         BAS   RE,TSTATID          TEST/LOAD STATION ID # TO TABLE              
         BAS   RE,TSTATION         TEST/LOAD STATION CALLS TO TABLE             
         BAS   RE,TSTAFREQ         TEST/LOAD STATION FREQ TO TABLE              
         LA    R7,STATABLN(R7)     BUMP TO NEXT AVAILABLE SLOT                  
         ST    R7,ANEXTSTA                                                      
         MVC   0(2,R7),=X'FFFF'    SET END OF TABLE                             
         L     R1,STACTR                                                        
         LA    R1,1(R1)            INCREMENT COUNTER                            
         ST    R1,STACTR           SAVE COUNT                                   
         B     LDIR0020            GO BACK FOR NEXT RECORD                      
         EJECT                                                                  
TREPNAME NTR1                                                                   
         MVC   DREPNAME(8,R7),REPNAME+12(R2)                                    
         LA    R3,COCONVRT         SET A(REPNAME TABLE)                         
TREP0020 EQU   *                                                                
         CLI   0(R3),0             END OF TABLE?                                
         BE    TREP0200            YES - NOT FOUND                              
         CLI   REPNAME+13(R2),X'50' SECOND CHARACTER = '&'?                     
         BNE   TREP0030            NO                                           
         MVI   REPNAME+13(R2),C'+' YES - REPLACE WITH '+'                       
TREP0030 EQU   *                                                                
         CLC   0(8,R3),REPNAME+12(R2)                                           
         BE    TREP0040            CARD NAME FOUND IN TABLE                     
         LA    R3,LCOCONV(R3)      NOT FOUND: BUMP TO NEXT SLOT                 
         B     TREP0020            GO BACK FOR NEXT                             
TREP0040 EQU   *                                                                
         MVC   DREPCODE(2,R7),12(R3)                                            
*                                  INSERT POWER CODE INTO TABLE                 
         B     TREP0240                                                         
TREP0200 EQU   *                                                                
         OI    DSTATUS(R7),X'80'   SET ERROR FOUND FLAG                         
         MVC   P+1(14),=C'REP NOT FOUND:'                                       
         MVC   P+20(8),REPNAME+12(R2)                                           
         GOTO1 REPORT                                                           
TREP0240 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
TSTATID  NTR1                                                                   
         LA    R3,STATID#(R2)      SET A(STATION ID#) HEADER FIELD              
         BAS   RE,PACKIT           CHECK STATION ID # FIELD                     
         BZ    TSTA0200            ERROR RETURNED                               
         ST    R0,DUB              SET RETURNED VALUE                           
         MVC   DSTATID#(4,R7),DUB                                               
*                                  INSERT POWER CODE INTO TABLE                 
         B     TSTA0240                                                         
TSTA0200 EQU   *                                                                
         OI    DSTATUS(R7),X'80'   SET ERROR FOUND FLAG                         
         MVC   P+1(14),=C'ID# NOT VALID:'                                       
         MVC   P+20(8),STATID#+12(R2)                                           
         GOTO1 REPORT                                                           
TSTA0240 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
TSTATION NTR1                                                                   
         MVC   DSTATION(4,R7),12(R4)     LOAD 4-CHAR STATION CALLS              
         MVC   DSTATION+4(1,R7),44(R4)   LOAD MEDIA CODE                        
         XIT1                                                                   
         EJECT                                                                  
TSTAFREQ NTR1                                                                   
         LA    R3,STAFREQ(R2)      SET A(STATION FREQ) HEADER FIELD             
         TM    2(R3),X'80'         NUMERIC FREQUENCY?                           
         BO    TFRQ0020            YES - USE WORKBLOCK AS I/P                   
         L     RE,AWORKBLK         SET A(WORKSPACE)                             
         XC    MYWORK2,MYWORK2                                                  
         MVC   MYWORK2+8(12),STAFREQ+12(RE)   SET STATION FREQ                  
         MVC   MYWORK2+5(01),STAFREQ(RE)      SET LENGTH                        
*                                  SPLIT STATION FREQUENCY                      
         GOTO1 =V(SCANNER),DMCB,MYWORK2,AWRKBLK3,C',=.='                        
                                                                                
         CLI   DMCB+4,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                                                                               
*   TEST PRNTBL                                                                 
         CLI   QUESTOR+2,C'Y'      DISPLAY SCAN RESULT?                         
         BNE   TEST0050                                                         
         MVC   P+1(12),=C'FREQ    OKAY'                                         
         GOTO1 REPORT                                                           
         L     R4,AWRKBLK3         A(SCANNER OUTPUT)                            
         LA    RF,256                                                           
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
TEST0050 EQU   *                                                                
***      DC    H'0'                                                             
*   TEST PRNTBL END                                                             
*                                                                               
         L     R3,AWRKBLK3         SET A(WORKBLOCK3) HEADER FIELD               
         TM    2(R3),X'80'         NUMERIC IN FIRST FIELD?                      
         BNO   TFRQ0200            NO  - ERROR                                  
         TM    2+32(R3),X'80'      NUMERIC IN SECOND FIELD?                     
         BNO   TFRQ0200            NO  - ERROR                                  
         ZIC   RE,0(R3)            BUILD NEW LENGTH                             
         LA    R1,12(R3)           SET A(FIRST FIELD)                           
         AR    R1,RE               BUMP TO END                                  
         ZIC   RF,32(R3)           SET L(SECOND HALF)                           
         AR    RE,RF               FIGURE NEW LENGTH                            
         STC   RE,0(R3)            PUT BACK INTO FIELD                          
         ZIC   R2,32(R3)           SET L(SECOND FIELD)                          
         BCTR  R2,0                BACK OFF 1 FOR EX                            
         EX    R2,TFRQMV01         MOVE BY LENGTH                               
         B     TFRQ0020                                                         
TFRQMV01 MVC   0(0,R1),12+32(R3)   MOVE BY LENGTH                               
TFRQ0020 EQU   *                                                                
         BAS   RE,PACKIT           CHECK STATION FREQ FIELD                     
         BZ    TFRQ0200            ERROR RETURNED                               
         ST    R0,DUB              SET RETURNED VALUE                           
         MVC   DSTAFREQ(4,R7),DUB                                               
*                                  INSERT POWER CODE INTO TABLE                 
         B     TFRQ0240                                                         
TFRQ0200 EQU   *                                                                
         OI    DSTATUS(R7),X'80'   SET ERROR FOUND FLAG                         
         MVC   P+1(15),=C'FREQ NOT VALID:'                                      
         MVC   P+20(8),MYWORK2+8                                                
         GOTO1 REPORT                                                           
TFRQ0240 EQU   *                                                                
         XIT1                                                                   
PACKIT   NTR1                                                                   
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,0(R3)            EXTRACT LENGTH OF FIELD                      
         LTR   R1,R1                                                            
         BZ    PACKX               LENGTH ERROR                                 
         TM    2(R3),X'80'         NUMERIC FIELD SET?                           
         BZ    PACKX               NO  - THIS IS AN ERROR                       
         BCTR  R1,0                                                             
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
PACKX    XIT1  REGS=(R0)                                                        
         SPACE 1                                                                
VARPACK  PACK  DUB,12(0,R3)                                                     
         SPACE 1                                                                
         EJECT                                                                  
                                                                                
***>>>                                                                          
*                .2.4.6.8.0.2                                                   
COCONVRT DC    C'ABC         ',C'IB'                                            
LCOCONV  EQU   *-COCONVRT                                                       
         DC    C'ALLIED      ',C'AQ'                                            
         DC    C'CABALLERO   ',C'MG'                                            
         DC    C'CUMULUS     ',C'UO'                                            
         DC    C'INFINITY    ',C'IF'                                            
         DC    C'MCGAVREN    ',C'MG'                                            
         DC    C'D+R         ',C'D4'                                            
         DC    C'SUB1        ',C'U1'     TEST FILE                              
         DC    C'SUB2        ',C'U2'     TEST FILE                              
         DC    C'SUB3        ',C'UR'     TEST FILE                              
*                                  '&' -> '+'                                   
         DC    X'0000'                                                          
         DC    H'0'                                                             
LDIR0400 EQU   *                                                                
         CLI   QUESTOR,C'Y'        PRINT TABLES?                                
         BNE   LDIR0440                                                         
*                                                                               
         L     R2,ASTAAREA         SET A(STATION TABLE)                         
*                                                                               
         LA    R3,1                SET COUNTER                                  
LDIR0420 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    LDIR0440            YES                                          
*                                                                               
         EDIT  (R3),(5,P+1)                                                     
         LA    R3,1(R3)                                                         
         MVC   P+16(04),=C'STA:'                                                
         MVC   P+23(PRTTABLN),0(R2)                                             
         GOTO1 REPORT                                                           
*                                                                               
         LA    R2,STATABLN(R2)     BUMP TO NEXT SLOT                            
         B     LDIR0420            GO BACK FOR NEXT SLOT                        
LDIR0440 EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P+1(07),=C'STACTR='                                              
         EDIT  STACTR,(6,P+10)                                                  
         GOTO1 REPORT                                                           
*                                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
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
GETREC#2 LA    R6,GETREC                                                        
         B     LINKFIL2                                                         
         SPACE 2                                                                
LINKFIL2 NTR1                                                                   
         MVC   DMCB+12(4),AIO2                                                  
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),REPFILE,KEY+28,             X        
               ,(0,DMWORK)                                                      
         B     DMCHECK                                                          
GETRECRD LA    R6,GETREC                                                        
         B     LINKFILE                                                         
         SPACE 2                                                                
PREC     LA    R6,PUTREC                                                        
         B     LINKFILE                                                         
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),REPFILE,KEY+28,             X        
               REC,(0,DMWORK)                                                   
         B     DMCHECK                                                          
*                                                                               
*                                                                               
WRT      MVC   COMMAND(8),DMWRT                                                 
                                                                                
         B     DIRCTRY                                                          
*                                                                               
DIRCTRY  NTR1                                                                   
         IC    R4,DMINBTS                                                       
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPDIR',KEYSAVE,KEY               
         OC    DMCB+8(1),DMCB+8                                                 
         XIT1                                                                   
COMMAND  DS    CL8                                                              
         SPACE 3                                                                
*        DATA MANAGER INTERFACE (CHECK ERRORS)                                  
         SPACE 1                                                                
DMCHECK  EQU   *                                                                
*                                                                               
         TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BO    NEXIT                                                            
         TM    DMCB+8,X'02'        TEST FOR RECORD DELETED                      
         BO    EQXIT               DELETE SET - PROCESS                         
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
         XIT1                                                                   
         SPACE 1                                                                
NEXIT    LTR   RB,RB                                                            
         XIT1                                                                   
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
         XIT1                                                                   
         SPACE 2                                                                
TRACEDM8 DS    C                                                                
TRACEKEY DS    CL32                                                             
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
AREC     DS    A                                                                
ASPOFFTB DS    A                                                                
ANEXTSPO DS    A                                                                
AMISSSTA DS    A                                                                
AMISSADV DS    A                                                                
SAVEAIO  DS    A                                                                
AIO2     DS    A                                                                
ABLDAREA DS    A                                                                
ASTAAREA DS    A                   STATION AREA                                 
ANEXTSTA DS    A                   NEXT STATION SLOT                            
AJOLAREA DS    A                   STATION JOIN/LEAVE AREA                      
ANEXTJOL DS    A                   NEXT JOIN/LEAVE SLOT                         
JOLCTR   DS    F                                                                
ASALAREA DS    A                                                                
ANEXTSAL DS    A                                                                
SALCTR   DS    F                                                                
ATAPEREC DS    A                                                                
AWORKBLK DS    A                                                                
AWRKBLK2 DS    A                                                                
AWRKBLK3 DS    A                                                                
LBLDAREA DS    F                                                                
CONBYTES DS    F                                                                
BUYBYTES DS    F                                                                
SKIPCTR  DS    F                                                                
COVCTR   DS    F                                                                
NOCONCOV DS    F                                                                
BUYCTR   DS    F                                                                
STACTR   DS    F                                                                
STAREAD  DS    F                                                                
STATOTAL DS    F                                                                
STASKIPD DS    F                                                                
STALVDAT DS    F                                                                
STACOPYD DS    F                                                                
STAOUTPT DS    F                                                                
STAOLDNW DS    F                                                                
STAONFIL DS    F                                                                
STADUPED DS    F                                                                
STAPASS1 DS    F                                                                
STAPASS2 DS    F                                                                
STAGHOST DS    F                                                                
MISMATCH DS    F                                                                
SORTCTR  DS    F                                                                
SORTTYP1 DS    F                                                                
SORTTYP2 DS    F                                                                
STYP1RED DS    F                                                                
STYP2RED DS    F                                                                
MATCHSRT DS    F                                                                
NOBUYCON DS    F                                                                
DROP02C  DS    F                                                                
DROP02SC DS    F                                                                
DROP07C  DS    F                                                                
DROP07SC DS    F                                                                
DROP11C  DS    F                                                                
DROP11SC DS    F                                                                
DROP82C  DS    F                                                                
DROP82SC DS    F                                                                
COMBOCTL DS    F                                                                
         XIT1    .1.2.3.4.5.6.7.8.9.0.1.2.3.4.5.6                               
FOXES    DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                              
DUMMYREP DS    CL14                DUMMY 'STATION TABLE' ENTRY                  
*                                     WHEN STATION NOT IN LIST                  
COMPARE  DS    CL6                 COMPARE AREA: SORT VS BUY                    
*                                                                               
MYWORK   DS    CL64                                                             
MYWORK2  DS    CL64                                                             
MYWORK3  DS    CL64                                                             
OLDCON   DS    CL4                 ORIGINAL CONTRACT NUMBER                     
NEWCON   DS    CL4                 NEW      CONTRACT NUMBER                     
NEWCONRV DS    CL4                 NEW      CONTRACT NUMBER (REV)               
OLDCONRV DS    CL4                 OLD      CONTRACT NUMBER (REV)               
OLDSTA   DS    CL5                                                              
OLDOFF   DS    CL2                                                              
OLDADV   DS    CL4                                                              
OLDAGY   DS    CL6                 ORIGINAL AGENCY/OFFICE                       
ELEM     DS    CL64                                                             
*                                                                               
LASTSTA  DS    CL5                                                              
OLDREP   DS    CL2                 OLD REP CODE                                 
NEWREP   DS    CL2                 NEW REP CODE                                 
REPREP   DS    CL2                 REP REP CODE (REPLACEMENT)                   
OLDCONV# DS    CL2                 OLD REPCODE                                  
HIGHCON  DS    XL4                                                              
LOWCON   DS    XL4                                                              
SAVEKEY  DS    CL(L'KEY)                                                        
REKEY    DS    CL(L'KEY)                                                        
COVERFLG DS    CL1                                                              
*                                                                               
STATABLN EQU   100                                                              
PRTTABLN EQU   48                                                               
*                                                                               
*                                                                               
ELCODE   DS    CL1                                                              
         SPACE 3                                                                
         DS    0H                                                               
INTAPE1  DCB   DDNAME=INTAPE1,DSORG=PS,RECFM=VB,LRECL=255,             X        
               BLKSIZE=6233,MACRF=GM,EODAD=LDIR0400                             
         SPACE 3                                                                
         SPACE 3                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL4008              AREA FOR RECORD: 4K CONTRACT                 
         DS    0D                                                               
*  INCLUDE REGENREPA               REP RECORD                                   
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
*                                                                               
RECD     DSECT                                                                  
RECORD   DS    CL4008                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENREPA         REP RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSTA          STATION RECORD                               
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
MASTD    DSECT                                                                  
       ++INCLUDE DDMASTC                                                        
       EJECT                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'154REREPSALE 05/01/02'                                      
         END                                                                    
