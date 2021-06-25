*          DATA SET REREPFZ02  AT LEVEL 135 AS OF 09/10/03                      
*PHASE REFZ02A                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'REREPFZ02 - MSTREET COMPARE'                                    
***********************************************************************         
*   QUESTOR     Y  =  DISPLAY SORT RECORDS RELEASED                   *         
*   QUESTOR+1   Y  =  DISPLAY SORT RECORDS RETURNED                   *         
*   QUESTOR+2   Y  =  TEST:  ONLY PROCESS FIRST 100 RECORDS OF FILES  *         
*   QUESTOR+3   Y  =  DISPLAY KEY VALUES                              *         
*   QUESTOR+4   Y  =  DISPLAY TAPE INPUT RECORDS                      *         
*   QUESTOR+8   -  =  NO STATION/DDS ID SEQUENCE                      *         
*   QUESTOR+9   -  =  NO DDS ID/STATION SEQUENCE                      *         
*   QUESTOR+10  -  =  NO STATION/UID    SEQUENCE                      *         
*   QUESTOR+11  -  =  NO UID/STATION    SEQUENCE                      *         
***********************************************************************         
*    QOPTION1   Y  =  CONTROL FILE FOR CURRENT DATA                   *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
*   CHANGES:                                                          *         
*   JUL15/03 (BU ) --- OPTION: CURRENT BOOK = CONTROL FILE            *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
         PRINT NOGEN                                                            
REFZ02   CSECT                                                                  
         NMOD1 0,**FZ02**,R8,R9,RR=R5                                           
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
*                                                                               
         USING PLINED,P                                                         
         ST    R5,RELO                                                          
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   *+12                                                             
         BRAS  RE,INIT                                                          
         B     EXIT                                                             
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         BRAS  RE,DUMPTAPE         DUMP TAPE FILE                               
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         BRAS  RE,DOREPORT         PRODUCE REPORT                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1                                                                   
         MVI   KEYCHANG,0          INITIALIZE KEY TYPE INDICATOR                
***      GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ IN FILE OF ACTIVE RADIO STATIONS                                         
***********************************************************************         
         SPACE 1                                                                
***********************************************************************         
* PROCESS STATION RECORDS FOR THIS AGENCY AS REQUESTED                *         
***********************************************************************         
         SPACE 1                                                                
DUMPTAPE NTR1                                                                   
**       MVC   P+1(27),=C'PROCESSING PRIOR   PERIOD  '                          
**       GOTO1 REPORT                                                           
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         OPEN  (FILEIN2,INPUT)     OPEN PRIOR FILE                              
         GOTO1 DUMPFILE,DMCB,0     PROCESS PRIOR   MSTREET DATA                 
         CLOSE FILEIN2                                                          
         GOTO1 SORTER,DMCB,=C'END'                                              
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         CLI   QOPTION1,C'Y'       USE CONTROL FILE FOR CURRENT?                
         BE    DTAP0020                                                         
         MVC   P+1(35),=C'PROCESSING CURRENT PERIOD FROM TAPE'                  
         GOTO1 REPORT                                                           
         OPEN  (FILEIN,INPUT)      OPEN CURRENT FILE                            
         GOTO1 DUMPFILE,DMCB,1     PROCESS CURRENT MSTREET TAPE                 
         CLOSE FILEIN                                                           
         B     DTAP0040                                                         
DTAP0020 EQU   *                                                                
         MVC   P+1(35),=C'PROCESSING CURRENT PERIOD FROM DISK'                  
         GOTO1 REPORT                                                           
         GOTO1 DUMPFILE,DMCB,2     PROCESS CURRENT MSTREET DISK                 
DTAP0040 EQU   *                                                                
***      MVC   P+1(27),=C'NEW FILES HAVE BEEN WRITTEN'                          
***      GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* DUMPFILE: DUMP EITHER FILEIN OR FILEIN1, DEPENDING ON FLAG          *         
*                                                                     *         
***********************************************************************         
DUMPFILE NTR1                                                                   
         MVC   FILEFLAG,3(R1)      SAVE FLAG INDICATOR                          
*                                                                               
***      MVC   P+1(11),=C'INPUT FLAG='                                          
***      MVC   P+12(1),FILEFLAG                                                 
***      GOTO1 REPORT                                                           
*                                                                               
         CLI   FILEFLAG,2          PROCESS CURRENT DATA FROM DISK?              
         BNE   DFIL0020            NO                                           
         LA    RF,REC              SET A(IO AREA)                               
         ST    RF,AIO                                                           
*                                                                               
*                                                                               
*   OPEN CONTROL SYSTEM TO ACCESS CONTROL FILE MSTREET RECORDS                  
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'CONTROL',                +        
               =C'NCTFILE X',AIO,0                                              
         XC    WORK,WORK                                                        
         MVI   WORK,X'99'          SET TYPE 99 RECORD                           
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,AIO                      
*                                  READ THE FIRST RECORD                        
         B     DFIL0055            PROCESS FIRST RECORD                         
*                                                                               
DFIL0020 EQU   *                                                                
         LA    R0,RECINL           CLEAR INPUT RECORD AREA                      
         LHI   R1,RECLQ                                                         
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         CLI   FILEFLAG,0          PROCESS PRIOR DATA?                          
         BNE   DFIL0040            NO                                           
*                                                                               
*  TEST                                                                         
***      MVC   P+1(11),=C'GET PRIOR ='                                          
***      MVC   P+12(1),FILEFLAG                                                 
***      GOTO1 REPORT                                                           
*  TEST                                                                         
*                                                                               
         GET   FILEIN2,RECINL      GET INPUT FROM PRIOR   FILE                  
*                                                                               
*  TEST                                                                         
***      MVC   P+1(11),=C'PRIOR     ='                                          
***      MVC   P+12(64),REC                                                     
***      GOTO1 REPORT                                                           
*  TEST                                                                         
*                                                                               
         B     DFIL0060                                                         
DFIL0040 EQU   *                                                                
         CLI   FILEFLAG,1          PROCESS CURRENT DATA FROM TAPE?              
         BNE   DFIL0050            NO                                           
*                                                                               
         GET   FILEIN,RECINL       GET INPUT FROM CURRENT FILE                  
*                                                                               
*  TEST                                                                         
***      MVC   P+1(11),=C'CURR      ='                                          
***      MVC   P+12(64),REC                                                     
***      GOTO1 REPORT                                                           
*  TEST                                                                         
*                                                                               
         B     DFIL0060                                                         
DFIL0050 EQU   *                                                                
*                                                                               
*   DISK OPTION FOR CURRENT FILE:  RECORDS WILL BE RETURNED FROM                
*        THE CONTROL FILE ITSELF.  THESE RECORDS ARE IN THE SAME                
*        FORMAT AS THOSE ON THE TAPE.  ONLY TYPE X'9900' RECORDS                
*        WILL BE PROCESSED.                                                     
*                                                                               
*  TEST                                                                         
***      MVC   P+1(11),=C'GET DISK  ='                                          
***      MVC   P+12(1),FILEFLAG                                                 
***      GOTO1 REPORT                                                           
*  TEST                                                                         
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'CTFILE',WORK,AIO                      
DFIL0055 EQU   *                                                                
         CLC   REC(2),=X'9900'     RADIO RECORD ON CONTROL?                     
         BNE   DFIL0400            NO  - FINISHED                               
DFIL0060 EQU   *                                                                
*                                                                               
         LA    R5,REC                                                           
         USING CT99RECD,R5                                                      
*                                                                               
         CLI   CT99KTYP,CT99KTYQ   TYPE 99 RECORD?                              
         BNE   DFIL0020            NO  - SKIP IT                                
         CLI   CT99KSUB,CT99KSRA   SUBTYPE = RADIO?                             
         BNE   DFIL0020            NO  - SKIP IT                                
         LA    RF,CT99KUID         CHECK FOR 'SPECIAL DDS ID/UIDS'              
         LA    R0,6                                                             
DFIL0065 EQU   *                                                                
         CLI   0(RF),C'0'          CHECK AGAINST CHARACTER 'F0'                 
         BL    DFIL0020            LOW = NOT NUMERIC: SKIP IT                   
         LA    RF,1(RF)                                                         
         BCT   R0,DFIL0065         CHECK EACH POSITION                          
*                                                                               
         CLI   FILEFLAG,1          PROCESS CURRENT DATA FROM TAPE?              
         BE    DFIL0067            YES - COUNT AGAINST CURRENT                  
         CLI   FILEFLAG,2          PROCESS CURRENT DATA FROM DISK?              
         BNE   DFIL0068            NO  - COUNT AGAINST PRIOR                    
DFIL0067 EQU   *                                                                
         L     RF,FILECTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,FILECTR          SAVE COUNTER FOR CURRENT FILE                
         CLI   QUESTOR+2,C'Y'      TEST RUN?                                    
         BNE   DFIL0070            NO                                           
         CLC   FILECTR,=F'100'     YES - TEST COUNTER                           
         BH    DFIL0400            FORCE FINISH                                 
         B     DFIL0070                                                         
DFIL0068 EQU   *                                                                
         L     RF,FILE1CTR                                                      
         LA    RF,1(RF)                                                         
         ST    RF,FILE1CTR         SAVE COUNTER FOR PRIOR FILE                  
         CLI   QUESTOR+2,C'Y'      TEST RUN?                                    
         BNE   DFIL0070            NO                                           
         CLC   FILE1CTR,=F'100'    YES - TEST COUNTER                           
         BH    DFIL0400            FORCE FINISH                                 
DFIL0070 EQU   *                                                                
         CLI   QUESTOR+4,C'Y'      DISPLAY TAPE INPUT?                          
         BNE   DFIL0080            NO                                           
         MVC   P+1(04),=C'REC:'                                                 
         MVC   P+5(64),REC                                                      
         GOTO1 REPORT                                                           
DFIL0080 EQU   *                                                                
         XC    SORTREC,SORTREC     CLEAR SORT RECORD                            
         MVI   STYP,0              SET 'STA/DDS ID' RECORD INDICATOR            
         MVC   S2NDKEY,CT99KUID    INSERT DDS ID INTO 2ND KEY                   
         MVC   ORIGDDS,CT99KUID    SAVE ORIGINAL DDS ID                         
*                                                                               
*   THE DDS ID IS NOW KEPT IN THE KEY.  THE ORIGINAL UID IS KEPT AS AN          
*        ITEM IN THE DATA PORTION OF THE RECORD.                                
*                                                                               
         DROP  R5                                                               
*                                                                               
         MVC   SOLDNEW,FILEFLAG    SET PRIOR/CURRENT INDICATOR                  
         CLI   FILEFLAG,2          CURRENT FROM DISK?                           
         BNE   DFIL0082            NO                                           
         MVI   SOLDNEW,1           YES - SET TO NEW                             
DFIL0082 EQU   *                                                                
         LA    R5,28(R5)           SET TO X'01' ELEMENT OF RECORD               
         CLI   0(R5),X'01'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CRCLD,R5                                                         
         CLI   CRCLBND,C'F'        FM STATION?                                  
         BE    DFIL0100            YES                                          
         CLI   CRCLBND,C'A'        AM STATION?                                  
         BNE   DFIL0020            NO  - SKIP THIS RECORD                       
DFIL0100 EQU   *                                                                
         MVC   ORIGUID,CRCLUID     SAVE ORIGINAL UID                            
         CLC   ORIGUID,SPACES      ANY VALUE IN ORIGINAL UID?                   
         BH    DFIL0110            YES - LEAVE IT                               
         MVC   ORIGUID,ORIGDDS     NO  - FORCE TO DDS ID                        
DFIL0110 EQU   *                                                                
         MVC   S1STKEY,CRCLCLL     INSERT CALLS AND BAND                        
         MVC   SFREQ,CRCLFRQ       INSERT FREQUENCY                             
         MVC   SCITY,CRCLCTY       INSERT CITY                                  
         MVC   SSTATE,CRCLSTE      INSERT STATE                                 
         MVC   SDDSID,ORIGUID      INITIALLY INSERT ORIG UID                    
         DROP  R5                                                               
         ZIC   RF,1(R5)            BUMP TO NEXT ELEMENT                         
         AR    R5,RF                                                            
         CLI   0(R5),X'02'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CRFMD,R5                                                         
         MVC   SFORMAT,CRFMFMT     INSERT FORMAT                                
         MVC   SOWNER,CRFMOWN      INSERT OWNER                                 
         MVC   SREP1,CRFMREP1      INSERT REP1                                  
         MVC   SREP2,CRFMREP2      INSERT REP2                                  
         MVC   SAMCODE,CRFMAMC     INSERT AMCODE                                
*                                                                               
         DROP  R5                                                               
*                                                                               
         ZIC   RF,1(R5)            BUMP TO NEXT ELEMENT                         
         AR    R5,RF                                                            
         CLI   0(R5),X'03'                                                      
         BNE   DFIL0120                                                         
         USING CRCHD,R5                                                         
*                                                                               
         MVC   SHISTORY,CRCHHST1   MOVE CALL HIST/DATES 1/2                     
         DROP  R5                                                               
*                                                                               
DFIL0120 EQU   *                                                                
         CLI   QUESTOR+8,C'-'      SUPPRESS STATION/DDS ID SEQUENCE?            
         BE    DFIL0130            YES                                          
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
DFIL0130 EQU   *                                                                
*                                  INSERT 1ST OF TWO SORT RECS                  
         BAS   RE,DISPSORT                                                      
*                                                                               
         MVI   STYP,1              SET 'DDSID/STATION' RECORD INDICATOR         
         MVC   DUB(6),S1STKEY      SAVE OFF FIRST KEY                           
         MVC   S1STKEY,S2NDKEY     REVERSE 1ST AND SECOND KEYS                  
         MVC   S2NDKEY,DUB                                                      
         CLI   QUESTOR+9,C'-'      SUPPRESS DDS ID/STATION SEQUENCE?            
         BE    DFIL0140            YES                                          
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
DFIL0140 EQU   *                                                                
*                                  INSERT 2ND OF TWO SORT RECS                  
         BAS   RE,DISPSORT                                                      
*                                                                               
*                                                                               
         MVI   STYP,2              SET 'STA/UID' RECORD INDICATOR               
         MVC   DUB(6),S1STKEY      SAVE OFF FIRST KEY                           
         MVC   S1STKEY,S2NDKEY     REVERSE 1ST AND SECOND KEYS                  
         MVC   S2NDKEY,ORIGUID     INSERT ORIGINAL UID INTO KEY                 
         MVC   SDDSID,ORIGDDS      INSERT ORIGINAL DDS ID INTO RECORD           
         CLI   QUESTOR+10,C'-'     SUPPRESS STATION/UID    SEQUENCE?            
         BE    DFIL0150            YES                                          
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
DFIL0150 EQU   *                                                                
*                                  INSERT 3RD OF FOUR SORT RECS                 
         BAS   RE,DISPSORT                                                      
*                                                                               
         MVI   STYP,3              SET 'STA/UID' RECORD INDICATOR               
         MVC   DUB(6),S1STKEY      SAVE OFF FIRST KEY                           
         MVC   S1STKEY,S2NDKEY     REVERSE 1ST AND SECOND KEYS                  
         MVC   S2NDKEY,DUB                                                      
         CLI   QUESTOR+11,C'-'     SUPPRESS UID/STATION    SEQUENCE?            
         BE    DFIL0160            YES                                          
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
DFIL0160 EQU   *                                                                
*                                  INSERT 4TH OF FOUR SORT RECS                 
         BAS   RE,DISPSORT                                                      
*                                                                               
         B     DFIL0020            GO BACK FOR NEXT                             
*                                                                               
*                                                                               
DFIL0400 EQU   *                                                                
         CLI   FILEFLAG,0          PROCESS PRIOR DATA?                          
         BNE   DFIL0410            NO                                           
*                                                                               
         OPEN  (MSTREETP,OUTPUT)                                                
         CLOSE MSTREETP            MAKE SURE IT'S CLEAN                         
         OPEN  (MSTREETP,OUTPUT)                                                
         B     DFIL0420                                                         
DFIL0410 EQU   *                                                                
DFIL0415 EQU   *                                                                
         OPEN  (MSTREETC,OUTPUT)                                                
         CLOSE MSTREETC            MAKE SURE IT'S CLEAN                         
         OPEN  (MSTREETC,OUTPUT)                                                
DFIL0420 EQU   *                                                                
         GOTO1 GETSORT                                                          
                                                                                
         CLI   STYP,X'FF'          EOF                                          
         BE    DFIL0800            YES                                          
         BAS   RE,DISPBACK         DISPLAY RETURNED RECORDS                     
         CLI   FILEFLAG,0          PROCESS PRIOR DATA?                          
         BNE   DFIL0430            NO                                           
*                                                                               
         PUT   MSTREETP,SORTREC    YES - WRITE OUT RETURNED PRIOR REC           
         B     DFIL0420            GO BACK FOR NEXT                             
DFIL0430 EQU   *                                                                
         PUT   MSTREETC,SORTREC    WRITE OUT RETURNED CURRENT REC               
         B     DFIL0420            GO BACK FOR NEXT                             
DFIL0800 EQU   *                   END OF FILE                                  
         CLI   FILEFLAG,0          PROCESS PRIOR DATA?                          
         BNE   DFIL0810            NO                                           
*                                                                               
         CLOSE MSTREETP            YES - CLOSE PRIOR FILE                       
         B     DFIL0900                                                         
DFIL0810 EQU   *                   END OF FILE                                  
         CLOSE MSTREETC            YES - CLOSE PRIOR FILE                       
         B     DFIL0900                                                         
DFIL0900 EQU   *                   END OF FILE                                  
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
* DISPSORT: DISPLAY SORT RECORDS AS RELEASED                         *          
***********************************************************************         
DISPSORT NTR1                                                                   
         CLI   QUESTOR,C'Y'        DISPLAY SORT RECS RELEASED?                  
         BNE   DSOR0020            NO                                           
         MVC   P+1(10),=C'SORTOUT  :'                                           
         MVC   P+11(64),SORTREC                                                 
         GOTO1 REPORT                                                           
DSOR0020 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
* DISPBACK: DISPLAY SORT RECORDS AS RETURNED                         *          
***********************************************************************         
DISPBACK NTR1                                                                   
         CLI   QUESTOR+1,C'Y'      DISPLAY SORT RECS RETURNED?                  
         BNE   DBAK0020            NO                                           
         MVC   P+1(10),=C'SORTBACK :'                                           
         MVC   P+12(06),=C'PRIOR '                                              
         CLI   FILEFLAG,0          PROCESS PRIOR DATA?                          
         BE    DBAK0010            YES                                          
DBAK0010 EQU   *                                                                
         MVC   P+12(06),=C'NEW   '                                              
         MVC   P+21(64),SORTREC                                                 
         GOTO1 REPORT                                                           
DBAK0020 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
* GETSORT:  RETURN SORT RECORDS, SET END OF FILE CONDITION            *         
***********************************************************************         
*   SORT RETURN AND END-OF-FILE TESTING                                         
*                                                                               
         DS    0H                                                               
GETSORT  NTR1                                                                   
         L     RC,0(R1)            RELOAD A(WORK SPACE)                         
         CLI   STYP,X'FF'          EOF REACHED?                                 
         BE    GS040               YES                                          
         MVI   STYP,X'FF'                                                       
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         LTR   R6,R6               TEST RETURN ZERO=EOF                         
         BZ    GS040               YES -                                        
         MVC   SORTREC,0(R6)                                                    
*        MVC   P+35(10),SORTREC+1  **TEST**                                     
*        GOTO1 REPORT              **TEST**                                     
GS040    EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* DOREPORT:  PRODUCE ANALYSIS FROM MSTREETP/MSTREETC FILES            *         
*                                                                     *         
***********************************************************************         
DOREPORT NTR1                                                                   
*                                                                               
*   SET UP EOD ADDRESSES FOR MSTREET FILES                                      
*                                                                               
         OPEN  (MSTREETP,INPUT)                                                 
         OPEN  (MSTREETC,INPUT)                                                 
*                                                                               
***      MVC   P+1(35),=C'MSTREET FILES ARE OPEN             '                  
***      GOTO1 REPORT                                                           
*                                                                               
         GET   MSTREETP,STREETP    THROW-AWAY READ                              
*                                                                               
***      MVC   P+1(35),=C'THROW-AWAY READ MSTREETP DONE      '                  
***      GOTO1 REPORT                                                           
*                                                                               
         GET   MSTREETC,STREETC    THROW-AWAY READ                              
*                                                                               
***      MVC   P+1(35),=C'THROW-AWAY READ MSTREETC DONE      '                  
***      GOTO1 REPORT                                                           
*                                                                               
*                                                                               
*   SET LOWKEY TO THE LOWER KEY OF PRIOR OR CURRENT                             
*                                                                               
         MVC   LOWKEY,STREETC      SET LOWKEY TO CURRENT                        
         CLC   STREETP(13),STREETC COMPARE KEYS (PRIME THE PUMP)                
         BH    DORE0010            PRIOR > CURRENT                              
         MVC   LOWKEY,STREETP      PRIOR <= CURRENT                             
DORE0010 EQU   *                                                                
*                                                                               
         CLC   LOWKEY(4),=X'FFFFFFFF'                                           
*                                  ALL KEYS PROCESSED?                          
         BE    DORE0800            YES - END OF JOB                             
*                                                                               
*   AT THIS POINT, RECORD KEYS CAN ONLY EQUAL OR BE GREATER THAN                
*        LOWKEY.  NO RECORD KEY WILL EVER BE LESS THAN LOWKEY.                  
*                                                                               
         MVI   STRPFLAG,0          INITIALIZE PRIOR   FLAG                      
         CLC   LOWKEY,STREETP      PRIOR   KEY = LOWKEY?                        
         BNE   DORE0020            NO                                           
         MVI   STRPFLAG,1          YES - SET FLAG                               
DORE0020 EQU   *                                                                
         MVI   STRCFLAG,0          INITIALIZE CURRENT FLAG                      
         CLC   LOWKEY,STREETC      CURRENT KEY = LOWKEY?                        
         BNE   DORE0030            NO                                           
         MVI   STRCFLAG,1          YES - SET FLAG                               
DORE0030 EQU   *                                                                
         CLC   STRPFLAG,STRCFLAG   BOTH FLAGS SET?                              
         BE    DORE0120            YES - NO REPORT NEEDED                       
*                                                                               
         CLI   QUESTOR+3,C'Y'      DISPLAY KEY VALUES?                          
         BNE   DORE0040            NO                                           
         MVC   P+01(07),=C'LOWKEY:'                                             
         MVC   P+08(13),LOWKEY                                                  
         MVC   P+22(07),=C'STPKEY:'                                             
         MVC   P+30(13),STREETP                                                 
         MVC   P+44(07),=C'STCKEY:'                                             
         MVC   P+51(13),STREETC                                                 
         GOTO1 REPORT                                                           
DORE0040 EQU   *                                                                
         LA    R7,P                                                             
         USING PLINED,R7                                                        
*                                                                               
*   DEVELOP REPORT CONDITIONS HERE                                              
*                                                                               
         CLI   STRPFLAG,1          PRIOR   KEY MATCHED?                         
         BNE   DORE0060            NO                                           
         CLI   KEYCHANG,0          STATION/DDS ID SEQUENCE?                     
         BNE   DORE0050            NO                                           
         CLC   LOWKEY(7),STREETC   YES - CURRT KEY FOR SAME STATION?            
         BNE   DORE0050                                                         
         MVC   PMSG2(19),=C'FORMAT/FREQ CHANGE?'                                
DORE0050 EQU   *                                                                
         MVC   PMSG1(24),=C'OLD: NO LONGER ON FILE  '                           
         GOTO1 FORMLINE,DMCB,STREETP                                            
         GOTO1 REPORT                                                           
         B     DORE0120                                                         
DORE0060 EQU   *                                                                
         MVC   PMSG1(24),=C'NEW: ADDED ON THIS TAPE '                           
         CLI   KEYCHANG,0          STATION/DDS ID SEQUENCE?                     
         BNE   DORE0070            NO                                           
         CLC   LOWKEY(7),STREETP   YES - PRIOR KEY FOR SAME STATION?            
         BNE   DORE0070                                                         
         MVC   PMSG2(19),=C'FORMAT/FREQ CHANGE?'                                
DORE0070 EQU   *                                                                
         GOTO1 FORMLINE,DMCB,STREETC                                            
         GOTO1 REPORT                                                           
         B     DORE0120                                                         
DORE0120 EQU   *                                                                
         CLI   STRPFLAG,1          PRIOR   KEY MATCHED?                         
         BNE   DORE0140            NO  - REUSE IT                               
         CLC   =X'FFFFFFFF',STREETP                                             
*                                  YES - END OF FILE REACHED?                   
         BE    DORE0140            YES - DON'T READ ANOTHER                     
         GET   MSTREETP,STREETP    YES - READ NEXT RECORD                       
DORE0140 EQU   *                                                                
         CLI   STRCFLAG,1          CURRENT KEY MATCHED?                         
         BNE   DORE0160            NO  - REUSE IT                               
         CLC   =X'FFFFFFFF',STREETC                                             
*                                  YES - END OF FILE REACHED?                   
         BE    DORE0160            YES - DON'T READ ANOTHER                     
         GET   MSTREETC,STREETC    YES - READ NEXT RECORD                       
DORE0160 EQU   *                                                                
         CLC   STREETP(13),STREETC COMPARE KEYS (PRIME THE PUMP)                
         BH    DORE0180            PRIOR > CURRENT                              
         MVC   LOWKEY,STREETP      PRIOR <= CURRENT                             
         B     DORE0200            PRIOR   SET AS LOWKEY                        
DORE0180 EQU   *                                                                
         MVC   LOWKEY,STREETC      CURRENT SET AS LOWKEY                        
DORE0200 EQU   *                                                                
         CLC   LOWKEY(1),KEYCHANG  SAME KEY IN PROGRESS?                        
         BE    DORE0010            YES                                          
         MVC   KEYCHANG,LOWKEY     NO  - SAVE NEW KEYTYPE                       
         MVI   FORCEHED,C'Y'       TURN ON PAGE CHANGE                          
         MVC   RCSUBPRG,KEYCHANG   CHANGE HEADING                               
         B     DORE0010            GO BACK AND COMPARE AGAIN                    
DORE0700 EQU   *                                                                
         MVC   STREETP(4),=X'FFFFFFFF'                                          
*                                  END OF STREETP REACHED                       
         B     DORE0140                                                         
DORE0710 EQU   *                                                                
         MVC   STREETC(4),=X'FFFFFFFF'                                          
*                                  END OF STREETC REACHED                       
         B     DORE0160                                                         
DORE0800 EQU   *                                                                
         XIT1                                                                   
         DROP  R7                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* FORMLINE:  FORMAT PRINT LINE.  INPUT IS IN P1                       *         
***********************************************************************         
FORMLINE NTR1                                                                   
         L     R5,0(R1)            SET A(INPUT RECORD)                          
         USING SORTREC,R5                                                       
         LA    R7,P                                                             
         USING PLINED,R7                                                        
         MVC   P1STKEY,S1STKEY     INSERT FIRST  KEY                            
         MVC   P2NDKEY,S2NDKEY     INSERT SECOND KEY                            
         MVC   PDDSID,SDDSID       INSERT DDS ID CODE                           
         MVC   PFREQ,SFREQ         INSERT FREQUENCY                             
         MVC   PCITY,SCITY         INSERT CITY                                  
         MVC   PSTATE,SSTATE       INSERT STATE                                 
         MVC   PFORMAT,SFORMAT     INSERT FORMAT                                
         MVC   POWNER,SOWNER       INSERT OWNER                                 
         MVC   PREP1,SREP1         INSERT REP1                                  
         MVC   PREP2,SREP2         INSERT REP2                                  
         MVC   PAMCODE,SAMCODE     INSERT AMCODE                                
         GOTO1 REPORT                                                           
         USING PLINED2,R7                                                       
         CLC   SHISTORY(10),SPACES ANY HISTORY AT ALL?                          
         BNH   FLIN0800            NO                                           
         MVC   PHISTC1,SHISTORY    INSERT FIRST HISTORY                         
         GOTO1 DATCON,DMCB,(3,SHISTORY+7),(5,PHISTD1)                           
         CLC   SHISTORY+10(10),SPACES    SECOND STATION IN HIST?                
         BNH   FLIN0020            NO                                           
         MVC   PHISTC2,SHISTORY+10 INSERT FIRST HISTORY                         
         GOTO1 DATCON,DMCB,(3,SHISTORY+17),(5,PHISTD2)                          
FLIN0020 EQU   *                                                                
         GOTO1 REPORT                                                           
FLIN0800 EQU   *                                                                
         XIT1                                                                   
         DROP  R7,R5                                                            
         EJECT                                                                  
***********************************************************************         
* DCBS                                                                *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
FILEIN   DCB   DSORG=PS,MACRF=GM,DDNAME=FILEIN,RECFM=VB,LRECL=2048,    +        
               BLKSIZE=25000,EODAD=DFIL0400                                     
*                                                                               
FILEIN2  DCB   DSORG=PS,MACRF=GM,DDNAME=FILEIN2,RECFM=VB,LRECL=2048,   +        
               BLKSIZE=25000,EODAD=DFIL0400                                     
*                                                                               
MSTREETP DCB   DDNAME=MSTREETP,DSORG=PS,LRECL=96,BLKSIZE=5760,         X        
               MACRF=(GM,PM),RECFM=FB,EODAD=DORE0700                            
*                                                                               
MSTREETC DCB   DDNAME=MSTREETC,DSORG=PS,LRECL=96,BLKSIZE=5760,         X        
               MACRF=(GM,PM),RECFM=FB,EODAD=DORE0710                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
RELO     DS    F                   RELOCATION FACTOR                            
FILECTR  DS    F                   CURRENT FILE RECORD COUNT                    
FILE1CTR DS    F                   PRIOR   FILE RECORD COUNT                    
AIO      DS    A                                                                
         EJECT                                                                  
*                                                                               
VHELLO   DC    V(HELLO)                                                         
*                                                                               
LOWKEY   DS    CL13                KEY COMPARE AREA                             
STRPFLAG DS    CL1                                                              
STRCFLAG DS    CL1                                                              
KEYCHANG DS    XL1                                                              
*                                                                               
RECINL   DS    F                                                                
REC      DS    512C                I/O AREA                                     
RECLQ    EQU   *-RECINL                                                         
*                                                                               
STREETP  DS    96C                 I/O AREA FOR PRIOR                           
STREETC  DS    96C                 I/O AREA FOR CURRENT                         
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,14,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=096'                                   
         SPACE 2                                                                
SORTREC  DS    0CL96                                                            
STYP     DS    CL1                 0   = STATION/DDS ID                         
*                                  1   = DDS ID/STATION                         
*                                  2   = STATION/UID                            
*                                  3   = UID/STATION                            
S1STKEY  DS    CL6                 STYP = 0/2 = STATION                         
*                                  STYP = 1/3 = DDS ID/UID                      
S2NDKEY  DS    CL6                 STYP = 0/2 = DDS ID/UID                      
*                                  STYP = 1/3 = STATION                         
SOLDNEW  DS    CL1                 0 = PRIOR   MSTREET FILE                     
*                                  1 = CURRENT MSTREET FILE                     
SDDSID   DS    CL6                 ORIG UID/DDS ID                              
*                                     DEPENDS ON STYP VALUE                     
SFREQ    DS    CL5                 FREQUENCY                                    
SCITY    DS    CL24                CITY                                         
SSTATE   DS    CL2                 STATE                                        
SFORMAT  DS    CL3                 FORMAT                                       
SOWNER   DS    CL5                 OWNER                                        
SREP1    DS    CL4                 REP1                                         
SREP2    DS    CL4                 REP2                                         
SAMCODE  DS    CL3                 AMCODE                                       
SHISTORY DS    CL20                HISTORY CALL LETTERS                         
         DS    CL6                 SPARE                                        
LSORTREC EQU   *-SORTREC                                                        
*                                                                               
ORIGUID  DS    CL6                                                              
ORIGDDS  DS    CL6                                                              
*                                                                               
RECORD   DS    CL1024                                                           
*                                                                               
*  KEY BUILDING EQUATES:  DISPLACEMENTS INTO SORTREC                            
*                                                                               
DTYPE    EQU   STYP-SORTREC        RECORD TYPE                                  
D1STKEY  EQU   S1STKEY-SORTREC     FIRST  KEY                                   
DOLDNEW  EQU   SOLDNEW-SORTREC     PRIOR/CURRENT FLAG                           
D2NDKEY  EQU   S2NDKEY-SORTREC     SECOND KEY                                   
DDDSID   EQU   SDDSID-SORTREC      DDS ID                                       
DFREQ    EQU   SFREQ-SORTREC       FREQUENCY                                    
DCITY    EQU   SCITY-SORTREC                                                    
DSTATE   EQU   SSTATE-SORTREC                                                   
DFORMAT  EQU   SFORMAT-SORTREC                                                  
DOWNER   EQU   SOWNER-SORTREC                                                   
DREP1    EQU   SREP1-SORTREC                                                    
DREP2    EQU   SREP2-SORTREC                                                    
DAMCODE  EQU   SAMCODE-SORTREC                                                  
*                                                                               
FILEFLAG DS    CL1                                                              
*                                                                               
*                                                                               
*  INCLUDE REGENREPA               REP RECORD                                   
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
PLINED   DSECT                                                                  
P1STKEY  DS    CL6                 1ST KEY                                      
         DS    C                                                                
P2NDKEY  DS    CL6                 2ND KEY                                      
         DS    C                                                                
PDDSID   DS    CL6                 DDS ID                                       
         DS    C                                                                
PFREQ    DS    CL5                 FREQUENCY                                    
         DS    C                                                                
PCITY    DS    CL24                CITY                                         
         DS    C                                                                
PSTATE   DS    CL2                 STATE                                        
         DS    C                                                                
PFORMAT  DS    CL3                 FORMAT                                       
         DS    C                                                                
POWNER   DS    CL5                 OWNER                                        
         DS    C                                                                
PREP1    DS    XL4                 REP1                                         
         DS    C                                                                
PREP2    DS    XL4                 REP2                                         
         DS    C                                                                
PAMCODE  DS    CL3                 AM CODE                                      
         DS    C                                                                
PMSG1    DS    CL24                1ST MESSAGE                                  
         DS    C                                                                
PMSG2    DS    CL24                2ND MESSAGE                                  
LPLINE   EQU   *-PLINED                                                         
*                                                                               
PLINED2  DSECT                                                                  
         DS    CL7                 FILLER                                       
PHISTC1  DS    CL6                 HISTORY CALL 1                               
         DS    C                                                                
PHISTD1  DS    CL8                 HISTORY DATE 1                               
         DS    C                                                                
PHISTC2  DS    CL6                 HISTORY CALL 2                               
         DS    C                                                                
PHISTD2  DS    CL8                 HISTORY DATE 2                               
         DS    C                                                                
LPLINE2  EQU   *-PLINED2                                                        
*                                                                               
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE CTGENRAD                                                       
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
MASTD    DSECT                                                                  
       ++INCLUDE DDMASTC                                                        
       EJECT                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'135REREPFZ02 09/10/03'                                      
         END                                                                    
