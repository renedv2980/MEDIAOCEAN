*          DATA SET REREPK303  AT LEVEL 014 AS OF 05/01/02                      
*PHASE REK302C,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE BINSRCH                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'REREPK302A (REK302A) --- KATZ STATION CONVERSION'               
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPK302A -- KATZ CONVERSION:  STATION FILE CONVERSION.  *            
*                      ACCEPT KATZ TAPE, CONVERT MASTER RECORDS    *            
*                      AND GENERATE ASSOCIATED DDS FORMAT RECORDS  *            
*                                                                  *            
*        SPECIAL VERSION:  UPDATE OFFTEAM ELEMENTS WITH ORIGINAL   *            
*                      TEAMS REPORTED.                             *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* JUL21/95 (BU ) --- ORIGINAL ENTRY, BASED ON SWITCHER (REREPSW02) *            
*                                                                  *            
* NOV21/95 (BU ) --- SPLIT OUT STATION RECS BY COMPANY             *            
*                                                                  *            
* MAY06/96 (BU ) --- OFFTEAM ELEMENT UPDATE                        *            
*                                                                  *            
********************************************************************            
*   NOTE:  ADJUST COMPTABL FOR PRODUCTION RUN!!                    *            
*                                                                  *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*  QRECORD+20 - COUNTERS FOR DISPLAYING RANGE OF RECORDS PROCESSED.*            
*               20-25 =  START   26-31 =  END                      *            
*  QRECORD+36 - REP CODE TO INSERT INTO RECORDS.                   *            
*                                                                  *            
*  LIST OF DISPLAYS (REQUESTOR VALUE TRIGGER = Y IN BYTE:)         *            
*     QUESTOR+0   =   Y  =  DISPLAY INPUT RECORDS                  *            
*     QUESTOR+0   =   D  =  DISPLAY ALL INPUT RECORDS              *            
*     QUESTOR+1   =   Y  =  DISPLAY RECORDS PUT TO OUTPUT          *            
*     QUESTOR+2   =   Y  =  MARKET#/NAME LIST                      *            
*     QUESTOR+3   =   Y  =  DISPLAY MARKET RECORDS PUT TO OUTPUT   *            
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
REK302   CSECT                                                                  
         NMOD1 0,**REK3**,R9,RR=R5                                              
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BNE   MAINEXIT                                                         
         DS    0H                                                               
         GOTO1 INITIAL,DMCB,(RC)                                                
*                                                                               
         MVC   P+1(28),=C'BEGINNING STATION CONVERSION'                         
         GOTO1 REPORT                                                           
*                                                                               
         L     R4,ARECAREA         SET A(TAPE RECD DELIVERY AREA)               
MAIN0040 EQU   *                                                                
         GET   INTAPE,(R4)         READ TAPE RECORD INTO RDA                    
*                                  EOF --> MAIN0080                             
*                                                                               
         GOTO1 STAPROC,DMCB,(RC)   PROCESS STATION RECORD                       
         B     MAIN0040            GO BACK FOR NEXT RECORD                      
*                                                                               
MAIN0080 EQU   *                                                                
         MVC   P+1(25),=C'ENDING STATION CONVERSION'                            
         GOTO1 REPORT                                                           
         GOTO1 DISPTOTS,DMCB,(RC)  DISPLAY TOTALS FOR RUN                       
*                                                                               
         CLOSE FILOUTA             CLOSE OUTPUT FILES                           
         CLOSE FILOUTB                                                          
         CLOSE (INTAPE,REWIND)                                                  
MAINEXIT XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  STAPROC:  CONVERT KATZ STATIONS TO DDS FORMAT                 *              
******************************************************************              
*                                                                               
STAPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R4,ARECAREA         SET A(INPUT RECORD)                          
         USING KTZSTAD,R4          SET DSECT FOR KATZ RECORD                    
         LA    R3,REC                                                           
         USING RSTAREC,R3                                                       
         XCEFL REC,1008                                                         
         CLI   QUESTOR+0,C'D'      DISPLAY STATION RECORD?                      
         BNE   STAP0020            NOT SPECIAL REQUEST                          
         GOTO1 DISPIPUT,DMCB,(RC)                                               
STAP0020 EQU   *                                                                
         L     RF,OLDSTAS                                                       
         LA    RF,1(RF)                                                         
         ST    RF,OLDSTAS                                                       
         CLI   QUESTOR+2,C'Y'      DISPLAY STATION #/MARKET NAME?               
         BNE   STAP0040            NOT SPECIAL REQUEST                          
         MVC   P+1(3),KMKTNUM                                                   
         MVC   P+5(18),KMKTNAM                                                  
         MVC   P+25(5),KMKTSTA                                                  
         GOTO1 REPORT                                                           
STAP0040 EQU   *                                                                
         CLC   KMKTSTA,SPACES      ANY STATION IN FIELD?                        
         BE    STAP0480            NO  - SKIP IT!                               
         CLI   QGROUP,C'K'         KATZ TV STATIONS PROCESSING?                 
         BNE   STAP0080            NO  - SELTEL PROCESSING                      
         CLI   KMKTDIV,C'A'        YES - KATZ AMERICAN?                         
         BE    STAP0120            YES - USE IT                                 
         CLI   KMKTDIV,C'C'        YES - KATZ CONTINENTAL?                      
         BE    STAP0120            YES - USE IT                                 
         CLI   KMKTDIV,C'I'        YES - KATZ NATIONAL?                         
         BE    STAP0120            YES - USE IT                                 
         B     STAP0480            NO  - NOT KATZ TV - SKIP IT                  
STAP0080 EQU   *                   TEST FOR SELTEL                              
         CLI   QGROUP,C'S'         SELTEL TV STATIONS PROCESSING?               
         BE    *+6                 YES - SELTEL PROCESSING                      
         DC    H'0'                NOT RECOGNIZED:  DUMP IT OUT                 
         CLI   KMKTDIV,C'S'        SELTEL WARRIORS?                             
         BE    STAP0120            YES - USE IT                                 
         CLI   KMKTDIV,C'T'        SELTEL KNIGHTS?                              
         BE    STAP0120            YES - USE IT                                 
         CLI   KMKTDIV,C'N'        YES - SELTEL INTERNATIONAL?                  
         BE    STAP0120            YES - USE IT                                 
         B     STAP0480            NO  - NOT SELTEL TV - SKIP IT                
STAP0120 EQU   *                                                                
         CLC   LASTSTAT,KMKTSTA    STATION ALREADY SEEN?                        
         BE    STAP0480            YES - SKIP IT                                
         MVC   LASTSTAT,KMKTSTA    NO  - PROCESS IT                             
         CLI   QUESTOR+0,C'Y'      DISPLAY STATION RECORD?                      
         BNE   STAP0160            NOT SPECIAL REQUEST                          
         GOTO1 DISPIPUT,DMCB,(RC)                                               
STAP0160 EQU   *                                                                
*                                                                               
*   BUILD KEY TO ACCESS RECORD ON FILE.                                         
*                                                                               
         MVI   RSTAKTYP,2          INSERT RECORD TYPE                           
         LA    RF,COMPTEST         SET A(COMPANY TABLE)                         
         CLI   QOPTION2,C'P'       PRODUCTION POWER CODES?                      
         BNE   STAP0180            NO  - USE V4/V5/V6                           
         LA    RF,COMPPROD         YES - USE AM/CQ/NK                           
STAP0180 EQU   *                                                                
         CLC   KMKTDIV,0(RF)                                                    
         BE    STAP0190                                                         
         LA    RF,3(RF)            BUMP TO NEXT ENTRY                           
         CLI   0(RF),0             END OF TABLE REACHED?                        
         BNE   STAP0180            NO  - GO BACK AND CHECK                      
         DC    H'0'                YES - SHOULDN'T HAPPEN                       
COMPTEST EQU   *                                                                
         DC    C'AV4'              AV4                                          
         DC    C'CV5'              CV5                                          
         DC    C'IV6'              IV6                                          
         DC    X'0000'                                                          
COMPPROD EQU   *                                                                
         DC    C'AAM'              LIVE VALUES FOR PRODUCTION RUN!!             
         DC    C'CCQ'              LIVE VALUES FOR PRODUCTION RUN!!             
         DC    C'INK'              LIVE VALUES FOR PRODUCTION RUN!!             
         DC    X'0000'                                                          
         DS    H'0'                                                             
STAP0190 EQU   *                                                                
         MVC   RSTAKREP,1(RF)      INSERT NEW REP CODE                          
         MVC   SAVEREP,1(RF)       SAVE THE REP CODE                            
         MVC   RSTAKSTA(4),KMKTSTA INSERT STATION W/O MEDIA                     
         MVI   RSTAKSTA+4,C' '     CLEAR MEDIA BYTE                             
         XC    KEY,KEY                                                          
         MVC   KEY(27),RSTAKEY     SET KEY FOR READ                             
         BAS   RE,HIGHDIR          READ KEY                                     
*                                                                               
*   TEST                                                                        
         MVC   P+1(5),=C'KEYS:'                                                 
         MVC   P+10(27),KEY                                                     
         MVC   P+40(27),KEYSAVE                                                 
         GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    STAP0195            YES                                          
         MVC   P+1(18),=C'STATION NOT FOUND:'                                   
         MVC   P+20(7),KEYSAVE+20                                               
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         B     STAP0480                                                         
STAP0195 EQU   *                                                                
         BAS   RE,GETRECRD         RETRIEVE RECORD FROM FILE                    
         CLI   QUESTOR+1,C'Y'      DISPLAY STATION PUT RECORD?                  
         BNE   STAP0200            NOT SPECIAL REQUEST                          
         GOTO1 DISPPUT,DMCB,(RC),0                                              
         GOTO1 REPORT              INSERT A BLANK LINE                          
STAP0200 EQU   *                                                                
*                                                                               
         GOTO1 GENOFTMS,DMCB,(RC),(R4),(R3)                                     
*                                  INSERT OFFICE/TEAMS                          
         CLI   QOPTION1,C'U'       HARD UPDATE?                                 
         BNE   STAP0250            NO                                           
         BAS   RE,PUTRECRD         GENERATE NEW OUTPUT                          
STAP0250 EQU   *                                                                
         CLI   QUESTOR+1,C'Y'      DISPLAY STATION PUT RECORD?                  
         BNE   STAP0480            NOT SPECIAL REQUEST                          
         GOTO1 DISPPUT,DMCB,(RC),0                                              
         GOTO1 REPORT              INSERT A BLANK LINE                          
STAP0480 EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
         DROP  R3,R4                                                            
         EJECT                                                                  
INITIAL  NTR1                                                                   
         OPEN  (INTAPE,(INPUT))                                                 
         OPEN  (FILOUTA,(OUTPUT))                                               
         OPEN  (FILOUTB,(OUTPUT))                                               
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
*                                     AT THIS TIME SIZE IS UNKNOWN              
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE FIRST PAGE BREAK                       
****     CLC   QRECORD+36(2),SPACES                                             
*                                  NEW REP CODE = SPACE?                        
***      BNE   *+6                 NO                                           
****     DC    H'0'                YES - NEED CODE                              
****     MVC   KATZREP,QRECORD+36  SAVE CODE                                    
         CLC   QRECORD+20(12),SPACES                                            
*                                  ANY DISPLAY VALUES?                          
         BE    INIT0060            NO                                           
         PACK  DUB,QRECORD+20(6)                                                
         CVB   RF,DUB                                                           
         ST    RF,LOWCTR           SAVE LOW VALUE                               
         PACK  DUB,QRECORD+26(6)                                                
         CVB   RF,DUB                                                           
         ST    RF,HIGHCTR          SAVE HI  VALUE                               
INIT0060 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   GENOFTMS:  INSERT THE OFFICE TEAM ENTRIES                                   
*                                                                               
GENOFTMS NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R4,4(R1)            RESET A(RECORD IN PROGRESS)                  
         USING KTZSTAD,R4          SET DSECT FOR KATZ RECORD                    
*                                                                               
         GOTO1 HELOSTAD                                                         
*                                  DELETE OFF/TEAM ELEMENTS                     
         L     R3,8(R1)            RESET A(STATION RECORD)                      
         USING RSTAREC,R3                                                       
         MVI   ELTBILD1,4          INSERT ELEMENT CODE                          
         MVI   ELTBILD1+1,11       INSERT ELEMENT LENGTH                        
         MVI   ELTBILD1+4,C'T'                                                  
*                                  INSERT TV DIVISION CODE                      
         CLC   KMKTATTM,SPACES     ANY ATLANTA TEAM?                            
         BE    GENO0020            NO                                           
         MVC   ELTBILD1+2(2),=C'AT' INSERT ATLANTA OFFICE                       
         MVC   TEAMWORK(2),ELTBILD1+2                                           
         MVC   TEAMWORK+2(1),KMKTATTM                                           
         BAS   RE,FINDTEAM         FIND/INSERT TEAM CODE                        
         GOTO1 HELOSTA1                                                         
*                                  INSERT ELEMENT INTO RECORD                   
GENO0020 EQU   *                                                                
         CLC   KMKTCHTM,SPACES     ANY CHICAGO TEAM?                            
         BE    GENO0040            NO                                           
         MVC   ELTBILD1+2(2),=C'CH' INSERT CHICAGO OFFICE                       
         MVC   TEAMWORK(2),ELTBILD1+2                                           
         MVC   TEAMWORK+2(1),KMKTCHTM                                           
         BAS   RE,FINDTEAM         FIND/INSERT TEAM CODE                        
         GOTO1 HELOSTA1                                                         
*                                  INSERT ELEMENT INTO RECORD                   
GENO0040 EQU   *                                                                
         CLC   KMKTDATM,SPACES     ANY DALLAS  TEAM?                            
         BE    GENO0060            NO                                           
         MVC   ELTBILD1+2(2),=C'DA' INSERT DALLAS  OFFICE                       
         MVC   TEAMWORK(2),ELTBILD1+2                                           
         MVC   TEAMWORK+2(1),KMKTDATM                                           
         BAS   RE,FINDTEAM         FIND/INSERT TEAM CODE                        
         GOTO1 HELOSTA1                                                         
*                                  INSERT ELEMENT INTO RECORD                   
GENO0060 EQU   *                                                                
         CLC   KMKTLATM,SPACES     ANY LA      TEAM?                            
         BE    GENO0080            NO                                           
         MVC   ELTBILD1+2(2),=C'LA' INSERT LA      OFFICE                       
         MVC   TEAMWORK(2),ELTBILD1+2                                           
         MVC   TEAMWORK+2(1),KMKTLATM                                           
         BAS   RE,FINDTEAM         FIND/INSERT TEAM CODE                        
         GOTO1 HELOSTA1                                                         
*                                  INSERT ELEMENT INTO RECORD                   
GENO0080 EQU   *                                                                
         CLC   KMKTNYTM,SPACES     ANY NEW YORK TEAM?                           
         BE    GENO0100            NO                                           
         MVC   ELTBILD1+2(2),=C'NY' INSERT NEW YORK OFFICE                      
         MVC   TEAMWORK(2),ELTBILD1+2                                           
         MVC   TEAMWORK+2(1),KMKTNYTM                                           
         BAS   RE,FINDTEAM         FIND/INSERT TEAM CODE                        
         GOTO1 HELOSTA1                                                         
*                                  INSERT ELEMENT INTO RECORD                   
GENO0100 EQU   *                                                                
         CLC   KMKTSFTM,SPACES     ANY SF      TEAM?                            
         BE    GENO0120            NO                                           
         MVC   ELTBILD1+2(2),=C'SF' INSERT SF      OFFICE                       
         MVC   TEAMWORK(2),ELTBILD1+2                                           
         MVC   TEAMWORK+2(1),KMKTSFTM                                           
         BAS   RE,FINDTEAM         FIND/INSERT TEAM CODE                        
         GOTO1 HELOSTA1                                                         
*                                  INSERT ELEMENT INTO RECORD                   
GENO0120 EQU   *                                                                
         XC    ELTBILD1,ELTBILD1   CLEAR THE ELEMENT                            
         XIT1                                                                   
         DROP  R3,R4                                                            
         EJECT                                                                  
*                                                                               
*   FINDTEAM:  SCAN TEAM STRUCTURE, AND REPLACE OFF/TEAM WITH ASSOC-            
*       IATED DDS TEAM.                                                         
*                                                                               
FINDTEAM NTR1                                                                   
         MVI   ELTBILD1+5,C' '     CLEAR THE TEAM CODE                          
         LA    R1,TVDIVKTZ         SET A(KATZ TEAMS)                            
         CLI   QGROUP,C'K'         KATZ RUN?                                    
         BE    FTEA0020            YES                                          
         LA    R1,TVDIVSEL         NO  - SET A(SELTEL TEAMS)                    
FTEA0020 EQU   *                                                                
         CLI   0(R1),0             END OF TABLE REACHED?                        
         BE    FTEA0100            YES - LEAVE IT BLANK                         
         CLC   TEAMWORK(3),0(R1)   OFFICE/TEAM FOUND IN TABLE?                  
         BE    FTEA0040            YES - USE ASSOCIATED TEAM                    
         LA    R1,LDIVTEM(R1)      NO  - BUMP TO NEXT TEAM                      
         B     FTEA0020            GO BACK FOR NEXT                             
FTEA0040 EQU   *                                                                
         MVC   ELTBILD1+5(1),2(R1) INSERT ORIGINAL TEAM                         
***>>>   MVC   ELTBILD1+5(1),3(R1) INSERT TEAM FOR OFFICE/TEAM FOUND            
FTEA0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   TEST TEAM TABLE.  THESE ARE NOT THE FINAL TEAMS.                            
*                                                                               
TVDIVKT2 EQU   *                                                                
         DC    C'CHRAV4'             KATZ AMERICAN (A)                          
LDIVTEM2 EQU   *-TVDIVKT2                                                       
         DC    C'LARBV4'                                                        
         DC    C'NYWCV4'                                                        
         DC    C'CHWDV4'                                                        
         DC    C'LAWEV4'                                                        
         DC    C'NYBFV4'                                                        
         DC    C'CHBGV4'                                                        
         DC    C'NY*HV4'                                                        
         DC    C'CH*IV4'                                                        
         DC    C'NYEJV4'                                                        
         DC    C'CHEKV4'                                                        
         DC    C'MNE+V4'           STAN PERLIN CHECKING THIS ONE                
         DC    C'NYR#V4'           STAN PERLIN CHECKING THIS ONE                
         DC    C'CHGLV5'             KATZ CONTINENTAL (C)                       
         DC    C'LAGMV5'                                                        
         DC    C'CHSNV5'                                                        
         DC    C'LASOV5'                                                        
         DC    C'ATI?V5'                                                        
         DC    C'ATQ>V5'                                                        
         DC    C'CHZPV5'                                                        
         DC    C'CHOQV5'                                                        
         DC    C'CHH-V4'           STAN PERLIN CHECKING THIS ONE                
         DC    C'DAH$V4'           STAN PERLIN CHECKING THIS ONE                
         DC    C'NYHRV5'                                                        
         DC    C'NYJSV5'                                                        
         DC    C'NYKTV5'                                                        
         DC    C'NYTUV5'                                                        
         DC    C'NYVVV5'                                                        
         DC    C'NYUWV5'                                                        
         DC    C'NYLXV6'              KATZ NATIONAL (I)                         
         DC    C'NYAYV6'                                                        
         DC    C'NYDZV6'                                                        
         DC    C'CHL1V6'                                                        
         DC    C'CHA2V6'                                                        
         DC    C'CHD3V6'                                                        
         DC    C'LAA4V6'             NOTE:  LANCERS AND SABERS HAVE             
         DC    C'LAA5V6'             SAME TEAM CODES IN LA AND SF               
         DC    C'LAD6V6'                                                        
         DC    C'SFA7V6'                                                        
         DC    C'SFA8V6'                                                        
         DC    C'SFD9V6'                                                        
         DC    H'00'               DELIMITER                                    
*                                                                               
*                                                                               
*   PRODUCTION TEAM TABLE.  FINAL TEAMS NOT SET.                                
*                                                                               
TVDIVKTZ EQU   *                                                                
         DC    C'CHRAAM'             KATZ AMERICAN (A)                          
LDIVTEM  EQU   *-TVDIVKTZ                                                       
         DC    C'LARBAM'                                                        
         DC    C'NYWCAM'                                                        
         DC    C'CHWDAM'                                                        
         DC    C'LAWEAM'                                                        
         DC    C'NYBFAM'                                                        
         DC    C'CHBGAM'                                                        
         DC    C'NY*HAM'                                                        
         DC    C'CH*IAM'                                                        
         DC    C'NYEJAM'                                                        
         DC    C'CHEKAM'                                                        
         DC    C'CHGLCQ'             KATZ CONTINENTAL (C)                       
         DC    C'LAGMCQ'                                                        
         DC    C'CHSNCQ'                                                        
         DC    C'LASOCQ'                                                        
         DC    C'ATI?CQ'                                                        
         DC    C'ATQ>CQ'                                                        
         DC    C'CHZPCQ'                                                        
         DC    C'CHOQCQ'                                                        
         DC    C'NYHRCQ'                                                        
         DC    C'NYJSCQ'                                                        
         DC    C'NYKTCQ'                                                        
         DC    C'NYTUCQ'                                                        
         DC    C'NYVVCQ'                                                        
         DC    C'NYUWCQ'                                                        
         DC    C'NYLXNK'              KATZ NATIONAL (I)                         
         DC    C'NYAYNK'                                                        
         DC    C'NYDZNK'                                                        
         DC    C'CHL1NK'                                                        
         DC    C'CHA2NK'                                                        
         DC    C'CHD3NK'                                                        
         DC    C'LAA4NK'             NOTE:  LANCERS AND SABERS HAVE             
         DC    C'LAA5NK'             SAME TEAM CODES IN LA AND SF               
         DC    C'LAD6NK'                                                        
         DC    C'SFA7NK'                                                        
         DC    C'SFA8NK'                                                        
         DC    C'SFD9NK'                                                        
         DC    H'00'               DELIMITER                                    
*                                                                               
TVDIVSEL EQU   *                                                                
         DC    C'AT0AV1'       SELTEL WARRIORS (S)                              
         DC    C'DA0OV1'                                                        
         DC    C'NY1QV1'              CONQUERORS (S)                            
         DC    C'CH1NV1'                                                        
         DC    C'NY2SV1'              SAXONS (S)                                
         DC    C'LA24V1'              SAXONS (S)                                
         DC    C'NY5YV1'              VOYAGERS (S)                              
         DC    C'CH5GV1'                                                        
         DC    C'LA51V1'                                                        
         DC    C'NY9DV1'              DRAGONS (S)                               
         DC    C'CH9RV1'                                                        
         DC    C'LA95V1'                                                        
         DC    C'NY3HV1'       HIGHLANDERS (T)                                  
         DC    C'NY4WV1'       WARLORDS (T)                                     
         DC    C'CH4LV1'                                                        
         DC    C'LA43V1'                                                        
         DC    C'NY6CV1'       CRUSADERS (T)                                    
         DC    C'CH6UV1'                                                        
         DC    C'LA62V1'                                                        
         DC    C'AT7XV1'       KNIGHTS (T)                                      
         DC    C'DA7TV1'                                                        
         DC    C'NY8KV1'       VIKINGS (T)                                      
         DC    C'CH8IV1'                                                        
         DC    C'LA86V1'                                                        
*                                                                               
*   FOLLOWING ARE INTERNATIONAL/ASIAN TEAMS                                     
*                                                                               
         DC    C'NYC V2'           CBC/ENGLISH (SELTEL INTERNATL (N)            
         DC    C'CHC V2'                                                        
         DC    C'LAC V2'                                                        
         DC    C'NYF V2'           CBC/FRENCH                                   
         DC    C'CHF V2'                                                        
         DC    C'LAF V2'                                                        
         DC    C'NYN V2'           CBC/NEWS                                     
         DC    C'CHN V2'                                                        
         DC    C'LAN V2'                                                        
         DC    C'NYS V6'           ASIAN                                        
         DC    C'CHS V2'                                                        
         DC    C'LAS V2'                                                        
         DC    H'00'               DELIMITER                                    
         EJECT                                                                  
HELOSTA1 NTR1                                                                   
         LA    R3,REC                                                           
         USING RSTAREC,R3                                                       
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RSTAREC,ELTBILD1,0             
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
HELOSTAD NTR1                                                                   
         LA    R3,REC                                                           
         USING RSTAREC,R3                                                       
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(4,RSTAREC),0,0                
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
HELOSTA2 NTR1                                                                   
         LA    R3,REC                                                           
         USING RSTAREC,R3                                                       
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RSTAREC,ELTBILD1,     X        
               =C'ADD=CODE'                                                     
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
HELOMKT1 NTR1                                                                   
         LA    R3,REC                                                           
         USING RMKTREC,R3                                                       
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RMKTREC,ELTBILD1,0             
         XIT1                                                                   
         DROP  R3                                                               
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
GETRECRD LA    R6,GETREC                                                        
         B     LINKFILE                                                         
         SPACE 2                                                                
PUTRECRD LA    R6,PUTREC                                                        
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
*  DISPPUT:  DISPLAY RECORD JUST 'PUT' TO OUTPUT.                *              
*                                                                *              
******************************************************************              
*                                                                               
DISPPUT  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         CLC   OLDSTAS,LOWCTR      DISPLAY RANGE OF RECORDS                     
         BL    DIPU0090                                                         
         CLC   OLDSTAS,HIGHCTR                                                  
         BH    DIPU0090                                                         
         GOTO1 REPORT                                                           
         MVC   P+1(14),=C'STATION OUTPUT'                                       
         EDIT  OLDSTAS,(5,P+20)                                                 
         GOTO1 REPORT                                                           
         LA    R4,REC              A(RECORD LENGTH FIELD)                       
         SR    RF,RF                                                            
         ZICM  RF,REC+27,2                                                      
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
DIPU0090 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DISPIPUT:  DISPLAY KATZ RECORD INPUT.                         *              
*                                                                *              
******************************************************************              
*                                                                               
DISPIPUT NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         CLC   OLDSTAS,LOWCTR      DISPLAY RANGE OF RECORDS                     
         BL    DIPI0120                                                         
         CLC   OLDSTAS,HIGHCTR                                                  
         BH    DIPI0120                                                         
         B     DIPI0040                                                         
DISPIPT1 NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
DIPI0040 EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P+1(14),=C'STATION INPUT'                                        
***      GOTO1 REPORT                                                           
         L     R4,ARECAREA         A(RECORD LENGTH FIELD)                       
****>>>  GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',80,=C'1D'                  
*                                  DISPLAY 80 CHARACTER RECORD                  
         MVC   P+16(80),0(R4)      DISPLAY INPUT RECORD                         
         GOTO1 REPORT                                                           
DIPI0120 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DISPTOTS:                                                     *              
******************************************************************              
*                                                                               
DISPTOTS NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVC   P+1(24),=C'STATION RECORDS READ   :'                             
         EDIT  OLDSTAS,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'STATION RECORDS WRITTEN:'                             
         EDIT  NEWSTAS,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'MARKET  RECORDS WRITTEN:'                             
         EDIT  NEWMKTS,(12,P+30),COMMAS=YES                                     
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
ARECAREA DS    A                   TAPE RECORD DELIVERY AREA                    
LBLDAREA DS    F                                                                
OLDSTAS  DS    F                                                                
NEWSTAS  DS    F                                                                
NEWMKTS  DS    F                                                                
CONCTR   DS    F                                                                
LOWCTR   DS    F                                                                
HIGHCTR  DS    F                                                                
KATZREP  DS    CL2                                                              
SAVEREP  DS    CL2                                                              
FLAGBYTS DS    0CL12               FLAGS                                        
ELTBILD1 DS    CL128                                                            
LASTSTAT DS    CL4                                                              
TEAMWORK DS    CL6                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
ELCODE   DS    CL1                                                              
         SPACE 3                                                                
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
FILOUTB  DCB   DDNAME=FILOUTB,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
INTAPE   DCB   DDNAME=INTAPE,DSORG=PS,RECFM=FB,LRECL=80,               X        
               BLKSIZE=800,MACRF=GM,EODAD=MAIN0080                              
*                                                                               
*                                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL1008              AREA FOR RECORD                              
         SPACE 2                                                                
         DS    0H                  HALFWORD ALIGNMENT NEEDED.                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
KTZSTAD  DSECT                                                                  
KMKTNUM  DS    CL3      +0         MARKET NUMBER                                
KMKTNAM  DS    CL18     +3         MARKET NAME                                  
KMKTNETW DS    CL1      +21        MARKET NETWORK                               
KMKTCHNL DS    CL2      +22        MARKET CHANNEL                               
KMKTSTA  DS    CL5      +24        STATION CALL LETTERS                         
KMKTCOMP DS    CL30     +29        COMP STATIONS: 6 X 5 CHARS                   
KMKTATTM DS    CL1      +59        ATLANTA TEAM                                 
KMKTDATM DS    CL1      +60        DALLAS TEAM (SELTEL ONLY)                    
         DS    CL4      +61        SPARE                                        
KMKTTMZN DS    CL1      +65        TIME ZONE                                    
KMKTCOMS DS    CL1      +66        CUSTOM COMMENTS                              
KMKTLATM DS    CL1      +67        LOS ANGELES TEAM                             
KMKTSFTM DS    CL1      +68        SAN FRANCISCO TEAM                           
KMKTSRVC DS    CL1      +69        SERVICE                                      
KMKTRANK DS    CL1      +70        RANK                                         
KMKTLOST DS    CL1      +71        LOST MARKET (L=LOST)                         
KMKTDIV  DS    CL1      +72        DIVISION                                     
KMKTNYTM DS    CL1      +73        NEW YORK TEAM                                
KMKTCHTM DS    CL1      +74        CHICAGO  TEAM                                
KMKTKWIX DS    CL1      +75        KWIX                                         
KMKTREGN DS    CL1      +76        REGION                                       
KMKTPTCH DS    CL1      +77        PITCH STATION                                
KMKTSTCD DS    CL2      +78        STATE CODE                                   
*                                                                               
*                                                                               
         EJECT                                                                  
*  INCLUDE REGENSTA                STATION     RECORD                           
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
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
         SPACE 4                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014REREPK303 05/01/02'                                      
         END                                                                    
