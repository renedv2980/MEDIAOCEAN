*          DATA SET REREPE102  AT LEVEL 122 AS OF 05/01/02                      
*PHASE REE102A,*                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE BINSRCH                                                                
*INCLUDE DAYPAK                                                                 
*INCLUDE TIMVAL                                                                 
*INCLUDE REGENBUC                                                               
*INCLUDE RECUP                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PERVERT                                                                
*INCLUDE QSORT                                                                  
         TITLE 'REREPE102  (REE102A) --- KATZ EDI T/A'                          
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPE102  -- KATZ EDI TURNAROUND                         *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*                                                                  *            
*  TO DISPLAY A RANGE OF CONTRACTS BEING CONVERTED (SERIAL COUNT   *            
*     FROM X TO Y)                                                 *            
*                                                                  *            
*     QRECORD+20-25  =  SIX=DIGIT LOW-COUNTER VALUE FOR DISPLAYS   *            
*     QRECORD+26-31  =  SIX=DIGIT HI -COUNTER VALUE FOR DISPLAYS   *            
*                       BOTH VALUES MUST BE ENTERED FOR DISPLAYS   *            
*                                                                  *            
*  LIST OF DISPLAYS (REQUESTOR VALUE TRIGGER = Y IN BYTE:)         *            
*     QUESTOR     =    Y   =  DISPLAY OUTPUT RECORDS               *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
REE102   CSECT                                                                  
         NMOD1 0,**REE1**,R8,R9,RR=R5                                           
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
         LA    R2,CON#TABL         SET A(CONTRACT # TABLE)                      
MAIN0040 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    MAIN0100            YES - FINISHED                               
         GOTO1 CONTPROC,DMCB,(RC),(R2)                                          
*                                  PROCESS CONTRACT RECORDS                     
         LA    R2,LCONTABL(R2)     BUMP TO NEXT TABLE ENTRY                     
         B     MAIN0040            GO BACK FOR NEXT RECORD                      
*                                                                               
CON#TABL DC    C'12345678'                                                      
LCONTABL EQU   *-CON#TABL                                                       
         DC    C'12345678'                                                      
         DC    X'0000'                                                          
*                                                                               
MAIN0100 EQU   *                                                                
         GOTO1 =A(DISPTOTS),DMCB,(RC),RR=RELO  DISPLAY TOTALS FOR RUN           
*                                                                               
*                                                                               
         CLOSE FILOUTA             CLOSE OUTPUT FILES                           
         CLOSE FILOUTB                                                          
*                                                                               
MAIN0900 XIT1                                                                   
         EJECT                                                                  
*                                                                               
******************************************************************              
*  CONTPROC:  FOR EACH CONTRACT, PRODUCE A SINGLE KATZ FORMAT    *              
*     ORDER.                                                     *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
CONTPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            A(CONTRACT IN TABLE)                         
*                                                                               
         L     RF,CONCTR           INCREMENT RECORD COUNTER                     
         LA    RF,1(RF)               FOR COMPANY IN PROGRESS                   
         ST    RF,CONCTR                                                        
         XC    KEY,KEY             CLEAR KEY FOR BUYLINE READ                   
         MVI   KEY,X'8C'           INSERT RECORD TYPE                           
*                                                                               
*    WILL REQUIRE REVISION WHEN MORE THAN SELTEL IS USING.                      
*                                                                               
***>>>   MVC   KEY+21,=C'SZ'       INSERT SELTEL REP CODE                       
         MVC   KEY+21,=C'V1'       INSERT SELTEL REP CODE                       
         GOTO1 =V(HEXIN),DMCB,0(R2),KEY+23,8                                    
*                                  NINE'S COMP THE CON #                        
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),KEY+23(4)                                             
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   KEY+23(4),WORK+15   INSERT THE COMP'D KEY                        
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    CPRO0020            YES                                          
         MVC   P+1(11),=C'NOT FOUND: '                                          
         MVC   P+15(10),0(R2)                                                   
         GOTO1 REPORT                                                           
         B     CPRO2000            EXIT                                         
CPRO0020 EQU   *                                                                
         MVC   AIOAREA,AIOAREA1    LOAD A(RECORD)                               
         GOTO1 GETREC                                                           
         LA    R4,RECORD3          CLEAR TARGET RECORD                          
         L     R5,=F'16300'        LOAD FULL WORD LENGTH                        
         LA    R6,SPACES                                                        
         XC    DUB,DUB             SET UP PADDING CHARACTER                     
         MVI   DUB,C' '            INSERT PADDING CHARACTER                     
         L     R7,DUB                                                           
         MVCL  R4,R6               CLEAR KATZ BUILD RECORD AREA                 
*                                     FOR 16300 BYTES                           
         LA    R3,RECORD3                                                       
         USING KCONREC1,R3                                                      
         GOTO1 HEXOUT,DMCB,RCONKCON,WORK,4,=C'TOG'                              
         MVC   KCONNUM,WORK+1      INSERT CONTRACT NUMBER                       
*                                                                               
**********************************************************************          
*        PROCESS S/P SECTION                                                    
**********************************************************************          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,6               INSERT S/P REC TYPE                          
***>>>   MVC   RSALKREP,=C'SZ'     INSERT REP CODE                              
         MVC   RSALKREP,=C'V1'     INSERT REP CODE                              
         MVC   RSALKSAL,RCONSAL    INSERT S/P CODE                              
         GOTO1 HIGH                                                             
         CLC   KEY,KEYSAVE         KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                NO                                           
         MVC   AIOAREA,AIOAREA2    LOAD A(RECORD2)                              
         MVC   KSALNAM,RSALNAME    INSERT SALESPERSON NAME                      
         MVC   SAVESP,RSALMRG      SAVE S/P NUMBER FOR DISPLAY                  
         MVC   KSALPHON(3),RSALTEL                                              
         MVC   KSALPHON+3(3),RSALTEL+4                                          
         MVC   KSALPHON+6(4),RSALTEL+8                                          
*                                  STRING IN TELEPHONE NUMBER                   
**********************************************************************          
**********************************************************************          
         MVI   KDIVISN,C'S'        INSERT DIVISION                              
*                                  WILL HAVE TO BE MADE SOFT                    
         LA    RF,TVDIVSEL         A(SELTEL PORTION OF FILE)                    
         MVC   WORK(2),RCONKOFF    SET UP ARGUMENT                              
         MVC   WORK+2(1),RCONTEM+1 INSERT TEAM CODE                             
CPRO0040 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE?                                
         BE    CPRO0080            YES - NO TEAM                                
         CLC   WORK(3),0(RF)       OFFICE/TEAM FOUND?                           
         BE    CPRO0060            YES                                          
         LA    RF,LDIVTEM(RF)      NO  - BUMP TO NEXT ENTRY                     
         B     CPRO0040            GO BACK FOR NEXT                             
CPRO0060 EQU   *                                                                
         MVC   KTEAM,3(RF)         INSERT TEAM FROM TABLE                       
         B     CPRO0080                                                         
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
*    TEAM CODE TABLE IS REVERSED FROM CONVERSION:                               
*       1 - 2 =  OFFICE                                                         
*       3 - 4 =  DDS TEAM CODE                                                  
*       5 - 6 =  KATZ TEAM CODE                                                 
*                                                                               
TVDIVKTZ EQU   *                                                                
         DC    C'CHRA  '             KATZ AMERICAN (A)                          
LDIVTEM  EQU   *-TVDIVKTZ                                                       
         DC    C'LARB  '                                                        
         DC    C'NYWC  '                                                        
         DC    C'CHWD  '                                                        
         DC    C'LAWE  '                                                        
         DC    C'NYBF  '                                                        
         DC    C'CHBG  '                                                        
         DC    C'NY*H  '                                                        
         DC    C'CH*I  '                                                        
         DC    C'NYEJ  '                                                        
         DC    C'CHEK  '                                                        
         DC    C'CHGL  '             KATZ CONTINENTAL (C)                       
         DC    C'LAGM  '                                                        
         DC    C'CHSN  '                                                        
         DC    C'LASO  '                                                        
         DC    C'ATI?  '                                                        
         DC    C'ATQ>  '                                                        
         DC    C'CHZP  '                                                        
         DC    C'CHOQ  '                                                        
         DC    C'NYHR  '                                                        
         DC    C'NYJS  '                                                        
         DC    C'NYKT  '                                                        
         DC    C'NYTU  '                                                        
         DC    C'NYVV  '                                                        
         DC    C'NYUW  '                                                        
         DC    C'NYLX  '              KATZ NATIONAL (I)                         
         DC    C'NYAY  '                                                        
         DC    C'NYDZ  '                                                        
         DC    C'CHL1  '                                                        
         DC    C'CHA2  '                                                        
         DC    C'CHD3  '                                                        
         DC    C'LAA4  '             NOTE:  LANCERS AND SABERS HAVE             
         DC    C'LAA5  '             SAME TEAM CODES IN LA AND SF               
         DC    C'LAD6  '                                                        
         DC    C'SFA7  '                                                        
         DC    C'SFA8  '                                                        
         DC    C'SFD9  '                                                        
         DC    H'00'               DELIMITER                                    
*                                                                               
TVDIVSEL EQU   *                                                                
         DC    C'ATA0V1'       SELTEL WARRIORS (S)                              
         DC    C'DAO0V1'                                                        
         DC    C'NYQ1V1'              CONQUERORS (S)                            
         DC    C'CHN1V1'                                                        
         DC    C'NYS2V1'              SAXONS (S)                                
         DC    C'LA42V1'              SAXONS (S)                                
         DC    C'NYY5V1'              VOYAGERS (S)                              
         DC    C'CHG5V1'                                                        
         DC    C'LA15V1'                                                        
         DC    C'NYD9V1'              DRAGONS (S)                               
         DC    C'CHR9V1'                                                        
         DC    C'LA59V1'                                                        
         DC    C'NYH3V1'       HIGHLANDERS (T)                                  
         DC    C'NYW4V1'       WARLORDS (T)                                     
         DC    C'CHL4V1'                                                        
         DC    C'LA34V1'                                                        
         DC    C'NYC6V1'       CRUSADERS (T)                                    
         DC    C'CHU6V1'                                                        
         DC    C'LA26V1'                                                        
         DC    C'ATX7V1'       KNIGHTS (T)                                      
         DC    C'DAT7V1'                                                        
         DC    C'NYK8V1'       VIKINGS (T)                                      
         DC    C'CHI8V1'                                                        
         DC    C'LA68V1'                                                        
*                                                                               
*   FOLLOWING ARE INTERNATIONAL/ASIAN TEAMS                                     
*                                                                               
         DC    C'NYXCV2'           CBC/ENGLISH (SELTEL INTERNATL (N)            
         DC    C'CHYCV2'                                                        
         DC    C'LAZCV2'                                                        
         DC    C'NY1FV2'           CBC/FRENCH                                   
         DC    C'CH2FV2'                                                        
         DC    C'LA3FV2'                                                        
         DC    C'NY4NV2'           CBC/NEWS                                     
         DC    C'CH5NV2'                                                        
         DC    C'LA6NV2'                                                        
         DC    C'NY7S56'           ASIAN                                        
         DC    C'CH8SV2'                                                        
         DC    C'LA9SV2'                                                        
         DC    H'00'               DELIMITER                                    
*                                                                               
         EJECT                                                                  
CPRO0080 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,RCONCREA),(0,WORK)                                
         MVC   WORK+6(2),WORK+2    MOVE MM TO FIRST POSITIONS                   
         MVI   WORK+8,C'/'                                                      
         MVC   WORK+9(2),WORK+4    MOVE DD TO NEXT  POSITIONS                   
         MVI   WORK+11,C'/'                                                     
         MVC   WORK+12(2),WORK+4   MOVE YY TO NEXT POSITIONS                    
         MVC   KENTRYD(8),WORK+6   INSERT ENTRY DATE                            
         MVC   KENTERD(8),WORK+6   INSERT CREATE DATE                           
         MVC   KREVDATE(8),WORK+6  INSERT REVISION DATE                         
*                                                                               
*   DDS DOESN'T HAVE A MARKET NUMBER                                            
*                                                                               
         MVC   KSTATION(4),RCONKSTA INSERT STATION CALL LETTERS                 
*                                                                               
*                                                                               
**********************************************************************          
*        PROCESS ADV SECTION                                                    
**********************************************************************          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,8               INSERT ADV REC TYPE                          
***>>>   MVC   RSALKREP,=C'SZ'     INSERT REP CODE                              
         MVC   RADVKREP,=C'V1'     INSERT REP CODE                              
         MVC   RADVKADV,RCONKADV   INSERT ADV CODE                              
         GOTO1 HIGH                                                             
         CLC   KEY,KEYSAVE         KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                NO                                           
         MVC   AIOAREA,AIOAREA2    LOAD A(RECORD2)                              
         GOTO1 GETREC              RETRIEVE RECORD                              
         MVC   KADV#,RADVKATZ      INSERT KATZ ORIGINAL ADVERTISER              
         MVC   KADVNAM(20),RADVNAME    INSERT ADVERTISER  NAME                  
**********************************************************************          
         LA    R6,RCONREC                                                       
         MVI   ELCODE,5            FIND PRODUCT CODE ELEMENT                    
         BAS   RE,GETEL                                                         
         BNZ   CPRO0120            NOT FOUND                                    
         MVC   KPROD(20),0(R6)     INSERT PRODUCT NAME                          
CPRO0120 EQU   *                                                                
*                                                                               
**********************************************************************          
*        PROCESS AGY SECTION                                                    
**********************************************************************          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,10              INSERT AGY REC TYPE                          
***>>>   MVC   RAGYKREP,=C'SZ'     INSERT REP CODE                              
         MVC   RAGYKREP,=C'V1'     INSERT REP CODE                              
         MVC   RAGYKAGY,RCONKAGY   INSERT AGY CODE                              
         MVC   RAGYKAOF,RCONKAOF   INSERT AGY OFFICE CODE                       
         GOTO1 HIGH                                                             
         CLC   KEY,KEYSAVE         KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                NO                                           
         MVC   AIOAREA,AIOAREA2    LOAD A(RECORD2)                              
         GOTO1 GETREC              RETRIEVE RECORD                              
         MVC   KAGYNAM(20),RAGYNAM1    INSERT ADVERTISER  NAME                  
         MVC   KADDR1(20),RAGYADD1 INSERT FIRST LINE OF ADDR                    
         MVC   KADDR2(20),RAGYADD2 INSERT 2ND   LINE OF ADDR                    
         XC    KEY,KEY                                                          
         MVI   KEY,X'1A'           INSERT AGY REC TYPE                          
***>>>   MVC   RAGYKREP,=C'SZ'     INSERT REP CODE                              
         MVC   RAGYKREP,=C'V1'     INSERT REP CODE                              
         MVC   RAGYKAGY,RCONKAGY   INSERT AGY CODE                              
         MVC   RAGYKAOF,RCONKAOF   INSERT AGY OFFICE CODE                       
         GOTO1 HIGH                                                             
         CLC   KEY,KEYSAVE         KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                NO                                           
         MVC   AIOAREA,AIOAREA2    LOAD A(RECORD2)                              
         GOTO1 GETREC              RETRIEVE RECORD                              
         MVC   KAGY#,RAGY2EQU      INSERT KATZ ORIGINAL ADVERTISER              
**********************************************************************          
         MVC   KBUYER(20),RCONBUYR   INSERT BUYER                               
         MVC   KSERVICE,RCONRTGS   INSERT RATING SERVICE                        
*                                                                               
*   SETTING FOR CX-TYPE ???                                                     
*                                                                               
         GOTO1 DATCON,DMCB,(3,RCONDATE),(0,WORK)                                
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
         MVC   KBILLWK(3),WORK+6   INSERT BILLING WEEK START                    
         MVC   WORK+6(2),WORK+2    MOVE MM TO FIRST POSITIONS                   
         MVC   WORK+8(2),WORK+4    MOVE DD TO NEXT  POSITIONS                   
         MVC   WORK+10(2),WORK+4   MOVE YY TO NEXT POSITIONS                    
         MVC   KSCHED1(6),WORK+6   INSERT START DATE                            
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(0,WORK)                              
         CLC   RCONDATE(3),RCONDATE+3                                           
*                                  START/END DATES SAME?                        
         BE    CPRO0160            YES                                          
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
         MVC   KBILLWK+4(3),WORK+6   INSERT BILLING WEEK END                    
         MVI   KBILLWK+3,C'-'      INSERT SEPARATOR                             
CPRO0160 EQU   *                                                                
         MVC   WORK+6(2),WORK+2    MOVE MM TO FIRST POSITIONS                   
         MVC   WORK+8(2),WORK+4    MOVE DD TO NEXT  POSITIONS                   
         MVC   WORK+10(2),WORK+4   MOVE YY TO NEXT POSITIONS                    
         MVC   KSCHED1+7(6),WORK+6 INSERT END   DATE                            
         MVI   KSCHED1+6,C'-'      INSERT SEPARATOR                             
         EDIT  RCONWKS,(2,K#WEEKS),FILL=0                                       
*                                  INSERT NUMBER OF WEEKS                       
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'        LOOK FOR SAR ELEMENT ELEMENTS                
         BAS   RE,GETEL                                                         
         BNZ   CPRO0180            NO SAR ELEMENT                               
         ZIC   R1,RSARXSHG-RSARXEL(R6)                                          
*                                  RETRIEVE SHARE GOAL                          
         EDIT  (R1),(3,KBUDGET),FILL=0                                          
CPRO0180 EQU   *                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,2            LOOK FOR CONTRACT COMMENT ELEMENTS           
         BAS   RE,GETEL                                                         
         BNZ   CPRO0200            NO COMMENTS                                  
         ZIC   RF,1(R6)            GET COMMENT LENGTH                           
         SH    RF,=H'3'            SUBTRACT FOR CONTROL + EX                    
         LA    RE,KCOMMT1                                                       
         EX    RF,CPRO0190         MOVE COMMENT BY LENGTH                       
         BAS   RE,NEXTEL           GET NEXT 02 ELEMENT                          
         BNZ   CPRO0200            NOT FOUND                                    
         ZIC   RF,1(R6)            GET COMMENT LENGTH                           
         SH    RF,=H'3'                                                         
         LA    RE,KCOMMT2                                                       
         EX    RF,CPRO0190                                                      
         B     CPRO0200                                                         
CPRO0190 EQU   *                                                                
         MVC   0(0,RE),2(R6)       MOVE COMMENT BY LENGTH                       
CPRO0200 EQU   *                                                                
         MVI   KTOTIND,C'T'        SET INDICATOR TO 'TOTAL'                     
                                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*  CODE TO PROCESS A BUY KEY FROM THE CONTRACT RECORD                           
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'0B'           INSERT RECORD TYPE                           
*                                                                               
*   MUST BE MADE SOFT                                                           
*                                                                               
         MVC   KEY+16(2),=C'SZ'    INSERT REP CODE                              
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),RCONKCON                                              
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         PACK  KEY+18(1),WORK+18(1)    REVERSE THE COMPLIMENT                   
         PACK  KEY+19(1),WORK+17(1)                                             
         PACK  KEY+20(1),WORK+16(1)                                             
         PACK  KEY+21(1),WORK+15(1)                                             
         GOTO1 HIGH                                                             
CPRO2000 EQU   *                                                                
         EJECT                                                                  
*                                                                               
*   BUILDSAR:  BUILD THE X'12' ELEMENT                                          
*                                                                               
BUILDSAR NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R5,4(R1)            RESET A(KATZ RECORD)                         
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   BUILDBUY: CONVERT DDS BUYLINES TO KATZ FORMAT.                              
*                                                                               
BUILDBUY NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
*   COMMERCIAL LENGTH TABLE                                                     
*      POS 1-2  =  KATZ CODE                                                    
*      POS 3-4  =  DDS VALUE:  POS 3 = MINUTES FLAG IF X'80'                    
*                              POS 4 = COMMERCIAL LENGTH                        
*                                                                               
COMMLENG DC    C'**',AL1(0,30)     SPORTS FORCED TO 30'S                        
LLEN     EQU   *-COMMLENG                                                       
         DC    C'05',AL1(0,05)     5      SECONDS                               
         DC    C'10',AL1(0,10)     10     SECONDS                               
         DC    C'15',AL1(0,15)     15     SECONDS                               
         DC    C'20',AL1(0,20)     20     SECONDS                               
         DC    C'30',AL1(0,30)     30     SECONDS                               
         DC    C'40',AL1(0,40)     40     SECONDS                               
         DC    C'45',AL1(0,45)     45     SECONDS                               
         DC    C'60',AL1(0,60)     60     SECONDS                               
         DC    C'75',AL1(0,75)     75     SECONDS                               
         DC    C'90',AL1(0,90)     90     SECONDS                               
         DC    C'12',AL1(128,02)   2      MINUTES                               
         DC    C'18',AL1(128,03)   3      MINUTES                               
         DC    C'24',AL1(128,04)   4      MINUTES                               
         DC    C'5M',AL1(128,05)   5      MINUTES                               
         DC    C'HH',AL1(128,30)   30     MINUTES                               
         DC    C'1H',AL1(128,60)   60     MINUTES                               
         DC    C'2H',AL1(128,120)  120    MINUTES                               
         DC    C'5H',AL1(129,44)   300    MINUTES                               
         DC    X'0000'             DELIMITER                                    
*              LAST ENTRY REPRESENTS 5 HOURS -> 300 MINUTES                     
*              300 = 012C HEX.  HIGH ORDER BIT -> 812C.                         
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
******************************************************************              
*  DISPIPUT:  DISPLAY KATZ RECORD INPUT.                         *              
******************************************************************              
*                                                                               
DISPIPUT NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         LA    R4,RECORD3                                                       
         USING KCONREC1,R4                                                      
         GOTO1 REPORT                                                           
         MVC   P+1(14),=C'CON#/DIVISION='                                       
         MVC   P+20(7),KCONNUM                                                  
         MVC   P+30(2),KDIVISN                                                  
         EDIT  CONCTR,(7,P+36)     INSERT RECORD NUMBER                         
         GOTO1 REPORT                                                           
*                                                                               
         DROP  R4                                                               
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'FIRST PORTION:  CONTRACT'                             
         GOTO1 REPORT                                                           
         SR    RF,RF                                                            
         LA    RF,LKTZREC1         GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*                                  DISPLAY FIRST PORTION OF RECORD              
         LR    R5,R4                                                            
         A     R5,DISPPRT3         SET FOR END OF LOOP                          
         A     R4,DISPBUYS         BUMP TO BUY SECTION OF RECORD                
DIPI0040 EQU   *                                                                
         CLC   0(10,R4),SPACES     ANYTHING IN ENTRY?                           
         BE    DIPI0080            NO  - BUYS FINISHED                          
         GOTO1 REPORT                                                           
         MVC   P+1(07),=C'BUYLINE'                                              
         GOTO1 REPORT                                                           
         LA    RF,KBUYLEN          YES - DISPLAY THE ENTRY                      
         LA    RF,20(RF)           DISPLAY FIRST PART OF NEXT LINE              
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*                                  DISPLAY BUY PORTION OF RECORD                
         LA    R4,KBUYLEN(R4)                                                   
         CR    R4,R5               ALL 144 ENTRIES SCANNED?                     
         BL    DIPI0040            NO  - GO BACK FOR NEXT                       
DIPI0080 EQU   *                                                                
         LA    R5,KCICSREV-KCONPRT3(R5)                                         
*                                  BUMP TO PART2 OF THIRD PART                  
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'THIRD PORTION:  FIRST PT'                             
         GOTO1 REPORT                                                           
         LA    RF,LKTZ#3P2         SET LENGTH                                   
         GOTO1 =V(PRNTBL),DMCB,(0,(R5)),(R5),C'DUMP',(RF),=C'1D'                
*                                  DISPLAY BUY PORTION OF RECORD                
         LA    R5,KKEYDEMO-KCICSREV(R5)                                         
*                                  BUMP TO PART3 OF THIRD PART                  
         GOTO1 REPORT                                                           
         MVC   P+1(24),=C'THIRD PORTION:  2ND   PT'                             
         GOTO1 REPORT                                                           
         LA    RF,LKTZ#3P3         SET LENGTH                                   
         GOTO1 =V(PRNTBL),DMCB,(0,(R5)),(R5),C'DUMP',(RF),=C'1D'                
*                                  DISPLAY BUY PORTION OF RECORD                
DIPI0120 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
*   HELLO CALLS:  LABEL NAME INDICATES RECORD AND ELEMENT ADDRESS:              
*        HELO  =  HELLO CALL                                                    
*        CON   =  CONTRACT RECORD, BUY   =  BUY RECORD                          
*        1     =  ELTBILD1, ETC                                                 
*                                                                               
HELOCON1 NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,ELTBILD1,0             
         XC    ELTBILD1,ELTBILD1                                                
         B     HELOEXIT                                                         
         SPACE 3                                                                
HELOCON2 NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,ELTBILD2,0             
         XC    ELTBILD2,ELTBILD2                                                
         B     HELOEXIT                                                         
         SPACE 3                                                                
HELOCON3 NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,ELTBILD3,0             
         XC    ELTBILD3,ELTBILD3                                                
         B     HELOEXIT                                                         
         SPACE 3                                                                
HELOBUY1 NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RBUYREC,ELTBILD1,0             
         XC    ELTBILD1,ELTBILD1                                                
         B     HELOEXIT                                                         
         SPACE 3                                                                
HELOBUY2 NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RBUYREC,ELTBILD2,0             
         XC    ELTBILD2,ELTBILD2                                                
         B     HELOEXIT                                                         
         SPACE 3                                                                
HELOBUY3 NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RBUYREC,ELTBILD3,0             
         XC    ELTBILD3,ELTBILD3                                                
         B     HELOEXIT                                                         
         SPACE 3                                                                
HELOBUYA NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RBUYREC,ELTBILD1,0             
*                                  DON'T CLEAR THE ELEMENT                      
         B     HELOEXIT                                                         
         SPACE 3                                                                
HELOEXIT EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  DISPPUT:  DISPLAY RECORD JUST 'PUT' TO OUTPUT.                *              
*                                                                *              
******************************************************************              
*                                                                               
DISPPUT  NTR1                                                                   
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         CLC   CONCTR,LOWCTR       DISPLAY RANGE OF RECORDS                     
         BL    DIPU0090                                                         
         CLC   CONCTR,HIGHCTR                                                   
         BH    DIPU0090                                                         
         GOTO1 REPORT                                                           
         MVC   P+1(8),=C'CONTRACT'                                              
         CLI   REC,X'0C'           CONTRACT?                                    
         BE    DIPU0020            YES                                          
         MVC   P+1(8),=C'BUY REC '                                              
DIPU0020 EQU   *                                                                
         MVC   P+10(06),=C'OUTPUT'                                              
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
******************************************************************              
*  PUTRECS:  GENERATE OUTFILE ENTRIES                            *              
******************************************************************              
*                                                                               
PUTRECS  NTR1                                                                   
         L     RF,PUTCTR           INCREMENT COUNTER                            
         LA    RF,1(RF)                                                         
         ST    RF,PUTCTR           PUT IT BACK                                  
*                                                                               
*        MVC   P+1(07),=C'PUTREC:'                                              
*        MVC   P+20(27),REC                                                     
*        EDIT  PUTCTR,(5,P+10)                                                  
*        GOTO1 REPORT                                                           
*                                                                               
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
*   INITIALIZATIONS ....                                                        
INITIAL  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         OPEN  (FILOUTA,(OUTPUT))                                               
         OPEN  (FILOUTB,(OUTPUT))                                               
         SPACE 1                                                                
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
         LA    RF,RECORD2                                                       
         ST    RF,AIOAREA2                                                      
         LA    RF,RECORD                                                        
         ST    RF,AIOAREA1                                                      
*                                                                               
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
TOTCTR   DS    F                                                                
SHOCTR   DS    F                                                                
INCCTR   DS    F                                                                
CONCTR   DS    F                                                                
BUYCTR   DS    F                                                                
LOWCTR   DC    F'99999'            LOW DISPLAY COUNT                            
HIGHCTR  DC    F'99999'            HIGH COUNTER                                 
PUTCTR   DS    F                                                                
PUTCTR2  DS    F                                                                
AIOAREA  DS    F                                                                
AIOAREA1 DS    F                                                                
AIOAREA2 DS    F                                                                
DISPBUYS DC    F'724'              DISPLACEMENT TO BUYLINES                     
DISPPRT3 DC    F'11956'            DISPLACEMENT TO THIRD PART OF REC            
DATEWORK DS    CL48                DATE WORK AREA                               
COMMAND  DS    CL8                                                              
ELTBILD1 DS    CL128                                                            
ELTBILD2 DS    CL128                                                            
ELTBILD3 DS    CL128                                                            
RUNSTRT  DS    F                                                                
RUNTIME  DS    F                                                                
RUNEND   DS    F                                                                
WORK2    DS    CL256                                                            
ENTRYDAT DS    CL2                                                              
SAVESP   DS    CL3                 BINARY S/P NUMBER                            
DATES1   DS    CL6                 FIRST FLIGHT DATES                           
DATEALT1 DS    CL1                 ALTERNATE FLAG                               
DATEWKS1 DS    CL2                 # WEEKS                                      
LDATESET EQU   *-DATES1                                                         
DATES2   DS    CL6                 SECOND FLIGHT DATES                          
DATEALT2 DS    CL1                 ALTERNATE FLAG                               
DATEWKS2 DS    CL2                 # WEEKS                                      
DATES3   DS    CL6                 THIRD FLIGHT DATES                           
DATEALT3 DS    CL1                 ALTERNATE FLAG                               
DATEWKS3 DS    CL2                 # WEEKS                                      
LDATES   EQU   *-DATES1                                                         
*                                                                               
       ++INCLUDE REKTZOFF                                                       
         EJECT                                                                  
       ++INCLUDE REKTZDMO                                                       
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
REC      DS    CL1008              AREA FOR RECORD                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
*  INCLUDE REGENBUY                BUY RECORD                                   
*  INCLUDE REGENCON                CONTRACT RECORD                              
*  INCLUDE REGENSAL                SALESPERSON RECORD                           
*  INCLUDE REGENSTA                STATION     RECORD                           
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
FILED    DSECT                                                                  
RECORD   DS    CL1024                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENBUY          BUY RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCON          CONTRACT RECORD                              
         EJECT                                                                  
RECORD2  DS    CL1024                                                           
         ORG   RECORD2                                                          
       ++INCLUDE REGENSAL          SALESPERSON RECORD                           
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REGENSTA          STATION     RECORD                           
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REGENADV          AGENCY      RECORD                           
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REGENAGY          AGENCY      RECORD                           
         EJECT                                                                  
         ORG   RECORD2                                                          
       ++INCLUDE REGENAGY2         AGENCY2     RECORD                           
         EJECT                                                                  
RECORD3  DS    CL16300                                                          
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE REKTZC01                                                       
         EJECT                                                                  
       ++INCLUDE REKTZC02                                                       
         EJECT                                                                  
       ++INCLUDE REKTZC03                                                       
         EJECT                                                                  
*********************************************************************           
         CSECT                                                                  
*                                                                               
******************************************************************              
*  DISPTOTS:                                                     *              
*                                                                *              
*                                                                *              
******************************************************************              
*                                                                               
DISPTOTS NMOD1 0,**DTOT**                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
**       MVC   P+1(24),=C'TOTAL CONTRACTS READ   :'                             
**       EDIT  TOTCTR,(12,P+30),COMMAS=YES                                      
**       GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
*        COMMUNICATION WITH DATA MANAGER (DIRECTORY)                            
         SPACE                                                                  
*READ1    MVC   COMMAND(8),DMREAD                                               
*        B     DIRCTRY1                                                         
*        SPACE 2                                                                
*SEQ1     MVC   COMMAND(8),DMRSEQ                                               
*        B     DIRCTRY1                                                         
*        SPACE 2                                                                
*HIGH1    MVC   COMMAND(8),DMRDHI                                               
*        MVC   KEYSAVE,KEY                                                      
*        B     DIRCTRY1                                                         
*        SPACE 2                                                                
*ADD1     MVC   COMMAND(8),DMADD                                                
*        B     DIRCTRY1                                                         
*        SPACE 2                                                                
*WRITE1   MVC   COMMAND(8),DMWRT                                                
*        B     DIRCTRY1                                                         
*        SPACE 2                                                                
*DIRCTRY1 NTR1                                                                  
*        IC    R4,DMINBTS                                                       
*        MVC   KEYSAVE,KEY                                                      
*        GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPDIR',KEYSAVE,KEY               
*        B     RGENIOD1                                                         
         EJECT                                                                  
*        COMMUNICATION WITH DATA MANAGER (FILE)                                 
         SPACE 3                                                                
*GREC1    MVC   COMMAND(8),GETREC                                               
*        B     FILE1                                                            
         SPACE 2                                                                
*PREC1    MVC   COMMAND(8),PUTREC                                               
*        B     FILE1                                                            
         SPACE 2                                                                
*AREC1    MVC   COMMAND(8),ADDREC                                               
*        B     FILE1                                                            
         SPACE 2                                                                
*FILE1    NTR1                                                                  
*        LA    R2,KEY+28                                                        
*        CLI   COMMAND,C'A'                                                     
*        BNE   *+8                                                              
*        LA    R2,KEY                                                           
*        IC    R4,DMINBTS                                                       
*        ICM   R5,15,AIOAREA                                                    
*        GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPFILE',               X         
*              (R2),(R5),DMWORK                                                 
         SPACE 2                                                                
*                  DATA MANAGER ERRORS AND EXIT                                 
         SPACE 3                                                                
*RGENIOD1 OC    DMCB+8(1),DMCB+8                                                
*                                                                               
*        XIT1                      RETURN                                       
*                                                                               
*                                                                               
* * * * * * * * * END OF INCLUDE DATASET RGENIO * * * * * * * * * *             
         LTORG                                                                  
         EJECT                                                                  
         DROP  RC                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'122REREPE102 05/01/02'                                      
         END                                                                    
