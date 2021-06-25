*          DATA SET RECNT6F    AT LEVEL 011 AS OF 09/16/02                      
*PHASE T8026FA,*                                                                
*INCLUDE OUTDAY                                                                 
*INCLUDE UNTIME                                                                 
*INCLUDE DAYUNPK                                                                
         TITLE 'T8026F - RECNT6F - COLUMBINE EC'                                
*                                                                               
*********************************************************************           
*                                                                   *           
*        RECNT6F --- COLUMBINE  ELECTRONIC CONTRACT INTERFACE       *           
*********************************************************************           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* MAR25/94 (BU ) --- ORIGINAL ENTRY                                 *           
*                                                                   *           
* APR01/94 (BU ) --- TIME-STAMP:  ADJUST HOURS...                   *           
*                                                                   *           
* APR22/94 (BU ) --- EXTRA 'LONG-AGENCY-NAME' RECORD OUTPUT.        *           
*                                                                   *           
* APR25/94 (BU ) --- FIX CALENDAR SINGLE-DAY ERROR                  *           
*                                                                   *           
* JUN02/95 (SKU) --- UPDATE EDICT DSECT                             *           
*                                                                   *           
* SEP06/95 (BU ) --- SET SPONSORSHIP LENGTH TO ZERO                 *           
*                                                                   *           
* SEP29/95 (BU ) --- CLASS 'G' SET LIVE, PER D. EIS, FOR EDICT      *           
*                    TESTING                                        *           
*                                                                   *           
* JAN11/96 (BU ) --- SEND STORED COMMENT, IF FOUND                  *           
*                    LEAVE MIDNIGHT AS 2400, 1201A --> 0001         *           
*                                                                   *           
* DEC12/95 (SKU) --- 2K CONTRACT SUPPORT                            *           
*                                                                   *           
* FEB06/96 (RHV) --- FIX COMMENT DISP BUG                           *           
*                                                                   *           
* JUN11/96 (BU ) --- ADD PTRY, TELE, SELT CODES                     *           
*                                                                   *           
* 26JUL96 (SKU) --- CHANGE TO USE THMS FOR TIME STAMPING            *           
*                                                                   *           
* 20FEB97 (BU )  --- OUTPUT STATION CALLS RATHER THAN ZEROS         *           
*                                                                   *           
* 09JUL97 (BU )  --- UPGRADE FOR YR 2000                            *           
*                                                                   *           
* 24JUL97 (SKU)  --- 4K CONTRACT SUPPORT                            *           
*                                                                   *           
* 26AUG97 (SKU)  --- BUG FIX OF WRONG COMMENT LENGTH                *           
*                                                                   *           
* 08APR98 (BU )  --- RADIO VERSION:  A/F OVERRIDES LAST STATION CALL*           
*                                                                   *           
* 17JUN98 (BU )  --- RADIO VERSION:  A/F OVERRIDES 1ST  STATION CALL*           
*                                                                   *           
* 16JUL98 (BU )  --- CHRISTAL RADIO:  ADD TO TRANSLATE              *           
*                                                                   *           
* 26JUL99 (BU )  --- SKIP CANCELLED BUYS                            *           
*                                                                   *           
* 01MAR01 (BU )  --- ENCODA ROUTING                                 *           
*                                                                   *           
* 04MAY01 (BU )  --- PROGRAM NAME ADJUSTMENT OF COMMENTS            *           
*                                                                   *           
* 16SEP02 (BU )  --- FOX (FB) = FXFX: PER DEAN HARDER               *           
*                                                                   *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
T8026F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LOCALEND-LOCALWRK,*T8026F*,R9,RR=R8                              
         ST    R8,RELX                                                          
         LR    R3,RC               SET A(MODULE WORK SPACE)                     
         USING LOCALWRK,R3         SET DSECT FOR AREA                           
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,VTWA                                                          
         USING TWAD,RA                                                          
****>    LA    R8,SPOOLAR          SET A(SPOOLAR) RATHER INDIRECTLY             
*        LA    R8,RBUYREC             3000 AFTER RBUYREC                        
*        A     R8,=F'3000'                                                      
         L     R8,ASPULAR                                                       
         ST    R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*********************************************************************           
*        MAIN LINE PROCESSING                                                   
*********************************************************************           
MAIN     EQU   *                                                                
         BAS   RE,INIT             SET INITAL ADDRS AND VALUES                  
         BAS   RE,GETCON           READ CONTRACT RECORD                         
         BNZ   EXXMOD                                                           
*                                                                               
         GOTO1 =A(GETSTA),DMCB,(RC),RCONKSTA,RR=YES                             
*                                  READ STATION RECORD INTO AIO3                
         BNZ   EXXMOD                                                           
*                                                                               
         GOTO1 =A(FINDID),DMCB,(RC),RR=YES      FIND SEND ID                    
         BNZ   EXXMOD                                                           
*                                                                               
         GOTO1 =A(PQOPENA),DMCB,(RC),(R8),RR=YES                                
*                                  OPEN PRINT QUEUE                             
*                                                                               
         GOTO1 =A(EDICT),DMCB,(RC),(R8),RR=YES                                  
*                                  CREATE EDICT HEADER CARDS                    
*                                                                               
MN100    EQU   *                   NO NEED TO 'GET REP'                         
*                                                                               
*   TOTALS ARE NOT REQUIRED IN THE COLUMBINE VERSION. THE CODE,                 
*      THOUGH NOT ACCESSED, IS LEFT IN, IN EVENT THE SITUATION                  
*      CHANGES.  BILL UHR.   MARCH 1994.                                        
*                                                                               
****>    BAS   RE,TOTLBUYS         READ BUYS TO GET TOTALS                      
*                                                                               
MN200    EQU   *                                                                
         BAS   RE,BLD01REC         'EORTYP' CONTROL RECORD                      
*                                                                               
MN300    EQU   *                                                                
         BAS   RE,BLD03REC         CONTRACT MAIN LINE ADD                       
*                                                                               
MN400    EQU   *                                                                
         BAS   RE,BLD05REC         CONTRACT ENDING ADD                          
*                                                                               
MN600    EQU   *                                                                
         BAS   RE,DATETIME         DATE AND TIME STAMP                          
*                                                                               
         MVI   SPMODE,X'FF'        CLOSE THE PRINT QUEUE                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXXMOD                                                           
         EJECT                                                                  
                                                                                
*********************************************************************           
*        TOTLBUYS  ---  SCAN BUYS TO DETERMINE TOTAL SPOTS, $$$*                
*********************************************************************           
*TOTLBUYS NTR1                                                                  
*         XC    KEY,KEY             INITIALIZE KEY                              
*         LA    RF,KEY                                                          
*         USING RBUYREC,RF                                                      
*         MVI   RBUYKEY,X'0B'       INSERT ID                                   
*         MVC   RBUYKREP,REPALPHA                                               
*         LR    RE,RA                                                           
*         AH    RE,=Y(TWAWORKQ)     4K                                          
*         USING TWAWORK,RE                                                      
*         MVC   RBUYKCON,TWACNUM                                                
*         DROP  RE,RF                                                           
*         GOTO1 VHIGH                                                           
*         B     TOBU0040                                                        
*TOBU0020 EQU   *                                                               
*         GOTO1 VSEQ                                                            
*TOBU0040 EQU   *                                                               
*         CLC   KEY(22),KEYSAVE     COMPARE THROUGH CONTRACT #                  
*         BNE   TOBU0900            NO MORE BUYS - FINISHED                     
*TOBU0060 EQU   *                                                               
*         MVI   ION,2               SET IO2 AREA AS INPUT                       
*         GOTO1 VGETREC,DMCB,RBUYREC                                            
*         LA    R4,RBUYELEM         CYCLE THRU DATE ELEMENTS                    
*TOBU0080 EQU   *                                                               
*         CLI   0(R4),0             END OF RECORD?                              
*         BE    TOBU0020            YES - GO BACK FOR NEXT BUY REC              
*         CLI   0(R4),3             DATE ELEMENT?                               
*         BE    TOBU0100            YES                                         
*         ZIC   RF,1(R4)            NO  - BUMP TO NEXT ELEMENT                  
*         AR    R4,RF                                                           
*         B     TOBU0080            GO BACK FOR NEXT                            
*TOBU0100 EQU   *                                                               
*         USING RBUYDTEL,R4                                                     
*         ZIC   RF,RBUYDTNW         # PER WEEK                                  
*         ZIC   R2,RBUYDTWK         # OF WEEKS                                  
*         SR    RE,RE                                                           
*         MR    RE,R2               # PER WK * # OF WKS = TTL SPOTS             
*         LA    R1,RBUYELEM         APPLY MISSED SPOTS                          
*TOBU0120 EQU   *                                                               
*         CLI   0(R1),0             END OF RECORD?                              
*         BE    TOBU0180            YES                                         
*         CLI   0(R1),6             MISSED ELEMENT?                             
*         BE    TOBU0160            YES                                         
*         CLI   0(R1),7             MISSED CREDIT ISSUED?                       
*         BE    TOBU0160            YES                                         
*TOBU0140 EQU   *                                                               
*         ZIC   R5,1(R1)            NO  - BUMP TO NEXT ELEMENT                  
*         AR    R1,R5                                                           
*         B     TOBU0120            GO BACK FOR NEXT                            
*TOBU0160 EQU   *                   CHECK MISSED DATE VS DATE ELT               
*         USING RBUYMSEL,R1                                                     
**                                                                              
**   BOTH BUY MISSED AND CREDIT MISSED USING RBUYMSEL FOR DSECT HERE            
**                                                                              
*         CLC   RBUYMSDT,RBUYDTST   CHECK START DATE                            
*         BL    TOBU0140            EARLIER - CHECK NEXT DATE                   
*         CLC   RBUYMSDT,RBUYDTED   CHECK END   DATE                            
*         BH    TOBU0140            LATER   - CHECK NEXT DATE                   
**                                                                              
**   MISSED ELEMENT APPLIES, SUBTRACT MISSED SPOTS                              
**                                                                              
*         ZIC   R5,RBUYMSSP         MISSED SPOTS                                
*         SR    RF,R5                                                           
*         B     TOBU0140            GO BACK FOR NEXT                            
**                                                                              
*         DROP  R1                                                              
*TOBU0180 EQU   *                                                               
*         LR    R5,RF               SAVE TOTAL SPOTS                            
*         A     RF,TOTLSPTS         ACCUMULATE TOTAL SPOTS                      
*         ST    RF,TOTLSPTS         STORE IT BACK                               
*         SR    RE,RE               CALCULATE TOTAL COST                        
*         LR    RF,R5               RESET TOTAL SPOTS FOR BUY                   
*         MVC   FULL,RBUYCOS        MOVE OUT FOR ALIGNMENT                      
*         M     RE,FULL             SPOTS * SPOT COST = LINE COST               
*         A     RF,TOTLDOLS         ACCUMULATE TOTAL COST                       
*         ST    RF,TOTLDOLS         STORE IT BACK                               
**                                                                              
**   FOLLOWING CODE IS TEST CODE TO DISPLAY RESULTS WITHIN PRINTOUT             
**                                                                              
*****>    MVC   P+1(23),=C'SPOTS:            COST:'                             
*****>    EDIT  (R5),(11,P+7),COMMAS=YES                                        
*****>    EDIT  RBUYCOS,(15,P+31),2,COMMAS=YES                                  
*****>    GOTO1 SPOOL,DMCB,(R8)                                                 
**                                                                              
**   END OF TEST CODE                                                           
**                                                                              
*         ZIC   RF,1(R4)            BUMP TO NEXT ELEMENT                        
*         AR    R4,RF                                                           
*         B     TOBU0080            GO BACK FOR NEXT EFF DATE ELT               
*TOBU0900 EQU   *                                                               
**                                                                              
**   FOLLOWING CODE IS TEST CODE TO DISPLAY RESULTS WITHIN PRINTOUT             
**                                                                              
*****>    MVC   P+1(31),=C'TOT SPOTS:            TOT COST:'                     
*****>    EDIT  TOTLSPTS,(11,P+11),COMMAS=YES                                   
*****>    EDIT  TOTLDOLS,(11,P+39),2,COMMAS=YES                                 
*****>    GOTO1 SPOOL,DMCB,(R8)                                                 
**                                                                              
**   END OF TEST CODE                                                           
**                                                                              
*         DROP  R4                                                              
*         XIT1                                                                  
         EJECT                                                                  
*********************************************************************           
*        BLD01REC  ---  BUILDS THE 'EORTYP' CONTROL RECORD                      
*********************************************************************           
BLD01REC NTR1                                                                   
         MVI   EORTYP,C' '         SPACE FILL THE RECORD                        
         MVC   EORTYP+1(127),EORTYP                                             
         MVI   EORTYP,C'H'         INSERT RECORD TYPE                           
         MVC   WORK(10),FOXZEROS                                                
         GOTO1 HEXOUT,DMCB,RCONKCON,WORK+2,4,=C'TOG'                            
         MVC   EOCON#,WORK         INSERT CONTRACT NUMBER                       
*                                                                               
*   SET ORDER/ISSUE DATE                                                        
         GOTO1 DATCON,DMCB,(3,RCONCREA),(X'20',WORK+1)                          
         BAS   RE,SETCNTRY         SET CENTURY INDICATOR                        
         MVC   EODATE,WORK         INSERT DATE                                  
         MVC   ORDISSDT,WORK       SAVE ORDER/ISSUE DATE                        
*                                                                               
*   GET FLIGHT DATES                                                            
*                                                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,WORK2,0,DUB                       
*                                                                               
*   SET FLIGHT START DATE                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK2),(X'20',WORK+1)                             
         BAS   RE,SETCNTRY         SET CENTURY INDICATOR                        
         MVC   EOSDATE,WORK        INSERT DATE                                  
*                                                                               
*   SET FLIGHT END   DATE                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK2+6),(X'20',WORK+1)                           
         BAS   RE,SETCNTRY         SET CENTURY INDICATOR                        
         MVC   EOEDATE,WORK        INSERT DATE                                  
         MVI   EOCOOP,C' '         CO-OP FLAG: SPACE-FILL                       
         MVC   EOADVNAM(20),ADVNAME                                             
*                                  INSERT ADVERTISER NAME                       
*                                                                               
*   EOEST# - AGENCY EST/PURCHASE ORDER #??                                      
*                                                                               
         BAS   RE,GETDEMO          RETRIEVE PRIME DEMO                          
*                                                                               
         MVC   EOREP,RUNREP        SOURCE/REP HOUSE                             
         MVC   EOSALOFF(2),RCONKOFF                                             
*                                  INSERT SALES OFFICE                          
         MVC   EOAGYNAM(22),AGYNAME INSERT AGENCY NAME                          
         MVC   EOSTAT#,RCONKSTA    INSERT STATION CALL LETTERS                  
         CLI   RCONKSTA+4,C'F'     FM STATION?                                  
         BE    BLD10010            YES                                          
         CLI   RCONKSTA+4,C'A'     AM STATION?                                  
         BNE   BLD10015            YES                                          
BLD10010 EQU   *                                                                
         MVC   EOSTAT#+0(1),RCONKSTA+4                                          
***>>>   MVC   EOSTAT#+3(1),RCONKSTA+4                                          
*                                  OVERLAY 4TH POSITION WITH A/F                
BLD10015 EQU   *                                                                
*        CLC   RCONKSTA,=C'WICU '  SPECIAL STATION?                             
*        BE    BLD10020            YES - USE IT                                 
*        CLC   RCONKSTA,=C'WFXP '  SPECIAL STATION?                             
*        BE    BLD10020            YES - USE IT                                 
*        MVC   EOSTAT#,FOXZEROS    NO  - INSERT STATION NUMBER                  
BLD10020 EQU   *                                                                
         EDIT  RCONMOD,(1,EOREV#),ZERO=NOBLANK                                  
         MVC   EOSALNAM(20),SALNAME                                             
*                                  INSERT SALEPERSON NAME                       
         GOTO1 PRINTREC,DMCB,(RC)                                               
*                                                                               
         CLC   AGYNAME+22(11),SPACES     MORE AGENCY NAME?                      
         BE    BLD10040            NO                                           
         BAS   RE,BLD01AGY         YES - ADD TYPE 'A' RECORD                    
*                                                                               
BLD10040 EQU   *                                                                
         BAS   RE,BLD02COM         ADD CONTRACT COMMENTS                        
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* IF AGENCY NAME EXCEEDS 22 CHARS, TYPE 'A' RECORD IS GENERATED                 
*                                                                               
BLD01AGY NTR1                                                                   
         MVI   EORTYP+11,C' '      SPACE FILL RECORD AFTER CON#                 
         MVC   EORTYP+12(116),EORTYP+11                                         
         MVI   EORTYP,C'A'         INSERT RECORD TYPE                           
         MVC   EORTYP+11(33),AGYNAME                                            
*                                                                               
         GOTO1 PRINTREC,DMCB,(RC)                                               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   IF CONTRACT COMMENTS ARE PRESENT, COMMENT RECORDS ARE GENERATED.            
*                                                                               
BLD02COM NTR1                                                                   
         LA    R6,RCONREC          A(COMMENT RECORD)                            
         MVI   ELCODE,X'02'        LOOK FOR CONTRACT COMMENT                    
BLDC0020 EQU   *                                                                
         BAS   RE,GETEL            GET FIRST ELEMENT                            
         B     BLDC0060                                                         
BLDC0040 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
BLDC0060 EQU   *                                                                
         BNE   BLDC0120            NOT FOUND:  COMMENT TYPE DONE                
         GOTO1 =A(CHKCMTS),DMCB,(RC),(R6),1,RR=Y                                
         BNZ   BLDC0080            NOT STANDARD                                 
*                                                                               
*   BASE COMMENT RECORD ON STANDARD COMMENT                                     
*                                                                               
         LR    R7,R6                    SAVE R6                                 
         L     R6,AIO4             SET A(STANDARD COMMENT RECORD)               
BLDC0065 MVI   E3RTYP,C' '         SPACE FILL THE RECORD                        
         MVC   E3RTYP+1(127),E3RTYP                                             
         MVI   E3RTYP,C'C'         INDICATE COMMENT RECORD                      
         MVI   E3COMTYP,C'H'       INDICATE COMMENT TYPE                        
         MVC   WORK(10),FOXZEROS                                                
         GOTO1 HEXOUT,DMCB,RCONKCON,WORK+2,4,=C'TOG'                            
         MVC   E3RREF,WORK         INSERT CONTRACT NUMBER                       
         MVC   E3BYLN,FOXZEROS     CONTRACT COMMENT: NO BUYLINE #               
         ZIC   RF,0(R6)            GET ELEMENT LENGTH                           
         LA    RE,2                                                             
         SR    RF,RE               SUBTRACT L(CONTROL)                          
*                                                                               
         EX    RF,BLDC0070         MOVE COMMENT                                 
*                                                                               
         MVC   E3STAT#,RCONKSTA    INSERT STATION CALL LETTERS                  
         CLI   RCONKSTA+4,C'F'     FM STATION?                                  
         BE    BLDC0066            YES                                          
         CLI   RCONKSTA+4,C'A'     AM STATION?                                  
         BNE   BLDC0067            YES                                          
BLDC0066 EQU   *                                                                
         MVC   E3STAT#+0(1),RCONKSTA+4                                          
***      MVC   E3STAT#+3(1),RCONKSTA+4                                          
*                                  OVERLAY 4TH POSITION WITH A/F                
BLDC0067 EQU   *                                                                
*        CLC   RCONKSTA,=C'WICU '  SPECIAL STATION?                             
*        BE    BLDC0069            YES - USE IT                                 
*        CLC   RCONKSTA,=C'WFXP '  SPECIAL STATION?                             
*        BE    BLDC0069            YES - USE IT                                 
*        MVC   E3STAT#,FOXZEROS    INSERT STATION #                             
BLDC0069 EQU   *                                                                
         MVC   E3REP,RUNREP        INSERT SOURCE/REP HOUSE                      
         EDIT  RCONMOD,(1,E3RREV),ZERO=NOBLANK                                  
*                                  INSERT REVISION NUMBER                       
*                                                                               
         GOTO1 PRINTREC,DMCB,(RC)                                               
*                                                                               
         ZIC   RF,0(R6)                 EL LENGTH                               
         AR    R6,RF                    POINT TO NEXT COMMENT LN                
         CLI   0(R6),X'FF'              END OF COMMENT?                         
         BE    *+8                                                              
         B     BLDC0065                                                         
*                                                                               
         LR    R6,R7                    RESTORE R6                              
         B     BLDC0040            GO BACK FOR NEXT ELEMENT                     
*                                                                               
BLDC0070 MVC   E3COMM(0),1(R6)                                                  
*                                                                               
BLDC0080 EQU   *                                                                
         MVI   E3RTYP,C' '         SPACE FILL THE RECORD                        
         MVC   E3RTYP+1(127),E3RTYP                                             
         MVI   E3RTYP,C'C'         INDICATE COMMENT RECORD                      
         MVI   E3COMTYP,C'H'       INDICATE COMMENT TYPE                        
         MVC   WORK(10),FOXZEROS                                                
         GOTO1 HEXOUT,DMCB,RCONKCON,WORK+2,4,=C'TOG'                            
         MVC   E3RREF,WORK         INSERT CONTRACT NUMBER                       
         MVC   E3BYLN,FOXZEROS     CONTRACT COMMENT: NO BUYLINE #               
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT L(CONTROL) AND OVERHEAD             
*                                                                               
         EX    RF,BLDC0900         MOVE COMMENT                                 
*                                                                               
         MVC   E3STAT#,RCONKSTA    INSERT STATION CALL LETTERS                  
         CLI   RCONKSTA+4,C'F'     FM STATION?                                  
         BE    BLDC0090            YES                                          
         CLI   RCONKSTA+4,C'A'     AM STATION?                                  
         BNE   BLDC0095            YES                                          
BLDC0090 EQU   *                                                                
         MVC   E3STAT#+0(1),RCONKSTA+4                                          
***      MVC   E3STAT#+3(1),RCONKSTA+4                                          
*                                  OVERLAY 4TH POSITION WITH A/F                
BLDC0095 EQU   *                                                                
*        CLC   RCONKSTA,=C'WICU '  SPECIAL STATION?                             
*        BE    BLDC0100            YES - USE IT                                 
*        CLC   RCONKSTA,=C'WFXP '  SPECIAL STATION?                             
*        BE    BLDC0100            YES - USE IT                                 
*        MVC   E3STAT#,FOXZEROS    INSERT STATION #                             
BLDC0100 EQU   *                                                                
         MVC   E3REP,RUNREP        INSERT SOURCE/REP HOUSE                      
         EDIT  RCONMOD,(1,E3RREV),ZERO=NOBLANK                                  
*                                  INSERT REVISION NUMBER                       
         GOTO1 PRINTREC,DMCB,(RC)                                               
*                                                                               
         B     BLDC0040            GO BACK FOR NEXT ELEMENT                     
*                                                                               
BLDC0120 EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
BLDC0900 MVC   E3COMM(0),2(R6)                                                  
         EJECT                                                                  
*                                                                               
*    THIS ROUTINE BUILDS THE 'E2RTYP' BUYLINE RECORD.                           
*      1.  FOR BUY RECORDS WITH MULTIPLE EFFECTIVE DATES,                       
*          ONE TYPE 'M'  RECORD PER EFFECTIVE DATE IS GENERATED.                
*      2.  COMMENTS ARE GEN'D AS TYPE 'C' RECORDS                               
*                                                                               
BLD03REC NTR1                                                                   
         XC    EXTRAKEY,EXTRAKEY   CLEAR EXTRAKEY AREA                          
         XC    SVBYLN#,SVBYLN#     CLEAR BUYLINE COUNT                          
*                                                                               
*   NOTE RE:  SVBYLN#.  OUTPUT BUYLINE NUMBERS WILL NOT NECESSARILY             
*      CORRESPOND WITH DDS NUMBERS.  IF A BUYLINE CONTAINS MULTIPLE             
*      EFFECTIVE DATE ELEMENTS THAT CANNOT BE REDUCED TO A SINGLE               
*      ENTRY, EACH WILL GENERATE A CONSECUTIVELY NUMBERED OUTPUT                
*      RECORD.  FOR EXAMPLE:  BUY # 1 CONTAINS 2 EFF DATE ELEMENTS,             
*      BUY # 2 CONTAINS 1 EFF DATE ELEMENT.  THE OUTPUT WILL CONSIST            
*      OF THREE RECORDS:  # 1 =  BUY # 1, EFF DATE # 1                          
*                         # 2 =  BUY # 1, EFF DATE # 2                          
*                         # 3 =  BUY # 2, EFF DATE # 1.                         
*      COLUMBINE HAS INDICATED THAT THEY HAVE A METHOD TO KEEP TRACK            
*      OF WHAT BELONGS TO WHAT.   BILL UHR (MAR24/94, AFTER A CONVERSE          
*      WITH CANDY HELGERSON OF COLUMBINE.)                                      
*                                                                               
BLD30020 EQU   *                                                                
         GOTO1 READBUY,DMCB,(RC)                                                
         BZ    BLD30460            NO MORE BUYS - EXIT                          
         CLC   RBUYKMLN(2),=X'FFFF'        BUY PLAN RECORD?                     
         BE    BLD30020            YES - BYPASS                                 
         CLI   RBUYCHGI,C'C'       CANCELLED?                                   
         BE    BLD30020            YES - SKIP IT                                
         CLI   RBUYCHGI+1,C'C'     CANCELLED?                                   
         BE    BLD30020            YES - SKIP IT                                
         TM    RBUYDUR,X'40'       SPORTS BUY?                                  
         BO    BLD30020            YES - SKIP IT                                
         XC    COLLAPSE,COLLAPSE   USED IN COLLAPSE BUY ROUTINE                 
         XC    MULTEFDT,MULTEFDT   CTR FOR MULTIPLE EFF DATES                   
         MVI   E2RTYP,C' '         SPACE FILL THE RECORD                        
         MVC   E2RTYP+1(127),E2RTYP                                             
         GOTO1 BLD3MKGD,DMCB,(RC)  SET MG + COMMENT FLAGS                       
*                                                                               
         OC    ZEROSPTS,ZEROSPTS   ZERO SPOTS PER WEEK?                         
         BZ    BLD30060            NO                                           
         NI    COMFLAGS,X'7F'      TURN OFF M/G SWITCH                          
BLD30060 EQU   *                                                                
         MVI   E2RTYP,C'M'         INSERT RECORD TYPE                           
         MVC   WORK(10),FOXZEROS                                                
         GOTO1 HEXOUT,DMCB,RCONKCON,WORK+2,4,=C'TOG'                            
         MVC   E2RREF,WORK         INSERT CONTRACT NUMBER                       
         MVC   E2ORDDTE,ORDISSDT   INSERT ORDER/ISSUE DATE                      
         MVC   E2STAT#,RCONKSTA    INSERT STATION CALL LETTERS                  
         CLI   RCONKSTA+4,C'F'     FM STATION?                                  
         BE    BLD30065            YES                                          
         CLI   RCONKSTA+4,C'A'     AM STATION?                                  
         BNE   BLD30066            YES                                          
BLD30065 EQU   *                                                                
         MVC   E2STAT#+0(1),RCONKSTA+4                                          
***      MVC   E2STAT#+3(1),RCONKSTA+4                                          
*                                  OVERLAY 4TH POSITION WITH A/F                
BLD30066 EQU   *                                                                
*        CLC   RCONKSTA,=C'WICU '  SPECIAL STATION?                             
*        BE    BLD30067            YES - USE IT                                 
*        CLC   RCONKSTA,=C'WFXP '  SPECIAL STATION?                             
*        BE    BLD30067            YES - USE IT                                 
*        MVC   E2STAT#,FOXZEROS    INSERT STATION #                             
BLD30067 EQU   *                                                                
         MVC   E2REP,RUNREP        INSERT SOURCE/REP HOUSE                      
         EDIT  RCONMOD,(1,E2RREV),ZERO=NOBLANK                                  
*                                  INSERT REVISION NUMBER                       
         EDIT  RBUYCOS,(11,E2COST),FILL=0                                       
*                                  INSERT SPOT COST                             
         CLI   DTSTRNGS,2          HOW MANY D/T STRINGS?                        
         BL    BLD30100            ONE                                          
         MVC   E2FILL(15),=C'MULTI DAY/TIMES'                                   
*                                     SET 'PATTERN RECORDS FOLLOW'              
         MVC   E2STTM(8),FOXZEROS  INSERT ZEROS FOR START/END TIMES             
****>>   MVC   E2DESC(2),=C'*P'    INSERT 'PATTERN NEEDED'                      
*                                     *P NOT BEING USED                         
         MVC   E2LLEN,FOXZEROS     INITIALIZE LENGTH OF SPOT                    
         MVC   E2SPONSR,FOXZEROS   INITIALIZE LENGTH OF SPONSORSHIP             
         TM    RBUYDUR,X'80'       BUY IN MINUTES?                              
         BO    BLD30070            YES                                          
         EDIT  RBUYDUR,(3,E2LLEN),FILL=0,ZERO=NOBLANK                           
*                                  NO  - IN SECONDS                             
         B     BLD30120                                                         
BLD30070 EQU   *                   SPOT LENGTH IN MINUTES                       
         ZICM  R1,RBUYDUR,2        LOAD SPOT LENGTH                             
         SLL   R1,17               DROP HIGH BIT, BYTE 3                        
         SRL   R1,17               MOVE BACK TO ORIGINAL POS                    
         SR    R0,R0                                                            
         M     R0,=F'60'           MINUTES * 60 = TOTAL SECONDS                 
         EDIT  (R1),(3,E2LLEN),FILL=0,ZERO=NOBLANK                              
*                                  INSERT CALCULATED SECONDS                    
         B     BLD30120                                                         
BLD30100 EQU   *                                                                
         GOTO1 DYTIME,DMCB,(RC)    ONE CODE:  GET/TRANSLATE IT                  
BLD30120 EQU   *                                                                
         XC    FIRSTSW,FIRSTSW     INITIALIZE 'ALTERNATE WEEK' IND              
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'        GET EFFECTIVE DATE ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   BLD30380            NOT FOUND                                    
         ST    R6,AEFFDATE         SAVE A(EFF DATE ELEMENT)                     
         USING RBUYDTCD,R6                                                      
         TM    RBUYDTIN,X'40'      ALTERNATE WEEK INDIC SET?                    
         BNO   BLD30180            NO                                           
         MVI   FIRSTSW,1           YES - SET INDICATOR                          
*                                                                               
         DROP  R6                                                               
*                                                                               
*   LOOP THROUGH MULTIPLE EFFECTIVE DATES                                       
*                                                                               
BLD30180 EQU   *                                                                
         L     R6,AEFFDATE         RESET A(EFFECTIVE DATE ELEMENT)              
         ZIC   RF,1(R6)            L(EFFECTIVE DATE ELEMENT)                    
         BCTR  RF,0                DECREMENT 1                                  
         EX    RF,BLD30200         MOVE EFF DATE TO TABLE                       
         B     BLD30220                                                         
*                                                                               
BLD30200 MVC   BLDTABLE(0),0(R6)                                                
*                                                                               
BLD30220 EQU   *                                                                
         CLI   COLLAPSE,X'FF'      ANY MORE '03' ELEMENTS?                      
         BE    BLD30380            NO                                           
         GOTO1 DTEINFO,DMCB,(RC),(R6)                                           
         MVC   E2NRWK,FOXZEROS     SET # SPOTS TO ZERO                          
         CLI   SPOTSWK,0           ANY SPOTS IN THIS WEEK?                      
         BZ    BLD30360            NO  - DON'T PROCESS BUY                      
         EDIT  ORIGSPTS,(2,E2NRWK),FILL=0                                       
*                                  INSERT NUMBER PER WEEK                       
*                                  USE SPOTS FROM EFF DATE ELT,                 
*                                        NOT BUYLINE: MAY DIFFER                
         CLI   DAYCNT+3,1          SINGLE DAY IN CALENDAR?                      
         BNE   BLD30230            NO  - LEAVE AS IS                            
         OC    DYCNTFLG,DYCNTFLG   ALREADY DONE?                                
         BNZ   BLD30230            YES - DON'T DO IT AGAIN                      
         BAS   RE,CLNDRSPT         CHECK CALENDAR DAYS                          
BLD30230 EQU   *                                                                
         MVI   E2FLITE,C'1'        SET WEEK INDICATOR TO EVERY                  
         CLI   FIRSTSW,0           ALTERNATE WEEKS?                             
         BZ    BLD30240            NO                                           
         MVI   E2FLITE,C'2'        SET WEEK INDICATOR TO ALTERNATE              
BLD30240 EQU   *                                                                
         ZIC   RF,MULTEFDT                                                      
         LA    RF,1(RF)            ADD TO # EFF DATES                           
         STC   RF,MULTEFDT         SAVE IT BACK                                 
         L     RF,SVBYLN#          BUMP BUYLINE COUNT                           
         LA    RF,1(RF)                                                         
         ST    RF,SVBYLN#          PUT IT BACK                                  
         EDIT  SVBYLN#,(3,E2BYLN),FILL=0,ZERO=NOBLANK                           
*                                  INSERT NEW BUYLINE#                          
*                                                                               
         GOTO1 PRINTREC,DMCB,(RC)                                               
*                                                                               
BLD30360 EQU   *                                                                
         XC    BLDTABLE,BLDTABLE                                                
         MVC   E2STDT(14),FOXZEROS CLEAR DATES                                  
         MVI   FIRSTSW,0           CLEAR FIRSTSW                                
         B     BLD30180            GET NEXT EFF DATE ELEMENT                    
BLD30380 EQU   *                                                                
         OC    ZEROSPTS,ZEROSPTS   ZERO SPOTS PER WEEK?                         
         BZ    BLD30400            NO  - CONTINUE                               
         CLI   MULTEFDT,2          IF ZERO SPOTS AND ONLY 1                     
         BL    BLD30020               EFF DATE, DON'T SEND COMMENTS             
BLD30400 EQU   *                                                                
         OC    COMFLAGS,COMFLAGS   ANY COMMENTS EXPECTED?                       
         BZ    BLD30420            NO  -                                        
         GOTO1 BLD04REC,DMCB,(RC)  YES - ADD CMMTS/MG AS APPROP.                
         MVI   E3RTYP,C'M'         RESET TYPE TO 'MAIN'                         
BLD30420 EQU   *                                                                
         B     BLD30020            ACCESS NEXT BUY RECORD                       
BLD30460 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   RETRIEVE AUXILIARY DATA, SET MG AND/OR COMMENT FLAG.                        
*        SET MULTI D/T STRING FLAGS IF PATTERN RECORDS NEEDED.                  
*                                                                               
BLD3MKGD NTR1                                                                   
         XC    DTSTRNGS,DTSTRNGS                                                
         XC    COMFLAGS,COMFLAGS                                                
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,X'21'        LOOK FOR PROGRAM NAME ELT                    
         BAS   RE,GETEL                                                         
         BNE   BMKG0005            NO PROGRAM NAME ELT FOUND                    
         OI    COMFLAGS,X'40'      FOUND - SET COMMENT FLAG                     
BMKG0005 EQU   *                                                                
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,X'84'        LOOK FOR ORD COMMENTS                        
         BAS   RE,GETEL                                                         
         BNE   BMKG0010            NO ORDER COMMENTS FOUND                      
         OI    COMFLAGS,X'40'      FOUND - SET COMMENT FLAG                     
BMKG0010 EQU   *                                                                
         LA    R6,RBUYREC          RESET A(BUY REC)                             
         MVI   ELCODE,X'04'        LOOK FOR CONTRACT COMMENT                    
         BAS   RE,GETEL                                                         
         BNE   BMKG0020            NO CONTRACT COMMENT FOUND                    
         OI    COMFLAGS,X'40'      FOUND - SET COMMENT FLAG                     
BMKG0020 EQU   *                                                                
         LA    R6,RBUYREC          RESET A(BUY REC)                             
         MVI   ELCODE,X'05'        LOOK FOR MAKEGOOD ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   BMKG0030            NO MAKEGOOD ELEMENT FOUND                    
         OI    COMFLAGS,X'80'      FOUND - SET MG FLAG                          
         MVC   CNVDATE,3(R6)       SAVE MAKEGOOD START DATE                     
*                                                                               
*   IS THIS SUPPOSED TO BE MAKEGOOD START DATE?????                             
*                                                                               
BMKG0030 EQU   *                                                                
         LA    R6,RBUYREC          RESET A(BUY REC)                             
         MVI   ELCODE,X'02'        COUNT EFFECTIVE D/T STRINGS                  
         BAS   RE,GETEL                                                         
         BNE   BMKG0060            NOT FOUND - DONE                             
         B     BMKG0050                                                         
BMKG0040 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT D/T STRING                          
         BNE   BMKG0060            NOT FOUND - DONE                             
BMKG0050 EQU   *                                                                
         ZIC   RF,DTSTRNGS                                                      
         LA    RF,1(RF)            INCREMENT                                    
         STC   RF,DTSTRNGS                                                      
         CLI   DTSTRNGS,1          MORE THAN ONE DAY/TIME STRING?               
         BNH   BMKG0040            NO  - LOOK FOR ANOTHER                       
BMKG0060 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   THIS ROUTINE BUILDS THE 'E3RTYP' COMMENT RECORD                             
*                                                                               
*   NOTE:  THERE MUST BE COMMENTS TO PRODUCE A COMMENT RECORD                   
*                                                                               
BLD04REC NTR1                                                                   
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,X'21'        LOOK FOR PROGRAM NAME                        
BLD40020 EQU   *                                                                
         BAS   RE,GETEL            GET FIRST ELEMENT                            
         B     BLD40060                                                         
BLD40040 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
BLD40060 EQU   *                                                                
         BNE   BLD40120            NOT FOUND:  COMMENT TYPE DONE                
         MVI   E3RTYP,C' '         SPACE FILL THE RECORD                        
         MVC   E3RTYP+1(127),E3RTYP                                             
         MVI   E3RTYP,C'C'         INDICATE COMMENT RECORD                      
         MVI   E3COMTYP,C'M'       INDICATE COMMENT TYPE                        
         MVC   WORK(10),FOXZEROS                                                
         GOTO1 HEXOUT,DMCB,RCONKCON,WORK+2,4,=C'TOG'                            
         MVC   E3RREF,WORK         INSERT CONTRACT NUMBER                       
         EDIT  SVBYLN#,(3,E3BYLN),FILL=0,ZERO=NOBLANK                           
*                                  INSERT NEW BUYLINE#                          
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT L(CONTROL) + 1                      
         CLI   ELCODE,X'84'        ORDER COMMENT?                               
         BNE   BLD40080            NO                                           
         BCTR  RF,0                YES - SUBTRACT 1 FOR                         
*                                     STATION/REP COMMENT INDICATOR             
         EX    RF,BLD40900         MOVE 1ST COMMENT (BUY)                       
         B     BLD40100                                                         
BLD40080 EQU   *                                                                
         EX    RF,BLD40920         MOVE 1ST COMMENT (ORDER)                     
BLD40100 EQU   *                                                                
         LA    R2,1(R2)            INCREMENT COMMENT COUNTER                    
*                                                                               
         MVC   E3STAT#,RCONKSTA    INSERT STATION CALL LETTERS                  
         CLI   RCONKSTA+4,C'F'     FM STATION?                                  
         BE    BLD40105            YES                                          
         CLI   RCONKSTA+4,C'A'     AM STATION?                                  
         BNE   BLD40106            YES                                          
BLD40105 EQU   *                                                                
         MVC   E3STAT#+0(1),RCONKSTA+4                                          
***      MVC   E3STAT#+3(1),RCONKSTA+4                                          
*                                  OVERLAY 4TH POSITION WITH A/F                
BLD40106 EQU   *                                                                
*        CLC   RCONKSTA,=C'WICU '  SPECIAL STATION?                             
*        BE    BLD40110            YES - USE IT                                 
*        CLC   RCONKSTA,=C'WFXP '  SPECIAL STATION?                             
*        BE    BLD40110            YES - USE IT                                 
*        MVC   E3STAT#,FOXZEROS    INSERT STATION #                             
BLD40110 EQU   *                                                                
         MVC   E3REP,RUNREP        INSERT SOURCE/REP HOUSE                      
         EDIT  RCONMOD,(1,E3RREV),ZERO=NOBLANK                                  
*                                  INSERT REVISION NUMBER                       
         GOTO1 PRINTREC,DMCB,(RC)                                               
*                                                                               
         B     BLD40040            GO BACK FOR NEXT ELEMENT                     
*                                                                               
BLD40120 EQU   *                                                                
         CLI   ELCODE,X'04'        BUY COMMENTS DONE?                           
         BE    BLD40200            YES - COMMENTS FINISHED                      
         CLI   ELCODE,X'21'        PROGRAM NAME DONE?                           
         BNE   BLD40160            NO  - MUST BE ORDER COMMENTS                 
         LA    R6,RBUYREC          YES - RESET A(BUY RECORD)                    
         MVI   ELCODE,X'84'        GO BACK AND DO ORDER COMMENTS                
         B     BLD40020                                                         
BLD40160 EQU   *                                                                
         LA    R6,RBUYREC          NO  - RESET A(BUY RECORD)                    
         MVI   ELCODE,X'04'        GO BACK AND DO THEM                          
         B     BLD40020                                                         
BLD40200 EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
BLD40900 MVC   E3COMM(0),3(R6)                                                  
BLD40920 MVC   E3COMM(0),2(R6)                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
*   THIS ROUTINE BUILDS THE 'E4RTYP' CONTRACT ENDING RECORD                     
*                                                                               
*                                                                               
BLD05REC NTR1                                                                   
         MVI   E4RTYP,C' '         SPACE FILL THE RECORD                        
         MVC   E4RTYP+1(127),E4RTYP                                             
         MVC   WORK(10),FOXZEROS                                                
         MVI   E4RTYP,C'E'         INSERT RECORD TYPE                           
         GOTO1 HEXOUT,DMCB,RCONKCON,WORK+2,4,=C'TOG'                            
         MVC   E4RREF,WORK         INSERT CONTRACT NUMBER                       
         MVC   E4STAT#,RCONKSTA    INSERT STATION CALL LETTERS                  
         CLI   RCONKSTA+4,C'F'     FM STATION?                                  
         BE    BLD50020            YES                                          
         CLI   RCONKSTA+4,C'A'     AM STATION?                                  
         BNE   BLD50030            YES                                          
BLD50020 EQU   *                                                                
         MVC   E4STAT#+0(1),RCONKSTA+4                                          
***      MVC   E4STAT#+3(1),RCONKSTA+4                                          
*                                  OVERLAY 4TH POSITION WITH A/F                
BLD50030 EQU   *                                                                
*        CLC   RCONKSTA,=C'WICU '  SPECIAL STATION?                             
*        BE    BLD50100            YES - USE IT                                 
*        CLC   RCONKSTA,=C'WFXP '  SPECIAL STATION?                             
*        BE    BLD50100            YES - USE IT                                 
*        MVC   E4STAT#,FOXZEROS    INSERT STATION NUMBER                        
BLD50100 EQU   *                                                                
         MVC   E4REP,RUNREP        INSERT SOURCE/REP HOUSE                      
         ZIC   R1,TRANSCNT         INSERT RECORD COUNT:                         
*                                     CONTROL RECORD NOT INCLUDED               
         EDIT  (R1),(2,E4RECCT),FILL=0                                          
*                                                                               
         GOTO1 PRINTREC,DMCB,(RC)                                               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
*        INIT --- SET INITAL ADDRS AND VALUES                                   
*********************************************************************           
INIT     NTR1                                                                   
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
*                                                                               
         GOTO1 =A(RESOLVE),DMCB,(RC),RR=YES                                     
*                                                                               
         LA    RE,ADCONS           RELOCATE A-TYPES                             
         LA    RF,OUTDAY                                                        
         LA    R0,NADCONS                                                       
INIT10   EQU   *                                                                
         L     R1,0(RE)                                                         
         A     R1,RELX                                                          
         ST    R1,0(RF)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,INIT10                                                        
*                                                                               
         MVC   AREGENSC,VREGENSC   SAVE A(STANDARD COMMENT RTN)                 
         XC    TRANSCNT,TRANSCNT   CLEAR ACCUMULATORS                           
         XC    TOTALAMT,TOTALAMT                                                
         XC    TLSPOTS,TLSPOTS                                                  
         XC    EXTRAKEY,EXTRAKEY                                                
*                                                                               
         MVI   NOPQCLOS,0          ASSUME WE HAVE OUTPUT                        
         MVI   FOXZEROS,X'F0'                                                   
         MVC   FOXZEROS+1(L'FOXZEROS-1),FOXZEROS                                
         XC    LATREND,LATREND     SET EARLIEST END DATE                        
         MVC   ERLYSTRT(3),=X'FFFFFF'                                           
*                                  SET LATEST START DATE                        
         MVI   INTTYPE,C'E'        SET INPUT TYPE TO 'EC'                       
         LA    RF,SPOOLEND-SPOOLD  CLEAR THE SPOOL AREA                         
         LA    RE,SPOOLD                                                        
         XCEF                                                                   
         MVI   SPACES,C' '         INITIALIZE SPACES FIELD                      
         MVC   SPACES+1(131),SPACES                                             
         MVC   SPOOLDM,DATAMGR     INITIALIZE SPOOLER DATAMGR                   
         L     RF,AFACILS          A(FACILITIES LIST)                           
         LM    R2,R4,8(RF)         A(TERMINAL INPUT AREA??)                     
         ST    R3,ATIA                                                          
         MVC   SPOOLBUF,ATIA                                                    
         MVC   SCANNER(16),24(R4)                                               
         XC    VPRINT,VPRINT                                                    
         MVC   RCDATCON,DATCON     SET A(DATCON)                                
         MVC   RCCOMFAC,ACOMFACS                                                
         MVI   DMOUTBTS,X'7D'                                                   
         MVI   DMFILE,C'R'                                                      
         XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
*        GETCON --- READ CONTRACT RECORD                                        
*********************************************************************           
GETCON   NTR1                                                                   
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR                                               
         DROP  RF                                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
         MVC   RUNREP,=C'BLAR'     SET TO 'BLAIR'                               
         CLC   RCONKREP,=C'BL'     BLAIR?                                       
         BE    GCON0020            YES                                          
         MVC   RUNREP,=C'FXFX'     SET TO 'FOX'                                 
         CLC   RCONKREP,=C'FB'     FOX?                                         
         BE    GCON0020            YES                                          
         MVC   RUNREP,=C'KATZ'     SET TO 'KATZ '                               
         CLC   RCONKREP,=C'AM'     AMERICAN?                                    
         BE    GCON0020            YES                                          
         CLC   RCONKREP,=C'CQ'     CONTINENTAL?                                 
         BE    GCON0020            YES                                          
         CLC   RCONKREP,=C'NK'     NATIONAL?                                    
         BE    GCON0020            YES                                          
         CLC   RCONKREP,=C'CR'     CHRISTAL?                                    
         BE    GCON0020            YES                                          
         MVC   RUNREP,=C'PTRY'     SET TO 'PTRY '                               
         CLC   RCONKREP,=C'PV'     PETRY?                                       
         BE    GCON0020            YES                                          
         MVC   RUNREP,=C'TELE'     SET TO 'TELE '                               
         CLC   RCONKREP,=C'B1'     TELEMUNDO?                                   
         BE    GCON0020            YES                                          
         MVC   RUNREP,=C'SELT'     SET TO 'SELT '                               
         CLC   RCONKREP,=C'SZ'     SELTEL?                                      
         BE    GCON0020            YES                                          
         MVC   RUNREP,=C'TEST'     SET TO 'TEST '                               
*                                                                               
*   WILL SET TO 'TEST' IF UNRECOGNIZED!!!                                       
*                                                                               
GCON0020 EQU   *                                                                
*                                                                               
*   RETRIEVE SUPPORT INFORMATION FROM VARIOUS RECORDS                           
*                                                                               
         XC    RREPKEY,RREPKEY                                                  
         MVI   RREPKEY,X'01'       REP RECORD                                   
         MVC   RREPKREP,RCONKREP   INSERT REP CODE                              
         MVC   KEY,RREPKEY                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    GCON0040                                                         
         DC    H'0',C'MISSING REP    RECORD'                                    
         DS    0H                                                               
GCON0040 EQU   *                                                                
         GOTO1 VGETREC,DMCB,RREPREC                                             
         MVC   REPNAME,RREPABBR    SAVE REP ABBREV NAME                         
*                                                                               
         XC    RAGYKEY,RAGYKEY                                                  
         MVI   RAGYKEY,X'0A'       AGENCY RECORD                                
         MVC   RAGYKAGY(6),RCONKAGY                                             
*                                  INSERT AGENCY/AGY OFF CODES                  
         MVC   RAGYKREP,RCONKREP   INSERT REP CODE                              
         MVC   KEY,RAGYKEY                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    GCON0060                                                         
         DC    H'0',C'MISSING AGENCY RECORD'                                    
         DS    0H                                                               
GCON0060 EQU   *                                                                
         GOTO1 VGETREC,DMCB,RAGYREC                                             
         MVC   AGYNAME,RAGYNAM2    SAVE CONTRACT AGENCY NAME                    
         MVC   ZIPCODE,RAGYZIP     SAVE ZIP CODE                                
*                                                                               
         XC    RADVKEY,RADVKEY                                                  
         MVI   RADVKEY,X'08'       ADVERT RECORD                                
         MVC   RADVKADV,RCONKADV   INSERT ADVERTISER CODE                       
         MVC   RADVKREP,RCONKREP   INSERT REP CODE                              
         MVC   KEY,RADVKEY                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    GCON0080                                                         
         DC    H'0',C'MISSING ADVERT RECORD'                                    
         DS    0H                                                               
GCON0080 EQU   *                                                                
         GOTO1 VGETREC,DMCB,RADVREC                                             
         MVC   ADVNAME,RADVNAME    SAVE ADVERT NAME                             
*                                                                               
         XC    RSALKEY,RSALKEY                                                  
         MVI   RSALKEY,X'06'       S/P    RECORD                                
         MVC   RSALKSAL,RCONSAL    INSERT SALESPERSON                           
         MVC   RSALKREP,RCONKREP   INSERT REP CODE                              
         MVC   KEY,RSALKEY                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    GCON0100                                                         
         DC    H'0',C'MISSING S/P    RECORD'                                    
         DS    0H                                                               
GCON0100 EQU   *                                                                
         GOTO1 VGETREC,DMCB,RSALREC                                             
         MVC   SALNAME,RSALNAME    SAVE S/P    NAME                             
*                                                                               
         XC    ROFFKEY,ROFFKEY                                                  
         MVI   ROFFKEY,X'04'       OFFICE RECORD                                
         MVC   ROFFKOFF,RCONKOFF   INSERT OFFICE CODE                           
         MVC   ROFFKREP,RCONKREP   INSERT REP CODE                              
         MVC   KEY,ROFFKEY                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    GCON0120                                                         
         DC    H'0',C'MISSING OFFICE RECORD'                                    
         DS    0H                                                               
GCON0120 EQU   *                                                                
         GOTO1 VGETREC,DMCB,ROFFREC                                             
         MVC   OFFNAME,ROFFNAME    SAVE OFFICE NAME                             
*                                                                               
         CLC   RCONPRD,SPACES      ANY PRODUCT CODE?                            
         BNE   GCON0160            YES - GET RECORD                             
         LA    R6,RCONREC          NO  - RETRIEVE X'05' ELEMENT                 
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BE    GCON0140                                                         
         DC    H'0'                NO PRODUCT CODE NAME ELEMENT                 
GCON0140 EQU   *                                                                
         MVC   PRODNAME,2(R6)      LOAD PRODUCT NAME                            
*                                     FIXED LEN ELEMENT OF 20 CHARS             
         B     GCON0200            EXIT                                         
GCON0160 EQU   *                                                                
         XC    RPRDKEY,RPRDKEY                                                  
         MVI   RPRDKEY,X'09'       OFFICE RECORD                                
         MVC   RPRDKADV,RCONKADV   INSERT ADVERTISER                            
         MVC   RPRDKPRD,RCONPRD    INSERT PRODUCT CODE                          
         MVC   RPRDKREP,RCONKREP   INSERT REP CODE                              
         MVC   KEY,RPRDKEY                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    GCON0180                                                         
         DC    H'0',C'MISSING PRODUCT RECORD'                                   
         DS    0H                                                               
GCON0180 EQU   *                                                                
         GOTO1 VGETREC,DMCB,RPRDREC                                             
         MVC   PRODNAME,RPRDNAME   SAVE PRODUCT NAME                            
*                                                                               
GCON0200 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
PRINTREC NTR1                                                                   
         XC    LINE,LINE           NEVER FORCE A HEADLINE                       
         ZIC   RF,TRANSCNT         COUNT # OF TRANSACTION RECORDS               
         LA    RF,1(RF)                                                         
         STC   RF,TRANSCNT         SAVE COUNT                                   
*                                                                               
PRC20020 EQU   *                                                                
         LA    R4,EORTYP           A(OUTPUT RECORD)                             
PRC20040 EQU   *                                                                
         CLC   =C'DOWNLOADER',RCONBUYR                                          
*                                  FORMAT OUTPUT AS DOWNLOADABLE?               
         BE    PRC20060            YES                                          
*                                  NO  - STANDARD OUTPUT                        
         MVC   P(128),0(R4)                                                     
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRC20050 EQU   *                                                                
         XIT1                                                                   
PRC20060 EQU   *                                                                
         MVC   P+1(128),0(R4)                                                   
         MVI   P,C'"'              INSERT TEXT DOWNLOAD INDS                    
         MVI   P+129,C'"'                                                       
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         B     PRC20050            DONE                                         
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
READBUY  NTR1                                                                   
         MVC   KEY,EXTRAKEY        INITIALIZE OR RESET KEY                      
         OC    EXTRAKEY,EXTRAKEY   ANY PRIOR KEY?                               
         BNZ   REBU0020            YES - DO SEQ READ                            
         LA    RF,KEY                                                           
         USING RBUYREC,RF                                                       
         MVI   RBUYKEY,X'0B'       INSERT ID                                    
         MVC   RBUYKREP,REPALPHA                                                
         LR    RE,RA                                                            
         AH    RE,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RE                                                       
         MVC   RBUYKCON,TWACNUM                                                 
         DROP  RE,RF                                                            
         GOTO1 VHIGH                                                            
         B     REBU0040                                                         
REBU0020 EQU   *                                                                
         GOTO1 VSEQ                                                             
REBU0040 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     COMPARE THROUGH CONTRACT #                   
         BE    REBU0060            BUY FOUND                                    
         SR    R0,R0               NO MORE BUYS:  CC = ZERO                     
         LTR   R0,R0                                                            
         B     REBU0080            GO BACK                                      
REBU0060 EQU   *                                                                
         MVC   EXTRAKEY(27),KEY    SAVE KEY FOUND                               
         MVI   ION,2               SET IO2 AREA AS INPUT                        
         GOTO1 VGETREC,DMCB,RBUYREC                                             
         LTR   RB,RB               BUY FOUND:  CC NOT = ZERO                    
REBU0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   TRANSLATE DAY AND TIME TO JDS FORMAT.                                       
*                                                                               
DYTIME   NTR1                                                                   
         XC    DAYCNT,DAYCNT       CLEAR DAY COUNT                              
         XC    DYCNTFLG,DYCNTFLG   CLEAR DAY COUNT SET FLAG                     
         LA    R6,RBUYREC          A(BUY RECORD)                                
         MVI   ELCODE,2            LOOK FOR DAY/TIME ELEMENT                    
         BAS   RE,GETEL            GET FIRST DAY/TIME ELEMENT                   
         BNE   DYTI0280            NOT FOUND - EXIT                             
         CLI   DTSTRNGS,2          2 OR MORE D/T STRINGS?                       
         BL    DYTI0040            NO  - ONLY 1                                 
         MVC   E2FILL(15),=C'MULTI DAY/TIMES'                                   
*                                  NO CALENDAR FIELDS WILL BE SET               
         MVC   E2DESC(14),SPACES   CLEAR ROTATOR FIELD                          
         MVC   E2DESC(2),=C'*P'    INSERT 'PATTERN NEEDED'                      
*                                     *P NOT BEING USED                         
         MVC   E2STTM(8),FOXZEROS  SET START/END TIME TO ZERO                   
         B     DYTI0080                                                         
DYTI0040 EQU   *                                                                
*                                                                               
         GOTO1 CALNDRFL,DMCB,(RC)                                               
*                                  FILL CALENDAR FIELDS                         
DYTI0080 EQU   *                                                                
         MVC   E2LLEN,FOXZEROS     INITIALIZE LENGTH OF SPOT                    
         MVC   E2SPONSR,FOXZEROS   INITIALIZE LENGTH OF SPONSORSHIP             
         TM    RBUYDUR,X'80'       BUY IN MINUTES?                              
         BO    DYTI0120            YES                                          
         EDIT  RBUYDUR,(3,E2LLEN),FILL=0,ZERO=NOBLANK                           
*                                  NO  - IN SECONDS                             
         B     DYTI0240                                                         
DYTI0120 EQU   *                   SPOT LENGTH IN MINUTES                       
         ZICM  R1,RBUYDUR,2        LOAD SPOT LENGTH                             
         SLL   R1,17               DROP HIGH BIT, BYTE 3                        
         SRL   R1,17               MOVE BACK TO ORIGINAL POS                    
         SR    R0,R0                                                            
         M     R0,=F'60'           MINUTES * 60 = TOTAL SECONDS                 
         EDIT  (R1),(3,E2LLEN),FILL=0,ZERO=NOBLANK                              
*                                  INSERT CALCULATED SECONDS                    
DYTI0240 EQU   *                                                                
         MVC   E2STTM(08),FOXZEROS LOAD TIMES WITH X'F0'                        
         CLC   =C'VAR',4(R6)       'VARIOUS' TIMES?                             
         BE    DYTI0245            YES                                          
         CLC   =C'NON',4(R6)       'NONE' TIMES?                                
         BNE   DYTI0247            NO                                           
DYTI0245 EQU   *                                                                
         MVC   E2STTM,=C'0000'     SET START TIME TO ZERO                       
         MVC   E2ENTM,=C'9999'     SET END TIME TO 9999                         
         B     DYTI0280            EXIT                                         
DYTI0247 EQU   *                                                                
         EDIT  (2,4(R6)),(4,E2STTM),FILL=0,ZERO=NOBLANK                         
*                                  INSERT START TIME                            
         CLC   =C'CC',6(R6)        END TIME = CC?                               
         BNE   DYTI0250            NO                                           
         MVC   E2ENTM,=C'9999'     YES - FILL END TIME WITH 'F9'                
         B     DYTI0260                                                         
DYTI0250 EQU   *                                                                
         EDIT  (2,6(R6)),(4,E2ENTM),FILL=0,ZERO=NOBLANK                         
*                                  INSERT END   TIME                            
DYTI0260 EQU   *                                                                
         CLC   =C'2400',E2STTM     MIDNIGHT?                                    
         BE    DYTI0270            YES - LEAVE AS IS                            
         CLC   =C'24',E2STTM       NO  - BEGIN TIME = 24?                       
         BNE   DYTI0270            NO                                           
         MVC   E2STTM(2),=C'00'    YES - SET TO 0000                            
DYTI0270 EQU   *                                                                
         CLC   =C'2400',E2ENTM     MIDNIGHT?                                    
         BE    DYTI0280            YES - LEAVE AS IS                            
         CLC   =C'24',E2ENTM       NO  - START TIME = 24?                       
         BNE   DYTI0280            NO                                           
         MVC   E2ENTM(2),=C'00'    YES  - SET TO 0000                           
DYTI0280 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*    GET CALENDAR DAYS FROM BITS 8-15 OF 02 ELEMENT                             
*                                                                               
CALNDRFL NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,2            LOOK FOR DAY/TIME ELEMENT                    
         BAS   RE,GETEL            GET D/T ELEMENT                              
         BNE   CAXX0100            NOT FOUND - EXIT                             
         LA    RF,E2DESC           A(CALENDAR)                                  
         ZIC   RE,3(R6)            GET DAYS BYTE IN REG                         
         SLL   RE,25               SHIFT DAYS BYTE TO HI-ORDER                  
*                                     DROP 'SPARE' BIT                          
         LA    R0,7                LOOP CONTROL                                 
CALN0060 EQU   *                                                                
         LTR   RE,RE               SET CONDITION CODES FOR REG                  
         BNM   CALN0080            NOT MINUS = BIT NOT SET                      
         MVC   0(2,RF),=C'XX'      MINUS = BIT SET: SET CALENDAR                
         L     R1,DAYCNT           BUMP DAY COUNTER                             
         LA    R1,1(R1)                                                         
         ST    R1,DAYCNT           STORE IT BACK                                
CALN0080 EQU   *                                                                
         LA    RF,2(RF)            BUMP TO NEXT CALENDAR POSITION               
         SLL   RE,1                SHIFT BITS UP 1                              
         BCT   R0,CALN0060         TEST NEXT BIT                                
CAXX0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CLNDRSPT:  IF SINGLE DAY IN CALENDAR, REPLACE INDICATOR IN                  
*      CALENDAR WITH NUMBER OF SPOTS.                                           
*                                                                               
CLNDRSPT NTR1                                                                   
         LA    R2,E2DESC           YES - SET DAY TO # SPOTS                     
         LA    R0,7                                                             
CLND0100 EQU   *                                                                
         CLC   0(2,R2),=C'XX'      DAY SET IN CALENDAR?                         
         BE    CLND0120            YES - CONVERT TO # SPOTS                     
         LA    R2,2(R2)            NO  - BUMP TO NEXT POSITION                  
         BCT   R0,CLND0100         GO BACK FOR NEXT                             
         DC    H'0'                SHOULDN'T HAPPEN, EVER!                      
CLND0120 EQU   *                                                                
         EDIT  ORIGSPTS,(2,0(R2)),FILL=0                                        
         MVI   DYCNTFLG,1          SET DAY COUNT SET FLAG = NON-ZERO            
         XIT1                                                                   
         EJECT                                                                  
                                                                                
*                                                                               
*    GET BUY DATE INFORMATION                                                   
*                                                                               
DTEINFO  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R6,4(R1)            RESET A(EFF DATE ELEMENT)                    
         USING RBUYDTEL,R6                                                      
         MVC   STARTDTE,RBUYDTST   SAVE START DATE                              
         MVC   NOFWKS,RBUYDTWK     SET NUMBER OF WEEKS                          
         MVC   SPOTSWK,RBUYDTNW    SET NUMBER SPOTS/WEEK                        
         SR    RE,RE                                                            
         ZIC   RF,NOFWKS           CALCULATE TOTAL SPOTS                        
         ZIC   R1,SPOTSWK                                                       
         MR    RE,R1               # WEEKS X SPOTS/WK = TOTAL SPOTS             
         L     RE,TLSPOTS                                                       
         AR    RE,RF                                                            
         ST    RE,TLSPOTS          SAVE TOTAL SPOTS                             
         SR    R0,R0               USE SPOTS TO CALCULATE $$                    
         MVC   FULL,RBUYCOS        RATE FOR LINE                                
         L     R1,FULL             TOTAL SPOTS (THIS ELT) * RATE =              
         MR    R0,RF                  TOTAL $$ FOR EFF DATE ELT                 
         L     RF,TOTALAMT         ADD TO TOTAL AMOUNT                          
         AR    RF,R1                                                            
         ST    RF,TOTALAMT                                                      
         XC    COLLAPSE,COLLAPSE   CLEAR COLLAPSE FLAG                          
         GOTO1 CHKDATE,DMCB,(RC),(R6)                                           
*                                                                               
         MVI   NOFWKS,0            CLEAR NUMBER OF WEEKS                        
*                                                                               
*   SHIFT 'USING' FROM EFFECTIVE DATE ELEMENT TO BLDTABLE AREA,                 
*      WHERE A 'REVISED/COLLAPSED' EFFECTIVE DATE ELEMENT MAY HAVE              
*      BEEN BUILT.  IF IT HASN'T, IT STILL LOOKS LIKE THE ONE PASSED            
*      IN AT THE ORIGINAL CALL.                                                 
*                                                                               
         LA    R6,BLDTABLE                                                      
*                                                                               
         TM    BLDTABLE+8,X'40'    ALTERNATE WEEK?                              
         BNO   DINF0020            NO                                           
         MVI   FIRSTSW,1           YES - SET INDICATOR                          
DINF0020 EQU   *                                                                
         MVC   ENDDTE,RBUYDTED     FORCE END DATE IN                            
         OC    RBUYDTED,RBUYDTED   ANY END DATE?                                
         BNZ   DINF0070            YES - END DATE EXISTS                        
*                                     USE FORCED DATE                           
         MVC   HOLDDATE(3),CKSTRDTE                                             
*                                  TEMPORARY HOLD AREA                          
         ZIC   R0,RBUYSTED         GET START/END DAY OF WEEK                    
*                                  START DAY  STAYS IN R0                       
         SRDL  R0,4                SHIFT END DATE INTO R1                       
         SRL   R1,28               SHIFT END DATE TO LOW-ORDER                  
         SR    R1,R0               SUBTRACT START FROM END                      
         BZ    DINF0060            SAME DAY:  NO ADJUSTMENT                     
         BP    DINF0040                                                         
         LA    R1,7(R1)            NEGATIVE:  MAKE POSITIVE                     
DINF0040 EQU   *                                                                
         ST    R1,DAYBUMP          SET DAYS TO ADD                              
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,FRSTDATE)                            
*                                  CONVERT DATE: YMD BIN -> YYMMDD              
         L     RF,DAYBUMP                                                       
         GOTO1 ADDAY,DMCB,FRSTDATE,SECDATE,(RF)                                 
         GOTO1 DATCON,DMCB,(0,SECDATE),(3,FRSTDATE)                             
*                                  CONVERT DATE: YYMMDD -> YMD BIN              
DINF0060 EQU   *                                                                
         MVC   ENDDTE,FRSTDATE     SAVE NEW END DATE                            
DINF0070 EQU   *                                                                
         XC    DAYBUMP,DAYBUMP                                                  
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,FRSTDATE)                            
*                                  CONVERT DATE: YMD BIN -> YYMMDD              
DINF0080 EQU   *                                                                
         L     RF,DAYBUMP                                                       
         GOTO1 ADDAY,DMCB,FRSTDATE,SECDATE,(RF)                                 
         GOTO1 DATCON,DMCB,(0,SECDATE),(3,FRSTDATE)                             
*                                  CONVERT DATE: YYMMDD -> YMD BIN              
         CLC   ENDDTE(3),FRSTDATE                                               
*                                  END DATE REACHED?                            
         BL    DINF0120            YES                                          
         ZIC   RF,RBUYDTNW         ACCUM # SPOTS/WK                             
         ZIC   RF,NOFWKS           BUMP NUMBER OF WEEKS                         
         LA    RF,1(RF)                                                         
         STC   RF,NOFWKS                                                        
*                                                                               
*    I DON'T KNOW IF ABOVE ACCUMULATIONS ARE NECESSARY IN THIS                  
*        FASHION.  DURING TESTING, CHECK RESULTS OBTAINED.                      
*                                                                               
         LA    RF,7                                                             
         TM    RBUYDTIN,X'40'      ALTERNATING WEEK?                            
         BNO   DINF0100            NO                                           
         LA    RF,14               YES                                          
DINF0100 EQU   *                                                                
         ST    RF,DAYBUMP                                                       
*                                                                               
*   USE PREVIOUS 1ST DATE TO CALCULATE NEW FIRST DATE                           
         MVC   SECDATE(3),FRSTDATE                                              
         GOTO1 DATCON,DMCB,(3,SECDATE),(0,FRSTDATE)                             
*                                  CONVERT DATE: YMD BIN -> YYMMDD              
         B     DINF0080            GO BACK FOR NEXT                             
DINF0120 EQU   *                                                                
         MVC   E2STDT(14),FOXZEROS                                              
         XC    HOLDDATE,HOLDDATE                                                
*                                                                               
*   THE 'ERLYSTRT/LATREND' DATES ARE USED TO CALCULATE THE FLIGHT               
*      OF THE ORDER FROM THE BUY RECORD DATES.  THIS TECHNIQUE IS               
*      NOT USED IN COLUMBINE.  THE CODE REMAINS, BECAUSE IT HAS NO              
*      EFFECT.                                                                  
*                                                                               
         CLC   ERLYSTRT,STARTDTE   EARLIER START DATE?                          
         BL    DINF0140            NO                                           
         MVC   ERLYSTRT,STARTDTE   YES - SAVE IT                                
DINF0140 EQU   *                                                                
         CLC   ENDDTE,LATREND      LATER   END   DATE?                          
         BL    DINF0160            NO                                           
         MVC   LATREND,ENDDTE      YES - SAVE IT                                
DINF0160 EQU   *                                                                
*                                                                               
*    INSERT START DATE                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(3,STARTDTE),(X'20',WORK+1)                          
         BAS   RE,SETCNTRY         SET CENTURY INDICATOR                        
         MVC   E2STDT,WORK         INSERT BUY START DATE                        
*                                                                               
DINF0180 EQU   *                                                                
*                                                                               
*    INSERT END   DATE                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(3,ENDDTE),(X'20',WORK+1)                            
         BAS   RE,SETCNTRY         SET CENTURY INDICATOR                        
         MVC   E2ENDT,WORK         INSERT BUY END DATE                          
*                                                                               
DINF0200 EQU   *                                                                
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*   ROUTINE COLLAPSES EFFECTIVE DATE ELEMENTS, IF AT ALL POSSIBLE,              
*        TO ATTEMPT TO LIMIT AMOUNT OF LINES BEING TRANSMITTED.                 
*        LINES ARE CHECKED FOR CONTINUITY, EITHER SINGLE WEEK OR                
*        ALTERNATING WEEK.                                                      
*   REASON BEHIND THIS:  A USER WILL (FROM TIME TO TIME) ENTER A                
*        BUYLINE OF 10 WEEKS AS 10 UNIQUE EFFECTIVE DATES.  THIS                
*        WOULD GENERATE 1 LINE WITH 10 SUBLINES, AND COULD FLOOD                
*        THE BIAS SYSTEM WITH MORE LINES/SUBLINES THAN IT COULD                 
*        HANDLE.                                                                
*                                                                               
CHKDATE  NTR1                                                                   
         L     RC,0(R1)            RESET A (WORKSPACE)                          
         L     R6,4(R1)            A(1ST EFFECTIVE DATE ELT)                    
         USING RBUYDTEL,R6                                                      
         MVC   CKSTRDTE,STARTDTE   SAVE 1ST EFFECTIVE START DATE                
         MVC   ORIGSPTS,RBUYDTNW   SAVE SPOTS/WK FOR 1ST ELEMENT                
CHKD0020 EQU *                                                                  
         L     R6,AEFFDATE         A(EFF DATE ELEMENT IN PROGRESS)              
         BAS   RE,NEXTEL           GET NEXT 03 ELEMENT                          
         BNE   CHKD0120            NO MORE - EXIT                               
         ST    R6,AEFFDATE         SAVE A(EFF DATE ELEMENT)                     
         MVC   SVSTRDTE,RBUYDTST   SAVE EFFECT START DATE                       
         MVC   SVSPTSWK,RBUYDTNW   SAVE SPOTS PER WEEK                          
         CLC   SVSPTSWK,SPOTSWK    HAS THIS WEEK BEEN MADE GOOD?                
         BNE   CHKD0140            YES - CAN'T COLLAPSE WEEKS                   
*                                                                               
*   ABOVE TEST IS STRANGE:  ARE SPOTS/WEEK CHANGED IF LINE IS MADE              
*        GOOD?  DOES IT GET TO THE EFFECTIVE DATE ELEMENT, OR TOTAL             
*        SPOTS FOR LINE?  THIS MAY NOT BE A VALID TEST.....                     
*                                                                               
         CLC   ORIGSPTS,RBUYDTNW   SPOTS/WK:  THIS ELT SPOTS/WK SAME            
*                                     AS STARTING ELEMENT'S SPOTS/WK?           
         BNE   CHKD0140            NO  - CAN'T COLLAPSE THE ELEMENTS            
         CLC   NOFWKS,RBUYDTWK     NUMBER OF WEEKS SAME:                        
*                                     1ST EFF DATE VS THIS ONE                  
         BNE   CHKD0140            NO  - CAN'T COLLAPSE                         
         CLI   NOFWKS,1            NUMBER OF WEEKS = 1?                         
         BNE   CHKD0140            NO  - DON'T COLLAPSE                         
         CLI   FIRSTSW,0           ALTERNATE WEEK PATTERN?                      
         BNZ   CHKD0040            YES                                          
*                                                                               
* BUMP PREVIOUS EFF DATE ELEMENT START DATE                                     
*                                                                               
         GOTO1 DATCON,DMCB,(3,CKSTRDTE),(0,FRSTDATE)                            
*                                  CONVERT DATE: YMD BIN -> YYMMDD              
         LA    RF,7                LOOK FOR SEQUENTIAL                          
         ST    RF,DAYBUMP                                                       
         GOTO1 ADDAY,DMCB,FRSTDATE,SECDATE,(RF)                                 
         GOTO1 DATCON,DMCB,(0,SECDATE),(3,THIRDATE)                             
*                                  CONVERT DATE: YYMMDD -> YMD BIN              
         CLC   THIRDATE(3),SVSTRDTE                                             
*                                  SEQUENTIAL BUY WEEK?                         
         BE    CHKD0060            YES - SET SWITCH AND EXIT                    
         CLI   COLLAPSE,0          HAS THIS BUY BEEN SEQ COLLAPSED?             
         BNZ   CHKD0140            YES - NO LONGER SEQ, SO EXIT                 
CHKD0040 EQU   *                                                                
         LA    RF,14               LOOK FOR ALTERNATING                         
         ST    RF,DAYBUMP                                                       
         GOTO1 ADDAY,DMCB,FRSTDATE,SECDATE,(RF)                                 
         GOTO1 DATCON,DMCB,(0,SECDATE),(3,THIRDATE)                             
*                                  CONVERT DATE: YYMMDD -> YMD BIN              
         CLC   THIRDATE(3),SVSTRDTE                                             
*                                  ALTERNATING BUY WEEK?                        
         BNE   CHKD0140            NO  - NOT SEQ OR ALTERNATING                 
         MVI   FIRSTSW,1           SET ALTERNATING WEEK FLAG                    
*                                                                               
CHKD0060 EQU   *                                                                
         MVI   COLLAPSE,1          INDICATE COLLAPSE BUY                        
         MVC   CKSTRDTE,THIRDATE   SET TO NEW DATE                              
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         BCTR  RF,0                DECREMENT FOR EX                             
         EX    RF,CHKD0080         MOVE TO BLDTABLE                             
         B     CHKD0100                                                         
CHKD0080 MVC   BLDTABLE(0),0(R6)                                                
CHKD0100 EQU   *                                                                
         SR    RE,RE               CALCULATE NUMBER OF SPOTS                    
         ZIC   RF,NOFWKS           NUMBER OF WEEKS *                            
         ZIC   R1,SPOTSWK             SPOTS PER WEEK=                           
         MR    RE,R1                     SPOTS FOR EFF DATE ELT                 
         L     RE,TLSPOTS          ADD IT TO ACCUMULATOR                        
         AR    RE,RF                                                            
         ST    RE,TLSPOTS                                                       
         SR    R0,R0               USE SPOTS TO CALCULATE $$                    
         MVC   FULL,RBUYCOS        RATE FOR LINE                                
         L     R1,FULL             TOTAL SPOTS (THIS ELT) * RATE =              
         MR    R0,RF                  TOTAL $$ FOR EFF DATE ELT                 
         L     RF,TOTALAMT         ADD TO TOTAL AMOUNT                          
         AR    RF,R1                                                            
         ST    RF,TOTALAMT                                                      
         ZIC   RF,SVSPTSWK                                                      
         ZIC   R1,SPOTSWK                                                       
         AR    RF,R1                                                            
         STC   RF,SVSPTSWK         TOTAL SPOTS                                  
         B     CHKD0020        GO BACK FOR NEXT ELEMENT                         
CHKD0120 EQU   *                                                                
         MVI   COLLAPSE,X'FF'      SET COLLAPSE TO 'NO MORE'                    
CHKD0140 EQU   *                                                                
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*   FILL IN CONTRACT '15' ELEMENT WITH DATE AND TIME STAMP                      
*                                                                               
DATETIME NTR1                                                                   
         MVC   KEY,RCONREC         REESTABLISH CONTRACT RECORD                  
*                                     DUE TO INTERVENING BUY RECORDS            
         GOTO1 VHIGH                                                            
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RCONREC                                             
         XC    NEW15ELT,NEW15ELT   CLEAR DATE/TIME ELT AREA                     
         MVC   NEW15ELT(2),=X'1514'                                             
*                                  SET ELT TYPE (X'15'), LEN=20                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'15'        LOOK FOR EC CONTROL ELT                      
         BAS   RE,GETEL                                                         
         BNE   DTIM0020            NOT FOUND:  USE NEW ELT                      
         MVC   NEW15ELT,0(R6)      FOUND:  INSERT INTO ELT AREA                 
DTIM0020 EQU   *                                                                
         GOTO1 VDELELEM,DMCB,(X'15',RCONREC)                                    
*                                  DELETE ANY OLD X'15' ELEMENT(S)              
         LA    R6,NEW15ELT                                                      
         USING RCONECEL,R6                                                      
         GOTO1 DATCON,DMCB,(5,DUB),(2,RCONECDT)                                 
*                                  INSERT DATE INTO ELT                         
         ZICM  RF,RCONECCT,2       BUMP COUNTER                                 
         LA    RF,1(RF)                                                         
         STCM  RF,3,RCONECCT       PUT COUNTER BACK                             
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,12               SHIFT OFF SECONDS AND SIGN                   
         STCM  R1,3,RCONECTM                                                    
*                                                                               
         GOTO1 VADDELEM,DMCB,RCONREC,NEW15ELT                                   
*                                  ADD UPDATED X'15' ELEMENT                    
         GOTO1 VPUTREC,DMCB,RCONREC                                             
*                                  REWRITE UPDATED CONTRACT RECORD              
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*   SETCNTRY:  SET CENTURY FLAG OF DATE.  INPUT IS ALWAYS IN                    
*      WORK+1.  INDICATOR IS SET INTO WORK.                                     
*                                                                               
SETCNTRY NTR1                                                                   
         MVI   WORK,C'0'           SET CENTURY TO '2OTH'                        
         CLI   WORK+1,C'9'         20TH CENTURY?                                
         BE    SCEN0020            YES                                          
         MVI   WORK,C'1'           NO - SET TO '21ST'                           
SCEN0020 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   GETDEMO:  RETRIEVE THE DEMO FROM THE SAR ELEMENT.                           
*                                                                               
GETDEMO  NTR1                                                                   
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   GDEM0100                                                         
         USING RSARXEL,R6                                                       
         LA    R2,WORK                                                          
*        LA    R4,RCONREC+1100     USE IO2 AFTER 2K CONVERSION                  
         L     R4,AIO2                                                          
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         LA    R5,WORK3                                                         
         XC    WORK3(30),WORK3                                                  
         MVC   0(L'RSARXDEM,R5),RSARXDEM        DEMOS + ENDING ZERO             
         LA    R0,6                                                             
GDEM0020 CLI   1(R5),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R5),C'I'                                                       
         LA    R5,3(R5)                                                         
         BCT   R0,GDEM0020                                                      
         LA    R5,WORK3                                                         
*                                                                               
         GOTO1 DEMCON,DMCB,(6,(R5)),(9,0(R2)),(0,DBLOCKD)                       
         DROP  R4,R6                                                            
         LA    R4,EODEMO           A(DEMO LABEL OUTPUT)                         
         LA    R6,6                SET LOOP CONTROL                             
GDEM0040 EQU   *                                                                
         CLI   0(R2),C'W'          WOMEN?                                       
         BE    GDEM0060            YES                                          
         CLI   0(R2),C'V'          VIEWERS/KIDS,ETC?                            
         BE    GDEM0060            YES                                          
         CLI   0(R2),C'M'          MEN?                                         
         BNE   GDEM0045            NO                                           
         LA    RF,EODEMO           A(DEMO LABEL OUTPUT)                         
         CR    R4,RF               ANYTHING PUT IN OUTPUT YET?                  
         BE    GDEM0060            NO  - MUST BE 1ST CHARACTER                  
         LR    RF,R2               YES - WAS PRECEDING CHAR 'W'?                
         BCTR  RF,0                BACK UP 1 CHAR                               
         CLI   0(RF),C'W'                                                       
         BE    GDEM0080            YES - SKIP IT (WM)                           
         B     GDEM0060            NO  - ACCEPT IT AS 'MEN'                     
GDEM0045 EQU   *                                                                
         CLI   0(R2),C'A'          ADULT?                                       
         BE    GDEM0060            YES                                          
         CLI   0(R2),C'0'          AGE CHAR?                                    
         BNL   GDEM0050            YES                                          
         B     GDEM0080            NO  - SKIP IT                                
GDEM0050 EQU   *                                                                
         CLI   0(R2),C'9'          AGE CHAR?                                    
         BH    GDEM0080            NO                                           
GDEM0060 EQU   *                                                                
         MVC   0(1,R4),0(R2)       MOVE CHAR TO OUTPUT                          
         LA    R4,1(R4)            INCREMENT A(EODEMO)                          
GDEM0080 EQU   *                                                                
         LA    R2,1(R2)            INCREMENT A(DDS DEMO LABEL)                  
         BCT   R6,GDEM0040         GO BACK FOR NEXT                             
GDEM0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        ADDRESS                                                                
*                                                                               
RELX     DS    F                                                                
*YEAR     DS    CL2                 EBCDIC - FOR DISPLAY                        
*BYEAR    DS    X                   BINARY YEAR                                 
*STATFLAG DS    X                                                               
*HASCANLN EQU   X'80'               ORDER HAS CANCELLED BUYLINES                
         SPACE 1                                                                
ADCONS   DS    0A                  A/V TYPES                                    
         DC    V(OUTDAY)                                                        
         DC    V(REGENPBY)                                                      
         DC    V(REGENBUC)                                                      
         DC    V(REGENTL2)                                                      
         DC    V(UNTIME)                                                        
NADCONS  EQU   (*-ADCONS)/4                                                     
         SPACE 2                                                                
DASH     DC    51C'-'                                                           
         SPACE 2                                                                
*RMSCMODE DC    X'00'               REGENSC MODES                               
*                                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE REGENPBYD                                                      
         EJECT                                                                  
       ++INCLUDE RECNTFMTD                                                      
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
         EJECT                                                                  
LOCALWRK DSECT                                                                  
       ++INCLUDE RECNTCOL                                                       
*                                                                               
*      LOCAL VARIABLES                                                          
*                                                                               
         DS    CL4                 FILLER                                       
FOXZEROS DS    CL24                STRING OF X'F0'                              
EXTRAKEY DS    CL34                BUY KEY SAVE AREA                            
COLLAPSE DS    XL1                                                              
MULTEFDT DS    XL1                                                              
ZEROSPTS DS    XL1                                                              
TRANSCNT DS    XL1                                                              
COMFLAGS DS    XL1                 COMMENT FLAGS                                
*                                  BIT 0  =  MG SWITCH                          
*                                  BIT 1  =  BUY COMMENTS EXIST                 
ORDISSDT DS    CL7                 ORDER/ISSUE DATE                             
FIRSTSW  DS    XL1                                                              
TRANSCT  DS    XL1                 TRANSACTION COUNTER                          
TOTALAMT DS    F                   TOTAL VALUE OF ORDER                         
SVBYLN#  DS    F                   SAVED BUYLINE COUNT                          
BLDTABLE DS    XL24                (???? CHECK LENGTH)                          
SVSL#    DS    XL1                 SAVE SUBLINE #                               
CNVDATE  DS    CL6                                                              
HOLDDATE DS    CL6                                                              
DATEWORK DS    CL12                DATE WORK AREA                               
         ORG   DATEWORK                                                         
FRSTDATE DS    CL6                                                              
SECDATE  DS    CL6                                                              
THIRDATE DS    CL3                                                              
CMTCTR   DS    XL1                 COMMENT COUNTER                              
NOFWKS   DS    XL1                 NUMBER OF WEEKS CTR                          
ALTWKS   DS    XL1                 ALTERNATING WEEKS                            
STARTDTE DS    XL3                 START DATE                                   
ENDDTE   DS    XL3                 END   DATE                                   
ERLYSTRT DS    XL3                 EARLIEST START DATE                          
LATREND  DS    XL3                 EARLIEST END DATE                            
SVCNTWK  DS    XL1                 SAVE NUMBER OF WEEKS                         
SPOTSWK  DS    XL1                 SPOTS PER WEEK                               
CKSTRDTE DS    CL3                 FOR CHECKDATE ROUTINE                        
SVSTRDTE DS    CL3                 FOR CHECKDATE ROUTINE                        
SVSPTSWK DS    XL1                 FOR CHECKDATE ROUTINE                        
ORIGSPTS DS    XL1                 STARTING SPOTS/WK FOR COLLAPSE               
DYCNTFLG DS    XL1                 CALENDAR-SET FLAG:                           
*                                     ZERO = NOT SET                            
*                                     NON=ZERO = SET                            
RUNREP   DS    CL4                 REP OF RUN                                   
*                                                                               
TLSPOTS  DS    F                   TOTAL SPOTS                                  
DAYBUMP  DS    F                                                                
DAYCNT   DS    F                                                                
SPOTLEFT DS    F                                                                
SPOTMAX  DS    F                                                                
SPOTMAX2 DS    F                                                                
AEFFDATE DS    A                                                                
TOTLSPTS DS    F                   TOTAL SPOTS IN ORDER                         
TOTLDOLS DS    F                   TOTAL $$    IN ORDER                         
*********************************************************************           
*    FIELDS USED TO BUILD DAY/TIME STRINGS + COMMENTS OF GROUPS                 
*        OF D/T STRINGS FOR THE 0105 RECORDS                                    
*                                                                               
DTLENGTH DS    XL1                 LENGTH OF DAY/TIME STRING                    
DTBUILD  DS    16B                 CONSTRUCT DAT/TIME STRING                    
DTCOMNT  DS    CL60                BUILD 0105 COMMENT DAY/TIMES                 
DTCOMEND EQU   *                                                                
DTCOMLEN EQU   *-DTCOMNT                                                        
DTSTRNGS DS    XL1                 NUMBER OF DAY/TIME STRINGS                   
*********************************************************************           
*    FIELDS USED TO HOLD DAYS AND TIMES BEFORE TRANSLATING TO                   
*        VALID BIAS FORMAT                                                      
XDATE    DS    CL12                TRANSLATE DATE                               
XTIME    DS    CL4                 TRANSLATE TIME                               
*********************************************************************           
         DS    0H                                                               
EOPRECS  DS    0CL48               EOP SAVE AREAS                               
EOPADV   DS    CL12                                                             
EOPAGY   DS    CL12                                                             
EOPOFF   DS    CL12                                                             
EOPSAL   DS    CL12                                                             
*                                                                               
SUPPRECS DS    0CL100              SUPPORT RECORD INFORMATION                   
AGYNAME  DS    CL33                                                             
ADVNAME  DS    CL20                                                             
SALNAME  DS    CL20                                                             
OFFNAME  DS    CL20                                                             
PRODNAME DS    CL20                                                             
REPNAME  DS    CL10                                                             
ZIPCODE  DS    CL10                                                             
         DS    0F                                                               
AREGENSC DS    A                                                                
BLOCK    DS    480C                DEMO WORK AREA                               
NEW15ELT DS    CL20                EC CONTROL ELEMENT STORAGE                   
STAOPTS  DS    XL1                                                              
*                                                                               
LOCALEND EQU   *                                                                
         EJECT                                                                  
*                                                                               
*   CHKCMTS:  CHECKS ELEMENT PASSED IN.  IF 'C=' OR 'SC=',                      
*        COMMENT IS RETRIEVED AND FORMATTED FOR OUTPUT.                         
*   P1   =   A(WORKSPACE)                                                       
*   P2   =   A(COMMENT ELEMENT CONTROL)                                         
*   P3   =   TYPE OF RECORD:  1  =  HEADER COMMENT (COMMENT)                    
*                             2  =  REP ORDER COMMENT (INSTRUCTION)             
         CSECT                                                                  
         DS    0F                                                               
CHKCMTS  NMOD1 0,**CMTS**                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R7,4(R1)            RESET A(COMMENT ELEMENT)                     
         L     R5,8(R1)            SET INDICATOR FLAG                           
         LA    R7,2(R7)            PASS CONTROL OF ELEMENT                      
         CLC   0(2,R7),=C'C='      STANDARD COMMENT?                            
         BE    CCMT0040            YES                                          
         CLC   0(3,R7),=C'SC='     SFM COMMENT?                                 
         BNE   CCMT0400            NO  - EXIT                                   
CCMT0040 EQU   *                                                                
         L     R2,AIO4             SET A(IO AREA 4)                             
         ICM   RF,15,=AL4(CONLENQ)                                              
*                                                                               
         PRINT GEN                                                              
         XCEF  0(R2),(RF)          CLEAR THE AREA                               
         GOTO1 AREGENSC,DMCB,(3,0(R7)),(R2),DATAMGR,RCONREC,GETTXT              
                                                                                
         PRINT NOGEN                                                            
         SR    R0,R0               SET CC = ZERO                                
         B     CCMT0800            EXIT                                         
*                                  COMMENT FOUND OR NOT WILL BE                 
*                                     DETERMINED AFTER CALL                     
CCMT0400 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
CCMT0800 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
****INSERT                                                                      
*                                                                               
*********************************************************************           
*- GETSTA -- READ STATION RECORD INTO IO3.                                      
*********************************************************************           
GETSTA   NMOD1 0,*GSTA*                                                         
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R4,4(R1)            SET A(STATION IN CONTRACT REC)               
GSTA05   DS    0H                                                               
         LA    R5,KEY                                                           
         USING RSTAKEY,R5                                                       
         XC    KEY,KEY                                                          
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,0(R4)      INSERT STATION                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    GSTA10                                                           
         DC    H'0',C'MISSING STA REC'                                          
GSTA10   GOTO1 VGETREC,DMCB,RSTAREC                                             
*                                                                               
*- PICK UP RECORD DATA                                                          
*                                                                               
*    NEW DATA TO DEFINE THE OUTPUT FOR ELECTRONIC CONTRACTING MAY               
*        HAVE TO BE IDENTIFIED.  THIS WILL PERMIT THE SPOOLING OF               
*        THE EC REPORT IN THE RIGHT DIRECTION.                                  
*                                                                               
GSTA20   DS    0H                                                               
*                                                                               
         LA    R6,RSTAREC          STATION RECORD                               
         MVI   ELCODE,X'08'        EXTRA DESCRIPTION ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   GSTA55              NOT THERE                                    
*                                                                               
         USING RSTAXXEL,R6                                                      
         MVC   STAOPTS,RSTAOPTB    SAVE OPTION FLAG                             
*                                                                               
GSTA55   EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
*                                  IF RECORD NOT FOUND, JOB BLOWS UP            
         XIT1                                                                   
         DROP  R5,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*- FINDID -- FIGURE OUT WHERE THE CONTRACT IS SUPPOSED TO GO FROM               
*            SOMETHING IN THIS MESS......                                       
*                                                                               
*  INPUT       RSTAREC                                                          
*                                                                               
*  RETURN                                                                       
*        BYTE  PARAMETER INTO 'PQOPEN'                                          
*        CC    ZERO  = GOOD EXIT.  SEND ID AND FORMAT FILLED IN                 
*        CC    NON-0 = COPY NOT REQUIRED.  EXIT ASAP.                           
*                                                                               
*********************************************************************           
*- SENDER = STATION - STATION WILL INITIATE THE EC/E2 COMMAND.                  
*                                                                               
*  ID = TERMINAL                                                                
*                                                                               
*********************************************************************           
FINDID   NMOD1 0,*FIND*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         MVC   SENDID,TWAUSRID                                                  
*                                                                               
         SR    R0,R0               SET CC = ZERO                                
*                                  ALWAYS RETURN A ZERO CC                      
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*   PQOPENA:                                                                    
*                                                                               
* 'BYTE' CONTAINS A PARAMETER TO THIS ROUTINE.  IF BYTE IS A 'W',               
* THEN CLASS 'G' REPORTS WILL BE CREATED WITH STATUS 'KEEP'.  THIS              
* WILL CAUSE THEM TO BE PICKED UP BY WESTERN UNION, NOT GRAPHNET.               
*                                                                               
PQOPENA  NMOD1 0,*PQOP*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R8,4(R1)            RESET A(SPOOLWORK)                           
         XC    SPOOLKEY,SPOOLKEY                                                
         OI    SPOOLIND,X'40'   ALLOW USER VALUES-CLASS,LPP,COPIES              
         LA    R3,SPOOLKEY                                                      
         MVC   SPOOLKEY+12(3),=C'ECR'                                           
         MVC   SPOOLKEY+1(11),SPACES                                            
         MVC   1(8,R3),CONCNUM                                                  
         SPACE 2                                                                
VPQ10    CLI   1(R3),C'0'                                                       
         BNE   VPQ20                                                            
         MVC   1(7,R3),2(R3)                                                    
         B     VPQ10                                                            
         SPACE 2                                                                
VPQ20    DS    0H                                                               
         GOTO1 SQUASHER,DMCB,SPOOLKEY+1,11                                      
         MVI   SPOOLKEY+16,68      68 LINES TO A PAGE                           
         MVI   SPMODE,0                                                         
         SPACE 1                                                                
         CLI   INTTYPE,C'E'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RF,SPOOLKEY                                                      
         USING PQPLD,RF                                                         
*                                                                               
*   CLASS WILL BE CHANGED ONCE A NEW TYPE IS ASSIGNED TO THIS                   
*        FUNCTION                                                               
*                                                                               
****>>>  MVI   PLCLASS,C' '        TEMPORARY                                    
         MVI   PLCLASS,C'G'        CLASS G                                      
*                                  LIVE PER DAVID EISENBERG: 9/29/95            
         SPACE 1                                                                
         OC    SENDID,SENDID       IF SEND ID, STORE IN PLUSER                  
         BZ    VPQ30                                                            
         MVC   PLUSER(2),SENDID                                                 
*                                                                               
         DROP  RF                                                               
*                                                                               
VPQ30    LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         LA    RE,TWASPKEY                                                      
         DROP  RF                                                               
         ST    RE,SPOOLQLK         SET EXTENDED KEY ADDRESS                     
         USING PQPLD,RE                                                         
         XC    0(133,RE),0(RE)                                                  
         MVI   QLEXTRA,X'FF'                                                    
         MVC   QLRETNL,=H'6'                                                    
         MVC   QLRETND,=H'2'                                                    
         MVC   QLRETNL,=H'36'      ORDER WORKSHEETS                             
         MVI   QLSYS,C'R'          FORM/COPIES COLUMN                           
         MVC   QLPRG,=C'EC'                                                     
         SPACE 1                                                                
         CLC   SENDID(2),=X'0406'  FOR GRAPHNET COPY, USE                       
         BNE   VPQ50                                                            
         MVC   QLRETND,=H'26'      PRTD/SENT RETENTION OF 26, NOT 2             
         DROP  RE                                                               
VPQ50    GOTO1 SPOOL,PARAS,(R8)                                                 
         MVC   SPOOLRPN,SPOOLKEY+19                                             
         XIT1                                                                   
         SPACE 3                                                                
OKMESS   DC    C'XXX,12345 HAS BEEN SPOOLED. PAGES=NNN,LINES=NNNN'              
CONXMSG  DC    C'**** CONTRACT CONFIRMED ****                    '              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* CREATES EDICT HEADER CARDS FOR EDICT PROCESSING                               
*                                                                               
EDICT    NMOD1 0,*EDIC*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R8,4(R1)            RESET A(SPOOLWORK)                           
                                                                                
         MVI   FORCEHED,C'N'       PREVENT HEADLINES                            
         MVI   LINE,0                                                           
         MVC   P+4(5),=C'*HDR*'                                                 
                                                                                
         MVC   P+9(6),=C'EDICT='                                                
         TM    STAOPTS,X'10'       ENCODA ROUTING?                              
         BNO   EDICT01             NO                                           
         MVC   P+15(7),=C'*ENCODA' YES                                          
         B     EDICT06                                                          
EDICT01  EQU   *                                                                
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CTIREC,R6                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,TWAUSRID                                                 
         DROP  R6                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO2                  
*                                  KEY AND KEYSAVE NOT AFFECTED BECAUSE         
         L     R6,AIO2                                                          
         CLC   KEY(L'CTIKEY),0(R6)   IT ISN'T A GENCON READ HIGH                
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE!                               
*                                                                               
         LA    R6,28(R6)           OFFSET TO FIRST ELEMENT                      
*                                                                               
EDICT03  DS    0H                                                               
         CLI   0(R6),0             NOT FOUND, EXIT                              
         BNE   *+6                                                              
         DC    H'0'                MUST BE THERE!                               
                                                                                
         CLI   0(R6),CTDSCELQ                                                   
         BE    EDICT05                                                          
         ZIC   RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                MUST BE THERE!                               
                                                                                
         AR    R6,RF                                                            
         B     EDICT03                                                          
*                                                                               
EDICT05  DS    0H                                                               
         USING CTDSCD,R6                                                        
         CLI   CTDSCLEN,3          MUST BE AT LEAST 3 CHAR LONG                 
         BL    EDICTX                                                           
         ZIC   R1,CTDSCLEN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+15(0),CTDSC                                                    
         DROP  R6                                                               
EDICT06  EQU   *                                                                
         MVI   P+34,C'W'           WIDE REPORT - 132 CHARS                      
         MVC   P+54(2),RCONTEM     TEAM                                         
*                                  SEND SPECIAL PRINT LINE                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* PRINT A ++DDS CARD WITH 58 BYTES OF DATA FOR EDICT XACTION PROG               
*                                                                               
         LA    R5,P                                                             
         USING EDICTD,R5                                                        
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST,=C'RE'      FOR SYSTEM REP                               
                                                                                
         MVI   EDIPROG,C'E'        FOR TYPE E (ELECTRONIC CONTRACT)             
         MVC   EDIPROG+1(2),REPALPHA                                            
                                                                                
         MVC   EDIIDEN,=C'TRN'     XACTION FILE DATA                            
                                                                                
         MVC   EDIRCNRP,RCONKREP   REP CODE                                     
         MVC   EDIRCNOF,RCONKOFF   OFF CODE                                     
         MVC   EDIRCNSP,RCONSAL    SALESPERSON CODE                             
         MVC   EDIRCNAG,RCONKAGY   AGENCY CODE                                  
         MVC   EDIRCNAO,RCONKAOF   CITY CODE                                    
         MVC   EDIRCNAD,RCONKADV   ADVERTISER CODE                              
         MVC   EDIRCNCT,RCONKCON   CONTRACT TYPE                                
* FLIGHT START AND END DATES                                                    
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,WORK2,0,DUB                       
         GOTO1 DATCON,DMCB,(0,WORK2),(5,EDIRCNFS)                               
         GOTO1 DATCON,DMCB,(0,WORK2+6),(5,EDIRCNFE)                             
* LATEST VERSION NUMBER                                                         
         LA    R6,RCONREC                                                       
         USING RCONSEND,R6                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   EDICT20                                                          
         CLC   RCONSRV,RCONSSV     SHOW LATEST VERSION                          
         BH    EDICT10                                                          
         EDIT  (1,RCONSSV),(3,EDIRCNVN),ALIGN=LEFT                              
         B     EDICT20                                                          
EDICT10  EDIT  (1,RCONSRV),(3,EDIRCNVN),ALIGN=LEFT                              
                                                                                
EDICT20  MVC   EDIRCNST,RCONKSTA   STATION CALLS                                
* CONTRACT NUMBER                                                               
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RCONKCON                                                 
         EDIT  (P5,WORK),(8,EDIRCNHN),ALIGN=LEFT                                
         DROP  R5,R6                                                            
*                                  SEND SPECIAL PRINT LINE                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         MVI   FORCEHED,C'Y'                                                    
                                                                                
EDICTX   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* INCLUDE DMPRTQL                                                               
* INCLUDE CTGENFILE                                                             
       ++INCLUDE DMPRTQL                                                        
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*          DATA SET RECNT64    AT LEVEL 072 AS OF 08/24/93                      
*   RESOLVE:  FILL IN THE ADDRESSES OF CALLED ROUTINES                          
* DERIVED FROM THE RECNT80 MODULE                                               
*                                                                               
         CSECT                                                                  
RESOLVE  NMOD1 0,*RESO*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,CORES                                                         
         LA    R4,CLIST                                                         
         SPACE 1                                                                
RESO0020 XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A00'                                           
         MVC   DMCB+7(1),0(R4)     OVERLAY NUMBER                               
         GOTO1 CALLOV,DMCB                                                      
         MVC   0(4,R2),DMCB                                                     
         LA    R2,4(R2)                                                         
         LA    R4,1(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   RESO0020            GO BACK FOR NEXT                             
         SPACE 1                                                                
         MVC   BOOKVAL(56),CORES   GET CORE RESIDENT PHASE                      
         MVC   UPVAL,CORES+56                                                   
         SPACE 1                                                                
         XIT1                                                                   
CLIST    DC    X'00'               BOOKVAL                                      
         DC    X'01'               CENTER                                       
         DC    X'02'               CHOPPER                                      
         DC    X'03'               DAYVAL                                       
         DC    X'E0'          ***DEMOCON***  (WAS DEMCON)                       
         DC    X'05'               DEMEX                                        
         DC    X'06'               DEMOTAB                                      
         DC    X'07'               DEMVAL                                       
         DC    X'08'               DEMUP                                        
*         DC    X'09'                                                           
*         DC    X'0A'                                                           
*         DC    X'0B'                                                           
         DC    X'0C'               SPOOL                                        
         DC    X'0D'               SQUASHER                                     
         DC    X'0E'               TIMVAL                                       
         DC    X'0F'               UNDAY                                        
         DC    X'10'               UNDERLIN                                     
*         DC    X'11'               UNTIME                                      
*         DC    X'12'               XSORT                                       
         DC    X'13'               UPVAL                                        
         DC    X'FF'                                                            
         SPACE 1                                                                
CORES    DS    0F                                                               
VBOOKVAL DS    V                                                                
VCENTER  DS    V                                                                
VCHOPPER DS    V                                                                
VDAYVAL  DS    V                                                                
VDEMCON  DS    V                                                                
VDEMEX   DS    V                                                                
VDEMOTAB DS    V                                                                
VDEMVAL  DS    V                                                                
VDEMUP   DS    V                                                                
*VINVEDIT DS    V                  NOT USED - FROM AVAILS...                    
*VPAVCOND DS    V                                                               
*VPAVEXPL DS    V                                                               
VSPOOL   DS    V                                                                
VSQUASH  DS    V                                                                
VTIMVAL  DS    V                                                                
VUNDAY   DS    V                                                                
VUNDERLN DS    V                                                                
VUNTIME  DS    V                                                                
VXSORT   DS    V                                                                
VUPVAL   DS    V                                                                
         EJECT                                                                  
         LTORG                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011RECNT6F   09/16/02'                                      
         END                                                                    
