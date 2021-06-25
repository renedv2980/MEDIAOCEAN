*          DATA SET REREQ02S   AT LEVEL 140 AS OF 09/23/03                      
*          DATA SET REREQ02    AT LEVEL 074 AS OF 06/07/99                      
*PHASE T80702A,*                                                                
*INCLUDE SCANNER                                                                
*INCLUDE REQTWA                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE PERVERT                                                                
         SPACE                                                                  
         MACRO                                                                  
&TAG     ZERO  &ADDR,&LEN,&BYTE=X'0'                                            
.*                                                                              
         AIF   ('&LEN' EQ '0').ZERO20                                           
.*                                                                              
         LA    RE,&ADDR-1                                                       
         LA    RF,&LEN                                                          
         IC    R0,=&BYTE                                                        
Z&SYSNDX STC   R0,0(RF,RE)                                                      
         BCT   RF,Z&SYSNDX                                                      
.ZERO20  ANOP                                                                   
         MEND                                                                   
REQ2     TITLE 'T80702 - REREQ02 - REP REQUEST SCREEN VAL/RQST ADD/CHG'         
*                                                                               
***********************************************************************         
*                                                                     *         
*  REREQ02 -- PHASE T80702 -- REP REQUEST SCREEN VAL/ADD/CHANGE       *         
*                                                                     *         
*  VALIDATES REQUEST SCREEN FIELD INPUT, PERFORMS WHOLE-REQUEST       *         
*  EDITS (SO-CALLED 'AFTER PROCESS'), AND ADDS REQUEST TO FILE.       *         
*                                                                     *         
*  PROCESSES CHANGE FOR CURRENT REQUEST (ON THE SCREEN)               *         
*  (POINTED TO BY 'DAONSCRN')                                         *         
*                                                                     *         
*  ** NOTE **  DDS TUBES WILL BYPASS ALL REQUIRED FIELDS              *         
*                                                                     *         
*  COMPENSATION S/P ACCESS PERMISSION IS HARDCODED IN OPT2FLD.        *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
       ++INCLUDE REREQPRF                                                       
***********************************************************************         
*                                                                     *         
*  MOD LOG                                                            *         
*  -------                                                            *         
*  ***HISTORY PRE-96 HAS BEEN DELETED.  B. UHR.  MAY29/97             *         
*                                                                     *         
*  JAN19/96 (BU ) --- ADD CON/UNC OPTION TO REPORT                    *         
*                                                                     *         
*  JAN26/96 (BU ) --- ADD DR OPTION TO REPORT                         *         
*                                                                     *         
*  FEB07/96 (RHV) --- FIX AFFILIATE TABLE                             *         
*                                                                     *         
*  MAY09/96 (BU ) --- DISABLE PROFILE BYTE DEFAULTING.  ALSO SET      *         
*                     PROFILE LOCKOUT OF 26/27 REPORTS FROM STATION   *         
*                     SIDE.                                           *         
*                                                                     *         
*  OCT04/96 (SKU) --- FIX RFP BUG FOR 3G                              *         
*                                                                     *         
*  JAN02/97 (BU ) --- SET PROFILE LOCKOUT OF 2E/2F/2G/2H/4B REPORTS   *         
*                     FROM STATION SIDE.                                        
*                                                                     *         
*  MAY29/97 (BU ) --- UPGRADE FOR YEAR 2000                           *         
*                                                                     *         
*  DEC01/97 (BU ) --- ADD 'ALL' OPTION TO REPORT                      *         
*                                                                     *         
*  JAN06/98 (BU ) --- TERRITORY FILTERING                             *         
*                                                                     *         
*  FEB19/98 (BU ) --- READDRESS FOR INCREASED IOWORK TO 6K            *         
*                                                                     *         
*  FEB24/98 (BU ) --- EXCLUDE BACK BILLING OPTION                     *         
*                                                                     *         
*  NOV20/00 (BU ) --- CLEAR CHANNEL MASTER SET ACCESS                 *         
*                                                                     *         
*  OCT03/01 (BU ) --- FORCE BUYLINE CODE REPORTS TO RZ                *         
*                                                                     *         
*                                                                     *         
*                     ***  END TOMBSTONE  ***                         *         
***********************************************************************         
*                                                                     *         
*   R8 AND R6 ARE USED AS ADDITIONAL BASE REGISTERS.  AFTER THEM,     *         
*          'RC' IS ALSO USED.                                         *         
*                                                                     *         
*                                                                     *         
*   NOTE:  'RC' IS USED AS A PROGRAM BASE REGISTER AND DOES NOT POINT *         
*           TO THE WORK AREA                                          *         
*                                                                     *         
*          FURTHER, RC CANNOT BE SET-UP IN THE NMOD MACRO BECAUSE     *         
*           THE MACRO TRIES TO SET RC TO THE WORKAREA                 *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
         PRINT NOGEN                                                            
T80702   CSECT                                                                  
         NMOD1 0,T80702,R8,R6,RR=R2                                             
*                                                                               
         LA    RC,2048(R6)         ESTABLISH 'RC' AS BASE REGISTER              
*                                     AFTER LAST (R6) REGISTER                  
         LA    RC,2048(RC)                                                      
         USING T80702+12288,RC     DISPLACEMENT INTO MODULE                     
*                                                                               
         L     R9,0(R1)            A(WORK AREA FROM CALLER)                     
         USING REQWRK,R9                                                        
*                                                                               
         ST    R2,SUBRELO          SUB-OVERLAY RELOCATION FACTOR                
*                                                                               
         LR    RF,R9               SET A(REQWRK AREA)                           
         AH    RF,=Y(DIOWORK)      SET A(IOWORK W/IN DSECT)                     
*                                                                               
         MVC   0(4,RF),=C'*IO*'    INSERT FLAG                                  
         LA    RF,4(RF)            BUMP PAST FLAG                               
         ST    RF,AIOADDR2         SAVE ADDRESS OF IOWORK                       
*                                                                               
         L     RA,ASAVE            A(TWA)                                       
         USING TWAD,RA                                                          
*                                                                               
         LA    RE,REQDEF           USE REQTBL ENTRY IN TWA                      
         ST    RE,AREQNTRY                                                      
*                                                                               
         CLI   ACTION,EQADD        ADD?                                         
         BE    ADNEWREQ                                                         
*                                                                               
         CLI   ACTION,EQCHA        CHANGE?                                      
         BE    ADNEWREQ                                                         
         DC    H'0'                WE DON'T BELONG HERE                         
         TITLE 'REREQ02 -- MAINLINE ADD LOGIC'                                  
*                                                                               
*- ADNEWREQ -- ADD NEW REQUEST MAINLINE LOGIC                                   
*                                                                               
ADNEWREQ EQU   *                                                                
*                                                                               
         MVI   NOMASTER,0                                                       
         CLC   =C'SOON',RQSWHEN    TEST IF 'SOON'                               
         BNE   ADNR0020                                                         
         CLI   IAMAMAST,C'Y'                                                    
         BNE   ADNR0020                                                         
         LA    RE,REQDEF                                                        
         TM    RQCNTL2(RE),RQ2MASOV                                             
         BZ    ADNR0020                                                         
         MVI   NOMASTER,2                                                       
         B     ERROR                                                            
*                                                                               
ADNR0020 EQU   *                                                                
         GOTO1 DATCON,P1,(5,0),(0,TODAY) FOR DEFAULTING                         
*                                                                               
         GOTO1 =A(NEWREQ),P1,(R9),RR=SUBRELO                                    
*                                                                               
         LA    R3,REQMAP           UNPROTECTED FIELD MAP                        
*                                                                               
ADNR0040 EQU   *                                                                
         CLC   =XL2'00',0(R3)       END OF FIELDS?                              
         BE    ADNR0140                                                         
*                                                                               
         ST    R3,AMAPNTRY                                                      
*                                                                               
         L     R2,MAPFLD(R3)       A(FIELD HEADER TO VAL)                       
         AR    R2,RA                                                            
*                                                                               
         ST    R2,CURSPOS          PUT CURSOR HERE IF ERROR                     
*                                                                               
         TM    MAPCNTL(R3),FCXPAN  FOLLOWED BY EXPANSION?                       
         BZ    ADNR0060                                                         
*                                                                               
         ZIC   RE,0(R2)                                                         
         AR    RE,R2                                                            
         GOTO1 ACLRSCRN,P1,(RE),(RE) CLEAR EXPANSION                            
*                                                                               
*- TEST FOR REQUIRED INPUT.                                                     
ADNR0060 EQU   *                                                                
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   ADNR0100            YES.                                         
*                                                                               
         TM    MAPCNTL(R3),FCVAL   VALIDATE ON NO INPUT?                        
         BO    ADNR0100                                                         
*                                                                               
*- DDS TUBES NOT REQUIRED TO ENTER FIELDS IF REQUESTOR = DDS                    
*                                                                               
         CLC   =C'DDS',RQSOPT                                                   
         BNE   ADNR0080                                                         
*                                                                               
         CLI   TWAOFFC,C'*'                                                     
         BE    ADNR0120                                                         
*                                                                               
ADNR0080 EQU   *                                                                
         MVI   ERRDATA,MISSING     REQUIRED FIELD IS MISSING                    
         MVI   ERRCNTL,ECMSGNUM                                                 
*                                                                               
         TM    1(R2),X'08'         HI-INTENSITY = REQUIRED.                     
         BO    ERROR                                                            
         B     ADNR0120            OPTIONAL FIELD.  SKIP IT.                    
*                                                                               
*- CALL VALIDATION ROUTINE DEFINED BY REQMAP ENTRY.                             
ADNR0100 EQU   *                                                                
         MVI   ERRCNTL,ECMSGNUM    INVALID INPUT                                
         MVI   ERRDATA,INVIPT                                                   
*                                                                               
         XC    KEY,KEY             CLEAR FOR EACH VAL RTN                       
*                                                                               
         BAS   RE,ORBLANKS         CONVERT NULLS TO BLANKS                      
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,3,MAPVAL(R3)     VAL ROUTINE NUMBER                           
         LA    RF,FLDVAL           SET END OF TABLE                             
         LA    RF,FLDVALX-FLDVAL(RF)                                            
         PRINT GEN                                                              
         GOTO1 GOINDEX,P1,(R4),FLDVAL,(RF)                                      
         PRINT NOGEN                                                            
         BNZ   ERROR                                                            
*                                                                               
ADNR0120 LA    R3,MAPLNTRY(R3)     NEXT REQMAP ENTRY                            
         B     ADNR0040                                                         
*                                                                               
*- ALL FIELD VALIDATED.                                                         
*  CHECK STATION LIMITED OFFICE ACCESS.                                         
*  NEED WHOLE-REQUEST EDITS?                                                    
ADNR0140 EQU   *                                                                
         CLI   WHEN,EQSOON                                                      
         BNE   ADNR0160                                                         
*                                                                               
*   SOON REQUEST:  CHECK FOR FILTER FIELD MINIMUMS                              
*                                                                               
         GOTO1 =A(SOONFLDS),DMCB,(RC),RR=SUBRELO                                
         BNZ   ERROR               NEEDS ADDL FILTERS FOR 'SOON'                
ADNR0160 EQU   *                                                                
         GOTO1 =A(OFFLIMIT),DMCB,(RC),RR=SUBRELO                                
         BNZ   ERROR               STATION LIMIT TESTS                          
*                                                                               
         GOTO1 =A(OF2LIMIT),DMCB,(RC),RR=SUBRELO                                
         BNZ   ERROR               OFFICE LIMIT TESTS.                          
*                                                                               
         CLI   TWAACCS,C'$'        STATION MAKING REQUEST?                      
         BNE   ADNR0200            NO                                           
         LA    R3,PROFDATA+2       YES - STATION 26/27 LOCKOUT SET?             
         TM    0(R3),X'40'                                                      
         BNO   ADNR0200            NO                                           
         L     RE,AREQNTRY         YES - A(REQUEST ENTRY)                       
         CLC   =C'26',RQOPT4ID(RE) 26 REPORT?                                   
         BE    ADNR0180            YES - REJECT THE REQUEST                     
         CLC   =C'2E',RQOPT4ID(RE) 2E REPORT?                                   
         BE    ADNR0180            YES - REJECT THE REQUEST                     
         CLC   =C'2F',RQOPT4ID(RE) 2F REPORT?                                   
         BE    ADNR0180            YES - REJECT THE REQUEST                     
         CLC   =C'2G',RQOPT4ID(RE) 2G REPORT?                                   
         BE    ADNR0180            YES - REJECT THE REQUEST                     
         CLC   =C'2H',RQOPT4ID(RE) 2H REPORT?                                   
         BE    ADNR0180            YES - REJECT THE REQUEST                     
         CLC   =C'4B',RQOPT4ID(RE) 4B REPORT?                                   
         BE    ADNR0180            YES - REJECT THE REQUEST                     
         CLC   =C'27',RQOPT4ID(RE) 27 REPORT?                                   
         BNE   ADNR0200            NO                                           
ADNR0180 EQU   *                                                                
         GOTO1 PUTCURS,P1,VCONTRC2,STANOTGD,L'STANOTGD                          
         B     ERROR               FAILS 'FRIDAY' TEST                          
ADNR0200 EQU   *                                                                
         CLI   FRIDTEST,C'Y'       REP REQUIRES FRIDAY TEST?                    
         BNE   ADNR0240            NO                                           
         LA    RE,REQDEF           CHECK FOR 'FRIDAY' FLAG                      
         TM    RQCNTL2(RE),RQ2FRIDY 'FRIDAY' FLAG SET?                          
         BZ    ADNR0240            NO                                           
         GOTO1 =A(VALFRI),P1,(R9),RR=SUBRELO                                    
*                                  YES - REQUIRES FRIDAY TESTING                
         BZ    ADNR0240            PASSES FRIDAY TEST                           
*                                                                               
*   'FRIDAY' TESTING SPLITS HERE.  SOME REPORT(S) REQUIRE ADDITIONAL            
*     FILTERING, OTHER(S) MAY BE UNACCEPTABLE IF NOT SUBMITTED ON               
*     FRIDAY ITSELF.                                                            
         L     RE,AREQNTRY         A(REQUEST ENTRY)                             
         CLC   =C'1G',RQCARDID(RE) OFFICE BUDGET REPORT?                        
         BE    ADNR0220            YES - REJECT THE REQUEST                     
         GOTO1 PUTCURS,P1,VOFFICEC,NDOFFSTA,L'NDOFFSTA                          
         B     ERROR               FAILS 'FRIDAY' TEST                          
ADNR0220 EQU   *                                                                
         GOTO1 PUTCURS,P1,VGRPSUBG,ONLYFRID,L'ONLYFRID                          
         B     ERROR               FAILS 'FRIDAY' TEST                          
ADNR0240 EQU   *                                                                
         L     R3,AREQNTRY                                                      
         SR    R4,R4                                                            
         ICM   R4,1,RQXTRVAL(R3)   EXTRA VAL ROUTINE NUMBER                     
         BZ    ADNR0260                                                         
         GOTO1 GOINDEX,P1,(R4),XVAL,XVALX                                       
         BNZ   ERROR                                                            
*                                                                               
*- PREPARE REQUEST RECORD FOR ADDING                                            
*  FILL IN REQUEST HEADER, COUNT RQST CARDS, ETC.                               
ADNR0260 EQU   *                                                                
         GOTO1 =A(REPSTNAM),DMCB,(RC),RR=SUBRELO                                
*                                  LOOK FOR REP= OPTION                         
         BZ    ADNR0280            ERROR RETURNED                               
         LA    RE,NOTMSTER                                                      
         LA    RF,L'NOTMSTER                                                    
         ST    RE,AERROR                                                        
         ST    RF,AERROR+4                                                      
         LA    R3,RQSOPTH                                                       
         ST    R3,CURSPOS                                                       
         B     ERROR                                                            
ADNR0280 EQU   *                                                                
         GOTO1 =A(XFILNAM),DMCB,(RC),RR=SUBRELO                                 
*                                  LOOK FOR F= OPTION                           
         BZ    ADNR0360            NO ERROR                                     
         CLI   HALF2,1             ERROR RETURN:  NO FIND?                      
         BNE   ADNR0300            NO                                           
         LA    RE,XFILNG           ERROR RETURNED                               
         LA    RF,L'XFILNG                                                      
         B     ADNR0340                                                         
ADNR0300 EQU   *                                                                
         CLI   HALF2,2             ERROR RETURN:  ACCESS DENIED?                
         BNE   ADNR0320            NO                                           
         LA    RE,XFILBDRP         ERROR RETURNED                               
         LA    RF,L'XFILBDRP                                                    
         B     ADNR0340                                                         
ADNR0320 EQU   *                                                                
         CLI   HALF2,3             ERROR RETURN:  NO SOONS?                     
         BE    *+6                 YES                                          
         DC    H'0'                ABORT:  ERROR UNRECOGNIZED                   
         LA    RE,XFILSOON         ERROR RETURNED                               
         LA    RF,L'XFILSOON                                                    
         B     ADNR0340                                                         
ADNR0340 EQU   *                                                                
         ST    RE,AERROR                                                        
         ST    RF,AERROR+4                                                      
         LA    R3,RQSOPTH                                                       
         ST    R3,CURSPOS                                                       
         B     ERROR                                                            
ADNR0360 EQU   *                                                                
         LA    RE,REQDEF           CHECK FOR 'BUYLINE READER' FLAG              
         TM    RQCNTL2(RE),RQ2BYRED      BUYLINE READER NEEDED?                 
         BNO   ADNR0370            NO                                           
         MVI   RBUYCOD,C'Y'        YES - SET 'USE BUYLINE READER'               
ADNR0370 EQU   *                                                                
         GOTO1 =A(OPT2FLD),DMCB,(RC),RR=SUBRELO                                 
         BNZ   ERROR               ERROR ENCOUNTERED                            
*                                                                               
         GOTO1 =A(REQHEAD),DMCB,(R9),RR=SUBRELO                                 
*                                                                               
*- ADD REQUEST TO FILE                                                          
*  OR CHANGE CURRENT REQUEST.                                                   
ADNR0380 EQU   *                                                                
*                                                                               
         CLI   ACTION,EQCHA        CHANGE?                                      
         BNE   ADNR0400                                                         
*                                                                               
*- CHANGING A SOON HAS TO BE BLOCKED BY BASE VALIDATOR.                         
         CLI   WHEN,EQSOON                                                      
         BNE   *+6                                                              
         DC    H'0'                SOON CAN'T BE A CHANGE                       
         MVI   RTNFLAG,1           SET RTNFLAG FOR 'DOCHANGE'                   
         B     ADNR0460                                                         
*                                                                               
*- ADD ACTION....SELECT OVERNIGHT OR SOON PROCESS                               
ADNR0400 EQU   *                                                                
         MVI   RTNFLAG,2           SET RTNFLAG FOR 'DOFILE'                     
         CLI   WHEN,EQOVRN                                                      
         BNE   ADNR0440                                                         
*                                                                               
         TM    RFPSTAT,RFPINUSE    ADDING REQUEST TO GROUP                      
         BZ    ADNR0460                                                         
         MVI   RTNFLAG,4           SET RTNFLAG FOR 'DOFILE/RFP'                 
         XC    AERROR,AERROR       SET FOR MY MESSAGE                           
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPRADD                                                
         GOTO1 AGETOVLY,P1,('OVRFP',0)                                          
         CLI   QRFPMODE,QRFPOK                                                  
         BNE   ADNR0420                                                         
         XC    CURSPOS,CURSPOS                                                  
         B     COMMGOOD                                                         
*                                                                               
ADNR0420 DS    0H                                                               
         GOTO1 =A(BAD0420),DMCB,(R9),RR=SUBRELO                                 
         B     COMMBAD                                                          
*                                                                               
ADNR0440 DS    0H                                                               
         MVI   RTNFLAG,3           SET RTNFLAG FOR 'DOSOON'                     
         CLI   WHEN,EQSOON                                                      
         BE    ADNR0460                                                         
*                                                                               
         DC    H'0'                WHEN DO YOU EXPECT THIS TO RUN?              
*                                                                               
ADNR0460 EQU   *                                                                
*                                                                               
*   ALL MAINTENANCE ROUTINES HAVE BEEN MOVED TO RQST05 MODULE,                  
*        TO PERMIT INSERTION OF ACCESS CHECK CODE PRIOR TO                      
*        COMPLETION OF REQUEST                                                  
*                                                                               
*        CLI   RTNFLAG,3           'DOSOON' REQUESTED?                          
*        BNE   ADNR0480            NO  - CHANGE AND ADD ARE                     
*                                     DONE IN RQST05 MODULE                     
*        GOTO1 (RF),P1,(R9),RR=SUBRELO    DO SOMETHING WITH REQUEST             
*        BNZ   ERROR                                                            
*                                                                               
ADNR0480 EQU   *                                                                
         LA    RE,RQSACTH                                                       
         ST    RE,CURSPOS                                                       
         SPACE 2                                                                
         CLC   RREC3,SPACES        IS THERE A THIRD CARD?                       
         BNH   ADNR0500            NO                                           
         MVI   R2CONTIN,C'+'       YES - SET CONTINUATION INDICATOR             
ADNR0500 EQU   *                                                                
         SR    R0,R0               GOOD CC                                      
         B     EXIT                                                             
*                                                                               
ERROR    EQU   *                   UNTIL WE NEED SPECIAL ERROR PROC             
         LTR   RD,RD               ^0 CC                                        
*                                                                               
EXIT     XIT1                      GENERIC EXIT                                 
         TITLE 'REREQ02 -- ERROR MESSAGES AND EQUATES'                          
*                                                                               
*- ERROR MESSAGES                                                               
*                                                                               
REQCHA   DC    C'REQUEST HAS BEEN CHANGED'                                      
WHOLEQ   DC    C'DATES MUST BE ON QUARTERLY BOUNDARIES'                         
SAMEYEAR DC    C'START/END DATES MUST BE IN SAME YEAR'                          
OPTCACCA DC    C'OPTION IS ''A'': ACCOUNTING OPTION MUST BE ''C'''              
OPT4AND5 DC    C'CAN''T SPECIFY OPTIONS 4 AND 5 IN SAME REQUEST'                
NEEDR    DC    C'GROUP MUST BE ''R'' (RADIO)'                                   
NEEDTV   DC    C'GROUP MUST BE ''T'' (TV)'                                      
NEEDWPER DC    C'FORMAT MUST BE ''A'' (WHOLE PERIOD)'                           
BOTHNONE DC    C'NEED BOTH START/END DATES OR NEITHER'                          
XDATE1   DC    C'FIRST DATE FIELD INVALID'                                      
XDATE2   DC    C'SECOND DATE FIELD INVALID'                                     
NOJCL    DC    C'JCL BOOK NOT ON FILE.  CONTACT DDS.'                           
DEPTH#   DC    C'DEPTH CANNOT EXCEED # STATIONS IN LIST'                        
NOTSAME  DC    C'''FROM''/''TO'' CANNOT BE THE SAME VALUE.'                     
NOTDOWN  DC    C'REQUIRES OUTPUT=''DOWN'''                                      
BANFILTS DC    C'NO FILTERS ALLOWED WITH ENTRY OF THIS FIELD'                   
NEEDSTAT DC    C'STATION FILTER REQUIRED FOR ''SOON'' REQUEST'                  
NOSTAT   DC    C'NO ''STATION'' FILTER FOR 14 REPORT'                           
NEEDGRUP DC    C'''DATES'' REQUIRE AT LEAST ''GROUP'' FILTER'                   
NEEDCNTR DC    C'NO FILTERS ENTERED - PLEASE RESUBMIT'                          
NORTCOPY DC    C'NO-RATE -COPY REQUIRES A CONTRACT NUMBER'                      
ALLFLDS  DC    C'SERVICE/BOOK/DEMOCODE ALL REQUIRED'                            
NEEDLT13 DC    C'DATE RANGE MUST BE 12 MONTHS OR LESS'                          
NODATE   DC    C'NOT ''BOTH:'' DATE NOT PERMITTED'                              
BOTHDATE DC    C'''BOTH'' REQUIRES CUTOFF DATE'                                 
MONEXBUD DC    C'DATES REQUESTED CROSS BUDGET YEAR BOUNDARIES'                  
EOMPAST  DC    C'CANNOT CLOSE PRIOR MONTHS'                                     
NOEOMREC DC    C'EOM RECORD NOT ON FILE'                                        
REPTABOO DC    C'''MASTER'' REP MAY NOT REQUEST THIS REPORT'                    
NDOFFSTA DC    C'NOT FRIDAY: REQUIRES OFFICE OR STATION FILTER'                 
ONLYFRID DC    C'NOT FRIDAY: REQUEST REJECTED'                                  
REVCLOSE DC    C'REVERSE:  START/END DATES MUST BE EQUAL'                       
NOTMSTER DC    C'THIS OPTION PERMITTED FOR MASTER REP ONLY'                     
XFILNG   DC    C'CROSS-FILE REFERENCE IS INVALID'                               
XFILBDRP DC    C'REP NOT PERMITTED TO USE THIS CROSS-FILE REF'                  
XFILSOON DC    C'CROSS-FILE NOT PERMITTED FOR SOON REQUESTS'                    
STANOTGD DC    C'STATION MAY NOT REQUEST THIS REPORT'                           
NEEDSOFT DC    C'SOON REPORT REQUIRES ''SOFT'' '                                
NEEDASAT DC    C'''SEND'' REQUIRES DATE'                                        
         SPACE                                                                  
         DS    0H                  ALIGNMENT                                    
*                                                                               
*- ERROR MESSAGE NUMBER EQUATES (STANDARD  SGS FROM FILE)                       
*                                                                               
MISSING  EQU   1                   REQUIRED FIELD NOT ENTERED                   
*                                                                               
INVIPT   EQU   2                   INVALID INPUT                                
*                                                                               
NOTFOUND EQU   53                  RECORD NOT FOUND                             
*                                                                               
SECLOCK  EQU   55                  SECURITY LOCKOUT                             
*                                                                               
ENDBFRST EQU   64                  END DATE BEFORE START                        
*                                                                               
MAX12MON EQU   170                 MAXIMUM DATE RANGE IS 12 MONTHS              
*                                                                               
GRPCONF  EQU   180                 STATION NOT IN GRP/SUBGRP                    
*                                                                               
SALCONF  EQU   181                 SALESMAN NOT IN DIV/TEAM                     
*                                                                               
CLSCONF  EQU   182                 CATEGORY NOT IN THIS CLASS                   
*                                                                               
OFFSTA   EQU   242                 OFFICE OR STATION REQUIRED                   
*                                                                               
NEED12M  EQU   252                 DATE SPAN MUST BE 12 MONTHS                  
         TITLE 'REREQ02 -- GOINDEX - CALL RTN VIA INDEX'                        
*                                                                               
*- GOINDEX - CALL ROUTINE VIA INDEX NUMBER AND ADDRESS LIST                     
*            WITH BRANCH-PAST-LIST CHECKING                                     
*                                                                               
*  REGISTERS RE, RF, R0 AND R1 USED INTERNALLY. ALL OTHERS UNCHANGED            
*                                                                               
*  INPUT:  P1 = INDEX NUMBER (4 BYTE BINARY, 1ST RTN = 1)                       
*          P2 = A(ADDRESS LIST TO INDEX)                                        
*          P3 = A(END OF ADDRESS LIST)                                          
GOINDEX  NTR1                                                                   
         LM    RE,R0,0(R1)         PICK UP IPT PARMS                            
         LTR   RE,RE                                                            
         BNZ   *+6                                                              
         DC    H'0'                INDEX NUMBER CAN'T BE 0                      
*                                                                               
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                A(ADDRESS LIST) CAN'T BE 0                   
*                                                                               
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                A(END OF ADDRESS LIST) CAN'T BE 0            
*                                                                               
         BCTR  RE,0                INDEX # -1                                   
         SLL   RE,2                * 4 = DISPLACEMENT                           
*                                                                               
         AR    RF,RE               START OF LIST + DISP = ENTRY                 
         CR    RF,R0                                                            
         BL    *+6                                                              
         DC    H'0'                INDEX EXTENDS PAST END OF LIST               
*                                                                               
         L     RF,0(RF)            PICK UP A(ROUTINE FROM LIST)                 
         A     RF,SUBRELO          RELOCATE                                     
         BR    RF                                                               
         B     EXIT                                                             
         TITLE 'REREQ02 -- OFFICE LIMIT TESTING FOR STATIONS'                   
         TITLE 'GENEX -- KEY VALIDATOR/EXPANSION DISPLAY'                       
*                                                                               
*- GENEX - GENERIC KEY AND RECORD READER WITH OPTIONAL EXPANSION MOVE.          
*          ALSO HAS OPTIONAL DATA TO REQUEST CARD MOVE.                         
*                                                                               
*  ** EXPANSION NOTE **                                                         
*     DATA LENGTH IN RECORD MUST BE AT LEAST AS LONG AS EXPANSION               
*     FIELD LENGTH.  DATA ONLY MOVED FOR LENGTH OF FIELD.                       
*                                                                               
*  INPUT: AMAPNTRY - A(REQMAP ENTRY) FOR THIS FIELD                             
*         KEY      - KEY TO FIND                                                
*         P1       - BYTE 1 - I/O CONTROL                                       
*                      0 = EXACT HIT ONLY (REP IS HI IN KEY)                    
*                      1 = LOOK FOR 'ZZ' REP (REP IS LOW IN KEY)                
*                    BYTES 2-4 - EXPANSION DISPLACEMENT IN RECORD               
*                                                                               
*         P2       - A(DATA TO MOVE INTO REQUEST CARD)                          
*         P3       - A(REQUEST CARD FIELD) - FILLED IN IF VALID DATA.           
*         P4       - LENGTH OF DATA TO MOVE                                     
*         P5       - LENGTH OF EXPANSION DATA. 0 = USE FIELD LENGTH.            
*         P6       - NOT IN USE. MUST BE 0                                      
*                                                                               
GENEX    NTR1  WORK=(R4,3)                                                      
*                                                                               
*- MOVE INPUT PARMS TO TEMP. WORK AREA                                          
         MVC   0(24,4),0(R1)       P1-P6                                        
*                                                                               
*- PARAMETERS NOT CURRENTLY IN USE MUST BE 0                                    
         CLC   =F'0',P6-P1(R4)                                                  
         BE    *+6                                                              
         DC    H'0'                P6 NOT 0                                     
*                                                                               
         ZIC   R2,0(R4)            I/O CONTROL BYTE                             
         L     R3,0(R4)            RECORD EXPANSION DISPLACEMENT                
GENEX020 GOTO1 DIRREAD,P1          LOOK FOR EXACT KEY                           
         CLI   P3-P1(R1),0                                                      
         BE    GENEX100            FOUND.                                       
         CLI   P3-P1(R1),X'10'     NOT FOUND?                                   
         BNE   COMMBAD             DMGR ERROR                                   
*                                                                               
*- KEY NOT ON FILE.  SHOULD WE LOOK FOR 'ZZ' REP?                               
         LTR   R2,R2                                                            
         BZ    COMMBAD             NO. ERROR.                                   
         MVC   KEYSAVE+25(2),=C'ZZ'                                             
         MVC   KEY(27),KEYSAVE                                                  
         SR    R2,R2               GET OUT IF NO HIT THIS TIME                  
         B     GENEX020                                                         
*                                                                               
*- KEY IS VALID.  MOVE DATA TO REQUEST CARD.                                    
GENEX100 EQU   *                                                                
         LM    RE,RF,P2-P1(R4)     RE=A(DATA), RF=A(REQ CARD FLD)               
         ICM   R1,15,P4-P1(R4)     LENGTH                                       
         BZ    GENEX200            NO MOVE ON 0 LENGTH                          
         BCTR  R1,0                                                             
         EX    R1,GENEX120                                                      
         B     GENEX200                                                         
         SPACE                                                                  
GENEX120 MVC   0(0,RF),0(RE)       MOVE DATA TO REQ CARD                        
         SPACE                                                                  
*                                                                               
*- IF FIELD HAS EXPANSION, READ IN RECORD                                       
*  AND MOVE EXPANSION TO SCREEN, ELSE EXIT WITH GOOD CC.                        
GENEX200 EQU   *                                                                
         L     R2,AMAPNTRY                                                      
         TM    MAPCNTL(R2),FCXPAN                                               
         BZ    COMMGOOD            NO EXPANSION. JUST EXIT.                     
*                                                                               
         GOTO1 FILREAD,P1                                                       
         L     R2,MAPFLD(R2)       INPUT FIELD DISPLACEMENT                     
         AR    R2,RA               + TWA START = INPUT FIELD                    
*                                                                               
*- CLEAR EXPANSION FIELD TO SPACES                                              
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               A(EXPANSION FIELD)                           
         GOTO1 ACLRSCRN,P1,(R2),(R2)                                            
*                                                                               
         L     RF,AIOWORK          RECORD IS HERE                               
         AR    R3,RF               R3 = A(EXPANSION DATA IN RECORD)             
*                                                                               
         ICM   RE,15,P5-P1(R4)     EXPANSION LENGTH                             
         BNZ   GENEX220                                                         
*                                                                               
         ZIC   RE,0(R2)            USE FIELD LENGTH AS EXPANSION LENGTH         
         LA    RF,8                                                             
         TM    1(R2),X'02'         EXTENDED FIELD?                              
         BZ    *+8                                                              
         LA    RF,8(RF)                                                         
         SR    RE,RF                                                            
*                                                                               
GENEX220 BCTR  RE,0                LESS 1 FOR EX                                
*                                                                               
         EX    RE,GENEX900         FROM RECORD TO EXPAN. FLD                    
*                                                                               
         B     COMMGOOD                                                         
         SPACE                                                                  
GENEX900 MVC   8(0,R2),0(R3)       MOVE EXPANSION TO SCREEN                     
         EJECT                                                                  
*&&DO                                                                           
* CODES BELOW MOVED FOR OTHER MODULES TO OBTAIN                                 
* ADDRESSIBILITY TO "SPACES" TO SAVE 180 BYTES                                  
*- ORBLANKS -- 'OR' BLANKS INTO SCREEN FIELD                                    
*  INPUT:  R2 = A(FIELD HEADER)                                                 
ORBLANKS EQU   *                                                                
         ZIC   RF,0(R2)                                                         
         SH    RF,=H'8'                                                         
         TM    1(R2),X'02'         EXTENDED HEADER?                             
         BZ    ORBLNK10                                                         
         SH    RF,=H'8'                                                         
ORBLNK10 EQU   *                                                                
         BCTR  RF,0                LESS 1 FOR THE 'EX'                          
         EX    RF,ORBLNK20                                                      
         BR    RE                                                               
ORBLNK20 OC    8(0,R2),SPACES                                                   
         SPACE 2                                                                
SPACES   DC    CL80' '                                                          
*&&                                                                             
         TITLE 'INDIVIDUAL FIELD VALIDATION ROUTINES'                           
*                                                                               
*- REGION VALIDATION                                                            
OFFRVAL  EQU   *                             OFFICE REGION - FIND BITS          
         MVI   KEY,X'03'                                                        
         MVC   KEY+23(2),TWAAGY                                                 
         MVC   KEY+25(2),8(R2)                                                  
         GOTO1 GENEX,P1,(0,RREGNAME-RREGREC),KEY+25,ROFFR,2,0,0                 
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
*- OFFICE VALIDATION                                                            
OFFVAL   EQU   *                             OFFICE - FIND BITS                 
         CLI   RRRGFLAG,C'Y'       SPECIAL IF RRG                               
         BNE   OFFVAL5                                                          
         CLI   8(R2),C'*'          AND FIELD STARTS WITH *                      
         BE    OFFVAL8                                                          
                                                                                
OFFVAL5  DS    0H                                                               
         MVI   KEY,X'04'                                                        
         MVC   KEY+23(2),TWAAGY                                                 
         MVC   KEY+25(2),8(R2)                                                  
         GOTO1 GENEX,P1,(0,ROFFNAME-ROFFREC),KEY+25,ROFF,2,0,0                  
         B     COMMXIT                                                          
OFFVAL8  DS    0H                                                               
         MVI   KEY,X'38'                                                        
         MVC   KEY+19(2),TWAAGY                                                 
         MVC   KEY+21(2),=C'OF'                                                 
         MVC   KEY+23(4),9(R2)                                                  
         OC    KEY+23(4),SPACES                                                 
                                                                                
         GOTO1 DIRREAD,P1          LOOK FOR EXACT KEY                           
         CLI   P3-P1(R1),0         NOT FOUND?                                   
         BNE   COMMBAD             DMGR ERROR                                   
                                                                                
OFFVALX  DS    0H                                                               
         MVC   ROFF,=C'* '                                                      
         MVC   ROFFSET(4),KEY+23                                                
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
*- SALESPERSON VALIDATION                                                       
SMANVAL  EQU   *                             OFFICE - FIND BITS                 
         CLI   RRRGFLAG,C'Y'       SPECIAL IF RRG                               
         BNE   SMANVAL5                                                         
         CLI   8(R2),C'*'          AND FIELD STARTS WITH *                      
         BE    SMANVAL8                                                         
                                                                                
SMANVAL5 DS    0H                                                               
         MVI   KEY,X'06'                                                        
         MVC   KEY+22(2),TWAAGY                                                 
         MVC   KEY+24(3),8(R2)                                                  
         GOTO1 GENEX,P1,(0,RSALNAME-RSALREC),KEY+24,RSMAN,3,0,0                 
         B     COMMXIT                                                          
                                                                                
SMANVAL8 DS    0H                                                               
         MVI   KEY,X'38'                                                        
         MVC   KEY+19(2),TWAAGY                                                 
         MVC   KEY+21(2),=C'SP'                                                 
         MVC   KEY+23(4),9(R2)                                                  
         OC    KEY+23(4),SPACES                                                 
                                                                                
         GOTO1 DIRREAD,P1          LOOK FOR EXACT KEY                           
         CLI   P3-P1(R1),0         NOT FOUND?                                   
         BNE   COMMBAD             DMGR ERROR                                   
                                                                                
SMANVALX DS    0H                                                               
         MVC   RSMAN,=C'*  '                                                    
         MVC   RSETSAL(4),KEY+23                                                
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
*- ADVERTISER VALIDATION                                                        
ADVVAL   EQU   *                             OFFICE - FIND BITS                 
         CLI   RRRGFLAG,C'Y'       SPECIAL IF RRG                               
         BNE   ADVVAL10                                                         
         CLI   8(R2),C'*'          AND FIELD STARTS WITH *                      
         BE    ADVVAL20                                                         
                                                                                
ADVVAL10 DS    0H                                                               
         MVI   KEY,X'08'                                                        
         MVC   KEY+21(4),8(R2)                                                  
         MVC   KEY+25(2),TWAAGY                                                 
         GOTO1 GENEX,P1,(0,RADVNAME-RADVREC),KEY+21,RADV,4,0,0                  
         B     COMMXIT                                                          
                                                                                
ADVVAL20 DS    0H                                                               
         GOTO1 =A(GETREP),DMCB,(RC),RR=SUBRELO                                  
*                                  CHECK IF REP A SUBSIDIARY                    
         BZ    ADVVAL30                                                         
         XC    KEY,KEY             IF REP A SUBSIDIARY,                         
         MVI   KEY,X'38'             VALIDATE USING MASTER REP                  
*                                                                               
         L     RE,AIOADDR2         SET ADDRESSABILITY TO RECORD                 
         USING RREPREC,RE                                                       
*                                                                               
         MVC   KEY+19(2),RREPMAST                                               
         DROP  RE                                                               
         B     ADVVAL40                                                         
                                                                                
ADVVAL30 DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'38'                                                        
         MVC   KEY+19(2),TWAAGY                                                 
                                                                                
ADVVAL40 DS    0H                                                               
         MVC   KEY+21(2),=C'AD'                                                 
         MVC   KEY+23(4),9(R2)                                                  
         OC    KEY+23(4),SPACES                                                 
                                                                                
         GOTO1 DIRHIGH,P1                                                       
         CLC   KEY(27),KEYSAVE                                                  
         BNE   COMMBAD             KEY NOT FOUND                                
                                                                                
ADVVALX  DS    0H                                                               
         MVC   RADV,=C'*   '                                                    
         MVC   RSETADV(4),KEY+23                                                
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
*- PRODUCT VALIDATION                                                           
PRDVAL   EQU   *                             OFFICE - FIND BITS                 
         MVI   KEY,X'09'                                                        
         MVC   KEY+18(4),RADV                                                   
         MVC   KEY+22(3),8(R2)                                                  
         MVC   KEY+25(2),TWAAGY                                                 
         GOTO1 GENEX,P1,(0,RPRDNAME-RPRDREC),KEY+22,RPRO,3,0,0                  
         BNZ   COMMBAD                                                          
PRDV0040 EQU   *                                                                
         GOTO1 FILREAD                                                          
*                                                                               
         L     RE,AIOADDR2         SET ADDRESSABILITY TO RECORD                 
         USING RPRDREC,RE                                                       
*                                                                               
         MVC   NETCON(8),RPRDNET#                                               
         DROP  RE                                                               
         B     COMMGOOD                                                         
         SPACE 2                                                                
*                                                                               
*- PRODUCT CLASS VALIDATION                                                     
PROCLVAL EQU   *                                                                
         MVI   KEY,X'0D'                                                        
         MVC   KEY+23(2),TWAAGY                                                 
         MVC   KEY+25(2),8(R2)                                                  
         GOTO1 GENEX,P1,(0,RCLSNAME-RCLSREC),KEY+25,RPROCL,2,0,0                
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
*- PRODUCT CATEGORY VALIDATION                                                  
PROCAVAL EQU   *                                                                
         MVI   KEY,X'0F'                                                        
         MVC   KEY+23(2),TWAAGY                                                 
         MVC   KEY+25(2),8(R2)                                                  
         GOTO1 GENEX,P1,(0,RCTGNAME-RCTGREC),KEY+25,RPROCA,2,0,0                
         BNZ   COMMXIT                                                          
*  NOTE:  CATEGORY CODE NO LONGER HAS AN EXPANSION.  IT IS USING THE            
*        CATEGORY RECORD TO PROVIDE THE CLASS CODE.  WITH NO EXPAN-             
*        SION, THE CATEGORY RECORD IS NOT READ.  NOW, THE READ MUST             
*        BE DONE LOCALLY.                                                       
*                                                                               
         GOTO1 FILREAD,P1          READ THE RECORD                              
*                                                                               
*- IF CLASS NOT GIVEN, USE CLASS FROM CATEGORY RECORD                           
*                                                                               
         L     RE,AIOADDR2         SET ADDRESSABILITY TO RECORD                 
         USING RCTGREC,RE                                                       
*                                                                               
         CLC   RPROCL,SPACES                                                    
         BNE   PCTG100                                                          
         MVC   RPROCL,RCTGCLSS                                                  
*                                                                               
*- CHECK FOR CLASS CONFLICT                                                     
PCTG100  EQU   *                                                                
*                                                                               
         CLC   RPROCL,RCTGCLSS                                                  
         BE    COMMGOOD                                                         
*                                                                               
         DROP  RE                                                               
*                                                                               
         MVI   ERRCNTL,ECMSGNUM                                                 
         MVI   ERRDATA,CLSCONF                                                  
         B     COMMBAD                                                          
         SPACE 2                                                                
*                                                                               
*- DIVISION/TEAM VALIDATION                                                     
SDIVVAL  EQU   *                                                                
         MVI   KEY,X'05'                                                        
         MVC   KEY+23(2),TWAAGY                                                 
         MVC   KEY+25(2),8(R2)                                                  
         GOTO1 GENEX,P1,(0,RTEMDVNM-RTEMREC),KEY+25,RSDIV,2,0,0                 
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
*- AGENCY/AGENCY OFFICE.                                                        
*                                                                               
*  FORMAT:  AAAA,OO                                                             
*           AAAA-OO                                                             
*           AAAAOO       (POSITIONAL)                                           
*                                                                               
*  WHERE AAAA = 1-4 CHARACTER AGENCY CODE                                       
*        OO   = 1-2 OPTIONAL AGENCY OFFICE CODE. (DEFAULT TO BLANK)             
*                                                                               
AGYVAL   EQU   *                             AGENCY - FIND BITS                 
         CLC   =C'T=',8(R2)        TERRITORY FILTER REQUEST?                    
         BNE   AGVA0020            NO                                           
         CLI   5(R2),4             YES - FIELD LENGTH = 4 CHARS?                
         BNE   COMMBAD             NO  - CONSIDER IT AN ERROR                   
         B     AGVA0100            YES - PROCESS TERRITORY FILTER               
AGVA0020 EQU   *                                                                
         XC    SCANBLOK(LSCANBLK),SCANBLOK                                      
         BAS   RE,DELIMITR                                                      
         GOTO1 =V(SCANNER),P1,(R2),(2,SCANBLOK),RR=SUBRELO                      
*                                                                               
         CLI   P2,0                                                             
         BE    COMMBAD             NO INPUT IS ERROR                            
*                                                                               
         CLI   P2,2                                                             
         BH    COMMBAD             MORE THAN 2 IPT FIELDS IS ERROR              
*                                                                               
*- MOVE FIELDS DIRECTLY TO REQUEST CARD.                                        
         LA    RF,SCANBLK2+12      ASSUME AAAA,OO FORMAT                        
         CLI   P2,2                                                             
         BE    AGVA0180                                                         
*                                                                               
         CLI   SCANBLK1+0,1        INPUT = 1 CHARACTER?                         
         BNE   AGVA0040            NO                                           
         CLI   SCANBLK1+12,C'*'    YES - IS VALUE AN ASTERISK?                  
         BNE   COMMBAD             NO  - IT'S AN ERROR                          
         L     RF,AREQNTRY         A(REQUEST ENTRY)                             
         TM    RQCNTL2(RF),RQ2RRG  RRG REQUEST?                                 
         BNO   COMMBAD             NO  - IT'S AN ERROR                          
         MVI   RAGY,C'*'           SET REQUEST CARD FOR '*'                     
         B     COMMGOOD            EXIT - SET CC TO ACCEPTED                    
                                                                                
AGVA0040 EQU   *                   SPECIAL FOR SET RECORD                       
         CLI   RRRGFLAG,C'Y'       IF FORMAT IS *NAME                           
         BNE   AGVA0160                                                         
         CLI   8(R2),C'*'                                                       
         BNE   AGVA0160                                                         
                                                                                
         GOTO1 =A(GETREP),DMCB,(RC),RR=SUBRELO                                  
*                                  CHECK IF REP A SUBSIDIARY                    
         BZ    AGVA0060                                                         
         XC    KEY,KEY             IF REP A SUBSIDIARY,                         
         MVI   KEY,X'38'             VALIDATE USING MASTER                      
*                                                                               
         L     RE,AIOADDR2         SET ADDRESSABILITY TO RECORD                 
         USING RREPREC,RE                                                       
*                                                                               
         MVC   KEY+19(2),RREPMAST                                               
         DROP  RE                                                               
         B     AGVA0080                                                         
                                                                                
AGVA0060 DS    0H                                                               
         XC    KEY,KEY             IF REP A SUBSIDIARY,                         
         MVI   KEY,X'38'                                                        
         MVC   KEY+19(2),TWAAGY                                                 
                                                                                
AGVA0080 DS    0H                                                               
         MVC   KEY+21(2),=C'AG'                                                 
         MVC   KEY+23(4),9(R2)                                                  
         OC    KEY+23(4),SPACES                                                 
                                                                                
         GOTO1 DIRREAD,P1          LOOK FOR EXACT KEY                           
         CLI   P3-P1(R1),0         NOT FOUND?                                   
         BNE   COMMBAD             DMGR ERROR                                   
                                                                                
         MVC   RAGY(6),=C'*     '                                               
         MVC   RSETAGY(4),KEY+23                                                
         B     COMMXIT                                                          
AGVA0100 DS    0H                                                               
*                                                                               
*                                  PROCESS TERRITORY FILTER                     
*                                                                               
         GOTO1 =A(GETREP),DMCB,(RC),RR=SUBRELO                                  
*                                  CHECK IF REP A SUBSIDIARY                    
         BZ    AGVA0120                                                         
         XC    KEY,KEY             IF REP A SUBSIDIARY,                         
         MVI   KEY,X'3D'             VALIDATE USING MASTER                      
*                                                                               
         L     RE,AIOADDR2         SET ADDRESSABILITY TO RECORD                 
         USING RREPREC,RE                                                       
*                                                                               
         MVC   KEY+23(2),RREPMAST                                               
         DROP  RE                                                               
         B     AGVA0140                                                         
                                                                                
AGVA0120 DS    0H                                                               
         XC    KEY,KEY             IF REP A SUBSIDIARY,                         
         MVI   KEY,X'3D'                                                        
         MVC   KEY+23(2),TWAAGY                                                 
                                                                                
AGVA0140 DS    0H                                                               
         MVC   KEY+25(2),10(R2)    INSERT TERRITORY CODE                        
                                                                                
         GOTO1 DIRREAD,P1          LOOK FOR EXACT KEY                           
         CLI   P3-P1(R1),0         NOT FOUND?                                   
         BNE   COMMBAD             DMGR ERROR                                   
                                                                                
         MVC   RAGY(6),=C'+     '  SET TERRITORY IDENTIFIER                     
         MVC   RSETTER(2),KEY+25                                                
         B     COMMXIT                                                          
                                                                                
AGVA0160 DS    0H                                                               
         LA    RF,SCANBLK1+12+4    ASSUME AAAAOO FORMAT                         
         CLI   SCANBLK1+0,4                                                     
         BH    AGVA0180                                                         
*                                                                               
         LA    RF,SPACES           NO AGENCY OFFICE                             
*                                                                               
AGVA0180 MVC   RAGYO,0(RF)         AGY OFFICE CODE OR BLANKS                    
*                                                                               
         MVC   RAGY,SCANBLK1+12    AGENCY CODE                                  
*                                                                               
         MVI   KEY,X'0A'                                                        
         MVC   KEY+19(4),RAGY                                                   
         MVC   KEY+23(2),RAGYO                                                  
         MVC   KEY+25(2),TWAAGY                                                 
         GOTO1 GENEX,P1,(0,RAGYNAM1-RAGYREC),0,0,0,0,0                          
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
*- VALIDATE STATION                                                             
STAVAL   EQU   *                                                                
         CLI   RRRGFLAG,C'Y'       SPECIAL IF RRG                               
         BNE   STAV090                                                          
         CLI   8(R2),C'*'          AND FIELD STARTS WITH *                      
         BNE   STAV090             NOT SPECIAL                                  
*        GOTO1 =A(GETREP),DMCB,(RC),RR=SUBRELO                                  
*                                  CHECK IF REP A SUBSIDIARY                    
*        BZ    STAV030                                                          
*        XC    KEY,KEY             IF REP A SUBSIDIARY,                         
*        MVI   KEY,X'38'             VALIDATE USING MASTER REP                  
*        MVC   KEY+19(2),RREPMAST                                               
*        B     STAV040                                                          
                                                                                
STAV030  DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'38'                                                        
         MVC   KEY+19(2),TWAAGY                                                 
                                                                                
STAV040  DS    0H                                                               
         MVC   KEY+21(2),=C'ST'                                                 
         MVC   KEY+23(4),9(R2)                                                  
         OC    KEY+23(4),SPACES                                                 
                                                                                
         GOTO1 DIRREAD,P1          LOOK FOR EXACT KEY                           
         CLI   P3-P1(R1),0         NOT FOUND?                                   
         BNE   COMMBAD             DMGR ERROR                                   
         MVI   R2CONTIN,C'+'       INSERT CONTINUATION INDICATOR                
         MVC   RSTA,=C'*    '                                                   
         MVC   RSETSTA(4),KEY+23                                                
         B     COMMXIT                                                          
         SPACE 2                                                                
STAV090  EQU   *                                                                
         BAS   RE,STAKEY           PARSE STA IPT/BLD KEY                        
         GOTO1 GENEX,P1,(0,RSTAMKT-RSTAREC),KEY+22,RSTA,5,0,0                   
         BNZ   COMMXIT                                                          
*                                                                               
         L     RE,AIOADDR2         SET ADDRESSABILITY TO RECORD                 
         USING RSTAREC,RE                                                       
         CLI   RSTAKSTA+4,C'C'     COMBO STATION REQUEST?                       
         BNE   STAV095             NO                                           
         CLI   IAMAMAST,C'Y'       MASTER REQUEST?                              
         BNE   STAV095             NO                                           
         GOTO1 PUTCURS,P1,VSTATION,NSTAMAST,L'NSTAMAST                          
         B     COMMBAD                                                          
NSTAMAST DC    C'NO COMBO STATION AT MASTER LEVEL'                              
         DS    0F                                                               
STAV095  EQU   *                                                                
*                                                                               
*                                                                               
*- IF GROUP/SUBGROUP NOT GIVEN, GET FROM STATION RECORD                         
         CLC   RGROUP(2),SPACES                                                 
         BNE   STAV100                                                          
         CLI   IAMAMAST,C'Y'       MASTER REQUEST?                              
         BE    COMMGOOD            YES - DON'T REPLACE RGROUP                   
         MVC   RGROUP(2),RSTAGRUP                                               
*                                                                               
*- CHECK FOR CLASS CONFLICT                                                     
STAV100  EQU   *                                                                
         CLC   RGROUP(2),RSTAGRUP                                               
         BE    COMMGOOD                                                         
         DROP  RE                                                               
*                                                                               
         MVI   ERRCNTL,ECMSGNUM                                                 
         MVI   ERRDATA,GRPCONF                                                  
         B     COMMBAD                                                          
         SPACE 2                                                                
*                                                                               
*- STAKEY -- PARSE STATION FIELD INPUT (6 CHARS)                                
*            AND BUILD KEY FOR FILE READING.                                    
STAKEY   NTR1                                                                   
         MVI   KEY,X'02'           RECORD ID                                    
         MVC   KEY+20(2),TWAAGY                                                 
         OC    KEY+22(5),SPACES                                                 
*                                                                               
         ZIC   R0,5(R2)            PARSE OUT POSSIBLE '-'                       
         LA    RF,8(R2)                                                         
         LA    RE,KEY+22                                                        
STAK0220 CLI   0(RF),C'-'                                                       
         BNE   STAK0240                                                         
*                                                                               
         LA    RF,1(RF)            SKIP THE '-'                                 
         LA    RE,KEY+26           BAND MUST GO HERE                            
         BCTR  R0,0                LESS 1 ON IPT LEN (THE '-')                  
         LTR   R0,R0                                                            
         BZ    STAK0250            NOTHING AFTER THE '-' (WABC- )               
         LA    R0,1                OUT OF LOOP AFTER NEXT MOVE.                 
*                                                                               
STAK0240 MVC   0(1,RE),0(RF)       MOVE DATA TO KEY                             
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,STAK0220                                                      
*                                                                               
*- IF BAND = 'T' (TELEVISION) THEN BLANK OUT BAND IN KEY.                       
STAK0250 EQU   *                                                                
         CLI   KEY+26,C'T'                                                      
         BNE   STAK0260                                                         
         MVI   KEY+26,C' '         TV IS BLANK ON FILE                          
*                                                                               
STAK0260 EQU   *                   FINI                                         
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
*- ORBLANKS -- 'OR' BLANKS INTO SCREEN FIELD                                    
*  INPUT:  R2 = A(FIELD HEADER)                                                 
ORBLANKS EQU   *                                                                
         ZIC   RF,0(R2)                                                         
         SH    RF,=H'8'                                                         
         TM    1(R2),X'02'         EXTENDED HEADER?                             
         BZ    ORBLNK10                                                         
         SH    RF,=H'8'                                                         
ORBLNK10 EQU   *                                                                
         BCTR  RF,0                LESS 1 FOR THE 'EX'                          
         EX    RF,ORBLNK20                                                      
         BR    RE                                                               
ORBLNK20 OC    8(0,R2),SPACES                                                   
         SPACE 2                                                                
SPACES   DC    CL80' '                                                          
*                                                                               
*- GROUP/SUBGROUP VALIDATION                                                    
GSGVAL   EQU   *                                                                
         CLI   RRRGFLAG,C'Y'       SPECIAL IF RRG                               
         BNE   GSGVAL10                                                         
         CLI   8(R2),C'*'          AND FIELD STARTS WITH *                      
         BE    GSGVAL20                                                         
                                                                                
GSGVAL10 DS    0H                                                               
         MVI   KEY,X'07'                                                        
         MVC   KEY+23(2),TWAAGY                                                 
         MVC   KEY+25(2),8(R2)                                                  
         GOTO1 GENEX,P1,(0,RGRPNAME-RGRPREC),KEY+25,RGROUP,2,0,0                
         B     COMMXIT                                                          
                                                                                
GSGVAL20 DS    0H                  IDENTIFIER STARTS WITH '*'                   
         GOTO1 =A(GETREP),DMCB,(RC),RR=SUBRELO                                  
*                                  CHECK IF REP A SUBSIDIARY                    
         BZ    GSGVAL30                                                         
         XC    KEY,KEY             IF SUBSIDIARY, VALIDATE USING MASTER         
         MVI   KEY,X'38'                                                        
*                                                                               
         L     RE,AIOADDR2         SET ADDRESSABILITY TO RECORD                 
         USING RREPREC,RE                                                       
*                                                                               
         MVC   KEY+19(2),RREPMAST                                               
         DROP  RE                                                               
         B     GSGVAL40                                                         
                                                                                
GSGVAL30 DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'38'                                                        
         MVC   KEY+19(2),TWAAGY                                                 
                                                                                
GSGVAL40 DS    0H                                                               
         MVC   KEY+21(2),=C'GS'                                                 
         MVC   KEY+23(4),9(R2)                                                  
         OC    KEY+23(4),SPACES                                                 
                                                                                
         GOTO1 DIRREAD,P1          LOOK FOR EXACT KEY                           
         CLI   P3-P1(R1),0         NOT FOUND?                                   
         BNE   COMMBAD             DMGR ERROR                                   
                                                                                
GSGVALX  DS    0H                                                               
         MVC   RGROUP(2),=C'* '                                                 
         MVC   RSETGSG(4),KEY+23                                                
         B     COMMXIT                                                          
         EJECT                                                                  
*                                                                               
*- CONTRACT NUMBER                                                              
CONVAL   EQU   *          CONTRACT FIND BITS                                    
         GOTO1 =A(RCONVAL),DMCB,(RC),(R2),RR=SUBRELO                            
         BNZ   COMMBAD                                                          
         B     COMMGOOD                                                         
*                                                                               
*- CONTRACT NUMBER ONLY OR FILTERS ONLY                                         
*                                                                               
*  THIS APPLIES TO THE '10' ONLY.  CONFLICT BETWEEN STANDARD FILTERS            
*        AND A SINGLE CONTRACT NUMBER REQUIRE SPECIAL HANDLING.                 
*     BILL UHR.  NOV 1990.                                                      
*                                                                               
LREQFLD1 EQU   RSEQ-ROFFR                L(FILTERS ON REQUEST CARD)             
LREQFLD2 EQU   ROPTN3-ROPTN2             L(FILTERS ON REQUEST CARD)             
LREQFLD3 EQU   RCONTIN+1-ROPTN           L(FILTERS ON REQUEST CARD)             
LREQFLD4 EQU   RSTRD-ROFFR               L(FILTERS ON REQUEST CARD)             
CONVAL2  EQU   *          CONTRACT FIND BITS                                    
CONV21   EQU   *                                                                
         CLC   ROFFR(LREQFLD1),SPACES    ANY FILTERS - 1ST PART                 
         BNE   CONV22                    YES - CNTRCT # NOT ALLOWED             
         CLC   ROPTN2(LREQFLD2),SPACES   ANY FILTERS - 2ND PART                 
         BE    CONV23                    NO  - CNTRCT # ALLOWED                 
CONV22   EQU   *                                                                
         GOTO1 PUTCURS,P1,VCONTRC2,BANFILTS,L'BANFILTS                          
         B     COMMBAD                                                          
CONV23   EQU   *                                                                
         MVI   RSEQ,C'1'                 SET SEQUENCE # FOR REPORT              
         TM    4(R2),X'08'                                                      
         BZ    COMMBAD             MUST BE NUMERIC ONLY                         
         SR    R7,R7                                                            
         IC    R7,5(R2)                                                         
         BCTR  R7,0                                                             
         EX    R7,CNVAPACK2                                                     
         OI    DUB+7,X'0F'                                                      
         UNPK  RCON,DUB                                                         
         PACK  DUB,RCON                                                         
         OI    DUB+7,X'0F'                                                      
         ZAP   WORK(8),=P'99999999'                                             
         SP    WORK(8),DUB                                                      
         UNPK  DUB,WORK(8)                                                      
         OI    DUB+7,X'F0'                                                      
         PACK  WORK(5),DUB(9)                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'8C'                                                        
         MVC   KEY+23(4),WORK                                                   
         MVC   KEY+21(2),TWAAGY                                                 
         GOTO1 DIRHIGH                                                          
         BNZ   COMMBAD             DMGR ERROR                                   
         CLC   KEY(27),KEYSAVE                                                  
         BE    CONV220                                                          
*                                                                               
         MVI   ERRCNTL,ECMSGNUM    RECORD NOT FOUND MSG.                        
         MVI   ERRDATA,NOTFOUND                                                 
         B     COMMBAD      NOT FOUND                                           
*                                                                               
CONV220  EQU   *                                                                
         GOTO1 FILREAD             READ IN CONTRACT                             
*                                                                               
*                                                                               
         L     RE,AIOADDR2         SET ADDRESSABILITY TO RECORD                 
         USING RCONREC,RE                                                       
*                                                                               
         MVC   RAGY,RCONKAGY       MOVE FIELDS FROM CONTRACT TO REQUEST         
         MVC   RAGYO,RCONKAOF                                                   
         MVC   RADV,RCONKADV                                                    
         MVC   RSTA,RCONKSTA                                                    
         MVC   RSDIV(2),RCONTEM                                                 
         MVC   RSMAN,RCONSAL                                                    
         DROP  RE                                                               
         B     COMMGOOD                                                         
*                                                                               
CNVAPACK2 PACK DUB,8(0,R2)        EXECUTED                                      
         EJECT                                                                  
*                                                                               
*- AFFILIATE                                                                    
AFFVAL   EQU   *                                                                
         LA    R7,AFFTAB                                                        
AFFVO    CLC   8(3,R2),0(R7)       SCREEN -VS- TBL                              
         BE    AFFV1                                                            
         LA    R7,3(R7)                                                         
         CLI   0(R7),0                                                          
         BE    COMMBAD                                                          
         B     AFFVO                                                            
*                                                                               
AFFV1    MVC   R2AFFL(3),8(R2)                                                  
         B     COMMGOOD                                                         
*                                                                               
AFFTAB   DS    0C                                                               
         DC    C'ABC'                                                           
         DC    C'CBS'                                                           
         DC    C'NBC'                                                           
         DC    C'IND'                                                           
         DC    C'FOX'                                                           
         DC    C'TEL'                                                           
         DC    C'UNI'                                                           
         DC    C'GAL'                                                           
         DC    C'UPN'                                                           
         DC    C'WBT'                                                           
         DC    X'00'                                                            
         DS    0H                                                               
         EJECT                                                                  
*                                                                               
*- CONTRACT TYPE.                                                               
*  1 OR 2 CHAR INPUT.   'T' OR '*T'  (*=EXCLUDE)                                
*                                                                               
*  EXCLUDES PASSED AS LOWER CASE.                                               
*                                                                               
CTYPVAL  EQU   *                                                                
         CLI   5(R2),2             TWO-CHARACTER LENGTH (EXCLUDE)?              
         BE    CTYPV20             YES                                          
         BL    CTYPV18             NO  - SINGLE CHARACTER INPUT                 
         CLI   RRRGFLAG,C'Y'       RRG RUN?                                     
         BNE   CTYPV18             NO  - TREAT AS NORMAL VALIDATION             
         CLI   8(R2),C'*'          SET REQUEST?                                 
         BNE   COMMBAD             NO  - ERROR                                  
*                                                                               
*    CONTRACT TYPE IS NOT A MASTER RECORD, THEREFORE NO MASTER SET              
*                                                                               
****     GOTO1 =A(GETREP),DMCB,(RC),RR=SUBRELO                                  
****                               CHECK IF REP A SUBSIDIARY                    
****     BZ    CTYP010             NOT MASTER                                   
****     MVC   KEY+19(2),RREPMAST  INSERT MASTER REP                            
****     B     CTYP015                                                          
CTYP010  EQU   *                                                                
         MVC   KEY+19(2),TWAAGY    INSERT MASTER REP FOR CONTYPE                
CTYP015  EQU   *                                                                
         MVI   KEY,X'38'                                                        
         MVC   KEY+21(2),=C'CT'                                                 
         MVC   KEY+23(4),9(R2)                                                  
         OC    KEY+23(4),SPACES                                                 
                                                                                
         GOTO1 DIRREAD,P1          LOOK FOR EXACT KEY                           
         CLI   P3-P1(R1),0         NOT FOUND?                                   
         BNE   COMMBAD             DMGR ERROR                                   
                                                                                
         MVI   RCONTYP,C'*'                                                     
         MVC   RCTYSET(4),KEY+23                                                
         B     COMMXIT                                                          
         SPACE 2                                                                
CTYPV18  EQU   *                                                                
*                                                                               
*- CAN BE ANY CHARACTER EXCEPT '*'                                              
         CLI   8(R2),C'*'                                                       
         BE    COMMBAD                                                          
         MVC   RCONTYP,8(R2)                                                    
         B     COMMGOOD                                                         
*                                                                               
CTYPV20  CLI   8(R2),C'*'          * MEANS EXCLUDE                              
         BNE   COMMBAD                                                          
         MVC   RCONTYP,9(R2)       PICK UP 2ND INPUT BYTE.                      
*                                                                               
         CLI   RCONTYP,C'*'        CAN'T BE '*'                                 
         BE    COMMBAD                                                          
         NI    RCONTYP,X'BF'       MAKE IT LOWER CASE (EXCLUDE)                 
*                                                                               
         B     COMMGOOD                                                         
         EJECT                                                                  
*                                                                               
*- TVB REGION FIELD                                                             
*                                                                               
*- TVB REGION CODE.  'XX' MUST BE IN TVBLST.                                    
*                                                                               
TVBVAL   EQU   *                                                                
*                                                                               
         CLI   5(R2),2             TVB NO MORE THAN 2                           
         BH    COMMBAD                                                          
         LA    RE,TVBLST                                                        
TVBV10   CLI   0(RE),X'FF'         EOT?                                         
         BE    COMMBAD                                                          
*                                                                               
         CLC   8(2,R2),0(RE)       SCREEN MATCHES TABLE?                        
         BE    TVBV20                                                           
*                                                                               
         LA    RE,20(RE)           NEXT TVB LIST ENTRY                          
         B     TVBV10                                                           
*                                                                               
*- MOVE DATA TO RQST REC                                                        
TVBV20   EQU   *                                                                
         MVC   RTVB(2),8(R2)                                                    
         B     COMMGOOD                                                         
*  NO LONGER PUT UP TVB EXPANSION!!!                                            
*                                                                               
*- PUT UP TVB EXPANSION                                                         
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         FOUT  (R2)                                                             
         LR    R3,RE               SAVE A(TVB TBL ENTRY) FROM GOTO1             
         GOTO1 ACLRSCRN,P1,(R2),(R2)                                            
         MVC   8(18,R2),2(R3)                                                   
         B     COMMGOOD                                                         
         SPACE 3                                                                
       ++INCLUDE RETVBTAB                                                       
         DS    H'0'                ALIGNMENT                                    
*                                                                               
SUBRELO  DS    A                   SUBOVERLAY RELOCATION FACTOR                 
*                                                                               
         EJECT                                                                  
*                                                                               
*- MARKET CODE.  MUST BE ON FILE.                                               
MKTVAL   EQU   *                                                                
*                                                                               
         CLI   RRRGFLAG,C'Y'       SPECIAL IF RRG                               
         BNE   MKTV0050                                                         
         CLI   8(R2),C'*'          AND FIELD STARTS WITH *                      
         BE    MKTV0100                                                         
*                                                                               
MKTV0050 EQU   *                                                                
         CLI   5(R2),4             MARKET NO MORE THAN 4                        
         BH    COMMBAD                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,X'2B'                                                        
         MVC   KEY+21(2),TWAAGY                                                 
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,MKTVALEX                                                      
         GOTO1 GENEX,P1,(0,ROWNNAME-ROWNREC),8(R2),RMKT,4,0,0                   
         B     COMMXIT                                                          
MKTVALEX MVC   KEY+23(0),8(R2)                                                  
         SPACE 3                                                                
MKTV0100 EQU   *                                                                
         GOTO1 =A(GETREP),DMCB,(RC),RR=SUBRELO                                  
*                                  CHECK IF REP A SUBSIDIARY                    
         BZ    MKTV0150                                                         
         XC    KEY,KEY             IF REP A SUBSIDIARY,                         
         MVI   KEY,X'38'             VALIDATE USING MASTER REP                  
*                                                                               
         L     RE,AIOADDR2         SET ADDRESSABILITY TO RECORD                 
         USING RREPREC,RE                                                       
*                                                                               
         MVC   KEY+19(2),RREPMAST  INSERT MASTER REP CODE                       
         DROP  RE                                                               
         B     MKTV0200                                                         
                                                                                
MKTV0150 DS    0H                                                               
         XC    KEY,KEY             NOT A SUBSIDIARY                             
         MVI   KEY,X'38'                                                        
         MVC   KEY+19(2),TWAAGY    USE SIGNON REP CODE                          
MKTV0200 DS    0H                                                               
         MVC   KEY+21(2),=C'MK'                                                 
         MVC   KEY+23(4),9(R2)                                                  
         OC    KEY+23(4),SPACES                                                 
         GOTO1 DIRREAD,P1          LOOK FOR EXACT KEY                           
         CLI   P3-P1(R1),0         NOT FOUND?                                   
         BNE   COMMBAD             DMGR ERROR                                   
         MVC   RMKT,=C'*   '       SET MARKET SET INDICATOR                     
         MVC   RSETMKT(4),KEY+23                                                
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
*- CREDIT RATING.  MUST BE BETWEEN 1 AND 6.                                     
CREDRAT  EQU   *                                                                
*                                                                               
         CLI   8(R2),C'1'          MUST BE 1 OR MORE                            
         BL    COMMBAD                                                          
         CLI   8(R2),C'6'          MUST BE 6 OR LESS                            
         BH    COMMBAD                                                          
         MVC   R2CREDR,8(R2)                                                    
         B     COMMGOOD                                                         
         SPACE 3                                                                
*                                                                               
*- LIABILITY POSITION COMMENT                                                   
LIABPOS  EQU   *                                                                
*                                                                               
         CLI   5(R2),2           2 CHARS ONLY.                                  
         BNE   COMMBAD                                                          
         CLI   8(R2),C'0'        1ST DIGIT: MUST BE 0 - 9                       
         BL    COMMBAD                                                          
         CLI   8(R2),C'9'                                                       
         BH    COMMBAD                                                          
         CLI   9(R2),C'0'        2ND DIGIT: MUST BE 0 - 9                       
         BL    COMMBAD                                                          
         CLI   9(R2),C'9'                                                       
         BH    COMMBAD                                                          
         CLC   8(2,R2),=C'00'                                                   
         BE    COMMBAD                                                          
         MVC   R2LIABP(2),8(R2)                                                 
         B     COMMGOOD                                                         
*                                                                               
*- OWNER CODE.  MUST BE ON FILE.                                                
OWNERVAL EQU   *                                                                
*                                                                               
         CLI   5(R2),3             OWNER NO MORE THAN 3                         
         BH    COMMBAD                                                          
         MVI   KEY,X'2A'                                                        
         MVC   KEY+22(2),TWAAGY                                                 
         MVC   KEY+24(3),8(R2)                                                  
         GOTO1 GENEX,P1,(0,ROWNNAME-ROWNREC),8(R2),ROWNER,3,0,0                 
         B     COMMXIT                                                          
         SPACE 3                                                                
*                                                                               
*- POINT PERSON CODE.  MUST BE ON FILE.                                         
PPSONVAL EQU   *                                                                
*                                                                               
         CLI   RRRGFLAG,C'Y'       SPECIAL IF RRG                               
         BNE   PPRVAL20                                                         
         CLI   8(R2),C'*'          AND FIELD STARTS WITH *                      
         BE    PPRVAL40                                                         
                                                                                
PPRVAL20 EQU   *                                                                
         CLI   5(R2),3             POINTPERSON NO MORE THAN 3                   
         BH    COMMBAD                                                          
         MVI   KEY,X'31'                                                        
         MVC   KEY+22(2),TWAAGY                                                 
         MVC   KEY+24(3),8(R2)                                                  
         GOTO1 GENEX,P1,(0,ROWNNAME-ROWNREC),8(R2),RPPSON,3,0,0                 
         B     COMMXIT                                                          
PPRVAL40 DS    0H                                                               
         GOTO1 =A(GETREP),DMCB,(RC),RR=SUBRELO                                  
*                                  CHECK IF REP A SUBSIDIARY                    
         BZ    PPRVAL60                                                         
*                                                                               
         L     RE,AIOADDR2         SET ADDRESSABILITY TO RECORD                 
         USING RREPREC,RE                                                       
*                                                                               
         MVC   KEY+19(2),RREPMAST  INSERT MASTER REP                            
         DROP  RE                                                               
         B     PPRVAL80                                                         
PPRVAL60 DS    0H                                                               
         MVC   KEY+19(2),TWAAGY    INSERT LOCAL REP                             
PPRVAL80 DS    0H                                                               
         MVI   KEY,X'38'                                                        
         MVC   KEY+21(2),=C'PP'                                                 
         MVC   KEY+23(4),9(R2)                                                  
         OC    KEY+23(4),SPACES                                                 
                                                                                
         GOTO1 DIRREAD,P1          LOOK FOR EXACT KEY                           
         CLI   P3-P1(R1),0         NOT FOUND?                                   
         BNE   COMMBAD             DMGR ERROR                                   
                                                                                
         MVC   RPPSON,=C'*  '                                                   
         MVC   RPPRSET(4),KEY+23                                                
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
*- MARKET RANK.  MUST BE 1 - 9 INCLUSIVE.                                       
RANKVAL  EQU   *                                                                
*                                                                               
         CLI   5(R2),1           1 CHARS ONLY.                                  
         BNE   COMMBAD                                                          
         CLI   8(R2),C'1'        MUST BE 1 - 9                                  
         BL    COMMBAD                                                          
         CLI   8(R2),C'9'                                                       
         BH    COMMBAD                                                          
         MVC   RRANK(1),8(R2)                                                   
         B     COMMGOOD                                                         
*                                                                               
         SPACE 3                                                                
*                                                                               
*- BUDGET YEAR.  MUST BE 00 - 99 INCLUSIVE.                                     
BYVAL    EQU   *                                                                
*                                                                               
         CLI   5(R2),2           2 CHARS ONLY.                                  
         BNE   COMMBAD                                                          
         CLI   8(R2),C'0'        1ST DIGIT: MUST BE 0 - 9                       
         BL    COMMBAD                                                          
         CLI   8(R2),C'9'                                                       
         BH    COMMBAD                                                          
         CLI   9(R2),C'0'        2ND DIGIT: MUST BE 0 - 9                       
         BL    COMMBAD                                                          
         CLI   9(R2),C'9'                                                       
         BH    COMMBAD                                                          
         MVC   ROPTN4(2),8(R2)                                                  
         B     COMMGOOD                                                         
*                                                                               
         SPACE 3                                                                
*                                                                               
*- OLDREP:   VALIDATES REP ENTERED AGAINST CONTROL FILE                         
*                                                                               
OLDREP   EQU   *                                                                
         GOTO1 =A(VROLDREP),DMCB,(RC),(R2),RR=SUBRELO                           
         BNZ   COMMBAD                                                          
         B     COMMGOOD                                                         
*                                                                               
*- TKOSTAC:  VALIDATES STATION FOR TAKEOVER AUTHORIZATION                       
*                                                                               
TKOSTAC  EQU   *                                                                
         GOTO1 =A(VRTKOSTA),DMCB,(RC),(R2),RR=SUBRELO                           
         BNZ   COMMBAD                                                          
         B     COMMGOOD                                                         
*                                                                               
*- TKOAGYC:  VALIDATES AGENCY AGAINST SOURCE FILE                               
*                                                                               
TKOAGYC  EQU   *                                                                
         GOTO1 =A(VRTKOAGY),DMCB,(RC),(R2),RR=SUBRELO                           
         BNZ   COMMBAD                                                          
         B     COMMGOOD                                                         
*                                                                               
*- TKOSTAC:  VALIDATES OFFICE AGAINST SOURCE FILE                               
*                                                                               
TKOOFFC  EQU   *                                                                
         GOTO1 =A(VRTKOOFF),DMCB,(RC),(R2),RR=SUBRELO                           
         BNZ   COMMBAD                                                          
         B     COMMGOOD                                                         
*                                                                               
CLOSMON  EQU   *                                                                
         GOTO1 =A(VRCLOSMN),DMCB,(RC),(R2),RR=SUBRELO                           
         BNZ   COMMBAD                                                          
         B     COMMGOOD                                                         
*                                                                               
*- NETWORK CONTRACT NUMBER VALIDATION                                           
NCONVAL  EQU   *                                                                
*                                                                               
         GOTO1 =A(NCONVL2),DMCB,(RC),(R2),RR=SUBRELO                            
         BNZ   COMMBAD                                                          
         B     COMMGOOD                                                         
*                                                                               
*                                                                               
*- RANK MAX VALUE.  NUMERIC 1-99.  PASSED AS OPTION 1 AND OPTION 2              
*                                                                               
RKMAXNTR NTR1                      RANK MAX WITH AN NTR                         
RKMAXVAL EQU   *                   RANK MAX                                     
         ZIC   R7,5(R2)            GET FIELD LENGTH                             
         LA    R5,8(R2)            CHECK EACH POSITION FOR NUMBER               
RKMX0010 EQU   *                                                                
         CLI   0(R5),C'0'                                                       
         BL    COMMBAD                                                          
         CLI   0(R5),C'9'                                                       
         BH    COMMBAD                                                          
         LA    R5,1(R5)            BUMP ADDRESS                                 
         BCT   R7,RKMX0010         GO BACK FOR NEXT                             
         ZIC   R7,5(R2)            ALL NUMERIC - RESET LENGTH                   
         BCTR  R7,0                                                             
         EX    R7,RKMPACK                                                       
         CP    DUB,=P'1'                                                        
         BL    COMMBAD             VALID VALUES ARE 1-99                        
         OI    DUB+7,X'0F'                                                      
         LA    RF,RQSNUMH          A(NUMBER FIELD IN HEADER)                    
         CLI   5(RF),2             TWO CHARACTERS ENTERED?                      
         BNE   RKMX04              NO - CHECK FOR 3 CHARACTER INPUT             
         CLC   RQSNUM(2),=C'1K'    STRATEGY SEEDER REQUEST?                     
         BNE   RKMX08              NO  -                                        
         B     RKMX06              YES                                          
*                                                                               
RKMX04   EQU   *                                                                
*                                                                               
         CLI   5(RF),3             THREE CHARACTERS ENTERED?                    
         BNE   RKMX08              NO  - (?) WHAT HAS BEEN ENTERED?             
         CLC   RQSNUM(2),=C'R1K'   YES - STRATEGY SEEDER REQUEST?               
         BNE   RKMX08              NO  -                                        
*                                                                               
RKMX06   EQU   *                                                                
         UNPK  RRANKMAX(2),DUB     GOES INTO THIRD CARD FIELDS                  
         B     COMMGOOD            EXIT                                         
RKMX08   EQU   *                                                                
         UNPK  ROPTN(2),DUB        GOES INTO OPTION1 AND OPTION2                
         B     COMMGOOD                                                         
*                                                                               
RKMPACK  PACK  DUB,8(0,R2)         EXECUTED                                     
         SPACE 2                                                                
*                                                                               
*- DEPTH.  NUMERIC, 1-99.  ** CARD 2 FIELD **                                   
*                                                                               
DEPTHV   EQU   *                                                                
         ICM   R5,3,ROPTN          SAVE FROM RANK MAX                           
         BAS   RE,RKMAXNTR         NUMERIC EDIT                                 
         BNZ   COMMBAD                                                          
         MVC   RDEPTH,ROPTN        MOVE INTO RQST FIELD                         
         STCM  R5,3,ROPTN          PUT BACK OPTION 1, 2                         
         B     COMMGOOD                                                         
         EJECT                                                                  
*                                                                               
*- STATION LIST VALIDATION.   ** CARD 2 FIELDS **                               
*                                                                               
*  BUILDS RSTALST, LIST OF 5 CHAR INTERNAL STATION CODES.                       
*  SETS   RSTALST# TO NUMBER OF ITEMS IN LIST.                                  
*                                                                               
*  ON 1ST FIELD OF LIST, 'FCLIST' BIT SHOULD BE OFF                             
*     (INITIALIZE LIST COUNT)                                                   
*  ALL FOLLOWING FIELDS SHOULD HAVE 'FCLIST' BIT ON.                            
*                                                                               
STALSTV  EQU   *                                                                
         L     RE,AMAPNTRY                                                      
         TM    MAPCNTL(RE),FCLIST                                               
         BNZ   STALST10                                                         
         MVI   RSTALST#,0          INIT BINARY COUNTER                          
*                                                                               
STALST10 EQU   *                                                                
         ZIC   RE,RSTALST#         BUMP # ITEMS IN LIST                         
         LA    RE,1(RE)                                                         
         STC   RE,RSTALST#                                                      
*                                                                               
         BCTR  RE,0                FIND A(STA) IN LIST                          
         MH    RE,=H'5'            5 BYTES/STATION                              
         LA    R3,RSTALST                                                       
         AR    R3,RE               A(STATION CODE OUTPUT AREA)                  
*                                                                               
         BAS   RE,STAKEY           PARSE INPUT/BUILD KEY                        
*                                                                               
*- VALIDATE STATION CODE AND MOVE TO STATION LIST.  NO EXPANSION.               
         GOTO1 GENEX,P1,0,KEY+22,(R3),5,0,0                                     
         B     COMMXIT                                                          
         EJECT                                                                  
*                                                                               
*- BUDGET ALLOCATION LIST VALIDATION.   ** CARD 2 FIELDS **                     
*                                                                               
*  BUILDS R2ALCLST, LIST OF UP TO 5, 4 CHAR DATA PAIRS OF CONTRACT TYPE         
*   AND PERCENT                                                                 
*                                                                               
*  PERCENT MUST TOTAL TO 100                                                    
*                                                                               
ALCLSTV  EQU   *                                                                
*                                                                               
*   ONLY FIRST ENTRY NEEDS INPUT.  REST MAY BE EMPTY                            
*                                                                               
         LA    R3,R2ALCLST                                                      
         LA    R4,5                                                             
ALCL10   EQU   *                                                                
         CLI   0(R3),0                                                          
         BZ    ALCL20                                                           
         CLI   0(R3),C' '                                                       
         BE    ALCL20                                                           
         LA    R3,4(R3)                                                         
         BCT   R4,ALCL10                                                        
         DC    H'0'                                                             
*                                                                               
ALCL20   EQU   *                                                                
         XC    SCANBLOK(LSCANBLK),SCANBLOK                                      
         GOTO1 =V(SCANNER),P1,(R2),(2,SCANBLOK),RR=SUBRELO                      
         CLI   P2,2                                                             
         BNE   COMMBAD             NEED 2 INPUT FIELDS                          
*                                                                               
         CLI   SCANBLK1+0,1        LENGTH OF INPUT                              
         BNE   COMMBAD                                                          
         MVC   0(1,R3),SCANBLK1+12 GET THE CONTRACT TYPE                        
*                                                                               
         CLI   SCANBLK2+0,3        LENGTH OF INPUT                              
         BH    COMMBAD                                                          
         CLI   SCANBLK2+2,X'A0'    NUMERIC/HEX                                  
         BNE   COMMBAD                                                          
         LA    R4,SCANBLK2+12                                                   
         ZIC   R5,SCANBLK2+0                                                    
         BCTR  R5,0                DECREMENT LEN BY 1 FOR EX                    
         EX    R5,ALCLEX1                                                       
         CVB   R4,DUB                                                           
         EDIT  (R4),(3,1(R3)),FILL=0,ZERO=NOBLANK                               
         B     COMMGOOD                                                         
         SPACE 2                                                                
ALCLEX1  PACK  DUB(8),0(0,R4)                                                   
         EJECT                                                                  
*                                                                               
*- BUDGET ALLOCATION RESET LIST VALIDATION.   ** CARD 2 FIELDS **               
*                                                                               
*  BUILDS R2ALCLST, LIST OF UP TO 5, 4 CHAR CONTRACT TYPE FOLLOWED              
*   BY THREE UNUSED POSITIONS WHICH HOLD PERCENTS IN THE 1C REPORT              
*                                                                               
ALCLST2  EQU   *                                                                
*                                                                               
*   ONLY FIRST ENTRY NEEDS INPUT.  REST MAY BE EMPTY                            
*                                                                               
         LA    R3,R2ALCLST                                                      
         LA    R4,5                                                             
ALCL210  EQU   *                                                                
         CLI   0(R3),0                                                          
         BZ    ALCL220                                                          
         CLI   0(R3),C' '                                                       
         BE    ALCL220                                                          
         LA    R3,4(R3)                                                         
         BCT   R4,ALCL210                                                       
         DC    H'0'                                                             
*                                                                               
ALCL220  EQU   *                                                                
         XC    SCANBLOK(LSCANBLK),SCANBLOK                                      
         GOTO1 =V(SCANNER),P1,(R2),(2,SCANBLOK),RR=SUBRELO                      
*                                                                               
         CLI   SCANBLK1+0,1        LENGTH OF INPUT                              
         BNE   COMMBAD                                                          
         MVC   0(1,R3),SCANBLK1+12 GET THE CONTRACT TYPE                        
*                                                                               
         B     COMMGOOD                                                         
         EJECT                                                                  
*                                                                               
*  ROUTINE TO VALIDATE/RETURN DESCRIPTION OF YADR RECORD                        
YADREC   EQU   *                                                                
         MVI   KEY,X'33'                                                        
         MVC   KEY+17,TWAAGY                                                    
         MVI   KEY+19,C' '         PADDING FOR SHORT KEY                        
         MVC   KEY+20(7),KEY+19                                                 
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
*                                                                               
         EX    RF,YADR0080         MOVE BY LENGTH                               
*                                                                               
         GOTO1 GENEX,P1,(0,RRDANMNM-RRDAREC),8(R2),RYADREC,8,20,0               
         B     COMMXIT                                                          
*                                                                               
YADR0080 MVC   KEY+19(0),8(R2)                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
*  ROUTINE TO VALIDATE LABEL RECORD                                             
LABREC   EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'36'                                                        
         MVC   KEY+17(2),TWAAGY                                                 
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,LABR0080         MOVE BY LENGTH                               
*                                                                               
         OC    KEY+19(8),SPACES    PAD WITH SPACES                              
         MVC   RLBLREC,KEY+19                                                   
*                                                                               
         GOTO1 DIRHIGH             GET LABEL RECORD                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   COMMBAD             NO KEY, INVALID INPUT                        
         B     COMMXIT                                                          
*                                                                               
LABR0080 MVC   KEY+19(0),8(R2)                                                  
*                                                                               
         EJECT                                                                  
*  ROUTINE TO VALIDATE DEVELOPMENTAL SALESPERSON CODE                           
DEVSP    EQU   *                                                                
         MVI   KEY,X'3A'                                                        
         MVC   KEY+22(2),TWAAGY                                                 
         MVC   KEY+24(3),8(R2)                                                  
         GOTO1 GENEX,P1,(0,RDSPNAME-RDSPREC),KEY+24,R2DEVSP,3,10,0              
         B     COMMXIT                                                          
         EJECT                                                                  
*  ROUTINE TO VALIDATE NEW BUSINESS DEVELOPMENT TYPE CODE                       
BUSTYP   EQU   *                                                                
         CLI   RRRGFLAG,C'Y'       SPECIAL IF RRG                               
         BNE   BTYVAL5                                                          
         CLI   8(R2),C'*'          AND FIELD STARTS WITH *                      
         BE    BTYVAL8                                                          
                                                                                
BTYVAL5  DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'3B'                                                        
         MVC   KEY+23(2),TWAAGY                                                 
         MVC   KEY+25(2),8(R2)                                                  
         GOTO1 GENEX,P1,(0,RDCTDESC-RDCTREC),KEY+25,R2DEVCTY,2,10,0             
         B     COMMXIT                                                          
BTYVAL8  DS    0H                                                               
         GOTO1 =A(GETREP),DMCB,(RC),RR=SUBRELO                                  
*                                  CHECK IF REP A SUBSIDIARY                    
         BZ    BTYVAL10                                                         
*                                                                               
         L     RE,AIOADDR2         SET ADDRESSABILITY TO RECORD                 
         USING RREPREC,RE                                                       
*                                                                               
         MVC   KEY+19(2),RREPMAST  INSERT MASTER REP                            
         DROP  RE                                                               
         B     BTYVAL20                                                         
BTYVAL10 DS    0H                                                               
         MVC   KEY+19(2),TWAAGY                                                 
BTYVAL20 DS    0H                                                               
         MVI   KEY,X'38'                                                        
         MVC   KEY+21(2),=C'DT'                                                 
         MVC   KEY+23(4),9(R2)                                                  
         OC    KEY+23(4),SPACES                                                 
                                                                                
         GOTO1 DIRREAD,P1          LOOK FOR EXACT KEY                           
         CLI   P3-P1(R1),0         NOT FOUND?                                   
         BNE   COMMBAD             DMGR ERROR                                   
                                                                                
         MVC   R2DEVCTY,=C'* '                                                  
         MVC   RDTYSET(4),KEY+23                                                
         B     COMMXIT                                                          
         SPACE 2                                                                
         EJECT                                                                  
*  ROUTINE TO VALIDATE SPOT LENGTH OPTION                                       
SPLNVAL  EQU   *                                                                
         CLI   8(R2),C'Y'                                                       
         BE    SPLNV10                                                          
         CLI   8(R2),C'N'                                                       
         BNE   COMMBAD                                                          
                                                                                
SPLNV10  DS    0H                                                               
         MVC   RSPLNOPT,8(R2)                                                   
         B     COMMXIT                                                          
         EJECT                                                                  
*                                                                               
*  ROUTINE TO VALIDATE TARGET AGENCY                                            
TARGAGY  EQU   *                                                                
*                                                                               
*- AGENCY/AGENCY OFFICE.                                                        
*                                                                               
*  FORMAT:  AAAA,OO                                                             
*           AAAA-OO                                                             
*           AAAAOO       (POSITIONAL)                                           
*                                                                               
*  WHERE AAAA = 1-4 CHARACTER AGENCY CODE                                       
*        OO   = 1-2 OPTIONAL AGENCY OFFICE CODE. (DEFAULT TO BLANK)             
*                                                                               
         XC    SCANBLOK(LSCANBLK),SCANBLOK                                      
         BAS   RE,DELIMITR                                                      
         GOTO1 =V(SCANNER),P1,(R2),(2,SCANBLOK),RR=SUBRELO                      
         CLI   P2,0                                                             
         BE    COMMBAD             NO INPUT IS ERROR                            
*                                                                               
         CLI   P2,2                                                             
         BH    COMMBAD             MORE THAN 2 IPT FIELDS IS ERROR              
*                                                                               
*- MOVE FIELDS DIRECTLY TO REQUEST CARD.                                        
         LA    RF,SCANBLK2+12      ASSUME AAAA,OO FORMAT                        
         CLI   P2,2                                                             
         BE    TARG100                                                          
*                                                                               
         LA    RF,SCANBLK1+12+4    ASSUME AAAAOO FORMAT                         
         CLI   SCANBLK1+0,4                                                     
         BH    TARG100                                                          
*                                                                               
         LA    RF,SPACES           NO AGENCY OFFICE                             
*                                                                               
TARG100  MVC   RTAGYO,0(RF)        AGY OFFICE CODE OR BLANKS                    
*                                                                               
         MVC   RTAGY,SCANBLK1+12   AGENCY CODE                                  
*                                                                               
         MVI   KEY,X'0A'                                                        
         MVC   KEY+19(4),RTAGY                                                  
         MVC   KEY+23(2),RTAGYO                                                 
         MVC   KEY+25(2),TWAAGY                                                 
         GOTO1 GENEX,P1,(0,RAGYNAM1-RAGYREC),0,0,0,0,0                          
         B     COMMXIT                                                          
         EJECT                                                                  
*                                                                               
*  ROUTINE TO VALIDATE RATING SERVICE                                           
RSERV    EQU   *                                                                
         CLI   5(R2),1             ONLY ONE CHARACTER                           
         BH    COMMBAD                                                          
         CLI   8(R2),C'A'          ARBITRON?                                    
         BE    RS0098              YES                                          
         CLI   8(R2),C'B'          BIRCH?                                       
         BE    RS0098              YES                                          
         CLI   8(R2),C'N'          NEILSEN ?                                    
         BE    RS0098              YES                                          
         CLI   8(R2),C'S'          SRC?                                         
         BE    RS0098              YES                                          
         B     COMMBAD                                                          
RS0098   EQU   *                                                                
         MVC   R2RSVC,8(R2)        MOVE TO REQUEST CARD                         
         B     COMMGOOD                                                         
         SPACE 5                                                                
*                                                                               
*  ROUTINE TO VALIDATE RATING BOOK                                              
RBOOK    EQU   *                                                                
         ZIC   R3,5(R2)            LENGTH OF INPUT                              
         LA    R4,8(R2)            A(DATE INPUT: 1ST POSITION)                  
         LA    R5,R2RBOOK          A(DATE OUTPUT: 1ST POSITION)                 
         CLI   0(R4),C'E'          LEADING 'E' IN DATE INPUT?                   
         BE    RBK0010             YES                                          
         CLI   0(R4),C'P'          LEADING 'P' IN DATE INPUT?                   
         BNE   RBK0016             NO                                           
RBK0010  EQU   *                                                                
         BCTR  R3,0                DEDUCT LEADING CHAR FROM LENGTH              
         MVC   R2RBOOK(1),0(R4)    LOAD 'E' OR 'P' TO CARD                      
         LA    R4,1(R4)            A(DATE INPUT: 2ND POSITION)                  
         LA    R5,R2RBOOK+1        A(DATE OUTPUT: 2ND POSITION)                 
RBK0016  EQU   *                                                                
         GOTO1 DODATE,P1,((R3),(R4)),WORK,1                                     
         BNZ   COMMBAD             INVALID DATE ENTERED                         
         BCTR  R3,0                DEDUCT 1 FOR EXECUTE                         
         EX    R3,RBK0080                                                       
         B     COMMXIT                                                          
*                                                                               
RBK0080  MVC   0(0,R5),0(R4)                                                    
*                                                                               
         SPACE 5                                                                
*                                                                               
*  ROUTINE TO VALIDATE DEMOGRAPHIC CODE                                         
DEMOCD   EQU   *                                                                
         L     RF,APARM                                                         
         L     RF,16(RF)           A(COMFACS) FROM PARAM LIST                   
*                                  FROM MONITOR                                 
         USING COMFACSD,RF                                                      
         MVC   DEMOVAL,CDEMOVAL                                                 
*                                                                               
         DROP  RF                                                               
*                                                                               
         LA    R5,DBDEMOB          A(DEMO INTERFACE AREA)                       
         USING DEMOD,R5                                                         
*                                                                               
         XC    DBLOCK,DBLOCK       INITIALIZE DBLOCK FOR USE                    
*                                                                               
         ST    RF,DBCOMFCS         INSERT A(COMFACS) INTO DBLOCK                
*                                                                               
         MVC   DBSELAGY,TWAAGY     INSERT USER CODE                             
         MVC   DBFILE,=C'PAV'      INSERT DATA TYPE                             
         MVI   DBSELMED,C'T'       INSERT MEDIA                                 
*                                                                               
*   IF THIS IS TO BE USED FOR RADIO, THIS WILL REQUIRE REVISION                 
*      TO HANDLE THE GEOGRAPHICAL QUALIFIER                                     
*                                                                               
         GOTO1 DEMOVAL,P1,(1,(R2)),(1,DEMWORK),DBLOCK                           
         CLI   P2,0                ERROR?                                       
         BE    COMMBAD             YES                                          
*                                                                               
         MVI   R2DEMO,C'0'             FORCE-LOAD GEO QUAL = X'F0'              
         MVC   R2DEMO+1(1),DEMWORK+1   DEMOGRAPHIC QUALIFIER                    
         LA    R2,DEMWORK+2            SET VALID 'EDIT' PARAMS                  
         LA    R3,R2DEMO+2                                                      
         EDIT  (1,(R2)),(3,(R3)),FILL=0                                         
         B     COMMGOOD                                                         
         EJECT                                                                  
*                                                                               
*- DATE VALIDATION ROUTINES.                                                    
*                                                                               
*  AREQMAP = A(CURRENT ENTRY)                                                   
*  R2      = A(FIELD HEADER)                                                    
*                                                                               
*  FIELDS WHERE 2 DATES MAY BE ENTERED, SCANNER IS USED FOR PARSE.              
*                                                                               
*  DELIMITER MAY BE ',' OR '-' BETWEEN DATES.                                   
*                                                                               
*  FORMAT BITS:  X'04' = MMMDD/YY FORMAT                                        
*                X'08' = MMM/YY   FORMAT                                        
*                X'80' = DEFAULT END TO START DATE                              
*                X'40' = DEFAULT START TO END DATE                              
*                X'20' = DEFAULT 1ST DATE TO TODAY                              
*                X'10' = DEFAULT 2ND DATE TO TODAY                              
*                                                                               
         SPACE                                                                  
STRDVAL  EQU   *                                                                
                                                                                
         ZIC   R3,5(R2)            LENGTH OF INPUT                              
         LA    R4,8(R2)            A(DATE TO CHECK)                             
         GOTO1 DODATE,P1,((R3),(R4)),RSTRD,1                                    
         BNZ   STRVAL10            ERROR, BUT CHECK IF RFP                      
         B     COMMXIT                                                          
STRVAL10 EQU   *                                                                
         GOTO1 =A(STRDMOD),DMCB,(RC),RR=RELO                                    
         BNZ   COMMBAD                                                          
         B     COMMXIT                                                          
*                                                                               
*- END DATE ONLY                                                                
*  IF START DATE GIVEN, IT MAY NOT PRECEED END DATE                             
ENDDVAL  EQU   *                                                                
         ZIC   R3,5(R2)            LENGTH OF INPUT                              
         LA    R4,8(R2)            A(DATE TO CHECK)                             
         GOTO1 DODATE,P1,((R3),(R4)),RENDD,2                                    
         BNZ   ENDDV10             ERROR, BUT CHECK IF RFP                      
*                                                                               
         BAS   RE,DATORDER         CHECK FOR END BEFORE START                   
         B     COMMXIT                                                          
*                                                                               
ENDDV10  DS    0H                                                               
         TM    RFPSTAT,RFPINUSE                                                 
         BZ    COMMBAD             NOT RFP, ERROR                               
*                                                                               
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPSYMB                                                
         MVC   QRFPWORK,8(R2)                                                   
         OC    QRFPWORK,SPACES                                                  
         GOTO1 AGETOVLY,P1,('OVRFP',0)                                          
         CLI   QRFPMODE,QRFPOK                                                  
         BNE   COMMBAD             INVALID SYMBOLIC EQUATE                      
*                                                                               
         L     R3,AMAPNTRY                                                      
         TM    MAPFMT(R3),X'08'    FOR DATES WITH MMM/YY                        
         BO    ENDDV15                                                          
*                                                                               
         CLC   QRFPDICT,=Y(E#END)  END                                          
         BE    ENDDV20             INVALID SYMBOLIC EQUATE                      
         CLC   QRFPDICT,=Y(E#PEND) OR PERIOD END                                
         BE    ENDDV20             INVALID SYMBOLIC EQUATE                      
         B     COMMBAD                                                          
*                                                                               
ENDDV15  DS    0H                                                               
         CLC   QRFPDICT,=Y(E#ENDM) OR END MONTH                                 
         BE    ENDDV20             INVALID SYMBOLIC EQUATE                      
         CLC   QRFPDICT,=Y(E#PENDM) OR PERIOD END MONTH                         
         BNE   COMMBAD             INVALID SYMBOLIC EQUATE                      
*                                                                               
ENDDV20  DS    0H                                                               
         MVC   RENDD,SPACES        STORE ESC SEQ IN REC CARD                    
         MVC   RENDD(L'QRFPESC),QRFPESC                                         
         MVI   RENDD+3,6           LEN(END DATE)                                
         B     COMMXIT                                                          
*                                                                               
*- START,END DATES                                                              
STRTENDV EQU   *                                                                
         XC    SCANBLOK(LSCANBLK),SCANBLOK                                      
         BAS   RE,DELIMITR                                                      
         GOTO1 =V(SCANNER),P1,(R2),(2,SCANBLOK),RR=SUBRELO                      
         CLI   P2,0                                                             
         BE    COMMBAD             NO INPUT IS ERROR                            
*                                                                               
         CLI   P2,2                                                             
         BH    COMMBAD             MORE THAN 2 IPT FIELDS IS ERROR              
*                                                                               
*- START DATE IN 1ST SCAN BLOCK                                                 
         LA    RE,XDATE1                                                        
         LA    RF,L'XDATE1                                                      
         STM   RE,RF,AERROR                                                     
*                                                                               
         ZIC   R3,SCANBLK1+0       LENGTH OF INPUT                              
         LA    R4,SCANBLK1+12      A(DATE TO CHECK)                             
         GOTO1 DODATE,P1,((R3),(R4)),RSTRD,1                                    
         BNZ   STENDV10            ERROR, BUT CHECK IF RFP                      
*                                                                               
*- END DATE IN 2ND SCAN BLOCK                                                   
         LA    RE,XDATE2                                                        
         LA    RF,L'XDATE2                                                      
         STM   RE,RF,AERROR                                                     
*                                                                               
         ZIC   R3,SCANBLK2+0       LENGTH OF INPUT                              
         LA    R4,SCANBLK2+12      A(DATE TO CHECK)                             
         GOTO1 DODATE,P1,((R3),(R4)),RENDD,2                                    
         BNZ   STENDV10            ERROR, BUT CHECK IF RFP                      
*                                                                               
         BAS   RE,DATORDER         CHECK FOR END BEFORE START                   
         B     COMMXIT                                                          
*                                                                               
STENDV10 DS    0H                                                               
         TM    RFPSTAT,RFPINUSE    NOT USING RFP, ERROR                         
         BZ    COMMBAD                                                          
*                                                                               
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPSYMB                                                
         MVC   QRFPWORK,8(R2)                                                   
         OC    QRFPWORK,SPACES                                                  
         GOTO1 AGETOVLY,P1,('OVRFP',0)                                          
         CLI   QRFPMODE,QRFPOK                                                  
         BNE   COMMBAD             INVALID SYMBOLIC EQUATE                      
*                                                                               
         L     R3,AMAPNTRY                                                      
         TM    MAPFMT(R3),X'08'    FOR DATES WITH MMM/YY                        
         BO    STENDV15                                                         
*                                                                               
         CLC   QRFPDICT,=Y(E#STEND)  START, END DATES                           
         BE    STENDV20            INVALID SYMBOLIC EQUATE                      
         CLC   QRFPDICT,=Y(E#PSTEND)  START, END DATES                          
         BE    STENDV20            INVALID SYMBOLIC EQUATE                      
         B     COMMBAD                                                          
*                                                                               
STENDV15 DS    0H                                                               
         CLC   QRFPDICT,=Y(E#STENDM)  START, END MONTHS                         
         BE    STENDV20            INVALID SYMBOLIC EQUATE                      
         CLC   QRFPDICT,=Y(E#PSTEDM) PERIOD START, END MONTHS                   
         BNE   COMMBAD             INVALID SYMBOLIC EQUATE                      
*                                                                               
STENDV20 DS    0H                                                               
         MVC   RSTRD,SPACES        STORE ESC SEQ IN REC CARD                    
         MVC   RSTRD(L'QRFPESC),QRFPESC                                         
         MVI   RSTRD+3,12          LEN(START/END DATE)                          
         B     COMMXIT                                                          
*                                                                               
*- START,END DATES WHERE START MUST BE MONDAY, END MUST BE SUNDAY               
STENMS   EQU   *                                                                
         XC    SCANBLOK(LSCANBLK),SCANBLOK                                      
         BAS   RE,DELIMITR                                                      
         GOTO1 =V(SCANNER),P1,(R2),(2,SCANBLOK),RR=SUBRELO                      
         CLI   P2,0                                                             
         BE    COMMBAD             NO INPUT IS ERROR                            
*                                                                               
         CLI   P2,2                                                             
         BH    COMMBAD             MORE THAN 2 IPT FIELDS IS ERROR              
*                                                                               
*- START DATE IN 1ST SCAN BLOCK                                                 
         LA    RE,XDATE1                                                        
         LA    RF,L'XDATE1                                                      
         STM   RE,RF,AERROR                                                     
*                                                                               
         ZIC   R3,SCANBLK1+0       LENGTH OF INPUT                              
         LA    R4,SCANBLK1+12      A(DATE TO CHECK)                             
         GOTO1 DODATE,P1,((R3),(R4)),RSTRD,1                                    
         BNZ   STENDV10            ERROR, BUT CHECK IF RFP                      
*        BNZ   COMMBAD             ERROR                                        
         MVC   P1,P2               O/P DODATE=A(YYMMDD(EBCDIC))                 
         GOTO1 =V(GETDAY),P1,,DUB,RR=SUBRELO                                    
         CLI   P1,X'1'             FIRST DAY  SHOULD BE MONDAY                  
         BNZ   STENDV10            ERROR, BUT CHECK IF RFP                      
*        BNE   COMMBAD                                                          
*                                                                               
*- END DATE IN 2ND SCAN BLOCK                                                   
         LA    RE,XDATE2                                                        
         LA    RF,L'XDATE2                                                      
         STM   RE,RF,AERROR                                                     
*                                                                               
         ZIC   R3,SCANBLK2+0       LENGTH OF INPUT                              
         LA    R4,SCANBLK2+12      A(DATE TO CHECK)                             
         GOTO1 DODATE,P1,((R3),(R4)),RENDD,2                                    
         BNZ   STENDV10            ERROR, BUT CHECK IF RFP                      
*        BNZ   COMMBAD             ERROR                                        
         MVC   P1,P2               O/P DODATE=A(YYMMDD(EBCDIC))                 
         GOTO1 =V(GETDAY),P1,,DUB,RR=SUBRELO                                    
         CLI   P1,X'7'             LAST DAY  SHOULD BE MONDAY                   
         BNZ   STENDV10            ERROR, BUT CHECK IF RFP                      
*        BNE   COMMBAD                                                          
*                                                                               
         BAS   RE,DATORDER         CHECK FOR END BEFORE START                   
         B     COMMXIT                                                          
*                                                                               
         SPACE 2                                                                
*                                                                               
*                                                                               
*- START,END DATES                                                              
EXTRADAT EQU   *                                                                
         XC    SCANBLOK(LSCANBLK),SCANBLOK                                      
         BAS   RE,DELIMITR                                                      
         GOTO1 =V(SCANNER),P1,(R2),(2,SCANBLOK),RR=SUBRELO                      
         CLI   P2,0                                                             
         BE    COMMBAD             NO INPUT IS ERROR                            
*                                                                               
         CLI   P2,2                                                             
         BH    COMMBAD             MORE THAN 2 IPT FIELDS IS ERROR              
*                                                                               
*- START DATE IN 1ST SCAN BLOCK                                                 
         LA    RE,XDATE1                                                        
         LA    RF,L'XDATE1                                                      
         STM   RE,RF,AERROR                                                     
*                                                                               
         ZIC   R3,SCANBLK1+0       LENGTH OF INPUT                              
         LA    R4,SCANBLK1+12      A(DATE TO CHECK)                             
         GOTO1 DODATE,P1,((R3),(R4)),RXTRAFRM,1                                 
         BNZ   EXTRAV10            ERROR, BUT CHECK IF RFP                      
*                                                                               
*- END DATE IN 2ND SCAN BLOCK                                                   
         LA    RE,XDATE2                                                        
         LA    RF,L'XDATE2                                                      
         STM   RE,RF,AERROR                                                     
*                                                                               
         ZIC   R3,SCANBLK2+0       LENGTH OF INPUT                              
         LA    R4,SCANBLK2+12      A(DATE TO CHECK)                             
         GOTO1 DODATE,P1,((R3),(R4)),RXTRATO,2                                  
         BNZ   EXTRAV10            ERROR, BUT CHECK IF RFP                      
*                                                                               
         CLI   RXTRAFRM,C' '       ANY START DATE                               
         BE    EXTRAV08                                                         
         CLC   RXTRATO,RXTRAFRM    END LESS THAN START?                         
         BL    EXTRAV09                                                         
*                                                                               
EXTRAV08 B     COMMGOOD            DATES IN CORRECT ORDER                       
*                                                                               
EXTRAV09 EQU   *                                                                
         MVI   ERRCNTL,ECMSGNUM    END BEFORE START                             
         MVI   ERRDATA,ENDBFRST                                                 
         B     COMMBAD                                                          
*                                                                               
EXTRAV10 DS    0H                                                               
         TM    RFPSTAT,RFPINUSE    NOT USING RFP, ERROR                         
         BZ    COMMBAD                                                          
*                                                                               
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPSYMB                                                
         MVC   QRFPWORK,8(R2)                                                   
         OC    QRFPWORK,SPACES                                                  
         GOTO1 AGETOVLY,P1,('OVRFP',0)                                          
         CLI   QRFPMODE,QRFPOK                                                  
         BNE   COMMBAD             INVALID SYMBOLIC EQUATE                      
*                                                                               
         L     R3,AMAPNTRY                                                      
         TM    MAPFMT(R3),X'08'    FOR DATES WITH MMM/YY                        
         BO    EXTRAV15                                                         
*                                                                               
         CLC   QRFPDICT,=Y(E#STEND)  START, END DATES                           
         BE    EXTRAV20            INVALID SYMBOLIC EQUATE                      
         CLC   QRFPDICT,=Y(E#PSTEND)  START, END DATES                          
         BE    EXTRAV20            INVALID SYMBOLIC EQUATE                      
         B     COMMBAD                                                          
*                                                                               
EXTRAV15 DS    0H                                                               
         CLC   QRFPDICT,=Y(E#STENDM)  START, END MONTHS                         
         BE    EXTRAV20            INVALID SYMBOLIC EQUATE                      
         CLC   QRFPDICT,=Y(E#PSTEDM) PERIOD START, END MONTHS                   
         BNE   COMMBAD             INVALID SYMBOLIC EQUATE                      
*                                                                               
EXTRAV20 DS    0H                                                               
         MVC   RXTRAFRM,SPACES     STORE ESC SEQ IN REC CARD                    
         MVC   RXTRAFRM(L'QRFPESC),QRFPESC                                      
         MVI   RXTRAFRM+3,12       LEN(START/END DATE)                          
         B     COMMXIT                                                          
*                                                                               
*- CUTOFF DATE                                                                  
CUTOFF   EQU   *                                                                
         XC    SCANBLOK(LSCANBLK),SCANBLOK                                      
         BAS   RE,DELIMITR                                                      
         GOTO1 =V(SCANNER),P1,(R2),(2,SCANBLOK),RR=SUBRELO                      
         CLI   P2,0                                                             
         BE    COMMXIT             NO INPUT                                     
*                                                                               
         CLI   P2,1                                                             
         BH    COMMBAD             MORE THAN 1 IPT FIELD IS ERROR               
*                                                                               
*- START DATE IN 1ST SCAN BLOCK                                                 
         LA    RE,XDATE1                                                        
         LA    RF,L'XDATE1                                                      
         STM   RE,RF,AERROR                                                     
*                                                                               
         ZIC   R3,SCANBLK1+0       LENGTH OF INPUT                              
         LA    R4,SCANBLK1+12      A(DATE TO CHECK)                             
         GOTO1 DODATE,P1,((R3),(R4)),RSTRD,1                                    
         BNZ   COMMBAD             ERROR                                        
*                                                                               
         B     COMMXIT                                                          
*                                                                               
*                                                                               
*- BUDGET START,END DATES                                                       
BUDSTEND EQU   *                                                                
         XC    SCANBLOK(LSCANBLK),SCANBLOK                                      
         BAS   RE,DELIMITR                                                      
         GOTO1 =V(SCANNER),P1,(R2),(2,SCANBLOK),RR=SUBRELO                      
         CLI   P2,0                                                             
         BE    COMMBAD             NO INPUT IS ERROR                            
*                                                                               
         CLI   P2,2                                                             
         BH    COMMBAD             MORE THAN 2 IPT FIELDS IS ERROR              
*                                                                               
*- START DATE IN 1ST SCAN BLOCK                                                 
         LA    RE,XDATE1                                                        
         LA    RF,L'XDATE1                                                      
         STM   RE,RF,AERROR                                                     
*                                                                               
         ZIC   R3,SCANBLK1+0       LENGTH OF INPUT                              
         LA    R4,SCANBLK1+12      A(DATE TO CHECK)                             
         GOTO1 DODATE,P1,((R3),(R4)),R2BUDST,1                                  
         BNZ   BUDSTE10            ERROR                                        
*                                                                               
*- END DATE IN 2ND SCAN BLOCK                                                   
         LA    RE,XDATE2                                                        
         LA    RF,L'XDATE2                                                      
         STM   RE,RF,AERROR                                                     
*                                                                               
         ZIC   R3,SCANBLK2+0       LENGTH OF INPUT                              
         LA    R4,SCANBLK2+12      A(DATE TO CHECK)                             
         GOTO1 DODATE,P1,((R3),(R4)),R2BUDED,2                                  
         BNZ   BUDSTE10            ERROR                                        
*                                                                               
         BAS   RE,DATORDER         CHECK FOR END BEFORE START                   
         B     COMMXIT                                                          
*                                                                               
BUDSTE10 DS    0H                                                               
         TM    RFPSTAT,RFPINUSE                                                 
         BZ    COMMBAD                                                          
*                                                                               
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPSYMB                                                
         MVC   QRFPWORK,8(R2)                                                   
         OC    QRFPWORK,SPACES                                                  
         GOTO1 AGETOVLY,P1,('OVRFP',0)                                          
         CLI   QRFPMODE,QRFPOK                                                  
         BNE   COMMBAD             INVALID SYMBOLIC EQUATE                      
*                                                                               
*              BUDGET DATES DEFAULT TO MMM/YY FOR NOW                           
*                                                                               
         CLC   QRFPDICT,=Y(E#BSTEND) BUDGET START, END DATES                    
         BE    BUDSTE20            INVALID SYMBOLIC EQUATE                      
*                                                                               
BUDSTE20 DS    0H                                                               
         MVC   R2BUDST,SPACES      STORE ESC SEQ IN REC CARD                    
         MVC   R2BUDST(L'QRFPESC),QRFPESC                                       
         MVI   R2BUDST+3,12        LEN(BUDGET START/END DATES)                  
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
*- AS AT DATE                                                                   
ASADVAL  EQU   *                                                                
         XC    SCANBLOK(LSCANBLK),SCANBLOK                                      
         BAS   RE,DELIMITR                                                      
         GOTO1 =V(SCANNER),P1,(R2),(2,SCANBLOK),RR=SUBRELO                      
         CLI   P2,0                ANY INPUT?                                   
         BE    COMMXIT             NO  - LEAVE BLANK                            
         CLI   P2,2                MORE THAN 2 FIELDS?                          
         BH    COMMBAD             YES - ERROR                                  
*                                                                               
*- START DATE IN 1ST SCAN BLOCK                                                 
         LA    RE,XDATE1                                                        
         LA    RF,L'XDATE1                                                      
         STM   RE,RF,AERROR                                                     
*                                                                               
         ZIC   R3,SCANBLK1+0       LENGTH OF INPUT                              
         LA    R4,SCANBLK1+12      A(DATE TO CHECK)                             
         GOTO1 DODATE,P1,((R3),(R4)),RASAD,1                                    
         BNZ   ASAD0050            ERROR                                        
*                                                                               
*- END   DATE IN 2ND SCAN BLOCK                                                 
         LA    RE,XDATE2                                                        
         LA    RF,L'XDATE2                                                      
         STM   RE,RF,AERROR                                                     
*                                                                               
         ZIC   R3,SCANBLK2+0       LENGTH OF INPUT                              
         LTR   R3,R3               IS SECOND DATE ENTERED?                      
         BZ    COMMXIT             NO  - VALIDATION COMPLETE                    
         LA    R4,SCANBLK2+12      A(DATE TO CHECK)                             
         GOTO1 DODATE,P1,((R3),(R4)),R2ASAT2,2                                  
         BNZ   ASAD0050            ERROR                                        
*                                                                               
*   FOR FROM-TO AS-AT DATES, SET 'TO' DATE TO SUNDAY OF WEEK.                   
*                                                                               
         CLI   DAILYPAC,C'Y'       DAILY PACING USER?                           
         BE    ASAD0020            YES - DON'T CHANGE ENTERED DATES             
         GOTO1 =V(GETDAY),P1,R2ASAT2,DUB,RR=SUBRELO                             
         CLI   P1,X'7'             IS IT SUNDAY?                                
         BE    ASAD0010            YES - CHECK FROM-DATE                        
*                                  NO  - SET AS-AT 'TO' TO SUNDAY               
         ZIC   RE,P1               GET DAY OF WEEK NUMBER                       
         LNR   RE,RE               MAKE IT NEGATIVE                             
         LA    RE,7(RE) SUBTRACT IT FROM 7                                      
         ST    RE,P3               SET ADDAY ADJUST VALUE                       
         GOTO1 =V(ADDAY),P1,R2ASAT2,R2ASAT2,RR=SUBRELO                          
         GOTO1 DATCON,DMCB,(0,R2ASAT2),(X'20',R2ASAT2)                          
*                                  DROP INTERNAL DATE FORMAT                    
*                                                                               
ASAD0010 EQU   *                                                                
*                                                                               
*   FOR FROM-TO AS-AT DATES, SET 'FROM' DATE TO MONDAY OF WEEK.                 
*                                                                               
         GOTO1 =V(GETDAY),P1,RASAD,DUB,RR=SUBRELO                               
         CLI   P1,X'1'             IS IT MONDAY?                                
         BE    COMMGOOD            YES - FINISHED                               
         ZIC   RE,P1               NO  - SET AS-AT 'FROM' TO MONDAY             
         BCTR  RE,0                                                             
         LNR   RE,RE               DERIVE ADJUSTMENT                            
         ST    RE,P3               SET ADDAY ADJUST VALUE                       
         GOTO1 =V(ADDAY),P1,RASAD,RASAD,RR=SUBRELO                              
         GOTO1 DATCON,DMCB,(0,RASAD),(X'20',RASAD)                              
*                                  DROP INTERNAL DATE FORMAT                    
*                                                                               
ASAD0020 EQU   *                                                                
         CLC   R2ASAT2,RASAD       CHECK START VS END ORDER                     
         BL    ASADERR             END < START = ERROR                          
         B     COMMGOOD                                                         
*                                                                               
ASADERR  EQU   *                                                                
         MVI   ERRCNTL,ECMSGNUM    END BEFORE START                             
         MVI   ERRDATA,ENDBFRST                                                 
*                                                                               
ASAD0050 DS    0H                  ERROR, BUT CHECK IF RFP IN USE               
         TM    RFPSTAT,RFPINUSE                                                 
         BZ    COMMBAD             NO, RFP NOT IN USE, ERROR                    
*                                                                               
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPSYMB                                                
         MVC   QRFPWORK,SCANBLK1+12                                             
         OC    QRFPWORK,SPACES                                                  
         GOTO1 AGETOVLY,P1,('OVRFP',0)                                          
         CLI   QRFPMODE,QRFPOK                                                  
         BNE   COMMBAD             INVALID SYMBOLIC EQUATE                      
*                                                                               
         CLC   QRFPDICT,=Y(E#ASAT)                                              
         BNE   COMMBAD             INVALID SYMBOLIC EQUATE                      
*                                                                               
         MVC   RASAD,SPACES        STORE ESC SEQ IN REC CARD                    
         MVC   RASAD(L'QRFPESC),QRFPESC                                         
         MVI   RASAD+3,6           LEN(AS AT DATE)                              
*                                                                               
         ZIC   RF,SCANBLK2+0       LENGTH OF INPUT                              
         LTR   RF,RF               IS SECOND DATE ENTERED?                      
         BZ    COMMXIT             NO  - VALIDATION COMPLETE                    
*                                                                               
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPSYMB                                                
         MVC   QRFPWORK,SCANBLK2+12                                             
         OC    QRFPWORK,SPACES                                                  
         GOTO1 AGETOVLY,P1,('OVRFP',0)                                          
         CLI   QRFPMODE,QRFPOK                                                  
         BNE   COMMBAD             INVALID SYMBOLIC EQUATE                      
*                                                                               
         CLC   QRFPDICT,=Y(E#ASAT2)                                             
         BNE   COMMBAD             INVALID SYMBOLIC EQUATE                      
*                                                                               
         MVC   R2ASAT2,SPACES      STORE ESC SEQ IN REC CARD                    
         MVC   R2ASAT2(L'QRFPESC),QRFPESC                                       
         MVI   R2ASAT2+3,6         LEN(AS AT 2 DATE)                            
         B     COMMXIT                                                          
         EJECT                                                                  
*                                                                               
*- DODATE -- VALIDATE 1 DATE.                                                   
*                                                                               
*  INPUT:  P1 = BYTE 1 - LENGTH TO VALIDATE                                     
*               BYTES 2-4 - A(INPUT)                                            
*          P2 = A(OUTPUT)                                                       
*               BYTE 0 = X'80':  RETURN X'F0F0' YR 2000 DATE                    
*          P3 = DATE NUMBER (1 OR 2) FOR DEFAULTING                             
*               (1=START, 2=END)                                                
*                                                                               
*  FORMAT BITS:  X'04' = MMMDD/YY FORMAT                                        
*                X'08' = MMM/YY   FORMAT                                        
*                X'80' = DEFAULT END TO START DATE                              
*                X'40' = DEFAULT START TO END DATE                              
*                X'20' = DEFAULT 1ST DATE TO TODAY                              
*                X'10' = DEFAULT 2ND DATE TO TODAY                              
*                                                                               
DODATE   NTR1                                                                   
         L     R3,AMAPNTRY         REQMAP ENTRY FOR THIS FIELD                  
*                                                                               
         ZIC   R4,P1               SAVE LENGTH OF DATA                          
         LTR   R4,R4                                                            
         BNZ   DD40                PROCESS INPUT.  NO DEFAULTS.                 
*                                                                               
*- TRY FOR DEFAULTS                                                             
         CLI   P3+3,1              1ST (START) DATE?                            
         BNE   DD30                                                             
*                                                                               
         TM    MAPFMT(R3),X'20'    DEFAULT TO TODAY?                            
         BZ    DD25                                                             
*                                                                               
DD20     L     RE,P2                                                            
         MVC   0(6,RE),TODAY                                                    
         B     DD70                                                             
*                                                                               
DD25     TM    MAPFMT(R3),X'40'    DEFAULT START TO END?                        
         BZ    COMMBAD                                                          
         MVC   RSTRD,RENDD                                                      
         B     COMMGOOD                                                         
*                                                                               
DD30     EQU   *                                                                
         CLI   P3+3,2              2ND (END) DATE?                              
         BE    *+6                                                              
         DC    H'0'                INVALID DATE NUMBER                          
*                                                                               
         TM    MAPFMT(R3),X'10'    DEFAULT TO TODAY?                            
         BO    DD20                                                             
*                                                                               
         TM    MAPFMT(R3),X'80'    DEFAULT END TO START                         
         BZ    COMMBAD                                                          
         MVC   RENDD,RSTRD                                                      
         B     COMMGOOD                                                         
*                                                                               
DD40     EQU   *                                                                
         SR    R2,R2               ASSUME MMMDD/YY FORMAT                       
         TM    MAPFMT(R3),X'04'                                                 
         BO    DD50                                                             
         LA    R2,2                TRY FOR MMM/YY FORMAT                        
         TM    MAPFMT(R3),X'08'                                                 
         BO    DD50                                                             
         DC    H'0'                INVALID FORMAT                               
DD50     EQU   *                                                                
         STC   R2,P1               DATVAL FORMAT CODE                           
         MVI   P2,X'80'            RETURN REGULAR DATE FOR YR 2000              
         GOTO1 DATVAL,P1                                                        
         ICM   RF,15,P1            LENGTH OF VALID IPT.                         
         BZ    COMMBAD                                                          
*                                                                               
         CR    R4,RF               LEN OF IPT -VS- LEN OF DATE                  
         BNE   COMMBAD                                                          
*                                                                               
*- IF MMM/YY FORMAT DATES, BLANK OUT DAYS                                       
DD70     EQU   *                                                                
         TM    MAPFMT(R3),X'08'                                                 
         BZ    COMMGOOD            NOT MMM/YY FORMAT. EXIT.                     
*                                                                               
         L     RE,P2                                                            
         MVC   4(2,RE),=CL2'  '    BLANK 'DD' OF 'YYMMDD'                       
         B     COMMGOOD                                                         
*                                                                               
         SPACE 2                                                                
*                                                                               
*- DATORDER -- CHECK FOR END BEFORE START DATE.                                 
*  CC 0 = GOOD, ^0 = BAD.                                                       
DATORDER NTR1                                                                   
         CLI   RSTRD,C' '          ANY START DATE                               
         BE    DOOK                                                             
         XC    WORK,WORK                                                        
         MVC   WORK(6),RENDD                                                    
         CLC   WORK+4(2),SPACES    ANY DAY IN DATE?                             
         BNE   DATO0020            YES                                          
         MVC   WORK+4(2),=C'01'    NO  - SET DAY = 01                           
DATO0020 EQU   *                                                                
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+12)                                 
         MVC   WORK(6),RSTRD                                                    
         CLC   WORK+4(2),SPACES    ANY DAY IN DATE?                             
         BNE   DATO0040            YES                                          
         MVC   WORK+4(2),=C'01'    NO  - SET DAY = 01                           
DATO0040 EQU   *                                                                
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+15)                                 
         CLC   WORK+12(3),WORK+15  END LESS THAN START?                         
         BL    DOERR                                                            
*                                                                               
DOOK     B     COMMGOOD            DATES IN CORRECT ORDER                       
*                                                                               
DOERR    EQU   *                                                                
         MVI   ERRCNTL,ECMSGNUM    END BEFORE START                             
         MVI   ERRDATA,ENDBFRST                                                 
         B     COMMBAD                                                          
         EJECT                                                                  
*...............................  GET NEW CODE ...............                  
*                                                                               
*- 12 MONTH PERIOD RTN                                                          
*                                                                               
*  ACCEPTS AN INPUT (END) DATE AS YYMM OR YYMMDD AND CACLULATES A               
*  START DATE FROM IT AS FOLLOWS:                                               
*                                                                               
*  START DATE = END DATE - 11 MONTHS.  (1 FULL YEAR)                            
*                                                                               
PERDVAL  EQU   *                                                                
         GOTO1 =A(XPERDVAL),DMCB,(RC),RR=SUBRELO                                
         BNZ   COMMBAD                                                          
         B     COMMXIT                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
*- BASIS VALIDATION -- NO FORMAT BITS IN USE.                                   
*  1 BYTE INPUT.  SEE LIST FOR VALUES.                                          
*                                                                               
BASISVAL EQU   *                                                                
         MVC   RBASIS,8(R2)        ASSUME OK                                    
*                                                                               
         LA    RE,VBASIS           VALUES                                       
         LA    R0,#BASIS           NUMBER TO CHECK                              
         B     LISTCK                                                           
         SPACE 2                                                                
*                                                                               
*- STATION TYPE VALIDATION -- NO FORMAT BITS IN USE.                            
*  1 BYTE INPUT.  SEE LIST FOR VALUES.                                          
*                                                                               
STATVAL  EQU   *                                                                
         MVC   RSTAT,8(R2)         ASSUME OK                                    
*                                                                               
         LA    RE,VSTATYP          A(VALUE LIST)                                
         L     RF,AREQNTRY         A(REQUEST ENTRY)                             
         TM    RQCNTL2(RF),RQ2RRG  RRG REQUEST?                                 
         BNO   STAV0010            NO  - ONLY PERMIT THREE VALUES               
         LA    R0,#STATYP2         YES - PERMIT FIVE VALUES                     
         B     LISTCK                                                           
*                                                                               
STAV0010 EQU   *                                                                
         LA    R0,#STATYP          CHECK THREE VALUES                           
         B     LISTCK                                                           
         SPACE 2                                                                
*                                                                               
*- ACCOUNTING OPTION VALIDATION                                                 
AOPTVAL  EQU   *                                                                
         MVC   RACCTOPT,8(R2)                                                   
         LA    R1,VACCTOPT                                                      
         B     IDLIST                                                           
         SPACE 2                                                                
*                                                                               
*- READ SEQUENCE                                                                
SEQVAL   EQU   *                   SEQUENCE - FIND BITS                         
         MVC   RSEQ,8(R2)          ASSUME OK.                                   
         LA    R1,VSEQ                                                          
         B     IDLIST                                                           
         SPACE 2                                                                
*                                                                               
*- RECAP OPTIONS                                                                
RCPVAL   EQU   *                                                                
         MVC   RACCTOPT,8(R2)      PASSED AS ACCOUNTING OPTION                  
         LA    R1,VRECAP                                                        
         B     IDLIST                                                           
         SPACE 2                                                                
*                                                                               
*- TOTALS CONTROL.  PASSED AS OPTION 2                                          
TCNTVAL  EQU   *                                                                
         MVC   ROPTN2,8(R2)                                                     
         LA    R1,VTOTCTL                                                       
         B     IDLIST                                                           
*                                                                               
OPT1VAL  EQU   *                   OPTIONS VALIDATION                           
         L     R1,=A(OPT1VTBL)                                                  
         A     R1,SUBRELO                                                       
         LA    RE,ROPTN                                                         
         B     OPTNVAL                                                          
*                                                                               
OPT2VAL  EQU   *                                                                
         L     R1,=A(OPT2VTBL)                                                  
         A     R1,SUBRELO                                                       
         LA    RE,ROPTN2                                                        
         B     OPTNVAL                                                          
*                                                                               
OPT3VAL  EQU   *                                                                
         L     R1,=A(OPT3VTBL)                                                  
         A     R1,SUBRELO                                                       
         LA    RE,ROPTN3                                                        
         B     OPTNVAL                                                          
*                                                                               
OPT4VAL  EQU   *                                                                
         L     R1,=A(OPT4VTBL)                                                  
         A     R1,SUBRELO                                                       
         LA    RE,ROPTN4                                                        
         B     OPTNVAL                                                          
*                                                                               
OPT5VAL  EQU   *                                                                
         L     R1,=A(OPT5VTBL)                                                  
         A     R1,SUBRELO                                                       
         LA    RE,ROPTN5                                                        
         B     OPTNVAL                                                          
*                                                                               
OPT6VAL  EQU   *                                                                
         L     R1,=A(OPT6VTBL)                                                  
         A     R1,SUBRELO                                                       
         LA    RE,R2OPT6           WAS ROPTN6                                   
         B     OPTNVAL                                                          
*                                                                               
*- ALL OPTION VALS COME HERE.  RE=A(REQUEST CARD FIELD)                         
*  MOVE INPUT INTO RECORD AND USE STANDARD ID-LIST ROUTINE                      
OPTNVAL  MVC   0(1,RE),8(R2)       MOVE FIELD TO RQST                           
         B     IDLIST                                                           
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*  VALUE LISTS FOR 1 BYTE LIST OR ID-LIST FIELDS.                     *         
*                                                                     *         
*  LISTS ARE 1 BYTE PER ENTRY.  SELF-ADJUSTING EQUATE COUNTS NUMBER   *         
*  OF ITEMS ACTIVE IN LIST.                                           *         
*                                                                     *         
*  ID-LISTS ARE VARIABLE LENGTH ENTRIES IN THE FORMAT                 *         
*  XL1'LENGTH OF ENTRY'  (INCLUDES LENGTH OF ENTRY BYTE)  X'00'=END.  *         
*  CL2'REPORT ID' OR XL2'FFFF' (ALL REPORT ID'S)                      *         
*  C'LIST OF VALUES'  NUMBER OF ITEMS IN LIST IS ENTRY LENGTH -3      *         
*                     (3 BYTES=ENTRY OVERHEAD)                        *         
*                                                                     *         
***********************************************************************         
         SPACE                                                                  
VBASIS   EQU   *                   LIST OF VALUES                               
         DC    C'B'                                                             
         DC    C'C'                                                             
         DC    C'4'                                                             
         DC    C'5'                                                             
#BASIS   EQU   *-VBASIS            NUMBER OF ITEMS IN LIST                      
         SPACE                                                                  
VSTATYP  EQU   *                   VALUE LIST: ORIGINAL SET                     
         DC    C'C'                COMPARABLE                                   
         DC    C'N'                NEW                                          
         DC    C'O'                OLD                                          
#STATYP  EQU   *-VSTATYP           NUMBER OF ITEMS IN ORIG LIST                 
*                                  ADDITIONAL VALUES                            
         DC    C'J'                JOINED/NEW                                   
         DC    C'L'                LEFT/OLD                                     
#STATYP2 EQU   *-VSTATYP           NUMBER OF ITEMS IN TOTAL LIST                
         SPACE                                                                  
VACCTOPT EQU   *                                                                
         DC    AL1(3+2),C'20',C'TP'                                             
         DC    AL1(3+2),C'21',C'TP'                                             
         DC    AL1(3+2),C'37',C'OP'                                             
         DC    AL1(3+2),C'47',C'OA'                                             
         DC    AL1(3+2),C'5F',C'NA'                                             
         DC    X'00'               EOT                                          
         SPACE                                                                  
VSEQ     DS    0C                                                               
         DC    AL1(3+1),C'20',C'S'                                              
         DC    AL1(3+1),C'21',C'S'                                              
         DC    AL1(3+2),C'30',C'SG'                                             
         DC    AL1(3+2),C'32',C'SG'                                             
         DC    AL1(3+2),C'34',C'SG'                                             
         DC    AL1(3+2),C'36',C'SG'                                             
         DC    AL1(3+3),C'38',C'OMS'                                            
         DC    AL1(3+1),C'40',C'M'                                              
         DC    AL1(3+1),C'42',C'M'                                              
         DC    AL1(3+1),C'47',C'S'                                              
         DC    AL1(3+2),C'50',C'OS'                                             
         DC    AL1(3+2),C'55',C'OS'                                             
         DC    AL1(3+2),C'60',C'OS'                                             
         DC    AL1(3+1),C'72',C'A'                                              
         DC    AL1(3+1),C'76',C'A'                                              
         DC    AL1(3+1),C'82',C'M'                                              
         DC    AL1(3+2),C'87',C'LM'                                             
         DC    AL1(3+1),C'88',C'M'                                              
         DC    AL1(3+1),C'90',C'O'                                              
         DC    AL1(3+2),C'OR',C'OS'                                             
         DC    X'00'                                                            
         SPACE                                                                  
VRECAP   EQU   *                                                                
         DC    AL1(3+2),C'15',C'BR'               FOR THE 15 REPORT             
         DC    AL1(3+2),C'2A',C'EB'               FOR THE 2A REPORT             
         DC    AL1(3+2),C'2B',C'EB'               FOR THE 2B REPORT             
         DC    AL1(3+4),C'3V',C'SOVB'             FOR THE 3V REPORT             
         DC    AL1(3+5),C'4R',C'RSBMO'            FOR THE 4R REPORT             
         DC    AL1(3+4),C'6B',C'RDMC'             FOR THE 6B REPORT             
         DC    AL1(3+2),C'8F',C'SP'               FOR THE 8F REPORT             
         DC    AL1(3+4),C'AC',C'ABCE'             FOR THE AC REPORT             
         DC    AL1(3+4),C'FR',C'SOMB'             FOR THE FR REPORT             
         DC    AL1(3+4),C'PR',C'SOMB'             FOR THE PR REPORT             
         DC    AL1(3+2),C'PR',C'AB'               FOR THE PS REPORT             
         DC    AL1(3+4),C'PX',C'BRD '             FOR THE PX REPORT             
         DC    AL1(3+4),C'RS',C'BRDS'             FOR THE RS REPORT             
         DC    AL1(3+4),C'SR',C'SOMB'             FOR THE SR REPORT             
         DC    AL1(3+3),C'SO',C'OMB'              FOR THE SO REPORT             
         DC    AL1(3+3),XL2'FFFF',C'BRD'          EVERYBODY ELSE                
         DC    X'00'                                                            
         SPACE                                                                  
VTOTCTL  EQU   *                                                                
         DC    AL1(3+27),C'32',C'ABCDEFGHIJKLMNOPQRSTUVWXYZ1'                   
         DC    AL1(3+26),XL2'FFFF',C'ABCDEFGHIJKLMNOPQRSTUVWXYZ'                
         DC    X'00'                                                            
         DS    0H                  ALIGNMENT                                    
         TITLE 'IDLIST AND LISTCK -- GENERIC LIST CHECKING'                     
*                                                                               
*- IDLIST -- LIST CHECKING BY REPORT ID.                                        
*                                                                               
*  FIND REPORT ID IN USER'S LIST, THEN CHECK SCREEN INPUT                       
*  BYTE AGAINST LIST OF VALUES FOR REPORT.                                      
*                                                                               
*  REPORT ID LIST FORMAT:                                                       
*                                                                               
*  XL1'LENGTH OF ENTRY'  (INCLUSIVE)   X'00' = END OF LIST.                     
*                                                                               
*  CL2'ALPHA REPORT ID'                                                         
*    - OR -                                                                     
*  XL2'FFFF' GENERIC ID (USE 1 ENTRY FOR MULTIPLE RQSTS)                        
*                                                                               
*  CL???'VARIABLE LENGTH VALUE LIST FOR REPORT - 1 BYTE PER VALUE'              
*                                                                               
*  *NOTE* IF REPORT ID NOT FOUND IN LIST, DATA PASSED THRU                      
*         UNCHECKED.  (GOOD FOR 'TYPE' FIELDS)                                  
*                                                                               
*  INPUT:  R1 = A(LIST)                                                         
*                                                                               
*  NOTE:   AFTER FINDING REPORT ENTRY, VALUES CHECKED VIA 'LISTCK'              
*                                                                               
*  NOTE:   THE X'FFFF' ID LIST ENTRY SHOULD BE LAST.                            
*          (AFTER ALL SPECIFIC-REPORT ENTRIES)                                  
*                                                                               
IDLIST   EQU   *                                                                
         SR    R0,R0                                                            
         LR    RE,R1               SAVE A(START OF LIST)                        
IDLIST10 IC    R0,0(R1)            FIND REQUEST NUM ENTRY                       
         LTR   R0,R0                                                            
*                                                                               
         BZ    COMMGOOD            ID NOT IN LIST. RETURN GOOD CC.              
*                                                                               
IDLIST20 CLC   RPTNUM,1(R1)        THIS REPORT?                                 
         BE    IDLIST30                                                         
         CLC   =XL2'FFFF',1(R1)    GENERIC ID ENTRY?                            
         BE    IDLIST30                                                         
         AR    R1,R0               NEXT ID LIST ENTRY                           
         B     IDLIST10                                                         
*                                                                               
*- SET UP FOR 'LISTCK'                                                          
IDLIST30 SH    R0,=H'3'            R0 = # ITEMS IN LIST                         
         LA    RE,3(R1)            RE = A(LIST OF VALUES)                       
         B     LISTCK                                                           
         SPACE 2                                                                
*                                                                               
*- LISTCK -- GENERAL 1 BYTE LIST -VS- SCREEN INPUT CHECK                        
*                                                                               
*  R0 = NUMBER OF ITEMS IN LIST                                                 
*  R2 = A(HEADER OF SCREEN FIELD TO CHECK)                                      
*  RE = A(LIST OF VALUES)                                                       
LISTCK   EQU   *                                                                
         CLC   0(1,RE),8(R2)       LIST -VS- SCREEN                             
         BE    COMMGOOD            A HIT!  WE PASS.                             
         LA    RE,1(RE)                                                         
         BCT   R0,LISTCK                                                        
*                                                                               
         B     COMMBAD                                                          
         TITLE 'REREQ02 -- WHOLE-REQUEST EDIT ROUTINES'                         
**********************************************************************          
*                                                                    *          
*  WHOLE-REQUEST EDIT ROUTINES                                       *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
*                                                                               
*- VAL RTN 01                                                                   
*                                                                               
*        1. GROUP CODE MUST BE FILLED IN                                        
*        2. END DATE DEFAULTS TO START DATE IF NOT GIVEN                        
*        3. START AND END DATES MUST BE IN SAME YEAR                            
*        4. BLAIR STATION/OFFICE TEST                                           
*        5. QUARTERLY REQUEST CHECK.                                            
*                                                                               
VALR01   EQU   *                                                                
         LA    RF,RQSNUMH          A(NUMBER FIELD IN HEADER)                    
         CLI   5(RF),2             TWO CHARACTERS ENTERED?                      
         BE    V01R05              YES                                          
         LA    RF,1(RF)            NO  - SKIP 'R'                               
*                                                                               
V01R05   EQU   *                                                                
*                                                                               
         CLC   8(2,RF),=C'3K'                                                   
         BE    COMMGOOD                                                         
*                                                                               
         BAS   RE,GRPVAL           GROUP IS REQUIRED                            
         BNZ   COMMBAD                                                          
*                                                                               
         CLC   RENDD,=6C' '        DEFAULT END TO START                         
         BNE   V01R10                                                           
         MVC   RENDD,RSTRD                                                      
V01R10   EQU   *                                                                
*                                                                               
*                                                                               
*- START/END DATES NEED TO BE IN SAME YEAR.                                     
V01R12   EQU   *                                                                
*                                                                               
         L     RE,AREQNTRY         PROGRAM ID NOT YET LOADED                    
*                                     TO REQUEST CARD                           
         CLC   =C'A0',RQOPT4ID(RE) A0/A1 DON'T NEED W/IN YEAR TEST              
*                                                                               
         BE    V01R20                                                           
         CLC   =C'A1',RQOPT4ID(RE) A0/A1 DON'T NEED W/IN YEAR TEST              
*                                                                               
         BE    V01R20                                                           
         CLC   =C'P2',RQOPT4ID(RE) P2 DOESN'T NEED W/IN YEAR TEST               
*                                                                               
         BE    V01R20                                                           
         CLC   =C'P3',RQOPT4ID(RE) P3 DOESN'T NEED W/IN YEAR TEST               
*                                                                               
         BE    V01R20                                                           
         CLC   =C'P4',RQOPT4ID(RE) P4 DOESN'T NEED W/IN YEAR TEST               
*                                                                               
         BE    V01R20                                                           
         CLC   =C'P5',RQOPT4ID(RE) P5 DOESN'T NEED W/IN YEAR TEST               
*                                                                               
         BE    V01R20                                                           
*                                                                               
         GOTO1 PUTCURS,P1,VSTARTDT,SAMEYEAR,L'SAMEYEAR                          
         CLC   RENDD(2),RSTRD                                                   
         BNE   COMMBAD                                                          
         B     V01R20                                                           
*                                                                               
*- R24/R25 - 12 MONTH MAX TEST                                                  
V01R16   EQU   *                                                                
         BAS   RE,MAX12            12 MONTH MAX TEST                            
         BNZ   COMMBAD                                                          
*                                                                               
V01R20   EQU   *                                                                
*                                                                               
*- REP-SPECIFIC PROCESSING --                                                   
*  BLAIR TV REQUIRES STATION OR OFFICE                                          
         BAS   RE,BLRSTAOF                                                      
         BNZ   COMMBAD                                                          
*                                                                               
*- CHECK FOR WHOLE NUMBER OF QUARTERS IF QUARTERLY REQUEST.                     
         BAS   RE,QUARTRCK                                                      
         BNZ   COMMBAD                                                          
*                                                                               
         B     COMMGOOD            REQUEST PASSED                               
         EJECT                                                                  
*                                                                               
*- VAL RTN 02                                                                   
*                                                                               
*        1. GROUP CODE MUST BE FILLED IN                                        
*        2. 12 MONTH MAX TEST                                                   
*        3. SPECIAL '34' REPORT EDIT: OPTION = 'C' NEEDS ACCOUNTING             
*           OPTION = 'A'                                                        
*                                                                               
VALR02   EQU   *                                                                
         BAS   RE,GRPVAL           GROUP REQUIRED                               
         BNZ   COMMBAD                                                          
*                                                                               
         BAS   RE,MAX12            12 MONTH MAX                                 
         BNZ   COMMBAD                                                          
*                                                                               
         L     RE,AREQNTRY         PROGRAM ID NOT YET LOADED                    
*                                     TO REQUEST CARD                           
*****>>  CLC   =C'PR',RQOPT4ID(RE) FOR THE PR REPORT ONLY                       
*****>>  CLC   =C'PX',RQOPT4ID(RE) FOR THE PX REPORT ONLY                       
*                                                                               
*****>>  BE    V02R10                                                           
*                                                                               
         CLC   RNUM,=C'34'         FOR THE 34 REPORT ONLY                       
         BNE   COMMGOOD                                                         
*                                                                               
         CLI   ROPTN,C'C'          AN OPTION OF 'C'                             
         BNE   COMMGOOD                                                         
*                                                                               
         CLI   RACCTOPT,C'A'       REQUIRES ACCTG OPTION OF 'A'                 
         BE    COMMGOOD                                                         
*                                                                               
         GOTO1 PUTCURS,P1,VACCOPTN,OPTCACCA,L'OPTCACCA                          
         B     COMMBAD                                                          
*                                                                               
V02R10   EQU   *                                                                
         SR    R0,R0               CLEAR DISPLACEMENT VALUE                     
         CLI   ROPTN,C'B'          BOTH PENDING AND FORECAST?                   
         BE    V02R40              YES - NO TABLE LOOKUP NEEDED                 
         LA    R0,1                                                             
         CLI   ROPTN,C'F'          FORECAST ONLY?                               
         BE    V02R20              YES -                                        
         LA    R0,2                MUST BE PENDING ONLY                         
V02R20   EQU   *                                                                
         LA    RF,FORTABLE         SET A(TABLE)                                 
V02R30   EQU   *                                                                
         CLI   0(RF),0             END OF TABLE?                                
         BNE   *+6                 NO                                           
         DC    H'0'                YES - DUMP IT OUT                            
         CLC   0(1,RF),RACCTOPT    CARD ENTRY = TABLE?                          
         BE    V02R35              YES                                          
         LA    RF,3(RF)            NO  - BUMP TO NEXT TABLE ENTRY               
         B     V02R30              GO BACK FOR NEXT                             
V02R35   EQU   *                                                                
         AR    RF,R0               YES - REPLACE VALUE                          
         MVC   RACCTOPT,0(RF)                                                   
V02R40   EQU   *                                                                
         MVI   ROPTN,C' '          CLEAR OPTION1                                
         B     COMMGOOD                                                         
*                                                                               
FORTABLE EQU   *                                                                
         DC    CL03'SVU'           STATION REPORT VALUES                        
         DC    CL03'OPQ'           OFFICE  REPORT VALUES                        
         DC    CL03'MLK'           S/P     REPORT VALUES                        
         DC    CL03'BCD'           OFF+S/P REPORT VALUES                        
         DC    H'0'                DELIMITER                                    
         EJECT                                                                  
*                                                                               
*- VAL RTN 03                                                                   
*                                                                               
*        1. GROUP CODE MUST BE FILLED IN                                        
*        2. 12 MONTH MAX TEST                                                   
*        3. BLAIR TV STATION/OFFICE TEST                                        
*                                                                               
VALR03   EQU   *                                                                
         BAS   RE,GRPVAL           GROUP REQUIRED                               
         BNZ   COMMBAD                                                          
*                                                                               
         BAS   RE,MAX12            12 MONTH MAX                                 
         BNZ   COMMBAD                                                          
*                                                                               
*- REP-SPECIFIC PROCESSING --                                                   
*  BLAIR TV REQUIRES STATION OR OFFICE                                          
         BAS   RE,BLRSTAOF                                                      
         BNZ   COMMBAD                                                          
         B     COMMGOOD                                                         
         SPACE 2                                                                
*                                                                               
*- VAL RTN 04                                                                   
*                                                                               
*        1. GROUP CODE MUST BE FILLED IN                                        
*        2. CAN'T USE BOTH OPTION 4 AND OPTION 5                                
*                                                                               
VALR04   EQU   *                                                                
         BAS   RE,GRPVAL           GROUP REQUIRED                               
         BNZ   COMMBAD                                                          
*                                                                               
         CLI   ROPTN4,C' '         CAN'T USE OPTION 4                           
         BE    COMMGOOD                                                         
         CLI   ROPTN5,C' '         AND OPTION 5 TOGETHER                        
         BE    COMMGOOD                                                         
*                                                                               
         GOTO1 PUTCURS,P1,VOPTION5,OPT4AND5,L'OPT4AND5                          
         B     COMMBAD                                                          
         EJECT                                                                  
*                                                                               
*- VAL RTN 05                                                                   
*                                                                               
*        1. GROUP CODE MUST BE FILLED IN.                                       
*                                                                               
*  ** NOTE **  NO 'NTR1' FOR THIS ROUTINE!                                      
*              (LET GRPVAL ROUTINE SAVE/RESTORE REGISTERS)                      
*                                                                               
VALR05   EQU   *                                                                
         BAS   RE,GRPVAL                                                        
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
*- VAL RTN 06                                                                   
*                                                                               
*        1. START/END DATE ENTERED/OMITTED TEST  (NOT FOR 8C REPORT)            
*        2. STATION OR OFFICE REQUIRED                                          
*        3. GROUP MUST BE FILLED IN                                             
*        4. GROUP MUST BE 'R' (RADIO ONLY)                                      
*                                                                               
VALR06   EQU   *                                                                
         CLC   RNUM,=C'8C'         SKIP DATE CHK FOR 8C                         
         BE    V06R10                                                           
         BAS   RE,DTEVAL           START + END CHECK                            
         BNZ   COMMBAD                                                          
*                                                                               
V06R10   EQU   *                   NEED OFFICE OR STATION                       
         CLC   ROFF,=2C' '                                                      
         BNE   V06R20                                                           
         CLC   RSTA,=5C' '                                                      
         BNE   V06R20                                                           
*                                                                               
         GOTO1 PUTCURS,P1,VOFFICEC,('ECMSGNUM',MISSING),0                       
         B     COMMBAD                                                          
*                                                                               
V06R20   EQU   *                                                                
         BAS   RE,GRPVAL           GROUP IS REQUIRED                            
         BNZ   COMMBAD                                                          
*                                                                               
         CLI   RGROUP,C'R'         MUST BE RADIO                                
         BE    COMMGOOD                                                         
*                                                                               
         GOTO1 PUTCURS,P1,VGRPSUBG,NEEDR,L'NEEDR                                
         B     COMMBAD                                                          
         EJECT                                                                  
*                                                                               
*- VAL RTN 07                                                                   
*                                                                               
*        1. STATION OR OFFICE REQUIRED                                          
*        2. GROUP MUST BE FILLED IN                                             
*        3. GROUP MUST BE 'R' (RADIO ONLY)                                      
*                                                                               
VALR07   EQU   *                                                                
*                                                                               
*- DDS TUBES NOT REQUIRED TO ENTER FIELDS IF REQUESTOR = DDS                    
*                                                                               
         CLC   =C'DDS',RQSOPT                                                   
         BNE   V07R10                                                           
*                                                                               
         CLI   TWAOFFC,C'*'                                                     
         BE    V07R20                                                           
*                                                                               
V07R10   EQU   *                                                                
         CLC   ROFF,=2C' '         STATION OR OFFICE REQUIRED                   
         BNE   V07R20                                                           
         CLC   RSTA,=5C' '                                                      
         BNE   V07R20                                                           
*                                                                               
         GOTO1 PUTCURS,P1,VOFFICEC,('ECMSGNUM',MISSING),0                       
         B     COMMBAD                                                          
*                                                                               
V07R20   EQU   *                                                                
         BAS   RE,GRPVAL           GROUP IS REQUIRED                            
         BNZ   COMMBAD                                                          
*                                                                               
         CLI   RGROUP,C'R'         MUST BE RADIO                                
         BE    COMMGOOD                                                         
*                                                                               
         GOTO1 PUTCURS,P1,VGRPSUBG,NEEDR,L'NEEDR                                
         B     COMMBAD                                                          
         SPACE 2                                                                
*                                                                               
*- VAL RTN 08                                                                   
*                                                                               
*        1. TURN OFF RECAP IF SINGLE OFFICE (R60 ONLY)                          
*        2. 50/58/60 REQUIRE 'A=WHOLE PERIOD'                                   
*        3. EDITS REQUIRED FOR VAL RTN 01                                       
*                                                                               
VALR08   EQU   *                                                                
*                                                                               
*   CUME REPORTS FOR 50/58/60 NEED WHOLE PERIOD.  BILL UHR. OCT12/94            
*                                                                               
         LA    RF,RQSNUMH          A(NUMBER FIELD IN HEADER)                    
         CLI   5(RF),2             TWO CHARACTERS ENTERED?                      
         BE    V08R10              YES                                          
         LA    RF,1(RF)            NO  - SKIP 'R'                               
                                                                                
V08R10   EQU   *                                                                
*                                                                               
         CLC   8(2,RF),=C'50'                                                   
         BE    V08R12                                                           
         CLC   8(2,RF),=C'58'                                                   
         BE    V08R12                                                           
         CLC   8(2,RF),=C'60'                                                   
         BNE   V08R14                                                           
*                                                                               
V08R12   EQU   *                                                                
         CLI   ROPTN3,C'Z'         REPORT REQUEST W/CUMES?                      
         BE    V08R13              YES                                          
*                                                                               
         CLI   ROPTN3,C'Q'         REPORT REQUEST W/CUMES?                      
         BNE   V08R14              NO                                           
*                                                                               
V08R13   EQU   *                                                                
         CLI   R2OPT6,C'A'         MUST BE A=WHOLE PERIOD                       
         BE    V08R14                                                           
*                                                                               
         GOTO1 PUTCURS,P1,VFORMATC,NEEDWPER,L'NEEDWPER                          
         B     COMMBAD                                                          
*                                                                               
V08R14   EQU   *                                                                
         CLC   8(2,RF),=C'60'                                                   
         BNE   V08R16                                                           
         CLC   ROFF,=5C' '         IF ONE OFFICE                                
         BE    V08R16                                                           
         MVI   RACCTOPT,C'D'       DON'T BOTHER WITH RECAP                      
V08R16   EQU   *                                                                
         BAS   RE,VALR01           DO VAL RTN 01 EDITS                          
*                                                                               
         B     COMMXIT             RETURN CODE SET BY VAL RTN                   
         EJECT                                                                  
*                                                                               
*- VAL RTN 09                                                                   
*                                                                               
*        1. TURN OFF RECAP IF SINGLE STATION                                    
*        2. EDITS REQUIRED FOR VAL RTN 01                                       
*        3. 51/59/61 REQUIRE 'A=WHOLE PERIOD'                                   
*                                                                               
VALR09   EQU   *                                                                
         CLC   RSTA,=5C' '         IF ONE STATION                               
         B     V09R10                                                           
*                                                                               
*  DON'T RESET RECAP OPTION IF SINGLE STATION.                                  
*                                                                               
***      BE    V09R10                                                           
         CLI   RSTA,C'*'           SET REQUEST?                                 
         BE    V09R10              YES - DON'T RESET                            
         MVI   RACCTOPT,C'D'       DON'T BOTHER WITH RECAP                      
         B     V09R10                                                           
*                                                                               
V09R10   EQU   *                                                                
*                                                                               
*   CUME REPORTS FOR 51/59/61 NEED WHOLE PERIOD.  BILL UHR. OCT12/94            
*                                                                               
         LA    RF,RQSNUMH          A(NUMBER FIELD IN HEADER)                    
         CLI   5(RF),2             TWO CHARACTERS ENTERED?                      
         BE    V09R20              YES                                          
         LA    RF,1(RF)            NO  - SKIP 'R'                               
*                                                                               
V09R20   EQU   *                                                                
*                                                                               
         CLC   8(2,RF),=C'51'                                                   
         BE    V09R30                                                           
         CLC   8(2,RF),=C'59'                                                   
         BE    V09R30                                                           
         CLC   8(2,RF),=C'61'                                                   
         BNE   V09R30                                                           
*                                                                               
V09R30   EQU   *                                                                
         CLI   ROPTN3,C'Z'         REPORT REQUEST W/CUMES?                      
         BE    V09R35              YES                                          
*                                                                               
         CLI   ROPTN3,C'Q'         REPORT REQUEST W/CUMES?                      
         BNE   V09R40              NO                                           
*                                                                               
V09R35   EQU   *                                                                
*                                                                               
         CLI   R2OPT6,C'A'         MUST BE A=WHOLE PERIOD                       
         BE    V09R40                                                           
*                                                                               
         GOTO1 PUTCURS,P1,VFORMATC,NEEDWPER,L'NEEDWPER                          
         B     COMMBAD                                                          
*                                                                               
V09R40   EQU   *                                                                
         BAS   RE,VALR01           DO VAL RTN 01 EDITS                          
*                                                                               
         B     COMMXIT             CC SET BY VAL RTN 01                         
         SPACE 2                                                                
*                                                                               
*- VAL RTN 10  (INTEREP INTERFACE TAPE)                                         
*                                                                               
*        1. NON-WEEKLY FORMAT - SET START TO END DATE (1 MONTH RUN)             
*        2. AS AT DATE MUST BE FILLED IN                                        
*                                                                               
VALR10   EQU   *                                                                
         CLI   ROPTN,C'W'          LEAVE DATES ALONE FOR WEEKLY                 
         BE    V10R20                                                           
         CLI   RSTRD,C'0'          RFP IN USE? RFP USES 20-2D IN THE            
         BL    V10R20              BYTE. ANYTHING < C'0' IS RFP CODE            
         MVC   RSTRD(6),RENDD      START=END DATE ON NON-WEEKLY                 
V10R20   EQU   *                                                                
         CLC   RASAD,=6C' '        MUST BE THERE                                
         BNE   COMMGOOD                                                         
*                                                                               
         GOTO1 PUTCURS,P1,VASATDAT,('ECMSGNUM',MISSING),0                       
         B     COMMBAD                                                          
         EJECT                                                                  
*                                                                               
*- VAL RTN 11 -- DONUT REPORT                                                   
*                                                                               
*        1. 12 MONTH MAX ON DATE RANGE                                          
*        2. CONVERT BINARY # ITEMS IN STATION LIST (1 BYTE)                     
*           TO 2 BYTE EBCDIC                                                    
*        3. DEPTH MUST BE LESS THAN OR EQUAL TO NUMBER                          
*           OF ITEMS IN STATION LIST.                                           
*                                                                               
VALR11   EQU   *                                                                
         BAS   RE,MAX12                                                         
         BNZ   COMMBAD                                                          
*                                                                               
         ZIC   R3,RSTALST#                                                      
         EDIT  (R3),(2,RSTALST#),ZERO=NOBLANK,FILL=0                            
*                                                                               
         CLC   RDEPTH,RSTALST#                                                  
         BNH   COMMGOOD                                                         
*                                                                               
         GOTO1 PUTCURS,P1,VDEPTHNO,DEPTH#,L'DEPTH#                              
         B     COMMBAD                                                          
         SPACE 2                                                                
*                                                                               
*- VAL RTN 12 -- R18 CONTRACT SWITCHER                                          
*                R90 REPORT                                                     
*                                                                               
*        1. 'FROM' AND 'TO' (OPT 1 AND 2) CAN'T BE THE SAME                     
*                                                                               
VALR12   EQU   *                                                                
*                                  CHECK 90: MUST BE DOWNLOAD!                  
*                                  ALSO SKIP FROM/TO TEST                       
*                                                                               
         LA    RF,RQSNUMH          A(NUMBER FIELD IN HEADER)                    
         CLI   5(RF),2             TWO CHARACTERS ENTERED?                      
         BE    VALR12AA            YES                                          
         LA    RF,1(RF)            NO  - SKIP 'R'                               
*                                                                               
VALR12AA EQU   *                                                                
*                                                                               
         CLC   8(2,RF),=C'90'      90 REPORT?                                   
         BE    VALR12A             YES - SKIP FROM/TO                           
*                                                                               
         CLC   ROPTN,ROPTN2                                                     
         BNE   COMMGOOD                                                         
*                                                                               
         GOTO1 PUTCURS,P1,VOPTION1,NOTSAME,L'NOTSAME                            
         B     COMMBAD                                                          
VALR12A  EQU   *                                                                
*                                                                               
*                                                                               
         CLC   RQSOUT(4),=C'DOWN'  90 MUST BE DOWNLOAD                          
         BE    VALR12B             YES - DOWNLOAD                               
*                                                                               
         GOTO1 PUTCURS,P1,VOPTION1,NOTDOWN,L'NOTDOWN                            
         B     COMMBAD                                                          
VALR12B  EQU   *                                                                
         CLI   ROPTN2,C'B'         IS OPTION2 'BOTH'?                           
         BNE   VALR12C             NO  - DATE NOT PERMITTED                     
         CLC   RSTRD,SPACES        YES - DATE ENTERED?                          
         BNE   COMMGOOD                                                         
         GOTO1 PUTCURS,P1,VCUTOFF,BOTHDATE,L'BOTHDATE                           
         B     COMMBAD                                                          
VALR12C  EQU   *                                                                
         CLC   RSTRD,SPACES        NOT BOTH:  DATE ENTERED?                     
         BE    COMMGOOD            NO                                           
         GOTO1 PUTCURS,P1,VCUTOFF,NODATE,L'NODATE                               
         B     COMMBAD                                                          
*                                                                               
*                                                                               
*                                                                               
*- VAL RTN 13 --                                                                
*                                                                               
*        1. 12 MONTH REQUIRED                                                   
*        2. OPTION 1 REQUIRED                                                   
*        3. OPTION 2 REQUIRED                                                   
*                                                                               
VALR13   EQU   *                                                                
         BAS   RE,NEED12                                                        
         BNZ   COMMBAD                                                          
*                                                                               
         CLI   ROPTN,0                                                          
         BE    VR1310                                                           
         CLI   ROPTN,C' '                                                       
         BNE   VR1320                                                           
VR1310   EQU   *                                                                
         GOTO1 PUTCURS,P1,VOPTION1,('ECMSGNUM',MISSING),0                       
         B     COMMBAD                                                          
VR1320   EQU   *                                                                
*                                                                               
         CLI   ROPTN2,0                                                         
         BE    VR1350                                                           
         CLI   ROPTN2,C' '                                                      
         BNE   VR1360                                                           
VR1350   EQU   *                                                                
         GOTO1 PUTCURS,P1,VOPTION2,('ECMSGNUM',MISSING),0                       
         B     COMMBAD                                                          
VR1360   EQU   *                                                                
*                                                                               
         B     COMMGOOD                                                         
*                                                                               
         EJECT                                                                  
*                                                                               
*- VAL RTN 14 --                                                                
*                                                                               
*  THIS ROUTINE IS A DEPARTURE FROM THE NORMAL FLOW OF THIS PROGRAM.            
*  THE '10' REPORT WILL USE THE SHORT DEFAULT FILTERS AS WELL AS                
*  SPECIAL FILTERS (CONTRACT # AND/OR NO-RATE-COPY).  THESE OPTIONS             
*  A) ARE MUTUALLY EXCLUSIVE  B) REQUIRE DIFFERENT REPORT SEQUENCE              
*  NUMBERS  C) NEED SUBSTANTIAL CROSS-VALIDATION OF OPTIONS                     
*                                                                               
*           ARGUMENT TABLE                                                      
*(VERTICAL ARGUMENTS ARE 'IF ENTERED'.  HORIZONTAL ARGUMENTS ARE                
*    ALLOW/DISALLOW/REQUIRE )                                                   
*                BASIC   CONTRACT#  NO-RATE  PERIOD   GROUP   SEQ#              
*             | FILTERS|          |   COPY | ST/END | FILTER |     |            
*-------------|--------|----------|--------|--------|--------|-----|---         
*BASIC FILTERS|XXXXXXXX| DISALLOW |DISALLOW| ALLOW  | ALLOW  |  S  |            
*-------------|--------|----------|--------|--------|--------|-----|---         
*CONTRACT #   |DISALLOW|XXXXXXXXXX|  ALLOW |DISALLOW|DISALLOW|  1  |            
*-------------|--------|----------|--------|--------|--------|-----|---         
*NO-RATE-COPY |DISALLOW|   ALLOW  |XXXXXXXX|DISALLOW|DISALLOW|  1  |            
*-------------|--------|----------|--------|--------|--------|-----|---         
*PERIOD ST/END|  ALLOW | DISALLOW |DISALLOW|XXXXXXXX| REQUIRE|  S  |            
*-------------|--------|----------|--------|--------|--------|-----|---         
*GROUP FILTER |  ALLOW | DISALLOW |DISALLOW| ALLOW  |XXXXXXXX|  S  |            
*-------------|--------|----------|--------|--------|--------|-----|---         
*                                                                               
*                                                                               
VALR14   EQU   *                                                                
         CLI   RSEQ,C'1'           SEQ# SET BY CNTRCT# ENTRY?                   
         BE    VALR14A0            YES - LEAVE IT                               
         MVI   RSEQ,C'S'           SET TO FILTERS IF UNSET                      
VALR14A0 EQU   *                                                                
         CLI   ROPTN,C'N'          NO-RATE-COPY SELECTED?                       
         BNE   VALR14B0            NO                                           
         CLI   RSEQ,C'1'           NEED CONTRACT #                              
         B     COMMGOOD            CONTRACT # PRESENT - OKAY                    
*                                                                               
*    PERMIT REQUEST ENTRY FOR ALL CONTRACTS                                     
*                                                                               
***>>>   BE    COMMGOOD            CONTRACT # PRESENT - OKAY                    
         GOTO1 PUTCURS,P1,VOPTION1,NORTCOPY,L'NORTCOPY                          
         B     COMMBAD                                                          
VALR14B0 EQU   *                                                                
         CLI   RSEQ,C'1'                                                        
         BE    COMMGOOD            CNTRCT# ONLY ENTERED                         
*                                                                               
*   AT THIS POINT, REQUEST IS NOT FOR CONTRACT # AND/OR NO-RATE-COPY            
*                                                                               
*                                                                               
         LA    R2,RQSWHENH         TEST IF 'SOON'                               
         CLC   =C'SOON',8(R2)      'SOON' REQUIRES STATION                      
         BNE   VALR14C0                                                         
*                                                                               
*  IF NO OTHER FILTERS, REQUIRE 'STATION' AS MINIMUM FILTER                     
*                                                                               
         CLC   ROFFR(LREQFLD1),SPACES    ANY FILTERS - 1ST PART                 
         BNE   VALR14C0                                                         
         CLC   ROPTN2(LREQFLD2),SPACES   ANY FILTERS - 2ND PART                 
         BNE   VALR14C0                                                         
         CLC   RSTA,SPACES         ANY STATION?                                 
         BNE   VALR14C0            NO STATION = ERROR                           
         GOTO1 PUTCURS,P1,VSTATION,NEEDSTAT,L'NEEDSTAT                          
         B     COMMBAD                                                          
VALR14C0 EQU   *                                                                
         CLC   RSTRD(12),SPACES    'PERIOD' REQUIRES GROUP                      
         BE    VALR14D0                                                         
*                                                                               
*  IF NO OTHER FILTERS, REQUIRE 'GROUP' AS MINIMUM FILTER                       
*                                                                               
         CLC   ROFFR(LREQFLD4),SPACES    ANY FILTERS - 1ST PART                 
         BNE   VALR14D0                                                         
         CLC   ROPTN2(LREQFLD2),SPACES   ANY FILTERS - 2ND PART                 
         BNE   VALR14D0                                                         
         GOTO1 PUTCURS,P1,VGRPSUBG,NEEDGRUP,L'NEEDGRUP                          
         B     COMMBAD                                                          
VALR14D0 EQU   *                                                                
         CLC   ROFFR(LREQFLD1),SPACES    ANY FILTERS - 1ST PART                 
         BNE   COMMGOOD            FILTERS ENTERED - OKAY                       
         CLC   ROPTN(LREQFLD3),SPACES   ANY FILTERS - REMAINDER                 
         BNE   COMMGOOD            FILTERS ENTERED - OKAY                       
         GOTO1 PUTCURS,P1,VCONTRC2,NEEDCNTR,L'NEEDCNTR                          
         B     COMMBAD                                                          
         EJECT                                                                  
*                                                                               
*- VAL RTN 15 -- INTEREP 1C BUDGET ALLOCATION VALIDATION                        
*                                                                               
*        1. 12 MONTH REQUIRED                                                   
*        2. PERCENTS MUST ADD TO 100                                            
*                                                                               
VALR15   EQU   *                                                                
         BAS   RE,NEED12                                                        
         BNZ   COMMBAD                                                          
         GOTO1 =A(BUD12),DMCB,(RC),RR=SUBRELO                                   
         BNZ   COMMBAD                                                          
*                                                                               
         LA    R3,R2ALCLST                                                      
         LA    R4,5                                                             
         SR    R5,R5                ADD THEM IN R5                              
VALR1510 EQU   *                                                                
         CLI   0(R3),0                                                          
         BE    VALR1520                                                         
         CLI   0(R3),C' '                                                       
         BE    VALR1520                                                         
         PACK  DUB(8),1(3,R3)                                                   
         CVB   RF,DUB                                                           
         AR    R5,RF                                                            
         LA    R3,4(R3)                                                         
         BCT   R4,VALR1510                                                      
*                                                                               
VALR1520 EQU   *                                                                
         CH    R5,=H'100'                                                       
         BNE   COMMBAD                                                          
         B     COMMGOOD                                                         
         EJECT                                                                  
*                                                                               
*- VAL RTN 16 -- R30 YADR REPORT                                                
*                                                                               
*        1. DATES CAN'T EXCEED 12 MONTHS                                        
*                                                                               
*        2. SERVICE, BOOK, AND DEMO CODE MUST ALL BE PRESENT                    
*           IF ANY FIELD HAS BEEN ENTERED.                                      
*                                                                               
VALR16   EQU   *                                                                
         BAS   RE,NEEDNG12         CAN'T EXCEED 12 MONTHS                       
         BNZ   COMMBAD                                                          
         CLC   R2RSVC(12),SPACES   ANYTHING IN SVC,BOOK,DEMO?                   
         BE    VALR0099            NO  - FINISHED                               
*                                                                               
*  IF ONE FIELD HAS BEEN ENTERED, ALL MUST BE ENTERED.  THIS                    
*    MAY CHANGE IN THE FUTURE.                                                  
*                                                                               
         LA    R2,VRSERV                                                        
         CLC   R2RSVC,SPACES       ANYTHING IN SERVICE?                         
         BE    VALR0098            NO  - ERROR                                  
         LA    R2,VRBOOK                                                        
         CLC   R2RBOOK,SPACES      ANYTHING IN BOOK?                            
         BE    VALR0098            NO  - ERROR                                  
         LA    R2,VDEMOCD                                                       
         CLC   R2DEMO,SPACES       ANYTHING IN DEMO?                            
         BNE   VALR0099            YES - FINISHED                               
VALR0098 EQU   *                                                                
         GOTO1 PUTCURS,P1,(R2),ALLFLDS,L'ALLFLDS                                
         B     COMMBAD                                                          
VALR0099 EQU   *                                                                
         B     COMMGOOD                                                         
         SPACE 5                                                                
*                                                                               
*- VAL RTN 17                                                                   
*                                                                               
*        1. GROUP CODE MUST BE FILLED IN                                        
*        2. END DATE DEFAULTS TO START DATE IF NOT GIVEN                        
*        3. 12 MONTH MAX TEST                                                   
*                                                                               
VALR17   EQU   *                                                                
         BAS   RE,GRPVAL           GROUP IS REQUIRED                            
         BNZ   COMMBAD                                                          
*                                                                               
         CLI   RGROUP,C'T'         MUST BE TV                                   
         BE    V17R08                                                           
*                                                                               
         GOTO1 PUTCURS,P1,VGRPSUBG,NEEDTV,L'NEEDTV                              
         B     COMMBAD                                                          
*                                                                               
V17R08   EQU   *                                                                
         CLC   RENDD,=6C' '        DEFAULT END TO START                         
         BNE   V17R10                                                           
         MVC   RENDD,RSTRD                                                      
V17R10   EQU   *                                                                
         BAS   RE,MAX12            12 MONTH MAX TEST                            
         BNZ   COMMBAD                                                          
         B     COMMGOOD                                                         
         SPACE 4                                                                
*                                                                               
* - VAL RTN 18  - INTEREP BUDGET PROJECTION                                     
*                                                                               
*        1.  MAX 12 MONTHS ON REQUEST                                           
*        2.  DATE RANGE MUST BE WITHIN BUDGET YEAR - CAN'T OVERLAP              
*        3.  NEED OPTIONS 1 AND 2                                               
*                                                                               
VALR18   EQU   *                                                                
         BAS   RE,MAX12                                                         
         BNZ   COMMBAD                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'           RETRIEVE REP RECORD                          
         MVC   KEY+25(2),TWAAGY    INSERT REP CODE                              
         GOTO1 DIRREAD                                                          
         GOTO1 FILREAD                                                          
*                                                                               
         L     RE,AIOADDR2         SET ADDRESSABILITY TO RECORD                 
         USING RREPREC,RE                                                       
*                                                                               
         ZIC   R3,RREPFMON         BUDGET START MONTH                           
         DROP  RE                                                               
         PACK  DUB(8),RSTRD+2(2)   REQUEST START MONTH                          
         CVB   R1,DUB              CONVERT TO BINARY                            
         CR    R1,R3               REQ START VS BUDGET START                    
         BL    VR1895              ERROR: REQ START < BUDGET START              
         PACK  DUB(8),RENDD+2(2)   REQUEST END MONTH                            
         CVB   R1,DUB                                                           
         CLC   RSTRD(2),RENDD      START/END IN SAME YEAR?                      
         BE    VR1808              YES                                          
         LA    R1,12(R1)           NO  - ADD 12 TO END MONTH                    
*                                                                               
VR1808   EQU   *                                                                
         LA    R3,11(R3)           CALC MONTH END OF BUDGET YEAR                
VR1810   EQU   *                                                                
         CR    R1,R3               END MONTH VS BUDGET END MONTH                
         BNH   COMMGOOD            END DATE WITHIN BUDGET YEAR ALSO             
VR1895   EQU   *                                                                
         GOTO1 PUTCURS,P1,VSTARTDT,MONEXBUD,L'MONEXBUD                          
         B     COMMBAD                                                          
         SPACE 4                                                                
*                                                                               
* - VAL RTN 19  - 16(CLOSE-OUT REPORT)                                          
*                                                                               
*        1.  MAX 12 MONTHS ON REQUEST                                           
*        2.  FIRST MONTH CANNOT BE CLOSED ACCORDING TO THE EOM RECORD           
*             --- UNLESS YOU ARE A DDS TERMINAL                                 
*                                                                               
VALR19   EQU   *                                                                
         BAS   RE,MAX12                                                         
         BNZ   COMMBAD                                                          
*                                                                               
         L     RE,AREQNTRY                                                      
         CLI   ROPTN3,C'Y'         REVERSE CLOSEOUT REQUEST?                    
         BNE   VR1920              NO                                           
         CLC   RSTRD,RENDD         YES - START = END DATES?                     
         BE    VR1920              YES - ACCEPT IT - CHECK SPECIAL              
         GOTO1 PUTCURS,P1,VSTARTDT,REVCLOSE,L'REVCLOSE                          
*                                  NO  - SEND BACK MESSAGE                      
         B     COMMBAD                                                          
VR1920   EQU   *                                                                
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   VR1960              NO                                           
         CLC   =C'OVERRIDE',RQSNAME                                             
*                                  YES - SPECIAL REQUEST TO CLOSE OUT           
*                                     PRIOR MONTHS?                             
         BE    COMMGOOD            YES - PERMIT IT                              
*                                  NO  - CHECK DATES                            
VR1960   EQU   *                                                                
         MVC   WORK(4),RSTRD                                                    
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+6)                                  
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'18'           RETRIEVE EOM RECORD                          
         MVC   KEY+24(2),TWAAGY    INSERT REP CODE                              
         MVC   KEY+26(1),WORK+6                                                 
         GOTO1 DIRREAD                                                          
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   VR1980                                                           
         GOTO1 FILREAD                                                          
*                                                                               
         L     RE,AIOADDR2         SET ADDRESSABILITY TO RECORD                 
         USING REOMREC,RE                                                       
*                                                                               
         LA    R2,REOMDATE                                                      
         DROP  RE                                                               
         ZIC   R3,WORK+7           GET MONTH NUMBER                             
         SLL   R3,1                *2 BYTES PER                                 
         AR    R2,R3                                                            
         GOTO1 DATCON,DMCB,(5,0),(2,WORK+9)                                     
         CLC   WORK+9(2),0(R2)                                                  
         BNH   COMMGOOD          TODAY MUST BEFORE THE MONTH'S END DATE         
         GOTO1 PUTCURS,P1,VSTARTDT,EOMPAST,L'EOMPAST                            
         B     COMMBAD                                                          
         SPACE 4                                                                
VR1980   EQU   *                                                                
         GOTO1 PUTCURS,P1,VSTARTDT,NOEOMREC,L'NOEOMREC                          
         B     COMMBAD                                                          
         SPACE 4                                                                
*                                                                               
* - VAL RTN 20  - 1G (OFFICE BUDGET REPORT)                                     
*                                                                               
*        1.  12 MONTHS ON REQUEST                                               
*        2.  NEED ONE OF FOLLOWING:                                             
*            SUBGROUP                                                           
*            OFFICE                                                             
*            TEAM                                                               
*                                                                               
VALR20   EQU   *                                                                
*                                                                               
         BAS   RE,MAX12            NO MORE THAN 12 MONTHS IN DATES              
         BNZ   COMMBAD                                                          
*                                                                               
         CLC   RGROUP(2),SPACES    ANY GROUP OR GROUP/SUBGROUP?                 
         BNE   COMMGOOD            YES - ACCEPT IT                              
         CLC   ROFF(2),SPACES      ANY OFFICE?                                  
         BNE   COMMGOOD            YES - ACCEPT IT                              
         CLC   RSDIV(2),SPACES     ANY DIVISION/TEAM?                           
         BNE   COMMGOOD            YES - ACCEPT IT                              
         GOTO1 PUTCURS,P1,VGRPSUBG,NEEDCNTR,L'NEEDCNTR                          
         B     COMMBAD                                                          
         SPACE 4                                                                
*                                                                               
*                                                                               
* - VAL RTN 21  - 5F (COMMISSION/REVENUE)                                       
*        GROUP CODE MUST BE FILLED IN                                           
*                                                                               
VALR21   EQU   *                                                                
         BAS   RE,GRPVAL           GROUP REQUIRED                               
         BNZ   COMMBAD                                                          
         B     COMMGOOD                                                         
         EJECT                                                                  
*                                                                               
*- VAL RTN 22                                                                   
*                                                                               
*        1. GROUP CODE MUST BE FILLED IN                                        
*        2. END DATE DEFAULTS TO START DATE IF NOT GIVEN                        
*        3  12 MONTH MAX TEST                                                   
*        4. BLAIR STATION/OFFICE TEST                                           
*        5. QUARTERLY REQUEST CHECK.                                            
*                                                                               
VALR22   EQU   *                                                                
         BAS   RE,GRPVAL           GROUP IS REQUIRED                            
         BNZ   COMMBAD                                                          
*                                                                               
         CLC   RENDD,=6C' '        DEFAULT END TO START                         
         BNE   V22R10                                                           
         MVC   RENDD,RSTRD                                                      
*                                                                               
*- R24/R25 - 12 MONTH MAX TEST                                                  
V22R10   EQU   *                                                                
         BAS   RE,MAX12            12 MONTH MAX TEST                            
         BNZ   COMMBAD                                                          
*                                                                               
*- REP-SPECIFIC PROCESSING --                                                   
*  BLAIR TV REQUIRES STATION OR OFFICE                                          
         BAS   RE,BLRSTAOF                                                      
         BNZ   COMMBAD                                                          
*                                                                               
*- CHECK FOR WHOLE NUMBER OF QUARTERS IF QUARTERLY REQUEST.                     
         BAS   RE,QUARTRCK                                                      
         BNZ   COMMBAD                                                          
*                                                                               
         B     COMMGOOD            REQUEST PASSED                               
         EJECT                                                                  
*                                                                               
*- VAL RTN 23                                                                   
*                                                                               
*        1. GROUP CODE MUST BE FILLED IN                                        
*        2. END DATE DEFAULTS TO START DATE IF NOT GIVEN                        
*        3. BLAIR STATION/OFFICE TEST                                           
*        4. QUARTERLY REQUEST CHECK.                                            
*                                                                               
VALR23   EQU   *                                                                
         BAS   RE,GRPVAL           GROUP IS REQUIRED                            
         BNZ   COMMBAD                                                          
*                                                                               
         CLC   RENDD,=6C' '        DEFAULT END TO START                         
         BNE   V23R10                                                           
         MVC   RENDD,RSTRD                                                      
V23R10   EQU   *                                                                
*                                                                               
*- R24/R25 - 12 MONTH MAX TEST                                                  
         BAS   RE,MAX12            12 MONTH MAX TEST                            
         BNZ   COMMBAD                                                          
*                                                                               
V23R20   EQU   *                                                                
*                                                                               
*- REP-SPECIFIC PROCESSING --                                                   
*  BLAIR TV REQUIRES STATION OR OFFICE                                          
         BAS   RE,BLRSTAOF                                                      
         BNZ   COMMBAD                                                          
*                                                                               
*- CHECK FOR WHOLE NUMBER OF QUARTERS IF QUARTERLY REQUEST.                     
         BAS   RE,QUARTRCK                                                      
         BNZ   COMMBAD                                                          
*                                                                               
         B     COMMGOOD            REQUEST PASSED                               
         EJECT                                                                  
*                                                                               
*- VAL RTN 24                                                                   
*                                                                               
*        1. GROUP CODE MUST BE FILLED IN                                        
*        2. FOR DATE RANGE, SITUATION ANALYSIS RECORD MUST                      
*           EXIST FOR THIS STATION.                                             
*                                                                               
VALR24   EQU   *                                                                
*                                                                               
         GOTO1 =A(NVALR24),DMCB,(RC),RR=SUBRELO                                 
         BNZ   COMMBAD                                                          
         B     COMMGOOD                                                         
         EJECT                                                                  
*                                                                               
*   VALR25  - SPECIAL VALIDATION FOR THE 14 REPORT                              
*                                                                               
VALR25   EQU   *                                                                
*                                                                               
         CLC   RSTA,SPACES         ANY STATION?                                 
         BE    VALR25C0            NO  - ACCEPTED                               
*                                  YES - STATION NOT PERMITTED                  
         GOTO1 PUTCURS,P1,VSTATION,NOSTAT,L'NOSTAT                              
         B     COMMBAD                                                          
VALR25C0 EQU   *                                                                
         BAS   RE,MAX12                                                         
         BNZ   COMMBAD                                                          
         CLC   =C'SOON',RQSWHEN    TEST IF 'SOON'                               
         BNE   VALR2540                                                         
         CLI   ROPTN,C'S'          SOON:  OPTION = SOFT?                        
         BE    VALR2540            YES                                          
         GOTO1 PUTCURS,P1,VOPTION1,NEEDSOFT,L'NEEDSOFT                          
         B     COMMBAD                                                          
VALR2540 EQU   *                                                                
         B     COMMGOOD                                                         
         EJECT                                                                  
*                                                                               
*   VALR26  - SPECIAL VALIDATION FOR THE 15 REPORT                              
*                                                                               
VALR26   EQU   *                                                                
*                                                                               
         CLI   ROPTN,C'R'          'RECEIVE' REQUEST?                           
         BE    VALR2640            YES - NO DATE NEEDED                         
*                                  NO  - 'SEND' REQUIRES DATE                   
         CLC   RASAD,SPACES        ANY DATE ENTERED?                            
         BNZ   VALR2540            YES - ACCEPT REQUEST                         
         GOTO1 PUTCURS,P1,VASATDAT,NEEDASAT,L'NEEDASAT                          
         B     COMMBAD                                                          
VALR2640 EQU   *                                                                
         B     COMMGOOD                                                         
*                                                                               
*   VALR27  - SPECIAL VALIDATION FOR THE TK REPORT:                             
*        RETRIEVES THE STATION RECORD ENTERED, AND CALCULATES                   
*        THE TAKEOVER DATE, IF NOT IN ERROR.                                    
*                                                                               
VALR27   EQU   *                                                                
*                                                                               
         GOTO1 =A(NVALR27),DMCB,(RC),RR=SUBRELO                                 
         BNZ   COMMBAD                                                          
         B     COMMGOOD                                                         
         EJECT                                                                  
*                                                                               
*   VALR28  - SPECIAL VALIDATION FOR THE 78 REPORT:                             
*        IF OPTION IS 'DOWNLOAD', OPTION1 MUST BE 'C'                           
*                                                                               
VALR28   EQU   *                                                                
*                                                                               
         GOTO1 =A(NVALR28),DMCB,(RC),RR=SUBRELO                                 
         BNZ   COMMBAD                                                          
         B     COMMGOOD                                                         
         EJECT                                                                  
*                                                                               
*   VALR29  - SPECIAL VALIDATION FOR THE 70 REPORT:                             
*        IF OPTION3 HAS INPUT, MUST DATE FLD FILLED IN                          
*                                                                               
VALR29   EQU   *                                                                
*                                                                               
         GOTO1 =A(NVALR29),DMCB,(RC),RR=SUBRELO                                 
         BNZ   COMMBAD                                                          
         B     COMMGOOD                                                         
         EJECT                                                                  
*                                                                               
*                                                                               
         TITLE 'SUBROUTINES USED BY WHOLE-REQUEST EDITS'                        
*                                                                               
*- DELIMITER -- SET UP P3 WITH DELIMITER FOR SCANNER.                           
*                                                                               
*  INSPECT USER FIELD FOR POSSIBLE DELIMITERS.                                  
*                                                                               
*  R2 = A(FIELD HEADER)                                                         
*                                                                               
DELIMITR NTR1                                                                   
         MVC   P3,=CL4',=,='       DEFAULT TO , AND =                           
         ZIC   R0,5(R2)            IPT LENGTH FOR LOOP COUNTER                  
         LA    R2,8(R2)                                                         
DELIM20  EQU   *                                                                
         CLI   0(R2),C','          COMMA?                                       
         BE    DELIM40                                                          
         CLI   0(R2),C'-'          DASH?                                        
         BE    DELIM40                                                          
         LA    R2,1(R2)                                                         
         BCT   R0,DELIM20                                                       
         B     DELIM50             NO HIT.  LEAVE P3 AS DEFAULT                 
DELIM40  MVC   P3+2(1),0(R2)       FIELD SEPARATOR TO P3                        
DELIM50  B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
*- MAX12 -- START AND END DATES MUST BE WITHIN 12 MONTHS                        
*                                                                               
*  CC 0 = GOOD, ^0 = FAILED                                                     
*                                                                               
MAX12    NTR1                                                                   
         TM    RFPSTAT,RFPINUSE                                                 
         BO    COMMGOOD            RFP KEYWORD IN USE, SKIP                     
*                                                                               
         CLC   RSTRD(12),SPACES    NO TEST IF NO DATES.                         
         BE    COMMGOOD                                                         
*                                                                               
MAX12_10 DS    0H                                                               
         GOTO1 =A(CHECK12),DMCB,(R9),RR=SUBRELO                                 
         CLC   P4+2(2),=X'000D'    13 MONTHS OR MORE?                           
         BL    COMMGOOD            NO  - IT'S OKAY TO PROCESS                   
*                                                                               
MAX12_20 DS    0H                                                               
         GOTO1 PUTCURS,P1,VSTARTDT,('ECMSGNUM',MAX12MON),0                      
         B     COMMBAD                                                          
         SPACE 2                                                                
*                                                                               
*- NEED12 -- START AND END DATES MUST BE 12 MONTHS                              
*                                                                               
*  CC 0 = GOOD, ^0 = FAILED                                                     
*                                                                               
NEED12   NTR1                                                                   
         TM    RFPSTAT,RFPINUSE                                                 
         BO    COMMGOOD            RFP KEYWORD IN USE, SKIP                     
*                                                                               
         CLC   RSTRD(12),SPACES    BAD IF NO DATES.                             
         BE    COMMBAD                                                          
*                                                                               
         GOTO1 =A(CHECK12),DMCB,(R9),RR=SUBRELO                                 
         CLC   P4+2(2),=X'000C'    12 MONTHS?                                   
         BE    COMMGOOD            YES - IT'S OKAY TO PROCESS                   
*                                                                               
*                                                                               
         GOTO1 PUTCURS,P1,VSTARTDT,('ECMSGNUM',NEED12M),0                       
         B     COMMBAD                                                          
         SPACE 2                                                                
*                                                                               
*- NEEDNG12 -- START AND END DATES MUST BE 12 MONTHS OR LESS                    
*                                                                               
*  CC 0 = GOOD, ^0 = FAILED                                                     
*                                                                               
NEEDNG12 NTR1                                                                   
         TM    RFPSTAT,RFPINUSE                                                 
         BO    COMMGOOD            RFP KEYWORD IN USE, SKIP                     
*                                                                               
         CLC   RSTRD(12),SPACES    BAD IF NO DATES.                             
         BE    NNG0010                                                          
*                                                                               
         GOTO1 =A(CHECK12),DMCB,(R9),RR=SUBRELO                                 
         CLC   P4+2(2),=X'000C'    12 MONTHS OR LESS                            
         BNH   COMMGOOD            YES - IT'S OKAY TO PROCESS                   
*                                                                               
NNG0010  EQU   *                                                                
         GOTO1 PUTCURS,P1,VSTARTDT,NEEDLT13,L'NEEDLT13                          
         B     COMMBAD                                                          
         SPACE 2                                                                
*                                                                               
*- CHECK FOR WHOLE NUMBER OF QUARTERS ON QUARTERLY FORMAT REQUESTS.             
*  A 'Q' IN OPTION 6 INDICATES QUARTERLY FORMAT                                 
*                                                                               
*  CC 0 = GOOD, ^0 = FAILED                                                     
*                                                                               
QUARTRCK NTR1                                                                   
         TM    RFPSTAT,RFPINUSE                                                 
         BO    COMMGOOD            RFP KEYWORD IN USE, SKIP                     
*                                                                               
         CLI   R2OPT6,C'Q'         TEST FOR QUARTERS FORMAT                     
         BNE   COMMGOOD                                                         
         CLC   RSTRD,RENDD         START DATE = END DATE - ERROR                
         BE    QCKBAD                                                           
         PACK  DUB,RSTRD+2(2)      START/END DATES DEFINE                       
         CVB   R0,DUB                  A WHOLE NUMBER OF QUARTERS               
         BCTR  R0,0                BACK UP TO 'END OF PREVIOUS QUARTER'         
         SRDA  R0,32                                                            
         D     R0,=F'3'                                                         
         LTR   R0,R0                                                            
         BNZ   QCKBAD                                                           
         PACK  DUB,RENDD+2(2)                                                   
         CVB   R0,DUB                                                           
         SRDA  R0,32                                                            
         D     R0,=F'3'                                                         
         LTR   R0,R0                                                            
         BZ    COMMGOOD                                                         
*                                                                               
QCKBAD   EQU   *                                                                
         GOTO1 PUTCURS,P1,VSTARTDT,WHOLEQ,L'WHOLEQ                              
         B     COMMBAD                                                          
         SPACE 2                                                                
*                                                                               
*- BLAIR TV GROUPS -- STATION OR OFFICE IS REQUIRED                             
*                                                                               
BLRSTAOF NTR1                                                                   
         GOTO1 PUTCURS,P1,VOFFICEC,('ECMSGNUM',OFFSTA),0                        
*                                                                               
*- DDS TUBES NOT REQUIRED TO ENTER FIELDS IF REQUESTOR = DDS                    
*                                                                               
         CLC   =C'DDS',RQSOPT                                                   
         BNE   BL0010                                                           
*                                                                               
         CLI   TWAOFFC,C'*'                                                     
         BE    COMMGOOD                                                         
*                                                                               
BL0010   EQU   *                                                                
         CLC   RREP,=C'BL'         FOR BLAIR ONLY                               
         BNE   COMMGOOD                                                         
         CLI   RGROUP,C'T'         IF GROUP IS TV -                             
         BNE   COMMGOOD                                                         
         CLC   ROFF,=2C' '         CHECK FOR OFFICE OR STATION                  
         BNE   COMMGOOD                                                         
         CLC   RSTA,=5C' '                                                      
         BE    COMMBAD                                                          
         OC    RSTA(5),RSTA                                                     
         BZ    COMMBAD                                                          
         B     COMMGOOD                                                         
         SPACE 2                                                                
*                                                                               
*- GROUP CODE MUST BE FILLED IN IN REQUEST CARD                                 
*  CC 0 = GOOD, ^0 = BAD                                                        
GRPVAL   NTR1                                                                   
         SR    R3,R3               ASSUME OK                                    
         CLI   WHEN,EQSOON         SOON REQUEST?                                
         BE    GRPVAL20            YES - MINIMUM FILT ALREADY TESTED            
*                                                                               
         CLI   RGROUP,C' '         GROUP ENTERED?                               
         BNE   GRPVAL20            YES                                          
         CLI   IAMAMAST,C'Y'       NO - MASTER REQUEST?                         
         BNE   GRPVAL05            NO                                           
         CLC   RSTA,SPACES         YES - ANY STATION ENTERED?                   
         BNE   GRPVAL20            YES - ACCEPT BLANK GROUP                     
*                                                                               
GRPVAL05 EQU   *                                                                
*                                                                               
*- DDS TUBES NOT REQUIRED TO ENTER FIELDS IF REQUESTOR = DDS                    
*                                                                               
         CLC   =C'DDS',RQSOPT                                                   
         BNE   GRPVAL10                                                         
*                                                                               
         CLI   TWAOFFC,C'*'                                                     
         BE    GRPVAL20                                                         
*                                                                               
GRPVAL10 EQU   *                                                                
         LR    R3,RD               SET TO ^0 (BAD)                              
         GOTO1 PUTCURS,P1,VGRPSUBG,('ECMSGNUM',MISSING),0                       
*                                                                               
GRPVAL20 LTR   R3,R3               SET CC                                       
         B     COMMXIT                                                          
         SPACE 2                                                                
*                                                                               
*- DTEVAL -- START/END DATES MUST BOTH BE ENTERED OR                            
*            NEITHER MAY BE ENTERED.                                            
*                                                                               
DTEVAL   NTR1                                                                   
         CLI   RSTRD,C' '          MUST HAVE BOTH                               
         BE    DTEV10                                                           
         CLI   RENDD,C' '                                                       
         BE    DTEVERR             START W/O END                                
         B     COMMGOOD                                                         
*                                                                               
DTEV10   CLI   RENDD,C' '          OR NEITHER                                   
         BE    COMMGOOD                                                         
*                                                                               
DTEVERR  EQU   *                                                                
         GOTO1 PUTCURS,P1,VSTARTDT,BOTHNONE,L'BOTHNONE                          
         B     COMMBAD             ^0 = BAD                                     
*                                                                               
         TITLE 'PUTCURS -- FIELD FINDED/ERROR MSG SETTING'                      
*                                                                               
*- PUTCURS -- FIND FIELD ON SCREEN FOR CURSOR POSITIONING.                      
*             PUT UP ERROR MESSAGES.                                            
*                                                                               
*  INPUT:  P1 = FIELD NUMBER EQUATE. ('V' FIELD EQUATE)                         
*          P2/P3 = MOVED TO 'AERROR' FOR DOMSG CALL.                            
*                                                                               
*  NOTE:  EXACT FIELD SELECTED FOR POSITION IF FOUND IN REQMAP.                 
*         IF NOT FOUND, REQMAP SCANNED FOR FUNCTIONALLY                         
*         EQUIVALENT FIELD.  FAILING THAT, CURSOR PLACED IN ACTION.             
*                                                                               
PUTCURS  NTR1                                                                   
         MVC   AERROR(8),4(R1)     PASS P2/P3 TO DOMSG                          
         TM    AERROR,ECMSGNUM                                                  
         BZ    PC050                                                            
         L     RF,AERROR           LOAD ERR MSG # TO DATA BYTE                  
         STC   RF,ERRDATA                                                       
*                                                                               
*- FIRST, TRY FOR EXACT MATCH UP BETWEEN FIELD EQU AND REQ MAP.                 
PC050    BAS   RE,LOOKMAP                                                       
         BZ    COMMXIT                                                          
*                                                                               
*- IS THERE AN EQUIVALENT VALUE FIELD NUMBER?                                   
         BAS   RE,EQUIVFLD                                                      
         BNZ   PC200               NO. CURSOR TO ACTION                         
*                                                                               
*- LOOK FOR EQUIVALENT FIELDS IN REQMAP.                                        
         L     R2,P2               A(EQUIV FIELD LIST)                          
PC100    CLC   =XL2'00',0(R2)                                                   
         BE    PC200               NO EQUIV FIELD EITHER                        
*                                                                               
         MVC   P1+2(2),0(R2)       2 BYTE FIELD NUMBER                          
         BAS   RE,LOOKMAP                                                       
         BZ    COMMXIT             A HIT!                                       
*                                                                               
         LA    R2,2(R2)            NEXT EQUIV FIELD                             
         B     PC100                                                            
         SPACE                                                                  
*                                                                               
*- COURT OF LAST RESORT....ACTION FIELD.                                        
PC200    LA    RE,RQSACTH                                                       
         ST    RE,CURSPOS                                                       
*                                                                               
PCEXT    EQU   *                                                                
         B     COMMXIT                                                          
         EJECT                                                                  
*                                                                               
*- LOOKMAP -- SEARCH REQMAP FOR FIELD IN P1. (LOW 2 BYTES)                      
*             IF FOUND, POINT CURSOR TO THIS FIELD                              
LOOKMAP  NTR1                                                                   
         LA    R1,REQMAP                                                        
LKMAP20  CLC   =XL2'00',0(R1)      END OF MAP?                                  
         BE    COMMBAD                                                          
         CLC   MAPVAL(2,R1),P1+2   SAME FIELD?                                  
         BE    LKMAP40                                                          
         LA    R1,MAPLNTRY(R1)                                                  
         B     LKMAP20                                                          
*                                                                               
LKMAP40  EQU   *                   A HIT. SET CURSPOS                           
         ICM   R2,15,MAPFLD(R1)    HEADER DISPLACEMENT                          
         AR    R2,RA               ...IN THE TWA                                
         ST    R2,CURSPOS                                                       
         B     COMMGOOD            FOUND FIELD.  CURSPOS SET.                   
         SPACE 2                                                                
*                                                                               
*- EQUIVFLD -- FIND EQUIVALENT FIELDS, IF ANY                                   
*        P1 = F'FIELD NUMBER'                                                   
*                                                                               
* RETURNS P2 = A(EQUIVALENT FIELD LIST)                                         
*                                                                               
* LIST OF 2 BYTE FIELD NUMBERS, ENDS WITH XL2'00'                               
*                                                                               
EQUIVFLD NTR1                                                                   
         LA    R1,EQFLDS                                                        
EQF20    CLC   =XL2'00',0(R1)                                                   
         BE    COMMBAD             NO EQUIV FIELDS                              
*                                                                               
         CLC   2(2,R1),P1+2        USER'S FIELD?                                
         BE    EQF40                                                            
*                                                                               
         LH    RF,0(R1)            DISP TO NEXT ENTRY                           
         AR    R1,RF                                                            
         B     EQF20                                                            
*                                                                               
EQF40    LA    RF,4(R1)            A(FIELD LIST)                                
         ST    RF,P2                                                            
         B     COMMGOOD            FOUND EQUIV LIST.                            
         EJECT                                                                  
         TITLE 'COMMON XIT LABELS'                                              
*                                                                               
*- COMMON XIT AND COND CODE SETTING BRANCH LABELS                               
*                                                                               
COMMGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     COMMTEST                                                         
*                                                                               
COMMBAD  EQU   *                                                                
         LA    R0,1                                                             
*                                                                               
COMMTEST EQU   *                                                                
         LTR   R0,R0                                                            
COMMXIT  EQU   *                                                                
         XIT1                                                                   
         TITLE 'EQUIVALENT FIELD LISTS'                                         
*                                                                               
*- EQUIVALENT FIELD LISTS.                                                      
*                                                                               
*  H'LENGTH OF ENTRY'  H'0' = END OF LIST                                       
*  H'PRIMARY FIELD NUMBER FOR MATCH'                                            
*  1-N H'EQUIVALENT FIELDS'  END FIELD LIST WITH H'0'                           
*                                                                               
EQFLDS   DS    0H                  HALF-WORD BOUNDARY                           
*                                                                               
ESTATION DC    AL2(XSTATION-*),AL2(VSTATION)     STATION                        
         DC    AL2(VSTAOWNR)                                                    
         DC    H'0'                                                             
XSTATION EQU   *                                                                
*                                                                               
ESTARTDT DC    AL2(XSTARTDT-*),AL2(VSTARTDT)     START DATE                     
         DC    AL2(VSTRTEND)                                                    
         DC    AL2(VPERDSTR)                                                    
         DC    AL2(VPERDSTE)                                                    
         DC    AL2(VASATDAT)                                                    
         DC    H'0'                                                             
XSTARTDT EQU   *                                                                
*                                                                               
         DC    H'0'                END OF EQUIVALENT FIELDS                     
         TITLE 'MAIN BODY LTORG'                                                
*                                                                               
* ----   LTORG USED WITHIN MAIN LTORG                                           
*                                                                               
         LTORG                                                                  
         TITLE 'VALIDATION ADDRESS LISTS'                                       
*                                                                               
*- EXTRA VALIDATION ROUTINE ADDRESS LIST.                                       
*                                                                               
*  ROUTINES TO DO CROSS-EDITS AFTER ALL FIELDS VALIDATED                        
*                                                                               
         DS    0A                            WORD ALIGNMENT                     
XVAL     EQU   *                                                                
         DC    A(VALR01)                     01                                 
         DC    A(VALR02)                     02                                 
         DC    A(VALR03)                     03                                 
         DC    A(VALR04)                     04                                 
         DC    A(VALR05)                     05                                 
         DC    A(VALR06)                     06                                 
         DC    A(VALR07)                     07                                 
         DC    A(VALR08)                     08                                 
         DC    A(VALR09)                     09                                 
         DC    A(VALR10)                     10 FOR ASAT DATE                   
         DC    A(VALR11)                     11 DONUT REPORT                    
         DC    A(VALR12)                     12 R18 CONTRACT SWITCH             
         DC    A(VALR13)                     13 R1A INTEREP BUDGETS             
         DC    A(VALR14)                     14 10  CONTRACTS                   
         DC    A(VALR15)                     15 R1C INTEREP BUDGETS             
         DC    A(VALR16)                     16 30  YADR REPORT                 
         DC    A(VALR17)                     17 BL REGIONAL REPORT              
         DC    A(VALR18)                     18 R1B INTEREP BUDGETS             
         DC    A(VALR19)                     19 R16 TO CHECK EOM REC            
         DC    A(VALR20)                     20 R1G OFFICE BUDGET               
         DC    A(VALR21)                     21 R5F BLAIR COMMISSION            
         DC    A(VALR22)                     22 RG0 G-SERIES REPORTS            
         DC    A(VALR23)                     23 VAL01 NOT W/IN 1 YEAR           
         DC    A(VALR24)                     24 GRP/SUBGROUP NEEDED             
         DC    A(VALR25)                     25 14 REPORT                       
         DC    A(VALR26)                     26 15 REPORT                       
*                                     NEEDS STRATEGY SIT ANALYSIS               
         DC    A(VALR27)                     27 TK REPORT                       
         DC    A(VALR28)                     28 78 REPORT                       
         DC    A(VALR29)                     29 70 REPORT (DTE FLTR)            
XVALX    EQU   *                                                                
         SPACE 2                                                                
*                                                                               
*- FIELD VALIDATION ADDRESS LIST                                                
*                                                                               
FLDVAL   EQU   *                                                                
         DC    A(OFFRVAL)          01 - REGION                                  
         DC    A(OFFVAL)           02 - OFFICE                                  
         DC    A(GSGVAL)           03 - GROUP/SUBGROUP                          
         DC    A(DUMP)      @@     04 - SUBGROUP ONLY                           
         DC    A(STALSTV)          05 - STATION LIST                            
         DC    A(SDIVVAL)          06 - SALES DIVISION/TEAM                     
         DC    A(SMANVAL)          07 - SALESMAN                                
         DC    A(STAVAL)           08 - STATION                                 
         DC    A(ADVVAL)           09 - ADVERTISER                              
         DC    A(AGYVAL)           10 - AGENCY                                  
         DC    A(PROCLVAL)         11 - PRODUCT CLASS                           
         DC    A(PROCAVAL)         12 - PRODUCT CATEGORY                        
         DC    A(CTYPVAL)          13 - CONTRACT TYPE                           
         DC    A(STATVAL)          14 - STATION TYPE                            
         DC    A(PRDVAL)           15 - PRODUCT                                 
         DC    A(STAVAL)           16 - STATION                                 
         DC    A(STRDVAL)          17 - START DATE                              
         DC    A(ENDDVAL)          18 - END DATE                                
         DC    A(ASADVAL)          19 - ASAT DATE                               
         DC    A(STRTENDV)         20 - START,END DATES                         
         DC    A(BASISVAL)         21 - BASIS                                   
         DC    A(STRTENDV)         22 - PERIOD START, END DATES                 
         DC    A(STRDVAL)          23 - PERIOD START                            
         DC    A(ENDDVAL)          24 - PERIOD END                              
         DC    A(SEQVAL)           25 - SEQUENCE                                
         DC    A(DEPTHV)           26 - DEPTH                                   
         DC    A(OPT1VAL)          27 - OPTION 1                                
         DC    A(EXTRADAT)         28 - EXTRA DATE RANGE                        
         DC    A(OPT2VAL)          29 - OPTION 2                                
         DC    A(STENMS)           30 - START, END MONDAY, SUNDAY               
         DC    A(AOPTVAL)          31 - ACCOUNTING OPTION                       
         DC    A(CUTOFF)           32 - CUTOFF DATE                             
         DC    A(CONVAL)           33 - CONTRACT                                
         DC    A(CONVAL2)          34 - CONTRACT ONLY OR FILTERS ONLY           
         DC    A(TCNTVAL)          35 - TOTALS CONTROL                          
         DC    A(DUMP)      **     36 - N/D                                     
         DC    A(OPT3VAL)          37 - OPTION 3                                
         DC    A(DUMP)      **     38 - N/D                                     
         DC    A(AFFVAL)           39 - AFFILATE                                
         DC    A(DUMP)      **     40 - N/D                                     
         DC    A(OPT4VAL)          41 - OPTION 4                                
         DC    A(DUMP)      **     42 - N/D                                     
         DC    A(OPT5VAL)          43 - OPTION 5                                
         DC    A(DUMP)      **     44 - N/D                                     
         DC    A(OPT6VAL)          45 - OPTION 6                                
         DC    A(DUMP)      **     46 - N/D                                     
         DC    A(OPT6VAL)          47 - FORMAT (OPTION 6)                       
         DC    A(DUMP)      **     48 - N/D                                     
         DC    A(RKMAXVAL)         49 - RANK MAX                                
         DC    A(DUMP)      **     50 - N/D                                     
         DC    A(OPT3VAL)          51 - RANK PERIOD (OPTION 3)                  
         DC    A(BYVAL)            52 - BUDGET YEAR                             
         DC    A(RCPVAL)           53 - RECAP                                   
         DC    A(STRDVAL)          54 - ONE MONTH ONLY PERIOD                   
         DC    A(PERDVAL)          55 - ONE MONTH ENDING PERIOD                 
         DC    A(CREDRAT)          56 - CREDIT RATING                           
         DC    A(LIABPOS)          57 - LIABILITY POSITION COMMENT              
         DC    A(DUMP)      **     58 - N/D                                     
         DC    A(DUMP)      **     59 - N/D                                     
         DC    A(TVBVAL)           60 - TVB REGION                              
         DC    A(OWNERVAL)         61 - STATION OWNER                           
         DC    A(MKTVAL)           62 - MARKET CODE                             
         DC    A(RANKVAL)          63 - STATION RANK                            
         DC    A(PPSONVAL)         64 - POINTPERSON                             
         DC    A(NCONVAL)          65 - NETWORK CONTRACT NUMBER                 
         DC    A(BUDSTEND)         66 - BUDGET START, END                       
         DC    A(ALCLSTV)          67 - BUDGET ALLOCATION LIST                  
         DC    A(ALCLST2)          68 - BUDGET ALLOC RESET LIST                 
         DC    A(YADREC)           69 - YADR RECORD                             
         DC    A(TARGAGY)          70 - TARGET AGENCY                           
         DC    A(RSERV)            71 - RATING SERVICE                          
         DC    A(DEMOCD)           72 - DEMOGRAPHIC CODE                        
         DC    A(RBOOK)            73 - RATING BOOK                             
         DC    A(LABREC)           74 - LABEL RECORD                            
*                                                                               
*   FOLLOWING TWO ENTRIES SHOULDN'T BE IN THIS TABLE:                           
*                                                                               
         DC    A(DUMP)      **     75 - 'ACTIVITY'                              
         DC    A(DUMP)      **     76 - 'START/END'                             
         DC    A(DEVSP)            77 - DEVELOPMENTAL SALESPERSON               
         DC    A(BUSTYP)           78 - NEW BUSINESS DEVELOPMENT TYPE           
         DC    A(SPLNVAL)          79 - SPOT LENGTH BREAK OUT                   
         DC    A(DUMP)      **     80 -                                         
         DC    A(DUMP)      **     81 -                                         
         DC    A(DUMP)      **     82 -                                         
         DC    A(DUMP)      **     83 -                                         
         DC    A(DUMP)      **     84 -                                         
         DC    A(DUMP)      **     85 -                                         
         DC    A(OLDREP)           86 - TAKEOVER OLD REP                        
         DC    A(TKOSTAC)          87 - TAKEOVER STATION                        
         DC    A(TKOAGYC)          88 - TAKEOVER AGENCY FILTER                  
         DC    A(TKOOFFC)          89 - TAKEOVER OFFICE FILTER                  
         DC    A(CLOSMON)          90 - CLOSED THRU MONTH                       
         DC    A(ASADVAL)          91 - COMMISSION EFFECTIVE DATE               
*                                       USE AS AT DATE                          
FLDVALX  EQU   *                                                                
         SPACE 2                                                                
DUMP     DS    H'0'                INVALID INDEX VALUE                          
         TITLE 'REREQ02 (T80702) - OPTION VALIDATION TABLES'                    
*                                                                               
*        OPTION COLUMN VALIDATION TABLES                                        
*         FOR OPTIONS 1 THRU 6                                                  
*                                                                               
         SPACE                                                                  
OPT1VTBL DS    0C                                                               
         DC    AL1(3+1),C'10',C'N'                                              
         DC    AL1(3+2),C'14',C'US'    UPDATE/SOFT                              
         DC    AL1(3+2),C'15',C'RS'    DARE STATION SWITCH                      
         DC    AL1(3+2),C'16',C'US'    UPDATE/SOFT                              
         DC    AL1(3+3),C'17',C'MAB'   DONUT REPORT                             
         DC    AL1(3+3),C'18',C'ATO'   CONTRACT SWITCH                          
         DC    AL1(3+2),C'19',C'YN'    INVOICE CONTROL LIST                     
         DC    AL1(3+4),C'1A',C'SORA'  BUDGET REPORT                            
         DC    AL1(3+4),C'1B',C'SORA'  BUDGET PROJECTION REPORT                 
         DC    AL1(3+4),C'1C',C'SOBT'  BUDGET ALLOCATION REPORT                 
         DC    AL1(3+4),C'1D',C'TZX$'  BUDGET ALLOC RESET REPORT                
         DC    AL1(3+2),C'1J',C'YN'    SUPPRESS RETURN ADDRESS                  
         DC    AL1(3+4),C'1L',C'SORA'  BUDGET PROJECTION REPORT                 
         DC    AL1(3+6),C'20',C'DEFGHI'                                         
         DC    AL1(3+6),C'21',C'DEFGHI'                                         
         DC    AL1(3+1),C'25',X'FF'                                             
         DC    AL1(3+3),C'30',C'DST'   YADR REPORT                              
         DC    AL1(3+1),C'34',C'S'     PERMIT 'STATION TOTALS'                  
         DC    AL1(3+2),C'37',C'WM'                                             
         DC    AL1(3+1),C'38',C'S'                                              
         DC    AL1(3+2),C'3G',C'WM'                                             
         DC    AL1(3+2),C'3H',C'WM'                                             
         DC    AL1(3+2),C'3I',C'WM'                                             
         DC    AL1(3+3),C'3U',C'DR '                                            
         DC    AL1(3+2),C'41',C'M '                                             
         DC    AL1(3+3),C'47',C'WMP'                                            
         DC    AL1(3+2),C'4B',C'C '                                             
         DC    AL1(3+2),C'4G',C'WM'                                             
         DC    AL1(3+4),C'70',C'ABID'                                           
         DC    AL1(3+3),C'71',C'OSB'                                            
         DC    AL1(3+2),C'72',C'FA'                                             
         DC    AL1(3+2),C'73',C'AB'                                             
         DC    AL1(3+2),C'74',C'YN'                                             
         DC    AL1(3+2),C'76',C'FL'                                             
         DC    AL1(3+3),C'78',C'CNO'                                            
         DC    AL1(3+2),C'79',C'YN'                                             
         DC    AL1(3+2),C'7A',C'AB'                                             
         DC    AL1(3+2),C'7B',C'AE'                                             
         DC    AL1(3+1),C'82',C'Y'                                              
         DC    AL1(3+1),C'85',C'Y'                                              
         DC    AL1(3+1),C'87',C'S'                                              
         DC    AL1(3+1),C'8A',C'W'                                              
         DC    AL1(3+2),C'8B',C'NS'                                             
         DC    AL1(3+2),C'90',C'AS'                                             
         DC    AL1(3+3),C'91',C'AIB'                                            
         DC    AL1(3+5),C'CO',C'FNPY '                                          
         DC    AL1(3+2),C'FR',C'Y '                                             
         DC    AL1(3+3),C'G0',C'FT '                                            
         DC    AL1(3+3),C'G1',C'FT '                                            
         DC    AL1(3+3),C'G2',C'FT '                                            
         DC    AL1(3+3),C'G3',C'FT '                                            
         DC    AL1(3+3),C'G4',C'FT '                                            
         DC    AL1(3+3),C'G5',C'FT '                                            
         DC    AL1(3+3),C'G8',C'FT '                                            
         DC    AL1(3+3),C'G9',C'FT '                                            
         DC    AL1(3+2),C'NM',C'AC'                                             
         DC    AL1(3+3),C'PR',C'PIB'                                            
***      DC    AL1(3+2),C'PS',C'AB'                                             
         DC    AL1(3+5),C'PX',C'FNPY '                                          
         DC    AL1(3+2),C'TK',C'OA'                                             
         DC    AL1(3+3),C'X3',C'PIB'                                            
         DC    X'00'                                                            
         SPACE                                                                  
OPT2VTBL DS    0C                                                               
         DC    AL1(3+2),C'16',C'CD'  CLEAR/DON'T CLEAR INVOICE DOLLARS          
         DC    AL1(3+3),C'18',C'ATO' CONTRACT SWITCH                            
         DC    AL1(3+2),C'19',C'YN'  INVOICE CONTROL LIST                       
         DC    AL1(3+3),C'1A',C'WPF'                                            
         DC    AL1(3+4),C'1B',C'ABCT'                                           
         DC    AL1(3+3),C'1J',C'ABS'                                            
         DC    AL1(3+2),C'1K',C'FC'         STRATEGY SEEDER                     
         DC    AL1(3+4),C'1L',C'ABCT'                                           
         DC    AL1(3+1),C'20',C'Z'   SHOW CON'S ORD OR INV = 0 NOT BOTH         
         DC    AL1(3+1),C'21',C'Z'   SHOW CON'S ORD OR INV = 0 NOT BOTH         
         DC    AL1(3+1),C'25',C'Y'                                              
         DC    AL1(3+2),C'2K',C'N '                                             
         DC    AL1(3+5),C'30',C'SDBAN'       YADR REPORT                        
         DC    AL1(3+2),C'40',C'B '                                             
         DC    AL1(3+2),C'41',C'B '                                             
         DC    AL1(3+2),C'47',C'YN'          47   REPORT                        
         DC    AL1(3+9),C'70',C'BMSDCAN#E'                                      
         DC    AL1(3+3),C'71',C'ONB'         OWNER REPORT                       
         DC    AL1(3+4),C'72',C'NLB '        NAT/LOC/BOTH                       
         DC    AL1(3+2),C'73',C'AD'                                             
         DC    AL1(3+2),C'76',C'YN'                                             
         DC    AL1(3+2),C'7B',C'AI'          ACTIVE/INACTIVE                    
         DC    AL1(3+3),C'80',C'RSO'                                            
         DC    AL1(3+1),C'81',C'S'                                              
         DC    AL1(3+1),C'87',C'S'                                              
         DC    AL1(3+3),C'90',C'AIB'                                            
         DC    AL1(3+2),C'PS',C'AB'                                             
****     DC    AL1(3+7),C'PX',C'ABCDEFG'                                        
         DC    AL1(3+3),C'SZ',C'SOT'                                            
         DC    AL1(3+2),C'TK',C'AT'                                             
         DC    AL1(3+3),C'WB',C'SOT'                                            
         DC    X'00'                                                            
         SPACE                                                                  
OPT3VTBL DS    0C                                                               
         DC    AL1(3+3),C'16',C'YN '                                            
         DC    AL1(3+4),C'19',C'OIB '        INVOICE CONTROL LIST               
         DC    AL1(3+2),C'1C',C'YN'                                             
         DC    AL1(3+2),C'1J',C'YN'          LASER PRINTER OPTION               
         DC    AL1(3+3),C'20',C'SIB'         20: STN, INTFC CD, BOTH            
         DC    AL1(3+4),C'30',C'SDBN'        YADR REPORT                        
         DC    AL1(3+2),C'41',C'C '                                             
         DC    AL1(3+2),C'45',C'SO'          45: STATION OR OFFICE              
         DC    AL1(3+4),C'50',C'YPQZ'                                           
         DC    AL1(3+4),C'51',C'YPQZ'                                           
         DC    AL1(3+4),C'58',C'YPQZ'                                           
         DC    AL1(3+4),C'59',C'YPQZ'                                           
         DC    AL1(3+2),C'5A',C'YP'                                             
         DC    AL1(3+4),C'5B',C'YPQZ'                                           
         DC    AL1(3+5),C'5F',C'STOPC'                                          
         DC    AL1(3+4),C'60',C'YPQZ'                                           
         DC    AL1(3+4),C'61',C'YPQZ'                                           
         DC    AL1(3+2),C'6B',C' B'                                             
         DC    AL1(3+4),C'70',C'CLX '        CREAT DTE/LAST UP DTE              
         DC    AL1(3+4),C'76',C'NLB '        NAT/LOC/BOTH                       
         DC    AL1(3+5),C'87',C'AMOSP'                                          
         DC    AL1(3+2),C'8E',C'NY'                                             
         DC    AL1(3+2),C'8N',C'NY'               FOR THE 8N REPORT             
         DC    AL1(3+2),C'AO',C'NY'               FOR THE AO REPORT             
         DC    AL1(3+2),C'AS',C'NY'               FOR THE AS REPORT             
         DC    AL1(3+2),C'A0',C'P '               FOR THE A0 REPORT             
         DC    AL1(3+3),C'B0',C'NYB'              FOR THE B0 REPORT             
         DC    AL1(3+3),C'B1',C'NYB'              FOR THE B1 REPORT             
         DC    AL1(3+2),C'C0',C'CP'               FOR CLASS RANKER              
         DC    AL1(3+4),C'C1',C'CPDQ'             FOR CLASS RANKER              
         DC    AL1(3+1),C'C1',C'X'                FOR CLASS RANKER              
         DC    AL1(3+2),C'WB',C'FT'        ROBINSON REPORT                      
         DC    AL1(3+2),C'SZ',C'FT'        ROBINSON REPORT                      
         DC    AL1(3+2),C'D0',C'YP'                                             
         DC    AL1(3+2),C'D1',C'YP'                                             
         DC    AL1(3+2),C'D2',C'YP'                                             
         DC    AL1(3+2),C'D3',C'YP'                                             
         DC    AL1(3+2),C'F0',C'CP'                                             
         DC    AL1(3+10),C'G0',C'CPFNADQGOB'                                    
         DC    AL1(3+10),C'G1',C'CPFNADQGOB'                                    
         DC    AL1(3+2),C'G2',C'CP'                                             
         DC    AL1(3+2),C'G3',C'CP'                                             
         DC    AL1(3+2),C'G4',C'CP'                                             
         DC    AL1(3+2),C'G5',C'CP'                                             
         DC    AL1(3+10),C'G8',C'CPFNADQGOB'                                    
         DC    AL1(3+10),C'G9',C'CPFNADQGOB'                                    
         DC    AL1(3+2),C'K1',C'VR'                                             
         DC    AL1(3+2),C'K2',C'VR' +                                           
         DC    AL1(3+2),C'K3',C'VR' +                                           
         DC    AL1(3+2),C'K4',C'VR' +                                           
         DC    AL1(3+2),C'K5',C'VR' +                                           
         DC    AL1(3+2),C'K6',C'VR' +                                           
         DC    AL1(3+2),C'K7',C'VR'                                             
         DC    AL1(3+3),C'K8',C'YNJ'                                            
         DC    AL1(3+2),C'KC',C'VR' +                                           
         DC    AL1(3+2),C'KD',C'VR' +                                           
         DC    AL1(3+2),C'OR',C'YP'                                             
         DC    AL1(3+2),C'PR',C'MS'                                             
***      DC    AL1(3+2),C'PS',C'AB'                                             
***      DC    AL1(3+2),C'PX',C'MS'                                             
         DC    AL1(3+2),C'SR',C'BA'                                             
         DC    AL1(3+2),C'SS',C'BA'                                             
         DC    AL1(3+2),C'SO',C'BA'                                             
         DC    AL1(3+3),C'TK',C'YZ '                                            
         DC    AL1(3+2),C'X3',C'MS'                                             
         DC    X'00'                                                            
         SPACE                                                                  
OPT4VTBL DS    0C                                                               
         DC    AL1(3+5),C'87',C'AMOSP'                                          
         DC    X'00'                                                            
         SPACE                                                                  
OPT5VTBL DS    0C                                                               
         DC    AL1(3+5),C'87',C'AMOSP'                                          
         DC    X'00'                                                            
         SPACE                                                                  
OPT6VTBL DS    0C                                                               
         DC    AL1(3+3),C'12',C'MQA'                                            
         DC    AL1(3+3),C'13',C'MQA'                                            
         DC    AL1(3+2),C'1J',C'YN'  DETAIL OPTION                              
         DC    AL1(3+3),C'22',C'MQA'                                            
         DC    AL1(3+3),C'23',C'MQA'                                            
         DC    AL1(3+3),C'24',C'MQA'                                            
         DC    AL1(3+3),C'26',C'MQA'                                            
         DC    AL1(3+3),C'27',C'MQA'                                            
         DC    AL1(3+3),C'28',C'MQA'                                            
         DC    AL1(3+3),C'29',C'MQA'                                            
         DC    AL1(3+3),C'2G',C'MQA'                                            
         DC    AL1(3+3),C'2I',C'MQA'                                            
         DC    AL1(3+3),C'2K',C'MQA'                                            
         DC    AL1(3+3),C'2X',C'MQA'                                            
         DC    AL1(3+2),C'30',C'YN'              YADR REPORT                    
         DC    AL1(3+3),C'3M',C'MQA'                                            
         DC    AL1(3+3),C'3V',C'MQA'                                            
         DC    AL1(3+3),C'43',C'MQA'                                            
         DC    AL1(3+3),C'44',C'MQA'                                            
         DC    AL1(3+3),C'45',C'MQA'                                            
         DC    AL1(3+3),C'46',C'MQA'                                            
         DC    AL1(3+3),C'48',C'MQA'                                            
         DC    AL1(3+3),C'4B',C'MQA'                                            
         DC    AL1(3+3),C'50',C'MQA'                                            
         DC    AL1(3+3),C'51',C'MQA'                                            
         DC    AL1(3+3),C'52',C'MQA'                                            
         DC    AL1(3+3),C'53',C'MQA'                                            
         DC    AL1(3+3),C'54',C'MQA'                                            
         DC    AL1(3+3),C'55',C'MQA'                                            
         DC    AL1(3+3),C'56',C'MQA'                                            
         DC    AL1(3+3),C'57',C'MQA'                                            
         DC    AL1(3+3),C'58',C'MQA'                                            
         DC    AL1(3+3),C'59',C'MQA'                                            
         DC    AL1(3+3),C'5A',C'MQA'                                            
         DC    AL1(3+3),C'5B',C'MQA'                                            
         DC    AL1(3+3),C'6B',C'MQA'                                            
         DC    AL1(3+3),C'6E',C'MQA'                                            
         DC    AL1(3+3),C'60',C'MQA'                                            
         DC    AL1(3+3),C'61',C'MQA'                                            
         DC    AL1(3+3),C'62',C'MQA'                                            
         DC    AL1(3+3),C'63',C'MQA'                                            
         DC    AL1(3+3),C'65',C'MQA'                                            
         DC    AL1(3+3),C'66',C'MQA'                                            
         DC    AL1(3+3),C'68',C'MQA'                                            
         DC    AL1(3+3),C'69',C'MQA'                                            
         DC    AL1(3+3),C'6C',C'MQA'                                            
         DC    AL1(3+3),C'6D',C'MQA'                                            
         DC    AL1(3+1),C'87',C'P'                                              
         DC    AL1(3+3),C'8E',C'MQA'                                            
         DC    AL1(3+3),C'8N',C'MQA'                                            
         DC    AL1(3+3),C'AC',C'MQA'                                            
         DC    AL1(3+3),C'B0',C'MQA'                                            
         DC    AL1(3+3),C'B1',C'MQA'                                            
         DC    AL1(3+3),C'BL',C'MQA'                                            
         DC    AL1(3+3),C'D0',C'MQA'                                            
         DC    AL1(3+3),C'D1',C'MQA'                                            
         DC    AL1(3+3),C'D2',C'MQA'                                            
         DC    AL1(3+3),C'D3',C'MQA'                                            
         DC    AL1(3+3),C'F0',C'MQA'                                            
         DC    AL1(3+3),C'G0',C'MQA'                                            
         DC    AL1(3+3),C'G1',C'MQA'                                            
         DC    AL1(3+3),C'G2',C'MQA'                                            
         DC    AL1(3+3),C'G3',C'MQA'                                            
         DC    AL1(3+3),C'G4',C'MQA'                                            
         DC    AL1(3+3),C'G5',C'MQA'                                            
         DC    AL1(3+3),C'G8',C'MQA'                                            
         DC    AL1(3+3),C'G9',C'MQA'                                            
         DC    AL1(3+3),C'K0',C'MQA'                                            
         DC    AL1(3+3),C'K1',C'MQA'                                            
         DC    AL1(3+3),C'K2',C'MQA'                                            
         DC    AL1(3+3),C'K3',C'MQA'                                            
         DC    AL1(3+3),C'K4',C'MQA'                                            
         DC    AL1(3+3),C'K5',C'MQA'                                            
         DC    AL1(3+3),C'K6',C'MQA'                                            
         DC    AL1(3+3),C'K7',C'MQA'                                            
         DC    AL1(3+3),C'K8',C'MQA'                                            
         DC    AL1(3+3),C'K9',C'MQA'                                            
         DC    AL1(3+3),C'KA',C'MQA'                                            
         DC    AL1(3+3),C'KB',C'MQA'                                            
         DC    AL1(3+3),C'KC',C'MQA'                                            
         DC    AL1(3+3),C'KD',C'MQA'                                            
         DC    AL1(3+3),C'KP',C'MQA'                                            
         DC    AL1(3+3),C'KQ',C'MQA'                                            
         DC    AL1(3+3),C'MP',C'MQA'                                            
         DC    AL1(3+3),C'NM',C'MQA'                                            
         DC    AL1(3+3),C'N1',C'MQA'                                            
         DC    AL1(3+3),C'OR',C'MQA'                                            
         DC    AL1(3+3),C'PP',C'MQA'                                            
         DC    AL1(3+3),C'P0',C'MQA'                                            
         DC    AL1(3+3),C'P2',C'MQA'                                            
         DC    AL1(3+3),C'P3',C'MQA'                                            
         DC    AL1(3+3),C'P4',C'MQA'                                            
         DC    AL1(3+3),C'P5',C'MQA'                                            
         DC    AL1(3+3),C'VR',C'MQA'                                            
         DC    AL1(3+3),C'XK',C'MQA'                                            
         DC    AL1(3+3),C'XL',C'MQA'                                            
         DC    AL1(3+3),C'XM',C'MQA'                                            
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
* RETRIEVE REP RECORD                                                           
*                                                                               
NCONVL2  NMOD1 0,**CVL2**                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            RESET A(SCREEN FIELD)                        
*                                                                               
         MVC   RNCNUM(8),8(R2)     ALWAYS LOAD NET CON #                        
         B     NCONGOOD            ACCEPT WHATEVER IS ENTERED                   
*                                                                               
         MVI   KEY,X'89'           SET NET CON # ID                             
         MVC   KEY+10(2),TWAAGY    SET REP CODE                                 
         MVC   KEY+12(8),8(R2)     SET NET CON #                                
         GOTO1 DIRHIGH             FIND NET CON # ALT KEY                       
         CLC   KEYSAVE(20),KEY     KEY FOUND?                                   
         BNE   NCONBAD             NOT ON FILE - ERROR                          
         OC    RADV(7),SPACESCV                                                 
         CLC   RADV(4),SPACESCV    ANY ADVERTISER ENTERED?                      
         BNE   NCVL0020            YES                                          
         MVC   RADV(4),KEY+20      NO  - INSERT ADV INTO REQUEST                
         B     NCVL0040                                                         
NCVL0020 EQU   *                                                                
         CLC   RADV(4),KEY+20      ADV ENTERED = NC# KEY ADV?                   
         BE    NCVL0040            YES                                          
         B     NCVL0080            NO  - ERROR CONDITION                        
NCVL0040 EQU   *                                                                
         CLC   RPRO(3),SPACESCV    ANY PRODUCT ENTERED?                         
         BNE   NCVL0060            YES                                          
         MVC   RPRO(3),KEY+24      NO  - INSERT PROD INTO REQUEST               
         B     NCONGOOD                                                         
NCVL0060 EQU   *                                                                
         CLC   RPRO(3),KEY+24      PROD ENTERED = NC# KEY PROD?                 
         BE    NCONGOOD            YES                                          
NCVL0080 EQU   *                                                                
         GOTO1 PUTCURS,P1,VNETCON,BADADVPR,L'BADADVPR                           
         B     NCONBAD                                                          
NCONGOOD EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         B     NCONXIT                                                          
NCONBAD  EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
NCONXIT  EQU   *                                                                
         XIT1                                                                   
BADADVPR DC    C'NETWORK CONTRACT # WRONG FOR ADV/PROD ENTERED'                 
SPACESCV DC    CL80' '                                                          
         LTORG                                                                  
                                                                                
         EJECT                                                                  
*                                                                               
* LOOK AT REP RECORD TO SEE IF REP IS A SUBSIDIARY                              
* IF REP IS SUBSIDIARY, ROUTINE RETURNS COMMBAD WITH REP RECORD                 
*   IN IOWORK (SO CALLER HAS ACCESS TO RREPMAST)                                
GETREP   NMOD1 0,**GREP**                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'           RETRIEVE REP RECORD                          
         MVC   KEY+25(2),TWAAGY    INSERT REP CODE                              
         GOTO1 DIRREAD                                                          
         GOTO1 FILREAD                                                          
*                                                                               
         L     RE,AIOADDR2                                                      
         USING RREPREC,RE                                                       
*                                                                               
         CLC   RREPKREP,=C'NU'     CLEAR CHANNEL?                               
         BNE   GETR0010            NO                                           
         MVC   RREPMAST,=C'K3'     YES - INSERT 'MASTER'                        
         B     GETBAD              TREAT AS SUBSIDIARY                          
GETR0010 EQU   *                                                                
         CLC   =X'0000',RREPMAST                                                
         BE    GETGOOD                                                          
         CLC   =X'4040',RREPMAST                                                
         BE    GETGOOD                                                          
         CLC   =X'FFFF',RREPMAST                                                
         BNE   GETBAD                                                           
         LA    RF,RREPELEM         FIND X'02' ELEMENT                           
GETR0020 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BNE   *+6                 NO                                           
         DC    H'0'                YES - SHOULDN'T HAPPEN                       
         CLI   0(RF),2             SUBSIDIARY REP LIST                          
         BE    GETR0040                                                         
         ZIC   R1,1(RF)            BUMP TO NEXT ELEMENT                         
         AR    RF,R1                                                            
         B     GETR0020            GO BACK FOR NEXT                             
GETR0040 EQU   *                                                                
         ZIC   R1,2(RF)            GET SUB COUNT                                
         SLA   R1,1                DOUBLE THE COUNT                             
         BCTR  R1,0                SUBTRACT 1 FOR EX                            
         EX    R1,GETR0100         MOVE SUBS BY LENGTH                          
         B     GETGOOD                                                          
GETR0100 MVC   SUBREPS(0),10(RF)                                                
*                                                                               
GETGOOD EQU    *                                                                
         SR    R0,R0                                                            
         B     GETTEST                                                          
*                                                                               
GETBAD   EQU   *                                                                
         LA    R0,1                                                             
*                                                                               
GETTEST EQU   *                                                                 
         LTR   R0,R0                                                            
         XIT1                                                                   
*                                                                               
         DROP  RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
RCONVAL  NMOD1 0,**CNVA**                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            RESET SCREEN POINTER                         
CNVA0020 SR    R7,R7                                                            
         IC    R7,5(R2)                                                         
         BCTR  R7,0                                                             
         EX    R7,CNVAPACK                                                      
         OI    DUB+7,X'0F'                                                      
         UNPK  RCON,DUB                                                         
         PACK  DUB,RCON                                                         
         OI    DUB+7,X'0F'                                                      
         ZAP   WORK(8),=P'99999999'                                             
         SP    WORK(8),DUB                                                      
         UNPK  DUB,WORK(8)                                                      
         OI    DUB+7,X'F0'                                                      
         PACK  WORK(5),DUB(9)                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'8C'                                                        
         MVC   KEY+23(4),WORK                                                   
         MVC   KEY+21(2),TWAAGY                                                 
         GOTO1 DIRHIGH                                                          
         BNZ   CNVA0800            DMGR ERROR                                   
         CLC   KEY(27),KEYSAVE                                                  
         BE    CNVA0040                                                         
*                                                                               
         MVI   ERRCNTL,ECMSGNUM    RECORD NOT FOUND MSG.                        
         MVI   ERRDATA,NOTFOUND                                                 
         B     CNVA0800     NOT FOUND                                           
*                                                                               
CNVA0040 EQU   *                                                                
         GOTO1 FILREAD             READ IN CONTRACT                             
*                                                                               
*                                                                               
         L     RE,AIOADDR2                                                      
         USING RCONREC,RE                                                       
*                                                                               
         MVC   RAGY,RCONKAGY       MOVE FIELDS FROM CONTRACT TO REQUEST         
         MVC   RAGYO,RCONKAOF                                                   
         MVC   RADV,RCONKADV                                                    
         MVC   RSTA,RCONKSTA                                                    
         MVC   RSDIV(2),RCONTEM                                                 
         MVC   RSMAN,RCONSAL                                                    
         SR    R0,R0               SET CC ZERO                                  
         B     CNVA0900                                                         
*                                                                               
         DROP  RE                                                               
*                                                                               
CNVA0800 EQU   *                                                                
         LTR   RB,RB              SET CC NOT ZERO                               
CNVA0900 EQU   *                                                                
         XIT1                                                                   
*                                                                               
CNVAPACK PACK  DUB,8(0,R2)        EXECUTED                                      
         EJECT                                                                  
*                                                                               
*   NVALR24:  VALR24 ROUTINE, RELOCATED FOR SPACE REASONS                       
*                                                                               
NVALR24  NMOD1 0,*NV24*            GET STATION RECORD FOR GROUP/SUB             
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'           RECORD ID                                    
         MVC   KEY+20(2),TWAAGY                                                 
         MVC   KEY+22(5),RSTA      INSERT STATION                               
         GOTO1 DIRREAD             READ KEY                                     
         GOTO1 FILREAD             READ RECORD                                  
*        *                                                                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'39'           INSERT RECORD TYPE                           
         MVI   KEY+10,1            INSERT 'SIT ANALYSIS' REC TYPE               
         MVC   KEY+11(2),TWAAGY    INSERT REP CODE                              
*                                                                               
         L     RE,AIOADDR2                                                      
         USING RSTAREC,RE                                                       
*                                                                               
         MVC   KEY+13(2),RSTAGRUP  INSERT GROUP CODE                            
         MVC   KEY+15(5),RSTAKSTA  INSERT STATION CALLS                         
         DROP  RE                                                               
         MVC   WORK+4(2),=C'01'    INSERT PADDER                                
         MVC   WORK(4),RSTRD       CONVERT START DATE                           
         GOTO1 DATCON,DMCB,(0,WORK),(19,WORK+6)                                 
*                                  CONVERT DATE TO JULIAN                       
         ZAP   WORK+0(4),=P'0'                                                  
         MVO   WORK+0(4),WORK+6(3)                                              
*                                  CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+14(4),=P'999999'                                            
         SP    WORK+14(4),WORK+0(4)                                             
*                                  GET 9'S COMPLEMENT                           
         MVO   WORK+10(4),WORK+14(4)                                            
*                                  CHANGE TO PWOS                               
         MVC   KEY+23(3),WORK+10   INSERT START DATE                            
*                                                                               
         MVC   WORK+4(2),=C'01'    INSERT PADDER                                
         MVC   WORK(4),RENDD       CONVERT END   DATE                           
         GOTO1 DATCON,DMCB,(0,WORK),(19,WORK+6)                                 
*                                  CONVERT DATE TO JULIAN                       
         ZAP   WORK+0(4),=P'0'                                                  
         MVO   WORK+0(4),WORK+6(3)                                              
*                                  CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+14(4),=P'999999'                                            
         SP    WORK+14(4),WORK+0(4)                                             
*                                  GET 9'S COMPLEMENT                           
         MVO   WORK+10(4),WORK+14(4)                                            
*                                  CHANGE TO PWOS                               
         MVC   KEY+20(3),WORK+10   INSERT END DATE                              
         GOTO1 DIRREAD             READ THE KEY                                 
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    NVAL0060                                                         
         GOTO1 PUTCURS,P1,VSTATION,NOSITANL,L'NOSITANL                          
         LTR   RB,RB               SET CC NOT ZERO:  ERROR                      
         B     NVAL0080                                                         
NVAL0060 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
NVAL0080 EQU   *                                                                
         XIT1                                                                   
*                                                                               
NOSITANL DC    C'STATION HAS NO SITUATION ANALYSIS FOR PERIOD'                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   NVALR27:  VALR27 ROUTINE, RELOCATED FOR SPACE REASONS                       
*        DEVELOPS TAKEOVER DATE FROM STATION RECORD.                            
*                                                                               
NVALR27  NMOD1 0,*NV27*            GET STATION RECORD                           
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'           RECORD ID                                    
         MVC   KEY+20(2),TWAAGY                                                 
         MVC   KEY+22(5),RSTA      INSERT STATION                               
         GOTO1 DIRREAD             READ KEY                                     
         GOTO1 FILREAD             READ RECORD                                  
*                                                                               
         L     R4,AIOADDR2                                                      
         USING RSTAREC,R4                                                       
*                                                                               
         PRINT GEN                                                              
         GOTO1 DATCON,DMCB,(3,RSTASTRT),(X'20',WORK)                            
*                                  GET EBCDIC DATE OF JOIN                      
         PRINT NOGEN                                                            
         DROP  R4                                                               
         GOTO1 =V(GETDAY),DMCB,WORK,DUB,RR=SUBRELO                              
         CLI   DMCB,X'1'           IS FIRST DAY MONDAY?                         
         BE    NV270020            YES                                          
         ZIC   RE,DMCB             GET DAY OF WEEK NUMBER                       
         BCTR  RE,0                SUBTRACT 1 DAY                               
         LNR   RE,RE               MAKE IT NEGATIVE                             
         ST    RE,DMCB+8           SET ADDAY ADJUST VALUE                       
         GOTO1 =V(ADDAY),DMCB,WORK,WORK,,RR=SUBRELO                             
NV270020 EQU   *                                                                
         MVC   RSTRD,WORK          INSERT TKO DATE INTO CARD                    
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
*                                                                               
NOMESSGE DC    C'IN CASE A MESSAGE IS NEEDED'                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   NVALR28:  VALR28 ROUTINE, RELOCATED FOR SPACE REASONS                       
*                                                                               
NVALR28  NMOD1 0,*NV28*            DOWNLOAD REQUIRES OPT1 = C                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         CLC   RQSOUT(4),=C'DOWN'  DOWNLOAD REQUEST?                            
         BNE   VALR28A             NO  -                                        
         CLI   ROPTN,C'C'          YES - OPTION1 = C?                           
         BE    VALR28A             YES                                          
         GOTO1 PUTCURS,P1,VOPTION1,DOWNNG,L'DOWNNG                              
         LTR   RB,RB               NO  - SET CC NOT ZERO                        
         B     VALR28B                                                          
VALR28A  EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
VALR28B  EQU   *                                                                
         XIT1                                                                   
*                                                                               
DOWNNG   DC    C'OPTION1 MUST BE "C" FOR DOWNLOAD'                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   NVALR29:  VALR29 ROUTINE, RELOCATED FOR SPACE REASONS                       
*                                                                               
NVALR29  NMOD1 0,*NV29*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         CLI   ROPTN3,C' '         OPTION3 BLANK?                               
         BE    VALR29D             YES - NORMAL ACTIVITY DATE                   
         CLI   ROPTN3,C'X'         CLOSED THRU REPORT REQUEST?                  
         BE    VALR29A             YES - NORMAL ACTIVITY DATE                   
         CLC   RSTRD,SPACDTE       IS DTE FLD EMPTY?                            
         BH    VALR29D             IF OPTION3 FULL, DTE MUST BE FULL            
         GOTO1 PUTCURS,P1,VSTRTEND,FILDTE,L'FILDTE                              
         LTR   RB,RB               NO  - SET CC NOT ZERO                        
         B     VALR29Z                                                          
VALR29A  EQU   *                                                                
         CLC   R2CLOSMN,SPACDTE    IS CLOSED THRU DATE FLD EMPTY?               
         BH    VALR29D             IF OPTION3 = X, DTE MUST BE FULL             
         GOTO1 PUTCURS,P1,VCLOSMON,CLOSDTE,L'CLOSDTE                            
         LTR   RB,RB               NO  - SET CC NOT ZERO                        
         B     VALR29Z                                                          
VALR29D  EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
VALR29Z  EQU   *                                                                
         XIT1                                                                   
*                                                                               
SPACDTE  DC    CL6' '              SPACES FOR DATE COMPARISON                   
FILDTE   DC    C'MUST FILL IN STR/END DATE IF OPTION3 IS USED'                  
CLOSDTE  DC    C'MUST FILL IN CLOSED THRU DATE IF OPTION3 = X'                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   VROLDREP: VALIDATES REP INPUT AGAINST CONTROL FILE                          
*                                                                               
         DS    0F                                                               
VROLDREP NMOD1 0,*OREP*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            RESET A(OLD REP FIELD)                       
*                                                                               
         L     RF,APARM                                                         
         L     RF,16(RF)           A(COMFACS FROM PARAM LIST)                   
         USING COMFACSD,RF                                                      
         MVC   GETFACT,CGETFACT                                                 
         MVC   VSWITCH,CSWITCH                                                  
         DROP  RF                                                               
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         MVC   ORIGUTL,FASYS       SAVE ORIGINAL SYSTEM NUMBER                  
         DROP  RF                                                               
*                                                                               
         GOTO1 ACCSREP             RETRIEVE SOURCE REP ID CODE                  
*                                                                               
         XC    WORKREP,WORKREP                                                  
         MVI   WORKREP,C'I'        FIND CONTROL FILE ID RECORD                  
         LA    RF,0(R2)            A(SOURCE REP)                                
         ZIC   RE,5(RF)            L(SOURCE REP INPUT)                          
         BCTR  RE,0                -1 FOR EX                                    
         EX    RE,VOLD0800         MOVE BY LENGTH                               
         OC    WORKREP+15(10),SPACESOR                                          
         MVC   RNCNUM(8),WORKREP+15                                             
*                                  ALWAYS LOAD TO NETWORK CON# FIELD            
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORKREP,AIOWORK               
         CLI   8(R1),0             FOUND?                                       
         BNE   VOLD0080            NOT FOUND - SHOW MESSAGE                     
         L     R1,AIOWORK                                                       
         CLC   WORKREP(25),0(R1)   CHECK THE KEY                                
         BNE   VOLD0080            NOT FOUND                                    
         LA    R1,28(R1)           FOUND - POINT TO FIRST ELEMENT               
         B     VOLD0120                                                         
VOLD0080 EQU   *                                                                
         GOTO1 PUTCURS,P1,VOLDREP,NGOLDREP,L'NGOLDREP                           
         B     VOLD0200                                                         
VOLD0120 EQU   *                                                                
         CLI   0(R1),X'06'         AGENCY ID ELEMENT?                           
         BNE   VOLD0140            NO                                           
         MVC   SRCEREP,2(R1)       YES - SAVE 2-CHAR REP ID                     
         B     VOLD0160            BUMP TO NEXT ELEMENT                         
VOLD0140 EQU   *                                                                
         CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   VOLD0160            NO                                           
         CLI   2(R1),X'08'         IS IT 'REP' SYSTEM?                          
         BE    VOLD0180            YES                                          
VOLD0160 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD                                
         BNE   VOLD0120            NO                                           
         B     VOLD0200            NO X'21' - RETURN ERROR                      
VOLD0180 EQU   *                                                                
         MVC   SRCEUTL,3(R1)       SAVE SOURCE UTL NUMBER                       
         SR    R0,R0               SET CC = ZERO                                
         B     VOLD0760                                                         
VOLD0200 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO:  ERROR                    
VOLD0760 EQU   *                                                                
         XIT1                                                                   
*                                                                               
VOLD0800 MVC   WORKREP+15(0),8(R2) LOAD SOURCE REP BY LENGTH                    
*                                                                               
NGOLDREP DC    C'SOURCE REP NOT FOUND ON FILE'                                  
SPACESOR DC    CL80' '                                                          
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ACCSREP:  RETRIEVE THE TARGET REP ID CODE                                     
*                                                                               
* OUTPUT - THREE-CHARACTER REP ID WILL BE PLACED IN 'TRGTREP'                   
***********************************************************************         
ACCSREP  NTR1                                                                   
         XC    WORKREP,WORKREP                                                  
         MVI   WORKREP,C'I'        FIND CONTROL USER ID RECORD                  
         MVC   WORKREP+23(2),TWAUSRID                                           
*                                  INSERT TARGET REP SYS ID #                   
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORKREP,AIOWORK               
         L     R1,AIOWORK                                                       
         CLC   WORKREP(25),0(R1)   CHECK THE KEY                                
         BE    *+6                 FOUND                                        
         DC    H'0'                REP NOT FOUND???                             
         LA    R1,28(R1)           FIND SYS AUTHORIZATION ELEMENT               
ACRP0020 EQU   *                                                                
         CLI   0(R1),X'02'         AGENCY ID ELEMENT?                           
         BNE   ACRP0040            NO                                           
         MVC   TRGTREP,2(R1)       YES - SAVE 3-CHAR REP ID                     
*                                     REP CODES IN STATION RECORD               
*                                     PERMIT ONLY THREE CHARACTERS              
         B     ACRP0060            FINISHED                                     
ACRP0040 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD                                
         BNE   ACRP0020            NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
ACRP0060 EQU   *                                                                
         XIT1                                                                   
**********************************************************************          
*  SWITCH TO SOURCE REP FILE                                         *          
**********************************************************************          
SWSRCREP NMOD1 0,*SREP*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         CLC   ORIGUTL,SRCEUTL     SOURCE/TARGET ON SAME FILE?                  
         BE    SWSR0020            YES - DON'T SWITCH                           
         GOTO1 VSWITCH,DMCB,(SRCEUTL,X'FFFFFFFF'),0                             
         CLI   4(R1),0             SWITCHED OKAY?                               
         BE    SWSR0020            YES - NOW READ CONTRACT RECORDS              
         CLI   4(R1),2             NO  - SYSTEM NOT OPENED?                     
         BE    *+6                 YES                                          
         DC    H'0'                NO  - OTHER REASON                           
*                                                                               
         B     SWTGTSET                                                         
*                                                                               
SWSR0020 DS    0H                                                               
         XIT1                      SWITCHED TO SOURCE REP                       
*                                                                               
SWTGTSET NTR1                      ERROR: SWITCH BACK TO TARGET REP             
*        GOTO1 VSWITCH,DMCB,(X'08',X'FFFFFFFF'),0                               
         GOTO1 VSWITCH,DMCB,=C'REP ',0                                          
         CLI   4(R1),0             SWITCHED BACK OKAY?                          
         BE    SRCCLOSD            YES - EXIT WITH MESSAGE                      
         DC    H'0'                NO  - ABORT                                  
SRCCLOSD EQU   *                                                                
         GOTO1 PUTCURS,P1,VOLDREP,SRCCLOSE,L'SRCCLOSE                           
         LTR   RB,RB               SET CC NOT ZERO                              
         XIT1                                                                   
*                                                                               
SRCCLOSE DC    C'SOURCE FILE IS CLOSED !'                                       
*                                                                               
*                                                                               
SWTGTREP NMOD1 0,*TREP*            SWITCH BACK TO TARGET REP                    
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         CLC   ORIGUTL,SRCEUTL     SOURCE/TARGET ON SAME FILE?                  
         BE    SWTG0020            YES - DON'T SWITCH                           
*        GOTO1 VSWITCH,DMCB,(X'08',X'FFFFFFFF'),0                               
         GOTO1 VSWITCH,DMCB,=C'REP ',0                                          
         CLI   4(R1),0             SWITCHED BACK OKAY?                          
         BE    SWTG0020            YES - EXIT ROUTINE                           
         DC    H'0'                NO  - ABORT                                  
SWTG0020 EQU   *                                                                
         XIT1                                                                   
***>>>                                                                          
*                                                                               
*   VRTKOSTA: VALIDATES STATION FOR TAKEOVER AUTHORIZATION                      
*                                                                               
         DS    0F                                                               
VRTKOSTA NMOD1 0,*TKOS*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            RESET A(SOURCE STATION FIELD)                
         GOTO1 =A(SWSRCREP),DMCB,(RC),RR=YES                                    
*                                  SWITCH TO SOURCE REP                         
         BAS   RE,STAVAL2          CHECK FOR STATION ON SOURCE FILE             
         BNZ   VRST0700            NOT ON FILE:  ERROR                          
         GOTO1 =A(SWTGTREP),DMCB,(RC),RR=YES                                    
*                                  SWITCH BACK TO TARGET REP                    
         GOTO1 CHEKTSTA            CHECK TARGET STATION RECORD                  
         BZ    VRST0040            TARGET STATION CLEARED FOR TRANSFER          
         GOTO1 PUTCURS,P1,VTKOSTAC,TSTANOGD,L'TSTANOGD                          
         B     VRST0800            STATION NOT CLEARED FOR TRANSFER             
VRST0040 EQU   *                                                                
         GOTO1 =A(SWSRCREP),DMCB,(RC),RR=YES                                    
*                                  SWITCH TO SOURCE REP                         
         GOTO1 CHEKSSTA            CHECK SOURCE STATION RECORD                  
         BZ    VRST0060            SOURCE STATION CLEARED FOR TRANSFER          
         GOTO1 PUTCURS,P1,VTKOSTAC,SSTANOGD,L'SSTANOGD                          
         GOTO1 =A(SWTGTREP),DMCB,(RC),RR=YES                                    
*                                  SWITCH BACK TO TARGET REP                    
         B     VRST0800            STATION NOT CLEARED FOR TRANSFER             
VRST0060 EQU   *                                                                
         GOTO1 =A(SWTGTREP),DMCB,(RC),RR=YES                                    
*                                  SWITCH BACK TO TARGET REP                    
         B     VRST0850            EXIT CC ZERO                                 
VRST0700 EQU   *                                                                
         GOTO1 PUTCURS,P1,VTKOSTAC,TKONOSTA,L'TKONOSTA                          
         B     VRST0800            EXIT WITH ERROR                              
VRST0800 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
         B     VRST0900                                                         
VRST0850 EQU   *                                                                
         SR    R0,R0                                                            
VRST0900 EQU   *                                                                
         XIT1                                                                   
*                                                                               
TKONOSTA DC    C'SOURCE STATION NOT FOUND ON FILE'                              
TSTANOGD DC    C'TARGET STATION NOT CLEARED FOR TAKEOVER'                       
SSTANOGD DC    C'SOURCE STATION NOT CLEARED FOR TAKEOVER'                       
         XIT1                                                                   
         EJECT                                                                  
*- VALIDATE STATION                                                             
STAVAL2  NTR1                                                                   
*                                                                               
         MVI   KEY,X'02'           RECORD ID                                    
         MVC   KEY+20(2),SRCEREP   INSERT SOURCE REP CODE                       
         OC    KEY+22(5),STSPACES                                               
*                                                                               
         ZIC   R0,5(R2)            PARSE OUT POSSIBLE '-'                       
         LA    RF,8(R2)                                                         
         LA    RE,KEY+22                                                        
STVA0020 CLI   0(RF),C'-'                                                       
         BNE   STVA0040                                                         
*                                                                               
         LA    RF,1(RF)            SKIP THE '-'                                 
         LA    RE,KEY+26           BAND MUST GO HERE                            
         BCTR  R0,0                LESS 1 ON IPT LEN (THE '-')                  
         LTR   R0,R0                                                            
         BZ    STVA0060            NOTHING AFTER THE '-' (WABC- )               
         LA    R0,1                OUT OF LOOP AFTER NEXT MOVE.                 
*                                                                               
STVA0040 MVC   0(1,RE),0(RF)       MOVE DATA TO KEY                             
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,STVA0020                                                      
*                                                                               
*- IF BAND = 'T' (TELEVISION) THEN BLANK OUT BAND IN KEY.                       
STVA0060 EQU   *                                                                
         CLI   KEY+26,C'T'                                                      
         BNE   STVA0080                                                         
         MVI   KEY+26,C' '         TV IS BLANK ON FILE                          
*                                                                               
STVA0080 EQU   *                   FINI                                         
*                                                                               
         GOTO1 DIRHIGH             READ STATION KEY                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   STVA0100            NO                                           
         MVC   RSTA(5),KEY+22      YES - INSERT INTO REQ CARD                   
         MVC   SRCESTAT(5),KEY+22  SAVE SOURCE STATION                          
         B     STVA0120                                                         
*                                                                               
STVA0100 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
         B     STVA0140                                                         
STVA0120 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
STVA0140 EQU   *                                                                
         XIT1                                                                   
         SPACE 2                                                                
STSPACES DC    CL20' '                                                          
         LTORG                                                                  
         EJECT                                                                  
****>>>                                                                         
*                                                                               
*   CHEKTSTA: ACCESS THE REP STATION RECORD TO DETERMINE IF STATION             
*        IS CLEARED FOR TAKEOVER PROCESSING.  THIS CODE CHECKS THE              
*        SIGNON REP'S STATION RECORD, WHERE                                     
*              1.  THE STATION MUST EXIST                                       
*              2.  THE JOIN DATE MUST BE EQUAL OR BEFORE THE                    
*                  EFFECTIVE DATE                                               
*                                                                               
*                                                                               
*                                                                               
CHEKTSTA NTR1                                                                   
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,2               INSERT RECORD TYPE                           
         MVC   KEY+20(2),TWAAGY    INSERT REP CODE                              
         MVC   KEY+22(5),SRCESTAT  INSERT STATION CALL LETTERS                  
         GOTO1 DIRHIGH                                                          
         CLC   KEY(27),KEYSAVE     RECORD FOUND?                                
         BNE   TSTA0160            NO  - EXIT WITH ERROR                        
         GOTO1 FILREAD             RETRIEVE STATION RECORD                      
*                                                                               
         L     R4,AIOADDR2                                                      
         USING RSTAREC,R4                                                       
*                                                                               
*                                                                               
         MVC   SAVSTDAT,RSTASTRT                                                
         GOTO1 DATCON,DMCB,(3,RSTASTRT),(0,WORK)                                
*                                  GET EBCDIC DATE OF JOIN                      
         DROP  R4                                                               
         GOTO1 =V(GETDAY),DMCB,WORK,DUB,RR=SUBRELO                              
         CLI   DMCB,X'1'           IS FIRST DAY MONDAY?                         
         BE    TSTA0120            YES                                          
         ZIC   RE,DMCB             GET DAY OF WEEK NUMBER                       
         BCTR  RE,0                SUBTRACT 1 DAY                               
         LNR   RE,RE               MAKE IT NEGATIVE                             
         ST    RE,DMCB+8           SET ADDAY ADJUST VALUE                       
         GOTO1 =V(ADDAY),DMCB,WORK,WORK,,RR=SUBRELO                             
TSTA0120 EQU   *                                                                
         GOTO1 DATCON,DMCB,(0,WORK),(3,BEFFDATE)                                
*                                  GENERATE 3-CHAR BINARY DATE                  
         GOTO1 DATCON,DMCB,(0,WORK),(2,EFDTCOMP)                                
*                                  GENERATE 2-CHAR COMPRESSED DATE              
         SR    R0,R0               SET CC = ZERO                                
         B     TSTA0240                                                         
TSTA0160 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
TSTA0240 EQU   *                                                                
         XIT1                                                                   
****>>>                                                                         
*                                                                               
*   CHEKSSTA: ACCESS THE SOURCE STA RECORD TO DETERMINE IF STATION              
*        IS CLEARED FOR TAKEOVER PROCESSING.  THIS CODE CHECKS THE              
*        SOURCE REP'S STATION RECORD, WHERE                                     
*              1.  THE STATION MUST EXIST                                       
*              2.  THE LEAVE DATE MUST BE EQUAL OR BEFORE THE                   
*                  EFFECTIVE DATE                                               
*              3.  THE NEW REP MUST BE THE TARGET REP                           
*                                                                               
*                                                                               
CHEKSSTA NTR1                                                                   
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,2               INSERT RECORD TYPE                           
         MVC   KEY+20(2),SRCEREP   INSERT SOURCE REP CODE                       
         MVC   KEY+22(5),SRCESTAT  INSERT STATION CALL LETTERS                  
         GOTO1 DIRHIGH                                                          
         CLC   KEY(27),KEYSAVE     RECORD FOUND?                                
         BNE   SSTA0160            NO  - EXIT WITH ERROR                        
         GOTO1 FILREAD             RETRIEVE STATION RECORD                      
*                                                                               
         L     RE,AIOADDR2                                                      
         USING RSTAREC,RE                                                       
*                                                                               
         OC    RSTAEND,RSTAEND     LEAVE DATE ENTERED?                          
         BZ    SSTA0160            NO  - STA NOT MARKED FOR TAKEOVER            
         CLC   SAVSTDAT,RSTAEND                                                 
         BL    SSTA0160            EFFECTIVE DATE BEFORE LEAVE:                 
*                                     REJECTED                                  
         DROP  R5                                                               
*                                                                               
         LA    R6,RSTAELEM         SET A(01 DESCRIPTION ELEMENT)                
*                                                                               
         DROP  RE                                                               
*                                                                               
         USING RSTAFNEL,R6                                                      
SSTA0010 EQU   *                                                                
         CLI   0(R6),0             END OF RECORD?                               
         BE    SSTA0160            YES - REJECT                                 
         CLI   0(R6),X'0C'         FORMER REP/NEW REP ELEMENT?                  
         BE    SSTA0015            YES                                          
         ZIC   RF,1(R6)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R6,RF                                                            
         B     SSTA0010            GO BACK FOR NEXT                             
SSTA0015 EQU   *                                                                
         LA    RF,2                SET COMPARE LENGTH = 3 CHARS                 
         CLI   RSTAFNNE+2,C' '     LAST CHAR NEW REP = SPACE?                   
         BE    SSTA0020            YES - DROP COMPARE 1 POSITION                
         CLI   RSTAFNNE+2,X'00'    LAST CHAR NEW REP = BINARY ZERO?             
         BNE   SSTA0040            NO  -                                        
SSTA0020 EQU   *                                                                
         LA    RF,1                SET COMPARE LENGTH = 2 CHARS                 
SSTA0040 EQU   *                                                                
         EX    RF,SSTA004X         COMPARE BY LENGTH                            
         B     SSTA0060                                                         
SSTA004X CLC   RSTAFNNE(0),TRGTREP                                              
*                                  NEW REP VS SCREEN TARGET REP                 
SSTA0060 EQU   *                                                                
         BE    SSTA0200            SAME REP - ACCEPT                            
*                                                                               
*   NOT FOUND:  DO SECONDARY COMPARISON                                         
*        TABLE SETUP:                                                           
*              CHARS 0 - 2 = DARE TABLE ENTRY/OLD-NEW STATION                   
*              CHARS 3 - 5 = ACCEPTABLE SIGNON ID (3 CHARS)                     
*                                                                               
*                                                                               
         LA    R4,SECRPTBL                                                      
SSTA0080 EQU   *                                                                
         CLI   0(R4),0             END OF TABLE?                                
         BE    SSTA0160            YES - SOURCE NOT VALIDATED                   
         CLC   RSTAFNNE(3),0(R4)   STATION/NEW REP = TABLE ENTRY?               
         BNE   SSTA0100            NO                                           
         CLC   TRGTREP(3),3(R4)    YES - VALID SIGN ON?                         
         BE    SSTA0200            YES - PROCEED                                
SSTA0100 EQU   *                                                                
         LA    R4,LSECRPTB(R4)     NO  - BUMP TO NEXT TABLE ENTRY               
         B     SSTA0080            GO BACK FOR NEXT                             
SECRPTBL EQU   *                                                                
         DC    C'ARPALL'                                                        
LSECRPTB EQU   *-SECRPTBL                                                       
         DC    C'CMLCUM'                                                        
         DC    X'0000'             DELIMITER                                    
*                                                                               
         DC    H'0'                                                             
*                                                                               
         DROP  R6                                                               
*                                  NOT SAME REP - REJECT                        
*                                                                               
SSTA0160 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
         B     SSTA0240                                                         
SSTA0200 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
SSTA0240 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
****>>>                                                                         
*                                                                               
*   VRTKOAGY: VALIDATES AGENCY AGAINST SOURCE FILE                              
*                                                                               
         DS    0F                                                               
VRTKOAGY NMOD1 0,*TKOA*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            RESET A(OLD REP FIELD)                       
         GOTO1 =A(SWSRCREP),DMCB,(RC),RR=YES                                    
*                                  SWITCH TO SOURCE REP                         
         XC    SCANBLOK(LSCANBLK),SCANBLOK                                      
         BAS   RE,DELIMIT2                                                      
         GOTO1 =V(SCANNER),P1,(R2),(2,SCANBLOK),RR=SUBRELO                      
         CLI   P2,0                                                             
         BE    VRAG0750            NO INPUT IS ERROR                            
*                                                                               
         CLI   P2,2                                                             
         BH    VRAG0750            MORE THAN 2 IPT FIELDS IS ERROR              
*                                                                               
*- MOVE FIELDS DIRECTLY TO REQUEST CARD.                                        
         LA    RF,SCANBLK2+12      ASSUME AAAA,OO FORMAT                        
         CLI   P2,2                                                             
         BE    VRAG0600                                                         
*                                                                               
VRAG0020 EQU   *                   SPECIAL FOR SET RECORD                       
VRAG0040 DS    0H                                                               
         LA    RF,SCANBLK1+12+4    ASSUME AAAAOO FORMAT                         
         CLI   SCANBLK1+0,4                                                     
         BH    VRAG0600                                                         
*                                                                               
         LA    RF,AGSPACES         NO AGENCY OFFICE                             
*                                                                               
VRAG0600 MVC   RAGYO,0(RF)         AGY OFFICE CODE OR BLANKS                    
*                                                                               
         MVC   RAGY,SCANBLK1+12    AGENCY CODE                                  
*                                                                               
         MVI   KEY,X'0A'                                                        
         MVC   KEY+19(4),RAGY                                                   
         MVC   KEY+23(2),RAGYO                                                  
         MVC   KEY+25(2),SRCEREP                                                
         GOTO1 DIRHIGH                                                          
         CLC   KEY(27),KEYSAVE     KEY FOUND ON FILE?                           
*                                                                               
         BNZ   VRAG0700            NOT ON FILE:  ERROR                          
         GOTO1 =A(SWTGTREP),DMCB,(RC),RR=YES                                    
*                                  SWITCH BACK TO TARGET REP                    
         B     VRAG0800                                                         
VRAG0700 EQU   *                                                                
         GOTO1 PUTCURS,P1,VTKOAGYC,AGYNOTFD,L'AGYNOTFD                          
         B     VRAG0775                                                         
VRAG0750 EQU   *                                                                
         GOTO1 PUTCURS,P1,VTKOAGYC,AGYINVAL,L'AGYINVAL                          
VRAG0775 EQU   *                                                                
         GOTO1 =A(SWTGTREP),DMCB,(RC),RR=YES                                    
*                                  SWITCH BACK TO TARGET REP                    
         LTR   RB,RB               SET CC NOT = ZERO:  ERROR                    
         B     VRAG0900                                                         
VRAG0800 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
VRAG0900 EQU   *                                                                
         XIT1                                                                   
AGYNOTFD DC    C'AGENCY NOT FOUND ON SOURCE FILE'                               
AGYINVAL DC    C'AGENCY INVALID AS ENTERED'                                     
AGSPACES DC    CL20' '                                                          
         EJECT                                                                  
*                                                                               
*- DELIMITER -- SET UP P3 WITH DELIMITER FOR SCANNER.                           
*                                                                               
*  INSPECT USER FIELD FOR POSSIBLE DELIMITERS.                                  
*                                                                               
*  R2 = A(FIELD HEADER)                                                         
*                                                                               
DELIMIT2 NTR1                                                                   
         MVC   P3,=CL4',=,='       DEFAULT TO , AND =                           
         ZIC   R0,5(R2)            IPT LENGTH FOR LOOP COUNTER                  
         LA    R2,8(R2)                                                         
DELI0020 EQU   *                                                                
         CLI   0(R2),C','          COMMA?                                       
         BE    DELI0040                                                         
         CLI   0(R2),C'-'          DASH?                                        
         BE    DELI0040                                                         
         LA    R2,1(R2)                                                         
         BCT   R0,DELI0020                                                      
         B     DELI0050            NO HIT.  LEAVE P3 AS DEFAULT                 
DELI0040 MVC   P3+2(1),0(R2)       FIELD SEPARATOR TO P3                        
DELI0050 XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   VRCLOSMN: VALIDATES CLOSE MONTH ENTERED                                     
*                                                                               
         DS    0F                                                               
VRCLOSMN NMOD1 0,*CLOS*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         ZIC   R3,5(R2)            LENGTH OF INPUT                              
         LA    R4,8(R2)            A(DATE TO CHECK)                             
         GOTO1 =A(DODATE2),P1,((R3),(R4)),R2CLOSMN,1,(RC),RR=SUBRELO            
         BZ    VRCL0800            OKAY RETURN                                  
         GOTO1 PUTCURS,P1,VCLOSMON,CLOSDTNG,L'CLOSDTNG                          
         LTR   RB,RB               SET CC NOT = ZERO:  ERROR                    
         B     VRCL0900                                                         
VRCL0800 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
VRCL0900 EQU   *                                                                
         XIT1                                                                   
CLOSDTNG DC    C'CLOSED THRU DATE INVALID'                                      
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   VRTKOOFF: VALIDATES OFFICE AGAINST SOURCE FILE                              
*                                                                               
         DS    0F                                                               
VRTKOOFF NMOD1 0,*TKOO*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            RESET A(OLD REP FIELD)                       
         GOTO1 =A(SWSRCREP),DMCB,(RC),RR=YES                                    
*                                  SWITCH TO SOURCE REP                         
         OC    8(2,R2),=X'4040'    'OR' IN SPACES                               
         MVC   ROFF,8(R2)          OFFICE CODE                                  
         CLC   ROFF,=C'  '         ANY OFFICE ENTERED?                          
         BE    VROF0800            NO  - INDICATE OKAY                          
*                                                                               
         MVI   KEY,X'04'                                                        
         MVC   KEY+23(2),SRCEREP   INSERT SOURCE REP                            
         MVC   KEY+25(2),ROFF      INSERT SOURCE OFFICE                         
         GOTO1 DIRHIGH                                                          
         CLC   KEY(27),KEYSAVE     KEY FOUND ON FILE?                           
*                                                                               
         BNZ   VROF0700            NOT ON FILE:  ERROR                          
         GOTO1 =A(SWTGTREP),DMCB,(RC),RR=YES                                    
*                                  SWITCH BACK TO TARGET REP                    
         B     VROF0800                                                         
VROF0700 EQU   *                                                                
         GOTO1 =A(SWTGTREP),DMCB,(RC),RR=YES                                    
*                                  SWITCH BACK TO TARGET REP                    
         GOTO1 PUTCURS,P1,VTKOOFFC,OFFNOTFD,L'OFFNOTFD                          
         LTR   RB,RB               SET CC NOT = ZERO:  ERROR                    
         B     VROF0900                                                         
VROF0800 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
VROF0900 EQU   *                                                                
         XIT1                                                                   
OFFNOTFD DC    C'OFFICE NOT FOUND ON FILE'                                      
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   CHECK12:  SET PERVERT RESULTS FOR DATE CHECKING ROUTINES                    
*                                                                               
         DS    0F                                                               
CHECK12  NMOD1 0,*CK12*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVC   WORK(4),RSTRD       INSERT START DATE                            
         MVC   WORK+4(2),=C'15'    SET TO 15TH OF MONTH                         
         GOTO1 DATCON,DMCB,(0,WORK),(0,WORK+12)                                 
*                                  SET YEAR-2000 STYLE EBCDIC DATE              
         MVC   WORK+6(4),RENDD     INSERT END   DATE                            
         MVC   WORK+10(2),=C'15'   SET TO 15TH OF MONTH                         
         GOTO1 DATCON,DMCB,(0,WORK+6),(0,WORK+18)                               
         GOTO1 =V(PERVERT),DMCB,WORK+12,WORK+18,0,0,RR=YES                      
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
         DS    0F                                                               
XPERDVAL NMOD1 0,*XPDV*                                                         
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         BAS   RE,STRDNTR          FILL IN START DATE.                          
         BNZ   PERDV30             ERROR, BUT CHECK IF RFP IN USE               
*                                                                               
         MVC   RENDD,RSTRD        END = START                                   
*                                                                               
         CLC   RSTRD+2(2),=C'12'  IS MONTH DECEMBER?                            
         BNE   PERDV20            NO-                                           
         MVC   RSTRD+2(2),=C'01'  YES - JUST NEED TO CHANGE START               
         B     PERDV60            MONTH TO JANUARY                              
*                                                                               
* NEED TO SUBTRACT ONE FROM YEAR AND ADD ONE TO THE MONTH                       
PERDV20  EQU   *                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(4),RSTRD       LOAD YYMM                                    
         MVC   WORK+4(2),=C'01'    SET DAY TO 01                                
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+6)                                  
         ZIC   RF,WORK+6           SET YEAR BACK 1                              
         BCTR  RF,0                                                             
         STC   RF,WORK+6                                                        
         ZIC   RF,WORK+7                                                        
         LA    RF,1(RF)            SET MONTH UP 1                               
         STC   RF,WORK+7                                                        
         GOTO1 DATCON,DMCB,(3,WORK+6),(X'20',WORK)                              
         MVC   RSTRD(4),WORK       RESET YEAR AND MONTH                         
         B     PERDV60                                                          
*                                                                               
PERDV30  DS    0H                                                               
         TM    RFPSTAT,RFPINUSE    RFP NOT IN USE, ERROR                        
         BZ    PERDV70                                                          
*                                                                               
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPSYMB                                                
         MVC   QRFPWORK(6),8(R2)                                                
         OC    QRFPWORK,SPACESPV                                                
         GOTO1 AGETOVLY,P1,('OVRFP',0)                                          
         CLI   QRFPMODE,QRFPOK                                                  
         BNE   PERDV70             INVALID SYMBOLIC EQUATE                      
*                                                                               
         L     R3,AMAPNTRY                                                      
         TM    MAPFMT(R3),X'08'    FOR DATES WITH MMM/YY                        
         BO    PERDV40                                                          
*                                                                               
         CLC   QRFPDICT,=Y(E#STENDF)                                            
         BE    PERDV50             INVALID SYMBOLIC EQUATE                      
         B     PERDV70                                                          
*                                                                               
PERDV40  DS    0H                                                               
         CLC   QRFPDICT,=Y(E#STEDFM)                                            
         BNE   PERDV70             INVALID SYMBOLIC EQUATE                      
*                                                                               
PERDV50  DS    0H                                                               
         MVC   RSTRD(12),SPACESPV      STORE ESC SEQ IN REC CARD                
         MVC   RSTRD(L'QRFPESC),QRFPESC                                         
         MVI   RSTRD+3,12                                                       
PERDV60  EQU   *                                                                
         SR    R0,R0               RETURN CC = ZERO                             
         B     PERDV80                                                          
PERDV70  EQU   *                                                                
         LTR   RB,RB               RETURN CC NOT = ZERO                         
PERDV80  EQU   *                                                                
         XIT1                                                                   
*                                                                               
SPACESPV DC    CL80' '                                                          
         EJECT                                                                  
*                                                                               
*- START DATE ONLY                                                              
STRDNTR  NTR1                      ENTRY WITH AN NTR                            
*                                                                               
         ZIC   R3,5(R2)            LENGTH OF INPUT                              
         LA    R4,8(R2)            A(DATE TO CHECK)                             
         GOTO1 =A(DODATE2),P1,((R3),(R4)),RSTRD,1,(RC),RR=SUBRELO               
         BNZ   STRDV10             ERROR, BUT CHECK IF RFP                      
         B     STRDV24                                                          
STRDV10  DS    0H                                                               
         TM    RFPSTAT,RFPINUSE    RFP NOT IN USE, ERROR                        
         BZ    STRDV28                                                          
*                                                                               
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPSYMB                                                
         MVC   QRFPWORK,8(R2)                                                   
         OC    QRFPWORK,SPACESSV                                                
         GOTO1 AGETOVLY,P1,('OVRFP',0)                                          
         CLI   QRFPMODE,QRFPOK                                                  
         BNE   STRDV28             INVALID SYMBOLIC EQUATE                      
*                                                                               
         L     R3,AMAPNTRY                                                      
         TM    MAPFMT(R3),X'08'    FOR DATES WITH MMM/YY                        
         BO    STRDV15                                                          
*                                                                               
         CLC   QRFPDICT,=Y(E#START)                                             
         BE    STRDV20             INVALID SYMBOLIC EQUATE                      
         B     STRDV28                                                          
*                                                                               
STRDV15  DS    0H                                                               
         CLC   QRFPDICT,=Y(E#STARTM)                                            
         BNE   STRDV28             INVALID SYMBOLIC EQUATE                      
*                                                                               
STRDV20  DS    0H                                                               
         MVC   RSTRD,SPACESSV      STORE ESC SEQ IN REC CARD                    
         MVC   RSTRD(L'QRFPESC),QRFPESC                                         
         MVI   RSTRD+3,6           LEN(START DATE)                              
STRDV24  EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         B     STRDV30                                                          
STRDV28  EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
         B     STRDV30                                                          
STRDV30  EQU   *                                                                
         XIT1                                                                   
*                                                                               
SPACESSV DC    CL80' '                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*- DODATE2 -- VALIDATE 1 DATE.                                                  
*                                                                               
*  INPUT:  P1 = BYTE 1 - LENGTH TO VALIDATE                                     
*               BYTES 2-4 - A(INPUT)                                            
*          P2 = A(OUTPUT)                                                       
*               BYTE 0 = X'80':  RETURN X'F0F0' YR 2000 DATE                    
*          P3 = DATE NUMBER (1 OR 2) FOR DEFAULTING                             
*               (1=START, 2=END)                                                
*          P5 = A(WORKSPACE)                                                    
*                                                                               
*  FORMAT BITS:  X'04' = MMMDD/YY FORMAT                                        
*                X'08' = MMM/YY   FORMAT                                        
*                X'80' = DEFAULT END TO START DATE                              
*                X'40' = DEFAULT START TO END DATE                              
*                X'20' = DEFAULT 1ST DATE TO TODAY                              
*                X'10' = DEFAULT 2ND DATE TO TODAY                              
*                                                                               
DODATE2  NMOD1 0,*DOD2*                                                         
         L     RC,16(R1)           RESET A(WORKSPACE) FROM P5                   
         L     R3,AMAPNTRY         REQMAP ENTRY FOR THIS FIELD                  
*                                                                               
         ZIC   R4,P1               SAVE LENGTH OF DATA                          
         LTR   R4,R4                                                            
         BNZ   DODA0080            PROCESS INPUT.  NO DEFAULTS.                 
*                                                                               
*- TRY FOR DEFAULTS                                                             
         CLI   P3+3,1              1ST (START) DATE?                            
         BNE   DODA0060                                                         
*                                                                               
         TM    MAPFMT(R3),X'20'    DEFAULT TO TODAY?                            
         BZ    DODA0040                                                         
*                                                                               
DODA0020 L     RE,P2                                                            
         MVC   0(6,RE),TODAY                                                    
         B     DODA0120                                                         
*                                                                               
DODA0040 TM    MAPFMT(R3),X'40'    DEFAULT START TO END?                        
         BZ    DODA0140                                                         
         MVC   RSTRD,RENDD                                                      
         B     DODA0160                                                         
*                                                                               
DODA0060 EQU   *                                                                
         CLI   P3+3,2              2ND (END) DATE?                              
         BE    *+6                                                              
         DC    H'0'                INVALID DATE NUMBER                          
*                                                                               
         TM    MAPFMT(R3),X'10'    DEFAULT TO TODAY?                            
         BO    DODA0020                                                         
*                                                                               
         TM    MAPFMT(R3),X'80'    DEFAULT END TO START                         
         BZ    DODA0140                                                         
         MVC   RENDD,RSTRD                                                      
         B     DODA0160                                                         
*                                                                               
DODA0080 EQU   *                                                                
         SR    R2,R2               ASSUME MMMDD/YY FORMAT                       
         TM    MAPFMT(R3),X'04'                                                 
         BO    DODA0100                                                         
         LA    R2,2                TRY FOR MMM/YY FORMAT                        
         TM    MAPFMT(R3),X'08'                                                 
         BO    DODA0100                                                         
         DC    H'0'                INVALID FORMAT                               
DODA0100 EQU   *                                                                
         STC   R2,P1               DATVAL FORMAT CODE                           
         MVI   P2,X'80'            RETURN REGULAR DATE FOR YR 2000              
         GOTO1 DATVAL,P1                                                        
         ICM   RF,15,P1            LENGTH OF VALID IPT.                         
         BZ    DODA0140                                                         
*                                                                               
         CR    R4,RF               LEN OF IPT -VS- LEN OF DATE                  
         BNE   DODA0140                                                         
*                                                                               
*- IF MMM/YY FORMAT DATES, BLANK OUT DAYS                                       
DODA0120 EQU   *                                                                
         TM    MAPFMT(R3),X'08'                                                 
         BZ    DODA0160            NOT MMM/YY FORMAT. EXIT.                     
*                                                                               
         L     RE,P2                                                            
         MVC   4(2,RE),=CL2'  '    BLANK 'DD' OF 'YYMMDD'                       
         B     DODA0160                                                         
DODA0140 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO:  ERROR                    
         B     DODA0180                                                         
DODA0160 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO:  ACCEPTED                     
DODA0180 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
*                                                                               
*- START DATE ONLY                                                              
STRDMOD  NMOD1 0,*SMOD*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         ZIC   R3,5(R2)            LENGTH OF INPUT                              
         LA    R4,8(R2)            A(DATE TO CHECK)                             
         GOTO1 =A(DODATE2),P1,((R3),(R4)),RSTRD,1,(RC),RR=SUBRELO               
         BNZ   SMODV10             ERROR, BUT CHECK IF RFP                      
         B     SMODV24                                                          
SMODV10  DS    0H                                                               
         TM    RFPSTAT,RFPINUSE    RFP NOT IN USE, ERROR                        
         BZ    SMODV28                                                          
*                                                                               
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPSYMB                                                
         MVC   QRFPWORK,8(R2)                                                   
         OC    QRFPWORK,SPACESMV                                                
         GOTO1 AGETOVLY,P1,('OVRFP',0)                                          
         CLI   QRFPMODE,QRFPOK                                                  
         BNE   SMODV28             INVALID SYMBOLIC EQUATE                      
*                                                                               
         L     R3,AMAPNTRY                                                      
         TM    MAPFMT(R3),X'08'    FOR DATES WITH MMM/YY                        
         BO    SMODV15                                                          
*                                                                               
         CLC   QRFPDICT,=Y(E#START)                                             
         BE    SMODV20             INVALID SYMBOLIC EQUATE                      
         B     SMODV28                                                          
*                                                                               
SMODV15  DS    0H                                                               
         CLC   QRFPDICT,=Y(E#STARTM)                                            
         BNE   SMODV28             INVALID SYMBOLIC EQUATE                      
*                                                                               
SMODV20  DS    0H                                                               
         MVC   RSTRD,SPACESMV      STORE ESC SEQ IN REC CARD                    
         MVC   RSTRD(L'QRFPESC),QRFPESC                                         
         MVI   RSTRD+3,6           LEN(START DATE)                              
SMODV24  EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         B     SMODV30                                                          
SMODV28  EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
         B     SMODV30                                                          
SMODV30  EQU   *                                                                
         XIT1                                                                   
*                                                                               
SPACESMV DC    CL80' '                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
*                                                                               
*- SOONFLDS -- SOON REQUEST:  TEST FOR MINIMUM OF FILTER FIELDS TO              
*        ENSURE REPORT DOESN'T RUN FOREVER.                                     
*        REQUEST MAY HAVE ONE OR MORE OF THE FOLLOWING FILTERS:                 
*            OFFICE/STATION/ADVERTISER/AGENCY/GROUP-SUBGROUP                    
*            GROUP ALONE MAY BE SPECIFIED ONLY WITH AN ADDL FILTER.             
*                                                                               
*                                                                               
SOONFLDS NMOD1 0,*SOFL*                                                         
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         LA    RF,RQSNUMH          A(NUMBER FIELD IN HEADER)                    
         CLI   5(RF),2             TWO CHARACTERS ENTERED?                      
         BE    SOFL0040            YES                                          
         LA    RF,1(RF)            NO  - SKIP 'R'                               
*                                                                               
SOFL0040 EQU   *                                                                
*                                                                               
         CLC   8(2,RF),=C'TK'      REQUEST FOR TK REPORT?                       
         BE    SOFL0100            YES - PERMIT ALL SOONS                       
         CLC   8(2,RF),=C'90'      REQUEST FOR 90 REPORT?                       
         BE    SOFL0100            YES - PERMIT ALL SOONS                       
         CLC   8(2,RF),=C'2H'      REQUEST FOR 2H REPORT?                       
         BE    SOFL0100            YES - PERMIT ALL SOONS                       
         CLC   8(2,RF),=C'95'      REQUEST FOR 95 REPORT?                       
         BE    SOFL0100            YES - PERMIT ALL SOONS                       
         CLC   8(2,RF),=C'FW'      REQUEST FOR FW REPORT?                       
         BE    SOFL0100            YES - PERMIT ALL SOONS                       
         CLC   8(2,RF),=C'FZ'      REQUEST FOR FZ REPORT?                       
         BE    SOFL0100            YES - PERMIT ALL SOONS                       
         CLI   8(RF),C'7'          REQUEST FOR 7-SERIES REPORTS?                
         BE    SOFL0100            YES - PERMIT ALL SOONS                       
*                                                                               
         CLC   ROFF,SPACESSF       ANY OFFICE SPECIFIED?                        
         BNE   SOFL0080            YES - OKAY                                   
         CLC   RSTA,SPACESSF       ANY STATION SPECIFIED?                       
         BNE   SOFL0100            YES - STATION ASSUMES GROUP FILTER           
         CLC   RAGY,SPACESSF       ANY AGENCY SPECIFIED?                        
         BNE   SOFL0080            YES - OKAY                                   
         CLC   RADV,SPACESSF       ANY ADVERTISER SPECIFIED?                    
         BNE   SOFL0080            YES - OKAY                                   
*                                  NO ADDITIONAL FILTERS:                       
*                                     SUBGROUP MUST BE SPECIFIED                
         CLI   RGROUP,C' '         ANY GROUP SPECIFIED?                         
         BE    SOFL0060            NO  - DON'T PERMIT IT                        
         CLI   RSGROUP,C' '        YES - ANY SUBGROUP?                          
         BNE   SOFL0100            YES - OKAY                                   
SOFL0060 EQU   *                                                                
         GOTO1 PUTCURS,P1,VGRPSUBG,NEEDFILT,L'NEEDFILT                          
         LTR   RB,RB               SET CC NOT ZERO, FOR ERROR                   
         B     SOFL0120                                                         
SOFL0080 EQU   *                                                                
*                                                                               
*   OFFICE/STATION/AGENCY/ADVERTISER ENTERED:                                   
*        ALSO REQUIRE AT LEAST 'GROUP'                                          
*                                                                               
         CLI   RGROUP,C' '         ANY GROUP SPECIFIED?                         
         BNE   SOFL0100            YES - ACCEPT IT                              
         GOTO1 PUTCURS,P1,VGRPSUBG,SOONGRUP,L'SOONGRUP                          
         LTR   RB,RB               SET CC NOT ZERO, FOR ERROR                   
         B     SOFL0120                                                         
SOFL0100 EQU   *                                                                
         SR    R0,R0               SET CC ZERO, FOR OKAY                        
SOFL0120 EQU   *                                                                
         XIT1                                                                   
*                                                                               
SPACESSF DC    CL12' '                                                          
NEEDFILT DC    C'''SOON'' REQUIRES GRP+AGY/ADV/OFF/STA OR GRP/SUBGRP'           
SOONGRUP DC    C'''SOON'' REQUIRES GROUP CODE'                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*- BUD12 -- START AND END DATES FOR BUDGETS MUST BE 12 MONTHS                   
*                                                                               
*  CC 0 = GOOD, ^0 = FAILED                                                     
*                                                                               
BUD12    NMOD1 0,*BD12*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         TM    RFPSTAT,RFPINUSE                                                 
         BO    BD120200            RFP KEYWORD IN USE, SKIP                     
*                                                                               
         CLC   R2BUDST(12),SPACEBD BAD IF NO DATES.                             
         BE    BD120300            NO GOOD                                      
*                                                                               
         PACK  DUB(4),R2BUDST(2)   MAX DURATION 12 MONTHS                       
         MP    DUB(4),=P'12'                                                    
         PACK  HALF,R2BUDST+2(2)                                                
         AP    DUB(4),HALF                                                      
         PACK  DUB+4(4),R2BUDED(2)                                              
         MP    DUB+4(4),=P'12'                                                  
         PACK  HALF,R2BUDED+2(2)                                                
         AP    DUB+4(4),HALF                                                    
         SP    DUB+4(4),DUB(4)                                                  
         CP    DUB+4(4),=P'11'    12 - 1                                        
         BE    BD120200            GOOD                                         
*                                                                               
         GOTO1 PUTCURS,P1,VBUDSTEN,('ECMSGNUM',NEED12M),0                       
         B     BD120300            NO GOOD                                      
BD120200 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         B     BD120400                                                         
BD120300 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
BD120400 EQU   *                                                                
         XIT1                                                                   
SPACEBD  DC    CL12' '                                                          
         LTORG                                                                  
         EJECT                                                                  
         SPACE 2                                                                
*                                                                               
*- OF2LIMIT -- TEST FOR LIMITED OFFICE ACCESS USING OFFICE FILTER               
*                                                                               
OF2LIMIT NMOD1 0,*O2LIM*                                                        
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*                                                                               
*                                                                               
*- CHECK FOR LIMITED OFFICE ACCESS                                              
*                                                                               
         L     RE,AREQNTRY                                                      
         TM    RQCNTL1(RE),RQ1OFLMT    LIMIT TEST REQUIRED?                     
         BZ    O2LM0200                NO                                       
*                                                                               
*- OUTSIDE OFFICE FOR A REP?                                                    
         CLC   =C'O=',TWAACCS      TEST FOR OFFICE LIMITED ACCESS               
         BNE   O2LM0200            NOT NEEDED                                   
*                                                                               
         TM    TWAAUTH,X'80'       TERMINAL HAS ACCESS TO ALL OFFICES?          
         BO    O2LM0200            YES                                          
*                                                                               
         CLC   ROFF,TWAACCS+2      NO, MUST MATCH RESTRICTED OFFICE             
         BE    O2LM0200                                                         
*- ACCESS DENIED.                                                               
*  PUT CURSOR IN STATION FIELD AND ISSUE OFFICE VIOLATION MSG.                  
O2LM0160 EQU   *                                                                
         GOTO1 PUTCURS,P1,VOFFICEC,OFFRESTR,L'OFFRESTR                          
*                                                                               
         LTR   RB,RB               SET CC NOT = ZERO                            
         B     O2LM0400                                                         
O2LM0200 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
O2LM0400 EQU   *                                                                
         XIT1                                                                   
OFFRESTR DC    C'USER NOT AUTHORIZED DATA FOR THIS OFFICE'                      
         DC    H'0'                                                             
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
*- OFFLIMIT -- TEST FOR LIMITED OFFICE ACCESS FOR STATIONS.                     
*                                                                               
OFFLIMIT NMOD1 0,*OLIM*                                                         
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         CLI   RSTA,C'*'           STATION SET REQUESTED?                       
         BNE   OLIM0070            NO                                           
         CLC   RSETSTA(4),=C'$GAN' YES - SPECIAL SET REQUESTED?                 
         BNE   OLIM0070            NO                                           
         CLC   TWAAGY,=C'BL'       YES - REQUEST FROM 'BLAIR'?                  
         BE    OLIM0200            YES - IGNORE SECURITY CHECK                  
         CLC   TWAAGY,=C'PV'       YES - REQUEST FROM 'PETRY'?                  
         BE    OLIM0200            YES - IGNORE SECURITY CHECK                  
*- DOES THIS REQUEST REQUIRE OFFICE LIMIT TESTING?                              
OLIM0070 EQU   *                                                                
         L     R1,AREQNTRY                                                      
         TM    RQCNTL1(R1),RQ1OFLMT                                             
         BZ    OLIM0200            NOT REQUIRED. EXIT OK.                       
*                                                                               
*- THIS TEST ONLY APPLIES TO STATIONS                                           
         CLI   TWAACCS,C'$'                                                     
         BNE   OLIM0200            NOT A STATION                                
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),TWAAGY                                                 
         MVC   KEY+22(5),RSTA                                                   
         GOTO1 DIRHIGH             GET STATION RECORD                           
         CLC   KEY(27),KEYSAVE                                                  
         BNE   OLIM0160            NO KEY, NO ACCESS                            
         GOTO1 FILREAD                                                          
*                                                                               
*                                                                               
         L     RE,AIOADDR2                                                      
         USING RSTAREC,RE                                                       
*                                                                               
         LA    R5,RSTAREC                                                       
         SR    R7,R7                                                            
         ICM   R7,3,RSTALEN                                                     
         AR    R7,R5                                                            
         LA    R5,RSTAELEM                                                      
*                                                                               
         DROP  RE                                                               
*                                                                               
         SR    R1,R1                                                            
*                                                                               
*- LOOK FOR THE X'06' SIGNON ID ELEMENT                                         
OLIM0080 CLI   0(R5),X'06'                                                      
         BNE   OLIM0120                                                         
         USING RSTASOEL,R5                                                      
         CLC   RSTASID,TWAUSRID   COMPARE STATION SIGN ON ID TO USER ID         
         BE    OLIM0200           OK                                            
         DROP  R5                                                               
*                                                                               
OLIM0120 ZIC   R1,1(R5)                                                         
         LTR   R1,R1                                                            
         BZ    OLIM0160            END OF RECORD??                              
         AR    R5,R1               BUMP TO NEXT ELEMENT                         
         CR    R5,R7               END OF RECORD?                               
         BL    OLIM0080            NO  - GO BACK FOR NEXT                       
*                                                                               
*- ACCESS DENIED.                                                               
*  PUT CURSOR IN STATION FIELD AND ISSUE SECURITY LOCKOUT MSG.                  
OLIM0160 EQU   *                                                                
         GOTO1 PUTCURS,P1,VSTATION,('ECMSGNUM',SECLOCK),0                       
*                                                                               
         LTR   RB,RB               SET CC NOT = ZERO                            
         B     OLIM0400                                                         
OLIM0200 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
OLIM0400 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*- NUMCARDS -- COUNT NUMBER OF CONTROL CARDS                                    
*                                                                               
*  RETURN: P1 = # CARDS (INCLUDING PRIMARY CARD)                                
*          P2 = LENGTH OF ALL CARDS + 80 BYTE HEADER                            
*                                                                               
NUMCARDS NTR1  BASE=*,LABEL=*                                                   
         L     R9,0(R1)            RESET A(WORKSPACE)                           
         SR    R1,R1               # CARDS                                      
         LA    RE,REQCARD                                                       
*                                                                               
NUMC100  EQU   *                                                                
         CLC   0(80,RE),SPACES     ALL BLANK?                                   
         BE    NUMC200                                                          
*                                                                               
         CLC   80(80,RE),SPACES    FORCE CONTINUATION CHAR IF NEXT              
*                                     CARD HAS DATA                             
         BE    NUMC150             NO DATA: CARD CONTAINS SPACES                
****>>>> BNH   NUMC150             NO DATA: SPACES OR BINARY ZERO               
         CLI   79(RE),CTINCHAR     ALREADY SET?                                 
         BE    NUMC150              YES                                         
         ZIC   RF,79(RE)           SAVE OFF WHAT'S THERE                        
         STC   RF,80(RE)            AND PUT IT ON THE NEXT CARD                 
         MVI   79(RE),CTINCHAR     AND FORCE THE CONTINUATION                   
NUMC150  EQU   *                                                                
*                                                                               
         LA    R1,1(R1)            BUMP CARD COUNT/ADDRESS                      
         LA    RE,80(RE)                                                        
*                                                                               
         LA    RF,REQREC                                                        
         LA    RF,LREQREC(RF)      A(END OF REQ RECORD)                         
         CR    RE,RF                                                            
         BL    NUMC100             DON'T EXCEED MAX RECORD.                     
*                                                                               
NUMC200  EQU   *                                                                
         ST    R1,P1               NUMBER OF CARDS (INCLUDING PRIMARY)          
         LA    RF,REQREC                                                        
         SR    RE,RF                                                            
         ST    RE,P2               LENGTH, INCLUDING REQ HEADER                 
*                                                                               
         XIT1                                                                   
*SPACES4  DC    CL80' '           USE SPACES INSTEAD TO SAVE 80 BYTES           
*                                                                               
         TITLE 'REQHEAD -- FILL IN REQUEST HEADER FOR ADD'                      
*                                                                               
*- REQHEAD -- FILL IN REQUEST HEADER FOR ADD                                    
*                                                                               
REQHEAD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R9,0(R1)            RESET A(WORKSPACE)                           
         GOTO1 =A(UNCFSCAN),DMCB,(R9),RR=SUBRELO                                
*                                  LOOK FOR OPTION = CONF/UNCF                  
         CLI   RRRGFLAG,C'Y'       RRG REQUEST?                                 
         BNE   REQH0020            NO  - DON'T BOTHER TO SCAN                   
         BAS   RE,CUR$SCAN         LOOK FOR OPTION=CUR$                         
         GOTO1 =A(DIRRESP),DMCB,(R9),RR=SUBRELO                                 
*                                  LOOK FOR OPTION = DR                         
         GOTO1 =A(PAYPROG),DMCB,(R9),RR=SUBRELO                                 
*                                  LOOK FOR OPTION = PP                         
         GOTO1 =A(LOSTSTA),DMCB,(R9),RR=SUBRELO                                 
*                                  LOOK FOR OPTION = R1/R2/R3                   
         GOTO1 =A(DOWNHEAD),DMCB,(R9),RR=SUBRELO                                
*                                  LOOK FOR OPTION = HD (D/L HEADINGS)          
         GOTO1 =A(ALTCALEN),DMCB,(R9),RR=SUBRELO                                
*                                  LOOK FOR OPTION = AC (ALT CALENDAR)          
         GOTO1 =A(TRADE),DMCB,(R9),RR=SUBRELO                                   
*                                  LOOK FOR OPTION = TR (TRADE)                 
         GOTO1 =A(ALLCOLS),DMCB,(R9),RR=SUBRELO                                 
*                                  LOOK FOR OPTION = ALL (ALL COLUMNS)          
         GOTO1 =A(BAKBILL),DMCB,(R9),RR=SUBRELO                                 
*                                  LOOK FOR OPTION = B+/B-/BA                   
*                                     BACK BILLING                              
         GOTO1 =A(COMPBUD),DMCB,(R9),RR=SUBRELO                                 
*                                  LOOK FOR OPTION = CB                         
*                                     COMPANY BUDGET                            
         GOTO1 =A(SALBUD),DMCB,(R9),RR=SUBRELO                                  
*                                  LOOK FOR OPTION = SP                         
*                                     STATION BUDGET                            
         GOTO1 =A(NATLOC),DMCB,(R9),RR=SUBRELO                                  
*                                  LOOK FOR OPTION = $N/$L                      
*                                     NATIONAL/LOCAL BILLING                    
         GOTO1 =A(RERFLG),DMCB,(R9),RR=SUBRELO                                  
*                                  LOOK FOR OPTION = R+/R-                      
*                                     RER ONLY/EXCLUDE RER                      
         GOTO1 =A(FORCST),DMCB,(R9),RR=SUBRELO                                  
*                                  LOOK FOR OPTION = FC                         
*                                     $0 FORECAST DISPLAY                       
REQH0020 EQU   *                                                                
*                                                                               
*                                                                               
*- BUILD NEW-STYLE RQST HEADER                                                  
         MVC   RQHAGY,TWAAGY       USER CODE                                    
         MVI   RQHCTRL,0           CONTROL FLAGS                                
         MVC   RQHOFF,TWAOFFC      OFFICE CODE                                  
         MVC   RQHOUT,IOUT         OUTPUT ID                                    
         MVI   RQHNUMB,0           NO BINARY RQST NUMBER                        
         MVC   RQHDEST,IDEST       DESTINATION ID                               
         MVC   RQHORIG,TWAUSRID    ORIGIN ID                                    
         MVI   RQHFLAG,RQHFLNK+RQHFTWO  LINKED, 2 CARD RQST                     
*                                                                               
*   ENTRY OF AN OPTIONAL 'SPACING' VALUE MAY CREATE A SECOND CARD,              
*     WHICH, FOR INSTANCES WHERE NO SECOND CARD HAD PREVIOUSLY                  
*     EXISTED, WILL BE 'THROWN AWAY' BY FILCON.  B UHR 8/91.                    
*     CONTRACT PRINT (R10) MUST NOT HAVE SPACING FORCED.                        
*                                                                               
         LA    RF,RQSNUMH          A(NUMBER FIELD IN HEADER)                    
         CLI   5(RF),2             TWO CHARACTERS ENTERED?                      
         BNE   REQH0040            NO - CHECK FOR 3 CHARACTER INPUT             
         CLC   RQSNUM(2),=C'10'    CONTRACT PRINT REQUEST?                      
         BE    REQH0080            YES - DON'T SET SPACING                      
         B     REQH0060            NO                                           
*                                                                               
REQH0040 EQU   *                                                                
*                                                                               
         CLI   5(RF),3             THREE CHARACTERS ENTERED?                    
         BNE   REQH0060            NO  - SET SPACING                            
         CLC   RQSNUM(2),=C'R10'   YES - CONTRACT PRINT REQUEST?                
         BE    REQH0080            YES - DON'T SET SPACING                      
*                                                                               
REQH0060 EQU   *                                                                
*                                                                               
         MVC   R2SPACE,RQSSPCE     INSERT OPTIONAL SPACING VALUE                
         OC    R2SPACE,SPACES      SCREEN MAY CONTAIN NULLS                     
*                                                                               
*                                                                               
*   COMBINE COMBO OPTION IS NO LONGER SET HERE.  IT IS NOW SET AS A             
*        KEYWORD OPTION IN OPT2FLD                                              
*                                                                               
***      MVC   RCOMPNO,RQSCMBO     INSERT OPTIONAL 'SUPPRESS COMBO'             
***      OC    RCOMPNO,SPACES      SCREEN MAY CONTAIN NULLS                     
*                                                                               
REQH0080 EQU   *                                                                
*                                                                               
         L     RE,AREQNTRY                                                      
         MVC   RNUM,RQCARDID(RE)   CARD ID FROM REQTBL                          
*                                                                               
         GOTO1 =A(NUMCARDS),DMCB,(R9),RR=SUBRELO                                
         CLI   P1+3,2              NUMBER OF CARDS                              
         BL    REQH0100            SINGLE CARD RQST                             
         L     R0,P1                                                            
         SH    R0,=H'1'            = # CARDS -1                                 
*                                                                               
         SLL   R0,4                COUNT TO HI-ORDER NIBBLE                     
         STC   R0,P1                                                            
         OC    RQHFLAG(1),P1       BITS 0-3 = # CARDS-1                         
*                                                                               
*   IF # CARDS-1 =2, THEN THIRD CARD HAS BEEN ENTERED.  TEST                    
*        FOR CROSS-FILE OPTION REQUEST.  SET REPORT GROUP 'RZ'                  
*        IF USED. ALSO TEST FOR REQUEST DRIVEN FROM BUYLINE CODE                
*        PASSIVE KEYS (X'9B01').  IF FOUND, SET GROUP 'RZ'.                     
*        IN NEITHER CASE CAN THESE REQUESTS BE GANGED WITH OTHERS.              
*                                                                               
         CLI   P1,2                CARD COUNT = 2?                              
         BL    REQH0100            NO  - DON'T TEST                             
         CLC   RXFILNM,SPACES      ANY CROSS-FILE ENTRY?                        
         BE    REQH0090            NO  - DON'T OVERRIDE REPORT GROUP            
         OC    RXFILNM,RXFILNM     DITTO                                        
         BZ    REQH0090            NO                                           
         MVC   RNUM(2),=C'RZ'      YES - FORCE SPECIAL GROUP                    
         B     REQH0100                                                         
REQH0090 EQU   *                                                                
         CLI   RBUYCOD,C'Y'        BUYLINE CODE PASSIVE REQ?                    
         BNE   REQH0100            NO                                           
REQH0095 EQU   *                                                                
         MVC   RNUM(2),=C'RZ'      YES - FORCE SPECIAL GROUP                    
*                                                                               
*- FILL IN BASIC CARD FIELDS                                                    
*                                                                               
REQH0100 EQU   *                                                                
         L     RE,AREQNTRY                                                      
         CLC   =XL2'00',RQOPT4ID(RE)                                            
         BE    REQH0120                                                         
         MVC   ROPTN4(2),RQOPT4ID(RE) OPTIONAL RRG ID                           
REQH0120 EQU   *                                                                
         MVC   RREP,TWAAGY         REP CODE                                     
         MVC   RREPO,IREQOFF       REQUESTING OFFICE                            
         MVC   RNAME,RQSNAME       REQUESTOR                                    
         OC    RNAME,SPACES        -SCREEN MAY HAVE NULLS                       
*                                                                               
         XIT1                                                                   
*SPACES3  DC    CL80' '            USE SPACES INSTEAD TO SAVE 80 BYTES          
*                                                                               
*- CUR$SCAN:  SCAN OPTION FIELD FOR 'CUR$'.  IF FOUND, SET RSUPDET              
*        IN THIRD REQUEST CARD                                                  
*                                                                               
CUR$SCAN NTR1                                                                   
         LA    R2,RQSOPT           SET A(OPTIONS FIELD)                         
         LA    RF,12                                                            
CSCA0020 EQU   *                                                                
         CLC   =C'CUR$',0(R2)      LOOK FOR OPTION                              
         BE    CSCA0060            FOUND                                        
         LA    R2,1(R2)            NOT FOUND - MOVE OVER 1                      
         BCT   RF,CSCA0020                                                      
         B     CSCA0080            NOT FOUND AT ALL - EXIT                      
CSCA0060 EQU   *                                                                
         MVI   RSUPDET,C'Y'        SET OPTION TO 'Y'                            
CSCA0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*- REPSTNAM:  SCAN OPTION FIELD FOR 'REP='.  IF FOUND, VALIDATE                 
*        EXISTENCE OF SET NAME, SET NAME IN CARD3                               
*                                                                               
REPSTNAM NMOD1 0,*RSNM*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,RQSOPT           SET A(OPTIONS FIELD)                         
         LA    RF,12                                                            
RSTN0020 EQU   *                                                                
         CLC   =C'REP=',0(R2)      LOOK FOR OPTION                              
         BE    RSTN0060            FOUND                                        
         LA    R2,1(R2)            NOT FOUND - MOVE OVER 1                      
         BCT   RF,RSTN0020                                                      
         B     RSTN0080            NOT FOUND AT ALL - EXIT                      
RSTN0060 EQU   *                                                                
         CLI   IAMAMAST,C'Y'       MASTER RUN?                                  
         BNE   RSTN0100            NO  - DON'T PERMIT 'REP='                    
         MVI   KEY,X'38'                                                        
         MVC   KEY+19(2),TWAAGY                                                 
         MVC   KEY+21(2),=C'RE'                                                 
         MVC   KEY+23(4),4(R2)     MOVE 4 CHARS PAST 'REP='                     
         OC    KEY+23(4),SPACES                                                 
                                                                                
         GOTO1 DIRREAD,P1          LOOK FOR EXACT KEY                           
         CLI   P3-P1(R1),0         NOT FOUND?                                   
         BNE   RSTN0100            ERROR                                        
                                                                                
         MVC   RREPSET(4),KEY+23                                                
RSTN0080 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         B     RSTN0120                                                         
RSTN0100 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
RSTN0120 EQU   *                                                                
         XIT1                                                                   
*                                                                               
*SPACES2  DC    CL20' '            USE SPACES INSTEAD TO SAVE 20 BYTES          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*- BAD0420:  CODE SHUFFLED OFF TO REGAIN ADDRESSABILITY                         
*                                                                               
BAD0420  NMOD1 0,*0420*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XC    RQSMSG,RQSMSG                                                    
         MVC   RQSMSG(60),=CL60'REQUEST CANNOT BE ADDED TO A SUBMITTED X        
               GROUP'                                                           
         CLI   QRFPMODE,QRFPNORO                                                
         BNE   BAD0900                                                          
         MVC   RQSMSG(60),=CL60'REQUEST CANNOT BE ADDED - MAXIMUM # OF X        
               REQUESTS IS 50'                                                  
BAD0900  EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*- OPT2FLD:  SCAN OPTION2 FIELD FOR VARIOUS KEYWORD OPTIONS.                    
*        VALIDATE, AND SET FLAGS IN CARD3                                       
*        CC=Y    -   COMBINE COMBOS                                             
*        A1=XX   -   FILTER FOR BUYLINE CODE ATTRIBUTE 1                        
*        A2=XX   -   FILTER FOR BUYLINE CODE ATTRIBUTE 2                        
*        BC=YYY  -   FILTER FOR BUYLINE CODE                                    
*        RB=Y    -   USE BUYLINE READER TO DRIVE REPORT                         
*        CP=XXX  -   FILTER FOR COMP S/P XXX                                    
*                                                                               
OPT2FLD  NMOD1 0,*OPT2*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,RQSOPT2H         SET A(OPTIONS FIELD)                         
         XC    SCANBLOK(LSCANBLK),SCANBLOK                                      
         BAS   RE,DELIMITR                                                      
         GOTO1 =V(SCANNER),P1,(R2),(8,SCANBLOK),RR=SUBRELO                      
         LA    R2,SCANBLOK         SET A(SCANBLOK)                              
OPT20020 EQU   *                                                                
         OC    0(2,R2),0(R2)       DOES SCAN SECTION HAVE DATA?                 
         BZ    OPT20800            NO  - OPTIONS FINISHED                       
         BAS   RE,OPT2CHK          YES - VALIDATE THE OPTION                    
         BNZ   OPT20900            ERROR ENCOUNTERED: EXIT CC NOT ZERO          
         LA    R2,32(R2)           BUMP TO NEXT OPTION                          
         B     OPT20020            GO BACK FOR NEXT                             
OPT20800 EQU   *                                                                
         SR    R0,R0               SET CC ZERO                                  
         B     OPT20920            EXIT                                         
OPT20900 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
OPT20920 EQU   *                                                                
         XIT1                                                                   
         DS    0F                                                               
         EJECT                                                                  
OPT2CHK  NTR1                                                                   
         CLC   =C'CC',12(R2)       COMBINE COMBOS?                              
         BNE   OCHK0040            NO                                           
         CLI   22(R2),C'Y'         YES - OPTION = Y?                            
         BNE   OCHK0400            NO  - ERROR                                  
         MVI   RCOMPNO,C'Y'        YES - SET CARD OUTPUT                        
         B     OCHK0900            EXIT CC ZERO                                 
OCHK0040 EQU   *                                                                
         CLC   =C'A1',12(R2)       ATTRIBUTE 1 FILTER?                          
         BNE   OCHK0080            NO                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'4B'                                                        
         MVC   KEY+21(2),TWAAGY                                                 
         MVI   KEY+23,0            KEYTYPE = 0                                  
         MVC   KEY+24(2),22(R2)    INSERT TWO CHARS FROM CODE                   
         CLI   KEY+25,C' '         2ND CHAR SPACE?                              
         BNE   OCHK0050            NO                                           
         MVI   KEY+25,X'00'                                                     
OCHK0050 EQU   *                                                                
                                                                                
         GOTO1 DIRREAD,P1          LOOK FOR EXACT KEY                           
         CLI   P3-P1(R1),0         NOT FOUND?                                   
         BNE   OCHK0440            ERROR                                        
         MVC   RBUYATT1,22(R2)     YES - SET CARD OUTPUT                        
         MVI   RBUYCOD,C'Y'        TURN ON 'USE BUYLINE READER'                 
         B     OCHK0900            EXIT CC ZERO                                 
OCHK0080 EQU   *                                                                
         CLC   =C'A2',12(R2)       ATTRIBUTE 2 FILTER?                          
         BNE   OCHK0100            NO                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'4B'                                                        
         MVC   KEY+21(2),TWAAGY                                                 
         MVI   KEY+23,0            KEYTYPE = 0                                  
         MVC   KEY+24(2),22(R2)    INSERT TWO CHARS FROM CODE                   
         CLI   KEY+25,C' '         2ND CHAR SPACE?                              
         BNE   OCHK0090            NO                                           
         MVI   KEY+25,X'00'                                                     
OCHK0090 EQU   *                                                                
                                                                                
         GOTO1 DIRREAD,P1          LOOK FOR EXACT KEY                           
         CLI   P3-P1(R1),0         NOT FOUND?                                   
         BNE   OCHK0480            ERROR                                        
         MVC   RBUYATT2,22(R2)     YES - SET CARD OUTPUT                        
         MVI   RBUYCOD,C'Y'        TURN ON 'USE BUYLINE READER'                 
         B     OCHK0900            EXIT CC ZERO                                 
OCHK0100 EQU   *                                                                
         CLC   =C'BC',12(R2)       BUYLINE CODE FILTER?                         
         BNE   OCHK0120            NO                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'4B'                                                        
         MVC   KEY+21(2),TWAAGY                                                 
         MVI   KEY+23,1            KEYTYPE = 1                                  
         MVC   KEY+24(3),22(R2)    INSERT THREE CHARS FROM CODE                 
         CLI   KEY+25,C' '         2ND CHAR SPACE?                              
         BNE   OCHK0110            NO                                           
         MVI   KEY+25,X'00'                                                     
OCHK0110 EQU   *                                                                
         CLI   KEY+26,C' '         3RD CHAR SPACE?                              
         BNE   OCHK0112            NO                                           
         MVI   KEY+26,X'00'                                                     
OCHK0112 EQU   *                                                                
         GOTO1 DIRREAD,P1          LOOK FOR EXACT KEY                           
         CLI   P3-P1(R1),0         NOT FOUND?                                   
         BNE   OCHK0520            ERROR                                        
         MVC   RBUYCODE,22(R2)     YES - SET CARD OUTPUT                        
         MVI   RBUYCOD,C'Y'        TURN ON 'USE BUYLINE READER'                 
         B     OCHK0900            EXIT CC ZERO                                 
OCHK0120 EQU   *                                                                
         CLC   =C'RB',12(R2)       USE BUYLINE READER?                          
         BNE   OCHK0140            NO                                           
         CLI   22(R2),C'Y'         YES - OPTION = Y?                            
         BNE   OCHK0600            NO  - ERROR                                  
         MVI   RBUYCOD,C'Y'        YES - TURN ON 'USE BUYLINE READER'           
         B     OCHK0900            EXIT CC ZERO                                 
OCHK0140 EQU   *                                                                
         CLC   =C'CP',12(R2)       COMP SP CODE FILTER?                         
         BNE   OCHK0160            NO                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'                                                        
         MVC   KEY+22(2),TWAAGY                                                 
         MVC   KEY+24(3),22(R2)    INSERT THREE CHARS FROM CODE                 
                                                                                
         GOTO1 DIRREAD,P1          LOOK FOR EXACT KEY                           
         CLI   P3-P1(R1),0         NOT FOUND?                                   
         BNE   OCHK0620            ERROR                                        
         MVC   RCOMPSP,22(R2)      YES - SET CARD OUTPUT                        
         B     OCHK0900            EXIT CC ZERO                                 
OCHK0160 EQU   *                                                                
         B     OCHK0560            ERROR - UNRECOGNIZED OPTION                  
OCHK0400 EQU   *                   ERROR: COMBO COMBINE                         
         LA    RE,COMBINER                                                      
         LA    RF,L'COMBINER                                                    
         B     OCHK0880            EXIT CC NOT ZERO                             
OCHK0440 EQU   *                   ERROR: A1 ATTRIBUTE NOT FOUND                
         LA    RE,A1ERROR                                                       
         LA    RF,L'A1ERROR                                                     
         B     OCHK0880            EXIT CC NOT ZERO                             
OCHK0480 EQU   *                   ERROR: A2 ATTRIBUTE NOT FOUND                
         LA    RE,A2ERROR                                                       
         LA    RF,L'A2ERROR                                                     
         B     OCHK0880            EXIT CC NOT ZERO                             
OCHK0520 EQU   *                   ERROR: BUYLINE CODE NOT FOUND                
         LA    RE,BUYCODER                                                      
         LA    RF,L'BUYCODER                                                    
         B     OCHK0880            EXIT CC NOT ZERO                             
OCHK0560 EQU   *                   ERROR: UNRECOGNIZED KEYWORD                  
         LA    RE,UNKNOWN                                                       
         LA    RF,L'UNKNOWN                                                     
         B     OCHK0880            EXIT CC NOT ZERO                             
OCHK0600 EQU   *                   ERROR: BAD READER REQUEST                    
         LA    RE,BADREDER                                                      
         LA    RF,L'BADREDER                                                    
         B     OCHK0880            EXIT CC NOT ZERO                             
OCHK0620 EQU   *                   ERROR: BAD READER REQUEST                    
         LA    RE,SPNOFIND                                                      
         LA    RF,L'SPNOFIND                                                    
         B     OCHK0880            EXIT CC NOT ZERO                             
OCHK0880 EQU   *                   ERROR: UNRECOGNIZED KEYWORD                  
         ST    RE,AERROR                                                        
         ST    RF,AERROR+4                                                      
         LA    R3,RQSOPT2H                                                      
         ST    R3,CURSPOS                                                       
         LTR   RB,RB               SET CC NOT ZERO                              
         B     OCHK0920                                                         
OCHK0900 EQU   *                   ERROR: BUYLINE CODE NOT FOUND                
         SR    R0,R0               SET CC ZERO                                  
OCHK0920 EQU   *                   ERROR: BUYLINE CODE NOT FOUND                
         XIT1                                                                   
COMBINER DC    C'COMBO COMBINE ERROR NOT "Y"'                                   
A1ERROR  DC    C'ATTRIBUTE 1 CODE IN ERROR'                                     
A2ERROR  DC    C'ATTRIBUTE 2 CODE IN ERROR'                                     
BUYCODER DC    C'BUYLINE CODE IN ERROR'                                         
UNKNOWN  DC    C'UNKNOWN KEYWORD FOUND'                                         
BADREDER DC    C'BUYLINE READER ERROR NOT "Y"'                                  
SPNOFIND DC    C'SALESPERSON NOT ON FILE'                                       
         LTORG                                                                  
         EJECT                                                                  
*- XFILNAM:  SCAN OPTION FIELD FOR 'F='.  IF FOUND, VALIDATE                    
*        EXISTENCE OF XFIL NAME, SET NAME IN CARD3                              
*                                                                               
XFILNAM  NMOD1 0,*XFIL*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,RQSOPT           SET A(OPTIONS FIELD)                         
         LA    RF,12                                                            
XFIL0020 EQU   *                                                                
         CLC   =C'F=',0(R2)        LOOK FOR OPTION                              
         BE    XFIL0040            FOUND                                        
         LA    R2,1(R2)            NOT FOUND - MOVE OVER 1                      
         BCT   RF,XFIL0020                                                      
         B     XFIL0100            NOT FOUND AT ALL - EXIT                      
XFIL0040 EQU   *                                                                
         CLC   =C'SOON',RQSWHEN    TEST IF 'SOON'                               
         BE    XFIL0160            XFIL NOT ALLOWED FOR SOONS                   
         LA    R2,2(R2)            SKIP 'F=', A(CODE)                           
         LA    R3,XFILCODE                                                      
XFIL0060 EQU   *                                                                
         CLI   0(R3),0             END OF TABLE REACHED?                        
         BE    XFIL0120            YES - CODE NOT FOUND                         
         CLC   0(4,R3),0(R2)       CARD CODE FOUND IN TABLE?                    
         BE    XFIL0080            YES - CODE FOUND IN TABLE                    
         LA    R3,LXFILSET(R3)     NO  - BUMP TO NEXT ENTRY                     
         B     XFIL0060            GO BACK FOR NEXT                             
XFIL0080 EQU   *                                                                
         CLC   TWAAGY,4(R3)        SIGNON = TABLE ENTRY?                        
         BNE   XFIL0140            NO  - NOT SAME REP                           
*                                     ACCESS DENIED                             
         MVC   RXFILNM(4),0(R2)    YES - INSERT CODE INTO CARD                  
XFIL0100 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         B     XFIL0200                                                         
XFIL0120 EQU   *                                                                
         MVI   HALF2,1             SET FLAG TO 'NO CODE'                        
         B     XFIL0180                                                         
XFIL0140 EQU   *                                                                
         MVI   HALF2,2             SET FLAG TO 'NO ACCESS'                      
         B     XFIL0180                                                         
XFIL0160 EQU   *                                                                
         MVI   HALF2,3             SET FLAG TO 'NO SOONS'                       
         B     XFIL0180                                                         
XFIL0180 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
XFIL0200 EQU   *                                                                
         XIT1                                                                   
*                                                                               
       ++INCLUDE REGENCROSS                                                     
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*- UNCFSCAN:  SCAN OPTION FIELD FOR 'UNCF' OR 'CONF'.  IF FOUND, SET            
*        RCONUNC IN SECOND REQUEST CARD.                                        
*                                                                               
UNCFSCAN NMOD1 0,*UNCF*                                                         
         L     R9,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,RQSOPT           SET A(OPTIONS FIELD)                         
         LA    RF,12                                                            
UNCF0020 EQU   *                                                                
         CLC   =C'UNCF',0(R2)      LOOK FOR OPTION UNCONFIRMED                  
         BE    UNCF0060            FOUND                                        
         CLC   =C'UC',0(R2)        LOOK FOR OPTION UNCONFIRMED                  
         BE    UNCF0060            FOUND                                        
         CLC   =C'CONF',0(R2)      LOOK FOR OPTION CONFIRMED                    
         BE    UNCF0070            FOUND                                        
         CLC   =C'CF',0(R2)        LOOK FOR OPTION CONFIRMED                    
         BE    UNCF0070            FOUND                                        
         LA    R2,1(R2)            NOT FOUND - MOVE OVER 1                      
         BCT   RF,UNCF0020                                                      
         B     UNCF0080            NOT FOUND AT ALL - EXIT                      
UNCF0060 EQU   *                                                                
         MVI   RCONUNC,C'U'        SET OPTION TO 'U'                            
         B     UNCF0080                                                         
UNCF0070 EQU   *                                                                
         MVI   RCONUNC,C'C'        SET OPTION TO 'C'                            
UNCF0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*- ALTCALEN:  SCAN OPTION FIELD FOR 'AC'. IF FOUND, SET                         
*        RALTCAL IN THIRD REQUEST CARD.                                         
*                                                                               
ALTCALEN NMOD1 0,*ACAL*                                                         
         L     R9,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,RQSOPT           SET A(OPTIONS FIELD)                         
         LA    RF,12                                                            
         MVI   RALTCAL,C' '        CLEAR ALTERNATE CALENDAR FIELD               
ACAL0020 EQU   *                                                                
         CLC   =C'AC',0(R2)        LOOK FOR OPTION ALTERNATE CALENDAR           
         BE    ACAL0060            FOUND                                        
         LA    R2,1(R2)            NOT FOUND - MOVE OVER 1                      
         BCT   RF,ACAL0020                                                      
         B     ACAL0080            NOT FOUND AT ALL - EXIT                      
ACAL0060 EQU   *                                                                
         MVI   RALTCAL,C'Y'        SET OPTION TO 'Y'                            
ACAL0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*- TRADE: SCAN OPTION FIELD FOR 'TR'. IF FOUND, SET                             
*        RALTCAL IN THIRD REQUEST CARD.                                         
*                                                                               
TRADE    NMOD1 0,*TRAD*                                                         
         L     R9,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,RQSOPT           SET A(OPTIONS FIELD)                         
         LA    RF,12                                                            
         MVI   RTRADE,C' '         CLEAR TRADE FIELD                            
TRAD0020 EQU   *                                                                
         CLC   =C'TR',0(R2)        LOOK FOR OPTION 'TRADE'                      
         BE    TRAD0060            FOUND                                        
         LA    R2,1(R2)            NOT FOUND - MOVE OVER 1                      
         BCT   RF,TRAD0020                                                      
         B     TRAD0080            NOT FOUND AT ALL - EXIT                      
TRAD0060 EQU   *                                                                
         MVI   RTRADE,C'Y'         SET OPTION TO 'Y'                            
TRAD0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*- ALLCOLS:  SCAN OPTION FIELD FOR 'ALL'. IF FOUND, SET                         
*        RDISALL IN THIRD REQUEST CARD.                                         
*                                                                               
ALLCOLS  NMOD1 0,*ALCL*                                                         
         L     R9,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,RQSOPT           SET A(OPTIONS FIELD)                         
         LA    RF,12                                                            
         MVI   RDISALL,C' '        CLEAR ALL COLUMNS FIELD                      
ALCL0020 EQU   *                                                                
         CLC   =C'ALL',0(R2)       LOOK FOR OPTION ALL COLUMNS                  
         BE    ALCL0060            FOUND                                        
         LA    R2,1(R2)            NOT FOUND - MOVE OVER 1                      
         BCT   RF,ALCL0020                                                      
         B     ALCL0080            NOT FOUND AT ALL - EXIT                      
ALCL0060 EQU   *                                                                
         MVI   RDISALL,C'Y'        SET OPTION TO 'Y'                            
ALCL0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*- COMPBUD:  SCAN OPTION FIELD FOR 'CB'. IF FOUND, SET                          
*        RCOMBUD IN THIRD REQUEST CARD.                                         
*                                                                               
COMPBUD  NMOD1 0,*CMBD*                                                         
         L     R9,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,RQSOPT           SET A(OPTIONS FIELD)                         
         LA    RF,12                                                            
         MVI   RCOMBUD,C' '        CLEAR COMPANY BUDGET FIELD                   
CMBD0020 EQU   *                                                                
         CLC   =C'CB',0(R2)        LOOK FOR OPTION COMPANY BUDGET               
         BE    CMBD0060            FOUND                                        
         LA    R2,1(R2)            NOT FOUND - MOVE OVER 1                      
         BCT   RF,CMBD0020                                                      
         B     CMBD0080            NOT FOUND AT ALL - EXIT                      
CMBD0060 EQU   *                                                                
         MVI   RCOMBUD,C'Y'        SET OPTION TO 'Y'                            
CMBD0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*- SALBUD:   SCAN OPTION FIELD FOR 'SP'. IF FOUND, SET                          
*        RSALBUD IN THIRD REQUEST CARD.                                         
*                                                                               
SALBUD   NMOD1 0,*STBD*                                                         
         L     R9,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,RQSOPT           SET A(OPTIONS FIELD)                         
         LA    RF,12                                                            
         MVI   RSALBUD,C' '        CLEAR S/P     BUDGET FIELD                   
STBD0020 EQU   *                                                                
         CLC   =C'SP',0(R2)        LOOK FOR OPTION S/P     BUDGET               
         BE    STBD0060            FOUND                                        
         LA    R2,1(R2)            NOT FOUND - MOVE OVER 1                      
         BCT   RF,STBD0020                                                      
         B     STBD0080            NOT FOUND AT ALL - EXIT                      
STBD0060 EQU   *                                                                
         MVI   RSALBUD,C'Y'        SET OPTION TO 'Y'                            
STBD0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*- NATLOC:  SCAN OPTION FIELD FOR '$N/$L'  IF FOUND, SET                        
*        RNATLOC IN THIRD REQUEST CARD.                                         
*                                                                               
NATLOC   NMOD1 0,*NALO*                                                         
         L     R9,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,RQSOPT           SET A(OPTIONS FIELD)                         
         LA    RF,12                                                            
         MVI   RNATLOC,C' '        CLEAR ALL COLUMNS FIELD                      
NALO0020 EQU   *                                                                
         CLC   =C'$N',0(R2)        LOOK FOR OPTION NATIONAL BILLING             
         BE    NALO0060            FOUND                                        
         CLC   =C'$L',0(R2)        LOOK FOR OPTION LOCAL    BILLING             
         BE    NALO0070            FOUND                                        
         LA    R2,1(R2)            NOT FOUND - MOVE OVER 1                      
         BCT   RF,NALO0020                                                      
         B     NALO0080            NOT FOUND AT ALL - EXIT                      
NALO0060 EQU   *                                                                
         MVI   RNATLOC,C'N'        SET OPTION TO 'NATIONAL'                     
         B     NALO0080            EXIT                                         
NALO0070 EQU   *                                                                
         MVI   RNATLOC,C'L'        SET OPTION TO 'LOCAL'                        
         B     NALO0080            EXIT                                         
NALO0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*- RERFLG:  SCAN OPTION FIELD FOR 'R+/R-'  IF FOUND, SET                        
*        RRERFLG IN THIRD REQUEST CARD.                                         
*                                                                               
RERFLG   NMOD1 0,*RERF*                                                         
         L     R9,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,RQSOPT           SET A(OPTIONS FIELD)                         
         LA    RF,12                                                            
         MVI   RRERFLG,C' '        CLEAR ALL COLUMNS FIELD                      
RERF0020 EQU   *                                                                
         CLC   =C'R+',0(R2)        LOOK FOR OPTION RER ONLY                     
         BE    RERF0060            FOUND                                        
         CLC   =C'R-',0(R2)        LOOK FOR OPTION EXCLUDE RER                  
         BE    RERF0070            FOUND                                        
         LA    R2,1(R2)            NOT FOUND - MOVE OVER 1                      
         BCT   RF,RERF0020                                                      
         B     RERF0080            NOT FOUND AT ALL - EXIT                      
RERF0060 EQU   *                                                                
         MVI   RRERFLG,C'+'        SET OPTION TO 'RER ONLY'                     
         B     RERF0080            EXIT                                         
RERF0070 EQU   *                                                                
         MVI   RRERFLG,C'-'        SET OPTION TO 'EXCLUDE RER'                  
         B     RERF0080            EXIT                                         
RERF0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*- FORCST:  SCAN OPTION FIELD FOR 'FC'  IF FOUND, SET                           
*        RFORCST IN THIRD REQUEST CARD.                                         
*                                                                               
FORCST   NMOD1 0,*FCST*                                                         
         L     R9,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,RQSOPT           SET A(OPTIONS FIELD)                         
         LA    RF,12                                                            
         MVI   RFORCST,C' '        CLEAR ALL COLUMNS FIELD                      
FCST0020 EQU   *                                                                
         CLC   =C'FC',0(R2)        LOOK FOR OPTION FC ONLY                      
         BE    FCST0060            FOUND                                        
         LA    R2,1(R2)            NOT FOUND - MOVE OVER 1                      
         BCT   RF,FCST0020                                                      
         B     FCST0080            NOT FOUND AT ALL - EXIT                      
FCST0060 EQU   *                                                                
         MVI   RFORCST,C'Y'        SET OPTION TO 'FC'                           
FCST0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*- BAKBILL:  SCAN OPTION FIELD FOR 'BB'. IF FOUND, SET                          
*        RNOBAKB IN THIRD REQUEST CARD.                                         
*                                                                               
BAKBILL  NMOD1 0,*BKBL*                                                         
         L     R9,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,RQSOPT           SET A(OPTIONS FIELD)                         
         LA    RF,12                                                            
         MVI   RNOBAKB,C' '        CLEAR ALL COLUMNS FIELD                      
BKBL0020 EQU   *                                                                
         CLC   =C'B+',0(R2)        LOOK FOR OPTION ONLY BACK BILLING            
         BE    BKBL0060            FOUND                                        
         CLC   =C'B-',0(R2)        LOOK FOR OPTION *NO* BACK BILLING            
         BE    BKBL0080            FOUND                                        
         CLC   =C'BA',0(R2)        LOOK FOR OPTION ALL       BILLING            
         BE    BKBL0090            FOUND                                        
         LA    R2,1(R2)            NOT FOUND - MOVE OVER 1                      
         BCT   RF,BKBL0020                                                      
         B     BKBL0100            NOT FOUND AT ALL - EXIT                      
BKBL0060 EQU   *                                                                
         MVI   RNOBAKB,C'+'        SET OPTION TO '+' (ONLY BB)                  
         B     BKBL0100                                                         
BKBL0080 EQU   *                                                                
         MVI   RNOBAKB,C'-'        SET OPTION TO '-' (*NO* BB)                  
         B     BKBL0100                                                         
BKBL0090 EQU   *                                                                
         MVI   RNOBAKB,C'A'        SET OPTION TO 'A' (ALL BLG)                  
         B     BKBL0100                                                         
BKBL0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*- DIRRESP :  SCAN OPTION FIELD FOR 'DR'. IF FOUND, SET                         
*        RDIRRES IN THIRD REQUEST CARD.                                         
*                                                                               
DIRRESP  NMOD1 0,*DIRR*                                                         
         L     R9,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,RQSOPT           SET A(OPTIONS FIELD)                         
         LA    RF,12                                                            
         MVI   RDIRRES,C' '        CLEAR DIRECT RESPONSE FIELD                  
DIRR0020 EQU   *                                                                
         CLC   =C'DR',0(R2)        LOOK FOR OPTION DIRECT RESPONSE              
         BE    DIRR0060            FOUND                                        
         LA    R2,1(R2)            NOT FOUND - MOVE OVER 1                      
         BCT   RF,DIRR0020                                                      
         B     DIRR0080            NOT FOUND AT ALL - EXIT                      
DIRR0060 EQU   *                                                                
         MVI   RDIRRES,C'Y'        SET OPTION TO 'Y'                            
DIRR0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*- PAYPROG :  SCAN OPTION FIELD FOR 'PP'. IF FOUND, SET                         
*        RPAYPRO IN THIRD REQUEST CARD.                                         
*                                                                               
PAYPROG  NMOD1 0,*PPAY*                                                         
         L     R9,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,RQSOPT           SET A(OPTIONS FIELD)                         
         LA    RF,12                                                            
         MVI   RPAYPRO,C' '        CLEAR PAID PROGRAMMING FIELD                 
PPAY0020 EQU   *                                                                
         CLC   =C'PP',0(R2)        LOOK FOR OPTION PAID PROGRAMMING             
         BE    PPAY0060            FOUND                                        
         LA    R2,1(R2)            NOT FOUND - MOVE OVER 1                      
         BCT   RF,PPAY0020                                                      
         B     PPAY0080            NOT FOUND AT ALL - EXIT                      
PPAY0060 EQU   *                                                                
         MVI   RPAYPRO,C'Y'        SET OPTION TO 'Y'                            
PPAY0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*- LOSTSTA :  SCAN OPTION FIELD FOR 'P1/P2'. IF FOUND, SET                      
*        RJNLEFT IN THIRD REQUEST CARD.                                         
*                                                                               
LOSTSTA  NMOD1 0,*LSTA*                                                         
         L     R9,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,RQSOPT           SET A(OPTIONS FIELD)                         
         LA    RF,12                                                            
         MVI   RJNLEFT,C' '        CLEAR JOIN/LEFT REPORT FIELD                 
LSTA0020 EQU   *                                                                
         CLC   =C'R1',0(R2)        LOOK FOR 'GROSS BILLING' REPORT              
         BE    LSTA0060            FOUND                                        
         CLC   =C'R2',0(R2)        LOOK FOR 'EQUAL BAR' REPORT                  
         BE    LSTA0080            FOUND                                        
         CLC   =C'R3',0(R2)        LOOK FOR 'KATZ COMP' REPORT                  
         BE    LSTA0100            FOUND                                        
         CLC   =C'R5',0(R2)        LOOK FOR 'JOIN/LEAV' REPORT                  
         BE    LSTA0120            FOUND                                        
         CLC   =C'R6',0(R2)        LOOK FOR 'TOM OLSON' REPORT                  
         BE    LSTA0140            FOUND                                        
         LA    R2,1(R2)            NOT FOUND - MOVE OVER 1                      
         BCT   RF,LSTA0020                                                      
         B     LSTA0200            NOT FOUND AT ALL -                           
LSTA0060 EQU   *                                                                
         MVI   RJNLEFT,C'1'        SET OPTION TO 'GROSS BILLING'                
         B     LSTA0200                                                         
LSTA0080 EQU   *                                                                
         MVI   RJNLEFT,C'2'        SET OPTION TO 'EQUAL BAR'                    
         B     LSTA0200                                                         
LSTA0100 EQU   *                                                                
         MVI   RJNLEFT,C'3'        SET OPTION TO 'KATZ COMP'                    
         B     LSTA0200                                                         
LSTA0120 EQU   *                                                                
         MVI   RJNLEFT,C'5'        SET OPTION TO 'JOIN/LEAV'                    
         B     LSTA0200                                                         
LSTA0140 EQU   *                                                                
         MVI   RJNLEFT,C'6'        SET OPTION TO 'TOM OLSON'                    
         B     LSTA0200                                                         
LSTA0200 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*- DOWNHEAD :  SCAN OPTION FIELD FOR 'HD'. IF FOUND, SET                        
*        RDNHEAD IN THIRD REQUEST CARD.                                         
*                                                                               
DOWNHEAD NMOD1 0,*DHED*                                                         
         L     R9,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,RQSOPT           SET A(OPTIONS FIELD)                         
         LA    RF,12                                                            
         MVI   RDNHEAD,C' '        CLEAR DOWNLOAD HEADINGS FIELD                
DHED0020 EQU   *                                                                
         CLC   =C'HD',0(R2)        LOOK FOR DOWNLOAD HEADINGS OPTION            
         BE    DHED0060            FOUND                                        
         LA    R2,1(R2)            NOT FOUND - MOVE OVER 1                      
         BCT   RF,DHED0020                                                      
         B     DHED0200            NOT FOUND AT ALL -                           
DHED0060 EQU   *                                                                
         MVI   RDNHEAD,C'Y'        SET OPTION TO 'DOWNLOAD HEADINGS'            
DHED0200 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         TITLE 'REREQ02 -- SEED DEFAULTS INTO NEW REQUEST'                      
*                                                                               
*- NEWREQ -- PREPARE REQUEST RECORD(S) FOR NEW ADD.                             
*                                                                               
*  SEED DEFAULTS                                                                
*  INITIALIZE EXTENDED REQUEST                                                  
*                                                                               
NEWREQ   NMOD1 0,*NWRQ*,RR=R5                                                   
         L     R9,0(R1)                                                         
         USING REQWRK,R9                                                        
         ZERO  RQHHDR,L'RQHHDR             CLEAR RQST REC HEADER                
         ZERO  REQCARD,LREQCARD,BYTE=C' '  BLANK-FILL ALL CARDS                 
*                                                                               
*- SEED DEFAULTS IN PRIMARY AND/OR EXTENDED CARDS                               
*                                                                               
*  DEFAULT LIST LAYOUT:  (END WITH XL2'00')                                     
*        XL2'REQCARD DISPLACEMENT'                                              
*        XL1'DATA LENGTH'                                                       
*        X'VARIABLE LENGTH DATA'                                                
*                                                                               
         LA    RE,DEFAULTS                                                      
NREQ0010 EQU   *                                                                
         CLC   =XL2'00',0(RE)      END OF LIST?                                 
         BE    NREQ0080                                                         
         LA    R1,REQCARD          DISPLACEMENT STARTS AT CARD                  
*                                                                               
NREQ0020 SR    R0,R0                                                            
         ICM   R0,3,0(RE)          PICK UP 2 BYTE DISPLACEMENT                  
         AR    R1,R0               R1=A(FIELD)                                  
*                                                                               
         CLI   2(RE),1             FIELD LENGTH = 1 POS?                        
         BE    NREQ0060            YES                                          
         CLC   3(3,RE),=C'PRO'     NO  - USE PROFILE?                           
         BNE   NREQ0060            NO                                           
         LA    R3,PROFDATA+2       YES - SET A(PROFILES)                        
         ZIC   RF,6(RE)            GET BYTE DISPLACEMENT                        
         AR    RF,R3               ADD BYTE DISPL TO A(PROFILES)                
         ZIC   R3,0(RF)            GET BYTE FROM PROFILE                        
         ZIC   RF,7(RE)            GET PROFILE BIT W/IN BYTE                    
         LTR   RF,RF               ANY DISPLACEMENT?                            
         BZ    NREQ0040            NO  - USE AS IS                              
NREQ0030 EQU   *                                                                
         SLL   R3,1                SHIFT BIT ONE LEFT                           
         BCT   RF,NREQ0030         GO BACK FOR NEXT                             
NREQ0040 EQU   *                   BIT NOW LEFT-ALIGNED IN BYTE                 
         STC   R3,0(R1)            STORE ALIGNED BYTE IN CARD FIELD             
         ZIC   RF,2(RE)            RESET LENGTH                                 
         BCTR  RF,0                DECREMENT TO MATCH BUMP CODE                 
         TM    0(R1),X'80'         IS BIT ON?                                   
         BNO   NREQ0050            NO                                           
         MVC   0(1,R1),8(RE)       YES - SET 'ON' VALUE                         
         B     NREQ0070            GO TO BUMP CODE                              
NREQ0050 EQU   *                                                                
         MVC   0(1,R1),9(RE)       NO  - SET 'OFF' VALUE                        
         B     NREQ0070            GO TO BUMP CODE                              
NREQ0060 EQU   *                                                                
         ZIC   RF,2(RE)            DATA LENGTH                                  
         BCTR  RF,0                LESS 1 FOR THE EX                            
         EX    RF,DFLTMOVE                                                      
*                                                                               
NREQ0070 EQU   *                                                                
         LA    RE,3+1(RF,RE)       NEXT DEFAULT LIST ENTRY                      
         B     NREQ0010                                                         
*                                                                               
NREQ0080 EQU   *                                                                
         L     RE,AREQNTRY         A(REQUEST ENTRY)                             
         TM    RQCNTL2(RE),RQ2RRG  RRG REQUEST?                                 
         BNO   NREQ0090            NO                                           
         MVI   RCONTIN,C'+'        SET CONTINUATION CHARACTER                   
         MVI   RRRGFLAG,C'Y'       SET RRG RUN FLAG                             
*                                                                               
NREQ0090 EQU   *                                                                
         XIT1                                                                   
         SPACE                                                                  
DFLTMOVE MVC   0(0,R1),3(RE)       DFLT DATA FROM LIST TO CARD/REC              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*- VALFRI:  IF 'FRIDAY' TEST FLAG IS SET, DATE OF REQUEST MUST BE               
*    CHECKED.  IF DAY OF WEEK IS NOT FRIDAY OR SATURDAY, REQUEST                
*    REQUIRES EITHER A STATION OR AN OFFICE FILTER.                             
*    FOR 1G OFFICE BUDGET REPORT, IF DAY IS NOT FRIDAY, REQUEST                 
*      IS NOT ACCEPTED.                                                         
*                                                                               
VALFRI   NMOD1 0,*FRI*,RR=R5                                                    
         L     R9,0(R1)                                                         
         USING REQWRK,R9                                                        
         GOTO1 =V(GETDAY),P1,TODAY,WORK,RR=YES                                  
         CLC   WORK(3),=C'   '     ERROR RETURN?                                
         BNE   *+6                 NO  - CHECK IT OUT                           
         DC    H'0'                VALUE IN 'TODAY' NOT GOOD?                   
         CLC   =C'BILUHR',RQSNAME  **TEST** TREAT ANY DAY AS                    
         BE    VALF0030            **TEST**    FRIDAY                           
         CLI   P1,5                FRIDAY?                                      
         BE    VALF0030            YES - ACCEPT IT                              
*                                                                               
*   IF NOT FRIDAY, 1G REPORT IS REJECTED OUTRIGHT.  OTHER REPORT(S)             
*      ARE CHECKED FOR PRESENCE OF ADDITIONAL FILTERS.                          
*                                                                               
VALF0020 EQU   *                                                                
         CLC   RNUM,=C'1G'         NO  - OFFICE BUDGET REPORT?                  
         BE    VALF0040            YES - REJECT THE REQUEST                     
         CLC   ROFF(2),=C'  '      NO  - OFF OR STA FILTER NEEDED               
         BNE   VALF0030            ACCEPT IT                                    
         CLC   RSTA(5),=C'     '                                                
         BNE   VALF0030            ACCEPT IT                                    
         LA    RE,1                SET CC NOT ZERO                              
         B     VALF0040            EXIT                                         
VALF0030 EQU   *                                                                
         SR    RE,RE               SET CC ZERO                                  
VALF0040 EQU   *                                                                
         LTR   RE,RE                                                            
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         TITLE 'WORK AREA'                                                      
*                                                                               
*- REQTWA, REQWRK AND REGENREQ2                                                 
*        PRINT OFF                                                              
       ++INCLUDE REREQTWA                                                       
       ++INCLUDE REREQWRK                                                       
         PRINT ON                                                               
         SPACE                                                                  
*                                                                               
*- LOCAL WORK AREA                                                              
         ORG   USERWORK                                                         
LOCALWRK EQU   *        <----------INSERT VARIABLES BELOW THIS LABEL            
*                                                                               
AMAPNTRY DS    A                   A(CURRENT REQMAP ENTRY)                      
*                                                                               
SCANBLOK DS    0C                  SCANNER OUTPUT BLOCK                         
SCANBLK1 DS    CL32                (INCLUDE 1 32-BYTE ENTRY FOR EACH            
SCANBLK2 DS    CL32                 POSSIBLE PARAMETER)                         
SCANBLK3 DS    CL32                                                             
SCANBLK4 DS    CL32                                                             
SCANBLK5 DS    CL32                                                             
SCANBLK6 DS    CL32                                                             
SCANBLK7 DS    CL32                                                             
SCANBLK8 DS    CL32                                                             
LSCANBLK EQU   *-SCANBLOK                                                       
*                                                                               
SCANCARD DS    CL80                SCANNER INPUT CARD                           
*                                                                               
TEMP     DS    CL30                SPOOK WORK AREA                              
*                                                                               
TODAY    DS    CL6                 SYSTEM DATE.  YYMMDD                         
*                                                                               
DEMOVAL  DS    A                   DEMO VALIDATION ROUTINE                      
DBDEMOB  DS    288C                DEMO BLOCK AREA                              
DEMWORK  DS    CL30                DEMO OUTPUT AREA                             
*                                                                               
GETFACT  DS    A                   A(GETFACT ROUTINE)                           
VSWITCH  DS    F                   A(SWITCH ROUTINE)                            
SRCEREP  DS    CL2                                                              
TRGTREP  DS    CL3                                                              
SRCEUTL  DS    CL1                                                              
ORIGUTL  DS    CL1                                                              
SAVSTDAT DS    CL3                                                              
SRCESTAT DS    CL8                                                              
BEFFDATE DS    CL3                 EFFECTIVE DATE: BINARY                       
EFDTCOMP DS    CL2                 EFFECTIVE DATE: COMPRESSED                   
WORKREP  DS    CL64                                                             
SUBREPS  DS    CL36                18 SUB REP CODES                             
FLAG10   DS    CL1                                                              
SVTWAAGY DS    CL2                                                              
ASUBREP  DS    F                                                                
RTNFLAG  DS    CL1                 EXITING ROUTINE FOR 05 MODULE                
*                                                                               
LOCALWKX EQU   *        <----------INSERT VARIABLES ABOVE THIS LABEL            
AMTLOCAL EQU   USERWRKX-LOCALWKX  AMOUNT LOCAL WRK AREA                         
         DS    (AMTLOCAL)X                                                      
         SPACE 2                                                                
*                                                                               
*- RECORD DSECTS FOLLOW                                                         
*                                                                               
*  RECORD      ORG                                                              
*  --------    ------                                                           
*  REGENREG    IOWORK                                                           
*  REGENOFF    IOWORK                                                           
*  REGENSTA    IOWORK                                                           
*  REGENADV    IOWORK                                                           
*  REGENPRD    IOWORK                                                           
*  REGENAGY    IOWORK                                                           
*  REGENTEM    IOWORK                                                           
*  REGENSAL    IOWORK                                                           
*  REGENCTG    IOWORK                                                           
*  REGENCLS    IOWORK                                                           
*  REGENCON    IOWORK                                                           
*  REGENGRP    IOWORK                                                           
*  REGENOWN    IOWORK                                                           
*  REGENRDA    IOWORK                                                           
*  REGENREPA   IOWORK                                                           
*  REGENEOM    IOWORK                                                           
*                                                                               
         ORG   IOWORK                                                           
       ++INCLUDE REGENREG                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENOFF                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENSTA                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENADV                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENPRD                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENAGY                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENTEM                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENSAL                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENDSP                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENDCT                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENCTG                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENCLS                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENCON                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENGRP                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENOWN                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENRDA                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENREPA                                                      
         ORG   IOWORK                                                           
       ++INCLUDE REGENEOM                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENLAB                                                       
         ORG   IOWORK                                                           
       ++INCLUDE REGENSET                                                       
       ++INCLUDE DDSPOOK                                                        
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
*                                                                               
*  CONTROL BLOCK FOR DEMOGRAPHIC REQUIREMENTS                                   
*  DSECT TO COVER DEMO INTERFACE MODULE STORAGE                                 
DEMOD    DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         SPACE 5                                                                
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'140REREQ02S  09/23/03'                                      
         END                                                                    
