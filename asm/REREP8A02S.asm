*          DATA SET REREP8A02S AT LEVEL 021 AS OF 05/01/02                      
*PHASE RE8A02A,*                                                                
*INCLUDE CENTER                                                                 
*INCLUDE SCANNER                                                                
         TITLE 'SALES ACTIVITY REPORT - RE8A'                                   
***********************************************************************         
*          REREP8A02 - SALES ACTIVITY REPORT - RE8A                   *         
*                                                                     *         
*---------------------------------------------------------------------*         
* MOD LOG:                                                            *         
*  --------                                                           *         
*                                                                     *         
*  11/30/89  PJS  FIX OUTDATED TEST.  CURRENTLY FLAGS CONTRACTS       *         
*                 STARTING LESS THAN OR EQUAL TO OUTDATED DATE        *         
*                 AS OUTDATED.  CHANGED TO LESS THAN (ONLY).          *         
*                                                                     *         
* 25JAN91  (EFJ) --- UPDATED TO HANDLE BOP DEMOS VALIDATED BY DEMOVAL *         
*                     IN CONTRACT                                     *         
*                                                                     *         
* 19OCT92  (BU ) --- 'TRUE DATE' CHECKED BEFORE EPL ENTRY DATE.       *         
*                                                                     *         
* 15MAR94  (BU ) --- BOP ELEMENT REFERENCE CONTRACT # CHECK FOR COMBO *         
*                    ORDERS TO GET AROUND A RECNT4B BUG               *         
*                                                                     *         
*                    ***  END TOMBSTONE  ***                          *         
***********************************************************************         
*                                                                               
RE8A02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE8A                                                         
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     R9,FILEC                                                         
         USING FILED,R9                                                         
         LA    R8,2048(RB)         R8 IS SECOND BASE REGISTER                   
         LA    R8,2048(R8)                                                      
         USING RE8A02+4096,R8                                                   
         SPACE                                                                  
         SPACE                                                                  
ACT1     CLI   MODE,REQFRST                                                     
         BNE   ACT20                                                            
****                                                                            
* DEMOCON FIX                                                                   
         GOTOX LOADER,DMCB,=CL8'T00AE0',0                                       
         MVC   VDEMOCON,4(R1)                                                   
         OC    VDEMOCON,VDEMOCON                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
****                                                                            
         SPACE                                                                  
         RELOC                                                                  
         ST    RE,FACTOR           RELOCATION FACTOR                            
         SPACE                                                                  
         L     RF,=V(CENTER)                                                    
         AR    RF,RE                                                            
         ST    RF,CENTER                                                        
         L     RF,=V(SCANNER)                                                   
         AR    RF,RE                                                            
         ST    RF,SCANNER                                                       
         SPACE                                                                  
         LA    RF,SORTC            INITIALIZE SORTER                            
         ST    RF,ASORTC                                                        
         LR    R2,RF                                                            
         GOTO1 SORTER,DMCB,SORTFLD,RECTYPE,(40,(R2))                            
         MVI   SORTINIT,YES        SET FLAG FOR SORTER INITIALIZATION           
         SPACE                                                                  
ACT2     MVI   RCSUBPRG,0                                                       
         CLI   QOPTION1,W          HAS THE WORKSHEET OPTION BEEN                
         BNE   *+12                ELECTED-NO                                   
         MVI   RCSUBPRG,1          YES-SET SUB PROGRAM AND EXIT                 
         B     ACT12                                                            
         SPACE                                                                  
         SPACE                                                                  
         CLI   FIRSTSW,C'Y'                                                     
         BNE   ACT2L                                                            
         MVC   SVRCDATE,RCDATE     SAVE ORIGINAL RCDATE                         
         MVI   FIRSTSW,C'N'                                                     
         SPACE 1                                                                
ACT2L    MVC   RCDATE,SVRCDATE     USE ORIGINAL RCDATE                          
         CLC   QSTART,SPACES       FOR NO START DATE, USE TODAY TO              
         BNE   ACT3                GENERATE A MONDAY-SUNDAY WEEK.               
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,QSTART)                                
         B     ACT5                                                             
         SPACE                                                                  
ACT3     CLC   QEND,SPACES         GENERATE A MONDAY-SUNDAY WEEK IF             
         BE    ACT5                ONLY A START DATE IS GIVEN                   
         CLC   QSTART,QEND         BLOW UP IF REPORT PERIOD IS                  
         BNH   ACT10               NOT IN CHRONOLOGICAL ORDER.                  
         DC    H'0'                                                             
         SPACE 2                                                                
* GENERATE A MONDAY-SUNDAY WEEK USING TODAY OR QSTART                           
*                                                                               
ACT5     GOTO1 GETDAY,DMCB,QSTART,DMCB+8                                        
         CLC   DMCB+4(3),SPACES                                                 
         BNE   *+6                                                              
         DC    H'0'                INVALID DATE FORMAT                          
         SPACE                                                                  
         CLI   DMCB,MONDAY                                                      
         BE    ACT7                DATE IS A MONDAY                             
         ZIC   R2,DMCB             BACK UP DATE TO A                            
         BCTR  R2,0                MONDAY                                       
         LNR   R2,R2                                                            
         GOTO1 ADDAY,DMCB,QSTART,QSTART,(R2)                                    
         SPACE                                                                  
ACT7     GOTO1 ADDAY,DMCB,QSTART,QEND,6                                         
         B     ACT10               QEND IS NOW SUNDAY                           
         SPACE 2                                                                
* GENERATE OUTDATED DATE AND REPORT PERIOD                                      
*                                                                               
ACT10    GOTO1 ADDAY,DMCB,QEND,DUB,1                                            
         GOTO1 DATCON,(R1),(0,DUB),(3,OUTDATE)                                  
         MVC   RCDATE(2),DUB+2     RESTORE RCDATE MONTH                         
         MVI   RCDATE+2,C'/'       USING DUMMY RUN DATE IN DUB                  
         MVC   RCDATE+3(2),DUB+4   DAY                                          
         MVI   RCDATE+5,C'/'                                                    
         MVC   RCDATE+6(2),DUB     YEAR                                         
         SPACE                                                                  
         GOTO1 (RF),(R1),(0,QSTART),(3,START)                                   
         GOTO1 (RF),(R1),(0,QEND),(3,END)                                       
         GOTO1 (RF),(R1),(0,QSTART),(5,DTLN)                                    
         SPACE                                                                  
         MVI   DTLN+8,DASH                                                      
         GOTO1 (RF),(R1),(0,QEND),(5,DTLN+9)                                    
         B     ACT12                                                            
         SPACE                                                                  
ACT12    MVC   QEND,SPACES         DUMMY THE REPORT START DATE TO               
         MVC   QSTART,=C'840625'   AVOID EXCLUSION OF CONTRACTS.                
         B     ACTOUT              (THIS IS OLD BOP POINTER CODE)               
         EJECT                                                                  
ACT20    CLI   MODE,PROCCONT                                                    
         BNE   ACT50                                                            
         SPACE                                                                  
* DOES CONTRACT QUALIFY FOR REPORT                                              
*                                                                               
         CLI   RCONKSTA+4,SPACE    SKIP TV CONTRACTS                            
         BE    ACTOUT                                                           
         CLI   RCONTYPE,C'X'       OMIT TEST CONTRACTS                          
         BE    ACTOUT                                                           
         CLC   RCONDATE(3),=X'540619'   NO CONTRACTS THAT START                 
         BL    ACTOUT                   BEFORE 6/25/84                          
         TM    RCONCNTL,X'81'      NO DELETED OR COMPRESSED                     
         BNZ   ACTOUT              CONTRACTS                                    
         SPACE                                                                  
ACT22    LA    RE,RECORD           CLEAR AREA FOR BUILDING                      
         LR    R3,RE               SORT RECORD                                  
         LA    RF,L'RECORD                                                      
         XCEF                                                                   
         USING SORTD,R3                                                         
         SPACE 2                                                                
* FIND SUM OF CONTRACT BUCKETS, THEN LOOK FOR EPL.  IF NO EPL DECIDE            
* BETWEEN OUTDATED AND PENDING. IF EPL TEST THAT CONTRACT WAS RESOLVED          
* IN REPORT PERIOD AND WHETHER ITS A WIN OR LOSS                                
*                                                                               
ACT25    LA    R5,RCONREC          LOOK FOR BUCKET ELEMENTS                     
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   ACT30               NO BUCKET ELS                                
         SPACE                                                                  
ACT28    SR    R6,R6               CLEAR ACCUMULATOR                            
         USING RCONBKEL,R5                                                      
         MVC   FULL,RCONBKAM       BUCKET AMOUNT                                
         A     R6,FULL                                                          
         BAS   RE,NEXTEL           ANY MORE ELEMENTS                            
         BE    *-14                YES                                          
         STCM  R6,15,SORTORD       NO-STORE TOTAL                               
         B     ACT30                                                            
         DROP  R5                                                               
         SPACE                                                                  
ACT30    LA    R5,RCONREC          LOOK FOR EPL ELEMENT                         
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BE    ACT34               FOUND ELEMENT                                
         CLI   QOPTION1,W          ONLY CONTRACTS W/O EPL DATA                  
         BE    ACT36               APPEAR ON WORKSHEET                          
         SPACE                                                                  
ACT32    CLC   RCONDATE(3),OUTDATE CONTRACT START V. OUTDATED DATE              
*                                                                               
         BNL   ACT33               GREATER OR EQUAL = PENDING                   
*                                                                               
         OI    SORTOPP,OUTDATED    LESS THAN = OUTDATED                         
         B     ACT36                                                            
*                                                                               
ACT33    EQU   *                   PENDING                                      
         OI    SORTOPP,PENDING                                                  
         B     ACT36                                                            
         SPACE                                                                  
         USING RCONSPEL,R5                                                      
ACT34    EQU   *                                                                
         MVI   TRUDATE,C'N'        SET TRUE ACTIVITY DATE FLAG                  
         BAS   RE,CHECKX08         LOOK FOR TRUE ACTIVITY DATE                  
         CLI   TRUDATE,C'A'        ACCEPTED ON TRUE ACTIVITY DATE?              
         BE    ACT34A              YES - DON'T CHECK EPL ENTRY DATE             
         CLI   TRUDATE,C'O'        OUTSIDE REPORT PERIOD BASED ON               
         BE    ACTOUT                 TRUE ACTIVITY DATE?                       
         CLC   RCONSPDT,START      TEST THAT EPL INPUT DATE IS                  
         BL    ACTOUT                 WITHIN REPORT PERIOD                      
         CLC   RCONSPDT,END                                                     
         BH    ACTOUT                                                           
ACT34A   EQU   *                                                                
         CLC   RCONSPST+5(3),=C'CAN'   TEST FOR CONCELLED CONTRACT              
         BNE   *+12                                                             
         OI    SORTOPP,CANCEL                                                   
         B     ACT36                                                            
         CLI   QOPTION1,W          FOR WORKSHEET, OMIT CONTRACTS                
         BE    ACTOUT                 WITH EPL                                  
         OC    RCONSPAM,RCONSPAM   TEST THIS STATION'S EPL AMOUNT               
         BNZ   ACT35                                                            
         OC    SORTORD,SORTORD     EPL ZERO - TEST ORDERED DOLLARS              
         BZ    *+12                                                             
         OI    SORTOPP,WIN                    NOT ZERO - WIN                    
         B     ACT36                                                            
         OI    SORTOPP,LOSS                   ZERO - LOSS                       
         B     ACT36                                                            
         EJECT                                                                  
*                                                                               
ACT35    OI    SORTOPP,WIN         EPL NOT ZERO - WIN                           
         ICM   RE,15,RCONSPAM                                                   
         MH    RE,=H'100'                                                       
         STCM  RE,15,SORTORD                                                    
         DROP  R5                                                               
         B     ACT36                                                            
         EJECT                                                                  
*                                                                               
*  CHECKX08:  BEFORE EPL ENTRY DATE IS CHECKED TO DETERMINE IF                  
*     CONTRACT ACTIVITY WAS WITHIN REPORT PERIOD, LOOK FOR X'08'                
*     ELEMENT, AND THE TRUE ACTIVITY DATE THERE.                                
*                                                                               
CHECKX08 NTR1                                                                   
         LA    R5,RCONREC                                                       
         MVI   ELCODE,X'08'        LOOK FOR X'08'                               
         BAS   RE,GETEL                                                         
         BNE   CX080090            NOT FOUND                                    
*                                                                               
         USING RCONACEL,R5                                                      
*                                                                               
         OC    RCONACTA,RCONACTA   ANY TRUE ACTIVITY DATE?                      
         BZ    CX080090            NO DATE IN ELEMENT                           
         CLC   RCONACTA,START      TRUE ACTIVITY DATE WITHIN                    
         BL    CX080060               REPORT PERIOD?                            
         CLC   RCONACTA,END                                                     
         BH    CX080060                                                         
         MVI   TRUDATE,C'A'        SET TRUE DATE FLAG TO 'ACCEPTED'             
         B     CX080090            EXIT ROUTINE                                 
CX080060 EQU   *                                                                
         MVI   TRUDATE,C'O'        SET TRUE DATE FLAG TO 'OUTSIDE'              
*                                     REPORT PERIOD                             
CX080090 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
* SEARCH FOR THE BOP ELEMENT AND FILL SORT KEY                                  
*                                                                               
ACT36    LA    R5,RCONREC                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   ACTOUT              NOT FOUND-SKIP CONTRACT                      
         ST    R5,WORD             SAVE BOP ELEMENT ADDRESS                     
         SPACE                                                                  
         USING RCONBPEL,R5                                                      
         MVC   SORTREF,RCONBPRF    REFERENCE CONTRACT                           
         DROP  R5                                                               
         OC    SORTREF,SORTREF     ANY REFERENCE CONTRACT #?                    
         BNZ   ACT36B              YES                                          
         BAS   RE,CHKCOMBO         SPECIAL TEST: COMBO/REFCON#                  
ACT36B   EQU   *                                                                
         MVC   SORTCON,RCONKCON                                                 
         MVC   SORTOFF,ROFFNAME                                                 
         MVC   SORTMAN,RSALNAME                                                 
         MVC   SORTADV,RADVNAME                                                 
         MVC   SORTAGY,RAGYNAM1                                                 
         CLC   RCONPRD,SPACES      WAS PRODUCT CODE GIVEN                       
         BE    ACT37               NO                                           
         MVC   SORTPRD,RPRDNAME    YES-OBTAIN NAME FROM PRD RECORD              
         B     ACT40                                                            
         EJECT                                                                  
*                                                                               
* CHKCOMBO:  LOOK FOR 'COMBO' (X'17') ELEMENT.  IF PRESENT, AND                 
*    SORTREF = X'00000000', FORCE RCONKCON INTO FIELD                           
*                                                                               
CHKCOMBO NTR1                                                                   
         LA    R5,RCONREC                                                       
         MVI   ELCODE,X'17'        LOOK FOR COMBO CONTROL ELEMENT               
         BAS   RE,GETEL                                                         
         BNE   CCMB0090            NOT FOUND:  EXIT                             
         MVC   SORTREF,RCONKCON    FOUND:  FORCE CONTRACT # INTO                
*                                     REFERENCE CONTRACT NUMBER                 
CCMB0090 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
ACT37    LA    R5,RCONREC          FIND PRODUCT NAME INPUT FROM                 
         MVI   ELCODE,X'05'        CONTRACT EXPANSION ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   ACT40                                                            
         USING RCONEXEL,R5                                                      
         MVC   SORTPRD,RCONEXPR                                                 
         DROP  R5                                                               
         SPACE 2                                                                
* COMPLETE SORT RECORD AND PUT TO WORK FILE                                     
*                                                                               
ACT40    MVC   SORTCST,RCONDATE    CONTRACT START                               
         MVC   SORTSTA,RCONKSTA    CALL LETTERS                                 
         SPACE 2                                                                
ACT42    L     R5,WORD             RESTORE BOP ELEMENT ADDRESS                  
         USING RCONBPEL,R5                                                      
         MVC   SORTAWKS,RCONBAWK                                                
         MVC   SORTLEN,SPACES                                                   
         CLI   RCONBPCL+2,SPACE    MOVE 2 CHARACTERS OF LENGTH IF               
         BE    ACT43               THIRD IS A BLANK OR LESS THAN 'A'            
         CLI   RCONBPCL+2,C'A'     WHEN IT IS AN ASSUMED DELIMITER.             
         BL    ACT43                                                            
         MVC   SORTLEN,RCONBPCL                                                 
         B     ACT44                                                            
         SPACE                                                                  
ACT43    MVC   SORTLEN+1(2),RCONBPCL                                            
         B     ACT44                                                            
         SPACE                                                                  
ACT44    XC    WORK,WORK           EXTRACT FIRST DEMO                           
         XC    WORK2,WORK2                                                      
*                                                                               
         CLI   RCONBPDM,X'FF'      VALIDATED BY DEMOVAL?                        
         BNE   ACT44A                                                           
*                                                                               
* OPEN CTFILE FOR DEMOCON                                                       
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'REP',=C'NCTFILE X'                      
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         MVC   DBCOMFCS,VCOMFACS                                                
         DROP  RF                                                               
         MVI   DBSELMED,C'R'                                                    
         SPACE                                                                  
         XC    WORK(30),WORK                                                    
         MVC   WORK(L'RCONBPDM-1),RCONBPDM+1                                    
*                                                                               
         CLI   WORK+1,C'T'         FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   WORK+1,C'I'                                                      
*                                                                               
         GOTO1 VDEMOCON,DMCB,(1,WORK),(9,SORTDEMO),(0,DBLOCK)                   
         B     ACT44X                                                           
*                                                                               
ACT44A   LA    RE,L'WORK2                                                       
         STC   RE,WORK2                                                         
         LA    RF,RCONBPDM                                                      
         LA    R0,L'RCONBPDM                                                    
         BAS   RE,FINDLEN                                                       
         STC   R0,WORK2+5                                                       
         MVC   WORK2+8(L'RCONBPDM),RCONBPDM                                     
         GOTO1 SCANNER,DMCB,WORK2,(2,WORK)                                      
         CLI   DMCB+4,0                                                         
         BE    *+10                INVALID DATA                                 
         MVC   SORTDEMO,WORK+12    FIRST DEMO                                   
*                                                                               
ACT44X   OC    SORTDEMO,SPACES                                                  
         MVC   SORTOBJ,RCONBPOB    BUYING OBJECTIVE                             
         MVC   SORTBRK,RCONBPDT    LAST BOP ADD/CHA DATE IS BREAKING DT         
         LA    RE,SORTEND-SORTREC  COMPUTE RECORD LENGTH                        
         STH   RE,SORTRLEN                                                      
         DROP  R5                                                               
         SPACE                                                                  
ACT45    GOTO1 SORTER,DMCB,=C'PUT',(R3)                                         
         MVI   SORTINIT,NO         RECORDS HAVE BEEN PUT TO SORTER              
         B     ACTOUT                                                           
         EJECT                                                                  
ACT50    CLI   MODE,REQLAST                                                     
         BNE   ACTOUT                                                           
         SPACE                                                                  
         CLI   SORTINIT,YES        EXIT RIGHT AWAY IF NO RECORDS HAVE           
         BNE   ACT54               BEEN PUT TO SORTER                           
         GOTO1 SORTER,DMCB,=C'END'                                              
         B     ACTOUT                                                           
         SPACE                                                                  
ACT54    XC    LASTREF,LASTREF     CLEAR AREA FOR LAST REF CON NUM.             
         MVC   CONOFF,SPACES       CLEAR OFFICE BREAK CONTROL                   
         MVC   CONMAN,SPACES       CLEAR SALESMAN CONTROL                       
         SPACE                                                                  
ACT55    GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R3,DMCB+4                                                        
         LTR   R3,R3                                                            
         BP    ACT60                                                            
         CLC   PX,SPACES           TEST FOR SUSPENDED LINE AT EOF               
         BE    ACTOUT              NONE THERE-EXIT                              
         MVC   P,PX                                                             
         BAS   RE,PRTLINE          PRINT LINE BEFORE EXIT                       
         MVC   PX,SPACES                                                        
         B     ACTOUT                                                           
         SPACE 2                                                                
* OFFICE BREAK ROUTINE                                                          
*                                                                               
ACT60    EQU   *                                                                
         B     ACT61                                                            
*                                                                               
*   TEST                                                                        
QUIKDISP NTR1                                                                   
         MVC   P+1(SKEYLEN+4),SORTREC **TEST                                    
         GOTO1 REPORT                                                           
         MVC   P+1(22),=C'REF=     CON=     LST='                               
         MVC   P+5(4),SORTREF                                                   
         MVC   P+14(4),SORTCON                                                  
         MVC   P+23(4),LASTREF                                                  
         GOTO1 REPORT                                                           
         XIT1                                                                   
*   END TEST                                                                    
*                                                                               
ACT61    EQU   *                                                                
         CLC   CONOFF,SPACES                                                    
         BE    ACT62               FIRST TIME THROUGH                           
         CLC   CONOFF,SORTOFF      TEST FOR CHANGE IN OFFICE                    
         BE    ACT65               NONE                                         
         SPACE                                                                  
ACT62    CLC   PX,SPACES           IS THERE A LINE IN SUSPENSE                  
         BE    *+20                NO                                           
         MVC   P,PX                                                             
         BAS   RE,PRTLINE          YES-PRINT IT FIRST                           
         MVC   PX,SPACES           CLEAR SUSPENSE LINE                          
         MVC   OFFNAM,SPACES        PREPARE OFFICE FOR HEADLINES                
         LA    RF,OFFNAM                                                        
         LA    R0,L'OFFNAM                                                      
         MVC   OFFNAM(L'SORTOFF),SORTOFF                                        
         MVC   CONOFF,SORTOFF      UPDATE OFFICE CONTROL                        
         BAS   RE,FINDLEN          FIND NAME LENGTH                             
         LA    R1,OFFNAM            POINT R1 TO NAME START                      
         AR    R1,R0               ADD NAME LENGTH                              
         LA    R1,1(R1)            POINT PAST A SEPARATING SPACE                
         MVC   0(6,R1),=C'OFFICE'                                               
         GOTO1 CENTER,DMCB,OFFNAM,L'OFFNAM                                      
         MVC   PAGE,=H'1'          RESTART PAGE NUMBERING                       
         MVI   FORCEHED,YES                                                     
         SPACE 2                                                                
* SALESMAN BREAK ROUTINE                                                        
*                                                                               
ACT65    CLC   SORTMAN,CONMAN                                                   
         BE    ACT70               NO CHANGE                                    
         CLC   PX,SPACES           PRINT A LINE IN SUSPENSE                     
         BE    ACT68               BEFORE THE UPCOMING PAGE BREAK               
         MVC   P,PX                                                             
         BAS   RE,PRTLINE                                                       
         MVC   PX,SPACES                                                        
         SPACE                                                                  
ACT68    MVC   WORK2,SPACES                                                     
         MVC   WORK2(L'SORTMAN),SORTMAN                                         
         LA    RF,WORK2            A(FIELD)                                     
         LA    R0,L'SORTMAN+1      LENGTH OF FIELD                              
         BAS   RE,FINDLEN          FIND LENGTH OF NAME                          
         MVC   CONMAN,SORTMAN      UPDATE CONTROL                               
         STC   R0,CONMANL          SAVE LENGTH OF SALESMAN'S NAME               
         MVI   FORCEHED,YES                                                     
         SPACE 2                                                                
* HANDLE FORMATTING OF PRINT LINE(S)                                            
*                                                                               
ACT70    EQU   *                                                                
*****>   BAS   RE,QUIKDISP                                                      
         CLC   SORTREF,SORTCON     TEST FOR REFERENCE CONTRACT                  
         BE    ACT72               OR A SLAVE NOT PRECEDED BY ITS               
         CLC   SORTREF,LASTREF     MASTER WHCIH IS CONSIDERED TO BE             
         BE    ACT80               A REFERENCE CONTRACT.                        
         SPACE                                                                  
ACT72    CLC   PX,SPACES                                                        
         BE    ACT74                                                            
         MVC   P,PX                PRINT A SUSPENDED LINE FIRST                 
         BAS   RE,PRTLINE                                                       
         MVC   PX,SPACES           CLEAR SUSPENDED LINE                         
         SPACE                                                                  
ACT74    BAS   RE,SKIPLIN          SKIP A LINE BEFORE REFERENCE K.              
         MVC   LASTREF,SORTREF     SAVE REFERENCE NUMBER                        
         CLI   QOPTION1,W          GO TO SEPARATE FORMATTING LOGIC              
         BE    ACT76               FOR WORKSHEET                                
         SPACE                                                                  
         MVC   P+1(20),SORTADV                                                  
         MVC   PX+1(20),SORTAGY                                                 
         MVC   P+22(20),SORTPRD                                                 
         LA    R4,PX+22                                                         
         GOTO1 DATCON,DMCB,(3,SORTCST),(5,(R4))                                 
         LA    R4,PX+31                                                         
         EDIT  (B1,SORTAWKS),(2,(R4))                                           
         MVC   PX+33(3),=C'WKS'                                                 
         MVC   PX+37(3),SORTLEN                                                 
         MVC   P+43(9),SORTDEMO                                                 
         MVC   PX+43(20),SORTOBJ                                                
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(3,SORTBRK),(5,P+53)                                 
         B     ACT80                                                            
         SPACE 2                                                                
* FILL LEFT SIDE OF WORKSHEET LINE (REFERENCE CONTRACTS)                        
*                                                                               
ACT76    MVI   REFSW,YES           SET SWITCH TO STOP EXTRA SKIPD LINE          
         MVC   P+1(20),SORTADV                                                  
         MVC   PSECOND+1(20),SORTAGY                                            
         MVC   P+22(20),SORTPRD                                                 
         LA    R4,PSECOND+22                                                    
         GOTO1 DATCON,DMCB,(3,SORTCST),(5,(R4))                                 
         LA    R4,PSECOND+33                                                    
         EDIT  (B1,SORTAWKS),(2,(R4))                                           
         MVC   PSECOND+35(3),=C'WKS'                                            
         MVC   PSECOND+39(3),SORTLEN                                            
         MVC   P+45(9),SORTDEMO                                                 
         B     ACT80                                                            
         SPACE 2                                                                
* COMPLETE RIGHT SIDE OF SALES ACTIVITY PRINT LINE                              
*                                                                               
ACT80    CLI   QOPTION1,W                                                       
         BE    ACT90                                                            
         LA    R4,P                                                             
         CLC   PX,SPACES           FILL P UNLESS                                
         BE    *+18                THERE IS A BLANK PRINT LINE                  
         CLC   P,SPACES            AND DATE IN THE SUSPENSE LINE.               
         BNE   *+8                                                              
         LA    R4,PX                                                            
         SPACE                                                                  
         MVC   64(4,R4),SORTSTA    STATION FIELD                                
         MVI   68(R4),DASH                                                      
         MVC   69(1,R4),SORTSTA+4                                               
         MVI   70(R4),C'M'                                                      
         SPACE                                                                  
         CLI   67(R4),SPACE        IF CALL LETTERS ARE ONLY 3 DIGITS,           
         BNE   *+14                SHIFT THE DASH AND BAND TO THE               
         MVC   67(3,R4),68(R4)     LEFT.                                        
         MVI   70(R4),SPACE                                                     
         SPACE                                                                  
         MVO   DUB(5),SORTCON      CONTRACT NUMBER                              
         OI    DUB+4,X'0F'                                                      
         EDIT  (P5,DUB),(7,72(R4))                                              
         SPACE                                                                  
         TM    SORTOPP,CANCEL                                                   
         BZ    *+14                                                             
         MVC   88(9,R4),=C'CANCELLED'                                           
         B     ACT84               PRINT CANCELLED CONTRACT NOW.                
         SPACE                                                                  
         OC    SORTORD,SORTORD                                                  
         BZ    ACT82               NO DOLLARS                                   
         ICM   R6,15,SORTORD       ROUND WIN DOLLARS TO NEAREST                 
         SR    R7,R7               DOLLAR                                       
         SRDA  R6,31                                                            
         D     R6,=F'100'                                                       
         LTR   R7,R7                                                            
         BM    *+8                                                              
         AH    R7,=H'1'                                                         
         SRA   R7,1                                                             
         EDIT  (R7),(7,89(R4))                                                  
         SPACE                                                                  
ACT82    TM    SORTOPP,PENDING                                                  
         BNO   ACT82A                                                           
         OC    SORTORD,SORTORD     TEST FOR ANY DOLLARS                         
         BZ    *+12                NONE                                         
         MVI   109(R4),ASTER       FOR DOLLARS, PUT ASTERISK IN W/L             
         B     ACT84               COLUMN.                                      
         MVI   84(R4),ASTER                                                     
         B     ACT84                                                            
         SPACE                                                                  
ACT82A   TM    SORTOPP,OUTDATED                                                 
         BNO   ACT83                                                            
         OC    SORTORD,SORTORD     TEST FOR DOLLARS                             
         BZ    *+12                NONE                                         
         MVI   109(R4),ASTER       PUT ASTER IN W/L COLUMN IF DOLLARS           
         B     ACT84                                                            
         GOTO1 DATCON,DMCB,(3,SORTCST),(5,98(R4))                               
         B     ACT84                                                            
         SPACE                                                                  
ACT83    TM    SORTOPP,WIN                                                      
         BO    *+12                                                             
         MVI   109(R4),C'L'                                                     
         B     ACT84                                                            
         MVI   109(R4),C'W'                                                     
         SPACE                                                                  
ACT84    ZIC   RE,LINE                                                          
         ZIC   RF,MAXLINES                                                      
         LA    RE,1(RE)                                                         
         CLC   PX,SPACES           TEST TO SEE IF 1 MORE LINE                   
         BE    *+18                WILL FIT UNLESS P AND PX ARE                 
         CLC   P,SPACES            FILLED WHEN THE TEST IS                      
         BE    *+8                 3 MORE LINES                                 
         LA    RE,2(RE)                                                         
         CR    RE,RF               ON PAGE                                      
         BNH   *+8                 YES                                          
         MVI   FORCEHED,YES                                                     
         SPACE                                                                  
         CLC   P,SPACES                                                         
         BNE   ACT86                                                            
         MVC   P,PX                                                             
         MVC   PX,SPACES                                                        
         SPACE                                                                  
ACT86    BAS   RE,PRTLINE                                                       
         B     ACT100                                                           
         SPACE 2                                                                
* COMPLETE WORKSHEET PRINT LINE AND CONTROL ITS PRINTING                        
*                                                                               
ACT90    MVC   P+54(4),SORTSTA     STATION FIELD                                
         MVI   P+58,DASH                                                        
         MVC   P+59(1),SORTSTA+4                                                
         MVI   P+60,C'M'                                                        
         SPACE                                                                  
         CLI   P+57,SPACE          SHIFT CALL LETTERS AND BAND TO               
         BNE   *+14                LEFT IF CALL LETTERS ARE ONLY 3              
         MVC   P+57(3),P+58        DIGITS.                                      
         MVI   P+60,SPACE                                                       
         SPACE                                                                  
         MVO   DUB(5),SORTCON                                                   
         OI    DUB+4,X'0C'                                                      
         EDIT  (P5,DUB),(7,P+62)                                                
         SPACE                                                                  
         TM    SORTOPP,CANCEL      TEST FOR CANCELLED CONTRACT                  
         BZ    *+14                                                             
         MVC   P+70(9),=C'CANCELLED'                                            
         B     ACT95                                                            
         ICM   R6,15,SORTORD       ROUND ORDERED DOLLARS TO NEAREST             
         SR    R7,R7               DOLLAR                                       
         SRDA  R6,31                                                            
         D     R6,=F'100'                                                       
         LTR   R7,R7                                                            
         BM    *+8                                                              
         AH    R7,=H'1'                                                         
         SRA   R7,1                                                             
         EDIT  (R7),(7,P+71)                                                    
         SPACE                                                                  
ACT95    ZIC   RE,LINE                                                          
         LA    RE,2(RE)                                                         
         CLM   RE,1,MAXLINES                                                    
         BNH   *+8                                                              
         MVI   FORCEHED,YES                                                     
         SPACE                                                                  
         BAS   RE,PRTLINE                                                       
         CLI   REFSW,YES                                                        
         BE    *+8                                                              
         BAS   RE,SKIPLIN                                                       
         MVI   REFSW,NO                                                         
         B     ACT100                                                           
         SPACE 2                                                                
ACT100   B     ACT55               READ NEXT SORT RECORD                        
         SPACE 3                                                                
ACTOUT   XMOD1 1                                                                
         EJECT                                                                  
* SUPPORTING SUB-ROUTINES                                                       
*                                                                               
         GETEL R5,DATADISP,ELCODE                                               
         SPACE 2                                                                
* ROUTINE TO FIND LENGTH OF LEFT JUSTIFIED DATA                                 
*              RF -  A(FIELD)      R0 - L'FIELD                                 
*                                                                               
FINDLEN  AR    RF,R0                                                            
         BCTR  RF,0                                                             
         CLI   0(RF),SPACE                                                      
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10             RESULT IS LEFT IN R0                         
         BR    RE                                                               
         SPACE 2                                                                
* ROUTINE TO CONTROL HEADLINE AND PRINT LINE PRINTING                           
*                                                                               
PRTLINE  ST    RE,FULL                                                          
         CLI   FORCEHED,YES                                                     
         BNE   PRINTIT                                                          
         MVC   HEAD3+42(28),OFFNAM                                              
         CLI   QOPTION1,W          FOR WORKSHEET, PUT SALESMAN NAME             
         BNE   *+14                UP TOP.                                      
         MVC   HEAD3+88(20),CONMAN                                              
         B     PRINTIT                                                          
         SPACE                                                                  
         MVC   HEAD3+88(17),DTLN                                                
         MVC   HEAD11+1(20),CONMAN                                              
         ZIC   R1,CONMANL                                                       
         MVI   HEAD12+1,DASH                                                    
         SH    R1,=H'2'                                                         
         BM    PRINTIT             LENGTH WAS ONLY 1                            
         EX    R1,*+8              PROPAGATE DASHES                             
         B     PRINTIT                                                          
         MVC   HEAD12+2(0),HEAD12+1                                             
         SPACE                                                                  
PRINTIT  MVI   SPACING,1                                                        
         GOTO1 REPORT                                                           
         L     RE,FULL                                                          
         BR    RE                                                               
         SPACE 2                                                                
* ROUTINE TO CONTROL SKIPPING A LINE                                            
*                                                                               
SKIPLIN  CLI   FORCEHED,YES        DO NOT SKIP WHEN SWITCH IS SET               
         BER   RE                                                               
         ZIC   RF,LINE                                                          
         LA    RF,1(RF)                                                         
         CLM   RF,1,MAXLINES       TEST LINE COUNT VS. MAX                      
         BHR   RE                  DO NOT SKIP IF IT WOULD FORCE                
         MVI   SPACING,1           NEW PAGE                                     
         ST    RE,FULL             SAVE RETURN POINT                            
         GOTO1 REPORT                                                           
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* CONSTANTS                                                                     
*                                                                               
SORTFLD  DC    CL80'SORT FIELDS=(5,108,A),FORMAT=BI,WORK=1'                     
RECTYPE  DC    CL80'RECORD TYPE=V,LENGTH=1000'                                  
REFSW    DC    C'N'                                                             
SORTINIT DC    C'N'                                                             
PATCH    DC    XL30'0'                                                          
*                                                                               
* ADDRESSES                                                                     
*                                                                               
ASORTC   DS    A                                                                
VDEMOCON DS    A                   * NEW CODE FOR DEMOCON FIX                   
CENTER   DS    A                                                                
SCANNER  DS    A                                                                
FACTOR   DS    F                                                                
*                                                                               
* STORAGE                                                                       
*                                                                               
ELCODE   DS    C                                                                
OUTDATE  DS    CL3                                                              
START    DS    CL3                 REPORT START                                 
END      DS    CL3                 REPORT END                                   
DTLN     DS    CL17                                                             
WORK2    DS    CL28                                                             
LASTREF  DS    CL4                                                              
CONOFF   DS    CL20                                                             
OFFNAM   DS    CL28                                                             
CONMAN   DS    CL20                                                             
CONMANL  DS    X                                                                
PX       DC    CL132' '                                                         
SVRCDATE DS    CL8                 RUN DATE                                     
FIRSTSW  DC    C'Y'                FIRST TIME SWITCH                            
TRUDATE  DC    C'N'                TRUE ACTIVITY DATE FLAG                      
*                                  N  =  NOT PRESENT                            
*                                  O  =  OUTSIDE REPORT PERIOD                  
*                                  A  =  ACCEPTED FOR REPORT                    
*                                                                               
* DBLOCK                                                                        
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
*                                                                               
         DS    0D                                                               
RECORD   DS    CL500               AREA TO BUILD SORT RECORDS                   
         SPACE                                                                  
SORTC    DS    0D                  SORTER BUFFER                                
         DC    41000X'00'                                                       
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
SPACE    EQU   C' '                                                             
DASH     EQU   C'-'                                                             
ASTER    EQU   C'*'                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
MONDAY   EQU   1                                                                
PENDING  EQU   X'01'                                                            
OUTDATED EQU   X'02'                                                            
WIN      EQU   X'04'                                                            
LOSS     EQU   X'08'                                                            
CANCEL   EQU   X'10'                                                            
W        EQU   C'W'                                                             
         SPACE 2                                                                
* SORT RECORD DSECT                                                             
*                                                                               
SORTD    DSECT                                                                  
SORTREC  DS    0D                                                               
SORTRLEN DS    H                   RECORD LENGTH                                
         DS    H                   ALWAYS ZERO                                  
SORTOFF  DS    CL20                                                             
SORTMAN  DS    CL20                                                             
SORTADV  DS    CL20                                                             
SORTAGY  DS    CL20                                                             
SORTPRD  DS    CL20                                                             
SORTREF  DS    CL4                                                              
SORTCON  DS    CL4                                                              
SKEYLEN  EQU   (*-SORTOFF)         END OF KEY                                   
         SPACE                                                                  
SORTFIXD DS    0C                  START OF FIXED DATA                          
SORTOPP  DS    B                   PENDING/WIN/LOSS ETC.                        
SORTCST  DS    CL3                 CONTRACT START                               
SORTORD  DS    CL4                                                              
SORTSTA  DS    CL5                                                              
         SPACE                                                                  
SORTVAR  DS    0C                  START OF BOP DATA                            
SORTAWKS DS    B                                                                
SORTLEN  DS    CL3                                                              
SORTDEMO DS    CL10                                                             
SORTOBJ  DS    CL20                                                             
SORTBRK  DS    CL3                 BREAKING DATE                                
SORTEND  EQU   *                   END OF RECORD                                
         SPACE 2                                                                
* WORKD AND FILE DSECTS                                                         
         PRINT OFF                                                              
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REGENALL                                                       
       ++INCLUDE REREPMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021REREP8A02S05/01/02'                                      
         END                                                                    
