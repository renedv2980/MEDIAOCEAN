*          DATA SET RENWR01S   AT LEVEL 045 AS OF 12/17/03                      
*PHASE T83001A,*                                                                
*INCLUDE COVAIL                                                                 
         TITLE 'T83001 - REP WRITER APPLICATION - MACROS'                       
***********************************************************************         
*                                                                     *         
*        MACRO FOR ENTRIES IN DATA TABLE                              *         
*                                                                     *         
***********************************************************************         
*                                                                               
         MACRO                                                                  
         DTTAB &CD,&NAME,&RECID=,&SOON=                                         
*                                                                               
         LCLC  &TEXT                                                            
.*                                                                              
&TEXT    SETC  '       '.'&NAME'.' TABLE'                                       
.*                                                                              
         AIF   (T'&RECID NE 'O').DTT10                                          
.*                                                                              
         DC    AL1(R&CD.KTYQ)&TEXT                                              
.*                                                                              
         AGO   .DTT20                                                           
.*                                                                              
.DTT10   ANOP                                                                   
         DC    AL1(R&RECID.KTYQ)&TEXT                                           
.DTT20   ANOP                                                                   
.*                                                                              
         DC    XL3'00'             SPARE                                        
.*                                                                              
&TEXT    SETC  '       '.'&NAME'.' ID'                                          
.*                                                                              
         DC    CL8'*&CD.TAB*'&TEXT                                              
.*                                                                              
&TEXT    SETC  ' A(START OF '.'&NAME'.' TABLE)'                                 
.*                                                                              
         DC    AL4(RE&CD.TBA-RENWRIOD)&TEXT                                     
.*                                                                              
&TEXT    SETC  ' '.'&NAME'.' TABLE LENGTH'                                      
.*                                                                              
         DC    AL4(&CD.TOHDQ+&CD.TMAXQ*&CD.TENTL)&TEXT                          
         AIF   (T'&SOON NE 'O').DTT30                                           
         DC    AL4(&CD.TOHDQ+&CD.TMAXQ*&CD.TENTL)&TEXT                          
         AGO   .DTT999                                                          
.DTT30   ANOP                                                                   
         DC    AL4(&CD.TOHDQ+&CD.TMAXQ*&CD.TENTL/&SOON)                         
.DTT999  ANOP                                                                   
.*                                                                              
         MEND                                                                   
*                                                                               
         TITLE 'T83001 - REP WRITER APPLICATION'                                
***********************************************************************         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 - WORK REG                                              *         
*          R4 - WORK REG, KEY DSECT POINTER, GETEL REG                *         
*          R5 - WORK REG                                              *         
*          R6 - WORK REG & ELEM POINTER                               *         
*          R7 - POINTER TO RENWRIOD                                   *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (RENWR00-T83000)         *         
*                  - IN AIO FROM HYPER CONTROLLER (T00A30)            *         
*             AIO2 -                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
***********************************************************************         
*                                                                     *         
*  08NOV95 (BU ) --- CHANGE REGENALL TO REGENALL1 (2K BFR/CONTRACT)   *         
*                                                                               
***********************************************************************         
         TITLE 'T83001 - REP WRITER APPLICATION INIT'                           
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
T83001   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T83001,RR=R2                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         LA    R7,RENWRIOD                                                      
         USING RENWRIOD,R7                                                      
*                                                                               
         ST    R2,RELORW01         SAVE RELOCATION FACTOR                       
*                                                                               
*        FIND UTL ADDRESS IF OFF-LINE                                           
*                                                                               
         CLI   OFFLINE,C'Y'     IF OFF-LINE OPEN CONTROL SYSTEM FILES           
         BNE   INIT10                                                           
*                                                                               
         L     RF,ATWA             POINT TO TWA                                 
         L     RF,TWAMASTC-TWATASK(RF) POINT TO MASTC                           
         L     RE,MCUTL-MCBLOCK(RF)  POINT TO UTL                               
         ST    RE,REUTLA           SAVE ADDRESS                                 
         MVC   REQSE,4(RE)         SAVE STARTING SE NUMBER                      
*                                                                               
INIT10   DS    0H                                                               
*                                                                               
         MVI   ERROR,0             CLEAR ERROR MESSAGE BYTES                    
         XC    RERROR,RERROR                                                    
*                                                                               
         TITLE 'T83001 - REP WRITER APPLICATION - MODE'                         
***********************************************************************         
*                                                                     *         
*        DETERMINE CALLING MODE                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     MODEX                                                            
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   *+12                                                             
         BAS   RE,VREC                                                          
         B     MODEX                                                            
*                                                                               
         CLI   MODE,DISPREC        DISPLAY  RECORD                              
         BNE   *+12                                                             
         BAS   RE,DREC                                                          
         B     MODEX                                                            
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   *+8                                                              
         BAS   RE,PREP                                                          
*                                                                               
MODEX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'RENWR01 - REP WRITER REPORT MASTER - VKEY'                      
**********************************************************************          
*                                                                    *          
*        VALIDATE HEADER FIELDS                                      *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
VKEY     DS    0H                                                               
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF OFF-LINE                             
         BE    VKEYFLTX                                                         
*                                                                               
         TM    REREQPRF+1,X'80'    SKIP IF AUTH NEEDED FOR CHANGE               
         BNO   VKEYFLTX                                                         
*                                                                               
         CLI   REWNAM,C'#'         SKIP IF FORMAT NOT A TEMPLATE                
         BNE   VKEYFLTX                                                         
*                                                                               
         CLI   ACTNUM,ACTLIST      OKAY IF ACTION LIST                          
         BE    *+8                                                              
         CLI   ACTNUM,ACTDIS       OKAY IF ACTION DISPLAY                       
         BE    *+8                                                              
         CLI   ACTNUM,ACTREP       OKAY IF ACTION REPORT                        
         BE    VKEYFLTX                                                         
*                                                                               
         CLI   ACTNUM,ACTSEL       IF ACTION SELECT                             
         BNE   VKEYSELX                                                         
*                                                                               
         CLI   THISLSEL,C'C'          OKAY IF NOT CHANGE                        
         BE    *+8                                                              
         CLI   THISLSEL,CHASELQ                                                 
         BE    *+8                                                              
         CLI   THISLSEL,C'D'                  NOR DELETE                        
         BE    *+8                                                              
         CLI   THISLSEL,DELSELQ                                                 
         BNE   VKEYFLTX                                                         
*                                                                               
VKEYSELX DS    0H                                                               
*                                                                               
         TM    TWAAUTH,X'40'       IF AUTHORIZED                                
         BO    *+12                                                             
         CLI   DDS,C'Y'            IF DDS TERMINAL                              
         BNE   VKEYFLTE            ELSE NOT ALLOWED TO CHANGE TEMPLATE          
*                                                                               
VKEYFLTX DS    0H                                                               
*                                                                               
VKEYX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
VKEYFLTE DS    0H                  INVALID FILTER                               
*                                                                               
         LA    R2,CONACTH          CURSOR TO ACTION FIELD                       
         MVI   FIELDERR,0          START OF ACTION FIELD                        
         LHI   RF,RWERFLNV         NOT AUTHORIZED TO CHANGE TEMPLATE            
         B     VKEYFLER                                                         
*                                                                               
VKEYFLER DS    0H                                                               
*                                                                               
         STCM  RF,3,RERROR                                                      
*                                                                               
         GOTO1 CURSERR                                                          
*                                                                               
         TITLE 'RENWR01 - REP WRITER REPORT MASTER - DREC'                      
**********************************************************************          
*                                                                    *          
*        DISPLAY WRITER FORMAT                                       *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
DREC     DS    0H                                                               
*                                                                               
DRECX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'T83001 - REP WRITER APPLICATION - VREC'                         
***********************************************************************         
*                                                                     *         
*        VALIDATE REQUEST PARAMETERS                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VREC     NTR1  LABEL=*                                                          
*                                                                               
         ICM   R3,15,TWAMASTC      POINT TO MASTER CONTROL BLOCK                
         BZ    VRECMSTX            NOT OFF-LINE                                 
         USING MASTD,R3            ESTABLISH MASTER CONTROL BLOCK               
*                                                                               
         LA    R4,MCREMOTE         POINT TO REMOTE CONTROL BLOCK                
         USING REMOTED,R4          ESTABLISH REMOTE CONTROL BLOCK               
*                                                                               
         OC    REMOTKEY,REMOTKEY   SKIP IF NOT A DIRECT REPORT                  
         BZ    VRECMSTX                                                         
*                                                                               
         GOTO1 USERVAL             GET REP NAME & ADDRESS                       
*                                                                               
*        BREAK OUT REPORTS ON QUEUE                                             
*                                                                               
         CLI   REREPPRF+17,C'Y'    SKIP IF NOT BREAKING OUT REPORTS             
         BNE   VRECMSTX                                                         
*                                                                               
         GOTO1 TWAVPRNT,DMCB,=C'CLOSE'   STOP CURRENT REPORT                    
*                                                                               
         AP    MCREQNO,=P'1'       BUMP REQUEST NUMBER                          
*                                                                               
         L     RF,MCVREMOT         POINT TO DDPRINT'S REMOTEC                   
         MVC   0(L'MCREMOTE,RF),MCREMOTE  REPLACE WITH CURRENT REMOTE           
         LR    R4,RF               SWITCH POINTERS                              
*                                                                               
         MVC   REMOTAOP,MCVPQOPN                                                
         MVC   REMOTABF,MCVPQBUF                                                
         MVC   REMOTADM,MCVDMGR                                                 
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,CONWHENH+5     FIELD LENGTH                                 
         BZ    VRECREQX            SHOULDN'T HAPPEN                             
         LA    R1,CONWHEN-1(RF)    FIND REQUESTOR                               
*                                  LOOK FOR FIRST COMMA                         
         CLI   0(R1),C','                                                       
         BE    *+16                                                             
         AHI   R1,-1               NEXT CHARACTER                               
         BCT   RF,*-12                                                          
         B     VRECREQX            NO COMMA -SHOULDN'T HAPPEN                   
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,CONWHENH+5     ORIGINAL LENGTH                              
         SR    RE,RF               INITIALS LENGTH                              
         BM    VRECREQX            SHOULDN'T HAPPEN                             
         LR    RF,RE                                                            
*                                                                               
         MVC   REMOTJID,RESPACES   INIT INITIALS                                
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   REMOTJID(0),1(R1)   USER'S INITIALS                              
*                                                                               
         MVC   REMOTDSC(3),=C'RXW' REPORT TYPE                                  
*                                                                               
         LA    R1,REMOTDSC+3       NEXT AVAILABLE POSITION                      
         MVI   0(R1),C'-'          SEPARATOR                                    
         MVC   1(7,R1),REWNAM      REPORT NAME                                  
*                                                                               
VRECREQX DS    0H                                                               
*                                                                               
VRECMSTX DS    0H                                                               
*                                                                               
         DROP  R3                                                               
*                                                                               
         LA    RF,RWAADTAB                                                      
         ST    RF,REAADTBA         SET A(ASATDATES TABLE)                       
*                                                                               
         GOTO1 USERVAL             GET REP NAME & ADDRESS                       
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF OFF-LINE                             
         BE    VRECFLTX                                                         
*                                                                               
         TM    REREQPRF+1,X'80'    SKIP IF AUTH NEEDED FOR CHANGE               
         BNO   VRECFLTX                                                         
*                                                                               
         CLI   REWNAM,C'#'         SKIP IF FORMAT NOT A TEMPLATE                
         BNE   VRECFLTX                                                         
*                                                                               
         CLI   ACTNUM,ACTLIST      OKAY IF ACTION LIST                          
         BE    *+8                                                              
         CLI   ACTNUM,ACTDIS       OKAY IF ACTION DISPLAY                       
         BE    *+8                                                              
         CLI   ACTNUM,ACTREP       OKAY IF ACTION REPORT                        
         BE    VRECFLTX                                                         
*                                                                               
         CLI   ACTNUM,ACTSEL       IF ACTION SELECT                             
         BNE   VRECSELX                                                         
*                                                                               
         CLI   THISLSEL,C'C'          OKAY IF NOT CHANGE                        
         BE    *+8                                                              
         CLI   THISLSEL,CHASELQ                                                 
         BE    *+8                                                              
         CLI   THISLSEL,C'D'                  NOR DELETE                        
         BE    *+8                                                              
         CLI   THISLSEL,DELSELQ                                                 
         BNE   VRECFLTX                                                         
*                                                                               
VRECSELX DS    0H                                                               
*                                                                               
         TM    TWAAUTH,X'40'       IF AUTHORIZED                                
         BO    *+12                                                             
         CLI   DDS,C'Y'            IF DDS TERMINAL                              
         BNE   VRECFLTE            ELSE NOT ALLOWED TO CHANGE TEMPLATE          
*                                                                               
VRECFLTX DS    0H                                                               
*                                                                               
         LA    R2,REWOPTH          VALIDATE OPTIONS BEFORE OTHERS               
         GOTO1 VALOPTS                                                          
*                                                                               
*        INIT PQ INDEX DATA TABLE IF NEEDED                                     
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF NOT OFFLINE,                         
         BNE   VRECINXX                                                         
*                                                                               
         TM    NEWOPTS,NOPTPQIX    IF PQ INDEX OPTION SELECTD                   
         BO    VRECINX1             YES - ALWAYS GIVE INDEX                     
*                                                                               
         L     RF,TWAMASTC         ALLOCATE PQINDEX TABLE?                      
         USING MASTD,RF                                                         
*                                                                               
         TM    MCOPT1,MCQ1PQIX     DDMAST OPTION MUST BE ON                     
         BZ    VRECINXX                                                         
*                                                                               
         OC    MCREMOTE,MCREMOTE     MUST BE DIRECT                             
         BNZ   *+14                                                             
         OC    MCREMPQK,MCREMPQK     OR  SOON REPORT                            
         BZ    VRECINXX                                                         
*                                                                               
         DROP  RF                                                               
*                                                                               
VRECINX1 DS    0H                                                               
*                                                                               
         LA    R3,MAXROWS+MAXMIDS+MAXHEADS+MAXCOLS MAX ENTRIES IN TAB           
         MHI   R3,PQINDXEQ         MAX RECS * L'ENTRY                           
         AHI   R3,4                EXTRA 4 BYTES FOR TABLE LENGTH               
*                                                                               
         ST    R3,DMCB+4           AMOUNT OF STORAGE WANTED                     
         ST    R3,DMCB+8                                                        
*                                                                               
         GOTO1 =V(COVAIL),DMCB,C'GET'  GET STORAGE FOR TABLE                    
         ICM   RE,15,4(R1)         NO ERRORS TOLERATED                          
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ST    R3,0(RE)            +0 = L'TABLE                                 
*                                                                               
         AHI   R3,-4               SIZE OF TABLE                                
         AHI   RE,4                A(START OF TABLE)                            
         ST    RE,APQINDEX         SAVE START ADDRESS                           
*                                                                               
         XCEF  (RE),(R3)           INIT TABLE                                   
*                                                                               
VRECINXX DS    0H                                                               
*                                                                               
******   GOTO1 INITDRON            INITIALIZE DRONE                             
*                                                                               
         LA    R2,REWGRPH          GROUP/SUBGROUP                               
         GOTO1 VALIGS                                                           
*                                                                               
         LA    R2,REWSTAH          STATION                                      
         GOTO1 VALISTA                                                          
*                                                                               
         LA    R2,REWPERH          PERIOD DATES                                 
         GOTO1 VALIPDT                                                          
*                                                                               
         LA    R2,REWREGH          REGION                                       
         GOTO1 VALIREG                                                          
*                                                                               
         LA    R2,REWOFFH          OFFICE                                       
         GOTO1 VALIOFF                                                          
*                                                                               
         LA    R2,REWTEMH          DIV/TEAM                                     
         GOTO1 VALIDT                                                           
*                                                                               
         LA    R2,REWSALH          SALESPERSON                                  
         GOTO1 VALISAL                                                          
*                                                                               
         LA    R2,REWAGYH          AGENCY                                       
         GOTO1 VALIAGY                                                          
*                                                                               
         LA    R2,REWADVH          ADVERTISER                                   
         GOTO1 VALIADV                                                          
*                                                                               
         LA    R2,REWPRDH          PRODUCT                                      
         GOTO1 VALIPRD                                                          
*                                                                               
         TITLE 'T83001 - REP WRITER APPLICATION - VRSOON'                       
***********************************************************************         
*                                                                     *         
*        IF A SOON REQUEST MAKE SURE MINIMUM FILTERING BEING DONE     *         
*              1. STATION OR OFFICE MUST BE FILTERED                  *         
*              IF GROUP BEING FILTERED THEN                           *         
*               ONE OF SUB-GROUP, ADVERTISER OR AGENCY                *         
*                 MUST BE FILTERED                                    *         
*                                                                     *         
*              BEING FILTERED MEANS SINGLE OR SET VALUE GIVEN         *         
*              IN SOME CASES OTHER ITEMS BEING FILTERED MEAN          *         
*              MEAN THAT DESIRED ITEM IS ALSO FILTERED.               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRSOON   DS    0H                                                               
*                                                                               
         CLC   =C'SOON',CONWHEN    SKIP IF SOON REQUEST                         
         B     VRSOONX                                                          
*MN      BNE   VRSOONX                                                          
*                                                                               
*        OKAY IF OFFICE BEING FILTERED                                          
*                                                                               
         OC    REFOFF,REFOFF       IF OFFICE FILTER ENTERED                     
         BNZ   VRSOONX                REPORT OKAY                               
         OC    REOFFSID,REOFFSID   IF OFFICE SET RECORD ID ENTERED              
         BNZ   VRSOONX                REPORT OKAY                               
*                                                                               
*        OKAY IF STATION BEING FILTERED                                         
*                                                                               
         OC    REFSTA,REFSTA       IF STATION FILTER ENTERED                    
         BNZ   VRSOONX                REPORT OKAY                               
         OC    RESTASID,RESTASID   IF STATION SET RECORD ID ENTERED             
         BNZ   VRSOONX                REPORT OKAY                               
         OC    REFMKT,REFMKT       IF MARKET FILTER ENTERED                     
         BNZ   VRSOONX                REPORT OKAY                               
         OC    REFAFF,REFAFF       IF AFFILIATION FILTER ENTERED                
         BNZ   VRSOONX                REPORT OKAY                               
         OC    REFRNK,REFRNK       IF MARKET RANK FILTER ENTERED                
         BNZ   VRSOONX                REPORT OKAY                               
         OC    REFOWN,REFOWN       IF OWNER FILTER ENTERED                      
         BNZ   VRSOONX                REPORT OKAY                               
         OC    REFTVB,REFTVB       IF TVB REGION FILTER ENTERED                 
         BNZ   VRSOONX                REPORT OKAY                               
         OC    REFSTTP,REFSTTP     IF STATION TYPE FILTER ENTERED               
         BNZ   VRSOONX                REPORT OKAY                               
*                                                                               
*        AT THIS POINT GROUP MUST BE FILTERED                                   
*                                                                               
         OC    REFGRP,REFGRP       IF GROUP NOT FILTERED                        
         BNZ   *+10                                                             
         OC    RESTASID,RESTASID   AND GROUP SET RECORD ID NOT ENTERED          
         BZ    VRSOONE                ERROR                                     
*                                                                               
         OC    REFSGR,REFSGR       OKAY IF SUBGROUP ENTERED                     
         BNZ   VRSOONX                                                          
*                                                                               
*        ELSE AGENCY OR ADVERTISER FILTER REQUIRED                              
*                                                                               
         OC    REFADV,REFADV       IF ADVERTISER FILTER ENTERED                 
         BNZ   VRSOONX                REPORT OKAY                               
         OC    READVSID,READVSID   IF ADVERTISER SET RECORD ID ENTERED          
         BNZ   VRSOONX                REPORT OKAY                               
*                                                                               
         OC    REFAGY,REFAGY       IF AGENCY FILTER ENTERED                     
         BNZ   VRSOONX                REPORT OKAY                               
         OC    REAGYSID,REAGYSID   IF AGENCY SET RECORD ID ENTERED              
         BNZ   VRSOONX                REPORT OKAY                               
         OC    REFTER,REFTER       IF TERRITORY ID ENTERED                      
         BNZ   VRSOONX                REPORT OKAY                               
*                                                                               
         B     VRSOONE             SOON RESTRICTION ERROR                       
*                                                                               
VRSOONE  DS    0H                  SOON RESTRICTION ERROR                       
*                                                                               
         MVI   ERROR,RWESOON       SET ERROR CODE                               
         LA    R2,REWGRPH          CURSOR TO GROUP FIELD                        
         GOTO1 ERREX                                                            
*                                                                               
VRSOONX  DS    0H                                                               
*                                                                               
         TITLE 'T83001 - REP WRITER APPLICATION - VREC'                         
***********************************************************************         
*                                                                     *         
*        VALIDATE KEYWORDS                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRKYW    DS    0H                                                               
*                                                                               
         CLI   REWRCAPH+5,0        IF RECAPS ENTERED                            
         BE    VRREC1X                                                          
*                                     ADD RECORD NUMBER TO KEY                  
         XC    BLOCK(42),BLOCK                                                  
         MVC   BLOCK(2),=X'0602'      LENGTH OF SPLIT FIELD                     
         MVC   BLOCK+12(30),SPACES                                              
         MVC   BLOCK+12(8),=C'RECORD'                                           
         MVC   BLOCK+22(2),=C'NP'                                               
*                                                                               
         LA    R4,BLOCK               POINT TO CREATED SCANNER BLOCK            
*                                                                               
         GOTO1 VROWDRON               VALIDATE RECORD AS KEYWORD                
*                                                                               
         CLI   DRERROR,0                                                        
         BNE   *-2                                                              
*                                                                               
         NI    DRFLAGO,X'FF'-X'80' TURN OFF PRINT IND ON ALL HEADLINES          
         NI    DRHEAD1,X'FF'-X'80'                                              
         NI    DRHEAD2,X'FF'-X'80'                                              
         NI    DRHEAD3,X'FF'-X'80'                                              
         NI    DRHEAD4,X'FF'-X'80'                                              
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF NOT OFF-LINE                         
         BNE   VRREC1X                                                          
*                                                                               
         GOTO1 GROWDRON            GENERATE ROW                                 
*                                                                               
VRREC1X  DS    0H                                                               
*                                                                               
         LA    R2,REWLEFTH         LEFT HEADERS                                 
         MVI   MAX,4                                                            
         GOTO1 VALLEFT                                                          
*                                                                               
         LA    R2,REWRGHTH         RIGHT HEADERS                                
         MVI   MAX,3                                                            
         GOTO1 VALRIGHT                                                         
*                                                                               
         LA    R2,REWMIDH          MID LINE                                     
         MVI   MAX,1                                                            
         GOTO1 VALMID                                                           
*                                                                               
         LA    R2,REWROWSH         ROWS                                         
         MVI   MAX,8                                                            
         GOTO1 VALROWS                                                          
*                                                                               
         LA    R2,REWCOLSH         COLUMNS                                      
         MVI   MAX,14                                                           
         GOTO1 VALCOLS                                                          
*                                                                               
         LA    R2,REWTITLH         USER TITLES                                  
         GOTO1 VALTITS                                                          
*                                                                               
         CLI   REWRCAPH+5,0        SKIP IF NO RECAPS ENTERED                    
         BE    VRRCAPX                                                          
*                                                                               
         MVC   DRSTBUF,DRCURBUF    RESET DRONE BUFFER START                     
*                                                                               
         GOTO1 INITDRON            RE-INITIALIZE DRONE                          
*                                                                               
         XC    BLOCK(42),BLOCK                                                  
         MVC   BLOCK(2),=X'0602'      LENGTH OF SPLIT FIELD                     
         MVC   BLOCK+12(30),SPACES                                              
         MVC   BLOCK+12(8),=C'RECORD'                                           
         MVC   BLOCK+22(2),=C'NP'                                               
*                                                                               
         LA    R4,BLOCK               POINT TO CREATED SCANNER BLOCK            
*                                                                               
         GOTO1 VROWDRON               VALIDATE RECORD AS KEYWORD                
*                                                                               
         CLI   DRERROR,0                                                        
         BNE   *-2                                                              
*                                                                               
         NI    DRFLAGO,X'FF'-X'80' TURN OFF PRINT IND ON ALL HEADLINES          
         NI    DRHEAD1,X'FF'-X'80'                                              
         NI    DRHEAD2,X'FF'-X'80'                                              
         NI    DRHEAD3,X'FF'-X'80'                                              
         NI    DRHEAD4,X'FF'-X'80'                                              
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF NOT OFF-LINE                         
         BNE   VRREC2X                                                          
*                                                                               
         GOTO1 GROWDRON            GENERATE ROW                                 
*                                                                               
VRREC2X  DS    0H                                                               
*                                                                               
         SR    R0,R0                                                            
         IC    R0,ROWWIDTH         SAVE CURRENT ROW WIDTH                       
*                                                                               
         LA    R2,REWRCAPH         RECAPS                                       
         MVI   MAX,4                                                            
         GOTO1 VALROWS                                                          
*                                                                               
         LA    R2,REWCOLSH         COLUMNS                                      
         MVI   MAX,14                                                           
         GOTO1 VALCOLS                                                          
*                                                                               
         CLM   R0,1,ROWWIDTH       USE SMALLER OF MAIN RPT AND RECAP            
         BNL   *+8                                                              
         STC   R0,ROWWIDTH             MAIN RPT SHORTER ROW AREA                
*                                                                               
VRRCAPX  DS    0H                                                               
*                                                                               
         GOTO1 WRAPDRON                                                         
*                                                                               
         TITLE 'T83001 - REP WRITER APPLICATION - CHKBUFF'                      
***********************************************************************         
*                                                                     *         
*        BASED ON RECORD TYPES NEEDED FOR REPORT (FOUND IN REDATA)    *         
*        CHECK THAT MONSTER BUFFER WILL HOLD ALL THE TABLES NEEDED    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VRTABS   DS    0H                                                               
*                                                                               
         MVC   FULL,=AL4(5000000)  BUFFER SIZE                                  
*                                                                               
         CLC   =C'SOON',CONWHEN    IF SOON REQUEST                              
         BNE   *+10                                                             
         MVC   FULL,=AL4(2500000)     NEW BUFFER SIZE                           
*                                                                               
         LA    R2,REDATA           POINT TO LIST OF DATA TYPES NEEDED           
         LA    R0,L'REDATA         MAX ENTRIES IN LIST                          
*                                                                               
         CLI   0(R2),0             FIND END OF LIST                             
         BE    *+24                                                             
         CLI   0(R2),RREPKTYQ      DONE IF REP TYPE IN LIST                     
         BE    *+20                                                             
         LA    R2,1(R2)                                                         
         BCT   R0,*-20                                                          
         B     *+8                 NO ROOM                                      
         MVI   0(R2),RREPKTYQ      ADD REP ID TO LIST                           
*                                                                               
         LA    R2,REDATA           POINT TO LIST OF DATA TYPES NEEDED           
         LA    R0,L'REDATA         MAX ENTRIES IN LIST                          
*                                                                               
         CLI   0(R2),0             FIND END OF LIST                             
         BE    *+24                                                             
         CLI   0(R2),RGRPKTYQ      DONE IF GRP TYPE IN LIST                     
         BE    *+20                                                             
         LA    R2,1(R2)                                                         
         BCT   R0,*-20                                                          
         B     *+8                 NO ROOM                                      
         MVI   0(R2),RGRPKTYQ      ADD GRP ID TO LIST                           
*                                                                               
         LA    R2,REDATA           RESET POINTER                                
         LA    R0,L'REDATA         RESET COUNTER                                
         SR    RF,RF               INIT LENGTH OF TABLES                        
*                                                                               
VRTB1LP  DS    0H                                                               
*                                                                               
         CLI   0(R2),0             DONE AT END OF LIST                          
         BE    VRTB1DN                                                          
*                                                                               
         LM    R3,R5,DATABXLE      BXLE PARAMETERS FOR DATATAB                  
         USING DATATABD,R3         ESTABLISH DATATAB ENTRY                      
*                                                                               
         A     R3,RELORW01         RE-LOCATE ADDRESSES                          
         A     R5,RELORW01                                                      
*                                                                               
VRTB2LP  DS    0H                                                               
*                                                                               
         CLC   DTBRECID,0(R2)      MATCH DATA IN LIST TO TABLE ENTRY            
         BE    VRTB2FD                                                          
*                                                                               
VRTB2CN  DS    0H                                                               
*                                                                               
         BXLE  R3,R4,VRTB2LP       NEXT ENTRY IN TABLE                          
         B     VRTB2DN             NO ENTRY IN TABLE FOR THIS DATA              
*                                                                               
VRTB2FD  DS    0H                  TABLE ENTRY FOUND                            
*                                                                               
         LA    RF,8(RF)            ALLOW FOR TABLE IDENTIFIER                   
*                                                                               
         ICM   RE,15,DTBTABLN      GET TABLE LENGTH                             
         CLC   =C'SOON',CONWHEN    IF SOON REQUEST                              
         BNE   *+8                                                              
         ICM   RE,15,DTBTABSL         GET SOON TABLE LENGTH                     
*                                                                               
         AR    RF,RE               NEW BUFFER LENGTH                            
*                                                                               
         SRL   RF,3                FIND NEXT DOUBLE WORD                        
         LA    RF,1(RF)                                                         
         SLL   RF,3                                                             
*                                                                               
         C     RF,FULL             CANNOT EXCEED BUFFER LENGTH                  
         BH    VRBUFFLE                                                         
*                                                                               
VRTB2DN  DS    0H                                                               
*                                                                               
VRTB1CN  DS    0H                                                               
*                                                                               
         LA    R2,1(R2)            BUMP LIST POINTER                            
         BCT   R0,VRTB1LP                                                       
*                                                                               
VRTB1DN  DS 0H                                                                  
*                                                                               
         B     VRTABX                                                           
*                                                                               
VRTABX   DS    0H                                                               
*                                                                               
VRECX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
VRBUFFLE DS    0H                  TOO MANY TABLES FOR BUFFER                   
*                                                                               
         MVI   ERROR,RWEBUFLE      SET ERROR CODE                               
         LA    R2,CONACTH          CURSOR TO ACTION FIELD                       
         GOTO1 ERREX                                                            
*                                                                               
VRECFLTE DS    0H                  INVALID FILTER                               
*                                                                               
         LA    R2,CONACTH          CURSOR TO ACTION FIELD                       
         MVI   FIELDERR,0          START OF ACTION FIELD                        
         LHI   RF,RWERFLNV         NOT AUTHORIZED TO CHANGE TEMPLATE            
*                                                                               
         STCM  RF,3,RERROR                                                      
*                                                                               
         GOTO1 CURSERR                                                          
*                                                                               
VRRFLNME DS    0H                  FILTER DOES NOT MATCH                        
*                                                                               
         MVC   8(L'TWRFLTR,R2),TWRFLTR RE-DISPLAY ENTERED FILTER                
         MVI   5(R2),L'TWRFLTR         FIELD LENGTH                             
         OI    6(R2),X'80'             FORCE RE-TRANSMISSION                    
*                                                                               
         TWAXC REWGRPH             CLEAR SCREEN                                 
*                                                                               
         LHI   RF,RWERFLNM                                                      
         STCM  RF,3,RERROR                                                      
*                                                                               
VRECERR  DS    0H                                                               
*                                                                               
         OI    REWNAMH+1,X'01'     SET FIELD TO BE MODIFIED                     
         OI    REWNAMH+6,X'80'     FORCE TRANSMISSION                           
         XC    TWRFLTR,TWRFLTR     CLEAR SAVED FILTER                           
*                                                                               
         GOTO1 CURSERR                                                          
*                                                                               
         TITLE 'T83001 - REP WRITER APPLICATION - PREP'                         
***********************************************************************         
*                                                                     *         
*        PRINT REPORT                                                 *         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PREP     NTR1  LABEL=*                                                          
*                                                                               
         GOTO1 INITDRIV            INITIALIZE DRIVER                            
*                                                                               
         L     R4,AGLOBAL          ESTABLISH DRIVER GLOBAL STORAGE              
         USING GLOBALD,R4                                                       
*                                                                               
         LA    R1,HOOK             APPLICATION HOOK                             
         ST    R1,GLAHOOK                                                       
*                                                                               
         MVI   GLMODE,GLINIT       INITIALIZE DRIVER                            
         GOTO1 DRIVER,DMCB,(R4)                                                 
*                                                                               
         MVC   REACOMFC,ACOMFACS   SET UP FOR VRWIO                             
         LA    R1,IOHOOK           SET IO HOOK ADDRESS                          
         ST    R1,REHOOK                                                        
*                                                                               
         MVC   REACCS,TWAACCS      PASS THROUGH LIMIT ACCESS                    
         MVC   REAUTH,TWAAUTH                   AND AUTHORIZATION               
         MVC   REUSERID,TWAORIG                 AND REQUESTING ID#              
         MVC   RESE,REQSE          DEFAULT SE NUMBER                            
         MVC   REREP,TWAAGY                                                     
         MVC   READDAY,ADDAY       A(ADDAY)                                     
*                                                                               
         CLC   REREP,=C'NU'        REP NU                                       
         BNE   *+10                                                             
         MVC   REMREP,=C'K3'           GETS MASTER REP K3                       
*                                                                               
*        GET WORKAREA FOR CONTRACT ANALYSIS                                     
*        ONE SECTION NEED FOR REPORT AND EACH ASATDATE COLUMN FILTER            
*                                                                               
*        CALCULATE SIZE OF GETMAIN AREA NEEDED                                  
*                                                                               
         L     RE,=AL4(NWMTENTL)   SIZE OF NEWMON TABLE SECTION                 
         SR    RF,RF                                                            
         IC    RF,REQAADFN         NUMBER OF AS-AT-DATE COLUMN FILTERS          
         LA    RF,1(RF)            ADD ONE FOR REST OF REPORT                   
         MR    RE,RE               SIZE OF GETMAIN NEEDED                       
*                                                                               
         GOTO1 =V(COVAIL),DMCB,C'GET',(RF),(RF)                                 
         OC    DMCB+4(4),DMCB+4    WORK AREA RETRIEVED?                         
         BNZ   *+6                 YES                                          
         DC    H'0'                NO  - CAN'T GO ON                            
*                                                                               
         MVC   REANWMON,DMCB+4     SAVE ADDRESS                                 
*                                                                               
         L     RF,REANWMON         POINT TO WORKAREA                            
         XCEFL 0(RF),DMCB+8        CLEAR WORKAREA                               
*                                                                               
*        GET STACK BUFFER IF NEEDED                                             
*                                                                               
         TM    STACKSW,STCKBUFQ    SKIP IF NO STACK BUFFER NEEDED               
         BNO   PRTSVBX                                                          
*                                                                               
         ICM   R3,15,*+8           GET SIZE OF GETMAIN AREA                     
         B     *+8                                                              
         DC    AL4(60000)               60K BUFFER                              
*                                                                               
         OC    RESVBBFA,RESVBBFA   CHECK FOR MULTIPLE ENTRIES                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,RESVBBFA         BUFFER ADDRESS SAVEAREA                      
*                                                                               
         GETMAIN EC,LV=(R3),A=(R4)  GET CORE                                    
*                                                                               
         ST    R3,RESVBBFL         SAVE RETURNED BUFFER LENGTH                  
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
         L     R4,RESVBBFA         BUFFER ADDRESS SAVEAREA                      
         USING STKVLBFD,R4         ESTABLISH BUFFER                             
*                                                                               
         LA    RF,SVBFSTRT         SET BUFFER START ADDRESS                     
         ST    RF,SVBFSTA                                                       
*                                                                               
         XC    SVBFLEN,SVBFLEN       INIT ENTRY LENGTH                          
*                                                                               
         BCTR  RF,0                                                             
         ST    RF,SVBFENDA         INDICATE BUFFER IS EMPTY                     
*                                                                               
         DROP  R4                                                               
*                                                                               
PRTSVBX  DS    0H                                                               
*                                                                               
*        GET MONSTER BUFFER TO HOLD ALL OF THE TABLES                           
*                                                                               
         ICM   R3,15,*+8           GET SIZE OF GETMAIN AREA                     
         B     *+8                                                              
         DC    AL4(5000000)        BUFFER SIZE                                  
*                                                                               
         CLC   =C'SOON',CONWHEN    IF SOON REQUEST                              
         BNE   *+16                                                             
         ICM   R3,15,*+8              GET SIZE OF SOON GETMAIN AREA             
         B     *+8                                                              
         DC    AL4(2500000)           BUFFER SIZE                               
*                                                                               
         OC    REBUFFA,REBUFFA     CHECK FOR MULTIPLE ENTRIES                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,REBUFFA          BUFFER ADDRESS SAVEAREA                      
*                                                                               
         GETMAIN EC,LV=(R3),A=(R4)  GET CORE                                    
*                                                                               
         ST    R3,REBUFFL          SAVE RETURNED BUFFER LENGTH                  
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
         TITLE 'T83001 - REP WRITER APPLICATION - PRTABS'                       
***********************************************************************         
*                                                                     *         
*        BASED ON RECORD TYPES NEED FOR REPORT (FOUND IN REDATA)      *         
*        ALLOCATE PART OF MONSTER BUFFER TO TABLE                     *         
*              SAVE STARTING ADDRESS AND LENGTH                       *         
*              SET TAG                                                *         
*        CALL VRWIO TO FILL TABLE                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRTABS   DS    0H                                                               
*                                                                               
         MVC   REBFNXTA,REBUFFA    INIT A(NEXT AREA AVAILBLE IN BUFFER)         
*                                                                               
         LA    R2,REDATA           POINT TO LIST OF DATA TYPES NEEDED           
         LA    R0,L'REDATA         MAX ENTRIES IN LIST                          
*                                                                               
         CLI   0(R2),0             FIND END OF LIST                             
         BE    *+24                                                             
         CLI   0(R2),RREPKTYQ      DONE IF REP TYPE IN LIST                     
         BE    *+20                                                             
         LA    R2,1(R2)                                                         
         BCT   R0,*-20                                                          
         B     *+8                 NO ROOM                                      
         MVI   0(R2),RREPKTYQ      ADD REP ID TO LIST                           
*                                                                               
*        ALLOCATE A TABLE FOR SECOND AGENCY TABLE IF NEEDED                     
*                                                                               
         TM    REFTYPS2,REFTRTYQ+REFTAG2Q  SKIP IF TABLE NOT NEEDED             
         B     PRTBAG2X                                                         
         BZ    PRTBAG2X                                                         
*                                                                               
         L     RF,REBFNXTA         POINT TO NEXT TABLE AREA                     
*                                                                               
         MVC   0(8,RF),=CL8'**AG2TAB**'  IDENTIFY TABLE                         
*                                                                               
         LA    RF,8(RF)            START OF TABLE                               
*                                                                               
         ST    RF,REAG2TBA         SAVE TABLE START ADDRESS                     
*                                                                               
         L     RE,=AL4(AG2TOHDQ+AG2TMAXQ*AG2TENTL)   TABLE LENGTH               
*                                                                               
         CLC   =C'SOON',CONWHEN    IF SOON REQUEST                              
         BNE   *+8                                                              
         SRL   RE,1                   GET SOON TABLE LENGTH                     
*                                                                               
         AR    RF,RE               POINT TO END OF BUFFER                       
*                                                                               
         SRL   RF,3                FIND NEXT DOUBLE WORD                        
         LA    RF,1(RF)                                                         
         SLL   RF,3                                                             
*                                                                               
         ST    RF,REBFNXTA         START OF NEXT TABLE                          
*                                                                               
         L     RE,REBUFFA          CALCULATE BUFFER LENGTH                      
         SR    RF,RE                                                            
*                                                                               
         C     RF,REBUFFL          CANNOT EXCEED BUFFER LENGTH                  
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
PRTBAG2X DS    0H                                                               
*                                                                               
*        ADD TABLES FOR NEEDED DATA AND INITIALIZE THE TABLE                    
*                                                                               
         LA    R2,REDATA           RESET POINTER                                
         LA    R0,L'REDATA         RESET COUNTER                                
*                                                                               
PRTB1LP  DS    0H                                                               
*                                                                               
         CLI   0(R2),0             DONE AT END OF LIST                          
         BE    PRTB1DN                                                          
*                                                                               
         LM    R3,R5,DATABXLE      BXLE PARAMETERS FOR DATATAB                  
         USING DATATABD,R3         ESTABLISH DATATAB ENTRY                      
*                                                                               
         A     R3,RELORW01         RE-LOCATE ADDRESSES                          
         A     R5,RELORW01                                                      
*                                                                               
PRTB2LP  DS    0H                                                               
*                                                                               
         CLC   DTBRECID,0(R2)      MATCH DATA IN LIST TO TABLE ENTRY            
         BE    PRTB2FD                                                          
*                                                                               
PRTB2CN  DS    0H                                                               
*                                                                               
         BXLE  R3,R4,PRTB2LP       NEXT ENTRY IN TABLE                          
         B     PRTB2DN             NO ENTRY IN TABLE FOR THIS DATA              
*                                                                               
PRTB2FD  DS    0H                  TABLE ENTRY FOUND                            
*                                                                               
*****    CLI   DTBRECID,RAG2KTYQ   SKIP IF SECOND AGENCY TABLE                  
*****    BE    PRTB2DN                                                          
*                                                                               
         L     RF,REBFNXTA         POINT TO NEXT TABLE AREA                     
*                                                                               
         MVC   0(8,RF),DTBTABID    IDENTIFY TABLE                               
*                                                                               
         LA    RF,8(RF)            START OF TABLE                               
*                                                                               
         ICM   RE,15,DTBSTRTA      POINT TO START SAVEAREA                      
         LA    RE,RENWRIOD(RE)                                                  
         ST    RF,0(RE)            SAVE TABLE START ADDRESS                     
*                                                                               
         ICM   RE,15,DTBTABLN      GET TABLE LENGTH                             
         CLC   =C'SOON',CONWHEN    IF SOON REQUEST                              
         BNE   *+8                                                              
         ICM   RE,15,DTBTABSL         GET SOON TABLE LENGTH                     
*                                                                               
         AR    RF,RE               POINT TO END OF BUFFER                       
*                                                                               
         SRL   RF,3                FIND NEXT DOUBLE WORD                        
         LA    RF,1(RF)                                                         
         SLL   RF,3                                                             
*                                                                               
         ST    RF,REBFNXTA         START OF NEXT TABLE                          
*                                                                               
         L     RE,REBUFFA          CALCULATE BUFFER LENGTH                      
         SR    RF,RE                                                            
*                                                                               
         C     RF,REBUFFL          CANNOT EXCEED BUFFER LENGTH                  
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        CREATE TABLE                                                           
*                                                                               
         MVC   REREAD,DTBRECID     SET RECORD TO READ ID                        
*                                                                               
         GOTO1 VRWIO,DMCB,RENWRIOD    FILL TABLE                                
*                                                                               
PRTB2DN  DS    0H                                                               
*                                                                               
PRTB1CN  DS    0H                                                               
*                                                                               
         LA    R2,1(R2)            BUMP LIST POINTER                            
         BCT   R0,PRTB1LP                                                       
*                                                                               
PRTB1DN  DS 0H                                                                  
*                                                                               
         B     PRTABX                                                           
*                                                                               
PRTABX   DS    0H                                                               
*                                                                               
*        READ DETAIL RECORDS AND PASS TO DRIVER                                 
*                                                                               
         CLI   FLAVOR,C'R'         IF RGON REPORT                               
         BNE   PRNRGN                                                           
*                                                                               
         MVI   REREAD,RNRGKTYQ     SET RGON AS RECORD TYPE                      
*                                                                               
         GOTO1 VRWIO,DMCB,RENWRIOD                                              
*                                                                               
         B     PRDTLX                                                           
*                                                                               
PRNRGN   DS    0H                                                               
*                                                                               
*        KEYS READ IN FLIGHT DATE ORDER                                         
*                                                                               
*        READ DETAIL RECORDS AND PASS TO DRIVER                                 
*                                                                               
         CLI   FLAVOR,C'F'         IF FLIGHT DATE REPORT                        
         BNE   PRFLTN                                                           
*                                                                               
         MVI   REREAD,RFLTKTYQ     SET FLIGHT DATE AS RECORD TYPE               
*                                                                               
         GOTO1 VRWIO,DMCB,RENWRIOD                                              
*                                                                               
         B     PRDTLX                                                           
*                                                                               
PRFLTN   DS    0H                                                               
*                                                                               
*        STANDARD DETAIL IS CONTRACT RECORDS                                    
*                                                                               
         MVI   REREAD,RCONKTYQ                                                  
         GOTO1 VRWIO,DMCB,RENWRIOD                                              
*                                                                               
PRDTLX   DS    0H                                                               
*                                                                               
*        CHECK IF BUDGET RECORDS ARE NEEDED                                     
*                                                                               
         LA    R2,REDATA           RESET POINTER                                
         LA    R0,L'REDATA         RESET COUNTER                                
*                                                                               
         CLI   0(R2),RBUDKTYQ      LOOK FOR BUDGET KEYWORD                      
         BE    *+16                                                             
         LA    R2,1(R2)                                                         
         BCT   R0,*-12                                                          
         B     PRBUDX                                                           
*                                                                               
*        PROCESS BUDGET RECORDS                                                 
*                                                                               
         MVI   REREAD,RBUDKTYQ                                                  
*                                                                               
         XC    REBDYTAB,REBDYTAB   INIT BUDGET YEAR TABLE                       
*                                                                               
         GOTO1 VRWIO,DMCB,RENWRIOD                                              
*                                                                               
PRBUDX   DS    0H                                                               
*                                                                               
         LA    R2,REDATA           RESET POINTER                                
         LA    R0,L'REDATA         RESET COUNTER                                
*                                                                               
         CLI   0(R2),RCBDKTYQ      LOOK FOR COMPANY BUDGET KEYWORD              
         BE    *+16                                                             
         LA    R2,1(R2)                                                         
         BCT   R0,*-12                                                          
         B     PRCBDX                                                           
*                                                                               
*        PROCESS COMPANY BUDGET RECORDS                                         
*                                                                               
         MVI   REREAD,RCBDKTYQ                                                  
*                                                                               
         XC    REBDYTAB,REBDYTAB   INIT BUDGET YEAR TABLE                       
*                                                                               
         GOTO1 VRWIO,DMCB,RENWRIOD                                              
*                                                                               
PRCBDX   DS    0H                                                               
*                                                                               
*        CHECK IF OFFICE BUDGET RECORDS ARE NEEDED                              
*                                                                               
         LA    R2,REDATA           RESET POINTER                                
         LA    R0,L'REDATA         RESET COUNTER                                
*                                                                               
         CLI   0(R2),ROBDKTYQ      LOOK FOR OFFICE BUDGET KEYWORD               
         BE    *+16                                                             
         LA    R2,1(R2)                                                         
         BCT   R0,*-12                                                          
         B     PROBDX                                                           
*                                                                               
*        PROCESS OFFICE BUDGET RECORDS                                          
*                                                                               
         MVI   REREAD,ROBDKTYQ                                                  
*                                                                               
         XC    REBDYTAB,REBDYTAB   INIT BUDGET YEAR TABLE                       
*                                                                               
         GOTO1 VRWIO,DMCB,RENWRIOD                                              
*                                                                               
PROBDX   DS    0H                                                               
*                                                                               
*        CHECK IF SALESPERSONS BUDGET RECORDS ARE NEEDED                        
*                                                                               
         LA    R2,REDATA           RESET POINTER                                
         LA    R0,L'REDATA         RESET COUNTER                                
*                                                                               
         CLI   0(R2),RBSPKTYQ      LOOK FOR SAL BUDGET KEYWORD                  
         BE    *+16                                                             
         LA    R2,1(R2)                                                         
         BCT   R0,*-12                                                          
         B     PRBSPX                                                           
*                                                                               
*        PROCESS SAL BUDGET RECORDS                                             
*                                                                               
         MVI   REREAD,RBSPKTYQ                                                  
*                                                                               
         XC    REBDYTAB,REBDYTAB   INIT BUDGET YEAR TABLE                       
*                                                                               
         GOTO1 VRWIO,DMCB,RENWRIOD                                              
*                                                                               
PRBSPX   DS    0H                                                               
*                                                                               
*        CHECK IF STATION/SALESPERSONS BUDGET RECORDS ARE NEEDED                
*                                                                               
         LA    R2,REDATA           RESET POINTER                                
         LA    R0,L'REDATA         RESET COUNTER                                
*                                                                               
         CLI   0(R2),RBSSKTYQ      LOOK FOR STA/SAL BUDGET KEYWORD              
         BE    *+16                                                             
         LA    R2,1(R2)                                                         
         BCT   R0,*-12                                                          
         B     PRBSSX                                                           
*                                                                               
*        PROCESS STA/SAL BUDGET RECORDS                                         
*                                                                               
         MVI   REREAD,RBSSKTYQ                                                  
*                                                                               
         XC    REBDYTAB,REBDYTAB   INIT BUDGET YEAR TABLE                       
*                                                                               
         GOTO1 VRWIO,DMCB,RENWRIOD                                              
*                                                                               
PRBSSX   DS    0H                                                               
*                                                                               
         L     R4,AGLOBAL          ESTABLISH DRIVER GLOBAL STORAGE              
         USING GLOBALD,R4                                                       
*                                                                               
         MVI   GLMODE,GLOUTPUT     THEN PRINT THE REPORTS                       
         GOTO1 DRIVER,DMCB,(R4)                                                 
*                                                                               
*        RESET TO REQUESTING UTL                                                
*                                                                               
         CLI   REQSE,0             IF REQUESTING SE KNOWN                       
         BE    *+18                                                             
         ICM   RF,15,REUTLA           POINT TO UTL                              
         BZ    *+10                                                             
         MVC   4(1,RF),REQSE       RESET UTL TO REQUESTING SE                   
*                                                                               
*        CLOSE ANY OPEN BOXES                                                   
*                                                                               
         CLI   DOWNOPT,C'Y'        SKIP FOR DOWNLOAD REPORTS                    
         BE    PREPCLSX                                                         
*                                                                               
         CLI   PRNTSW,C' '         SKIP IF NO DATA FOR REPORT                   
         BNH   PREPCLSX                                                         
*                                                                               
         L     RF,ABOX             ESTABLISH BOX CONTROL BLOCK                  
         USING BOXD,RF                                                          
*                                                                               
         CLI   BOXYORN,C'Y'       SKIP BOX CLOSE IF NO BOXES USED               
         BNE   PREPCLSX                                                         
*                                                                               
         MVI   BOXREQ,C'C'         CLOSE LAST BOX                               
*                                                                               
         L     R1,GENHEAD          RESET SPOOL HEAD HOOK                        
         ST    R1,HEADHOOK                                                      
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT A LINE                                 
*                                                                               
PREPCLSX DS    0H                                                               
*                                                                               
*        FREE REQUESTED STORAGE                                                 
*                                                                               
*        CALCULATE SIZE OF GETMAIN AREA NEEDED                                  
*                                                                               
         L     RE,=AL4(NWMTENTL)   SIZE OF NEWMON TABLE SECTION                 
         SR    RF,RF                                                            
         IC    RF,REQAADFN         NUMBER OF AS-AT-DATE COLUMN FILTERS          
         LA    RF,1(RF)            ADD ONE FOR REST OF REPORT                   
         MR    RE,RE               SIZE OF GETMAIN NEEDED                       
*                                                                               
         GOTO1 =V(COVAIL),DMCB,C'FREE',REANWMON,(RF)                            
*                                                                               
*        RELEASE MONSTROUS BUFFER                                               
*                                                                               
         ICM   R3,15,REBUFFL       LENGTH OF OFFICE TABLE                       
         BZ    PRBUFFX             NONE USED SKIP                               
*                                                                               
         LA    R4,REBUFFA          BUFFER ADDRESS SAVEAREA                      
*                                                                               
         FREEMAIN EC,LV=(R3),A=(R4)  RELEASE CORE                               
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
         XC    REBUFFA,REBUFFA     RESET ADDRESS                                
*                                                                               
PRBUFFX  DS    0H                                                               
*                                                                               
         ICM   R3,15,RESVBBFL      LENGTH OF BUFFER                             
         BZ    PRTSVBCX            NONE USED SKIP                               
*                                                                               
         LA    R4,RESVBBFA         BUFFER ADDRESS SAVEAREA                      
*                                                                               
         FREEMAIN EC,LV=(R3),A=(R4)  RELEASE CORE                               
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
         XC    RESVBBFA,RESVBBFA   RESET ADDRESS                                
*                                                                               
PRTSVBCX DS    0H                                                               
*                                                                               
*        FREE UP PQINDEX TABLE                                                  
*                                                                               
         ICM   R3,15,APQINDEX      POINT TO PQINDEX TABLE                       
         BZ    PREPINXX            NOT THERE                                    
*                                                                               
         AHI   R3,-4               BACK UP TO START OF BUFFER                   
         L     R5,0(R3)            GET BUFFER LENGTH                            
*                                                                               
         GOTO1 =V(COVAIL),DMCB,C'FREE',(R3),(R5)                                
*                                                                               
PREPINXX DS    0H                                                               
*                                                                               
PREPX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'T83001 - REP WRITER APPLICATION - DATATAB'                      
***********************************************************************         
*                                                                     *         
*        TABLE TO ALLOCATE STORAGE FOR TABLES AND TO FILL THEM        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DATATAB  DS    0D                                                               
*                                                                               
DATABXLE DS    0A                                                               
         DC    A(DATASTRT)         A(TABLE START)                               
         DC    AL4(DATAENTL)       TABLE ENTRY LENGTH                           
         DC    A(DATAEND)          A(TABLE END)                                 
*                                                                               
         TITLE 'T83001 - REP WRITER APPLICATION - IOHOOK'                       
***********************************************************************         
*                                                                     *         
*        IOHOOK FOR CONTRACT AND OPTIONAL PWC RECS                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IOHOOK   NTR1  LABEL=*                                                          
*                                                                               
         CLI   REMODE,PROCREC      IF THERE IS SOMETHING INTERESTING            
         BNE   IOHOOKX                                                          
*                                                                               
         BAS   RE,SUBCON           MAY CONTROL AT SUB RECORD LEVEL              
*                                                                               
IOHOOKX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'T83001 - REP WRITER APPLICATION - SUBCON'                       
***********************************************************************         
*                                                                     *         
*              SUB RECORD CONTROL                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SUBCON   NTR1  LABEL=*                                                          
*                                                                               
         L     R6,REAREC           MUST MATCH ON RECORD                         
*                                                                               
         L     R4,AGLOBAL          ESTABLISH DRIVER WORKAREA                    
         USING GLOBALD,R4                                                       
*                                                                               
         MVI   GLMODE,GLINPUT                                                   
         XC    ATHISEL,ATHISEL     (NO SUB CONTROL)                             
*                                                                               
         GOTO1 DRIVER,DMCB,(R4)                                                 
*                                                                               
SUBCONX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         TITLE 'T83001 - REP WRITER APPLICATION -HOOK'                          
***********************************************************************         
*                                                                     *         
*        ANALYZE DRIVER APPLICATION HOOK                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HOOK     NTR1  LABEL=*                                                          
*                                                                               
         CLI   GLHOOK,GLHEAD       HEADLINE HOOK                                
         BE    HKHEAD                                                           
*                                                                               
         CLI   GLHOOK,GLPRINT      ABOUT TO PRINT A LINE HOOK                   
         BE    HKPRINT                                                          
*                                                                               
         CLI   GLHOOK,GLFIRST      FIRST TIME HOOK                              
         BE    HKFIRST                                                          
*                                                                               
         CLI   GLHOOK,GLINREC      START OF RECORD                              
         BE    HKINREC                                                          
*                                                                               
         B     HOOKX                                                            
*                                                                               
         TITLE 'T83001 - REP WRITER APPLICATION -HKHOOK'                        
***********************************************************************         
*                                                                     *         
*        HEADLINE HOOK                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HKHEAD   DS    0H                                                               
*                                                                               
         GOTO1 GENHEAD                                                          
*                                                                               
         B     HOOKX                                                            
*                                                                               
         TITLE 'T83001 - REP WRITER APPLICATION -HKPRINT'                       
***********************************************************************         
*                                                                     *         
*        ABOUT TO PRINT A LINE                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HKPRINT  DS    0H                                                               
*                                                                               
         CLI   SORTSW,C'X'         IF NO PRINT SWITCH IS ON                     
         BNE   HKPRINT1                                                         
*                                                                               
         MVI   GLHOOK,GLDONT          DON'T PRINT LINE                          
*                                                                               
         TM    REQCFOPT,REQCFBCK      IF DETAILS ARE TO BE BACKEDOUT            
         BNO   HKPRINT1                                                         
*                                                                               
         TM    GLINDS,GLTOTLIN        AND THIS IS NOT A TOTAL LINE              
         BO    HKPRINT1                                                         
*                                                                               
         MVI   GLHOOK,GLBCKOUT        SET APPROPRIATE RETURN CODE               
*                                                                               
HKPRINT1 DS    0H                                                               
*                                                                               
         MVI   SORTSW,0            CLEAR SWITCH                                 
*                                                                               
         GOTO1 =A(HKINX)           HANDLE PRINT QUEUE INIDCIES                  
*                                                                               
         CLI   GLHOOK,GLDONT       IF LINE IS ACTUALLY PRINTED                  
         BE    *+8                                                              
         MVI   PRNTSW,C'Y'            INDICATE RPT HAS AT LEAST 1 LINE          
*                                                                               
HKPRINTX DS    0H                                                               
         B     HOOKX                                                            
*                                                                               
         TITLE 'PRWRI01 - FIRST TIME CONTROLS - HKFIRST'                        
***********************************************************************         
*                                                                     *         
*        FIRST TIME CONTROLS                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HKFIRST  DS    0H                                                               
*                                                                               
*        DETERMINE WHICH KEYWORD IS BREAKING                                    
*                                                                               
HKFINX   DS    0H                                                               
*                                                                               
         ICM   RF,15,APQINDEX      POINT TO PQINDEX TABLE                       
         BZ    HKFINXX             SKIP IF NO TABLE PRESENT                     
*                                                                               
         USING PQINDEX,RF          ESTABLISH TABLE ENTRY                        
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,GLARGS         GET LEVEL BREAK                              
         BZ    HKFINXX             SKIP LEVEL 0 BREAKS                          
*                                                                               
         AHI   R1,-1               DECREMENT FOR INDEXING                       
         MHI   R1,PQINDXEQ         CALCULATE INDEX INTO TABLE                   
*                                                                               
         AR    RF,R1               POINT TO ENTRY IN PQINDEX TABLE              
*                                                                               
         OI    PQSTATUS,PQCHG      INDICATE THIS ROW HAS CHANGED                
         OI    NEWOPTS,NOPTCHGQ    INDICATE CHANGE SOMEWHERE                    
*                                                                               
         L     RE,GLADTENT         POINT TO OUTPUT DESCRIPTION                  
         USING DROD,RE             ESTABLLISH OUTPUT DESCRIPTION                
         MVC   PQAOUT,DROAPOS      SAVE OUTPUT ADDRESS                          
*                                                                               
         DROP  RE,RF                                                            
*                                                                               
HKFINXX  DS    0H                                                               
*                                                                               
HKFIRSTX B     HOOKX                                                            
*                                                                               
HOOKX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'T83001 - REP WRITER APPLICATION -HKINREC'                       
***********************************************************************         
*                                                                     *         
*        ABOUT TO START BUILDING A SORT RECORD                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HKINREC  DS    0H                                                               
*                                                                               
         MVI   SORTSW,0            RESET SORTSW                                 
*                                                                               
HKINRECX DS    0H                                                               
         B     HOOKX                                                            
*                                                                               
         TITLE 'T83001 - REP WRITER APPLICATION -LTORG'                         
***********************************************************************         
*                                                                     *         
*        LTORG                                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T83001 - REP WRITER APPLICATION -HKINX'                         
***********************************************************************         
*                                                                     *         
*        PRINT INDEX HEADERS ON FIRST LINE TO PRINT                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HKINX    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC                                                          
         USING SPOOLD,R8                                                        
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         USING CONHEADH-64,RA                                                   
*                                                                               
         USING RENWRIOD,R7                                                      
*                                                                               
         ST    R2,RELORW01         SAVE RELOCATION FACTOR                       
*                                                                               
         ICM   R3,15,APQINDEX      POINT TO PQINDEX TABLE                       
         BZ    HKINXX              SKIP IF NONE AVAILABLE                       
*                                                                               
         CLI   PRNTSW,C' '         SKIP IF NOT FIRST LINE OF REPORT             
         BH    HKINXHLX                                                         
*                                                                               
*        IF PQIX=Y, PRINT INDEX HEADER                                          
*                                                                               
         USING PQINDEX,R3          ESTABLISH PQINDEX TABLE ENTRY                
*                                                                               
         MVC   P,SPACES            INIT PRINT LINE                              
*                                                                               
         MVC   P(06),=C'<DECL>'    START OF HEADING DECLARATIONS                
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         MVC   P,SPACES                                                         
*                                                                               
         LA    R2,P                                                             
*                                                                               
         MVC   0(09,R2),=C'<REQNAME '                                           
         AHI   R2,9                                                             
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,REWNAMH+5      LENGTH OF REQUEST NAME                       
         BZ    *+22                NO ENTRY                                     
         AHI   RE,-1               DECREMENT FOR EXECUTE                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),REWNAM      PASS REQUEST FORMAT NAME                     
*                                                                               
         LA    R2,1(RE,R2)                                                      
         MVI   0(R2),C'>'                                                       
*                                                                               
         GOTO1 (RF),(R1)                                                        
         MVC   P,SPACES                                                         
*                                                                               
         LA    R2,P                                                             
         MVC   0(03,R2),=C'<IH '                                                
         LA    R2,4(R2)                                                         
*                                                                               
*        PASS HEADLINES FOR HEADLINES                                           
*                                                                               
         LA    R0,MAXROWS+MAXMIDS+MAXHEADS+MAXCOLS  MAX ENTRIES IN TAB          
*                                                                               
HKINXHLP DS    0H                                                               
*                                                                               
         OC    PQKEYWRD,PQKEYWRD   DONE WHEN TABLE EXHAUSTED                    
         BZ    HKINXHDN                                                         
*                                                                               
         CLI   PQPOSO,C'H'         ONLY INTERESTED IN HEADLINES                 
         BNE   HKINXHDN                                                         
*                                                                               
         BRAS  RE,IXFMT            ADD HEADLINE TO STRING                       
*                                                                               
HKINXHCN DS    0H                                                               
*                                                                               
         AHI   R3,PQINDXEQ         BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,HKINXHLP                                                      
*                                                                               
HKINXHDN DS    0H                                                               
*                                                                               
         MVI   0(R2),C'>'          CLOSE EXPRESSION                             
         GOTO1 (RF),(R1)           PRINT HEADLINES                              
         MVC   P,SPACES                                                         
*                                                                               
         LTR   R0,R0               DONE IF NO ENTRIES LEFT IN TABLE             
         BZ    HKINXHDX                                                         
*                                                                               
*        PASS HEADLINES FOR MIDLINES                                            
*                                                                               
*        CURRENTLY ONLY HEADLINES IN TABLE SO THIS IS MOOT                      
*                                                                               
         LA    R2,P                                                             
         MVC   0(03,R2),=C'<IM '   INDICATE MIDLINE HEADINGS                    
         LA    R2,4(R2)                                                         
*                                                                               
HKINXMLP DS    0H                                                               
*                                                                               
         OC    PQKEYWRD,PQKEYWRD   ANY MORE DATA?                               
         BZ    HKINXMDN             NO                                          
*                                                                               
         CLI   PQPOSO,C'M'         LOOKING FOR MIDLINES                         
         BE    *+8                                                              
         CLI   PQPOSO,C'R'         LOOKING FOR ROWS                             
         BNE   HKINXMDN            ELSE DONE                                    
*                                                                               
         BRAS  RE,IXFMT            ADD HEADLINES TO EXPRESSION                  
*                                                                               
HKINXMCN DS    0H                                                               
*                                                                               
         AHI   R3,PQINDXEQ         BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,HKINXMLP                                                      
*                                                                               
HKINXMDN DS    0H                                                               
*                                                                               
         MVI   0(R2),C'>'          CLOSE EXPRESSION                             
         GOTO1 (RF),(R1)           PRINT EXPRESSION                             
         MVC   P,SPACES                                                         
*                                                                               
         LTR   R0,R0               DONE IF NO ENTRIES LEFT IN TABLE             
         BZ    HKINXHDX                                                         
*                                                                               
*        PUT OUT HEADLINES FOR COLUMNS                                          
*                                                                               
         LA    R2,P                                                             
         MVC   0(03,R2),=C'<IC '   INDICATE COLUMNS NEXT                        
         LA    R2,4(R2)                                                         
*                                                                               
HKINXCLP DS    0H                                                               
*                                                                               
         OC    PQKEYWRD,PQKEYWRD   ANY MORE DATA?                               
         BZ    HKINXCDN             NO                                          
*                                                                               
         BRAS  RE,IXFMT            ADD HEADLINES TO EXPRESSION                  
*                                                                               
HKINXCCN DS    0H                                                               
*                                                                               
         AHI   R3,PQINDXEQ         BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,HKINXCLP                                                      
*                                                                               
HKINXCDN DS    0H                                                               
*                                                                               
         MVI   0(R2),C'>'          CLOSE EXPRESSION                             
         GOTO1 (RF),(R1)           PRINT EXPRESSION                             
         MVC   P,SPACES                                                         
*                                                                               
HKINXHDX DS    0H                                                               
*                                                                               
         CLI   DOWNOPT,C'Y'        DOWNLOAD?                                    
         BNE   HKINXDNX             NO                                          
*                                                                               
         MVC   P(10),=C'<FMT DATA>'                                             
         GOTO1 (RF),(R1)                                                        
         MVC   P,SPACES                                                         
         B     HKINXDNN                                                         
*                                                                               
HKINXDNX DS    0H                                                               
*                                                                               
         MVC   P(04),=C'<HL '      NUMBER OF HEADLINES                          
         EDIT  LASTHEAD,(2,P+4),FILL=0                                          
         MVI   P+6,C'>'                                                         
         GOTO1 (RF),(R1)                                                        
         MVC   P,SPACES                                                         
*                                                                               
HKINXDNN DS    0H                                                               
*                                                                               
         MVC   P(07),=C'</DECL>'   END OF HEADING DECLARATIONS                  
         GOTO1 (RF),(R1)                                                        
         MVC   P,SPACES                                                         
*                                                                               
         DROP  R3                                                               
*                                                                               
HKINXHLX DS    0H                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        PRINT DATA FOR LEVEL THAT CHANGED AND ALL LOWER              *         
*                                                                     *         
*        HEADLINES ONLY AT THIS POINT                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HKINXLV  DS    0H                                                               
*                                                                               
         TM    NEWOPTS,NOPTCHGQ    SKIP IF NO CHANGES AROUND                    
         BNO   HKINXLVX                                                         
*                                                                               
         NI    NEWOPTS,X'FF'-NOPTCHGQ TURN OFF CHANGE INDICATOR                 
*                                                                               
         CLI   DOWNOPT,C'Y'        IF WE ARE DOWNLOADING, NO DATA LINES         
         BE    HKINXLVX                                                         
*                                                                               
         ICM   R3,15,APQINDEX      POINT TO START OF PQINDEX TABLE              
         BZ    HKINXLVX            NO TABLE AVAILABLE                           
         USING PQINDEX,R3          ESTABLISH TABLE ENTRY                        
*                                                                               
         LA    R2,P                                                             
*                                                                               
         MVI   HALF,0                                                           
         LA    R1,1                INIT LEVEL COUNTER                           
         LA    R4,MAXHEADS         INDEX LINES ONLY FOR HEAD CHANGES            
         LA    R2,P                                                             
*                                                                               
HKINXLVL DS    0H                                                               
*                                                                               
         CLI   PQPOSO,C'H'         INDEX LINES ONLY FOR HEAD CHANGES            
         BNE   HKINXLVC                                                         
*                                                                               
         TM    PQSTATUS,PQCHG      HAS THIS KEY CHANGED?                        
         BNO   HKINXLVC             NO                                          
*                                                                               
         NI    PQSTATUS,X'FF'-PQCHG       RESET CHANGE FLAG                     
*                                                                               
         CLI   HALF,0              ANY ENTRIES YET?                             
         BNE   HKINXLV5             YES                                         
*                                                                               
         MVI   HALF,1              SET HAVE ONE NOW                             
*                                                                               
         MVC   0(06,R2),=C'<DATA '                                              
         AHI   R2,6                                                             
         EDIT  (R1),(2,0(R2)),FILL=0   01=                                      
         MVI   2(R2),C'='                                                       
         AHI   R2,3                                                             
*                                                                               
HKINXLV5 DS    0H                                                               
*                                                                               
*        PASS CODE VALUE OF HEADLINE                                            
*                                                                               
         L     RE,PQAOUT           GET A(OUTPUT SORT AREA)                      
         AHI   RE,L'LABLAREA+1     RE=CODE PORTION                              
         LA    RF,L'CODEAREA                                                    
         AHI   RF,-1                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RE)       MOVE DATA TO PRINT LINE                      
*                                                                               
         LA    R2,1(RF,R2)         GET LAST USED PRINT POSN                     
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVI   1(R2),X'5E'         SEMI-COLON                                   
         AHI   R2,2                                                             
*                                                                               
HKINXLVC DS    0H                                                               
*                                                                               
         AHI   R1,1                BUMP LEVEL COUNTER                           
         AHI   R3,PQINDXEQ         BUMP TO NEXT TABLE ENTRY                     
         BCT   R4,HKINXLVL                                                      
*                                                                               
         DROP  R3                                                               
*                                                                               
HKINXLVD DS    0H                                                               
*                                                                               
         CLI   HALF,1              SKIP IF NOTHING PRINTED                      
         BNE   HKINXLVX                                                         
*                                                                               
         MVI   0(R2),C'>'                                                       
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         MVC   P,SPACES                                                         
*                                                                               
HKINXLVX DS    0H                                                               
*                                                                               
HKINXX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRWRI01 - FORMAT INDEX DATA - IXFMT'                            
***********************************************************************         
*                                                                     *         
* FORMAT INDEX DATA                                                   *         
*                                                                     *         
*   INPUT    R2 = A(FORMAT AREA)                                      *         
*            R3 = A(PQINDEX ENTRY)                                    *         
*                                                                     *         
*   RETURN   FORMATTED DATA, FOLLOWED BY A SEMI-COLON                 *         
*            R2 = A(NEXT BLANK SPACE)                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IXFMT    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC                                                          
         USING SPOOLD,R8                                                        
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         USING CONHEADH-64,RA                                                   
*                                                                               
         USING PQINDEX,R3          ESTABLISH INDEX TABLE ENTRY                  
*                                                                               
         MVC   0(8,R2),PQKEYWRD    DISPLAY KEYWORD                              
*                                                                               
         AHI   R2,8                POINT TO END OF KEYWORD                      
         CLI   0(R2),C' '          FIND LAST NON-BLANK IN KEYWORD               
         BH    *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
         CLC   PQHEAD1(96),SPACES   SKIP IF NO HEADLINES                        
         BNH   IXFMTDN                                                          
*                                                                               
*        PUT OUT HEADLINES                                                      
*                                                                               
         LA    R1,PQHEAD1          POINT TO HEADER                              
         LA    RE,95               EXECUTE LENGTH                               
         MVC   1(2,R2),=C'="'                                                   
         AHI   R2,3                                                             
         LA    R0,4                MAX HEADERS                                  
*                                                                               
IXFMTLP  DS    0H                                                               
*                                                                               
         MVC   0(24,R2),0(R1)      MOVE OUT HEADER                              
         OC    0(24,R2),SPACES     MAKE UPPER CASE                              
*                                                                               
         AHI   R2,24               END OF HEADER                                
*                                                                               
         CLI   0(R2),C' '          BACK UP TO LAST CHAR                         
         BH    *+8                                                              
         BCT   R2,*-8                                                           
*                                                                               
         MVI   1(R2),C'"'          CLOSE QUOTE                                  
         AHI   R2,1                                                             
*                                                                               
         BCT   R0,*+8                                                           
         B     IXFMTDN                                                          
*                                                                               
IXFMTCN  DS    0H                                                               
*                                                                               
         AHI   R1,24               NEXT HEADER                                  
         AHI   RE,-24              L'REMAINING HEADERS                          
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),SPACES      ANY LEFT?                                    
         BNH   IXFMTDN                                                          
*                                                                               
         MVC   1(2,R2),=C',"'                                                   
         AHI   R2,3                                                             
         B     IXFMTLP                                                          
*                                                                               
IXFMTDN  DS    0H                                                               
*                                                                               
         MVI   1(R2),X'5E'         SEMICOLON                                    
         AHI   R2,2                                                             
*                                                                               
IXFMTX   DS    0H                                                               
         XIT1  REGS=(R2)                                                        
*                                                                               
         DROP  R3                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRI01 - FORMAT INDEX DATA - DATASTRT'                         
***********************************************************************         
*                                                                     *         
*        TABLE FOR DATA DESCRIPTION                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         PRINT GEN                                                              
*                                                                               
DATASTRT DS    0D                  START OF TABLE                               
*                                                                               
         DTTAB ADV,ADVERTISER,SOON=3                                            
*                                                                               
DATAENTL EQU   *-DATASTRT          ENTRY LENGTH                                 
*                                                                               
***      DTTAB AGY,AGENCY/OFFICE,SOON=2                                         
         DTTAB AG2,AGY/OFF/EXTENSION,SOON=2                                     
         DTTAB BCD,BUYCODE                                                      
         DTTAB CLS,CLASS                                                        
         DTTAB COM,COMMISSION/RATE                                              
         DTTAB CTG,CATEGORY                                                     
         DTTAB CTY,CONTRACT/TYPE                                                
         DTTAB DCT,DEVELOP/TYPE                                                 
         DTTAB DSP,DEVELOP/PERSON                                               
         DTTAB GRP,GROUP/SUBGRP                                                 
         DTTAB MKT,MARKET                                                       
         DTTAB OFF,OFFICE                                                       
         DTTAB OWN,OWNER                                                        
         DTTAB PRD,PRODUCT,SOON=2                                               
         DTTAB PTP,POINT/PERSON                                                 
         DTTAB REG,REGION                                                       
         DTTAB REP,REP                                                          
         DTTAB SAL,SALESPERSON                                                  
         DTTAB STA,STATION                                                      
         DTTAB TEM,DIV/TEAM                                                     
         DTTAB TER,TERRITORY                                                    
         DTTAB SJL,JOIN/LEAVE                                                   
*                                                                               
DATAEND  EQU   *-1                 END OF TABLE                                 
         PRINT NOGEN                                                            
*                                                                               
         TITLE 'T83001 - REP WRITER APPLICATION -DATATABD'                      
***********************************************************************         
*                                                                     *         
*        DSECT FOR TABLE THAT ALLOCATES STORAGE TO VARIOUS TABLES     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DATATABD DSECT                                                                  
DTBRECID DS    X                   RECORD ID FOR TABLE                          
         DS    XL3                 SPARE                                        
DTBTABID DS    CL8                 TABLE IDENTIFIER                             
DTBSTRTA DS    A                   A(START OF TABLE)                            
DTBTABLN DS    A                   LENGTH OF TABLE                              
DTBTABSL DS    A                   LENGTH OF TABLE FOR F DTBTABLN FIRST         
*                                                                               
DTBENTL  EQU   *-DATATABD          LENGTH OF ENTRY IN TABLE                     
*                                                                               
         TITLE 'T83001 - REP WRITER APPLICATION -RENWRWORKD'                    
***********************************************************************         
*                                                                     *         
*        WRITER WORKING STORAGE                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
       ++INCLUDE RENWRWORKD                                                     
*                                                                               
         TITLE 'T83001 - REP WRITER APPLICATION -DSCECTS'                       
***********************************************************************         
*                                                                     *         
*              OTHER DSECTS ARE HIDDEN IN HERE                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         PRINT ON                                                               
*RENWRFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE RENWRFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE RENWRF1D                                                       
*                                                                               
READRECS DS    XL1                                                              
         ORG   CONHEAD-64+X'0D00'                                               
TWRFLTR  DS    XL4                 FILTER FOR STORED FORMAT                     
       ++INCLUDE DDGENTWA                                                       
*                                                                               
*RENWRDSCTS                                                                     
         PRINT OFF                                                              
       ++INCLUDE RENWRDSCTS                                                     
         PRINT ON                                                               
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*REGLOBEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE RENWREQUS                                                      
         PRINT ON                                                               
*DDMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*DDREMOTED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
*REGENALL                                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENALL1                                                      
         PRINT ON                                                               
*REGENDCT                                                                       
         PRINT OFF                                                              
REDCTRCD DSECT                                                                  
       ++INCLUDE REGENDCT                                                       
         PRINT ON                                                               
*REGENDSP                                                                       
         PRINT OFF                                                              
REDSPRCD DSECT                                                                  
       ++INCLUDE REGENDSP                                                       
         PRINT ON                                                               
*REGENPWC                                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENPWC                                                       
         PRINT ON                                                               
*DRGLOBAL                                                                       
         PRINT OFF                                                              
       ++INCLUDE DRGLOBAL                                                       
         PRINT ON                                                               
*DRIVETABLE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DRIVETABLE                                                     
         PRINT ON                                                               
*FAFACTS                                                                        
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
*FAPQPL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAPQPL                                                         
         PRINT ON                                                               
*FATIOB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045RENWR01S  12/17/03'                                      
         END                                                                    
