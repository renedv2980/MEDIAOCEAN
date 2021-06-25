*          DATA SET RERIS01S   AT LEVEL 078 AS OF 05/01/02                      
*          DATA SET RERIS01    AT LEVEL 036 AS OF 04/24/00                      
*PHASE T80D01A,*                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE PERVERT                                                                
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE SCANNER                                                                
*INCLUDE ALTBCMON                                                               
         TITLE 'RERIS01 - T80D01 - RIS VALIDATE INPUT HEADER'                   
*                                                                               
*********************************************************************           
*                                                                   *           
*        RERIS01 --- RIS/ON-LINE CONTRACT INFORMATION VALIDATER     *           
*                                                                   *           
* ----------------------------------------------------------------- *           
*                                                                   *           
* REP PROGRAM CONTROL BITS (SVPGPBIT)                               *           
*                                                                   *           
* 0 - IF SINGLE STATION, REQUIRE OR NOT ONE OTHER SINGLE FILTER     *           
* 1 - IF STATION GROUP, REQUIRE OR NOT TWO OTHER FILTERS            *           
* 2 - IF SINGLE SALESPERSON, REQUIRE OR NOT ADVERTISER OR AGENCY    *           
* 3 - ALTERNATE DOLLAR DISPLAY DEFAULT VALUE:  BOOKED, NOT ESTIMATE *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 02/13/89 (MRR) --- ADD 'OLIST' OPTION (LISTS TRAFFIC #, AGY # AND *           
*  (LEVEL 40)         ADV # IN LIEU OF OTHER STUFF IS DIS MID-LINE. *           
*                                                                   *           
* JUL18/89 (PJS) --- ALLOW SHORT BAND INPUT                         *           
*                    (ACCEPT -F, -A, OR -C FOR -FM, -AM, -CM)       *           
*                                                                   *           
*                --- CHECK FOR EXTRA INPUT AFTER 'SHORT' CALL       *           
*                    LETTERS (IE: WOR-AMX)                          *           
*                                                                   *           
*                --- ALLOW SINGLE STATION & 'ALL' EVERYWHERE ELSE   *           
*                                                                   *           
* 08/02/89  PJS  RELEASE SINGLE STATION RESTRICTIONS UNLESS         *           
*                REP CODE IS 'BL' (BLAIR)                           *           
*                                                                   *           
* DEC22/89 (MRR) --- REMOVE SATURDAY RUNNING RESTRICTION            *           
*                                                                   *           
* JAN16/90 (MRR) - USE REP RECORD PROFILES TO CONTROL USE OF 'ALL'  *           
*                   AS SPECIFIED ABOVE                              *           
*                - MERGE RERIS11 INTO THIS MODULE                   *           
*                                                                   *           
* 02/08/90  PJS  - GIVE SPECIFIC MESSAGES ON PROFILE VIOLATIONS     *           
*                                                                   *           
* FEB12/90 (MRR) --- A LIST COMMAND IN THE OPTIONS FIELD IS STILL   *           
*                     A 'LIST' VIS-A-VIE FILTER CHECKING            *           
*                                                                   *           
* FEB24/92 (BU ) --- ALTERNATE DOLLAR DISPLAY                       *           
*                                                                   *           
* SEP02/92 (BU ) --- ADJUST PRIOR WEEK DATES IN ACCORDANCE WITH RRG *           
*                                                                   *           
* DEC03/92 (BU ) --- COMBO STATION PROCESSING:  CHECK REP CODE FOR  *           
*                    PARTICIPATION, LOAD PARTICIPATING STATIONS,    *           
*                    FORCE MULTI-SINGLE-STATION CYCLE.              *           
*                                                                   *           
* MAY06/93 (BU ) --- FIX 'INVALID STATION' ERROR                    *           
*                                                                   *           
* MAY21/93 (BU ) --- SAVE/RESET DISPLACEMENT OF 'ACMBOSTA'          *           
*                                                                   *           
* NOV11/93 (BU ) --- OVERRIDE OFFICE REQUIREMENT ON LIST$ IF        *           
*                    'LIST$DDS' ENTERED                             *           
*                                                                   *           
* DEC02/93 (BU ) --- ADD 'D4' TO COMBO REP LIST                     *           
*                                                                   *           
* DEC07/93 (BU ) --- COMBO STATION REQUEST:  SKIP NON-PARTICIPATING *           
*                    STATION.                                       *           
*                                                                   *           
* AUG05/94 (BU ) --- DON'T TEST 'ZZ' IF PROFILE BIT SET             *           
*                                                                   *           
* MAY24/95 (BU ) --- ADD 'S1' AND 'IF' TO COMBO REP LIST            *           
*                                                                   *           
* JUN01/95 (BU ) --- ADD 'CM' TO COMBO REP LIST                     *           
*                                                                   *           
* OCT26/95 (SKU) --- ADD K9 AND RT TO COMBO PROCESS LIST            *           
*                                                                   *           
* JAN05/96 (BU ) --- ADD 'S3' TO COMBO REP LIST                     *           
*                                                                   *           
* FEB26/96 (BU ) --- ADD 'CN' TO COMBO REP LIST                     *           
*                                                                   *           
* JUL19/96 (BU ) --- ADD 'AQ' TO COMBO REP LIST                     *           
*                                                                   *           
* NOV13/96 (RHV) --- SWAPPING TO BROWSE                             *           
*                                                                   *           
* <*** DARK TIMES ***>                                              *           
*                                                                   *           
* AUG05/97 (JRD) --- SLIGHT REORGANIZATION, PROBABLY FOR THE WORST  *           
*                    NEW OPTIONS(D, -D, DS=, DT=, P, -P, F, -F)     *           
*                                                                   *           
* AUG08/97 (JRD) --- SETS AND STATION MARKETS                       *           
*                    COMMENT OUT ALL VALIDATED BIT CHECKS. NOT USED *           
*                      ANYWAY SINCE THEY GET CLEARED IN THE 00      *           
*                                                                   *           
* OCT24/97 (BU ) --- RERISWRKB --> RERISWRKC                          *         
*                    RGENEROL INCLUDED EXPLICITLY                     *         
*                                                                   *           
* FEB20/98 (BU ) --- ADD MEDIA = L FOR TV STATIONS                  *           
*                                                                   *           
* MAR30/98 (BU ) --- ADD L7/CABALLERO TV                            *           
*                                                                   *           
* MAY19/98 (BU ) --- ADD IB/ABC RADIO                               *           
*                                                                   *           
* DEC09/98 (BU ) --- PERMIT S/P TEAM FILTERING (FOR TEAM OVERRIDE)  *           
*                                                                   *           
* JAN12/99 (BU ) --- FIX YEAR-2000 BUG                              *           
*                                                                   *           
* MAR25/99 (BU ) --- ADD 'NX' TO COMBO REP LIST                     *           
*                                                                   *           
* APR13/99 (BU ) --- ADD 'UO' TO COMBO REP LIST                     *           
*                                                                   *           
* MAY17/99 (BU ) --- ADD 'V5' TO COMBO REP LIST                     *           
*                                                                   *           
* AUG16/99 (BU ) --- ADD 'QD' TO COMBO REP LIST                     *           
*                                                                   *           
* DEC06/99 (BU ) --- ADD 'NU' TO COMBO REP LIST                     *           
*                                                                   *           
* JAN13/00 (RHV) --- PAR SECURITY                                   *           
*                                                                   *           
* APR24/00 (BU ) --- ADD 'G8' TO COMBO REP LIST                     *           
*                                                                   *           
* JUL19/00 (BU ) --- FIX SETS FOR MASTER/SUBSIDIARY                 *           
*                                                                   *           
* MAR14/02 (HQ ) --- ADD PRODUCT CODE AND 3 CHAR PRODUCT NAME FILTER*           
*                    FOR EXAMPLE, PLIST,P=FOR                       *           
*                                                                   *           
*                    **  END TOMBSTONE  **                          *           
*********************************************************************           
*                                                                               
T80D01   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*0D01*,R9,RR=RE                                                
         L     RC,0(R1)                                                         
         USING T80DFFD,RA                                                       
         USING GENOLD,RC                                                        
         ST    RE,RELO1            SAVE RELOCATION FACTOR                       
**********************                                                          
* SET INITIAL VALUES *                                                          
**********************                                                          
*                                                                               
         LR    RE,RA               CALCULATE A(STATION STORAGE FIELD)           
         AH    RE,=Y(STASTORD)                                                  
         ST    RE,ASTASTOR         SET A(STATION STORAGE FIELD)                 
*                                     RESET FOR EACH PASS, IN CASE THIS         
*                                     MODULE LOADS IN ANOTHER WORKSPACE         
*                                                                               
         XC    LISTBYTE,LISTBYTE   LISTING FLAGS                                
         XC    NUMLIST,NUMLIST     NUMBER OF SINGLE LISTERS                     
         XC    NUMFILT,NUMFILT     NUMBER OF SINGLE FILTERS                     
         XC    SINGSTA,SINGSTA     SINGLE STATION FLAG                          
         XC    SINGSAL,SINGSAL     SINGLE SALESPERSON FLAG                      
         XC    ADVORAGY,ADVORAGY   ADV OR AGY FILTER ENTERED                    
         MVI   FILTFLG,0                                                        
         ZAP   DLRTOTAL,=P'0'                                                   
         ZAP   SHRTOT,=P'0'                                                     
         ZAP   ALLSHR,=P'0'                                                     
*                                                                               
         LA    R1,STATOTAL         PREPARE STATOTAL AREAS                       
         LA    RE,6                                                             
ZAPIT    ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   RE,ZAPIT                                                         
*                                                                               
         XC    CONTOT,CONTOT                                                    
         XC    SHRPCT,SHRPCT                                                    
         MVI   RISTATUS,0                                                       
         MVI   VIEWS,0                                                          
         MVI   VIEWS3,0                                                         
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(SETDISPS)                                                  
         USING SETDISPD,RE                                                      
         XC    SETSTAS,SETSTAS                                                  
         XC    SETADVS,SETADVS                                                  
         XC    SETAGYS,SETAGYS                                                  
         XC    SETCTYPE,SETCTYPE                                                
         XC    SETSALS,SETSALS                                                  
         XC    SETOFFS,SETOFFS                                                  
         XC    SETGRPS,SETGRPS                                                  
         MVC   NEXTSETD,=A(FIRSTSET)                                            
         DROP  RE                                                               
         LR    RE,RA                                                            
         A     RE,=A(FIRSTSET)     CLEAR START OF TABLES                        
         XC    0(10,RE),0(RE)                                                   
         EJECT                                                                  
********************                                                            
* VALIDATE STATION *                                                            
********************                                                            
VALSTATN LA    R2,RISSTAH                                                       
         L     RE,ASTASTOR         SET A(STATION STORAGE AREA)                  
         XC    0(251,RE),0(RE)     CLEAR ALL 251 CHARS                          
****     XC    CMBOSTAS,CMBOSTAS   RESET LIST OF COMBO STATIONS                 
         MVI   CMBOREP,C'N'        AND CLEAR CMBOREP FLAG                       
*                                                                               
         MVC   TBLGRP(7),ALL                                                    
         XC    NEXTBYTE,NEXTBYTE                                                
         LA    R3,FLDERR           TEST NO INPUT                                
         CLI   5(R2),0                                                          
         BE    VSTAERR                                                          
*                                                                               
         USING TWAD,R4             VALUE LEFT AFTER A 'GETMSG' CALL             
         LR    R4,RA                  FOR ERROR IS WRONG - CLEAR IT             
         CLC   TWAACCS,=X'00000001'                                             
         BNE   *+10                NO                                           
         XC    TWAACCS,TWAACCS     YES - CLEAR IT OUT                           
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    VALS0040                                                         
         OC    TWAACCS,TWAACCS                                                  
         BZ    VALS0040            NO RESTRICTION FOR THIS ID                   
         CLC   TWAACCS(2),=C'O='                                                
         BE    VALS0040            OFFICE RESTRICTION                           
         CLI   TWAACCS,C'$'                                                     
         BNE   VALS0020                                                         
         CLI   RISSTA,C'*'         SET REQUEST?                                 
         BE    VALS0080                                                         
         B     VALS0180            STATION LIMITED ACCESS                       
                                                                                
VALS0020 CLC   TWAACCS,8(R2)       MUST BE STATION                              
         BE    VALS0040                                                         
         LA    R3,STAERR                                                        
         B     VSTAERR                                                          
*                                                                               
VALS0040 CLC   RISSTA(2),=C'G-'    GROUP REQUEST?                               
         BNE   VALS0060            NO                                           
         GOTO1 =A(VALGRP),RR=Y                                                  
         BNE   VSTAERR             ERROR                                        
         B     VALS0320                                                         
*                                                                               
VALS0060 DS    0H                                                               
         CLC   RISSTA(2),=C'M='    MARKET REQUEST?                              
         BNE   VALS0080            NO                                           
         GOTO1 =A(VALMKT),RR=Y                                                  
         BE    VALS0320            NO ERROR                                     
         L     R3,DUB              RESET ERROR CODE                             
         B     VMKTERR             ERROR                                        
VALS0080 DS    0H                                                               
         LA    R3,STAERR                                                        
         CLI   RISSTA,C'*'         SET REQUEST?                                 
         BNE   VALS0160            NO                                           
         CLC   =C'$GAN',RISSTA+1   ..IF $GAN                                    
         BNE   VALS0100                                                         
         CLC   =C'BL',REPALPHA     ..AND BL                                     
         BE    VALS0120            .. NO CHECKSET                               
         CLC   =C'PV',REPALPHA     ..OR  PV                                     
         BE    VALS0120            .. NOCHECKSET                                
VALS0100 GOTO1 =A(VALSET),DMCB,=C'ST',=A(STADISP),RISSTA+1,RR=Y                 
         BNE   VSTAERR                                                          
                                                                                
VALS0120 CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    VALS0140            OK                                           
         CLI   TWAACCS,C'$'        STATION ?                                    
         BNE   VALS0140            OK                                           
         LA    R3,55               SECURITY LOCKOUT                             
         GOTO1 =A(CHKSET),DMCB,(RA),RR=Y                                        
         BNE   VSTAERR                                                          
VALS0140 EQU   *                                                                
                                                                                
*PFOL    MVI   LISTBYTE,LISTSTA  <- THEY DON'T LIKE SAFE LIMITS                 
         MVC   TBLSTA,ALL                                                       
         XC    RISSTAN,RISSTAN                                                  
         B     VALS0320                                                         
*                                                                               
         CLC   RISSTA(3),=C'ALL'   TEST ALL OPTION                              
         BE    VSTAERR             -DISALLOWED                                  
*                                                                               
VALS0160 CLC   RISSTA(4),=C'LIST'                                               
         BNE   VALS0180                                                         
         MVI   LISTBYTE,LISTSTA                                                 
         MVC   TBLSTA,ALL                                                       
         XC    RISSTAN,RISSTAN                                                  
         B     VALS0320                                                         
*                                                                               
VALS0180 MVI   KEY,2               SET KEY                                      
         XC    KEY+1(19),KEY+1                                                  
         MVC   KEY+20(2),REPALPHA                                               
         OC    RISSTA,SPACES                                                    
         BAS   RE,DASH                                                          
         LTR   R7,R7                                                            
         BNZ   *+8                                                              
         LA    R7,4                                                             
*                                                                               
*- SPECIAL TEST FOR SHORT CALL LETTER STATIONS:                                 
*  PREVENT 'EXTRA' GARBAGE AT END OF LINE (IE: WOR-AMX)                         
         LA    RE,4                                                             
         CR    R7,RE               CALL LETTER LEN -VS- 4                       
         BNL   VALS0220            NOT SHORT....NO TEST NEEDED                  
*                                                                               
         SR    RE,R7               RE=1,2, OR 3 BYTES TO CHECK                  
         LA    R8,8+3(R7,R2)       R8=A(1ST BYTE AFTER BAND)                    
VALS0200 CLI   0(R8),C' '                                                       
         BNE   VSTAERR             NON-BLANK = ERROR                            
         LA    R8,1(R8)                                                         
         BCT   RE,VALS0200                                                      
*                                                                               
VALS0220 EQU   *                                                                
         BCTR  R7,0                                                             
         MVC   KEY+22(4),SPACES                                                 
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   KEY+22(0),RISSTA                                                 
         LA    R7,10(R7,R2)        POINT R7 TO MEDIA PART OF INPUT              
         MVI   KEY+26,C' '                                                      
         CLC   0(2,R7),SPACES                                                   
         BE    VALS0240                                                         
         CLC   0(2,R7),=C'TV'                                                   
         BE    VALS0240            TV                                           
         MVI   KEY+26,C'L'                                                      
         CLC   0(2,R7),=C'L '                                                   
         BE    VALS0240            LOW-POWER TV                                 
*                                                                               
         MVI   KEY+26,C'A'                                                      
         CLC   0(2,R7),=C'AM'                                                   
         BE    VALS0240            AM                                           
*                                                                               
         CLC   0(2,R7),=C'A '      ALSO AM                                      
         BE    VALS0240                                                         
*                                                                               
         MVI   KEY+26,C'F'                                                      
         CLC   0(2,R7),=C'FM'                                                   
         BE    VALS0240            FM                                           
*                                                                               
         CLC   0(2,R7),=C'F '      ALSO FM                                      
         BE    VALS0240                                                         
*                                                                               
         MVI   KEY+26,C'C'                                                      
         CLC   0(2,R7),=C'CM'                                                   
         BE    VALS0240            CM- COMBINED STATIONS                        
*                                                                               
         CLC   0(2,R7),=C'C '      ALSO CM                                      
         BNE   VSTAERR                                                          
*                                                                               
VALS0240 EQU   *                                                                
         MVI   SINGSTA,1           SET SINGLE STATION FLAG                      
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE     TEST POINTER FOUND                           
         BNE   VSTAERR                                                          
*                                                                               
         BAS   RE,GETREC                                                        
         GOTO1 =A(ACTIVSTA),RR=Y   SET ACTIVE STATION, IF MASTER                
*                                                                               
         MVC   STALETS,RSTAKSTA    SAVE STATION CALL LETTERS                    
         CLI   TWAACCS,C'$'        TEST FOR STATION LIMITED ACCESS              
         BNE   VALS0300                                                         
         LA    R3,55               SECURITY LOCKOUT                             
         LA    R5,RSTAREC                                                       
         SR    R7,R7                                                            
         ICM   R7,3,RSTALEN                                                     
         AR    R7,R5                                                            
         BCTR  R7,0         R7 = A(LAST BYTE OF STATION REC), FOR BXLE          
         LA    R5,RSTAELEM                                                      
         SR    R6,R6                                                            
*                                                                               
VALS0260 CLI   0(R5),6      SEARCH FOR ELEMENT FOR VALID SIGNON ID'S            
         BNE   VALS0280                                                         
         USING RSTASOEL,R5                                                      
         CLC   TWAUSRID,RSTASID   COMPARE USER ID TO STATION SIGN ON ID         
         BE    VALS0300           OK                                            
         DROP  R4                                                               
*                                                                               
VALS0280 IC    R6,1(R5)                                                         
         BXLE  R5,R6,VALS0260                                                   
         B     VSTAERR            NOT OK                                        
         DROP  R5                                                               
*                                                                               
VALS0300 MVC   RISSTAN,RSTAMKT                                                  
         MVC   TBLSTA,RSTAKSTA                                                  
         MVC   TBLGRP(2),RSTAGRUP                                               
*                                                                               
VALS0320 EQU   *                                                                
         CLI   RSTAKSTA+4,C'C'     COMBO STATION?                               
         BNE   VALS0340            NO                                           
*                                  YES - CHECK REP TO SEE IF                    
*                                     COMBO PROCESSING NEEDED                   
*                                                                               
         GOTO1 =A(CHEKCMBO),DMCB,(RC),RR=Y                                      
*                                                                               
VALS0340 EQU   *                                                                
         LA    R4,RISSTANH                                                      
         FOUT  (R4)                                                             
         OI    4(R2),X'20'         SET PREV VALID                               
*                                                                               
VALS0900 DS    0H                                                               
         EJECT                                                                  
*******************                                                             
* VALIDATE OFFICE *                                                             
*******************                                                             
         LA    R2,RISOFFH          SET CURSOR                                   
**JRD    TM    4(R2),X'20'         TEST ALREADY VALID                           
**JRD    BO    VALOFFX                                                          
*                                                                               
         XC    NEXTBYTE,NEXTBYTE                                                
         LA    R3,FLDERR           TEST NO INPUT                                
         CLI   5(R2),0                                                          
         BE    VALOFF0                                                          
*                                                                               
         LA    R3,OFFERR                                                        
         CLI   RISOFF,C'*'         SET REQUEST?                                 
         BNE   VALOFFB             NO                                           
*                                                                               
         LR    R4,RA                                                            
         USING TWAD,R4                                                          
         CLI   TWAACCS,C'$'        .IF STATION                                  
         BE    *+12                .ACCES TO ALL OFFICES                        
         TM    12(R4),X'80'        TEST IF TERMINAL ALLOWED ACCESS              
         BO    *+12                                                             
         CLI   1(R4),C'*'          TEST FOR DDS TERMINAL                        
         BNE   VOFFERR                                                          
         DROP  R4                                                               
*                                                                               
         GOTO1 =A(VALSET),DMCB,=C'OF',=A(OFFDISP),RISOFF+1,RR=Y                 
         BNE   VOFFERR                                                          
         MVC   TBLOFF,ALL                                                       
         B     VALOFF4                                                          
*                                                                               
VALOFFB  CLC   RISOFF(3),=C'ALL'   TEST ALL OPTION                              
         BNE   VALOFF1                                                          
*                                                                               
VALOFF0  LR    R4,RA                                                            
         USING TWAD,R4                                                          
         CLI   TWAACCS,C'$'        .IF STATION                                  
         BE    VOFFALL             .ACCES TO ALL OFFICES                        
         DROP  R4                                                               
         TM    12(RA),X'80'        TEST IF TERMINAL ALLOWED ACCESS              
         BO    *+12                                                             
         CLI   1(RA),C'*'          TEST FOR DDS TERMINAL                        
         BNE   VOFFERR                                                          
*                                                                               
VOFFALL  MVC   TBLOFF,ALL                                                       
         XC    RISOFFN,RISOFFN                                                  
         B     VALOFF4                                                          
*                                                                               
VALOFF1  EQU   *                                                                
         CLC   RISOFF(4),=C'LIST'  TEST LIST OPTION                             
         BNE   VALOFF2                                                          
         TM    12(RA),X'80'        TEST IF TERMINAL ALLOWED ACCESS              
         BO    *+12                                                             
         CLI   1(RA),C'*'          OR IF DDS TERMINAL                           
         BNE   VOFFERR                                                          
         CLI   LISTBYTE,0          TEST 2 LIST OPTIONS                          
         BNE   ERRLIST                                                          
         MVI   LISTBYTE,LISTOFF                                                 
         MVC   TBLOFF,ALL                                                       
         XC    RISOFFN,RISOFFN                                                  
         ZIC   RF,NUMLIST                                                       
         LA    RF,1(RF)                                                         
         STC   RF,NUMLIST                                                       
         B     VALOFF4                                                          
*                                                                               
VALOFF2  EQU   *                                                                
         CLI   5(R2),2             TEST LENGTH ERROR                            
         BH    VOFFERR                                                          
         TM    12(RA),X'80'        TEST IF TERMINAL ALLOWED ACCESS              
         BO    VALOFF3                                                          
         CLI   1(RA),C'*'          OF IF DDS TERMINAL                           
         BE    VALOFF3                                                          
*                                                                               
         USING TWAD,R4                                                          
         LR    R4,RA                                                            
         CLC   TWAACCS(2),=C'O='   OFFICE RESTRICTION                           
         BNE   VALOFF3                                                          
         CLC   TWAACCS+2(2),RISOFF  OFFICE MUST MATCH                           
         BNE   VOFFERR                                                          
         DROP  R4                                                               
*                                  BUILD KEY, GET OFFICE                        
VALOFF3  MVI   KEY,4                                                            
         XC    KEY+1(22),KEY+1                                                  
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),RISOFF                                                 
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VOFFERR                                                          
*                                                                               
         BAS   RE,GETREC                                                        
         MVC   RISOFFN,ROFFNAME                                                 
         MVC   TBLOFF,ROFFKOFF                                                  
         ZIC   RF,NUMFILT                                                       
         LA    RF,1(RF)                                                         
         STC   RF,NUMFILT                                                       
*                                                                               
VALOFF4  LA    R4,RISOFFNH                                                      
         FOUT  (R4)                                                             
         OI    4(R2),X'20'         SET 'PREV VALIDATED'                         
*                                                                               
VALOFFX  DS    0H                                                               
         EJECT                                                                  
*******************                                                             
* VALIDATE AGENCY *                                                             
*******************                                                             
         LA    R2,RISAGYH                                                       
**JRD    TM    4(R2),X'20'         TEST ALREADY VALID                           
**JRD    BO    VALAGYX                                                          
*                                                                               
         CLI   8(R2),C'='          BROWSE REQUEST?                              
         BNE   VALAGYA                                                          
         GOTO1 (RFBROWSE,VREPFACS),DMCB,ACOMFACS,BASERD,(R2),0,        +        
               (0,C' AGY'),0                                                    
         DC    H'0'           BROWSE SHOULD HAVE TAKEN IT FROM HERE             
*                                                                               
VALAGYA  DS    0H                                                               
         XC    NEXTBYTE,NEXTBYTE                                                
         LA    R3,FLDERR           TEST NO INPUT                                
         CLI   5(R2),0                                                          
         BE    VALAGY0                                                          
*                                                                               
         LA    R3,AGYERR                                                        
         CLI   RISAGY,C'*'         SET REQUEST?                                 
         BNE   VALAGYB             NO                                           
         GOTO1 =A(VALSET),DMCB,=C'AG',=A(AGYDISP),RISAGY+1,RR=Y                 
         BNE   VAGYERR                                                          
         MVC   TBLAOFF,ALL                                                      
         MVC   TBLAGY,ALL                                                       
         B     VALAGY4                                                          
*                                                                               
VALAGYB  DS    0H                                                               
         CLC   RISAGY(3),=C'ALL'   TEST ALL OPTION                              
         BNE   VALAGY1                                                          
         CLI   5(R2),3                                                          
         BH    VALAGY2                                                          
*                                                                               
VALAGY0  MVC   TBLAOFF,ALL                                                      
         MVC   TBLAGY,ALL                                                       
         B     VALAGY4                                                          
*                                                                               
VALAGY1  EQU   *                                                                
         CLC   RISAGY(4),=C'LIST'                                               
         BNE   VALAGY2                                                          
         CLI   LISTBYTE,0                                                       
         BNE   ERRLIST                                                          
         MVI   LISTBYTE,LISTAGY                                                 
         MVC   TBLAGY,ALL                                                       
         MVC   TBLAOFF,ALL                                                      
         ZIC   RF,NUMLIST                                                       
         LA    RF,1(RF)                                                         
         STC   RF,NUMLIST                                                       
         B     VALAGY4                                                          
*                                                                               
VALAGY2  OC    RISAGY,SPACES                                                    
         MVI   KEY,X'0A'           SET UP KEY                                   
         XC    KEY+1(18),KEY+1                                                  
         MVC   KEY+19(8),SPACES                                                 
         BAS   RE,DASH                                                          
         LTR   R7,R7                                                            
         BNZ   *+8                                                              
         LA    R7,4                                                             
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   KEY+19(0),RISAGY                                                 
         LA    R7,10(R2,R7)                                                     
         MVC   KEY+23(2),0(R7)                                                  
         MVC   KEY+25(2),REPALPHA                                               
*                                                                               
VALAGY3  BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE     AGENCY CODE FOUND?                           
         BNE   VAGYERR             NO                                           
         CLC   REPALPHA,KEY+25     YES - SAME REP?                              
         BNE   VAGYERR             NO  - NOT FOUND                              
*                                                                               
         BAS   RE,GETREC           YES - GET RECORD                             
         MVC   TBLAGY,RAGYKAGY                                                  
         MVC   TBLAOFF,RAGYKAOF                                                 
         CLC   RAGYKAOF,SPACES                                                  
         BNE   *+10                                                             
         MVC   TBLAOFF,ALL                                                      
         ZIC   RF,NUMFILT                                                       
         LA    RF,1(RF)                                                         
         STC   RF,NUMFILT                                                       
         ZIC   RF,ADVORAGY                                                      
         LA    RF,1(RF)                                                         
         STC   RF,ADVORAGY                                                      
*                                                                               
VALAGY4  EQU   *                                                                
         OI    4(R2),X'20'                                                      
*                                                                               
VALAGYX  DS    0H                                                               
         EJECT                                                                  
***********************                                                         
* VALIDATE ADVERTISER *                                                         
***********************                                                         
         LA    R2,RISADVH                                                       
**JRD    TM    4(R2),X'20'         TEST ALREADY VALID                           
**JRD    BO    VALADVX                                                          
*                                                                               
         CLI   8(R2),C'='          BROWSE REQUEST?                              
         BNE   VALADVA                                                          
         GOTO1 (RFBROWSE,VREPFACS),DMCB,ACOMFACS,BASERD,(R2),0,        +        
               (0,C' ADV'),0                                                    
         DC    H'0'           BROWSE SHOULD HAVE TAKEN IT FROM HERE             
*                                                                               
VALADVA  DS    0H                                                               
         XC    NEXTBYTE,NEXTBYTE                                                
         LA    R3,FLDERR                                                        
         CLI   5(R2),0             TEST NO INPUT                                
         BE    VALADV0                                                          
*                                                                               
         LA    R3,ADVERR                                                        
         CLI   RISADV,C'*'         SET REQUEST?                                 
         BNE   VALADVB             NO                                           
         GOTO1 =A(VALSET),DMCB,=C'AD',=A(ADVDISP),RISADV+1,RR=Y                 
         BNE   VADVERR                                                          
         MVC   TBLADV,ALL                                                       
         XC    RISADVN,RISADVN                                                  
         XC    PRDFILT,PRDFILT                                                  
         B     VALADV4                                                          
*                                                                               
VALADVB  CLC   RISADV(3),=C'ALL'                                                
         BNE   VALADV1                                                          
         CLI   5(R2),3                                                          
         BH    VALADV2                                                          
*                                                                               
VALADV0  MVC   TBLADV,ALL                                                       
         XC    RISADVN,RISADVN                                                  
         XC    PRDFILT,PRDFILT                                                  
         B     VALADV4                                                          
*                                                                               
VALADV1  EQU   *                                                                
         CLC   RISADV(4),=C'LIST'                                               
         BNE   VALADV2                                                          
         CLI   LISTBYTE,0                                                       
         BNE   ERRLIST                                                          
         MVI   LISTBYTE,LISTADV                                                 
         MVC   TBLADV,ALL                                                       
         XC    RISADVN,RISADVN                                                  
         ZIC   RF,NUMLIST                                                       
         LA    RF,1(RF)                                                         
         STC   RF,NUMLIST                                                       
         B     VALADV4                                                          
*                                                                               
VALADV2  EQU   *                                                                
         MVI   KEY,8               SET KEY                                      
         XC    KEY+1(22),KEY+1                                                  
         MVC   KEY+21(4),RISADV                                                 
*                                                                               
*----------------------------------+   ADVERTISER-PRODUCT (AAA-BBB)?            
*                                  º                                            
         MVI   BYTE2,0             º   USE  BYTE2 FOR ADV-PROD FLAG             
         CLI   KEY+23,C'-'         º                                            
         BNE   VALADV2A            º                                            
         MVI   BYTE2,3             º   USE BYTE2 FOR ADV LENGTH + 1             
         XC    KEY+23(2),KEY+23    º                                            
         B     VALADV2C            º                                            
VALADV2A CLI   KEY+24,C'-'         º                                            
         BNE   VALADV2B            º                                            
         MVI   KEY+24,0            º                                            
         MVI   BYTE2,4             º                                            
         B     VALADV2C            º                                            
VALADV2B CLI   RISADV+4,C'-'       º                                            
         BNE   VALADV2C            º                                            
         MVI   BYTE2,5             º                                            
VALADV2C DS    0H                  º                                            
*                                  º                                            
*----------------------------------+                                            
*                                                                               
         OC    KEY+21(4),SPACES                                                 
         MVC   KEY+25(2),REPALPHA                                               
*                                                                               
VALADV2R BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE     ADVERTISER FOUND?                            
         BNE   VADVERR             NO                                           
         CLC   REPALPHA,KEY+25     YES - SAME REP?                              
         BNE   VADVERR             NO                                           
*                                                                               
         BAS   RE,GETREC           YES - GET RECORD                             
         MVC   RISADVN,RADVNAME                                                 
         MVC   TBLADV,RADVKADV                                                  
         ZIC   RF,NUMFILT                                                       
         LA    RF,1(RF)                                                         
         STC   RF,NUMFILT                                                       
         ZIC   RF,ADVORAGY                                                      
         LA    RF,2(RF)                                                         
         STC   RF,ADVORAGY                                                      
*                                                                               
VALADV4  EQU   *                                                                
         LA    R4,RISADVNH                                                      
         FOUT  (R4)                                                             
         OI    4(R2),X'20'         VALIDATE ON                                  
*                                                                               
         CLI   BYTE2,0             ADV-PROD FILTER                              
         BE    VALADVX             NO                                           
*                                                                               
         LA    R4,RISADV           YES/CHECK PROD CODE                          
         ZIC   R1,BYTE2            BYTE2=ADV LEN + 1                            
         AR    R4,R1               R4 POINTS TO PRODUCT                         
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING RPRDKEY,R1                                                       
         MVI   RPRDKEY,9                                                        
         MVC   RPRDKADV,TBLADV                                                  
         MVC   RPRDKREP,REPALPHA                                                
         MVC   RPRDKPRD,0(R4)                                                   
         OC    RPRDKPRD,SPACES                                                  
         GOTO1 HIGH                                                             
         DROP  R1                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VADVERR                                                          
         MVC   PRDFILT,0(R4)               SET PRODUCT FILTER                   
         OC    PRDFILT,SPACES                                                   
*                                                                               
VALADVX  DS    0H                                                               
         EJECT                                                                  
*****************************************                                       
* VALIDATE SALESPERSON OR DIVISION/TEAM *                                       
*****************************************                                       
         LA    R2,RISSLSH          TEST ALREADY VALID                           
**JRD    TM    4(R2),X'20'                                                      
**JRD    BO    VALSLTMX                                                         
*                                                                               
         CLI   8(R2),C'='          BROWSE REQUEST?                              
         BNE   VALSLTM0                                                         
         L     RE,4(RD)                                                         
         GOTO1 (RFBROWSE,VREPFACS),DMCB,ACOMFACS,BASERD,(R2),0,        +        
               (0,C' SAL'),0                                                    
         DC    H'0'           BROWSE SHOULD HAVE TAKEN IT FROM HERE             
*                                                                               
VALSLTM0 DS    0H                                                               
         XC    NEXTBYTE,NEXTBYTE                                                
         LA    R3,FLDERR                                                        
*                                                                               
         CLC   RISSLS(2),=C'D-'    TEST TEAM OPTION                             
         BNE   VALSAL0             NO - SALESPERSON                             
         EJECT                                                                  
*---------------*                                                               
* VALIDATE TEAM *                                                               
*---------------*                                                               
         LA    R3,TEAMERR                                                       
         CLI   5(R2),6                                                          
         BH    VSLTMERR                                                         
         CLC   RISSLS+2(3),=C'ALL'    DISALLOW D-ALL                            
         BE    VSLTMERR                                                         
         CLC   RISSLS+2(4),=C'LIST'                                             
         BNE   VALTEAM2                                                         
         CLI   LISTBYTE,0                                                       
         BNE   ERRLIST                                                          
         MVI   LISTBYTE,LISTDIV                                                 
         MVC   TBLDIV(5),ALL                                                    
         XC    RISSLSN,RISSLSN                                                  
         ZIC   RF,NUMLIST                                                       
         LA    RF,1(RF)                                                         
         STC   RF,NUMLIST                                                       
         B     VALTEAMX                                                         
*                                                                               
VALTEAM2 OC    RISSLS,SPACES                                                    
         MVI   KEY,5               SET KEY                                      
         XC    KEY+1(22),KEY+1                                                  
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),RISSLS+2                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VSLTMERR                                                         
*                                                                               
         BAS   RE,GETREC                                                        
         MVC   RISSLSN,RTEMDVNM                                                 
         MVC   TBLDIV(2),RTEMKTEM                                               
         CLI   RTEMKTEM+1,C' '                                                  
         BNE   *+10                                                             
         MVC   TBLTEAM,ALL                                                      
         MVC   TBLSLS,ALL                                                       
         MVC   TBLSLS2,ALL                                                      
*                                                                               
VALTEAMX DS    0H                                                               
         B     VALSLTMV                                                         
         EJECT                                                                  
*----------------------*                                                        
* VALIDATE SALESPERSON *                                                        
*----------------------*                                                        
VALSAL0  LA    R3,SLSERR                                                        
         CLI   5(R2),0                                                          
         BE    VALSAL2             PXZ                                          
*                                                                               
         CLI   RISSLS,C'*'         SET REQUEST?                                 
         BNE   VALSALB             NO                                           
         GOTO1 =A(VALSET),DMCB,=C'SP',=A(SALDISP),RISSLS+1,RR=Y                 
         BNE   VSLTMERR                                                         
         MVC   TBLSLS,ALL                                                       
         MVC   TBLSLS2,ALL                                                      
         MVC   TBLDIV,ALL                                                       
         MVC   TBLTEAM,ALL                                                      
         XC    RISSLSN,RISSLSN                                                  
         B     VALSALX                                                          
*                                                                               
VALSALB  CLC   RISSLS(3),=C'ALL'                                                
         BNE   VALSAL4                                                          
         OC    RISOFFN,RISOFFN     SLS=ALL AND OFF=XX DISALLOWED                
         BNZ   VSLTMERR                                                         
VALSAL2  MVC   TBLSLS,ALL                                                       
         MVC   TBLSLS2,ALL                                                      
         MVC   TBLDIV,ALL                                                       
         MVC   TBLTEAM,ALL                                                      
         XC    RISSLSN,RISSLSN                                                  
         B     VALSALX                                                          
*                                                                               
VALSAL4  CLC   RISSLS(4),=C'LIST'                                               
         BNE   VALSAL6                                                          
         MVC   TBLSLS,LIST                                                      
         MVC   TBLSLS2,LIST                                                     
         CLI   LISTBYTE,0                                                       
         BNE   ERRLIST                                                          
         MVI   LISTBYTE,LISTSLS                                                 
         MVC   TBLDIV(8),ALL       SET DIV,TEM,SLS,SLS2                         
         XC    RISSLSN,RISSLSN                                                  
         ZIC   RF,NUMLIST                                                       
         LA    RF,1(RF)                                                         
         STC   RF,NUMLIST                                                       
         B     VALSALX                                                          
*                                                                               
*  S/P KEY MAY BE SSS, OR SSS-TT, WHERE TT IS A TEAM FILTER                     
*        WITHIN THE S/P.  THIS IS FOR TEAM OVERRIDES.                           
*                                                                               
*                                                                               
VALSAL6  EQU   *                                                                
***                                                                             
         GOTO1 =V(SCANNER),DMCB,RISSLSH,(X'80',WORK2),C',=/=',RR=RELO1          
         CLI   DMCB+4,0                                                         
         BNE   *+6                                                              
         DC    H'0'                FIELD CHECKED: CAN'T HAPPEN                  
         XC    TBLDIV(2),TBLDIV    CLEAR DIV/TEAM FIELD                         
         CLI   WORK2,3             S/P CODE LENGTH > 3?                         
         BH    VSLTMERR            YES - ERROR OUT                              
         OC    WORK2+12(10),SPACES   SET BIN ZEROS TO SPACES                    
         OC    WORK2+32(1),WORK2+32  SECOND (TEAM) FIELD?                       
         BZ    VALSAL20            NO                                           
         CLI   WORK2+32,2          TEAM CODE LENGTH > 2?                        
         BH    VSLTMERR            YES - ERROR OUT                              
         OC    WORK2+44(10),SPACES SET BIN ZEROS TO SPACES                      
         MVC   TBLDIV(2),WORK2+44  LOAD TEAM VALUE                              
*                                                                               
VALSAL20 EQU   *                                                                
         MVI   KEY,6                                                            
         XC    KEY+1(21),KEY+1                                                  
         MVC   KEY+22(2),REPALPHA                                               
         MVC   KEY+24(3),WORK2+12  TAKE S/P CODE FROM SCAN                      
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VSLTMERR                                                         
*                                                                               
         BAS   RE,GETREC                                                        
*                                  USE LAST INITIAL OF SALESPERSON              
         MVC   RISSLSN,RSALNAME                                                 
         MVC   TBLSLS,SPACES                                                    
         MVC   TBLSLS(3),RSALKSAL      PXZ                                      
         MVC   TBLSLS2(3),RSALKSAL     PXZ                                      
         CLI   RSALKSAL+2,C' '                                                  
         BE    *+20                                                             
         MVC   TBLSLS(1),RSALKSAL+2                                             
         MVC   TBLSLS+1(2),RSALKSAL                                             
         B     *+16                                                             
         MVC   TBLSLS(1),RSALKSAL+1                                             
         MVC   TBLSLS+1(1),RSALKSAL                                             
         OC    TBLDIV(2),TBLDIV    ANY TEAM FILTER?                             
         BNZ   VALSAL40            YES - DON'T REPLACE FROM REC                 
*                                                                               
*   IF NO TEAM FILTER, DON'T FILTER S/P ON TEAM                                 
*                                                                               
**       MVC   TBLDIV(2),RSALTEAM  SET DIV & TEAM                               
VALSAL40 EQU   *                                                                
         ZIC   RF,NUMFILT                                                       
         LA    RF,1(RF)                                                         
         STC   RF,NUMFILT                                                       
         MVI   SINGSAL,1                                                        
*                                                                               
VALSALX  DS    0H                                                               
         SPACE 3                                                                
*----------------*                                                              
VALSLTMV DS    0H                  SALESPERSON/TEAM-DIV VALID                   
         LA    R4,RISSLSNH                                                      
         FOUT  (R4)                                                             
         OI    4(R2),X'20'         VALIDATE                                     
*                                                                               
VALSLTMX DS    0H                                                               
         EJECT                                                                  
*********************                                                           
* VALIDATE CATEGORY *                                                           
*********************                                                           
         LA    R2,RISCTGH                                                       
**JRD    TM    4(R2),X'20'         TEST ALREADY VALID                           
**JRD    BO    VALCTGX                                                          
*                                                                               
         XC    NEXTBYTE,NEXTBYTE                                                
         LA    R3,FLDERR           TEST NO INPUT                                
         CLI   5(R2),0                                                          
         BE    VALCTG0                                                          
*                                                                               
         LA    R3,CTGERR                                                        
         CLC   RISCTG(2),=C'C-'    TEST FOR CLASS OPTION                        
         BE    VCTGERR                                                          
*                                                                               
         CLC   =C'ALL',RISCTG      TEST ALL OPTION                              
         BNE   VALCTG1                                                          
*                                                                               
VALCTG0  MVC   TBLCLS,ALL                                                       
         MVC   TBLCTG,ALL                                                       
         B     VALCTG4                                                          
*                                                                               
VALCTG1  CLC   =C'LIST',RISCTG     TEST LIST OPTION                             
         BNE   VALCTG2                                                          
         CLI   LISTBYTE,0                                                       
         BNE   ERRLIST                                                          
         MVI   LISTBYTE,LISTCTG                                                 
         MVC   TBLCLS,ALL                                                       
         MVC   TBLCTG,ALL                                                       
         ZIC   RF,NUMLIST                                                       
         LA    RF,1(RF)                                                         
         STC   RF,NUMLIST                                                       
         B     VALCTG4                                                          
*                                                                               
VALCTG2  EQU   *                                                                
         MVI   KEY,X'0F'           SET KEY                                      
         XC    KEY+1(22),KEY+1                                                  
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),RISCTG                                                 
         OC    KEY+25(2),=C'  '                                                 
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VCTGERR                                                          
*                                                                               
         BAS   RE,GETREC                                                        
*                                                                               
         MVC   TBLCTG,RCTGKCTG                                                  
         MVC   TBLCLS,RCTGCLSS                                                  
         ZIC   RF,NUMFILT                                                       
         LA    RF,1(RF)                                                         
         STC   RF,NUMFILT                                                       
*                                                                               
VALCTG4  DS    0H                                                               
         OI    4(R2),X'20'         SET VALID                                    
*                                                                               
VALCTGX  DS    0H                                                               
         EJECT                                                                  
************************                                                        
* VALIDATE TARGET DEMO *                                                        
************************                                                        
         LA    R2,RISTARH                                                       
**JRD    TM    4(R2),X'20'         ALREADY VALID                                
**JRD    BO    VALDMOX                                                          
*                                                                               
         XC    TBLDEMO,TBLDEMO     CLEAR TBL FIELD                              
         CLI   5(R2),0             IS THERE ANY INPUT                           
         BNE   *+12                                                             
         OI    4(R2),X'20'                                                      
         B     VALDMOX                                                          
*                                                                               
         LA    R3,233              INVALID DEMO                                 
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'R'            *** ASSUME RADIO FOR NOW                
*                                                                               
         L     RF,VCALLOV          GET DEMOVAL                                  
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000AD9'                                           
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(0,0(R2)),(1,WORK),(C'Y',DBLOK),0                      
         CLI   DMCB+4,0                                                         
         BE    ERROR                                                            
         MVC   TBLDEMO,WORK                                                     
         OI    RISTARH+4,X'20'     SET VALIDATED                                
*                                                                               
VALDMOX  DS    0H                                                               
         EJECT                                                                  
**************************                                                      
* VALIDATE CREATION DATE *                                                      
**************************                                                      
VALCDAT  EQU   *                                                                
         LA    R2,RISCRTH                                                       
**JRD    TM    4(R2),X'20'         ALREADY VALIDATED                            
**JRD    BO    VALCDATX                                                         
         XC    TBLCDATS(4),TBLCDATS     CLEAR HEADER DATES                      
         XC    TBLBDATS(6),TBLBDATS     CLEAR BUY DATE FILTERS                  
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         OI    4(R2),X'20'                                                      
         B     VALCDATX                                                         
*                                                                               
         MVI   BYTE,0              CLEAR BUY/HEADER FLAG                        
         CLI   8(R2),C'H'          IS IT HEADER CREATION DATE ?                 
         BNE   VALCD00             NO                                           
         ZIC   R1,5(R2)            YES/LET'S FUDGE INPUT FIELD                  
         SH    R1,=H'2'                AND GET RID OF 'H'                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),9(R2)                                                    
         LA    R1,1(R1)                                                         
         STC   R1,5(R2)                                                         
         MVI   BYTE,C'H'           SET HEADER FLAG                              
*                                                                               
VALCD00  LA    R3,2                INVALID FIELD                                
         BAS   RE,DASH             ONE OR TWO DATES                             
         LTR   R7,R7               R7=LENGTH(-1) UP TO DASH                     
         BZ    VALCD01             ONE DATE                                     
*---------------------*                                                         
* TWO DATE VALIDATION *                                                         
*---------------------*                                                         
         BCTR  R7,0                                                             
         EX    R7,*+8              SET 1ST DATE TO WORK                         
         B     *+10                                                             
         MVC   WORK(0),8(R2)       1ST DATE                                     
         GOTO1 VDATVAL,DMCB,(0,WORK),WORK+20                                    
         OC    DMCB,DMCB                                                        
         BZ    ERROR                                                            
         LA    R4,8(R2)            BUMP TO 2ND DATE                             
         LA    R7,2(R7)                                                         
         AR    R4,R7                                                            
         GOTO1 VDATVAL,DMCB,(0,0(R4)),WORK+30                                   
         OC    DMCB,DMCB                                                        
         BZ    ERROR                                                            
         CLC   WORK+20(6),WORK+30  COMPARE 1ST TO 2ND DATE                      
         BH    ERROR                                                            
         GOTO1 VDATCON,DMCB,(0,WORK+20),(2,TBLCDATS)                            
         GOTO1 VDATCON,DMCB,(0,WORK+30),(2,TBLCDATE)                            
         B     VALCD05                                                          
*------------------------*                                                      
* SINGLE DATE VALIDATION                                                        
*------------------------*                                                      
VALCD01  LA    R3,2                INVALID FIELD                                
         GOTO1 VDATVAL,DMCB,(0,RISCRT),WORK                                     
         OC    DMCB,DMCB                                                        
         BZ    ERROR                                                            
         GOTO1 VDATCON,DMCB,(0,WORK),(2,TBLCDATS)                               
         MVC   TBLCDATE,TBLCDATS                                                
*                                                                               
VALCD05  OI    RISCRTH+4,X'20'                                                  
         CLI   BYTE,C'H'           IS IT HEADER CREATION DATE?                  
         BNE   VALCD07                                                          
         MVI   WORK,C'H'           YES - RESET H IN SCREEN FIELD                
         LA    R2,RISCRTH                                                       
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+1(0),8(R2)     WORK HAS H+CREATION DATE                     
         LA    R1,1(R1)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),WORK        MOVE H+CREATION DATE TO SCREEN FIELD         
         LA    R1,1(R1)                                                         
         STC   R1,5(R2)            SET NEW LENGTH IN HEADER                     
         B     VALCDATX                                YES                      
*                                                                               
VALCD07  GOTO1 VDATCON,DMCB,(2,TBLCDATS),(3,TBLBDATS)  NO-BUY DATE FLTR         
         GOTO1 VDATCON,DMCB,(2,TBLCDATE),(3,TBLBDATE)                           
         XC    TBLCDATS(4),TBLCDATS                    CLEAR HEADER DTS         
*                                                                               
VALCDATX DS    0H                                                               
         EJECT                                                                  
****************************                                                    
* VALIDATE CONTRACT STATUS *                                                    
****************************                                                    
VALCTYPE EQU   *                                                                
         LA    R2,RISCSTH                                                       
**JRD    TM    4(R2),X'20'         ALREADY VALIDATED                            
**JRD    BO    VALCTX                                                           
         XC    TBLCTYP,TBLCTYP     NO,CLEAR FIELD                               
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         OI    4(R2),X'20'                                                      
         B     VALCTX                                                           
         LA    R3,2                INVALID FIELD                                
         LA    R1,CSTATBL                                                       
*                                                                               
VCT10    CLC   8(1,R2),0(R1)                                                    
         BE    VCT12                                                            
         LA    R1,1(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   VCT10                                                            
         B     ERROR                                                            
*                                                                               
VCT12    MVC   TBLCTYP,8(R2)                                                    
         OI    TBLCTYP+2,X'40'                                                  
         CLI   TBLCTYP,C'U'        ,,IF C'U'                                    
         BE    *+12                                                             
         CLI   TBLCTYP,C'C'        ,,OR C'C'                                    
         BNE   VCT12D                                                           
*                                                                               
         LA    R1,CONSTTBL         ,,NEEDS FURTHER CHECKING                     
VCT12B   CLC   =C'XXX',0(R1)                                                    
         BE    ERROR                                                            
         CLC   TBLCTYP,0(R1)                                                    
         BE    VCT12D                                                           
         LA    R1,3(R1)                                                         
         B     VCT12B                                                           
*                                                                               
VCT12D   OI    RISCSTH+4,X'20'                                                  
*                                                                               
VALCTX   DS    0H                                                               
         EJECT                                                                  
********************************************************************            
*        VALIDATE DATE(OPTIONS) FIELD THEN MONTHS FIELD                         
********************************************************************            
         GOTO1 =A(VALOPS),RR=Y                                                  
         BNE   VDATERR                                                          
*                                                                               
         LA    R2,RISSTAH                                                       
         OC    TBLSTA,TBLSTA       IS IT SPECIFIC STATION?                      
         BNZ   VAL04               YES                                          
*                                                                               
         CLI   PRNT,1              PRINTING?                                    
         BNE   VAL02               NO                                           
         MVI   PRNT,0                                                           
         B     VSTAREQ                                                          
*                                                                               
VAL02    TM    VIEWS,X'80'          ALL STATIONS OK IF NO TOTALS                
         BNO   VSTAREQ              NOT OK                                      
*                                                                               
VAL04    DS    0H                                                               
         LA    R2,RISDATEH                                                      
         OI    4(R2),X'20'                                                      
*                                                                               
*-----------------------*                                                       
* VALIDATE MONTHS FIELD *                                                       
*-----------------------*                                                       
         TM    LISTBYTE,LISTPRD    IF LIST OPTION 'PLIST',                      
         BO    VALMNTS3                                                         
         TM    LISTBYTE,LISTDLRS   'LIST,                                       
         BO    VALMNTS3                                                         
         TM    LISTBYTE,LISTTRAF   'OLIST,                                      
         BO    VALMNTS3                                                         
         TM    LISTBYTE,LISTUNCF   OR 'ULIST'                                   
         BO    VALMNTS3            SKIP SLS/OFF TEST                            
*                                                                               
VALMNTS2 EQU   *                                                                
         CLC   RISSLS(2),=C'D-'    IF TEAM OPTION                               
         BE    VALMNTS3            NO FILTERING REQUIREMENTS                    
         OC    RISSLSN,RISSLSN     IF SINGLE SALESPERSON                        
         BZ    VALMNTS3                                                         
*                                                                               
         LA    R3,177              MUST FILTER ON OFFICE                        
         LA    R2,RISOFFH                                                       
         OC    RISOFFN,RISOFFN                                                  
         BNZ   VALMNTS3                                                         
*                                                                               
         BAS   RE,CLRSCRN                                                       
         B     ERROR                                                            
*                                                                               
VALMNTS3 LA    R2,RISMNTSH                                                      
         LA    R3,FLDERR                                                        
**JRD    TM    4(R2),X'20'                                                      
**JRD    BO    VALMNTSX                                                         
*                                                                               
         LA    R3,MNTSERR                                                       
         XC    NEXTBYTE,NEXTBYTE                                                
*                                                                               
         CLI   5(R2),0                                                          
         BE    VMNTSERR                                                         
         OC    RISMNTS,SPACES                                                   
         CLC   =C'ALL',RISMNTS     ALL IS NOT VALID OPTION                      
         BE    VMNTSERR                                                         
         MVC   DUB,SPACES                                                       
         BAS   RE,DASH                                                          
         LTR   R7,R7                                                            
         BZ    VALMNTS7                                                         
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   DUB(0),RISMNTS                                                   
*                                                                               
         GOTO1 VDATVAL,DMCB,(2,DUB),WORK                                        
         OC    DMCB(4),DMCB                                                     
         BZ    VMNTSERR                                                         
         CLC   WORK(2),=C'74'                                                   
         BL    VMNTSERR                                                         
         LA    R1,1(R7)                                                         
         C     R1,DMCB                                                          
         BNE   VMNTSERR                                                         
         GOTO1 VDATCON,DMCB,(0,WORK),(3,TBLBGN)                                 
         LA    R7,10(R2,R7)                                                     
         GOTO1 VDATVAL,DMCB,(2,0(R7)),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    VMNTSERR                                                         
         SR    RE,RE                                                            
         IC    RE,RISMNTSH                                                      
         LA    R1,RISMNTSH                                                      
         LA    RE,0(R1,RE)                                                      
*                                                                               
         SR    R1,R1                                                            
         LA    R1,1(R1)                                                         
         LA    R7,1(R7)                                                         
         CR    R7,RE                                                            
         BNL   *+12                PAST END OF FIELD                            
         CLI   0(R7),C' '                                                       
         BNE   *-18                FOUND SPACE                                  
         C     R1,DMCB                                                          
         BNE   VMNTSERR                                                         
         GOTO1 VDATCON,DMCB,(0,WORK),(3,TBLEND)                                 
*                                                                               
         CLI   LISTBYTE,0          TEST ON $ OPTION                             
         BE    *+12                                                             
         CLI   LISTBYTE,LISTPNAM   TEST ON PROD NAME LIST                       
         BNE   VALMNTS8            NO TEST ON OTHER LIST OPTIONS                
*                                                                               
         MVC   HALF,TBLBGN         TEST PERIOD LENGTH                           
*                                                                               
         IC    RE,HALF                                                          
         LA    RE,1(RE)                                                         
         STC   RE,HALF                                                          
         LA    R3,LENTHERR                                                      
         CLC   TBLEND(2),HALF                                                   
         BNL   VMNTSERR                                                         
         CLI   LISTBYTE,LISTPNAM   IF PROD NAME LIST                            
         BE    VALMNTS8                                                         
         B     VALMNTSV                                                         
*--------------*                                                                
* SINGLE MONTH *                                                                
*--------------*                                                                
VALMNTS7 GOTO1 VDATVAL,DMCB,(2,RISMNTS),WORK                                    
         OC    DMCB(4),DMCB                                                     
         BZ    VMNTSERR                                                         
         CLC   WORK(2),=C'74'                                                   
         BL    VMNTSERR                                                         
         GOTO1 VDATCON,DMCB,(0,WORK),(3,TBLBGN)                                 
         MVC   TBLEND,TBLBGN                                                    
         CLI   LISTBYTE,0                                                       
         BE    VALMNTSV                                                         
*                                                                               
VALMNTS8 DS    0H                  GET BROADCAST S-E DATES WHEN LISTING         
         MVC   MOSTRT,TBLBGN       SAVE MONTH OF SERVICE FOR LISTDLR            
         MVC   MOSEND,TBLEND                                                    
         MVI   TBLBGN+2,1                                                       
         MVI   TBLEND+2,1                                                       
         GOTO1 VDATCON,DMCB,(3,TBLBGN),(0,WORK)                                 
         GOTO1 VDATCON,(R1),(3,TBLEND),(0,WORK+6)                               
         CLI   ESTBUCKT,X'53'      ALTERNATE CALENDAR REQUEST?                  
         BE    VALMNTS9            YES - SET UP ALTERNATE DATES                 
         GOTO1 =V(GETBROAD),(R1),(1,WORK),WORK+12,VGETDAY,VADDAY,RR=YES         
         GOTO1 (RF),(R1),(1,WORK+6),WORK+24                                     
         GOTO1 VDATCON,(R1),(0,WORK+12),(3,TBLBGN)                              
         GOTO1 (RF),(R1),(0,WORK+30),(3,TBLEND)                                 
         B     VALMNTSV                                                         
VALMNTS9 EQU   *                                                                
         GOTO1 =A(ALTDATES),RR=Y                                                
*                                                                               
VALMNTSV OI    4(R2),X'20'                                                      
*                                                                               
VALMNTSX DS    0H                                                               
         EJECT                                                                  
*************************                                                       
*VALIDATE CONTRACT TYPE *                                                       
*************************                                                       
         LA    R2,RISFILTH                                                      
**JRD    TM    4(R2),X'20'                                                      
**JRD    BO    VALFILTX                                                         
*                                                                               
         XC    TBLFILT,TBLFILT                                                  
         XC    NEXTBYTE,NEXTBYTE                                                
*                                                                               
         CLI   5(R2),0                                                          
         BE    VALFILT4                                                         
         LA    R3,2                                                             
         CLI   5(R2),1                                                          
         BNE   VALFILT2                                                         
         CLI   RISFILT,C'*'                                                     
         BE    ERROR                                                            
         MVC   TBLFILT,RISFILT                                                  
         B     VALFILT4                                                         
*                                                                               
VALFILT2 CLI   RISFILT,C'*'                                                     
         BNE   ERROR                                                            
         CLI   5(R2),2             LENGTH 2?                                    
         BE    VALFILT3            NO NOT SET REQUEST                           
*                                                                               
         GOTO1 =A(VALSET),DMCB,=C'CT',=A(CTYPDISP),RISFILT+1,RR=Y               
         BNE   ERROR                                                            
         B     VALFILT4                                                         
*                                                                               
VALFILT3 MVC   TBLFILT,RISFILT+1                                                
         NI    TBLFILT,X'BF'                                                    
*                                                                               
VALFILT4 OI    4(R2),X'20'                                                      
*                                                                               
VALFILTX DS    0H                                                               
         EJECT                                                                  
*************************************                                           
*ALTERNATE DOLLAR OPTION VALIDATION *                                           
*************************************                                           
         LA    R2,RISALTDH         A(ALTERNATE DOLLAR HEADER)                   
         CLI   5(R2),0             ANYTHING ENTERED?                            
         BNE   VALALT02            YES                                          
*                                                                               
         MVI   RISALTD,C'E'        DEFAULT IS ESTIMATE                          
         TM    SVPGPBIT,X'10'      NO/ LOOK AT PROFILE   X'10'=BKD$             
         BZ    *+8                                                              
         MVI   RISALTD,C'B'                                                     
         OI    RISALTDH+6,X'80'                                                 
         B     VALALT10                                                         
                                                                                
VALALT02 EQU   *                                                                
         CLI   5(R2),2                                                          
         BE    ERROR               TOO MANY CHARACTERS                          
         CLI   8(R2),C'B'                                                       
         BE    VALALT10                                                         
         CLI   8(R2),C'E'                                                       
         BE    VALALT10                                                         
         CLI   8(R2),C'I'                                                       
         BNE   ERROR                                                            
         B     VALALT10                                                         
VALALT03 EQU   *                                                                
         CLI   PREVALT$,C'N'       PREVIOUS VALUE ' ' OR 'N'?                   
*********************                                                           
         BNE   VALALT04            NEW                                          
         NI    SVPGP#,X'FF'-RREPQRIS     REREAD PROFILE                         
         B     VALALT08                                                         
****************                                                                
         BE    VALALT08            YES - DON'T FLIP-FLOP                        
VALALT04 EQU   *                                                                
         XI    SVPGPBIT,X'10'      FLIP-FLOP ALTERNATE BIT                      
VALALT08 EQU   *                                                                
         MVC   PREVALT$,8(R2)      SAVE ALT$ VALUE ENTERED                      
VALALT10 OI    4(R2),X'20'         TURN ON PREV VALID BIT                       
*                                                                               
         TM    VIEWS,X'07'         IF LIST$,LISTD,BUY$                          
         BZ    VALALTX                                                          
         CLI   8(R2),C'E'          E -> B                                       
         BNE   VALALTX                                                          
         MVI   8(R2),C'B'                                                       
         OI    RISALTDH+6,X'80'                                                 
*                                                                               
VALALTX  DS    0H                                                               
         EJECT                                                                  
**************************                                                      
*LAST CHANCE VALIDATIONS *                                                      
**************************                                                      
         CLI   NUMLIST,0                                                        
         BNE   VALEND              ANY LIST CUTS PROCESSING TIME                
         CLI   LISTBYTE,0                                                       
         BNE   VALEND                                                           
*                                                                               
*        BIT 0 = SINGLE STA REQUIRES ONE OTHER FILTER                           
*                                                                               
         TM    SVPGPBIT,X'80'      PROGRAM BIT 0 SET?                           
         BO    VLAST10             IF ON, SKIP TEST                             
         CLI   SINGSTA,0                                                        
         BE    VLAST10             SINGLE STATION MUST BE SET                   
         CLI   NUMFILT,0                                                        
         BNE   VLAST10             1 OR MORE PASSES TEST                        
         MVC   RISMESS(L'PRO00MSG),PRO00MSG                                     
         LA    R2,RISSTAH                                                       
         B     MYERROR                                                          
*                                                                               
*        BIT 1 = GROUP REQUIRES TWO FILTERS                                     
*                                                                               
VLAST10  EQU   *                                                                
         CLC   =C'M=',RISSTA       MARKET REQUEST?                              
         BE    VLAST20             YES - SKIP TEST                              
         TM    SVPGPBIT,X'40'      PROGRAM BIT 1 SET?                           
         BO    VLAST20             IF ON, SKIP TEST                             
         CLI   SINGSTA,0           SINGLE STATION MUST BE OFF                   
         BNE   VLAST20                                                          
         CLI   NUMFILT,1                                                        
         BH    VLAST20             MORE THAN 1 PASSES THE TEST                  
         MVC   RISMESS(L'PRO01MSG),PRO01MSG                                     
         LA    R2,RISSTAH                                                       
         B     MYERROR                                                          
*                                                                               
*        BIT 2 = SINGLE SALESPERSON REQUIRES ADV OR AGY                         
*                                                                               
VLAST20  EQU   *                                                                
         TM    SVPGPBIT,X'20'      PROGRAM BIT 2 SET?                           
         BO    VLAST30             IF ON, SKIP TEST                             
         CLI   SINGSAL,0           SINGLE SALESPERSON SET                       
         BE    VLAST30             NO, SKIP TEST                                
         CLI   ADVORAGY,0          FIELD SET IF EITHER FILTER SET               
         BNE   VLAST30                                                          
         MVC   RISMESS(L'PRO02MSG),PRO02MSG                                     
         LA    R2,RISADVH                                                       
         B     MYERROR                                                          
*                                                                               
VLAST30  EQU   *                                                                
         EJECT                                                                  
*********                                                                       
* EXITS *                                                                       
*********                                                                       
VALEND   DS    0H                                                               
         XC    WORK,WORK           BULD PAR SECURITY CHECK                      
         LA    R1,WORK                                                          
         USING SBLOCK,R1                                                        
*                                                                               
         CLI   RISOFFH+5,0                                                      
         BE    *+10                                                             
         MVC   SBOFFICE(2),TBLOFF                                               
*                                                                               
         CLI   RISSLSH+5,0                                                      
         BE    *+10                                                             
         MVC   SBSALES(3),TBLSLS2                                               
*                                                                               
         MVC   SBSTATN(5),TBLSTA                                                
*                                                                               
         CLC   RISSTA(2),=C'G-'                                                 
         BNE   *+10                                                             
         MVC   SBGROUP(2),TBLGRP                                                
*                                                                               
         CLI   LISTBYTE,0          'LIST' OPTION USED?                          
         BE    *+8                 NO                                           
         OI    SBBREAKS,GRPBREAK+STABREAK+OFFBREAK+SALBREAK                     
*                                                                               
         LR    R4,RA                                                            
         A     R4,=A(OFFDISP)                                                   
         ICM   R3,15,0(R4)         CHECK FOR USE OF OFFICE SET                  
         BZ    VALEND05                                                         
         AR    R3,RA                                                            
         TM    0(R3),X'80'         SET IN USE?                                  
         BZ    VALEND05            NO                                           
         CLI   RISOFF,C'*'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R3,RISOFFH+5                                                     
         BCTR  R3,0                                                             
         EX    R3,*+4                                                           
         MVC   SBOFFICE(0),RISOFF                                               
*                                                                               
VALEND05 DS    0H                                                               
         LR    R4,RA                                                            
         A     R4,=A(SALDISP)                                                   
         ICM   R3,15,0(R4)         CHECK FOR USE OF S/P SET                     
         BZ    VALEND10                                                         
         AR    R3,RA                                                            
         TM    0(R3),X'80'         SET IN USE?                                  
         BZ    VALEND10            NO                                           
         CLI   RISSLS,C'*'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R3,RISSLSH+5                                                     
         BCTR  R3,0                                                             
         EX    R3,*+4                                                           
         MVC   SBSALES(0),RISOFF                                                
*                                                                               
VALEND10 DS    0H                                                               
         LR    R4,RA                                                            
         A     R4,=A(STADISP)                                                   
         ICM   R3,15,0(R4)         CHECK FOR USE OF STATION SET                 
         BZ    VALEND20                                                         
         AR    R3,RA                                                            
         TM    0(R3),X'80'         SET IN USE?                                  
         BZ    VALEND20            NO                                           
         CLI   RISSTA,C'*'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R3,RISSTAH+5                                                     
         BCTR  R3,0                                                             
         EX    R3,*+4                                                           
         MVC   SBSTATN(0),RISSTA                                                
*                                                                               
VALEND20 DS    0H                                                               
         LR    R4,RA                                                            
         A     R4,=A(GRPDISP)                                                   
         ICM   R3,15,0(R4)         CHECK FOR USE OF GROUP SET                   
         BZ    VALEND30                                                         
         AR    R3,RA                                                            
         TM    0(R3),X'80'         SET IN USE?                                  
         BZ    VALEND30            NO                                           
         CLC   RISSTA(3),=C'G-*'                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R3,RISSTAH+5                                                     
         SHI   R3,3                                                             
         EX    R3,*+4                                                           
         MVC   SBGROUP(0),RISSTA+2                                              
*                                                                               
VALEND30 DS    0H                                                               
         DROP  R1                                                               
         GOTO1 (RFCKSEC,VREPFACS),DMCB,WORK,RISMESSH,ATIOB,RFBLOCK              
         BE    *+12                                                             
         LA    R2,RISSTAH                                                       
         B     MYERROR                                                          
*                                                                               
         BAS   RE,SETIOTBL         FILL IO TABLE FOR REPIO READER               
*                                                                               
VALEXIT  XC    DMCB(4),DMCB                                                     
         MVC   DMCB(1),LISTBYTE                                                 
         B     EXXMOD                                                           
*                                                                               
ERRLIST  LA    R3,175              ONLY 1 LIST ALLOWED AT A TIME                
         BAS   RE,CLRSCRN                                                       
         B     ERROR                                                            
*********************                                                           
* FIELD ERROR EXITS *                                                           
*********************                                                           
VSTAERR  BAS   RE,CLRSTA                                                        
         LA    R2,RISSTAH                                                       
         B     ERROR                                                            
*                                                                               
VOFFERR  BAS   RE,CLROFF                                                        
         LA    R2,RISOFFH                                                       
         B     ERROR                                                            
*                                                                               
VAGYERR  BAS   RE,CLRAGY                                                        
         LA    R2,RISAGYH                                                       
         B     ERROR                                                            
*                                                                               
VADVERR  BAS   RE,CLRADV                                                        
         LA    R2,RISADVH                                                       
         B     ERROR                                                            
*                                                                               
VSLTMERR BAS   RE,CLRSLS                                                        
         LA    R2,RISSLSH                                                       
         B     ERROR                                                            
*                                                                               
VCTGERR  BAS   RE,CLRCTG                                                        
         LA    R2,RISCTGH                                                       
         B     ERROR                                                            
*                                                                               
VMNTSERR BAS   RE,CLRSCRN                                                       
         LA    R2,RISMNTSH                                                      
         B     ERROR                                                            
*                                                                               
VMKTERR  EQU   *                                                                
         XC    RISMESS,RISMESS                                                  
         CH    R3,=H'904'          TABLE FULL?                                  
         BNE   VMKTERR2            NO  -  TABLE IS EMPTY                        
         MVC   RISMESS(31),=C'* TOO MANY STATIONS IN MARKET *'                  
         B     VMKTERR3                                                         
VMKTERR2 EQU   *                                                                
         MVC   RISMESS(31),=C'* NO STATIONS FOUND IN MARKET *'                  
VMKTERR3 EQU   *                                                                
         LA    R2,RISMESSH                                                      
         FOUT  (R2)                                                             
         LA    R2,RISSTAH          POSITION CURSOR                              
         B     MYERROR                                                          
*                                                                               
VDATERR  DS    0H                  ERROR IN DATE(OPTION) FIELD                  
         XC    RISMESS,RISMESS                                                  
         MVC   RISMESS(25),=C'* ERROR - INVALID OPTION*'                        
         LA    R2,RISMESSH                                                      
         FOUT  (R2)                                                             
         LA    R2,RISDATEH         POSITION CURSOR                              
         B     MYERROR                                                          
*                                                                               
VSTAREQ  BAS   RE,CLRSCRN                                                       
         XC    RISMESS,RISMESS                                                  
         MVC   RISMESS(37),=C'* STATION REQUIRED FOR THIS ACTION  *'            
         LA    R2,RISDATEH         POSITION CURSOR                              
         B     MYERROR                                                          
*                                                                               
*****************************************************************               
*- EXIT HERE IF MESSAGE ALREADY SET IN SCREEN FIELD.                            
*****************************************************************               
MYERROR  EQU   *                                                                
         BAS   RE,CLRSCRN          TURN OFF PREVALID BITS                       
         MVI   ERRAREA,X'FF'       MSG SET                                      
         B     EXIT                                                             
         EJECT                                                                  
*****************************************************************               
* FILL REPIOBLK FOR USE BY REPIO READER                                         
*****************************************************************               
SETIOTBL NTR1                                                                   
         LA    R2,REPIOTBL                                                      
         USING REPIOD,R2                                                        
*                                                                               
         LR    RE,R2               CLEAR REPIOBLK                               
         LA    RF,RIPIOLEN                                                      
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
*----------------------------------------------------------                     
* GET REPIO DATES                                                               
*   LISTBYTE = 0 ONLY WHEN READING $                                            
*----------------------------------------------------------                     
         CLI   LISTBYTE,0          IF ANY TYPE OF LIST REPORTING                
         BNE   SET04               I HAVE BROADCAST DATES                       
*                                                                               
         MVI   TBLBGN+2,1          OTHERWISE GET BROADCAST DATES                
         MVI   TBLEND+2,1                                                       
         GOTO1 VDATCON,DMCB,(3,TBLBGN),(0,WORK)                                 
         GOTO1 VDATCON,(R1),(3,TBLEND),(0,WORK+6)                               
         GOTO1 =V(GETBROAD),(R1),(1,WORK),WORK+12,VGETDAY,VADDAY,RR=YES         
         GOTO1 (RF),(R1),(1,WORK+6),WORK+24                                     
         GOTO1 VDATCON,(R1),(0,WORK+12),(2,RIPDATS)    START DATE               
         GOTO1 (RF),(R1),(0,WORK+30),(2,RIPDATE)       END DATE                 
         B     SET06                                                            
*                                                                               
SET04    DS    0H                                                               
         GOTO1 VDATCON,DMCB,(3,TBLBGN),(2,RIPDATS)     START DATE               
         GOTO1 VDATCON,DMCB,(3,TBLEND),(2,RIPDATE)     END DATE                 
*                                                                               
         MVI   RIPSKIP,C'Y'        SET SKIP READ ON                             
*                                                                               
SET06    DS    0H                  READ START DATE FOR REPIO IS                 
         L     R0,=F'-2'             RIPDATS-2 YEARS                            
         CLI   LISTBYTE,0            UNLESS LIST REPORTING                      
         BE    *+8                   - WHICH GETS RIPDATS-1 YEAR                
         L     R0,=F'-1'                                                        
*                                                                               
         GOTO1 VDATCON,DMCB,(2,RIPDATS),(0,WORK)                                
         GOTO1 VADDAY,DMCB,(C'Y',WORK),(0,WORK),(R0)                            
         GOTO1 VDATCON,DMCB,(0,WORK),(2,RIPDATSS)                               
*                                                                               
         MVC   RIPREP,REPALPHA                                                  
         L     R1,AIOAREA                                                       
         ST    R1,RIPIOARE                                                      
*                                                                               
         MVC   RIPAGY,TBLAGY       AGENCY                                       
         MVC   RIPAGOFF,TBLAOFF    AGENCY OFFICE                                
         MVC   RIPADV,TBLADV       ADVERTISER                                   
         MVC   RIPSTA,TBLSTA       STATION                                      
         MVC   RIPSAL,TBLSLS2      SALESPERSON                                  
         MVC   RIPCON,TBLFILT      CONTRACT TYPE                                
         MVC   RIPGRP,TBLGRP       GROUP                                        
         MVC   RIPSBGP,TBLSBGP     SUBGROUP                                     
         MVC   RIPCAT,TBLCTG       CATEGORY                                     
         MVC   RIPTEAM,TBLDIV        TEAM                                       
         MVC   RIPOFF,TBLOFF       OFFICE                                       
         MVC   RIPDEMO,TBLDEMO     DEMO                                         
         MVC   RIPCDATS,TBLCDATS    CREATION DATE START                         
         MVC   RIPCDATE,TBLCDATE    CREATION DATE END                           
*                                                                               
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*****************************************************************               
* DO SOME FUNNY STUFF TO THE SCREEN                                             
*****************************************************************               
CLEAR    DS    0H                                                               
CLRSTA   XC    RISSTAN,RISSTAN                                                  
         FOUT  RISSTANH                                                         
*                                                                               
CLROFF   XC    RISOFFN,RISOFFN                                                  
         FOUT  RISOFFNH                                                         
*                                                                               
CLRAGY   DS    0H                                                               
****     XC    RISAGYN,RISAGYN                                                  
****     FOUT  RISAGYNH                                                         
*                                                                               
CLRADV   XC    RISADVN,RISADVN                                                  
         FOUT  RISADVNH                                                         
*                                                                               
CLRSLS   XC    RISSLSN,RISSLSN                                                  
         FOUT  RISSLSNH                                                         
*                                                                               
CLRCTG   DS    0H                                                               
****     XC    RISCTGN,RISCTGN                                                  
****     FOUT  RISCTGNH                                                         
         SPACE 2                                                                
CLRSCRN  NI    RISSTAH+4,X'DF'                                                  
         NI    RISOFFH+4,X'DF'                                                  
         NI    RISAGYH+4,X'DF'                                                  
         NI    RISADVH+4,X'DF'                                                  
         NI    RISSLSH+4,X'DF'                                                  
         NI    RISCTGH+4,X'DF'                                                  
         NI    RISTARH+4,X'DF'                                                  
         NI    RISCRTH+4,X'DF'                                                  
         NI    RISCSTH+4,X'DF'                                                  
         NI    RISMNTSH+4,X'DF'                                                 
         NI    RISDATEH+4,X'DF'                                                 
         NI    RISFILTH+4,X'DF'                                                 
         LR    R6,R2                                                            
         SPACE 1                                                                
         LA    R2,RISTITLH                                                      
SCREEN2  CLI   0(R2),0                                                          
         BE    SCREEN6                                                          
         SR    R8,R8                                                            
         IC    R8,0(R2)                                                         
         SH    R8,=H'9'                                                         
         EX    R8,*+8                                                           
         B     *+10                                                             
         OC    8(0,R2),8(R2)                                                    
         BZ    SCREEN4                                                          
*                                                                               
         EX    R8,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         FOUT  (R2)                                                             
*                                                                               
SCREEN4  LA    R2,9(R2,R8)                                                      
         B     SCREEN2                                                          
         SPACE 1                                                                
SCREEN6  LR    R2,R6                                                            
         BR    RE                                                               
*********************************************************************           
* ROUTINE RETURNS LENGTH OF VALID FIELD BEFORE DASH IN R7 (LENGTH-1)            
* R2 POINTS TO FIELD HEADER                                                     
*********************************************************************           
DASH     SR    R8,R8                                                            
         IC    R8,5(R2)            LENGTH OF INPUT FIELD                        
         LA    R6,8(R2)            POINT R6 TO DATA                             
         SR    R7,R7                                                            
DASH2    CLI   0(R6),C'-'                                                       
         BE    DASH4                                                            
         LA    R6,1(R6)                                                         
         LA    R7,1(R7)            LENGTH OF VALID FIELD BEFORE DASH            
         BCT   R8,DASH2                                                         
         SR    R7,R7               SET R7 TO INDICATE NO DASH                   
         BR    RE                                                               
*                                                                               
DASH4    LTR   R7,R7               TEST DASH IN FIRST POSITION                  
         BZ    ERROR                                                            
         BR    RE                                                               
         EJECT                                                                  
*****************************************************************               
*  TABLE OF VALID INPUT FOR CONTRACT STATUS FILTERS U AND C                     
*****************************************************************               
CONSTTBL DS    0CL3                                                             
         DC    CL3'U$ '                                                         
         DC    CL3'C$ '                                                         
         DC    CL3'UO '                                                         
         DC    CL3'CO '                                                         
         DC    CL3'US '                                                         
         DC    CL3'UR '                                                         
         DC    CL3'USW'                                                         
         DC    CL3'URW'                                                         
         DC    CL3'XXX'            EOF                                          
*****************************************************************               
* CON STATUS TABLE (NEW,REVISED,CONFIRMED,PENDING,LOSS,TRADE)                   
*                  (UNCONFIRMED,INCOMPLETE)                                     
*****************************************************************               
CSTATBL  DC    C'NRCPLTUIM'                                                     
         DC    X'00'                                                            
STALETS  DS    CL5'     '                                                       
*****************************************************************               
*- PROGRAM-SPECIFIC MESSAGES                                                    
*****************************************************************               
PRO00MSG DC    C'SINGLE STATION REQUIRES ONE OTHER SINGLE FILTER. '             
PRO01MSG DC    C'REQUEST REQUIRES TWO OTHER SINGLE FILTERS.       '             
PRO02MSG DC    C'SINGLE SALESPERSON REQUIRES ADVERTISER OR AGENCY.'             
*****************************************************************               
*        CONSTANTS, WORKER, ET AL                                               
*****************************************************************               
SPACES   DC    CL20' '                                                          
*                                                                               
DBLOK    DS    0F                                                               
       ++INCLUDE DEDBLOCK                                                       
         SPACE 3                                                                
         EJECT                                                                  
       ++INCLUDE RGENEROL                                                       
         LTORG                                                                  
       ++INCLUDE RERISWRK                                                       
       ++INCLUDE REPIOBLK                                                       
       ++INCLUDE REGLBRW                                                        
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDCOMFACS                                                      
T80D01   CSECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
*************************************                                           
* CALL ALTERNATE CALENDAR BMONTH    *                                           
*************************************                                           
ALTDATES NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*   SET PARAMETER BLOCK OF ROUTINES                                             
*                                                                               
         L     RF,=V(GETBROAD)                                                  
         A     RF,RELO1            ADD 1ST LEVEL ADDRESSABILITY                 
         ST    RF,AGETBRD          SAVE A(GETBROAD)                             
         MVC   AGETDAY,VGETDAY     SAVE A(GETDAY)                               
         MVC   AADDAY,VADDAY       SAVE A(ADDAY)                                
         MVC   ADATCON,VDATCON     SET A(DATCON)                                
         MVC   ADTAMGR,VDATAMGR    SET A(DATAMGR)                               
         XC    DMCB+12(4),DMCB     CLEAR P4                                     
         MVC   DMCB+14(2),REPALPHA INSERT SIGNON ID                             
         GOTO1 =V(ALTBCMON),DMCB,(0,WORK),WORK+30,PARMLST,,STALETS,    X        
               0,RR=Y                                                           
         MVC   TBLBGN,WORK+30      START MONTH'S BCAST START DATE               
*                                                                               
****     GOTO1 VDATCON,DMCB,(3,WORK+30),(3,TBLBGN)                              
*                                                                               
         XC    DMCB+12(4),DMCB     CLEAR P4                                     
         MVC   DMCB+14(2),REPALPHA INSERT SIGNON ID                             
         GOTO1 =V(ALTBCMON),DMCB,(0,WORK+6),WORK+30,PARMLST,,STALETS,  X        
               0,RR=Y                                                           
         MVC   TBLEND,WORK+33      END MONTH'S BCAST END DATE                   
*                                                                               
***      GOTO1 VDATCON,DMCB,(0,WORK+36),(3,TBLEND)                              
*                                                                               
         XIT1                                                                   
TESTKEY  DC    X'01'                                                            
         DC    24X'00'                                                          
         DC    CL2'NB'                                                          
         DS    0F                                                               
PARMLST  DS    0XL(4*5)            ADDRESS PARAMETER BLOCK                      
AGETBRD  DS    A                   A(GETBROAD)                                  
AGETDAY  DS    A                   A(GETDAY  )                                  
AADDAY   DS    A                   A(ADDAY   )                                  
ADATCON  DS    A                   A(DATCON  )                                  
ADTAMGR  DS    A                   A(DATAMGR )                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************                                  
* ACTIVSTA:  SET ACTIVE REP IF MASTER STATION*                                  
**********************************************                                  
ACTIVSTA NTR1  BASE=*,LABEL=*                                                   
         LA    RF,MASTREPS                                                      
ASTA0020 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE?                                
         BE    ASTA0100            YES - FINISHED                               
         CLC   RSTAKREP,0(RF)      REP IN TABLE?                                
         BE    ASTA0040            YES - GET ACTIVE REP                         
         LA    RF,2(RF)            NO  - BUMP TO NEXT ENTRY                     
         B     ASTA0020            GO BACK FOR NEXT                             
ASTA0040 EQU   *                                                                
         LA    RF,RSTAELEM         SET A(1ST ELEMENT)                           
ASTA0060 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD REACHED?                       
         BE    ASTA0100            YES - NO ELEMENTS IN MASTER?                 
         CLI   0(RF),X'51'         FIND MASTER CONTROL ELEMENT                  
         BL    ASTA0080                                                         
         CLI   0(RF),X'53'                                                      
         BH    ASTA0100            NOT FOUND: EXIT                              
         MVC   RSTAKREP,2(RF)      LOAD ACTIVE REP CODE TO KEY                  
         MVC   KEY(27),RSTAREC     LOAD KEY FROM STA RECORD KEY                 
         BAS   RE,HIGH             READ FOR KEY                                 
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                NO  - MUST BE ON FILE                        
         BAS   RE,GETREC           RETRIEVE RECORD                              
         B     ASTA0100            EXIT                                         
ASTA0080 EQU   *                                                                
         ZIC   RE,1(RF)            BUMP TO NEXT ELEMENT                         
         AR    RF,RE                                                            
         B     ASTA0060            GO BACK FOR NEXT                             
ASTA0100 EQU   *                                                                
         XIT1                                                                   
MASTREPS DC    C'K3'               KATZ RADIO MASTER                            
         DC    C'MR'               KATZ TV    MASTER                            
         DC    C'IR'               INTEREP    MASTER                            
         DC    X'0000'                                                          
         DS    H'0'                                                             
         LTORG                                                                  
         EJECT                                                                  
***>>>   MARKET REQUEST: TABLE UP STATIONS IN MARKET                            
**********************************************************************          
*    FIND ALL STATIONS WITHIN MARKET - IF NO STATIONS, RETURN A      *          
*        CC NOT ZERO, WHICH WILL RETURN AN ERROR MESSAGE             *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
VALMKT   NTR1  BASE=*,LABEL=*                                                   
         XC    DUB,DUB             CLEAR A COUNTER                              
         LA    R3,2                SET INVALID FIELD MESSAGE                    
         L     R2,ASTASTOR         SET A(STATION STORAGE AREA)                  
         XC    KEY,KEY             CLEAR KEY                                    
         MVC   KEY(2),=X'8306'     INSERT RECORD TYPE CODE                      
         MVC   KEY+16(2),REPALPHA  INSERT REP CODE                              
         MVC   KEY+18(4),RISSTA+2  TAKE FOUR CHARACTERS FROM CODE               
*                                     TRAILING CHARS MUST BE BIN ZERO           
         CLI   RISSTAH+5,6         TEST MAX. LENGTH                             
         BH    VALMKTNE            ERROR IF > M-AAAA (6 CHARS)                  
         LA    R3,904              SET 'TABLE FULL' MESSAGE                     
         BAS   RE,HIGH             RETRIEVE FIRST KEY IN MARKET                 
         B     VALM0040                                                         
VALM0020 EQU   *                                                                
         BAS   RE,SEQ                                                           
VALM0040 EQU   *                                                                
         CLC   KEY(22),KEYSAVE                                                  
         BNE   VALMKTEQ            END OF MARKET                                
         CLI   KEY+26,C'C'         COMBO STATION?                               
         BE    VALM0020            YES - SKIP IT                                
         CLI   DUB+3,STASTOR#      IS TABLE FULL?                               
         BH    VALMKTNE            YES - SEND MESSAGE                           
*                                  NO                                           
         MVC   0(5,R2),KEY+22      INSERT STATION INTO TABLE                    
         LA    R2,5(R2)            BUMP TO NEXT SLOT                            
         XC    0(5,R2),0(R2)       CLEAR NEXT SLOT                              
         L     RF,DUB                                                           
         LA    RF,1(RF)                                                         
         ST    RF,DUB              PUT COUNTER BACK                             
*                                                                               
         B     VALM0020            GO BACK FOR NEXT KEY                         
*                                                                               
VALMKTNE EQU   *                                                                
         ST    R3,DUB              SET ERROR CODE BACK                          
         LTR   RB,RB               SET CC NOT ZERO                              
         B     VALM0900            EXIT                                         
VALMKTEQ EQU   *                                                                
         LA    R3,905                                                           
         L     RE,ASTASTOR         SET A(STATION STORAGE AREA)                  
         OC    0(5,RE),0(RE)       ANY ENTRIES IN TABLE?                        
         BZ    VALMKTNE            NO  - EXIT WITH ERROR                        
*                                                                               
*   CMBOSTAS WILL DOUBLE AS THE DRIVER FOR STATIONS WITHIN MARKET.              
*        SAME FLAG/STORAGE WILL DRIVE THE LOGIC IN BOTH THE                     
*        TOTAL DOLLAR DISPLAY AS WELL AS THE LIST FUNCTIONS.                    
*                                                                               
         MVI   CMBOREP,C'Y'        SET COMBO REP/MKT FLAG TO 'YES'              
         L     R2,ASTASTOR         A(LIST OF COMBO STATIONS)                    
***>>>   LA    R2,CMBOSTAS         A(LIST OF COMBO STATIONS)                    
         LR    RF,R2               SAVE DISPLACEMENT, NOT ADDRESS               
         SR    RF,RA                                                            
         ST    RF,ACMBOSTA                                                      
         MVC   TBLSTA,0(R2)        PRIME THE FIRST STATION                      
***>>>   MVC   TBLSTA,CMBOSTAS     PRIME THE FIRST STATION                      
*                                                                               
         CR    RB,RB               YES - SET CC ZERO                            
VALM0900 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***>>>   MARKET REQUEST: TABLE UP STATIONS IN MARKET END                        
*************************************                                           
* VALIDATE GROUP AS PART OF STATION *                                           
*************************************                                           
VALGRP   NTR1  BASE=*,LABEL=*                                                   
         CLI   RISSTA+2,C'*'       SET REQUEST?                                 
         BNE   VALGRP0             NO                                           
         GOTO1 =A(VALSET),DMCB,=C'GS',=A(GRPDISP),RISSTA+3,RR=Y                 
         BNE   VALGRPNE                                                         
         CLI   LISTBYTE,0                                                       
         BNE   ERRLIST                                                          
*PFOL    MVI   LISTBYTE,LISTGRP    <- THEY DON'T LIKE SAFE LIMITS               
         XC    RISSTAN,RISSTAN                                                  
*PFOL    ZIC   RF,NUMLIST                                                       
*PFOL    LA    RF,1(RF)                                                         
*PFOL    STC   RF,NUMLIST                                                       
         B     VALGRPEQ                                                         
*                                                                               
VALGRP0  LA    R3,GRUPERR                                                       
         MVC   TBLGRP(7),ALL                                                    
         CLI   5(R2),6                                                          
         BH    VALGRPNE                                                         
         CLC   RISSTA+2(3),=C'ALL'    DISALLOW G-ALL                            
         BE    VALGRPNE                                                         
         CLC   RISSTA+2(4),=C'LIST'                                             
         BNE   VALGRP2                                                          
         CLI   LISTBYTE,0                                                       
         BNE   ERRLIST                                                          
         MVI   LISTBYTE,LISTGRP                                                 
         XC    RISSTAN,RISSTAN                                                  
         ZIC   RF,NUMLIST                                                       
         LA    RF,1(RF)                                                         
         STC   RF,NUMLIST                                                       
         B     VALGRPEQ                                                         
*                                                                               
VALGRP2  OC    RISSTA,SPACES       SET GROUP KEY                                
         MVI   KEY,7                                                            
         XC    KEY+1(22),KEY+1                                                  
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),RISSTA+2                                               
         CLI   5(R2),4             TEST MAX. LENGTH                             
         BH    VALGRPNE                                                         
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VALGRPNE                                                         
*                                                                               
         BAS   RE,GETREC                                                        
         MVC   RISSTAN,RGRPNAME                                                 
         MVC   TBLGRP(2),RGRPKGRP                                               
         CLI   RGRPKGRP+1,C' '                                                  
         BNE   *+10                                                             
         MVC   TBLSBGP,ALL                                                      
         B     VALGRPEQ                                                         
*                                                                               
VALGRPNE LTR   RB,RB                                                            
         B     *+6                                                              
VALGRPEQ CR    RB,RB                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*   CHEKCMBO:  COMPARE REP AGAINST COMBO REP TABLE.  IF FOUND IN                
*        TABLE, RETRIEVE STATIONS PARTICIPATING IN THE COMBO FROM               
*        THE X'0A' ELEMENTS OF THE STATION RECORD, WHICH WILL BE                
*        CYCLED THROUGH THE 'READ' LOGIC RATHER THAN THE WXXX-C                 
*        STATION ORIGINALLY REQUESTED.                                          
***********************************************************************         
CHEKCMBO NMOD1 0,*CKCB*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         LA    R1,CMBOREPS         A(COMBO REP TABLE)                           
         MVI   CMBOREP,C'N'        RESET COMBO REP FLAG                         
         L     RE,ASTASTOR         RESET LIST OF COMBO STATIONS                 
         XC    0(251,RE),0(RE)                                                  
***>>>   XC    CMBOSTAS,CMBOSTAS   RESET LIST OF COMBO STATIONS                 
CHEK0010 EQU   *                                                                
         CLI   0(R1),0             END OF TABLE?                                
         BE    CHEK0050            YES - FINISHED - NOT COMBO REP               
         CLC   REPALPHA,0(R1)      REP FOUND IN TABLE?                          
         BE    CHEK0020            YES                                          
         LA    R1,2(R1)            NO  - BUMP TO NEXT ENTRY                     
         B     CHEK0010            GO BACK FOR NEXT                             
CHEK0020 EQU   *                                                                
         MVI   CMBOREP,C'Y'        SET COMBO REP FLAG TO 'YES'                  
         L     R2,ASTASTOR         A(LIST OF COMBO STATIONS)                    
***>>>   LA    R2,CMBOSTAS         A(LIST OF COMBO STATIONS)                    
         LR    RF,R2               SAVE DISPLACEMENT, NOT ADDRESS               
         SR    RF,RA                                                            
         ST    RF,ACMBOSTA         SET A(1ST STATION IN LIST)                   
         MVC   0(5,R2),RSTAKSTA    INSERT COMBO STATION INTO LIST               
         LA    R2,5(R2)            POSITION TO SECOND STATION                   
         LA    R1,RSTAELEM         RETRIEVE COMBO ELEMENTS                      
CHEK0030 EQU   *                                                                
         ZIC   R3,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R3                                                            
         CLI   0(R1),0             END OF RECORD?                               
         BE    CHEK0050            YES - FINISHED                               
         CLI   0(R1),X'0A'         COMBO STATION ELEMENT?                       
         BNE   CHEK0030            NO  - GO BACK FOR NEXT ELEMENT               
         CLI   1(R1),7             OLD FORMAT ELEMENT?                          
         BNH   CHEK0040            YES                                          
         CLI   7(R1),C'-'          NO  - NON-PARTICIPATING STATION?             
         BE    CHEK0030            YES - DON'T TABLE STATION                    
CHEK0040 EQU   *                                                                
         MVC   0(5,R2),2(R1)       YES - MOVE IN STATION                        
         LA    R2,5(R2)            BUMP STATION LIST                            
         B     CHEK0030            GO BACK FOR NEXT ELEMENT                     
CHEK0050 EQU   *                                                                
         XMOD1                                                                  
**********************************************************************          
*                   0.0.0.0.0.0.0.0.0.1.1.1.1.1.1.                              
*                   1.2.3.4.5.6.7.8.9.0.1.2.3.4.5.6.7.8.9.0.1.2.3.4.5.          
CMBOREPS DC    CL44'IRIRTOI1MGGPI2I8I9D4IFS1CMCNAQSJRTK9L7IBNXUO'               
         DC    CL44'K3K3BFCREAKFKUK4S3B3RSU1U2MSQTV5QDNUG8J0WC00'               
*                                                                               
*   NOTE:  'COMBO' STATION REPORTING MAY BE TURNED OFF BY INSERTING             
*          '00' IN FIRST ENTRY OF TABLE.  FOR THIS REASON, 'IR' HAS             
*          BEEN INCLUDED TWICE SO THAT, IF TABLE IS TURNED OFF, A               
*          TABLE VALUE IS NOT INADVERTENTLY LOST.                               
**********************************************************************          
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*                                                                               
**********************************************************************          
SUB1     NMOD1 0,*SUB1*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         GOTO1 =V(GETBROAD),DMCB,(1,WORK),WORK+12,VGETDAY,VADDAY,RR=YES         
         MVC   WORK+6(2),WORK+18   CALCULATE START BDCAST YEAR                  
         MVC   WORK+8(4),=C'0115'  SET JAN15 OF YEAR                            
         GOTO1 =V(GETBROAD),DMCB,(1,WORK+6),WORK+12,VGETDAY,VADDAY,    X        
               RR=YES                                                           
*                                                                               
*  CALCULATE WEEK # FROM BDCAST YEAR STRT THRU AS AT DATE                       
*                                                                               
         GOTO1 =V(PERVERT),DMCB,WORK+12,WORK,RR=YES                             
         ZICM  RF,DMCB+12,2        SAVE WEEK #                                  
         OC    DMCB+10(2),DMCB+10  ANY PERVERT DAYS REMAINING?                  
         BNZ   *+6                 YES - DON'T ADJUST WEEK #                    
         BCTR  RF,0                NO  - DECREMENT WEEK #                       
         STCM  RF,3,WORK+32        SAVE WEEK #                                  
*                                                                               
         GOTO1 VADDAY,DMCB,(C'Y',WORK+6),(0,WORK+6),-1                          
*                                  CALC PRIOR YEAR, SAME WEEK #                 
*                                                                               
         GOTO1 =V(GETBROAD),DMCB,(1,WORK+6),WORK+12,VGETDAY,VADDAY,    X        
               RR=YES                                                           
         ZICM  R2,WORK+32,2        INSERT WEEK # OF CURRENT DATE                
         CH    R2,=H'52'           IS CURRENT WEEK # 53?                        
*                                                                               
*  WEEK # IS UNDERSTATED BY 1.  DAYS 1-7 ARE WEEK 1, BUT PERVERT                
*    CALCULATION RETURNS 0.                                                     
*                                                                               
         BNE   SUB10020            NO                                           
         BCTR  R2,0                YES - SET IT TO 52 FOR OTHER YEARS           
SUB10020 EQU   *                                                                
         MH    R2,=H'7'            MULTIPLY BY 7 = # OF DAYS                    
         GOTO1 =V(ADDAY),DMCB,WORK+12,WORK+18,(R2),RR=YES                       
         MVC   WORK(6),WORK+18     INSERT ADJUSTED DATE                         
*                                                                               
         XMOD1                                                                  
         LTORG                                                                  
         EJECT                                                                  
*******************************************************************             
* VALIDATE OPTIONS (IN DATE FIELD)                                              
*******************************************************************             
VALOPS   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,RISDATEH                                                      
         XC    SAVEPRD,SAVEPRD                                                  
**JRD    TM    4(R2),X'20'                                                      
**JRD    BO    VALOPEQ                                                          
*                                                                               
         OI    VIEWS,X'80'            SET NO BOTTOM PAGE TOTALS                 
         MVI   PFJUMP,0                                                         
         MVI   VIEWS2,0                                                         
         MVI   DLRS,0                                                           
         MVI   BYTE2,0                                                          
         XC    NEXTBYTE,NEXTBYTE                                                
         MVI   ESTBUCKT,X'03'      ALTERNATE DOLLAR BUCKETS                     
         MVI   INVBUCKT,X'04'                                                   
*                                                                               
         XC    DVSFILT,DVSFILT                                                  
         XC    DVTFILT,DVTFILT                                                  
         XC    PPERFILT,PPERFILT                                                
*                                                                               
         CLI   5(R2),0                                                          
         BE    VALOP30             NO DATE/OPTION                               
*                                                                               
         GOTO1 =V(SCANNER),DMCB,RISDATEH,(4,GRID),RR=RELO1                      
         CLI   DMCB+4,0                                                         
         BE    VALOPNE                                                          
         MVC   BYTE2,DMCB+4                                                     
*                                                                               
         LA    R3,GRID                                                          
*--------------------*                                                          
* FIRST OPTION CHECK *                                                          
*--------------------*                                                          
         ZIC   R4,0(R3)            LENGTH TO COMPARE                            
         BCTR  R4,0                                                             
         LTR   R4,R4                                                            
         BM    VALOP30             NO FIRST FIELD                               
*                                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'PLIST'                                             
         BNE   VALOP10                                                          
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOPNE             YES - ERROR                                  
*        LA    R3,32(R3)           NEXT LINE                                    
*        CLC   22(3,R3),=CL3'   '  NO PRODUCT FILTER?                           
*        BE    *+10                                                             
*                                                                               
         MVI   PFJUMP,1                                                         
         CLI   LISTBYTE,0                                                       
         BNE   ERRLIST                                                          
         MVI   LISTBYTE,LISTPRD                                                 
         B     VALOP100                                                         
*                                                                               
VALOP10  EQU   *                                                                
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'OLIST'                                             
         BNE   VALOP12                                                          
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOPNE             YES - ERROR                                  
*                                                                               
         MVI   PFJUMP,1                                                         
         CLI   LISTBYTE,0                                                       
         BNE   ERRLIST                                                          
         MVI   LISTBYTE,LISTTRAF                                                
         B     VALOP100                                                         
*                                                                               
VALOP12  EQU   *                                                                
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'LIST$'                                             
         BNE   VALOP14                                                          
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOPNE             YES - ERROR                                  
*                                                                               
         MVI   PFJUMP,1                                                         
         OI    VIEWS,X'01'                                                      
         B     VALOP100                                                         
*                                                                               
VALOP14  EQU   *                                                                
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'LISTD'                                             
         BNE   VALOP16                                                          
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOPNE             YES - ERROR                                  
*                                                                               
         MVI   PFJUMP,2                                                         
         OI    VIEWS,X'02'            SET LISTD FLAG                            
         B     VALOP100                                                         
                                                                                
VALOP16  EQU   *                                                                
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'LISTP'   PRODUCT $ LIST                            
         BNE   VALOP17                                                          
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOPNE             YES - ERROR                                  
*                                                                               
         OI    LISTBYTE,LISTPNAM                                                
         MVI   PFJUMP,3                                                         
         OI    VIEWS,X'08'            SET LISTP FLAG                            
         B     VALOP100                                                         
                                                                                
VALOP17  EQU   *                                                                
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'LISTPX'  EXPANDED PRODUCT $ LIST                   
         BNE   VALOP18                                                          
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOPNE             YES - ERROR                                  
*                                                                               
         OI    LISTBYTE,LISTPNAM                                                
         MVI   PFJUMP,3                                                         
         OI    VIEWS,X'08'         SET LISTP FLAG                               
         OI    VIEWS3,X'80'        SET EXPAND FLAG                              
         B     VALOP100                                                         
                                                                                
VALOP18  EQU   *                                                                
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'BUY$'                                              
         BNE   VALOP20                                                          
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOPNE             YES - ERROR                                  
*                                                                               
         MVI   PFJUMP,1                                                         
         OI    VIEWS,X'04'                                                      
         MVI   DLRS,C'-'           EXCLUDE $0 CONTRACTS                         
         B     VALOP100            HAVE IT CHECK OPTIONS                        
                                                                                
VALOP20  EQU   *                                                                
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'ULIST'    UNCONFIRMED                              
         BNE   VALOP22                                                          
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOPNE             YES - ERROR                                  
*                                                                               
         MVI   PFJUMP,1                                                         
         MVI   TBLCTYP,C'U'                                                     
         CLI   LISTBYTE,0                                                       
         BNE   ERRLIST                                                          
         MVI   LISTBYTE,LISTUNCF                                                
         B     VALOP100                                                         
*                                                                               
VALOP22  EQU   *                                                                
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOP29             YES - MUST BE ADDITIONAL OPTION              
         CLI   12(R3),C'-'         DASH IN FIRST SLOT?                          
         BE    VALOP29             YES - MUST BE ADDITIONAL OPTION              
*                                                                               
         CLI   0(R3),0                                                          
         BNE   VALOP32                                                          
         B     VALOP30                                                          
*                                                                               
VALOP29  DS    0H                  RESET TO ASSUME ,OPTIONS                     
         LA    R3,GRID-32                                                       
         ZIC   RE,BYTE2                                                         
         LA    RE,1(RE)                                                         
         STC   RE,BYTE2                                                         
*                                  NO DATE/OPTION INPUT                         
VALOP30  GOTO1 VDATCON,DMCB,(5,0),(X'20',DUB)                                   
         MVC   WRKDATE(2),DUB+2    MONTH                                        
         MVI   WRKDATE+2,C'/'                                                   
         MVC   WRKDATE+3(2),DUB+4  DAY                                          
         MVI   WRKDATE+5,C'/'                                                   
         MVC   WRKDATE+6(2),DUB    YEAR                                         
         B     VALOP34                                                          
*                                                                               
VALOP32  DS    0H                  USE KEY FOR DUMMY FIELD                      
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         MVC   KEY+5(1),0(R3)                                                   
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),12(R3)                                                   
*                                                                               
         BAS   RE,DASH                                                          
         LTR   R7,R7               TEST 1 OR 2 DATES                            
         BNZ   VALOP42             2 DATES                                      
*                                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   WRKDATE(0),12(R3)                                                
VALOP34  GOTO1 VDATVAL,DMCB,(0,WRKDATE),WORK                                    
         OC    DMCB(4),DMCB                                                     
*        BZ    VALOPNE                                                          
         BZ    VALOP29                                                          
*                                                                               
         GOTO1 VMONDAY,DMCB,WORK,END2                                           
*                                  SUBTRACT ONE YEAR                            
         GOTO1 =A(SUB1),DMCB,(RC),RR=Y                                          
*                                                                               
*        CODE TO GET ROUND THE 53 WEEK YEAR PROBLEM                             
*        DATES 9/24/84 - 12/30/84 MUST HAVE PRIOR WEEK BACKED UP ONE            
*        DATES 12/31/84 - 12/29/85 MUST HAVE PRIOR WEEK FORWARD ONE             
*                                                                               
         GOTO1 VGETDAY,DMCB,WORK,FULL   GET PRIOR DAY OF WEEK                   
         CLC   WORK(6),=C'830924'  *                                            
         BL    VALOP40             *                                            
         CLC   WORK(6),=C'831230'  *                                            
         BH    VALOP36             *                                            
         CLI   DMCB,6              * 9/24/83 - 12/30/83 :                       
         BNL   VALOP40             * MON - FRI SUBTRACT ONE WEEK                
         L     R5,=F'-7'           *                                            
         B     VALOP38                                                          
*                                                                               
VALOP36  CLC   WORK(6),=C'841229'  * 12/31/83 - 2/26/84 :                       
         BH    VALOP40             * SAT - SUN ADD ONE WEEK                     
         CLI   DMCB,6              *                                            
         BL    VALOP40             * 2/27/84 - 12/29/84 :                       
         BH    *+14                * SUNDAY ADD ONE WEEK                        
         CLC   WORK(6),=C'840303'  *                                            
         BNL   VALOP40             *                                            
         L     R5,=F'7'            *                                            
*                                                                               
VALOP38  ST    R5,DMCB+8                                                        
         GOTO1 VADDAY,DMCB,WORK,DUB  ADD/SUBTRACT A WEEK                        
         MVC   WORK(6),DUB                                                      
*                                                                               
VALOP40  GOTO1 VMONDAY,DMCB,WORK,END1                                           
         XC    START1,START1                                                    
         XC    START2,START2                                                    
         B     VALOP100                                                         
*                                                                               
VALOP42  BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   DUB(0),RISDATE                                                   
*                                                                               
         GOTO1 VDATVAL,DMCB,(0,DUB),WORK                                        
         OC    DMCB(4),DMCB                                                     
*        BZ    VALOPNE                                                          
         BZ    VALOP29                                                          
*                                                                               
         GOTO1 VMONDAY,DMCB,WORK,START2                                         
*                                  SUBTRACT ONE YEAR                            
         GOTO1 =A(SUB1),DMCB,(RC),RR=Y                                          
         GOTO1 VMONDAY,DMCB,WORK,START1                                         
*                                                                               
         LA    R7,10(R2,R7)                                                     
*                                                                               
         GOTO1 VDATVAL,DMCB,(0,0(R7)),WORK                                      
         OC    DMCB(4),DMCB                                                     
*        BZ    VALOPNE                                                          
         BZ    VALOP29                                                          
*                                                                               
         GOTO1 VMONDAY,DMCB,WORK,END2                                           
*                                  SUBTRACT ONE YEAR                            
         GOTO1 =A(SUB1),DMCB,(RC),RR=Y                                          
         GOTO1 VMONDAY,DMCB,WORK,END1                                           
*--------------------------*                                                    
* ADDITIONAL OPTIONS CHECK *                                                    
*--------------------------*                                                    
VALOP100 EQU   *                                                                
         TM    VIEWS,X'01'+X'02'+X'08'+X'04'                                    
         BZ    VALOP102                                                         
*                                                                               
         TM    LISTBYTE,LISTPNAM   IS IT ALREADY SET FOR PRODLIST?              
         BO    VALOP102            OK                                           
         CLI   LISTBYTE,0                                                       
         BNE   ERRLIST                                                          
         MVI   LISTBYTE,LISTDLRS                                                
*                                                                               
VALOP102 EQU   *                                                                
         LA    R3,32(R3)                                                        
         ZIC   R2,BYTE2                                                         
         BCTR  R2,0                                                             
         LTR   R2,R2                                                            
         BNP   VALOPEQ             NO OPTIONS                                   
*                                                                               
VALOP110 DS    0H                                                               
         ZIC   R4,0(R3)                                                         
         BCTR  R4,0                                                             
*                                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'D'                                                 
         BNE   VALOP112                                                         
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOPNE             YES - ERROR                                  
*                                                                               
         TM    VIEWS2,X'08'        ALREADY EXCLUDE DARE?                        
         BO    VALOPNE             YES                                          
         OI    VIEWS2,X'04'                                                     
         B     VALOP300                                                         
*                                                                               
VALOP112 DS    0H                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'-D'                                                
         BNE   VALOP114                                                         
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOPNE             YES - ERROR                                  
*                                                                               
         TM    VIEWS2,X'04'        ALREADY INCLUDE DARE?                        
         BO    VALOPNE             YES                                          
         OI    VIEWS2,X'08'                                                     
         B     VALOP300                                                         
*                                                                               
VALOP114 DS    0H                                                               
         ZIC   R4,0(R3)                                                         
         BCTR  R4,0                                                             
*                                                                               
                                                                                
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'P'                                                 
         BNE   VALOP116                                                         
         CLI   1(R3),0                                                          
         BE    VALOP115                                                         
         CLI   1(R3),3                                                          
         BNE   VALOPNE             PRD CODE OR FIRST 3 CHAR OF NAME             
         MVC   SAVEPRD,22(R3)                                                   
         B     VALOP300                                                         
* HQ     CLI   1(R3),0             VALUE SET?                                   
* HQ     BNE   VALOPNE             YES - ERROR                                  
*                                                                               
VALOP115 DS    0H                                                               
         TM    VIEWS2,X'02'        ALREADY EXCLUDE PENDING?                     
         BO    VALOPNE             YES                                          
         OI    VIEWS2,X'01'                                                     
         B     VALOP300                                                         
*                                                                               
VALOP116 DS    0H                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'-P'                                                
         BNE   VALOP118                                                         
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOPNE             YES - ERROR                                  
*                                                                               
         TM    VIEWS2,X'01'        ALREADY INCLUDE PENDING?                     
         BO    VALOPNE             YES                                          
         OI    VIEWS2,X'02'                                                     
         B     VALOP300                                                         
*                                                                               
VALOP118 DS    0H                                                               
         ZIC   R4,0(R3)                                                         
         BCTR  R4,0                                                             
*                                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'F'                                                 
         BNE   VALOP120                                                         
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOPNE             YES - ERROR                                  
*                                                                               
         TM    VIEWS2,X'80'        ALREADY EXCLUDE FORECAST?                    
         BO    VALOPNE             YES                                          
         OI    VIEWS2,X'40'                                                     
         B     VALOP300                                                         
*                                                                               
VALOP120 DS    0H                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'-F'                                                
         BNE   VALOP122                                                         
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOPNE             YES - ERROR                                  
*                                                                               
         TM    VIEWS2,X'40'        ALREADY INCLUDE FORECAST?                    
         BO    VALOPNE             YES                                          
         OI    VIEWS2,X'80'                                                     
         B     VALOP300                                                         
*                                                                               
VALOP122 DS    0H                                                               
         ZIC   R4,0(R3)                                                         
         BCTR  R4,0                                                             
*                                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'X'                                                 
         BNE   VALOP124                                                         
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOPNE             YES - ERROR                                  
*                                                                               
         TM    VIEWS3,X'40'        ALREADY EXCLUDE TAKEOVER?                    
         BO    VALOPNE             YES                                          
         OI    VIEWS3,X'20'                                                     
         B     VALOP300                                                         
*                                                                               
VALOP124 DS    0H                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'-X'                                                
         BNE   VALOP126                                                         
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOPNE             YES - ERROR                                  
*                                                                               
         TM    VIEWS3,X'20'        ALREADY INCLUDE TAKEOVER?                    
         BO    VALOPNE             YES                                          
         OI    VIEWS3,X'40'                                                     
         B     VALOP300                                                         
*                                                                               
VALOP126 DS    0H                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'A'                                                 
         BNE   VALOP128                                                         
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOPNE             YES - ERROR                                  
*                                                                               
         MVI   ESTBUCKT,X'53'      ALTERNATE DOLLAR BUCKETS                     
         MVI   INVBUCKT,X'54'                                                   
         B     VALOP300                                                         
*                                                                               
VALOP128 DS    0H                                                               
*                                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'$T'                                                
         BNE   VALOP129                                                         
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOPNE             YES - ERROR                                  
*                                                                               
         MVI   ESTBUCKT,X'63'      TRADE DOLLAR BUCKETS                         
         MVI   INVBUCKT,X'64'                                                   
         B     VALOP300                                                         
*                                                                               
VALOP129 DS    0H                                                               
*                                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'DS'                                                
         BNE   VALOP146                                                         
         CLI   1(R3),0             VALUE SET?                                   
         BE    VALOPNE             NO - ERROR                                   
*                                                                               
         BAS   RE,VALDEVSL                                                      
         BNE   VALOPNE                                                          
         B     VALOP300                                                         
*                                                                               
VALOP146 DS    0H                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'DT'                                                
         BNE   VALOP148                                                         
         CLI   1(R3),0             VALUE SET?                                   
         BE    VALOPNE             NO - ERROR                                   
*                                                                               
         CLI   22(R3),C'*'         SET REQUEST?                                 
         BNE   VOP146A                                                          
*                                                                               
         GOTO1 =A(VALSET),DMCB,=C'DT',=A(DVTDISP),23(R3),RR=Y                   
         BNE   VALOPNE                                                          
         B     VALOP300                                                         
*                                                                               
VOP146A  DS    0H                                                               
         BAS   RE,VALDEVTP                                                      
         BNE   VALOPNE                                                          
         B     VALOP300                                                         
*                                                                               
VALOP148 DS    0H                                                               
*-----------------------------------------------------*                         
* OPTIONS FOR LIST$ AND LISTD AND LISTP AND BUY$ ONLY *                         
*-----------------------------------------------------*                         
         TM    VIEWS,X'01'+X'02'+X'08'+X'04'                                    
         BZ    VALOP299                                                         
*                                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'TOT'  PRINT BOTTOM PAGE TOTALS                     
         BNE   VALOP202                                                         
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOPNE             YES - ERROR                                  
*                                                                               
         NI    VIEWS,X'FF'-X'80'         YES                                    
         B     VALOP300                                                         
*                                                                               
VALOP202 DS    0H                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'0'    PRINT ONLY $0                                
         BNE   VALOP204                                                         
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOPNE             YES - ERROR                                  
*                                                                               
         CLI   DLRS,C'-'           ALREADY EXCLUDE $0?                          
         BE    VALOPNE             YES                                          
         MVI   DLRS,C'0'                                                        
         B     VALOP300                                                         
*                                                                               
VALOP204 DS    0H                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'-0'   EXCLUDE $0                                   
         BNE   VALOP206                                                         
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOPNE             YES - ERROR                                  
*                                                                               
         CLI   DLRS,C'0'           ALREADY PRINT ONLY $0?                       
         BE    VALOPNE             YES                                          
         MVI   DLRS,C'-'                                                        
         B     VALOP300                                                         
*                                                                               
VALOP206 DS    0H                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'C'    COMBO STATIONS ONLY                          
         BNE   VALOP208                                                         
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOPNE             YES - ERROR                                  
*                                                                               
         TM    FILTFLG,X'02'       EXCLUDE COMBO STATIONS?                      
         BNZ   VALOPNE             YES                                          
         OI    FILTFLG,X'01'       COMBO STATIONS ONLY                          
         B     VALOP300                                                         
*                                                                               
VALOP208 DS    0H                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'-C'   EXCLUDE COMBO STATIONS                       
         BNE   VALOP210                                                         
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOPNE             YES - ERROR                                  
*                                                                               
         TM    FILTFLG,X'01'       COMBO STATIONS ONLY?                         
         BNZ   VALOPNE             YES                                          
         OI    FILTFLG,X'02'       EXCLUDE COMBO STATIONS                       
         B     VALOP300                                                         
*                                                                               
VALOP210 DS    0H                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'-S'   EXCLUDE CONTRACTS WITH SHR PCT               
         BNE   VALOP212                                                         
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOPNE             YES - ERROR                                  
*                                                                               
         TM    VIEWS2,X'10'        ONLY CONTRACTS WITH SHR PCT?                 
         BO    VALOPNE             YES                                          
         OI    VIEWS2,X'20'                                                     
         B     VALOP300                                                         
*                                                                               
VALOP212 DS    0H                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'S'    ONLY CONTRACTS WITH SHR PCT                  
         BNE   VALOP214                                                         
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOPNE             YES - ERROR                                  
*                                                                               
         TM    VIEWS2,X'20'        EXCLUDE CONTRACTS WITH SHR PCT               
         BO    VALOPNE             YES                                          
         OI    VIEWS2,X'10'                                                     
         B     VALOP300                                                         
*                                                                               
VALOP214 DS    0H                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'SR'   SENT BY REP TODAY FILTER                     
         BNE   VALOP216                                                         
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOPNE             YES - ERROR                                  
*                                                                               
         OI    VIEWS,X'40'                                                      
         B     VALOP300                                                         
*                                                                               
VALOP216 DS    0H                                                               
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=CL8'SS'   SENT BY STATION TODAY FILTER                 
         BNE   VALOP218                                                         
         CLI   1(R3),0             VALUE SET?                                   
         BNE   VALOPNE             YES - ERROR                                  
*                                                                               
         OI    VIEWS,X'20'                                                      
         B     VALOP300                                                         
*                                                                               
VALOP218 DS    0H                                                               
*                                                                               
VALOP299 DS    0H                                                               
         B     VALOPNE                                                          
*                                                                               
VALOP300 LA    R3,32(R3)                                                        
         BCT   R2,VALOP110                                                      
*                                                                               
VALOPEQ  DS    0H                                                               
         LA    RE,GRID             CLEAR GRID                                   
         LA    RF,300                                                           
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
VALYES   CR    RB,RB                                                            
         B     *+6                                                              
VALNO    EQU   *                                                                
VALOPNE  LTR   RB,RB                                                            
         XIT1                                                                   
         SPACE 2                                                                
***********************************************************************         
* VALIDATE DEVSAL OPTION                                                        
*    R3 POINTS TO SCANNER LINE                                                  
***********************************************************************         
VALDEVSL NTR1                                                                   
         CLI   1(R3),L'RDSPKSAL    TOO LONG?                                    
         BH    VALNO               YES                                          
*                                                                               
         OC    22(L'RDSPKSAL,R3),=CL10' '   UPPERCASE                           
         LA    RE,KEY                                                           
         USING RDSPKEY,RE                                                       
         XC    RDSPKEY,RDSPKEY                                                  
         MVI   RDSPKTYP,X'3A'                                                   
         MVC   RDSPKSAL,22(R3)                                                  
         MVC   RDSPKREP,REPALPHA                                                
         DROP  RE                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDSPKEY),KEYSAVE                                           
         BNE   VALNO               DEV. SALESPERSON NOT ON RECORD               
         MVC   DVSFILT,22(R3)                                                   
         B     VALYES                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE DEVTYPE OPTION                                                       
*    R3 POINTS TO SCANNER LINE                                                  
***********************************************************************         
VALDEVTP NTR1                                                                   
         CLI   1(R3),L'RDCTKCTY    TOO LONG?                                    
         BH    VALNO               YES                                          
*                                                                               
         OC    22(L'RDCTKCTY,R3),=CL10' '   UPPERCASE                           
         LA    RE,KEY                                                           
         USING RDCTKEY,RE                                                       
         XC    RDCTKEY,RDCTKEY                                                  
         MVI   RDCTKTYP,X'3B'                                                   
         MVC   RDCTKCTY,22(R3)                                                  
         MVC   RDCTKREP,REPALPHA                                                
         DROP  RE                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDCTKEY),KEYSAVE                                           
         BNE   VALNO               DEV. SALESPERSON NOT ON RECORD               
         MVC   DVTFILT,22(R3)                                                   
         B     VALYES                                                           
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE SET REQUEST                                                          
*    P1 POINTS TO TWO CHARACTER SET TYPE                                        
*    P2 IS WHERE TO STORE THE ADDRESS OF THE TABLE IN THE TWA                   
*    P3 IS THE ADRESS OF THE SETID                                              
*    ALSO SETS NXTSETA                                                          
*                                                                               
* STORE DISPLACEMENTS!!!!                                                       
***********************************************************************         
VALSET   NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)                                                         
         L     R3,8(R1)                                                         
*                                                                               
         LR    R4,RA               SET DISPLACMENT TO SET                       
         AH    R4,=Y(NEXTDISP)                                                  
         L     RE,4(R1)                                                         
         L     RE,0(RE)                                                         
         AR    RE,RA                                                            
         MVC   0(4,RE),0(R4)                                                    
*                                                                               
         L     R4,0(R4)            POINT R4 TO SET AREA                         
         AR    R4,RA                                                            
*                                                                               
         XC    KEY,KEY             GET SET RECORD                               
         LA    R6,KEY                                                           
         USING RSETREC,R6                                                       
         MVI   RSETKTYP,RSETKTYQ                                                
         MVC   RSETKREP,REPALPHA                                                
*                                                                               
*   DETERMINE IF SUBSIDIARY IS IN PROCESS, AND, IF SO, IF SET NEEDS             
*        MASTER REP CODE RATHER THAN SUBSID.                                    
*                                                                               
         OC    MASTRREP,MASTRREP   SUBSIDIARY REQUEST?                          
         BZ    VASE0040            NO  -                                        
         LA    RF,SETNAMES         YES - CHECK IF MASTER NEEDED                 
         B     VASE0020                                                         
SETNAMES DC    C'AGADSPGSDTPPREMKXX'                                            
VASE0020 EQU   *                                                                
         CLC   =C'XX',0(RF)        END OF LIST?                                 
         BE    VASE0040            YES - MASTER REP NOT NEEDED                  
         CLC   0(2,RF),0(R2)       SET NAME IN LIST?                            
         BE    VASE0030            YES -                                        
         LA    RF,2(RF)            NO  - BUMP TO NEXT ENTRY IN LIST             
         B     VASE0020                                                         
VASE0030 EQU   *                                                                
         MVC   RSETKREP,MASTRREP   INSERT MASTER REP INTO KEY                   
VASE0040 EQU   *                                                                
*                                                                               
         MVC   RSETKSET,0(R2)                                                   
         MVC   RSETKID,0(R3)                                                    
         OC    RSETKID,SPACES                                                   
         DROP  R6                                                               
*                                                                               
         BAS   RE,HIGH             NOTE DMGR OFF R9                             
         CLC   KEY(L'RSETKEY),KEYSAVE                                           
         BNE   NO                                                               
*                                                                               
         GOTO1 GETREC              NOTE DMGR OFF R9                             
*                                                                               
         MVI   0(R4),X'80'         INDICATE IS A SET                            
         MVC   1(2,R4),0(R2)                                                    
*                                                                               
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'01'        NEW DESCRIPTION ELEMENT                      
         BAS   RE,GETEL2                                                        
         BNE   VNORMSET            NONE - MUST BE NORMAL SET RECORD             
*                                                                               
         USING RSET1DES,R6                                                      
         TM    RSET1FLG,X'08'      EXCLUSION SET?                               
         BZ    *+8                                                              
         OI    0(R4),X'40'         YES - SET EXCLUSION FLAG                     
*                                                                               
         TM    RSET1FLG,X'80'      SET OF SETS?                                 
         BNZ   VSETOSET            YES                                          
         DROP  R6                                                               
************************************************************                    
VNORMSET DS    0H                  PROCESS NORMAL SET RECORD                    
         XC    4(10,R4),4(R4)      CLEAR START OF SET                           
*                                                                               
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'20'        GET MEMBER ELEMENT                           
         BAS   RE,GETEL2                                                        
         BE    *+6                 FOUND                                        
         DC    H'0'                NOT FOUND                                    
         USING RSETMEMD,R6                                                      
         ZIC   R5,RSETMLEN         MEMBER LENGTH                                
         DROP  R6                                                               
*                                                                               
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         AR    RF,R6               END OF ELEM                                  
         LA    R3,4(R4)            BUILD SETS AFTER FLAG BYTE                   
         LR    R0,RA                                                            
         A     R0,=A(TWAMAXRL)     END OF TWA                                   
         LA    R6,3(R6)                                                         
*                                                                               
VNSET02  DS    0H                                                               
         LR    R1,R5                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R6)                                                    
         AR    R3,R5                                                            
         AR    R6,R5                                                            
         CR    R3,R0                                                            
         BNL   NOTWA                                                            
         CR    R6,RF                                                            
         BL    VNSET02                                                          
*                                                                               
         XC    0(10,R3),0(R3)      CLEAR END OF SET                             
         B     VALSET10            GOTO CLEAN UP                                
************************************************************                    
VSETOSET DS    0H                  PROCESS SET OF SETS                          
         OI    0(R4),X'20'         SET OF SETS FLAG                             
         XC    4(10,R4),4(R4)      CLEAR BEGINING OF SET                        
*                                                                               
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'20'        GET MEMBER ELEMENT                           
         BAS   RE,GETEL2                                                        
         BE    *+6                 FOUND                                        
         DC    H'0'                NOT FOUND                                    
         LA    RE,RCONREC                                                       
         XC    0(255,RE),0(RE)                                                  
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         LA    R0,3                SUBTRACT CONTROL                             
         SR    RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),3(R6)       KEEP IDENTIFIERS HERE                        
         ST    RE,FULL             KEEP ADDRESS                                 
*                                                                               
VSSET02  DS    0H                                                               
         L     RE,FULL                                                          
         CLC   0(4,RE),SPACES      ANY SET NAME?                                
         BNH   VALSET10            NO - GOTO CLEAN UP                           
*                                                                               
         LA    RF,4(RE)            SETUP NEXT SET NAME                          
         ST    RF,FULL                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'38'           ACCESS SET RECORD                            
         MVC   KEY+19(2),REPALPHA EP ALPHA CODE                                 
*                                                                               
*   DETERMINE IF SUBSIDIARY IS IN PROCESS, AND, IF SO, IF SET NEEDS             
*        MASTER REP CODE RATHER THAN SUBSID.                                    
*                                                                               
         OC    MASTRREP,MASTRREP   SUBSIDIARY REQUEST?                          
         BZ    VSSET08             NO  -                                        
         LA    RF,SETNAMES         YES - CHECK IF MASTER NEEDED                 
VSSET04 EQU    *                                                                
         CLC   =C'XX',0(RF)        END OF LIST?                                 
         BE    VSSET08             YES - MASTER REP NOT NEEDED                  
         CLC   0(2,RF),0(R2)       SET NAME IN LIST?                            
         BE    VSSET06             YES -                                        
         LA    RF,2(RF)            NO  - BUMP TO NEXT ENTRY IN LIST             
         B     VSSET04                                                          
VSSET06 EQU    *                                                                
         MVC   KEY+19(2),MASTRREP  INSERT MASTER REP INTO KEY                   
VSSET08 EQU    *                                                                
         MVC   KEY+21(2),0(R2)     INSERT SET TYPE                              
         MVC   KEY+23(4),0(RE)     INSERT SET NAME                              
         OC    KEY+23(4),SPACES                                                 
         GOTO1 HIGH                NOTE DMGR OFF R9                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VSSET02             SKIP DELETED SET                             
         GOTO1 GETREC              NOTE DMGR OFF R9                             
*                                                                               
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'01'        NEW DESCRIPTION ELEMENT                      
         USING RSET1DES,R6                                                      
         BAS   RE,GETEL2                                                        
         BNE   *+12                MUST BE NORMAL SET RECORD                    
         TM    RSET1FLG,X'80'      SET OF SETS?                                 
         BNZ   VSSET02             YES SKIP IT                                  
         DROP  R6                                                               
*-----------------------------------------------------------------              
VSSET10  DS    0H                  PROCESS NORMAL SET RECORD                    
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL2                                                        
         BE    *+6                 FOUND                                        
         DC    H'0'                NOT FOUND                                    
         USING RSETMEMD,R6                                                      
         ZIC   R5,RSETMLEN         MEMBER LENGTH                                
         DROP  R6                                                               
*                                                                               
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         AR    RF,R6               END OF ELEM                                  
         LR    R0,RA                                                            
         A     R0,=A(TWAMAXRL)     END OF TWA                                   
         LA    R6,3(R6)                                                         
*                                                                               
VSSET12  DS    0H                  CHECK FOR REPEATS                            
         LA    R3,4(R4)                                                         
*                                                                               
VSSET14  DS    0H                  CHECK FOR REPEATS                            
         CLI   0(R3),0             END OF TABLE                                 
         BE    VSSET16             YES - INSERT MEMBER HERE                     
         LR    R1,R5                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),0(R6)       MEMBER TYPE MATCH?                           
         BE    VSSET18             YES - SKIP THIS ONE                          
         AR    R3,R5                                                            
         CR    R3,R0                                                            
         BL    VSSET14                                                          
         B     NOTWA                                                            
*                                                                               
VSSET16  DS    0H                                                               
         LR    R1,R5                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R6)                                                    
         LA    R3,0(R5,R3)                                                      
         XC    0(10,R3),0(R3)      CLEAR MORE AT THE END OF SET                 
VSSET18  DS    0H                                                               
         AR    R6,R5                                                            
         CR    R6,RF               NEXT MEMBER                                  
         BL    VSSET12                                                          
         B     VSSET02             NEXT SET NAME                                
************************************************************                    
VALSET10 DS    0H                                                               
         STC   R5,3(R4)            PUT ENTRY LENGTH IN TABLE                    
         LA    RE,4(R4)                                                         
VALSET12 DS    0H                                                               
         CLI   0(RE),C' '                                                       
         BNH   VALSET14                                                         
         AR    RE,R5               R5 HAS MEMBER LENGTH                         
         B     VALSET12                                                         
*                                                                               
VALSET14 XC    0(10,RE),0(RE)                                                   
         LA    RE,1(RE)            SKIP NULL TERMINATOR                         
         SR    RE,RA                                                            
         LR    R4,RA                                                            
         AH    R4,=Y(NEXTDISP)                                                  
         ST    RE,0(R4)                                                         
*                                                                               
YES      CR    RB,RB                                                            
         B     *+6                                                              
NO       LTR   RB,RB                                                            
         XIT1                                                                   
NOTWA    DC    H'0'                                                             
         SPACE 2                                                                
         GETELN R6,34,ELCODE,2                                                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE STATIONS IN SET OK FOR LIMITED ACCESS                                
*                                                                               
***********************************************************************         
CHKSET   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(SETDISPS)                                                  
         USING SETDISPD,RE                                                      
         L     R2,SETSTAS                                                       
         AR    R2,RA                                                            
         TM    0(R2),X'40'         IF IT'S EXCLUSION SET                        
         BO    NOTOK               SKIP IT                                      
         LA    R2,4(R2)                                                         
                                                                                
*                                  R2 -> LIST OF STATIONS IN SET                
         DROP  RE                                                               
*                                                                               
CHKSET50 XC    KEY,KEY             GET STATION RECORD                           
         MVI   KEY,2                                                            
         XC    KEY+1(19),KEY+1                                                  
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(5),0(R2)      STATION                                     
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   NOTOK                                                            
                                                                                
CHKSET60 EQU   *                                                                
*                                                                               
         USING TWAD,R4                                                          
         LR    R4,RA                                                            
         BAS   RE,GETREC                                                        
         L     R5,AIOAREA                                                       
         USING RSTAREC,R5                                                       
         SR    R7,R7                                                            
         ICM   R7,3,RSTALEN                                                     
         AR    R7,R5                                                            
         BCTR  R7,0         R7 = A(LAST BYTE OF STATION REC), FOR BXLE          
         LA    R5,RSTAELEM                                                      
         SR    R6,R6                                                            
*                                                                               
CHKSET70 CLI   0(R5),6      SEARCH FOR ELEMENT FOR VALID SIGNON ID'S            
         BNE   CHKSET80                                                         
         USING RSTASOEL,R5                                                      
         LR    R4,RA                                                            
         USING TWAD,R4                                                          
         CLC   TWAUSRID,RSTASID   COMPARE USER ID TO STATION SIGN ON ID         
         BE    CHKSET90           OK                                            
         DROP  R4                                                               
*                                                                               
CHKSET80 IC    R6,1(R5)                                                         
         BXLE  R5,R6,CHKSET70                                                   
         B     NOTOK              NOT OK                                        
         DROP  R5                                                               
CHKSET90 DS    0H                                                               
         LA    R2,5(R2)            BUMP TO NEXT STATION IN SET LIST             
         CLI   0(R2),0                                                          
         BNE   CHKSET50            GO GET IT                                    
         BE    OK                  THAT'S ALL FOLKS!!!!!                        
*                                                                               
*                                                                               
OK       CR    RB,RB                                                            
         B     *+6                                                              
NOTOK    LTR   RB,RB                                                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* REGENDSP                                                                      
* REGENDCT                                                                      
* REGENSET                                                                      
*        PRINT OFF                                                              
       ++INCLUDE REGENDSP                                                       
       ++INCLUDE REGENDCT                                                       
       ++INCLUDE REGENSET                                                       
       ++INCLUDE REGENSBLK                                                      
*        PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'078RERIS01S  05/01/02'                                      
         END                                                                    
