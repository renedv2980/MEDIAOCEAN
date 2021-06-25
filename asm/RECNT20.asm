*          DATA SET RECNT20    AT LEVEL 122 AS OF 03/13/13                      
*PHASE T80220A,+0                                                               
         TITLE 'T80220 - CONTRACT DISPLAY MODULE - REPPAK'                      
*                                                                               
***********************************************************************         
*                                                                     *         
*        RECNT20 (T80220) --- BUILD DISPLAY FIELDS                    *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* REFER TO RECNTHIST FOR PAST HISTORY                                 *         
*                                                                     *         
* 13MAR13 SKU SKIP STATION CHECK IF GLOBBER CALLER IS STK             *         
* 27JAN11 SKU SAVE OFF STAD ADDITION OPTION FLAGS                     *         
* 03AUG10 SKU SCRIPT CONTRACT UPLOAD SUPPORT                          *         
* 17JUN08 SKU DO NOT PROTECT FIELDS FOR XML ORDERS                    *         
* 02NOV02 SKU REDI LINKED CONTRACTS USE WIP IN DISPLAY                *         
* 16JAN00 MLB DISPLAY POINT PERSON IN THE SALES PERSON FIELD          *         
* 30OCT99 BU  DISPLAY AGENCY CODE IF NOT ON FILE                      *         
* 01FEB99 SKU READ OFFICE T/A PRINT AT DDS PROFILE                    *         
* 15OCT98 RHV SALESMAN TEAM OVERRIDE                                  *         
* 24JUL97 SKU 4K CONTRACT SUPPORT                                     *         
* 03APR97 SKU EXTEND DARE CHECK TO POOL/VARIOUS ORDERS                *         
* 22JAN97 SKU DARE TAKEOVER SUPPORT                                   *         
* 07OCT96 SKU SUPPORT LOW POWER STATION                               *         
* 30JUL96 RHV WRITE ADDT'L STATION OPTION BITS TO TWA                 *         
*         SKU ALLOW CHANGE TO ADV FIELD IF LINKED TO DARE             *         
* 05JUN96 SKU EOP SUPPORT FOR TRAFFIC FORMAT C AND K                  *         
* 30MAY96 RHV READ CONTYPE REC FOR CON/WRKSHT OPTION BITS & PUT TO TWA*         
* 24MAY96 SKU SUPPORT FOR EXPANDED ESTIMATE NUMBER                    *         
* 08APR96 RHV SUPPORT 34 BYTE AGY ADDRESS FIELDS FOR ALL USERS        *         
*         SKU INITIALIZE SPOTPAK REP-TO-SPOT TWA DATA FIELDS TO ZERO  *         
* 29FEB96 RHV SUPPORT PETRY 34 BYTE AGY ADDR FIELDS                   *         
* 22FEB96 SKU CONVERTED CONTRACT W/O ACE/EASYLINK MUST REREAD STATION *         
*             FOR THE INFORMATION                                     *         
* 31JAN96 SKU KATZ EDI DARE ORDERS HAVE NORMAL FIELD ACCESS           *         
* 06JAN96 SKU PROFILE 24 TO PRINT PTP OVER SALESPERSON FOR TYPE D     *         
* 19DEC95 WSB PRINTS CODE IF FINDS INCORRECT KEY                      *         
* 13DEC95 WSB ADDED CHECKING FOR CORRECT KEY AFTER VHIGH              *         
*         SKU 2K CONTRACT SUPPORT                                     *         
***********************************************************************         
*                                                                               
T80220   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80220,R9                                                      
         L     RC,0(R1)            WORK                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         LR    R8,RA                                                            
         AH    R8,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R8                                                       
*              CLEAR CONTRACT FIELDS WITH BLANKS                                
         SPACE 1                                                                
         GOTO1 VCLEAR,DMCB,0                                                    
*                                                                               
* READ CONTYPE RECORD FOR CON/WRKSHT OPTION BITS                                
         BAS   RE,GETCONT                                                       
*                                                                               
* DON'T DISPLAY PETRY/SWITCHED STATION RECORDS                                  
PETRYFOX EQU   693                                                              
*                                                                               
*&&DO                                                                           
         B     PFOX0010            PETRY TESTING SKIPPED.                       
*                                                                               
         CLC   RCONKREP,=C'PV'     PETRY?                                       
*                                                                               
         BNE   PFOX0010            NO                                           
*                                                                               
*        PETRY MUST BE PERMITTED TO CLOSE OUT SWITCHED STATION                  
*              ORDERS SO THEY DON'T APPEAR ON THE ACTIVITY REPORTS.             
*                                                                               
         CLC   CONBACT(3),=C'SPL'  ALLOW SPL FOR SWITCHED STAS                  
         BE    PFOX0010                                                         
         CLC   CONBACT(2),=C'CD'   ALLOW SPL FOR SWITCHED STAS                  
         BE    PFOX0010                                                         
*                                                                               
         CLC   RCONKSTA(4),=C'WFXT' SWITCHED STATION?                           
         BE    PFOX0002            YES                                          
         CLC   RCONKSTA(4),=C'WTXF' SWITCHED STATION?                           
         BE    PFOX0002            YES                                          
         CLC   RCONKSTA(4),=C'WNYW' SWITCHED STATION?                           
         BE    PFOX0002            YES                                          
         CLC   RCONKSTA(4),=C'WFLD' SWITCHED STATION?                           
         BE    PFOX0002            YES                                          
         CLC   RCONKSTA(4),=C'KTTV' SWITCHED STATION?                           
         BNE   PFOX0010            YES                                          
PFOX0002 EQU   *                                                                
         LA    R2,CONCACTH                                                      
         LA    R3,PETRYFOX                                                      
         B     ERROR                                                            
*&&                                                                             
PFOX0010 EQU   *                                                                
*                                                                               
* CHECK IF KATZ/SELTEL CONVERTED                                                
         BAS   RE,CHKCONV                                                       
*                                                                               
         NI    TWADARE,X'FF'-X'01'                                              
*                                  CLEAR 'DARE ORDER BEING CHANGED'             
*                                                                               
         CLC   =C'CHA',CONACT      REQUEST FOR CHANGE?                          
         BNE   DISP0020            NO  - DON'T PLAY WITH SCREEN                 
*                                                                               
         GOTO1 =A(CHKDARE),DMCB,(RC),RR=Y                                       
*                                                                               
         TITLE 'T80220 - BUILD CONTRACT DISPLAY FIELDS'                         
*              BUILD AGENCY                                                     
DISP0020 XC    IOAREA(32),IOAREA                                                
         MVI   RAGYKTYP,10         AGENCY REC TYPE                              
         MVC   RAGYKAGY,RCONKAGY   AGY                                          
         MVC   RAGYKAOF,RCONKAOF   OFFICE                                       
         MVC   RAGYKREP,REPALPHA                                                
*                                                                               
         MVC   KEY,IOAREA                                                       
         MVC   RAGYNAM1,WAGYEXP                                                 
*                                                                               
         GOTO1 VHIGH                                                            
*        CLC   KEY(25),KEYSAVE                                                  
*        BNE   DISP0120                                                         
*        CLC   KEY+25(2),=C'ZZ'                                                 
*        BE    DISP0040                                                         
*        CLC   KEY+25(2),KEYSAVE+25                                             
*        BE    DISP0040                                                         
*        MVC   KEY+25(2),=C'ZZ'                                                 
*        GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    DISP0040            FOUND                                        
*                                                                               
*   NOT FOUND:  DISPLAY CONTRACT AGENCY/AGY OFF CODES                           
*                                                                               
         OC    RCONKAGY,RCONKAGY                                                
         BZ    DISP0030                                                         
         MVC   CONAGY(4),RCONKAGY  AGENCY                                       
         CLC   RCONKAOF,MYSPACES   OFFICE?                                      
         BE    DISP0030                                                         
         LA    RE,CONAGY                                                        
         MVI   CONAGY+4,C' '                                                    
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),RCONKAOF    AGENCY OFFICE                                
DISP0030 DS    0H                                                               
         MVC   CONAGYN(15),=C'*** MISSING ***'                                  
         B     DISP0120                                                         
*                                                                               
DISP0040 GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   TWAAGNM1,RAGYNAM1                                                
         MVC   TWAAGNM2,RAGYNAM2                                                
         XC    TWAAGAD1,TWAAGAD1                                                
         XC    TWAAGAD2,TWAAGAD2                                                
         XC    TWAAGAD3,TWAAGAD3                                                
         MVC   TWAAGAD1(20),RAGYADD1                                            
         MVC   TWAAGAD3(20),RAGYCITY                                            
         MVC   TWAAGSTT,RAGYSTAT                                                
         MVC   TWAAGZIP,RAGYZIP                                                 
         MVC   TWAAEASY,RAGYPRO2   PROFILE FOR EASYLINK AGY COPY                
         MVC   TWAACFPT,RAGYPRO3   PROFILE FOR SUPPRESSING CONF PRINT           
         MVC   TWAARISK,RAGYRISK   AGENCY CREDIT RISK                           
         MVC   TWAALIAB,RAGYLIAB   AGENCY LIABILITY POSITION                    
         MVC   TWAAGFL,RAGYFLAG    AGENCY REC FLAG BYTE                         
*                                                                               
DISP0060 MVC   CONAGY(4),RAGYKAGY  AGENCY                                       
         CLC   RAGYKAOF,MYSPACES   OFFICE?                                      
         BE    DISP0080                                                         
         LA    RE,CONAGY                                                        
         MVI   CONAGY+4,C' '                                                    
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),RAGYKAOF    AGENCY OFFICE                                
DISP0080 DS    0H                                                               
         MVC   CONAGYN,RAGYNAM1                                                 
*                                                                               
         XC    CONARSK,CONARSK     ALWAYS CLEAR CREDIT RISK CATEGORY            
         OC    TWAARISK,TWAARISK   NO RATING ASSIGNED, ASSUM OK                 
         BZ    DISP0100                                                         
         CLI   TWAARISK,1          RISK=OK, SHOW NOTHING                        
         BE    DISP0100                                                         
         MVC   CONARSK(9),=C'AGY RISK=' DISPLAY RISK                            
         LA    R4,CONARSK+9                                                     
         EDIT  TWAARISK,(1,(R4))                                                
DISP0100 OI    CONARSKH+6,X'80'    XMIT                                         
*                                                                               
DISP0120 DS    0H                  READ AGENCY PART 2 RECORD TO GET             
         XC    TWAAGYPH,TWAAGYPH   INIT PHONE/FAX #S                            
         XC    TWAAFAX,TWAAFAX                                                  
*                                                                               
         XC    IOAREA(32),IOAREA   PHONE/FAX NUMBERS                            
         MVI   RAGK2TYP,RAGK2TYQ   AGENCY REC TYPE                              
         MVC   RAGK2AGY,RCONKAGY   AGY                                          
         MVC   RAGYKAOF,RCONKAOF   OFFICE                                       
         MVC   RAGYKREP,REPALPHA                                                
*                                                                               
         MVC   KEY,IOAREA                                                       
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DISP0160                                                         
*                                                                               
DISP0140 GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   TWAAGYPH,RAGY2FON                                                
         MVC   TWAAFAX,RAGY2FAX                                                 
*                                                                               
         TM    TWAAGFL,X'80'            EXPANDED ADDRESS?                       
         BZ    DISP0160                                                         
         LA    R6,RAGY2REC                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP0160                                                         
         MVC   TWAAGAD1,02(R6)     ADDRESS LINE 1                               
         MVC   TWAAGAD2(34),36(R6) ADDRESS LINE 2                               
*                                                                               
* NOTE: THE NEXT 3 LINES ARE TO FIX THE PROBLEM OF THE CITY BEING               
*       DUPLICATED IN THE 2ND ADDRESS FIELD IN PETRY CONVERTED RECS             
         CLC   TWAAGAD3(20),TWAAGAD2                                            
         BNE   *+10                                                             
         XC    TWAAGAD3,TWAAGAD3                                                
*                                                                               
DISP0160 DS    0H                                                               
*          FLOAT STATE AND ZIP ONTO 3RD LINE                                    
         LA    R1,TWAAGAD3                                                      
         LA    R2,L'TWAAGAD3-1(R1)                                              
DISP0165 CR    R2,R1                                                            
         BL    DISP0170                                                         
         OI    0(R2),X'40'                                                      
         CLI   0(R2),X'40'                                                      
         BNE   DISP0170                                                         
         BCT   R2,DISP0165                                                      
DISP0170 MVC   3(2,R2),TWAAGSTT                                                 
         MVC   7(10,R2),TWAAGZIP                                                
*                                                                               
         OC    TWAAGAD2,MYSPACES   IF 2ND LINE EMPTY, MOVE 3RD LINE             
         CLC   TWAAGAD2,MYSPACES   ON TO SECOND LINE                            
         BNE   DISP0175                                                         
         MVC   TWAAGAD2,TWAAGAD3                                                
         XC    TWAAGAD3,TWAAGAD3                                                
*                                                                               
DISP0175 DS    0H                                                               
*              BUYER                                                            
         MVC   CONBUY,RCONBUYR                                                  
*        CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
*        BNE   DISP0180                                                         
*                                                                               
* FOR DDS TERMINALS, DISPLAY D/A OF K, NOT SALESMAN NAME                        
*        MVC   CONBUY,MYSPACES                                                  
*        MVC   CONBUY(4),=C'D/A='                                               
*        GOTO1 HEXOUT,DMCB,TWAKADDR,CONBUY+5,L'TWAKADDR,=C'TOG'                 
*                                                                               
*                                                                               
*              ADVERTISER                                                       
DISP0180 XC    IOAREA(32),IOAREA                                                
         MVI   RADVKTYP,8                                                       
         MVC   RADVKADV,RCONKADV   ADVERTISER                                   
         MVC   RADVKREP,REPALPHA                                                
*                                                                               
         MVC   KEY,IOAREA                                                       
         MVC   RADVNAME,WADVEXP                                                 
         OC    WADVEXP,WADVEXP                                                  
         BNZ   DISP0220                                                         
         GOTO1 VHIGH                                                            
*        CLC   KEY(25),KEYSAVE                                                  
*        BNE   DISP0240                                                         
*        CLC   KEY+25(2),KEYSAVE+25                                             
*        BE    DISP0200                                                         
*        CLC   KEY+25(2),=C'ZZ'                                                 
*        BE    DISP0200                                                         
*        MVC   KEY+25(2),=C'ZZ'                                                 
*        GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    DISP0200                                                         
         MVC   CONADV(4),RCONKADV                                               
         MVC   CONADVN(15),=C'*** MISSING ***'                                  
         B     DISP0240                                                         
*                                                                               
*                                                                               
DISP0200 GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
DISP0220 MVC   CONADV(4),RADVKADV                                               
         MVC   CONADVN,RADVNAME                                                 
         EJECT                                                                  
*              STATION                                                          
DISP0240 XC    IOAREA(32),IOAREA   BUILD STATION KEY                            
         MVI   RSTAKTYP,2                                                       
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,RCONKSTA                                                
         MVC   RSTAMKT,WSTAEXP                                                  
*                                                                               
         CLC   =C'ADD',CONACT                                                   
         BE    DISP0250                                                         
*                                                                               
         OC    WSTAEXP,WSTAEXP                                                  
         BNZ   DISP0280                                                         
*                                                                               
DISP0250 DS    0H                                                               
         MVC   KEY,IOAREA                                                       
         GOTO1 VREAD                                                            
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   TWASTCDT,RSTACLDT   CLOSE DATE FOR INVOICES                      
         MVC   TWASTAST,RSTASTAT   STATION STATUS                               
         MVC   TWATRFMT,RSTATRAF   GET TRAFFICE SYSTEM                          
*                                                                               
         MVI   TWASTADF,0          SET STAD ADDITIONAL OPTION FLAGS             
         CLI   RSTAP2,C'Y'                                                      
         BNE   *+8                                                              
         OI    TWASTADF,X'40'      OPT 2: NO REP SEND                           
         CLI   RSTAP3,C'Y'                                                      
         BNE   *+8                                                              
         OI    TWASTADF,X'20'      OPT 3: ORS 48HRS RET                         
         CLI   RSTAP4,C'Y'                                                      
         BNE   *+8                                                              
         OI    TWASTADF,X'10'      OPT 4: CFX DISALLOWED                        
*                                                                               
         NI    TWAFLAGS,X'FF'-TWAFLPTQ                                          
         LA    RE,RSTAELEM                                                      
DISP0260 ZIC   R1,1(RE)                                                         
         AR    RE,R1                                                            
         CLI   0(RE),0                                                          
         BE    DISP0280                                                         
         CLI   0(RE),X'08'                                                      
         BL    DISP0260                                                         
         BH    DISP0280                                                         
         USING RSTAXXEL,RE                                                      
         MVC   TWASTAOA,RSTAOPTA   SAVE STA ADDTL OPTIONS 1                     
         MVC   TWASTAOB,RSTAOPTB   SAVE STA ADDTL OPTIONS 2                     
         MVC   TWAUNIST,7(RE)      UNI STATION STATUS                           
         TM    RSTAOPTB,X'40'      PROF FOR PATTERN/NOTATION?                   
         BZ    *+8                                                              
         OI    TWAFLAGS,TWAFLPTQ   USE PATTERN/NOTATION FLAG                    
         DROP  RE                                                               
*                                                                               
DISP0280 DS    0H                  SAVE OFF FORMER REP INFO                     
         XC    TWAFREP,TWAFREP                                                  
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'0C'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP0290                                                         
         USING RSTAFNEL,R6                                                      
         MVC   TWAFREP,RSTAFNFO                                                 
         DROP  R6                                                               
*                                                                               
DISP0290 DS    0H                                                               
         MVI   TWASTAOP,0                                                       
         XC    TWAECON,TWAECON     DEFAULT IS NOT EC STATION                    
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'08'        FIND EXTRA DESCRIP ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   DISP0300            NOT FOUND:  NOT EC STATION                   
                                                                                
         USING RSTAXXEL,R6                                                      
         MVC   TWASTAOP,RSTAOPTA                                                
         TM    RSTAXOPT,X'80'      FOUND - EC STATION?                          
         BZ    DISP0300            NO  -                                        
         DROP  R6                                                               
                                                                                
         MVC   TWAECON,RSTATRAF    YES, GET TRAFFICE SYSTEM                     
         CLI   TWAECON,C'W'        FOR FORMAT W                                 
         BNE   DISP0300                                                         
         MVI   TWAECON,C'B'        TREAT AS BIAS                                
                                                                                
DISP0300 DS    0H                                                               
         MVC   CONSTAM,RSTAMKT                                                  
*                                                                               
         MVC   WORK(4),RSTAKSTA                                                 
*                                                                               
         MVC   WORK+4(3),=C'-FM'                                                
         CLI   RSTAKSTA+4,C'F'                                                  
         BE    DISP0320                                                         
*                                                                               
         MVI   WORK+5,C'A'                                                      
         CLI   RSTAKSTA+4,C'A'                                                  
         BE    DISP0320                                                         
*                                                                               
         MVI   WORK+5,C'C'                                                      
         CLI   RSTAKSTA+4,C'C'                                                  
         BE    DISP0320                                                         
*                                                                               
         MVC   WORK+5(2),=C'L '                                                 
         CLI   RSTAKSTA+4,C'L'                                                  
         BE    DISP0320                                                         
*                                                                               
         MVC   WORK+5(2),=C'TV'                                                 
DISP0320 CLI   WORK+3,C' '                                                      
         BNE   *+14                                                             
         MVC   WORK+3(3),WORK+4                                                 
         MVI   WORK+6,C' '                                                      
*                                                                               
         MVC   CONSTA(7),WORK      STATION                                      
*                                                                               
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BNE   DISP0380                                                         
         CLC   =C'LIN',TWAGENCP    STATION TOOL KIT? SKIP                       
         BE    DISP0380                                                         
         SPACE 1                                                                
*           IF STATION IS USER, CHECK THAT THE STATION IS                       
*           AUTHORIZED TO SEE THIS CONTRACT.                                    
         SPACE 1                                                                
         LA    R2,CONCNUMH                                                      
         LA    R3,55               ERROR, SECURITY LOCKOUT                      
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP0360                                                         
         SPACE 1                                                                
         USING RSTASOEL,R6         STATION'S CONTRACTS                          
DISP0340 CLC   TWAUSRID,RSTASID                                                 
         BE    DISP0380                                                         
         BAS   RE,NEXTEL                                                        
         BE    DISP0340                                                         
         SPACE 1                                                                
         DROP  R6                                                               
DISP0360 MVC   CONAGY,MYSPACES     BLANK OUT THE OTHER FIELDS                   
         MVC   CONAGYN,MYSPACES                                                 
         MVC   CONBUY,MYSPACES                                                  
         MVC   CONADV,MYSPACES                                                  
         MVC   CONADVN,MYSPACES                                                 
         MVC   CONSTA,MYSPACES                                                  
         MVC   CONSTAM,MYSPACES                                                 
         B     ERROR                                                            
         EJECT                                                                  
DISP0380 DS    0H                                                               
         XC    TWAPDPTP,TWAPDPTP   INIT POINT PERSON CODE                       
         CLC   RCONPRD,MYSPACES    PRODUCT CODE?                                
         BE    DISP0440                                                         
* GET PRODUCT RECORD                                                            
         XC    IOAREA(32),IOAREA                                                
         MVI   RPRDKTYP,9                                                       
         MVC   RPRDKADV,RCONKADV                                                
         MVC   RPRDKPRD,RCONPRD                                                 
         MVC   RPRDKREP,REPALPHA                                                
         MVC   KEY,IOAREA                                                       
         MVC   RPRDNAME,WPRDEXP                                                 
         OC    WPRDEXP,WPRDEXP     PRODUCT LOOKED UP ALREADY?                   
         BZ    DISP0400                                                         
         MVC   CONPRD(2),=C'C='                                                 
         MVC   CONPRD+2(3),RCONPRD                                              
         MVI   CONPRDH+5,L'CONPRD                                               
         MVC   CONPRD+6(14),RPRDNAME                                            
         B     DISP0460                                                         
*                                                                               
DISP0400 GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DISP0460                                                         
*                                                                               
DISP0420 GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   TWAPRDNM,RPRDNAME                                                
         MVC   CONPRD(2),=C'C='                                                 
         MVC   CONPRD+2(3),RCONPRD                                              
         MVI   CONPRDH+5,L'CONPRD                                               
         MVC   CONPRD+6(14),RPRDNAME                                            
*                                                                               
         XC    TWASPCL,TWASPCL     INITIALIZE SPOTPAK DATA                      
         XC    TWASPPD,TWASPPD                                                  
         XC    TWASPPP,TWASPPP                                                  
         XC    TWASPP1,TWASPP1                                                  
         XC    TWASPP2,TWASPP2                                                  
         XC    TWASPES,TWASPES                                                  
*                                                                               
         TM    PROFILES+CNTPPTPB,CNTPPTPA                                       
         BZ    DISP0425            PROFILE SET FOR TYPE N/X                     
         CLI   RCONTYPE,C'N'                                                    
         BE    DISP0425                                                         
         CLI   RCONTYPE,C'X'                                                    
         BE    DISP0425                                                         
*                                                                               
DISP0423 DS    0H                                                               
         TM    PROFILES+CNTDPTPB,CNTDPTPA                                       
         BZ    DISP0430            PROFILE SET FOR TYPE D                       
         CLI   RCONTYPE,C'D'                                                    
         BNE   DISP0430                                                         
*                                                                               
DISP0425 DS    0H                                                               
         LA    R6,IOAREA           SAVE OFF POINT PERSON CODE                   
         MVI   ELCODE,X'02'        SO WE CAN PRINT THE POINT PERSON             
         BAS   RE,GETEL            NAME INSTEAD OF THE SALESPERSON'S            
         BNE   DISP0430            IN WORKSHEETS AND CONFIRMATIONS              
         USING RPRDNELM,R6                                                      
         MVC   TWAPDPTP,RPRDNPNT                                                
         DROP  R6                                                               
*                                                                               
DISP0430 DS    0H                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP0460                                                         
         USING RPRDSPOT,R6                                                      
         MVC   TWASPCL,RPRDSPCL    SPOTPAK CLIENT CODE                          
         MVC   TWASPPD,RPRDSPP1    SPOTPAK PRODUCT CODE                         
         MVC   TWASPPP,RPRDSPP2    SPOTPAK PIGGY BACK RODUCT                    
         MVC   TWASPP1,RPRDSPS1    SPOTPAK PRODUCT 1 SPLIT                      
         MVC   TWASPP2,RPRDSPS2    SPOTPAK PRODUCT 2 SPLIT                      
         MVC   TWASPES,RPRDSPES    SPOTPAK ESTIMATE NUMBER                      
         DROP  R6                                                               
*                                                                               
         L     R6,AIO4             SPOTPAK DAYPART MENU                         
         GOTO1 VREGENDP,DMCB,RPRDREC,(R6),DATAMGR,ACOMFACS,CALLOV,     X        
               VMEDGET                                                          
         MVC   TWADAYPT(L'TWADAYPT),0(R6)                                       
*                                                                               
         B     DISP0460                                                         
*              FIND PRODUCT ELEMENT                                             
DISP0440 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                NO X'05'                                     
         MVC   CONPRD,2(R6)        PRODUCT EXPANSION                            
         MVI   CONPRDH+5,L'CONPRD                                               
         EJECT                                                                  
*              GET SALESMAN                                                     
DISP0460 DS    0H                                                               
         XC    TWASALTL,TWASALTL   INIT PHONE/FAX#S                             
         XC    TWASALFX,TWASALFX                                                
*                                                                               
         XC    IOAREA(32),IOAREA                                                
         MVI   RSALKTYP,6          KEY TYPE - SALESMAN RECORD                   
         MVC   RSALKREP,REPALPHA   REP                                          
         MVC   RSALKSAL,RCONSAL    SALESMAN CODE                                
         MVC   KEY,IOAREA                                                       
         MVC   RSALNAME,WSALEXP                                                 
         OC    WSALEXP,WSALEXP     USED TO SAVE IO'S IF ALREADY THERE           
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DISP0470                                                         
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   TWASALTL,RSALTEL                                                 
         MVC   TWASALFX,RSALFAX                                                 
         MVC   WSALEXP,RSALNAME                                                 
*                                                                               
DISP0470 LA    R6,RCONREC                                                       
         MVI   ELCODE,X'9F'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP0480                                                         
         MVC   TWASALAS,22(R6)     SALES ASSISTANT                              
*                                                                               
DISP0480 DC    0H'0'                                                            
         XC    CONSAL,CONSAL                                                    
         MVC   CONSAL(3),RSALKSAL  SALESMAN CODE                                
         OC    WSALEXP,WSALEXP                                                  
         BZ    DISP0482                                                         
         CLC   RSALTEAM,RCONTEM    SALESMAN TEAM VS. K TEAM                     
         BE    DISP0483            NO OVERRIDE                                  
         LA    R1,CONSAL+2                                                      
         CLI   0(R1),C' '                                                       
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         MVI   1(R1),C'/'                                                       
         MVC   2(2,R1),RCONTEM     DISPLAY TEAM OVERRIDE                        
         B     DISP0483                                                         
*                                                                               
DISP0482 MVC   CONSALN,=C'***** MISSING ******'                                 
         B     DISP0486                                                         
*                                                                               
DISP0483 DS    0H                                                               
         TM    PROFILES+CNTPTPRB,CNTPTPRA                                       
         BZ    DISP0485                                                         
*                                                                               
         OC    TWAPDPTP,TWAPDPTP                                                
         BZ    DISP0485            NO POINT PERSON CODE                         
*                                                                               
         SR    R3,R3               COUNTER                                      
         LA    R1,RSALNAME+L'RSALNAME                                           
LOOP1    CHI   R3,L'RSALNAME       THIS LOOP TO GET RID OF BLANKS               
         BE    SKIP                                                             
         BCTR  R1,0                                                             
         AHI   R3,1                                                             
         CLI   0(R1),C' '                                                       
         BE    LOOP1                                                            
*                                                                               
         LR    R2,R1               R2 POINTS TO LAST CHARACTER                  
*                                                                               
LOOP2    CHI   R3,L'RSALNAME       THIS LOOP TO GET THE LAST NAME               
         BE    SKIP                                                             
         BCTR  R1,0                                                             
         AHI   R3,1                                                             
         CLI   0(R1),C' '                                                       
         BNE   LOOP2                                                            
*                                                                               
         AHI   R1,1                                                             
         SR    R2,R1                                                            
         CHI   R2,10                                                            
         BH    SKIP                                                             
         EX    R2,*+8                                                           
         B     GETPTP                                                           
         MVC   CONSALN(0),0(R1)                                                 
SKIP     MVC   CONSALN(10),0(R1)                                                
*                                                                               
GETPTP   XC    KEY,KEY             YES--BUILD POINT PERSON REC KEY              
         LA    R6,KEY                                                           
         USING RPTPREC,R6              POINT PERSON RECORD                      
         MVI   RPTPKTYP,X'31'      REC TYPE                                     
         MVC   RPTPKREP,REPALPHA   REP CODE                                     
         MVC   RPTPKREC,TWAPDPTP   POINT PERSON CODE                            
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         DROP  R6                                                               
*                                                                               
         GOTO1 (RFGETREC,VREPFACS),DMCB,KEY,IOAREA,0,DUB                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R6,IOAREA                                                        
*                                                                               
         SR    R4,R4               PUT '/' AFTER SALES PERSON NAME              
         LA    R3,CONSALN                                                       
         B     *+12                                                             
LOOP3    AHI   R3,1                                                             
         AHI   R4,1                                                             
         CLI   0(R3),C' '                                                       
         BNE   LOOP3                                                            
*                                                                               
         MVI   0(R3),C'/'                                                       
         AHI   R3,1                                                             
         AHI   R4,1                                                             
         LA    R5,L'CONSALN                                                     
         SR    R5,R4                                                            
         SHI   R5,1                ADJUST FOR EXECUTE LATER                     
*                                                                               
         SR    R4,R4               GET RID OF SPACES IN PTP                     
         LA    R1,RPTPNAME+L'RPTPNAME                                           
LOOP4    CHI   R4,L'RPTPNAME                                                    
         BE    DISP0485                                                         
         BCTR  R1,0                                                             
         AHI   R4,1                                                             
         CLI   0(R1),C' '                                                       
         BE    LOOP4                                                            
*                                                                               
         LR    R2,R1               POINT TO END OF LAST NAME                    
*                                                                               
LOOP5    CHI   R4,L'RPTPNAME       GET PTP LAST NAME                            
         BE    SKIP3                                                            
         BCTR  R1,0                                                             
         AHI   R4,1                                                             
         CLI   0(R1),C' '                                                       
         BNE   LOOP5                                                            
*                                                                               
         AHI   R1,1                                                             
         SR    R2,R1                                                            
SKIP3    EX    R5,*+8                                                           
         B     DISP0486                                                         
         MVC   0(0,R3),0(R1)                                                    
*        DROP  R6                                                               
*                                                                               
DISP0485 MVC   CONSALN,RSALNAME                                                 
*                                                                               
DISP0486 MVC   CONCAT(2),RCONCTGY  CATEGORY                                     
         OC    CONCAT(2),CONCAT    TRANSITION?                                  
         BNZ   *+10                                                             
         MVC   CONCAT(2),=C'XX'                                                 
         EJECT                                                                  
*              OFFICE                                                           
         XC    IOAREA(32),IOAREA   BUILD OFFICE REC KEY                         
         MVI   ROFFKTYP,4                                                       
         MVC   ROFFKREP,REPALPHA                                                
         MVC   ROFFKOFF,RCONKOFF                                                
         MVC   KEY,IOAREA                                                       
         MVC   ROFFNAME,WOFFEXP                                                 
         OC    WOFFEXP,WOFFEXP     USED TO SAVE IO'S IF ALREADY THERE           
         BNZ   DISP0500                                                         
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DISP0490                                                         
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   TWAOFAD1,ROFFADD1                                                
         MVC   TWAOFAD2,ROFFADD2                                                
         MVC   TWAOFSTT,ROFFSTT                                                 
         MVC   TWAOFZIP,ROFFZIP                                                 
         MVC   WOFFEXP,ROFFNAME                                                 
*                                                                               
DISP0490 CLI   TWAOFFC,C'*'        TEST IF DDS TERMINAL                         
         BE    DISP0500                                                         
         CLC   TWAACCS(2),=C'O='   TEST FOR OFFICE RESTRICTION                  
         BNE   DISP0500                                                         
         TM    TWAAUTH,X'80'       TEST IF TERMINAL ALLOWED ACCESS              
         BO    DISP0500            TO ALL OFFICES                               
         CLC   ROFFKOFF,TWAACCS+2  ELSE, COMPARE OFFICES                        
         BE    DISP0500                                                         
         LA    R3,55               ERROR, SECURITY LOCKOUT                      
         MVC   CONAGY,MYSPACES     BLANK OUT THE OTHER FIELDS                   
         MVC   CONAGYN,MYSPACES                                                 
         MVC   CONBUY,MYSPACES                                                  
         MVC   CONADV,MYSPACES                                                  
         MVC   CONADVN,MYSPACES                                                 
         MVC   CONSTA,MYSPACES                                                  
         MVC   CONSTAM,MYSPACES                                                 
         MVC   CONPRD,MYSPACES                                                  
         MVC   CONCAT,MYSPACES                                                  
         MVC   CONSAL,MYSPACES                                                  
         MVC   CONSALN,MYSPACES                                                 
         LA    R2,CONSALH                                                       
         B     ERROR                                                            
*                                                                               
DISP0500 OC    WOFFEXP,WOFFEXP                                                  
         BNZ   DISP0505                                                         
*                                                                               
         MVC   CONOFFN(2),ROFFKOFF                                              
         MVC   CONOFFN+2(14),=C'-- * MISSING *'                                 
         B     *+10                                                             
*                                                                               
DISP0505 MVC   CONOFFN,ROFFNAME                                                 
*                                                                               
         MVI   TWAOCFTA,C'N'       OFFICE REQUEST T/A ON ACTION CONF?           
         MVI   TWAPRDDS,C'N'       OFFICE REQUEST T/A PRINT AT DDS?             
         XC    TWAOFFFX,TWAOFFFX   INIT FAX NUMBER                              
*                                  READ OFFICE PART 2 REC FOR FAX #             
         XC    IOAREA(32),IOAREA   BUILD OFFICE REC KEY                         
         MVI   ROFF2TYP,ROFF2TYQ                                                
         MVC   ROFF2REP,REPALPHA                                                
         MVC   ROFF2OFF,RCONKOFF                                                
         MVC   KEY,IOAREA                                                       
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DISP0520                                                         
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   TWAOFFFX,ROFF2FAX                                                
         TM    ROFF2PRF,X'20'                                                   
         BZ    *+8                                                              
         MVI   TWAOCFTA,C'Y'       OFFICE REQUESTS T/A ON CONF ACTION           
*                                                                               
         TM    ROFF2PRF,X'02'                                                   
         BZ    *+8                                                              
         MVI   TWAPRDDS,C'Y'       OFFICE REQUESTS T/A PRINT AT DDS             
*                                                                               
DISP0520 DS    0H                                                               
         LA    R6,RCONREC          RETRIEVE DEV/INVOICE ELEMENT                 
         MVI   ELCODE,X'18'           IF ANY                                    
         BAS   RE,GETEL                                                         
         BNE   DISP0580            NONE FOUND                                   
*                                                                               
*              GET DEVELOPMENTAL SALESMAN                                       
*                                                                               
         XC    IOAREA(32),IOAREA                                                
         MVI   RDSPKTYP,X'3A'      KEY TYPE - DEV S/P RECORD                    
         MVC   RDSPKREP,REPALPHA   REP                                          
         USING RCONDVEL,R6                                                      
         OC    RCONDVSP,RCONDVSP   ANY DEV S/P?                                 
         BZ    DISP0550            NO                                           
*                                                                               
         MVC   RDSPKSAL,RCONDVSP   YES - INSERT DEV S/P CODE                    
         MVC   KEY,IOAREA                                                       
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DISP0540                                                         
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         MVC   TWDSPEXP,RDSPNAME   SAVE EXPANSION CODE                          
         MVC   CONDSP(3),RDSPKSAL  DEV S/P CODE                                 
         MVC   CONDSPN,RDSPNAME    DEV S/P NAME                                 
         B     DISP0550                                                         
*                                                                               
DISP0540 MVC   CONDSP,RDSPKSAL                                                  
         MVC   CONDSPN,=C'*** MISSING ***'                                      
*                                                                               
DISP0550 EQU   *                                                                
*                                                                               
*              GET DEVELOPMENTAL CONTRACT TYPE                                  
*                                                                               
         XC    IOAREA(32),IOAREA                                                
         MVI   RDCTKTYP,X'3B'      KEY TYPE - DEV CONTYPE RECORD                
         MVC   RDCTKREP,REPALPHA   REP                                          
         OC    RCONDVCT,RCONDVCT   ANY DEV CONTYPE?                             
         BZ    DISP0570            NO                                           
*                                                                               
         MVC   RDCTKCTY,RCONDVCT   YES - INSERT DEV CONTYPE CODE                
         MVC   KEY,IOAREA                                                       
         GOTO1 VHIGH               NO  - READ RECORD                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DISP0560                                                         
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         MVC   TWDCTEXP,RDCTDESC   SAVE EXPANSION CODE                          
         MVC   CONDCT(2),RDCTKCTY  DEV CONTYPE CODE                             
         MVC   CONDCTN,RDCTDESC    DEV CONTYPE DESCRIPTION                      
         B     DISP0570                                                         
*                                                                               
DISP0560 MVC   CONDCT,RDCTKCTY                                                  
         MVC   CONDCTN,=C'*** MISSING ***'                                      
*                                                                               
DISP0570 EQU   *                                                                
*                                                                               
         DROP  R6                                                               
*                                                                               
*              RATING                                                           
*                                                                               
DISP0580 EQU   *                                                                
         CLI   RCONRTGS,0                                                       
         BE    DISP0600                                                         
         LA    R5,RTGSTAB                                                       
         CLI   0(R5),X'FF'                                                      
         BE    DISP0600                                                         
         CLC   RCONRTGS,0(R5)                                                   
         BE    *+12                                                             
         LA    R5,3(R5)                                                         
         B     *-22                                                             
         MVC   CONRTG(3),0(R5)                                                  
         SPACE 1                                                                
         CLI   RCONRMON,0          RATING MONTH?                                
         BE    DISP0600                                                         
* DISPLAY RATING MONTH                                                          
         SR    RE,RE                                                            
         IC    RE,RCONRMON                                                      
         MH    RE,=H'3'                                                         
         LA    RE,MONTABLE-3(RE)                                                
         MVI   CONRTG+3,C'-'                                                    
         MVC   CONRTG+4(3),0(RE)                                                
         EJECT                                                                  
*              EI CODES                                                         
DISP0600 LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP0620            NO EI CODES                                  
         USING RCONIEL,R6                                                       
         MVC   CONIADV,RCONIADV                                                 
*MN                                                                             
         MVC   CONIPRD,RCONIPRD                                                 
         CLC   RCONIPR2,MYSPACES                                                
         BNH   *+10                                                             
         MVC   CONIPRD(4),=CL4'****'                                            
*MN                                                                             
*                                                                               
         MVC   CONIEST,RCONXEST    DEFAULT USING EXPANDED EST #                 
*                                                                               
         OC    CONIEST,MYSPACES    IF NO EXPANDED ESTIMATE NUMBER FOUND         
         CLC   CONIEST,MYSPACES    USE OLD FORMAT                               
         BNE   DISP0620                                                         
         MVC   CONIEST(L'RCONIEST),RCONIEST                                     
*                                                                               
DISP0620 DS    0H                                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,(X'80',RCONREC),CONDTES,0,DUB             
*                                                                               
* SET LENGTH OF DATES FOR RE-INPUT (ADDR)                                       
         MVI   CONDTESH+5,17                                                    
*                                                                               
DISP0630 DS    0H                                                               
         MVC   CONTYPE(L'RCONTYPE),RCONTYPE                                     
         SPACE 1                                                                
*              DISPLAY CONTRACT COMMENTS                                        
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP0700                                                         
         TM    TWAPROST,X'40'                                                   
         BZ    DISP0700            NO COMMENTS ON SCREEN                        
*                                                                               
         CLI   1(R6),3             WHAT THE HELL DOES THIS MEAN?                
         BL    DISP0700              (ELEM LT. 3 BYTES?)                        
         ZIC   R4,1(R6)            ELEM LEN                                     
         SH    R4,=H'3'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   CONCOM1(0),2(R6)                                                 
*                                                                               
         L     R5,AIO4                                                          
         GOTO1 VREGENSC,DMCB,(1,CONCOM1H),(R5),DATAMGR,RCONREC,GETTXT           
         BZ    DISP0640                                                         
         MVC   CONCOM1+14(24),=C'***CMNT REC NOT FOUND***'                      
*                                                                               
DISP0640 DS    0H                  NEXT COMMENT?                                
         MVI   ELCODE,2            RESTORE ELCODE                               
         BAS   RE,NEXTEL                                                        
         BNE   DISP0700                                                         
*                                                                               
         CLI   1(R6),3             AGAIN, WHAT THE HELL DOES THIS MEAN?         
         BL    DISP0700                                                         
*                                                                               
         ZIC   R4,1(R6)            ELEM LEN                                     
         SH    R4,=H'3'                                                         
         EX    R4,*+8              COMMENT 2                                    
         B     *+10                                                             
         MVC   CONCOM2(0),2(R6)                                                 
*                                                                               
         L     R5,AIO4                                                          
         GOTO1 VREGENSC,DMCB,(1,CONCOM2H),(R5),DATAMGR,RCONREC,GETTXT           
         BZ    DISP0700                                                         
         MVC   CONCOM2+14(24),=C'***CMNT REC NOT FOUND***'                      
*                                                                               
DISP0700 LA    R4,CONAGYH                                                       
         LA    R5,CONBACTH                                                      
         TM    TWAPROST,X'40'                                                   
         BO    *+8                                                              
         LA    R5,CONCOM1H         IF AVL OR PROP NO COMMENTS                   
* SET VALID FIELD BITS ON FOR ALL CONTRACT FIELDS                               
         SR    RE,RE                                                            
         OI    4(R4),X'20'                                                      
         IC    RE,0(R4)            FIELD LEN                                    
         LA    R4,0(RE,R4)         NEXT FIELD                                   
         CR    R4,R5                                                            
         BL    *-14                                                             
         XC    TWALSTKY,TWALSTKY                                                
         XC    TWALSTPL,TWALSTPL                                                
         SPACE 1                                                                
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET MAY SHOW VERSION NO.            
         BZ    DISP0760                                                         
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP0760                                                         
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRMED, SHOW MOD NUMBER                   
         BO    DISP0760                                                         
         ZIC   R5,1(R6)                                                         
         AR    R6,R5                                                            
         CLI   0(R6),X'20'         SHOULD BE SEND ELEMENT                       
         BNE   DISP0760                                                         
*                                                                               
         USING RCONSEND,R6                                                      
         CLC   RCONSRV,RCONSSV     SHOW LATEST VERSION                          
         BH    DISP0720                                                         
         EDIT  (1,RCONSSV),(3,CONMOD+8),ALIGN=LEFT                              
         B     DISP0730                                                         
DISP0720 EDIT  (1,RCONSRV),(3,CONMOD+8),ALIGN=LEFT                              
                                                                                
DISP0730 DS    0H                                                               
         TM    RCONMODR+1,X'40'    GRAPHNET?                                    
         BZ    DISP0740                                                         
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP0735                                                         
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'                                                   
         BO    DISP0740                                                         
         DROP  R6                                                               
*                                                                               
DISP0735 DS    0H                                                               
         MVC   CONMOD(7),=C'ESL VER'                                            
         B     DISP0770                                                         
*                                                                               
DISP0740 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISP0760                                                         
         USING RCONSEND,R6         R6 SHOULD STILL POINT TO 1F                  
         MVC   CONMOD+4(3),=C'VER'                                              
                                                                                
         TM    RCONSENF,X'10'+X'20' SHOW IF 'WORK IN PROGRESS'                  
         BO    DISP0770            ON EITHER SIDE                               
         MVC   CONMOD(3),=C'WIP'                                                
         B     DISP0770                                                         
         DROP  R6                                                               
                                                                                
DISP0760 CLI   RCONMOD,0           K MODIFICATION NUMBER                        
         BE    DISP0770                                                         
         MVC   CONMOD(7),=C'MOD NUM'                                            
         MVI   HALF,0                                                           
         MVC   HALF+1(1),RCONMOD                                                
         CLI   HALF+1,250                                                       
         BL    *+8                                                              
         MVI   HALF,255                                                         
         EDIT  (2,HALF),(3,CONMOD+8),ALIGN=LEFT,FLOAT=-                         
DISP0770 DS    0H                                                               
*** COMBO                                                                       
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'17'        IS THIS A COMBO CONTRACT?                    
         BAS   RE,GETEL                                                         
         BNE   DISP0830                                                         
*                                  YES, DISPLAY COMBO COMPONENT K'S             
         ZIC   R4,1(R6)            FIND END OF ELEMENT                          
         LA    R4,0(R4,R6)                                                      
*                                                                               
         MVC   CONCMBS,CONSTA      CURRENTLY DISPLAYED CONTRACT ALWAYS          
         MVC   CONCMBC,CONCNUM       FIRST IN COMBO DISPLAY LINE                
         LA    R6,2(R6)            POINT TO STATION CALL LETTERS                
         LA    R2,CONCMBCH                                                      
*                                                                               
DISP0780 DS    0H                  POINT TO STATION CALL LETTER FIELD           
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
DISP0790 CLC   RCONKSTA,0(R6)      FIND AND DISPLAY COMPONENTS OTHER            
         BNE   DISP0800              THAN ITSELF                                
         LA    R6,9(R6)                                                         
         CR    R6,R4                                                            
         BL    DISP0790                                                         
         B     DISP0810                                                         
*                                                                               
DISP0800 DS    0H                                                               
         MVC   8(4,R2),0(R6)       DISPLAY STATION CALL LETTERS                 
         MVI   12(R2),C'-'                                                      
         MVC   13(1,R2),4(R6)                                                   
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZAP   WORK2(5),=P'0'      DISPLAY CONTRACT #                           
         MVO   WORK2(5),5(4,R6)                                                 
         EDIT  (P5,WORK2),(8,8(R2)),ALIGN=LEFT                                  
         OI    6(R2),X'20'         PROTECT                                      
         LA    R6,9(R6)                                                         
         CR    R6,R4                                                            
         BL    DISP0780                                                         
*                                                                               
DISP0810 DS    0H                                                               
         LA    R5,4                                                             
         LA    R2,CONCMBCH         PROTECT ALL COMBO K# FIELDS                  
DISP0820 OI    6(R2),X'20'                                                      
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R5,DISP0820                                                      
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'17'        GET COMBO ELEMENT                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   RF,1(R6)            GET LENGTH OF ELEMENT                        
         S     RF,=F'2'            SUBT ELEMENT CODE AND LENGTHBYTES            
         SR    RE,RE                                                            
         D     RE,=F'9'            DIVIDE BY LENGTH OF ONE COMBO ENTRY          
         STC   RF,TWACOMBO         =NUMBER OF COMBO COMPONENTS                  
*                                                                               
*** COMBO                                                                       
DISP0830 DS    0H                                                               
         GOTO1 VCLEAR,DMCB,1                                                    
         MVC   TWACDTES,RCONDATE   SAVE K DATES                                 
         MVI   ELCODE,X'1E'        CHECK FOR REVISED DATE                       
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BNE   EXXMOD                                                           
         USING RCONRFEL,R6                                                      
         OC    RCONRFLT,RCONRFLT                                                
         BZ    EXXMOD                                                           
         MVC   TWACDTES,RCONRFLT   SAVE K REVISED DATES                         
         B     EXXMOD                                                           
         DROP  R6                                                               
         EJECT                                                                  
MONTABLE DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
*                                                                               
RTGSTAB  DC    CL3'ARB'                                                         
         DC    CL3'NSI'                                                         
         DC    CL3'BIR'            BIRCH                                        
         DC    CL3'SRC'                                                         
         DC    X'FF'                                                            
         EJECT                                                                  
*********************************************************************           
* CHECK IF KATZ CONVERTED ORDERS. IF IT IS, CHECK IF ACE OR EASYLINK            
* SET. IF NOT, CHECK STATION AND SEE IF IT IS SET AFTER THE CONVERSION          
* AND SET APPROPRIATE FLAGS                                                     
*********************************************************************           
CHKCONV  NTR1                                                                   
         TM    RCONMODR+1,X'10'                                                 
         BZ    CHKCONVX                                                         
*                                                                               
         TM    RCONMODR+1,X'80'+X'40'                                           
         BNZ   CHKCONVX                                                         
*                                                                               
*              STATION                                                          
         XC    IOAREA(32),IOAREA   BUILD STATION KEY                            
         MVI   RSTAKTYP,2                                                       
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,RCONKSTA                                                
*                                                                               
         MVC   KEY,IOAREA                                                       
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   CHKCONVX            NO X'05' - NOT ACE/GRAPHNET                  
*                                                                               
         USING RSTAXEL,R6                                                       
         OC    RSTARID,RSTARID     IS THERE RECEIVING ID                        
         BZ    CHKCONVX            NO                                           
         CLC   RSTARID,=X'0406'    GRAPHNET?                                    
         BNE   CHKCV10                                                          
         DROP  R6                                                               
*                                                                               
         OI    RCONMODR+1,X'40'    YES,MARK CONTRACT GRAPHNET                   
         B     CHKCV20                                                          
*                                                                               
CHKCV10  DS    0H                                                               
         OI    RCONMODR+1,X'80'    OTHERWISE, IT'S ACE                          
*                                                                               
CHKCV20  DS    0H                                                               
         MVC   KEY+28(4),TWAKADDR                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,AIO2                                                
*                                                                               
         GOTO1 VPUTREC,DMCB,RCONREC                                             
*                                                                               
CHKCONVX DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*********************************************************************           
* GETCONT READS CONTYPE RECORD & PUTS OPTION BYTES TO TWA FIELDS    *           
*********************************************************************           
GETCONT  DS    0H                                                               
         NTR1                                                                   
         MVI   TWAPRFK,0                DEFAULT VALUES FOR OPTION BYTES         
         MVI   TWAPRFW,0                                                        
         XC    TWAPRFA,TWAPRFA                                                  
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVI   0(R6),RCTYKTYQ                                                   
         MVC   24(2,R6),REPALPHA                                                
         MVC   26(1,R6),RCONTYPE                                                
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RCTYKEY),KEYSAVE                                           
         BNE   GCX                                                              
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   GCX                                                              
         USING RCTYFEL,R6                                                       
         MVC   TWAPRFK,RCTYFPRC         WRITE OPTIONS TO TWA                    
         MVC   TWAPRFW,RCTYFPRW                                                 
         MVC   TWAPRFA,RCTYFPRA                                                 
         DROP  R6                                                               
GCX      B     EXXMOD                                                           
         EJECT                                                                  
*********************************************************************           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
*               DDCOMFACS                                                       
*               FAFACTS                                                         
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
*********************************************************************           
* CHKDARE CHECKS FOR DARE ORDERS.  IF FOUND, LIMITS FIELD CHANGES   *           
*********************************************************************           
         CSECT                                                                  
CHKDARE  NMOD1 0,*CKDA*                                                         
         L     RC,0(R1)                                                         
         BAS   RE,CLRPROTS         CLEAR ALL PROTECTION BITS                    
*                                                                               
         LA    R6,RCONREC          LOOK FOR SEND ELEMENT                        
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL2                                                        
         BNE   CHDA0060            YES - NO EXTENDED ELEMENT                    
*                                     CONSIDER IT 'UNCONFIRMED'                 
         USING RCONXEL,R6                                                       
*                                                                               
         TM    RCONCONF,X'40'+X'20' CONFIRMED OR PREVIOUSLY CONFIRMED?          
         BZ    CHDA0060            YES - MUST BE UNCONFIRMED                    
         DROP  R6                                                               
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL2                                                        
         BNE   CHDA0160                                                         
         USING RCONDREL,R6                                                      
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    CHDA0160            PROTECT PRODUCT FIELD FOR                    
         TM    RCONDRF2,X'01'      EXIT IF XML                                  
         BO    CHDA0160                                                         
         TM    RCONDRF2,X'80'+X'10' POOL/VARIOUS                                
         BZ    CHDA0160                                                         
         OI    CONPRDH+1,X'20'     SET PRD FIELD TO 'PROTECTED'                 
         OI    CONPRDH+6,X'80'     SET PRD FIELD TO 'TRANSMIT'                  
         B     CHDA0110                                                         
         DROP  R6                                                               
*                                                                               
CHDA0060 EQU   *                                                                
         LA    R6,RCONREC          LOOK FOR DARE ELEMENT                        
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL2                                                        
         BNE   CHDA0160                                                         
         USING RCONDREL,R6                                                      
*                                                                               
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    CHDA0100            PROTECT PRODUCT FIELD FOR                    
         TM    RCONDRF2,X'01'      EXIT IF XML                                  
         BO    CHDA0160                                                         
         TM    RCONDRF2,X'80'+X'10' POOL/VARIOUS                                
         BZ    CHDA0100                                                         
         OI    CONPRDH+1,X'20'     SET PRD FIELD TO 'PROTECTED'                 
         OI    CONPRDH+6,X'80'     SET PRD FIELD TO 'TRANSMIT'                  
*                                                                               
CHDA0100 EQU   *                                                                
         CLI   RCONDRFG,0          ANYTHING SET IN STATUS BYTE?                 
         BE    CHDA0160            NO  - NOT DARE CONTRACT                      
* KATZ EDI ORDER? OR ONE SHOT? TAKEOVER?                                        
         TM    RCONDRFG,X'04'+X'02'+X'01'                                       
         BZ    CHDA0110            YES, DON'T TREAT AS DARE CONTRACT            
         OI    CONAGYH+1,X'20'     SET FIELD TO 'PROTECTED'                     
         OI    CONAGYH+6,X'80'     SET FIELD TO 'TRANSMIT'                      
         B     CHDA0160            YES - DON'T TREAT AS DARE CONTRACT           
         DROP  R6                                                               
*                                                                               
CHDA0110 EQU   *                                                                
         OI    TWADARE,X'01'       SET 'DARE ORDER' FLAG                        
         LA    R2,FIELDTBL         SET A(SCREEN FIELD TABLE)                    
*                                                                               
CHDA0120 EQU   *                                                                
         CLI   0(R2),X'FF'         TABLE DELIMITER REACHED?                     
         BE    CHDA0160            YES - RESTRICTED FIELDS                      
*                                     PROTECTED FOR DARE ORDERS                 
         TM    4(R2),X'80'         FIELD ALLOWS CHANGE?                         
         BO    CHDA0140            YES - GO TO NEXT FIELD                       
         MVC   DUB(4),0(R2)        NO  - GET A(FIELD HDR)                       
         L     R3,DUB                                                           
         LA    RF,TWAD             SET ADDRESSABILITY                           
         AR    R3,RF                                                            
         OI    1(R3),X'20'         SET FIELD TO 'PROTECTED'                     
         OI    6(R3),X'80'         SET FIELD TO 'TRANSMIT'                      
CHDA0140 EQU   *                                                                
         LA    R2,5(R2)            BUMP TO NEXT ENTRY                           
         B     CHDA0120            GO BACK FOR NEXT                             
CHDA0160 EQU   *                   RETURN WITH CC= ZERO                         
         SR    R0,R0               SET CC = ZERO                                
CHDA0180 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CLRPROTS:  CLEARS ALL PROTECTION BITS FROM FIELDS IF PREVIOUSLY             
*        SET.                                                                   
*                                                                               
CLRPROTS NTR1                                                                   
         LA    R2,FIELDTBL         SET A(SCREEN FIELD TABLE)                    
CLPR0020 EQU   *                                                                
         CLI   0(R2),X'FF'         TABLE DELIMITER REACHED?                     
         BE    CLPR0040            YES -                                        
         MVC   DUB(4),0(R2)        NO  - GET A(FIELD HEADER)                    
         L     R3,DUB                                                           
         LA    RF,TWAD             SET ADDRESSABILITY                           
         AR    R3,RF                                                            
         NI    1(R3),X'DF'         TURN OFF PROTECT BIT                         
         OI    6(R3),X'80'         TURN ON TRANSMIT BIT                         
         LA    R2,5(R2)            BUMP TO NEXT ENTRY                           
         B     CLPR0020            GO BACK FOR NEXT                             
CLPR0040 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
         GETEL2 R6,34,ELCODE                                                    
*                                                                               
*   CONTRACT SCREEN FIELD TABLE CONTAINS ADDRESS OF EACH SCREEN                 
*      FIELD HEADER, AND A FLAG BYTE.  IF FIELD CAN BE CHANGED,                 
*      FLAG BYTE IS SET TO X'80'.                                               
*                                                                               
FIELDTBL DC    AL4(CONCNUMH-TWAD),X'80'                                         
         DC    AL4(CONTYPEH-TWAD),X'80'                                         
         DC    AL4(CONBUYH-TWAD),X'00'                                          
         DC    AL4(CONADVH-TWAD),X'80'                                          
         DC    AL4(CONPRDH-TWAD),X'80'                                          
         DC    AL4(CONCATH-TWAD),X'80'                                          
         DC    AL4(CONSTAH-TWAD),X'00'                                          
         DC    AL4(CONSALH-TWAD),X'80'                                          
         DC    AL4(CONDSPH-TWAD),X'80'                                          
         DC    AL4(CONRTGH-TWAD),X'80'                                          
         DC    AL4(CONIADVH-TWAD),X'00'                                         
         DC    AL4(CONIPRDH-TWAD),X'00'                                         
         DC    AL4(CONIESTH-TWAD),X'00'                                         
         DC    AL4(CONDCTH-TWAD),X'80'                                          
         DC    AL4(CONCOM1H-TWAD),X'80'                                         
         DC    AL4(CONCOM2H-TWAD),X'80'                                         
EDITBL   DC    AL4(CONAGYH-TWAD),X'00'                                          
         DC    AL4(CONDTESH-TWAD),X'00'                                         
         DC    X'FF'               DELIMITER                                    
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'122RECNT20   03/13/13'                                      
         END                                                                    
