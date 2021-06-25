*          DATA SET PPPUB01    AT LEVEL 195 AS OF 05/16/16                      
*PHASE T40601A                                                                  
*INCLUDE SRCHPASS                                                               
*INCLUDE SRCHCALL                                                               
*                                                                               
*         CHANGE LOG                                                            
* KWAN MAY13/2016  BYPASS ZONE LOCK VALIDATION IF NO BASE PUB FOUND             
*                                                                               
* KWAN 11/23/15    ADD ZONE LOCK FOR TRADE DESK BUYS (USED IN RADIA)            
*                                                                               
* MHER 7/30/14     ADD SUPPORT FOR SAP INTERFACE                                
*                  PUGENEROL NOW COPIED INTO PROGRAM SO COULD PUT               
*                  CONSTANTS AFTER IT.                                          
*                  R9 NO LONGER POINTED TO PUBREC, IT'S ADDRESSABLE             
*                                                                               
*  KWAN 01/07/13   REMOVE "INCLUDE GETPROF" USE GETPROF FROM COMFACS            
*                                                                               
*  BOBY 05/11      AMERICAN PUBS CAN'T HAVE PST OR GST NE 'X'                   
*                                                                               
*  SMYE 10/10      ADD "LOCK" FIELD TO NAME SCREEN                              
*                                                                               
*  SMYE 11/06      REQUIRE EXISTENCE OF A BASE PUB TO ADD A PUB WITH            
*                  ZONE AND/OR EDITION IF PROFILE SO INDICATES                  
*                                                                               
*  SMYE 08/17/05   VALIDATE FAX NUMBER (NAMFAX) AS NUMERIC AND                  
*                  FORMAT AS NNN-NNN-NNNN WHEN 10-CHAR LENGTH                   
*                                                                               
*  BPLA 11/98      ADD FIELD FOR WEB SITE - COUNTY CODE MOVED TO SAME           
*                  FIELD LINE AS STATE CODE.                                    
*                                                                               
*  SMYE 3/97       DISALLOW MORE THAN 1 PST CODE ENTRY (AT VALPST)              
*                                                                               
*  BPLA 4/96       CHECK FOR SJR IN PUBLISHER CHECK NO-OPED                     
*                                                                               
*  BPLA  4/96      WHEN EDITING REPS (EDITREPS)                                 
*                  CHECK LENGTH OF "OLD" REP ELEM (IF FOUND)                    
*                  IF LENGTH IS GREATER THAN 32 (X'20')                         
*                  SET ESWITCH TO 1 (SO OLD ELEM WON'T BE DELETED)              
*                 -EVEN IF NO REPS ARE ENTERED (OTHER DATA MAY BE               
*                  PRESENT)                                                     
*                                                                               
*  BPLA  3/96      AT EDTREP2 CHECK REP INPUT LENGHT                            
*                  SOMEONE DIED ENTERING A FAX NUNMBER                          
*                                                                               
*                                                                               
*  BPLA  3/96      IN FINDAEL - DELETE OLD SUPPLEMENTAL ADDRESS                 
*                  ELEMENT (IT MIGHT HAVE THE WRONG LENGTH)                     
*                                                                               
*  BPLA  2/96      CHANGE PUBLISHER ERROR                                       
*                                                                               
*  SMYE  2/96    INCLUDE PUGENEROL (PUB VERSION OF PPGENEROL)                   
*                ALSO USE PUGENOLD (CURRENTLY SAME AS PPGENOLD)                 
*                                                                               
*  BPLA   12/95    PUBLISHER CHECK NO-OPED AND LANGUAGE CODE                    
*                  ACTIVATED (SINCE IT IS BEING RELEASED FIRST)                 
*                  SEARCH FOR "**PUBL" TO FIND NO-OPED CODE                     
*                                                                               
*  BPLA   7/95     WHEN ENTERING PUBLISHER (REP) CHECK TO BE SURE               
*                  REP IS DESIGNATED AS A PUBLISHER                             
*                                                                               
*  LWEI   6/03/93  PST                                                          
*                                                                               
*  BPLA   9/12/91  INCLUDE PPPUBWRK                                             
*                                                                               
*  BPLA   9/11/91  PUBLANG CODE NO-OPED FOR NOW (ALSO REMOVED FROM              
*                  SCREEN. REP NAME SEARCHING CODE ADDED (ALSO                  
*                  SCREEN FIELDS NO LONGER NUMERIC AND MADE BIGGER              
*                  (20 CHARACTERS)                                              
*                                                                               
*  GPLA   8/12/91  NAME SEARCHING CODE ADDED                                    
*                                                                               
*  BPLA   6/25/91  ADD PUBLANG (LANGUAGE FIELD)                                 
*                                                                               
*  ROSA   10/17/90 ADD GST AKA VAT TAX                             L03          
*+++++ NOTE EDITING CODE PATCHED AROUND                                         
*                                                                               
* SWON 12/27/89 CHECK FOR AND DISPLAY FAX NUMBER                   L02          
*               IF NAMFAXH NOT EMPTY, MOVE NAMFAX INTO PUBFAX       L02         
*                                                                               
*  ROSA   6/6/88  IF CANADIAN AGENCY  1- MOVE 90 INTO STATE CODE   L01          
*                 2- ACCEPT ANY ZIP CODE ENTRY                     L01          
*                 3- 000 IN  COUNTRY CODE                          L01          
         TITLE 'T40601 PRINTPAK   PUB NAME SCREEN'                              
*                                                                               
         PRINT NOGEN                                                            
T40601   CSECT                                                                  
         NMOD1 0,T40601,RR=RE                                                   
         L     RC,0(R1)                                                         
*                                                                               
         LA    R9,2048(RC)                                                      
         LA    R9,2048(R9)                                                      
         USING GENOLD,RC,R9                                                     
*                                                                               
         USING T406FFD,RA                                                       
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         USING T40601+4096,R7      NOTE USE OF SECOND BASE REGISTER             
*                                                                               
         ST    RE,RELO                                                          
*                                                                               
PUBRD    DS    0H                                                               
         LA    R4,PUBIO                                                         
         LH    R5,=H'4000'                                                      
         BAS   RE,CLEARWRK                                                      
         OC    PUBADDR,PUBADDR                                                  
         BZ    PUBSCRN                                                          
         MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,GETPUB                                                        
*                                                                               
PUBSCRN  DS    0H                                                               
         CLI   FORMAT,1            SEE IF ACTION = FORMAT                       
         BE    FORMATP                                                          
*                                                                               
*     PUBNAME SCREEN IN TWA - SO EDIT IT UNLESS ACTION =SRDS OR                 
*     DISPLAY  --- IF ACTION = CHANGE AND PUBREC IS A SRDS REC                  
*                  CHANGE ONLY THOSE FIELDS FROM THE LTLREC - THE               
*                  PUBREC FIELDS WILL BE PROTECTED                              
         CLI   BACT,2         SEE IF ACTION= SRDS OR DISPLAY                    
         BH    FORMATP        YES,THEN FORMAT- NO EDITING                       
         BNE   NEWPUB         ACT 1 = NEW                                       
*                                                                               
*        FOR CHANGE NEED NAMESEARCH 'BEFORE' CALL                               
*                                                                               
         LA    RF,SRCHBLK                                                       
         USING SEARCHD,RF                                                       
         MVC   SBSYSTEM,=CL6'PRINT'                                             
         MVI   SBID,C' '                                                        
         MVC   SBID+1(L'SBID-1),SBID                                            
         DROP  RF                                                               
         GOTO1 =V(SRCHPASS),DMCB,(C'B',SRCHBLK),ACOMFACS,PUBIO,        X        
               SRCHLST,PUBADDR,0,RR=RELO                                        
         B     FINDELE                                                          
*                             THEN MUST BE ADD - SO FORMAT PUBREC               
NEWPUB   MVC   PUBKEY(1),BMED                                                   
         MVC   PUBKEY+1(6),BPUB                                                 
         MVC   PUBKAGY(2),AGYALPHA                                              
         MVI   PUBKCOD,X'81'                                                    
         MVC   PUBKEY+25(2),=X'00E5'    CNG 01/06/88 00C1 TO 00E5               
         MVC   PUBNAMEL(2),=X'10C4'       02/03/88 10A0 TO 10C4 =196            
         B     EDITPUB                                                          
*                                                                               
FINDELE  LA    R4,PUBREC+33                                                     
CKELEM   CLI   0(R4),0                                                          
         BE    NONAMEL             NO ELEMENT FOUND BRANCH                      
         CLI   0(R4),X'10'         PUBNAMEL                                     
         BE    EDITPUB                                                          
         DC    H'0'              BAD PUB RECORD - NO NAME ELEMENT               
*                                MUST BE FIRST ELEMENT                          
*                                                                               
NONAMEL  LA    R3,PNAMERR                                                       
         B     ERROR                                                            
EDITPUB  CLC   PUBKAGY(2),AGYALPHA                                              
         BNE   STDERROR                                                         
         LA    R2,NAMNAMEH                                                      
         BAS   RE,ANY                                                           
         XC    PUBNAME,PUBNAME                                                  
         MVC   PUBNAME,NAMNAME                                                  
         LA    R2,NAMZNAMH                                                      
         XC    PUBZNAME,PUBZNAME                                                
         CLI   5(R2),0             ZONE NAME NOT REQUIRED                       
         BE    EDITPUB1                                                         
         MVC   PUBZNAME,NAMZNAM                                                 
*                                                                               
EDITPUB1 BRAS  RE,VALZLOCK         VALIDATE ZONE LOCK                           
         JNE   EXIT                                                             
*                                                                               
         LA    R2,NAMADDRH                                                      
         XC    PUBLINE1,PUBLINE1                                                
         CLI   BMED,C'O'           FOR OUTDOOR - ADDR NOT REQ                   
         BNE   EDITPUB2                                                         
         CLI   5(R2),0                                                          
         BE    EDITPUB3                                                         
*                                                                               
EDITPUB2 BAS   RE,ANY                                                           
         MVC   PUBLINE1,NAMADDR                                                 
EDITPUB3 LA    R2,NAMCITYH                                                      
         XC    PUBCITY,PUBCITY                                                  
         CLI   BMED,C'O'           FOR OUTDOOR - CITY NOT REQ                   
         BNE   EDITPUB4                                                         
         CLI   5(R2),0                                                          
         BE    EDITPUB5                                                         
*                                                                               
EDITPUB4 DS    0H                                                               
         CLI   NAMADL2H+5,0        CHK FOR INPUT IN LINE 2                      
         BE    EDTPUB4C            NO - REQUIRE CITY                            
         CLI   5(R2),0             YES - CITY OPTIONAL                          
         BE    EDITPUB5                                                         
*                                                                               
EDTPUB4C BAS   RE,ANY                                                           
         MVC   PUBCITY,NAMCITY                                                  
*                                                                               
EDITPUB5 DS    0H                                                               
         LA    R2,NAMSTATH                                                      
         BAS   RE,ANY                                                           
         MVC   PUBSTATE,NAMSTAT                                                 
         XC    PUBZIP,PUBZIP                                                    
         XC    PUBNWZIP,PUBNWZIP       ADD 01/06/88                             
         LA    R2,NAMZIPH                                                       
         LA    R3,ZIPERR                                                        
         CLI   5(R2),0                                                          
         BE    EDITPA                                                           
         CLI   5(R2),5                                                          
         BL    ERROR                    CNG 01/06/88 BNE TO BL                  
         BAS   RE,CKZIP                 ADD 01/06/88                            
         MVC   PUBNWZIP(10),NAMZIP      CNG 01/06/88 PUBZIP TO PUBNWZIP         
EDITPA   EQU   *                                                                
*                                                                               
*   NOW,I MUST BUILD PUBLINE2 FROM CITY,STATE,ZIP                               
         XC    PUBLINE2,PUBLINE2                                                
         LA    R2,NAMADL2H                                                      
         CLI   5(R2),0                                                          
         BE    EDITPB              NO INPUT - GO BUILD LINE 2                   
         MVC   PUBLINE2,NAMADL2                                                 
         B     EDITPR                                                           
*                                                                               
CKZIP    EQU   *                        *******************************         
         LARL  R1,TABLE                                                         
         MVC   ZIPWK(10),NAMZIP                 ADD 01/06/88          *         
         OC    ZIPWK+5(5),=5C' '                                      *         
         CLC   ZIPWK+5(5),=5C' '                                      *         
         BNE   CKZIP10                        CK 10 DIGITS ZIP        *         
         TR    ZIPWK(5),0(R1)                 CK 5 DIGITS ZIP         *         
         CLC   ZIPWK(5),=C'NNNNN'                                     *         
         BNE   ERRORZIP                       INVALID ZIP           L01         
         MVI   ZIP5FLAG,C'Y'             ADD 01/20/88 FLAG ZIP5       *         
         B     CKZIPOK                                              L01         
CKZIP10  TR    ZIPWK(10),0(R1)                                        *         
         CLC   ZIPWK(10),=C'NNNNN-NNNN'                               *         
         BE    CKZIPOK                                                *         
         B     ERRORZIP                       INVALID ZIP             *         
CKZIPOK  BR    RE                                                     *         
*                                                                     *         
*                                                                   L01         
ERRORZIP CLI   ANATION,C'C'       CANADIAN AGENCY         F         L01         
         BE    CKZIPOK                                              L01         
         CLC   NAMSTCD(2),=C'90'  OR CANADIAN PUB                               
         BE    CKZIPOK                                                          
         B     ERROR                                                L01         
*                                                                               
ZIP5FLAG DC    C'N'                        'Y' FOR 5 DIGITS ZIP       *         
ZIPWK    DS    CL10                                                   *         
*                                       *******************************         
EDITPB   DS    0H                                                               
         CLI   NAMCITYH+5,0    NO CITY - CAN'T BUILD PUBLINE2                   
         BE    EDITPR                                                           
         CLI   ZIP5FLAG,C'Y'         ADD 01/20/88                               
         BNE   EDITPB10              ADD 01/20/88  IT'S 10 DIGITS ZIP           
         MVI   ZIP5FLAG,C'N'         ADD 01/20/88  REINITIALIZE FLAG            
         SR    R5,R5                                                            
         IC    R5,NAMCITYH+5         LENGTH OF CITY                             
         BCTR  R5,R0                                                            
         LA    R6,PUBLINE2           FOR 5 DIGITS ZIP LINE 2 WILL BE:           
         EX    R5,MVCITY                   CITY, ST   XXXXX                     
         LA    R6,1(R6)                                                         
         AR    R6,R5                                                            
         MVI   0(R6),C','                                                       
         MVC   2(2,R6),NAMSTAT                                                  
         MVC   7(5,R6),NAMZIP                                                   
         FOUT  NAMADL2H,PUBLINE2,30                                             
         B     EDITPR                                                           
*                                                                               
EDITPB10 SR    R5,R5                     ****************************           
         IC    R5,NAMCITYH+5             LENGTH OF CITY             *           
         BCTR  R5,R0                     ADD 01/20/88               *           
         LA    R6,PUBLINE2                                          *           
         EX    R5,MVCITY          FOR 9 DIGITS ZIP LINE2 WILL BE:   *           
         LA    R6,1(R6)                  CITY,ST XXXXX-XXXX         *           
         AR    R6,R5                                                *           
         MVI   0(R6),C','                                           *           
         MVC   1(2,R6),NAMSTAT                                      *           
         MVC   4(10,R6),NAMZIP                                      *           
         FOUT  NAMADL2H,PUBLINE2,30                                 *           
         B     EDITPR                    ****************************           
*                                                                               
MVCITY   MVC   0(0,R6),NAMCITY                                                  
**NEW 5/2/89                                                                    
EDITPR   EQU   *                PUBLISHER                                       
         MVC   SAVPLSH,PUBPLSH     SAVE OLD PUBLISHER                           
         XC    PUBPLSH,PUBPLSH                                                  
         XC    NAMPLN,NAMPLN      CLEAR NAME                                    
         FOUT  NAMPLNH                                                          
         LA    R2,NAMPLSHH                                                      
         CLI   5(R2),0                                                          
         BE    EDITP1                                                           
         BAS   RE,EDTREP                                                        
         LA    R3,REPERR                                                        
*                                   ONLY FOR SJR FOR NOW                        
******   PUBLISHER CHECK FOR EVERYONE 4/96                                      
******   CLC   AGYALPHA,=C'SJ'                                                  
******   BNE   EDITPR3                                                          
         TM    PREPSTAT,X'01'       SEE IF DESIGNATED AS A PUBLISHER            
         BO    EDITPR3                                                          
         MVC   NAMPLN(25),=C'*REP MUST BE A PUBLISHER*'                         
         FOUT  NAMPLNH                                                          
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
*                                                                               
EDITPR3  MVC   PUBPLSH,NAMPLSH                                                  
         FOUT  (R2)                                                             
         FOUT  NAMPLNH,PREPNAME,30                                              
         B     EDITP1                                                           
**NEW 5/2/89                                                                    
EDITP1   EQU   *                                                                
         XC    PUBSTACD(5),PUBSTACD  CLEAR ST AND CTY                           
         LA    R2,NAMSTCDH                                                      
         CLI   5(R2),0                                                          
         BNE   EDITP1AA                                                         
         CLI   NAMCTCDH+5,0                                                     
         BE    EDITP1AB            SKIP IF NO STATE OR COUNTY                   
EDITP1AA EQU   *                                                                
         BAS   RE,ANY                                                           
         LA    R3,SCDERR                                                        
         CLI   5(R2),2                                                          
         BNE   ERROR                                                            
         XC    PUBSTACD,PUBSTACD                                                
         MVC   PUBSTACD,NAMSTCD                                                 
         LA    R2,NAMCTCDH                                                      
         BAS   RE,ANY                                                           
         LA    R3,CCDERR                                                        
         CLI   5(R2),3                                                          
         BNE   ERROR                                                            
         XC    PUBCNTCD,PUBCNTCD                                                
         MVC   PUBCNTCD,NAMCTCD                                                 
EDITP1AB EQU   *                                                                
*                                                                               
*        VALIDATE GST FIELD                                                     
*                                                                               
         LA    R2,NAMGSTCH                                        L03           
         LA    R3,FLDINV                                          L03           
*                                                                               
* IF COUNTRY OF ORIGIN IS CANADA AND STATE CODE IS 90, THEN       L03           
*    GST IS MANDITORY AND IS THE ONLY CONDITION GST CAN BE ENTERED              
*                                                                 L03           
         MVI   PUBGST,0            INITALIZE                      L03           
*                                                                 L03           
         CLI   ANATION,C'C'        SKIP IF NOT CANADIAN AGENCY                  
         BNE   NOTCANA                                            L03           
*                                                                 L03           
*NOT A   BAS   RE,ANY  REQUIRED FIELD PER MEL 10/18/90            L03           
*                                                                 L03           
         CLI   5(R2),0                                            L03           
         BE    EDITP1A0            0KAY IF NO INPUT                             
*                                                                 L03           
*        VALIDATE GST CODE                                        L03           
*                                                                 L03           
         LA    RF,VALGSTC                                         L03           
*                                                                 L03           
CLI255   CLI   0(RF),255           ERROR IF END OF TABLE          L03           
         BE    ERROR                                              L03           
*                                                                 L03           
         CLC   0(1,RF),NAMGSTC     OKAY IF MATCHED TO TABLE ENTRY L03           
         BE    *+12                                               L03           
         LA    RF,1(RF)                                           L03           
         B     CLI255                                             L03           
*                                                                 L03           
         MVC   PUBGST,NAMGSTC      SAVE GST TAX CODE              L03           
*                                                                 L03           
EDITP1A0 DS    0H                                                               
*                                                                               
         CLC   =C'90',PUBSTACD     IF NOT CANADIAN PUB                          
         BE    EDITP1AC                                                         
*                                                                 L03           
         CLI   PUBGST,C'X'            GST CODE MUST BE 'X'                      
         BE    EDITP1AC                                                         
*                                                                 L03           
         LARL  RE,PSTNOTX                                                       
         MVC   PBLMSG,0(RE)                                                     
         FOUT  PBLMSGH                                                          
*                                                                 L03           
         B     EXIT                                                             
*                                                                               
NOTCANA  CLI   5(R2),0                                            L03           
         BE    EDITP1AC                                           L03           
         LA    R3,FLDINV                                          L03           
         B     ERROR                                              L03           
*                                                                               
VALGSTC  DC    C'STXZ',X'FF'                                      L03           
*                                                                               
EDITP1AC DS    0H                                                 L03           
*                                                                               
*        VALIDATE PST CODE                                                      
*                                                                               
         LA    R2,NAMPSTH                                                       
*                                                                               
         CLI   ANATION,C'C'        CANADA                                       
         BNE   EDITPUB6                                                         
*                                                                               
         CLI   5(R2),0                                                          
         BE    EDITPUB6            NO INPUT                                     
*                                                                               
         LA    R3,FLDINV                                                        
         BAS   RE,VALPST                                                        
         BZ    EDITPUB6            PST OKAY                                     
*                                                                               
         CLI   ERRAREA,X'FF'       ERROR MESSAGE SET                            
         BE    EXIT                                                             
*                                                                               
         B     ERROR               ELSE LOOK UP MESSAGE                         
*                                                                               
EDITPUB6 DS    0H                                                               
*                                                                               
         LA    R2,NAMKDATH                                                      
         CLI   5(R2),0                                                          
         BNE   EDITP1A                                                          
         XC    PUBKILL(3),PUBKILL                                               
         B     EDITP1L                                                          
EDITP1A  XC    WORK(10),WORK                                                    
         GOTO1 VDATVAL,DMCB,(0,NAMKDAT),WORK                                    
         LA    R3,20               INVALID DATE FORMAT                          
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
*******  GOTO1 VDTCNV,DMCB,(0,WORK),(1,PUBKILL)                                 
         GOTO1 VDATCON,DMCB,(0,WORK),(3,PUBKILL)                                
*                                                                               
EDITP1L  DS    0H                       EDIT LANGUAGE                           
         XC    PUBLANG,PUBLANG                                                  
         LA    R3,FLDINV                                                        
         LA    R2,NAMLANGH                                                      
         CLI   5(R2),0                                                          
         BE    EDITPLK                                                          
         LA    RF,LANGTAB                                         L03           
EDITP1L5 CLI   0(RF),255                                          L03           
         BE    ERROR                                              L03           
         CLC   0(1,RF),NAMLANG                                    L03           
         BE    *+12                                               L03           
         LA    RF,1(RF)                                           L03           
         B     EDITP1L5                                           L03           
         MVC   PUBLANG,NAMLANG                                    L03           
         B     EDITPLK                                           L03            
                                                                                
LANGTAB  DC    C'EF'               E=ENGLISH,F=FRENCH                           
         DC    X'FFFF'             END OF TABLE                                 
*                                                                               
EDITPLK  DS    0H                       EDIT "LOCK"                             
         NI    PUBLOCSW,X'FF'-PUBLCKDQ  CLEAR SWITCH                            
         LA    R3,FLDINV                                                        
         LA    R2,NAMLOCKH                                                      
         CLI   5(R2),0                                                          
         BE    EDITP2                                                           
         CLI   8(R2),C'N'                                                       
         BNE   EDITPLK4                                                         
         FOUT  NAMLOCKH,=C' ',1                                                 
         B     EDITP2                                                           
EDITPLK4 DS    0H                                                               
         CLI   8(R2),C'Y'                                                       
         BNE   ERROR               NOT N,Y OR BLANK - ERROR                     
         OI    PUBLOCSW,PUBLCKDQ   SET "LOCKED"                                 
*****    B     EDITP2                                                           
*                                                                               
EDITP2   DS    0H                                                               
         MVC   WORK(15),PUBEDTS         SAVE OLD EDTS                           
         XC    PUBEDTS,PUBEDTS                                                  
         LA    R3,FLDINV           FLDINV                                       
         LA    R2,NAMEDTSH                                                      
         JIF   BMED,NE,C'O',OR,AGYALPHA,NE,=C'ZZ',EDITP2C                       
         FOUT  NAMEDTSH,=C'PR',2                                                
         MVI   5(R2),2             FAKE INPUT                                   
EDITP2C  CLI   5(R2),0                                                          
         BE    EDITP2H                                                          
         CLI   PBLMED,C'O'           MUST BE OUTDOOR                            
         BNE   ERROR                                                            
         CLI   BPUB+5,0           MUST BE NO EDITION                            
         BNE   ERROR                                                            
         JIF   NAMEDTS,=,=C'PR',OR,=C'RP',EDITP2B                               
         JIF   NAMEDTS,=,=X'D700',OR,=X'D900',EDITP2B                           
         B     ERROR                                                            
*                                                                               
EDITP2B  MVC   PUBEDTS(2),NAMEDTS                                               
         CLI   BACT,1              SEE IF ADD                                   
         BE    EDITP2J                                                          
*                                                                               
         LA    R1,2                                                             
         LA    R5,WORK                                                          
EDITP2E  LA    R6,3                                                             
         LA    RE,PUBEDTS                                                       
EDITP2F  CLC   0(1,R5),0(RE)                                                    
         BNE   EDITP2G                                                          
         LA    R5,1(R5)                                                         
         BCT   R1,EDITP2E                                                       
         B     EDITP2J                                                          
*                                                                               
EDITP2G  LA    RE,1(RE)                                                         
         BCT   R6,EDITP2F                                                       
         B     ERROR                                                            
*                                                                               
EDITP2H  CLC   WORK(2),=X'0000'            CAN'T DELETE EDITION CODE            
         BNE   ERROR                   CAN'T DELETE EDITION CODE                
         B     EDITP2X                                                          
*                                                                               
EDITP2J  DS    0H                                                               
         LA    R5,PUBEDTS          LIST  OF OUTDOOR EDITION CODES               
*                                                                               
EDITP2K  CLI   0(R5),0                                                          
         BE    EDITP2X                                                          
         MVC   KEY(25),PUBKEY      BUILD A KEY USING PUBKEY & ED. CODES         
         MVC   KEY+6(1),0(R5)                                                   
         BAS   RE,HIGHPUB          DO A DMREADHI,SAVE OFF KEY                   
         CLC   KEY(25),KEYSAVE     COMPARE SAVED KEY TO ACTUAL KEY              
         BNE   EDITP2L             IF NOT FOUND REPEAT LOOP                     
         TM    KEY+25,X'01'        PASSIVE POINTER                              
         BNZ   EDITP2L             IF FOUND GOTO EDITING                        
         LA    R3,52                                                            
         B     ERROR               IF NONE EXISTS GOTO ERROR                    
*                                                                               
EDITP2L  LA    R5,1(R5)                                                         
         B     EDITP2K                                                          
*                                                                               
*                                                                               
EDITP2X  B     EDITLTL                                                          
*                                                                               
         EJECT                                                                  
*                                  SUPPLEMENTAL ADDRESS INFO                    
EDITLTL  DS    0H                                                               
         XC    SADELEM,SADELEM                                                  
         MVI   ASWITCH,0                                                        
         B     FINDAEL             IF ELEMENT FOUND SET ASWITCH=1               
*                                                                               
EDITA    DS    0H                                                               
         LA    R5,SADELEM                                                       
         USING LTLADELD,R5         MOVE IN FIELDS FROM SCREEN                   
         XC    PUBATTN,PUBATTN                                                  
         MVI   ESWITCH,0     IF ANY FIELDS ENTERED SET ESWITCH=1                
         CLI   NAMATTNH+5,0                                                     
         BE    CKTELE                                                           
         MVI   ESWITCH,1                                                        
         MVC   PUBATTN,NAMATTN                                                  
CKTELE   XC    PUBTEL,PUBTEL                                                    
         CLI   NAMTELEH+5,0                                                     
         BE    CKFAX                                                L02         
         MVI   ESWITCH,1                                                        
         MVC   PUBTEL,NAMTELE                                                   
CKFAX    XC    PUBSFAXN,PUBSFAXN                                    L02         
         CLI   NAMFAXH+5,0                                          L02         
         BE    UPDATEA                                              L02         
         LA    R3,2        INVALID INPUT FIELD                                  
         MVI   ESWITCH,1                                            L02         
         LA    R2,NAMFAXH                                                       
         CLC   NAMFAX(3),=C'MB='                                    L02         
*SMY*    BNE   EDITFAX                                              L02         
         BNE   CKFAX10                                              L02         
         L     RF,ACOMFACS                                                      
         L     RF,CSCANNER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(R2),(3,SCANBLK)                                       
         CLI   DMCB+4,0                                                         
         BE    ERROR                                                            
         CLI   SCANBLK+1,X'08'     MUST BE 8 DIGITS                             
         BNE   ERROR                                                            
         TM    SCANBLK+3,X'80'     VALID NUMERIC                                
         BNO   ERROR                                                            
         CLC   SCANBLK+22(2),=C'62'                                             
         BNE   ERROR                                                            
         B     EDITFAX                                                          
*                                                                               
CKFAX10  DS    0H                                                               
         CLC   NAMFAX(3),=C'FX='                                                
         BE    EDITFAX                                                          
*                                  NOT MB= OR FX=                               
*                                                                               
*        EXTRACT ALL NUMERIC DIGITS FROM INPUT                                  
*                                                                               
         ZIC   R0,NAMFAXH+5        GET LENGTH OF INPUT                          
         XC    WORK,WORK           INIT WORKAREA                                
         LA    RF,NAMFAX           POINT TO INPUT                               
         LA    RE,WORK                                                          
*                                                                               
LHVFAXLP DS    0H                                                               
*                                                                               
         CLI   0(RF),C'0'          DROP NON-NUMERIC CHARACTERS                  
         BL    LHVFAXCN                                                         
         CLI   0(RF),C'9'                                                       
         BH    LHVFAXCN                                                         
*                                                                               
         MVC   0(1,RE),0(RF)       SAVE NUMERIC DIGIT                           
         AHI   RE,1                BUMP TO NEXT SAVE POSITION                   
*                                                                               
LHVFAXCN DS    0H                                                               
*                                                                               
         AHI   RF,1                BUMP TO NEXT INPUT CHARACTER                 
         BCT   R0,LHVFAXLP                                                      
*                                                                               
LHVFAXDN DS    0H                                                               
*                                                                               
         LA    RF,WORK             CALCULATE LENGTH OF FAX NUMBER               
         SR    RE,RF                                                            
*                                                                               
         CHI   RE,10               MUST BE AT LEAST 10 DIGITS                   
         BL    ERROR                                                            
*                                                                               
         BCTR  RE,0                PREP FOR EXECUTED MOVE                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PUBSFAXN(0),WORK    SAVE FAX NUMBER FOR POSSIBLE FORMAT          
*                                                                               
         MVC   NAMFAX,PUBSFAXN       AND FOR NON-FORMATTING                     
*                                                                               
*        RE-DISPLAY AND SAVE AS FORMATTED NUMBER IF 10 DIGITS                   
*                                                                               
         CHI   RE,9                AFTER USE IN EXECUTED MOVE ABOVE             
         BNE   EDITFAX                                                          
*                                                                               
         XC    NAMFAX,NAMFAX       CLEAR FIELD                                  
*                                                                               
         MVC   NAMFAX(3),PUBSFAXN  AREA CODE                                    
         MVI   NAMFAX+3,C'-'                                                    
         MVC   NAMFAX+4(3),PUBSFAXN+3  EXCHANGE                                 
         MVI   NAMFAX+7,C'-'                                                    
         MVC   NAMFAX+8(4),PUBSFAXN+6  NUMBER                                   
*                                                                               
EDITFAX  DS    0H                                                               
         OI    NAMFAXH+6,X'80'     XMIT                                         
         MVC   PUBSFAXN,NAMFAX                                                  
         B     UPDATEA                                                          
*                                                                               
FINDAEL  LA    R4,PUBREC+33                                                     
CKAEL    CLI   0(R4),0                                                          
         BNE   CKAEL1                                                           
         MVI   ASWITCH,1           NO PUBSADEL FOUND                            
         B     EDITA                                                            
CKAEL1   CLI   0(R4),X'11'                                                      
         BNE   NEXTA                                                            
*              MUST DELETE OLD SUPPLEMENTAL ADDR ELEMENT                        
*              IT MAY BE THE WRONG LENGTH                                       
*                                                                               
         GOTO1 VRECUP,DMCB,(1,PUBREC),0(R4),0                                   
         B     EDITA                                                            
*                                                                               
NEXTA    SR    R0,R0              FIND NEXT ELEMENT                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     CKAEL                                                            
*                                                                               
*     ASWITCH =1 IF PUBSADEL DID NOT EXIST ON LTLREC  SO USE RECUP              
*     TO ADD IT,IF NECCESSARY,ALSO    IT NEEDS TO BE DELETED IF                 
*     ESWITCH=0                                                                 
*                                                                               
UPDATEA  DS    0H                                                               
         CLI   ESWITCH,0           ALL FIELDS BLANK                             
         BE    EDITWEB                                                          
*                                                                               
         MVC   PUBSADEL(2),=X'113E'     FORMAT ELEMENT              L02         
         MVC   PUBSAOFF(3),=X'FFFFFF'                                           
         GOTO1 VRECUP,DMCB,(1,PUBREC),SADELEM,0(R4)                             
         B     EDITWEB                                                          
*                                                                               
         DROP  R5                                                               
         EJECT                                                                  
*                                 WEB SITE                                      
EDITWEB  DS    0H                                                               
         XC    WEBELEM,WEBELEM                                                  
         MVI   ASWITCH,0                                                        
         B     FINDWEL             IF ELEMENT FOUND SET ASWITCH=1               
*                                                                               
EDITW    DS    0H                                                               
         LA    R5,WEBELEM                                                       
         USING PUBWEBD,R5        MOVE IN FIELDS FROM SCREEN                     
         XC    PUBWEBS,PUBWEBS                                                  
         MVI   ESWITCH,0     IF ANY FIELDS ENTERED SET ESWITCH=1                
         CLI   NAMWEBSH+5,0                                                     
         BE    EDITW5                                                           
         MVI   ESWITCH,1                                                        
         LA    R2,NAMWEBSH                                                      
         ZIC   R1,5(R2)       INPUT LENGTH                                      
         LA    R1,2(R1)       ADJUST FOR ELEMENT CODE AND LENGTH                
         STC   R1,PUBWEBEL+1                                                    
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,MOVEWEB                                                       
         B     *+10                                                             
*                                                                               
MOVEWEB  MVC   PUBWEBS(0),NAMWEBS      EXECUTED                                 
*                                                                               
EDITW5   DS    0H                                                               
         B     UPDATEW                                                          
*                                                                               
*                                                                               
FINDWEL  LA    R4,PUBREC+33                                                     
CKWEL    CLI   0(R4),0                                                          
         BNE   CKWEL1                                                           
         MVI   ASWITCH,1           NO PUBWEBEL FOUND                            
         B     EDITW                                                            
CKWEL1   CLI   0(R4),X'70'                                                      
         BNE   NEXTW                                                            
*              MUST DELETE OLD WEB  ELEMENT                                     
*                                                                               
         GOTO1 VRECUP,DMCB,(1,PUBREC),0(R4),0                                   
         B     EDITW                                                            
*                                                                               
NEXTW    SR    R0,R0              FIND NEXT ELEMENT                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     CKWEL                                                            
*                                                                               
*     ASWITCH =1 IF PUBWEBEL DID NOT EXIST  SO USE RECUP                        
*     TO ADD IT,IF NECCESSARY - IT WILL HAVE BEEN DELETED                       
*                                                                               
UPDATEW  DS    0H                                                               
         CLI   ESWITCH,0           ALL FIELDS BLANK                             
         BE    EDITREPS                                                         
*                                                                               
         MVI   PUBWEBEL,X'70'     ELEMENT LENGTH SET ABOVE                      
         GOTO1 VRECUP,DMCB,(1,PUBREC),WEBELEM,0(R4)                             
         B     EDITREPS                                                         
*                                                                               
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
EDITREPS MVI   ASWITCH,0                                                        
         B     FINDREL                                                          
         USING LTLREPD,R4                                                       
EDITR    XC    PUBPAREP(24),PUBPAREP                                            
         XC    NAMPRN,NAMPRN                                                    
         FOUT  NAMPRNH                                                          
         MVI   ESWITCH,0                                                        
*                                                                               
         CLI   ASWITCH,0             SEE IF REP ELEM FOUND                      
         BNE   EDITR1                NO- GO TO EDITR1                           
*                                                                               
         CLI   PUBREPEL+1,X'20'      YES - THEN SEE IF "SHORT" ELEM             
         BNH   *+8                                                              
         MVI   ESWITCH,1       SO I WON'T DELETE OLD                            
*                              LONG ELEM (EVEN IF NO REPS ENTERED)              
*                              SINCE OTHER DATA MAY BE PRESENT                  
EDITR1   DS    0H                                                               
         LA    R2,NAMPREPH                                                      
EDITR2   CLI   NAMPREPH+5,0                                                     
         BE    CKCREP                                                           
         BAS   RE,EDTREP                                                        
EDITR4   MVI   ESWITCH,1                                                        
         MVC   PUBPAREP,NAMPREP                                                 
         FOUT  NAMPREPH                                                         
         FOUT  NAMPRNH,PREPNAME,30                                              
*                                                                               
CKCREP   XC    PUBCNREP,PUBCNREP                                                
         XC    NAMCRN,NAMCRN                                                    
         FOUT  NAMCRNH                                                          
         LA    R2,NAMCREPH                                                      
         CLI   NAMCREPH+5,0                                                     
         BE    CKTREP                                                           
         BAS   RE,EDTREP                                                        
CKCREP6  MVI   ESWITCH,1                                                        
         MVC   PUBCNREP,NAMCREP                                                 
         FOUT  NAMCREPH                                                         
         FOUT  NAMCRNH,PREPNAME,30                                              
         B     CKTREP                                                           
*                                                                               
*                                                                               
CKTREP   XC    PUBTRREP,PUBTRREP                                                
         XC    NAMTRN,NAMTRN                                                    
         FOUT  NAMTRNH                                                          
         LA    R2,NAMTREPH                                                      
         CLI   BMED,C'O'           OUTDOOR                                      
         BNE   CKTREP4                                                          
         CLI   5(R2),0                                                          
         BE    UPDATER                                                          
*                                                                               
CKTREP1  DS    0H                                                               
         BAS   RE,ANY                                                           
         CLI   5(R2),1             1 DIGIT MUST BE SUFFIX FOR OUTDOOR           
         BE    CKTREP2                                                          
         B     CKTREP4             ELSE EDIT LIKE NORMAL REP                    
*                                                                               
*                                                                               
CKTREP2  BAS   RE,EDTSUFX          EDIT SUFFIX REP                              
         MVC   PUBTRREP(3),=C'000'                                              
         MVC   PUBTRREP+3(1),NAMTREP                                            
         MVI   ESWITCH,1                                                        
         B     CKTREP6                                                          
CKTREP4  CLI   NAMTREPH+5,0                                                     
         BE    UPDATER                                                          
         BAS   RE,EDTREP                                                        
         MVI   ESWITCH,1                                                        
         MVC   PUBTRREP,NAMTREP                                                 
CKTREP6  FOUT  NAMTREPH                                                         
         FOUT  NAMTRNH,PREPNAME,30                                              
*                                                                               
         B     UPDATER                                                          
*                                                                               
*                                                                               
FINDREL  LA    R4,PUBREC+33                                                     
         CLI   0(R4),0                                                          
         BNE   CKREL1                                                           
         MVI   ASWITCH,1                                                        
         B     EDITR                                                            
CKREL1   CLI   0(R4),X'14'                                                      
         BNE   NEXTR                                                            
         CLC   2(3,R4),=X'FFFFFF'                                               
         BNE   NEXTR                                                            
         B     EDITR                                                            
*                                                                               
NEXTR    SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   CKREL1                                                           
         MVI   ASWITCH,1                                                        
         B     EDITR                                                            
*      ASWITCH=1 IF PUBREPEL DID NOT EXIST IN LTLREC SO USE RECUP               
*      TO ADD IT, IF NECESSARY, ALSO IT NEEDS TO BE DELETED IF                  
*      ESWITCH=0                                                                
*                                                                               
UPDATER  DS    0H                                                               
         CLI   ESWITCH,0                                                        
         BE    DELETER                                                          
         CLI   ASWITCH,1           SEE IF ELEMENT EXISTED                       
         BNE   EDITSAP                                                          
         B     UPDATE2                                                          
*                                 NO,NEED TO ADD IT                             
UPDATE1  CLI   ESWITCH,0                                                        
         BE    EDITSAP                                                          
UPDATE2  MVC   PUBREPEL(2),=X'1420'                                             
         MVC   PUBRPOFF(3),=X'FFFFFF'                                           
         GOTO1 VRECUP,DMCB,(1,PUBREC),PUBREPEL,PUBREPEL                         
         B     EDITSAP                                                          
*                                                                               
DELETER  CLI   ASWITCH,1           SEE IF ELEMENT EXISTED                       
         BE    EDITSAP             NO,THEN NO NEED TO DELETE IT                 
         GOTO1 VRECUP,DMCB,(1,PUBREC),PUBREPEL,0                                
*                                                                               
EDITSAP  CLI   SAPAGY,C'Y'                                                      
         BNE   EDITSAPX                                                         
*                                                                               
         LA    R2,NAMSAPH                                                       
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'91'                                                       
         MVI   ELEM+1,12                                                        
         MVC   ELEM+2(10),WORK                                                  
                                                                                
* REPLACE EXISTING SAPEL OR ADD NEW                                             
                                                                                
         LA    R4,PUBREC+33                                                     
         SR    R0,R0                                                            
*                                                                               
EDITSAP2 CLI   0(R4),0                                                          
         BE    EDITSAP6                                                         
         CLI   0(R4),X'91'                                                      
         BE    EDITSAP4                                                         
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     EDITSAP2                                                         
*                                                                               
EDITSAP4 GOTO1 VRECUP,DMCB,(1,PUBREC),(R4)                                      
*                                                                               
EDITSAP6 GOTO1 VRECUP,DMCB,(1,PUBREC),ELEM,(R4)                                 
*                                                                               
EDITSAPX DS    0H                                                               
         B     WRITEIT                                                          
         EJECT                                                                  
*                                                                               
FORMATP  DS    0H                                                               
         CLI   SAVSCRN,X'01'                                                    
         BNE   FMT2                                                             
         CLI   BACT,1                                                           
         BNE   FMT5                                                             
         MVI   FORMAT,0            TO GENERATE TURNAROUND                       
         B     NEWPUB                                                           
*                                                                               
FMT2     LA    R6,PBLLAST                                                       
         GOTO1 VCALLOV,WORK,(R6),X'D90406F1'                                    
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
                                                                                
*===========================================================                    
* GET THE ACCESS RECORD TO SEE IF SAP AGENCY                                    
*==============================================================                 
                                                                                
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING CT5REC,R4                                                        
*                                                                               
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGYALPHA                                                
         DROP  R4                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',WORK,MYIO                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   SAPAGY,C'N'                                                      
         LA    R4,MYIO                                                          
         LA    R4,CT5DATA-CT5REC(R4)                                            
         SR    R0,R0                                                            
*                                                                               
SETSAP2  CLI   0(R4),0                                                          
         JE    SETSAP10                                                         
         CLI   0(R4),X'B4'                                                      
         BE    SETSAP4                                                          
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     SETSAP2                                                          
*                                                                               
         USING CTAGDD,R4                                                        
*                                                                               
SETSAP4  TM    CTAGOPTS,CTAGSAP    TEST SAP AGY                                 
         JZ    SETSAP10                                                         
         MVI   SAPAGY,C'Y'                                                      
         J     SETSAPX                                                          
         DROP  R4                                                               
                                                                                
*=========================================================                      
* NOT AN SAP AGENCY - CLEAR TITLE AND PROTECT SAP FIELD                         
*=========================================================                      
                                                                                
SETSAP10 XC    NAMSAP,NAMSAP       SAP INPUT FIELD                              
         OI    NAMSAPH+6,X'80'                                                  
         OI    NAMSAPH+1,X'20'     SET TO PROTECTED                             
*                                                                               
         XC    NAMSTTL,NAMSTTL     SAP TITLE                                    
         OI    NAMSTTLH+6,X'80'                                                 
*                                                                               
SETSAPX  DS    0H                                                   L01         
         CLI   ANATION,C'C'      CANADIAN                          L01          
         BNE   NTCANADA                                             L01         
         FOUT  NAMSTCDH,=C'90',2                                    L01         
         FOUT  NAMCTCDH,=C'000',3                                   L01         
         B     FMT4                                                             
NTCANADA DS    0H                                                   L01         
*                                   NON-CANADIAN CLEAR GST/PST                  
         XC    NAMGSTA,NAMGSTA                                                  
         FOUT  NAMGSTAH                                                         
         OI    NAMGSTCH+1,X'20'     PROTECT GST FIELD                           
         XC    NAMPSTA,NAMPSTA                                                  
         FOUT  NAMPSTAH                                                         
         OI    NAMPSTH+1,X'20'     PROTECT PST FIELD                            
*                                                                               
FMT4     MVI   SAVSCRN,X'01'                                                    
FMT5     DS    0H                                                               
         EJECT                                                                  
*                                                                               
*     FIND PUBNAMEL AND FOUT FIELDS                                             
*                                                                               
PUTPFLDS LA    R4,PUBREC+33                                                     
CKNAME   CLI   0(R4),X'10'                                                      
         BNE   PUTLFLDS                                                         
         FOUT  NAMNAMEH,PUBNAME,20                                              
         FOUT  NAMZNAMH,PUBZNAME,20                                             
         FOUT  NAMADDRH,PUBLINE1,30                                             
         FOUT  NAMCITYH,PUBCITY,16                                              
         FOUT  NAMSTATH,PUBSTATE,2                                              
         FOUT  NAMZIPH,PUBNWZIP,10     CNG 01/06/88 PUBZIP TO PUBNWZIP          
         FOUT  NAMADL2H,PUBLINE2,30                                             
**NEW 5/2/89                                                                    
         FOUT  NAMPLSHH,PUBPLSH,4                                               
         XC    NAMPLN,NAMPLN           CLEAR NAME                               
         FOUT  NAMPLNH                                                          
         OC    PUBPLSH,PUBPLSH          SEE IF I HAVE ONE                       
         BZ    CKNAME5                                                          
         LA    R2,NAMPLSHH                                                      
         LA    R5,NAMPLNH                                                       
         BAS   RE,PUTRNAME                                                      
**NEW 5/2/89                                                                    
CKNAME5  FOUT  NAMSTCDH,PUBSTACD,2                                              
         FOUT  NAMCTCDH,PUBCNTCD,3                                              
         FOUT  NAMEDTSH,PUBEDTS,2                                               
         FOUT  NAMGSTCH,PUBGST,1                                  L03           
         BAS   RE,DISPPST                                                       
         FOUT  NAMLANGH,PUBLANG,1                                               
*                                                                               
         BRAS  RE,DISPPLOC         DISPLAY PUB LOCK SWITCH                      
*                                                                               
         OC    PUBKILL(3),PUBKILL                                               
         BNZ   DATE                                                             
         FOUT  NAMKDATH,SPACES,10                                               
         B     PUTSAP                                                           
*                                                                               
DATE     DS    0H                                                               
         MVC   NAMKDAT,SPACES                                                   
*                                                                               
         GOTO1 VDATCON,DMCB,(3,PUBKILL),(5,NAMKDAT)                             
         OI    NAMKDATH+6,X'80'                                                 
*                                                                               
PUTSAP   XC    NAMSAP,NAMSAP                                                    
         OI    NAMSAPH+6,X'80'                                                  
*                                                                               
         CLI   SAPAGY,C'Y'                                                      
         BNE   PUTSAPX                                                          
*                                                                               
         LA    R4,PUBREC+33                                                     
         SR    R0,R0                                                            
*                                                                               
PUTSAP2  CLI   0(R4),0             NO ELEMENT - SKIP                            
         BE    PUTSAPX                                                          
         CLI   0(R4),X'91'         ELEMENT FOUND                                
         BE    PUTSAP4                                                          
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     PUTSAP2                                                          
*                                                                               
PUTSAP4  MVC   NAMSAP,2(R4)                                                     
*                                                                               
PUTSAPX  DS    0H                                                               
*                                                                               
PUTEND   DS    0H                                                               
         B     PUTLFLDS                                                         
         EJECT                                                                  
*                                                                               
*    FIND  PUBSADEL AND FOUT FIELDS IF PRESENT                                  
*                                                                               
PUTLFLDS LA    R4,PUBREC+33                                                     
CKADDR   CLI   0(R4),X'11'                                                      
         BNE   NEXTAEL                                                          
         USING PUBSADEL,R4                                                      
         FOUT  NAMATTNH,PUBATTN,24                                              
         FOUT  NAMTELEH,PUBTEL,12                                               
         FOUT  NAMFAXH,PUBSFAXN,12                                              
         B     PUTWEB                                                           
*                                                                               
NEXTAEL  SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   CKADDR                                                           
         FOUT  NAMATTNH,SPACES,24      PUBSADEL NOT FOUND SO FORMAT             
         FOUT  NAMTELEH,SPACES,12                                               
         FOUT  NAMFAXH,SPACES,12                                                
         B     PUTWEB                                                           
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
*        FIND WEB ELEMENT AND FOUT FIELDS IF PRESENT                            
*                                                                               
PUTWEB   LA    R4,PUBREC+33                                                     
CHKWEB   CLI   0(R4),X'70'                                                      
         BNE   NEXTWEL                                                          
         USING PUBWEBD,R4                                                       
         ZIC   R5,1(R4)                                                         
         BCTR  R5,0                                                             
         BCTR  R5,0                                                             
         LTR   R5,R5             CHECK FOR ZERO LENGTH                          
         BZ    PUTREP                                                           
         XC    NAMWEBS,NAMWEBS    MUST CLEAR FIRST                              
         FOUT  NAMWEBSH,PUBWEBS,(R5)                                            
         B     PUTREP                                                           
*                                                                               
NEXTWEL  SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   CHKWEB                                                           
         LA    R5,L'NAMWEBS                                                     
         FOUT  NAMWEBSH,SPACES,(R5)                                             
         B     PUTREP                                                           
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*    FIND PUBREPEL IF PRESENT AND FOUT FIELDS                                   
*                                                                               
PUTREP   LA    R4,PUBREC+33                                                     
CKREPEL  CLI   0(R4),X'14'                                                      
         BNE   NEXTREL                                                          
         CLC   2(3,R4),=X'FFFFFF'                                               
         BNE   NEXTREL                                                          
         USING PUBREPEL,R4                                                      
         FOUT  NAMPREPH,SPACES,4                                                
         FOUT  NAMTREPH,SPACES,4                                                
         FOUT  NAMCREPH,SPACES,4                                                
         FOUT  NAMPRNH,SPACES,30      CLEAR NAMES                               
         FOUT  NAMTRNH,SPACES,30                                                
         FOUT  NAMCRNH,SPACES,30                                                
         LA    R2,NAMPREPH                                                      
         LA    R5,NAMPRNH                                                       
         OC    PUBPAREP,PUBPAREP                                                
         BZ    PUTREP1                                                          
         FOUT  NAMPREPH,PUBPAREP,4                                              
         BAS   RE,PUTRNAME                                                      
PUTREP1  LA    R2,NAMTREPH                                                      
         LA    R5,NAMTRNH                                                       
         OC    PUBTRREP,PUBTRREP                                                
         BZ    PUTREP2                                                          
         CLI   BMED,C'O'           OUTDOOR                                      
         BNE   PUTREP1C                                                         
         CLC   PUBTRREP(3),=3C'0'      SEE IF SUFFIX                            
         BNE   PUTREP1C                                                         
         FOUT  NAMTREPH,PUBTRREP+3,1                                            
         BAS   RE,PUTSUFX                                                       
         B     PUTREP2                                                          
*                                                                               
PUTREP1C DS    0H                                                               
         FOUT  NAMTREPH,PUBTRREP,4                                              
         BAS   RE,PUTRNAME                                                      
PUTREP2  LA    R2,NAMCREPH                                                      
         LA    R5,NAMCRNH                                                       
         OC    PUBCNREP,PUBCNREP                                                
         BZ    PROTECT                                                          
         FOUT  NAMCREPH,PUBCNREP,4                                              
         BAS   RE,PUTRNAME                                                      
         B     PROTECT                                                          
*                                                                               
NEXTREL  SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   CKREPEL                                                          
         FOUT  NAMPREPH,SPACES,4                                                
         FOUT  NAMTREPH,SPACES,4                                                
         FOUT  NAMCREPH,SPACES,4                                                
         FOUT  NAMPRNH,SPACES,30      CLEAR NAMES                               
         FOUT  NAMTRNH,SPACES,30                                                
         FOUT  NAMCRNH,SPACES,30                                                
         B     PROTECT                                                          
PROTECT  EQU   *                                                                
CKSRDS   DS    0H                                                               
         OC    PUBADDR,PUBADDR                                                  
         BZ    CKSRDS2                                                          
         CLC   PUBKAGY(2),AGYALPHA                                              
         BNE   PROTECTL                                                         
CKSRDS2  LA    R2,NAMNAMEH                                                      
         B     EXIT                                                             
*                                                                               
PROTECTL OI    NAMATTNH+1,X'20'                                                 
         OI    NAMTELEH+1,X'20'                                                 
         OI    NAMPREPH+1,X'20'                                                 
         OI    NAMTREPH+1,X'20'                                                 
         OI    NAMCREPH+1,X'20'                                                 
PROTECTP OI    NAMNAMEH+1,X'20'                                                 
         OI    NAMZNAMH+1,X'20'                                                 
         OI    NAMADDRH+1,X'20'                                                 
         OI    NAMCITYH+1,X'20'                                                 
         OI    NAMSTATH+1,X'20'                                                 
         OI    NAMZIPH+1,X'20'                                                  
         OI    NAMADL2H+1,X'20'                                                 
         OI    NAMSTCDH+1,X'20'                                                 
         OI    NAMGSTCH+1,X'20'                                   L03           
         OI    NAMCTCDH+1,X'20'                                                 
         OI    NAMEDTSH+1,X'20'                                                 
         OI    NAMKDATH+1,X'20'                                                 
         MVI   SAVSCRN,0                                                        
         CLI   BACT,2                                                           
         BH    DONE                                                             
         LA    R2,NAMNAMEH                                                      
         B     EXIT                                                             
*                                                                               
*                                                                               
EDTSUFX  ST    RE,SAVERE                                                        
         MVC   SAVEKEY(32),KEY                                                  
         XC    KEY,KEY                                                          
         MVC   KEY+4(4),PUBCNREP   SUFFIX TO CONTRACT REP                       
         MVC   KEY+8(1),8(R2)         SUFFIX FOR IOA TRAF REP                   
         B     EDTREP4             REST SAME AS EDTREP                          
*                                                                               
*                                                                               
EDTREP   ST    RE,SAVERE                                                        
***                                                                             
***      REP NAME SEARCHING                                                     
***                                                                             
*                                                                               
         ST    R2,FULL                                                          
         SR    R2,RA               GET DISPL OF KEY FIELD                       
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         MVC   DSPARM(DSPARML),SPACES                                           
         MVC   DSMEDCOD,BMED       SET MEDIA CODE                               
         DROP  R3                                                               
*                                                                               
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',(RA)),ACOMFACS,       X        
               ('DSPARML',WORK),(1,=CL8'REP'),0,RR=RELO                         
*                                                                               
         L     R2,FULL                                                          
****                                                                            
         MVC   SAVEKEY(32),KEY                                                  
         XC    KEY,KEY                                                          
         LA    R3,REPERR                                                        
         CLI   8(R2),C'A'         ALLOW 'A' AS FIRST CHARACTER                  
         BNE   EDTREP2                                                          
         CLI   5(R2),4                                                          
         BNE   ERROR                                                            
         MVC   KEY+4(4),8(R2)                                                   
         B     EDTREP4                                                          
*                                                                               
EDTREP2  DS    0H                                                               
         BAS   RE,ANY                                                           
         CLI   5(R2),4           MAX IS 4                                       
         BH    ERROR                                                            
         BAS   RE,PACK                                                          
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+4(4),DUB+5(3)                                                
EDTREP4  MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BMED                                                    
         MVI   KEY+3,X'11'         REP RECORD CODE                              
         BAS   RE,READ                                                          
         MVC   DMWORK1(96),DMWORK                                               
         BAS   RE,GETREC                                                        
         MVC   DMWORK(96),DMWORK1                                               
         MVC   8(4,R2),KEY+4                                                    
         CLI   KEY+8,0             SEE IF DOING SUFFIX                          
         BE    EDTREP6                                                          
         MVC   8(1,R2),KEY+8                                                    
         XC    9(3,R2),9(R2)                                                    
*                                                                               
EDTREP6  DS    0H                                                               
         L     RE,SAVERE                                                        
         MVC   KEY(32),SAVEKEY                                                  
         BR    RE                                                               
*                                                                               
EDTIOAR  DS    0H                  PAY AND CON REPS MUST MATCH                  
*                                  'BASE' PUB NUMBER                            
         PACK  DUB,8(5,R2)         PACK AN EXTRA BYTE                           
         CLC   BPUB+2(2),DUB+5                                                  
         BER   RE                  VALID RETURN                                 
         LA    R3,FLDINV           FLDINV                                       
         B     ERROR                                                            
*                                                                               
*                                                                               
PUTRNAME ST    RE,SAVERE                                                        
         MVC   SAVEKEY(32),KEY                                                  
         XC    KEY,KEY                                                          
         MVC   KEY+4(4),8(R2)                                                   
PUTRN4   MVC   KEY(2),PUBKAGY                                                   
         MVC   KEY+2(1),BMED                                                    
         MVI   KEY+3,X'11'                                                      
         BAS   RE,HIGH                                                          
         MVC   DMWORK1(96),DMWORK                                               
         CLC   KEYSAVE(25),KEY                                                  
         BNE   NOREP                                                            
         BAS   RE,GETREC                                                        
         FOUT  (R5),PREPNAME,30                                                 
         B     RETURN                                                           
*                                                                               
NOREP    XC    8(30,R5),8(R5)                                                   
         MVC   8(19,R5),=C'* REP NOT ON FILE *'                                 
RETURN   MVC   KEY(32),SAVEKEY                                                  
         MVC   DMWORK(96),DMWORK1                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PUTSUFX  DS    0H                  ROUTINE TO READ SUFFIX REP                   
         ST    RE,SAVERE           USE SAME SAVERE AS PUTREP                    
         MVC   SAVEKEY(32),KEY                                                  
         XC    KEY,KEY                                                          
         MVC   KEY+4(4),PUBCNREP                                                
         MVC   KEY+8(1),PUBTRREP+3                                              
         B     PUTRN4              REST SAME AS PUTREP                          
*                                                                               
*                                                                               
WRITEIT  CLI   BACT,1                                                           
         BE    ADDIT               PUBREC IN IOAREA WAS LAST RECORD             
         MVC   KEY+27(4),PUBADDR                                                
         CLC   PUBKAGY(2),AGYALPHA                                              
         BNE   WRITEL                                                           
         BAS   RE,PUTPUB           READ SO CAN GO TO PUTREC                     
         BRAS  RE,PRCZONES         PROCESS ZONES                                
         B     ADDPTS                                                           
*                                                                               
ADDIT    DS    0H                                                               
*                                                                               
         OC    PUBKZON(2),PUBKZON  ANY ZONE OR EDITION ?                        
         BZ    ADDITOK             NO - BASE PUB ONLY                           
*                                                                               
*      ZONE/EDITION ADD - TEST PROFILE FOR BASE PUB REQUIREMENT                 
*                                                                               
         XC    SVPROF,SVPROF       CLEAR PROFILE AREA                           
         XC    WORK,WORK           GET PB PROFILE                               
         MVC   WORK(4),=C'P0PB'                                                 
         MVC   WORK+4(2),PUBKAGY   AGENCY                                       
         MVC   WORK+6(1),PUBKMED   MEDIA                                        
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'C0',WORK),SVPROF,VDATAMGR                           
*                                                                               
         CLI   SVPROF,C'Y'         BASE PUB REQUIRED ?                          
         BNE   ADDITOK             NO                                           
*                                  LOOK FOR BASE PUB                            
         MVC   KEY(25),PUBKEY                                                   
         XC    KEY+5(2),KEY+5      CLEAR ZONE/EDITION                           
         BAS   RE,HIGHPUB                                                       
         CLC   KEYSAVE(25),KEY     BASE PUB FOUND ?                             
         BE    ADDITOK             YES - OKAY                                   
*                                  NO - CANNOT ADD                              
         LARL  RE,NOBASE                                                        
         MVC   PBLMSG,0(RE)                                                     
         FOUT  PBLMSGH                                                          
         LA    R2,PBLPUBH                                                       
         B     EXIT                                                             
*                                                                               
ADDITOK  DS    0H                                                               
*                                                                               
         MVC   KEY(25),PUBKEY                                                   
         BAS   RE,ADDPUB                                                        
         MVC   PUBADDR,KEY          SAVE ADDED ADDR                             
         NI    PBLPUBH+4,X'DF'      UNVAILDATE PUB TO FORCE RE-EDIT             
*                                   OTHERWISE SUCCESSIVE ADDS                   
*                                   WILL CAUSE DUMPS                            
         B     ADDPTS                                                           
*                                                                               
ADDPTS   DS    0H                                                               
         OC    PUBEDTS,PUBEDTS                                                  
         BZ    DELN          NO EDITION POINTERS  - GO CHECK PUBPLSH            
         LA    R4,PUBEDTS                                                       
*                                                                               
*                                                                               
ADDP1    CLI   0(R4),X'00'                                                      
         BE    DELN                                                             
         MVC   KEY(25),PUBKEY                                                   
         MVC   KEY+6(1),0(R4)          INSERT EDITION CODE                      
         OI    KEY+25,X'01'         SET CONTROL FOR PASSIVE POINTER             
         BAS   RE,HIGHPUB                                                       
         CLC   KEYSAVE(25),KEY                                                  
         BNE   ADDP2                                                            
         TM    KEY+25,X'01'         PASSIVE POINTER ALREADY THERE               
         BNZ   ADDP3                                                            
         LA    R3,52                 REAL PUB ALREADY ON FILE                   
         B     ERROR                                                            
*                                                                               
ADDP2    MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,ADDPUBD                                                       
*                                                                               
ADDP3    LA    R4,1(R4)                                                         
         B     ADDP1                                                            
*                                                                               
DELN     DS    0H                   ADD PUBLISHER POINTERS                      
         CLC   SAVPLSH,PUBPLSH      SEE IF PUBLISHER CHANGED                    
         BE    WRITEL                                                           
         OC    SAVPLSH,SAVPLSH       SEE IF I HAD A PUBLISHER                   
         BZ    ADDN                                                             
         XC    KEY,KEY                                                          
         MVI   KEY,X'F0'                                                        
         MVI   KEY+1,C'P'           FOR PUBLISHER                               
         MVC   KEY+2(2),PUBKAGY                                                 
         MVC   KEY+4(1),PUBKMED                                                 
         MVC   KEY+5(4),SAVPLSH                                                 
         MVC   KEY+10(6),PUBKPUB      PUB,ZONE,EDT                              
         OI    KEY+25,X'01'         SET CONTROL FOR PASSIVE POINTER             
         BAS   RE,HIGHPUB                                                       
         CLC   KEYSAVE(25),KEY                                                  
         BNE   DELN1                                                            
         OI    KEY+25,X'80'         DELETE OLD PUBLISHER                        
         BAS   RE,WRITEPUB                                                      
*                          NOW DO PUBLISHER POINTERS FOR EDITIONS               
DELN1    OC    PUBEDTS,PUBEDTS                                                  
         BZ    ADDN          NO EDITION POINTERS  - DONE                        
         LA    R4,PUBEDTS                                                       
*                                                                               
*                                                                               
DELN8    CLI   0(R4),X'00'                                                      
         BE    ADDN                                                             
         XC    KEY,KEY                                                          
         MVI   KEY,X'F0'                                                        
         MVI   KEY+1,C'P'                                                       
         MVC   KEY+2(2),PUBKAGY                                                 
         MVC   KEY+4(1),PUBKMED                                                 
         MVC   KEY+5(4),SAVPLSH                                                 
         MVC   KEY+10(6),PUBKPUB      PUB,ZONE,EDT                              
         MVC   KEY+15(1),0(R4)         EDITION                                  
         OI    KEY+25,X'01'         SET CONTROL FOR PASSIVE POINTER             
         BAS   RE,HIGHPUB                                                       
         CLC   KEYSAVE(25),KEY                                                  
         BNE   DELN9X                                                           
         OI    KEY+25,X'80'         DELETE OLD POINTER                          
         BAS   RE,WRITEPUB                                                      
*                                                                               
DELN9X   LA    R4,1(R4)                                                         
         B     DELN8                                                            
*                                                                               
ADDN     OC    PUBPLSH,PUBPLSH       CHECK FOR PUBLISHER                        
         BZ    WRITEL                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'F0'                                                        
         MVI   KEY+1,C'P'                                                       
         MVC   KEY+2(2),PUBKAGY                                                 
         MVC   KEY+4(1),PUBKMED                                                 
         MVC   KEY+5(4),PUBPLSH                                                 
         MVC   KEY+10(6),PUBKPUB      PUB,ZONE,EDT                              
         OI    KEY+25,X'01'         SET CONTROL FOR PASSIVE POINTER             
         BAS   RE,HIGHPUB                                                       
         CLC   KEYSAVE(25),KEY                                                  
         BNE   ADDN2                                                            
         B     ADDN6               ALREADY THERE SKIP TO ADDN6                  
*                                                                               
ADDN2    MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,ADDPUBD                                                       
*                                                                               
ADDN6    DS    0H                                                               
*                          NOW DO PUBLISHER POINTERS FOR EDITIONS               
         OC    PUBEDTS,PUBEDTS                                                  
         BZ    WRITEL        NO EDITION POINTERS  - DONE                        
         LA    R4,PUBEDTS                                                       
*                                                                               
*                                                                               
ADDN8    CLI   0(R4),X'00'                                                      
         BE    WRITEL                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'F0'                                                        
         MVI   KEY+1,C'P'                                                       
         MVC   KEY+2(2),PUBKAGY                                                 
         MVC   KEY+4(1),PUBKMED                                                 
         MVC   KEY+5(4),PUBPLSH                                                 
         MVC   KEY+10(6),PUBKPUB      PUB,ZONE,EDT                              
         MVC   KEY+15(1),0(R4)         EDITION                                  
         OI    KEY+25,X'01'         SET CONTROL FOR PASSIVE POINTER             
         BAS   RE,HIGHPUB                                                       
         CLC   KEYSAVE(25),KEY                                                  
         BNE   ADDN9                                                            
         B     ADDN9X                                                           
*                                                                               
ADDN9    MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,ADDPUBD                                                       
*                                                                               
*                                                                               
ADDN9X   LA    R4,1(R4)                                                         
         B     ADDN8                                                            
******** DROP  R6                                                               
*                                                                               
WRITEL   DS    0H                                                               
*                                                                               
*        NEED NAMESEARCH 'AFTER' OR 'NEW' CALL                                  
*                                                                               
         LA    R2,GENOLD                                                        
         AH    R2,=Y(APPLWRK-GENOLD)                                            
         USING APPLWRK,R2          ADDRESSABILITY FOR EXTENDED WORK             
         LA    RF,SRCHBLK                                                       
         USING SEARCHD,RF                                                       
         MVC   SBSYSTEM,=CL6'PRINT'                                             
         MVI   SBID,C' '                                                        
         MVC   SBID+1(L'SBID-1),SBID                                            
         DROP  RF                                                               
         MVI   BYTE,C'A'           'AFTER' FOR CHANGE                           
         CLI   BACT,1                                                           
         BNE   *+8                                                              
         MVI   BYTE,C'N'           'NEW' FOR AZDD                               
         GOTO1 =V(SRCHPASS),DMCB,(BYTE,SRCHBLK),ACOMFACS,PUBIO,        X        
               SRCHLST,PUBADDR,0,RR=RELO                                        
         DROP  R2                                                               
*                                                                               
         B     DONE                                                             
         EJECT                                                                  
*                                                                               
*        VALIDATE PST CODES                                                     
*                                                                               
VALPST   NTR1                                                                   
         LA    R4,PUBREC+33                                                     
*                                                                               
*        DELETE ANY EXISTING PST ELEMENT                                        
*                                                                               
VP10     CLI   0(R4),0             NO ELEMENT - SKIP DELETE                     
         BE    VP30                                                             
*                                                                               
         CLI   0(R4),X'90'         ELEMENT FOUND - DELETE                       
         BE    VP20                                                             
*                                                                               
         SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VP10                                                             
*                                                                               
VP20     GOTO1 VRECUP,DMCB,(1,PUBREC),0(R4),0                                   
*                                                                               
VP30     LA    R3,WORK                                                          
         USING PSTBLKD,R3          ESTABLISH PST CONTROL BLOCK                  
*                                                                               
         XC    WORK,WORK           CLEAR INTERFACE BLOCK                        
         MVI   PSTACT,PSTVALQ      ACTION = VALIDATE                            
         LA    R1,NAMPSTH                                                       
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    PSTOUT,PSTOUT                                                    
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,ACOMFACS    A(COMFACS)                                   
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A6B'  GET PST ADDRESS                          
*                                                                               
         GOTO1 VCALLOV,DMCB                                                     
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R3)                                                   
*                                                                               
         CLI   PSTERR,0            TEST FOR ERRORS                              
         BNE   VPNO                                                             
*                              TEST PSTOUT FOR MORE THAN ONE ENTRY              
         SR    R2,R2                                                            
         LA    R1,PSTOUT                                                        
         LA    RE,10               LOOP COUNTER                                 
*                                                                               
VPLUP    CLI   0(R1),0             ANY ENTRY ?                                  
         BE    *+8                 NO                                           
         LA    R2,1(R2)            YES - ADD TO ENTRY COUNT                     
*                                                                               
         LA    R1,1(R1)            BUMP TO NEXT BYTE                            
         BCT   RE,VPLUP                                                         
*                                                                               
         CH    R2,=H'1'            ONE ENTRY ?                                  
         BNE   VPNO                NO - ERROR                                   
*                                                                               
         CLC   =C'90',PUBSTACD     IF NOT CANADIAN PUB                          
         BE    VPLUP10                                                          
*                                                                               
         LARL  RE,NOPSTERR                                                      
         MVC   PBLMSG,0(RE)                                                     
         FOUT  PBLMSGH                                                          
*                                                                               
         MVI   ERRAREA,X'FF'                                                    
*                                                                               
         B     VPNO                   NO PST ALLOWED                            
*                                                                               
VPLUP10  DS    0H                                                               
*                                                                               
*        BUILD PST ELEMENT                                                      
*                                                                               
         LA    R2,PSTELEM                                                       
         XC    PSTELEM,PSTELEM                                                  
*                                                                               
         USING PUBPSTLD,R2                                                      
*                                                                               
         MVC   PUBPSTL(2),=X'900C'      PST ELEMENT                             
         MVC   PUBPSTC,PSTOUT                                                   
*                                                                               
         GOTO1 VRECUP,DMCB,(1,PUBREC),PSTELEM,0(R4)                             
*                                                                               
         BAS   RE,DISPPST                                                       
*                                                                               
VPYES    SR    R1,R1                                                            
*                                                                               
VPNO     LTR   R1,R1                                                            
         XIT1                                                                   
         DROP  R2                                                               
*                                                                 L03           
         EJECT                                                                  
*                                                                               
*        DISPLAY PST CODES                                                      
*                                                                               
DISPPST  NTR1                                                                   
         MVC   NAMPST,SPACES       OUTPUT                                       
         OI    NAMPSTH+6,X'80'                                                  
         LA    R4,PUBREC+33                                                     
*                                                                               
DP10     CLI   0(R4),0             NO ELEMENT - SKIP                            
         BE    DPX                                                              
         CLI   0(R4),X'90'         ELEMENT FOUND                                
         BE    DP20                                                             
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     DP10                                                             
*                                                                               
         USING PUBPSTLD,R4                                                      
DP20     OC    PUBPSTC,PUBPSTC     ANYTHING TO DISPLAY                          
         BZ    DPX                                                              
*                                                                               
         LA    R3,WORK                                                          
         USING PSTBLKD,R3                                                       
         XC    WORK,WORK           CLEAR INTERFACE BLOCK                        
         MVI   PSTACT,PSTFMTQ      ACTION = FORMAT                              
         LA    R1,PUBPSTC                                                       
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    PSTOUT,PSTOUT                                                    
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,ACOMFACS    A(COMFACS)                                   
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A6B'  GET PST ADDRESS                          
         GOTO1 VCALLOV,DMCB                                                     
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R3)                                                   
         MVC   NAMPST,PSTOUT       OUTPUT                                       
*                                                                               
DPX      XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
DONE     MVI   BYTE3,1                                                          
         B     EXXMOD                                                           
*                                                                               
STDERROR DS    0H                                                               
         LA    R3,STDERR                                                        
         LA    R2,PBLACTH                                                       
         B     ERROR                                                            
*                                                                               
STDERR   EQU   113                                                              
FLDINV   EQU   2                                                                
ZIPERR   EQU   121                                                              
REPERR   EQU   122                                                              
PNAMERR  EQU   120                                                              
SCDERR   EQU   127                                                              
CCDERR   EQU   128                                                              
ZLOCKERR EQU   339                 ZONE LOCKED, NO ADD/CHG TO PUB               
ZEXSTERR EQU   340                 ZONE EXIST, CANNOT LOCK ZONE                 
ZLCKPERR EQU   341                 ZONE LOCK ONLY AVAILABLE ON PUB LVL          
                                                                                
*                  INITIALISATION CODE                                          
                                                                                
INITL    LR    R4,RC               SET UP TO CLEAR WORK SPACE                   
         LR    R5,RD                                                            
         SR    R5,R4                                                            
         LR    R0,RE                                                            
         BAS   RE,CLEARWRK                                                      
         LM    R2,R4,0(R1)                                                      
         PACK  AGYNUM,0(1,R1)      AGENCY NUMBER                                
         LH    R5,0(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,FRSTFLD .        A(FIRST INPUT FIELD HEADER)                  
         LH    R5,2(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,LASTFLD .        A(LAST INPUT FIELD)                          
         MVC   NUMFLD,4(R2) .      NUMBER OF FIELDS                             
         ST    R3,VTWA .           A(TWA)                                       
         MVC   VDATAMGR(36),0(R4)  FACILITY LIST                                
         LR    RA,R3                                                            
         MVC   TERMNAL,0(RA) .     TERMINAL NUMBER                              
         MVC   AGYALPHA,14(RA)     ALPHA AGENCY CODE                            
         LA    R3,64(R3)                                                        
         ST    R3,ERRAREA          PRESET ERRAREA TO A(FIRST HEADER)            
         MVI   DMINBTS,X'C0'       PRESET DATAMGR CONTROL BITS                  
         MVI   DMOUTBTS,X'FD'      PRESET DATAMGR ERROR CHECK BITS              
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
CLEARWRK LTR   R5,R5               CLEAR STORAGE TO ZEROS                       
         BCR   8,RE                                                             
         CH    R5,=H'250'                                                       
         BNH   CLEAREST                                                         
         XC    0(250,R4),0(R4)                                                  
         LA    R4,250(R4)                                                       
         SH    R5,=H'250'                                                       
         B     CLEARWRK                                                         
                                                                                
CLEAREST BCTR  R5,R0                                                            
         EX    R5,VARCLEAR                                                      
         BR    RE                                                               
                                                                                
VARCLEAR XC    0(0,R4),0(R4)                                                    
         EJECT                                                                  
*                  FARMABLE CODE                                                
                                                                                
ANY      CLI   5(R2),0                                                          
         BNE   ANY2                                                             
         LA    R3,1                                                             
         B     ERROR                                                            
                                                                                
ANY2     TM    4(R2),X'10' .       IS IT VALID NUMERIC                          
         BCR   8,RE .              IF APPLICABLE                                
         LA    R3,3                                                             
         B     ERROR                                                            
                                                                                
PACK     SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         TM    4(R2),X'08'                                                      
         BCR   8,RE                        OR NON NUMERIC                       
         BCTR  R1,R0                                                            
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
         BR    RE                                                               
                                                                                
VARPACK  PACK  DUB,8(0,R2)                                                      
                                                                                
MOVE     MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         BCTR  R1,R0                                                            
         EX    R1,VARMOVE                                                       
         BR    RE                                                               
                                                                                
VARMOVE  MVC   WORK(0),8(R2)                                                    
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (DIRECTORY)                  
                                                                                
READ     MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
                                                                                
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
                                                                                
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
                                                                                
ADD      MVC   COMMAND,=C'DMADD '                                               
         B     DIRCTRY                                                          
                                                                                
WRITE    MVC   COMMAND,=C'DMWRT '                                               
         B     DIRCTRY                                                          
                                                                                
DIRCTRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (FILE)                       
                                                                                
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
                                                                                
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
                                                                                
ADDREC   MVC   COMMAND,=C'ADDREC'                                               
         B     FILE                                                             
                                                                                
FILE     NTR                                                                    
         LA    R2,KEY+27                                                        
         CLC   COMMAND(5),=C'DMDEL'                                             
         BE    *+12                                                             
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE',            X        
               (R2),IOAREA,(TERMNAL,DMWORK)                                     
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (PUBDIR)                     
                                                                                
READPUB  MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     PUBDIRY                                                          
                                                                                
SEQPUB   MVC   COMMAND,=C'DMRSEQ'                                               
         B     PUBDIRY                                                          
                                                                                
HIGHPUB  MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     PUBDIRY                                                          
                                                                                
WRITEPUB MVC   COMMAND,=C'DMWRT'                                                
         B     PUBDIRY                                                          
                                                                                
ADDPUBD  MVC   COMMAND,=C'DMADD '                                               
         B     PUBDIRY                                                          
                                                                                
PUBDIRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (PUBFILE)                    
                                                                                
GETPUB   MVC   COMMAND,=C'GETREC'                                               
         B     PUBFILE                                                          
                                                                                
PUTPUB   MVC   COMMAND,=C'PUTREC'                                               
         B     PUBFILE                                                          
                                                                                
ADDPUB   MVC   COMMAND,=C'ADDREC'                                               
         B     PUBFILE                                                          
                                                                                
PUBFILE  NTR                                                                    
         LA    R2,KEY+27                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBFILE',            X        
               (R2),PUBIO,(TERMNAL,DMWORK)                                      
         B     DMCHECK                                                          
         EJECT                                                                  
*                  DATA MANAGER ERRORS AND EXIT                                 
                                                                                
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         XIT                                                                    
                                                                                
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3 .             LET GETMSG SORT IT OUT                       
         B     ERROR                                                            
         EJECT                                                                  
*                  EXITS FROM PROGRAM                                           
                                                                                
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
                                                                                
EXIT     OI    6(R2),OI1C .        INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
EXXMOD   XIT1                                                                   
         EJECT                                                                  
SPACES   DC    80C' '                                                           
TABLE    DC    XL256'0'                                                         
         ORG   TABLE+C'-'                                                       
         DC    C'-'                                                             
         ORG   TABLE+C'0'                                                       
         DC    C'NNNNNNNNNN'                                                    
*                                                                               
NOPSTERR DC    CL60'NON-CANADIAN PUBS CANNOT HAVE ANY PST CODE.'                
PSTNOTX  DC    CL60'NON-CANADIAN PUBS MUST HAVE GST=X.'                         
NOBASE   DC    CL60'**** CANNOT ADD ZONE/EDTN PUB UNTIL BASE PUB ADDED'         
*                                                                               
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VALZLOCK NTR1  BASE=*,LABEL=*,WORK=(R4,LOCALWSL)                                
*                                                                               
         USING LOCALWSD,R4                                                      
*                                                                               
         MVC   SAVEKEY,KEY         SAVE ORIGINAL KEY                            
         MVI   BYTE4,0             RESTORE IO SEQUENCE SWITCH                   
         LA    R2,NAMZLOCH                                                      
         SR    R3,R3               INIT ERROR NUMBER                            
         CLI   BPUB+4,0            HAVE ZONE?                                   
         JE    VALZL20                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+00(01),BMED                                                  
         MVC   KEY+01(06),BPUB                                                  
         MVC   KEY+07(02),AGYALPHA                                              
         MVI   KEY+09,X'81'                                                     
         XC    KEY+5(2),KEY+5      SET TO BASE PUB                              
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         MVC   COMMAND,=C'DMRDHI'                                               
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBDIR',             +        
               KEY,KEY,(TERMNAL,0)                                              
*                                                                               
         CLC   KEY(10),KEYSAVE     FOUND BASE PUB?                              
         JNE   VALZL10                                                          
         MVC   COMMAND,=C'GETREC'                                               
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBFILE',            +        
               KEY+27,LOCALAIO,(TERMNAL,DMWORK)                                 
         MVC   KEY,SAVEKEY         RESTORE KEY                                  
         MVI   BYTE4,C'Y'          NEED TO RESTORE IO SEQUENCE                  
         LA    RE,LOCALAIO                                                      
         USING PUBREC,RE                                                        
         TM    PUBLOCSW,PUBZLCKQ   ZONE IS LOCKED?                              
         JZ    VALZL20                                                          
         LHI   R3,ZLOCKERR                                                      
         J     VALZL_EX                                                         
         DROP  RE                                                               
*                                                                               
VALZL10  CLI   5(R2),0             ANY INPUT?                                   
         JNE   VALZL_E1            NO BASE PUB - CANNOT HAVE ZONE LOCK          
         J     VALZL_X                                                          
*                                                                               
VALZL20  CLI   5(R2),0             ANY INPUT?                                   
         JE    VALZL_X                                                          
         CLI   BACT,1              ADD?                                         
         JNE   VALZL50                                                          
         NI    PUBLOCSW,X'FF'-PUBZLCKQ                                          
         CLI   8(R2),C'N'                                                       
         JE    VALZL_X                                                          
         CLI   8(R2),C'Y'                                                       
         JNE   VALZL_E1                                                         
         CLI   BPUB+4,0            HAVE ZONE?                                   
         JNE   VALZL_E5                                                         
         OI    PUBLOCSW,PUBZLCKQ                                                
         J     VALZL_X                                                          
*                                                                               
VALZL50  CLI   BACT,2              CHANGE?                                      
         JNE   VALZL_E1                                                         
         CLI   8(R2),C'N'                                                       
         JE    *+12                                                             
         CLI   8(R2),C'Y'                                                       
         JNE   VALZL_E1                                                         
         TM    PUBLOCSW,PUBZLCKQ   ZONE LOCKED?                                 
         JZ    VALZL54                                                          
         CLI   8(R2),C'Y'                                                       
         JE    VALZL_X                                                          
         J     VALZL56                                                          
VALZL54  CLI   8(R2),C'N'                                                       
         JE    VALZL_X                                                          
VALZL56  MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY+00(01),BMED                                                  
         MVC   KEY+01(06),BPUB                                                  
         MVC   KEY+07(02),AGYALPHA                                              
         MVI   KEY+09,X'81'                                                     
         MVI   KEY+5,1             SET TO FIRST ZONE                            
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         MVC   COMMAND,=C'DMRDHI'                                               
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBDIR',             +        
               KEY,KEY,(TERMNAL,0)                                              
*                                                                               
         CLC   KEY(10),KEYSAVE     FOUND ZONE?                                  
         JE    VALZL_E6                                                         
         MVC   KEY,SAVEKEY         RESTORE KEY                                  
         NI    PUBLOCSW,X'FF'-PUBZLCKQ                                          
         CLI   8(R2),C'N'                                                       
         JE    VALZL_X                                                          
         CLI   8(R2),C'Y'                                                       
         JNE   VALZL_E1                                                         
         OI    PUBLOCSW,PUBZLCKQ                                                
         J     VALZL_X                                                          
*                                                                               
VALZL_X  MVC   KEY,SAVEKEY         RESTORE ORIGINAL KEY & IO SEQUENCE           
         CLI   BYTE4,C'Y'          NEED TO RESTORE IO SEQUENCE?                 
         JNE   VALZL_XX                                                         
         CLI   BACT,2              CHANGE?                                      
         JNE   VALZL_XX                                                         
         XC    KEY,KEY             REBUILD PUB KEY AND RESTORE RECORD           
         MVC   KEY+00(01),BMED                                                  
         MVC   KEY+01(06),BPUB                                                  
         MVC   KEY+07(02),AGYALPHA                                              
         MVI   KEY+09,X'81'                                                     
         MVC   COMMAND,=C'DMRDHI'                                               
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBDIR',             +        
               KEY,KEY,(TERMNAL,0)                                              
         MVC   COMMAND,=C'GETREC'                                               
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBFILE',            +        
               KEY+27,LOCALAIO,(TERMNAL,DMWORK)                                 
         CLC   KEY+27(4),PUBADDR                                                
         JE    *+6                                                              
         DC    H'0'                PUB RECORD IS NOT RESTORED                   
VALZL_XX J     SETCCEQ                                                          
*                                                                               
VALZL_E1 LHI   R3,FLDINV           FIELD INVALID                                
         J     VALZL_EX                                                         
*                                                                               
VALZL_E5 LHI   R3,ZLCKPERR         LOCK ONLY AVAIL AT PUB LEVEL                 
         J     VALZL_EX                                                         
*                                                                               
VALZL_E6 LHI   R3,ZEXSTERR                                                      
*                                                                               
VALZL_EX BRAS  RE,GET_ETXT                                                      
         LTR   RB,RB               NOT EQUAL                                    
XIT_R2R3 XIT1  REGS=(R2,R3)        R2 AND R3 => FIELD AND ERROR NUMBER          
         LTORG                                                                  
         DROP  R4                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRCZONES NTR1  BASE=*,LABEL=*,WORK=(R4,LOCALWSL)                                
*                                                                               
         USING LOCALWSD,R4                                                      
*                                                                               
         TM    PUBLOCSW,PUBZLCKQ   ZONE IS LOCKED?                              
         JZ    PRCZON_X                                                         
         CLI   BACT,1              ADD?                                         
         JE    PRCZON_X                                                         
         CLI   BACT,2              CHANGE?                                      
         JNE   PRCZON_X                                                         
         CLI   BPUB+4,0            HAVE ZONE? MUST BE BASE PUB!                 
         JE    *+6                                                              
         DC    H'0'                                                             
         CLI   PUBIO+5,0           HAVE ZONE? MUST BE BASE PUB!                 
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SAVEKEY,KEY         SAVE ORIGINAL KEY                            
         SR    R3,R3               ZONE NUMBER TO UPDATE                        
*                                                                               
PRCZON24 AHI   R3,1                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(25),PUBIO                                                    
         STC   R3,KEY+5            SET TO FIRST ZONE                            
         MVC   KEYSAVE,KEY                                                      
         MVC   COMMAND,=C'DMRDHI'                                               
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBDIR',             +        
               KEY,KEY,(TERMNAL,0)                                              
*                                                                               
         CLC   KEY+0(5),KEYSAVE+0  STILL SAME MEDIA AND BASE PUB?               
         JNE   PRCZON64                                                         
         CLC   KEY+7(2),KEYSAVE+7  STILL SAME AGENCY?                           
         JNE   PRCZON64                                                         
         CLC   KEY+9(1),KEYSAVE+9  STILL SAME RECORD CODE X'81'?                
         JNE   PRCZON64                                                         
*                                                                               
         MVC   COMMAND,=C'GETREC'                                               
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBFILE',            +        
               KEY+27,LOCALAIO,(TERMNAL,DMWORK)                                 
         STC   R3,PUBIO+5                                                       
         MVC   COMMAND,=C'PUTREC'                                               
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBFILE',            +        
               KEY+27,PUBIO,(TERMNAL,DMWORK)                                    
*                                                                               
         J     PRCZON24            PROCESS NEXT ZONE                            
*                                                                               
PRCZON64 MVI   PUBIO+5,0           RESET TO BASE PUB                            
         MVC   KEY,SAVEKEY         RESTORE ORIGINAL KEY                         
*                                                                               
PRCZON_X XIT1                                                                   
         LTORG                                                                  
         DROP  R4                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DISPPLOC NTR1  BASE=*,LABEL=*      DISPLAY PUB LOCK SWITCH                      
*                                                                               
         MVI   NAMLOCK,C' '                                                     
         TM    PUBLOCSW,PUBLCKDQ   "LOCKED" ?                                   
         JZ    *+8                 YES                                          
         MVI   NAMLOCK,C'Y'                                                     
         FOUT  NAMLOCKH                                                         
*                                                                               
         NI    NAMZLOCH+1,X'FF'-X'20'                                           
         MVI   NAMZLOC,C' '                                                     
         TM    PUBLOCSW,PUBZLCKQ   ZONE LOCKED?                                 
         JZ    *+8                 YES                                          
         MVI   NAMZLOC,C'Y'                                                     
         CLI   BPUB+4,0            HAVE ZONE?                                   
         JE    *+8                                                              
         OI    NAMZLOCH+1,X'20'    PROTECT FIELD                                
         FOUT  NAMZLOCH                                                         
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
GET_ETXT LR    R0,RE               SAVE RETURN ADDRESS                          
         XC    PBLMSG,PBLMSG                                                    
         L     RF,ACOMFACS                                                      
         L     RF,(CGETTXT-COMFACSD)(RF)                                        
         GOTOR (RF),DMCB+12,(R3),0,(C'E',DMCB),0,0,0                            
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         ORG                                                                    
*** PPPUBWRK                                                                    
       ++INCLUDE PPPUBWRK                                                       
         ORG   PBLLAST                                                          
*PPPUBF1D                                                                       
       ++INCLUDE PPPUBF1D                                                       
*=========================================================                      
* ADDITIONAL TEMPSTR FOLLOWS!                                                   
*=========================================================                      
                                                                                
         ORG   NAMWORK                                                          
SAPAGY   DS    C                                                                
*                                                                               
GENOLD   DSECT                                                                  
         ORG   APPLWRK                                                          
MYIO     DS    XL2000                                                           
*                                                                               
         ORG   APPLWRK                                                          
DMWORK1  DS    12D                                                              
SAVEKEY  DS    CL32                                                             
PSTELEM  DS    CL12                                                             
ESWITCH  DS    CL1                                                              
ASWITCH  DS    CL1                                                              
SAVERE   DS    F                                                                
SAVPLSH  DS    CL4                                                              
RELO     DS    A                                                                
*                                                                               
         DS    0D                                                               
ELEM     DS    CL80                                                             
WEBELEM  EQU   ELEM                                                             
SADELEM  EQU   ELEM                                                             
PSTOUT   DS    CL64                                                             
SVPROF   DS    CL16                                                             
*                                                                               
SCANBLK  DS    3CL32                                                            
SRCHBLK  DS    XL512                                                            
SRCHLST  DS    CL(25*2*8+8+1)   SRCHPASS LIST AREA                              
         EJECT                                                                  
*                                                                               
LOCALWSD DSECT                                                                  
*                                                                               
         DS    0D                                                               
LOCALAIO DS    4000C               MQ MESSAGE BUILD AREA                        
LOCALWSX EQU   *                                                                
LOCALWSL EQU   LOCALWSX-LOCALWSD                                                
*                                                                               
LTLRECD  DSECT                                                                  
       ++INCLUDE LTLREC                                                         
         EJECT                                                                  
*                                                                               
LTLADELD DSECT                                                                  
       ++INCLUDE PUBSADEL                                                       
         EJECT                                                                  
*                                                                               
PUBWEBD  DSECT                                                                  
       ++INCLUDE PUBWEBEL                                                       
         EJECT                                                                  
*                                                                               
PUBPSTLD DSECT                                                                  
       ++INCLUDE PUBPSTEL                                                       
         EJECT                                                                  
*                                                                               
LTLREPD  DSECT                                                                  
       ++INCLUDE PUBREPEL                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDPSTBLK                                                       
         EJECT                                                                  
*                                                                               
*INCLUDE GESRCHBLKD                                                             
         PRINT OFF                                                              
       ++INCLUDE GESRCHBLKD                                                     
         PRINT ON                                                               
*                                                                               
*INCLUDE PPSRCHPARM                                                             
*INCLUDE DDCOMFACS                                                              
         PRINT OFF                                                              
       ++INCLUDE PPSRCHPARM                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'195PPPUB01   05/16/16'                                      
         END                                                                    
