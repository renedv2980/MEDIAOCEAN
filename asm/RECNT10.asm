*          DATA SET RECNT10    AT LEVEL 074 AS OF 01/12/17                      
*PHASE T80210A                                                                  
         TITLE 'RECNT10 (T80210) - CONTRACT HEADLINE EDIT PROGRAM'              
*                                                                               
*********************************************************************           
*                                                                   *           
*  RECNT10 (T80210) - CONTRACT PROGRAM HEADLINE VALIDATION          *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* REFER TO RECNTHIST FOR PAST HISTORY                               *           
*                                                                   *           
* 10JAN16 SKU SPEC-9097 ALLOW PROPOSER TO USE CONTRACT EVEN                     
*             IF CONTRACT=NO AND PROFILE#63 IS ON                               
* 15MAR13 KWA CHECK CONTRACT PROFILE FOR SALESPERSON CHANGE         *           
* 26DEC11 SKU ALSO CHECK LEAP YEAR IN A 53 WEEK BROADCAST CALENDAR  *           
* 08FEB11 SKU ZERO OUTSIDE BUCKETS IF FLIGHT IS SHORTENED           *           
* 04AUG10 SKU SCRIPT UPLOAD SUPPORT                                 *           
* 22MAY08 SKU PROFILE#60, PROPOSER CONTRACT TYPE Z UNIVISION SPECIAL*           
* 13APR03 HQ  VALIDATE COMBO STATION SELECTION,BOUNDRY CHECK        *           
* ??AUG03 MN  DIS/ADD AGENCY EQUIVALENCY RECORDS                    *           
* 25FEB03 SKU FIX ACE/EASYLINK BIT SET BUG                          *           
* 23DEC02 BU  BYPASS OFFICE/TEAM CHECK FOR MILLENNIUM (SZ)          *           
* 18SEP02 BU  ADD PASSIVE 'BF' FOR CODESWITCH                       *           
* 01FEB02 RHV CHECK STATION=NO LOCKOUT FOR HEADER CHANGE ALSO       *           
* 04DEC01 BU  ADD PAY S/P CODE IN 1E RANDOM ELEMENT                 *           
* 24OCT01 BU  ADD 'DO NOT USE' FOR AGY/ADV CODES                    *           
* 12APR01 RHV FIX PASSIV X'AB01' KEY RESTORE BUG                    *           
* 07FEB01 RHV PRODUCT CODE VS. CONTYPE VALIDATION                   *           
* 16JAN01 RHV AUTOGEN CHA FIXES                                     *           
* 15JUN00 BU  ADD/TRADE ACTION FIELD TREATMENT                      *           
* 02MAR00 SCH NEW BROWSE FOR PRODUCT FIELD                          *           
* 04NOV99 SKU SPECIAL CODE FOR AM->SZ TAKEOVER DARE CONTRACTS       *           
*             TO ALLOW UPDATE OF FLIGHT DATES W/O UPPING VERSION    *           
* 29OCT99 SKU SET CORPORATE AGENCY CONTRACT ADDED FLAG              *           
* 27MAY99 RHV NEW SONNET 9F KEY                                     *           
* 28APR99 RHV USE REVISED FLIGHT DATE FIELD                          *          
* 26APR99 JRD NTVSTRN TEMP CATEGORY CODE HARD-CODING                 *          
* 12FEB99 RHV NEW 20 ELEM LENGTH                                    *           
* 24NOV98 BU  MAKEGOOD PASSIVE KEY MAINTENANCE                      *           
*         SKU NTVSNY TEMP CATEGORY CODE HARD-CODING                 *           
* 17NOV98 SKU FIX FOX ADV EDIT BUG                                  *           
* 22OCT98 JRD SELWIN/PROPOSER CHECK USING RANDOM FLAG ELEMENT       *           
* 19OCT98 RHV ADD X'1E' RANDOM FLAG ELEMENT                         *           
* 14OCT98 RHV SALESMAN TEAM OVERRIDE                                *           
* 22JUN98 JRD ADD CHECK FOR SELWIN RECORD TO STATION CHANGE         *           
* 30APR98 SKU FIX CATEGORY CODE PROCESSING WHEN CALLED BY SEL       *           
* 24APR98 SKU ALLOW USE OF CONTRACT IF BUYER=ACC- AND CONTRACT=NO   *           
* 06APR98 SKU UPDATE MAKEGOOD OFFER RECORDS IF SALESPERSON CHANGES  *           
* 20MAR98 BU  CORRECT COMBO STATION $.00 INV FOR ALT CAL STATIONS   *           
* 17MAR98 SKU TEMP VERSION TRAP POINTER BUG                         *           
* 17MAR98 BU  $.00 INV FOR ALTERNATE CALENDAR STATIONS              *           
* 02MAR98 AST 'AB01' P-KEY FOR CONTRACT RECORDS ADDED               *           
* 12JAN98 BU  SPECIAL CODE TO PERMIT PRODUCT CODE CHANGE            *           
* 30DEC97 RHV CMT=NNNNNNNN COMMENT IMPORTING                        *           
* 16DEC97 JRD CHECK FLIGHT AGAINST ALTERNATE CALENDAR               *           
* 13OCT97 SKU READ FOR DELETED IN PROPOSAL CHECK                    *           
* 05SEP97 SKU AUTOHEADER DO NOT SKIP TYPE VALIDATION ANYMORE        *           
* 26AUG97 RHV USE BROWSE INTERFACE MODULE                           *           
* 24JUL97 SKU 4K CONTRACT SUPPORT                                   *           
* 10JUL97 SKU FIX CATEGORY CODE AUTOGEN BUG                         *           
* 19JUN97 SKU NEED TO UPDATE MAKEGOODS IF SALESPERSON OFFICE CHANGE *           
* 18JUN97 SKU 'REMOVED' DARE CHECK                                  *           
* 29APR97 BU  OPTIONAL RTS CONTRACT TYPE                            *           
* 07APR97 BU  FOR PETRY (PV), UNLOCK  CONVERSION AGY/ADVS           *           
* 06APR97 BU  FOR PETRY (PV), LOCK OUT ACCESS TO 5 SWITCHED STAS    *           
* 17MAR97 BU  FOR FOX NETWORK (FN), LOCK OUT CONVERSION AGY/ADVS    *           
* 20FEB97 DBU ADD NEW VALIDATIONS FOR AGENCY/PRODUCT/FLIGHT DATES   *           
* 22JAN97 BU  TURN ON  OFF/TEAM TEST FOR KATZ AGAIN                 *           
* 21JAN97 BU  TURN OFF OFF/TEAM TEST FOR KATZ                       *           
* 06JAN97 DBU STATIONS EXCLUSION                                    *           
* 14NOV96 SKU SKIP EI FIELDS IF AUTOHEADER                          *           
* 08NOV96 RHV THE ADVENT OF RECORD BROWSING                         *           
* 30SEP96 SKU USE BUCKET MONTH INSTEAD OF K START MONTH FOR ACC-BB  *           
*             SUPPORT LOW POWER STATION                             *           
* 24SEP96 SKU SKIP STA JOIN DATE CHECK FOR ACC-BB CONTRACTS         *           
* 15AUG96 SKU STORE COMMENT NOT REQUIRED FOR AUTOHEADER             *           
* 31JUL96 SKU DON'T SKIP ADV VALIDATION IF LINKED TO DARE           *           
* 25JUL96 SKU REMOVE PETRY PRODUCT CODE TEST OF 4/3/96              *           
* 19JUL96 JRD PROPOSAL SECOND PASSIVE KEY UPDATE                    *           
* 21JUN96 JRD PROPOSAL RECORDS SWITCH ELEMENT & PASSIVE KEY UPDATE  *           
* 03JUN96 BU  TURN OFF OFF/TEAM TEST FOR KATZ, TEMPORARY            *           
* 28MAY96 SKU SUPPORT FOR EXPANDED ESTIMATE NUMBER                  *           
* 18APR96 SKU FOR BUYER ACC-BB, SET X'03' BUCKET ACTIVITY DATE TO   *           
*             PREVIOUS QUARTER                                      *           
* 17APR96 SKU ALLOW PETRY TO DELETE CONTRACTS BY IGNORING SENT DATE *           
* 03APR96 SKU ALLOW PETRY TO CHANGE PRODUCT CODE IF BUYS EXIST      *           
* 29MAR96 BU  TURN ON  OFF/TEAM TEST FOR SELTEL                     *           
* 07MAR96 SKU NEW PENDING SUPPORT                                   *           
* 05MAR96 RHV ALLOW INCOMPLETE ENTRY OF IE FIELDS                   *           
* 20FEB96 BU  TURN OFF OFF/TEAM TEST FOR SELTEL, TEMPORARY          *           
* 31JAN96 SKU KATZ EDI SUPPORT                                      *           
*             FOR AUTOHEADER GENERATION, IF RUNNING IN              *           
*             BACKGROUND, SKIP PROFILE REQUIRED FIELDS              *           
* 02JAN96 SKU GENERATE NEW KEYS FOR NEW RIS                         *           
* 13DEC95 SKU 2K CONTRACT SUPPORT                                   *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*                                                                               
T80210   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80210,R9                                                      
         L     RC,0(R1)            WORK AREA                                    
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
*                                                                               
AUTOD    USING RCAUTOD,TWAGENBK                                                 
*                                                                               
         MVI   TWAACTMD,0          INITIALIZE ACTION MODE                       
         CLC   =C'CHA',CONACT                                                   
         JNE   *+8                                                              
         MVI   TWAACTMD,CONCHG_Q                                                
         CLC   =C'SCHA',CONACT                                                  
         JNE   *+8                                                              
         MVI   TWAACTMD,CONCHG_Q                                                
*                                                                               
         CLC   CONACT(3),=C'ADD'                                                
         BE    CONADD                                                           
* GET CONTRACT REC                                                              
         MVC   KEY+28(4),TWAKADDR                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,AIO4                                                
         GOTO1 VMOVEREC,DMCB,AIO4,RCONREC                                       
         GOTO1 VMOVEREC,DMCB,RCONREC,AIO2                                       
         SPACE 1                                                                
         LA    R2,CONCACTH                                                      
         LA    R3,ACTERR                                                        
         L     R4,AIO4                                                          
         TM    29(R4),X'01'        COMPRESSED CONTRACT                          
         BO    ERROR                                                            
         EJECT                                                                  
MAIN0010 EQU   *                                                                
PETRYFOX EQU   693                 ERROR MESSAGE NUMBER                         
*&&DO                                                                           
*                                                                               
*   PETRY SPECIAL TEST IS BEING SKIPPED                                         
*                                                                               
         B     MAIN0012                                                         
*                                                                               
         CLC   RCONKREP,=C'PV'     PETRY SPECIAL TEST?                          
         BNE   MAIN0012            NO                                           
         CLC   RCONKSTA(4),=C'WFXT'   SWITCHED STATION?                         
         BE    MAIN0011                                                         
         CLC   RCONKSTA(4),=C'WNYW'   SWITCHED STATION?                         
         BE    MAIN0011                                                         
         CLC   RCONKSTA(4),=C'KTTV'   SWITCHED STATION?                         
         BE    MAIN0011                                                         
         CLC   RCONKSTA(4),=C'WFLD'   SWITCHED STATION?                         
         BE    MAIN0011                                                         
         CLC   RCONKSTA(4),=C'WTXF'   SWITCHED STATION?                         
         BNE   MAIN0012                                                         
MAIN0011 EQU   *                                                                
         LA    R2,CONCACTH                                                      
         LA    R3,PETRYFOX                                                      
         B     ERROR                                                            
*&&                                                                             
MAIN0012 EQU   *                                                                
         MVI   BYTE3,0                                                          
* CHECK IF ANY BUCKETS                                                          
         SR    R4,R4                                                            
         LA    R3,RCONELEM         FIRST ELEM                                   
H5       CLI   0(R3),0             LAST?                                        
         BE    H10                                                              
         CLI   0(R3),3             ORD BUCKET?                                  
         BE    H7                                                               
         CLI   0(R3),4             INV BUCKET?                                  
         BNE   H6                                                               
         OC    6(4,R3),6(R3)       ZERO DOLLARS IN 04  IGNORE                   
         BNZ   H7                                                               
H6       IC    R4,1(R3)            NEXT ELEM                                    
         AR    R3,R4                                                            
         B     H5                                                               
H7       MVI   BYTE3,1                                                          
H10      CLC   CONACT(3),=C'DEL'                                                
         BNE   H11                                                              
         GOTO1 =A(DELETE),RR=Y                                                  
         B     EXXMOD                                                           
H11      CLI   TWAACTMD,CONCHG_Q   CHANGE?                                      
         BE    CHGCON                                                           
         CLC   =C'R#',CONACT                                                    
         BE    CHGCON                                                           
         CLC   CONACT(3),=C'RES'                                                
         BE    RESCON                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
RESCON   LA    R2,CONCACTH                                                      
         FOUT  CONCACTH,=C'RES',3                                               
         SPACE 1                                                                
         LA    R3,ACTERR           INVALID CONTRACT ACTION                      
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET CAN'T RESET MOD NUM.            
         BNZ   ERROR                                                            
         SPACE 1                                                                
         LA    R3,RESERR           RESET ERROR                                  
         CLI   RCONMOD,250                                                      
         BH    ERROR                                                            
         LA    R3,RE2ERR           NO DOUBLE RESET                              
         CLC   RCONRESD,TODAY                                                   
         BE    ERROR                                                            
         SPACE 1                                                                
         LA    R3,RE3ERR           K ADDED OR CHANGED TODAY                     
         CLC   RCONCREA,TODAY                                                   
         BE    ERROR                                                            
         CLC   RCONMODD,TODAY                                                   
         BE    ERROR                                                            
         SPACE 1                                                                
         MVC   RCONRESD,TODAY                                                   
         SR    RE,RE                                                            
         IC    RE,RCONMOD                                                       
         BCTR  RE,R0                                                            
         STC   RE,RCONMOD                                                       
         OI    RCONMODR,X'20'                                                   
         OI    TAREQ,1             T/A REQ IND                                  
         GOTO1 VPUTREC,DMCB,RCONREC                                             
         B     EDIT2                                                            
         EJECT                                                                  
CONADD   XC    RCONREC(250),RCONREC                                             
         MVI   RCONLEN+1,94        REC LEN (34 + 60)                            
         MVC   RCONELEM(2),=X'013C'    DESC ELEM CODE PLUS LENGTH               
*                                                                               
         TM    TWAGENFG,TWQGOGEN                                                
         BZ    CONADD10                                                         
         OI    RCONMODR,X'02'      FLAG CONTRACT ADDED BY AUTOGEN               
*                                                                               
CONADD10 DS    0H                                                               
         FOUT  CONAGYNH,MYSPACES,20                                             
         FOUT  CONADVNH,MYSPACES,20                                             
         FOUT  CONSTAMH,MYSPACES,20                                             
         FOUT  CONSALNH,MYSPACES,19                                             
         FOUT  CONOFFNH,MYSPACES,16                                             
         FOUT  CONDSPNH,MYSPACES,15                                             
         FOUT  CONDCTNH,MYSPACES,15                                             
         FOUT  CONBNUMH,MYSPACES,8                                              
         OI    CONBNUMH+4,X'20'                                                 
*                                                                               
         TM    TWAGENFG,TWQGOGEN   FOR DARE AUTOGEN, GIVE USER                  
         BZ    CHGCON05            A CLUE AS TO WHO THE ADVERTISER IS           
         OC    WADVEXP,WADVEXP                                                  
         BZ    CHGCON05                                                         
         MVC   CONADVN,WADVEXP                                                  
*                                                                               
*MN                                                                             
*        CLC   TWAGENCP,=C'CON'                                                 
*        BNE   CONADD30                                                         
         OC    AUTOD.RCAUDRDA,AUTOD.RCAUDRDA    IF NO DARE REC ADDR             
         BZ    CONADD35            PASSED, SKIP ADDING EQUIV RECORD             
         XC    KEY,KEY                                                          
         MVC   KEY+28(4),AUTOD.RCAUDRDA                                         
         GOTO1 VGETREC,DMCB,IOAREA                                              
         USING RDARREC,R6                                                       
         LA    R6,IOAREA                                                        
         MVC   AUTOD.RCAUAGY,RDARKAGY                                           
         MVC   AUTOD.RCAUAGOF,RDARKAOF                                          
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
         USING RDARCLEM,R6                                                      
         MVC   AUTOD.RCAUADV,RDARCLI                                            
         DROP  R6                                                               
                                                                                
         XC    KEY,KEY                                                          
         XC    RDROREC(255),RDROREC                                             
         MVI   RDROKTYP,X'4C'                                                   
         MVC   RDROKREP,REPALPHA                                                
         MVC   RDROKAGY,AUTOD.RCAUAGY                                           
         MVC   RDROKAOF,AUTOD.RCAUAGOF                                          
         MVC   RDROKOAD,AUTOD.RCAUADV                                           
         MVC   KEY(27),RDROREC                                                  
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   CONADD35                                                         
                                                                                
         GOTO1 VGETREC,DMCB,RDROREC                                             
         LA    R6,RDROREC                                                       
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RDROELEM,R6                                                      
         MVC   CONADV(L'RDROEQIV),RDROEQIV                                      
         OI    CONADVH+6,X'80'     TRANSMIT                                     
         OI    CONADVH+4,X'20'     MARK AS VALIDATED                            
         DROP  R6                                                               
                                                                                
CONADD30 EQU   *                                                                
*        XC    RADVKEY(27),RADVKEY                                              
*        MVI   RADVKTYP,8          RECORD TYPE FOR ADVERTISER                   
*        MVC   RADVKADV,CONADV                                                  
*        OC    RADVKADV,MYSPACES                                                
*        MVC   RADVKREP,REPALPHA                                                
*        MVC   KEY(27),IOAREA                                                   
*        GOTO1 VHIGH                                                            
*        CLC   KEY(27),KEYSAVE                                                  
*        BNE   CONADD35                                                         
*        GOTO1 VGETREC,DMCB,IOAREA                                              
*        MVC   CONADVN,RADVNAME                                                 
*        FOUT  CONADVNH,RADVNAME,20     ADVERTISER NAME                         
                                                                                
CONADD35 EQU   *                                                                
*MN                                                                             
         B     CHGCON05                                                         
*                                                                               
CHGCON   DS    0H                                                               
*              IF EITHER CONTRACT COMMENT CHANGED, RE-VALIDATE BOTH             
         TM    CONCOM1H+4,X'20'    COMMENT 1 CHANGED?                           
         BO    *+8                                                              
         NI    CONCOM2H+4,X'DF'    COMMENT 2 NOT PREVIOUSLY VALIDATED           
*                                                                               
         TM    CONCOM2H+4,X'20'    COMMENT 2                                    
         BO    *+8                                                              
         NI    CONCOM1H+4,X'DF'    COMMENT 1                                    
         EJECT                                                                  
*                                                                               
CHGCON05 DS    0H                                                               
*                                                                               
* IF SCRIPT UPLOAD, AND CONTRACT FOUND, UPDATE AUDIT                            
*                                                                               
         CLC   =C'ADD',CONACT                                                   
         BE    CHGCON10                                                         
         GOTOR RCUAUDIT                                                         
*                                                                               
CHGCON10 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'        RANDOM FLAGS ELEMENT PRESENT?                
         BAS   RE,GETEL                                                         
         BE    TYPED               YES - SKIP                                   
         LA    R6,WORK             NO - ADD EMPTY ELEMENT                       
         XC    WORK,WORK                                                        
         USING RCONRFEL,R6                                                      
         MVI   RCONRFCD,X'1E'                                                   
         MVI   RCONRFLN,RCONRFLQ                                                
*                                                                               
         CLC   =C'ADD',CONACT      ON ADD ONLY                                  
         BNE   CHGCON15                                                         
         LA    RF,CONCACTH         SET A(CONTRACT ACTION HEADER FIELD)          
         ZIC   RE,5(RF)            GET LENGTH OF INPUT FROM HEADER              
         LA    RF,8(RF)            SET A(CONTRACT ACTION FIELD)                 
         AR    RF,RE               SET A(1ST CHAR AFTER I/P IN ACTION)          
         SH    RF,=H'2'            BACK UP 2 CHARS                              
         CLC   =C'TR',0(RF)        SET TO 'TR' ORDER? (SHORT 'TRADE')           
         BE    CHGCON11            YES                                          
         SH    RF,=H'3'            BACK UP 3 CHARS FURTHER                      
         CLC   =C'TRADE',0(RF)     SET TO 'TRADE' ORDER? (LONG 'TRADE')         
         BNE   CHGCON12            NO                                           
CHGCON11 DS    0H                                                               
         OI    RCONRF1,X'08'       MARK CONTRACT AS TRADE ORDER                 
CHGCON12 DS    0H                                                               
         TM    PROFILES+CNTRPEAB,CNTRPEAA   PROF 46 (CHECK FOR L)               
         BZ    CHGCON15                                                         
         TM    TWAFLAGS,TWAFLHMQ   HOME MARKET IN PROGRESS?                     
         BZ    CHGCON15                                                         
         OI    RCONRF1,X'20'       MARK CONTRACT AS LOCAL ORDER                 
         DROP  R6                                                               
*                                                                               
CHGCON15 DS    0H                                                               
         GOTO1 VADDELEM,DMCB,RCONREC,WORK                                       
*                                                                               
* IF CON. TYPE CHANGES, IT DOESN'T AFFECT VERSION OR CONFIRMED STATUS           
TYPED    DS    0H                                                               
***      TM    CONTYPEH+4,X'20'                                                 
***      BO    *+8                                                              
         CLC   RCONTYPE,CONTYPE    CHANGE TO CON TYPE                           
         BE    *+8                                                              
         NI    CONPRDH+4,X'DF'     REVAL PRD IF TYPE CHANGES                    
*                                                                               
         LA    R2,CONTYPEH         CONTRACT TYPE                                
         LA    R3,1                MISSING INPUT FIELD                          
         CLI   TWAACTMD,CONCHG_Q   CHANGE?                                      
         BNE   TYPED1                                                           
*MN                                                                             
         MVC   SAVCTYP,RCONTYPE                                                 
*MN                                                                             
         GOTO1 =A(TYPCHG),DMCB,(RC),RR=Y CHECK THAT CHANGE IS VALID             
         BZ    TYPED1                                                           
         LA    R3,270              TYP CAN'T BE CHGD TO/FM N/X ONCE BUY         
         CLI   HALF,C'T'                                                        
         BNE   *+8                                                              
         LA    R3,590              'TO' TYPE CHANGE ERROR MSG                   
         CLI   HALF,C'F'                                                        
         BNE   TYPEDERR                                                         
         LA    R3,593              'TO' TYPE CHANGE ERROR MSG                   
         MVC   8(1,R2),RCONTYPE                                                 
         OI    6(R2),X'80'                                                      
TYPEDERR B     ERROR                                                            
TYPED1   MVI   RCONTYPE,0                                                       
         CLI   5(R2),0             INPUT?                                       
         BNE   TYPED4              YES, CHECK IT                                
         TM    PROFILES+CNTVTYPB,CNTVTYPA   INPUT REQUIRED?                     
         BZ    TYPED99             NO                                           
*        TM    TWAGENFG,TWQGOGEN   AUTO HEADER GEN MODE?                        
*        BZ    TYPED2                                                           
*        TM    AUTOD.RCAUFLAG,X'40'                                             
*        BZ    TYPED99             SKIP REQUIRED FIELD FOR AUTOHEADER?          
TYPED2   LA    R3,571                                                           
         B     ERROR                                                            
*                                                                               
TYPED4   EQU   *                                                                
         MVC   RCONTYPE,8(R2)                                                   
         XC    KEY,KEY             VALIDATE TYPE                                
         MVI   KEY,X'32'                                                        
         MVC   KEY+24(2),REPALPHA                                               
         MVC   KEY+26(1),RCONTYPE                                               
         GOTO1 VHIGH                                                            
         LA    R3,2                INVALID INPUT                                
         CLC   KEY(27),KEYSAVE                                                  
         BE    TYPED99                                                          
         TM    TWASTREO,TWARCUQ    SCRIPT IN PROGRESS?                          
         BZ    ERROR                                                            
         MVI   RCONTYPE,C'Y'       DEFAULT FOR SCRIPT                           
*                                                                               
TYPED99  EQU   *                   END OF TYPE EDITS                            
         CLI   RCONTYPE,C'N'       BRN?                                         
         BNE   *+8                                                              
         OI    TAREQ,1             T/A REQ IND                                  
         EJECT                                                                  
*              VALIDATE 'GENCY(OFFICE) CODE                                     
AGYED    EQU   *                                                                
         GOTOR AGYEDMOD                                                         
         BZ    BUYED               OKAY:  CONTINUE                              
         L     R2,DUB              RESET CURSOR ADDRESS                         
         L     R3,DUB+4            RESET ERROR CODE                             
         B     ERROR               ERROR RETURN:  EXIT                          
*                                                                               
         EJECT                                                                  
*              VALIDATE BUYER                                                   
BUYED    DS    0H                                                               
         GOTO1 =A(BUYERED),DMCB,(RC),RR=Y                                       
*                                                                               
*              VALIDATE ADVERTISER                                              
*                                                                               
ADVED    EQU   *                                                                
         MVC   SVCATADV,MYSPACES   CLEAR ADVERTISER CATEGORY                    
*        GOTO1 =A(CHKDARE),DMCB,(RC),RR=Y                                       
*                                  DARE ORDER?                                  
*        BZ    STAED               YES - SKIP EDIT TESTING                      
         LA    R2,CONADVH                                                       
         LA    R3,ADVERR                                                        
         MVC   SVADV,RCONKADV      SAVE LATER FOR PRODUCT LOCK                  
         TM    4(R2),X'20'         VALID ADVERTISER?                            
         BZ    ADVE0040                                                         
*                                                                               
         TM    CONPRDH+4,X'20'     PRD CODE CHANGED?                            
         BZ    ADVE0020                                                         
         TM    CONCATH+4,X'20'     CAT CODE CHANGED?                            
         BO    STAED               NEED ADV CAT CODE                            
*                                                                               
ADVE0020 DS    0H                                                               
         CLI   5(R2),0                                                          
         BNE   *+8                                                              
         MVI   5(R2),4                                                          
         GOTO1 VMOVE                                                            
         B     ADVE0080                                                         
*                                                                               
*              VALIDATE ADVERTISER                                              
ADVE0040 CLI   TWAACTMD,CONCHG_Q   CHANGE?                                      
         BNE   ADVE0060                                                         
         GOTO1 =A(CKXFER),DMCB,(RC),RR=Y                                        
         BZ    ADVE0060                                                         
         LA    R3,271              CANT CHG ADV ONCE K XFERRED                  
         B     ERROR                                                            
ADVE0060 EQU   *                                                                
*MN                                                                             
         TM    TWAGENFG,TWQGOGEN   FOR DARE AUTOGEN, GIVE USER                  
         BZ    ADVE0075            A CLUE AS TO WHO THE ADVERTISER IS           
                                                                                
         TM    CONADVH+4,X'20'     WAS THIS FIELD PREVIOUSLY                    
         BO    ADVE0075            READ AND VALIDATED?                          
         CLC   CONADV,MYSPACES     NO INPUT, SKIP                               
         BNH   ADVE0075                                                         
         OC    AUTOD.RCAUDRDA,AUTOD.RCAUDRDA    IF NO DARE REC ADDR             
         BZ    ADVE0075            PASSED, SKIP ADDING EQUIV RECORD             
                                                                                
         XC    RADVKEY(27),RADVKEY                                              
         MVI   RADVKTYP,8          RECORD TYPE FOR ADVERTISER                   
         MVC   RADVKADV,CONADV                                                  
         OC    RADVKADV,MYSPACES                                                
         MVC   RADVKREP,REPALPHA                                                
         MVC   KEY(27),IOAREA                                                   
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ADVE0075                                                         
                                                                                
         XC    RDROREC(255),RDROREC                                             
         MVI   RDROKTYP,X'4C'                                                   
         MVC   RDROKREP,REPALPHA                                                
         MVC   RDROKAGY,AUTOD.RCAUAGY                                           
         MVC   RDROKAOF,AUTOD.RCAUAGOF                                          
         MVC   RDROKOAD,AUTOD.RCAUADV                                           
         MVC   KEY,MYSPACES                                                     
         MVC   KEY(27),RDROREC                                                  
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    ADVE0070                                                         
                                                                                
         MVC   RDROLEN,=X'0043'                                                 
         MVI   RDROCODE,X'01'                                                   
         MVI   RDROELLN,32                                                      
         MVC   RDROEQIV,CONADV                                                  
         OC    RDROEQIV,MYSPACES                                                
         GOTO1 GETFACT,DMCB,0                                                   
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         MVC   RDROLUID,FASYM                                                   
         GOTO1 DATCON,DMCB,(5,0),(3,RDRODATE)                                   
         MVC   RDRODCHG,RDRODATE                                                
         GOTO1 VADDREC,DMCB,RDROREC                                             
                                                                                
ADVE0070 EQU   *                                                                
ADVE0075 EQU   *                                                                
*MN                                                                             
         NI    CONPRDH+4,X'DF'     MAKE SURE PRODUCT GETS REVALIDATED           
         MVI   UPVER,1      ACE/GRAPHNET FLAG - UP VERSION & UNCONFIRM          
         GOTO1 VMOVE                                                            
         CLI   TWAACTMD,CONCHG_Q   DO NOT ALLOW CHANGE FROM                     
         BNE   ADVE0080            'GEN' UNLESS CON HAS SAR                     
         CLC   REPALPHA,=C'BL'                                                  
         BNE   ADVE0080            ONLY DO THIS CHECK FOR BLAIR                 
         CLC   RCONKADV,=CL4'GEN'                                               
         BNE   ADVE0080                                                         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BE    ADVE0080            FOUND SAR ELEMENT                            
         LA    R3,240              SAR DATA MUST BE INPUT FIRST                 
         B     ERROR                                                            
         SPACE 1                                                                
ADVE0080 XC    IOAREA(32),IOAREA                                                
*                                                                               
         CLI   8(R2),C'='          BROWSE REQUEST?                              
         BNE   ADVE0100                                                         
*                                                                               
         L     RE,4(RD)            SET TO RECNT00 RD                            
         L     RE,4(RE)                                                         
         CLI   TWACOMBO,0                                                       
         BE    *+8                                                              
         L     RE,4(RE)            COMBOS NEED 1 MORE POP                       
         GOTOX (RFBROWSE,VREPFACS),DMCB,ACOMFACS,(RE),(R2),CONADVNH,   +        
               (0,C' ADV'),0                                                    
         DC    H'0'           BROWSE SHOULD HAVE TAKEN IT FROM HERE             
*                                                                               
ADVE0100 DS    0H                                                               
         MVC   RADVKADV,WORK                                                    
*                                                                               
ADVE0110 DS    0H                                                               
         MVI   RADVKTYP,8          RECORD TYPE FOR ADVERTISER                   
         MVC   RADVKREP,REPALPHA                                                
*                                                                               
         MVC   KEY,IOAREA                                                       
         GOTO1 VHIGH                                                            
*                                                                               
*        CLC   KEY(25),KEYSAVE                                                  
*        BNE   ERROR                                                            
*        CLC   KEY+25(2),KEYSAVE+25                                             
*        BE    ADVE0200                                                         
*        CLC   KEY+25(2),=C'ZZ'                                                 
*        BE    ADVE0200                                                         
*        MVC   KEY+25(2),=C'ZZ'                                                 
*        GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    ADVE0200                                                         
*                                                                               
         TM    TWASTREO,TWARCUQ    SCRIPT IN PROGRESS?                          
         BZ    ERROR                                                            
*                                                                               
* CHECK IF WE HAVE ALREADY TRIED TO ASSIGN A DEFAULT VALUE                      
*                                                                               
         CLC   =C'WOAD',KEYSAVE+RADVKADV-RADVKEY                                
         BE    ERROR                                                            
         XC    IOAREA(32),IOAREA                                                
         MVC   RADVKADV,=C'WOAD'                                                
         B     ADVE0110            ASSIGN DEFAULT AND GO AGAIN                  
*                                                                               
ADVE0200 GOTO1 VGETREC,DMCB,IOAREA                                              
ADNOUSED EQU   903                                                              
         CLC   =C'ADD',CONACT      ON ADD ONLY                                  
         BNE   ADVE0210                                                         
         LA    R3,ADNOUSED                                                      
         TM    RADVFLGS,X'02'      IS 'DO NOT USE' SET FOR ADVERT?              
         BO    ERROR               YES - EXIT WITH ERROR                        
*                                                                               
ADVE0210 EQU   *                                                                
         CLI   TWAACTMD,CONCHG_Q   ON CHANGE ONLY                               
         BNE   ADVE0220                                                         
         TM    CONADVH+4,X'20'     ADVERTISER CHANGED?                          
         BO    ADVE0220            NO                                           
         LA    R3,ADNOUSED                                                      
         TM    RADVFLGS,X'02'      IS 'DO NOT USE' SET FOR ADVERT?              
         BO    ERROR               YES - EXIT WITH ERROR                        
ADVE0220 EQU   *                                                                
         MVC   WADVEXP,RADVNAME                                                 
         MVC   SVCATADV,RADVCATG   SAVE CAT CODE FOR CATED VALIDATION           
         FOUT  CONADVNH,RADVNAME,20     ADVERTISER NAME                         
*                                                                               
         MVC   RCONKADV,RADVKADV   TO K REC                                     
*                                                                               
* DELETE EXTENDED ADVERTISER NAME FOR KATZ ADDED DURING CONVERSION              
*                                                                               
         TM    RCONMODR+1,X'10'    FOR KATZ CONVERTED CONTRACT ONLY             
         BZ    STAED                                                            
         GOTO1 VDELELEM,DMCB,(X'6F',RCONREC)                                    
         EJECT                                                                  
*              VALIDATE STATION                                                 
STAED    DS    0H                                                               
         GOTO1 =A(STAEDIT),DMCB,(RC),RR=Y                                       
*                                                                               
* VALIDATE PRODUCT                                                              
*                                                                               
PRDED    DS    0H                                                               
         GOTO1 =A(PRDEDIT),DMCB,(RC),RR=Y                                       
         TM    CONADVH+4,X'20'     IF ADVERTISER CHANGED                        
         BO    SALED                                                            
*                                                                               
* VALIDATE CATEGORY                                                             
CATED    EQU   *                                                                
         LA    R2,CONCATH                                                       
         LA    R3,CATERR                                                        
         TM    4(R2),X'20'                                                      
         BO    SALED                                                            
* IF CATEGORY CHANGES, IT DOESN'T AFFECT VERSION OR CONFIRMED STATUS            
         GOTO1 VMOVE                                                            
*                                                                               
         XC    IOAREA(32),IOAREA                                                
         MVI   RCTGKTYP,X'0F'                                                   
         MVC   RCTGKREP,REPALPHA                                                
         MVC   RCTGKCTG,WORK                                                    
*                                                                               
         CLC   RCTGKCTG,=C'XX'                                                  
         BNE   CATED08                                                          
         TM    TWAGENFG,TWQGOGEN   AUTO HEADER GEN MODE?                        
         BZ    ERROR                                                            
         TM    AUTOD.RCAUFLAG,X'40'                                             
         BZ    ERROR               SKIP REQUIRED FIELD FOR AUTOHEADER?          
*                                                                               
CATED08  DS    0H                                                               
         CLC   CONPRD(2),=C'C='    CHECK IF PRD CODE                            
         BNE   CATED10                                                          
         CLC   SVCAT,MYSPACES      YES, CHECK IF PRD PROVIDES A CATG CD         
         BE    CATED10                                                          
         MVC   RCTGKCTG,SVCAT      IF AVAILABLE,                                
         B     CATED20                PRD CODE ALWAYS OVERRIDE                  
*                                                                               
CATED10  CLC   SVCATADV,MYSPACES   CHECK IF ADV PROVIDES A CATG CODE            
         BE    CATED20                                                          
         OC    SVCATADV,SVCATADV                                                
         BZ    CATED20                                                          
         MVC   RCTGKCTG,SVCATADV                                                
*                                                                               
* SPECIAL FOR NTVSNY PER JODI 1/25/99 AND NTVSTRN 4/26/99                       
*                                                                               
CATED20  DS    0H                                                               
         CLC   RCTGKCTG,MYSPACES                                                
         BNE   CATED30                                                          
         CLC   =C'NY',REPALPHA                                                  
         BE    *+14                                                             
         CLC   =C'CV',REPALPHA                                                  
         BNE   CATED30                                                          
         MVC   RCTGKCTG,=C'VA'     DEFAULT VARIOUS                              
*                                                                               
CATED30  DS    0H                                                               
         MVC   KEY,IOAREA                                                       
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
         GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   RCONCTGY,RCTGKCTG   CATEGORY                                     
         EJECT                                                                  
*                                                                               
*              VALIDATE SALESMAN                                                
SALED    DS    0H                                                               
         GOTO1 =A(DOSALED),RR=Y                                                 
*                                                                               
*              VALIDATE DEVELOPMENTAL SALESMAN                                  
DEVSALED EQU   *                                                                
         LA    R2,CONDSPH                                                       
         GOTO1 =A(DEVSPRSN),DMCB,(RC),RR=Y                                      
         BZ    RTGED               NO ERROR                                     
         L     R3,FULL             ERROR - CODE RETURNED IN FULL                
         B     ERROR                                                            
*              VALIDATE RATING SOURCE                                           
RTGED    LA    R2,CONRTGH                                                       
         TM    4(R2),X'20'         VALID RATING SERVICE?                        
         BO    RTG100                                                           
                                                                                
         CLI   5(R2),0             IF FIELD HAS NO INPUT                        
         BNE   RTG10                                                            
         MVC   CONRTG(3),=C'NSI'                                                
         TM    TWATIME,X'40'       IF REP PROFILE DICTATES NSI AS               
         BZ    RTG05               DEFAULT RATING SERVICE, USE NSI              
         MVC   CONRTG(3),=C'ARB'   IF ON, USE ARB                               
                                                                                
RTG05    DS    0H                                                               
         MVI   CONRTGH+5,3                                                      
         OI    CONRTGH+6,X'80'     XMIT                                         
                                                                                
* IF RATING SRC CHANGES, IT DOESN'T AFFECT VERSION OR CONFIRMED STATUS          
RTG10    DS    0H                                                               
         GOTO1 VANY                                                             
         LA    R3,RTGERR                                                        
         MVI   RCONRTGS,0                                                       
         MVI   RCONRMON,0                                                       
         SPACE 1                                                                
         CLC   CONRTG(3),=C'NON'                                                
         BE    RTG100                                                           
*                                                                               
*- VALIDATE RATING SORCE & SAVE IN 'RCONRTGS'                                   
*  (ONLY STORE 3 CHARACTER RATING SOURCE IN RECORD)                             
*                                                                               
*  1) DETERMINE LENGTH OF RATING SOURCE (LEN .GT. 3 = ERROR)                    
*                                                                               
*  2) CHECK FOR VALID SOURCE VIA TABLE LOOKUP.                                  
*     ACCEPT IF SOURCE MATCHES ENTRY & MEETS MINIMUM COMPARE LENGTH.            
*     (FUTURE CONSIDERATION J.I.C. 1 CHARACTER ISN'T UNIQUE)                    
*                                                                               
*     THIS METHOD ACCEPTS 'A', 'AR', OR 'ARB' AS VALID                          
*                                                                               
*  3) SAVE 1 BYTE RATING SOURCE IN RECORD.                                      
*                                                                               
         SR    R6,R6               PUT RTG SOURCE LEN HERE                      
         IC    R6,5(R2)            INPUT LENGTH                                 
*                                                                               
         CLI   5(R2),3             MAX INPUT LENGTH                             
         BH    ERROR                                                            
*                                                                               
*- R6 = RTG SOURCE LENGTH.  LOOK FOR MATCH IN TABLE IF LENGTH                   
*  MEETS OR EXCEEDS MINIMUM TABLE MATCH LENGTH.                                 
*                                                                               
*  IF VALID RTG SOURCE FOUND, SAVE 3 BYTE SOURCE IN RECORD.                     
*  ELSE ERROR.                                                                  
RTG20    EQU   *                                                                
         LR    RE,R6               RE = RTG SOURCE LEN - 1                      
         BCTR  RE,0                                                             
*                                                                               
         LA    R5,RTGSTBL          TV RTG SOURCE TABLE                          
*                                                                               
         CLI   RCONKSTA+4,C' '     BLANK = TV                                   
         BE    RTG30                                                            
         CLI   RCONKSTA+4,C'L'     LOW POWER = TV                               
         BE    RTG30                                                            
*                                                                               
*        LA    R5,RRTGSTBL         RADIO RATING SOURCE TABLE                    
*                                                                               
RTG30    CLI   0(R5),X'00'         EOT?                                         
         BE    ERROR               RTG SOURCE IS INVALID                        
*                                                                               
         SR    RF,RF               MEET MINIMUM LENGTH?                         
         IC    RF,0(R5)                                                         
         CR    R6,RF                                                            
         BL    RTG40               SCREEN SOURCE TOO SMALL.                     
*                                                                               
         EX    RE,RTG50            TBL ENTRY -VS- SCREEN COMPARE                
         BNE   RTG40               NO HIT                                       
*                                                                               
         MVC   RCONRTGS(1),1(R5)   SAVE RTG SOURCE IN REC                       
         B     RTG100                                                           
*                                                                               
RTG40    LA    R5,LRTGSTBL(R5)     POINT TO NEXT TBL ENTRY                      
         B     RTG30                                                            
*                                                                               
RTG50    CLC   1(0,R5),8(R2)       RTGSTBL ENTRY -VS- SCREEN FLD                
*                                                                               
*- RATING SOURCE TABLE.  END WITH BYTE OF X'00'   LAYOUT:                       
*    XL1'MINIMUM COMPARE LENGTH FOR ENTRY'                                      
*    CL3'RATING SOURCE CODE'                                                    
*                                                                               
RTGSTBL  EQU   *                   TELEVISION TABLE                             
         DC    X'01',CL3'ARB'      ARBITRON                                     
LRTGSTBL EQU   *-RTGSTBL           <-- ENTRY LENGTH                             
         DC    X'01',CL3'NSI'      NIELSEN                                      
*        DC    X'01',CL3'SRC'      SRC                                          
         DC    X'00'               EOT                                          
         SPACE                                                                  
RRTGSTBL EQU   *                   RADIO TABLE                                  
*        DC    X'01',CL3'ARB'      ARBITRON                                     
*        DC    X'01',CL3'BIR'      BIRCH                                        
*        DC    X'00'               EOT                                          
*                                                                               
RTG100   DS    0H                                                               
         EJECT                                                                  
* VALIDATE EASI CODES (ADV, PROD, EST)                                          
CDED     DS    0H                                                               
         TM    CONIADVH+4,X'20'     TEST IF ALL 3 FIELDS VALID                  
         BZ    CDED5                                                            
         TM    CONIPRDH+4,X'20'                                                 
         BZ    CDED5                                                            
         TM    CONIESTH+4,X'20'                                                 
         BNZ   CDEDX                                                            
CDED5    MVI   TEMP,0              USE BYTE 1 OF TEMP TO INDICATE EASI          
         XC    RSTAKEY,RSTAKEY                                                  
         MVI   RSTAKTYP,X'02'      STA REC TYPE                                 
         MVC   RSTAKREP,REPALPHA   REP CODE                                     
         MVC   RSTAKSTA,RCONKSTA                                                
         MVC   KEY,RSTAKEY                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),RSTAKEY                                                  
         BE    CDED7                                                            
         DC    H'0'                                                             
CDED7    GOTO1 VGETREC,DMCB,RSTAREC                                             
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL                                                         
         BNE   CDED10                                                           
         SPACE 1                                                                
         USING RSTAXXEL,R6                                                      
         CLI   RSTAOPT4,C'Y'       IS THIS AN EASI STATION?                     
         BNE   CDED10              NO - DON'T BOTHER W/AGY                      
         DROP  R6                                                               
         XC    RAGYKEY,RAGYKEY                                                  
         MVI   RAGYKTYP,X'0A'      AGY REC TYPE                                 
         MVC   RAGYKAGY,RCONKAGY                                                
         MVC   RAGYKAOF,MYSPACES                                                
         MVC   RAGYKREP,REPALPHA   REP CODE                                     
         MVC   KEY,RAGYKEY                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),RAGYKEY                                                  
         BE    CDED9                                                            
         DC    H'0'                                                             
CDED9    GOTO1 VGETREC,DMCB,RAGYREC                                             
         LA    R6,RAGYREC                                                       
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         USING RAGYELEM,R6                                                      
         CLI   RAGYPRO1,C'Y'       IS THIS AN EASI AGY?                         
         BNE   CDED10              NO                                           
* EASI STA/AGY - SET FLAG                                                       
         MVI   TEMP,1                                                           
CDED10   DS    0H                                                               
         DROP  R6                                                               
*MN                                                                             
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         BNE   CDED12                                                           
         USING RCONIEL,R6                                                       
         MVC   TEMP+2(4),RCONIPRD                                               
         MVC   TEMP+6(4),RCONIPR2                                               
CDED12   EQU   *                                                                
*MN                                                                             
         LA    R2,CONIADVH                                                      
         GOTO1 VDELELEM,DMCB,(X'A2',RCONREC)                                    
         CLI   TEMP,1              EASI STATION?                                
         BNE   CDED20                                                           
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   CDED30              YES - GET NEXT FIELD                         
*                                                                               
         TM    TWAGENFG,TWQGOGEN   AUTO HEADER GEN MODE?                        
         BZ    CDED15                                                           
         TM    AUTOD.RCAUFLAG,X'40'                                             
         BZ    CDED20              SKIP REQUIRED FIELD FOR AUTOHEADER?          
*                                                                               
CDED15   DS    0H                                                               
         LA    R3,342              ERR - CODES REQD FOR EASI STA/AGY            
         B     ERROR                                                            
CDED20   DS    0H                  MAKE SURE NO OTHER EASI FLD INPUT            
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   CDED40                                                           
         CLI   CONIPRDH+5,0                                                     
         BNE   CDED40                                                           
         CLI   CONIESTH+5,0                                                     
         BNE   CDED40                                                           
         B     CDEDX                                                            
CDED30   DS    0H                                                               
*                                                                               
         TM    TWAGENFG,TWQGOGEN   AUTO HEADER GEN MODE?                        
         BZ    CDED35                                                           
         TM    AUTOD.RCAUFLAG,X'40'                                             
         BZ    CDED40              SKIP REQUIRED FIELD FOR AUTOHEADER?          
*                                                                               
CDED35   DS    0H                                                               
         LA    R3,343              ERR - ALL FIELDS MUST BE INPUT               
         LA    R2,CONIPRDH                                                      
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
         LA    R2,CONIESTH                                                      
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
CDED40   XC    WORK,WORK           ADD ELEMENT                                  
         LA    R6,WORK                                                          
         USING RCONIEL,R6                                                       
         MVI   RCONICD,X'A2'                                                    
         MVI   RCONILN,32                                                       
*MN                                                                             
         CLC   CONIPRD(4),=CL4'****'                                            
         BNE   CDED45                                                           
         MVC   RCONIPRD,TEMP+2                                                  
         MVC   RCONIPR2,TEMP+6                                                  
         B     CDED55                                                           
                                                                                
CDED45   EQU   *                                                                
         CLC   TEMP+6(4),MYSPACES                                               
         BNH   CDED50                                                           
         LA    R3,977              MUST CHANGE PRODUCT ON HIST SCREEN           
         LA    R2,CONIPRDH                                                      
         B     ERROR                                                            
*MN                                                                             
CDED50   MVC   RCONIPRD,CONIPRD                                                 
CDED55   MVC   RCONIADV,CONIADV                                                 
         MVC   RCONXEST,CONIEST                                                 
         GOTO1 VADDELEM,DMCB,RCONREC,WORK                                       
         MVI   UPVER,1       ACE/GRAPHNET FLAG - UP VERSION & UNCONFIRM         
CDEDX    DS    0H                                                               
         OI    CONIADVH+4,X'20'     NOW MAKE ALL 3 FIELDS VALID                 
         OI    CONIPRDH+4,X'20'                                                 
         OI    CONIESTH+4,X'20'                                                 
         DROP  R6                                                               
         EJECT                                                                  
*              VALIDATE DEVELOPMENTAL CONTRACT TYPE                             
         LA    R2,CONDCTH                                                       
         LA    R3,DEVCTERR                                                      
         GOTO1 =A(DEVCONTY),DMCB,(RC),RR=Y                                      
         BZ    DTED                SUCCESSFUL VALIDATION                        
         OC    MYP,MYP             FAILED: WHAT TYPE OF ERROR?                  
         BNZ   ERROR               DATA IN FIELD: CONTYP NOT FOUND              
         LA    R3,DEVSPREQ         NO DATA: DEV S/P REQS DEV CONTYPE            
         B     ERROR                                                            
*              VALIDATE START DATE                                              
DTED     LA    R2,CONDTESH                                                      
         TM    4(R2),X'20'         VALID?                                       
         BO    COMEDT                                                           
         MVI   UPVER,1      ACE/GRAPHNET FLAG - UP VERSION & UNCONFIRM          
         GOTO1 SCANNER,DMCB,CONDTESH,(2,WORK2),C',=-='                          
         CLI   DMCB+4,2            IF NOT 2 DATES, ERROR                        
         BE    DTED10                                                           
         LA    R3,EDTERR           MISSING END DATE                             
         B     ERROR                                                            
DTED10   LA    R3,SDTERR                                                        
         GOTO1 DATVAL,DMCB,WORK2+12,WORK                                        
*                                                                               
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    ERROR                                                            
         GOTO1 DATCON,DMCB,WORK,(3,TWASVSDT)      START DATE                    
*        MVC   TWASVSDT,RCONDATE   SAVE OFF                                     
         EJECT                                                                  
*                                                                               
* SKIP STATION JOIN DATE CHECK FOR BACK BILLING CONTRACTS                       
*                                                                               
         CLC   =C'ACC-BB',RCONBUYR                                              
         BE    DTED20                                                           
*                                                                               
* START DATE MUST BE ON OR AFTER STATION JOIN DATE                              
*                                                                               
         LA    R3,287                                                           
         CLC   TWASTJDT,TWASVSDT                                                
         BH    ERROR                                                            
*                                                                               
* VALIDATE END DATE                                                             
* END DATE IS SECOND IN SCANNER BLOCK                                           
*                                                                               
DTED20   DS    0H                                                               
         LA    R3,EDTERR                                                        
         GOTO1 DATVAL,DMCB,WORK2+44,WORK+6                                      
*                                                                               
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    ERROR                                                            
*                                                                               
         LA    R3,64               ERR - EDT BEFORE SDT                         
         CLC   WORK+6(6),WORK      END V START DATE                             
         BL    ERROR                                                            
*              PUT END DATE IN CONREC                                           
         GOTO1 DATCON,DMCB,WORK+6,(3,FULL)                                      
         MVC   TWASVEDT,FULL       SAVE OFF                                     
         MVI   UPVER,1      ACE/GRAPHNET FLAG - UP VERSION & UNCONFIRM          
* CHECK IF K DATES EXCEED 1 CALENDAR YR (13 BROADCAST MONTHS MAX)               
         LA    R3,CALERR                                                        
         CLC   TWASVSDT,FULL              YEARS                                 
         BH    ERROR                                                            
* COUNT K WEEKS                                                                 
ED5      SR    R4,R4                                                            
         MVC   WORK+12(6),WORK                                                  
ED10     LA    R4,1(R4)                                                         
         GOTO1 ADDAY,DMCB,WORK+12,WORK+18,7                                     
         MVC   WORK+12(6),WORK+18                                               
         CLC   WORK+12(6),WORK+6                                                
         BNH   ED10                                                             
*                                                                               
         STC   R4,RCONWKS                                                       
         LA    R3,49               ERROR - DATES CANT EXCEED 1 CAL YR           
         CHI   R4,53               FLT WKS VS. 53                               
         BH    ERROR               CAN'T BE HIGHER                              
         BL    ED12                LOWER - ALWAYS OK                            
*                                  ALLOW 53 WK YEAR ONLY WHEN 12/31 IS          
*                                  ON SUNDAY & FLT END IS IN LAST WEEK          
*                                  OF YEAR                                      
         CLI   FULL+1,12           END DATE IN DEC?                             
         BNE   ERROR               NO - CAN'T BE 53 WK YEAR                     
*                                                                               
         MVC   DUB(6),WORK+6       END DATE                                     
         MVC   DUB+4(2),=C'31'     12/31 OF END YEAR                            
*                                                                               
         TM    FULL,X'03'          TEST LEAP YEAR (DIVISIBLE BY 4)              
         BNZ   *+10                                                             
         MVC   DUB+4(2),=C'30'     12/30 OF END YEAR                            
*                                                                               
         GOTO1 GETDAY,DMCB,DUB,DUB                                              
         CLC   DUB(3),MYSPACES                                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),7             12/31 ON SUNDAY (OR 12/30 IF LEAP)           
         BNE   ERROR               NO - CAN'T BE 53 WK YEAR                     
*                                                                               
         TM    FULL,X'03'          TEST LEAP YEAR (DIVISIBLE BY 4)              
         BNZ   ED11                                                             
         CLC   WORK+10(2),=C'24'   END DATE IN LAST WK OF DECEMBER?             
         BL    ERROR               NO - 53 WKS NOT ALLOWED                      
         B     ED12                                                             
*                                                                               
ED11     DS    0H                                                               
         CLC   WORK+10(2),=C'25'   END DATE IN LAST WK OF DECEMBER?             
         BL    ERROR               NO - 53 WKS NOT ALLOWED                      
*                                                                               
* CHECK THAT K DATES NOT INCONSISTENT WITH BUCKETS                              
* TOTAL SPAN OF K DATES AND BUCKETS CANNOT EXCEED 13 MONTHS                     
* GET BROADCAST MONTHS OF CONTRACT                                              
ED12     DS    0H                                                               
         LA    R3,BUCERR                                                        
         GOTO1 VGTBROAD,DMCB,(1,WORK),WORK+12,GETDAY,ADDAY                      
         GOTO1 (RF),(R1),(1,WORK+6),WORK+24                                     
         GOTO1 DATCON,(R1),WORK+18,(3,DUB)    START MONTH                       
         GOTO1 (RF),(R1),WORK+30,(3,DUB+3)  END MONTH                           
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONRFEL,R6                                                      
         XC    RCONRFLT,RCONRFLT   DEFAULT = NO REVISED START/END DATE          
*                                                                               
         MVI   TEMP,0              CLEAR RCU ERROR FLAG                         
         LA    R5,RCONELEM         FIRST ELEM                                   
ED15     CLI   0(R5),3             ORD BUCKET?                                  
         BE    ED20                                                             
         CLI   0(R5),4             INV BUCKET?                                  
         BE    ED20                                                             
ED18     ZIC   R4,1(R5)                                                         
         AR    R5,R4               NEXT ELEM                                    
         CLI   0(R5),0             LAST?                                        
         BE    ED25                                                             
         B     ED15                                                             
*                                                                               
ED20     CLC   2(2,R5),DUB         K START YR-MON                               
         BL    ED23A                                                            
* CHECK AGAINST END                                                             
ED20A    DS    0H                                                               
         CLC   2(2,R5),DUB+3       FLIGHT END PAST EXISTING BUCKETS?            
         BNH   ED18                NO - OK                                      
*                                                                               
*                                  CHECK IF BUCKETS PAST K END NET $0           
         SR    R1,R1               MONTH/YR TOTAL                               
         B     ED22                                                             
ED21     ZIC   R4,1(R5)                                                         
         AR    R4,R5                                                            
         CLC   0(4,R4),0(R5)       BUCKET SAME TYPE/MONTH/YEAR?                 
         BNE   ED23                NO                                           
         LR    R5,R4                                                            
ED22     ICM   RF,15,6(R5)         ADD BUCKET TO TOTAL                          
         AR    R1,RF                                                            
         B     ED21                NEXT BUCKET                                  
ED23     LTR   R1,R1               SUM OF THESE MONTH/YR BUCKETS = 0?           
         BZ    ED24                NO - INVALID END DATE                        
*                                                                               
ED23A    DS    0H                                                               
         TM    TWASTREO,TWARCUQ    SCRIPT IN PROGRESS?                          
         BZ    ERROR                                                            
         MVI   TEMP,1              SET RCU ERROR FLAG                           
         B     ED18                SKIP IF BAD FLIGHT DATES                     
*                                                                               
ED24     DS    0H                                                               
         MVC   RCONRFLT(3),TWASVSDT                                             
         MVC   RCONRFLT+3(3),FULL                                               
         B     ED18                YES - OK                                     
*                                                                               
ED25     DS    0H                                                               
         CLI   TEMP,1              WAS RCU ERROR FLAG TRIGGERED?                
         BE    ED30                YES, DON'T UPDATE NEW FLIGHT                 
         MVC   RCONDATE(3),TWASVSDT ELSE TAKE NEW DATES                         
*                                                                               
         OC    RCONRFLT,RCONRFLT   DID WE HAVE USE FOR REVISED DATE?            
         BNZ   ED30                YES                                          
         MVC   RCONDATE+3(3),FULL  NO - UPDATE NORMAL DATE FIELD                
         DROP  R6                                                               
*                                                                               
* NOW CHECK IF EARLIEST + 1 YEAR EXCEEDS 13 MONTHS                              
ED30     DS    0H                                                               
         IC    R4,DUB              EARLIEST YEAR                                
         LA    R4,1(R4)                                                         
         STC   R4,DUB                                                           
         LA    R3,49               ERROR - DATES CANT EXCEED 1 CAL YR           
         CLC   DUB+3(2),DUB                                                     
         BH    ERROR                                                            
*                                                                               
         GOTO1 =A(ALTCAL),RR=Y                                                  
*                                                                               
         CLI   TWAACTMD,CONCHG_Q   CHANGE?                                      
         BNE   COMEDT                                                           
*              CHANGE - CHECK IF CONTRACT PERIOD MADE SMALLER                   
         CLC   RCONDATE(3),TWACDTES     NEW V OLD K START DATE                  
         BH    *+14                                                             
         CLC   FULL(3),TWACDTES+3 NEW V OLD K END DATES                         
         BNL   FCASTBUK                                                         
*                                                                               
*              CHECK BUYS TO SEE IF ALL WITHIN NEW K DATES                      
         XC    RBUYKEY,RBUYKEY                                                  
         MVI   RBUYKTYP,11         BUY REC TYPE                                 
         MVC   RBUYKREP,REPALPHA   REP CODE                                     
         MVC   RBUYKCON,TWACNUM                                                 
         MVC   KEY,RBUYKEY                                                      
         GOTO1 VHIGH                                                            
         B     GETBUY1                                                          
         SPACE 1                                                                
GETBUY   GOTO1 VSEQ                                                             
GETBUY1  CLC   KEY(22),KEYSAVE     LAST BUY?                                    
         BNE   FCASTBUK                                                         
         CLI   KEY+26,255          PLANREC?                                     
         BE    GETBUY                                                           
*                                                                               
         GOTO1 VGETREC,DMCB,RBUYREC                                             
*                                                                               
*              GET 1ST DATE ELEMENT                                             
         LA    R6,RBUYREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CKDATE   CLC   2(3,R6),RCONDATE    BUY START V K START DATE                     
         BL    *+14                                                             
         CLC   5(3,R6),FULL        BUY END V K END DATE                         
         BNH   *+12                                                             
         LA    R3,KDTERR                                                        
         B     ERROR                                                            
         BAS   RE,NEXTEL                                                        
         BNE   GETBUY                                                           
         B     CKDATE                                                           
         EJECT                                                                  
*                                                                               
*  FCASTBUK:  FOR FLIGHT DATE CHANGES (NOT NEW ADDS), CHECK                     
*       TO DETERMINE IF FORECAST BUCKETS MUST BE RESPREAD.                      
*                                                                               
FCASTBUK EQU   *                                                                
         GOTO1 =A(GENFCAST),DMCB,(RC),RR=Y                                      
*                                                                               
* SPECIAL FOR AM->SZ TAKEOVER DARE CONTRACTS, DON'T UP VERSION                  
*                                                                               
         CLC   =C'SZ',REPALPHA                                                  
         BNE   COMEDT                                                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   COMEDT                                                           
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'                                                   
         BZ    COMEDT              LINKED?                                      
         DROP  R6                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1C'        TAKEOVER?                                    
         BAS   RE,GETEL                                                         
         BNE   COMEDT                                                           
         USING RCONTKEL,R6                                                      
         CLC   =C'AM',RCONTKRP     FROM KATZ?                                   
         BNE   COMEDT                                                           
         DROP  R6                                                               
*                                                                               
         MVI   UPVER,0                                                          
*                                                                               
*  VALIDATE COMMENTS                                                            
COMEDT   GOTO1 =A(CMTRNT),DMCB,(RC),RR=Y                                        
ENDEDIT  CLI   TWAACTMD,CONCHG_Q   CHANGE?                                      
         BNE   END5                                                             
         L     RE,AIO2                                                          
         CLC   RCONKSTA,RCONKSTA-RCONREC(RE)                                    
         BE    END2                                                             
         LA    R3,SPLERR3                                                       
         LA    R2,CONSTAH                                                       
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BE    ERROR                                                            
* CHECK FOR DATE CHANGE                                                         
END2     L     RE,AIO2                                                          
         LA    RE,RCONDATE-RCONREC(RE)  OLD DATE                                
         CLC   RCONDATE(6),0(RE)                                                
         BE    END60                                                            
*                                                                               
* CHECK STATION INVOICE CLOSE MONTH - ADD 0 INV BUCKET IF MONTH CLOSED          
END5     OC    TWASTCDT,TWASTCDT   NO CLOSE DATE?                               
         BZ    END50                                                            
*                                                                               
         CLI   TWACOMBO,0          IF COMBO, DON'T CHECK FOR STATION            
         BNE   END50               CLOSE OUT.  IT'LL BE CHECKED LATER           
*                                  IN ADDCON                                    
* CHECK K START DATE                                                            
         CLC   RCONDATE(2),TWASTCDT                                             
         BH    END50                                                            
* CHECK MONTHS OF K                                                             
         GOTO1 DATCON,DMCB,(3,RCONDATE),WORK                                    
         GOTO1 (RF),(R1),(3,RCONDATE+3),WORK+6                                  
         XC    HALF,HALF                                                        
*                                                                               
* BUILD ZERO INVOICE BUCKET IN WORK2                                            
         MVC   WORK2(2),=X'040A'   ELEM CODE + LEN                              
         MVC   WORK2+4(2),MONDATE  ACTIVITY DATE                                
         XC    WORK2+6(4),WORK2+6                                               
*                                                                               
* GET BROADCAST MONTH                                                           
END10    GOTO1 VGTBROAD,(R1),(1,WORK),WORK+12,GETDAY,ADDAY                      
         GOTO1 DATCON,(R1),WORK+18,(3,FULL)                                     
         CLC   FULL(2),TWASTCDT                                                 
         BH    END50                                                            
         OC    HALF,HALF      FIRST TIME?                                       
         BZ    END15                                                            
*                                                                               
         CLC   HALF,FULL           SAME MONTH?                                  
         BE    END40                                                            
*                                                                               
END15    MVC   HALF,FULL           SAVE                                         
         MVC   WORK2+2(2),FULL                                                  
* NEXT ELEM                                                                     
END30    EQU   *                   LOOK FOR AN INV EL FOR THIS MONTH            
         GOTO1 HELLO,DMCB,(C'G',=C'REPFILE'),(X'04',RCONREC),          X        
               (2,WORK2+2)                                                      
         CLI   DMCB+12,0           FOUND?                                       
         BE    END40               DON'T PUT DUP ELEM                           
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(0,RCONREC),(0,WORK2)              
         LA    RF,TWAMCAST         CHECK FOR ALTERNATE CALENDAR USE             
         LA    R0,4                SET LOOP CONTROL                             
END32    EQU   *                                                                
         OC    0(5,RF),0(RF)       ANY ENTRY IN TABLE?                          
         BZ    END40               NO  - NOT ALTERNATE CALENDAR                 
         CLC   RCONKSTA,0(RF)      STATION IN TABLE?                            
         BE    END34               YES -                                        
         LA    RF,7(RF)            NO  - BUMP TO NEXT ENTRY                     
         BCT   R0,END32            GO BACK FOR NEXT                             
         B     END40               NOT ALTERNATE CALENDAR STATION               
END34    EQU   *                                                                
         OI    WORK2,X'50'         YES - PUT OUT $.00 ALTERNATE                 
*                                     CALENDAR INVOICE ELT                      
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(0,RCONREC),(0,WORK2)              
*                                                                               
         MVI   WORK2,X'04'         RESET ELEMENT CODE                           
* GET NEXT WEEK                                                                 
END40    EQU   *                                                                
         CLC   WORK(4),WORK+6                                                   
         BE    END50                                                            
         CLC   WORK2+2(2),TWASTCDT                                              
         BE    END50                                                            
         GOTO1 ADDAY,(R1),WORK,WORK+24,7                                        
         MVC   WORK(6),WORK+24                                                  
         B     END10                                                            
*                                                                               
END50    CLC   CONACT(3),=C'ADD'                                                
         BE    EDIT2                                                            
*                                                                               
*              CHANGE FUNCTION                                                  
END60    OI    RCONMODR,X'80'      K HEADLINE CHANGE CODE                       
         CLC   TODAY,RCONMODD                                                   
         BE    END100                                                           
         CLC   TODAY,RCONCREA                                                   
         BE    END100                                                           
         OI    TAREQ,1             T/A REQ IND FOR BASE                         
END100   DS    0H                                                               
* IF ACE/GRAPHNET & UPVER=1, UNCONFIRM CONTRACT & UP VERSION NUMBER             
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    EDIT2                                                            
* UPVER=0 IF ONLY CHANGE WAS TO CATG, RTG SRC OR TYPE                           
         CLI   UPVER,0                                                          
         BE    EDIT2                                                            
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW                                
         BZ    END230                                                           
         NI    RCONCONF,X'BF'      TURN OFF CONFIRMED NOW                       
         OI    RCONCONF,X'20'      TURN ON CONFIRMED PREV.                      
END230   OI    RCONCONF,X'80'      NOT CONFIRMED                                
         SPACE 1                                                                
         ZIC   R3,1(R6)            GET NEXT ELEMENT                             
         AR    R6,R3                                                            
         CLI   0(R6),X'20'         SHOULD BE SEND ELEMENT                       
         BE    END235                                                           
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
         SPACE 1                                                                
* REP CAN'T MAKE CHANGE IF STATION IS IN MIDDLE OF CHANGES                      
         SPACE 1                                                                
END235   TM    RCONSENF,X'10'      X'10'=STA VERS NOT ADVANCED                  
         BO    *+12                                                             
         LA    R3,167              LATEST STA VERSION NOT YET SENT              
         B     ERROR                                                            
         SPACE 1                                                                
         TM    RCONSENF,X'20'      REP VERSION NOT ADVANCED                     
         BZ    EDIT2                                                            
         DROP  R6                                                               
*                                                                               
* ADVANCE REP VERSION AND SAVE VERSION DATES                                    
*                                                                               
         MVC   WORK(4),HELLO       SAVE OFF VERSION DATE                        
         MVC   WORK+4(4),DATCON                                                 
         GOTO1 VREGENVR,DMCB,(C'R',RCONREC),WORK                                
         BNZ   ERROR                                                            
*                                                                               
EDIT2    DS    0H                  RESUBMIT PAR SECURITY CHECK                  
         XC    WORK,WORK           BULD PAR SECURITY CHECK                      
         LA    R1,WORK                                                          
         USING SBLOCK,R1                                                        
         MVC   SBOFFICE(2),RCONKOFF                                             
         MVC   SBSTATN(5),RCONKSTA                                              
         MVC   SBSALES(3),RCONSAL                                               
         OI    SBBREAKS,STABREAK+OFFBREAK+SALBREAK                              
         DROP  R1                                                               
*                                                                               
*   FOLLOWING CHECK IS DONE BEFORE PAR CALL, AND WILL IGNORE AN                 
*        ERROR RETURN IF THE CALL HAS BEEN MADE FOR A 'PROPOSER'                
*        AUTO ASSIGN.                                                           
*                                                                               
         LA    R3,TWAGLVX                                                       
SWP      USING GLVXFRSY,R3                                                      
         CLC   =C'SWP',SWP.GLVXFRPR      FROM PROPOSER PROGRAM?                 
         BE    EDIT2A              YES - IGNORE THE ERROR RETURN                
*                                                                               
         DROP  SWP                                                              
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         L     R1,AFACILS                                                       
         L     R3,0(R1)            ATIO                                         
         GOTO1 (RFCKSEC,VREPFACS),DMCB,WORK,CONMSGH,(R3),DUB                    
         BE    EDIT2A                                                           
         LA    R3,860                                                           
         LA    R2,CONCNUMH                                                      
         OI    6(R2),X'40'+X'80'                                                
         B     ERROR                                                            
*                                                                               
* 2K           BUILD OLD POINTERS AT IO3                                        
* 2K           BUILD NEW POINTERS AT IO3+800                                    
         SPACE 1                                                                
EDIT2A   DS    0H                                                               
         CLC   CONACT(3),=C'ADD'                                                
         BE    ADDCON                                                           
         CLC   =C'R#',CONACT                                                    
         BE    EDIT2B                                                           
         CLI   TWAACTMD,CONCHG_Q   CHANGE?                                      
         BNE   EXXMOD                                                           
         SPACE 1                                                                
*              CHANGE CONTRACT                                                  
* CHECK FOR CHANGE TO ANY CONTRACT KEY                                          
         SPACE 1                                                                
* NEW                                                                           
EDIT2B   DS    0H                                                               
         L     R6,AIO3                                                          
         LA    R6,800(R6)                                                       
         GOTO1 =A(PTRS),DMCB,(RC),(R6),0,RR=Y                                   
         GOTO1 VMOVEREC,DMCB,RCONREC,AIO4     SAVE NEW CONREC IN IO4            
* OLD                                                                           
         L     R8,AIO2                                                          
         GOTO1 VMOVEREC,(R1),(R8),RCONREC   MOVE OLD CONREC TO RCONREC          
* BUILD LIST OF OLD PTRS                                                        
         SPACE 1                                                                
         L     R6,AIO3                                                          
         GOTO1 =A(PTRS),DMCB,(RC),(R6),1,RR=Y                                   
         GOTO1 VMOVEREC,(R1),AIO4,RCONREC                                       
         SPACE 2                                                                
* IF BOP KEY HAS CHANGED (OTHER THAN DATE), PUT TODAY'S DATE                    
*    IN BOP ELEMENT                                                             
         SPACE 1                                                                
         L     R6,AIO3             OLD                                          
         LA    R8,800(R6)          NEW                                          
         LA    R4,7                ALLOW FOR 7 POINTERS                         
CE30     CLI   0(R6),X'DC'         BOP POINTER                                  
         BE    CE40                                                             
         LA    R6,32(R6)                                                        
         LA    R8,32(R8)                                                        
         BCT   R4,CE30                                                          
         B     CE70                                                             
         SPACE 1                                                                
         USING RCONBTYP,R6         BOP POINTER                                  
CE40     CLC   RCONBTYP(11),0(R8)  IF ANYTHING EXCEPT DATE CHANGES,             
         BNE   CE50                 PUT TODAY IN BOP ELEMENT                    
         CLC   RCONBREF(13),RCONBREF-RCONBTYP(R8)                               
         BE    CE70                                                             
         DROP  R6                                                               
CE50     DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   CE70                                                             
         USING RCONBPEL,R6                                                      
         MVC   RCONBPDT,TODAY                                                   
         DROP  R6                                                               
*                                                 RCONREC                       
* CHECK IF ANY KEY DIFFERENT                                                    
* DIVISION/TEAM/SALESMAN/CATEGORY CODES MAY CHANGE ANY TIME                     
* BOP KEY MAY CHANGE                                                            
         SPACE 1                                                                
CE70     L     R6,AIO3                                                          
         LA    R8,800(R6)          NEW                                          
         SPACE 1                                                                
         USING RCONREC,R6                                                       
         CLC   RCONKSTA(5),RCONKSTA-RCONKTYP(R8)  CHANGED STATION               
         BNE   EEND100                                                          
         CLC   RCONKEY(27),0(R8)                                                
         BNE   C200                CHANGE DATA IN 0C KEY                        
         SPACE 1                                                                
         USING RCONPTYP,R6                                                      
         LA    R6,32(R6)                                                        
         LA    R8,32(R8)                                                        
         CLC   RCONPTYP(27),0(R8)                                               
         BNE   C200                CHANGED THE NUMBER                           
         SPACE 1                                                                
         USING RCONQTYP,R6                                                      
         LA    R6,32(R6)                                                        
         LA    R8,32(R8)                                                        
         CLC   RCONQTYP(27),0(R8)                                               
         BNE   C200                                                             
         SPACE 1                                                                
         USING RCONRTYP,R6                                                      
         LA    R6,32(R6)                                                        
         LA    R8,32(R8)                                                        
         CLC   RCONRTYP(5),0(R8)   TEAM AND SALESMAN MAY CHANGE                 
         BNE   C200                                                             
         CLC   RCONRSTA(17),RCONRSTA-RCONRTYP(R8)                               
         BNE   C200                                                             
         SPACE 1                                                                
         USING RCONDTYP,R6                                                      
         LA    R6,32(R6)                                                        
         LA    R8,32(R8)                                                        
         CLC   RCONDTYP(4),0(R8)   CATEGORY MAY CHANGE                          
         BNE   C200                                                             
         CLC   RCONDOFF(21),RCONDOFF-RCONDTYP(R8)                               
         BNE   C200                                                             
         SPACE 1                                                                
         USING RCONSTYP,R6                                                      
         LA    R6,32(R6)                                                        
         LA    R8,32(R8)                                                        
         CLC   RCONSTYP(10),0(R8)  TEAM AND SALESMAN MAY CHANGE                 
         BNE   C200                                                             
         DROP  R6                                                               
         EJECT                                                                  
*  CAN ONLY ALLOW CALL LETTER CHANGE IF ACE/GRAPHNET STATUS IS                  
*  CONSISTENT BETWEEN THE OLD AND NEW.                                          
*                                                                               
EEND100  XC    KEY,KEY                                                          
         L     R3,AIO3             OLD POINTERS                                 
         LA    R8,800(R3)          NEW POINTERS                                 
*                                                                               
         LA    R3,RCONREC                                                       
         USING RCONREC,R3                                                       
*                                                                               
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(5),RCONKSTA-RCONKTYP(R8)   STATION                        
         BAS   R5,GETSTA                                                        
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   EEND105                                                          
         SPACE 1                                                                
         OC    10(2,R6),10(R6)     NO RECVNG ID-NEW NOT ACE/GRAPHNET            
         BZ    EEND105                                                          
         CLC   10(2,R6),=X'0406'   NEW IS GRAPHNET                              
         BNE   EEND103                                                          
         TM    RCONMODR+1,X'40'    IS OLD GRAPHNET                              
         BZ    EEND107              NO, ERROR                                   
         B     C200                                                             
*                                                                               
EEND103  TM    RCONMODR+1,X'80'    IS OLD ACE                                   
         BZ    EEND107             NO, ERROR                                    
         B     C200                                                             
*                                                                               
EEND105  TM    RCONMODR+1,X'C0'    IS OLD ACE OR GRAPHNET                       
         BZ    C200                NO, OK                                       
EEND107  LA    R2,CONCACTH                                                      
         LA    R3,62               MUST CHANGE TO LIKE STATION                  
         B     ERROR                                                            
         SPACE 1                                                                
         DROP  R3                                                               
         EJECT                                                                  
* ADD ANY CHANGED POINTERS                                                      
C200     DC    0H'0'                                                            
         L     R6,AIO3              OLD                                         
         LA    R8,800(R6)          NEW                                          
         GOTO1 =A(ADDPTRS),DMCB,(RC),(R6),(R8),TWAKADDR,RR=Y                    
**************                                                                  
**************                                                                  
*        GOTO1 =A(CHECKPTR),RR=Y                                                
**************                                                                  
**************                                                                  
         SPACE 1                                                                
* PUT CHANGED RECORD                                                            
         MVC   KEY+28(4),TWAKADDR                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,AIO4                                                
         GOTO1 VPUTREC,DMCB,RCONREC                                             
*                                                                               
         GOTOR UPDPROS                                                          
*                                                                               
         GOTO1 =A(UPDMKGS),DMCB,(RC),RR=Y                                       
*                                  UPDATE MAKEGOOD RECORD PASSIVES              
         BAS   RE,CKCRAGY          CHECK IF CORPORATE AGENCY                    
*                                                                               
*MN                                                                             
         CLI   TWAACTMD,CONCHG_Q   CHANGE?                                      
         BNE   SKIPSEED                                                         
         TM    PROFILES+CNTDOCTB,CNTDOCTA                                       
         BZ    SKIPSEED                                                         
         CLI   SAVCTYP,C'S'                                                     
         BE    *+12                                                             
         CLI   SAVCTYP,0                                                        
         BNE   SKIPSEED                                                         
         CLI   CONTYPE,C'N'                                                     
         BE    PROCSEED                                                         
         CLI   CONTYPE,C'X'                                                     
         BE    PROCSEED                                                         
         CLI   TWARTS,C'0'                                                      
         BE    SKIPSEED                                                         
         CLC   CONTYPE,TWARTS                                                   
         BNE   SKIPSEED                                                         
PROCSEED EQU   *                                                                
         GOTO1 =A(SEEDBUY),DMCB,(RC),RR=Y                                       
SKIPSEED EQU   *                                                                
*MN                                                                             
         B     EXXMOD                                                           
*MN                                                                             
SAVCTYP  DS    CL1                                                              
*MN                                                                             
                                                                                
* ROUTINE TO GET EOM RECORD                                                     
*                                                                               
GETEOM   GOTO1 VHIGH                                                            
         BAS   RE,CHECK                                                         
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
         GOTO1 VGETREC,DMCB,IOAREA GET EOM RECORD                               
         BAS   RE,CHECK                                                         
         BR    R8                                                               
         SPACE 3                                                                
* ROUTINE TO GET STATION RECORD                                                 
*                                                                               
GETSTA   GOTO1 VHIGH                                                            
         BAS   RE,CHECK                                                         
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
         GOTO1 VGETREC,DMCB,IOAREA    GET STATION RECORD                        
         BAS   RE,CHECK                                                         
         BR    R5                                                               
         TITLE 'ADD CONTRACT RECORD'                                            
ADDCON   DS    0H                                                               
* IF CONACT=ADDR, ADDB, ADDS, OR ADDO, GO TO OVL TO EDIT THAT INPUT             
* BEFORE ADDING CONTRACT, UNLESS...                                             
*                                                                               
* COMMENT OUT FOR ADDRESSIBILITY AS COMBO NO LONGER SUPPORTED                   
*&&DO                                                                           
         CLI   TWACOMBO,0          IF COMBO,                                    
         BE    ADDC1A              VALIDATE EACH COMPONENT STATION              
         GOTO1 =A(VALCOMST),DMCB,(RC),RR=Y                                      
*&&                                                                             
*                                                                               
ADDC1A   DS    0H                                                               
         CLC   =C'ADDS',CONACT                                                  
         BE    ADDC2                                                            
*                                                                               
*        CLI   TWACOMBO,0          IF COMBO, SKIP THESE FOR NOW                 
*        BNE   ADDC3               WILL BRANCH TO APPROPRIATE COMBO OV          
*                                                                               
         CLC   =C'ADDR',CONACT                                                  
         BE    ADDC1                                                            
         CLC   =C'ADDB',CONACT                                                  
         BE    ADDC1                                                            
         CLC   =C'ADDO',CONACT                                                  
         BNE   ADDC3                                                            
*                                                                               
         GOTO1 VLOAD,DMCB,(X'49',0),=C'EDIT'                                    
         B     ADDC3                                                            
*                                                                               
ADDC1    DS    0H                                                               
         GOTO1 VLOAD,DMCB,(X'27',0),=C'EDIT'                                    
         B     ADDC3                                                            
*                                                                               
ADDC2    DS    0H                                                               
*                                                                               
* ALWAYS CALL X'71' MODULE FROM NOW ON FOR PENDING ADDS                         
*                                                                               
*&&DO                                                                           
         CLI   SASPADDS,C'Y'       'ADDS/SASP USER' FLAG SET?                   
         BE    ADDC2A              YES - CALL 71 OVERLAY                        
         GOTO1 VLOAD,DMCB,(X'45',0),=C'EDIT'                                    
         B     ADDC3                                                            
ADDC2A   EQU   *                                                                
*&&                                                                             
         GOTO1 VLOAD,DMCB,(X'71',0),=C'EDIT'                                    
*                                                                               
ADDC3    DS    0H                                                               
         MVI   RCONKTYP,12         CONTRACT RECORD TYPE                         
         MVC   RCONKREP,REPALPHA   ALPHA REP CODE                               
         MVC   RCONCREA,TODAY      CREATION OR BUYLINE 1 ADDED DATE             
         MVC   RCONHDRD,TODAY      HEADER CREATION DATE (NEVER CHANGED)         
         TM    RCONMODR+1,X'C0'    ONLY ACE/GRAPHNET SET MOD TO -1              
         BZ    *+8                                                              
         MVI   RCONMOD,X'FF'                                                    
         OI    TAREQ,1             T/A REQ IND FOR BASE                         
*                                                                               
*              GET NEXT REP CONTRACT NUMBER                                     
         ZAP   WORK(5),=P'99999999'                                             
         ZAP   WORK+5(5),=P'99999999'                                           
         CLC   CONACT,=C'ADDN'     TEST FOR ASSIGNED NUMBER ACTION              
         BNE   ADDCON2             NO                                           
         MVC   RCONKCON,INTCONNO   MOVE IN ASSIGNED NUMBER                      
         ZAP   WORK+10(5),=P'99999999'                                          
         MVO   WORK+10(5),INTCONNO FIND NINES COMPLEMENT OF                     
         SP    WORK+5(5),WORK+10(5) ASSIGNED NUMBER                             
         B     ADDCON3                                                          
         SPACE                                                                  
ADDCON2  XC    KEY,KEY                                                          
         MVI   KEY,X'8C'           PASSIVE POINTER KEY TYPE                     
         MVC   KEY+21(2),REPALPHA  ALPHA REP CODE                               
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(23),KEYSAVE     SAME REP?                                    
         BNE   *+10                                                             
*              GET NEXT CONTRACT NUMBER                                         
         MVO   WORK+5(5),KEY+23(4) K NUMBER                                     
         SP    WORK(5),WORK+5(5)   GET POSITIVE                                 
         AP    WORK(5),=P'1'       NEXT K NUMBER                                
         MVO   WORK+10(5),WORK(5)                                               
         MVC   RCONKCON,WORK+10    TO K KEY                                     
         SPACE                                                                  
ADDCON3  OC    TWAXCON,TWAXCON                                                  
         BNZ   *+10                                                             
         MVC   TWAXCON,RCONKCON                                                 
*                                                                               
         CLC   CONACT,=C'ADDR'                                                  
         BE    ADDCON5                                                          
         CLC   CONACT,=C'ADDB'                                                  
         BE    ADDCON5                                                          
         B     ADDCON10                                                         
         EJECT                                                                  
ADDCON5  DC    0H'0'                                                            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   ADDCON10                                                         
         SPACE 1                                                                
         USING RCONBPEL,R6                                                      
         OC    RCONBPRF,RCONBPRF                                                
         BNZ   ADDCON10                                                         
         MVC   RCONBPRF,TWAXCON                                                 
         DROP  R6                                                               
         SPACE 1                                                                
ADDCON10 DC    0H'0'                                                            
*                                                                               
* FOR ACE/GRAPHNET CONTRACTS, MARK UNCONFIRMED, & BUILD X'20' SEND EL           
*                                                                               
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    ADDCON30                                                         
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   ADDCON15                                                         
         SPACE 1                                                                
         USING RCONXEL,R6          EXTENDED DESCRIPTION ELEMENT                 
         OI    RCONCONF,X'80'      UNCONFIRMED                                  
         TM    TWASTAST,X'02'      IS 'DON'T SEND' ALLOWED ON STATION           
         BZ    *+8                                                              
         OI    RCONSTAT,X'02'      YES                                          
         DROP  R6                                                               
         B     ADDCON20                                                         
         SPACE 1                                                                
ADDCON15 XC    WORK2,WORK2         BUILD NEW ELEM. IN WORK2                     
         MVC   WORK2(2),=X'1F18'                                                
         OI    WORK2+6,X'80'       UNCONFIRMED                                  
         TM    TWASTAST,X'02'      IS 'DON'T SEND' ALLOWED ON STATION           
         BZ    *+8                                                              
         OI    WORK2+7,X'02'       YES                                          
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
         SPACE 1                                                                
ADDCON20 DC    0H'0'                ADD SEND ELEMENT                            
         XC    WORK2(RCONSN3Q),WORK2                                            
         MVI   WORK2,X'20'                                                      
         MVI   WORK2+1,RCONSN3Q    NEW ELEMENT LENGTH                           
         MVI   WORK2+4,X'10'       START STA VERS. NOT ADVANCED                 
         MVI   WORK2+5,1           SET REP VERSION NUMBER TO 1                  
         MVI   WORK2+14,0          SET STATION VER. NUMBER TO 0                 
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
*                                                                               
         GOTOR RCUAUDIT                                                         
         EJECT                                                                  
**********************************************************************          
* NEW COMBO CONTRACT WILL LOOP TO ADD THE NUMBER OF COMPONENT CONTRACTS         
* SPECIFIED.  THE X'17' COMBO INFO ELEMENT WILL BE ADDED TO ALL KS.             
* ALSO, CONTRACTS WILL BE ADDED HIGHEST # FIRST TO INSURED CONSECUTIVE          
* CONTRACT #S.                                                                  
**********************************************************************          
ADDCON30 DS    0H                                                               
         OC    TWACOMBO,TWACOMBO   CHECK IF COMBO CONTRACT                      
         BZ    ADDCON70                                                         
*                                                                               
         XC    WORK2,WORK2         YES, ADD COMBO INFO ELEMENT                  
         MVI   WORK2,X'17'         ELEMENT CODE                                 
*                                                                               
         ZIC   R4,TWACOMBO         COMPUTE ELEMENT LENGTH                       
         MH    R4,=H'9'            # OF STATIONS * 9 (LETTER + MED)             
         LA    R4,2(R4)            PLUS 2 FOR CODE AND LENGTH                   
         STC   R4,WORK2+1                                                       
*                                                                               
         LA    R4,WORK2+2          BUILD REST OF ELEMENT                        
         LA    R2,CONCMBSH                                                      
*                                                                               
ADDCON35 MVC   0(4,R4),8(R2)       CALL LETTERS                                 
         MVC   4(1,R4),13(R2)      MEDIA                                        
*                                                                               
         ZIC   R0,0(R2)            POINT TO SELECTION FIELD                     
         AR    R2,R0                                                            
*                                                                               
         CLI   8(R2),C'X'          IF STATION SELECTED, INCLUDE IN ELEM         
         BE    ADDCON40                                                         
*                                                                               
         B     ADDCON45                                                         
*                                                                               
ADDCON40 DS    0H                  GENERATE CONT # FOR EACH COMBO STA           
         MVC   5(4,R4),RCONKCON                                                 
         LA    R4,9(R4)                                                         
*                                                                               
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RCONKCON                                                 
         AP    WORK(5),=P'1'                                                    
         MVO   WORK+10(5),WORK(5)                                               
         MVC   RCONKCON,WORK+10                                                 
*                                                                               
ADDCON45 DS    0H                                                               
         ZIC   R0,0(R2)            BUMP TO STATION FIELD                        
         AR    R2,R0                                                            
         LA    RF,CONCMBLH         PASS THE END YET?                            
         CR    R2,RF                                                            
         BL    ADDCON35            NO, KEEP LOOPING                             
*                                                                               
ADDCON50 DS    0H                  ADD COMBO INFO ELEMENT                       
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
*                                                                               
         ZIC   R4,TWACOMBO         BEWARE, USING R4 FOR ADDING COMBO            
         MVC   TWACMBPT,TWACOMBO   COMBO K COUNTER, SO WE KNOW WHICH            
*                                    COMBO K WE ARE WORKING ON                  
ADDCON55 DS    0H                                                               
         CLC   =C'ADDO',CONACT     TESTING FOR ADDO                             
         BNE   ADDCON56                                                         
         GOTO1 VLOAD,DMCB,(X'4A',0),=C'EDIT'                                    
*                                                                               
ADDCON56 CLC   =C'ADDB',CONACT     TESTING FOR ADDB                             
         BNE   ADDCON57                                                         
         GOTO1 VLOAD,DMCB,(X'4B',0),=C'EDIT'                                    
*                                                                               
ADDCON57 CLC   =C'ADDR',CONACT     TESTING FOR ADDR                             
         BNE   ADDCON60                                                         
         GOTO1 VLOAD,DMCB,(X'4B',0),=C'EDIT'                                    
*                                                                               
ADDCON60 DS    0H                  ASSIGN COMPONENT STATIONS TO KS              
         LA    R6,RCONREC          POINT R6 TO X'17' COMBO ELEMENT              
         MVI   ELCODE,X'17'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LR    RF,R4               ELEMENT X'17' IS COMBO STATIONS              
         BCTR  RF,0                FIND CORRESPONDING STATION COMPONENT         
         MH    RF,=H'9'              AND ADD TO EACH CONTRACT GENERATED         
         LA    RF,2(RF)            BUMP PASS CODE AND LENGTH                    
         AR    R6,RF               SINCE WE ARE ADDING CONTRACT #'S IN          
         MVC   RCONKSTA,0(R6)        REVERSE, GET LAST STA FIRST                
*                                                                               
         ZAP   WORK(5),=P'0'       PUT OUT HIGHEST K# FIRST                     
         MVO   WORK(5),RCONKCON                                                 
         SP    WORK(5),=P'1'                                                    
         MVO   WORK+10(5),WORK(5)                                               
         MVC   RCONKCON,WORK+10                                                 
*                                                                               
         CLC   CONACT,=C'ADDR'     UPDATE BOP REFERENCE NUMBER                  
         BE    ADDCON66            FOR ADDR/ADDB                                
         CLC   CONACT,=C'ADDB'                                                  
         BNE   ADDCON68                                                         
                                                                                
ADDCON66 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   ADDCON68                                                         
                                                                                
         USING RCONBPEL,R6                                                      
         CLC   CONACT,=C'ADDB'                                                  
         BE    ADDCON67                                                         
         OC    RCONBPRF,RCONBPRF                                                
         BNZ   ADDCON68                                                         
         MVC   RCONBPRF,TWAXCON                                                 
         B     ADDCON68                                                         
*                                                                               
ADDCON67 DS    0H                                                               
         MVC   RCONBPRF,RCONKCON                                                
         DROP  R6                                                               
*                                                                               
ADDCON68 DS    0H                                                               
*                                  CHECK IF STATION HAS CLOSE OUT DATE          
*                                  THIS WILL ALSO MOVE GROUP/SUBGROUP           
*                                  TO THE CONTRACT HEADER                       
         GOTO1 =A(CHKCLOUT),DMCB,(RC),RR=Y                                      
         EJECT                                                                  
ADDCON70 DS    0H                                                               
         LA    R3,TWAGLVX                                                       
SWP      USING GLVXFRSY,R3                                                      
         CLC   =C'SWP',SWP.GLVXFRPR      FROM PROPOSER PROGRAM?                 
         BNE   ADDCON72                                                         
         TM    PROFILES+CNTPRPTB,CNTPRPTA   PROF 60                             
         BZ    ADDCON72                                                         
         LA    R6,RCONREC                                                       
         USING RCONREC,R6                                                       
         MVI   RCONTYPE,C'Z'                                                    
         DROP  SWP,R6                                                           
*                                                                               
ADDCON72 DS    0H                                                               
         GOTO1 VADDREC,DMCB,RCONREC                                             
         OC    TWACOMBO,TWACOMBO                                                
         BZ    ADDCON73                                                         
*                                                                               
         ZAP   WORK+15(5),=P'99999999' MAKE SURE TWACNUM GETS                   
         MVO   WORK+15(5),RCONKCON     CORRECT K# LATER IN THE ROUTINE          
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         B     ADDCON75                                                         
*                                                                               
ADDCON73 DS    0H                                                               
         CLC   CONACT,=C'ADDN'     TEST FOR SPECIAL ACTION                      
         BE    ADDCON75            ALREADY HAVE RIGHT NINES COMPLEMENT          
         SP    WORK+5(5),=P'1'                                                  
ADDCON75 MVO   WORK+15(5),WORK+5(5)                                             
         MVC   TWACNUM,WORK+15                                                  
         PACK  TWACNUM(1),WORK+18(1)    REVERSE THE COMPLIMENT                  
         PACK  TWACNUM+1(1),WORK+17(1)                                          
         PACK  TWACNUM+2(1),WORK+16(1)                                          
         PACK  TWACNUM+3(1),WORK+15(1)                                          
         MVC   TWACDTES,RCONDATE                                                
         MVC   TWAKADDR,KEY        SAVE CONTRACT DISK ADDRESS                   
* ADD PASSIVE PTRS                                                              
         L     R6,AIO3             OLD                                          
         LR    RE,R6                                                            
         XCEF  (RE),800                                                         
         MVC   0(27,R6),RCONREC    DO NOT ADD MASTER POINTER                    
* BUILD PASSIVE PTRS                                                            
         LA    R8,800(R6)                                                       
         GOTO1 =A(PTRS),DMCB,(RC),(R8),0,RR=Y                                   
* ADD PTRS                                                                      
         GOTO1 =A(ADDPTRS),DMCB,(RC),(R6),(R8),TWAKADDR,RR=Y                    
*                                                                               
         OC    TWACOMBO,TWACOMBO   LOOP IF COMBO                                
         BZ    ADDCON80                                                         
*                                                                               
         ZIC   RF,TWACMBPT         DECREMENT COMBO K COUNTER                    
         BCTR  RF,0                REMEMBER WE ARE ADDING COMBO K# FROM         
         STC   RF,TWACMBPT         HIGHEST TO LOWEST                            
*                                  (RIGHT TO LEFT IN X'17' ELEMENT)             
         BCT   R4,ADDCON55                                                      
         BAS   RE,CKCRAGY          CHECK IF CORPORATE AGY CODE. IF YES,         
*                                  FLAG CONTRACT ADDED FOR THIS AGY             
         B     ADCON100                                                         
*                                                                               
ADDCON80 DS    0H                                                               
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RCONKCON                                                 
         EDIT  (P5,WORK),(8,CONCNUM),ALIGN=LEFT                                 
         FOUT  CONCNUMH                                                         
         OI    CONCNUMH+4,X'20'    SET PRE-VALID BIT...                         
         BAS   RE,CKCRAGY          CHECK IF CORPORATE AGY CODE. IF YES,         
*                                  FLAG CONTRACT ADDED FOR THIS AGY             
         OC    TWACOMBO,TWACOMBO                                                
         BZ    EXXMOD                                                           
*                                                                               
ADCON100 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'17'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                 MUST HAVE THIS ELEMENT FOR COMBO             
         DC    H'0'                                                             
*                                                                               
         SR    R4,R4                                                            
         ZIC   R5,1(R6)                                                         
         SHI   R5,2                                                             
         LA    R2,L'RCONCBST+L'RCONCBCN                                         
         DR    R4,R2               NUMBER OF COMBOS                             
         BCTR  R5,0                                                             
*                                                                               
         USING RCONCBEL,R6                                                      
         ZAP   WORK(5),=P'0'       REPLACE WITH NEXT AVAILABLE K#               
         MVO   WORK(5),RCONCBCN                                                 
         EDIT  (P5,WORK),(8,CONCNUM),ALIGN=LEFT                                 
         OI    CONCNUMH+6,X'80'    XMIT                                         
         DROP  R6                                                               
*                                                                               
         LA    R6,7(R6)            POINT TO COMBO K #'S                         
         LA    R2,CONCMBCH                                                      
*                                                                               
ADCON105 DS    0H                  DISPLAY COMBO STATION CONTRACT #S            
         OC    8(L'CONCMBC,R2),8(R2)                                            
         BZ    ADCON110                                                         
         ZAP   WORK(5),=P'0'       REPLACE WITH NEXT AVAILABLE K#               
         MVO   WORK(5),0(4,R6)                                                  
         EDIT  (P5,WORK),(8,8(R2)),ALIGN=LEFT                                   
         LA    R6,9(R6)                                                         
*                                                                               
ADCON110 DS    0H                                                               
         OI    1(R2),X'20'         PROTECT FIELD                                
         OI    6(R2),X'80'         XMIT                                         
         LA    RF,CONCMBLH                                                      
         CR    R2,RF                                                            
         BE    EXXMOD                                                           
*                                                                               
         ZIC   RF,0(R2)            BUMP TO NEXT SELECTION FIELD                 
         AR    R2,RF                                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
*                                                                               
         CHI   R5,0                COUNTER = 0?                                 
         BNH   ADCON110            YES, SKIP                                    
         SHI   R5,1                COUNTER - 1                                  
         B     ADCON105                                                         
*                                                                               
CHECK    TM    DMCB+8,X'FD'        ALL BUT DELETE                               
         BCR   8,RE                                                             
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
* IF CORPORATE AGENCY RECORD, FLAG CONTRACT ADDED                               
*                                                                               
CKCRAGY  NTR1                                                                   
         CLC   RCONKAOF,MYSPACES                                                
         BNE   CCAGYX                                                           
         XC    IOAREA(32),IOAREA                                                
         MVI   RAGYKTYP,X'0A'                                                   
         MVC   RAGYKAGY(6),RCONKAGY                                             
         MVC   RAGYKREP,REPALPHA                                                
*                                                                               
         MVC   KEY(27),IOAREA                                                   
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   CCAGYX                                                           
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         OI    RAGYFLAG,X'04'      FLAG CONTRACT ADDED FOR THIS AGY             
*                                                                               
         GOTO1 VPUTREC,DMCB,IOAREA                                              
*                                                                               
CCAGYX   DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
       ++INCLUDE RECNTAUTOD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE REGENPRO                                                       
       ++INCLUDE REGENTEM                                                       
*MN                                                                             
       ++INCLUDE REGENDAR                                                       
*MN                                                                             
         DSECT                                                                  
       ++INCLUDE REGENSAL2                                                      
         DSECT                                                                  
       ++INCLUDE REGENSBLK                                                      
         EJECT                                                                  
       ++INCLUDE DDGLVXCTLD                                                     
         EJECT                                                                  
BROADTBL DSECT                                                                  
BRDTABLE DS    0CL7                                                             
         ORG   BRDTABLE                                                         
BRDSTART DS    CL3                 BINARY MONTH START DATE                      
BRDEND   DS    CL3                 BINARY MONTH END   DATE                      
BRDWEEKS DS    CL1                 NUM WEEKS IN PERIOD                          
BRDLEN   EQU   *-BRDSTART          LENGTH OF ENTRY                              
         EJECT                                                                  
T80210   CSECT                                                                  
*********************************************************************           
* RCUAUDIT - UPDATE RCU AUDIT ELEMENT                               *           
*********************************************************************           
RCUAUDIT NTR1  BASE=*,LABEL=*                                                   
         TM    TWASTREO,TWARCUQ    SCRIPT IN PROGRESS?                          
         BZ    RCUAUDX                                                          
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'3C'                                                     
         BAS   RE,GETEL                                                         
         BE    RCUAUD50                                                         
         XC    WORK2,WORK2                                                      
RCUAD    USING RCONCUEL,WORK2                                                   
         MVI   RCUAD.RCONCUCD,X'3C'                                             
         MVI   RCUAD.RCONCULN,RCONCULQ                                          
         MVC   RCUAD.RCONCUAL,TWARCUSA                                          
         MVC   RCUAD.RCONCUW#,TWARCUT#                                          
         MVI   RCUAD.RCONCUCT,1                                                 
         GOTO1 DATCON,DMCB,(5,0),(2,RCUAD.RCONCU1D)                             
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,12               SHIFT OFF SECONDS AND SIGN                   
         STCM  R1,3,RCUAD.RCONCU1T                                              
*                                                                               
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
         B     RCUAUD80                                                         
         DROP  RCUAD                                                            
*                                                                               
RCUAUD50 DS    0H                                                               
         USING RCONCUEL,R6                                                      
         ZIC   R1,RCONCUCT                                                      
         AHI   R1,1                                                             
         STC   R1,RCONCUCT                                                      
         OC    RCONCU4D,RCONCU4D                                                
         BZ    RCUAUD60                                                         
         MVC   RCONCU2D(8),RCONCU3D                                             
*                                                                               
RCUAUD60 DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,RCONCU4D)                                   
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,12               SHIFT OFF SECONDS AND SIGN                   
         STCM  R1,3,RCONCU4T                                                    
*                                                                               
         DROP  R6                                                               
*                                                                               
RCUAUD80 DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONXEL,R6                                                       
         MVC   RCONTRF,TWARCUT#                                                 
         DROP  R6                                                               
*                                                                               
RCUAUDX  DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* SALESMAN VALIDATION ROUTINE                                                   
*********************************************************************           
DOSALED  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,CONSALH                                                       
         LA    R3,SALERR                                                        
         TM    4(R2),X'20'         VALID SALESMAN?                              
         BO    DOSA0200                                                         
*                                                                               
         CLC   =C'SCHA',CONACT     CHANGE SALESPERSON ACTION?                   
         JE    DOSA0010                                                         
         CLC   =C'CHA',CONACT                                                   
         JNE   DOSA0010                                                         
         TM    PROFILES+CNTSPERB,CNTSPERQ    ALLOWED TO CHANGE SPERSON?         
         JZ    DOSA0010                                                         
         LA    R3,CHGERR           FIELD CANNOT BE CHANGED                      
         J     ERROR                                                            
*                                                                               
DOSA0010 DS    0H                                                               
*                                                                               
* IF SALESMAN CHANGES, IT DOESN'T AFFECT VERSION OR CONFIRMED STATUS            
*                                                                               
*&&DO                                                                           
         GOTO1 =A(PROCHK),RR=Y                                                  
         BNE   *+12                                                             
         LA    R3,563                                                           
         B     ERROR               NO CHANGE W/PROPOSALS                        
*&&                                                                             
         XC    IOAREA(32),IOAREA                                                
*                                                                               
         CLI   8(R2),C'='          BROWSE REQUEST?                              
         BNE   DOSA0020                                                         
         L     RE,4(RD)            SET TO RECNT00 RD                            
         L     RE,4(RE)                                                         
         L     RE,4(RE)                                                         
         CLI   TWACOMBO,0                                                       
         BE    *+8                                                              
         L     RE,4(RE)            COMBOS NEED 1 MORE POP                       
         GOTOX (RFBROWSE,VREPFACS),DMCB,ACOMFACS,(RE),(R2),CONSALNH,   +        
               (X'80',C' SAL'),0                                                
         DC    H'0'           BROWSE SHOULD HAVE TAKEN IT FROM HERE             
*                                                                               
DOSA0020 DS    0H                                                               
         GOTO1 SCANNER,DMCB,(R2),(1,WORK2),C',=,/'                              
         CLI   4(R1),1                                                          
         BNE   DOSA0025                                                         
         CLI   WORK2+0,3                                                        
         BH    DOSA0025                                                         
*                                                                               
         MVI   RSALKTYP,6          SALESMAN REC TYPE                            
         MVC   RSALKREP,REPALPHA   REP CODE                                     
         MVC   RSALKSAL,WORK2+12                                                
         MVC   KEY,IOAREA                                                       
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    DOSA0030                                                         
*                                                                               
DOSA0025 DS    0H                                                               
         TM    TWASTREO,TWARCUQ    SCRIPT IN PROGRESS?                          
         BZ    ERROR                                                            
*                                                                               
         XC    IOAREA(32),IOAREA                                                
         MVI   RSALKTYP,6          SALESMAN REC TYPE                            
         MVC   RSALKREP,REPALPHA   REP CODE                                     
         MVC   RSALKSAL,=C'WOS'    SCRIPT DEFAULT                               
         MVC   KEY,IOAREA                                                       
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
*                                                                               
DOSA0030 DS    0H                                                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   TWASALTL,RSALTEL                                                 
         MVC   WSALEXP,RSALNAME                                                 
         FOUT  CONSALNH,RSALNAME,19 SALESMAN NAME                               
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1E'        FLAGS ELEMENT                                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONRFEL,R6                                                      
         NI    RCONRF1,X'FF'-X'80' DEFAULT TO NO TEAM OVERRIDE                  
*                                                                               
         MVC   RCONTEM,RSALTEAM    TEAM                                         
         CLI   WORK2+1,0           OVERRIDE TEAM?                               
         BE    DOSA0040            NO                                           
         CLC   RCONTEM,WORK2+22    SAME TEAM ANYWAY?                            
         BE    DOSA0040            YES - NOT REALLY OVERRIDE                    
         MVC   RCONTEM,WORK2+22    TEAM OVERRIDE                                
         OI    RCONRF1,X'80'       OVERRIDE FLAG                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'05'                                                        
         MVC   KEY+RTEMKREP-RTEMREC(2),REPALPHA                                 
         MVC   KEY+RTEMKTEM-RTEMREC(2),WORK2+22                                 
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    DOSA0040                                                         
         LA    R3,155              INVALID TEAM                                 
         B     ERROR                                                            
*                                                                               
DOSA0040 DS    0H                                                               
         TM    PROFILES+CNTPSALB,CNTPSALA   PROF 49 (PAY S/P USED)              
         BZ    DOSA0060            NOT USED                                     
*                                                                               
         CLC   CONACT(3),=C'ADD'   CONTRACT 'ADD' ACTION?                       
         BNE   DOSA0060            NO  - DON'T MODIFY CODE                      
*                                                                               
         MVC   RCONRPSP,RSALKSAL   USED - INSERT SALESPERSON CODE               
         MVC   RCONRSPO,RSALOFF    INSERT SALESPERSON OFFICE CODE               
DOSA0060 DS    0H                                                               
         DROP  R6                                                               
         MVC   RCONSAL,RSALKSAL    SALESMAN'S CODE                              
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         MVC   WORK(L'RSALOFF),RSALOFF                                          
*                                                                               
*  TEMPORARILY BYPASS THE OFFTEAM TESTING FOR KATZ TV.                          
*  TEMPORARILY BYPASS THE OFFTEAM TESTING FOR MILLENNIUM (SZ)                   
*  TEMPORARILY BYPASS THE OFFTEAM TESTING FOR TEST FILES (KATZ TV)              
*                                                                               
* REASON: DIRECT RESPONSE GROUP AT KATZ SELLS ACROSS ALL DIVISIONS.             
* BECAUSE OF THAT, ALL OF THE DIRECT RESPONSE SALESPEOPLE ARE NOT               
* REALLY ATTACHED TO A TEAM (THEY CAN'T BE), SO THEY ALL BELONG TO TEAM         
* "T."  THAT MEANS THAT WE COULD NOT USE THE OFFTEAM FEATURE, BECAUSE           
* THEN THE DIRECT RESPONSE PEOPLE WOULD NOT BE ABLE TO ENTER ANYTHING.          
*                                                                               
         CLC   =C'AM',REPALPHA     KATZ AMERICAN?                               
         BE    DOSA0140            YES - DON'T TEST OFF/TEAM SETUP              
         CLC   =C'CQ',REPALPHA     KATZ CONTINENTAL?                            
         BE    DOSA0140            YES - DON'T TEST OFF/TEAM SETUP              
         CLC   =C'NK',REPALPHA     KATZ NATIONAL?                               
         BE    DOSA0140            YES - DON'T TEST OFF/TEAM SETUP              
         CLC   =C'SZ',REPALPHA     MILLENNIUM?                                  
         BE    DOSA0140            YES - DON'T TEST OFF/TEAM SETUP              
*        CLC   =C'TV',REPALPHA     KATZ TV TEST FILES?                          
*        BE    DOSA0140            YES - DON'T TEST OFF/TEAM SETUP              
*                                                                               
* VALIDATE IF SALESPERSON'S OFF/TEAM CODE MATCHES THE STATION'S                 
*                                                                               
         OC    TWASTOTC,TWASTOTC   THERE ARE NONE, THEN SKIP                    
         BZ    DOSA0140                                                         
         LA    R4,15               15 SETS MAXIMUM                              
         LA    R5,TWASTOTC                                                      
*                                                                               
DOSA0080 CLC   RSALOFF,0(R5)       IF OFFICE IS NOT IN                          
         BE    DOSA0100            OFFTEAM LIST, SKIP VALIDATION                
         LA    R5,4(R5)                                                         
         BCT   R4,DOSA0080                                                      
         B     DOSA0140                                                         
*                                                                               
DOSA0100 DS    0H                  OFFICE IS IN OFFTEAM LIST,                   
         CLC   RSALOFF,0(R5)       VALIDATE TEAM                                
         BNE   DOSA0120                                                         
***>     CLC   RSALTEAM,2(R5)                                                   
         CLC   RCONTEM,2(R5)                                                    
         BE    DOSA0140            SALESPERSON AND STATION OFF/TEAM             
*                                                                               
DOSA0120 LA    R5,4(R5)                                                         
         BCT   R4,DOSA0100                                                      
         LA    R3,288                                                           
         B     ERROR               CODE MISMATCH                                
*                                                                               
DOSA0140 EQU   *                                                                
         OC    RSALLEAV,RSALLEAV   ANY LEAVE DATE?                              
         BZ    OFFED               NO  - SALESPERSON STILL ACTIVE               
         GOTO1 DATCON,DMCB,(5,FULL),(3,FULL)                                    
*                                  YES - CHECK AGAINST TODAY'S DATE             
         CLC   RSALLEAV,FULL       LEAVE DATE VS TODAY'S DATE                   
         BH    OFFED               NOT GONE YET: LD > TD                        
         LA    R3,SALLEFT          GONE - REJECT ENTRY                          
         B     ERROR               GO TO ERROR RTN                              
*                                                                               
DOSA0200 XIT1                                                                   
*                                                                               
SALLEFT  EQU   407                 SALESPERSON GONE ERROR CODE                  
         EJECT                                                                  
OFFED    DS    0H                                                               
*                                                                               
*        GOTO1 =A(ISSTEXCL),RR=Y                                                
*                                                                               
         XC    IOAREA(32),IOAREA                                                
         MVI   ROFFKTYP,4          OFFICE REC TYPE                              
         MVC   ROFFKREP,REPALPHA   REP CODE                                     
         MVC   ROFFKOFF,WORK                                                    
         MVC   KEY,IOAREA                                                       
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
*                                                                               
         CLI   TWAOFFC,C'*'        TEST IF DDS TERMINAL                         
         BE    OFF20                                                            
         CLC   TWAACCS(2),=C'O='   TEST FOR OFFICE RESTRICTION                  
         BNE   OFF20                                                            
         TM    TWAAUTH,X'80'       TEST IF TERMINAL ALLOWED ACCESS              
         BO    OFF20               TO ALL OFFICES                               
         CLC   ROFFKOFF,TWAACCS+2  ELSE, COMPARE OFFICES                        
         BE    OFF20                                                            
         LA    R3,55               ERROR, SECURITY LOCKOUT                      
         B     ERROR                                                            
*                                                                               
OFF20    GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   TWAOFAD1,ROFFADD1                                                
         MVC   TWAOFAD2,ROFFADD2                                                
         MVC   TWAOFSTT,ROFFSTT                                                 
         MVC   TWAOFZIP,ROFFZIP                                                 
         MVC   WOFFEXP,ROFFNAME                                                 
*                                                                               
         FOUT  CONOFFNH,ROFFNAME,16                                             
*                                                                               
         MVC   WORK(2),RCONKOFF                                                 
         MVC   RCONKOFF,ROFFKOFF                                                
         CLC   RCONKOFF,WORK       DIFFERENT OFFICE??                           
         BE    DOSA0200            NEED TO CHANGE MAKEGOOD OFFER RECORD         
*                                                                               
         GOTO1 =A(CHGMGREC),DMCB,(RC),RR=Y                                      
         B     DOSA0200                                                         
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE CONTRACT                                                               
***********************************************************************         
         DS       0H                                                            
DELETE   NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*DELCNT*'                                                    
*                                                                               
         LA    R2,CONCACTH                                                      
         FOUT  CONCACTH,=C'DEL',3                                               
*                                                                               
         GOTO1 =A(PROCHK),RR=Y                                                  
         BNE   *+12                                                             
         LA    R3,563                                                           
         B     ERROR                                                            
*                                                                               
         LA    R6,RCONREC          FOR DARE ORDERS                              
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   DEL05                                                            
*                                                                               
         USING RCONDREL,R6                                                      
         OC    RCONDRLK,RCONDRLK   LINKED TO AN AGENCY ORDER?                   
         BZ    DEL05                                                            
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    DEL03                                                            
         TM    RCONDRF2,X'08'      REMOVED??                                    
         BO    DEL05                                                            
         DROP  R6                                                               
*                                                                               
DEL03    DS    0H                                                               
         LA    R6,RCONREC          IF CONTRACT HAS NOT BEEN CONFIRMED           
         MVI   ELCODE,X'1F'        ACTION DELETE IS NOT ALLOWED                 
         BAS   RE,GETEL            SINCE DARE AGENCY ORDER IS STILL             
         BNE   ERROR               LINKED TO THIS CONTRACT                      
         USING RCONXEL,R6                                                       
         LA    R3,453                                                           
         TM    RCONCONF,X'40'+X'20'                                             
         BZ    ERROR                                                            
         DROP  R6                                                               
*                                                                               
DEL05    DS    0H                                                               
         LA    R3,ACTERR                                                        
         CLI   RCONTYPE,C'N'       TYPE N OR X CONTRACT CANNOT                  
         BE    ERROR                DELETE CONTRACT (REP TO SPOT K)             
         CLI   RCONTYPE,C'X'                                                    
         BE    ERROR                                                            
*                                                                               
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET                                 
         BZ    DEL25                                                            
         SPACE 1                                                                
* ACE/GRAPHNET CAN'T DELETE CONTRACT IF IT'S EVER BEEN SENT                     
         SPACE 1                                                                
         LA    R3,197              ERROR, ORDER HAS BEEN SENT                   
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    DEL10                                                            
         DC    H'0'                                                             
*                                                                               
* FOR CONVERTED ORDERS, SKIP CHECK IF LAST SENT DATE = CREATION DATE            
*                                                                               
DEL10    DS    0H                                                               
         TM    RCONMODR+1,X'10'    CONVERTED CONTRACT?                          
         BZ    DEL20                                                            
         GOTO1 DATCON,DMCB,(2,6(R6)),(3,WORK2)                                  
         CLC   RCONHDRD,WORK2                                                   
         BNE   DEL20               IF REP SEND DATE = CREATION DATE             
         CLC   =C'120000',8(R6)    AND TIME IS 120000                           
         BE    DEL25               SKIP SENT DATE CHECK                         
*                                                                               
DEL20    DS    0H                                                               
         OC    6(2,R6),6(R6)                                                    
         BNZ   ERROR               ORDER HAS BEEN SENT                          
         SPACE 1                                                                
DEL25    LA    R3,DELERR           HISTORICAL DATA                              
         CLI   BYTE3,1             ANY BUCKETS?                                 
         BE    ERROR                                                            
* CHECK FOR BUYS                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,11                                                           
         MVC   KEY+16(2),REPALPHA                                               
         MVC   KEY+18(4),TWACNUM                                                
         GOTO1 VHIGH                                                            
         B     DEL52                                                            
DEL50    GOTO1 VSEQ                                                             
DEL52    CLC   KEY(22),KEYSAVE                                                  
         BNE   DEL100                                                           
         CLI   KEY+26,255          PLAN LINE?                                   
         BNE   ERROR                                                            
         B     DEL50                                                            
DEL100   OI    RCONCNTL,X'80'                                                   
         MVC   RCONMODD,TODAY                                                   
         NI    CONCNUMH+4,X'DF'                                                 
* DELETE POINTER                                                                
         MVC   KEY,RCONREC                                                      
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VREAD                                                            
         OI    KEY+27,X'80'                                                     
         GOTO1 VWRITE                                                           
         GOTO1 VPUTREC,DMCB,RCONREC                                             
*                                                                               
* DECREMENT PRD CDE LOCK COUNTER                                                
*                                                                               
         CLC   RCONPRD,MYSPACES    PRODUCT CODE??                               
         BE    EXXMOD                                                           
         MVC   SVADV,RCONKADV                                                   
         MVC   SVPRD,RCONPRD                                                    
         GOTO1 =A(PRDLOCK),DMCB,(RC),RR=Y  DECREMENT PRODUCT CODE LOCK          
*                                            COUNTER                            
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECKS IF THERE ARE PROPOSALS AGAINST A CONTRACT                              
*                                                                               
* CC ON RETURN                                                                  
*    EQUAL - PROPOSALS                                                          
*    NE    - NO PROPOSALS                                                       
***********************************************************************         
         DS    0H                                                               
PROCHK   NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*PROCHK*'                                                    
*                                                                               
         LA    R6,KEY                                                           
         USING RPROKEY,R6                                                       
         XC    RPROKEY,RPROKEY                                                  
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,REPALPHA                                                
         ZAP   WORK+15(5),=P'0'                                                 
         MVO   WORK+15(5),RCONKCON                                              
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK(5),WORK+5(5)                                                
         MVC   RPROKCON,WORK                                                    
         DROP  R6                                                               
         GOTO1 VHIGH                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(RPROKPRO-RPROKEY),KEYSAVE                                    
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECKS IF THE CONTRACT IS UDED BY SELWIN                                      
*                                                                               
* CC ON RETURN                                                                  
*    EQUAL - SELWIN                                                             
*    NE    - !SELWIN                                                            
***********************************************************************         
         DS    0H                                                               
SLWCHK   NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*SLWCHK*'                                                    
*                                                                               
         LA    R6,RCONELEM                                                      
SLWC0010 DS    0H                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             END OF RECORD?                               
         BE    SLWCNO              YES                                          
         CLI   0(R6),X'1E'         RANDOM FLAG ELEMENT?                         
         BNE   SLWC0010            NO                                           
*                                                                               
         USING RCONRFEL,R6                                                      
         TM    RCONRF1,X'40'       HAS SELWIN BEEN HERE?                        
         BZ    SLWCNO              NO                                           
         DROP  R6                                                               
*                                                                               
         CR    RB,RB                                                            
         B     *+6                                                              
SLWCNO   LTR   RB,RB                                                            
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* GETCON - IMPORT COMMENT TEXT FROM OTHER CONTRACT                              
*                                                                               
GETCMT   NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'CMT=',8(R2)       HAVE 'CMT=NNNNNNNN' SYNTAX?                 
         BE    GCMT010              YES                                         
         SR    R0,R0                NO - SET CC                                 
         B     EXXMOD               BYE                                         
*                                                                               
GCMT010  DS    0H                                                               
         GOTO1 SCANNER,DMCB,(R2),AIO4                                           
         LA    R3,2                                                             
         CLI   4(R1),1                                                          
         BNE   ERROR                                                            
         L     R4,AIO4              SCANNER OUTPUT                              
         TM    3(R4),X'80'         NUMERIC?                                     
         BZ    ERROR                                                            
*                                                                               
         XC    KEY,KEY              LOOKUP CONTRACT                             
         MVI   KEY,X'8C'                                                        
         MVC   KEY+21(2),REPALPHA                                               
         GOTOX (RFCONNUM,VREPFACS),DMCB,(7,8(R4)),(2,KEY+23)                    
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
         GOTO1 VGETREC,DMCB,AIO4                                                
*                                                                               
         L     R6,AIO4                                                          
         CLC   RCONKCON,23(R6)     SAME K?                                      
         BE    ERROR                                                            
*                                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    GCMT020                                                          
         LA    R3,759              NO CMT ON SOURCE K                           
         B     ERROR                                                            
GCMT020  DS    0H                                                               
         GOTO1 VADDELEM,DMCB,RCONREC,0(R6)                                      
         BAS   RE,NEXTEL                                                        
         BE    GCMT020                                                          
         LTR   RB,RB                                                            
         B     EXXMOD                                                           
         LTORG                                                                  
***********************************************************************         
* CHECKS THE FLIGHT DATE AGAINST ALTERNATE CALENDARS, GOES TO ERROR             
* IF THE FLIGHT IS NOT COVERED                                                  
***********************************************************************         
         DS    0H                                                               
ALTCAL   NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*ALTCAL*'                                                    
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),ACOMFACS                                                 
         MVC   WORK+4(2),REPALPHA                                               
*                                                                               
         OC    TWACOMBO,TWACOMBO   COMBO?                                       
         BZ    ALTCAL10            NO - SKIP COMBO SETUP                        
*                                                                               
         MVC   WORK+10(5),RCONKSTA SAVE CURRENT STATION                         
         LA    R2,CONCMBSH         POINT TO COMPONENTS                          
ALTCAL02 DS    0H                                                               
         LR    R4,R2               CHECK IF STATION IS SELECTED                 
         ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         CLI   8(R4),C'X'          SELECTED STATION?                            
         BNE   ALTCAL50            NO - NEXT STATION                            
*                                                                               
         MVC   RCONKSTA(4),8(R2)       CALL LETTER                              
         MVC   RCONKSTA+4(1),13(R2)    BAND                                     
*                                                                               
ALTCAL10 DS    0H                                                               
         MVI   WORK+08,0                                                        
         OC    RCONKREP,RCONKREP                                                
         BNZ   *+8                                                              
         MVI   WORK+08,X'FF'                                                    
*                                                                               
         GOTOX (RFVALTCL,VREPFACS),DMCB,(WORK+08,RCONREC),VGTBROAD,0,  X        
               WORK                                                             
         BE    *+12                                                             
         L     R3,0(R1)                                                         
         B     ERROR                                                            
*                                                                               
         OC    TWACOMBO,TWACOMBO   COMBO?                                       
         BZ    ALTCALX             NO EXIT                                      
*                                                                               
ALTCAL50 DS    0H                                                               
         MVC   RCONKSTA,WORK+10    RESET ORIGNIAL STATION                       
*                                                                               
         LA    RF,CONCMBLH         IF 4TH AND LAST STATION                      
         CR    R2,RF               WE ARE DONE                                  
         BNL   ALTCALX                                                          
*                                                                               
         ZIC   R0,0(R2)            ELSE, BUMP TO NEXT CALL LETTER               
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     ALTCAL02                                                         
*                                                                               
ALTCALX  DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   AGYEDMOD:  AGENCY EDIT NMOD.  TESTS AGENCY CODE ENTERED.  IF OKAY,          
*        RETURNS CC = ZERO.  IF BAD, RETURNS CC NOT = ZERO.                     
*                                                                               
AGYEDMOD NTR1  BASE=*,LABEL=*                                                   
         MVI   UPVER,0    ACE/GRAPHNET FLAG TO UP VERSION & UNCONFIRM           
         LA    R2,CONAGYH                                                       
         LA    R3,AGYERR                                                        
         TM    4(R2),X'20'                                                      
         BO    AGYE0400            EXIT CC= ZERO                                
         MVI   UPVER,1      ACE/GRAPHNET FLAG - UP VERSION & UNCONFIRM          
         XC    IOAREA(32),IOAREA                                                
*                                                                               
         CLI   8(R2),C'='          BROWSE REQUEST?                              
         BNE   AGYE0020                                                         
         L     RE,4(RD)            SET TO RECNT00 RD                            
         L     RE,4(RE)                                                         
         L     RE,4(RE)                                                         
         CLI   TWACOMBO,0                                                       
         BE    *+8                                                              
         L     RE,4(RE)            COMBOS NEED 1 MORE POP                       
         GOTOX (RFBROWSE,VREPFACS),DMCB,ACOMFACS,(RE),(R2),CONAGYNH,   +        
               (0,C' AGY'),0                                                    
         DC    H'0'           BROWSE SHOULD HAVE TAKEN IT FROM HERE             
*                                                                               
AGYE0020 DS    0H                                                               
         MVI   RAGYKTYP,10         REC TYPE                                     
         GOTO1 VMOVE                                                            
         LA    RE,WORK                                                          
*                                                                               
         MVI   RAGYKAGY,C' '                                                    
         MVC   RAGYKAGY+1(5),RAGYKAGY                                           
*                                                                               
* CHECK FOR AGENCY OFFICE                                                       
         CLI   0(RE),C'-'                                                       
         BE    AGYE0040                                                         
         CLI   0(RE),C' '                                                       
         BE    AGYE0060                                                         
         LA    RE,1(RE)                                                         
         B     *-20                                                             
*              AGENCY OFFICE                                                    
AGYE0040 MVC   RAGYKAOF,1(RE)      AGENCY OFFICE                                
         LA    RF,WORK+1                                                        
         SR    RE,RF                                                            
         EX    RE,MOVEAGY                                                       
         B     AGYE0080                                                         
*                                                                               
MOVEAGY  MVC   RAGYKAGY(0),WORK                                                 
*                                                                               
AGYE0060 MVC   RAGYKAGY(4),WORK                                                 
*                                                                               
AGYE0080 DS    0H                                                               
         MVC   RAGYKREP,REPALPHA                                                
         MVC   KEY,IOAREA                                                       
         ST    R3,FULL             SAVE CURRENT ERROR MESSAGE                   
         CLC   =C'V4',REPALPHA     FOX NETWORK:  TEST REP?                      
         BE    AGYE0100            NO  - DON'T CHECK FOR LOCKOUT                
         CLC   =C'FN',REPALPHA     FOX NETWORK?                                 
         BNE   AGYE0160            NO  - DON'T CHECK FOR LOCKOUT                
AGYE0100 EQU   *                                                                
         LA    R3,SPECCHRS                                                      
         B     AGYE0120                                                         
SPECCHRS DC    C'#$%=*+!@><()-":'                                               
*                                  SPECIAL CONVERSION VALUES                    
         DC    X'FFFF'             DELIMITER                                    
         DS    0F                                                               
AGYE0120 EQU   *                                                                
         CLI   0(R3),X'FF'         DELIMITER REACHED?                           
****>>>> BE    AGYE0160            YES - NOT SPECIAL CHARACTER                  
*                                                                               
         B     AGYE0160            PERMIT USE IN ALL CASES                      
*                                                                               
         CLC   RAGYKAGY+3(1),0(R3)                                              
*                                  SPECIAL CHARACTER FOUND?                     
         BE    AGYE0140            YES - SEND BACK MESSAGE                      
         LA    R3,1(R3)                                                         
         B     AGYE0120            GO BACK FOR NEXT CODE                        
AGYE0140 EQU   *                                                                
         LA    R3,NOCONCDE         SET ERROR MESSAGE                            
         B     AGYE0500            EXIT CC NOT ZERO                             
NOCONCDE EQU   675                                                              
AGYE0160 EQU   *                                                                
         L     R3,FULL             RESET PRIOR ERROR MESSAGE                    
         GOTO1 VHIGH                                                            
*        CLC   KEY(25),KEYSAVE                                                  
*        BNE   AGYE0500                                                         
*        CLC   KEY+25(2),REPALPHA                                               
*        BE    AGYE0180                                                         
*        MVC   KEYSAVE+25(2),=C'ZZ'                                             
*        CLC   KEY+25(2),=C'ZZ'                                                 
*        BE    AGYE0180                                                         
*        MVC   KEY+25(2),=C'ZZ'                                                 
*        GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    AGYE0180                                                         
         TM    TWASTREO,TWARCUQ    SCRIPT IN PROGRESS?                          
         BZ    AGYE0500                                                         
*                                                                               
* CHECK IF WE HAVE ALREADY TRIED TO ASSIGN A DEFAULT VALUE                      
*                                                                               
         CLC   =C'WOAG  ',KEYSAVE+RAGYKAGY-RAGYKEY                              
         BE    AGYE0500                                                         
         XC    IOAREA(32),IOAREA                                                
         MVI   RAGYKTYP,10         REC TYPE                                     
         MVC   RAGYKAGY(6),=C'WOAG  '                                           
         B     AGYE0080            ASSIGN DEFAULT AND GO AGAIN                  
*                                                                               
* CHECK IF DEFAULT                                                              
AGYE0180 GOTO1 VGETREC,DMCB,IOAREA                                              
AGNOUSED EQU   902                                                              
AGADMISS EQU   515                                                              
         CLC   =C'ADD',CONACT      ON ADD ONLY                                  
         BNE   AGYE0190                                                         
         LA    R3,AGNOUSED                                                      
         TM    RAGYFLAG,X'02'      IS 'DO NOT USE' SET FOR AGENCY?              
         BO    AGYE0500            YES - EXIT WITH ERROR                        
*                                                                               
AGYE0190 EQU   *                                                                
         CLI   TWAACTMD,CONCHG_Q   CHANGE?                                      
         BNE   AGYE0195                                                         
         TM    CONAGYH+4,X'20'     AGENCY CHANGED?                              
         BO    AGYE0195            NO                                           
         LA    R3,AGNOUSED                                                      
         TM    RAGYFLAG,X'02'      IS 'DO NOT USE' SET FOR AGENCY?              
         BO    AGYE0500            YES - EXIT WITH ERROR                        
AGYE0195 EQU   *                                                                
         LA    R3,AGADMISS         SET 'AGENCY ADDR MISSING' ERROR              
         CLC   RAGYADD1(15),=C'**NOT ENTERED**'                                 
         BNE   AGYE0200            AGENCY ADDRESS ENTERED                       
         CLC   RAGYADD2(15),=C'**NOT ENTERED**'                                 
         BE    AGYE0500            AGENCY ADDRESS MISSING                       
AGYE0200 EQU   *                                                                
         MVC   WAGYEXP,RAGYNAM1    SAVE EXPANSION FOR DISPLAY                   
         CLC   KEY+23(2),MYSPACES                                               
         BNE   AGYE0240                                                         
         SPACE 2                                                                
* DEFAULT - CHECK IF OFFICE EXISTS                                              
         LA    R3,AODERR                                                        
         MVC   KEY+25(2),IOAREA+25                                              
AGYE0220 GOTO1 VSEQ                                                             
         CLC   KEY(23),IOAREA      SAME AGENCY?                                 
         BNE   AGYE0240                                                         
         CLC   KEY+23(2),MYSPACES                                               
         BE    AGYE0220                                                         
         CLC   KEY+25(2),IOAREA+25                                              
         BE    AGYE0500                                                         
         B     AGYE0220                                                         
AGYE0240 FOUT  CONAGYNH,RAGYNAM1,20                                             
         MVC   RCONKAGY(6),RAGYKAGY                                             
*                                                                               
         XC    CONARSK,CONARSK     ALWAYS CLEAR CREDIT RISK FIELD               
         OC    RAGYRISK,RAGYRISK   NO RATING ASSIGNED, ASSUM OK                 
         BZ    AGYE0260                                                         
         CLI   RAGYRISK,1          RISK=OK, SHOW NOTHING                        
         BE    AGYE0260                                                         
         MVC   CONARSK(9),=C'AGY RISK='  SHOW RISK                              
         LA    R2,CONARSK+9                                                     
         EDIT  RAGYRISK,(1,(R2))                                                
AGYE0260 OI    CONARSKH+6,X'80'    XMIT                                         
*                                                                               
* DELETE EXTENDED AGENCY ADDRESSES FOR KATZ ADDED DURING CONVERSION             
*                                                                               
         TM    RCONMODR+1,X'10'    FOR KATZ CONVERTED CONTRACT ONLY             
         BZ    AGYE0400                                                         
         GOTO1 VDELELEM,DMCB,(X'70',RCONREC)                                    
         GOTO1 VDELELEM,DMCB,(X'71',RCONREC)                                    
         GOTO1 VDELELEM,DMCB,(X'72',RCONREC)                                    
         GOTO1 VDELELEM,DMCB,(X'73',RCONREC)                                    
AGYE0400 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         B     AGYE0600                                                         
AGYE0500 EQU   *                                                                
         ST    R2,DUB              SAVE ERROR ADDRESS                           
         ST    R3,DUB+4            SAVE ERROR RETURN CODE                       
         LA    R0,1                SET CC NOT = ZERO                            
AGYE0600 EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***>>>                                                                          
***********************************************************************         
* UPDATE PROPOSAL X'02' SWITCH ELEMENTS AND THE PRIMARY STATION ELEMENT         
***********************************************************************         
UPDPROS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RE,KEY                                                           
         XC    KEY,KEY                                                          
*                                                                               
         USING RPROKEY,RE                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,REPALPHA                                                
         ZAP   WORK+15(5),=P'0'                                                 
         MVO   WORK+15(5),RCONKCON                                              
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK(5),WORK+5(5)                                                
         MVC   RPROKCON,WORK                                                    
         XC    RPROKPRO,RPROKPRO                                                
         DROP  RE                                                               
*                                                                               
UPDPRO2  GOTO1 VHIGH                                                            
         CLC   KEY(RPROKPRO-RPROKMST),KEYSAVE                                   
         BNE   UPDPROX                                                          
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,AIO4                                                
*                                                                               
         L     R6,AIO4                                                          
         LA    R6,RPROR1ST-RPROHDRD(R6)                                         
         MVI   ELCODE,RPRSWELQ                                                  
         BAS   RE,FIRSTEL                                                       
         BNE   UPDPRO30            NO SWITCH ELEMENT TO CHANGE                  
*                                                                               
         MVC   WORK2,KEY                                                        
PK       USING RPROKEY,WORK2                                                    
*                                                                               
         LR    R4,R6                                                            
         USING RPRSWELD,R4                                                      
         CLC   RPRSWSAL,RCONSAL                                                 
         BNE   *+14                                                             
         CLC   RPRSWSTA,RCONKSTA                                                
         BE    UPDPRO9                                                          
*                                                                               
         LA    RE,KEY                                                           
         XC    KEY,KEY                                                          
         USING RPROKEY,RE                                                       
         MVI   RPROPTYP,RPROPTYQ                                                
         MVI   RPROPSTY,RPROPSBQ                                                
         MVC   RPROPRCD,REPALPHA                                                
         MVC   RPROPSAL,RPRSWSAL                                                
         MVC   RPROPSTA,RPRSWSTA                                                
         MVC   RPROPCON,PK.RPROKCON                                             
         MVC   RPROPPRO,PK.RPROKPRO                                             
         DROP  RE                                                               
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RPROKMST),KEYSAVE                                          
         BE    *+14                                                             
         MVC   KEY,KEYSAVE                                                      
         B     UPDPRO4                                                          
*                                                                               
         OI    KEY+RPROKCTL-RPROKEY,X'80'       DELETED                         
         GOTO1 VWRITE                                                           
*                                                                               
UPDPRO4  LA    RE,KEY              NEW PASSIVE KEY                              
         USING RPROKEY,RE                                                       
         MVC   RPROPSAL,RCONSAL                                                 
         MVC   RPROPSTA,RCONKSTA                                                
         NI    RPROKCTL,X'FF'-X'80'                                             
         OC    RPROKDA,RPROKDA                                                  
         BNZ   *+10                                                             
         MVC   RPROKDA,WORK2+(RPROKDA-RPROKEY)                                  
         DROP  RE                                                               
*                                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RPROKMST),KEYSAVE                                          
         BNE   UPDPRO8                                                          
         NI    KEY+(RPROKCTL-RPROKEY),X'FF'-X'80'                               
         GOTO1 VWRITE                                                           
         B     UPDPRO9                                                          
*                                                                               
UPDPRO8  MVC   KEY,KEYSAVE                                                      
         GOTO1 VADD                                                             
*                                                                               
UPDPRO9  DS    0H                                                               
         CLC   RPRSWSAL,RCONSAL                                                 
         BE    UPDPRO19                                                         
*                                                                               
         LA    RE,KEY                                                           
         XC    KEY,KEY                                                          
         USING RPROKEY,RE                                                       
         MVI   RPROOTYP,RPROOTYQ                                                
         MVI   RPROOSTY,RPROOSBQ                                                
         MVC   RPROORCD,REPALPHA                                                
         MVC   RPROOOFF,RPRSWOFF                                                
         MVC   RPROOSAL,RPRSWSAL                                                
         MVC   RPROOCON,PK.RPROKCON                                             
         MVC   RPROOPRO,PK.RPROKPRO                                             
         DROP  RE,PK                                                            
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RPROKMST),KEYSAVE                                          
         BE    *+14                                                             
         MVC   KEY,KEYSAVE                                                      
         B     UPDPR14                                                          
*                                                                               
         OI    KEY+RPROKCTL-RPROKEY,X'80'       DELETED                         
         GOTO1 VWRITE                                                           
*                                                                               
UPDPR14  LA    RE,KEY              NEW PASSIVE KEY                              
         USING RPROKEY,RE                                                       
         MVC   RPROOOFF,RCONKOFF                                                
         MVC   RPROOSAL,RCONSAL                                                 
         NI    RPROKCTL,X'FF'-X'80'                                             
         OC    RPROKDA,RPROKDA                                                  
         BNZ   *+10                                                             
         MVC   RPROKDA,WORK2+(RPROKDA-RPROKEY)                                  
         DROP  RE                                                               
*                                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RPROKMST),KEYSAVE                                          
         BNE   UPDPR18                                                          
         NI    KEY+(RPROKCTL-RPROKEY),X'FF'-X'80'                               
         GOTO1 VWRITE                                                           
         B     UPDPRO19                                                         
*                                                                               
UPDPR18  MVC   KEY,KEYSAVE                                                      
         GOTO1 VADD                                                             
*                                                                               
UPDPRO19 MVC   KEY,WORK2                                                        
*                                                                               
UPDPRO20 MVC   RPRSWSAL,RCONSAL                                                 
         MVC   RPRSWOFF,RCONKOFF                                                
         MVC   RPRSWTEM,RCONTEM                                                 
         MVC   RPRSWSTA,RCONKSTA                                                
         MVC   RPRSWADV,RCONKADV                                                
         MVC   RPRSWAGY,RCONKAGY                                                
         MVC   RPRSWAOF,RCONKAOF                                                
         MVC   RPRSWGRP,RCONKGRP                                                
         MVC   RPRSWFLT,RCONDATE                                                
         XC    RPRSWDSP,RPRSWDSP                                                
         XC    RPRSWDCT,RPRSWDCT                                                
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'18'        DEVELOPMENTAL INVOCE ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   UPDPRO25            NOT FOUND                                    
         USING RCONDVEL,R6                                                      
         MVC   RPRSWDSP,RCONDVSP                                                
         MVC   RPRSWDCT,RCONDVCT                                                
         DROP  R4,R6                                                            
*                                                                               
UPDPRO25 GOTO1 VPUTREC,DMCB,AIO4                                                
*                                                                               
UPDPRO30 DS    0H                                                               
         LA    RE,KEY                                                           
         USING RPROKEY,RE                                                       
         ZIC   RF,RPROKPRO                                                      
         LA    RF,1(RF)                                                         
         STC   RF,RPROKPRO                                                      
         XC    RPROKMEL,RPROKMEL                                                
         DROP  RE                                                               
         B     UPDPRO2             PROCESS NEXT RECORD                          
*                                                                               
UPDPROX  DS    0H                                                               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***>>>                                                                          
***********************************************************************         
* UPDATE MAKEGOOD RECORD SWITCH ELEMENTS AND PASSIVE KEYS             *         
***********************************************************************         
UPDMKGS  CSECT                                                                  
         NMOD1 0,*UPMG*                                                         
         L     RC,0(R1)                                                         
*                                                                               
*   BUILD A MAKEGOOD SWITCH ELEMENT, WHICH ALSO CONTROLS PASSIVES               
*                                                                               
         XC    WORK3(64),WORK3     CLEAR ELEMENT BUILD AREA                     
         MVI   WORK3,X'0A'         INSERT ELEMENT CODE                          
         MVI   WORK3+1,RMKGXLNQ    INSERT ELEMENT LENGTH                        
WRK3     USING RMKGXEL,WORK3                                                    
         MVC   WRK3.RMKGXSAL,RCONSAL                                            
*                                  INSERT SALESPERSON CODE                      
         MVC   WRK3.RMKGXOFF,RCONKOFF                                           
*                                  INSERT OFFICE CODE                           
         MVC   WRK3.RMKGXTEM,RCONTEM                                            
*                                  INSERT TEAM CODE                             
         MVC   WRK3.RMKGXSTA,RCONKSTA                                           
*                                  INSERT STATION CODE                          
         MVC   WRK3.RMKGXADV,RCONKADV                                           
*                                  INSERT ADVERTISER CODE                       
         MVC   WRK3.RMKGXAGY,RCONKAGY                                           
*                                  INSERT AGENCY CODE                           
         MVC   WRK3.RMKGXAOF,RCONKAOF                                           
*                                  INSERT AGENCY OFFICE CODE                    
         MVC   WRK3.RMKGXGRP,RCONKGRP                                           
*                                  INSERT GROUP/SUBGROUP                        
         MVC   WRK3.RMKGXFLT,RCONDATE                                           
*                                  INSERT FLIGHT DATES                          
         LA    RF,RCONELEM         FIND DEVELOPMENT ELT                         
UPMG0020 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    UPMG0060            YES - NO DEV ELT PRESENT                     
         CLI   0(RF),X'18'         DEVELOPMENT ELT?                             
         BE    UPMG0040            YES -                                        
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELEMENT                   
         AR    RF,RE                                                            
         B     UPMG0020            GO BACK FOR NEXT                             
UPMG0040 EQU   *                                                                
         MVC   WRK3.RMKGXDSP,RCONDVSP-RCONDVEL(RF)                              
*                                  INSERT DEV S/P                               
         MVC   WRK3.RMKGXDCT,RCONDVCT-RCONDVEL(RF)                              
*                                  INSERT DEV CON TYPE                          
UPMG0060 EQU   *                                                                
         DROP  WRK3                                                             
*                                  INSERT DEV CON TYPE                          
         XC    KEY,KEY                                                          
MKGKEY   USING RMKGKEY,KEY                                                      
*                                                                               
         MVI   MKGKEY.RMKGKTYP,X'11'      INSERT MAKEGOOD REC TYPE              
         MVC   MKGKEY.RMKGKREP,REPALPHA   INSERT REP CODE                       
         MVC   MKGKEY.RMKGKOFF,RCONKOFF                                         
*                                  INSERT OFFICE CODE                           
         MVC   MKGKEY.RMKGKSTA,RCONKSTA                                         
*                                  INSERT STATION CALLS                         
         ZAP   WORK+15(5),=P'0'                                                 
         MVO   WORK+15(5),RCONKCON                                              
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         PACK  WORK(1),WORK+18(1)   REVERSE THE COMPLIMENT                      
         PACK  WORK+1(1),WORK+17(1)                                             
         PACK  WORK+2(1),WORK+16(1)                                             
         PACK  WORK+3(1),WORK+15(1)                                             
         MVC   MKGKEY.RMKGKCON,WORK                                             
         DROP  MKGKEY                                                           
*                                                                               
UPMG0100 EQU   *                                                                
         GOTO1 VHIGH                                                            
         B     UPMG0140                                                         
UPMG0120 EQU   *                                                                
         GOTO1 VSEQ                READ NEXT RECORD                             
UPMG0140 EQU   *                                                                
         CLC   KEY(RMKGKGR1-RMKGKEY),KEYSAVE                                    
*                                  MAKEGOOD KEY FOR CON# FOUND?                 
         BNE   UPMG0300            NO  - EXIT                                   
*                                                                               
         OC    KEY+RMKGKPLN-RMKGKEY(6),KEY+RMKGKPLN-RMKGKEY                     
*                                  YES - GROUP RECORD?                          
         BNZ   UPMG0120            NO  - SKIP THIS RECORD                       
         MVI   UPDATE,C'Y'         YES                                          
         GOTO1 VGETREC,DMCB,RMKGREC                                             
*                                  RETRIEVE THE RECORD                          
****>>                                                                          
         BAS   RE,MGRECUPD         UPDATE M/G REC AND PASSIVES                  
****>>                                                                          
UPMG0280 DS    0H                                                               
*                                                                               
*   NEED TO RESTART KEY, RETRIEVE ORIG RECORD                                   
*                                                                               
         MVC   KEY(27),RMKGREC     RESET KEY                                    
         GOTO1 VHIGH               RETRIEVE M/G RECORD KEY                      
         B     UPMG0120            GO BACK FOR NEXT RECORD                      
*                                                                               
UPMG0300 DS    0H                                                               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
MGRECUPD NTR1                                                                   
*--->>                                                                          
*   AT THIS POINT, DROP THE SWITCH ELT FROM THE RECORD, ADD THE                 
*        NEW SWITCH ELT FROM WORK3  AREA, PROCESS THE PASSIVE                   
*        KEYS, DELETING OLD, REACTIVATING WHERE NEEDED.                         
*                                                                               
         BAS   RE,OLD0AELT         GET OLD 0A, SAVE PASSIVE INFO                
*                                     ALSO DROPS ELT FROM RECORD                
         GOTO1 VADDELEM,DMCB,RMKGREC,WORK3                                      
*                                  INSERT NEW 0A ELT                            
         MVC   KEYSAVE,KEY         SAVE M/G KEY                                 
         MVC   WORK(27),KEY        SAVE M/G KEY AGAIN                           
         MVC   FULL,KEY+28         SET D/A OF RECORD                            
*                                     ORIGINAL IOAREA                           
         GOTO1 VPUTREC,DMCB,RMKGREC                                             
*                                  REWRITE M/G W/NEW 0A ELT                     
         BAS   RE,PASSREWR         REWRITE PASSIVE KEYS                         
         B     RECWX                                                            
RECWX    EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   PASSREWR:  RETRIEVE AND REWRITE PASSIVE KEYS                                
*                                                                               
PASSREWR NTR1                                                                   
*                                                                               
*   DELETE OLD PASSIVE POINTERS                                                 
*                                                                               
WKMGKEY  USING RMKGKEY,WORK                                                     
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'A0'           INSERT FIRST PASSIVE                         
         MVI   KEY+1,X'11'         INSERT ID FOR MG PASSIVE 1                   
         MVC   KEY+16(11),WKMGKEY.RMKGKSTA                                      
*                                  MOVE STA/CON#/GROUP TO NEW POSITIONS         
         MVC   KEY+9(2),WKMGKEY.RMKGKREP                                        
*                                  MOVE REP CODE TO NEW POSITION                
         DROP  WKMGKEY                                                          
*                                                                               
         MVC   KEY+11(3),SAVSAL    INSERT ORIGINAL S/P  INTO KEY                
         MVC   KEY+11(2),SAVTEM    INSERT ORIGINAL TEAM INTO KEY                
PAWR0010 EQU   *                                                                
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VHIGH               READ FOR KEY                                 
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BE    PAWR0020            YES -                                        
         MVC   KEY,KEYSAVE         NO  - RESET ORIGINAL KEY                     
         B     PAWR0040            PROCESS NEXT KEY                             
PAWR0020 EQU   *                                                                
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'        TURN ON 'DELETE' BIT                         
         GOTO1 VWRITE              REWRITE KEY AS DELETED                       
         BAS   RE,CHECKPAS                                                      
PAWR0040 EQU   *                                                                
         CLI   KEY,X'A1'           SECOND KEY ALREADY DONE?                     
         BE    PAWR0060            YES                                          
         MVI   KEY,X'A1'           NO  - SET TO NEXT PASSIVE KEY                
         MVC   KEY+11(2),SAVTEM    INSERT TEAM INTO KEY HIGH                    
         MVC   KEY+13(3),SAVSAL    INSERT S/P INTO KEY LOW                      
         B     PAWR0010            GO BACK AND DO NEXT KEY                      
PAWR0060 EQU   *                                                                
*                                                                               
*        DELETE A0 PASSIVES                                                     
*                                                                               
PAWR0101 DS    0H                                                               
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         LA    R5,KEY                                                           
*                                                                               
RMGSPKEY USING RMGSPTYP,R5         ESTABLISH PASSIVE                            
WKMGKEY  USING RMKGKEY,WORK                                                     
*                                                                               
         MVC   RMGSPKEY.RMGSPTYP,=X'A001' SET PASSIVE ID                        
         MVC   RMGSPKEY.RMGSPREP,WKMGKEY.RMKGKREP REPCODE                       
         MVC   RMGSPKEY.RMGSPSTA,WKMGKEY.RMKGKSTA STATION                       
         PACK  RMGSPKEY.RMGSPCON(1),WKMGKEY.RMKGKCON+3(1) CONTRACT              
         PACK  RMGSPKEY.RMGSPCON+1(1),WKMGKEY.RMKGKCON+2(1) REVERSE             
         PACK  RMGSPKEY.RMGSPCON+2(1),WKMGKEY.RMKGKCON+1(1) TO GET 9'S          
         PACK  RMGSPKEY.RMGSPCON+3(1),WKMGKEY.RMKGKCON(1)   COMPLEMENT          
         MVC   RMGSPKEY.RMGSPGRP,WKMGKEY.RMKGKGRP MAKEGOOD GROUP                
*                                                                               
         DROP  WKMGKEY                                                          
*                                                                               
         MVC   27(1,R5),29(R2)     STATUS                                       
*                                                                               
         MVC   RMGSPKEY.RMGSPSAL,SAVSAL   INSERT S/P INTO KEY                   
         MVC   RMGSPKEY.RMGSPADV,SAVADV   ADVERTISER CODE                       
         MVC   RMGSPKEY.RMGSPDAT,SAVDAT   FIRST OFFERED DATE                    
         MVC   RMGSPKEY.RMGSPWIP,SAVWIP   SET WIP STATUS                        
         MVC   RMGSPKEY.RMGSPSTT,SAVSTT   OFFER STATUS                          
         MVC   RMGSPKEY.RMGSPDST,SAVDST   DARE  STATUS                          
*                                                                               
*        DELETE ANY PASSIVE ALREADY ON FILE                                     
*                                                                               
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VHIGH               READ FOR KEY                                 
*                                                                               
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BE    PAWR0520            YES -                                        
*                                                                               
         MVC   KEY,KEYSAVE         NO  - RESET ORIGINAL KEY                     
         B     PAWR0540            PROCESS NEXT KEY                             
*                                                                               
PAWR0520 EQU   *                                                                
*                                                                               
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'        TURN ON 'DELETE' BIT                         
         GOTO1 VWRITE              REWRITE KEY AS DELETED                       
         BAS   RE,CHECKPAS                                                      
*                                                                               
PAWR0540 EQU   *                                                                
*                                                                               
         OC    SAVDSL,SAVDSL       SKIP IF NO DEV SALESPER                      
         BZ    PAWR0600                                                         
*                                                                               
         MVI   1(R5),X'02'         SET NEXT MG PASSIVE                          
         MVC   RMGSPKEY.RMGSPSAL,SAVDSL INSERT DEV SALESPER                     
*                                                                               
*        DELETE ANY PASSIVE ALREADY ON FILE                                     
*                                                                               
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VHIGH               READ FOR KEY                                 
*                                                                               
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BE    PAWR0560            YES -                                        
*                                                                               
         MVC   KEY,KEYSAVE         NO  - RESET ORIGINAL KEY                     
         B     PAWR0580            PROCESS NEXT KEY                             
*                                                                               
PAWR0560 EQU   *                                                                
*                                                                               
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'        TURN ON 'DELETE' BIT                         
         GOTO1 VWRITE              REWRITE KEY AS DELETED                       
         BAS   RE,CHECKPAS                                                      
*                                                                               
PAWR0580 EQU   *                                                                
*                                                                               
PAWR0600 DS    0H                                                               
*                                                                               
**-->>                                                                          
WKMGKEY  USING RMKGKEY,WORK                                                     
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'A0'           INSERT FIRST PASSIVE                         
         MVI   KEY+1,X'11'         INSERT ID FOR MG PASSIVE 1                   
         MVC   KEY+16(11),WKMGKEY.RMKGKSTA                                      
*                                  MOVE STA/CON#/GROUP TO NEW POSITIONS         
         MVC   KEY+9(2),WKMGKEY.RMKGKREP                                        
*                                  MOVE REP CODE TO NEW POSITION                
         DROP  WKMGKEY                                                          
**-->>                                                                          
         LA    RF,RMKGELEM         SET A(DESCRIPTOR ELT OF M/G REC)             
PAWR0080 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    RECWX               YES - EXIT ROUTINE: NO PASSIVES              
         CLI   0(RF),X'0A'         SWITCH/PASSIVE ELT OF RECORD?                
         BE    PAWR0100            YES - PROCESS                                
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELT                       
         AR    RF,RE                                                            
         B     PAWR0080            GO BACK FOR NEXT                             
PAWR0100 EQU   *                                                                
         MVC   KEY+11(3),RMKGXSAL-RMKGXEL(RF)                                   
*                                  INSERT SALESPERSON CODE                      
         MVC   KEY+14(2),RMKGXTEM-RMKGXEL(RF)                                   
*                                  INSERT S/P TEAM    CODE                      
PAWR0120 EQU   *                                                                
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VHIGH                                                            
*                                                                               
         MVC   KEY+28(4),FULL      INSERT D/A                                   
         CLC   KEYSAVE(27),KEY     KEY ALREADY ON FILE?                         
         BNE   PAWR0140            NO                                           
         NI    KEY+27,X'7F'        TURN OFF DELETE BIT                          
         GOTO1 VWRITE              YES - WRITE THE KEY                          
         B     PAWR0160            GO PROCESS NEXT KEY (IF ANY)                 
PAWR0140 EQU   *                                                                
         MVC   KEY(27),KEYSAVE     RESTORE KEY FOR 27 CHARS                     
         GOTO1 VADD                ADD NEW KEY                                  
PAWR0160 EQU   *                                                                
         CLI   KEY,X'A1'           SECOND KEY PROCESSED?                        
         BE    PAWR0180            YES - BOTH KEYS DONE                         
         MVI   KEY,X'A1'           NO  - SET SECOND KEY TYPE                    
         MVC   KEY+11(2),SAVTEM    SLIDE TEAM UP IN KEY                         
         MVC   KEY+13(3),SAVSAL    INSERT S/P BACK INTO KEY                     
         B     PAWR0120            GO BACK AND PROCESS                          
PAWR0180 EQU   *                                                                
*                                                                               
*        ADD MAKEGOOD A001, A002 PASSIVE KEYS                                   
*                                                                               
         XC    KEY,KEY             CLEAR NEW KEY                                
         LA    R5,KEY                                                           
*                                                                               
RMGSPKEY USING RMGSPTYP,R5         ESTABLISH PASSIVE                            
WKMGKEY  USING RMKGKEY,WORK                                                     
*                                                                               
         MVC   RMGSPKEY.RMGSPTYP,=X'A001' SET PASSIVE ID                        
         MVC   RMGSPKEY.RMGSPREP,WKMGKEY.RMKGKREP REPCODE                       
         MVC   RMGSPKEY.RMGSPSTA,WKMGKEY.RMKGKSTA STATION                       
         PACK  RMGSPKEY.RMGSPCON(1),WKMGKEY.RMKGKCON+3(1) CONTRACT              
         PACK  RMGSPKEY.RMGSPCON+1(1),WKMGKEY.RMKGKCON+2(1) REVERSE             
         PACK  RMGSPKEY.RMGSPCON+2(1),WKMGKEY.RMKGKCON+1(1) TO GET 9'S          
         PACK  RMGSPKEY.RMGSPCON+3(1),WKMGKEY.RMKGKCON(1)  COMPLEMENT           
         MVC   RMGSPKEY.RMGSPGRP,WKMGKEY.RMKGKGRP MAKEGOOD GROUP                
*                                                                               
         DROP  WKMGKEY                                                          
*                                                                               
         LA    RF,RMKGELEM                                                      
*                                  SET A(DESCRIPTOR ELT OF MKG RECORD)          
         USING RMKGSDEM,RF         ESTABLISH GROUP STATUS ELEMENT               
*                                                                               
         MVC   RMGSPKEY.RMGSPDAT,RMKGFOFD RMGSPKEY.RMGSPE 1ST OFFERED           
         MVC   RMGSPKEY.RMGSPWIP,RMKGSFG2 WIP STATUS                            
         MVC   RMGSPKEY.RMGSPSTT,RMKGSCST OFFER STATUS                          
         NI    RMGSPKEY.RMGSPSTT,X'FF'-RMKGSPAQ KILL OLD STATUS                 
         TM    RMKGSFG3,RMGF3SAQ   IF SELF APPLIED                              
         BNO   *+8                                                              
         OI    RMGSPKEY.RMGSPSTT,RMKGSLFQ SET INDICATOR                         
*                                                                               
         MVC   RMGSPKEY.RMGSPDST,RMKGSFG1 DARE STATUS                           
*                                                                               
         DROP  RF                                                               
*                                                                               
PASA0220 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    PASAX               YES - EXIT ROUTINE: NO PASSIVES              
         CLI   0(RF),X'0A'         SWITCH/PASSIVE ELT OF RECORD?                
         BE    PASA0240            YES - PROCESS                                
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELT                       
         AR    RF,RE                                                            
         B     PASA0220            GO BACK FOR NEXT                             
PASA0240 EQU   *                                                                
         USING RMKGXEL,RF                                                       
         MVC   RMGSPKEY.RMGSPSAL,RMKGXSAL SALESPERSON                           
         MVC   RMGSPKEY.RMGSPADV,RMKGXADV ADVERTISER                            
         MVC   SAVDSL,RMKGXDSP     SAVE DEVELOPMENTAL SALESPERSON               
*                                                                               
         DROP  RF                                                               
*                                                                               
PASA0260 EQU   *                                                                
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VHIGH                                                            
*                                                                               
         MVC   KEY+28(4),FULL      INSERT D/A                                   
         CLC   KEYSAVE(27),KEY     KEY ALREADY ON FILE?                         
         BNE   PASA0280            NO                                           
         NI    KEY+27,X'7F'        TURN OFF DELETE BIT                          
         GOTO1 VWRITE              YES - WRITE THE KEY                          
         B     PASA0300            GO PROCESS NEXT KEY (IF ANY)                 
PASA0280 EQU   *                                                                
         MVC   KEY(27),KEYSAVE     RESTORE KEY FOR 27 CHARS                     
         NI    KEY+27,X'7F'        TURN OFF DELETE BIT                          
         GOTO1 VADD                ADD NEW KEY                                  
PASA0300 EQU   *                                                                
         CLI   KEY+1,X'02'         SECOND KEY PROCESSED?                        
         BE    PASA0320            YES - BOTH KEYS DONE                         
         MVI   KEY+1,X'02'         NO  - SET SECOND KEY TYPE                    
         MVC   RMGSPKEY.RMGSPSAL,SAVDSL   USE DEV SALESPERSON                   
         OC    SAVDSL,SAVDSL       SKIP IF NO DEV SAL                           
         BZ    PASA0320                                                         
         B     PASA0260            GO BACK AND PROCESS                          
PASA0320 EQU   *                                                                
PASAX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
CHECKPAS EQU   *                                                                
         TM    DMCB+8,X'FD'                                                     
         BCR   8,RE                                                             
         DC    H'0'                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
*   OLD0AELT:  FIND EXISTING 0A ELT, SAVE S/P, TEAM CODES FOR                   
*        DELETING OLD KEY                                                       
*                                                                               
OLD0AELT NTR1                                                                   
         XC    DUB,DUB             USE DUB FOR INTERMEDIATE                     
         LA    R1,RMKGELEM                                                      
*                                                                               
*        SAVE FIELDS FOR DELETING PASSIVES                                      
*                                                                               
         USING RMKGSDEM,R1         ESTABLISH GROUP STATUS ELEMENT               
*                                                                               
         MVC   SAVDAT,RMKGFOFD     SAVE FIRST OFFERED DATE                      
         MVC   SAVWIP,RMKGSFG2     WIP STATUS                                   
         MVC   SAVSTT,RMKGSCST     OFFER STATUS                                 
         NI    SAVSTT,X'FF'-RMKGSPAQ KILL OLD STATUS                            
         TM    RMKGSFG3,RMGF3SAQ   IF SELF APPLIED                              
         BNO   *+8                                                              
         OI    SAVSTT,RMKGSLFQ        SET INDICATOR                             
*                                                                               
         MVC   SAVDST,RMKGSFG1     DARE  STATUS                                 
*                                                                               
         DROP  R1                                                               
*                                                                               
OLDA0020 EQU   *                                                                
         CLI   0(R1),0             END OF RECORD?                               
         BE    OLDA0100            YES - FINISHED - NO ELT                      
         CLI   0(R1),X'0A'         0A ELT?                                      
         BE    OLDA0040            YES - PROCESS IT                             
         ZIC   RF,1(R1)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R1,RF                                                            
         B     OLDA0020            GO BACK FOR NEXT                             
OLDA0040 EQU   *                                                                
         USING RMKGXEL,R1                                                       
         MVC   DUB(3),RMKGXSAL     SAVE S/P CODE                                
         MVC   DUB+3(2),RMKGXTEM   SAVE TEAM CODE                               
*                                                                               
*        SAVE FIELDS FOR PASSIVES                                               
*                                                                               
         MVC   SAVSAL,RMKGXSAL     SALESPERSON                                  
         MVC   SAVTEM,RMKGXTEM     TEAM                                         
         MVC   SAVADV,RMKGXADV     ADVERTISER                                   
         MVC   SAVDSL,RMKGXDSP     DEVELOPMENTAL SALESPERSON                    
*                                                                               
         DROP  R1                                                               
*                                                                               
         GOTO1 VDELELEM,DMCB,(X'0A',RMKGREC)                                    
*                                                                               
OLDA0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*--->>                                                                          
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* CHECK IF STATION HAS CLOSE OUT DATE.  IF TRUE, ADD X'04' INVOICE              
* ELEMENT TO CONTRACT BEFORE ADDREC.  OTHERWISE DELETE ALL X'04'                
* ELEMENTS (IF ADDED FROM A PREVIOUS COMBO).                                    
***********************************************************************         
CHKCLOUT CSECT                                                                  
         NMOD1 0,*CKCOUT*                                                       
         L     RC,0(R1)                                                         
                                                                                
         CLC   =C'ADD',CONACT      ONLY IN ACTION ADD!                          
         BNE   CKCOUTX                                                          
* DELETE ALL X'04'/X'54' ELEMENTS (IF ADDED BY PRECEDING CONTRACT)              
         GOTO1 VDELELEM,DMCB,(4,RCONREC)                                        
         GOTO1 VDELELEM,DMCB,(X'54',RCONREC)                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         MVC   KEY+20(2),REPALPHA  GET THE STATION RECORD USED BY THE           
         MVC   KEY+22(5),RCONKSTA  CONTRACT IN RCONREC                          
         BAS   R5,GETSTA                                                        
*                                                                               
*** FIX BUG OF COMBO STATIONS WITH DIFFERENT GROUP/SUBGROUPS                    
         MVC   RCONKGRP,RSTAGRUP                                                
*                                                                               
         OC    RSTACLDT,RSTACLDT   CLOSE OUT DATE?                              
         BZ    CKCOUTX             IF NONE, EXIT                                
* CHECK K START DATE                                                            
*                                                                               
*   THIS IS WRONG.  IF A COMBO ORDER IS ADDED FOR A CLOSED                      
*        PERIOD (WHY THEY'D DO THIS IS BEYOND ME), YOU CAN'T                    
*        USE RSTACLDT, WHICH IS THE STATION OF THE BASE                         
*        RECORD IN THE ORDER.  THERE IS NO REASON TO ASSUME                     
*        THAT ALL PARTICIPANTS HAVE BEEN CLOSED THROUGH                         
*        THE SAME DATE. BILL:  MAR21/98.                                        
*                                                                               
         CLC   RCONDATE(2),RSTACLDT                                             
         BH    CKCOUTX                                                          
* CHECK MONTHS OF K                                                             
         GOTO1 DATCON,DMCB,(3,RCONDATE),WORK                                    
         GOTO1 (RF),(R1),(3,RCONDATE+3),WORK+6                                  
         XC    HALF,HALF                                                        
*                                                                               
* BUILD ZERO INVOICE BUCKET IN WORK2                                            
         MVC   WORK2(2),=X'040A'   ELEM CODE + LEN                              
         MVC   WORK2+4(2),MONDATE  ACTIVITY DATE                                
         XC    WORK2+6(4),WORK2+6                                               
*                                                                               
* GET BROADCAST MONTH                                                           
CKCOUT05 GOTO1 VGTBROAD,(R1),(1,WORK),WORK+12,GETDAY,ADDAY                      
         GOTO1 DATCON,(R1),WORK+18,(3,FULL)                                     
         CLC   FULL(2),RSTACLDT                                                 
         BH    CKCOUTX                                                          
         OC    HALF,HALF      FIRST TIME?                                       
         BZ    CKCOUT10                                                         
*                                                                               
         CLC   HALF,FULL           SAME MONTH?                                  
         BE    CKCOUT20                                                         
*                                                                               
CKCOUT10 MVC   HALF,FULL           SAVE                                         
         MVC   WORK2+2(2),FULL                                                  
* NEXT ELEM                                                                     
*                                  LOOK FOR AN INV EL FOR THIS MONTH            
         GOTO1 HELLO,DMCB,(C'G',=C'REPFILE'),(X'04',RCONREC),          X        
               (2,WORK2+2)                                                      
         CLI   DMCB+12,0           FOUND?                                       
         BE    CKCOUT20            DON'T PUT DUP ELEM                           
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(0,RCONREC),(0,WORK2)              
         LA    RF,TWAMCAST         CHECK FOR ALTERNATE CALENDAR USE             
         LA    R0,4                SET LOOP CONTROL                             
CKCOUT12 EQU   *                                                                
         OC    0(5,RF),0(RF)       ANY ENTRY IN TABLE?                          
         BZ    CKCOUT20            NO  - NOT ALTERNATE CALENDAR                 
         CLC   RCONKSTA,0(RF)      STATION IN TABLE?                            
         BE    CKCOUT14            YES -                                        
         LA    RF,7(RF)            NO  - BUMP TO NEXT ENTRY                     
         BCT   R0,CKCOUT12         GO BACK FOR NEXT                             
         B     CKCOUT20            NOT ALTERNATE CALENDAR STATION               
CKCOUT14 EQU   *                                                                
         OI    WORK2,X'50'         YES - PUT OUT $.00 ALTERNATE                 
*                                     CALENDAR INVOICE ELT                      
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(0,RCONREC),(0,WORK2)              
         MVI   WORK2,X'04'         RESET ELEMENT CODE                           
* GET NEXT WEEK                                                                 
CKCOUT20 EQU   *                                                                
         CLC   WORK(4),WORK+6                                                   
         BE    CKCOUTX                                                          
         CLC   WORK2+2(2),RSTACLDT                                              
         BE    CKCOUTX                                                          
         GOTO1 ADDAY,(R1),WORK,WORK+24,7                                        
         MVC   WORK(6),WORK+24                                                  
         B     CKCOUT05                                                         
*                                                                               
CKCOUTX  DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD POINTER LIST IN P1                                           
*  P2 - 0=NEW POINTER                                                           
*       1=OLD POINTER                                                           
***********************************************************************         
PTRS     CSECT                                                                  
         NMOD1 0,**PTRS**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R2,4(R1)                                                         
         LR    RE,R2                                                            
         L     R3,8(R1)                                                         
         XCEF  (RE),800                                                         
* BUILD ACTIVE PTR                                                              
         MVI   0(R2),X'0C'                                                      
         MVC   02(02,R2),REPALPHA                                               
         MVC   04(02,R2),RCONKGRP                                               
         MVC   06(05,R2),RCONKSTA                                               
         MVC   11(02,R2),RCONKOFF                                               
         MVC   13(04,R2),RCONKAGY                                               
         MVC   17(02,R2),RCONKAOF                                               
         MVC   19(04,R2),RCONKADV                                               
         MVC   23(04,R2),RCONKCON                                               
         LA    R2,32(R2)                                                        
* CREATE PTR 1                                                                  
         MVI   0(R2),X'8C'                                                      
         MVC   21(02,R2),REPALPHA                                               
         ZAP   DUB(5),=P'99999999'                                              
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RCONKCON                                                 
         SP    DUB(5),WORK(5)                                                   
         MVO   WORK(5),DUB(5)                                                   
         MVC   23(4,R2),WORK                                                    
         LA    R2,32(R2)                                                        
* CREATE PTR 2                                                                  
         MVI   0(R2),X'9C'                                                      
         MVC   02(02,R2),REPALPHA                                               
         MVC   04(02,R2),RCONKOFF                                               
         MVC   06(02,R2),RCONKGRP                                               
         MVC   08(05,R2),RCONKSTA                                               
         MVC   13(04,R2),RCONKADV                                               
         MVC   17(04,R2),RCONKAGY                                               
         MVC   21(02,R2),RCONKAOF                                               
         MVC   23(04,R2),RCONKCON                                               
         LA    R2,32(R2)                                                        
* CREATE PTR 3                                                                  
         MVI   0(R2),X'AC'                                                      
         MVC   01(2,R2),REPALPHA                                                
         MVC   03(2,R2),RCONKOFF                                                
         MVC   05(2,R2),RCONTEM                                                 
* INVERT SALESMAN CODE FOR LAST NAME HIGH                                       
         LA    RE,RCONSAL+2        LAST INITIAL                                 
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,R0                                                            
         MVC   07(1,R2),0(RE)                                                   
         MVC   08(2,R2),RCONSAL                                                 
         CLI   RCONSAL+2,C' '      ONLY 2 INITIALS?                             
         BNE   *+8                                                              
         MVI   9(R2),C' '                                                       
         MVC   10(5,R2),RCONKSTA                                                
         MVC   15(4,R2),RCONKAGY                                                
         MVC   19(4,R2),RCONKADV                                                
         MVC   23(4,R2),RCONKCON                                                
         LA    R2,32(R2)                                                        
* CREATE PTR 4                                                                  
BCPTR    MVI   0(R2),X'BC'                                                      
         MVC   02(02,R2),REPALPHA                                               
         MVC   04(02,R2),RCONCTGY                                               
         MVC   06(02,R2),RCONKOFF                                               
         MVC   08(05,R2),RCONKSTA                                               
         MVC   13(04,R2),RCONKAGY                                               
         MVC   17(02,R2),RCONKAOF                                               
         MVC   19(04,R2),RCONKADV                                               
         MVC   23(04,R2),RCONKCON                                               
         LA    R2,32(R2)                                                        
* CREATE PTR BF                                                                 
BFPTR    MVI   0(R2),X'BF'                                                      
         MVC   02(02,R2),REPALPHA                                               
         MVC   04(06,R2),RCONKAGY                                               
         MVC   10(04,R2),RCONKADV                                               
         MVC   14(05,R2),RCONKSTA                                               
         MVC   19(02,R2),RCONKOFF                                               
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(2,21(R2))                            
         MVC   23(04,R2),RCONKCON                                               
         LA    R2,32(R2)                                                        
* CREATE PTR 5                                                                  
         MVI   0(R2),X'CC'                                                      
         MVC   01(02,R2),REPALPHA                                               
         MVC   03(05,R2),RCONKSTA                                               
         MVC   08(02,R2),RCONKOFF                                               
         MVC   10(02,R2),RCONTEM                                                
* INVERT SALESMAN                                                               
         LA    RE,RCONSAL+2                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,R0                                                            
         MVC   12(1,R2),0(RE)                                                   
         MVC   13(2,R2),RCONSAL                                                 
         CLI   RCONSAL+2,C' '                                                   
         BNE   *+8                                                              
         MVI   14(R2),C' '                                                      
*                                                                               
         MVC   15(04,R2),RCONKADV                                               
         MVC   19(04,R2),RCONKAGY                                               
         MVC   23(04,R2),RCONKCON                                               
         LA    R2,32(R2)                                                        
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   PT8D                NO BOP                                       
         SPACE 1                                                                
         USING RCONBPEL,R6                                                      
DCPRTR   MVI   0(R2),X'DC'         CREATE BOP POINTER FOR CHANGE                
         MVC   5(2,R2),REPALPHA                                                 
         MVC   7(4,R2),RCONKADV                                                 
         MVC   11(3,R2),TODAY      NEW POINTER GETS TODAYS DATE                 
         CLC   CONACT(3),=C'ADD'                                                
         BE    DC20                                                             
         LTR   R3,R3               0=NEW POINTER                                
         BZ    DC20                                                             
         MVC   11(3,R2),RCONBPDT   OLD PTR NEEDS OLD BOP CHANGE DATE            
*    (IN ADPTRS, ONLY ADD NEW POINTER IF CHANGE IS OTHER THAN DATE)             
*                                                                               
DC20     MVC   14(4,R2),RCONBPRF                                                
         MVC   18(5,R2),RCONKSTA                                                
         MVC   23(4,R2),RCONKCON                                                
         LA    R2,32(R2)                                                        
         DROP  R6                                                               
*                                                                               
*   CREATE X'8D' POINTERS                                                       
*                                                                               
PT8D     EQU   *                                                                
                                                                                
* - GET FLIGHT START/END DATES AND SAVE IN WORK+40                              
         GOTO1 DATCON,DMCB,(3,RCONDATE),(2,WORK+40)    START DATE               
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(2,WORK+42)  END DATE                 
                                                                                
* - GET DEMO FROM BOP OR SAR ELEMENT AND SAVE IN WORK+45                        
* - LOOK FOR DEMO MARKED AS PRIMARY (X'40' IN 1ST BYTE)                         
* - IF NO DEMO MARKED AS PRIMARY, USE 1ST DEMO AS DEFAULT                       
*                                                                               
         XC    WORK+45(3),WORK+45                                               
         LA    R4,RCONELEM                                                      
PPC8DLP  CLI   0(R4),0                                                          
         BE    PPC8D00                                                          
         CLI   0(R4),X'12'         SAR ELEMENT                                  
         BE    PPC8DDD                                                          
         CLI   0(R4),X'10'         BOP ELEMENT                                  
         BE    PPC8DEE                                                          
         ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    PPC8D00                                                          
         AR    R4,R1                                                            
         B     PPC8DLP             GO TO LOOP                                   
         USING RSARCO,R4                                                        
PPC8DDD  LA    RE,RSARDEM            DEMO                                       
         LA    RF,8                                                             
         MVC   WORK+45(3),RSARDEM    DEMO                                       
PPC8DDE  TM    0(RE),X'40'         IS IT MARKED AS PRIMARY ?                    
         BO    PPC8DDF               YES                                        
         LA    RE,3(RE)                                                         
         MVC   WORK+45(3),0(RE)                                                 
         BCT   RF,PPC8DDE                                                       
         MVC   WORK+45(3),RSARDEM    NO/USE 1ST AS DEFAULT                      
PPC8DDF  NI    WORK+45,X'FF'-X'40'          CLEAR MAIN DEMO INDICATOR           
         B     PPC8D00                                                          
                                                                                
         USING RCONBPEL,R4                                                      
PPC8DEE  LA    RE,RCONBPDM+1                                                    
         LA    RF,6                                                             
         MVC   WORK+45(3),RCONBPDM+1                                            
PPC8DEF  TM    0(RE),X'40'              IS IT MARKED AS PRIMARY DEMO?           
         BO    PPC8DEG                  YES                                     
         LA    RE,3(RE)                 NO/BUMP TO NEXT DEMO                    
         MVC   WORK+45(3),0(RE)                                                 
         BCT   RF,PPC8DEF                                                       
         MVC   WORK+45(3),RCONBPDM+1     NO PRIMARY/USE 1ST AS DEFAULT          
PPC8DEG  NI    WORK+45,X'FF'-X'40'           CLEAR MAIN DEMO INDICATOR          
         B     PPC8D00                                                          
         DROP R4                                                                
                                                                                
* BUILD BASIC KEY IN WORK                                                       
PPC8D00  XC    WORK(32),WORK                                                    
         LA    R4,WORK                                                          
         MVI   0(R4),X'8D'                                                      
         MVC   1(2,R4),RCONKREP                                                 
         MVC   8(2,R4),WORK+40    START DATE                                    
         MVC   10(2,R4),WORK+42    END DATE                                     
         MVC   12(4,R4),RCONKCON   CONTRACT NUMBER                              
                                                                                
* ID = 1 = AGENCY / ADVERTISER                                                  
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),1                                                         
         MVC   17(6,R2),RCONKAGY       AGENCY(4) AND OFFICE(2)                  
         MVC   23(4,R2),RCONKADV       ADVERTISER                               
         LA    R2,32(R2)                                                        
                                                                                
* ID = 2 = SALESPERSON/CONTRACT TYPE/GROUP-SUBG/CATEGORY/TEAM                   
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),2                                                         
         MVC   17(3,R2),RCONSAL                                                 
         MVC   20(1,R2),RCONTYPE                                                
         MVC   21(2,R2),RCONKGRP                                                
         MVC   23(2,R2),RCONCTGY                                                
         MVC   25(2,R2),RCONTEM                                                 
         LA    R2,32(R2)                                                        
                                                                                
* ID = 3 = OFFICE(SALESPERSON)/DEMO/CREATION DATE                               
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),3                                                         
         MVC   17(2,R2),RCONKOFF      SALESPERSON OFFICE                        
* CREATION DATE YMD -> BINARY                                                   
         GOTO1 DATCON,DMCB,(3,RCONHDRD),(2,22(R2))                              
         MVC   19(3,R2),WORK+45       DEMO                                      
PPC8DX   LA    R2,32(R2)                                                        
*                                                                               
* END X'8D' PASSIVE POINTERS                                                    
*                                                                               
*   CREATE X'8E' POINTERS                                                       
*   SIMILAR TO X'8D' POINTERS BUT HAVE STATION IN KEY INSTEAD OF 0'S            
*                                                                               
* WORK HAS BASIC KEY- REPLACE ZEROS OF X'8D' WITH STATION                       
PPCON8E  MVI   WORK,X'8E'                                                       
         MVC   WORK+3(5),RCONKSTA                                               
                                                                                
* ID = 1 = AGENCY / ADVERTISER                                                  
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),1                                                         
         MVC   17(6,R2),RCONKAGY       AGENCY(4) AND OFFICE(2)                  
         MVC   23(4,R2),RCONKADV       ADVERTISER                               
         LA    R2,32(R2)                                                        
                                                                                
* ID = 2 = SALESPERSON/CONTRACT TYPE/GROUP-SUBG/CATEGORY/TEAM                   
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),2                                                         
         MVC   17(3,R2),RCONSAL                                                 
         MVC   20(1,R2),RCONTYPE                                                
         MVC   21(2,R2),RCONKGRP                                                
         MVC   23(2,R2),RCONCTGY                                                
         MVC   25(2,R2),RCONTEM                                                 
         LA    R2,32(R2)                                                        
                                                                                
* ID = 3 = OFFICE(SALESPERSON)/DEMO/CREATION DATE                               
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),3                                                         
         MVC   17(2,R2),RCONKOFF      SALESPERSON OFFICE                        
* CREATION DATE YMD -> BINARY                                                   
         GOTO1 DATCON,DMCB,(3,RCONHDRD),(2,22(R2))                              
         MVC   19(3,R2),WORK+45    DEMO                                         
PPC8EX   LA    R2,32(R2)                                                        
                                                                                
* END X'8E' PASSIVE POINTERS                                                    
*                                                                               
         EJECT                                                                  
* ADD X'9D' PASSIVE POINTER (RIS/PRODUCT)                                       
*                                                                               
         MVI   0(R2),X'9D'                                                      
         MVC   1(2,R2),RCONKREP                                                 
         MVC   3(5,R2),RCONKSTA                                                 
         MVC   8(4,R2),RCONKADV                                                 
         MVC   23(4,R2),RCONKCON                                                
* NOW SET PRODUCT NAME OR CODE INTO KEY                                         
         MVI   12(R2),X'FF'              SET DEFAULT TO LAST                    
         MVC   13(3,R2),RCONPRD                                                 
         LA    R6,RCONREC          NOW LOOK FOR PRODUCT NAME                    
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   PP9DX                                                            
         USING RCONEXEL,R6                                                      
         MVC   12(9,R2),RCONEXPR   SET PRODUCT NAME                             
         DROP  R6                                                               
PP9DX    LA    R2,32(R2)           9D POINTER CREATED, BUMP                     
*                                                                               
* END X'9D' PASSIVE POINTER                                                     
*                                                                               
* SCRIPT UPLOAD PASSIVE POINTER                                                 
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'3C'                                                     
         BAS   RE,GETEL                                                         
         BNE   PTREC               NO SAR                                       
*                                                                               
         USING RCONCUEL,R6                                                      
         MVI   0(R2),X'AE'                                                      
         MVC   6(2,R2),REPALPHA                                                 
         MVC   13(10,R2),RCONCUW#                                               
         DROP  R6                                                               
         MVC   8(5,R2),RCONKSTA                                                 
         MVC   23(4,R2),RCONKCON                                                
*                                                                               
         LA    R2,32(R2)           9D POINTER CREATED, BUMP                     
*                                                                               
PTREC    DS    0H                                                               
         CLC   CONACT(3),=C'ADD'   ONLY ADD EC P-KEYS                           
         BNE   PTAB01                                                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   PTAB01              NO SAR                                       
         SPACE 1                                                                
         USING RSAREL,R6                                                        
         MVI   0(R2),X'EC'                                                      
         MVC   21(2,R2),REPALPHA                                                
         MVC   23(4,R2),TWACNUM                                                 
         LA    R2,32(R2)                                                        
         DROP  R6                                                               
PTAB01   DS    0H                                                               
* CREATE PRODUCT PASSIVE                                                        
         CLC   RCONPRD,=3C' '      IS THERE A PRODUCT?                          
         BNH   PTRX                NO, DON'T MAKE 'AB01' P-KEY                  
         MVC   0(2,R2),=X'AB01'                                                 
         MVC   10(2,R2),REPALPHA                                                
         MVC   12(4,R2),RCONKAGY                                                
         MVC   16(4,R2),RCONKADV                                                
         MVC   20(3,R2),RCONPRD                                                 
         MVC   23(4,R2),RCONKCON                                                
*                                                                               
PTRX     DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD POINTERS TO FILE                                               
*              P1=A(OLD PTR LIST)                                               
*              P2=A(NEW PTR LIST)                                               
*              P3=A(DISK ADDR)                                                  
***********************************************************************         
ADDPTRS  CSECT                                                                  
         NMOD1 0,*ADDPTR*                                                       
         L     RC,0(R1)                                                         
         LM    R2,R4,4(R1)                                                      
*                                                                               
         MVI   DMOUTBTS,0          FOR PROPER RECOVERY                          
         OI    DMINBTS,X'08'       PASS DELETES                                 
AP25     CLC   0(27,R2),0(R3)      SAME?                                        
         BE    AP100                                                            
*                                                                               
* DIFFERENT                                                                     
         CLI   0(R2),0             ADD?                                         
         BE    AP40                WRITE/RESTORE NEW KEY                        
*                                                                               
* CHANGE                                                                        
* DELETE OLD PTR                                                                
         MVC   KEY,0(R2)                                                        
         CLI   KEY,X'DC'           BOP POINTER ONLY CHANGED IF                  
         BNE   AP33                                                             
         CLC   0(11,R3),KEY        ADVERTISER OR                                
         BNE   AP33                                                             
         CLC   14(13,R3),KEY+14    REF #, STATION OR CON# CHANGES               
         BE    AP100                                                            
AP33     OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         BAS   RE,APCHECK                                                       
         CLC   KEY(27),KEYSAVE                                                  
         BNE   AP40                                                             
         MVI   KEY+27,X'FF'                                                     
         GOTO1 VWRITE                                                           
         BAS   RE,APCHECK                                                       
* ADD NEW PTR                                                                   
AP40     MVC   KEY,0(R3)                                                        
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         BAS   RE,APCHECK                                                       
         CLC   KEY(27),KEYSAVE     KEY ALREADY THERE?                           
         BE    *+14                                                             
         MVC   KEY,KEYSAVE                                                      
         B     AP50                                                             
* UNDELETE OLD PTR                                                              
         MVI   KEY+27,0                                                         
         GOTO1 VWRITE                                                           
         BAS   RE,APCHECK                                                       
         B     AP100                                                            
* ADD PTR                                                                       
AP50     MVI   KEY+27,0                                                         
         MVC   KEY+28(4),0(R4)     DISK ADDR                                    
         GOTO1 VADD                                                             
         BAS   RE,APCHECK                                                       
*                                                                               
* NEXT POINTER                                                                  
AP100    LA    R2,32(R2)                                                        
         LA    R3,32(R3)                                                        
         CLI   0(R3),0             LAST?                                        
         BNE   AP25                                                             
         MVI   DMOUTBTS,X'FD'                                                   
         B     EXXIT                                                            
*                                                                               
APCHECK  TM    DMCB+8,X'FD'        ALL BUT DELETE                               
         BCR   8,RE                                                             
         DC    H'0'                                                             
*                                                                               
EXXIT    XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* ROUTINE TO VERIFY RIS KEYS                                                    
***********************************************************************         
CHECKPTR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY             VALIDATE TYPE                                
         MVI   KEY,X'8D'                                                        
         MVC   KEY+1(2),REPALPHA                                                
         GOTO1 DATCON,DMCB,(3,RCONDATE),(2,KEY+8)    START DATE                 
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(2,KEY+10)  END DATE                  
         MVC   KEY+12(4),RCONKCON                                               
         NI    DMINBTS,X'FF'-X'08' DON'T PASS DELETES                           
         GOTO1 VHIGH                                                            
         CLC   KEY(16),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   KEY+16,1                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    DMINBTS,X'FF'-X'08' DON'T PASS DELETES                           
         GOTO1 VSEQ                                                             
*                                                                               
         CLI   KEY+16,2                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VSEQ                                                             
*                                                                               
         CLI   KEY+16,3                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VSEQ                                                             
*                                                                               
         XC    KEY,KEY             VALIDATE TYPE                                
         MVI   KEY,X'8E'                                                        
         MVC   KEY+1(2),REPALPHA                                                
         MVC   KEY+3(5),RCONKSTA                                                
         GOTO1 DATCON,DMCB,(3,RCONDATE),(2,KEY+8)    START DATE                 
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(2,KEY+10)  END DATE                  
         MVC   KEY+12(4),RCONKCON                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(16),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   KEY+16,1                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VSEQ                                                             
*                                                                               
         CLI   KEY+16,2                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VSEQ                                                             
*                                                                               
         CLI   KEY+16,3                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VSEQ                                                             
*                                                                               
CHKPX    DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
*********************************************************************           
*                                                                   *           
* TYPCHG CHECKS TO SEE IF CHANGES ARE VALID UNDER SPOTPAK TRANSFER  *           
* RULES.                                                            *           
*                                                                   *           
* CKXFER CHECKS IF ANY BUYLINES ARE ATTACHED TO K                   *           
*                                                                   *           
* PRDLOCK DECREMENTS PREVIOUS PRD CODE LOCK COUNTER                 *           
*                                                                   *           
* TYPCHG AND CKXFER WILL RETURN WITH CC<>0 FOR AN ERROR             *           
*                                                                   *           
*********************************************************************           
*                                                                   *           
TYPCHG   CSECT                                                                  
         NMOD1 0,*0210*                                                         
         L     RC,0(R1)                                                         
         CLC   RCONTYPE,8(R2)           ATTEMPT TO CHANGE CONTYPE?              
         BE    TCGOOD                   NO - MUST BE OK                         
         TM    PROFILES+CNTTYPCB,CNTTYPCA                                       
         BZ    TC100                                                            
*MN                                                                             
         TM    PROFILES+CNTDOCTB,CNTDOCTA                                       
         BZ    TC04                                                             
         LA    R2,CONTYPEH                                                      
         CLI   RCONTYPE,0                                                       
         BE    *+12                                                             
         CLI   RCONTYPE,C'S'                                                    
         BNE   TCBAD                                                            
         CLI   8(R2),C'X'                                                       
         BE    TC100                                                            
         CLI   8(R2),C'N'                                                       
         BE    TC100                                                            
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTYPE?                    
         BE    TCBAD               NO  -                                        
         CLC   8(1,R2),TWARTS      YES - SCREEN = OPTIONAL?                     
         BE    TC100               YES                                          
         B     TCBAD                                                            
TC04     EQU   *                                                                
*MN                                                                             
*                                                                               
         MVI   HALF,C'N'                USE N/X/OPT TYPE ERROR MSG              
         CLI   RCONTYPE,C'X'                                                    
         BE    TC10                                                             
         CLI   RCONTYPE,C'N'                                                    
         BE    TC10                                                             
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTYPE?                    
         BE    TC05                NO  -                                        
         CLC   RCONTYPE,TWARTS     YES - CONTYPE = OPTIONAL?                    
         BE    TC10                YES                                          
* TYPE NOT CURRENTLY N/X/OPTIONAL, CAN'T BECOME N/X/OPTIONAL IF BUYS            
TC05     EQU   *                                                                
         CLI   8(R2),C'X'                                                       
         BE    TC50                                                             
         CLI   8(R2),C'N'                                                       
         BE    TC50                                                             
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTYPE?                    
         BE    TC100               NO  -                                        
         CLC   8(1,R2),TWARTS      YES - SCREEN = OPTIONAL?                     
         BE    TC50                YES                                          
         B     TC100                                                            
* TYPE IS CURRENTLY N/X/OPT'L, MUST STAY N/X/OPT'L IF BUYS                      
TC10     CLI   8(R2),C'X'                                                       
         BE    TC100                                                            
         CLI   8(R2),C'N'                                                       
         BE    TC100                                                            
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTYPE?                    
         BE    TC50                NO  -                                        
         CLC   8(1,R2),TWARTS      YES - SCREEN = OPTIONAL?                     
         BE    TC100               YES                                          
*                                                                               
TC50     XC    RBUYKEY,RBUYKEY                                                  
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,REPALPHA                                                
         MVC   RBUYKCON,TWACNUM                                                 
         MVC   KEY,RBUYKEY                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(22),KEYSAVE                                                  
         BE    TCBAD               BUYS EXIST - ERROR                           
*                                                                               
* CHECK IF CURRENT OR FUTURE CONTYPE IS RESTRICTED                              
*                                                                               
TC100    DS    0H                                                               
         MVI   HALF,C'F'                USE 'FROM' TYPE ERROR MSG               
         TM    TWAPRFA,X'80'            CURRENT TYPE RESTRICTED?                
         BO    TC150                    YES - GO CHECK BUYS                     
*                                                                               
         MVI   HALF,C'T'                USE 'TO' TYPE ERR MSG                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'32'                                                        
         MVC   KEY+24(2),REPALPHA                                               
         MVC   KEY+26(1),8(R2)                                                  
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RCTYKEY),KEYSAVE                                           
         BNE   TCGOOD                   NO CONTYPE REC - NOT RESTRICTED         
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   TCGOOD                                                           
         USING RCTYFEL,R6                                                       
         TM    RCTYFPRA,X'80'                                                   
         BZ    TCGOOD                                                           
         DROP  R6                                                               
*                                                                               
TC150    XC    RBUYKEY,RBUYKEY                                                  
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,REPALPHA                                                
         MVC   RBUYKCON,TWACNUM                                                 
         MVC   KEY,RBUYKEY                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(22),KEYSAVE                                                  
         BNE   TCGOOD              NO BUYS EXIST - GOOD                         
*                                                                               
TCBAD    LA    R0,1                                                             
         B     *+6                                                              
TCGOOD   SR    R0,R0                                                            
         LTR   R0,R0                                                            
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* CKXFER CHECKS IF ANY BUYLINES ARE ATTACHED TO K                   *           
*********************************************************************           
*                                                                               
CKXFER   CSECT                                                                  
         NMOD1 0,*0210*                                                         
         L     RC,0(R1)                                                         
*                                                                               
         CLI   RCONTYPE,C'N'                                                    
         BE    CKXFER05                                                         
         CLI   RCONTYPE,C'X'                                                    
         BNE   CXGOOD                                                           
*                                                                               
CKXFER05 DS    0H                                                               
         TM    PROFILES+CNTSPOTB,CNTSPOTA                                       
         BO    CKXFER10                                                         
         TM    PROFILES+CNTTYPCB,CNTTYPCA                                       
         BZ    CXGOOD                                                           
*MN                                                                             
         TM    PROFILES+CNTDOCTB,CNTDOCTA                                       
         BO    CXGOOD                                                           
*MN                                                                             
*                                                                               
* GET FIRST BUY RECORD                                                          
CKXFER10 XC    RBUYKEY,RBUYKEY                                                  
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,REPALPHA                                                
         MVC   RBUYKCON,TWACNUM                                                 
         MVC   KEY,RBUYKEY                                                      
         OI    DMINBTS,X'08'       READ DELETE/CANCEL BUYS IF ANY               
         GOTO1 VHIGH                                                            
         NI    DMINBTS,X'FF'-X'08' RESET                                        
*                                                                               
CKXFER20 CLC   KEY(22),KEYSAVE                                                  
         BNE   CXGOOD              NO BUYS - FINE                               
*                                                                               
         OI    DMINBTS,X'08'       READ DELETE/CANCEL BUYS IF ANY               
         GOTO1 VGETREC,DMCB,RBUYREC                                             
         NI    DMINBTS,X'FF'-X'08' RESET                                        
*                                                                               
         CLI   RBUYCHGI,C'X'       ONLY DELETED BUYS ARE ALLOWED                
         BNE   CXBAD                                                            
*                                                                               
         OI    DMINBTS,X'08'       READ DELETE/CANCEL BUYS IF ANY               
         GOTO1 VSEQ                                                             
         NI    DMINBTS,X'FF'-X'08' RESET                                        
         B     CKXFER20                                                         
*                                                                               
CXBAD    LA    R0,1                                                             
         B     *+6                                                              
CXGOOD   SR    R0,R0                                                            
         LTR   R0,R0                                                            
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* PRDLOCK DECREMENTS PREVIOUS PRD CODE LOCK COUNTER                 *           
*********************************************************************           
PRDLOCK  CSECT                                                                  
         NMOD1 0,*0210*                                                         
         L     RC,0(R1)                                                         
*                                                                               
         CLC   SVPRD,MYSPACES      NO PRD CODE, DON'T BOTHER                    
         BE    PRDLXIT                                                          
*                                                                               
         XC    RPRDKEY,RPRDKEY     FIND PREVIOUS PRD REC SO WE                  
         MVI   RPRDKTYP,X'09'      CAN DECREMENT LOCK COUNTER                   
         MVC   RPRDKADV,SVADV      PREVIOUS ADVERTISER                          
         MVC   RPRDKPRD,SVPRD      PREVIOUS PRD CODE                            
         MVC   RPRDKREP,REPALPHA                                                
         MVC   KEY,RPRDKEY                                                      
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   PRDLXIT             IF NOT PRESENT, DON'T WORRY ABOUT IT         
*                                  MIGHT BE DELETED ALREADY                     
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,RPRDREC                                             
         MVI   ELCODE,X'01'                                                     
         LA    R6,IOAREA                                                        
         BAS   RE,GETEL                                                         
         BNE   PRDLXIT                                                          
*                                                                               
         OC    RPRDLOCK,RPRDLOCK   ALREADY ZERO, DON'T SUBT.. COUNTER           
         BZ    PRDLXIT             MIGHT NOT BE IN SYNC, BUT LET IT GO          
         ZICM  RF,RPRDLOCK,2       DECREMENT LOCK COUNTER                       
         SH    RF,=H'1'                                                         
         STCM  RF,3,RPRDLOCK                                                    
         GOTO1 VPUTREC,DMCB,RPRDREC                                             
         B     PRDLXIT                                                          
*                                                                               
PRDLXIT  XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* CHKDARE CHECKS FOR DARE ORDERS.  IF FOUND, PERMITS SKIP OF CHECKS *           
*********************************************************************           
CHKDARE  CSECT                                                                  
         NMOD1 0,*CKDA*                                                         
         L     RC,0(R1)                                                         
         LA    R2,RCONELEM         LOOK FOR DARE ELEMENT                        
CHDA0020 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    CHDA0160            YES - NOT A DARE ORDER                       
*                                     COULD BE AN ADD(S) REQUEST                
         CLI   0(R2),X'1F'         EXTENDED DESCRIPTION ELEMENT?                
         BE    CHDA0040            YES - CHECK FOR CONFIRM STATUS               
         ZIC   RF,1(R2)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R2,RF                                                            
         B     CHDA0020            GO BACK FOR NEXT                             
CHDA0040 EQU   *                                                                
         USING RCONXEL,R2                                                       
*                                                                               
         TM    RCONCONF,X'40'      CONFIRMED?                                   
         BO    CHDA0160            YES - MUST BE UNCONFIRMED                    
         TM    RCONCONF,X'20'      PREVIOUSLY CONFIRMED?                        
         BO    CHDA0160            YES - MUST BE UNCONFIRMED                    
*                                                                               
         DROP  R2                                                               
*                                                                               
CHDA0060 EQU   *                                                                
         LA    R2,RCONELEM         LOOK FOR DARE ELEMENT                        
CHDA0080 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    CHDA0160            YES - NO DARE ELEMENT                        
         CLI   0(R2),X'1D'         DARE ELEMENT?                                
         BE    CHDA0100            YES - CHECK FOR ACTIVITY                     
         ZIC   RF,1(R2)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R2,RF                                                            
         B     CHDA0080            GO BACK FOR NEXT                             
CHDA0100 EQU   *                                                                
         USING RCONDREL,R2                                                      
*                                                                               
         CLI   RCONDRFG,0          ANYTHING SET IN STATUS BYTE?                 
         BE    CHDA0160            NO  - NOT DARE CONTRACT                      
         TM    RCONDRFG,X'04'+X'02' KATZ EDI ORDER? OR ONE SHOT                 
         BNZ   CHDA0160            YES, DON'T TREAT AS DARE CONTRACT            
CHDA0140 EQU   *                   RETURN WITH CC= ZERO                         
         SR    R0,R0               SET CC = ZERO                                
         B     CHDA0180            DARE ORDER IN PROGRESS                       
CHDA0160 EQU   *                   RETURN WITH CC NOT = ZERO                    
         LTR   RB,RB               SET CC NOT = ZERO                            
*                                     NOT A DARE ORDER                          
CHDA0180 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* VALIDATE FILE COMMENTS, SFM COMMENTS AND/OR FREE FORM COMMENTS                
*********************************************************************           
CMTRNT   CSECT                                                                  
         NMOD1 0,*CMTE*                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         LA    R2,CONCOM1H         1ST COMMENT                                  
         TM    TWASTAOP,X'80'      STA OPTION #10?                              
         BZ    CMTRNT05            NO - SKIP CHECK                              
         TM    TWASTREO,TWARCUQ    SCRIPT IN PROGRESS?                          
         BNZ   CMTRNT05            YES, SKIP CHECK                              
*                                                                               
         TM    TWAGENFG,TWQGOGEN   AUTO HEADER GEN MODE?                        
         BZ    CMTRNT03                                                         
         TM    AUTOD.RCAUFLAG,X'40'                                             
         BZ    CMTRNT05            SKIP REQUIRED FIELD FOR AUTOHEADER?          
*                                                                               
CMTRNT03 DS    0H                                                               
         CLC   =C'C=',8(R2)        YES - REQUIRE STANDARD CMT                   
         BE    CMTRNT05                                                         
         LA    R3,600              ERROR, RETURN CONTROL TO USER                
         B     ERROR                                                            
*                                                                               
CMTRNT05 TM    4(R2),X'20'         VALID?                                       
         BO    CMTRNTX                                                          
*                                                                               
         CLC   CONACT(3),=C'ADD'   FOR ACTION ADD, RCONKREP IS NOT              
         BNE   CMTRNT10            ESTABLISHED, YET                             
         OC    RCONKREP,RCONKREP   WE'LL HAVE TO MOVE IT IN HERE                
         BNZ   CMTRNT10                                                         
         MVC   RCONKREP,REPALPHA                                                
*                                                                               
CMTRNT10 DS    0H                                                               
         CLC   =C'C=TO',CONCOM1    FLAG IF TAKEOVER CONTRACT                    
         BNE   CMTRNT20                                                         
         CLC   CONSTA(3),CONCOM1+4 CHECK CALL LETTER MATCH                      
         BNE   CMTRNT20            THREE LETTERS ARE CLOSE ENOUGH               
         OI    RCONMODR,X'18'      CONTRACT IS TAKEOVER/NOT PENDING             
                                                                                
CMTRNT20 L     R7,AIO4                                                          
         GOTO1 VREGENSC,DMCB,(0,CONCOM1H),(R7),DATAMGR,RCONREC,GETTXT           
         BZ    *+12                                                             
         L     RD,BASERD           ERROR, RETURN CONTROL TO USER                
         B     EXXMOD                                                           
*                                                                               
         MVI   UPVER,1     ACE/GRAPHNET FLAG - UP VERSION & UNCONFIRM           
         GOTO1 VDELELEM,DMCB,(2,RCONREC)     DELETE COMMENTS (CHANGE)           
*                                                                               
         GOTO1 =A(GETCMT),RR=Y                                                  
         BNZ   CMTRNTX                                                          
*                                                                               
         CLI   5(R2),0                                                          
*        BE    CMTRNTX                                                          
         BE    CMTRNT30                                                         
         MVC   WORK2+2(60),CONCOM1                                              
         MVI   WORK2,2             COMMENT ELEM CODE                            
*                                                                               
         IC    RE,5(R2)            LENGTH                                       
         LA    RE,2(RE)                                                         
         STC   RE,WORK2+1          ELEM LEN                                     
*              ADD 1ST COMMENT ELEMENT                                          
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2   ADD 1ST COMMENT ELEM               
*                                                                               
CMTRNT30 DS    0H                                                               
         CLI   CONCOM2H+5,0        2D COMMENT?                                  
         BE    CMTRNTX                                                          
*                                                                               
         L     R7,AIO4                                                          
         GOTO1 VREGENSC,DMCB,(0,CONCOM2H),(R7),DATAMGR,RCONREC,GETTXT           
         BZ    *+12                                                             
         L     RD,BASERD           ERROR, RETURN CONTROL TO USER                
         B     EXXMOD                                                           
*                                                                               
CMTRNT50 DS    0H                                                               
         IC    RE,CONCOM2H+5       LENGTH                                       
         LA    RE,2(RE)                                                         
         STC   RE,WORK2+1          2D COMMENT LENGTH                            
         MVC   WORK2+2(60),CONCOM2                                              
         MVI   WORK2,2             COMMENT ELEM CODE                            
*              ADD 2D COMMENT                                                   
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
*                                                                               
CMTRNTX  XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* STATION FIELD EDIT                                                            
**********************************************************************          
STAEDIT  CSECT                                                                  
         NMOD1 0,**STAE**                                                       
         L     RC,0(R1)                                                         
*&&DO                                                                           
         CLC   REPALPHA,=C'PV'     PETRY SPECIAL TEST?                          
         BNE   STAPV010            NO                                           
         CLC   CONSTA(4),=C'WFXT'   SWITCHED STATION?                           
         BE    STAPV001                                                         
         CLC   CONSTA(4),=C'WNYW'   SWITCHED STATION?                           
         BE    STAPV001                                                         
         CLC   CONSTA(4),=C'KTTV'   SWITCHED STATION?                           
         BE    STAPV001                                                         
         CLC   CONSTA(4),=C'WFLD'   SWITCHED STATION?                           
         BE    STAPV001                                                         
         CLC   CONSTA(4),=C'WTXF'   SWITCHED STATION?                           
         BNE   STAPV010                                                         
STAPV001 EQU   *                                                                
         LA    R2,CONCACTH                                                      
         LA    R3,PETRYFOX                                                      
         B     ERROR                                                            
*&&                                                                             
STAPV010 EQU   *                                                                
         LA    R2,CONSTAH                                                       
         LA    R3,STAERR                                                        
*                                                                               
         TM    4(R2),X'20'         READ STATION RECORD IF STATION               
         BZ    STAED009            1)NOT VALID                                  
         TM    CONSALH+4,X'20'                                                  
         BZ    STAED001            2)SALESPERSON NOT VALID                      
         TM    CONCOM1H+4,X'20'                                                 
         BZ    STAED001            3)COMMENT NOT VALID                          
         TM    CONDTESH+4,X'20'                                                 
         BO    STAEDX             4)EFFECTIVE START/END DATES NOT VALID         
*                                                                               
* STATION VALID BUT WE NEED TO RE-READ IT FOR VALIDATION OF                     
* SALESPERSON AND/OR EFFECTIVE CONTRACT START/END DATES                         
*                                                                               
STAED001 DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(5),RCONKSTA                                               
         BAS   R5,GETSTA                                                        
*                                                                               
         LA    R4,15               MAX 15 SETS OF OFF/TEAM PAIRS                
         LA    R5,TWASTOTC                                                      
*                                                                               
         MVC   TWASTJDT,RSTASTRT   SAVE OFF STATION JOIN DATE FOR               
*                                  VALIDATING EFFECTIVE DATES                   
         MVI   TWASTAOP,0                                                       
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'08'        GO FETCH AND SAVE OFF ADDITIONAL             
         BAS   RE,GETEL            STATION RECORD OPTION BITS                   
         BNE   *+10                                                             
         USING RSTAXXEL,R6                                                      
         MVC   TWASTAOP,RSTAOPTA                                                
         XC    TWAMCAST,TWAMCAST   CLEAR ALTERNATE CALENDAR STNS                
         TM    TWASTAOP,X'20'      ALTERNATE CALENDAR STATION?                  
         BNO   STAED002            NO                                           
         MVC   TWAMCAST(5),RCONKSTA  YES - INSERT INTO TABLE                    
STAED002 EQU   *                                                                
         DROP  R6                                                               
*                                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'04'        OFFICE/TEAM ELEMENT                          
         BAS   RE,GETEL                                                         
         USING RSTAOTEL,R6                                                      
         BNE   STAEDX                                                           
         B     STAED005                                                         
*                                                                               
STAED004 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   STAEDX                                                           
*                                                                               
STAED005 DS    0H                                                               
         MVC   0(4,R5),RSTAOTOF                                                 
         OC    0(4,R5),MYSPACES                                                 
         LA    R5,4(R5)                                                         
         BCT   R4,STAED004                                                      
         B     STAEDX                                                           
         DROP  R6                                                               
*                                                                               
STAED009 DS    0H                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 =A(PROCHK),RR=Y                                                  
         BNE   *+12                                                             
         LA    R3,563                                                           
         B     ERROR               NO CHANGE W/PROPOSALS                        
*                                                                               
         CLI   TWAACTMD,CONCHG_Q   CHANGE?                                      
         BNE   STAED00D                                                         
*                                                                               
         TM    AUTOD.RCAUFLAG,X'10'                                             
         BNZ   STAED010                                                         
*                                                                               
         GOTO1 =A(SLWCHK),RR=Y                                                  
         BNE   *+12                                                             
         LA    R3,810                                                           
         B     ERROR               NO CHANGE W/SELWIN                           
*                                                                               
*** COMBO                                                                       
STAED010 DS    0H                                                               
         CLI   TWACOMBO,0          FOR COMBO CONTRACT,                          
         BE    STAED00A                                                         
         CLI   TWACMBPT,1          AFTER FIRST PASS,DON'T CHECK STATION         
         BH    STAEDX              (WE ARE LOOPING IN THE 01 MODULE)            
         LA    R3,286              CANNOT CHANGE COMBO CONTRACT STATION         
         B     ERROR                                                            
*** COMBO                                                                       
*                                                                               
STAED00A DS    0H                                                               
*                                                                               
* CAN'T CHANGE TO COMBO AT ALL. ONLY VALID FOR ACTION ADD                       
*                                                                               
         CLI   CONSTA+5,C'C'       CHECK FOR COMBO STATION                      
         BE    STAED00B                                                         
         CLI   CONSTA+4,C'C'       TEST IF 3 CHAR CALL LETTERS                  
         BNE   STAED00C                                                         
*                                                                               
STAED00B DS    0H                                                               
         LA    R3,365              CANNOT CHANGE TO A COMBO STATION             
         B     ERROR                                                            
*                                                                               
STAED00C DS    0H                                                               
         GOTO1 =A(CKXFER),DMCB,(RC),RR=Y                                        
         BZ    STAED00D                                                         
         LA    R3,272              CANT CHG STA ONCE K XFERRED                  
         B     ERROR                                                            
STAED00D DS    0H                                                               
         MVI   UPVER,1    ACE/GRAPHNET FLAG - UP VERSION & UNCONFIRM            
         GOTO1 VMOVE                                                            
         XC    IOAREA(32),IOAREA                                                
         MVI   RSTAKTYP,2          STATION REC TYPE                             
         MVC   RSTAKREP,REPALPHA                                                
*                                                                               
         MVC   RSTAKSTA,WORK                                                    
         CLI   WORK+4,C'-'         BAND?                                        
         BNE   *+14                                                             
         MVC   RSTAKSTA+4(1),WORK+5                                             
         B     *+16                                                             
         CLI   WORK+3,C'-'                                                      
         BNE   *+8                                                              
         MVI   RSTAKSTA+3,C' '                                                  
*                                                                               
         CLI   RSTAKSTA+4,C'T'     TV?                                          
         BNE   *+8                                                              
         MVI   RSTAKSTA+4,C' '                                                  
*                                                                               
         MVC   KEY,IOAREA                                                       
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         MVC   TWASTAST,RSTASTAT                                                
*                                                                               
         MVI   TWASTAOP,0                                                       
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'08'        GO FETCH AND SAVE OFF ADDITIONAL             
         BAS   RE,GETEL            STATION RECORD OPTION BITS                   
         BNE   *+10                                                             
         USING RSTAXXEL,R6                                                      
         MVC   TWASTAOP,RSTAOPTA                                                
         XC    TWAMCAST,TWAMCAST   CLEAR ALTERNATE CALENDAR STNS                
         TM    TWASTAOP,X'20'      ALTERNATE CALENDAR STATION?                  
         BNO   STAED00E            NO                                           
         MVC   TWAMCAST(5),RSTAKSTA  YES - INSERT INTO TABLE                    
STAED00E EQU   *                                                                
         DROP  R6                                                               
*                                                                               
***>     CLC   =C'ADD',CONACT                                                   
***>     BNE   STAED2                                                           
         TM    CONSTAH+4,X'20'     ACTUAL CHANGE TO STATION?                    
         BO    STAED2              NO - SKIP THIS CHECK                         
         TM    RSTASTAT,X'40'      TEST FOR STATION LOCKOUT                     
         BZ    STAED1                                                           
         CLC   =C'ACC-',CONBUY     ALLOW USE OF CONTRACT IF WE ARE              
         BE    STAED1              DOING ACCOUNTING CONTRACTS                   
*                                                                               
* IF STATION HAS CONTRACT=NO BUT CONTRACT PROFILE 63 IS ON AND CALLER           
* IS PROPOSER, ALLOW THE USE OF THE CONTRACT PROGRAM                            
*                                                                               
         TM    PROFILES+CNTPROPB,CNTPROPQ   PROF 63                             
         BZ    STAED00F                                                         
         CLC   TWAGENCP,=C'SWP'                                                 
         BE    STAED1                                                           
*                                                                               
STAED00F EQU   *                                                                
         LA    R3,348              STATION LOCKOUT                              
         B     ERROR                                                            
*                                                                               
         SPACE 1                                                                
*   MARK CONTRACT AS ACE IF STATION RECORD HAS RECEIVING ID WHEN                
*   CONTRACT IS ADDED.                                                          
*   MARK IT GRAPHNET IF THAT RECEIVING ID IS 'GRAPH' (X'0406')                  
         SPACE 1                                                                
STAED1   DS    0H                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   STAED2              NO X'05' - NOT ACE/GRAPHNET                  
         OC    10(2,R6),10(R6)     IS THERE RECEIVING ID                        
         BZ    STAED2              NO                                           
         CLC   10(2,R6),=X'0406'   GRAPHNET?                                    
         BNE   STAED1A                                                          
         OI    RCONMODR+1,X'40'    YES,MARK CONTRACT GRAPHNET                   
         NI    RCONMODR+1,X'FF'-X'80' INCASE IT WAS ACE                         
         B     STAED2                                                           
*                                                                               
STAED1A  DS    0H                                                               
         OI    RCONMODR+1,X'80'    OTHERWISE, IT'S ACE                          
         NI    RCONMODR+1,X'FF'-X'40' INCASE IT WAS EASYLINK                    
         SPACE 1                                                                
*                                                                               
*- IF CONTRACT HAS ANY MONEY CALL LETTERS MAY NOT BE CHANGED.                   
STAED2   EQU   *                                                                
         CLI   TWAACTMD,CONCHG_Q   CHANGE?                                      
         BNE   STAED20                                                          
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    STAED15             ESTIMATE $'S                                 
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BE    STAED15             INVOICED $'S                                 
*                                                                               
         B     STAED20             NO MONEY.                                    
*                                                                               
STAED15  CLC   RCONKSTA,RSTAKSTA                                                
         BE    STAED20             SAME CALL LETTERS                            
*                                                                               
         LA    R3,349              MONEY ASSIGNED, NO CHANGE ALLOWED            
         B     ERROR                                                            
*                                                                               
STAED20  DS    0H                                                               
* SAVE OFF STATION OFF/TEAM CODES FOR VALIDATION OF SALESPERSON'S               
* OFF/TEAM CODE LATER ON                                                        
         LA    R4,15               MAX 15 SETS OF OFF/TEAM PAIRS                
         LA    R5,TWASTOTC                                                      
*                                                                               
         MVC   TWASTJDT,RSTASTRT   SAVE OFF STATION JOIN DATE FOR               
*                                  VALIDATING EFFECTIVE DATES                   
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'04'        OFFICE/TEAM ELEMENT                          
         BAS   RE,GETEL                                                         
         USING RSTAOTEL,R6                                                      
         BNE   STAED22                                                          
         B     STAED21A                                                         
*                                                                               
STAED21  DS     0H                                                              
         BAS   RE,NEXTEL                                                        
         BNE   STAED22                                                          
*                                                                               
STAED21A DS     0H                                                              
         MVC   0(4,R5),RSTAOTOF                                                 
         OC    0(4,R5),MYSPACES                                                 
         LA    R5,4(R5)                                                         
         BCT   R4,STAED21                                                       
         DROP  R6                                                               
*                                                                               
STAED22  DS    0H                                                               
         XC    TWACOMBO,TWACOMBO                                                
         CLI   CONSTA+5,C'C'       SPECIAL FOR COMBO STATION                    
         BE    STAED23                                                          
         CLI   CONSTA+4,C'C'       TEST IF 3 CHAR CALL LETTERS                  
         BNE   STAED60                                                          
*                                                                               
STAED23  OC    CONCMBS,CONCMBS     IF FIELD ALL NULL                            
         BZ    STAED25                                                          
         CLC   CONCMBS,MYSPACES    OR FIELD ALL SPACES                          
         BNE   STAED40             HAVE TO GET COMBO COMPONENTS                 
*                                                                               
STAED25  DS    0H                                                               
         SR    R4,R4               COUNT NUM OF PARTICIPATING STATIONS          
         LA    R3,284              MISSING COMPONENTS                           
         LA    R6,IOAREA                                                        
         USING RSTACSEL,R6                                                      
         MVI   ELCODE,X'0A'        COMBO STATION ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   ERROR               MISSING COMBO STATIONS                       
*                                                                               
STAED26  DS    0H                                                               
         CLI   RSTACSLN,8          ONLY NEW STATION ELEMENTS                    
         BL    STAED27             HAS PREFERRED STATUS FLAG                    
         CLI   RSTACPRF,C'-'                                                    
         BE    STAED28                                                          
*                                                                               
STAED27  DS    0H                                                               
         LA    R4,1(R4)            INCREMENT COUNTER                            
STAED28  DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    STAED26                                                          
*                                                                               
         CH    R4,=H'2'                                                         
         BL    ERROR               MUST HAVE AT LEAST TWO COMPONENTS            
*                                                                               
         LA    R2,CONCMBSH                                                      
         LA    R6,IOAREA                                                        
         USING RSTACSEL,R6                                                      
         MVI   ELCODE,X'0A'        COMBO STATION ELEMENT                        
         BAS   RE,GETEL                                                         
*                                                                               
STAED30  DS    0H                                                               
         CLI   RSTACSLN,8          ONLY NEW STATION ELEMENTS                    
         BL    STAED31             HAS PREFERRED STATUS FLAG                    
         CLI   RSTACPRF,C'-'       SKIP IF NOT PARICIPATING                     
         BE    STAED33                                                          
*                                                                               
STAED31  MVC   8(4,R2),RSTACS                                                   
         MVI   12(R2),C'-'                                                      
         MVC   13(1,R2),RSTACS+4                                                
         OI    6(R2),X'80'+X'08'   XMIT/HIGH INTENSITY                          
*                                                                               
         ZIC   R0,0(R2)            BUMP TO NEXT COMBO STATION DISPLAY           
         AR    R2,R0                                                            
         MVC   8(L'CONCMBC,R2),MYSPACES CLEAR THIS                              
         NI    6(R2),X'FF'-X'20'   MAKE SURE IT'S UNPROTECTED                   
         NI    1(R2),X'FF'-X'20'   MAKE SURE IT'S UNPROTECTED                   
         OI    6(R2),X'80'         XMIT                                         
         LA    RF,CONCMBLH         STOP IF WE ARE AT THE END                    
         CR    R2,RF                                                            
         BE    STAED38                                                          
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
STAED33  DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    STAED30                                                          
*                                                                               
STAED35  DS    0H                  NO MORE COMPONENTS,                          
         XC    8(L'CONCMBS,R2),8(R2)                                            
         XC    4(2,R2),4(R2)       CLEAR LENGTH AND INPUT INDICATORS            
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         ZIC   R0,0(R2)            BUMP TO NEXT COMBO STATION DISPLAY           
         AR    R2,R0                                                            
*                                                                               
         XC    8(L'CONCMBC,R2),8(R2)                                            
         XC    4(2,R2),4(R2)       CLEAR LENGTH AND INPUT INDICATORS            
         OI    1(R2),X'20'         MAKE PROTECTED                               
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         LA    RF,CONCMBLH         STOP IF WE ARE AT THE END                    
         CR    R2,RF                                                            
         BE    STAED38                                                          
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         B     STAED35                                                          
*                                                                               
STAED38  DS    0H                                                               
         LA    R2,CONCMBCH                                                      
         LA    R3,285              MAKE SELECTION                               
         B     ERROR                                                            
         DROP  R6                                                               
         EJECT                                                                  
STAED40  DS    0H                  VALIDATE USER SELECTION                      
         LA    R2,CONCMBCH                                                      
         CLI   7(R2),1             SEE IF NEED TO RE-DISPLAY COMBO              
         BH    STAED25               FIELDS (OLD DISP OUT LEN > 1)              
*                                    THIS MEANS THERE'S A K# HERE               
         SR    R4,R4               COUNTER FOR NUMBER SELECTED                  
*                                                                               
STAED45  DS    0H                                                               
         CLI   8(R2),C'X'          COUNT HOW MANY SELECTED                      
         BE    STAED46                                                          
*                                                                               
         CLI   8(R2),X'00'         A LITTLE MORE VALIDATION                     
         BE    STAED47                                                          
         CLI   8(R2),X'40'         NO INPUT - OK                                
         BE    STAED47                                                          
         LA    R3,2                                                             
         B     ERROR                                                            
*                                                                               
STAED46  DS    0H                                                               
         LA    R4,1(R4)            BUMP COUNTER FOR EACH SELECTED               
*                                                                               
STAED47  DS    0H                                                               
         LA    RF,CONCMBLH         STOP IF WE ARE AT THE END                    
         CR    R2,RF                                                            
         BE    STAED48                                                          
*                                                                               
         ZIC   R0,0(R2)            BUMP TO NEXT COMBO SELECTION FIELD           
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     STAED45                                                          
*                                                                               
STAED48  DS    0H                                                               
         CH    R4,=H'1'                                                         
         BH    STAED50                                                          
         LA    R2,CONCMBCH                                                      
         LA    R3,285                                                           
         B     ERROR               MUST SELECT AT LEAST 2                       
*                                                                               
STAED50  DS    0H                                                               
         STC   R4,TWACOMBO         NUMBER OF COMBO STATIONS                     
*                                                                               
STAED60  DS    0H                                                               
*** COMBO                                                                       
         MVC   WSTAEXP,RSTAMKT                                                  
         FOUT  CONSTAMH,RSTAMKT,20 MARKET                                       
         MVC   RCONKGRP,RSTAGRUP                                                
         MVC   RCONKSTA,RSTAKSTA   STATION TO CONTRACT KEY                      
         MVC   TWASTCDT,RSTACLDT   CLOSE DATE                                   
*                                                                               
STAEDX   DS    0H                                                               
*                                                                               
         GOTO1 =A(ISSTEXCL),DMCB,(RC),RR=Y                                      
*                                                                               
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* STATION FIELD EDIT                                                            
**********************************************************************          
PRDEDIT  CSECT                                                                  
         NMOD1 0,**PRDE**                                                       
         L     RC,0(R1)                                                         
*                                                                               
*MNTEST                                                                         
*        OI    PROFILES+CNTDOCTB,CNTDOCTA                                       
*MNTEST                                                                         
         LA    R2,CONPRDH                                                       
         XC    IOAREA(32),IOAREA                                                
*                                                                               
         CLI   8(R2),C'='          BROWSE REQUEST?                              
         BNE   PRD02                                                            
         XC    DUB,DUB                                                          
         MVC   DUB+4(4),RCONKADV                                                
*                                                                               
         L     RE,4(RD)            SET TO RECNT00 RD                            
         L     RE,4(RE)                                                         
         L     RE,4(RE)                                                         
         CLI   TWACOMBO,0                                                       
         BE    *+8                                                              
         L     RE,4(RE)            COMBOS NEED 1 MORE POP                       
         GOTOX (RFBROWSE,VREPFACS),DMCB,ACOMFACS,(RE),(R2),CONPRDH,    +        
               (X'80',C' PRD'),(0,DUB)                                          
         DC    H'0'           BROWSE SHOULD HAVE TAKEN IT FROM HERE             
*                                                                               
PRD02    CLC   8(2,R2),=C'C='                                                   
         BNE   PRD10                                                            
*                                                                               
         GOTO1 =A(VALAGFD),DMCB,(RC),RR=Y                                       
         LA    R2,CONPRDH                                                       
*                                                                               
         TM    PROFILES+CNTKTYPB,CNTKTYPA                                       
         BZ    PRD30                                                            
         TM    TWAGENFG,TWQGOGEN   AUTO HEADER GEN MODE?                        
         BZ    PRD05                                                            
         TM    AUTOD.RCAUFLAG,X'40'                                             
         BZ    PRD30               SKIP REQUIRED FIELD FOR AUTOHEADER?          
PRD05    CLI   CONTYPE,C'D'                                                     
         BE    PRD30                                                            
         CLI   CONTYPE,C'N'                                                     
         BE    PRD30                                                            
         CLI   CONTYPE,C'X'                                                     
         BE    PRD30                                                            
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTYPE?                    
         BE    PRD07               NO                                           
         CLC   CONTYPE(1),TWARTS   YES - CONTYPE = OPTIONAL TYPE?               
         BE    PRD30                                                            
PRD07    EQU   *                                                                
         LA    R3,442              PRD CODE ALLOWED ONLY FOR TYPES              
         B     ERROR                 D, N OR X                                  
                                                                                
PRD10    DS    0H                                                               
         TM    RCONMODR+1,X'10'    KATZ CONVERTED ORDER?                        
         BO    PRD20               YES - DON'T CHECK FOR TYPE                   
*MN                                                                             
*        TM    PROFILES+CNTDOCTB,CNTDOCTA                                       
*        BO    PRD20                                                            
*MN                                                                             
         LA    R3,67               MUST USE PRD CDE WHEN TYPE=N OR X            
*                                     OR OPTIONAL RTS TYPE                      
         LA    R4,CONTYPEH                                                      
         CLI   8(R4),C'X'                                                       
         BE    ERROR                                                            
         CLI   8(R4),C'N'                                                       
         BE    ERROR                                                            
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTYPE?                    
         BE    PRD20               NO                                           
         CLC   8(1,R4),TWARTS      YES - SCREEN TYPE = OPTIONAL TYPE?           
         BE    ERROR               YES - MUST USE PRODUCT CODE                  
*                                                                               
PRD20    DS    0H                                                               
         CLI   8(R4),C'D'                                                       
         BNE   PRD30                                                            
         TM    PROFILES+CNTPRD1B,CNTPRD1A   PRD CDE REQUIRED FOR TYPE=D         
         BZ    PRD30               NO                                           
         LA    R3,263              MUST USE PRODUCT CODE WHEN TYPE=D            
         B     ERROR                                                            
*                                                                               
PRD30    LA    R3,PRDERR                                                        
         MVC   SVPRD,RCONPRD       SAVE FOR PRODUCT LOCK                        
         TM    4(R2),X'20'         VALID?                                       
         BO    PRD40                                                            
         MVI   UPVER,1    ACE/GRAPHNET FLAG - UP VERSION & UNCONFIRM            
         B     PRD45               REVALIDATE IF CATG CHANGED                   
PRD40    TM    CONCATH+4,X'20'                                                  
         BZ    PRD45                                                            
         TM    CONADVH+4,X'20'     OR IF ADVERTISER CHANGED                     
*!!      BO    SALED                                                            
         B     PRDX                                                             
PRD45    DS    0H                                                               
         CLI   TWAACTMD,CONCHG_Q   CHANGE?                                      
         BNE   PRDED46                                                          
         GOTO1 =A(CKXFER),DMCB,(RC),RR=Y                                        
         BZ    PRDED46                                                          
         CLC   =C'C=',CONPRD       PRODUCT CODE ENTERED?                        
         BNE   PRD45A              NO                                           
         CLC   =C'**SPECIAL**',CONPRD+6                                         
*                                  YES - SPECIAL PERMISSION REQUESTED?          
         BE    PRDED46             YES - PERMIT CHANGE                          
PRD45A   EQU   *                                                                
         LA    R3,273              CANT CHG PRD ONCE K XFERRED                  
         B     ERROR                                                            
PRDED46  NI    CONCATH+4,X'DF'     REVALIDATE CATG IF PRD CHANGED               
         GOTO1 VMOVE                                                            
         GOTO1 VDELELEM,DMCB,(5,RCONREC)     DELETE ELEM (FOR CHANGE)           
* CHECK FOR PRD CODE                                                            
         CLC   WORK(2),=C'C='                                                   
         BNE   PRD100                                                           
*                                                                               
* VALIDATE ACTIVE STATUS OF POINT PERSON                                        
*                                                                               
PPRSNACT EQU   460                                                              
*                                                                               
         XC    IOAREA(32),IOAREA   RETRIEVE THE PRODUCT RECORD                  
         MVI   RPRDKTYP,9                                                       
         MVC   RPRDKADV,RCONKADV   ADVERTISER                                   
         MVC   RPRDKPRD,WORK+2                                                  
         MVC   RPRDKREP,REPALPHA                                                
         MVC   KEY,IOAREA                                                       
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR               PRODUCT RECORD NOT ON FILE                   
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         OC    RPRDCTY1(4),RPRDCTY1 ANY CONTYPE RESTRICTIONS?                   
         BZ    PRD49                NO - SKIP CHECK                             
         LA    R1,RPRDCTY1         FIRST CONTYPE                                
         LA    R3,RPRDCTY4         LAST CONTYPE                                 
PRD48    CLC   RCONTYPE,0(R1)                                                   
         BE    PRD49               MATCH - OK                                   
         LA    R1,1(R1)                                                         
         CR    R1,R3                                                            
         BNH   PRD48                                                            
         LA    R3,891              INVALID PRODUCT FOR THIS CONTYPE             
         B     ERROR                                                            
*                                                                               
PRD49    DS    0H                  PROD FOUND - CHANGE ERROR MESSAGE            
         LA    R3,PPRSNACT         POINT PERSON INACTIVE                        
         GOTO1 =A(POINTPSN),DMCB,(RC),RR=Y                                      
         BNZ   ERROR               POINT PERSON IS INACTIVE                     
*                                  RETRIEVE PROD REC FOR UPDATE                 
         XC    IOAREA(32),IOAREA                                                
         MVI   RPRDKTYP,9                                                       
         MVC   RPRDKADV,RCONKADV   ADVERTISER                                   
         MVC   RPRDKPRD,WORK+2                                                  
         MVC   RPRDKREP,REPALPHA                                                
         MVC   KEY,IOAREA                                                       
*                                                                               
         GOTO1 VHIGH               RECORD WILL BE FOUND                         
*                                                                               
PRD50    MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         MVC   SVCAT,RPRDCATG      NEED PRD CAT FOR LATER                       
*                                  PRD LOCK ROUTINE KILLS CATE                  
         MVC   WPRDEXP,RPRDNAME                                                 
         MVC   TWAPRDNM,RPRDNAME                                                
*                                                                               
         MVC   RCONPRD,WORK+2                                                   
         CLI   RCONTYPE,C'X'                                                    
         BE    PRD52                                                            
         CLI   RCONTYPE,C'N'                                                    
         BE    PRD52                                                            
         CLI   TWARTS,C'0'         ANY OPTIONAL RTS CONTRACT TYPE?              
         BE    PRD55               NO                                           
         CLC   RCONTYPE,TWARTS     CONTYPE = OPTIONAL CONTYPE?                  
         BNE   PRD55               NO                                           
PRD52    EQU   *                                                                
         MVI   ELCODE,X'03'                                                     
         LA    R6,IOAREA                                                        
         BAS   RE,GETEL                                                         
         BE    PRD55                                                            
         TM    PROFILES+CNTSPOTB,CNTSPOTA                                       
         BZ    PRD60                                                            
         LA    R3,265                                                           
         B     ERROR                                                            
*                                                                               
         USING RPRDSPOT,R6                                                      
PRD55    MVC   TWASPCL,RPRDSPCL    SPOTPAK CLIENT CODE                          
         MVC   TWASPPD,RPRDSPP1    SPOTPAK PRODUCT CODE                         
         MVC   TWASPPP,RPRDSPP2    SPOTPAK PIGGY BACK RODUCT                    
         MVC   TWASPP1,RPRDSPS1    SPOTPAK PRODUCT 1 SPLIT                      
         MVC   TWASPP2,RPRDSPS2    SPOTPAK PRODUCT 2 SPLIT                      
         MVC   TWASPES,RPRDSPES    SPOTPAK ESTIMATE NUMBER                      
         DROP  R6                                                               
*                                                                               
PRD60    OC    CONPRD(6),MYSPACES                                               
         MVC   CONPRD+6(14),RPRDNAME                                            
         FOUT  CONPRDH                                                          
*                                                                               
         ZICM  RF,RPRDLOCK,2       INCREMENT LOCK COUNTER                       
*                                                                               
         CLI   TWACOMBO,0          IF COMBO ORDER, INCREMENT BY THE             
         BE    PRD70               NUMBER OF CONTRACTS THAT WILL BE             
         ZIC   RE,TWACOMBO         USING THIS PRODUCT CODE                      
         AR    RF,RE                                                            
         B     PRD75                                                            
*                                                                               
PRD70    LA    RF,1(RF)                                                         
PRD75    STCM  RF,3,RPRDLOCK                                                    
         GOTO1 VPUTREC,DMCB,RPRDREC   UPDATE LOCK COUNTER IN PRD REC            
         GOTO1 =A(PRDLOCK),DMCB,(RC),RR=Y                                       
*                                                                               
* GET DAYPART MENU                                                              
*                                                                               
         L     R6,AIO4                                                          
         GOTO1 VREGENDP,DMCB,RPRDREC,(R6),DATAMGR,ACOMFACS,CALLOV,     X        
               VMEDGET                                                          
         MVC   TWADAYPT(L'TWADAYPT),0(R6)                                       
*                                                                               
*!!      B     CATED                                                            
         B     PRDX                                                             
*                                                                               
* NO CODE - JUST USE PRODUCT INPUT                                              
PRD100   LA    R3,1                ERROR, MISSING INPUT FIELD                   
         TM    TWASTREO,TWARCUQ    SCRIPT IN PROGRESS?                          
         BZ    *+8                                                              
         LA    R3,1015             TRAFFIC PRODUCT NAME REQUIRED                
*                                                                               
         CLC   WORK(20),MYSPACES   FORCE SOME INPUT                             
         BE    ERROR                                                            
         MVC   WORK2+2(20),WORK    PRODUCT                                      
         MVC   WORK2(2),=X'0516'   EXPANSION ELEM CODE + LENGTH                 
*                                                                               
*              ADD PRODUCT EXPANSION ELEMENT                                    
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2   IN BASE                            
         MVC   RCONPRD,MYSPACES                                                 
*                                                                               
PRDX     DS    0H                                                               
         XMOD1                                                                  
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* COMMENT OUT FOR ADDRESSIBILITY AS COMBO NO LONGER SUPPORTED                   
*&&DO                                                                           
**********************************************************************          
* FOR COMBO COMPONENT STATIONS, CHECK                                           
* - STATION LOCKOUT                                                             
* - CONTRACT START DATE ON OR AFTER STATION JOIN DATE                           
**********************************************************************          
VALCOMST CSECT                                                                  
         NMOD1 0,**VCST**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         XC    TWAMCAST,TWAMCAST   CLEAR ALTERNATE CALENDAR STATIONS            
*                                                                               
         OC    TWACOMBO,TWACOMBO   EXIT IF NOT COMBO                            
         BZ    VALSX                                                            
*                                                                               
         LA    R2,CONCMBSH         POINT TO COMPONENTS                          
*                                                                               
VALS010  DS    0H                                                               
         LR    R4,R2               CHECK IF STATION IS SELECTED                 
         ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         CLI   8(R4),C'X'          X DENOTES SELECTION                          
         BE    VALS020             EXIT IF NOT SELECTED                         
*                                                                               
         LA    RF,CONCMBLH         IF 4TH AND LAST STATION                      
         CR    R2,RF               WE ARE DONE                                  
         BNL   VALSX                                                            
VALS015  ZIC   R0,0(R2)            ELSE, BUMP TO NEXT CALL LETTER               
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     VALS010                                                          
*                                                                               
VALS020  DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         MVC   KEY+20(2),REPALPHA                                               
         MVC   KEY+22(4),8(R2)     CALL LETTER                                  
         MVC   KEY+26(1),13(R2)    BAND                                         
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VALS100                                                          
         GOTO1 VGETREC,DMCB,IOAREA GET STATION RECORD                           
*                                                                               
         LA    R6,IOAREA                                                        
IOD      USING RSTAREC,R6                                                       
*                                                                               
         TM    IOD.RSTASTAT,X'40'      TEST FOR STATION LOCKOUT                 
         BO    VALS200                                                          
*                                                                               
         CLC   IOD.RSTASTRT,RCONDATE                                            
         BH    VALS300             CHECK STA JOIN DATE VS K STRT DATE           
*                                                                               
*   CHECK FOR ALTERNATE CALENDAR USAGE FOR THIS STATION                         
*                                                                               
         MVC   DUB(2),RSTACLDT     SAVE CLOSE DATE FOR STATION                  
         LA    RF,RSTAELEM                                                      
         DROP  IOD                                                              
VALS030  EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    VALS015             NO ELEMENT - GO BACK FOR NEXT                
         CLI   0(RF),8             EXTENDED DESCRIPTOR ELEMENT?                 
         BE    VALS040             YES                                          
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELEMENT                   
         AR    RF,RE                                                            
         B     VALS030             GO BACK FOR NEXT                             
VALS040  EQU   *                                                                
         TM    RSTAOPTA-RSTAXXEL(RF),X'20'                                      
*                                  STATION ALTERNATE CALENDAR USER?             
         BNO   VALS015             NO  - TEST FOR NEXT STATION                  
         LA    RF,TWAMCAST         YES - SLOT STATION INTO TABLE                
VALS050  EQU   *                                                                
         OC    0(5,RF),0(RF)       SLOT EMPTY?                                  
         BZ    VALS060             YES                                          
         LA    RF,7(RF)            NO  - BUMP TO NEXT SLOT                      
         B     VALS050             GO BACK FOR NEXT SLOT                        
*                                     TEST FOR TABLE FULL NOT NEEDED            
VALS060  EQU   *                                                                
         MVC   0(5,RF),KEY+22      INSERT STATION INTO TABLE                    
         MVC   5(2,RF),DUB         INSERT CLOSE DATE FOR STATION                
         B     VALS015             GO BACK FOR NEXT                             
*                                                                               
VALS100  DS    0H                                                               
         LA    R3,STAERR           INVALID STATION                              
         B     VALS500                                                          
*                                                                               
VALS200  DS    0H                                                               
         LA    R3,348              STATION LOCKOUT                              
         B     VALS500                                                          
*                                                                               
VALS300  DS    0H                                                               
         LA    R3,287              START DATE MUST BE ON OR AFTER               
         B     VALS500             STATION JOIN DATE                            
*                                                                               
VALS500  DS    0H                                                               
         LR    R2,R4               POINT TO SELECTION FIELD SINCE               
         B     ERROR               THIS SELECTION IS INVALID                    
*                                                                               
VALSX    DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
*&&                                                                             
*                                                                               
*   GENFCAST:  CHECK SAR ELEMENT.  IF NEW TYPE (LEN NOT = 120 CHARS)            
*        DELETE EXISTING FORECAST BUCKETS, AND GENERATE NEW ONES                
*        BECAUSE A FLIGHT DATE CHANGE HAS OCCURRED.                             
*                                                                               
GENFCAST CSECT                                                                  
         NMOD1 0,*FORE*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'        FIND SAR ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   GENF0060            NO SAR ELEMENT                               
         LR    R4,R6               SET A(X'12' ELEMENT)                         
         USING RSARXEL,R4          ESTABLISH USING                              
         CLI   RSARXLEN,120        OLD STYLE ELEMENT?                           
*                                  NEW STYLE ELEMENT > 120 CHARS                
         BE    GENF0060            ORIGINAL: DON'T PROJECT                      
*                                                                               
*   ROUTINE TO SPREAD FORECAST DOLLARS FROM SAR ELEMENT OVER FLIGHT             
*        OF ORDER.  CONTRACT FLIGHT DATES ARE USED, NOT SAR FLIGHT              
*        DATES.                                                                 
*                                                                               
*                                                                               
*   INITIALIZE WORKSPACE FOR FORECAST SPREADING....                             
*                                                                               
         XC    NEW23ELT,NEW23ELT   SET NEW ELEMENT                              
         MVC   NEW23ELT(2),=X'230A'                                             
*                                                                               
         GOTO1 VDELELEM,DMCB,(X'23',RCONREC)                                    
*                                  DELETE EXISTING FORECAST BUCKETS             
         GOTO1 DATCON,DMCB,(5,WORK),(0,WORK)                                    
*                                  GET TODAY'S DATE EBCDIC                      
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
*                                  GET DAY OF WEEK OF TODAY'S DATE              
         ZIC   R2,DMCB             SAVE DAY OF WEEK RETURNED                    
         BCTR  R2,0                MAKE DAY OF WEEK ZERO/MONDAY REL             
         LNR   R2,R2               NEGATE THE VALUE                             
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R2)                                      
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,NEW23ELT+4)                            
*                                  INSERT IT INTO NEW 23 ELEMENT                
         BAS   RE,GENDAYS          GENERATE DAYTABLE                            
         BAS   RE,SPREDAYS         GENERATE DAYS WITHIN TABLE                   
GENF0020 EQU   *                                                                
         SR    RF,RF                                                            
         LA    R2,DAYTABLE         ACCUMULATE TOTAL DAYS                        
GENF0030 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    GENF0040            YES                                          
         ZIC   RE,3(R2)            TAKE DAYS FROM TABLE                         
         AR    RF,RE               ACCUMULATE                                   
         LA    R2,4(R2)            BUMP TO NEXT ENTRY                           
         B     GENF0030            GO BACK FOR NEXT                             
GENF0040 EQU   *                                                                
         ST    RF,TOTDAYS          SAVE IT FOR LATER                            
         MVC   FULL,RSARXBGT       LOAD MARKET $$ BUDGET FIGURE                 
         L     RF,FULL                                                          
         ZIC   R2,RSARXSHG         ADJUST WITH SHARE GOAL                       
         MR    RE,R2               MULTIPLY MARKET $ BY SHARE GOAL              
*                                     GIVING STATION $$                         
*                                                                               
*   NOW MULTIPLY BY 10 FOR PROPER DECIMAL ALIGNMENT                             
*                                                                               
         M     RE,=F'10'           MULTIPLY BY 10                               
         L     R2,TOTDAYS          DIV STA $$ BY TOTAL DAYS                     
*                                     GIVING $$ PER DAY                         
         SLDA  RE,1                DOUBLE FOR ROUNDING                          
         AR    RF,R2               ADD TOTDAYS FOR ROUNDING                     
         DR    RE,R2               DIVIDE BY TOTDAYS                            
         SRA   RF,1                DIVIDE BY 2                                  
         ST    RF,TOTDAYS          SAVE $$ PER DAY                              
         LA    R2,DAYTABLE                                                      
GENF0050 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    GENF0060            YES - FINISHED                               
         BAS   RE,GENBUCKS         NO  - GEN X'23' FORECAST BUCKET              
         LA    R2,4(R2)            BUMP TO NEXT BUCKET                          
         B     GENF0050            GO BACK FOR NEXT                             
GENF0060 EQU   *                                                                
         XC    MYP,MYP             CLEAR PRINT AREA FOR OTHERS                  
         XMOD1                                                                  
         EJECT                                                                  
GENDAYS  NTR1                                                                   
         MVC   CYCLEDAT,RCONDATE   CONTRACT FLIGHT DATES                        
*                                                                               
*   EXTRA PRINT LINE IS USED TO SET UP BROADCAST MONTH ARRAY.                   
*                                                                               
         LA    R2,MYP              A(DAYTABLE)                                  
         XC    MYP,MYP             INITIALIZE TABLE                             
         USING BROADTBL,R2                                                      
GDAY0020 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,CYCLEDAT),(0,DAYTABLE)                            
*                                  CONVERT START DATE TO EBCDIC                 
GDAY0040 EQU   *                                                                
         GOTO1 VGTBROAD,DMCB,(1,DAYTABLE),DAYTABLE+6,GETDAY,ADDAY               
         MVC   BRDWEEKS,DMCB       INSERT NUMBER OF WEEKS                       
*                                  GET BROADCAST DATES FOR MONTH                
         CLI   DMCB,X'FF'          ERROR?                                       
         BNE   *+6                 NO                                           
         DC    H'0'                SHOULDN'T HAPPEN!!                           
         GOTO1 DATCON,DMCB,(0,DAYTABLE+6),(3,BRDSTART)                          
*                                  INSERT START DATE IN TABLE                   
         GOTO1 DATCON,DMCB,(0,DAYTABLE+12),(3,BRDEND)                           
*                                  INSERT END   DATE IN TABLE                   
         CLC   CYCLEDAT+3(3),BRDEND                                             
*                                  CONTRACT FLIGHT END REACHED?                 
         BNH   GDAY0060            YES                                          
         GOTO1 DATCON,DMCB,(3,BRDEND),(0,DAYTABLE+6)                            
*                                  CONVERT END DATE TO EBCDIC                   
         LA    RF,1                DATE INCREMENT                               
         GOTO1 ADDAY,DMCB,DAYTABLE+6,DAYTABLE,(RF)                              
*                                  GET NEXT DAY, WHICH IS FIRST                 
*                                     DAY OF NEXT BDCST MONTH                   
         LA    R2,BRDLEN(R2)       BUMP TO NEXT TABLE ENTRY                     
         B     GDAY0040            GO BACK, SET NEXT MONTH                      
GDAY0060 EQU   *                                                                
         XC    DAYTABLE(56),DAYTABLE     CLEAR THE WORKAREA                     
         LA    R2,MYP              RESET A(BDCST MONTH TABLE)                   
         LA    R3,DAYTABLE                                                      
GDAY0080 EQU   *                                                                
         CLI   BRDEND,0            ANY ENTRY?                                   
         BZ    GDAY0100            NO  - FINISHED                               
         MVC   0(2,R3),BRDEND      MOVE BDCST MON END (YM) TO TABLE             
         LA    R2,BRDLEN(R2)       BUMP TO NEXT BDCST MONTH                     
         LA    R3,4(R3)            BUMP TO NEXT DAYTABLE                        
         B     GDAY0080            GO BACK FOR NEXT                             
GDAY0100 EQU   *                                                                
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
SPREDAYS NTR1                                                                   
         LA    R2,DAYTABLE         A(DAYTABLE)                                  
         LA    R3,MYP              A(BDCST MONTH TABLE)                         
         USING BROADTBL,R3                                                      
         CLC   BRDSTART,RCONDATE   IS FLIGHT START FIRST DAY                    
*                                     OF FIRST BROADCAST MONTH?                 
         BE    SPDA0040            YES                                          
         GOTO1 DATCON,DMCB,(3,RCONDATE),(0,WORK)                                
*                                  CONVERT FLIGHT START DATE                    
         CLC   RCONDATE+3(3),BRDEND                                             
*                                  IS FLIGHT END DATE EARLIER                   
*                                     THAN BROADCAST MONTH END DATE?            
         BNL   SPDA0020            NO  -                                        
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(0,WORK+6)                            
*                                  CONVERT FLIGHT END   DATE                    
*                                                                               
*   AT THIS POINT, BOTH FLIGHT START AND END ARE WITHIN THE FIRST               
*     BROADCAST MONTH, SO THAT THE NUMBER OF DAYS CALCULATION IS                
*     DONE FROM FLIGHT START TO FLIGHT END .                                    
*                                                                               
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         MVC   DAYTABLE+2(2),DMCB+8                                             
*                                  MOVE NUM DAYS TO 1ST TABLE NTRY              
         B     SPDA0100            EXIT ROUTINE                                 
*                                                                               
*   AT THIS POINT, FLIGHT START IS OTHER THAN BEGINNING OF BDCST                
*     MONTH, AND FLIGHT END IS EITHER AFTER THE BDCST MONTH, OR                 
*     THE LAST DAY OF THE MONTH.  NUMBER OF DAYS IS CALCULATED                  
*     FROM FLIGHT START TO BDCST MONTH END.                                     
*                                                                               
SPDA0020 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,BRDEND),(0,WORK+6)                                
*                                  CONVERT FLIGHT END   DATE                    
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         MVC   DAYTABLE+2(2),DMCB+8                                             
*                                  MOVE NUM DAYS TO 1ST TABLE NTRY              
         B     SPDA0080            FIRST ENTRY DONE                             
SPDA0040 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    SPDA0100            YES                                          
         CLC   RCONDATE+3(3),BRDEND                                             
*                                  END OF FLIGHT REACHED?                       
         BL    SPDA0060            YES - PARTIAL MONTH TO DO                    
         ZIC   RF,BRDWEEKS         NO  - CALCULATE DAYS FROM WEEKS              
         SR    RE,RE                                                            
         LA    R1,7                                                             
         MR    RE,R1               MULT WEEKS BY 7                              
         STC   RF,3(R2)            INSERT # DAYS INTO TABLE                     
         B     SPDA0080            GO TO NEXT SLOT                              
SPDA0060 EQU   *                                                                
*                                                                               
*   AT THIS POINT, FLIGHT END IS OTHER THAN END OF BROADCAST                    
*     MONTH.  NUMBER OF DAYS IS CALCULATED FROM BROADCAST MONTH                 
*     START DATE THROUGH FLIGHT END.                                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,BRDSTART),(0,WORK)                                
*                                  CONVERT BROADCAST MONTH START                
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(0,WORK+6)                            
*                                  CONVERT FLIGHT END   DATE                    
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         MVC   2(2,R2),DMCB+8                                                   
*                                  MOVE NUM DAYS TO LAST TABLE NTRY             
         B     SPDA0100            FINISHED                                     
*                                                                               
SPDA0080 EQU   *                                                                
         LA    R2,4(R2)            BUMP DAYTABLE                                
         LA    R3,BRDLEN(R3)       BUMP BDCST MONTH TABLE                       
         B     SPDA0040            GO BACK FOR NEXT                             
SPDA0100 EQU   *                                                                
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
GENBUCKS NTR1                                                                   
         MVC   NEW23ELT+2(2),0(R2) INSERT MONTH INTO 23 ELT                     
         SR    RE,RE                                                            
         ZIC   RF,3(R2)            NUMBER OF DAYS FOR MONTH *                   
         M     RE,TOTDAYS             $$ PER DAY = $$ FOR MONTH                 
         SLDA  RE,1                DOUBLE FOR ROUNDING                          
         AH    RF,=H'10'           ADD FOR ROUNDING                             
         D     RE,=F'10'           DIVIDE FOR DECIMAL SCALING                   
         SRA   RF,1                DIVIDE BY 2                                  
         ST    RF,FULL                                                          
         MVC   NEW23ELT+6(4),FULL  INSERT INTO X'23' ELEMENT                    
         GOTO1 VADDELEM,DMCB,RCONREC,NEW23ELT                                   
         XIT1                                                                   
*                                                                               
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   DEVSPRSN:  CHECK X'18' ELEMENT.  SEE NEXT NOTE                              
*                                                                               
DEVSPERR EQU   415                                                              
DEVSPACT EQU   459                                                              
DEVSTMIS EQU   469                                                              
*                                                                               
DEVSPRSN CSECT                                                                  
         NMOD1 0,*DVSP*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   AN X'18' ELEMENT MAY ALREADY BE IN THE RECORD.  IT MAY CONTAIN              
*     INVOICE DATA WHICH CANNOT BE LOST.  THE ELEMENT IS SEARCHED               
*     FOR.  IF PRESENT, IT IS SAVED, AND THE DEV CODE FIELDS ARE                
*     CLEARED.  IF NOT PRESENT, A SKELETON IS INITIALIZED.  AT THE              
*     END OF VALIDATION OF THESE TWO FIELDS, THE ELEMENT AS SAVED               
*     IS CHECKED TO SEE IF ANYTHING IS PRESENT.  IF SO, THE ELEMENT             
*     IS RESTORED TO THE RECORD.                                                
*                                                                               
         XC    MYP,MYP             USE 'MY PRINT LINE' AS TEMP SPACE            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'18'        LOOK FOR DEV/INV ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   DVSP0040            NO ELEMENT                                   
         ZIC   RF,1(R6)            FOUND - MOVE TO STORAGE                      
         BCTR  RF,0                DECREMENT FOR MOVE                           
         EX    RF,DVSP0020         MOVE BY LENGTH                               
         GOTO1 VDELELEM,DMCB,(X'18',RCONREC)                                    
*                                  DELETE THE OLD ELEMENT                       
         B     DVSP0060                                                         
DVSP0020 MVC   MYP(0),0(R6)        MOVE BY LENGTH                               
DVSP0040 EQU   *                                                                
         MVC   MYP(2),=X'1808'     SET BASIS FOR ELEMENT                        
DVSP0060 EQU   *                                                                
         CLI   5(R2),0             ANY DATA?                                    
         BNE   DVSP0070            NO  - CHECK PROFILE IF REQUIRED              
                                                                                
         TM    PROFILES+CNTRDEVB,CNTRDEVA                                       
*                                  REQUIRES DEVSAL AND DEVTYPE?                 
         BZ    DVSP0160            NO - FINISHED                                
         TM    TWAGENFG,TWQGOGEN   AUTO HEADER GEN MODE?                        
         BZ    DVSP0090                                                         
         TM    AUTOD.RCAUFLAG,X'40'                                             
         BZ    DVSP0160            SKIP REQUIRED FIELD FOR AUTOHEADER?          
         B     DVSP0090            YES - ERROR!                                 
*                                                                               
DVSP0070 DS    0H                                                               
         TM    4(R2),X'20'         VALID DEVELOPMENTAL SALESMAN?                
         BO    DVSP0160            EXIT OKAY:  CC=ZERO                          
*                                  NO  - CHANGE MADE                            
         XC    MYP+2(3),MYP+2         CLEAR DEV S/P CODE                        
         XC    TWDSPEXP,TWDSPEXP      CLEAR EXPANSION AREA                      
         GOTO1 VMOVE                                                            
         XC    IOAREA(32),IOAREA                                                
*                                                                               
         MVI   RDSPKTYP,X'3A'      DEVELOPMENTAL SALESMAN REC TYPE              
         MVC   RDSPKREP,REPALPHA   REP CODE                                     
         MVC   RDSPKSAL,WORK                                                    
         MVC   KEY,IOAREA                                                       
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DVSP0100            EXIT NG:    CC=NOT ZERO                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
         OC    RDSPLEAV,RDSPLEAV   ANY LEAVE DATE?                              
         BZ    DVSP0080            NO                                           
         GOTO1 DATCON,DMCB,(5,FULL),(2,FULL)                                    
*                                  YES - CHECK LEAVE VS TODAY'S DATE            
         CLC   RDSPLEAV,FULL                                                    
         BNH   DVSP0120            INACTIVE                                     
DVSP0080 EQU   *                                                                
         MVC   TWDSPEXP,RDSPNAME                                                
*                                  SAVE EXPANSION NAME                          
         FOUT  CONDSPNH,RDSPNAME,15                                             
*                                  DEV S/P NAME                                 
         MVC   MYP+RCONDVSP-RCONDVEL(3),RDSPKSAL                                
*                                  INSERT DEV S/P CODE                          
         B     DVSP0160            EXIT OKAY:  CC = ZERO                        
DVSP0090 EQU   *                                                                
         LA    RF,DEVSTMIS         ERROR = REQUIRED FIELD IS MISSING            
         ST    RF,FULL                                                          
         B     DVSP0140                                                         
DVSP0100 EQU   *                                                                
         LA    RF,DEVSPERR         ERROR = NOT FOUND                            
         ST    RF,FULL                                                          
         B     DVSP0140                                                         
DVSP0120 EQU   *                                                                
         LA    RF,DEVSPACT         ERROR = INACTIVE                             
         ST    RF,FULL                                                          
DVSP0140 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
         B     DVSP0180                                                         
DVSP0160 EQU   *                                                                
         SR    RF,RF               SET CC = ZERO                                
         LTR   RF,RF                                                            
DVSP0180 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   POINTPSN:  CHECK X'02' ELEMENT.  IF PRESENT, CHECK FOR POINT                
*        PERSON CODE.  IF PRESENT, CHECK TO ENSURE THAT IT IS IN                
*        ACTIVE STATUS.   IF NOT, RETURN AN ERROR CONDITION.                    
*                                                                               
POINTPSN CSECT                                                                  
         NMOD1 0,*PPSN*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         LA    R6,RPRDREC                                                       
         MVI   ELCODE,X'02'        LOOK FOR DEV/INV ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   PPSN0120            NO ELEMENT - RETURN CC = ZERO                
         USING RPRDNELM,R6         ELEMENT FOUND - POINT PERSON?                
         OC    RPRDNPNT,RPRDNPNT   ANY POINT PERSON?                            
         BZ    PPSN0120            NO  - RETURN CC = ZERO                       
         XC    IOAREA(32),IOAREA   YES - SET UP THE POINT PERSON KEY            
         LA    R3,IOAREA                                                        
         USING RPTPREC,R3                                                       
         MVI   RPTPKTYP,X'31'                                                   
         MVC   RPTPKREP,REPALPHA                                                
         MVC   RPTPKREC,RPRDNPNT   ADVERTISER                                   
         MVC   KEY,IOAREA                                                       
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   PPSN0080            NOT FOUND - RETURN CC NOT = ZERO             
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                  FOUND - RETRIEVE RECORD                      
         OC    RPTPLDAT,RPTPLDAT   ANY LEAVE DATE?                              
         BZ    PPSN0120            NO  - ACTIVE - ACCEPT IT                     
         GOTO1 DATCON,DMCB,(5,FULL),(2,FULL)                                    
*                                  GET TODAY'S DATE: COMPRESSED                 
         CLC   RPTPLDAT,FULL       LEAVE DATE VS TODAY'S DATE                   
         BNH   PPSN0080            GONE - RETURN CC NOT = ZERO                  
         B     PPSN0120            OKAY - RETURN CC = ZERO                      
PPSN0080 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
         B     PPSN0160                                                         
PPSN0120 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
PPSN0160 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R3,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   DEVCONTY:  VALIDATE ANY DEVELOPMENTAL CONTRACT TYPE ENTERED.                
*                                                                               
DEVCTERR EQU   416                                                              
DEVSPREQ EQU   420                                                              
*                                                                               
DEVCONTY CSECT                                                                  
         NMOD1 0,*DVCT*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   NOTE: ALWAYS CHECK CONTRACT TYPE.  IGNORE PREVIOUSLY VALID BIT              
*****>   TM    4(R2),X'20'         VALID DEVELOPMENTAL CONTRACT TYPE?           
*****>   BO    DVCT0020                                                         
*                                  NO  - CHANGE MADE                            
         XC    MYP+5(2),MYP+5         CLEAR DEV CONTYPE CODE                    
         XC    TWDCTEXP,TWDCTEXP      CLEAR EXPANSION AREA                      
         CLI   5(R2),0             ANY DATA?                                    
         BNE   DVCT0010            YES - CHECK IT OUT                           
         LA    R3,DEVSTMIS                                                      
         TM    PROFILES+CNTRDEVB,CNTRDEVA                                       
*                                  NO  - REQUIRES DEVSAL AND DEVTYPE?           
         BZ    DVCT0005                                                         
         TM    TWAGENFG,TWQGOGEN   AUTO HEADER GEN MODE?                        
         BZ    ERROR                                                            
         TM    AUTOD.RCAUFLAG,X'40'                                             
         BZ    DVCT0005            SKIP REQUIRED FIELD FOR AUTOHEADER?          
         B     ERROR               YES - ERROR                                  
*                                                                               
DVCT0005 EQU   *                                                                
         TM    PROFILES+CNTDEVSB,CNTDEVSA                                       
*                                  NO  - S/P REQUIRES TYPE?                     
***>>>   BO    DVCT0008            NO  - FINISHED                               
         BZ    DVCT0020            NO  - FINISHED                               
         TM    TWAGENFG,TWQGOGEN   AUTO HEADER GEN MODE?                        
         BZ    DVCT0020                                                         
         TM    AUTOD.RCAUFLAG,X'40'                                             
         BZ    DVCT0008            SKIP REQUIRED FIELD FOR AUTOHEADER?          
         B     DVCT0020            NO  - FINISHED                               
*                                                                               
DVCT0008 EQU   *                                                                
         OC    MYP+2(3),MYP+2      YES - ANY S/P ENTERED?                       
         BZ    DVCT0020            NO  - CONTYPE NOT NEEDED                     
         XC    MYP,MYP             YES - CONTYPE NOT PRESENT                    
*                                     SET ERROR FLAG                            
         B     DVCT0060            RETURN WITH ERROR                            
*                                                                               
DVCT0010 EQU   *                                                                
         GOTO1 VMOVE                                                            
         XC    IOAREA(32),IOAREA                                                
*                                                                               
         MVI   RDSPKTYP,X'3B'      DEVELOPMENTAL SALESMAN REC TYPE              
         MVC   RDCTKREP,REPALPHA   REP CODE                                     
         MVC   RDCTKCTY,WORK                                                    
         MVC   KEY,IOAREA                                                       
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DVCT0060            EXIT NG:    CC NOT = ZERO                    
         GOTO1 VGETREC,DMCB,IOAREA                                              
         MVC   TWDCTEXP,RDCTDESC                                                
*                                  SAVE EXPANSION NAME                          
         FOUT  CONDCTNH,RDCTDESC,15                                             
*                                  DEV CONTYPE'S NAME                           
         MVC   MYP+RCONDVCT-RCONDVEL(2),RDCTKCTY                                
*                                  INSERT DEV CONTRACT TYPE                     
         SPACE 5                                                                
DVCT0020 EQU   *                                                                
         OC    MYP+2(L'MYP-2),MYP+2 ANYTHING IN FIELD?                          
         BZ    DVCT0040            NO  - DON'T ADD TO RECORD                    
*                                     EXIT OKAY:  CC = ZERO                     
         GOTO1 VADDELEM,DMCB,RCONREC,MYP                                        
*                                  YES - ADD ELEMENT TO RECORD                  
DVCT0040 EQU   *                                                                
         SR    RF,RF               SET CC = ZERO                                
         LTR   RF,RF                                                            
         B     DVCT0080                                                         
DVCT0060 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
DVCT0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* IS STATION ON STATIONS EXCLUSION LIST ? *                                     
*********************************************************************           
ISSTEXCL CSECT                                                                  
         NMOD1 0,*ISSTA*                                                        
         L     RC,0(R1)                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RSA2KEY,R4                                                       
*                                                                               
         MVI   RSA2KTYP,X'46'      SECONDARY SALESMAN RECORD                    
         MVC   RSA2KREP,REPALPHA   REP CODE                                     
         MVC   RSA2KSAL,CONSAL     SALESMAN INITIALS                            
         CLI   RSA2KSAL+2,C'/'                                                  
         BNE   *+8                                                              
         MVI   RSA2KSAL+2,C' '     CASE OF 2 LETTER SAL & TEAM OVERRIDE         
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ISSTEX                                                           
*                                                                               
         GOTO1 VGETREC,DMCB,AIO3                                                
         L     R6,AIO3                                                          
         USING RSALEXEM,R6                                                      
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   ISSTEX                                                           
*                                                                               
         SR    R8,R8                                                            
         IC    R8,RSALEXLN         ELEMENT LENGTH                               
         LA    R0,7                CURRENT LENGTH                               
         LA    R5,RSALEXST         CURRENT POSITION                             
         LA    R3,STAEXCL          ERROR                                        
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(5),CONSTA                                                    
         CLI   CONSTA+4,C'-'         BAND?                                      
         BNE   *+14                                                             
         MVC   DUB+4(1),CONSTA+5                                                
         B     *+16                                                             
         CLI   CONSTA+3,C'-'                                                    
         BNE   *+8                                                              
         MVI   DUB+3,C' '                                                       
*                                                                               
         CLI   DUB+4,C'T'     TV?                                               
         BNE   *+8                                                              
         MVI   DUB+4,C' '                                                       
         OC    DUB(5),=C'     '                                                 
*                                                                               
ISST10   DS    0H                                                               
         CLC   DUB(5),0(R5)        IS STATION ON EXCL LIST?                     
         BE    ERROR               YES                                          
         LA    R5,5(R5)            NEXT STATION                                 
         AH    R0,=H'5'            INCREMENT LENGTH                             
         CR    R0,R8               ALL STATIONS HAVE BEEN CHECKED?              
         BNH   ISST10              NO                                           
*                                                                               
ISSTEX   DS    0H                                                               
         XMOD1                                                                  
         DROP  R4,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* CHECK IF ANY MAKEGOOD OFFERS. IF SO, WE NEED TO CHANGE THE RECORD(S)          
* SINCE THE SALESPERSON/OFFICE CHANGED                                          
*********************************************************************           
CHGMGREC CSECT                                                                  
         NMOD1 0,*CHMGR*                                                        
         L     RC,0(R1)                                                         
*                                                                               
         XC    KEY,KEY                                                          
MGKEY    USING RMKGKEY,KEY                                                      
         MVI   MGKEY.RMKGKTYP,X'11'                                             
         MVC   MGKEY.RMKGKREP,REPALPHA                                          
         MVC   MGKEY.RMKGKOFF,WORK                                              
         MVC   MGKEY.RMKGKSTA,RCONKSTA                                          
         MVC   MGKEY.RMKGKCON,TWACNUM                                           
         DROP  MGKEY                                                            
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
*                                                                               
CMGK10   DS    0H                  EXIT FOR NO MAKEGOOD OFFERS                  
         CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE                                    
         BNE   CMGKX                                                            
*                                                                               
         MVC   WORK(27),KEY                                                     
*                                                                               
         MVI   UPDATE,C'Y'         UPDATE RECORD                                
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
IOD      USING RMKGREC,IOAREA                                                   
         MVC   IOD.RMKGKOFF,RCONKOFF                                            
*                                                                               
*   S/P CODE IS NOW BEING CONTAINED WITHIN A SPECIAL S/P-TEAM                   
*        ELEMENT, ALONG WITH ALL REQUIRED SWITCH CODES                          
*                                                                               
***      OC    IOD.RMKGKPLN(6),IOD.RMKGKPLN                                     
***      BNZ   CMGK15              UPDATE SALESPERSON FOR GROUP RECORD          
***      MVC   IOD.RMKGSAL,RCONSAL ONLY                                         
*                                                                               
         DROP  IOD                                                              
*                                                                               
CMGK15   DS    0H                                                               
         GOTO1 VPUTREC,DMCB,IOAREA                                              
*                                                                               
         OI    KEY+27,X'80'        DELETE OLD KEY                               
         GOTO1 VWRITE                                                           
*                                                                               
MGKEY    USING RMKGKEY,KEY                                                      
         MVC   MGKEY.RMKGKOFF,RCONKOFF                                          
         DROP  MGKEY                                                            
*                                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     ADD/WRITE NEW KEY                            
         BNE   CMGK20                                                           
         NI    KEY+27,X'FF'-X'80'                                               
         GOTO1 VWRITE                                                           
         B     CMGK30                                                           
*                                                                               
CMGK20   DS    0H                                                               
         MVC   KEY,KEYSAVE                                                      
         NI    KEY+27,X'FF'-X'80'                                               
         GOTO1 VADD                                                             
*                                                                               
CMGK30   DS    0H                                                               
         MVC   KEY(27),WORK                                                     
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 VHIGH               RE-READ ORIGINAL KEY AFTER A WRITE           
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VSEQ                                                             
         B     CMGK10                                                           
*                                                                               
CMGKX    DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE BUYER                                                                
* IF BUYER IS CHANGED TO ACC-BB, SET ALL X'03' BUCKET ACTIVITY DATES            
* BACK TO THE MONDAY DATE OF LAST QUARTER                                       
***********************************************************************         
BUYERED  CSECT                                                                  
         NMOD1 0,*BUYR*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         LA    R2,CONBUYH                                                       
         TM    4(R2),X'20'         VALID BUYER?                                 
         BO    BUYERX                                                           
*                                                                               
* BUYER CHANGED FROM ACC-BB, REVALIDATE FLIGHT DATES                            
*                                                                               
         CLC   =C'ACC-BB',RCONBUYR                                              
         BNE   BUYER05                                                          
         NI    CONDTESH+4,X'FF'-X'20'                                           
*                                                                               
* IF BUYER NAME CHANGES, IT DOESN'T AFFECT VERSION OR CONFIRMED STATUS          
*                                                                               
BUYER05  DS    0H                                                               
         GOTO1 VMOVE                                                            
         MVC   RCONBUYR,WORK                                                    
*                                                                               
         CLC   =C'ACC-BB',RCONBUYR                                              
         BNE   BUYERX                                                           
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   BUYERX                                                           
*                                                                               
BUYER10  DS    0H                  RETREIVE THE ACTIVITY DATE                   
         XC    WORK,WORK                                                        
*                                                                               
         USING RCONBKEL,R6         MONTH OF BUCKET DICTATES                     
         MVC   WORK2(2),RCONBKYR   WHICH MONTH TO BACK UP                       
         MVI   WORK2+2,15          THIS WILL ENSURE CORRECT MONTH               
         GOTO1 DATCON,DMCB,(3,WORK2),(0,WORK)                                   
         XC    WORK2,WORK2                                                      
         DROP  R6                                                               
*                                                                               
         GOTO1 VGTBROAD,DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                       
         GOTO1 DATCON,DMCB,(0,WORK+12),(5,WORK+18)                              
         MVC   WORK+21(2),=C'15'                                                
         MVC   WORK+26(6),=C'(- M)'                                             
*                                                                               
         TM    PROFILES+CNTBBILB,CNTBBILA                                       
         BZ    BUYER12             PROFILE TO GO GET LAST MONTH                 
         MVI   WORK+28,C'1'        INSTEAD OF LAST QUARTER?                     
         B     BUYER18                                                          
*                                                                               
BUYER12  DS    0H                  FIND BROADCAST MONTH LAST QUARTER            
         LA    R3,QTABLE           GET FIRST MONDAY OF PREVIOUS QUARTER         
BUYER13  CLC   WORK+18(3),0(R3)                                                 
         BE    BUYER15                                                          
         LA    R3,L'QTABLE(R3)                                                  
         CLI   0(R3),X'FF'                                                      
         BNE   BUYER13                                                          
         LA    R3,579                                                           
         B     ERROR                                                            
*                                                                               
BUYER15  DS    0H                                                               
         MVC   WORK+28(1),3(R3)                                                 
*                                                                               
BUYER18  DS    0H                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CPERVAL                                                       
         DROP  RE                                                               
*                                                                               
         LA    R3,579                                                           
         GOTO1 (RF),DMCB,(14,WORK+18),(0,WORK2)                                 
         CLI   DMCB+4,1                                                         
         BE    ERROR                                                            
*                                  FIND START DATE OF BROADCAST MONTH           
PDATED   USING PERVALD,WORK2                                                    
         GOTO1 VGTBROAD,DMCB,(1,PDATED.PVALESTA),WORK,GETDAY,ADDAY              
         DROP  PDATED              MOVE COMPRESSED DATE                         
*                                                                               
BUYER20  DS    0H                                                               
         USING RCONBKEL,R6                                                      
         GOTO1 DATCON,DMCB,(0,WORK),(2,RCONBKWK)                                
         DROP  R6                                                               
*                                                                               
         BAS   RE,NEXTEL           DO NEXT                                      
         BE    BUYER10                                                          
*                                                                               
BUYERX   DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
QTABLE   DS    0CL4                                                             
         DC    C'JAN',C'3'                                                      
         DC    C'FEB',C'4'                                                      
         DC    C'MAR',C'5'                                                      
         DC    C'APR',C'3'                                                      
         DC    C'MAY',C'4'                                                      
         DC    C'JUN',C'5'                                                      
         DC    C'JUL',C'3'                                                      
         DC    C'AUG',C'4'                                                      
         DC    C'SEP',C'5'                                                      
         DC    C'OCT',C'3'                                                      
         DC    C'NOV',C'4'                                                      
         DC    C'DEC',C'5'                                                      
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
* PRODUCT CODE MUST EXIST. IF AGENCY ENERED, ORDER'S AGENCY FIELD               
* MUST EQUAL THE PRODUCT AGENCY CODE. FLIGHT DATES SHOULD BE INCLUSIVE          
***********************************************************************         
VALAGFD  NMOD1 0,*VALAGFD*                                                      
         L     RC,0(R1)                                                         
* SEE IF PRODUCT EXISTS                                                         
         LA    R2,CONPRDH                                                       
         XC    RPRDKEY,RPRDKEY                                                  
         MVI   RPRDKTYP,X'09'                                                   
         MVC   RPRDKADV,CONADV     ADVERTISER CODE                              
         OC    RPRDKADV,MYSPACES                                                
         MVC   RPRDKPRD,10(R2)     PRODUCT                                      
         OC    RPRDKPRD,MYSPACES                                                
         MVC   RPRDKREP,REPALPHA   REP CODE                                     
         MVC   TWASVKEY,KEY                                                     
         MVC   KEY(27),RPRDKEY                                                  
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VALERR1            IF NOT PRESENT, ERROR                         
         MVC   KEY(27),TWASVKEY                                                 
* VALIDATE AGY AND OFFICE                                                       
         LA    R2,CONAGYH          AGENCY FIELD                                 
         CLI   5(R2),0             AGENCY CODE ENTERED?                         
         BNE   VALAG10             YES                                          
         LA    R2,CONDTESH         DATES FIELD                                  
         CLI   5(R2),0             FLIGHT DATES ENTERED?                        
         BE    VALAGFDX            NOTHING TO VALIDATE                          
VALAG10  EQU   *                                                                
         GOTO1 VGETREC,DMCB,AIO3                                                
         L     R6,AIO3                                                          
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   VALAGFDX                                                         
         USING RPRDAGFL,R6                                                      
         LA    R2,CONAGYH                                                       
         CLI   5(R2),0                                                          
         BE    VALFL20                                                          
*                                                                               
         XC    WORK2,WORK2                                                      
         GOTO1 SCANNER,DMCB,(R2),(1,WORK2),C',=,-'                              
         CLC   RPRDAGAG,=F'0'      AGENCY?                                      
         BE    VALFL20             NOTHING TO COMPARE                           
         CLC   RPRDAGAG,WORK2+12   AGENCY FIELD AND PRODUCT AGY CODE            
         BNE   VALERR2                                                          
         CLC   RPRDAGAO,MYSPACES   PARENT LEVEL?                                
         BE    VALFL20             YES                                          
         CLC   RPRDAGAO,WORK2+22   OFFICE FIELD AND PRODUCT OFF CODE            
         BNE   VALERR3                                                          
VALFL20  EQU   *                                                                
* VALIDATE FLIGHT DATES                                                         
         XC    WORK2,WORK2                                                      
         XC    WORK,WORK                                                        
         LA    R2,CONDTESH         DATES FIELD                                  
         CLI   5(R2),0             FLIGHT DATES ENTERED?                        
         BE    VALAGFDX            NOTHING TO VALIDATE                          
         CLI   RPRDAGDF,X'0'                                                    
         BE    VALAGFDX                                                         
         CLI   RPRDAGDT,X'0'                                                    
         BE    VALAGFDX                                                         
         GOTO1 SCANNER,DMCB,CONDTESH,(2,WORK2),C',=-='                          
         CLI   DMCB+4,2            IF NOT 2 DATES, ERROR                        
         BE    VALFL30                                                          
         LA    R3,EDTERR           MISSING END DATE                             
         B     ERROR                                                            
VALFL30  LA    R3,SDTERR                                                        
         GOTO1 DATVAL,DMCB,(0,WORK2+12),WORK                                    
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    ERROR                                                            
         GOTO1 DATCON,DMCB,WORK,(3,TWASVSDT)                                    
*                                                                               
         GOTO1 DATVAL,DMCB,(0,WORK2+44),WORK+6                                  
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    ERROR                                                            
         GOTO1 DATCON,DMCB,WORK+6,(3,TWASVEDT)                                  
         CLC   RPRDAGDF,TWASVSDT                                                
         BH    VALERR4                                                          
         CLC   RPRDAGDT,TWASVEDT                                                
         BL    VALERR4                                                          
*        CLI   TWAACTMD,CONCHG_Q   CHANGE?                                      
*        BNE   VALAGFDX                                                         
*        GOTO1 VGETREC,DMCB,AIO3                                                
         B     VALAGFDX                                                         
*                                                                               
VALERR1  EQU   *                                                                
         LA    R3,109              INVALID  PRODUCT CODE                        
         B     ERROR                                                            
VALERR2  EQU   *                                                                
         LA    R3,647              AGENCY != PR. AGENCY CODE                    
         B     ERROR                                                            
VALERR3  EQU   *                                                                
         LA    R3,660              OFFICE != PRODUCT OFFICE CODE                
         B     ERROR                                                            
VALERR4  EQU   *                                                                
         LA    R3,65               OVERLAPPING DATES                            
         B     ERROR                                                            
VALERR5  EQU   *                                                                
         LA    R3,2                INVALID INPUT FIELD                          
         B     ERROR                                                            
VALAGFDX EQU   *                                                                
         DROP  R6                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
STAEXCL  EQU   648                                                              
         LTORG                                                                  
         EJECT                                                                  
         DROP  R7                                                               
*MN*******************************************************************          
*   BUILD A REP-TO-SPOT INTERFACE ELEMENT FOR SEEDING BUYS                      
**********************************************************************          
SEEDBUY  CSECT                                                                  
         NMOD1 0,**SEED**                                                       
         L     RC,0(R1)                                                         
                                                                                
         XC    SPIFELT,SPIFELT     CLEAR THE ELEMENT                            
         MVC   SPIFCDLN,=X'0830'   SET ELEMENT CODE/LENGTH                      
                                                                                
*        THE CONTRACT RECORD IS LOCATED AT RCONREC AT THIS TIME                 
         MVC   SPIFSTAT(5),RCONKSTA   INSERT STATION CALL LETTERS               
         MVC   SPIFRADV,RCONKADV   INSERT ADVERTISER CODE                       
         MVC   SPIFRPRD,RCONPRD    INSERT PRODUCT CODE                          
                                                                                
*        THE REP RECORD IS LOCATED AT IOAREA AT THIS TIME                       
         XC    KEY,KEY                                                          
         MVI   KEY,1                                                            
         MVC   KEY+25(2),REPALPHA                                               
         GOTO1 VHIGH                                                            
         LA    R3,337              NO REP RECORD                                
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
                                                                                
         GOTO1 VGETREC,DMCB,RREPREC                                             
                                                                                
         LA    RF,RREPELEM         FIND REP X'05' ELEMENT                       
         LA    R3,NOREPSPT         SET POSSIBLE ERROR CODE                      
DISP0040 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    ERROR               YES                                          
         CLI   0(RF),5             SPOTPAK INTERFACE CODES?                     
         BE    DISP0060            YES                                          
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELEMENT                   
         AR    RF,RE                                                            
         B     DISP0040            GO BACK FOR NEXT                             
DISP0060 EQU   *                                                                
         MVC   SPIFAGCD,RREPSPPC-RREPSPOT(RF)                                   
*                                  INSERT SPOTPAK AGENCY CODE                   
         MVC   SPIFSMED,RREPMED-RREPSPOT(RF)                                    
*                                  INSERT MEDIA CODE                            
                                                                                
*   RETRIEVE PRODUCT RECORD TO GET SPOT CODES                                   
                                                                                
         MVI   RPRDKTYP,09         PRODUCT RECORD TYPE                          
         MVC   RPRDKREP,REPALPHA   REP CODE                                     
         MVC   RPRDKADV,RCONKADV   INSERT ADV CODE FROM CON REC                 
         LA    R3,NOPRODCD         NO PRODUCT CODE ERROR MESSAGE                
         OC    RCONPRD,RCONPRD     ANY PRODUCT CODE?                            
         BZ    ERROR               NO  - DON'T PROCESS                          
         CLC   RCONPRD,MYSPACES    ANY PRODUCT CODE?                            
         BE    ERROR               NO  - DON'T PROCESS                          
         MVC   RPRDKPRD,RCONPRD    INSERT PRODUCT CODE INTO KEY                 
                                                                                
         MVC   KEY(27),RPRDKEY     MOVE SETUP KEY TO KEY                        
         GOTO1 VHIGH               READ KEY                                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                KEY NOT FOUND                                
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                  READ PRODUCT RECORD INTO IOAREA              
         LA    RF,RPRDELEM         FIND PRODUCT X'03' ELEMENT                   
         LA    R3,NOSPOTPK         SET POSSIBLE ERROR                           
DISP0080 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    ERROR               YES - NO X'03' ELEMENT                       
         CLI   0(RF),3             SPOTPAK INTERFACE ELEMENT?                   
         BE    DISP0100            YES                                          
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELEMENT                   
         AR    RF,RE                                                            
         B     DISP0080            GO BACK FOR NEXT                             
DISP0100 EQU   *                                                                
         MVC   SPIFSCLT(6),RPRDSPCL-RPRDSPOT(RF)                                
*                                  MOVE CLIENT/PRODUCT CODES                    
         MVC   SPIFEST#,RPRDSPES-RPRDSPOT(RF)                                   
*                                  MOVE ESTIMATE NUMBER                         
                                                                                
                                                                                
DISP0120 EQU   *                                                                
         MVI   RBUYKTYP,11         BUY REC TYPE                                 
         MVC   RBUYKREP,REPALPHA   REP CODE                                     
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   RBUYKCON,TWACNUM    K NUMBER                                     
         DROP  RF                                                               
                                                                                
*        CLC   =C'RESET',8(R2)     RTS RESET OF ALL BUYS?                       
*        BE    DISP0140                                                         
*        CLC   =C'ALL',8(R2)       RTS ALL BUYS?                                
*        BNE   DISP0160                                                         
*ISP0140 EQU   *                                                                
         LA    R0,1                                                             
         MVC   PATCH(2),=X'FF01'   SET RANGE FROM 1 TO 255                      
         B     DISP0180                                                         
                                                                                
*ISP0160 DS    0H                                                               
*        GOTO1 SCANNER,DMCB,(R2),(1,WORK2),0                                    
*        TM    WORK2+2,X'80'       FIRST LINE # NUMERIC?                        
*        BNO   ERROR               NO  - ERROR                                  
*        ZIC   R0,WORK2+7          TAKE FIRST LINE NUMBER                       
*        LTR   R0,R0                                                            
*        BZ    ERROR                                                            
*        STC   R0,PATCH            SET LAST LINE # TO FIRST                     
*        STC   R0,PATCH+1          SET RANGE COUNTER                            
*        OC    WORK2+1(1),WORK2+1  ANY SECOND VALUE?                            
*        BZ    DISP0180            NO  - PROCEED                                
*        TM    WORK2+3,X'80'       2ND LINE # NUMERIC?                          
*        BNO   ERROR               NO  - ERROR                                  
*        MVC   PATCH(1),WORK2+11   YES - TAKE 2ND LINE NUMBER                   
*                                                                               
DISP0180 DS    0H                                                               
         CH    R0,=H'254'                                                       
         BH    ERROR                                                            
         MVC   KEY,RBUYREC                                                      
         STC   R0,RBUYKLIN         LINE NUMBER                                  
                                                                                
         GOTO1 VHIGH                                                            
         B     DISP0220                                                         
DISP0200 GOTO1 VSEQ                                                             
DISP0220 CLC   KEY(22),KEYSAVE     SAME K?                                      
         BNE   DISPX               FINISHED                                     
         CLC   KEY+26(1),PATCH     WITHIN RANGE?                                
         BH    DISPX               NO  - HIGH - FINISHED                        
         LR    RF,RA               DIRECTORY RECORD FOUND                       
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   TWABADDR,KEY+28     SAVE DISK ADDR                               
         DROP  RF                                                               
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
         GOTO1 VMOVEREC,DMCB,IOAREA,RBUYREC                                     
         XI    RBUYRTS,X'80'       FLIP/FLOP BIT FOR REWRITE                    
         LA    R2,CONBNUMH                                                      
*        CLC   =C'ALLCLR',8(R2)    RESET TRANSFER CODES AND TARGET              
*                                     REP CODE?                                 
*        BNE   DISP0280            NO                                           
*        LA    R1,RBUYELEM         FIND X'08' ELEMENT                           
*ISP0240 EQU   *                                                                
*        CLI   0(R1),0             END OF RECORD?                               
*        BE    DISP0340            YES - NO X'08' - REWRITE RECORD              
*        CLI   0(R1),X'08'         SPOTPAK INTERFACE ELEMENT?                   
*        BE    DISP0260            YES - PROCESS IT                             
*        ZIC   R0,1(R1)            NO  - BUMP TO NEXT ELEMENT                   
*        AR    R1,R0                                                            
*        B     DISP0240            GO BACK FOR NEXT ELEMENT                     
*ISP0260 EQU   *                                                                
*        USING RBUYSPEL,R1                                                      
*        XC    RBUYSPL#,RBUYSPL#   ERASE SPOTPAK BUY NUMBER                     
*        XC    RBUYSPDT,RBUYSPDT   ERASE SPOTPAK TRANSFER DATE                  
*        XC    RBUYSPTM,RBUYSPTM   ERASE SPOTPAK TRANSFER TIME                  
*        LR    RF,RA               SET ADDRESSABILITY                           
*        AH    RF,=Y(TWAWORKQ)                                                  
*        USING TWAWORK,RF                                                       
*        MVC   RBUYSPAG,TWASPAG    RESET AGENCY POWER CODE                      
*        DROP  R1,RF                                                            
*        B     DISP0296                                                         
DISP0280 EQU   *                                                                
*        CLC   =C'RESET',8(R2)     DELETE X'08' ELEMENT AND REBUILD?            
*        BNE   DISP0296            NO                                           
         LA    R1,RBUYELEM         FIND X'08' ELEMENT                           
DISP0284 EQU   *                                                                
         CLI   0(R1),0             END OF RECORD?                               
*MN      BE    DISP0340            YES - NO X'08' - ADD ELEMENT                 
         BE    DISP0320            YES - NO X'08' - ADD ELEMENT                 
         CLI   0(R1),X'08'         SPOTPAK INTERFACE ELEMENT?                   
         BE    DISP0288            YES - DON'T PROCESS, SKIP                    
         ZIC   R0,1(R1)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R1,R0                                                            
         B     DISP0284            GO BACK FOR NEXT ELEMENT                     
DISP0288 EQU   *                                                                
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'08',RBUYREC),0,0                
*                                  DELETE OLD SPOTPAK EST ELEMENT               
         B     DISP0320            GO INSERT NEW X'08' ELEMENT                  
DISP0296 EQU   *                                                                
*        CLC   =C'ALLSEED',8(R2)   SEED ALL BUYS IF NO TRANSFER ELT?            
*        BNE   DISP0340            NO  - REWRITE THIS RECORD                    
*        LA    R1,RBUYELEM         FIND X'08' ELEMENT                           
*ISP0300 EQU   *                                                                
*        CLI   0(R1),0             END OF RECORD?                               
*        BE    DISP0320            YES - NO X'08' - ADD NEW X'08'               
*        CLI   0(R1),X'08'         SPOTPAK INTERFACE ELEMENT?                   
*        BE    DISP0340            YES - REWRITE RECORD W/NO NEW ELT            
*        ZIC   R0,1(R1)            NO  - BUMP TO NEXT ELEMENT                   
*        AR    R1,R0                                                            
*        B     DISP0300            GO BACK FOR NEXT ELEMENT                     
DISP0320 EQU   *                                                                
         GOTO1 VADDELEM,DMCB,RBUYREC,SPIFELT                                    
*                                  ADD NEW X'08' ELEMENT                        
DISP0340 EQU   *                                                                
         GOTO1 VPUTREC,DMCB,RBUYREC                                             
         B     DISP0200            GO BACK FOR NEXT                             
DISPX    XMOD1                                                                  
         EJECT                                                                  
SPIFELT  DS    0XL48               SPOTPAK INTERFACE ELEMENT                    
SPIFCDLN DS    XL2      +0         X'0830' CODE/LENGTH                          
SPIFAGCD DS    CL2      +2         SPOTPAK AGENCY CODE                          
SPIFSMED DS    CL1      +4         SPOTPAK MEDIA (FROM REP RECORD)              
SPIFSCLT DS    CL3      +5         SPOTPAK CLIENT                               
SPIFPROD DS    CL3      +8         SPOTPAK PRODUCT CODE                         
SPIFEST# DS    CL1      +11        SPOTPAK ESTIMATE NUMBER                      
         DS    CL6      +12        FILLER                                       
SPIFSTAT DS    CL5      +18        STATION CALL LETTERS                         
SPIFRADV DS    CL4      +23        REPPAK ADVERTISER                            
SPIFRPRD DS    CL3      +27        REPPAK PRODUCT                               
         DS    CL18     +30        FILLER                                       
*                                                                               
NOPRODCD EQU   676                                                              
NOSPOTPK EQU   677                                                              
NOREPSPT EQU   678                                                              
DDATERR  EQU   775                 DATE FORMAT ERROR                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*MN                                                                             
*                                                                               
*                                                                               
*                                                                               
* REGENACL                                                                      
       ++INCLUDE REGENACL                                                       
* FAFACTS                                                                       
       ++INCLUDE FAFACTS                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'074RECNT10   01/12/17'                                      
         END                                                                    
