*          DATA SET ACSCR05    AT LEVEL 087 AS OF 01/18/18                      
*PHASE T60C05A                                                                  
***********************************************************************         
*                                                                     *         
* THIS VERSION MATCHES THE UK VERSION LEVEL 85 AS OF 02/27/09         *         
*                                                                     *         
***********************************************************************         
* PID  LVL DATE    COMMENTS                                                     
* ---------------------------------                                             
* YNGX 086 08AUG17 PCA02728 RELINK TO EXTEND MAX NO OF COLUMNS                  
* GHOA 087 18JAN18 SPEC19883 SCRIBE DOWNLOAD SCREEN NOT SAVING VALUES           
*                                                                               
         TITLE 'PROFILE ELEMENTS MAINTAINANCE'                                  
T60C05   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,60C05,RA,RR=RE                                                 
*                                                                               
         USING TWAD,R5                                                          
         USING WORKD,R7                                                         
         USING LWSD,RC                                                          
         L     RC,APALOCAL                                                      
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         MVC   APCURSOR,ACURSOR    KEEP CURSOR WHERE IT WAS LAST                
         CLC   ATWA,ACURSOR        INSURE CURSOR WAS IN A FIELD                 
         BE    SCR01                                                            
         CLI   TWALREC,RECPRF      IF 1ST TIME IN THEN SET CURSOR TO...         
         BE    SCR02                                                            
*                                                                               
SCR01    LA    RE,PRFCODEH         FORMAT CODE FIELD                            
         ST    RE,APCURSOR                                                      
*                                                                               
SCR02    CLI   APMODE,APMVALK                                                   
         BNE   SCR10                                                            
         MVCDD AC@AUTO,AC#AUTO                                                  
         GOTO1 VDICTAT,APPARM,C'SU  ',AC@AUTO,0                                 
*                                                                               
         LA    R2,SCRTXT                                                        
SCR05    CLI   0(R2),X'FF'         END OF TABLE?                                
         BE    SCR10                                                            
         L     R4,0(,R2)           GET HEADER ADDRESS                           
         AR    R4,R5                                                            
         OI    6(R4),FVOXMT        TRANSMIT FIELD                               
         L     R3,4(,R2)           GET SCREEN NUMBER                            
         GOTO1 TEXTGET,APPARM,(R3),(R4),0                                       
         LA    R2,8(,R2)           BUMP TO NEXT                                 
         B     SCR05                                                            
*                                                                               
         USING RESRECD,R2                                                       
SCR10    LA    R2,IOKEY                                                         
         SR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *(RF)                                                            
*                                                                               
         B     VALKEY                                                           
         B     VALREC                                                           
         B     DISKEY                                                           
         B     DISREC                                                           
         B     EXIT                                                             
         B     EXIT                RESTORE                                      
         B     EXIT                VALSEL                                       
         B     EXIT                GETSEL                                       
         B     EXIT                DISSEL                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                LSTSCR                                       
         B     EXIT                VALREQ                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         EJECT                                                                  
EXIT     CLI   APPFKEY,PFKNEXT                                                  
         BE    EXIT97                                                           
         CLI   APMODE,APMVALK                                                   
         BE    EXIT95                                                           
         TM    TWAMODE,TWAMLSM     LIST/SELECT MODE?                            
         BZ    EXIT30                                                           
         CLI   APPFKEY,PFKEXIT     EXIT TO LIST                                 
         BE    EXIT99                                                           
*                                                                               
EXIT30   TM    TWASWPST,TWASWAP    SWAP TO NEW RECORD ACTION?                   
         BZ    EXIT95              NO                                           
         XC    APCURSOR,APCURSOR   DON'T SET CURSOR ON WRONG SCREEN             
*        NI    TWASWPST,TURNOFF-TWASWAP                                         
         MVI   APMODE,APMSWP           SWAP                                     
         MVC   APPARM(1),TWASWPRE      SWAP RECORD                              
         MVC   APPARM+1(1),TWASWPAC    SWAP ACTION                              
*                                                                               
EXIT95   OI    TWALSCTL,TWALSHLD                                                
*                                                                               
EXIT97   OI    TWALSCTL,TWALSRTN                                                
*                                                                               
EXIT99   CLC   FVMSGNO,=AL2(FVFOK)                                              
         XIT1                                                                   
         EJECT                                                                  
         SPACE 1                                                                
VALKEY   EQU   *                                                                
         MVI   NEWKEY,NO                                                        
         MVC   RESKEY,SPACES                                                    
         MVI   RESKTYP,RESKTYPQ    X'2D'                                        
         MVI   RESKSUB,RESKSUBQ    X'02'                                        
         MVC   RESKCPY,CUABIN      COMPANY CODE                                 
         GOTO1 AFVAL,PRFCODEH                                                   
         MVC   RESKFORM,FVIFLD                                                  
         BE    VALKEY15                                                         
         TM    PRFCODE+4,FVITHIS   ANY INPUT?                                   
         BZ    *+10                NO                                           
         MVC   SAVFORM,SPACES                                                   
         CLC   SAVFORM,SPACES                                                   
         BNH   IVALEKEY                                                         
         MVC   PRFCODE,SAVFORM                                                  
         OI    PRFCODEH+6,FVOXMT   TRANSMIT                                     
         MVC   RESKFORM,SAVFORM                                                 
*                                                                               
VALKEY15 MVC   SAVFORM,RESKFORM                                                 
         MVC   APRECKEY(L'RESKEY),RESKEY                                        
         LA    R1,IORD+IOACCFIL+IO1                                             
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(,R1)      READ FOR UPDATE                              
         GOTO1 AIO                                                              
         BL    VALKEY99            IO ERROR                                     
         BNE   VALKEY20                                                         
         L     R2,AIOAREA1                                                      
         GOTO1 GETTYPE,(R2)                                                     
         OI    SCRTYPH+6,FVOXMT                                                 
         MVC   SCRTYP(L'APREPCDE),APREPCDE                                      
*                                                                               
VALKEY19 MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         TM    TWAMODE,TWAMDFR     2ND PASS IN ACTION COPY?                     
         BZ    VALKEY98                                                         
         CLI   APACTN,ACTCPY                                                    
         BNE   VALKEY98                                                         
         OI    APINDS,APIOKADD     TURN ON TO TRICK ACTION COPY                 
         B     VALKEY98                                                         
*                                                                               
VALKEY20 TM    IOERR,IOEDEL        IS RECORD MARKED DELETED                     
         BNZ   VALKEY99            OK TO ADD RECORD                             
         MVI   APINDS,APIOKADD     RECORD NOT ON FILE, SO OK TO ADD             
         MVI   NEWKEY,YES                                                       
         L     R2,AIOAREA1                                                      
         XC    RESKEY(256),RESKEY  RESET AIO AREA                               
*                                                                               
VALKEY98 MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEY99 B     EXIT                                                             
         EJECT                                                                  
         SPACE 1                                                                
DISKEY   NI    TWASWPST,TURNOFF-TWASWAP                                         
         B     EXIT                                                             
         EJECT                                                                  
         SPACE 1                                                                
VALREC   EQU   *                                                                
         L     R2,AIOAREA1                                                      
         GOTO1 AFVAL,PRFNMEH                                                    
         BNE   VALREC05             NAME HAS NOT BEEN INPUT                     
         GOTO1 ADDNAME,APPARM,(R2),PRFNMEH    ADD FORMAT NAME                   
         BNE   VALREC99             ON   ERROR, EXIT                            
*                                                                               
VALREC05 L     R2,AIOAREA1                                                      
         MVC   RESKEY,APRECKEY                                                  
         CLI   NEWKEY,YES          ADDING A NEWKEY?                             
         BNE   VALREC10                                                         
         MVC   APREPNUM,APREPUL+1  RESET TO SINGLE LEDGER                       
         GOTO1 ADDREPTY,APPARM,(R2)                                             
         BNE   VALREC99            ON   ERROR, EXIT                             
*                                                                               
         USING RFLELD,R3                                                        
         LA    R3,APELEM                                                        
         XC    APELEM,APELEM                                                    
         MVI   RFLEL,RFLELQ        X'C5' FILTER TYPE                            
         MVI   RFLLN,RFLLNQ+2                                                   
         MVI   RFLTYPE,RFLLDG      SET LEDGER ON RECORD                         
         MVC   RFLDATA,APREPUL     DEFAULT UNIT AND LEDGER                      
         GOTO1 ADDEL,(R2)                                                       
         BNE   VALREC99                                                         
         DROP  R3                                                               
*                                                                               
VALREC10 XC    COLFLGS,COLFLGS     TO STORE COLUMN OPTIONS                      
*                                                                               
         USING STYELD,R1           SCRIBE FREE   FORM  ELEMENT                  
         MVI   COLWIDTH,0                                                       
         LR    R1,R2               ->     RECORD                                
         MVI   APELCODE,STYELQ     X'25'  FREE   FORM  SCRIBE   ELEMENT         
         GOTO1 GETEL               GET    COLUMN ELEMENT                        
         BNE   VALRC10A            NOT    FOUND, SKIP                           
         MVI   COLWIDTH,X'FF'      ASSUME WIDTH  >     255                      
         CLI   STYWIDTH,0          IS     WIDTH  >     255                      
         BH    VALRC10A            YES,   SKIP                                  
         MVC   COLWIDTH,STYWIDTH+1 SAVE   WIDTH  OF    REPORT                   
         DROP  R1                                                               
*                                                                               
VALRC10A DS    0H                                                               
*&&US                                                                           
         MVI   MYRCPSW,X'00'                                                    
         LR    R1,R2               ->     RECORD                                
         MVI   APELCODE,RCPELQ     X'C8'  SCRIBE RECAP ELEMENT                  
         GOTO1 GETEL               GET    COLUMN ELEMENT                        
         BNE   *+8                 NOT    FOUND, SKIP                           
         MVI   MYRCPSW,YES         RECAP  REPORT                                
*&&                                                                             
*                                                                               
         MVI   INACTFG,0           SET OPTION INACTIVE 0                        
         LR    R1,R2                                                            
         AH    R1,DATADISP         BUMP PAST KEY                                
         SR    R0,R0               COUNT ROWS                                   
         SR    RE,RE               COUNT ROWS                                   
         SR    RF,RF               COUNT COLUMNS                                
*                                                                               
VALREC11 CLI   0(R1),0                                                          
         BE    VALREC15                                                         
         CLI   0(R1),RRWELQ        IS IT A ROW ELEMENT?                         
         BNE   *+8                 NO                                           
         LA    RE,1(,RE)           YES, SO COUNT THE NUMBER OF ROWS             
*                                                                               
         USING RCLELD,R1                                                        
         CLI   0(R1),RCLELQ        IS IT A COLUMN ELEMENT?                      
         BNE   VALREC14            NO                                           
         LA    RF,1(,RF)           USE RF TO INDEX INTO ARRAY                   
         LA    R3,COLFLGS(RF)      SAVE THE COLUMN OPTIONS                      
         MVC   0(1,R3),RCLOPT      SAVE COLUMN OPTIONS                          
         TM    RCLOPT,RCLACCM+RCLEQU    CALCULATED COLUMN?                      
         BNO   VALREC14                                                         
         SR    R0,R0                                                            
         IC    R0,RCLDATLN         LENGTH OF RCLNDATA FIELD                     
         LA    R3,RCLNDATA                                                      
*                                                                               
VALREC12 CLI   0(R3),C'V'          LOOK FOR V%                                  
         BE    VALREC13                                                         
         LA    R3,1(,R3)                                                        
         BCT   R0,VALREC12                                                      
         B     VALREC14                                                         
*                                                                               
VALREC13 LA    R3,COLFLGS(RF)                                                   
         MVI   0(R3),X'FF'         MARK AS V%                                   
         MVI   INACTFG,C'V'        FOR VERTICAL %                               
*                                                                               
VALREC14 IC    R0,1(,R1)           GET LENGTH                                   
         AR    R1,R0               BUMP TO NEXT ELEMENT                         
         B     VALREC11                                                         
*                                                                               
VALREC15 STC   RE,NROWS            NUMBER OF ROWS                               
         STC   RF,NCOLS            NUMBER OF COLUMNS                            
         DROP  R1                                                               
*                                                                               
         USING RPFELD,R9                                                        
         LA    R9,APELEM                                                        
         XC    APELEM,APELEM                                                    
         MVI   APELCODE,RPFELQ     X'C4', DELETE HEADLINE ELEMENTS              
         MVI   RPFLN,RPFLNQ        LENGTH OF ELEMENT                            
         GOTO1 GETEL,(R2)                                                       
         BNE   VALREC16            NO ELEMENT TO DELETE                         
         SR    RF,RF                                                            
         IC    RF,1(,R1)                                                        
         BCTR  RF,0                                                             
         EXMVC RF,RPFEL,0(R1)      SAVE OLD ELEMENT                             
***********************************************************************         
*        CLEAR ITEMS THAT ARE ON SCREEN ONLY                          *         
***********************************************************************         
         SPACE 1                                                                
         MVI   RPFPOPT,0           PRINT OPTIONS                                
         MVI   RPFEDOPT,0          EDIT AMOUNTS                                 
         MVI   RPFPCTS,0           PERCENTS                                     
         MVI   RPFRND,0            ROUND                                        
         MVI   RPFNALPR,0          ADDRESS LINES TO PRINT                       
         XC    RPFRKON,RPFRKON                                                  
         XC    RPFRKBY,RPFRKBY                                                  
         MVI   RPFRKCL,0                                                        
         MVI   RPFRKOR,0                                                        
         MVI   RPFPOPT2,0                                                       
         NI    RPFPOPT3,RPFTEDIT   ONLY FLAG TO LEAVE ON (TIME SHEETS)          
         MVI   RPFIND,0                                                         
         MVI   RPFPITCH,0                                                       
         GOTO1 DELEL,(R2)                                                       
*                                                                               
VALREC16 MVI   RPFEL,RPFELQ        ELEMENT CODE                                 
         CLI   RPFLN,RPFLNQ        OLD ELEMENT LENGTH ?                         
         BNE   VALRC16B                                                         
         MVI   RPFLN,RPFLN2Q       MAKE IT THE NEW ELEMENT SIZE                 
         MVI   RPFFLDD,C' '        DEFAULT, FIELD DELIMITER                     
         MVI   RPFTXTD,C'"'        DEFAULT, TEXT DELIMITER                      
         MVI   RPFEOLD,X'5E'       DEFAULT, END OF LINE                         
         MVI   RPFEORD,C':'        DEFAULT, END OF REPORT                       
*                                                                               
VALRC16B CLC   PRFLFJT,APYES       LEFT JUSTIFIED?                              
         BNE   *+8                                                              
         OI    RPFPOPT,RPFLFJT     TURN ON BIT FOR LEFT JUSTIFICATION           
*                                                                               
         CLC   PRFBOX,APYES        BOXES?                                       
         BNE   *+8                                                              
         OI    RPFPOPT,RPFBOX      TURN ON BIT FOR BOXES                        
*                                                                               
         CLI   PRFBOX,C'X'         EXTRA BOXES?                                 
         BNE   *+12                                                             
         OI    RPFPOPT,RPFBOX      TURN ON BIT FOR BOXES                        
         OI    RPFPOPT3,RPFXTBOX   EXTRA BOX DETAIL                             
*                                                                               
         GOTO1 AFVAL,PRFIACTH                                                   
         CLC   PRFIACT,APNO                                                     
         BE    VALREC17                                                         
         CLI   PRFIACT,C'C'                                                     
         BNE   *+12                                                             
         OI    RPFIND,RPFICZRO     ELIMINATE REC. IF ALL AGE COL=ZERO           
         B     VALREC17                                                         
*                                                                               
         CLI   PRFIACT,C'A'        PRINT ALL LEVELS OF ACCOUNTS?                
         BNE   *+16                                                             
         OI    RPFPOPT,RPFIACT     TURN ON BIT FOR INACTIVE ACCOUNTS            
         OI    RPFIND,RPFACLVL                                                  
         B     VALREC17                                                         
*                                                                               
         CLC   PRFIACT,APYES       PRINT INACTIVE ACCOUNTS?                     
         BNE   *+12                                                             
         OI    RPFPOPT,RPFIACT     TURN ON BIT FOR INACTIVE ACCOUNTS            
         B     VALREC17                                                         
*                                                                               
         CLI   INACTFG,C'V'        VERTICAL % USED                              
         BE    IVALRVOP            INVALID WITH V% OR RANKING                   
         CLI   PRFIACT,C'B'        SHOW ACCOUNTS WITH BUDGETS ONLY              
         BNE   *+16                                                             
         OI    RPFPOPT2,RPFIABUD   TURN ON BIT FOR INACTIVE ACCOUNTS            
         MVI   INACTFG,C'R'        NO LONGER VAILD TO RANK OR HAVE V%           
         B     VALREC17                                                         
*                                                                               
         CLI   PRFIACT,C'T'        SHOW ACCOUNTS WITH TRANS. ONLY               
         BNE   IVALIPUT                                                         
         OI    RPFPOPT2,RPFIATRN   TURN ON BIT FOR INACTIVE ACCOUNTS            
         MVI   INACTFG,C'R'        NO LONGER VAILD TO RANK OR HAVE V%           
*                                                                               
VALREC17 CLC   PRFRTOT,APYES       PRINT REDUNDANT TOTALS?                      
         BNE   *+8                                                              
         OI    RPFPOPT,RPFRTOT     TURN ON BIT FOR REDUNDANT TOTALS             
*                                                                               
         CLC   PRFRDAT,APYES       PRINT REDUNDANT DATA?                        
         BNE   *+8                                                              
         OI    RPFPOPT,RPFRDAT     TURN ON BIT FOR REDUNDANT DATA               
*                                                                               
         CLC   PRFPCMA,APYES       PRINT USING COMMAS                           
         BNE   *+8                                                              
         OI    RPFEDOPT,RPFEDCMA   TURN ON COMMAS                               
*                                                                               
         CLC   PRFPZAM,APYES       PRINT ZERO AMOUNTS                           
         BNE   *+8                                                              
         OI    RPFEDOPT,RPFEDZRO   TURN ON ZERO AMOUNTS                         
*                                                                               
         CLC   PRFPZTO,APYES       PRINT ZERO TOTALS                            
         BNE   *+8                                                              
         OI    RPFPOPT,RPFZEROT    TURN ON ZERO TOTALS                          
*&&US                                                                           
         NI    RPFDNOPT,TURNOFF-RPFDDOWN                                        
*                                                                               
*        CLI   COLWIDTH,MXXRPTWD                                                
*        BNH   *+8                                                              
*        MVI   PRFPPAP,C'D'        FORCE DOWNLOAD                               
*        OI    PRFPPAPH+6,FVOXMT                                                
*                                                                               
         CLI   PRFPPAP,C'P'        PRINT AS PORTRAIT                            
         BNE   *+8                                                              
         OI    RPFPOPT,RPFPORT     YES                                          
*                                                                               
         CLI   PRFPPAP,C'D'        DOWN-LOAD ONLY REPORT ?                      
         BNE   VALRC17B            NO,  SKIP                                    
         OI    RPFDNOPT,RPFDDOWN   TURN ON DOWN-LOAD                            
*                                                                               
VALRC17B CLI   COLWIDTH,MAXRPTWD                                                
         BNH   VALRC17D                                                         
         TM    RPFDNOPT,RPFDDOWN                                                
         BZ    IVALWDTH                                                         
*&&                                                                             
VALRC17D CLC   PRFPUPP,APYES       PRINT USING UPPER CASE                       
         BE    *+8                                                              
         OI    RPFPOPT,RPFMCASE    USE MIXED CASE INSTEAD                       
*                                                                               
         CLC   PRFPEXP,APNO        PRINT MIDS/TOTS ACROSS COLUMN?               
         BE    *+8                 NO                                           
         OI    RPFPOPT2,RPFEXCOL   YES                                          
*                                                                               
         CLC   PRFPRQT,APNO        NO REQUEST TOTAL ?                           
         BNE   *+8                                                              
         OI    RPFPOPT3,RPFNORQT   DON'T PRINT REQUEST TOTAL                    
         CLI   PRFPRQT,C'S'        REQUEST TOTAL ON SEPRATE PAGE ?              
         BNE   *+8                                                              
         OI    RPFPOPT2,RPFTOTSP   YES                                          
*                                                                               
*        CLC   PRFPTSP,APNO        REQUEST TOTAL ON SEPARATE PAGE?              
*        BE    *+8                 NO                                           
*        OI    RPFPOPT2,RPFTOTSP   YES                                          
*&&UK                                                                           
         CLC   PRFPFCP,APYES       PRINT FOREIGN CURRENCY PREFIX?               
         BNE   *+8                 NO                                           
         OI    RPFIND,RPFIPRFX     YES                                          
*                                                                               
         CLC   PRFPXNC,APYES       EXCLUDE NON-FOREIGN CURR. TRANS?             
         BNE   *+8                 NO                                           
         OI    RPFIND,RPFIXNFC     YES                                          
*&&                                                                             
*&&US                                                                           
         GOTO1 AFVAL,PRFPBXOH                                                   
         CLC   PRFPBXO,APNO        NO NOTHING (REGULAR)                         
         BE    VALREC18            DO NADA, DEFAULT                             
         CLI   PRFPBXO,C'S'        STRIPES?                                     
         BNE   *+12                NO                                           
         OI    RPFPOPT2,RPFSTRPE   YES                                          
         B     VALREC18                                                         
*                                                                               
         CLI   PRFPBXO,C'I'        SHADE INSIDE THE BOXES?                      
         BNE   *+12                NO                                           
         OI    RPFPOPT2,RPFISHDE   YES                                          
         B     VALREC18                                                         
*                                                                               
         CLI   PRFPBXO,C'O'        SHADE INSIDE THE BOXES?                      
         BNE   IVALIPUT                                                         
         OI    RPFPOPT2,RPFOSHDE   YES                                          
*&&                                                                             
VALREC18 GOTO1 AFVAL,PRFPMINH                                                   
         BE    *+8                                                              
         MVI   PRFPMIN,C'T'        DEFAULT TO TRAILING                          
                                                                                
         CLC   PRFPMIN,APNO        WANT ANY MINUS SIGN ?                        
         BE    VALREC20            NO, ABSOLUTE VALUE - NO SIGN                 
                                                                                
         CLI   PRFPMIN,C'T'        TRAILING MINUS?                              
         BNE   *+12                                                             
         OI    RPFEDOPT,RPFEDTRL                                                
         B     VALREC20                                                         
*                                                                               
         CLI   PRFPMIN,C'B'        MINUS BRACKETS?                              
         BNE   *+12                                                             
         OI    RPFEDOPT,RPFEDBKT                                                
         B     VALREC20                                                         
*                                                                               
         CLI   PRFPMIN,C'L'        LEADING MINUS?                               
         BNE   *+12                                                             
         OI    RPFEDOPT,RPFEDLED                                                
         B     VALREC20                                                         
*                                                                               
         CLI   PRFPMIN,C'S'        MINUS AS "CR" ?                              
         BNE   IVALEDOP                                                         
         OI    RPFEDOPT,RPFEDCR                                                 
*                                                                               
VALREC20 CLC   PRFPERR,APYES       PRINT DIVISION ERRORS AS "EEEEE"?            
         BNE   *+8                                                              
         OI    RPFIND,RPFIDERR     TURN ON BIT FOR "EEEEEE"                     
*                                                                               
         CLI   PRFPUDT,C'A'        UNDERLINE ABOVE                              
         BNE   *+8                                                              
         OI    RPFPOPT3,RPFTPUL    UNDERLINE ON TOP                             
*                                                                               
         CLC   PRFPUDT,APNO        NO UNDERLINE                                 
         BNE   *+8                                                              
         OI    RPFPOPT3,RPFNOUL                                                 
*                                                                               
         CLC   PRFPCHD,APNO        PRINT COLUMN HEADINGS ?                      
         BNE   *+8                 YES                                          
         OI    RPFPOPT3,RPFNOCHD                                                
*                                                                               
         MVI   RPFNALPR,0          INIT   NUM   OF ADDR LINES TO PRINT          
         GOTO1 AFVAL,PRFPNALH      NUMBER OF    ADDRESS LINES TO PRINT          
         BE    VALREC22            DATA,  PROCESS  IT                           
*&&US*&& MVI   PRFPNAL,C'V'        YES,   DEFAULT  TO   V                       
*&&UK                                                                           
         MVI   PRFPNAL,C'4'        YES,   DEFAULT  TO   4                       
         CLI   APREPJCL,REPJCLR    TEST RECEIVABLES REPORT?                     
         BE    *+12                                                             
         CLI   APREPJCL,REPJCLV    TEST PRODUCTION REPORT?                      
         BNE   VALREC22                                                         
         MVI   PRFPNAL,C'5'        DEFAULT TO 5 FOR PL, RL AND VL               
*&&                                                                             
*                                                                               
VALREC22 CLI   PRFPNAL,C'V'        VARIABLE ?                                   
         BNE   VALREC24            NO,    SKIP                                  
         OI    RPFNALPR,RPFNALPV   SAY    VARIABLE                              
         B     VALREC26            CONTINUE                                     
*                                                                               
VALREC24 CLI   PRFPNAL,C'1'        LESS   THAN  1 ?                             
         BL    IVALIPUT            YES,   INVALID INPUT                         
*&&US*&& CLI   PRFPNAL,C'4'        MORE   THAN  C'4' ? (US)                     
*&&UK*&& CLI   PRFPNAL,C'5'        MORE   THAN  C'5' ? (UK)                     
         BH    IVALIPUT            YES,   INVALID INPUT                         
         MVC   RPFNALPR,PRFPNAL                                                 
         NI    RPFNALPR,X'0F'      MAKE C'1' TO C'4' TO BINARY                  
*                                                                               
VALREC26 GOTO1 AFVAL,PRFPCTSH                                                   
         MVC   RPFPCTS,PRFPCTS                                                  
         CLI   PRFPCTS,C'0'                                                     
         BE    VALREC30                                                         
         CLI   PRFPCTS,C'1'                                                     
         BE    VALREC30                                                         
         CLI   PRFPCTS,C'2'                                                     
         BNE   IVALIPUT                                                         
*                                                                               
VALREC30 GOTO1 AFVAL,PRFPTCHH                                                   
         BE    VALREC32            INPUT FOUND                                  
         MVC   PRFPTCH,AC@AUTO     NO INPUT SO SET TO AUTO PITCH                
         B     VALREC30                                                         
*                                                                               
VALREC32 SR    R1,R1                                                            
         TM    FVIIND,FVINUM       IS IT NUMERIC VALUE ?                        
         BZ    VALREC34                                                         
         L     R1,SCFULL           NUMBER IS IN SCFULL                          
         CHI   R1,24                                                            
         BH    IVALPTCH                                                         
         BE    VALREC36            PITCH 24                                     
         CHI   R1,10               PITCH 10 ?                                   
         BE    VALREC36            YES                                          
         CHI   R1,12               PITCH 12 ?                                   
         BE    VALREC36            YES                                          
         CHI   R1,15               PITCH 15 ?                                   
         BE    VALREC36            YES                                          
         CHI   R1,18               PITCH 18 ?                                   
         BE    VALREC36            YES                                          
         CHI   R1,20               PITCH 20 ?                                   
         BNE   IVALPTCH            NO                                           
         B     VALREC36                                                         
*                                                                               
VALREC34 SR    RF,RF                                                            
         IC    RF,FVXLEN           INPUT LENGTH-1                               
         EXCLC RF,PRFPTCH,AC@AUTO                                               
         BNE   IVALPTCH                                                         
         MVC   PRFPTCH,AC@AUTO                                                  
*                                                                               
VALREC36 OI    PRFPTCHH+6,FVOXMT   TRANSMIT                                     
         STC   R1,RPFPITCH         SAVE PITCH                                   
*                                                                               
VALREC40 GOTO1 AFVAL,PRFRNDH                                                    
*                                                                               
         LA    RE,RNDTAB                                                        
VALREC41 CLC   CULANG,0(RE)                                                     
         BE    VALREC42                                                         
         CLI   0(RE),LANGENG       DEFAULT LANG                                 
         BE    VALREC42                                                         
         LA    RE,5(,RE)           BUMP UP TO NEXT ENTRY                        
         B     VALREC41                                                         
*                                                                               
VALREC42 MVI   RPFRND,PENNY                                                     
         CLC   PRFRND,1(RE)        ROUND IN PENNIES?                            
         BE    VALREC43                                                         
         MVI   RPFRND,DOLLAR                                                    
         CLC   PRFRND,2(RE)        ROUND IN DOLLARS?                            
         BE    VALREC43                                                         
         MVI   RPFRND,THOUSAND                                                  
         CLC   PRFRND,3(RE)        ROUND IN THOUSANDS?                          
         BE    VALREC43                                                         
         MVI   RPFRND,MILLION                                                   
         CLC   PRFRND,4(RE)        ROUND IN MILLIONS?                           
         BNE   IVALIPUT                                                         
*                                                                               
VALREC43 GOTO1 AFVAL,PRFRKONH                                                   
         BNE   VALREC60                                                         
         CLI   INACTFG,C'R'        IS INACTIVE ACTION VALID?                    
         BNE   VALREC45                                                         
         GOTO1 AFVAL,PRFIACTH                                                   
         B     IVALRVOP                                                         
*                                                                               
VALREC45 MVC   RPFRKON(1),PRFRKON  SAVE "R" OR "C"                              
         TM    FVIFLD+1,X'F0'      IS IT A NUMBER?                              
         BNO   IVAL2CNU            2ND CHARACTER MUST BE NUMERIC                
         MVC   TEMPCHAR,FVIFLD+1                                                
         NI    TEMPCHAR,X'0F'      CONVERT TO NUMBER                            
         MVC   RPFRKON+1(1),TEMPCHAR      SAVE ROW/COL NUMBER                   
*                                                                               
         CLI   FVIFLD,C'R'         ARE WE RANKING ON ROW?                       
         BNE   VALREC52            NO                                           
         CLC   TEMPCHAR,NROWS      RANK UP TO N ROWS                            
         BH    IVALRWNO                                                         
         B     VALREC55                                                         
*                                                                               
VALREC52 CLI   FVIFLD,C'C'         ARE WE RANKING ON COLUMNS?                   
         BNE   IVAL1CRC            NO, INVALID ROW/COL OPTION                   
         CLC   TEMPCHAR,NCOLS      RANK UP TO N-TH COLUMN                       
         BH    IVALCLNO                                                         
         SR    RF,RF                                                            
         IC    RF,TEMPCHAR                                                      
         LA    RF,COLFLGS(RF)      POINT TO COLUMN OPTIONS IN ARRAY             
         TM    0(RF),ACCUM                                                      
         BO    IVALCNNU            COLUMN MUST BE NON-NUMERIC TYPE              
*                                                                               
VALREC55 GOTO1 AFVAL,PRFRKCLH      RANK ON WHAT COLUMN?                         
         BNE   IVALIPUT                                                         
         TM    FVIIND,FVINUM       IS IT NUMERIC?                               
         BZ    IVALNNUM                                                         
         SR    R1,R1                                                            
         IC    R1,FVXLEN           INPUT LENGTH-1                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  APDUB,PRFRKCL(0)                                                 
         CVB   R1,APDUB                                                         
         STC   R1,RPFRKCL                                                       
         SR    RF,RF                                                            
         IC    RF,NCOLS                                                         
         CR    R1,RF                                                            
         BH    IVALCLNO                                                         
         LA    RF,COLFLGS(R1)      POINT TO OPTIONS FOR COLUMN                  
         TM    0(RF),ACCUM                                                      
         BZ    IVALCNU             COLUMN MUST BE NUMERIC TYPE                  
         CLI   0(RF),X'FF'         IS IT V%                                     
         BE    IVALRKVT                                                         
*                                                                               
         GOTO1 AFVAL,PRFRKORH                                                   
         BNE   VALREC60                                                         
         MVC   RPFRKOR,FVIFLD                                                   
         CLI   PRFRKOR,C'A'        ASCENDING?                                   
         BE    VALREC60                                                         
         CLI   PRFRKOR,C'D'        DESCENDING?                                  
         BNE   IVALIPUT                                                         
*                                                                               
VALREC60 L     R1,AIOAREA1                                                      
         GOTO1 ADDEL                                                            
         BNE   VALREC99                                                         
*                                                                               
VALREC95 GOTO1 ADDID,APPARM,AIOAREA1                                            
         BNE   VALREC99             ON   ERROR, EXIT                            
         LA    R1,IOADD+IOACCFIL+IO1                                            
         TM    APINDS,APIOKADD           ADD A RECORD?                          
         BO    VALREC97                                                         
         LA    R1,IOWRITE+IOACCFIL+IO1                                          
         TM    APINDS,APIOKCHA           CHANGE A RECORD?                       
         BO    VALREC97                                                         
         DC    H'0'                      WHAT THE HELL?                         
*                                                                               
VALREC97 GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD WRITE OR SOMETHING DUDE                  
*                                                                               
VALREC99 CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    DISREC                                                           
         B     EXIT                                                             
         DROP  R2,R9                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
*                        DISPLAY HEADLINE DATA                        *         
*---------------------------------------------------------------------*         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1         PROFILE ELEMENT                              
         TWAXC PRFNMEH,PRFTABH                                                  
         GOTO1 GETNAME,APPARM,(R2),PRFNMEH                                      
         GOTO1 GETPER,APPARM,(R2),PRFOWNH                                       
         GOTO1 AFVAL,PRFCODEH                                                   
*                                                                               
         MVC   PRFLFJT,APNO        DEFAULT, LEFT JUSTIFY                        
         MVC   PRFBOX,APYES        DEFAULT, BOXES                               
         MVC   PRFIACT,APNO        DEFAULT, SHOW INACTIVE ACCOUNTS              
         MVC   PRFRTOT,APNO        DEFAULT, PRINT REDUNDANT TOTALS              
         MVC   PRFRDAT,APNO        DEFAULT, PRINT REDUNDANT DETAIL              
         MVI   PRFPCTS,C'2'        DEFAULT, TWO DECIMAL PLACES FOR %            
         MVC   PRFPCMA,APYES       DEFAULT, PRINT COMMAS                        
         MVC   PRFPZAM,APNO        DEFAULT, DON'T PRINT ZERO AMOUNTS            
         MVC   PRFPZTO,APNO        DEFAULT, DON'T PRINT ZERO TOTALS             
         MVC   PRFPERR,APNO        DEFAULT, PRINT ERROR AS BLANK                
         MVI   PRFPUDT,C'B'        DEFAULT, UNDERLINE BELOW TOTAL               
         MVC   PRFPCHD,APYES       DEFAULT, PRINT COLUMN HEADINGS               
*&&US*&& MVI   PRFPPAP,C'L'        DEFAULT, PRINT LANDSCAPE                     
         MVC   PRFPUPP,APYES       DEFAULT, USE UPPER CASE                      
*&&US*&& MVC   PRFPBXO,APNO        DEFAULT, NO SPECIAL PRINTING                 
         MVC   PRFPEXP,APNO        DEFAULT, NOT TO EXTEND                       
         MVC   PRFPRQT,APYES       DEFAULT, PRINT REQUEST TOTAL                 
*        MVC   PRFPTSP,APNO        DEFAULT, NOT ON SEPARATE PAGE                
*&&UK*&& MVC   PRFPFCP,APNO        DEFAULT, NO FOREIGN CURRENCY PREFIX          
*&&UK*&& MVC   PRFPXNC,APNO        DEFAULT, EXCLUDE NON-CURRENCY TRANS          
*&&US*&& MVI   PRFPNAL,C'V'        DEFAULT, # OF ADDRESS LINES TO PRINT         
*&&UK*&& MVI   PRFPNAL,C'4'        DEFAULT, # OF ADDRESS LINES TO PRINT         
         MVC   PRFPTCH,AC@AUTO                                                  
*                                                                               
         LA    RE,RNDTAB                                                        
DISREC01 CLI   0(RE),LANGENG       DEFAULT LANG                                 
         BE    DISREC02                                                         
         CLC   CULANG,0(RE)                                                     
         BE    DISREC02                                                         
         LA    RE,5(,RE)                                                        
         B     DISREC01                                                         
*                                                                               
DISREC02 ST    RE,RNDENTRY                                                      
         MVC   PRFRND,1(RE)        DEFAULT, PRINT TO THE PENNY                  
         MVI   APELCODE,RPFELQ                                                  
         GOTO1 GETEL,(R2)                                                       
         BNE   DISREC99                                                         
         LR    R9,R1                                                            
*                                                                               
         USING RPFELD,R9                                                        
         TM    RPFPOPT,RPFLFJT                                                  
         BZ    *+10                                                             
         MVC   PRFLFJT,APYES       LEFT JUSTIFY                                 
*                                                                               
         TM    RPFPOPT3,RPFXTBOX                                                
         BZ    *+8                                                              
         MVI   PRFBOX,C'X'         EXTRA BOX DETAIL                             
*                                                                               
         TM    RPFPOPT,RPFBOX                                                   
         BO    *+10                                                             
         MVC   PRFBOX,APNO         NO BOXES                                     
*                                                                               
         TM    RPFPOPT,RPFIACT                                                  
         BZ    *+10                                                             
         MVC   PRFIACT,APYES       SHOW INACTIVE ACCOUNTS                       
*                                                                               
         TM    RPFIND,RPFACLVL     ALL LEVELS PLUS INACTIVE ACCOUNTS            
         BZ    *+8                                                              
         MVI   PRFIACT,C'A'        SHOW INACTIVE ACCOUNTS                       
*                                                                               
         TM    RPFIND,RPFICZRO     ELIMINATE REC. IF ALL AGE COL=ZERO           
         BZ    *+8                                                              
         MVI   PRFIACT,C'C'        CHECK AGING COLUMNS ONLY                     
*                                                                               
         TM    RPFPOPT2,RPFIABUD   SHOW ACCOUNTS WITH BUDGETS ONLY              
         BZ    *+8                                                              
         MVI   PRFIACT,C'B'        YES                                          
*                                                                               
         TM    RPFPOPT2,RPFIATRN   SHOW ACCOUNTS WITH TRANS. ONLY               
         BZ    *+8                                                              
         MVI   PRFIACT,C'T'        YES                                          
*                                                                               
         TM    RPFPOPT,RPFRTOT                                                  
         BZ    *+10                                                             
         MVC   PRFRTOT,APYES       PRINT REDUNDANT TOTALS                       
*                                                                               
         TM    RPFPOPT,RPFRDAT                                                  
         BZ    *+10                                                             
         MVC   PRFRDAT,APYES       PRINT REDUNDANT DETAIL                       
*                                                                               
         TM    RPFEDOPT,RPFEDCMA   PRINT COMMAS?                                
         BO    *+10                YES                                          
         MVC   PRFPCMA,APNO        NO                                           
*                                                                               
         TM    RPFEDOPT,RPFEDZRO   PRINT ZEROS AS NONBLANK?                     
         BZ    *+10                NO                                           
         MVC   PRFPZAM,APYES       YES                                          
*                                                                               
         TM    RPFPOPT,RPFZEROT    PRINT ZERO TOTALS?                           
         BZ    *+10                NO                                           
         MVC   PRFPZTO,APYES       YES                                          
*&&US                                                                           
         TM    RPFPOPT,RPFPORT     PRINT PORTRAIT?                              
         BZ    *+8                 NO                                           
         MVI   PRFPPAP,C'P'        YES                                          
*                                                                               
         TM    RPFDNOPT,RPFDDOWN   DOWN LOAD ONLY?                              
         BZ    *+8                 NO                                           
         MVI   PRFPPAP,C'D'        YES                                          
*&&                                                                             
         TM    RPFPOPT,RPFMCASE    PRINT USING MIXED CASE?                      
         BZ    *+10                NO                                           
         MVC   PRFPUPP,APNO        YES                                          
*                                                                               
         TM    RPFPOPT2,RPFEXCOL   PRINT MIDS/TOTS ACROSS COLUMN?               
         BZ    *+10                NO                                           
         MVC   PRFPEXP,APYES       YES                                          
*                                                                               
         TM    RPFPOPT3,RPFNORQT   DON'T PRINT REQUEST TOTAL ?                  
         BZ    *+10                                                             
         MVC   PRFPRQT,APNO        NO REQUEST TOTAL                             
         TM    RPFPOPT2,RPFTOTSP   REQUEST TOTAL ON SEPARATE PAGE ?             
         BZ    *+8                 NO                                           
         MVI   PRFPRQT,C'S'        YES                                          
*                                                                               
*        TM    RPFPOPT2,RPFTOTSP   REQUEST TOTAL ON SEPARATE PAGE?              
*        BZ    *+10                NO                                           
*        MVC   PRFPTSP,APYES       YES                                          
*&&UK                                                                           
         TM    RPFIND,RPFIPRFX     PRINT FOREIGN CURRENCY PREFIX?               
         BZ    *+10                NO                                           
         MVC   PRFPFCP,APYES       YES                                          
*                                                                               
         TM    RPFIND,RPFIXNFC     EXCLUDE NON-FOREIGN CURR TRANS?              
         BZ    *+10                NO                                           
         MVC   PRFPXNC,APYES       YES                                          
*&&                                                                             
         TM    RPFIND,RPFIDERR     PRINT DIVISION ERROR AS "EEEEE"              
         BZ    *+10                NO                                           
         MVC   PRFPERR,APYES       YES                                          
*                                                                               
         TM    RPFPOPT3,RPFDBUL    DOUBLE UNDERLINE                             
         BZ    DISREC03            NO                                           
         MVI   PRFPUDT,C'E'                                                     
         TM    RPFPOPT3,RPFTPUL    UNDERLINE ABOVE (ON TOP)                     
         BZ    DISREC04            NO                                           
         MVI   PRFPUDT,C'D'                                                     
         B     DISREC04                                                         
*                                                                               
DISREC03 TM    RPFPOPT3,RPFTPUL    UNDERLINE ABOVE (ON TOP)                     
         BZ    *+8                 NO                                           
         MVI   PRFPUDT,C'A'                                                     
*                                                                               
         TM    RPFPOPT3,RPFNOUL    NO UNDERLINE                                 
         BZ    *+10                NO                                           
         MVC   PRFPUDT,APNO                                                     
*                                                                               
DISREC04 TM    RPFPOPT3,RPFNOCHD   PRINT COLUMN HEADINGS ?                      
         BZ    *+10                YES                                          
         MVC   PRFPCHD,APNO        NO                                           
*&&US                                                                           
         TM    RPFPOPT2,RPFSTRPE   PRINT STRIPED PAPER?                         
         BZ    *+8                 NO                                           
         MVI   PRFPBXO,C'S'        YES                                          
*                                                                               
         TM    RPFPOPT2,RPFISHDE   PRINT SHADE INSIDE THE BOX?                  
         BZ    *+8                 NO                                           
         MVI   PRFPBXO,C'I'        YES                                          
*                                                                               
         TM    RPFPOPT2,RPFOSHDE   PRINT SHADE OUTSIDE THE BOX?                 
         BZ    *+8                 NO                                           
         MVI   PRFPBXO,C'O'        YES                                          
*&&                                                                             
         CLI   PRFPMIN,C' '        ANY THING DISPLAYED ?                        
         BNE   *+12                                                             
         OI    RPFEDOPT,RPFEDTRL   DEFAULT TO TRAILING                          
         B     DISREC10                                                         
                                                                                
         TM    RPFEDOPT,RPFEDTRL   PRINT TRAILING MINUS SIGNS?                  
         BZ    *+12                YES                                          
         MVI   PRFPMIN,C'T'                                                     
         B     DISREC10                                                         
*                                                                               
         TM    RPFEDOPT,RPFEDLED   IS BIT SET                                   
         BZ    *+12                YES                                          
         MVI   PRFPMIN,C'L'        PRINT LEADING MINUS SIGNS?                   
         B     DISREC10                                                         
*                                                                               
         TM    RPFEDOPT,RPFEDBKT   IS BIT SET ?                                 
         BZ    *+12                YES                                          
         MVI   PRFPMIN,C'B'        PRINT BRACKETED MINUS SIGNS?                 
         B     DISREC10                                                         
*                                                                               
         TM    RPFEDOPT,RPFEDCR    IS BIT SET ?                                 
         BZ    *+12                YES                                          
         MVI   PRFPMIN,C'S'        PRINT MINUS AS "CR"                          
         B     DISREC10                                                         
                                                                                
         MVC   PRFPMIN,APNO        NO SIGN SET                                  
*                                                                               
DISREC10 TM    RPFNALPR,RPFNALPV   NUMBER OF   ADDRESS LINES TO PRINT           
         BZ    DISREC12                                                         
         MVI   PRFPNAL,C'V'        VARIABLE                                     
         B     DISREC14                                                         
*                                                                               
DISREC12 CLI   RPFNALPR,X'00'      INITIALIZED ?                                
         BE    DISREC14            NO,    USE  DEFAULTS                         
*&&US*&& CLI   RPFNALPR,X'04'      1-4 (US)?                                    
*&&UK*&& CLI   RPFNALPR,X'05'      1-5 (UK)?                                    
         BH    DISREC14            NO,    USE  DEFAULTS                         
         MVC   PRFPNAL,RPFNALPR    GET    NUM  1-4 US OR 1-5 UK                 
         OI    PRFPNAL,C'0'        MAKE   CHARACTER                             
*                                                                               
DISREC14 SR    R1,R1                                                            
         OI    PRFPTCHH+6,FVOXMT                                                
         ICM   R1,1,RPFPITCH       SET PITCH VALUE                              
         BNZ   DISREC15                                                         
         MVC   PRFPTCH,AC@AUTO     SET TO AUTO PITCH                            
         B     DISREC20                                                         
*                                                                               
DISREC15 MVC   PRFPTCH,SPACES                                                   
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  PRFPTCH(2),APDUB                                                 
*                                                                               
DISREC20 MVC   PRFPCTS,RPFPCTS     0/1/2 DECIMAL PLACES                         
         L     RE,RNDENTRY                                                      
         MVC   PRFRND,1(RE)        ROUND OPTION PENNY                           
         CLI   RPFRND,PENNY                                                     
         BE    DISREC30                                                         
         MVC   PRFRND,2(RE)        ROUND OPTION DOLLAR                          
         CLI   RPFRND,DOLLAR                                                    
         BE    DISREC30                                                         
         MVC   PRFRND,3(RE)        ROUND OPTION THOUSAND                        
         CLI   RPFRND,THOUSAND                                                  
         BE    DISREC30                                                         
         MVC   PRFRND,4(RE)        ROUND OPTION MILLION                         
*                                                                               
DISREC30 CLI   RPFRKON,0           ARE WE RANKING?                              
         BZ    DISREC99                                                         
         MVC   PRFRKON,RPFRKON                                                  
         OI    PRFRKON+1,X'F0'     CHANGE TO CHARACTER FORM NUMBER              
         SR    R1,R1                                                            
         IC    R1,RPFRKCL          RANKING COLUMN NUMBER                        
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  PRFRKCL,APDUB                                                    
         MVC   PRFRKOR,RPFRKOR     RANKING ORDER                                
*                                                                               
DISREC99 EQU   *                                                                
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
         SPACE 1                                                                
IVALEKEY MVC   FVMSGNO,=AL2(FVFEKEY)      ENTER KEY                             
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALEDOP MVC   FVMSGNO,=AL2(ACEEDOP)      INVALID NUMBER EDITING OPTION         
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALRKVT MVC   FVMSGNO,=AL2(1441)         INVALID TO RANK WITH V%               
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALRVOP MVC   FVMSGNO,=AL2(1442)         INVALID TO RANK WITH V%               
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVAL1CRC MVC   FVMSGNO,=AL2(ACE1CRC)      FIRST CHARCTER MUST BE R,C ..         
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVAL2CNU MVC   FVMSGNO,=AL2(ACE2CNU)      2ND CHARCTER MUST BE NUMERIC          
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALCNNU MVC   FVMSGNO,=AL2(ACECNNU)      COLUMN MUST BE NON-NUMERIC...         
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALCNU  MVC   FVMSGNO,=AL2(ACECNU)       COLUMN MUST BE NUMERIC TYPE           
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALCLNO MVC   FVMSGNO,=AL2(ACEIVCN)      INVALID COLUMN NUMBER                 
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALRWNO MVC   FVMSGNO,=AL2(ACEIVRN)      INVALID ROW NUMBER                    
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALIPUT MVC   FVMSGNO,=AL2(FVFNOTV)      INVALID INPUT                         
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALNNUM MVC   FVMSGNO,=AL2(FVFNOTN)      NOT NUMERIC                           
         B     IVALEXIT                                                         
*                                                                               
IVALPTCH MVC   FVMSGNO,=AL2(1452)         IVALID PITCH                          
         B     IVALEXIT                                                         
         SPACE 1                                                                
*&&US                                                                           
IVALWDTH MVC   FVMSGNO,=AL2(ACERTWD)      REPORT TOO WIDE                       
         LA    R2,PRFPPAPH                                                      
         ST    R2,FVADDR                                                        
         B     IVALEXIT                                                         
*&&                                                                             
         SPACE 1                                                                
IVALEXIT XC    APCURSOR,APCURSOR                                                
         B     EXIT                                                             
         SPACE 1                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
SCRTXT   DS    0F                                                               
         DC    AL4(PRFPO04H-TWAD,1604)   US (04) UK (04)                        
         DC    AL4(PRFPO06H-TWAD,1606)   US (06) UK (06)                        
*&&US*&& DC    AL4(PRFPO10H-TWAD,1610)   LANDSCAPE OR PORTRAIT                  
         DC    AL4(PRFPO11H-TWAD,1611)   UPPER CASE                             
         DC    AL4(PRFPO12H-TWAD,1612)   MIDS/TOTS ACROSS COLUMNS               
*&&US*&& DC    AL4(PRFPO13H-TWAD,1613)   STRIPES OR SHADING                     
         DC    AL4(PRFPO14H-TWAD,1607)   PRINT REQUEST TOTALS                   
*        DC    AL4(PRFPO14H-TWAD,1614)   TOTAL ON SEPARATE PAGE                 
*&&UK*&& DC    AL4(PRFPO15H-TWAD,1615)   FOREIGN CURR. PREFIX                   
*&&UK*&& DC    AL4(PRFPO16H-TWAD,1616)   EXCLUDE NON-FOREIGN                    
         DC    AL4(PRFPO17H-TWAD,1617)   PRINT DIVISION ERROR AS "E"            
         DC    AL4(PRFPO18H-TWAD,1618)   UNDERLINE TOTALS                       
         DC    AL4(PRFPR19H-TWAD,1619)   NUMBER OF ADDRESS LINES                
         DC    AL4(PRFPO28H-TWAD,1628)   PRINT COLUMN HEADINGS                  
         DC    AL4(PRFRO02H-TWAD,1620)                                          
         DC    AL4(PRFPT24H-TWAD,1624)   PITCH CONTROL                          
         DC    AL4(PRFRK01H-TWAD,1625)                                          
         DC    AL4(PRFRK02H-TWAD,1626)                                          
         DC    AL4(PRFRK03H-TWAD,1627)                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
RNDTAB   DS    0F                                                               
         DC    AL1(LANGEUS),CL4'PDTM'                                           
         DC    AL1(LANGEUK),CL4'PSTM'                                           
         DC    AL1(LANGGER),CL4'PDTM'                                           
         DC    AL1(LANGENG),CL4'PDTM'                                           
         EJECT ,                                                                
*---------------------------------------------------------------------*         
*                   DSECT FOR LOCAL WORKING STORAGE                   *         
*---------------------------------------------------------------------*         
LWSD     DSECT                                                                  
PENNY    EQU   C'P'                                                             
DOLLAR   EQU   C'D'                                                             
THOUSAND EQU   C'T'                                                             
MILLION  EQU   C'M'                                                             
RNDENTRY DS    A                                                                
*                                                                               
COLFLGS  DS    CL(MAXCOLS+1)       ONE BYTE ARRAY TO V% AND RANKING             
INACTFG  DS    XL1                 FLAG FOR INACTIVE ACCOUNT OPTION             
NCOLS    DS    XL1                 NUMBER OF COLUMNS                            
NROWS    DS    XL1                 NUMBER OF ROWS                               
COLWIDTH DS    XL1                 WIDTH OF COLUMNS                             
ONEXONLY DS    XL1                 ONE TIME ONLY FLAG                           
PARM_N   DS    XL1                 CURRENT PARAMETER WORKING ON                 
SAVEKEY1 DS    CL(L'RESKEY)                                                     
TEMPCHAR DS    CL1                                                              
AC@AUTO  DS    CL6                                                              
*&&US                                                                           
MYRCPSW  DS    CL1                 RECAP SWITCH                                 
*&&                                                                             
LWSX     DS    0C                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACSCRWRK                                                       
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
*  DSECT FOR HEADLINE DEFINITIONS                                     *         
***********************************************************************         
TWAD     DSECT                                                                  
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCRFAD                                                       
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'087ACSCR05   01/18/18'                                      
         END                                                                    
