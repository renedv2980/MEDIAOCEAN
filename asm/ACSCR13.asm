*          DATA SET ACSCR13    AT LEVEL 115 AS OF 08/13/19                      
*PHASE T60C13A                                                                  
*                                                                               
**********************************************************************          
* SMAN 106 05APR07 <LO01-5992> BRAND OCEAN REPORTING FOR NON (VB)               
*                  ACCENT MODULES                                               
* AHYD 107 13OCT15 <MOMOFLO-243> Add TAB delimeter for download                 
* YNGX 110 22SEP14 <PCA01185> download reports to USS server                    
* YNGX 111 22FEB15 <PCA02294> download option to remove Request pages           
* YNGX 112 17MAR16<PCA2358> Merge US and UK versions                            
* GHOA 113 31JUL18 SPEC-26412 EDIHUB VIA MQ MESSAGING                           
* GHOA 114 11JUL19 SPEC-37082 FIELD PROTECT EDI ID                              
* GHOA 115 12AUG19 SPEC-37082 FIELD PROTECT EDI ID: FIX BUG                     
***********************************************************************         
*                                                                     *         
* THIS VERSION MATCHES THE UK VERSION LEVEL 114 AS OF 11/29/17        *         
*                                                                     *         
***********************************************************************         
         TITLE 'Download Profile'                                               
         SPACE 2                                                                
T60C13   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,60C13,RA,R8,RR=RE,CLEAR=YES                                    
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
         CLC   ATWA,ACURSOR        INSURE CURSOR WAS IN A FIELD                 
         BE    SCR01                                                            
         CLI   TWALREC,RECDOWN     IF 1ST TIME IN THEN SET CURSOR TO...         
         BE    SCR02                                                            
*                                                                               
SCR01    LA    RE,DWNCODEH         FORMAT CODE FIELD                            
         ST    RE,ACURSOR                                                       
*                                                                               
SCR02    DS    0H                                                               
         CLI   APMODE,APMVALK                                                   
         BNE   SCR07                                                            
         LA    R2,SCRTXT                                                        
*                                                                               
SCR05    CLI   0(R2),X'FF'         END  OF   TABLE ?                            
         BE    SCR07                                                            
         L     R4,0(,R2)           GET  HEADER    ADDRESS                       
         AR    R4,R5                                                            
         OI    6(R4),FVOXMT        TRANSMIT  FIELD                              
         L     R3,4(,R2)           GET  SCREEN    NUMBER                        
         GOTO1 TEXTGET,APPARM,(R3),(R4),0                                       
         LA    R2,8(,R2)           BUMP TO   NEXT                               
         B     SCR05                                                            
*                                                                               
SCR07    DS    0H                                                               
         MVC   ERRORMSG,SPACES                                                  
         OI    DWNCODEH+6,FVOXMT+X'01'  NON-SENSE (MODIFY FIELD BIT TO          
*                                       REFRESH SCREEN ON HIT OF ENTER)         
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*&&UK*&& MVI   CURLANG,LANGEUK                                                  
*&&US*&& MVI   CURLANG,LANGEUS                                                  
         CLI   CULANG,LANGGER                                                   
         BH    SCR10                                                            
         CLI   CULANG,0                                                         
         BE    SCR10                                                            
         MVC   CURLANG,CULANG                                                   
         EJECT ,                                                                
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
         B     EXIT                DELETE COLUMN ELEMENT'S ONLY                 
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
         B     EXIT                COPY COLUMN ELEMENT'S                        
         B     EXIT                                                             
         B     EXIT                                                             
         EJECT ,                                                                
EXIT     CLI   APPFKEY,PFKNEXT                                                  
         BE    EXIT97                                                           
         CLI   APMODE,APMVALK                                                   
         BE    EXIT95                                                           
*        CLI   APMODE,APMDISK                                                   
*        BE    EXIT95                                                           
         TM    TWASWPST,TWASWAP    SHOULD     WE   SWAP ?                       
         BZ    EXIT95              NO                                           
*                                                                               
         CLI   APPFKEY,PFKHLP                                                   
         BNE   *+10                                                             
         XC    ACURDEF,ACURDEF          SET   TO   BEGINING  OF   HELP          
*                                       DON'T SET  CURSOR    ON                 
         XC    APCURSOR,APCURSOR              WRONG     SCREEN                  
*        MVI   APPFKEY,0                                                        
         MVI   APMODE,APMSWP                                                    
         MVC   APPARM(1),TWASWPRE       SWAP  TO   NEW  RECORD                  
         MVC   APPARM+1(1),TWASWPAC     SWAP  TO   NEW  ACTION                  
*                                                                               
EXIT95   OI    TWALSCTL,TWALSHLD                                                
*                                                                               
EXIT97   OI    TWALSCTL,TWALSRTN                                                
*                                                                               
EXIT99   DS    0H                                                               
         CLI   APMODE,APMVALR      IN   VALIDATE   RECORD                       
         BNE   EXIT999             NO,  SKIP                                    
         CLC   FVMSGNO,=AL2(FVFOK) ANY  ERRORS     FOUND ?                      
         BNE   EXIT999             YES, SKIP                                    
         CLI   APPFKEY,0           ANY  PF   KEY   DEPRESSED ?                  
         BNE   EXIT999             YES, SKIP                                    
         TM    TWASWPST,TWASWAP    SWAP TO   NEW   RECORD    ACTION ?           
         BO    EXIT999             YES, SKIP                                    
         MVC   APCURSOR,ACURSOR    NO,  SET  APPLICATION     CURSOR             
*                                                                               
EXIT999  CLC   FVMSGNO,=AL2(FVFOK) ANY  ERRORS     FOUND ?                      
*                                                                               
XIT      XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
*  VALKEY                                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R2                                                       
VALKEY   MVI   NEWKEY,NO           RESET TO NO                                  
         MVC   RESKEY,SPACES                                                    
         MVI   RESKTYP,RESKTYPQ    X'2D'                                        
         MVI   RESKSUB,RESKSUBQ    X'02'                                        
         MVC   RESKCPY,CUABIN      COMPANY CODE                                 
         CLI   TWALREC,RECDOWN     Down load                                    
         BE    *+8                                                              
         NI    GENIND,TURNOFF-GENPSWD   Reset                                   
*                                                                               
VALKEY10 GOTO1 AFVAL,DWNCODEH                                                   
         MVC   RESKFORM,FVIFLD                                                  
         BE    VALKEY15                                                         
         TM    DWNCODEH+4,FVITHIS  ANY INPUT?                                   
         BZ    *+10                                                             
         MVC   SAVFORM,SPACES                                                   
         CLC   SAVFORM,SPACES                                                   
         BNH   IVALEKEY            ENTER KEY                                    
         MVC   DWNCODE,SAVFORM                                                  
         OI    DWNCODEH+6,FVOXMT   TRANSMIT                                     
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
VALKEY19 MVI   APINDS,APIOKDIS+APIOKCHA                                         
         B     VALKEY90                                                         
*                                                                               
VALKEY20 MVI   STSEQ,1                                                          
         TM    IOERR,IOEDEL        IS RECORD MARKED DELETED                     
         BO    VALKEY99            OK TO ADD RECORD                             
         MVI   APINDS,APIOKADD     RECORD NOT ON FILE, SO OK TO ADD             
         MVI   NEWKEY,YES          WE ADDING A NEW RECORD                       
         L     R2,AIOAREA1                                                      
         XC    RESKEY(256),RESKEY  RESET AIO AREA                               
*                                                                               
VALKEY90 MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVI   SCEDIFLD,C'N'                                                    
         OC    ACASEC,ACASEC                                                    
         BZ    VALKEY99                                                         
         MVI   BYTE,40                        EDI ID FIELD                      
         LA    R2,BYTE                                                          
         GOTO1 VSECRET,ACPARM,('SECPFLDP',ACASEC),(R2)                          
         BL    VALKEY99                                                         
         MVI   SCEDIFLD,C'Y'                                                    
*                                                                               
VALKEY99 DS    0H                                                               
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  VALREC                                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R2                                                       
VALREC   MVC   SVKEY,IOKEY                                                      
         L     R2,AIOAREA1                                                      
         GOTO1 AFVAL,DWNNMEH                                                    
         BNE   VR002                       NAME HAS NOT BEEN INPUT              
         GOTO1 ADDNAME,APPARM,(R2),DWNNMEH GET  FORMAT NAME                     
         BNE   VR999                       ON   ERROR, EXIT                     
*                                                                               
VR002    DS    0H                                                               
*&&UK                                                                           
         MVI   XTALLOW,YES         ACCENT/QREPORT ALLOWED                       
         MVI   NROWS,0                                                          
         MVI   NCOLS,0                                                          
         USING RRWELD,R1                                                        
         L     R2,AIOAREA1                                                      
         MVI   APELCODE,RRWELQ     X'C2', ROW DATA ELEMENT                      
         GOTO1 GETEL,(R2)                                                       
         BE    *+12                                                             
         MVI   XTALLOW,NO         ACCENT/QREPORT NOT ALLOWED IF NO ROWS         
         B     VR005                                                            
                                                                                
         GOTO1 NEXTEL                                                           
         BE    *-2                                                              
         MVC   NROWS,RRWSEQ       GET TOTAL NUMBER OF ROWS                      
         DROP  R1                                                               
*                                                                               
         USING RCLELD,R1                                                        
VR005    L     R2,AIOAREA1                                                      
         MVI   APELCODE,RCLELQ     X'C3', COLUMN DATA ELEMENT                   
         GOTO1 GETEL,(R2)                                                       
         BNE   VR005A                                                           
         TM    RCLOPT,RCLACCM      COL 1 MUST BE AN ACCUMULATED COLUMN          
         BO    *+8                                                              
         MVI   XTALLOW,NO          ELSE ACCENT/QREPORT NOT ALLOWED              
         GOTO1 NEXTEL                                                           
         BE    *-2                                                              
         MVC   NCOLS,RCLSEQ        GET TOTAL NUMBER OF COLS                     
         DROP  R1                                                               
*&&                                                                             
         USING FFTELD,R1                                                        
VR005A   L     R2,AIOAREA1                                                      
         MVI   APELCODE,FFTELQ     X'DB' FREE FORM ELEM                         
         GOTOR GETEL,(R2)                                                       
         BNE   VR006                                                            
                                                                                
VR005E   CLI   FFTTYPE,FFTTESUB    SUBJECT                                      
         BE    VR005F                                                           
         CLI   FFTTYPE,FFTTEFIL    FILE                                         
         BE    VR005F                                                           
         CLI   FFTTYPE,FFTTEEXT    EXTENSION                                    
         BE    VR005F                                                           
         CLI   FFTTYPE,FFTTEDID    EDIHUB ID                                    
         BNE   *+8                                                              
VR005F   MVI   FFTEL,X'FF'         MARK DELETED                                 
         GOTOR NEXTEL                                                           
         BE    VR005E                                                           
                                                                                
         MVI   APELCODE,X'FF'                                                   
         GOTO1 DELEL,(R2)                                                       
         DROP  R1                                                               
                                                                                
         USING RPFELD,R9                                                        
VR006    LA    R9,APELEM                                                        
         XC    APELEM,APELEM       BUILD DEFAULT C4                             
         MVI   RPFEL,RPFELQ        ELEMENT CODE                                 
         MVI   RPFLN,RPFLN2Q       LENGTH                                       
         OI    RPFPOPT,RPFBOX      BOXES                                        
         OI    RPFEDOPT,RPFEDCMA   COMMAS                                       
         OI    RPFEDOPT,RPFEDTRL   TRAILING MINUS                               
         MVI   RPFPCTS,C'2'        2 DECIMAL PLACES FOR %                       
         MVI   RPFRND,C'P'         ROUNDING OPTION                              
         MVI   RPFFLDD,C' '        DEFAULT, FIELD DELIMITER                     
         MVI   RPFTXTD,C'"'        DEFAULT, TEXT DELIMITER                      
         MVI   RPFEOLD,X'5E'       DEFAULT, END OF LINE                         
         MVI   RPFEORD,C':'        DEFAULT, END OF REPORT                       
         MVI   APELCODE,RPFELQ                                                  
         MVI   DWNTYPE,0                                                        
*                                                                               
         GOTO1 GETEL,(R2)                                                       
         BNE   VR008                                                            
         SR    RF,RF                                                            
         IC    RF,1(,R1)                   GET ELEMENT LENGTH                   
         BCTR  RF,0                                                             
         EXMVC RF,RPFELD,0(R1)                                                  
         GOTO1 DELEL,(R2)                                                       
         CLI   RPFLN,RPFLNQ        OLD LENGTH ?                                 
         BNE   VR008                                                            
         MVI   RPFLN,RPFLN2Q       SET NEW LENGTH                               
                                                                                
VR008    MVI   RPFDTFMT,0          DEFAULTS                                     
         MVI   RPFDNOPT,0                                                       
         MVI   RPFNALDW,0          NUM OF ADDR LINES TO DOWNLOD                 
         MVI   RPFXMIT,0           INITIALIZE                                   
         MVI   RPFXMIT2,0                                                       
         MVI   RPFDNOP2,0                                                       
                                                                                
         USING DTFORMD,R3                                                       
         L     R2,AIOAREA1                                                      
         MVC   RESKEY,APRECKEY                                                  
         L     R3,=A(DATETAB)                                                   
         A     R3,APRELO                                                        
         GOTO1 AFVAL,DWNDFMTH                                                   
         BNE   VR016                                                            
         SR    R1,R1                                                            
         IC    R1,FVXLEN                                                        
         XC    A1STNTRY,A1STNTRY                                                
         DROP  R2                                                               
*                                                                               
VR012    CLI   0(R3),X'FF'         END OF TABLE?                                
         BNE   VR014                                                            
         L     R3,A1STNTRY         LOAD DEFAULT                                 
         B     VR016                                                            
*                                                                               
VR014    CLC   DTFLANG,CURLANG                                                  
         BNE   VR015                                                            
         OC    A1STNTRY,A1STNTRY                                                
         BNZ   *+8                                                              
         ST    R3,A1STNTRY                                                      
         EXCLC R1,DWNDFMT,DTFNAME                                               
         BE    VR016                                                            
*                                                                               
VR015    LA    R3,DTFLNQ(,R3)                                                   
         B     VR012                                                            
*                                                                               
VR016    MVC   RPFDTFMT,DTFCODE    FORMATED DATE NUMBER                         
         MVC   DWNDFMT,DTFNAME                                                  
         OI    DWNDFMTH+6,FVOXMT                                                
         DROP  R3                                                               
*                                                                               
VR018    CLC   DWNDCML,APYES                                                    
         BE    *+8                                                              
         OI    RPFDNOPT,RPFDNDEC   NO DECIMALS                                  
*                                                                               
         CLC   DWNTXT,APYES                                                     
         BNE   *+8                                                              
         OI    RPFDNOPT,RPFDTXT#   DOWNLOAD NUMBERS AS TEXT                     
*                                                                               
         CLC   DWNPAD,APYES                                                     
         BNE   *+8                                                              
         OI    RPFDNOPT,RPFDNPAD   PAD W/ LEADING ZEROS                         
*                                                                               
         CLC   DWNFXWD,APYES                                                    
         BNE   *+8                                                              
         OI    RPFDNOPT,RPFDNFXW   FIXED WIDTH FIELDS                           
*                                                                               
         CLC   DWNLFJT,APYES                                                    
         BNE   VR019                                                            
         GOTO1 AFVAL,DWNLFJTH                                                   
         TM    RPFDNOPT,RPFDTXT#+RPFDNFXW                                       
         BNO   IVALOPTM            OPTIONS MUST BE ON                           
         OI    RPFXMIT,RPFDLFJ$    LEFT JUSTIFY FIXED AMOUNT FIELD              
*                                                                               
*                                   NOTE: MAX WIDTH USED TO BE 165 BUT          
*                                         NOW IT IS BIGGER (198)                
VR019    CLC   DWNX165,APYES                                                    
         BNE   *+8                                                              
         OI    RPFDNOPT,RPFDDOWN   FORCE DOWN-LOAD                              
*                                                                               
         CLC   DWNCOLH,APYES                                                    
         BE    *+8                                                              
         OI    RPFDNOPT,RPFDNCOL   EXCLUDE COLUMN HEADINGS                      
*                                                                               
         CLC   DWNROWS,APYES                                                    
         BNE   *+8                                                              
         OI    RPFDNOPT,RPFDNROW   INCLUDE ROWS                                 
*&&UK                                                                           
         CLC   DWNRQPG,APNO        DEFAULT IS Y                                 
         BNE   *+8                                                              
         OI    RPFDNOP2,RPFDNRPG   NO REQUEST PAGE                              
*&&                                                                             
         CLC   DWNTOTS,APYES                                                    
         BNE   *+8                                                              
         OI    RPFDNOPT,RPFDNTOT   INCLUDE TOTALS                               
*                                                                               
         MVC   SAVDNOPT,RPFDNOPT   SAVE THE OPTION BYTE                         
*                                                                               
         GOTO1 AFVAL,DWNDNALH      NUMBER OF    ADDR LINES TO DOWN-LOAD         
         BNE   VR020               NO     DATA, USE DEFAULT                     
         CLI   DWNDNAL,C'0'        LESS   THAN  C'0' ?                          
         BL    IVALIPUT            YES,   INVALID    INPUT                      
*&&US*&& CLI   DWNDNAL,C'4'        MORE   THAN  C'4' ? (US)                     
*&&UK*&& CLI   DWNDNAL,C'5'        MORE   THAN  C'5' ? (UK)                     
         BH    IVALIPUT            YES,   INVALID    INPUT                      
         ZIC   RE,DWNDNAL          GET    CHAR  NUMBER                          
         LA    RF,C'0'             GET    CHAR  ZERO                            
         SR    RE,RF               GET    HEX   NUMBER                          
         STC   RE,RPFNALDW         SAVE   HEX   NUMBER                          
*                                                                               
VR020    MVC   EDICTKEY,SPACES                                                  
         GOTOR AFVAL,DWNEDICH      TRANSMIT TYPE                                
         BE    VR020D                                                           
         GOTOR AFVAL,DWNESUBH      SUBJECT                                      
         BE    IVALIPUT                                                         
         GOTOR AFVAL,DWNEFILH      FILE                                         
         BE    IVALIPUT                                                         
         GOTOR AFVAL,DWNEEXTH      FILE EXTENSION                               
         BE    IVALIPUT                                                         
         B     VR030                                                            
                                                                                
VR020D   TM    CUSTAT,CUSDDS       DDS ONLY                                     
         BZ    IVALIPUT                                                         
         OI    DWNEDICH+6,FVOXMT                                                
                                                                                
         ZIC   RF,FVILEN           INPUT LENGTH                                 
         LR    R1,RF                                                            
         LA    RE,FVIFLD                                                        
         AR    RE,RF               POINT TO END OF DATA                         
         SHI   RE,1                POINT TO LAST CHARACTER                      
         XC    SVREG,SVREG         FOUND COMMA, NO                              
         MVI   SVLEN,0                                                          
                                                                                
VR020E   CLC   0(1,RE),SCCOMMA                                                  
         JE    VR020J                                                           
         SHI   RE,1                                                             
         BRCT  RF,VR020E                                                        
         LR    RF,R1               RELOAD RF WITH LENGTH                        
         J     VR020K                                                           
                                                                                
VR020J   SHI   RF,1                ONE FOR COMMA                                
         AHI   RE,1                POINT PAST COMMA                             
         ST    RE,SVREG            FOUND COMMA, A(AFTER COMMA)                  
         SR    R1,RF               LENGTH REMAINING                             
         SHI   R1,1                SUBTRACT ONE FOR COMMA                       
         STC   R1,SVLEN            SAVE LENGTH OF TEXT                          
                                                                                
VR020K   CHI   RF,3                MINIMAL LENGTH OF INPUT                      
         JL    IVALIPUT                                                         
         CHI   RF,8                                                             
         JH    IVALIPUT            MAXIMUM LENGTH OF INPUT                      
         SHI   RF,1                ONE FOR EX                                   
                                                                                
         EXCLC RF,=CL3'FTP',DWNEDIC      FTP                                    
         JNE   VR022                                                            
         CLI   SVLEN,0             FOUND COMMA                                  
         JNE   VR021               YES                                          
         BRAS  RE,VALESUB                                                       
         JNL   IVALIPUT            NOT ALLOWED TO HAVE INPUT                    
         BRAS  RE,VALEFIL                                                       
         JNL   IVALIPUT            NOT VALID                                    
         OI    RPFXMIT,RPFXFTP     YES, SO OKAY                                 
         J     VR028                                                            
                                                                                
VR021    SR    RF,RF               FTP,XXXXX                                    
         IC    RF,SVLEN            LENGTH OF XXXXX                              
         SHI   RF,1                                                             
         JM    IVALIPUT                                                         
         L     RE,SVREG            TEXT OF EDICT KEY                            
         MVC   EDICTKEY,SPACES                                                  
         EXMVC RF,EDICTKEY,0(RE)                                                
         MVC   RPFEDNME,EDICTKEY                                                
         BRAS  RE,VALESUB                                                       
         JH    IVALIPUT                                                         
         BRAS  RE,VALEFIL                                                       
         JNE   VR999                                                            
         BRAS  RE,VALEEXT          FILE EXTENSION                               
         BNE   VR999               MUST BE VALID EXTENSION                      
         OI    RPFXMIT,RPFXFIL     FILE TRANSMISSION WITH OVERRIDE              
         J     VR028                                                            
                                                                                
VR022    DS    0H                                                               
*&&US                                                                           
         EXCLC RF,=CL4'DWN',DWNEDIC                                             
         JNE   VR022A                                                           
         OI    RPFXMIT,RPFXDSDN    DATASET TRANSMIT                             
         B     VR022B                                                           
*&&                                                                             
VR022A   EXCLC RF,=CL4'DSN',DWNEDIC                                             
         JNE   VR023                                                            
VR022B   SR    RF,RF                                                            
         IC    RF,SVLEN            LENGTH OF TEXT REMAINING                     
         SHI   RF,1                                                             
         BM    IVALIPUT                                                         
         L     RE,SVREG            TEXT OF EDICT KEY                            
         MVC   EDICTKEY,SPACES                                                  
         EXMVC RF,EDICTKEY,0(RE)                                                
         MVC   RPFEDNME,EDICTKEY                                                
                                                                                
*        CLC   DWNESUB,SPACES                                                   
*        JNH   IVALINOD            MUST HAVE INPUT - NO DATA INPUT              
         BRAS  RE,VALESUB                                                       
         JNE   IVALINOD            MUST HAVE VALID INPUT                        
         BRAS  RE,VALEFIL                                                       
         JH    IVALIPUT            Invalid input                                
         BRAS  RE,VALEEXT          FILE EXTENSION                               
         BNE   VR999               MUST BE VALID EXTENSION                      
         OI    RPFXMIT,RPFXDSN     DATASET TRANSMIT                             
*&&US                                                                           
         EXCLC RF,=CL4'DWN',DWNEDIC                                             
         JNE   VR028                                                            
         OI    RPFXMIT,RPFXDSDN    DATASET TRANSMIT                             
*&&                                                                             
         J     VR028                                                            
                                                                                
VR023    EXCLC RF,=CL6'ACCENT',DWNEDIC                                          
         BNE   VR025                                                            
         USING COMFACSD,R1                                                      
         USING XTRAINFD,RF                                                      
         L     R1,ACOM             A(COMFACS)                                   
         L     RF,CXTRAINF         A(XTRAINFO) IN COMFACS                       
         DROP  R1                                                               
*&&UK                                                                           
         CLI   XTALLOW,YES         ACCENT ALLOWED ?                             
         BNE   IVALACNT                                                         
                                                                                
         CLC   APREPCDE,AC@RCV                                                  
         BE    IVALIPUT                                                         
         CLC   APREPCDE,AC@PAY                                                  
         BE    IVALIPUT                                                         
         CLC   APREPCDE,AC@EXP                                                  
         BE    IVALIPUT                                                         
*&&                                                                             
         TM    XIFLAG1,XITSTADV                                                 
         BO    VR024                                                            
         TM    GENIND,GENPSWD      Password set ?                               
         BZ    IVALIPUT                                                         
         DROP  RF                                                               
VR024    CLI   SVLEN,0             WAS A COMMA SUPPLIED                         
         JNE   IVALIPUT            YES, SO NOT VALID                            
         OI    RPFXMIT,RPFXACNT                                                 
         BRAS  RE,VALEEXT          FILE EXTENSION                               
         BNE   VR999               MUST BE VALID EXTENSION                      
         CLI   FVILEN,0                                                         
         BE    IVALIPUT                                                         
         OI    DWNTYPE,DWNTACNT    ACCENT ENABLED                               
         J     VR030                                                            
                                                                                
VR025    EXCLC RF,=CL8'QREPORTS',DWNEDIC                                        
         BNE   VR027                                                            
         USING COMFACSD,R1                                                      
         USING XTRAINFD,RF                                                      
         L     R1,ACOM             A(COMFACS)                                   
         L     RF,CXTRAINF         A(XTRAINFO) IN COMFACS                       
         DROP  R1                                                               
                                                                                
*&&UK*&& CLI   XTALLOW,YES         QREPOR ALLOWED ?                             
*&&UK*&& BNE   IVALACNT                                                         
         TM    XIFLAG1,XITSTADV                                                 
         BO    VR026                                                            
         TM    GENIND,GENPSWD      Password set ?                               
         BZ    IVALIPUT                                                         
         DROP  RF                                                               
VR026    CLI   SVLEN,0             WAS A COMMA SUPPLIED                         
         JNE   IVALIPUT            YES, SO NOT VALID                            
         XR    RE,RE                                                            
         IC    RE,NROWS                                                         
         XR    RF,RF                                                            
         IC    RF,NCOLS                                                         
         AR    RE,RF                                                            
         CHI   RE,20                                                            
         BH    IVALROCO                                                         
*                                                                               
         OI    RPFXMIT,RPFXQREP                                                 
         BRAS  RE,VALEEXT          FILE EXTENSION                               
         BNE   VR999               MUST BE VALID EXTENSION                      
         CLI   FVILEN,0                                                         
         BE    IVALIPUT                                                         
         OI    DWNTYPE,DWNTQREP    QREPORT ENABLED                              
         J     VR030                                                            
*                                                                               
VR027    DS    0H                                                               
*&&UK                                                                           
         EXCLC RF,=CL3'BDE',DWNEDIC                                             
         BNE   VR027A                                                           
         BRAS  RE,VALESUB          TEST BDE PROFILE NAME                        
         JNE   IVALIPUT                                                         
         BRAS  RE,VALEFIL          TEST FILE NAME GIVEN                         
         JNE   IVALIPUT                                                         
         BRAS  RE,VALEEXT          TEST FILE EXTENSION                          
         JNE   IVALIPUT                                                         
         OI    RPFXMIT,RPFXBDE                                                  
         J     VR030                                                            
*                                                                               
VR027A   EXCLC RF,=CL3'USS',DWNEDIC                                             
         BNE   VR027B                                                           
         BRAS  RE,VALEFIL          TEST SFTP URL GIVEN                          
         JNE   IVALIPUT                                                         
         OI    RPFXMIT,RPFXUSS                                                  
         J     VR030                                                            
*&&                                                                             
VR027B   DS    0H                                                               
         EXCLC RF,=CL6'EDIHUB',DWNEDIC                                          
         BNE   IVALIPUT                                                         
*                                                                               
         OI    DWNEDIDH+6,FVAPROT                                               
         CLI   SCEDIFLD,C'Y'                                                    
         BNE   VR027D                                                           
         NI    DWNEDIDH+6,X'FF'-FVAPROT       UNPROTECT EDI ID                  
         OI    DWNEDIDH+6,FVOXMT                                                
*                                                                               
VR027D   LA    R2,DWNEDIDH         EDI ID FIELD                                 
         ST    R2,APCURSOR                                                      
         BRAS  RE,VALEID           TEST WE HAVE EDIHUB ID ON COMPANY            
         JNE   VR999                                                            
         OI    RPFXMIT2,RPFXEDI                                                 
         J     VR030                                                            
*                                                                               
VR028    BAS   RE,VALEDICT                                                      
         BNE   VR999                                                            
*                                                                               
VR030    DS    0H                                                               
*&&UK                                                                           
         CLC   DWNMSSF,APYES                                                    
         BNE   VR034                                                            
         TM    RPFXMIT,RPFXUSS     TEST TYPE = USS                              
         BO    *+12                                                             
         TM    RPFXMIT2,RPFXEDI    TEST TYPE = EDIHUB                           
         BZ    IVALMSSF            ONLY VALID IF TYPE = USS /EDIHUB             
         OI    RPFDNOP2,RPFDSFTP   SEND REPORT TO MS SFTP SERVERS               
*&&                                                                             
VR034    GOTO1 AFVAL,DWNFLDDH      SEPERATOR FIELD                              
         GOTO1 VALFDLM,C' '        CHECK FIELD DELIMITER, SET DEFAULT           
         BNE   VR999                                                            
         MVC   RPFFLDD,APBYTE      APBYTE SET IN VALFDLM                        
*                                                                               
         GOTO1 AFVAL,DWNEOTDH      TEXT  DELIMITER                              
         GOTO1 VALFDLM,C'"'        CHECK FIELD DELIMITER                        
         BNE   VR999                                                            
         MVC   RPFTXTD,APBYTE      APBYTE SET IN VALFDLM                        
*                                                                               
         GOTO1 AFVAL,DWNEOLDH      END OF LINE DELIMITER                        
         GOTO1 VALFDLM,X'5E'       CHECK FIELD DELIMITER                        
         BNE   VR999                                                            
         MVC   RPFEOLD,APBYTE      APBYTE SET IN VALFDLM                        
*                                                                               
         GOTO1 AFVAL,DWNEORDH      END OF RECORD DELIMITER                      
         GOTO1 VALFDLM,C':'        CHECK FIELD DELIMITER                        
         BNE   VR999                                                            
         MVC   RPFEORD,APBYTE      APBYTE SET IN VALFDLM                        
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD    ELEMENT                               
         BNE   VR999               ON     ERROR, EXIT                           
         DROP  R9                                                               
*                                  CHECK WIDTH NOT OVER 198                     
         USING STYELD,R1           SCRIBE FREE   FORM  ELEMENT                  
VR060    SR    R6,R6               INITIALIZE    WIDTH OF   LINE                
         L     R1,AIOAREA1                                                      
         MVI   APELCODE,STYELQ     X'25'  FREE   FORM  SCRIBE   ELEMENT         
         GOTO1 GETEL               GET    COLUMN ELEMENT                        
         BE    *+6                 NOT    FOUND, SKIP                           
         DC    H'00'               ELEMENT SHOULD BE ON RECORD                  
*                                                                               
         NI    STYSTAT,TURNOFF-(STYSACNT+STYSQREP)                              
         TM    DWNTYPE,DWNTACNT    ACCENT ENABLED?                              
         JZ    *+8                 NO                                           
         OI    STYSTAT,STYSACNT    YES                                          
         TM    DWNTYPE,DWNTQREP    QREPORT ENABLED?                             
         JZ    *+8                 NO                                           
         OI    STYSTAT,STYSQREP    YES                                          
*                                                                               
         TM    SAVDNOPT,RPFDDOWN   CHECK FOR DOWN-LOAD                          
         BO    VR100               YES, DOWN-LOAD                               
         LH    R6,STYWIDTH         GET    WIDTH  OF    REPORT                   
         CHI   R6,MAXRPTWD         CHECK  MAX    WIDTH                          
         BH    IVALWDTH            REPORT TOO    WIDE                           
         DROP  R1                                                               
*                                                                               
VR100    DS    0H                                                               
         MVC   IOKEY,SVKEY                                                      
         GOTO1 ADDID,APPARM,AIOAREA1                                            
         BNE   VR999               ON     ERROR, EXIT                           
         LA    R1,IOADD+IOACCFIL+IO1                                            
         TM    APINDS,APIOKADD     ADD    A      RECORD ?                       
         BO    VR850                                                            
         LA    R1,IOWRITE+IOACCFIL+IO1                                          
         TM    APINDS,APIOKCHA     CHANGE A      RECORD ?                       
         BO    VR850                                                            
         DC    H'0'                                                             
*                                                                               
VR850    GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR999    CLC   FVMSGNO,=AL2(FVFOK) ANY    ERRORS FOUND ?                        
         BE    DISREC              NO,    CONTINUE                              
         B     EXIT                YES,   EXIT                                  
         EJECT ,                                                                
***********************************************************************         
*  DISKEY                                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R2                                                       
DISKEY   DS    0H                                                               
         LA    R2,APRECKEY                                                      
         MVC   DWNCODE,RESKFORM                                                 
         NI    TWASWPST,TURNOFF-TWASWAP                                         
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISREC                                                             *         
***********************************************************************         
         SPACE 1                                                                
DISREC   TWAXC DWNNMEH,DWNOWNH                                                  
         TWAXC DWNDFMTH,DWNEEXTH                                                
*                                                                               
         L     R2,AIOAREA1         COLUMN ELEMENTS                              
         GOTO1 GETNAME,APPARM,(R2),DWNNMEH                                      
         GOTO1 GETPER,APPARM,(R2),DWNOWNH                                       
*                                                                               
         MVI   DWNDFMT,0           DATE FORMAT  (DEFAULTS)                      
         MVC   DWNDCML,APYES       DECIMAL                                      
         MVC   DWNTXT,APNO         AMOUNTS NOT IN TEXT FORMAT                   
         MVC   DWNPAD,APNO         PAD $ COLS W/ LEADING ZEROS                  
         MVC   DWNFXWD,APNO        FIXED WIDTH FIELDS                           
         MVC   DWNX165,APNO        FORMAT EXCEEDS 198 CHARACTERS                
         MVC   DWNLFJT,APNO        LEFT JUSTIFY AMOUNT FIELDS                   
         MVC   DWNCOLH,APYES       INCLUDE COLUMN HEADINGS                      
         MVC   DWNROWS,APNO        INCLUDE ROWS                                 
         MVC   DWNTOTS,APNO        INCLUDE TOTALS                               
*&&UK*&& MVC   DWNRQPG,APYES       SET DEFAULT TO INCLUDE REQUEST PAGE          
*&&UK*&& MVC   DWNMSSF,APNO        SEND REPORT TO MS SFTP SERVER                
         MVI   DWNDNAL,C'0'        NUMBER OF ADDRESS LINES TO DOWN-LOAD         
         MVCDD DWNFLDD,AC#BLNK     BETWEEN TEXT DELIMITER                       
         MVI   DWNEOTD,C'"'        AROUND TEXT DELIMITER                        
         MVI   DWNEOLD,X'5E'       SEMI-COLON  END OF LINE DELIMITER            
         MVI   DWNEORD,C':'        END OF REPORT DELIMITER                      
         MVC   DWNEDIC,SPACES                                                   
         OI    DWNEDICH+6,FVOXMT                                                
         MVC   DWNEDID,SPACES                                                   
         OI    DWNEDIDH+6,FVAPROT                                               
         CLI   SCEDIFLD,C'Y'                                                    
         BNE   DR008                                                            
         NI    DWNEDIDH+6,X'FF'-FVAPROT       UNPROTECT EDI ID                  
DR008    OI    DWNEDIDH+6,FVOXMT                                                
*                                                                               
         USING RPFELD,R9                                                        
         L     R2,AIOAREA1                                                      
         MVI   APELCODE,RPFELQ     X'C4', PROFILE ELEMENT                       
         GOTO1 GETEL,(R2)                                                       
         BNE   EXIT                                                             
         LR    R9,R1                                                            
*                                                                               
         USING DTFORMD,R3                                                       
         L     R3,=A(DATETAB)                                                   
         A     R3,APRELO                                                        
         XC    A1STNTRY,A1STNTRY                                                
*                                                                               
DR010    CLI   0(R3),X'FF'         END OF TABLE                                 
         BNE   DR012                                                            
         L     R3,A1STNTRY         LOAD DEFAULT                                 
         B     DR016                                                            
*                                                                               
DR012    CLC   DTFLANG,CURLANG                                                  
         BNE   DR014                                                            
         OC    A1STNTRY,A1STNTRY                                                
         BNZ   *+8                                                              
         ST    R3,A1STNTRY                                                      
         CLC   RPFDTFMT,DTFCODE                                                 
         BE    DR016                                                            
*                                                                               
DR014    LA    R3,DTFLNQ(,R3)                                                   
         B     DR010                                                            
*                                                                               
DR016    MVC   DWNDFMT,DTFNAME                                                  
         OI    DWNDFMTH+6,FVOXMT                                                
         DROP  R3                                                               
*                                                                               
         TM    RPFDNOPT,RPFDNDEC   IF ON THEN NO DECIMALS                       
         BZ    *+10                                                             
         MVC   DWNDCML,APNO                                                     
         OI    DWNDCMLH+6,FVOXMT                                                
*                                                                               
         TM    RPFDNOPT,RPFDTXT#   IF ON THEN $ IN TEXT FORMAT                  
         BZ    *+10                                                             
         MVC   DWNTXT,APYES                                                     
         OI    DWNTXTH+6,FVOXMT                                                 
*                                                                               
         TM    RPFDNOPT,RPFDNPAD   IF ON THEN PAD WITH LEADING ZEROS            
         BZ    *+10                                                             
         MVC   DWNPAD,APYES                                                     
         OI    DWNPADH+6,FVOXMT                                                 
*                                                                               
         TM    RPFDNOPT,RPFDNFXW   IF ON THEN FIXED COLUMN WIDTHS               
         BZ    *+10                                                             
         MVC   DWNFXWD,APYES                                                    
         OI    DWNFXWDH+6,FVOXMT                                                
*                                                                               
         TM    RPFXMIT,RPFDLFJ$   IF ON THEN LEFT JUSTIFY AMOUNTS               
         BZ    *+10                                                             
         MVC   DWNLFJT,APYES                                                    
         OI    DWNLFJTH+6,FVOXMT                                                
*                                                                               
         TM    RPFDNOPT,RPFDDOWN   FORCE DONWLOAD?                              
         BZ    *+10                                                             
         MVC   DWNX165,APYES                                                    
         OI    DWNX165H+6,FVOXMT                                                
*                                                                               
         TM    RPFDNOPT,RPFDNCOL   IF ON NO COLUMN HEADINGS                     
         BZ    *+10                                                             
         MVC   DWNCOLH,APNO                                                     
         OI    DWNCOLHH+6,FVOXMT                                                
*                                                                               
         TM    RPFDNOPT,RPFDNROW   IF ON INCLUDE ROWS                           
         BZ    *+10                                                             
         MVC   DWNROWS,APYES                                                    
         OI    DWNROWSH+6,FVOXMT                                                
*&&UK                                                                           
         TM    RPFPOPT3,RPFNORQP   IF ON NO REQUEST PAGE FOR DOWNLOAD           
         BZ    *+10                                                             
         MVC   DWNRQPG,APNO                                                     
         OI    DWNRQPGH+6,FVOXMT                                                
*                                                                               
         TM    RPFDNOP2,RPFDSFTP   IF ON SEND TO MS SFPT SERVER                 
         BZ    *+10                                                             
         MVC   DWNMSSF,APYES                                                    
         OI    DWNMSSFH+6,FVOXMT                                                
*&&                                                                             
         TM    RPFDNOPT,RPFDNTOT   IF ON INCLUDE TOTALS                         
         BZ    *+10                                                             
         MVC   DWNTOTS,APYES                                                    
         OI    DWNTOTSH+6,FVOXMT                                                
*                                                                               
*&&US*&& CLI   RPFNALDW,X'04'      NUM  OF   ADDRESS LINES TO DOWN-LOAD         
*&&UK*&& CLI   RPFNALDW,X'05'                                                   
         BH    DR018                                                            
         MVC   DWNDNAL,RPFNALDW    GET  HEX  NUMBER                             
         OI    DWNDNAL,C'0'        MAKE CHAR NUMBER                             
         OI    DWNDNALH+6,FVOXMT   TRANSMIT                                     
*                                                                               
DR018    CLI   RPFLN,RPFLNQ        OLD LENGTH ?                                 
         BE    DR030                                                            
         GOTO1 DISFDLM,LWPARM,DWNFLDDH,RPFFLDD                                  
         GOTO1 DISFDLM,LWPARM,DWNEOTDH,RPFTXTD                                  
         GOTO1 DISFDLM,LWPARM,DWNEOLDH,RPFEOLD                                  
         GOTO1 DISFDLM,LWPARM,DWNEORDH,RPFEORD                                  
                                                                                
*&&UK                                                                           
         TM    RPFXMIT,RPFXBDE     BDE FILE TRANSFER                            
         JZ    DR018A                                                           
         MVC   DWNEDIC(3),=CL3'BDE'                                             
         OI    DWNEDICH+6,FVOXMT                                                
         J     DR026                                                            
*                                                                               
DR018A   TM    RPFXMIT,RPFXUSS     USS FILE TRANSFER                            
         JZ    DR019                                                            
         MVC   DWNEDIC(3),=CL3'USS'                                             
         OI    DWNEDICH+6,FVOXMT                                                
         J     DR026                                                            
*&&                                                                             
DR018B   TM    RPFXMIT2,RPFXEDI    EDIHUB FILE TRANSFER                         
         JZ    DR019                                                            
         MVC   DWNEDIC(6),=CL6'EDIHUB'                                          
         OI    DWNEDICH+6,FVOXMT                                                
         J     DR026                                                            
*                                                                               
DR019    TM    RPFXMIT,RPFXFTP+RPFXFIL   FILE TRANSFER PROTOCAL OR              
         JZ    DR020                     FILE NAME ?                            
         MVC   DWNEDIC(3),=CL3'FTP'                                             
         OI    DWNEDICH+6,FVOXMT                                                
         TM    RPFXMIT,RPFXFIL                                                  
         JZ    DR026                                                            
         MVC   DWNEDIC+3(1),SCCOMMA                                             
         MVC   DWNEDIC+4(8),RPFEDNME                                            
         J     DR026                                                            
*                                                                               
DR020    TM    RPFXMIT,RPFXDSN     DATA SET NAME                                
         JZ    DR022                                                            
         MVC   DWNEDIC(3),=CL3'DSN'                                             
*&&US                                                                           
         TM    RPFXMIT,RPFXDSDN                                                 
         BZ    *+10                                                             
         MVC   DWNEDIC(3),=CL3'DWN'                                             
*&&                                                                             
         MVC   DWNEDIC+3(1),SCCOMMA                                             
         MVC   DWNEDIC+4(8),RPFEDNME                                            
         J     DR026                                                            
                                                                                
DR022    TM    RPFXMIT,RPFXACNT    ACCENT                                       
         JZ    DR024                                                            
*&&US                                                                           
         MVC   DWNEDIC(6),=CL6'******'                                          
         TM    CUSTAT,CUSDDS       DDS ONLY                                     
         BZ    *+10                                                             
*&&                                                                             
         MVC   DWNEDIC(6),=CL6'ACCENT'                                          
         OI    DWNEDICH+6,FVOXMT                                                
         J     DR026                                                            
                                                                                
DR024    TM    RPFXMIT,RPFXQREP    QUICK REPORT                                 
         JZ    DR026                                                            
*&&US                                                                           
         MVC   DWNEDIC(8),=CL8'********'                                        
         TM    CUSTAT,CUSDDS       DDS ONLY                                     
         BZ    *+10                                                             
*&&                                                                             
         MVC   DWNEDIC(8),=CL8'QREPORTS'                                        
         OI    DWNEDICH+6,FVOXMT                                                
                                                                                
         USING FFTELD,R1                                                        
DR026    L     R2,AIOAREA1                                                      
         MVI   APELCODE,FFTELQ     X'DB', FREE FORM ELEMENT                     
         GOTOR GETEL,(R2)                                                       
         JNE   DR030                                                            
                                                                                
DR026A   OI    DWNESUBH+6,FVOXMT                                                
         OI    DWNEFILH+6,FVOXMT                                                
         OI    DWNEEXTH+6,FVOXMT                                                
         OI    DWNEDIDH+6,FVOXMT                                                
         LA    RE,DWNEDIDH                                                      
         CLI   FFTTYPE,FFTTEDID    EDIHUB ID                                    
         BE    DR027                                                            
         LA    RE,DWNESUBH                                                      
         CLI   FFTTYPE,FFTTESUB                                                 
         BE    DR027                                                            
         LA    RE,DWNEFILH                                                      
         CLI   FFTTYPE,FFTTEFIL                                                 
         BE    DR027                                                            
         LA    RE,DWNEEXTH                                                      
         CLI   FFTTYPE,FFTTEEXT                                                 
         BNE   DR028                                                            
*&&US                                                                           
         TM    CUSTAT,CUSDDS       DDS ONLY                                     
         BO    DR027                                                            
         TM    RPFXMIT,RPFXACNT    ACCENT                                       
         BZ    DR027                                                            
         MVC   8(3,RE),=C'***'                                                  
         B     DR028                                                            
*&&                                                                             
DR027    ZIC   RF,0(,RE)           LENGTH OF FIELD + HDR                        
         SHI   RF,8                                                             
         CLM   RF,1,FFTDLEN                                                     
         BL    *+8                                                              
         IC    RF,FFTDLEN          LENGTH OF DATA                               
         BCTR  RF,0                                                             
         EXMVC RF,8(RE),FFTDATA                                                 
                                                                                
DR028    GOTOR NEXTEL                                                           
         BE    DR026A                                                           
         DROP  R1                                                               
*                                                                               
DR030    DS    0H                                                               
         OI    RPFXMIT,RPFXDSN     DATASET TRANSMIT                             
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   DR099                                                            
         CLI   APACTN,ACTDIS       IN   DISPLAY MODE ?                          
         BE    DR099               YES, SKIP                                    
         CLI   TWALREC,RECDOWN     IF   FIRST TIME IN, THEN SET CURSOR          
         BE    DR099               NO,  SKIP                                    
*                                                                               
         LA    RE,DWNCODEH         FORMAT CODE FIELD                            
         ST    RE,APCURSOR         SET  APPLICATION CURSOR                      
*                                                                               
DR099    DS    0H                                                               
         B     EXIT                                                             
         DROP  R9                                                               
         EJECT ,                                                                
**********************************************************************          
* Translate into text special delimiters                                        
**********************************************************************          
DISFDLM  NTR1                                                                   
         L     R3,0(,R1)           A(SCREEN FIELD HEADER)                       
         L     R4,4(,R1)           A(PROFILE DATA)                              
         SR    R1,R1                                                            
         IC    R1,0(,R3)           LENGTH OF FIELD + HDR                        
         AHI   R1,-9                                                            
         EXMVC R1,8(R3),SPACES     CLEAR                                        
         MVC   8(1,R3),0(R4)       MOVE IN FIELD DATA                           
         LA    R1,1(,R1)           ADD BACK TO ONE FROM EXCUTE INSTR.           
         CLI   0(R4),0             IS IT NULL, THEN SET TO "NONE"               
         BNE   DISFDLM1                                                         
         MVI   8(R3),ESCLFJTQ                                                   
         MVC   9(2,R3),=AL2(AC#NONE)                                            
         STC   R1,11(,R3)                                                       
         B     DISFDLM4                                                         
*                                                                               
DISFDLM1 CLI   0(R4),X'05'         IS IT TAB, THEN SET TO "TAB"                 
         BNE   DISFDLM2                                                         
         MVI   8(R3),ESCLFJTQ                                                   
         MVC   9(2,R3),=AL2(AC#TAB)                                             
         STC   R1,11(,R3)                                                       
         B     DISFDLM4                                                         
*                                                                               
DISFDLM2 CLI   0(R4),C' '          IS IT BLANK, THEN SET TO "BLANK"             
         BNE   DISFDLM4                                                         
         MVI   8(R3),ESCLFJTQ                                                   
         MVC   9(2,R3),=AL2(AC#BLNK)                                            
         STC   R1,11(,R3)                                                       
*                                                                               
DISFDLM4 OI    6(R3),FVOXMT                                                     
         B     XIT                                                              
         EJECT ,                                                                
**********************************************************************          
* Validate possible delimiters                                                  
**********************************************************************          
VALFDLM  NTR1                                                                   
         STC   R1,APBYTE           SET DEFAULT PASSED IN R1                     
         MVCDD AC@BLANK,AC#BLNK                                                 
         GOTO1 VDICTAT,APPARM,C'SU  ',AC@BLANK,0                                
         MVCDD AC@NONE,AC#NONE                                                  
         GOTO1 VDICTAT,APPARM,C'SU  ',AC@NONE,0                                 
         MVCDD AC@TAB,AC#TAB                                                    
         GOTO1 VDICTAT,APPARM,C'SU  ',AC@TAB,0                                  
         CLI   FVILEN,1            CHECK LENGTH OF INPUT                        
         BL    VALFDLMX                                                         
         MVC   APBYTE,FVIFLD       MOVE IN SINGLE CHARACTER VALUE               
         BE    VALFDLMX                                                         
         SR    R1,R1                                                            
         IC    R1,FVXLEN                                                        
         MVI   APBYTE,C' '                 SET TO "BLANK"                       
         EXCLC R1,FVIFLD,AC@BLANK                                               
         BE    VALFDLMX                                                         
         MVI   APBYTE,X'05'                SET TO "TAB"                         
         EXCLC R1,FVIFLD,AC@TAB                                                 
         BE    VALFDLMX                                                         
         MVI   APBYTE,0                    SET TO "NONE"                        
         EXCLC R1,FVIFLD,AC@NONE                                                
         BE    VALFDLMX                                                         
         MVC   FVMSGNO,=AL2(FVFNOTV)       INVALID INPUT                        
VALFDLMX CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         EJECT ,                                                                
**********************************************************************          
* VALIDATE EDICT RECORD                                              *          
**********************************************************************          
         USING CTIREC,R3                                                        
         USING RPFELD,R9                                                        
VALEDICT NTR1                                                                   
         LA    R2,DWNCODEH         FORMAT CODE FIELD                            
         ST    R2,APCURSOR                                                      
         LA    RF,8                                                             
         AHI   R2,8                                                             
         TM    RPFXMIT,RPFXFTP                                                  
         BO    VEDICT04                                                         
                                                                                
         LA    R2,DWNEDICH                                                      
         ST    R2,APCURSOR                                                      
         ZIC   RF,0(,R2)           LENGTH OF FIELD + HDR                        
         SHI   RF,8                                                             
         AHI   R2,8                ADD 8 FOR HDR, A(DATA)                       
VEDICT02 CLC   0(1,R2),SCCOMMA                                                  
         BE    *+14                                                             
         AHI   R2,1                NEXT FIELD                                   
         BCT   RF,VEDICT02                                                      
         DC    H'00'                                                            
                                                                                
         AHI   R2,1                BUMP PAST COMMA                              
         SHI   RF,1                                                             
         B     VEDICT05                                                         
                                                                                
VEDICT04 GOTOR CKFILNM,APPARM,(R2),(RF)                                         
         BNE   VEDICTX                                                          
                                                                                
VEDICT05 TM    RPFXMIT,RPFXFTP                                                  
         BO    VEDICT16                                                         
         TM    RPFXMIT,RPFXFIL                                                  
         BO    VEDICT25                                                         
         CLC   =C'GENERIC',EDICTKEY   Skip company check for GENERIC            
         BE    VEDICT25                                                         
         LA    R0,L'EDICTKEY-3                                                  
         LA    RE,EDICTKEY+L'EDICTKEY-1                                         
VEDICT10 CLI   0(RE),C' '                                                       
         BH    VEDICT12                                                         
         BCTR  RE,0                                                             
         BCT   R0,VEDICT10                                                      
*                                                                               
VEDICT12 SHI   RE,1                                                             
         CLC   CUAALF,0(RE)        MUST BE CORRECT COMPANY ALPHA                
         BE    VEDICT25             - ALPHA IS AT END OF EDICT NAME             
         MVC   FVMSGNO,=AL2(1640)  EDICT RECORD NOT SETUP                       
         B     VEDICTX                                                          
*                                                                               
VEDICT16 LA    R3,IOKEY                                                         
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,CUUSER                                                   
         GOTO1 AIO,IORD+IOCTFILE+IO3                                            
         MVC   FVMSGNO,=AL2(2085)  COMPANY RECORD NOT FOUND                     
         L     R3,AIOAREA3                                                      
         CLC   CTIKEY,IOKEY                                                     
         BNE   VEDICTX                                                          
*                                                                               
         USING CTDSCD,R4                                                        
         LA    R4,CTIDATA                                                       
         SR    R1,R1                                                            
VEDICT18 CLI   0(R4),0             EOR                                          
         BE    VEDICTX                                                          
         CLI   0(R4),CTDSCELQ      X'02', DESCRIPTION ELEMENT                   
         BE    VEDICT20                                                         
         IC    R1,1(,R4)                                                        
         AR    R4,R1                                                            
         B     VEDICT18                                                         
*                                                                               
VEDICT20 IC    R1,CTDSCLEN                                                      
         SHI   R1,(CTDSC-CTDSCD+1)                                              
         BM    VEDICTX                                                          
         EXMVC R1,EDICTKEY,CTDSC     COMPANY LOGON CODE ?                       
         DROP  R4                                                               
*                                                                               
         USING EDIKEYD,R3                                                       
VEDICT25 LA    R3,IOKEY                                                         
         XC    EDIKEY,EDIKEY                                                    
         MVI   EDIKSYS,EDIKSYSQ    X'05' - KEY SYSTEM FOR ALL SYSTEMS           
         MVI   EDITYPE,EDITYPEQ    X'07' - EDICT TYPE                           
         MVC   EDINAME,EDICTKEY                                                 
         OC    EDINAME,SPACES                                                   
         GOTO1 AIO,IO3+IORD+IOCTFILE                                            
         MVC   FVMSGNO,=AL2(1640)  EDICT RECORD NOT SETUP                       
         BNE   VEDICTX                                                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VEDICTX  CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         DROP  R3,R9                                                            
         EJECT ,                                                                
**********************************************************************          
* VALIDATE EDIHUB - MAKE SURE WE HAVE EDIHUB ID ON COMPANY           *          
**********************************************************************          
VALEID   NTR1                                                                   
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
         OC    DWNEDID,SPACES      MAKE IT UPPERCASE                            
         CLC   CUAALF,DWNEDID      CHECK ALPHA CODE MATCHES                     
         JNE   VALEIDX                                                          
         CLI   DWNEDID+2,C'U'      3RD CHAR HAS TO BE U                         
         JNE   VALEIDX                                                          
         CLI   DWNEDID+3,C' '                                                   
         JE    VALEIDX                                                          
*                                                                               
         USING FFTELD,RF                                                        
         MVC   SVELEM,APELEM                                                    
         XC    APELEM,APELEM                                                    
         LA    RF,APELEM                                                        
         MVI   FFTEL,FFTELQ        X'DB' Free form element                      
         MVI   FFTLN,9                                                          
         MVI   FFTTYPE,FFTTEDID    X'9A' EDIHUB ID used by Scribe               
         MVI   FFTDLEN,4                                                        
         MVC   FFTDATA(4),DWNEDID  ALPHA + U + ?                                
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD    ELEMENT                               
         BNE   VALEIDX             ON     ERROR, EXIT                           
         MVC   APELEM,SVELEM                                                    
*                                                                               
VALEIDOK DS    0H                                                               
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
*                                                                               
VALEIDX  MVC   FVMSGNO,=AL2(FVFNOTV)       INVALID INPUT                        
         B     XIT                                                              
         DROP  RF                                                               
         EJECT ,                                                                
***********************************************************************         
*  VERIFY SUB DESCRIPTION AND ADD ELEMENT                                       
***********************************************************************         
VALESUB  ST    RE,SVRE                                                          
         GOTO1 AFVAL,DWNESUBH         SUB LINE                                  
         JE    VALESUB1               ERROR                                     
         SR    R1,R1                  Branch Low                                
         CLC   =C'GENERIC',EDICTKEY   SUB LINE unnecessary if GENERIC           
         JNE   VALESUBX                                                         
         AHI   R1,1                                                             
         J     VALESUBX                                                         
                                                                                
         USING FFTELD,RE                                                        
VALESUB1 LA    RE,APELEM                                                        
         MVC   SVELEM,APELEM                                                    
         XC    APELEM,APELEM                                                    
         MVI   FFTEL,FFTELQ        X'DB'                                        
         MVI   FFTTYPE,FFTTESUB                                                 
         MVC   FFTDLEN,FVILEN                                                   
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EXMVC RF,FFTDATA,FVIFLD                                                
         AHI   RF,(FFTDATA-FFTELD)+1                                            
         STC   RF,FFTLN                                                         
         DROP  RE                                                               
                                                                                
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD    ELEMENT                               
         LHI   R1,1                Branch Equal                                 
         JE    *+8                 ON     ERROR, EXIT                           
         LHI   R1,2                Branch High                                  
         MVC   APELEM,SVELEM                                                    
                                                                                
VALESUBX CHI   R1,1                                                             
         L     RE,SVRE                                                          
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE FILE NAME AND ADD ELEMENT                                           
***********************************************************************         
VALEFIL  ST    RE,SVRE                                                          
         GOTO1 AFVAL,DWNEFILH      FILE NAME                                    
         JE    VALEFIL1            Yes                                          
         SR    R1,R1               No input                                     
         J     VALEFILX                                                         
                                                                                
VALEFIL1 SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
*&&UK                                                                           
         CLC   =CL3'USS',DWNEDIC   TEST USS TRANSMISSION TYPE                   
         JE    VALEFIL2            YES - DON'T VALIDATE AS IT'S SFTP            
*                                  For BDE:                                     
         CHI   RF,80-24            Max=L'NAME-L'USERID.YYMMDD.HHMMSS            
         JH    VALEFILN                                                         
*&&                                                                             
         GOTOR CKFILNM,APPARM,FVIFLD,(RF)                                       
         BNE   VALEFILN                                                         
                                                                                
         USING FFTELD,RE                                                        
VALEFIL2 LA    RE,APELEM                                                        
         MVC   SVELEM,APELEM                                                    
         XC    APELEM,APELEM                                                    
         MVI   FFTEL,FFTELQ        X'DB'                                        
         MVI   FFTTYPE,FFTTEFIL                                                 
         MVC   FFTDLEN,FVILEN                                                   
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EXMVC RF,FFTDATA,FVIFLD                                                
         AHI   RF,(FFTDATA-FFTELD)+1                                            
         STC   RF,FFTLN                                                         
         CLI   MACRO,YES                                                        
         BNE   *+8                                                              
         OI    FFTSEQ,X'80'        Indicate marco in use                        
         DROP  RE                                                               
                                                                                
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD    ELEMENT                               
         BNE   VALEFILN            ON     ERROR, EXIT                           
         LHI   R1,1                Set okay                                     
         MVC   APELEM,SVELEM       RESTORE                                      
         B     VALEFILX                                                         
                                                                                
VALEFILN LHI   R1,2                High for invalid                             
VALEFILX CHI   R1,1                Input okay if 1                              
         L     RE,SVRE                                                          
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE EXTENSIONS                                                *         
***********************************************************************         
VALEEXT  NTR1                                                                   
         GOTO1 AFVAL,DWNEEXTH      FILE EXTENSION                               
         JNE   VALEETX                                                          
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTOR CKFILNM,APPARM,FVIFLD,(RF)                                       
         BNE   VALEETX                                                          
                                                                                
         USING RPFELD,R9                                                        
         TM    RPFXMIT,RPFXACNT+RPFXQREP   ACCENT OR QREPORT ?                  
         BZ    VALEET40                                                         
                                                                                
         USING EXTTABD,R2                                                       
         LA    RF,EXTTABQ         EXT. FOR ACCENT/QREPORT                       
         L     R2,=A(EXTTAB)                                                    
         A     R2,APRELO                                                        
VALEET10 CLC   APREPJCL,EXTREPTY                                                
         JNE   VALEET20                                                         
         CLC   EXTNAME,FVIFLD                                                   
         JNE   VALEET20                                                         
         MVC   BYTE,RPFXMIT                                                     
         NI    BYTE,RPFXACNT+RPFXQREP                                           
         NC    BYTE,EXTXMIT       DO WE SUPPORT THIS XMIT TYPE ?                
         JNZ   VALEET40           YES - OK                                      
         J     VALEET25           NO - ERROR                                    
*                                                                               
VALEET20 AHI   R2,EXTTLNQ                                                       
         BCT   RF,VALEET10                                                      
VALEET25 MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         J     VALEETX                                                          
                                                                                
         USING FFTELD,RE                                                        
VALEET40 LA    RE,APELEM                                                        
         MVC   SVELEM,APELEM                                                    
         XC    APELEM,APELEM                                                    
         MVI   FFTEL,FFTELQ        X'DB'                                        
         MVI   FFTLN,FFTDATA-FFTELD+3                                           
         MVI   FFTTYPE,FFTTEEXT    94                                           
         TM    RPFXMIT,RPFXACNT+RPFXQREP                                        
         BZ    VALEET42                                                         
         MVI   FFTDLEN,L'EXTNAME           EXTENSION LENGTH                     
         MVC   FFTSEQ,EXTACTNO             ACTION NUMBER                        
         MVC   FFTDATA(L'EXTNAME),EXTNAME  EXTENSION VALUE                      
         B     VALEET50                                                         
         DROP  R2,R9                                                            
                                                                                
VALEET42 MVC   FFTDLEN,FVILEN                                                   
         MVI   FFTSEQ,0                                                         
         ZIC   RF,FVXLEN                                                        
         EXMVC RF,FFTDATA,FVIFLD                                                
         LA    RF,FFTDATA-FFTELD+1(RF)                                          
         STC   RF,FFTLN                                                         
         DROP  RE                                                               
                                                                                
VALEET50 L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)          ADD    ELEMENT                               
         BE    *+6                 ON     ERROR, EXIT                           
         DC    H'00'                                                            
                                                                                
         MVC   APELEM,SVELEM       RESTORE                                      
                                                                                
VALEETX  CLC   FVMSGNO,=AL2(FVFOK)     SET CC                                   
         J     XIT                                                              
*                                            FOR ACCENT OR QUICK REPS           
EXTTAB   DC    AL1(REPJCL1,ACACTUTL,EXTACNT+EXTQREP),CL3'UTL'                   
EXTTLNQ  EQU   *-EXTTAB                                                         
         DC    AL1(REPJCL1,ACACTTMS,EXTACNT+EXTQREP),CL3'TMS'                   
         DC    AL1(REPJCLV,ACACTJBS,EXTACNT+EXTQREP),CL3'JBS'                   
         DC    AL1(REPJCLG,ACACTFPL,EXTACNT),CL3'FPL'                           
         DC    AL1(REPJCLR,ACACTDEB,EXTQREP),CL3'DEB'                           
         DC    AL1(REPJCLP,ACACTCRD,EXTQREP),CL3'CRD'                           
         DC    AL1(REPJCLX,ACACTEXP,EXTQREP),CL3'EXP'                           
*&&US*&& DC    AL1(REPJCLX,ACACTEXP,EXTACNT+EXTQREP),CL3'EXP'                   
*&&US*&& DC    AL1(REPJCLX,ACACEXP2,EXTQREP),CL3'EXP'                           
EXTTABQ  EQU   (*-EXTTAB)/EXTTLNQ                                               
                                                                                
*-----------------------------------------------------*                         
* ACCENT AND QUICK REPORT ACTION NUMBERS              *                         
*-----------------------------------------------------*                         
ACACTINC EQU   0                   NOT YET USED                                 
ACACTCPL EQU   1                   Clinet P&L                                   
ACACTUTL EQU   5                   Utilization                                  
ACACTTMS EQU   6                   Time sheets                                  
ACACTJBS EQU   12                  Job summary                                  
ACACTFPL EQU   19                  Financial P&L (General ledger)               
ACACEXP2 EQU   39                  Expense                                      
                                                                                
ACACTCRD EQU   244                 Creditors                                    
ACACTDEB EQU   247                 Debtors                                      
ACACTEXP EQU   250                 Expenses                                     
*CACTINC EQU   253                 Income   for later use                       
         EJECT ,                                                                
***********************************************************************         
*  CHECK FOR A VALID FILE NAME                                        *         
***********************************************************************         
CKFILNM  NTR1                                                                   
         MVI   MACRO,NO                                                         
         L     RE,0(,R1)           A(DATA)                                      
         LR    R5,RE                                                            
         LR    R6,RE                                                            
         L     RF,4(,R1)           LENGTH OF DATA                               
         AR    RE,RF                                                            
         BCTR  RE,0                A(END OF DATA)                               
                                                                                
CKFILNM2 CLI   0(RE),C' '          TRAILING BLANKS                              
         BH    CKFILNM3            FIRST CHARACTER FROM THE END                 
         BCTR  RE,0                                                             
         BCT   RF,CKFILNM2                                                      
         DC    H'00'               GOT TO HAVE AT LEAST ONE CHARACTER           
*                                                                               
CKFILNM3 SHI   RF,1                One for execute                              
         SR    R2,R2                                                            
                                                                                
CKFILNM5 DS    0H                                                               
         LH    R0,=AL2(FVFOK)      ALL IS OK                                    
         EX    RF,VFILTRT1                                                      
         BZ    CKFILXIT                                                         
         CHI   R2,3                                                             
         BH    CKFILNO                                                          
         SLL   R2,2                                                             
         B     *(R2)                                                            
         B     CK1STCHR                                                         
         B     CK1MACRO                                                         
         B     CK2MACRO                                                         
                                                                                
CK1STCHR LHI   R0,2228             1ST CHARACTER "A"-"Z" ONLY                   
         CR    R1,R6               Same location (1st char)                     
         BE    CKFILXIT                                                         
         AHI   R1,1                                                             
         LH    R0,=AL2(FVFOK)      Reset R0                                     
         B     CKFILNXT            Okay as is                                   
                                                                                
CK1MACRO AHI   R1,1                                                             
CK2MACRO CLI   0(R1),C'&&'         Is it a macro ?                              
         BNE   CKFILNXT            No so continue                               
         AHI   R1,1                                                             
         CLC   =CL8'DATETIME',0(R1)                                             
         BNE   CKFILNXT                                                         
         MVI   MACRO,YES                                                        
         AHI   R8,8                                                             
         B     CKFILNXT                                                         
                                                                                
CKFILNXT LR    R5,R1               R5 start of scan again                       
         LR    RF,RE               RE end of string                             
         SR    RF,R1               RF New length to scan for                    
         BP    CKFILNM5            Keep processing                              
         B     CKFILXIT            Okay then                                    
                                                                                
VFILTRT1 TRT   0(0,R5),VFILWDWS                                                 
                                                                                
CKFILNO  LHI   R0,2229                                                          
         MVC   FVXTRA(1),0(R1)     Show bad character                           
                                                                                
CKFILXIT STCM  R0,3,FVMSGNO                                                     
         CH    R0,=AL2(FVFOK)                                                   
         B     XIT                                                              
*                                                                               
* Valid windows file names characters I allow                                   
*                                                                               
*                   0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.                            
VFILWDWS DS    0H                                                               
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 00-0F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 10-1F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 20-2F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 30-3F                     
         DC    XL16'01FFFFFFFFFFFFFFFFFF00FFFF000203' 40-4F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFF0000FF000000' 50-5F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFF0000FFFF' 60-6F                     
         DC    XL16'FFFFFFFFFFFFFFFFFF00FF00000000FF' 70-7F                     
         DC    XL16'FF000000000000000000FFFFFFFFFFFF' 80-8F                     
         DC    XL16'FF000000000000000000FFFFFFFFFFFF' 90-9F                     
         DC    XL16'FF000000000000000000FFFFFFFFFFFF' A0-AF                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' B0-BF                     
         DC    XL16'00000000000000000000FFFFFFFFFFFF' C0-CF                     
         DC    XL16'00000000000000000000FFFFFFFFFFFF' D0-DF                     
         DC    XL16'FFFF0000000000000000FFFFFFFFFFFF' E0-EF                     
         DC    XL16'00000000000000000000FFFFFFFFFFFF' F0-FF                     
                                                                                
VFILMF   DS    0H                                                               
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 00-0F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 10-1F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 20-2F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 30-3F                     
         DC    XL16'01FFFFFFFFFFFFFFFFFF00FFFF000203' 40-4F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFF0000FF000000' 50-5F                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFF0000FFFF' 60-6F                     
         DC    XL16'FFFFFFFFFFFFFFFFFF00FF00000000FF' 70-7F                     
         DC    XL16'FF000000000000000000FFFFFFFFFFFF' 80-8F                     
         DC    XL16'FF000000000000000000FFFFFFFFFFFF' 90-9F                     
         DC    XL16'FF000000000000000000FFFFFFFFFFFF' A0-AF                     
         DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' B0-BF                     
         DC    XL16'00000000000000000000FFFFFFFFFFFF' C0-CF                     
         DC    XL16'00000000000000000000FFFFFFFFFFFF' D0-DF                     
         DC    XL16'FFFF0000000000000000FFFFFFFFFFFF' E0-EF                     
         DC    XL16'00000000000000000000FFFFFFFFFFFF' F0-FF                     
         EJECT ,                                                                
***********************************************************************         
*  ERROR TO DISPLAY ON TOP OF SCREEN                                  *         
***********************************************************************         
         SPACE 1                                                                
IVALWDTH MVC   FVMSGNO,=AL2(ACERTWD)       TOO WIDE                             
         LA    R1,DWNX165H                                                      
         ST    R1,FVADDR                                                        
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALEKEY MVI   STSEQ,1                                                          
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALINOD MVC   FVMSGNO,=AL2(FVFNONE)       NO DATA INPUT                        
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALIPUT MVC   FVMSGNO,=AL2(FVFNOTV)       INVALID INPUT                        
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALOPTM MVC   FVMSGNO,=AL2(1454)          MUST HAVE $ & FIX TEXT LEN           
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALROCO MVC   FVMSGNO,=AL2(ACEQR20)       MORE THAN 20 ROWS AND COLS           
         B     IVALEXIT                                                         
         SPACE 1                                                                
IVALACNT LA    RE,DWNCODEH                 FORMAT CODE FIELD                    
         ST    RE,FVADDR                                                        
         MVC   FVMSGNO,=AL2(ACEACNAL)      ACCENT NOT ALLOWED                   
         B     IVALEXIT                                                         
         SPACE 1                                                                
*&&UK                                                                           
IVALMSSF LA    RE,DWNMSSFH                 SEND TO MS SERVER FIELD              
         ST    RE,FVADDR                                                        
         MVC   FVMSGNO,=AL2(ACEIMSSF)      VALID FOR TYPE=EDIHUB/USS            
         B     IVALEXIT                                                         
         SPACE 2                                                                
*&&                                                                             
IVALEXIT DS    0H                                                               
         XC    APCURSOR,APCURSOR           CLEAR APPLICATION CURSOR             
         B     EXIT                                                             
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
DATETAB  DC    AL1(LANGEUS,00),CL10'MMMDD/YY'     DATCON  8 ( 9)                
         DC    AL1(LANGEUS,01),CL10'YYMMDD'               0                     
         DC    AL1(LANGEUS,20),CL10'YYYYMMDD'            20                     
         DC    AL1(LANGEUS,21),CL10'MMMDD/YYYY'          21                     
         DC    AL1(LANGEUS,23),CL10'YYYY-MM-DD'          23                     
         DC    AL1(LANGEUS,10),CL10'MM/DD/YY'            10                     
         DC    AL1(LANGEUS,30),CL10'DDMMMYY'           NONE                     
         DC    AL1(LANGEUS,31),CL10'MM/DD/YYYY'        NONE                     
         DC    AL1(LANGEUS,32),CL10'DDMMYYYY'          NONE                     
         DC    AL1(LANGEUS,33),CL10'MMDDYY'            NONE                     
         DC    AL1(LANGEUS,34),CL10'MMDDYYYY'          NONE                     
*                                                                               
         DC    AL1(LANGEUK,00),CL10'DDMMMYY'      DATCON  8                     
         DC    AL1(LANGEUK,01),CL10'YYMMDD'               0                     
         DC    AL1(LANGEUK,20),CL10'YYYYMMDD'            20                     
         DC    AL1(LANGEUK,21),CL10'DDMMMYYYY'           21                     
         DC    AL1(LANGEUK,23),CL10'YYYY-MM-DD'          23                     
         DC    AL1(LANGEUK,10),CL10'DD/MM/YY'            10                     
         DC    AL1(LANGEUK,11),CL10'MMMDD/YY'            11                     
         DC    AL1(LANGEUK,05),CL10'DD MMM YY'            5                     
         DC    AL1(LANGEUK,13),CL10'DD.MM.YY'            13                     
*                                                                               
         DC    AL1(LANGGER,00),CL10'DD.MM.YY'     DATCON  8                     
         DC    AL1(LANGGER,01),CL10'YYMMDD'               0                     
         DC    AL1(LANGGER,20),CL10'YYYYMMDD'            20                     
         DC    AL1(LANGGER,21),CL10'DD.MM.YYYY'          21                     
         DC    AL1(LANGGER,23),CL10'YYYY-MM-DD'          23                     
         DC    AL1(LANGGER,10),CL10'DD/MM/YY'            10                     
         DC    AL1(LANGGER,11),CL10'MMMDD/YY'            11                     
         DC    X'FF'                                                            
         EJECT ,                                                                
SCRTXT   DS    0F                                                               
         DC    AL4(DWNP020H-TWAD,1730)   BETWEEN FIELD DELIMITER                
         DC    AL4(DWNP003H-TWAD,1630)   AMOUNTS IN TEXT FORMAT                 
         DC    AL4(DWNP030H-TWAD,1731)   TEXT DELIMITER                         
         DC    AL4(DWNP004H-TWAD,1631)   PAD AMOUNTS W/LEADING ZEROS            
         DC    AL4(DWNP040H-TWAD,1732)   END OF  LINE DELIMITER                 
         DC    AL4(DWNP005H-TWAD,1632)   FIXED WIDTH FIELDS                     
         DC    AL4(DWNP060H-TWAD,1638)   LEFT JUSTIFY AMOUNT FIELDS             
*        DC    AL4(DWNP061H-TWAD,1679)   INCLUDE REQUEST PAGE                   
*&&UK*&& DC    AL4(DWNP061H-TWAD,5900)   FOR FUTURE USE                         
         DC    AL4(DWNP062H-TWAD,1699)   EDI ID                                 
*&&UK*&& DC    AL4(DWNP063H-TWAD,1714)   SEND REPORT TO SFTP SERVERS            
         DC    AL4(DWNP050H-TWAD,1733)   END OF REPORT DELIMITER                
         DC    AL4(DWNP006H-TWAD,1633)   FORMAT CAN EXCEED 198 CHARS            
         DC    AL4(DWNP007H-TWAD,1634)   INCLUDE COLUMN HEADINGS                
         DC    AL4(DWNP008H-TWAD,1635)   INCLUDE ROWS                           
         DC    AL4(DWNP009H-TWAD,1636)   INCLUDE TOTALS                         
         DC    AL4(DWNP010H-TWAD,1621)   # OF ADDRESS LINES TO DOWNLOAD         
         DC    AL4(DWNP011H-TWAD,1637)   TRANSMISSION FORMAT                    
         DC    AL4(DWNP012H-TWAD,1622)   SUB :                                  
         DC    AL4(DWNP013H-TWAD,1623)   FILE:                                  
         DC    AL4(DWNP070H-TWAD,1629)   EXT :                                  
         DC    X'FF'                                                            
         EJECT ,                                                                
*              DSECT FOR LOCAL WORKING STORAGE                                  
         SPACE 1                                                                
LWSD     DSECT                                                                  
MAXPARM  EQU   20                                                               
CURWIDTH DS    XL1                 CURRENT REPORT WIDTH                         
DISPNEL  DS    XL1                 NUMBER OF ELEMENTS DISPLAYED                 
ERRORMSG DS    CL60                                                             
SVKEY    DS    CL(L'IOKEY)         SAVE IOKEY                                   
SVREG    DS    A                   SAVE A SINGLE REGISTER                       
SVRE     DS    A                   SAVE RE (RETURN ADDRESS)                     
SVREGS   DS    6A                  SAVE SOME REGISTERS                          
LWPARM   DS    6F                                                               
A1STNTRY DS    A                   1ST ENTRY IN TABLE FOR LANG                  
MACRO    DS    C                   Yes/No Macro in use                          
SVLEN    DS    AL1                 SAVE LENGTH OF TEXT                          
BYTE     DS    XL1                                                              
CURLANG  DS    AL1                                                              
SAVDNOPT DS    XL1                 SAVE RPFDNOPT                                
AC@BLANK DS    CL5                                                              
AC@NONE  DS    CL5                                                              
AC@TAB   DS    CL5                                                              
EDICTKEY DS    CL8                 EDICT KEY NAME                               
NROWS    DS    XL1                 NUMBER OF ROWS                               
NCOLS    DS    XL1                 NUMBER OF COLUMNS                            
*                                                                               
*&&UK                                                                           
XTALLOW  DS    CL1                 YES/NO ACCENT/QREPORT ALLOWED                
*&&                                                                             
DWNTYPE  DS    XL1                                                              
DWNTACNT EQU   X'80'               ACCENT ENABLED                               
DWNTQREP EQU   X'40'               QUICK REPORTING ENABLED                      
*                                                                               
SCEDIFLD DS    C                   SECURITY FOR EDI FIELD                       
SVELEM   DS    XL256                                                            
LWSX     DS    0C                                                               
         EJECT ,                                                                
DTFORMD  DSECT                                                                  
DTFLANG  DS    AL1                                                              
DTFCODE  DS    AL1                                                              
DTFNAME  DS    CL10                                                             
DTFLNQ   EQU   *-DTFORMD                                                        
         SPACE 2                                                                
EXTTABD  DSECT                                                                  
EXTREPTY DS    AL1                 REPORT TYPE                                  
EXTACTNO DS    AL1                 ACTION NUMBER                                
EXTXMIT  DS    AL1                 TRANSMISSION TYPE                            
EXTACNT  EQU   RPFXACNT            ACCENT                                       
EXTQREP  EQU   RPFXQREP            QREPORT                                      
EXTNAME  DS    CL3                 EXTENSION NAME                               
EXTLNQ   EQU   *-EXTTABD                                                        
         EJECT ,                                                                
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*CTGENEDICT                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENEDICT                                                     
         PRINT ON                                                               
*ACSCRWRK                                                                       
       ++INCLUDE ACSCRWRK                                                       
         EJECT ,                                                                
TWAD     DSECT                                                                  
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCRE9D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'115ACSCR13   08/13/19'                                      
         END                                                                    
