*          DATA SET CTGEN23X   AT LEVEL 012 AS OF 08/22/00                      
*          DATA SET CTGEN23    AT LEVEL 010 AS OF 19/04/95                      
*PHASE TA0B23A                                                                  
*INCLUDE GETBOOK                                                                
*INCLUDE SCRAMBLE                                                               
*INCLUDE SQUASHER                                                               
         SPACE 1                                                                
         TITLE 'CTGEN23 - File Maintenance - Book Editor'                       
GEN23    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*GEN23**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    R7,AR7                                                           
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
         CLI   APPFKEY,PFK12       Fudge for copy                               
         BNH   XX1                                                              
         XR    RF,RF                                                            
         IC    RF,APPFKEY                                                       
         SH    RF,=Y(PFK12)                                                     
         STC   RF,APPFKEY                                                       
         SPACE 1                                                                
XX1      ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         SPACE 1                                                                
         B     VALKEY              VALIDATE KEY                                 
         B     VALREC              VALIDATE RECORD                              
         B     DISKEY              DISPLAY KEY                                  
         B     DISREC              DISPLAY RECORD                               
         B     DELREC              DELETE RECORD                                
         B     RESREC              RESTORE RECORD                               
         B     VALSEL              VALIDATE LIST/SELECT PARAMETERS              
         B     GETSEL              GET NEXT LIST/SELECT RECORD                  
         B     DISSEL              DISPLAY LIST/SELECT RECORD                   
         B     EXIT                VALIDATE LIST/SELECT DATA                    
         B     EXIT                LIST/SELECT FIRST TIME MODE                  
         B     EXIT                PROCESS LIST SELECT ACTION                   
         B     EXIT                FIRST TIME FOR LIST/SELECT SCREEN            
         B     LSTSCR              LAST TIME FOR LIST/SELECT SCREEN             
         B     VALREQ              VALIDATE REQUEST DETAILS                     
         B     PRTREP              PRINT THE REPORT                             
         B     EXIT                NEW SCREEN LOADED - SET SCREEN DATA          
         B     EXIT                PUT KEY FIELDS USING APUTKEY                 
         B     VALREC1             COPY/RENAME RECORD WITH NEW KEY              
         B     EXIT                FIRST SCREEN OF DISPLAY                      
         B     EXIT                DISPLAY LIST/SELECT RECORD AFTER LFM         
         B     EXIT                INITIALIZE FOR DRIVER/DROOL                  
         B     EXIT                INPUT FOR DRIVER/DROOL                       
         B     EXIT                OUTPUT FOR DRIVER/DROOL                      
         B     EXIT                HOOK FOR DRIVER/DROOL                        
         B     EXIT                LIST SCREEN DISPLAY COMPLETE                 
         SPACE 1                                                                
EXITL    CLI   *,X'FF'            Set CC low                                    
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* Routine to validate key of record                                   *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   EQU   *                                                                
         LA    R2,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         MVC   APWORK,SPACES       Prefill APWORK with spaces                   
         CLI   APACTN,ACTREP                                                    
         BE    *+12                                                             
         LA    R3,SCRTYPEH                                                      
         B     *+8                                                              
         LA    R3,REPTYPH                                                       
         ST    R3,APCURSOR                                                      
         MVC   FVMSGNO,=AL2(FVFNOTV) Field Not Valid                            
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,(R3)          Record type required - Valid types           
         BNE   VALKEYX             are J/L/S/O at present                       
         LA    R4,TYPTAB                                                        
         USING TYPTABD,R4                                                       
         SPACE 1                                                                
VALKEY05 CLI   TYPTYPE,X'FF'       End of table                                 
         BNE   *+8                                                              
         B     VALKEYX                                                          
         SPACE 1                                                                
VALKEY10 CLC   TYPTYPE,FVIFLD      Record type                                  
         BE    *+12                                                             
         LA    R4,TYPTABLQ(R4)                                                  
         B     VALKEY05                                                         
         SPACE 1                                                                
         ICM   R4,15,TYPADDR       Validation address                           
         A     R4,APRELO                                                        
         BR    R4                                                               
         DROP  R4                                                               
         SPACE 1                                                                
VALKEY20 EQU   *                   Script source record                         
         USING CTLREC,R2                                                        
         MVI   CTLKTYP,CTLKTYPQ                                                 
         MVI   CTLKSCR,CTLKSCRQ                                                 
         B     VALKEY55                                                         
         SPACE 1                                                                
VALKEY30 EQU   *                   Library book record                          
         MVI   CTLKTYP,CTLKTYPQ                                                 
         B     VALKEY55                                                         
         DROP  R2                                                               
         SPACE 1                                                                
VALKEY40 EQU   *                   JCL Book record                              
         USING CTJREC,R2                                                        
         MVI   CTJKTYP,CTJKTYPQ                                                 
*&&UK*&& B     VALKEY55            UK SYSTEM IS OK                              
*                                                                               
         CLI   APACTN,ACTDIS       DISPLAY IS OK                                
         BE    VALKEY55                                                         
         CLI   APACTN,ACTREP       REPORT IS OK                                 
         BE    VALKEY55                                                         
         CLI   APACTN,ACTLST       LIST IS OK                                   
         BE    VALKEY55                                                         
         GOTO1 VGETFACT,APPARM,0   TEST SYSTEM IS OK                            
         L     R1,0(R1)                                                         
         CLI   FASYSID-FACTSD(R1),1                                             
         BE    VALKEY55                                                         
         TM    CUSTAT,CUSPER       MUST BE PERSONAL PASSWORD                    
         BZ    VALKEYX                                                          
         CLC   CUAALF,=C'**'       FOR AGENCY **                                
         BNE   VALKEYX                                                          
         B     VALKEY55                                                         
         DROP  R2                                                               
         SPACE 1                                                                
VALKEY50 EQU   *                   Xtract Record                                
         USING CTLREC,R2                                                        
         MVI   CTLKTYP,CTLKTYPQ                                                 
         MVI   CTLKSCR,CTLKXTRQ                                                 
         B     VALKEY55                                                         
         DROP  R2                                                               
         SPACE 1                                                                
VALKEY55 MVI   FVMINL,1                                                         
         MVC   FVMSGNO,=AL2(FVFNOTV) Field Not Valid                            
         CLI   APACTN,ACTREP                                                    
         BE    *+12                                                             
         LA    R3,SCRNAMEH                                                      
         B     *+8                                                              
         LA    R3,REPNAMEH                                                      
         ST    R3,APCURSOR                                                      
         GOTO1 AFVAL,(R3)                                                       
         BNE   VALKEYX                                                          
         SPACE 1                                                                
         ZIC   RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),FVIFLD                                                 
         USING CTJREC,R2                                                        
         MVC   CTJKID,APWORK                                                    
         MVI   CTJKSUB,0                                                        
         MVC   APRECKEY(L'CTJKEY),CTJKEY Save this key                          
         DROP  R2                                                               
         SPACE 1                                                                
         LA    R1,IORDD+IOCONFIL+IO1                                            
         CLI   APACTN,ACTDIS       Display only?                                
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)       No - Read for Update then                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O Error exit here                          
         BNE   *+12                Record not found?                            
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VALKEYY                                                          
         TM    IOERR,IOEDEL        Test Record Deleted                          
         BNZ   *+12                                                             
         MVI   APINDS,APIOKADD     Not Deleted, so no record exists             
         B     VALKEYY                                                          
         MVI   APINDS,APIOKDIS+APIOKRES Deleted record found                    
         SPACE 1                                                                
VALKEYY  MVC   FVMSGNO,=AL2(FVFOK) OK exit point here                           
         XC    APCURSOR,APCURSOR                                                
         SPACE 1                                                                
         LA    R4,SCRACT1H         First text line                              
         USING SHOWSCRD,R4                                                      
         LA    R0,MAXTXTNO                                                      
         USING CTLREC,R2                                                        
VALKEYX1 CLI   CTLKSCR,CTLKXTRQ                                                 
         DROP  R2                                                               
         BNE   *+12                                                             
         OI    SHOWTXTH+(FLDATB-FLDHDRD),FATBLC                                 
         B     *+8                                                              
         NI    SHOWTXTH+(FLDATB-FLDHDRD),255-FATBLC                             
         AH    R4,TXTLEN           Bump to next screen line                     
         BCT   R0,VALKEYX1                                                      
         SPACE 1                                                                
VALKEYX  XR    RE,RE               Display line at bottom of screen             
         TM    TWAMODE,TWAMLSM                                                  
         BZ    *+8                                                              
         LA    RE,3(RE)                                                         
         CLI   APACTN,ACTCHA       Order is Dis/Cha/SCha/LDis/LCha/             
         BNE   *+20                LSCha                                        
         LA    RE,1(RE)                                                         
         CLI   SCRTYPE,C'S'                                                     
         BNE   *+8                                                              
         LA    RE,1(RE)                                                         
         MH    RE,=H'78'                                                        
         LA    RE,MSGLINS(RE)                                                   
         MVC   SCRLSTE,0(RE)                                                    
         SPACE 1                                                                
         LA    RF,SCRLSTEH                                                      
         USING FLDHDRD,RF                                                       
         LA    R0,L'SCRLSTE                                                     
         STC   R0,FLDILEN                                                       
         STC   R0,FLDOLEN                                                       
         OI    FLDIIND,FINPTHIS                                                 
         OI    FLDOIND,FOUTTRN                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Routine to add or change a book record                                        
***********************************************************************         
         SPACE 1                                                                
VALREC1  MVI   APPFKEY,PFK03       Fudge for copy                               
         SPACE 1                                                                
VALREC   EQU   *                                                                
         CLI   SCRSHOW,C'N'        Have we displayed previously?                
         BE    VREC001             Yes                                          
         SPACE 1                                                                
         BAS   RE,VR1                                                           
         XC    APPFKEY,APPFKEY                                                  
         B     VREC001                                                          
VR1      NTR1                      Cheat to display the record                  
         B     DISREC              in order that TIA/TMPSTR set up ok           
         SPACE 1                                                                
VREC001  LA    R9,SAVAREA                                                       
         AH    R9,SAFETY                                                        
         USING POINTDS,R9                                                       
         SPACE 1                                                                
         XC    SCRIPT,SCRIPT                                                    
         CLI   APACTN,ACTADD       Add?                                         
         BNE   VREC002             No                                           
         CLI   EDFLAG,C'F'         First time in?                               
         BE    VREC002             No - Read TEMPSTR then                       
         SPACE 1                                                                
         XC    ACTV04,ACTV04       Active Elements                              
         XC    PASS04,PASS04       Passive Elements                             
         XC    BKHIVAL(ACTV04-BKHIVAL),BKHIVAL Highest used                     
         B     VREC003                                                          
         SPACE 1                                                                
VREC002  XC    APPARM(32),APPARM   Read in Tempstr                              
         LA    R8,X'02'            Tempstr page #                               
         SLL   R8,24                                                            
         LH    R3,=X'4800'         18K                                          
         ICM   R3,12,=C'L='                                                     
         GOTO1 VDMGR,APPARM,=C'DMREAD',=C'TEMPSTR',(R8),ATIA,0,(R3)             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
VREC003  CLI   APPFKEY,PFK04       Update Action                                
         BNE   VREC004                                                          
         CLI   SCRTYPE,C'S'        Only valid for scripts                       
         BNE   VREC004                                                          
         OC    DOLAST,DOLAST                                                    
         BNZ   VREC004                                                          
         SPACE 1                                                                
         BAS   RE,SAVEALL          Save everything to disk                      
         SPACE 1                                                                
         LA    RF,SCRNAMEH                                                      
         USING FLDHDRD,RF                                                       
         MVI   DOLAST,C'C'         Indicator for valreq                         
         MVC   SCRIPT,SCRNAME      Name of Script                               
         MVI   APMODE,APMSWP       Swap                                         
         MVI   APPARM,RECBOK       To BOOK/UPDATE                               
         MVI   APPARM+1,ACTUPD                                                  
         MVC   FVMSGNO,=AL2(FVFOK) Exit point here                              
         B     EXIT                                                             
         DROP  RF                                                               
         SPACE 1                                                                
VREC004  XC    DOLAST,DOLAST                                                    
         CLI   APPFKEY,PFK03       Only ever save record if PF3                 
         BNE   EDITOR              Otherwise go to Editor.                      
         BAS   RE,SAVEALL          Save all records                             
         MVC   FVMSGNO,=AL2(FVFOK) OK exit point here                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Save the Book currently being edited, & all the sub books with it   *         
***********************************************************************         
         SPACE 1                                                                
SAVEALL  NTR1                                                                   
         BAS   RE,EDITLAST         Register changes on screen for save          
         L     R8,ATIA             First go and build all small records         
         USING BIGDS,R8                                                         
         LA    R3,READLINE         Zeroth one - not used at present..           
         LA    R1,1                                                             
         ZIC   R4,BKHIVAL          Highest book used                            
         MH    R4,LINLENG                                                       
         LA    R4,0(R3,R4)         Last one we will need to check               
         AH    R3,LINLENG          First one                                    
         SPACE 1                                                                
VR020    CLC   1(72,R3),SPACES     Input this time to this line?                
         BE    *+8                 **NOTE** 72 =L'readline-1                    
         BAS   RE,VR_MINOR         Build record for this line                   
         SPACE 1                                                                
VR030    LA    R1,1(R1)            Bump sub #                                   
         AH    R3,LINLENG          Next line                                    
         CR    R3,R4               More records left to check?                  
         BH    VR040               All records read - write master              
         B     VR020               Next book line                               
         SPACE 1                                                                
VR040    EQU   *                   WRITE MASTER RECORD HERE                     
         LA    R2,IOKEY            ------------------------                     
         USING CTJREC,R2                                                        
         MVC   CTJKEY(L'CTJKEY),APRECKEY                                        
         LA    R3,APELEM                                                        
         SPACE 1                                                                
VR050    L     R2,AIOAREA1                                                      
         CLI   APACTN,ACTADD                                                    
         BE    VR060               Nothing to delete on an add                  
         SPACE 1                                                                
         USING CTINDD,R3           Delete Index                                 
         MVI   CTINDEL,CTINDELQ                                                 
         MVI   APELEM+1,0                                                       
         GOTO1 ADELELS,CTJREC                                                   
         DROP  R3                                                               
         SPACE 1                                                                
         USING CTACTD,R3           Delete Activity                              
         MVI   CTACTEL,CTACTELQ                                                 
         MVI   APELEM+1,0                                                       
         GOTO1 ADELELS,CTJREC                                                   
         DROP  R3                                                               
         SPACE 1                                                                
         USING CTDSCD,R3           Delete Description                           
         MVI   CTDSCEL,CTDSCELQ                                                 
         MVI   APELEM+1,0                                                       
         GOTO1 ADELELS,CTJREC                                                   
         DROP  R3                                                               
         SPACE 1                                                                
VR060    EQU   *                                                                
         CLI   APACTN,ACTCHA       No need to build key for Change              
         BE    VR061                                                            
         SPACE 1                                                                
         MVC   CTJKEY(L'CTJKEY),APRECKEY                                        
         MVI   CTJKSUB,0                                                        
         LA    R1,CTJDATA-CTJKEY+1 Build Key                                    
         STCM  R1,3,CTJLEN         Initial Length                               
         MVI   CTJDATA,0           EOR indicator                                
         SPACE 1                                                                
VR061    MVI   FVMINL,1                                                         
         GOTO1 AFVAL,SCRDESCH      Any Description?                             
         BNE   VR065               No input                                     
         SPACE 1                                                                
         XC    APELEM,APELEM                                                    
         USING CTDSCD,R3                                                        
         MVI   CTDSCEL,CTDSCELQ    Build Description Element                    
         ZIC   RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CTDSC(0),FVIFLD                                                  
         LA    RF,3(RF)                                                         
         STC   RF,CTDSCLEN                                                      
         GOTO1 AADDELS,CTJREC      Description Element                          
         SPACE 1                                                                
VR065    EQU   *                                                                
         GOTO1 ASETACT,CTJREC      Activity Element                             
         SPACE 1                                                                
VR070    LA    RF,PASS04           Passive elements                             
         XR    R1,R1                                                            
VR075    CLI   0(RF),0             Any Passives Still?                          
         BE    VR080               No                                           
         LA    RF,1(RF)                                                         
         LA    R1,1(R1)                                                         
         B     VR075                                                            
         SPACE 1                                                                
VR080    LTR   R1,R1               Any Passives at all?                         
         BZ    VR090               No can write OK.                             
         LA    RF,ACTV04           Active element                               
         CLI   0(RF),0             Find last active                             
         BE    *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
         SPACE 1                                                                
         LA    RF,1(RF)            Leave 1 blank as a sparator                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),PASS04      Move in unused passives                      
         SPACE 1                                                                
VR090    EQU   *                   Build 04 Element                             
         USING CTINDD,R3                                                        
         MVI   CTINDEL,CTINDELQ    Code                                         
         MVI   CTINDLEN,X'FE'      Fixed length element                         
         MVC   CTINDHI,BKHIVAL                                                  
         MVC   CTINDEX,ACTV04                                                   
         GOTO1 AADDELS,CTJREC      Add element                                  
         CLI   APACTN,ACTCHA                                                    
         BE    VR100                                                            
         SPACE 1                                                                
         GOTO1 AIO,IOADD+IOCONFIL+IO1 Add record                                
         BE    VR110                                                            
         DC    H'0'                Die if N.O.K                                 
         SPACE 1                                                                
VR100    GOTO1 AIO,IOWRITE+IOCONFIL+IO1 Write record                            
         BE    *+6                                                              
         DC    H'0'                Die if N.O.K                                 
         SPACE 1                                                                
VR110    MVC   FVMSGNO,=AL2(FVFOK) OK exit point here                           
         XC    EDFLAG,EDFLAG                                                    
         SPACE 1                                                                
VALRECX  B     EXIT                                                             
         DROP  R8,R9                                                            
         EJECT                                                                  
***********************************************************************         
* Routine that writes sub-records                                     *         
***********************************************************************         
         SPACE 1                                                                
VR_MINOR NTR1                                                                   
         LA    R2,IOKEY            Key built here                               
         USING CTLKEY,R2                                                        
         MVC   IOKEY(L'CTLKEY),APRECKEY Get master key                          
         STC   R1,CTLKSUB          Move sub # into master key                   
         STC   R1,APHALF           Move sub # into holding area                 
         SPACE 1                                                                
         L     R2,AIOAREA2         Use this IOAREA for sub-books                
         LA    R4,APELEM                                                        
         USING CTDATD,R4                                                        
         GOTO1 AIO,IORDUPD+IOCONFIL+IO2 Go and try to read a record             
         BNL   *+6                 I/O Error exit here                          
         DC    H'0'                                                             
         BE    VR_03                                                            
         TM    IOERR,IOEDEL        Test Record Deleted                          
         BO    VR_03               Not deleted, so no record exists             
         MVI   APBYTE,C'X'         Flag to add record                           
         SPACE 1                                                                
VR_03    MVC   CTLKEY(L'CTLKEY),IOKEY Build initial key                         
         LA    R1,CTLDATA-CTLKEY+1                                              
         STCM  R1,3,CTLLEN         Poke length into this                        
         MVI   CTLDATA,0           Bung a 0 at the end                          
         MVI   CTLSTAT,0           Bung a 0 in status byte                      
         MVC   CTLKSUB,APHALF      Get back the sub #                           
         MVC   IOKEY(L'CTLKEY),CTLKEY Move it back                              
         LR    RE,R3               Get A(This line)                             
         CLC   =C'++INCLUDE',1(RE) Included book?                               
         BE    VR_10                                                            
         SPACE 1                                                                
         LH    RF,LINLENG          L' a line in memory                          
         BCTR  RF,0                                                             
         LA    RF,0(RE,RF)         A(Last Data on this line)                    
         LA    RE,1(RE)            A(First character)                           
         CLI   0(RF),C' '          Looking for 1st character on line            
         BNE   *+10                which is non-space (ie significant)          
         BCTR  RF,0                ..                                           
         B     *-10                ..                                           
         SR    RF,RE               Gives length to move into element            
         BNM   VR_04               Line has no data on it                       
         CLI   APBYTE,C'X'         ADD?                                         
         BE    VR_06               Yes - ignore it then                         
         LA    RF,0                Save a * then                                
         MVI   0(RE),C'*'          Pretend it`s a comment...                    
         SPACE 1                                                                
VR_04    XC    APELEM,APELEM                                                    
         EX    RF,*+8              Significant length of data                   
         B     *+10                                                             
         MVC   CTDATA(0),0(RE)                                                  
         LA    R1,CTDATA-CTDATEL+1(RF) Length of element                        
         STC   R1,CTDATLEN                                                      
         MVI   CTDATEL,CTDATELQ    Element code                                 
         GOTO1 AADDELS,CTLREC                                                   
         BE    *+6                 Error on add                                 
         DC    H'0'                                                             
         SPACE 1                                                                
VR_05    EQU   *                   Write record                                 
         CLI   APBYTE,C'X'                                                      
         BNE   *+12                                                             
         LA    R1,IOADD+IOCONFIL+IO2                                            
         B     *+8                                                              
         LA    R1,IOWRITE+IOCONFIL+IO2                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                Die if N.O.K                                 
         B     VR_06               Next record                                  
         SPACE 1                                                                
VR_06    XC    APBYTE,APBYTE       Clear flag                                   
         XIT1                                                                   
         SPACE 1                                                                
VR_10    XC    APELEM,APELEM                                                    
         LA    RE,11(RE)           Points to type                               
         USING CTINCD,R4                                                        
         CLI   0(RE),C'S'                                                       
         BNE   VR_101                                                           
         MVC   CTINCLUD(2),=C'LS'                                               
         B     VR_111                                                           
         SPACE 1                                                                
VR_101   CLI   0(RE),C'Q'                                                       
         BNE   VR_11                                                            
         MVC   CTINCLUD(2),=C'LQ'                                               
         B     VR_111                                                           
         SPACE 1                                                                
VR_11    MVC   CTINCLUD(1),0(RE)                                                
VR_111   LA    RF,26                                                            
         STC   RF,CTINCLEN                                                      
         MVI   CTINCEL,CTINCELQ                                                 
         LA    RE,2(RE)            Check here for , exit on error               
         MVC   CTINCLUD+14(10),0(RE)                                            
         GOTO1 AADDELS,CTLREC                                                   
         BE    *+6                 Error on add                                 
         DC    H'0'                                                             
         SPACE 1                                                                
         B     VR_05                                                            
         DROP  R2                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* Routine to display key of record                                    *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         USING CTJREC,R2                                                        
         MVC   APWORKX,SPACES      Preset to spaces                             
         CLC   =C'LS',CTJKEY       Special for Scripts                          
         BNE   *+12                                                             
         MVI   APWORK,C'S'                                                      
         B     DISKEY10                                                         
         CLC   =C'LQ',CTJKEY       Special for Xtracts                          
         BNE   *+12                                                             
         MVI   APWORK,C'Q'                                                      
         B     DISKEY10                                                         
         MVC   APWORK(1),CTJKEY                                                 
         SPACE 1                                                                
DISKEY10 GOTO1 DISPFLD,SCRTYPEH    Record Type                                  
         SPACE 1                                                                
         MVC   APWORK(L'CTJKID),CTJKID Move in name                             
         GOTO1 DISPFLD,SCRNAMEH                                                 
         SPACE 1                                                                
DISKEYX  B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Routine to Display Book Record                                      *         
***********************************************************************         
         SPACE 1                                                                
DISREC   EQU   *                                                                
         L     R8,ATIA             A(TIA) where record is built                 
         USING BIGDS,R8                                                         
         LA    R8,READLINE                                                      
         LA    R9,MAXMEM           L'TIA bits being used                        
         MH    R9,LINLENG                                                       
         LA    R2,0                From address is dummy                        
         LA    R3,C' '             Length 0/Pad with spaces                     
         SLL   R3,24                                                            
         MVCL  R8,R2               Clear TIA to ZERO                            
         L     R8,ATIA                                                          
         LA    R0,MAXMEM                                                        
         LA    R1,READLINE                                                      
         SPACE 1                                                                
DREC001  MVI   0(R1),0                                                          
         LA    R1,L'READLINE(R1)                                                
         BCT   R0,DREC001                                                       
         SPACE 1                                                                
         L     R2,AIOAREA1         A(This Record)                               
         LA    R9,SAVAREA                                                       
         AH    R9,SAFETY                                                        
         USING POINTDS,R9                                                       
         SPACE 1                                                                
         TWAXC SCRDESCH,SCRDESCH   Clear Description                            
         TWAXC SCRACT1H,SCRLSTD,PROT=Y Clear Data Portion of Screen             
         XC    APELEM,APELEM                                                    
         XC    APWORK,APWORK                                                    
         SPACE 1                                                                
         MVC   FVMSGNO,=AL2(FVFNOTV) Field Not Valid                            
         USING CTJREC,R2                                                        
         LA    R3,APELEM                                                        
         USING CTDSCD,R3                                                        
         MVI   CTDSCEL,CTDSCELQ    Code for Description element                 
         GOTO1 AGETELS,CTJREC                                                   
         ICM   R3,15,APPARM        Found one?                                   
         BZ    DREC010             No                                           
         ZIC   R1,CTDSCLEN                                                      
         SH    R1,=H'3'            CodeºLengthºMVC                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),CTDSC     Move in Description                          
         DROP  R3                                                               
         GOTO1 DISPFLD,SCRDESCH    Display Description                          
         SPACE 1                                                                
DREC010  LA    R3,APELEM                                                        
         USING CTINDD,R3                                                        
         MVI   CTINDEL,CTINDELQ    Get the index element                        
         GOTO1 AGETELS,CTJREC                                                   
         ICM   R3,15,APPARM                                                     
         BNZ   DREC01A                                                          
         TM    APINDS,APIOKADD                                                  
         BO    DISRECX                                                          
         DC    H'0'                                                             
         SPACE 1                                                                
DREC01A  XC    ACTV04,ACTV04       Clear Active and Passive elements            
         XC    PASS04,PASS04                                                    
         MVC   BKHIVAL,CTINDHI     Save highest book value                      
         MVC   ACTV04,CTINDEX      Move the Index entries in                    
         LA    R3,ACTV04                                                        
         SPACE 1                                                                
DREC011  CLI   0(R3),0             End of actives                               
         BE    *+12                                                             
         LA    R3,1(R3)            Bump along actives                           
         B     DREC011                                                          
         SPACE 1                                                                
         XR    R1,R1                                                            
         LA    R3,1(R3)            Bump to where passives will begin            
         ST    R3,APFULL           Save this address                            
         SPACE 1                                                                
DREC012  CLI   0(R3),0             Passive?                                     
         BE    DREC013                                                          
         LA    R1,1(R1)            Passive count in R1                          
         LA    R3,1(R3)            Bump to next location                        
         B     DREC012                                                          
         SPACE 1                                                                
DREC013  LTR   R1,R1               R1 holds anything?                           
         BZ    DREC020             No, no passives then                         
         L     R3,APFULL                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PASS04(0),0(R3)     Move in passives and save them               
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R3),0(R3)       Remove passives from active                  
         SPACE 1                                                                
DREC020  BAS   RE,SHIFTY           Deal with PFKEY Presses                      
*        BNE   EXITL                                                            
         SPACE 1                                                                
         L     R4,AIOAREA3                                                      
         MVC   0(25,R4),APRECKEY   Key for 1st time in                          
         SPACE 1                                                                
DREC030  XC    APPARM(L'APPARM),APPARM Clear my Parm List                       
         L     R1,SCAUTL           A(UTL)                                       
         USING UTLD,R1                                                          
         MVC   APPARM+12(1),TNUM   Terminal # required for UK                   
         ST    R4,APPARM                                                        
         DROP  R1                                                               
         SPACE 1                                                                
         MVI   FVMINL,1            Resolve nested books?                        
         GOTO1 AFVAL,SCRSHOWH                                                   
         BNE   DREC031             Default to no.                               
         CLI   APACTN,ACTCHA                                                    
         BE    DREC031                                                          
         CLI   FVIFLD,C'Y'                                                      
         BNE   DREC031                                                          
         MVI   APPARM,0            Yes 0(APPARM) must be 0                      
         MVI   SCRSHOW,C'Y'                                                     
         B     DREC032                                                          
         SPACE 1                                                                
DREC031  MVI   APPARM,X'FF'        No 0(APPARM) must be N.Z.                    
         MVI   SCRSHOW,C'N'                                                     
         SPACE 1                                                                
DREC032  OI    SCRSHOWH+6,X'80'    Build GETBOOK Parm list                      
         L     RF,=V(GETBOOK)                                                   
         A     RF,APRELO                                                        
         ST    RF,AGETBOOK                                                      
         LA    R1,APPARM                                                        
         LA    RF,APWORKX                                                       
         ST    RF,4(R1)                                                         
         MVI   4(R1),0                                                          
         L     RF,VDMGR                                                         
         ST    RF,8(R1)                                                         
         XC    APWORKX,APWORKX                                                  
         XC    LINCNT,LINCNT                                                    
         LA    R4,SCRACT1H         First text line                              
         USING SHOWSCRD,R4                                                      
         MVC   CCOUNT,=H'1'        First sequence # for memory cards            
         SPACE 1                                                                
DREC035  GOTO1 AGETBOOK,APPARM     DISPLAY LOOP STARTS HERE                     
         LA    R1,APPARM           ------------------------                     
         MVI   SEQFLAG,255         Set flag                                     
         TM    8(R1),X'80'         End of Book                                  
         BO    DREC050                                                          
         MVI   SEQFLAG,0           Clear flag                                   
         CLI   8(R1),0             Ok                                           
         BE    DREC040                                                          
         TM    8(R1),X'02'         Deleted - can ignore it                      
         BO    DREC035                                                          
*        B     DISRECX                                                          
         B     DREC050             temporary fix                                
         SPACE 1                                                                
DREC040  LH    RF,CCOUNT           Current sequence # (See Below)               
         BCTR  RF,0                -1 for position                              
         LA    R1,MAXMEM           Max lines in memory                          
         CR    RF,R1                                                            
         BH    DREC045             Not for memory...                            
         SPACE 1                                                                
         LA    R1,ACTV04           Active elements                              
         LA    RF,0(RF,R1)         Index to get sequence #                      
         ZIC   R1,0(RF)            ...                                          
         MH    R1,LINLENG          Multiply Seq # by L' line                    
         LA    RE,READLINE         A(First Line of text)                        
         AR    RE,R1               A(This Line)                                 
         MVC   1(72,RE),APWORKX    Lines saved in TIA                           
         SPACE 1                                                                
DREC045  MVC   APFULL,APWORK+72    Line Number saved for display later          
         LH    R1,CCOUNT           Bump and save                                
         LA    R1,1(R1)            ..                                           
         STH   R1,CCOUNT           Next sequence #                              
         BCTR  R1,0                                                             
         LH    RF,SEQLO            Compare #-1 with first for display           
         CR    RF,R1               Can we start to display yet?                 
         BH    DREC035             No                                           
         CLI   LINCNT,MAXTXTNO     Can more be displayed on screen              
         BE    DREC035             No, just load up memory then                 
         SPACE 1                                                                
         ZIC   R1,LINCNT           Bump and save                                
         LA    R1,1(R1)            ..                                           
         STC   R1,LINCNT           next line #                                  
         ZIC   R1,COLSTRT          Load Right shift from above                  
         LA    RE,80               L' a line in APWORKX                         
         SR    RE,R1               Amount to display in RE                      
         LA    R1,APWORK(R1)       Start position in R1                         
         XC    EDWORK,EDWORK                                                    
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),0(R1)     Move stuff to start of APWORK                
         LA    RE,APWORK+1(RE)     Start of Garbage                             
         LA    RF,APWORK+L'APWORKX                                              
         MVI   0(RE),0             Clear what`s after data                      
         CR    RE,RF                                                            
         LA    RE,1(RE)                                                         
         BL    *-10                                                             
         GOTO1 DISPFLD,SHOWTXTH    Display it                                   
         XC    APWORKX,APWORKX                                                  
         MVC   APWORK(4),APFULL    Current Line # (Saved earlier)               
         GOTO1 DISPFLD,SHOWLINH                                                 
         SPACE 1                                                                
         AH    R4,TXTLEN           Bump to next screen line                     
         XC    APWORKX,APWORKX     Clear W/S                                    
         B     DREC035             Get next book                                
         SPACE 1                                                                
DREC050  EQU   *                                                                
         LH    R1,CCOUNT                                                        
         BCTR  R1,0                                                             
         STH   R1,SEQLST           Last reached for shifty...                   
         MVC   FVMSGNO,=AL2(FVFOK) OK message                                   
         XC    APPARM(32),APPARM                                                
         LA    R8,X'02'            Tempstr page #2                              
         SLL   R8,24                                                            
         GOTO1 VDMGR,APPARM,=C'DMWRT',=C'TEMPSTR',(R8),ATIA                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         TM    TWAMODE,TWAMLSM                                                  
         BZ    DISRECX                                                          
         OI    TWALSCTL,TWALSHLD+TWALSRTN+TWALSHSL                              
         MVI   ACPFRET,PFK12       Supposed to make this the return key         
         MVI   ACPFXIT,PFK12                                                    
         SPACE 1                                                                
DISRECX  MVI   APMODE,APMPFKS      Enable PFKeys                                
         GOTO1 ADISACT,CTJREC      Display Activity                             
         CLI   APACTN,ACTCHA       Change?                                      
         BNE   EXIT                                                             
         LA    R1,SCRACT1H                                                      
         ST    R1,APCURSOR                                                      
         B     EXIT                                                             
         DROP  R8,R9                                                            
         EJECT                                                                  
***********************************************************************         
* Routine to Delete a Script Record                                   *         
***********************************************************************         
         SPACE 1                                                                
DELREC   L     R2,AIOAREA1                                                      
         OI    CTJSTAT,X'80'       Set Delete Flag on in Record                 
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                Die if N.O.K                                 
DELRECX  B     EXIT                That`s all folks...                          
         SPACE 1                                                                
***********************************************************************         
* Routine to restore a Deleted Script Record                          *         
***********************************************************************         
         SPACE 1                                                                
RESREC   L     R2,AIOAREA1                                                      
         NI    CTJSTAT,X'FF'-X'80' Turn off Delete Flag in Record               
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                Die if N.O.K                                 
RESRECX  B     EXIT                Easy life...                                 
         EJECT                                                                  
***********************************************************************         
* Routine to Validate Select Parameters                               *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   LA    R2,APRECKEY                                                      
         TM    APINDS,APILFLST     First time in?                               
         BZ    *+8                                                              
         OI    APINDS,APILFLST     Yes                                          
         MVC   FVMSGNO,=AL2(FVFNOTV) Field Not Valid                            
         LA    R1,LSTTYPEH                                                      
         ST    R1,APCURSOR                                                      
         LA    R3,TYPTAB                                                        
         USING TYPTABD,R3                                                       
         SPACE 1                                                                
VALSEL05 CLI   TYPTYPE,X'FF'       End of table                                 
         BE    VALSELX                                                          
         CLC   TYPTYPE,LSTTYPE     Record type                                  
         BE    *+12                                                             
         LA    R3,TYPTABLQ(R3)                                                  
         B     VALSEL05                                                         
         SPACE 1                                                                
         ICM   R3,15,TYPADDR1      Validation address                           
         A     R3,APRELO                                                        
         BR    R3                                                               
         DROP  R3                                                               
         SPACE 1                                                                
VALSEL20 EQU   *                   Script source record                         
         USING CTLREC,R2                                                        
         MVI   CTLKTYP,CTLKTYPQ                                                 
         MVI   CTLKSCR,CTLKSCRQ                                                 
         B     VALSEL55                                                         
         SPACE 1                                                                
VALSEL30 EQU   *                   Library book record                          
         MVI   CTLKTYP,CTLKTYPQ                                                 
         B     VALSEL55                                                         
         DROP  R2                                                               
         SPACE 1                                                                
VALSEL40 EQU   *                   JCL Book record                              
         USING CTJREC,R2                                                        
         MVI   CTJKTYP,CTJKTYPQ                                                 
         B     VALSEL55                                                         
         DROP  R2                                                               
         SPACE 1                                                                
VALSEL50 EQU   *                   Xtract Record                                
         USING CTLREC,R2                                                        
         MVI   CTLKTYP,CTLKTYPQ                                                 
         MVI   CTLKSCR,CTLKXTRQ                                                 
         B     VALSEL55                                                         
         DROP  R2                                                               
         SPACE 1                                                                
VALSEL55 MVI   FVMINL,1                                                         
         LA    RF,LSTNAMEH                                                      
         ST    RF,APCURSOR                                                      
         GOTO1 AFVAL,LSTNAMEH                                                   
         BNE   VALSELY                                                          
         SPACE 1                                                                
         MVC   APWORK,SPACES                                                    
         ZIC   RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),SCRNAME                                                
         USING CTJREC,R2                                                        
         MVC   CTJKID,APWORK                                                    
         MVC   IOKEY(L'CTJKEY),CTJKEY Save this key                             
         DROP  R2                                                               
         SPACE 1                                                                
VALSELY  MVC   FVMSGNO,=AL2(FVFOK)                                              
         LA    R0,LSTACT1H         A(Line 1) of List screen                     
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H                                                      
         SR    R1,R0               Length of List line                          
         STH   R1,APPARM+6                                                      
         XC    APCURSOR,APCURSOR                                                
         SPACE 1                                                                
VALSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Get next List/Select Record                                         *         
***********************************************************************         
         SPACE 1                                                                
GETSEL   LA    R2,IOKEY                                                         
         USING CTJREC,R2                                                        
         MVC   CTJKEY,APRECKEY     Get saved Key                                
         TM    APINDS,APILFLST     Test First Time In                           
         BZ    GETSEL2                                                          
         B     GETSEL6             Read High                                    
         SPACE 1                                                                
GETSEL2  TM    APINDS,APILRERD     Test Sequence Broken by User I/O             
         BZ    GETSEL4                                                          
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    GETSEL8                                                          
         B     GETSELN                                                          
         SPACE 1                                                                
GETSEL4  TM    APINDS,APILNSEQ     Test Read or ReadHI                          
         BO    GETSEL8                                                          
GETSEL6  LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
GETSEL8  LA    R1,IOCONFIL+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   GETSELN             EOF Exit                                     
         L     R2,AIOAREA1                                                      
         SPACE 1                                                                
         CLC   APRECKEY(2),0(R2)   Make sure it is same type                    
         BNE   GETSELN             No, then exit                                
         CLI   CTJKSUB,0           Only Display Masters                         
         BNE   GETSEL8                                                          
         SPACE 1                                                                
GETSELOK MVC   FVMSGNO,=AL2(FVFOK) OK Exit point                                
         MVC   APRECKEY(L'CTJKEY),CTJKEY Save Key                               
         B     GETSELX                                                          
         SPACE 1                                                                
GETSELN  MVI   APMODE,APMEOFS      Set EOF                                      
         SPACE 1                                                                
GETSELX  B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Display List/Select Line                                            *         
***********************************************************************         
         SPACE 1                                                                
DISSEL   L     R2,AIOAREA1                                                      
         USING CTJREC,R2                                                        
         ICM   R4,15,APPARM                                                     
         USING LISTLIND,R4                                                      
         SPACE 1                                                                
         MVC   LENAME,CTJKID       Name                                         
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING CTACTD,R3                                                        
         MVI   APELEM,CTACTELQ                                                  
         GOTO1 AGETELS,CTJREC                                                   
         ICM   R3,15,APPARM        R3=(Matching element)                        
         BZ    DISSELX                                                          
         SPACE 1                                                                
         ZIC   R1,CTACTDT+2        Day                                          
         EDIT  (R1),(2,LEACTVD),DUB=APDUB,WRK=APWORK,ZERO=NOBLANK               
         ZIC   R1,CTACTDT+1        Month                                        
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MTHTAB1(R1)                                                   
         MVC   LEACTVM,0(R1)                                                    
         ZIC   R1,CTACTDT          Year                                         
         EDIT  (R1),(2,LEACTVY),DUB=APDUB,WRK=APWORK,ZERO=NOBLANK               
         DROP  R3                                                               
         SPACE 1                                                                
         LA    R3,APELEM                                                        
         USING CTDSCD,R3                                                        
         MVI   APELEM,CTDSCELQ                                                  
         GOTO1 AGETELS,CTJREC                                                   
         ICM   R3,15,APPARM        R3=(Matching element)                        
         BZ    DISSEL5                                                          
         ZIC   R1,CTDSCLEN                                                      
         SH    R1,=H'3'                                                         
         CH    R1,=H'50'           Max chars of description to display          
         BL    *+8                                                              
         LA    R1,49                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LEDESC(0),CTDSC     Data of description element                  
         SPACE 1                                                                
DISSEL5  LA    R3,APELEM                                                        
         USING CTINDD,R3                                                        
         MVI   CTINDEL,CTINDELQ                                                 
         GOTO1 AGETELS,CTJREC                                                   
         ICM   R3,15,APPARM        R3=(Matching element)                        
         BZ    DISSELX                                                          
         LA    R3,CTINDEX                                                       
         DROP  R3                                                               
         SPACE 1                                                                
         XR    R1,R1                                                            
DISS010  CLI   0(R3),0             Last index                                   
         BE    DISS020                                                          
         LA    R1,1(R1)            Bump Count                                   
         CLI   R1,251              Count too big have to die here               
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R3,1(R3)            Next index                                   
         B     DISS010                                                          
         SPACE 1                                                                
DISS020  EDIT  (R1),(3,LELEN),DUB=APDUB,WRK=APWORK,ZERO=NOBLANK                 
         DROP  R2                                                               
         SPACE 1                                                                
DISSELX  B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* Routine to handle last for screen (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* Routine to validate report request screen                           *         
***********************************************************************         
         SPACE 1                                                                
VALREQ   EQU   *                                                                
         CLI   APACTN,ACTUPD       Update is Compile for SCRIPTS                
         BE    VALREQ1                                                          
         SPACE 1                                                                
         L     R9,AREP                                                          
         USING REPD,R9                                                          
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPREQH       Validate Requestor                           
         BNE   VALREQX                                                          
         MVC   INUSER,FVIFLD       Set Requestor                                
         SPACE 1                                                                
         GOTO1 AVALWHEN,REPWHENH   Validate when                                
         BNE   VALREQX                                                          
         SPACE 1                                                                
         GOTO1 AVALDEST,REPDESTH   Validate destination ID                      
         BNE   VALREQX                                                          
         SPACE 1                                                                
         LA    R2,IOKEY                                                         
         MVC   APWORK,SPACES       Prefill APWORK with spaces                   
         SPACE 1                                                                
         LA    R1,REPTYPH                                                       
         ST    R1,APCURSOR                                                      
         MVC   FVMSGNO,=AL2(FVFNOTV) Field Not Valid                            
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPTYPH       Record type required - Valid types           
         BNE   VALREQX             are J/L/S/O at present                       
         LA    R3,TYPTAB                                                        
         USING TYPTABD,R3                                                       
         SPACE 1                                                                
VALREQ05 CLI   TYPTYPE,X'FF'       End of table                                 
         BNE   VALREQ10                                                         
         B     VALREQX                                                          
         SPACE 1                                                                
VALREQ10 CLC   TYPTYPE,REPTYP      Record type                                  
         BE    *+12                                                             
         LA    R3,TYPTABLQ(R3)                                                  
         B     VALREQ05                                                         
         SPACE 1                                                                
         ICM   R3,15,TYPADDR       Validation address                           
         A     R3,APRELO                                                        
         BR    R3                                                               
         DROP  R3                                                               
         SPACE 1                                                                
VALREQ20 EQU   *                   Script source record                         
         USING CTLREC,R2                                                        
         MVI   CTLKTYP,CTLKTYPQ                                                 
         MVI   CTLKSCR,CTLKSCRQ                                                 
         B     VALREQ55                                                         
         SPACE 1                                                                
VALREQ30 EQU   *                   Library book record                          
         MVI   CTLKTYP,CTLKTYPQ                                                 
         B     VALREQ55                                                         
         DROP  R2                                                               
         SPACE 1                                                                
VALREQ40 EQU   *                   JCL Book record                              
         USING CTJREC,R2                                                        
         MVI   CTJKTYP,CTJKTYPQ                                                 
         B     VALREQ55                                                         
         DROP  R2                                                               
         SPACE 1                                                                
VALREQ50 EQU   *                   Xtract Record                                
         USING CTLREC,R2                                                        
         MVI   CTLKTYP,CTLKTYPQ                                                 
         MVI   CTLKSCR,CTLKXTRQ                                                 
         B     VALREQ55                                                         
         DROP  R2                                                               
         SPACE 1                                                                
VALREQ55 MVI   FVMINL,1                                                         
         MVC   FVMSGNO,=AL2(FVFNOTV) Field Not Valid                            
         GOTO1 AFVAL,REPNAMEH                                                   
         BNE   VALREQX                                                          
         ZIC   RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),REPNAME                                                
         USING CTJREC,R2                                                        
         MVC   CTJKID,APWORK                                                    
         MVC   APRECKEY(L'CTJKEY),CTJKEY Save this key                          
         DROP  R2                                                               
         SPACE 1                                                                
         GOTO1 AIO,IORDD+IOCONFIL+IO1                                           
         BL    VALREQX             I/O Error exit here                          
         SPACE 1                                                                
VALREQY  MVC   FVMSGNO,=AL2(FVFOK) OK exit point here                           
         XC    APCURSOR,APCURSOR                                                
         SPACE 1                                                                
VALREQX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Routine to validate request for script compile                      *         
***********************************************************************         
         SPACE 1                                                                
VALREQ1  EQU   *                                                                
         L     R9,AREP                                                          
         USING REPD,R9                                                          
         XC    REPFORM,REPFORM                                                  
         XC    REPCHAR,REPCHAR                                                  
         MVC   REPMAKER,=C'G25  '                                               
         MVI   INWIDTH,C'W'                                                     
         MVC   INUSER,=C'SCP'      SET REQUESTOR SCP                            
         MVI   INWHEN,MIXIOKN      SET NOW                                      
         SPACE 1                                                                
VREQ010  LA    R1,SAVAREA                                                       
         AH    R1,SAFETY                                                        
         USING POINTDS,R1                                                       
         CLI   DOLAST,C'C'        Was this from a PF04?                         
         BNE   VREQ020                                                          
         LA    RF,SCRTYPEH                                                      
         USING FLDHDRD,RF                                                       
         LA    R0,1                                                             
         STC   R0,FLDILEN                                                       
         STC   R0,FLDOLEN                                                       
         MVI   SCRTYPE,C'S'                                                     
         OI    FLDIIND,FINPTHIS                                                 
         OI    FLDOIND,FOUTTRN                                                  
         LA    RF,SCRNAMEH                                                      
         USING FLDHDRD,RF                                                       
         LA    R0,L'SCRNAME                                                     
         STC   R0,FLDILEN                                                       
         STC   R0,FLDOLEN                                                       
         OI    FLDIIND,FINPTHIS                                                 
         OI    FLDOIND,FOUTTRN                                                  
         MVC   SCRNAME,SCRIPT                                                   
         DROP  RF                                                               
         DROP  R1                                                               
         SPACE 1                                                                
VREQ020  LA    R2,IOKEY                                                         
         USING CTLREC,R2                                                        
         MVC   APWORK,SPACES       Prefill APWORK with spaces                   
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,SCRNAMEH      Validate name                                
         BNE   VALREQ1X                                                         
         MVC   REPDESC,FVIFLD                                                   
         SPACE 1                                                                
         MVI   CTLKTYP,CTLKTYPQ                                                 
         MVI   CTLKSCR,CTLKSCRQ                                                 
         MVC   CTLKNAME,FVIFLD                                                  
         MVI   CTLKSUB,0                                                        
         MVC   APRECKEY(L'CTLKEY),CTLKEY Save this key                          
         DROP  R2                                                               
         SPACE 1                                                                
         GOTO1 AIO,IORDD+IOCONFIL+IO1                                           
         BL    VALREQ1X            I/O ERROR EXIT HERE                          
         SPACE 1                                                                
VALREQ1Y MVC   FVMSGNO,=AL2(FVFOK) OK exit point here                           
         XC    APCURSOR,APCURSOR                                                
         SPACE 1                                                                
VALREQ1X B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Routine to Generate Report                                          *         
***********************************************************************         
         SPACE 1                                                                
PRTREP   CLI   APACTN,ACTUPD       Update is COMPILE for Scripts                
         BE    PRTREP1                                                          
         SPACE 1                                                                
         L     R9,AREP                                                          
         L     R2,AIOAREA1         A(This Record)                               
         SPACE 1                                                                
         MVC   FVMSGNO,=AL2(FVFNOTV) Field Not Valid                            
         USING CTJREC,R2                                                        
         LA    R3,APELEM                                                        
         USING CTDSCD,R3                                                        
         SPACE 1                                                                
         MVCDD REPP1(10),CT#TITLE                                               
         MVC   REPP1+15(L'CTJKID),CTJKID                                        
         GOTO1 VREPORT,REPD                                                     
         MVC   REPP1,SPACES                                                     
         GOTO1 VREPORT,REPD                                                     
         SPACE 1                                                                
         MVI   CTDSCEL,CTDSCELQ    Code for Description element                 
         GOTO1 AGETELS,CTJREC                                                   
         ICM   R3,15,APPARM        Found one?                                   
         BZ    PRTR010             No                                           
         MVCDD REPP1(15),CT#DESC                                                
         ZIC   R1,CTDSCLEN                                                      
         SH    R1,=H'3'            CodeºLengthºMVC                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   REPP1+16(0),CTDSC   Move in Description                          
         DROP  R3                                                               
         GOTO1 VREPORT,REPD                                                     
         MVC   REPP1,SPACES                                                     
         GOTO1 VREPORT,REPD                                                     
         SPACE 1                                                                
PRTR010  L     R4,AIOAREA3                                                      
         MVC   0(25,R4),APRECKEY   Key for 1st time in                          
         SPACE 1                                                                
         XC    APPARM(32),APPARM   Clear my Parm List                           
         L     R1,SCAUTL           A(UTL)                                       
         USING UTLD,R1                                                          
         MVC   APPARM+12(1),TNUM   Terminal # required for UK                   
         DROP  R1                                                               
         SPACE 1                                                                
         ST    R4,APPARM           A(Save AREA for GETBOOK)                     
         MVI   APPARM,0            Expand ++INCLUDES                            
         L     RF,=V(GETBOOK)                                                   
         A     RF,APRELO                                                        
         ST    RF,AGETBOOK         A(GETBOOK)                                   
         LA    R1,APPARM                                                        
         LA    RF,APWORKX          A(Card area)                                 
         ST    RF,4(R1)                                                         
         MVI   4(R1),0             Start seq # is 0                             
         L     RF,VDMGR            A(Data Manager)                              
         ST    RF,8(R1)                                                         
         XC    APWORKX,APWORKX                                                  
         SPACE 1                                                                
PRTR035  GOTO1 AGETBOOK,APPARM     Print Loop                                   
         LA    R1,APPARM                                                        
         TM    8(R1),X'80'         End of Book                                  
         BO    PRTRX                                                            
         CLI   8(R1),0             Ok                                           
         BE    PRTR040                                                          
         TM    8(R1),X'02'         Deleted - can ignore it                      
         BZ    PRTRX                                                            
         SPACE 1                                                                
PRTR040  MVC   REPP1(80),APWORKX                                                
         GOTO1 VREPORT,REPD                                                     
         XC    APWORKX,APWORKX     Clear W/S                                    
         B     PRTR035             Get next book                                
         SPACE 1                                                                
PRTRX    MVI   APMODE,APMPFKS                                                   
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Routine to compile a script as a NOW job                            *         
***********************************************************************         
         SPACE 1                                                                
PRTREP1  L     R9,AREP                                                          
         L     R2,AIOAREA1         A(This Record)                               
         MVC   REPHW1(26),=C'On - Line Script Compiler'                         
         SPACE 1                                                                
         MVC   FVMSGNO,=AL2(FVFNOTV) Field Not Valid                            
         USING CTLREC,R2                                                        
         LA    R3,APELEM                                                        
         USING CTDSCD,R3                                                        
         MVI   CTDSCEL,CTDSCELQ    Code for Description element                 
         GOTO1 AGETELS,CTLREC                                                   
         ICM   R3,15,APPARM        Found one?                                   
         BZ    PRT10               No                                           
         MVCDD REPPW1(15),CT#DESC                                               
         ZIC   R1,CTDSCLEN                                                      
         SH    R1,=H'3'            CodeºLengthºMVC                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   REPPW1+16(0),CTDSC  Move in Description                          
         DROP  R3                                                               
         GOTO1 VREPORT,REPD                                                     
         SPACE 1                                                                
PRT10    L     R1,VDMGR            Build address list for SCRAMBLE              
         ST    R1,DMGR             ..                                           
         L     R1,VHELLO           ..                                           
         ST    R1,HELLO            ..                                           
         L     R1,=V(SQUASHER)     ..                                           
         A     R1,APRELO           ..                                           
         ST    R1,SQUASH           ..                                           
         SPACE 1                                                                
         L     R1,=V(SCRAMBLE)     Script Compiler                              
         A     R1,APRELO                                                        
         ST    R1,VSCRAMBL                                                      
         SPACE 1                                                                
         L     R4,AIOAREA3                                                      
         MVC   0(25,R4),APRECKEY   Key for 1st time in                          
         SPACE 1                                                                
         XC    APPARM(32),APPARM   Clear my Parm List                           
         L     R1,SCAUTL           A(UTL)                                       
         USING UTLD,R1                                                          
         MVC   APPARM+12(1),TNUM   Terminal # required for UK                   
         DROP  R1                                                               
         SPACE 1                                                                
         ST    R4,APPARM           A(Save AREA for GETBOOK)                     
         MVI   APPARM,0            Expand ++INCLUDES                            
         L     RF,=V(GETBOOK)                                                   
         A     RF,APRELO                                                        
         ST    RF,AGETBOOK         A(GETBOOK)                                   
         LA    R1,APPARM                                                        
         LA    RF,APWORKX          A(Card area)                                 
         ST    RF,4(R1)                                                         
         MVI   4(R1),0             Start seq # is 0                             
         L     RF,VDMGR            A(Data Manager)                              
         ST    RF,8(R1)                                                         
         SPACE 1                                                                
         L     R2,AIOAREA1         A(This Record)                               
         USING CTJREC,R2                                                        
         GOTO1 VSCRAMBL,PLIST,(RC),LEAVEIT,ADDR,(X'C0',CARDGET),       *        
               PRINTOUT,CTJKID                                                  
         SPACE 1                                                                
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         XC    APWORK,APWORK                                                    
         MVC   APWORK(4),=C'=DQU'                                               
         GOTO1 DISPFLD,GENSRVH                                                  
         LA    R1,SAVAREA                                                       
         AH    R1,SAFETY                                                        
         USING POINTDS,R1                                                       
         CLI   DOLAST,C'C'         Was this from a PF04?                        
         BNE   EXIT                                                             
         DROP  R1                                                               
         SPACE 1                                                                
HERE     MVI   APMODE,APMRET       Swap back to CHANGE action                   
         MVI   APPARM,RECBOK                                                    
         MVI   APPARM+1,ACTCHA                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Routine to get a line of source                                     *         
***********************************************************************         
         SPACE 1                                                                
CARDGET  NTR1                                                                   
         L     RC,0(R1)                                                         
         L     R7,AR7                                                           
         L     RB,APBASE1                                                       
         L     RA,APBASE2                                                       
         L     R2,4(R1)                                                         
         LR    R3,R1               Must save R1 before GOTO1                    
         GOTO1 AGETBOOK,APPARM     Line get book                                
         LA    R1,APPARM                                                        
         TM    8(R1),X'80'         End of Book                                  
         BO    CARD10                                                           
         CLI   8(R1),0             Ok                                           
         BE    CARD05                                                           
         TM    8(R1),X'02'         Deleted - can ignore it                      
         BZ    CARD10                                                           
         SPACE 1                                                                
CARD05   MVC   0(72,R2),APWORKX    Give it to scramble                          
         CLC   =C'@@',APWORK       Conditional assembly                         
         BNE   XIT                                                              
         SPACE 1                                                                
CARD10   MVI   0(R3),X'FF'         Indicates EOF to SCRAMBLE                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* Routine to print out a line of compiled script & object code        *         
***********************************************************************         
         SPACE 1                                                                
PRINTOUT NTR1                                                                   
         L     RC,0(R1)            Get Back RC                                  
         L     R7,AR7                                                           
         L     RB,APBASE1                                                       
         L     RA,APBASE2                                                       
         L     R9,AREP                                                          
         MVI   REPWIDTH,REPWIDEQ   165 chars                                    
         L     R2,4(R1)            Line of I/O script code                      
         MVC   REPPW1,0(R2)                                                     
         SPACE 1                                                                
         ZIC   R0,8(R1)            # of errors on this line                     
         L     R2,8(R1)            A(1st CL80 error block)                      
         SPACE 1                                                                
         GOTO1 VREPORT,REPD        Print line                                   
         SPACE 1                                                                
         LTR   R0,R0               Any Errors?                                  
         BZ    XIT                 No                                           
         SPACE 1                                                                
PRO010   MVC   REPPW1(L'REPPW1),SPACES                                          
         MVC   REPPW1(80),0(R2)    Print out an error line                      
         LA    R2,80(R2)                                                        
         GOTO1 VREPORT,REPD        Print line                                   
         BCT   R0,PRO010           Any more errors                              
         B     XIT                                                              
         DROP  R9                                                               
         EJECT                                                                  
***********************************************************************         
* Routines to deal with line and column shifts                        *         
***********************************************************************         
         SPACE 1                                                                
SHIFTY   NTR1                                                                   
         L     R8,ATIA                                                          
         USING BIGDS,R8                                                         
         LA    R9,SAVAREA                                                       
         AH    R9,SAFETY                                                        
         USING POINTDS,R9                                                       
         SPACE 1                                                                
         LA    RF,ACTV04                                                        
         XR    R1,R1                                                            
         SPACE 1                                                                
SHIF000  CLI   0(RF),0                                                          
         BE    SHIF001                                                          
         LA    RF,1(RF)                                                         
         LA    R1,1(R1)                                                         
         B     SHIF000                                                          
         SPACE 1                                                                
SHIF001  STH   R1,SEQMX            Highest sequence # currently active          
         LA    R1,SCRSEQNH         LINE SHIFT HERE                              
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,SCRSEQNH      Any line to move to entered?                 
         BNE   SHIF010             No input, default to start                   
*                                                                               
         TM    FVIIND,FVINUM       Make sure it's a number.                     
*        BO    *+14                                                             
*        MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*        B     EXITL                                                            
         BZ    SHIF010              temp fix @@                                 
                                                                                
*                                                                               
         XC    APDUB,APDUB                                                      
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  APDUB,FVIFLD(0)                                                  
         CVB   R1,APDUB            Binary equivalent of I/P                     
         B     *+8                                                              
         SPACE 1                                                                
SHIF010  LA    R1,1                No Seq # I/P default to first line           
         CLI   APPFKEY,PFK07       Up 10?                                       
         BNE   SHIF015                                                          
         SH    R1,=H'10'           Reduce line count by 10                      
         BP    *+8                 Less then 1?                                 
         LA    R1,1                Then set to 1                                
         SPACE 1                                                                
SHIF015  CLI   APPFKEY,PFK08       Down 10?                                     
         BNE   SHIF016                                                          
         AH    R1,=H'10'           Bump line count by 10                        
         SPACE 1                                                                
SHIF016  STH   R1,SEQLO            Save Seq #                                   
         CLI   BKHIVAL,0           First time in on add                         
         BE    SHIF025                                                          
         SPACE 1                                                                
         MVI   FVMINL,1            Resolve nested books?                        
         GOTO1 AFVAL,SCRSHOWH                                                   
         BNE   SHIF020                                                          
         CLI   FVIFLD,C'Y'         Yes - special for length                     
         BNE   SHIF020                                                          
         CLI   SEQFLAG,255         Reached end of book yet?                     
         BNE   SHIF026                                                          
         MVC   SEQMX,SEQLST        New maximum...                               
         SPACE 1                                                                
SHIF020  CLC   SEQLO,SEQMX         Can we go this high?                         
         BL    SHIF026             No, error                                    
         MVC   SEQLO,SEQMX         Move in highest seq # used                   
         B     SHIF026                                                          
         SPACE 1                                                                
SHIF025  LA    R1,1                First time in on add BKHIVAL is 0            
         STC   R1,BKHIVAL          Set to 1                                     
         STH   R1,SEQLO            Start here                                   
         STH   R1,SEQMX            Start here                                   
         STC   R1,ACTV04           Start here                                   
         SPACE 1                                                                
SHIF026  XR    R1,R1                                                            
         ICM   R1,3,SEQLO          Start Sequence #                             
         BNZ   *+12                                                             
         LA    R1,1                                                             
         STCM  R1,3,SEQLO                                                       
*                                                                               
         EDIT  (R1),(4,SCRSEQN),WRK=EDWORK,DUB=APDUB,ZERO=NOBLANK,ALIGNX        
               =LEFT                                                            
         OI    SCRSEQNH+6,X'80'    Show this number to user                     
         SPACE 1                                                                
         LH    R1,SEQLO                                                         
         LA    R1,MAXTXTNO(R1)     Display lines on screen                      
         ZIC   RF,BKHIVAL                                                       
         CR    R1,RF               Can we go this high?                         
         BL    *+8                                                              
         STH   RF,SEQHI            Set to highest value possible                
         STH   R1,SEQHI                                                         
         CLC   SEQHI,SEQMX         Did we go this high ?                        
         BL    *+10                                                             
         MVC   SEQHI,SEQMX         Set to highest value possible                
         SPACE 1                                                                
         MVI   FVMINL,1            COLUMN SHIFT                                 
*                                  ------------                                 
         GOTO1 AFVAL,SCRCOLNH      Column to shift to?                          
         BNE   SHIF028             Default 0                                    
         SPACE 1                                                                
         XC    APDUB,APDUB         Get I/P and convert it to Hex                
         ZIC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  APDUB,FVIFLD(0)                                                  
         CVB   R1,APDUB            Hex equivalent in R1 now                     
         B     *+8                                                              
         SPACE 1                                                                
SHIF028  LA    R1,0                Nothing I/P default to 0                     
         CLI   APPFKEY,PFK09       Left?                                        
         BNE   SHIF029                                                          
         SH    R1,=H'10'                                                        
         BNM   *+8                 Minimum is 0                                 
         LA    R1,0                                                             
         SPACE 1                                                                
SHIF029  CLI   APPFKEY,PFK10       Right?                                       
         BNE   *+8                                                              
         LA    R1,10(R1)                                                        
         SPACE 1                                                                
         CH    R1,=H'40'           Furthest right shift possible                
         BL    *+8                                                              
         LA    R1,40               maximumum is 40                              
         STC   R1,COLSTRT          Save it                                      
         EDIT  (R1),(2,SCRCOLN),DUB=APDUB,WRK=EDWORK,ZERO=NOBLANK,ALIGNX        
               =LEFT                                                            
         OI    SCRCOLNH+6,X'80'    Edit out this field & Display it             
         XIT1                                                                   
         DROP  R8                                                               
         EJECT                                                                  
***********************************************************************         
* Text Editor Routines here                                           *         
***********************************************************************         
         SPACE 1                                                                
EDITOR   L     R2,ATIA             A(The Records)                               
         USING BIGDS,R2                                                         
         SPACE 1                                                                
         CLI   APACTN,ACTADD       Been into editor flag for add                
         BNE   *+8                                                              
         MVI   EDFLAG,C'F'                                                      
         SPACE 1                                                                
         BAS   RE,SCRNCHNG         Deal with changes on screen                  
         SPACE 1                                                                
         BAS   RE,SCRNEDIT         Deal with action requests                    
         SPACE 1                                                                
         BAS   RE,SHIFTY           Deal with PFKEY moves                        
*        BNE   EXITL                                                            
         SPACE 1                                                                
         BAS   RE,SHOWSCRN         Show the revised screen                      
         MVC   INSLAST,INSTHIS     Reset cursor position for insert             
         MVI   INSTHIS,0                                                        
         SPACE 1                                                                
*                                  SAVE TEMPSTR HERE                            
*                                  -----------------                            
         SPACE 1                                                                
S_TMPS   XC    APPARM(32),APPARM                                                
         LA    R8,X'02'            Tempstr page #                               
         SLL   R8,24                                                            
         GOTO1 VDMGR,APPARM,=C'DMWRT',=C'TEMPSTR',(R8),ATIA                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         CLC   ERRFLAG,=H'0'       'Error' Messages set?                        
         BNE   MESSAGE                                                          
         MVC   ERRFLAG,=H'35'      Move pending                                 
         CLI   MOVEFLAG,0                                                       
         BNE   MESSAGE                                                          
         MVC   ERRFLAG,=H'34'      Record changed PF3 to save                   
         B     MESSAGE                                                          
         EJECT                                                                  
***********************************************************************         
* Routine to deal with PF3 key press for record save                  *         
***********************************************************************         
         SPACE 1                                                                
EDITLAST NTR1                                                                   
         L     R2,ATIA             A(The Records)                               
         USING BIGDS,R2                                                         
         LA    R9,SAVAREA                                                       
         AH    R9,SAFETY                                                        
         USING POINTDS,R9                                                       
         BAS   RE,SCRNCHNG         Deal with changes on screen                  
         BAS   RE,SCRNEDIT         Deal with action requests                    
         BAS   RE,SHOWSCRN         Show the revised screen                      
         MVC   INSLAST,INSTHIS     Reset cursor position for insert             
         MVI   INSTHIS,0                                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Routine to check for changes on the screen                          *         
***********************************************************************         
         SPACE 1                                                                
SCRNCHNG NTR1                                                                   
         XR    R1,R1                                                            
         ICM   R1,3,SEQLO          Start Sequence #                             
         BNZ   *+12                                                             
         LA    R1,1                                                             
         STCM  R1,3,SEQLO                                                       
*                                                                               
         XC    LINCNT,LINCNT                                                    
         LA    R4,SCRTXT1H         First text field                             
         SPACE 1                                                                
SCRC010  GOTO1 AFVAL,(R4)          Validate Text field                          
         TM    FVIIND,FVITHIS      Input this time?                             
         BZ    SCRC020             No input this time, bump to next             
         SPACE 1                                                                
         ZIC   R0,LINCNT           Current line #                               
         LH    R1,SEQLO                                                         
         AR    R1,R0               R1 holds the displacement in ACTV04          
         LA    RF,ACTV04(R1)       RF=A(Index to this line)                     
         BCTR  RF,0                                                             
         CLI   0(RF),0             Found a line with no number                  
         BNE   *+8                                                              
         BAS   RE,SCRC040          Go and put one in...                         
         ZIC   R1,0(RF)            R1 holds Book Record #                       
         MH    R1,LINLENG                                                       
         LA    R1,READLINE(R1)     R1 now at start of record in TIA             
         MVI   0(R1),C'X'          Flag for changes                             
         SPACE 1                                                                
         ZIC   RF,COLSTRT          Start column of screen display               
         LA    R1,1(RF,R1)         Move in data to here                         
         LA    RE,72               72 cols to 1st number is fixed **            
         SR    RE,RF               Max that can be moved in                     
         SPACE 1                                                                
         BCTR  RE,0                Reduce for MVC                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SPACES      Clear Memory Card of previous crap           
         SPACE 1                                                                
         LA    RE,1(RE)            Put back for compare                         
         ZIC   RF,FVILEN           L' input on screen                           
         CLI   FVILEN,0            Card been cleared?                           
         BNE   *+8                                                              
         LA    RF,72                                                            
         CR    RE,RF               Can all be moved in?                         
         BH    *+6                 Yes                                          
         LR    RF,RE               No, only move max allowed                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),FVIFLD      Move in data to memory card                  
         SPACE 1                                                                
SCRC020  ZIC   R1,LINCNT           Bump Line Count                              
         LA    R1,1(R1)            ..                                           
         STC   R1,LINCNT           and save it.                                 
         CLI   LINCNT,MAXTXTNO     Any lines left on screen?                    
         BE    SCRC030             No - finish                                  
         SPACE 1                                                                
         LH    R0,SEQLO            Start Sequence #                             
         AR    R1,R0               R1 holds the displacement in ACTV04          
         BCTR  R1,0                Line before this one                         
         BCTR  R1,0                Line before this one                         
         LA    RF,ACTV04(R1)       RF=A(Index to this line)                     
         CLI   0(RF),0             Empty                                        
         BE    SCRC030                                                          
         SPACE 1                                                                
         AH    R4,TXTLEN                                                        
         B     SCRC010                                                          
         SPACE 1                                                                
SCRC030  XIT1                                                                   
         SPACE 1                                                                
SCRC040  LH    R1,SEQHI            Bump high sequence #                         
         LA    R1,1(R1)            and save it - for display later              
         STH   R1,SEQHI                                                         
         SPACE 1                                                                
         CLI   PASS04,0            Any passives to reuse?                       
         BNE   SCRC042                                                          
         ZIC   R1,BKHIVAL          Get highest card used so far                 
         LA    R1,1(R1)            Bump to next                                 
         STC   R1,0(RF)                                                         
         STC   R1,BKHIVAL          Save it                                      
         BR    RE                                                               
         SPACE 1                                                                
SCRC042  ZIC   R1,PASS04           Re-use first passive                         
         STC   R1,0(RF)                                                         
         MVC   PASS04(L'PASS04-1),PASS04+1 Shuffle up passives                  
         MVI   PASS04+L'PASS04-1,0 Just to be neat...                           
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Routine to deal with action requests                                *         
***********************************************************************         
         SPACE 1                                                                
SCRNEDIT NTR1                                                                   
         XC    LINCNT,LINCNT       Line Count for screen                        
         LA    R4,SCRACT1H         First Action field                           
         SPACE 1                                                                
SCRE010  GOTO1 AFVAL,(R4)          Validate action field                        
         TM    FVIIND,FVITHIS      Input this time?                             
         BZ    SCRE020             No input this time, bump to next             
         SPACE 1                                                                
         LA    R1,ACCTAB           Table of supported actions                   
SCRE015  CLI   0(R1),X'FF'         E.O.T.                                       
         BE    SCRE020                                                          
         CLC   FVIFLD(1),0(R1)     Match?                                       
         BE    *+12                No                                           
         LA    R1,ACCTABLQ(R1)                                                  
         B     SCRE015                                                          
         SPACE 1                                                                
         ICM   RF,15,1(R1)         A(Command handling routine)                  
         A     RF,APRELO           Relocation factor                            
         BASR  RE,RF               Do it                                        
         SPACE 1                                                                
SCRE020  ZIC   R1,LINCNT           Bump Line Count                              
         LA    R1,1(R1)            ..                                           
         STC   R1,LINCNT           and save it.                                 
         AH    R4,TXTLEN           Next text field                              
         LA    RF,SCRLSTDH         Last line on screen                          
         CR    R4,RF               Any left?                                    
         BH    SCRE030             No                                           
         B     SCRE010             Go and validate next field                   
         SPACE 1                                                                
SCRE030  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* Delete a line action                                                *         
***********************************************************************         
         SPACE 1                                                                
ACCDEL   NTR1                                                                   
         ST    R4,APCURSOR                                                      
         XR    RF,RF                                                            
         IC    RF,LINCNT           CURRENT LINE NUMBER (ZERO BASED)             
         LH    R1,SEQLO                                                         
         LA    R1,0(R1,RF)         R1 HOLDS DISPLACEMENT INTO ACTV04            
         LA    RF,ACTV04(R1)                                                    
         BCTR  RF,0                MAKE ZERO BASED                              
         XR    R1,R1                                                            
         IC    R1,0(RF)            R1 HOLDS BOOK RECORD #                       
         LA    RE,L'ACTV04         LENGTH OF ACTV04                             
         SR    RE,RF               RE holds length to move                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),1(RF)       Close up 04 element                          
         LA    RF,ACTV04+L'ACTV04-1                                             
         MVI   0(RF),0             Move in 0 at end of element                  
         SPACE 1                                                                
         XR    RE,RE                                                            
         IC    RE,BKHIVAL          DELETED HIGHEST BOOK USED ?                  
         CR    R1,RE               IF SO REDUCE # OF BKHIVAL & FORGET           
         BNE   ACCD010             STORING IT IN PASS04 - PROGRAM WILL          
         BCTR  RE,0                PICK UP ON REUSE CYCLE ANYWAY                
         STC   RE,BKHIVAL                                                       
         B     ACCD020                                                          
*                                                                               
ACCD010  LA    RF,PASS04           PUT SUB # INTO PASSIVE ELEMENT               
         CLI   0(RF),0             AT END SO IT CAN BE REUSED                   
         BE    *+12                ..                                           
         LA    RF,1(RF)            ..                                           
         B     *-12                ..                                           
         STC   R1,0(RF)            ..                                           
*                                                                               
ACCD020  MH    R1,LINLENG          Index into record stored in TIA              
         LA    R1,READLINE(R1)     R1 now at start of record                    
         LH    RF,LINLENG          Length of a memory card                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SPACES      Clear memory card to spaces                  
         MVI   0(R1),C'X'          Flag for I/P                                 
         B     DELA020                                                          
         SPACE 1                                                                
DELA020  ZIC   R1,LINCNT           Need to set linecount back to this           
         BCTR  R1,0                line so next line will be checked            
         STC   R1,LINCNT                                                        
         XIT1                                                                   
         SPACE 1                                                                
***********************************************************************         
* Routine to move a line                                              *         
***********************************************************************         
         SPACE 1                                                                
ACCMOVE  NTR1                                                                   
         LH    RF,SEQLO                                                         
         ZIC   R1,LINCNT                                                        
         LA    RF,0(RF,R1)         Index here                                   
         CLI   FVIFLD,C'M'         Move from?                                   
         BNE   MOVE001                                                          
         STC   RF,MOVEFR           Save this index                              
         MVI   MOVEFLAG,C'M'       Set Move Flag on for move                    
         MVI   MOVECOPY,C'M'       Set Copy Flag on for move                    
         B     MOVE006                                                          
         SPACE 1                                                                
MOVE001  CLI   FVIFLD,C'C'         Copy from?                                   
         BNE   MOVE002                                                          
         STC   RF,MOVEFR           Save this index                              
         MVI   MOVEFLAG,C'C'       Set Move Flag on for copy                    
         MVI   MOVECOPY,C'C'       Set Copy Flag on for copy                    
         B     MOVE006                                                          
         SPACE 1                                                                
MOVE002  CLI   FVIFLD,C'A'         Move to after?                               
         BNE   MOVE004                                                          
         LA    RF,1(RF)                                                         
         STC   RF,MOVETO           Save this index                              
         MVI   MOVEFLAG,C'A'       Set Move Flag on for move                    
         ST    R4,APCURSOR                                                      
         B     MOVE006                                                          
         SPACE 1                                                                
MOVE004  CLI   FVIFLD,C'B'         Move to before?                              
         BNE   MOVE005                                                          
         STC   RF,MOVETO           Save this index                              
         ST    R4,APCURSOR                                                      
         MVI   MOVEFLAG,C'B'       Set Move Flag on for move                    
         B     MOVE006                                                          
         SPACE 1                                                                
MOVE005  CLI   FVIFLD,C'X'         Cancel move action                           
         BE    MOVE030                                                          
         SPACE 1                                                                
MOVE006  CLI   MOVEFR,0            Set move from?                               
         BE    XIT                 No                                           
         CLI   MOVETO,0            Set move to?                                 
         BE    XIT                 No                                           
         SPACE 1                                                                
         CLI   MOVECOPY,C'C'       Copy action?                                 
         BNE   MOVE007             No                                           
         ZIC   RF,MOVEFR                                                        
         LA    RF,ACTV04(RF)                                                    
         BAS   RE,RCCCOPY                                                       
         SPACE 1                                                                
MOVE007  ZIC   RF,MOVETO                                                        
         LA    RF,ACTV04(RF)       A(Move to)                                   
         BCTR  RF,0                                                             
         ZIC   RE,MOVEFR                                                        
         LA    RE,ACTV04(RE)       A(Move from)                                 
         CR    RE,RF                                                            
         BNE   MOVE008             Trying to move to same place                 
         CLI   MOVECOPY,C'C'                                                    
         BNE   ERREX                                                            
         SPACE 1                                                                
MOVE008  BCTR  RE,0                                                             
         CR    RE,RF                                                            
         BL    MOVE020                                                          
         SPACE 1                                                                
         ZIC   R0,0(RE)            Character to be moved                        
         BCTR  RE,0                                                             
         SPACE 1                                                                
MOVE010  MVC   1(1,RE),0(RE)                                                    
         BCTR  RE,0                                                             
         CR    RE,RF                                                            
         BNL   MOVE010                                                          
         STC   R0,0(RF)                                                         
         B     MOVE030                                                          
         SPACE 1                                                                
MOVE020  BCTR  RF,0                                                             
         CLI   MOVECOPY,C'C'                                                    
         BNE   *+8                                                              
         LA    RF,1(RF)                                                         
         LR    R1,RF               Save A(Move to)                              
         ZIC   R0,0(RE)            Character to be moved                        
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),1(RE)       Shuffle along                                
         STC   R0,0(R1)                                                         
         B     MOVE030                                                          
         SPACE 1                                                                
ERREX    MVC   ERRFLAG,=H'36'      Invalid option                               
         B     MOVE030             Clear all flags                              
         SPACE 1                                                                
MOVE030  XC    MOVETO,MOVETO                                                    
         XC    MOVEFR,MOVEFR                                                    
         XC    MOVEFLAG,MOVEFLAG                                                
         XC    MOVECOPY,MOVECOPY                                                
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* Routine to insert a line                                            *         
***********************************************************************         
         SPACE 1                                                                
ACCINS   NTR1                                                                   
         AH    R4,TXTLEN           L'to next line                               
         LR    R1,R4                                                            
         SR    R1,R5               R5=A(TWA)                                    
         ST    R1,DISPCURS         Displacement from TWA for cursor             
         ZIC   R3,0(R4)                                                         
         AR    R4,R3                                                            
         ST    R4,APCURSOR         Field for this time cursor                   
         LH    RF,SEQLO                                                         
         ZIC   R1,LINCNT                                                        
         LA    RF,0(RF,R1)         # where line to be INSErted                  
         LA    RF,ACTV04(RF)       A(Index to the line to go in)                
         BCTR  RF,0                                                             
         LA    R0,ACTV04+L'ACTV04-1 A(End of ACTV04)                            
         LR    RE,RF                                                            
         SPACE 1                                                                
INSE005  CLI   0(RF),0             End of Index lines?                          
         BE    INSE010             Yes                                          
         LA    RF,1(RF)            Bump to next                                 
         CR    R0,RF               Loop check                                   
         BNH   ERREX                                                            
         B     INSE005             Try again                                    
         SPACE 1                                                                
INSE010  XC    EDWORK,EDWORK       Temp storage of cards                        
         SR    RF,RE               RF=L'cards to be shuffled right              
         BZ    INSE015                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   EDWORK(0),0(RE)     Move into temp storage                       
         SPACE 1                                                                
INSE015  CLI   PASS04,0            Passive to reuse?                            
         BE    INSE020             No                                           
         IC    R1,PASS04           Get passive                                  
         MVC   PASS04(L'PASS04-1),PASS04+1 Close ranks                          
         STC   R1,0(RE)            Save it                                      
         B     INSE030                                                          
         SPACE 1                                                                
INSE020  IC    R1,BKHIVAL          Highest book used thus far                   
         LA    R1,1(R1)            Next one                                     
         STC   R1,0(RE)            Save it                                      
         STC   R1,BKHIVAL          ..                                           
         SPACE 1                                                                
INSE030  EX    RF,*+8              Move other cards back                        
         B     *+10                                                             
         MVC   1(0,RE),EDWORK                                                   
         MH    R1,LINLENG          Clear this line of data                      
         LA    R1,READLINE(R1)     in memory                                    
         MVC   1(72,R1),SPACES                                                  
         MVI   0(R1),C'X'          Mark for modified                            
         MVI   INSTHIS,X'FF'                                                    
         SPACE 1                                                                
         ZIC   R1,LINCNT           Need to index into next+1 to keep            
         LA    R1,1(R1)            screen lines and 04 element in synch         
         STC   R1,LINCNT           ..                                           
         SPACE 1                                                                
         XIT1                      That`s all folks                             
         EJECT                                                                  
***********************************************************************         
* Routine to replicate a line                                         *         
***********************************************************************         
         SPACE 1                                                                
ACCREP   NTR1                                                                   
         ST    R4,APCURSOR                                                      
         LH    RF,SEQLO                                                         
         ZIC   R1,LINCNT                                                        
         LA    RF,0(RF,R1)         # where line to be replicated                
         LA    RF,ACTV04(RF)       A(Index to the line to go in)                
RCCCOPY  LA    R0,ACTV04+L'ACTV04-1 A(End of ACTV04)                            
         ST    RF,ARSPR            For data for copy...                         
         ST    RE,ARETEMP          For copy...                                  
         LR    RE,RF                                                            
         SPACE 1                                                                
REPE005  CLI   0(RF),0             End of Index lines?                          
         BE    REPE010             Yes                                          
         LA    RF,1(RF)            Bump to next                                 
         CR    R0,RF               Loop check                                   
         BNH   ERREX                                                            
         B     REPE005             Try again                                    
         SPACE 1                                                                
REPE010  XC    EDWORK,EDWORK       Temp storage of cards                        
         SR    RF,RE               RF=L'cards to be shuffled right              
         BZ    REPE015                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   EDWORK(0),0(RE)     Move into temp storage                       
         SPACE 1                                                                
REPE015  CLI   PASS04,0            Passive to reuse?                            
         BE    REPE020             No                                           
         ZIC   R1,PASS04           Get passive                                  
         MVC   PASS04(L'PASS04-1),PASS04+1 Close ranks                          
         STC   R1,0(RE)            Save it                                      
         B     REPE030                                                          
         SPACE 1                                                                
REPE020  ZIC   R1,BKHIVAL          Highest book used thus far                   
         LA    R1,1(R1)            Next one                                     
         STC   R1,0(RE)            Save it                                      
         STC   R1,BKHIVAL          ..                                           
         SPACE 1                                                                
REPE030  EX    RF,*+8              Move other cards back                        
         B     *+10                                                             
         MVC   1(0,RE),EDWORK                                                   
         MH    R1,LINLENG                                                       
         LA    R1,READLINE(R1)     A(Inserted line)                             
         SPACE 1                                                                
         CLI   MOVECOPY,C'C'                                                    
         BE    REPE040                                                          
         SPACE 1                                                                
         LH    RF,SEQLO                                                         
         ZIC   RE,LINCNT                                                        
         BCTR  RE,0                                                             
         LA    RF,0(RF,RE)         # where line to be copied from               
         LA    RF,ACTV04(RF)       A(Index to the line to copy)                 
         B     *+10                                                             
         SPACE 1                                                                
REPE040  L     RF,ARSPR                                                         
         BCTR  RF,0                                                             
         SPACE 1                                                                
         ZIC   RE,0(RF)            Index #                                      
         MH    RE,LINLENG                                                       
         LA    RE,READLINE(RE)     A(Line to copy from)                         
         SPACE 1                                                                
         MVC   1(72,R1),1(RE)      Copy data across                             
         MVI   0(R1),C'X'          Mark for modified                            
         ZIC   R1,LINCNT           Need to index into next+1 to keep            
         LA    R1,1(R1)            screen lines and 04 element in synch         
         STC   R1,LINCNT           ..                                           
         CLI   MOVECOPY,C'C'                                                    
         BNE   XIT                                                              
         L     RE,ARETEMP                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Routine to display memory cards onto the screen                     *         
***********************************************************************         
         SPACE 1                                                                
SHOWSCRN NTR1                                                                   
         XC    LINCNT,LINCNT       Reset screen position                        
         LA    R4,SCRACT1H         First display line                           
         USING SHOWSCRD,R4                                                      
         SPACE 1                                                                
         TWAXC SCRACT1H,SCRLSTD,PROT=Y Clear Data Portion of Screen             
         SPACE 1                                                                
SHOW010  LH    RF,SEQLO            Sequence # to start from                     
         BCTR  RF,0                                                             
         ZIC   RE,LINCNT           Current position on screen                   
         LA    R1,0(RF,RE)         First card + Current count                   
         LA    R1,ACTV04(R1)       A(This card`s index)                         
         CLI   0(R1),0             Any more cards to display?                   
         BE    SHOW050             Finished!                                    
         LA    RF,READLINE         Start of memory cards                        
         ZIC   RE,0(R1)                                                         
         MH    RE,LINLENG          Index into memory cards                      
         LA    RF,0(RE,RF)         RF=A(This memory Card)                       
         MVC   APWORKX(72),1(RF)   Move in data from this card                  
         SPACE 1                                                                
         LH    RF,SEQLO            BUILD SEQUENCE # FOR DISPLAY HERE            
         ZIC   RE,LINCNT           ---------------------------------            
         LA    RF,0(RE,RF)         Current sequence #                           
         CVD   RF,APDUB            Convert to packed                            
         OI    APDUB+L'APDUB-1,X'0F' Zone it correctly                          
         UNPK  APFULL(4),APDUB     UNPK it for display later                    
         LA    R1,APWORKX+72       Move onto display line @+72                  
         MVC   0(4,R1),APFULL                                                   
         SPACE 1                                                                
         ZIC   RF,COLSTRT          SET COLUMN FOR DISPLAY HERE                  
*                                  ---------------------------                  
         LA    RE,79               Max possible to display -1                   
         SR    RE,RF               How much to display this time                
         LA    RF,APWORKX(RF)      Start of display                             
         XC    EDWORK,EDWORK                                                    
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   EDWORK(0),0(RF)     Save data to be displayed                    
         XC    APWORKX,APWORKX                                                  
         MVC   APWORKX,EDWORK      Move into APWORK data for display            
         GOTO1 DISPFLD,SHOWTXTH    Display data                                 
         XC    APWORKX,APWORKX                                                  
         MVC   APWORK(4),APFULL    Saved sequence # from previously             
         GOTO1 DISPFLD,SHOWLINH    Display sequence #                           
         SPACE 1                                                                
SHOW020  CLI   MOVEFLAG,0         Move action pending?                          
         BE    SHOW040            No                                            
         SPACE 1                                                                
         LH    RF,SEQLO                                                         
         ZIC   R1,LINCNT                                                        
         LA    RF,0(R1,RF)                                                      
         ZIC   R0,MOVEFR                                                        
         CR    R0,RF                                                            
         BNE   SHOW030                                                          
         B     SHOW035                                                          
         SPACE 1                                                                
SHOW030  IC    R0,MOVETO           Sequence # to move to                        
         CR    R0,RF                                                            
         BNE   SHOW040                                                          
         SPACE 1                                                                
SHOW035  MVC   APWORK,SPACES                                                    
         MVC   APWORK(1),MOVEFLAG                                               
         GOTO1 DISPFLD,SHOWACTH                                                 
         SPACE 1                                                                
SHOW040  EQU   *                                                                
         ZIC   R1,LINCNT           Bump current display line #                  
         LA    R1,1(R1)            ..                                           
         STC   R1,LINCNT           and save it                                  
         AH    R4,TXTLEN           Next Text                                    
         CLI   LINCNT,MAXTXTNO     Can we display any more?                     
         BNE   SHOW010             Finished!                                    
         SPACE 1                                                                
SHOW050  EQU   *                                                                
         CLI   INSTHIS,X'FF'       Was there insert this time?                  
         BE    XIT                 Yes leave well alone                         
         CLI   INSLAST,X'FF'       Was insert last time                         
         BNE   XIT                 No ignore                                    
         L     R1,DISPCURS         Last insert                                  
         AR    R1,R5                                                            
         ST    R1,APCURSOR         Save it                                      
         SPACE 1                                                                
XIT      L     R1,APCURSOR         Did APCURSOR get set?                        
         LA    R2,SCRACT1H                                                      
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         ST    R2,APCURSOR                                                      
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Message set by routine                                                        
***********************************************************************         
         SPACE 1                                                                
MESSAGE  XC    APPARM(32),APPARM                                                
         LA    RF,APPARM                                                        
         MVI   8(RF),C'I'                                                       
         MVI   21(RF),X'FF'                                                     
         LH    R1,ERRFLAG                                                       
         STH   R1,2(RF)                                                         
         MVC   FVMSGNO,=AL2(FVFGTSET)                                           
         B     EXIT                                                             
         SPACE 1                                                                
**********************************************************************          
* General Field Transmit if Changed                                  *          
* R1=A(TWA Header)                                                   *          
* APWORK must contain the new text                                   *          
**********************************************************************          
         SPACE 1                                                                
DISPFLD  ZIC   RF,FVTLEN-FVIHDR(R1)                                             
         SH    RF,=Y(L'FVIHDR)                                                  
         TM    FVATRB-FVIHDR(R1),FVAXTND                                        
         BZ    *+8                                                              
         SH    RF,=Y(L'FVIHDR)     Knock off Header Extension                   
         BCTR  RF,0                                                             
         EX    RF,DISPFLDC         Compare Field contents                       
         BER   RE                  Equal don`t bother to move in data           
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         EX    RF,DISPFLDM         Move in new data                             
         BR    RE                                                               
         SPACE 1                                                                
DISPFLDC CLC   L'FVIHDR(0,R1),APWORK                                            
DISPFLDM MVC   L'FVIHDR(0,R1),APWORK                                            
         EJECT                                                                  
***********************************************************************         
* Find next element of same code within Record                        *         
* Entry      R3 = A(Current Element)                                  *         
*        APELEM = Element Code to find                                *         
* Exit    CC EQ - Found - R3=A(New Element)                           *         
*         CC NE - Not Found                                           *         
***********************************************************************         
         SPACE 1                                                                
NEXTEL   ZIC   RF,1(R3)            L'Element                                    
         AR    R3,RF               A(Next element)                              
         ICM   RF,1,1(R3)          L'Element                                    
         BNZ   *+8                                                              
         LTR   RB,RB               Force CC Non Zero                            
         BR    RE                                                               
         CLC   0(1,R3),APELEM      Same as last?                                
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Literals and Constants                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
SPACES   DC    CL165' '                                                         
         SPACE 1                                                                
TYPTAB   EQU   *                                                                
         DC    C'S',AL4(VALKEY20),AL4(VALSEL20),AL4(VALREQ20)                   
TYPTABLQ EQU   *-TYPTAB                                                         
         DC    C'L',AL4(VALKEY30),AL4(VALSEL30),AL4(VALREQ30)                   
         DC    C'J',AL4(VALKEY40),AL4(VALSEL40),AL4(VALREQ40)                   
         DC    C'Q',AL4(VALKEY50),AL4(VALSEL50),AL4(VALREQ50)                   
         DC    X'FF'                                                            
         SPACE 1                                                                
ACCTAB   EQU   *                                                                
         DC    C'D',AL4(ACCDEL)                                                 
ACCTABLQ EQU   *-ACCTAB                                                         
         DC    C'I',AL4(ACCINS)                                                 
         DC    C'R',AL4(ACCREP)                                                 
         DC    C'M',AL4(ACCMOVE)                                                
         DC    C'C',AL4(ACCMOVE)                                                
         DC    C'A',AL4(ACCMOVE)                                                
         DC    C'B',AL4(ACCMOVE)                                                
         DC    C'X',AL4(ACCMOVE)                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
SAFETY   DC    Y(SAVAREAX-SAVAREA)                                              
         SPACE 1                                                                
MTHTAB1  DC    C'JanFebMarAprMayJunJulAugSepOctNovDec'                          
         SPACE 1                                                                
TXTLEN   DC    Y(SCRTXT2-SCRTXT1)                      L LINE                   
MAXTXTNO EQU   ((SCRLSTE-SCRACT1)/(SCRTXT2-SCRTXT1))                            
         SPACE 1                                                                
LINLENG  DC    Y(L'READLINE)                                                    
         SPACE 1                                                                
MSGLINS  DS    0C                                                               
         DC    CL78'PF1=Help,7=Up,8=Down,9=Left,10=Right'                       
         DC    CL78'PF1=Help,PF3=Save,PF7=Up,PF8=Down,9=Left,10=Right'          
         DC    CL78'PF1=Help,PF3=Save,PF4=Compile,PF7=Up,PF8=Down,9=Lef+        
               t,10=Right'                                                      
         DC    CL78'PF1=Help,7=Up,8=Down,9=Left,10=Right,12=Return to L+        
               ist'                                                             
         DC    CL78'PF1=Help,PF3=Save,PF7=Up,PF8=Down,9=Left,10=Right,1+        
               2=Return'                                                        
         DC    CL78'PF1=Help,PF3=Save,PF4=Compile,PF7=Up,PF8=Down,9=Lef+        
               t,10=Right,12=Return'                                            
         EJECT                                                                  
***********************************************************************         
* DSECTS                                                              *         
***********************************************************************         
         SPACE 1                                                                
* FAUTL                                                                         
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
* CTGENWRK                                                                      
* FAFACTS                                                                       
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENWRK                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENADD                                              0        
         ORG   GENTABH                                                          
       ++INCLUDE CTGENACD                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGENA5D                                                       
         SPACE 1                                                                
         ORG   SAVAREA+4096                                                     
LINESRD  DS    X                                                                
LINESHI  DS    X                                                                
         EJECT                                                                  
         SPACE 1                                                                
TYPTABD  DSECT                                                                  
TYPTYPE  DS    C                                                                
TYPADDR  DS    AL4                                                              
TYPADDR1 DS    AL4                                                              
TYPADDR2 DS    AL4                                                              
         SPACE 1                                                                
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
ARSPR    DS    A                                                                
ARETEMP  DS    A                                                                
EDWORK   DS    XL251                                                            
ERRFLAG  DS    H                                                                
AGETBOOK DS    V                                                                
ADDR     DS    0D                                                               
DMGR     DS    A                                                                
HELLO    DS    A                                                                
SQUASH   DS    A                                                                
VSCRAMBL DS    A                                                                
AR7      DS    A                                                                
PLIST    DS    XL24                                                             
LEAVEIT  DS    4096C               Keep your paws off it`s for SCRAMBLE         
LOCALX   EQU   *                                                                
         SPACE 1                                                                
POINTDS  DSECT                                                                  
SCRIPT   DS    XL10                Script Name                                  
DOLAST   DS    XL1                 Last Action - General                        
ACTLAST  DS    XL1                 Last Action - Book Record                    
BKHIVAL  DS    XL1                 Highest book # used                          
CCOUNT   DS    H                                                                
SEQLO    DS    H                   Display from                                 
SEQHI    DS    H                   Highest book used                            
SEQMX    DS    H                   Display to                                   
SEQLST   DS    H                   Getbook flag                                 
SEQFLAG  DS    XL1                 Getbook flag                                 
COLSTRT  DS    XL1                 Start column display                         
LINCNT   DS    XL1                 Current line #                               
LINMOD   DS    XL1                 Modified line #                              
INSLAST  DS    XL1                 Insert on last entry?                        
INSTHIS  DS    XL1                 Insert on this entry?                        
DISPCURS DS    F                   A(Insert field header)                       
MOVETO   DS    XL1                 Used for move/to action                      
MOVEFR   DS    XL1                 ..                                           
MOVEFLAG DS    XL1                 Flag for move/to action                      
MOVECOPY DS    XL1                 Flag for copy action                         
EDFLAG   DS    XL1                 Been to editor flag for TWA read             
ACTV04   DS    XL251               ACTIVE ELEMENTS                              
PASS04   DS    XL251               PASSIVE ELEMENTS                             
         SPACE 1                                                                
LISTLIND DSECT                                                                  
         DS    CL8                                                              
         DS    CL3                                                              
         DS    CL8                                                              
LENAME   DS    CL10                                                             
         ORG   LENAME                                                           
LENAMEO  DS    CL8                                                              
         DS    CL2                                                              
         DS    CL1                                                              
LEACTVD  DS    CL2                                                              
LEACTVM  DS    CL3                                                              
LEACTVY  DS    CL2                                                              
         DS    CL2                                                              
LEDESC   DS    CL50                                                             
         DS    CL1                                                              
LELEN    DS    CL3                                                              
         SPACE 1                                                                
SHOWSCRD DSECT                                                                  
SHOWACTH DS    XL8                                                              
SHOWACT  DS    XL3                                                              
         DS    XL8                                                              
SHOWTXTH DS    XL8                                                              
SHOWTXT  DS    XL68                                                             
         DS    XL8                                                              
SHOWLINH DS    XL8                                                              
SHOWLIN  DS    XL4                                                              
         DS    XL8                                                              
         SPACE 1                                                                
BIGDS    DSECT                                                                  
READLINE DS    250CL73                                                          
BIGDQ    DS    0C                                                               
MAXMEM   EQU   250                                                              
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012CTGEN23X  08/22/00'                                      
         END                                                                    
