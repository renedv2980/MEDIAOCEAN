*          DATA SET SEACS11X   AT LEVEL 008 AS OF 05/01/02                      
*PHASE TA0D11,*                                                                 
*INCLUDE SCINKEY                                                                
         TITLE 'SEACS11 - SECURITY ACCESS - PERSON REPORT FUNCTIONS'            
ACS11    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACS11*,RA,R9,R8,RR=RE                                        
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING SAPEREC,R2          R2=A(RECORD KEY)                             
         L     RC,ASYSWORK                                                      
         USING SYSWORK,RC          RC=A(SYSTEM + LOCAL W/S)                     
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         OC    OPTAGY,OPTAGY                                                    
         BZ    *+14                                                             
         MVC   AGENCYID,OPTAGY                                                  
         B     *+10                                                             
         MVC   AGENCYID,CUAALF                                                  
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         SPACE 1                                                                
         B     EXIT                01 - APMVALK                                 
         B     EXIT                02 - APMVALR                                 
         B     EXIT                03 - APMDISK                                 
         B     EXIT                04 - APMDISR                                 
         B     EXIT                05 - APMDELR                                 
         B     EXIT                06 - APMRESR                                 
         B     EXIT                07 - APMVALP                                 
         B     EXIT                08 - APMGETS                                 
         B     EXIT                09 - APMDISS                                 
         B     EXIT                10 - APMVALS                                 
         B     EXIT                11 - APMFLST                                 
         B     EXIT                12 - APMPROC                                 
         B     EXIT                13 - APMFSCR                                 
         B     EXIT                14 - APMLSCR                                 
         B     VALREQ              15 - APMVALQ                                 
         B     PRTREP              16 - APMREPP                                 
         B     EXIT                17 - APMSETT                                 
         B     PUTKEY              18 - APMPUTK                                 
         B     EXIT                19 - APMNEWK                                 
         B     EXIT                20 - APMFRP                                  
         B     EXIT                21 - APMDISS2                                
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                           *         
***********************************************************************         
         SPACE 1                                                                
         USING SAPEREC,R2                                                       
         USING REPD,R4             R4=A(REPORT WORK AREA)                       
VALREQ   L     R4,AREP                                                          
         XC    APRECKEY,APRECKEY                                                
         XC    SELOPT(SELOPTL),SELOPT  CLEAR FILTERS                            
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPREQH       VALIDATE REQUESTOR                           
         BNE   VALREQX                                                          
         MVC   INUSER,FVIFLD       SET REQUESTOR                                
*                                                                               
         GOTO1 AVALWHEN,REPWHENH   VALIDATE WHEN                                
         BNE   VALREQX                                                          
*                                                                               
         GOTO1 AVALDEST,REPDESTH   VALIDATE DESTINATION ID                      
         BNE   VALREQX                                                          
*                                                                               
         GOTO1 AVALOTYP,REPOTYPH   VALIDATE OUTPUT TYPE                         
         BNE   VALREQX                                                          
*                                                                               
         GOTO1 AFVAL,REPREQPH      VALIDATE REQUEST PASSWORD                    
         BNE   *+10                                                             
         MVC   REPPSWD,FVIFLD                                                   
*                                                                               
         LA    R2,APRECKEY         SET UP INITIAL KEY                           
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,AGENCYID    AGENCY ALPHA                                 
         GOTO1 VDATCON,APPARM,(X'05',0),(X'02',TODAY)                           
         MVC   TODAYC,FFILL                                                     
         XC    TODAYC,TODAY                                                     
         XC    SAVPID,SAVPID                                                    
*                                  VALIDATE FILTER PARAMETERS                   
         GOTO1 AFVAL,REPPIDH       VALIDATE FIRST PESONAL ID                    
         BNE   *+10                                                             
         MVC   SAPEPID,FVIFLD                                                   
*                                                                               
VQ002    GOTO1 AFVAL,REPUSRH       GET USER ID FILTER                           
         BNE   VQ010                                                            
         CLI   INWHEN,MIXIOKN      TEST FOR NOW REPORT                          
         BE    SAENOW              'NOW' REPORT CAN'T USE THIS FILTER           
         GOTO1 AVALUID,REPUSRH                                                  
         BNE   VALREQX                                                          
         MVC   SELUSR,FVIFLD                                                    
*                                                                               
VQ010    GOTO1 AFVAL,REPNAMEH      VALIDATE LAST NAME FILTER                    
         BNE   VQ020                                                            
         MVC   SELNAM,FVIFLD                                                    
         MVC   SELNAML,FVILEN                                                   
*                                                                               
VQ020    GOTO1 AFVAL,REPDEFH       VALIDATE EFFECTIVE DATA FILTER               
         BNE   VQ030                                                            
         MVC   SELDEFC,FFILL                                                    
         ZIC   R0,FVILEN                                                        
         MVC   APBYTE,CULANG                                                    
         OI    APBYTE,PVINSGLO+PVINSGLS                                         
         GOTO1 VPERVAL,APPARM,((R0),FVIFLD),(APBYTE,APWORK)                     
         CLI   4(R1),PVRCONE                                                    
         BE    VQ026                                                            
         SR    RE,RE                                                            
         ZIC   RF,FVXLEN                                                        
         EX    RF,FLDALL           'ALL' KEYWORD                                
         BNE   SAEIIF                                                           
         LA    R1,REPDEFH                                                       
         BAS   RE,ECHOALL                                                       
         XC    SELDEFC,FFILL                                                    
         B     VQ028                                                            
VQ026    XC    SELDEFC,APWORK+PVALCSTA-PERVALD                                  
VQ028    MVC   SAPEDEF,SELDEFC                                                  
         MVI   SELDEFF,1           FLAG FILTER SET                              
*                                                                               
VQ030    GOTO1 AFVAL,REPAGRH       VALIDATE ACCESS GROUP FILTER                 
         BNE   VQ040                                                            
         LA    R2,IOKEY                                                         
         USING SAAGREC,R2                                                       
         XC    SAAGKEY,SAAGKEY     CHECK ACCESS GROUP RECORD EXISTS             
         MVI   SAAGTYP,SAAGTYPQ                                                 
         MVI   SAAGSUB,SAAGSUBQ                                                 
         MVC   SAAGAGY,AGENCYID                                                 
         MVC   SAAGAGR,FVIFLD                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         MVC   SELAGR,FVIFLD       SAVE FILTER                                  
*                                                                               
VQ040    GOTO1 AFVAL,REPOFFH       VALIDATE OFFICE CODE FILTER                  
         BNE   VQ050                                                            
         LA    R2,IOKEY                                                         
         USING SAOFREC,R2                                                       
         XC    SAOFKEY,SAOFKEY     CHECK OFFICE RECORD EXISTS                   
         MVI   SAOFTYP,SAOFTYPQ                                                 
         MVI   SAOFSUB,SAOFSUBQ                                                 
         MVC   SAOFAGY,AGENCYID                                                 
         MVC   SAOFOID,FVIFLD                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         MVC   SELOFF,FVIFLD       SAVE OFFICE FILTER                           
*                                                                               
VQ050    GOTO1 AFVAL,REPDIDH       VALIDATE DEPARTMENT CODE FILTER              
         BNE   VQ100                                                            
         OC    SELOFF,SELOFF                                                    
         BZ    SAEODF                                                           
         LA    R2,IOKEY                                                         
         USING SADPREC,R2                                                       
         XC    SADPKEY,SADPKEY     CHECK DEPARTMENT RECORD EXISTS               
         MVI   SADPTYP,SADPTYPQ                                                 
         MVI   SADPSUB,SADPSUBQ                                                 
         MVC   SADPAGY,AGENCYID                                                 
         MVC   SADPOID,SELOFF                                                   
         MVC   SADPDID,FVIFLD                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         MVC   SELDID,FVIFLD       SAVE DEPARTMENT FILTER                       
*                                                                               
*                                  VALIDATE SORT ORDER PARAMETERS               
VQ100    MVI   FVMAXL,1                                                         
         GOTO1 AFVAL,REPSPEH       VALIDATE SORT BY PERSON NAME                 
         BH    VALREQX                                                          
         BL    VQ110               DEFAULT TO PERSONAL-ID                       
         CLI   INWHEN,MIXIOKN      TEST FOR NOW REPORT                          
         BE    SAENOW              'NOW' REPORT CAN'T SORT                      
*                                  CAN BE BY (L)AST OR (F)IRST NAME             
         MVC   FVMSGNO,=AL2(CE#PSRTN)                                           
         ZIC   RF,FVXLEN           MUST BE L OR F                               
         EX    RF,FLDLAST                                                       
         BNE   VQ102                                                            
         OI    SELSRT,X'02'        SAVE SORT OPTION                             
         B     VQ110               SORT LAST NAME                               
VQ102    EX    RF,FLDFIRST         SORT FIRST NAME                              
         BNE   VALREQX             SORT OPTION NOT VALID                        
         OI    SELSRT,X'01'        SAVE SORT OPTION                             
*                                  VALIDATE SORT BY EITHER                      
*                                    SECURITY ACCESS GROUP                      
*                                    OR OFFICE                                  
VQ110    MVI   FVMAXL,1                                                         
         GOTO1 AFVAL,REPSGRH       CHECK FOR SECURITY GROUP SORT                
         BH    VALREQX                                                          
         BL    VQ120                                                            
         GOTO1 AFVAL,REPSOFH         AND OFFICE NOT REQUESTED                   
         BNL   SAEIIF                                                           
         GOTO1 AFVAL,REPSGRH         AND OFFICE NOT REQUESTED                   
         CLI   INWHEN,MIXIOKN      TEST FOR NOW REPORT                          
         BE    SAENOW              'NOW' REPORT CAN'T SORT                      
         ZIC   RF,FVXLEN           MUST BE Y FOR YES                            
         EX    RF,FLDYES                                                        
         BNE   SAEIIF              SORT OPTION NOT VALID                        
         OI    SELSRT,X'10'        SECURITY ACCESS GROUP SORT                   
         B     VQ200                                                            
*                                                                               
VQ120    MVI   FVMAXL,1            CHECK FOR OFFICE SORT                        
         GOTO1 AFVAL,REPSOFH                                                    
         BH    VALREQX                                                          
         BL    VQ200               DEFAULT TO ACROSS AGENCY                     
         CLI   INWHEN,MIXIOKN      TEST FOR NOW REPORT                          
         BE    SAENOW              'NOW' REPORT CAN'T SORT                      
         ZIC   RF,FVXLEN           MUST BE Y FOR YES                            
         EX    RF,FLDYES                                                        
         BNE   SAEIIF              SORT OPTION NOT VALID                        
         OI    SELSRT,X'20'        OFFICE SORT                                  
*                                                                               
VQ200    MVCDD REPDESC,CT#PRSL     SET REPORT DESCRIPTION PARAMETERS            
         GOTO1 VDICTAT,APPARM,C'SL  ',REPDESC                                   
         MVI   REPHEADI,REPHSPAC+REPHCLRA                                       
         MVI   REPMIDSI,REPMSPAC+REPMCLRA                                       
         MVI   REPFOOTN,0                                                       
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS                                                       
         OI    REPIND2,REPILOW                                                  
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VALREQX                                                          
*                                                                               
VALREQX  B     EXIT                                                             
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GENERATE REPORT                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING SAPEREC,R2                                                       
         USING REPD,R4                                                          
PRTREP   L     R4,AREP             POINT TO REPORT BLOCK                        
         MVI   SORTINIT,0          SORT SWITCH                                  
         MVI   LBOXIND,0           INITIALISE BOX PARAMETERS                    
         ICM   RF,15,REPABOX                                                    
         BZ    PR008                                                            
         USING BOXD,RF                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,C'N'                                                      
         MVI   BOXBLANK,C'N'                                                    
         MVI   BOXWT,1                                                          
         OI    BOXDDCTL,BOXDDUL+BOXDDLC                                         
         MVI   BOXROWS,C' '                                                     
         MVC   BOXROWS+1(L'BOXROWS-1),BOXROWS                                   
         MVI   BOXCOLS,C' '                                                     
         MVC   BOXCOLS+1(L'BOXCOLS-1),BOXCOLS                                   
         TM    SELSRT,X'10'+X'20'                                               
         BZ    PR002                                                            
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         B     PR004                                                            
PR002    MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+6,C'M'                                                   
PR004    MVI   BOXROWS+99,C'B'                                                  
         MVI   BOXCOLS+(LBLT-LBOX),C'L'                                         
         TM    SELSRT,X'01'+X'02'                                               
         BZ    *+12                                                             
         MVI   BOXCOLS+(LBC1-LBOX),C'C'                                         
         B     *+8                                                              
         MVI   BOXCOLS+(LBC0-LBOX),C'C'                                         
         MVI   BOXCOLS+(LBC2-LBOX),C'C'                                         
         MVI   BOXCOLS+(LBC3-LBOX),C'C'                                         
         MVI   BOXCOLS+(LBC4-LBOX),C'C'                                         
         MVI   BOXCOLS+(LBRT-LBOX),C'R'                                         
         MVI   LBOXIND,1                                                        
*                                                                               
PR008    LA    R2,IOKEY            READ PERSON RECORDS                          
         MVC   SAPEKEY(L'SAPEKEY),APRECKEY SET INITIAL KEY VALUE                
         LA    R1,IOHI+IOCONFIL+IO1                                             
         B     PR014               GET FIRST RECORD                             
*                                  GET NEXT RECORD (SEQUENCE BROKEN)            
PR010    LA    R2,IOKEY                                                         
         MVC   SAPEKEY(L'SAPEKEY),APRECKEY                                      
         GOTO1 AIO,IORD+IOCONFIL+IO1                                            
         BNE   PR300                                                            
*                                  GET NEXT RECORD (IN SEQUENCE)                
PR012    LA    R1,IOSQ+IOCONFIL+IO1                                             
PR014    GOTO1 AIO                                                              
         BNE   PR300                                                            
*                                                                               
PR020    L     R2,AIOAREA1         VALIDATE KEY DATA                            
         CLI   SAPETYP,SAPETYPQ    CHECK PERSON RECORD TYPE                     
         BNE   PR300                                                            
         CLI   SAPESUB,SAPESUBQ                                                 
         BNE   PR300                                                            
         CLC   SAPEAGY,AGENCYID                                                 
         BNE   PR300                                                            
         CLI   SELDEFF,0           FILTER EFFECTIVE DATE IN KEY                 
         BZ    PR022                                                            
         CLC   SAPEDEF,SELDEFC                                                  
         BL    PR012                                                            
         B     PR024                                                            
PR022    CLC   SAPEDEF,TODAYC                                                   
         BL    PR012                                                            
         CLC   SAPEPID,SAVPID                                                   
         BE    PR012                                                            
         MVC   SAVPID,SAPEPID                                                   
*                                                                               
PR024    MVC   APRECKEY(L'SAPEKEY),SAPEKEY                                      
         MVC   REPSUBPG,SELSRT     CHECK IF IN SORTING MODE                     
         CLI   REPSUBPG,0                                                       
         BE    PR030                                                            
         CLI   SORTINIT,1                                                       
         BE    PR030                                                            
         LA    R1,SORTLEN          IF SO INITIALISE SORT                        
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  RECCARD+22(3),APDUB+6(2)                                         
         GOTO1 TWAOVSRT,APPARM,SORTCARD,RECCARD,0                               
         MVI   SORTINIT,1                                                       
PR030    MVI   SORTKEY,C' '        CLEAR SORTER KEY & RECORD                    
         MVC   SORTKEY+1(SORTLEN-1),SORTKEY                                     
         XC    SORTAGR,SORTAGR                                                  
*                                                                               
         L     R2,AIOAREA1                                                      
         MVC   SORTPID,SAPEPID     PERSONAL-ID                                  
         MVI   APFLAG,0            FLAG FOR PASSWORD RECORD                     
         MVI   SELAGRF,0           FLAG FOR ACCESS GROUP                        
         LA    R3,SAPEDATA                                                      
*                                                                               
*                                  GET ELEMENT DATA INTO SAVE BUFFERS           
PR040    CLI   0(R3),0             TEST END-OF-RECORD                           
         BE    PR090                                                            
         CLI   0(R3),SANAMELQ      TEST NAME ELEMENT                            
         BE    PR060                                                            
         CLI   0(R3),SAPERELQ      TEST TITLE ELEMENT                           
         BE    PR070                                                            
         CLI   0(R3),SAAGCELQ      TEST ACCESS GROUP ELEMENT                    
         BE    PR080                                                            
         CLI   0(R3),SAPWDELQ      TEST PASSWORD POINTER ELEMENT                
         BE    PRPWD                                                            
PR050    SR    R0,R0                                                            
         IC    R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     PR040                                                            
*                                                                               
         USING SANAMD,R3           NAME ELEMENT                                 
PR060    MVI   SAVNAME,C' '                                                     
         MVC   SAVNAME+1(L'SAVNAME-1),SAVNAME                                   
         XC    SAVNAME(2),SAVNAME                                               
         CLI   SANAMLN,SANAMLNQ                                                 
         BL    PR050                                                            
         LA    RE,SANAMELN                                                      
         LA    RF,SAVNAME+2                                                     
         SR    R1,R1                                                            
         TM    SANAMIND,SANAMIFN   FIRST NAME PRESENT                           
         BZ    PR062                                                            
         IC    R1,0(RE)                                                         
         STC   R1,SAVNAME                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SAVNAME+2(0),1(RE)    SAVE FIRST NAME                            
         LA    RF,2(R1,RF)                                                      
         LA    RE,2(R1,RE)         BUMP TO NEXT                                 
PR062    TM    SANAMIND,SANAMIMN   IF THERE IS A MIDDLE NAME                    
         BZ    PR064                                                            
         IC    R1,0(RE)            TAKE LENGTH                                  
         LA    RE,1(R1,RE)         AND BUMP TO NEXT                             
PR064    TM    SANAMIND,SANAMILN   LAST NAME PRESENT                            
         BZ    PR050                                                            
         IC    R1,0(RE)                                                         
         STC   R1,SAVNAME+1                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),1(RE)                                                    
         OC    SELNAM,SELNAM       FILTER ON LAST NAME                          
         BZ    PR050                                                            
         IC    R1,SELNAML                                                       
         SR    RF,RF                                                            
         IC    RF,0(RE)                                                         
         CR    R1,RF                                                            
         BH    PR010                                                            
         MVC   APWORK,CAPFILL                                                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    APWORK(0),1(RE)                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SELNAM(0),APWORK                                                 
         BNE   PR010                                                            
         B     PR050                                                            
*                                                                               
         USING SAPERD,R3           PERSONNEL DETAILS ELEMENT                    
PR070    CLI   SAPERLN,SAPERLNQ                                                 
         BL    PR050                                                            
*&&UK                                                                           
*                                  CHECK OFFICE SECURITY MANAGER                
         GOTO1 ATSTOMAN,SAPEROFF                                                
         BNE   PR070A                                                           
         B     PR070B                                                           
*&&                                                                             
*&&US                                                                           
*                                  CHECK OFFICE/DEPT SECURITY MANAGER           
         GOTO1 ATSTDMAN,APPARM,SAPEROFF,SAPERDID                                
         BE    PR070B                                                           
*&&                                                                             
PR070A   MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     PR010                                                            
PR070B   OC    SELOFF,SELOFF                                                    
         BZ    PR072                                                            
         CLC   SELOFF,SAPEROFF     FILTER OFFICE CODE                           
         BNE   PR010                                                            
PR072    OC    SELDID,SELDID                                                    
         BZ    PR074                                                            
         CLC   SELDID,SAPERDID     FILTER DEPARTMENT CODE                       
         BNE   PR010                                                            
PR074    MVC   SORTEXT,SAPEREXT    EXTENSION NUMBER                             
         MVC   SORTOFF,SAPEROFF    OFFICE                                       
         MVC   SORTDID,SAPERDID    DEPARTMENT                                   
         SR    R1,R1                                                            
         IC    R1,SAPERLN          FULL LENGTH OF TITLE ELEMENT                 
         SH    R1,=Y(SAPERLNQ+1)   MINUS FIXED PORTION LENGTH + 1               
         BM    PR050                                                            
         EX    R1,*+8                                                           
         B     PR050                                                            
         MVC   SORTTIT(0),SAPERTIT TITLE                                        
*                                                                               
         USING SAAGCD,R3           ACCESS GROUP ELEMENT                         
PR080    CLI   SAAGCLN,SAAGCLNQ                                                 
         BL    PR050                                                            
         MVI   SELAGRF,1                                                        
         OC    SELAGR,SELAGR                                                    
         BZ    PR082                                                            
         CLC   SELAGR,SAAGCCOD                                                  
         BNE   PR010                                                            
PR082    MVC   SORTAGR,SAAGCCOD                                                 
         B     PR050                                                            
*                                  PASSWORD POINTER ELEMENT                     
         USING SAPWDD,R3                                                        
PRPWD    OC    SELUSR,SELUSR       ONLY REQUIRED IF USERID FILTER               
         BZ    PR050                                                            
         CLI   SAPWDLN,SAPWDLNQ                                                 
         BL    PR010                                                            
         LA    R2,IOKEY                                                         
         USING SA0REC,R2                                                        
         XC    SA0KEY,SA0KEY       READ PASSWORD RECORD                         
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         MVC   SA0KCODE,SAPWDCOD                                                
         GOTO1 AIO,IORD+IOCONFIL+IO3                                            
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   APFLAG,1            FLAG PASSWORD RECORD FOUND                   
         B     PR050                                                            
         EJECT                                                                  
*                                                                               
*                                  HERE AFTER ELEMENTS READ                     
*                                                                               
PR090    CLI   APFLAG,0            CHECK IF PASSWORD RECORD DATA                
         BE    PR100                 FOUND OK AND FILTER ON DETAILS             
         L     R2,AIOAREA3                                                      
         OC    SELUSR,SELUSR       FILTER ON COMPATIBLE USERID                  
         BZ    PR100                                                            
         MVC   APWORK,SELUSR                                                    
         BAS   RE,FILTUSER         FILTER USER ID COMPATIBLE                    
         BE    PR100                 OK                                         
         B     PR010                 ELSE NEXT RECORD RESTART SEQUENCE          
*                                                                               
         USING SAPEREC,R2                                                       
PR100    L     R2,AIOAREA1                                                      
         CLI   SELAGRF,0           TEST IF ACCESS GROUP ELEMENT FOUND           
         BNE   *+14                                                             
         OC    SELAGR,SELAGR         ELSE CHECK IF FILTERED                     
         BNZ   PR010                                                            
         MVC   APHALF,FFILL        SAVE EFFECTIVE DATE IN SORT KEY              
         XC    APHALF,SAPEDEF                                                   
         XC    APWORK,APWORK                                                    
         GOTO1 VDATCON,APPARM,(2,APHALF),(X'51',APWORK)                         
         MVC   SORTDEF,APWORK                                                   
         MVC   SORTNAM,SAVNAME+2                                                
         MVC   SORTLNAM,SORTNAM    SAVE LOWER CASE NAME                         
         CLI   REPSUBPG,0          CHECK IF NO SORT REQUIRED                    
         BZ    PR200                 IF SO EXIT TO PRINT LINE                   
*                                                                               
*                                  ELSE BUILD SORT KEYS FOR SORT MODE           
         TM    REPSUBPG,X'02'      SORT BY LAST NAME                            
         BZ    PR110                                                            
         XC    APWORK,APWORK                                                    
         SR    R1,R1                                                            
         SR    R2,R2                                                            
         IC    R1,SAVNAME                                                       
         IC    R2,SAVNAME+1                                                     
         LA    RE,SAVNAME+2                                                     
         LA    RE,1(R1,RE)                                                      
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),0(RE)    SWAP ROUND NAMES                              
         LA    RE,APWORK                                                        
         LA    RE,1(R2,RE)                                                      
         MVI   0(RE),C' '                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RE),SAVNAME+2   SWAP ROUND NAMES                             
         MVC   SORTNAM,APWORK                                                   
*                                                                               
PR110    MVC   SORTLNAM,SORTNAM    SAVE ORIGINAL LOWER CASE NAME                
*                                  CONVERT SORT NAME TO UPPERCASE               
         LA    RE,SORTNAM                                                       
         LA    RF,L'SORTNAM(RE)                                                 
PR120    OI    0(RE),X'40'                                                      
         LA    RE,1(RE)                                                         
         CR    RE,RF                                                            
         BL    PR120                                                            
*                                                                               
         LA    R2,SORTKEY                                                       
         TM    REPSUBPG,X'10'      SORT ACCESS GROUP                            
         BZ    PR122                                                            
         OC    SORTAGR,SORTAGR                                                  
         BNZ   *+8                                                              
         MVI   SORTAGR,X'FF'                                                    
         MVC   0(L'SORTAGR,R2),SORTAGR                                          
         LA    R2,L'SORTAGR(R2)                                                 
         B     PR140                                                            
*                                                                               
PR122    TM    REPSUBPG,X'20'      SORT OFFICE/DEPARTMENT                       
         BZ    PR140                                                            
         MVC   0(L'SORTOFF,R2),SORTOFF                                          
         LA    R2,L'SORTOFF(R2)                                                 
         MVC   0(L'SORTDID,R2),SORTDID                                          
         LA    R2,L'SORTDID(R2)                                                 
*                                                                               
PR140    TM    REPSUBPG,X'01'+X'02'                                             
         BNZ   PR160                                                            
         MVC   0(L'SORTPID,R2),SORTPID                                          
         LA    R2,L'SORTPID(R2)                                                 
*                                  PUT RECORD TO SORTER                         
PR160    MVC   0(L'SORTNAM,R2),SORTNAM                                          
         GOTO1 TWAOVSRT,APPARM,=C'PUT',SORTKEY                                  
         B     PR010               GET NEXT PERSON RECORD FROM FILE             
         SPACE 2                                                                
PR200    DS    0H                  PRINT AN UNSORTED LINE                       
         MVC   LINPID1,SORTPID     PERSONAL-ID                                  
         MVC   LINNAM1,SORTLNAM    NAME                                         
         MVC   LINEXT,SORTEXT      EXTENSION                                    
         MVC   LINTIT,SORTTIT      TITLE                                        
         MVC   LINAGR1,SORTAGR     ACCESS GROUP                                 
         MVC   LINOFF1,SORTOFF     OFFICE                                       
         MVC   LINDID1,SORTDID     DEPARTMENT                                   
         MVC   LINDEF,SORTDEF      EFFECTIVE DATE                               
         GOTO1 VREPORT,REPD                                                     
         B     PR010               GET NEXT PERSON RECORD FROM FILE             
         EJECT                                                                  
*                                  HERE AFTER LAST PERSON RECORD READ           
PR300    MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLI   REPSUBPG,0          CHECK IF IN SORT MODE                        
         BZ    PRTREPX               ELSE EXIT PROCESS                          
*                                                                               
*                                  RETRIEVE RECORDS FROM SORTER                 
         MVC   LASTAGR,SPACES      CLEAR LAST ACCESS GROUP                      
         MVC   LASTOFF,SPACES      CLEAR LAST OFFICE                            
         MVC   LASTDID,SPACES      CLEAR LAST DEPARTMENT                        
         CLI   SORTINIT,1          CHECK SORT HAS OCCURRED                      
         BNE   PRTREPX                                                          
*                                  GET SORTED RECORDS                           
PR400    GOTO1 TWAOVSRT,APPARM,=C'GET'                                          
         ICM   RE,15,APPARM+4                                                   
         BZ    PRTREPX             LAST RECORD                                  
         LA    R1,SORTLEN          GET SORT KEY                                 
         LR    RF,R1                                                            
         LA    R0,SORTKEY                                                       
         MVCL  R0,RE                                                            
         TM    REPSUBPG,X'10'      SORT BY ACCESS GROUP                         
         BO    PR500                                                            
         TM    REPSUBPG,X'20'      SORT BY OFFICE/DEPARTMENT                    
         BO    PR600                                                            
         B     PR700                                                            
         EJECT                                                                  
*                                                                               
PR500    CLC   LASTAGR,SORTAGR     SORTING WITHIN ACCESS GROUP                  
         BE    PR700                                                            
         LA    R2,IOKEY            R2=A(ACCESS GROUP KEY)                       
         USING SAAGREC,R2                                                       
         XC    SAAGKEY,SAAGKEY     BUILD GROUP KEY AND READ RECORD              
         MVI   SAAGTYP,SAAGTYPQ                                                 
         MVI   SAAGSUB,SAAGSUBQ                                                 
         MVC   SAAGAGY,AGENCYID                                                 
         MVC   SAAGAGR,SORTAGR                                                  
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BNE   PR510                                                            
         L     R1,AIOAREA2         DISPLAY GROUP NAME                           
         GOTO1 AGETGNAM                                                         
         MVC   REPH5+26(L'SAAGNNAM),APWORK                                      
PR510    OI    REPHEADI,REPHFRCE                                                
         MVC   LASTAGR,SORTAGR                                                  
         B     PR700                                                            
         EJECT                                                                  
*                                                                               
PR600    CLC   LASTOFF,SORTOFF     SORTING WITHIN OFFICE                        
         BE    PR640                                                            
         MVC   LASTDID,SPACES                                                   
         LA    R2,IOKEY            R2=A(OFFICE KEY)                             
         USING SAOFREC,R2                                                       
         XC    SAOFKEY,SAOFKEY     BUILD OFFICE KEY AND READ RECORD             
         MVI   SAOFTYP,SAOFTYPQ                                                 
         MVI   SAOFSUB,SAOFSUBQ                                                 
         MVC   SAOFAGY,AGENCYID                                                 
         MVC   SAOFOID,SORTOFF                                                  
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BNE   PR610                                                            
         L     R1,AIOAREA2         DISPLAY OFFICE NAME                          
         GOTO1 AGETONAM                                                         
         MVC   REPH5+10(L'SAOFFNAM),APWORK                                      
PR610    OI    REPHEADI,REPHFRCE                                                
         MVC   LASTOFF,SORTOFF                                                  
         EJECT                                                                  
*                                                                               
PR640    CLC   LASTDID,SORTDID     SORTING WITHIN DEPARTMENT                    
         BE    PR700                                                            
         SR    RE,RE                                                            
         IC    RE,REPLINE                                                       
         LA    RE,5(RE)                                                         
         CLM   RE,1,REPMAXL                                                     
         BNH   PR660                                                            
         OI    REPHEADI,REPHFRCE                                                
         B     PR670                                                            
PR660    CLC   LASTDID,SPACES                                                   
         BE    PR670                                                            
         ICM   RF,15,REPABOX                                                    
         BZ    *+8                                                              
         MVI   BOXREQ-BOXD(RF),C'B'                                             
         GOTO1 VREPORT,REPD                                                     
PR670    MVC   LASTDID,SORTDID                                                  
         B     PR700                                                            
         EJECT                                                                  
*                                  DISPLAY STANDARD LINE                        
PR700    MVC   LINTIT,SORTTIT                                                   
         MVC   LINAGR1,SORTAGR                                                  
         MVC   LINOFF1,SORTOFF                                                  
         MVC   LINDID1,SORTDID                                                  
         MVC   LINEXT,SORTEXT                                                   
         MVC   LINDEF,SORTDEF                                                   
         TM    REPSUBPG,X'01'+X'02'                                             
         BNZ   PR710                                                            
         MVC   LINPID1,SORTPID     PERSONAL-ID                                  
         MVC   LINNAM1,SORTLNAM    NAME                                         
         B     PR720                                                            
PR710    MVC   LINNAM2,SORTLNAM    NAME POSITION 2                              
         MVC   LINPID2,SORTPID     PERSONAL-ID POSITION 2                       
*                                                                               
PR720    GOTO1 VREPORT,REPD        PRINT THE LINE                               
         SR    RE,RE                                                            
         IC    RE,REPLINE                                                       
         SR    RF,RF                                                            
         ICM   RF,1,REPMAXL                                                     
         LA    RE,2(RE)                                                         
         CR    RE,RF                                                            
         BL    PR400                                                            
         OI    REPHEADI,REPHFRCE                                                
         MVC   LASTOFF,SPACES      FORCE OFFICE ON NEW PAGE                     
         MVC   LASTDID,SPACES      FORCE DEPARTMENT ON NEW PAGE                 
         MVC   LASTAGR,SPACES      FORCE ACCESS GROUP ON NEW PAGE ?             
         B     PR400               GET NEXT SORTED RECORD                       
         SPACE 2                                                                
PRTREPX  ICM   RF,15,REPABOX                                                    
         BZ    *+8                                                              
         MVI   BOXREQ-BOXD(RF),C'C'                                             
         GOTO1 VREPORT,REPD        PRINT LAST LINE                              
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
*                                                                               
         DROP  R3,R4,RF                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT KEY OF RECORD TO SAVED KEY TABLE                     *         
***********************************************************************         
         SPACE 1                                                                
         USING SAPEREC,R2                                                       
PUTKEY   LA    R2,APRECKEY                                                      
         LA    R3,APELEM                                                        
         SR    R0,R0                                                            
*                                                                               
         MVI   0(R3),KEYPID                                                     
         MVI   1(R3),10                                                         
         MVC   2(8,R3),SAPEPID                                                  
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
         MVI   0(R3),0                                                          
         GOTO1 APUTKEY                                                          
*                                                                               
PUTKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FIND IF USER ID IN APWORK IS COMPATIBLE WITH RECORD      *         
* R2 = A(RECORD)                                                      *         
***********************************************************************         
         SPACE 1                                                                
FILTUSER NTR1                      BUILD COMPATIBLE ID TABLE                    
         GOTO1 VGETIDS,APPARM,(C'C',(R2)),ATIA,VDMGR                            
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'00'               IO ERROR                                     
         CLI   0(R1),0                                                          
         BE    YES                 NULL TABLE SO CAN ACCESS ANY                 
         L     R1,4(R1)                                                         
FUSER010 CLI   0(R1),X'FF'         TEST E-O-L                                   
         BE    NO                                                               
         CLC   0(10,R1),=CL10'ALL'                                              
         BNE   FUSER020                                                         
         TM    CUSTAT,CUSDDS       ONLY VALID FOR DDS TERMINALS                 
         BZ    FUSER030                                                         
         B     YES                                                              
FUSER020 CLC   0(10,R1),APWORK     MATCH ID WITH TABLE                          
         BE    YES                                                              
FUSER030 LA    R1,12(R1)           GET NEXT TABLE ENETRY                        
         B     FUSER010                                                         
         EJECT                                                                  
*                                  GETTXT MESSAGE # ERROR EXITS                 
SAEIIF   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  INPUT FIELD ERROR                            
SAEFTL   MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     NO                  INPUT FIELD TOO LONG                         
SAEFNN   MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     NO                  INPUT FIELD NOT NUMERIC                      
SAEFTS   MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     NO                  INPUT FIELD TOO SHORT                        
SAEFNH   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  INPUT FIELD ERROR                            
SAEMIF   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     NO                  MISSING FIELD                                
SAEIIO   MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     NO                  I/O ERROR                                    
SAERNF   MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     NO                  RECORD NOT FOUND                             
SAEDIF   MVC   FVMSGNO,=AL2(FVFDUPE)                                            
         B     NO                  DUPLICATE                                    
SAERAE   MVC   FVMSGNO,=AL2(FVFERAE)                                            
         B     NO                  ALREADY EXISTS                               
SAERTB   MVC   FVMSGNO,=AL2(CE#RECTB)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  RECORD TOO BIG                               
SAEODF   MVC   FVMSGNO,=AL2(CE#OFFDP)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  MUST ENTER OFFICE CODE FOR DEPT              
SAEPWR   MVC   FVMSGNO,=AL2(CE#PWDRM)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  PASSWORD HAS BEEN REMOVED                    
SAEPED   MVC   FVMSGNO,=AL2(CE#PIDED)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  CAN NOT CHANGE PID AND EFF. DATE             
SAEPEA   MVC   FVMSGNO,=AL2(CE#PIDAE)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  PERSONAL-ID RECORD ALREADY EXISTS            
SAEPWA   MVC   FVMSGNO,=AL2(CE#PWDAE)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  PASSWORD RECORD ALREADY EXISTS               
SAEFED   MVC   FVMSGNO,=AL2(CE#FLDED)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  CAN NOT CHANGE FIELD WITH FUTURE ED.         
SAEFPE   MVC   FVMSGNO,=AL2(CE#FLDPC)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  CAN NOT CHANGE FIELD/PENDING RECORD          
SAEPWD   MVC   FVMSGNO,=AL2(CE#PWDEL)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  PASSWORD RECORD DELETED                      
SAEPEC   MVC   FVMSGNO,=AL2(CE#PIDCH)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  PERSONAL ID HAS INCOMPATIBLE PWD             
SAEPWC   MVC   FVMSGNO,=AL2(CE#PWDCH)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  PASSWORD HAS INCOMPATIBLE PID                
SAETST   MVC   FVMSGNO,=AL2(CE#TESTM)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  TEST MESSAGE                                 
SAEED1   MVC   FVMSGNO,=AL2(CE#DEFIN)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  INVALID EFFECTIVE DATE                       
SAEED2   MVC   FVMSGNO,=AL2(CE#DEFAC)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  INVALID EFFECTIVE DATE FOR ACTION            
SAEED3   MVC   FVMSGNO,=AL2(CE#DEFUP)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  EFFECTIVE DATE FUTURE FOR CHANGE             
SAELST   MVC   FVMSGNO,=AL2(CE#IDLST)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  WRONG LIST=NAME FORMAT                       
SAEINF   MVC   FVMSGNO,=AL2(CE#IDRNF)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  USER ID RECORD NOT FOUND                     
SAEWNF   MVC   FVMSGNO,=AL2(CE#WLRNF)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  USER ID LIST RECORD NOT FOUND                
SAEFPI   MVC   FVMSGNO,=AL2(CE#FEPID)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  FIRST ENTRY MUST BE PID                      
SAEACT   MVC   FVMSGNO,=AL2(CE#AGICT)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  USER ID AGENCY NOT SAME AS CONNECT           
SAEGI0   MVC   FVMSGNO,=AL2(CE#GID00)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  GETID EXIT CODE 00                           
SAEGIF   MVC   FVMSGNO,=AL2(CE#GIDFF)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  GETID EXIT CODE FF                           
SAEINC   MVC   FVMSGNO,=AL2(CE#IDNPL)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  USER ID NOT IN PID COMPATIBLE LIST           
SAELNC   MVC   FVMSGNO,=AL2(CE#LINPL)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  LIST ID NOT IN PID COMPATIBLE LIST           
SAENOW   MVC   FVMSGNO,=AL2(CE#NONOW)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  CANT PRINT IN NOW MODE                       
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         B     EXIT                                                             
*                                                                               
ECHOALL  MVC   FVIFLD-FVIHDR(L'CT@ALL,R1),CT@ALL                                
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         BR    RE                                                               
*                                  EXECUTE TYPE INSTRUCTIONS                    
*                                    INPUT FIELD KEYWORD COMPARISONS            
FLDLAST  CLC   FVIFLD(0),CT@LAST                                                
FLDFIRST CLC   FVIFLD(0),CT@FIRST                                               
FLDNOW   CLC   FVIFLD(0),CT@NOW                                                 
FLDCURR  CLC   FVIFLD(0),CT@CURNT                                               
FLDALL   CLC   FVIFLD(0),CT@ALL                                                 
FLDDPT   CLC   FVIFLD(0),CT@DPT                                                 
FLDACCS  CLC   FVIFLD(0),CT@ACCS                                                
FLDYES   CLC   FVIFLD(0),CT@YES                                                 
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
CTFILE   DC    C'CTFILE '                                                       
SPACES   DC    32C' '                                                           
FFILL    DC    32X'FF'                                                          
CAPFILL  DC    (L'APWORK)X'40'                                                  
SORTCARD DC    CL80'SORT FIELDS=(1,80,A),FORMAT=BI'                             
RECCARD  DC    C'RECORD TYPE=F,LENGTH=(000,,,,)  '                              
         EJECT                                                                  
REPSPEC  DS    0X                  REPORT HEADING SPECIFICATIONS                
         SPROG X'00',X'01',X'02',X'10',X'11',X'12',X'20',X'21',X'22'            
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,CT#PRSL,30,L                                               
         SPEC  H2,57,CT#PRSL,30,LU                                              
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  M1,57,CT#EXTN,4,L                                                
         SPEC  M1,65,CT#JOBT,30,L                                               
         SPEC  M1,120,CT#EFFD,12,L                                              
         SPEC  M1,98,CT#GROUP,9,L                                               
         SPEC  M1,107,CT#OFF,3,L                                                
         SPEC  M1,112,CT#DPT,4,L                                                
*                                                                               
         SPROG X'00',X'10',X'20'                                                
         SPEC  M1,3,CT#PID,9,L                                                  
         SPEC  M1,14,CT#NAME,6,L                                                
         SPROG X'01',X'11',X'21'                                                
         SPEC  M1,3,CT#NAME,6,L                                                 
         SPROG X'02',X'12',X'22'                                                
         SPEC  M1,3,CT#NAME,6,L                                                 
         SPROG X'01',X'11',X'21',X'02',X'12',X'22'                              
         SPEC  M1,46,CT#PID,9,L                                                 
         SPROG X'10',X'11',X'12'                                                
         SPEC  H5,1,CT#SECGN,24,L                                               
         SPROG X'20',X'21',X'22'                                                
         SPEC  H5,1,CT#OFF,8,L                                                  
         SPEC  END                                                              
* SEACSDICT                                                                     
       ++INCLUDE SEACSDICT                                                      
         EJECT                                                                  
* SEACSWRK                                                                      
       ++INCLUDE SEACSWRK                                                       
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSB7D                                                       
         ORG                                                                    
         EJECT                                                                  
*                                  WORKING STORAGE SAVED IN TWA                 
         ORG   SAVOVER                                                          
         SPACE 2                                                                
REPD     DSECT                     ** DSECT COVERS PRINT LINE **                
         ORG   REPP1                                                            
LBOX     DS    0X                                                               
LBLT     DS    C                                                                
         DS    C                                                                
LINPID1  DS    CL(L'SORTPID)       PERSONAL ID (POSITION 1)                     
         DS    C                                                                
LBC0     DS    C                                                                
         DS    C                                                                
LINNAM1  DS    CL(L'SORTLNAM)      NAME (POSITION 1)                            
         ORG   LINPID1                                                          
LINNAM2  DS    CL(L'SORTLNAM)      NAME (POSITION 2)                            
         DS    C                                                                
LBC1     DS    C                                                                
         DS    C                                                                
LINPID2  DS    CL(L'SORTPID)       PERSONAL ID (POSITION 2)                     
         DS    C                                                                
LBC2     DS    C                                                                
         DS    C                                                                
LINEXT   DS    CL(L'SORTEXT)       EXTENSION NUMBER                             
         DS    C                                                                
LBC3     DS    C                                                                
         DS    C                                                                
LINTIT   DS    CL(L'SORTTIT)       TITLE                                        
         DS    C                                                                
LBC4     DS    C                                                                
         DS    C                                                                
LINAGR1  DS    CL(L'SORTAGR)       ACCESS GROUP                                 
         DS    C                                                                
LINOFF1  DS    CL(L'SORTOFF)       OFFICE                                       
         DS    CL3                                                              
LINDID1  DS    CL(L'SORTDID)       DEPARTMENT                                   
         DS    CL3                                                              
LBC5     DS    C                                                                
         DS    C                                                                
LINDEF   DS    CL(L'SORTDEF)       EFFECTIVE DATE                               
         DS    C                                                                
LBRT     DS    C                                                                
         EJECT                                                                  
WORKD    DSECT                                                                  
         ORG   APLOCAL             ** DSECT TO COVER LOCAL W/S **               
SAVPARM  DS    8F                                                               
RETURN   DS    A                                                                
SA0KEYSV DS    XL(L'APRECKEY)      PASSWORD RECORD KEY SAVE                     
PIDIOERR DS    XL(L'IOERR)         PERSON RECORD IOERR                          
PWDIOERR DS    XL(L'IOERR)         PASSWORD RECORD IOERR                        
PIDREAD  DS    XL1                 PERSON RECORD READ PASS FLAG                 
PWDREAD  DS    XL1                 PASSWORD RECORD READ PASS FLAG               
PIDIND   DS    XL1                 PERSONAL ID CODE INPUT FLAG                  
PWDIND   DS    XL1                 PASSWORD CODE INPUT FLAG                     
TODAY    DS    XL2                 TODAYS DATE COMPRESSED BINARY                
TODAYC   DS    XL2                 TODAYS DATE COMPLEMENT COMPRESSED            
*                                                                               
AGENCYID DS    XL2                 AGENCY ALPHA ID                              
*                                                                               
GETSEQF  DS    XL1                 GETSEL READ SEQUENCE BROKEN FLAG             
FLDCNT   DS    XL1                 SCREEN FIELD COUNTER                         
COUNT    DS    XL1                 COUNTER                                      
*                                                                               
SAVPID   DS    CL(L'SAPEPID)       SAVE LAST PID ENTERED FOR LIST               
SAVPWD   DS    CL(L'SA0KCODE)      SAVE LAST PWD READ FOR LIST                  
*                                                                               
SELOPT   DS    0X                  SELECT OPTIONS                               
SELPID   DS    CL(L'SAPEPID)       PERSONAL ID FILTER                           
SELPIDL  DS    CL1                 PERSONAL ID FILTER LENGTH                    
SELPIDSP DS    CL1                 PERSONAL ID SPECIAL CHAR                     
SELKEYCL DS    CL1                                                              
SELNAM   DS    CL20                SURNAME FILTER                               
SELNAML  DS    XL1                 SURNAME FILTER LENGTH                        
SELPWD   DS    CL10                PASSWORD CODE FILTER SAVE                    
SELPWDF  DS    XL1                 PASSWORD FILTER FLAG                         
SELDEFC  DS    XL2                 EFFECTIVE DATE COMPLEMENT FILTER             
SELDEFF  DS    XL1                 EFFECTIVE DATE PRESENT ONLY FLAG             
SELUSR   DS    CL(L'SAID)          USER ID FILTER                               
SELAGR   DS    CL8                 ACCESS GROUP FILTER                          
SELAGRF  DS    XL1                 ACCESS GROUP FILTER FLAG                     
SELOFF   DS    CL2                 OFFICE CODE FILTER                           
SELDID   DS    CL3                 DEPARTMENT ID CODE FILTER                    
SELSRT   DS    CL1                 SORT SEQUENCE:                               
*                                  C'1' - PERSONAL ID WITHIN DEPT               
*                                  C'2' - FIRST NAME                            
*                                  C'3' - FIRST NAME WITHIN DEPT                
*                                  C'4' - LAST NAME                             
*                                  C'5' - LAST NAME WITHIN DEPT                 
SELOPTL  EQU   *-SELOPT                                                         
*                                                                               
*                                  ** SORTER KEY & RECORD **                    
SORTKEY  DS    CL(L'SORTOFF+L'SORTDID+L'SORTPID+L'SORTNAM+L'SORTAGR)            
SORTPID  DS    CL(L'SAPEPID)       PERSONAL ID                                  
SORTNAM  DS    CL40                NAME (CONVERTED TO UPPER CASE)               
SORTLNAM DS    CL40                NAME (ORIGINAL LOWER CASE)                   
SORTEXT  DS    CL(L'SAPEREXT)      EXTENSION NUMBER                             
SORTOFF  DS    CL(L'SAPEROFF)      DEPARTMENT                                   
SORTDID  DS    CL(L'SAPERDID)      DEPARTMENT                                   
SORTTIT  DS    CL30                TITLE                                        
SORTAGR  DS    CL(L'SAAGCCOD)      ACCESS GROUP CODE                            
SORTDEF  DS    CL10                EFFECTIVE DATE                               
SORTLEN  EQU   *-SORTKEY                                                        
*                                                                               
SORTINIT DS    XL1                 SORTER INITIALISED SWITCH                    
LBOXIND  DS    XL1                 BOX SET INDICATOR                            
LASTAGR  DS    CL(L'SELAGR)        LAST ACCESS GROUP                            
LASTOFF  DS    CL(L'SELOFF)        LAST OFFICE                                  
LASTDID  DS    CL(L'SELDID)        LAST DEPARTMENT                              
SAVNAME  DS    CL(L'APWORK)        PERSON NAME SAVE BUFFER                      
*                                                                               
BLOCK    DS    20CL32              SCANNER BLOCKS                               
BLOCK1   DS    20CL32                                                           
BLOCK2   DS    20CL32                                                           
BLKCNT   DS    XL1                                                              
*                                                                               
LOCALX   EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SEACS11X  05/01/02'                                      
         END                                                                    
