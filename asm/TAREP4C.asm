*          DATA SET TAREP4C    AT LEVEL 043 AS OF 12/16/13                      
*PHASE T7034CC,*                                                                
*INCLUDE TAADDCON                                                               
*INCLUDE DLFLD                                                                  
         TITLE 'T7034C - NEW HIRE REPORT'                                       
T7034C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7034C                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(CONTROLLER W/S)                         
         L     RA,ATWA                                                          
         USING T703FFD,RA          RA=A(SCREEN)                                 
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          R9=A(SYSTEM W/S)                             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           R8=A(SPOOL DSECT)                            
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING TNHD,R7             R7=A(LOCAL W/S)                              
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
*                                                                               
         MVC   AMASTD,TWAMASTC     SAVE ADDRESS OF MASTER                       
*                                                                               
         CLI   MODE,VALREC         VALIDATE SCREEN                              
         BE    *+12                                                             
         CLI   MODE,DISPREC                                                     
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
         CLI   MODE,PRINTREP       PROCESS REPORT                               
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
*                                                                               
         BAS   RE,PREPD            DOWNLOAD DATA TO PQ                          
         B     XIT                                                              
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO VALIDATE KEY                                          
         SPACE                                                                  
VKEY     NTR1                                                                   
         LA    R2,SCREMPH          EMPLOYER                                     
         CLI   5(R2),0                                                          
         BE    FLDMISS             REQUIRE INPUT                                
         GOTO1 RECVAL,DMCB,TLEMCDQ,(R2),0                                       
*                                                                               
         LA    R2,SCRPERH          VALIDATE PERIOD                              
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         GOTO1 PDVAL,DMCB,(R3)                                                  
         MVC   NHPER,PVALCPER      SAVE PRINTABLE PERIOD                        
         MVC   NHSTR,PVALPSTA                                                   
         MVC   NHEND,PVALPEND                                                   
*                                                                               
         BAS   RE,VALOPT           VALIDATE OPTIONS                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE OPTIONS FIELD                                
         SPACE                                                                  
VALOPT   NTR1                                                                   
         LA    R2,SCROPTH          VALIDATE OPTIONS                             
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)            NUMBER OF SCANNER ENTRIES                    
*                                                                               
VOPT10   CLC   =C'TAPE',SCDATA1                                                 
         BNE   VOPT20                                                           
         CLI   SCDATA2,C'Y'                                                     
         BE    VOPTNEXT                                                         
         CLI   SCDATA2,C'N'                                                     
         BNE   FLDINV                                                           
         OI    NHOPTS,NHNOTAPE     DON'T GENERATE A TAPE                        
         B     VOPTNEXT                                                         
*                                                                               
VOPT20   CLC   =C'TRACE',SCDATA1                                                
         BNE   FLDINV                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   FLDINV                                                           
         OI    NHOPTS,NHTRACE      TRACE RECORDS                                
         B     VOPTNEXT                                                         
*                                                                               
VOPTNEXT LA    R3,SCANNEXT         BUMP TO NEXT                                 
         BCT   R0,VOPT10                                                        
*                                                                               
VOPTX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO GET RECORDS AND PRINT REPORT                          
         SPACE                                                                  
PREP     NTR1                                                                   
         L     R2,=A(SSNTAB)                                                    
         ST    R2,SSNENT           SAVE A(NEXT ENTRY IN SSNTAB)                 
         ZAP   TOTCOUNT,=P'0'      RECORD COUNTER FOR TAPE                      
*                                                                               
         L     R2,TWADCONS                                                      
         USING TWADCOND,R2                                                      
         MVC   PRNTBL,TPRNTBL      A(PRNTBL)                                    
         MVC   DYNALLOC,TDYNALLO   A(DYNALLOC)                                  
         MVC   ALOGOC,TLOGOC       A(LOGOC)                                     
         DROP  R2                                                               
*                                                                               
         L     R2,AMASTD                                                        
         USING MASTD,R2                                                         
         MVC   ALOGO,MCVLOGO       A(LOGO)                                      
         MVC   AREMOT,MCVREMOT     A(REMOTE)                                    
         DROP  R2                                                               
*                                                                               
         LA    R1,MYSPECS          SET A(SPECS) FOR PRINTING                    
         ST    R1,SPECS                                                         
         LA    R1,HDHOOK           SET A(HEADLINE HOOK)                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         USING TLCKPD,R3                                                        
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLCKPCD,TLCKECDQ    READ EMPLOYEE'S CHECK RECORDS                
PREP5    MVC   SYSFIL,=CL8'CHKFIL' SET TO USE CHECK FILE                        
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         GOTO1 HIGH                                                             
PREP6    CLI   TLCKPCD,TLCKECDQ    TEST STILL CORRECT POINTER                   
         BNE   PREP40                                                           
         CLI   TLCKECUR,C'U'       TEST IT IS US$                               
         BE    *+12                                                             
         MVI   TLCKEEMP,X'FF'      IF NOT, SKIP TO NEXT CURRENCY                
         B     PREP5                                                            
         CLC   TLCKEEMP,TGEMP      TEST EMPLOYER MATCHES INPUT                  
         BE    *+12                                                             
         MVI   TLCKEDTE,X'FF'      IF NOT, SKIP TO NEXT EMPLOYER                
         B     PREP5                                                            
         CLC   SVSSN,TLCKESSN      IF ALREADY PROCESSED THIS SSN                
         BNE   *+12                                                             
PREP9    MVI   TLCKECUR,X'FF'      SKIP TO NEXT SSN                             
         B     PREP5                                                            
         CLI   TLCKEDTE,0          ELSE BRANCH IF HAVE CHECK DATE               
         BNE   PREP11                                                           
         MVC   TLCKEDTE,=X'010000' IF NO CHECK DTE, SKIP TO ONE WITH IT         
         B     PREP5                                                            
PREP11   MVC   FULL(3),TLCKEDTE    SAVE CHECK DATE IN FULL                      
         XC    FULL(3),=3X'FF'     UNCOMPLEMENT SAVED CHECK DATE                
         SPACE                                                                  
         CLC   FULL(3),NHSTR       IF CHECK DATE BEFORE PERIOD START            
         BL    PREP9               BUMP TO NEXT SSN                             
         CLC   FULL(3),NHEND       IF CHECK DATE IS AFTER PERIOD END            
         BNH   PREP14                                                           
         GOTO1 SEQ                 BUMP TO NEXT CHECK RECORD                    
         B     PREP6               AND CONTINUE LOOKING                         
         SPACE                                                                  
PREP14   MVC   SVSSN,TLCKESSN      SAVE THIS SSN                                
         MVC   SVDOH,FULL          SAVE DATE OF HIRE                            
*                                                                               
         GOTO1 GETREC                                                           
                                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TATUELQ      GET TAX UNIT DETAILS                         
         BAS   RE,GETEL                                                         
         B     PREP14B                                                          
PREP14A  BAS   RE,NEXTEL                                                        
PREP14B  BNE   PREP15              NONE, LOOK FOR CAST TAX UNIT                 
         USING TATUD,R4                                                         
         CLC   =C'FD ',TATUUNIT    SKIP FEDERAL                                 
         BE    PREP14A                                                          
         MVC   SVWST,TATUUNIT                                                   
         B     PREP18                                                           
                                                                                
PREP15   L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ      GET CAST DETAILS                             
         BAS   RE,GETEL                                                         
         B     PREP15B                                                          
PREP15A  BAS   RE,NEXTEL                                                        
PREP15B  BNE   PREP18              NONE, LOOK FOR CAST TAX UNIT                 
         USING TACAD,R4                                                         
         CLC   =C'FD ',TACAUNIT    SKIP FEDERAL                                 
         BE    PREP15A                                                          
         MVC   SVWST,TACAUNIT                                                   
*                                                                               
PREP18   BRAS  RE,CTY2STAT         CONVERT CITY TO STATE                        
PREP19   ZIC   R1,TLCKEDTE+2                                                    
         LA    R1,1(R1)            BUMP TO NEXT CHECK DATE                      
         STC   R1,TLCKEDTE+2                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(TLCKEDTE-TLCKPD),KEYSAVE  SEE IF SAME UPTO DATE              
         BNE   PREP20              IF NOT, ALREADY AT NEXT SSN/CURR/EMP         
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(1,FULL),(0,DUB)                                     
         GOTO1 ADDAY,DMCB,DUB,WORK,F'-180'  CALC 180 DAYS BEFORE IT             
         GOTO1 DATCON,DMCB,(0,WORK),(1,TGFULL)                                  
         XC    TGFULL(3),=3X'FF'   COMPLEMENT RETURNED CHECK DATE               
         CLC   TLCKEDTE,TGFULL                                                  
         BNH   PREP9               NO GOOD, SKIP TO NEXT SSN NOW                
         MVI   TLCKECUR,X'FF'      ELSE SET TO SKIP TO NEXT SSN LATER           
         SPACE                                                                  
PREP20   MVC   SVKEY,KEY           SAVE KEY                                     
         MVC   SYSFIL,SVSYSFIL     RESET TO TALFIL                              
         MVC   SYSDIR,SVSYSDIR                                                  
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',SVSSN)  GET W4 RECORD                 
         BE    *+6                                                              
         DC    H'0'                DIE IF NOT FOUND                             
         L     R4,AIO                                                           
         MVI   ELCODE,TAW4ELQ      GET W4 DETAILS EL                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                DIE IF NOT FOUND                             
         USING TAW4D,R4                                                         
         CLI   TAW4TYPE,TAW4TYIN   ONLY WANT INDIVIDUALS                        
         BNE   PREP25                                                           
         TM    TAW4STA2,TAW4SFGN   IF FOREIGN ADDRESS                           
         BO    PREP25              PROCESS NEXT RECORD                          
         SPACE                                                                  
         GOTO1 MYTRACE,DMCB,=C'GOOD SSN',SVSSN,9                                
         GOTO1 MYTRACE,DMCB,=C'NEXT CHK KEY',SVKEY,32                           
         SPACE                                                                  
         L     R2,SSNENT           R2=A(NEXT ENTRY)                             
         CLI   0(R2),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                DIE IF END OF TABLE-INCREASE MAXSSN          
         PACK  DUB,SVSSN           CONVERT TO BINARY                            
         CVB   R1,DUB                                                           
         ST    R1,0(R2)                                                         
         MVC   4(L'SVDOH,R2),SVDOH                                              
         MVC   7(L'SVWST-1,R2),SVWST                                            
         AHI   R2,9                BUMP TO NEXT ENTRY                           
         ST    R2,SSNENT           SAVE A(NEXT ENTRY)                           
         AP    TOTCOUNT,=P'1'      INCREMENT COUNT                              
         SPACE                                                                  
PREP25   MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         B     PREP5               READ TO PROCESS THIS CHECK RECORD            
         SPACE                                                                  
PREP40   MVC   SYSFIL,SVSYSFIL     RESET TO TALFIL                              
         MVC   SYSDIR,SVSYSDIR                                                  
         CP    TOTCOUNT,=P'0'      IF HAVE AT LEAST ONE NEW HIRE                
         BE    PREPX                                                            
         LA    R3,TAPEREC          R3=A(TAPE AREA)                              
         BAS   RE,OPENTAPE         OPEN THE TAPE                                
         BAS   RE,PROCHEAD         PROCESS HEADER RECORD IF NEEDED              
         BRAS  RE,GTEMP            GET EMPLOYER INFO                            
         BAS   RE,GTSSN            GET INFO FOR SSNTAB AND PUT ON TAPE          
         BAS   RE,PRNTOT           PRINT TOTAL NUMBER OF NEW HIRES              
         BAS   RE,CLOSTAPE         CLOSE THE TAPE                               
PREPX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PUTS A HEADER RECORD TO TAPE IF NECESSARY                
         SPACE                                                                  
         USING RECPGD,R3           R3=A(TAPE RECORD)                            
PROCHEAD NTR1                                                                   
         CLC   TGEMP,=C'PG '       ONLY IF EMPLOYER IS PG                       
         BNE   XIT                                                              
         MVI   RPGTYPE,C'1'        SET REC TYPE 1                               
         MVC   RPGDATE,TGTODY20    TODAYS DATE CCYYMMDD                         
         EDIT  (P6,TOTCOUNT),(9,RPGCOUNT),FILL=0                                
         MVC   RPG1SP1,SPACES                                                   
         MVC   RPG1SP2,SPACES                                                   
         MVC   RPG1SP3,SPACES                                                   
         MVC   RPG1SP4,SPACES                                                   
         MVC   RPG1SP5,SPACES                                                   
         MVC   RPG1SP6,SPACES                                                   
         MVC   RPG1SP7,SPACES                                                   
         BAS   RE,PUTTAPE                                                       
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE GETS SORTER RECORDS/PUTS TO TAPE/PRINTS REPORT           
         SPACE                                                                  
         USING PRNTD,R2                                                         
GTSSN    NTR1                                                                   
         L     R4,=A(SSNTAB)       R4=A(CURRENT ENTRY IN SSNTAB)                
         ZAP   DUB,TOTCOUNT                                                     
         CVB   R1,DUB              R1=NUMBER OF ENTRIES                         
         LA    R2,P                R2=A(PRINT LINE)                             
         LA    R0,NCHUNKS          R0=A(NUMBER OF CHUNKS)                       
         SPACE                                                                  
GETSSN5  ICM   RE,15,0(R4)                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TGSSN,DUB           CONVERT TO EBCDIC                            
         MVC   SVDOH,4(R4)         HIRE DATE                                    
         MVC   SVWST,SPACES                                                     
         MVC   SVWST(L'SVWST-1),7(R4)         WORK STATE                        
                                                                                
         BAS   RE,PROCW4           PROCESS THE W4 REC                           
*                                                                               
         MVC   PRTW4SSN,TGSSN      MOVE INFO TO PRINT LINE                      
         MVC   PRTW4FST,WORK                                                    
         MVC   PRTW4LST,WORK+16                                                 
         MVC   PRTW4MID,WORK+32                                                 
         LA    R2,PRTLNQ(R2)       PT TO NEXT CHUNK                             
         BCT   R0,GETSSN10                                                      
         BAS   RE,PRNTIT           PRINT THE LINE                               
         LA    R2,P                R2=A(PRINT LINE)                             
         LA    R0,NCHUNKS          R0=A(NUMBER OF CHUNKS)                       
*                                                                               
GETSSN10 AHI   R4,9                BUMP TO NEXT ENTRY                           
         BCT   R1,GETSSN5          DECREMENT COUNT                              
*                                                                               
         CLC   P,SPACES            IF SOMETHING LEFT TO PRINT                   
         BE    *+8                                                              
         BAS   RE,PRNTIT           PRINT IT                                     
         B     XIT                                                              
         SPACE 3                                                                
*              ROUNTINE GETS THE W4 RECORD INFO AND PUTS TO TAPE                
         SPACE                                                                  
PROCW4   NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',TGSSN)  GET W4 RECORD                 
         BE    *+6                                                              
         DC    H'0'                DIE IF NOT FOUND                             
         L     R4,AIO                                                           
         MVI   ELCODE,TAW4ELQ      GET W4 DETAILS EL                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                DIE IF NOT FOUND                             
         USING TAW4D,R4                                                         
         MVC   WORK(16),TAW4NAM1    FIRST NAME                                  
         MVC   WORK+16(16),TAW4NAM2 LAST NAME                                   
         CLI   TAW4LEN,TAW4LN2Q     IF ELEMENT IS NEW LENGTH                    
         BNE   *+10                                                             
         MVC   WORK+32(16),TAW4MIDN MIDDLE NAME                                 
         OC    WORK+32(16),SPACES                                               
         SPACE 1                                                                
         MVC   W4SEX,TAW4SEX       W4 SEX                                       
         CLI   W4SEX,C'F'                                                       
         BE    PROCW410                                                         
         CLI   W4SEX,C'M'                                                       
         BE    PROCW410                                                         
         MVI   W4SEX,C'U'                                                       
         SPACE 1                                                                
PROCW410 L     R4,AIO                                                           
         MVI   ELCODE,TAA2ELQ      GET ADDRESS EL                               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                DIE IF NOT FOUND                             
         SPACE                                                                  
         CLC   TGEMP,=C'PG '       IF EMPLOYER IS PG                            
         BNE   *+12                                                             
         BAS   RE,SETPGREC         SET TAPE RECORD FOR PG                       
         B     *+8                                                              
         BAS   RE,SETTPREC         ELSE, SET OTHER EMPLOYERS RECORD             
         SPACE                                                                  
         BAS   RE,PUTTAPE          PUT RECORD TO TAPE                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET TAPE RECORD FOR EMPLOYER PG                       
         SPACE 1                                                                
         USING RECPGD,R3           R3=A(TAPE RECORD)                            
         USING TAA2D,R4            R4=A(W4 ADDRESS)                             
SETPGREC NTR1                                                                   
         MVI   RPGTYPE,C'2'        RECORD TYPE = 2                              
         MVC   RPGW4SSN,TGSSN      EMPLOYEE SSN                                 
         MVC   RPGW4FST,WORK                FIRST NAME                          
         MVC   RPGW4MID,WORK+32             MIDDLE NAME                         
         MVC   RPGW4LST(16),WORK+16         LAST NAME                           
         OC    RPGW4LST,SPACES                                                  
*                                           STREET ADDRESS                      
         MVC   RPGW4AD1(L'TAA2ADD1),TAA2ADD1                                    
         OC    RPGW4AD1,SPACES                                                  
         MVC   RPGW4AD2(L'TAA2ADD2),TAA2ADD2                                    
         OC    RPGW4AD2,SPACES                                                  
         MVC   RPGW4AD3(L'TAA2ADD3),TAA2ADD3                                    
         OC    RPGW4AD3,SPACES                                                  
         MVC   RPGW4CTY,TAA2CITY            CITY                                
         MVC   RPGW4ST,TAA2ST               STATE                               
         MVC   RPGW4ZIP,TAA2ZIP             ZIP                                 
         MVC   RPGW4ZPX,SPACES              ZIP CODE EXTENSION                  
         MVC   RPGW4FGN,SPACES              FOREIGN ADDRESS                     
         MVC   RPGW4DOB,=8C'0'              DATE OF BIRTH                       
         MVC   RPGW4DOH,TGTODY20            DATE OF HIRE                        
         MVC   RPGW4SOW,=C'OH'              STATE OF HIRE                       
         MVC   RPGW4SEX,W4SEX               SEX                                 
         MVI   RPGW4LEF,C'N'                LEFT WORK                           
*                                  EMPLOYER NAME                                
         MVC   RPGEMNAM(L'EMPNAME),EMPNAME                                      
         OC    RPGEMNAM,SPACES                                                  
         MVC   RPGEMEIN,FDID                FED EIN                             
         MVC   RPGEMSIN,STATEID             STATE EIN                           
         OC    RPGEMSIN,SPACES                                                  
         MVC   RPGEMAD1(L'TADCADD),TADCADD  STREET ADDRESS                      
         OC    RPGEMAD1,SPACES                                                  
         MVC   RPGEMAD2,SPACES                                                  
         MVC   RPGEMAD3,SPACES                                                  
         MVC   RPGEMCTY,TADCCITY            CITY                                
         MVC   RPGEMST,TADCSTAT             STATE                               
         MVC   RPGEMZIP,TADCZIP             ZIP                                 
         MVC   RPGEMZPX,SPACES              ZIP CODE EXTENSION                  
         MVI   RPGEMITC,C'N'                INCOME TAX CREDIT                   
         MVC   RPGEMFGN,SPACES              FOREIGN ADDRESS                     
         MVC   RPGEMOFG,SPACES              OPT FOREIGN ADDRESS                 
         MVC   RPGEMOAD,SPACES              OPTIONAL ADDRESS                    
         MVC   RPGEMOA2,SPACES                                                  
         MVC   RPG2SP1,SPACES               BLANKS                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET TP RECORD                                         
         SPACE 1                                                                
         USING RECTPD,R3           R3=A(TAPE RECORD)                            
         USING TAA2D,R4            R4=A(W4 ADDRESS)                             
SETTPREC NTR1                                                                   
         MVC   RTPID,=C'W4'        RECORD TYPE = W4                             
         MVC   RTPW4SSN,TGSSN      EMPLOYEE SSN                                 
         MVC   RTPW4FST,WORK                FIRST NAME                          
         MVC   RTPW4MID,WORK+32             MIDDLE NAME                         
         MVC   RTPW4LST(16),WORK+16         LAST NAME                           
         OC    RTPW4LST,SPACES                                                  
*                                                                               
         MVC   RTPW4AD1(L'TAA2ADD1),TAA2ADD1 STREET ADDRESS                     
         OC    RTPW4AD1,SPACES                                                  
         MVC   RTPW4AD2(L'TAA2ADD2),TAA2ADD2                                    
         OC    RTPW4AD2,SPACES                                                  
         MVC   RTPW4CTY,TAA2CITY            CITY                                
                                                                                
         CLI   TAA2LEN,TAA2LNQ              IF OLD STYLE ADDRESS                
         BL    STR10                                                            
         CLC   TAA2CTRY,=C'US'              OR COUNTRY IS US                    
         BNE   STR20                                                            
STR10    MVC   RTPW4ST,TAA2ST               STATE                               
         MVC   RTPW4ZIP,TAA2ZIP             ZIP                                 
         GOTO1 DATCON,DMCB,(1,SVDOH),(20,RTPEMHDT)                              
         MVC   RTPEMWST,SVWST                                                   
*                                  EMPLOYER INFO                                
STR20    MVC   RTPEMNAM(L'EMPNAME),EMPNAME NAME                                 
         OC    RTPEMNAM,SPACES                                                  
         MVC   RTPEMFED,FDID                FED ID                              
         MVC   RTPEMAD1(L'TADCADD),TADCADD  STREET ADDRESS                      
         OC    RTPEMAD1,SPACES                                                  
         MVC   RTPEMAD2,SPACES                                                  
         MVC   RTPEMCTY,TADCCITY            CITY                                
         MVC   RTPEMST,TADCSTAT             STATE                               
         MVC   RTPEMZIP,TADCZIP             ZIP                                 
         MVC   RTPEMAD2,SPACES                                                  
         SPACE                                                                  
*        MVC   RTPSP0,SPACES                BLANKS                              
         MVC   RTPSP1,SPACES                                                    
         MVC   RTPSP2,SPACES                                                    
         MVC   RTPSP4,SPACES                                                    
         MVC   RTPSP5,SPACES                                                    
         MVC   RTPSP6,SPACES                                                    
         MVC   RTPSP7,SPACES                                                    
         MVC   RTPSP8,SPACES                                                    
         B     XIT                                                              
         EJECT                                                                  
*======================================================================         
*              ROUTINE TO GET FROM DATASET AND PRINT SECOND REPORT              
*======================================================================         
PREPD    NTR1                                                                   
         BRAS  RE,NEWPRTQ2          SET NEW PRINT QUEUE REPORT                  
         GOTO1 REQTWA,DMCB,(3,ATWA),,VPRINT,(C'B',ABOX)                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 SPOOL,DMCB,(R5)                                                  
*                                                                               
         L     R2,AMASTD         DO NOT PRINT LOGOS                             
         USING MASTD,R2                                                         
         NI    MCPRTIND,X'FF'-MCPRTINL                                          
         DROP  R2                                                               
*                                                                               
         L     R2,=A(TADOWN)                                                    
         OPEN  ((2),INPUT)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,DLBLOCK                                                       
         USING DLCBD,R3                                                         
         BRAS  RE,INITDWN                                                       
*                                                                               
PREPD2   L     R4,AIO1                                                          
         GET   (R2),(R4)           GET FROM DISK AND PRINT                      
         L     R4,AIO1                                                          
*                                                                               
         SR    R0,R0                                                            
         LA    R1,801              RECORD LENGTH IN DCBRLN                      
         D     R0,=F'40'           R1  = # OF 40 BYTE CHUNKS                    
         STC   R0,REM              REM = LEFT OVER BYTES                        
*                                                                               
         LTR   R1,R1                                                            
         BZ    PREPD4                                                           
*                                                                               
PREPD3   STC   R1,QUO                                                           
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
*                                                                               
         MVI   DLCBLEN,40                                                       
         MVC   DLCBFLD(40),0(R4)    PASS DATA                                   
         GOTO1 =V(DLFLD),DLCBD                                                  
*                                                                               
         LA    R4,40(R4)           BUMP TO NEXT TAPE FIELD                      
         ZIC   R1,QUO                                                           
         BCT   R1,PREPD3                                                        
*                                                                               
PREPD4   CLI   REM,0                                                            
         BE    PREPD5                                                           
*                                                                               
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
         MVC   DLCBLEN,REM                                                      
         ZIC   RE,REM                                                           
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),0(R4)    PASS DATA                                    
         GOTO1 =V(DLFLD),DLCBD                                                  
*                                                                               
PREPD5   MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 =V(DLFLD),DLCBD                                                  
         B     PREPD2                                                           
*                                                                               
NOMORE   CLOSE ((2))               CLOSE THE DATASET                            
         LTR   RF,RF                                                            
         BZ    NOMORE2                                                          
         DC    H'0'                                                             
NOMORE2  MVI   DLCBACT,DLCBEOR                                                  
         GOTO1 =V(DLFLD),DLCBD                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO OPEN TAPE                                             
         SPACE                                                                  
OPENTAPE NTR1                                                                   
         TM    NHOPTS,NHNOTAPE     NO TAPE REQUESTED?                           
         BO    OPENTX                                                           
         MVC   SVDCB,NHTAPE        SAVE DCB                                     
**NO-OP  LA    R2,NHTAPE                                                        
         LA    R2,TADOWN                                                        
*&&DO                                                                           
**NO-OP  FEB/2010                                                               
*                                                                               
         CLC   TGEMP,=C'PG '       IF EMPLOYER ISN'T PG, DCB IS CORRECT         
         BNE   OPENT8                                                           
         MVC   62(2,R2),=H'27710'  ELSE FOR PG, SET ITS BLOCK SIZE              
         MVC   82(2,R2),=H'815'    AND RECORD LENGTH                            
OPENT8   MVC   WORK(20),=CL20'TALTAPE.TA0NH  1'                                 
         MVC   WORK+13(2),TGEMP    SET EMPLOYER                                 
         GOTO1 DYNALLOC,DMCB,(0,=CL8'NHTAPE'),(0,WORK)                          
*&&                                                                             
         OPEN  ((2),OUTPUT)                                                     
OPENTX   B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO PUT RECORD TO TAPE                                    
         SPACE                                                                  
PUTTAPE  NTR1                                                                   
         GOTO1 MYTRACE,DMCB,=C'TAPE REC',TAPEREC,RTPLNQ                         
         TM    NHOPTS,NHNOTAPE     AND DOING A TAPE                             
         BO    XIT                                                              
**NO-OP  LA    R2,NHTAPE                                                        
         LA    R2,TADOWN                                                        
         LA    R3,TAPEREC                                                       
         PUT   (R2),(R3)          PUT IT TO TAPE                                
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO CLOSE TAPE                                            
         SPACE                                                                  
CLOSTAPE NTR1                                                                   
         TM    NHOPTS,NHNOTAPE      IF TAPE REQUESTED                           
         BO    CLOSTX                                                           
**NO-OP  LA    R2,NHTAPE            CLOSE IT                                    
         LA    R2,TADOWN                                                        
         CLOSE ((2))                                                            
         MVC   NHTAPE(NHLNQ),SVDCB  RESTORE DCB                                 
CLOSTX   B     XIT                                                              
         EJECT                                                                  
*              THIS ROUTINE PRINTS OUT A LINE OF INFO                           
         SPACE                                                                  
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)    PRINT IT                                      
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO PRINT TOTAL NUMBER RECORDS ON TAPE                    
         SPACE                                                                  
PRNTOT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)     PRINT A BLANK LINE                           
         EDIT  TOTCOUNT,(8,P+1),ALIGN=LEFT                                      
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(9,R1),=CL9'NEW HIRES'                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO TRACE RECORD                                          
         SPACE                                                                  
MYTRACE  NTR1                                                                   
         TM    NHOPTS,NHTRACE      IF TRACE ON                                  
         BZ    MYTRACEX                                                         
         L     R2,0(R1)            A(LITERAL)                                   
         L     RF,8(R1)            SET LENGTH OF RECORD                         
         L     R4,4(R1)            A(RECORD)                                    
         GOTO1 PRNTBL,DMCB,(R2),(R4),C'DUMP',(RF),=C'2D'                        
MYTRACEX B     XIT                                                              
         SPACE 2                                                                
*              HEADLINE HOOK ROUTINE                                            
         SPACE                                                                  
HDHOOK   NTR1                                                                   
         MVC   H3+10(L'TGEMP),TGEMP       EMPLOYER CODE & NAME                  
         MVC   H3+15(36),EMPNAME                                                
         MVC   H3+108(L'NHPER),NHPER      PERIOD                                
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS, EXITS, CONSTANTS, ETC.                                   
*                                                                               
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         GOTO1 ERREX                                                            
         SPACE 1                                                                
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         GOTO1 ERREX                                                            
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
NHTAPE   DCB   DDNAME=NHTAPE,DSORG=PS,RECFM=FB,LRECL=801,              X        
               BLKSIZE=32040,MACRF=PM                                           
NHLNQ    EQU   *-NHTAPE                                                         
*                                                                               
TADOWN   DCB   DDNAME=TADOWN,DSORG=PS,RECFM=FB,LRECL=801,              X        
               BLKSIZE=32040,MACRF=(GM,PM),EODAD=NOMORE                         
TALNQ    EQU   *-TADOWN                                                         
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
*                                                                               
MYSPECS  SSPEC H1,2,RUN                                                         
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,120,PAGE                                                      
         SSPEC H2,100,REQUESTOR                                                 
         SSPEC H1,55,C'NEW HIRE REPORT'                                         
         SSPEC H2,55,15X'BF'                                                    
*                                                                               
         SSPEC H3,2,C'EMPLOYER'                                                 
         SSPEC H3,100,C'PERIOD'                                                 
         SSPEC H7,2,C'SS NUMBER EMPLOYEE NAME'                                  
         SSPEC H8,2,C'--------- -------------'                                  
         SSPEC H7,34,C'SS NUMBER EMPLOYEE NAME'                                 
         SSPEC H8,34,C'--------- -------------'                                 
         SSPEC H7,66,C'SS NUMBER EMPLOYEE NAME'                                 
         SSPEC H8,66,C'--------- -------------'                                 
         SSPEC H7,98,C'SS NUMBER EMPLOYEE NAME'                                 
         SSPEC H8,98,C'--------- -------------'                                 
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*              ROUTINE TO CONVERT CITIES TO STATES                              
***********************************************************************         
*                                                                               
CTY2STAT NTR1  BASE=*,LABEL=*                                                   
         CLI   SVWST+2,C' '        NOT A CITY                                   
         JH    XIT                 YES, LEAVE                                   
                                                                                
         CLC   SVWST,=C'CIN'                                                    
         BE    *+10                                                             
         CLC   SVWST,=C'CLV'                                                    
         BNE   *+10                                                             
         MVC   SVWST,=C'OH '                                                    
                                                                                
         CLC   SVWST,=C'DET'                                                    
         BNE   *+10                                                             
         MVC   SVWST,=C'MI '                                                    
                                                                                
         CLC   SVWST,=C'PHL'                                                    
         BNE   *+10                                                             
         MVC   SVWST,=C'PA '                                                    
                                                                                
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              ROUTINE GETS EMPLOYER INFO                                       
***********************************************************************         
*                                                                               
GTEMP    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'A8',TGEMP),0                              
         MVC   EMPNAME,TGNAME      SAVE EMPLOYER NAME                           
                                                                                
         MVI   ELCODE,TATIELQ      SET FEDERAL EIN                              
         MVI   FULL,TATITYUN                                                    
         MVC   FULL+1(3),=C'FD '                                                
         GOTO1 GETL,DMCB,(4,FULL)                                               
         JE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,FDID                                                          
         BAS   RE,SETID                                                         
                                                                                
         CLC   =C'PG ',TGEMP       IF EMPLOYER IS PG                            
         JNE   GTEMP10                                                          
         MVC   FULL+1(3),=C'OH '   SET STATE EIN TOO                            
         GOTO1 GETL,DMCB,(4,FULL)                                               
         JE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,STATEID                                                       
         BAS   RE,SETID                                                         
                                                                                
GTEMP10  L     R4,AIO              GET ADDRESS                                  
         MVI   ELCODE,TAADELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING TAADD,R4                                                         
         ZIC   R1,TAADLEN                                                       
         AHI   R1,-4                                                            
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   BLOCK(0),TAADADD                                                 
         OC    BLOCK(120),SPACES                                                
         LA    R1,BLOCK                                                         
         ST    R1,TADCAADD                                                      
         MVI   TADCSTA2,0                                                       
         MVC   TADCASQ,SQUASHER                                                 
         MVC   TADCACHP,CHOPPER                                                 
         GOTO1 =V(TAADDCON),DMCB,ADDCOND                                        
         J     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET EIN                                               
         SPACE 1                                                                
         USING TATID,R4                                                         
SETID    NTR1                                                                   
         LR    R3,R1               R3=A(SAVED EIN)                              
         L     R4,TGELEM                                                        
         LA    R1,L'TATIID                                                      
         LA    R2,TATIID                                                        
*                                                                               
SETTID4  CLI   0(R2),C'0'          ONLY MOVE OUT NUMERICS                       
         JL    SETTID6                                                          
         MVC   0(1,R3),0(R2)                                                    
         LA    R3,1(R3)                                                         
SETTID6  LA    R2,1(R2)                                                         
         BCT   R1,SETTID4                                                       
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              ROUTINE TO SET UP SECOND REPORT ON QUEUE                         
***********************************************************************         
*                                                                               
NEWPRTQ2 NTR1  BASE=*,LABEL=*                                                   
         L     R2,ALOGOC                                                        
         USING LOGOD,R2                                                         
         MVI   LOGOTYPE,C'E'                                                    
         GOTO1 ALOGO,DMCB,(R2)     END REPORT LOGOS                             
*                                                                               
         TM    WHEN,X'20'          SOON?                                        
         BO    NPRT10                                                           
         GOTO1 VPRINT,DMCB,=C'CLOSE'                                            
*                                                                               
NPRT10   XC    SPECS,SPECS         CLEAR SPECS                                  
         XC    HEADHOOK,HEADHOOK   AND HEADLINE HOOK                            
         L     R2,ABOX                                                          
         USING BOXD,R2                                                          
         LA    RE,132                                                           
         ST    RE,BOXWIDTH         SET LENGTH OF PRINT LINE                     
*                                                                               
         L     RF,AMASTD                                                        
         USING MASTD,RF                                                         
         L     R2,AREMOT                                                        
         USING REMOTED,R2                                                       
         TM    WHEN,X'20'          SOON?                                        
         BZ    NPRT20                                                           
         XC    MCREMPQK,MCREMPQK                                                
         B     NPRT30                                                           
*                                                                               
NPRT20   MVC   REMOTABF,MCVPQBUF                                                
         MVC   REMOTADM,MCVDMGR                                                 
         MVC   REMOTAOP,MCVPQOPN                                                
         MVC   REMOTDST,MCDESTID                                                
         XC    MCALTREF,MCALTREF                                                
         MVI   REMOTCPY,C'1'                                                    
         MVI   REMOTCLS,C'Q'                                                    
         MVC   REMOTJID,=C'TNH'                                                 
NPRT30   MVC   REMOTKEY(11),SPACES                                              
         MVC   REMOTSYS(6),=C'NHDATA'                                           
         MVC   REMOTFRM(4),=C'DATA'                                             
         XIT1                                                                   
         DROP  R2,RF                                                            
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SET UP SECOND REPORT ON QUEUE                         
*                                                                               
         USING DLCBD,R3                                                         
INITDWN  NTR1  BASE=*,LABEL=*                                                   
         XC    DLCBD(DLCBXLX),DLCBD                                             
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,SPLATDWN         A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         MVC   P,SPACES                                                         
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)    MAXIMUM LENGTH OF PRINT LINE                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON FOR END OF LINE                   
         MVI   DLCXEORC,C':'       END OF REPORT                                
         OI    DLCBFLG1,DLCBFXTN                                                
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              USER SUPPLIED PRINT ROUTINE - DOWNLOAD                           
         SPACE 1                                                                
SPLATDWN NTR1                                                                   
         MVI   LINE,1              PREVENT PAGE BREAK                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SSNTAB                                                              *         
***********************************************************************         
         DC    C'**SSNTAB**'                                                    
         DS    0D                                                               
MAXSSN   EQU   13000                                                            
SSNTAB   DS    (MAXSSN)XL9         TABLE OF NEW HIRE'S SSN IN BINARY            
*                                  3 BYTE HIRE DATE + 2 CHAR WORK STATE         
         DC    X'FF'               MARK END OF TABLE                            
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
*                                                                               
TNHD     DSECT                                                                  
         DS    0A                                                               
       ++INCLUDE TAADDCOND                                                      
*                                                                               
PRNTBL   DS    A                   A(PRNTBL)                                    
DYNALLOC DS    A                   A(DYNALLOC)                                  
SSNENT   DS    A                   A(NEXT ENTRY IN SSNTAB)                      
*                                                                               
AMASTD   DS    A                                                                
ALOGOC   DS    A                   A(LOGOC)                                     
ALOGO    DS    A                   A(LOGO)                                      
AREMOT   DS    A                   A(REMOTE)                                    
*                                                                               
NCHUNKS  EQU   4                   NUMBER OF CHUNKS PER PRINT LINE              
*                                                                               
NHOPTS   DS    XL1                 OPTIONS                                      
NHNOTAPE EQU   X'80'               DON'T GENERATE TAPE                          
NHTRACE  EQU   X'40'               TRACE RECORDS                                
*                                                                               
NHPER    DS    CL17                PRINTABLE PERIOD                             
NHSTR    DS    XL6                 PWOS START                                   
NHEND    DS    XL6                 PWOS END                                     
*                                                                               
EMPNAME  DS    CL36                SAVED EMPLOYER NAME                          
FDID     DS    CL9                 FEDERAL EIN                                  
STATEID  DS    CL12                STATE EIN                                    
TOTCOUNT DS    PL6                 RECORD COUNTER FOR TAPE                      
SVSSN    DS    CL9                 SAVED SS NUMBER                              
SVDOH    DS    CL8                 SAVED DATE OF HIRE                           
SVWST    DS    CL3                 SAVED WORK STATE                             
W4SEX    DS    CL1                 PERFORMERS GENDER                            
SVKEY    DS    CL(L'TLCKPKEY)      SAVED KEY                                    
SVDCB    DS    CL96                SAVED DCB                                    
*                                                                               
TAPEREC  DS    CL(RTPLNQ)          TAPE RECORD AREA                             
*                                                                               
DLBLOCK  DS    CL(DLCBXLX)                                                      
QUO      DS    XL1                                                              
REM      DS    XL1                                                              
TNHLNQ   EQU   *-TNHD                                                           
         EJECT                                                                  
*              DSECT TO COVER TAPE RECORD FOR TP OR PP (NOT PG)                 
*                                                                               
RECTPD   DSECT                                                                  
RTPID    DS    CL2                 "W4"                                         
RTPW4SSN DS    CL9                 EMPLOYEE'S SS#                               
RTPW4FST DS    CL16                EMPLOYEE'S FIRST NAME                        
RTPW4MID DS    CL16                EMPLOYEE'S MIDDLE NAME                       
RTPW4LST DS    CL30                EMPLOYEE'S LAST NAME                         
RTPW4AD1 DS    CL40                EMPLOYEE'S ADDRESS LINE 1                    
RTPW4AD2 DS    CL40                EMPLOYEE'S ADDRESS LINE 2                    
RTPSP1   DS    CL40                BLANKS                                       
RTPW4CTY DS    CL25                EMPLOYEE'S CITY                              
RTPW4ST  DS    CL2                 EMPLOYEE'S STATE                             
RTPW4ZIP DS    CL5                 EMPLOYEE'S ZIP CODE                          
RTPSP2   DS    CL54                BLANKS                                       
RTPEMHDT DS    CL8                 EMPLOYEE HIRE DATE                           
RTPEMWST DS    CL2                 EMPLOYEE WORK STATE                          
RTPEMFED DS    CL9                 EMPLOYER FEIN                                
RTPSP4   DS    CL12                BLANKS                                       
RTPEMNAM DS    CL45                EMPLOYER NAME                                
RTPEMAD1 DS    CL40                EMPLOYER ADDRESS LINE 1                      
RTPEMAD2 DS    CL40                EMPLOYER ADDRESS LINE 2                      
RTPSP5   DS    CL40                BLANKS                                       
RTPEMCTY DS    CL25                EMPLOYER CITY                                
RTPEMST  DS    CL2                 EMPLOYER STATE                               
RTPEMZIP DS    CL5                 EMPLOYER ZIP CODE                            
RTPSP6   DS    CL100               BLANKS                                       
RTPSP7   DS    CL100               BLANKS                                       
RTPSP8   DS    CL94                BLANKS                                       
RTPLNQ   EQU   *-RECTPD                                                         
         EJECT                                                                  
*              DSECT TO COVER TAPE RECORD FOR PG                                
*                                                                               
RECPGD   DSECT                                                                  
RPGTYPE  DS    CL1                 TYPE 1 FOR HEADER                            
RPGDATE  DS    CL8                 RUN DATE                                     
RPGCOUNT DS    CL9                 RECORD COUNT NOT INCLUDING HEADER            
RPG1SP1  DS    CL132               BLANKS                                       
RPG1SP2  DS    CL132               BLANKS                                       
RPG1SP3  DS    CL132               BLANKS                                       
RPG1SP4  DS    CL132               BLANKS                                       
RPG1SP5  DS    CL132               BLANKS                                       
RPG1SP6  DS    CL132               BLANKS                                       
RPG1SP7  DS    CL5                 BLANKS                                       
*                                                                               
         ORG   RPGDATE             TYPE 2 FOR DETAIL                            
RPGW4SSN DS    CL9                 EMPLOYEE'S SS#                               
RPGW4FST DS    CL16                EMPLOYEE'S FIRST NAME                        
RPGW4MID DS    CL16                EMPLOYEE'S MIDDLE NAME                       
RPGW4LST DS    CL30                EMPLOYEE'S LAST NAME                         
RPGW4AD1 DS    CL40                EMPLOYEE'S ADDRESS LINE 1                    
RPGW4AD2 DS    CL40                EMPLOYEE'S ADDRESS LINE 2                    
RPGW4AD3 DS    CL40                EMPLOYEE'S ADDRESS LINE 3                    
RPGW4CTY DS    CL25                EMPLOYEE'S CITY                              
RPGW4ST  DS    CL2                 EMPLOYEE'S STATE                             
RPGW4ZIP DS    CL5                 EMPLOYEE'S ZIP CODE                          
RPGW4ZPX DS    CL4                 EMPLOYEE'S ZIP CODE EXTENTION                
RPGW4FGN DS    CL42                EMPLOYEE'S FOREIGN COUNTRY CODE,             
*                                  COUNTRY NAME AND POSTAL CODE                 
RPGW4DOB DS    CL8                 DEFAULT TO "00000000"                        
RPGW4DOH DS    CL8                 DEFAULT TO TODAY'S DATE                      
RPGW4SOW DS    CL2                 DEFAULT TO "OH"                              
RPGW4SEX DS    CL1                 "U" FOR UNKNOWN                              
RPGW4LEF DS    CL1                 "U" FOR UNKNOWN                              
RPGEMEIN DS    CL9                 EMPLOYER FEDERAL EIN                         
RPGEMITC DS    CL1                 EMPLOYER INCOME TAX CREDIT                   
RPGEMSIN DS    CL12                EMPLOYER STATE EIN                           
RPGEMNAM DS    CL45                EMPLOYER NAME                                
RPGEMAD1 DS    CL40                EMPLOYER ADDRESS LINE 1                      
RPGEMAD2 DS    CL40                EMPLOYER ADDRESS LINE 2                      
RPGEMAD3 DS    CL40                EMPLOYER ADDRESS LINE 3                      
RPGEMCTY DS    CL25                EMPLOYER CITY                                
RPGEMST  DS    CL2                 EMPLOYER STATE                               
RPGEMZIP DS    CL5                 EMPLOYER ZIP CODE                            
RPGEMZPX DS    CL4                 EMPLOYER ZIP CODE EXTENSION                  
RPGEMFGN DS    CL42                EMPLOYER'S FOREIGN COUNTRY CODE,             
*                                  COUNTRY NAME AND POSTAL CODE                 
RPGEMOAD DS    CL132               EMPLOYER OPTIONAL ADDRESS                    
RPGEMOA2 DS    CL24                                                             
RPGEMOFG DS    CL42                EMPLOYER'S FOREIGN OPTIONAL ADDRESS          
RPG2SP1  DS    CL62                BLANKS                                       
RPGLNQ   EQU   *-RECPGD                                                         
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
*                                                                               
PRNTD    DSECT                                                                  
         DS    CL1                 SPACE                                        
PRTW4SSN DS    CL9                 EMPLOYEE'S SS #                              
         DS    CL1                 SPARE                                        
PRTW4FST DS    CL1                 EMPLOYEE'S FIRST INITIAL                     
         DS    CL1                 SPACE                                        
PRTW4MID DS    CL1                 EMPLOYEE'S MIDDLE INITIAL                    
         DS    CL1                 SPACE                                        
PRTW4LST DS    CL16                EMPLOYEE'S LAST NAME                         
         DS    CL1                 SPARE                                        
PRTLNQ   EQU   *-PRNTD                                                          
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPDCD                                                       
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDBIGBOX                                                                      
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* DDPERVALD                                                                     
* DDMASTD                                                                       
* DDREMOTED                                                                     
* DDLOGOD                                                                       
* DDDLCB                                                                        
* DDTWADCONS                                                                    
* TAREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDDLCB                                                         
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043TAREP4C   12/16/13'                                      
         END                                                                    
