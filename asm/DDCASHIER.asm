*          DATA SET DDCASHIER  AT LEVEL 038 AS OF 10/13/15                      
*PHASE T00AA5A                                                                  
*INCLUDE CLUNPK                                                                 
*        TITLE 'CASHIER - BILL RECORD AND CASH APPLIED BUFFER                   
         TITLE 'CASHIER - BILL RECORD AND CASH APPLIED BUFFER -REGS'            
***********************************************************************         
*                                                                     *         
*        CASHIER - BUILDS A TSAROFF BUFFER FOR BILL RECORDS AND       *         
*              THE CASH APPLIED TRANSACTION ELEMENTS FROM ACCPAK      *         
*              ALSO WILL RETRIEVE DATA FROM BUFFER                    *         
*                                                                     *         
*NTRY    PARM0    A(CSHIERD - CONTROL BLOCK)                          *         
*                                                                     *         
*                                                                     *         
*EXIT    A(COPY OF BUFFER RECORD)                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
*        REGISTER USAGE                                                         
*                                                                               
CT5RECRG EQU   R2,,,,GR32          CT5REC   REGISTER                            
CPYRECRG EQU   R2,,,,GR32          CPYREC   REGISTER                            
CLTRECRG EQU   R2,,,,GR32          CLTHDR   REGISTER                            
CTSYSDRG EQU   R3,,,,GR32          CTSYSD   REGISTER                            
*                                                                               
MPDRECRG EQU   R2,,,,GR32          MPDREC   REGISTER                            
MBTELDRG EQU   R3,,,,GR32          MBTELD   REGISTER                            
*                                                                               
TRNRECRG EQU   R4,,,,GR32          TRNRECD  REGISTER                            
*                                                                               
CPYELDRG EQU   R5,,,,GR32          CPYELD   REGISTER                            
GDAELDRG EQU   R5,,,,GR32          ODAELD   REGISTER                            
OTHELDRG EQU   R5,,,,GR32          OTHELD   REGISTER                            
TRNELDRG EQU   R5,,,,GR32          TRNELD   REGISTER                            
MBIELDRG EQU   R5,,,,GR32          MBIELD   REGISTER                            
MDTELDRG EQU   R5,,,,GR32          MDTELD   REGISTER                            
BNDELDRG EQU   R5,,,,GR32          BNDELD   REGISTER                            
PTAELDRG EQU   R5,,,,GR32          PTAELD   REGISTER                            
*                                                                               
CSHSVRG  EQU   R7,,,,GR32          CASHIER SAVEAREA REGISTER                    
*                                                                               
TSARRG   EQU   R8,,,,GR32          TSAR CONTROL BLOCK REGISTER                  
PBILRCRG EQU   R9,,,,GR32          PBILLREC REGISTER                            
*                                                                               
CSHIERRG EQU   RA,,,,GR32          CSHIERD  REGISTER                            
CSHWRKRG EQU   RC,,,,GR32          CSHIERWD REGISTER                            
*                                                                               
         TITLE 'CASHIER - BILL RECORD AND CASH APPLIED BUFFER -START'           
***********************************************************************         
*                                                                     *         
*        CASHIER - PROGRAM START                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CASHIER  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 CSHIERWL,CASHIER,RR=R2                                           
*                                                                               
         USING CSHIERWD,CSHWRKRG  LOCAL WORKING STORAGE                         
*                                                                               
         ST    R2,RELO             SAVE RELOCATION FACTOR                       
*                                                                               
         MVC   CSHWPRMS(CSHWPRML),0(R1)  SAVE PARAMETER LIST                    
*                                                                               
         L     CSHIERRG,CSHWBLKA                                                
         USING CSHIERD,CSHIERRG   CASHIER CONTROL BLOCK                         
*                                                                               
         L     CSHSVRG,=A(CSHSV)   ESTABLISH SAVEAREA REGISTER                  
         A     CSHSVRG,RELO        RELOCATE ADDRESS                             
*                                                                               
         USING CSHSV,CSHSVRG       ESTABLISH SAVEAREA                           
*                                                                               
         MVI   CSHERR,0            INIT RETURN CODE                             
*                                                                               
         LA    TSARRG,TSARDC                                                    
         USING TSARD,TSARRG       TSAROFF CONTROL BLOCK                         
*                                                                               
         LH    RF,=Y(IOAREA1-CSHIERWD) SET IO AREAS ADDRESSES                   
         LA    RF,CSHIERWD(RF)                                                  
         ST    RF,AIO1                                                          
*                                                                               
         LH    RF,=Y(IOAREA2-CSHIERWD) SET IO AREAS ADDRESSES                   
         LA    RF,CSHIERWD(RF)                                                  
         ST    RF,AIO2                                                          
*                                                                               
         LH    RF,=Y(IOAREA3-CSHIERWD) SET IO AREAS ADDRESSES                   
         LA    RF,CSHIERWD(RF)                                                  
         ST    RF,AIO3                                                          
*                                                                               
         L     RF,CSHUTLA          POINT TO UTL                                 
         MVC   SYSTEMSV,4(RF)      SAVE SYSTEM NUMBER                           
*                                                                               
         GOTO1 CSHDATCA,DMCB,(5,FULL),(1,PTODAY) GET TODAY'S DATE               
*                                                                               
         TITLE 'CASHIER - ACTION DETERMINATION'                                 
***********************************************************************         
*                                                                     *         
*        ACTION DETERMINATION                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         CLI   CSHACT,CSHINIQ      INITIALIZATION                               
         BE    INITROUT                                                         
*                                                                               
         CLI   CSHACT,CSHADDQ      ADD TO BUFFER                                
         BE    ADD                                                              
*                                                                               
         CLI   CSHACT,CSHRDHQ      READ HIGH                                    
         JE    READ                                                             
*                                                                               
         CLI   CSHACT,CSHNXTQ      READ NEXT RECORD                             
         JE    NEXT                                                             
*                                                                               
         CLI   CSHACT,CSHCLSQ      CLOSE                                        
         J     CLOSETS                                                          
*                                                                               
         DC    H'0'                UNKNOWN ACTION                               
*                                                                               
         TITLE 'DDCASHIER - INITIALIZATION - INIT'                              
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INITROUT DS    0H                                                               
*                                                                               
         GOTOR INIT                                                             
*                                                                               
*                                                                               
INITROUX DS    0H                                                               
         J     CASHIERX                                                         
*                                                                               
         TITLE 'DDCASHIER - CASH BUFFER - ADD'                                  
***********************************************************************         
*                                                                     *         
*        ADD RECORD TO BUFFER                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ADD      DS    0H                                                               
*                                                                               
         BRAS  RE,CUTDTS           SET CUTOFF DATES                             
         TITLE 'DDCASHIER - READ A0 PROFILE - RDA0'                             
***********************************************************************         
*                                                                     *         
*        READ A0 PROFILE FOR SYSTEM,AGENCY AND MEDIA                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RDA0     DS    0H                                                               
*                                                                               
*        READ A0 PROFILE                                                        
*                                                                               
         TM    CSHCTL,CSHCCSHQ     SKIP IF CASH DATA NOT NEEDED                 
         BNO   RDA0X                                                            
*                                                                               
         XC    WORK,WORK                                                        
*                                                                               
         MVC   WORK(1),CSHSYS      SET SYSTEM                                   
*                                                                               
         MVC   WORK+1(3),=C'0A0'                                                
         MVC   WORK+4(2),CSHAGYCH  AGENCY                                       
         MVC   WORK+6(1),CSHMED    MEDIA                                        
*                                                                               
         L     PBILRCRG,CSHBLLA    POINT TO BILL RECORD                         
*                                                                               
         CLI   CSHSYS,CSHPRTQ      SKIP IF NOT PRINT SYSTEM                     
         BNE   RDA0CLPN                                                         
*                                                                               
         USING PBILLRCD,PBILRCRG   ESTABLISH PRINT BILL RECORD                  
*                                                                               
         MVC   WORK+7(3),PBILKCLT    CLIENT                                     
*                                                                               
         MVC   SVBILLDT,PBILLDAT   SAVE DATE OF BILLING                         
*                                                                               
         B     RDA0CLTX                                                         
*                                                                               
         DROP  PBILRCRG                                                         
*                                                                               
RDA0CLPN DS    0H                                                               
*                                                                               
         CLI   CSHSYS,CSHSPTQ      SKIP IF NOT SPOT SYSTEM                      
         BE    *+8                                                              
         CLI   CSHSYS,CSHNETQ      OR          NET  SYSTEM                      
         BNE   RDA0CLSN                                                         
*                                                                               
         USING BILLRECD,PBILRCRG   ESTABLISH SPOT/NET BILL RECORD               
*                                                                               
         L     RF,CSHCLTA          POINT TO CLIENT RECORD                       
         USING CLTHDRD,RF                                                       
*                                                                               
         SR    R0,R0                                                            
         IC    R0,CPROF+6          GET AAN OPTION                               
*                                                                               
         DROP  RF                                                               
*                                                                               
         GOTO1 =V(CLUNPK),DMCB,((R0),BKEYCLT),WORK+7,RR=RELO CLT CODE           
*                                                                               
         MVC   SVBILLDT,BDATE      SAVE DATE OF BILLING                         
*                                                                               
         B     RDA0CLTX                                                         
*                                                                               
         DROP  PBILRCRG                                                         
*                                                                               
RDA0CLSN DS    0H                                                               
*                                                                               
RDA0CLTX DS    0H                                                               
*                                                                               
         XC    ACCAGY,ACCAGY       INIT ACC AGENCY CODE                         
*                                                                               
*        CHECK CLIENT RECORD FOR OFFICE                                         
*                                                                               
         CLI   CSHSYS,CSHPRTQ      SKIP IF NOT PRINT SYSTEM                     
         BNE   RDA0CRPN                                                         
*                                                                               
         L     CLTRECRG,CSHCLTA    POINT TO CLIENT RECORD                       
         USING PCLTREC,CLTRECRG    ESTABLISH AS CLIENT REC KEY                  
*                                                                               
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF  GET CLIENT OFFICE                            
*                                                                               
         MVC   CLTOFFSV,PCLTOFF    SAVE CLIENT OFFICE                           
         MVC   ACCAGY,PCLTACCA     SAVE ACC AGENCY ID                           
*                                                                               
         B     RDA0CRX                                                          
*                                                                               
         DROP  CLTRECRG                                                         
*                                                                               
RDA0CRPN DS    0H                                                               
*                                                                               
         CLI   CSHSYS,CSHSPTQ      SKIP IF NOT SPOT SYSTEM                      
         BE    *+8                                                              
         CLI   CSHSYS,CSHNETQ      OR          NET  SYSTEM                      
         BNE   RDA0CRSN                                                         
*                                                                               
*        CHECK CLIENT RECORD FOR OFFICE                                         
*                                                                               
         USING BILLRECD,PBILRCRG   ESTABLISH SPOT/NET BILL RECORD               
*                                                                               
         L     CLTRECRG,CSHCLTA    POINT TO CLIENT RECORD                       
         USING CLTHDR,CLTRECRG     ESTABLISH AS CLIENT REC KEY                  
*                                                                               
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE  GET CLIENT OFFICE                            
*                                                                               
******   GOTO1 =V(CLUNPK),DMCB,((R0),CKEYCLT),WORK+7,RR=RELO CLT CODE           
*                                                                               
         MVC   CLTOFFSV,COFFICE    SAVE CLIENT OFFICE                           
*                                                                               
         MVC   ACCAGY,CACCAGY      SAVE ACC AGENCY OVERRIDE                     
*                                                                               
         B     RDA0CRX                                                          
*                                                                               
         DROP  CLTRECRG                                                         
*                                                                               
RDA0CRSN DS    0H                                                               
*                                                                               
RDA0CRX  DS    0H                                                               
*                                                                               
         CLC   A0IDSV(12),WORK     SKIP IF PROFILE ALREADY RETRIEVED            
         BE    RDA0X                                                            
*                                                                               
         MVC   A0IDSV,WORK         SAVE PROFILE ID                              
         XC    A0PROF,A0PROF       INIT A0 PROF                                 
*                                                                               
         GOTO1 CSHGTPRA,DMCB,(X'C0',WORK),A0PROF,CSHDMGRA                       
*                                                                               
RDA0X    DS    0H                                                               
*                                                                               
         TITLE 'DDCASHIER - ACCPAK SYSTEM DATA - CTAUTH'                        
***********************************************************************         
*                                                                     *         
*        GET ACCPAK SYSTEM NUMBER AND COMPANY CODE                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CTAUTH   DS    0H                                                               
*                                                                               
         TM    CSHCTL,CSHCCSHQ     SKIP IF CASH DATA NOT NEEDED                 
         BNO   CTAUTHX                                                          
*                                                                               
         BRAS  RE,ACCTSTS          TEST FOR AGENCY OVERRIDES                    
*                                                                               
         L     RF,CSHUTLA          POINT TO UTL                                 
         MVI   4(RF),X'0A'         SET TO THE CONTROL SYSTEM                    
*                                                                               
         XC    KEY,KEY             INIT KEY AREA                                
         LA    CT5RECRG,KEY        ESTABLISH CT5REC (SYSTEM ACCESS REC)         
         USING CT5REC,CT5RECRG                                                  
*                                                                               
         MVI   CT5KTYP,CT5KTYPQ    RECORD ID                                    
         MVC   CT5KALPH,CSHAGYCH   SET AGENCY CODE                              
*                                                                               
         CLC   =C'YN',CSHAGYCH     IF YN                                        
         BNE   CTAUTYN1                                                         
*                                                                               
*****    CLC   SVBILLDT,SVYNDATE   IF BEFORE CUT OFF DATE                       
*****    BNL   CTAUTHYN1                                                        
*                                                                               
         MVC   CT5KALPH,=C'YN'        USE CALLING AGENCY                        
*                                                                               
         B     CTAUTYN2                                                         
*                                                                               
CTAUTYN1 DS    0H                                                               
*                                                                               
         CLC   A0PROF+14(2),=C'00' IF ALTERNATE AGENCY IN PROFILE               
         BE    *+20                                                             
         CLC   A0PROF+14(2),=C'  ' IF ALTERNATE AGENCY IN PROFILE               
         BNH   *+10                                                             
         MVC   CT5KALPH,A0PROF+14     USE IT                                    
*                                                                               
         CLC   ACCAGY,=C'  '       IF ACC AGENCY OVERRIDE EXISTS                
         BNH   *+10                                                             
         MVC   CT5KALPH,ACCAGY        USE IT                                    
*                                                                               
CTAUTYN2 DS    0H                                                               
*                                                                               
         CLC   =C'YN',CSHAGYCH     IF YN, ALWAYS RE-READ RECORD                 
         BE    *+10                                                             
         CLC   =C'FR',CSHAGYCH     IF FR, ALWAYS RE-READ RECORD                 
         BE    *+14                                                             
         CLC   CT5KEYSV,KEY        SKIP IF DATA ALEADY RETRIEVED                
         BE    CTAUTHX                                                          
*                                                                               
         MVC   CT5KEYSV,KEY        SAVE KEY                                     
*                                                                               
         GOTO1 CSHDMGRA,DMCB,=CL8'DMREAD',=CL8'CTFILE',KEY,AIO1,0 READ          
         CLI   8(R1),0             NO ERRORS ALLOWED                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     CT5RECRG,AIO1       POINT TO FOUND RECORD                        
*                                                                               
         LA    CTSYSDRG,CT5DATA    POINT TO FIRST ELEMENT IN RECORD             
         SR    R0,R0                                                            
*                                                                               
CTAUTHLP DS    0H                                                               
*                                                                               
         USING CTSYSD,CTSYSDRG     ESTABLISH SYS AUTH ELEMENT                   
*                                                                               
         CLI   CTSYSEL,0           MUST FIND AUTH ELEMENT                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   CTSYSEL,CTSYSELQ    FIND SYSTEM AUTH ELEMENT                     
         BNE   CTAUTHCN                                                         
*                                                                               
         CLI   CTSYSNUM,6          MUST BE FOR ACC SYSTEM                       
         BE    CTAUTHFD                                                         
*                                                                               
CTAUTHCN DS    0H                                                               
*                                                                               
         IC    R0,CTSYSLEN         BUMP TO NEXT ELEMENT                         
         AR    CTSYSDRG,R0                                                      
         B     CTAUTHLP                                                         
*                                                                               
CTAUTHFD DS    0H                                                               
*                                                                               
         MVC   ACCCD,CTSYSAGB      SAVE COMPANY CODE                            
         MVC   ACCSE,CTSYSSE       SAVE SE NUMBER                               
         MVC   ACCCDPST,CTSYSAGB   SAVE COMPANY CODE                            
         MVC   ACCSEPST,CTSYSSE    SAVE SE NUMBER                               
*                                                                               
         XC    ACCSALPH,ACCSALPH   INIT AGENCY ALPH CD FOR SPLIT FILES          
*                                                                               
         CLC   A0PROF+14(2),=C'00' IF ALTERNATE AGENCY IN PROFILE               
         BE    *+20                                                             
         CLC   A0PROF+14(2),=C'  ' IF ALTERNATE AGENCY IN PROFILE               
         BNH   *+10                                                             
         MVC   ACCSALPH,CSHAGYCH      USE CURRENT AGENCY IN KEY                 
*                                                                               
         CLC   ACCAGY,=C'  '       IF ACC AGENCY OVERRIDE EXISTS                
         BNH   *+10                                                             
         MVC   ACCSALPH,CSHAGYCH      USE CURRENT AGENCY IN KEY                 
*                                                                               
*        REVERSE ABOVE IN CERTAIN CASES                                         
*                                                                               
         CLC   CSHAGYCH,=C'FM'     HARD CODE TEST FOR FM                        
         BE    *+10                                                             
         CLC   CSHAGYCH,=C'RB'     HARD CODE TEST FOR RB                        
         BE    *+10                                                             
         CLC   CSHAGYCH,=C'WD'     HARD CODE TEST FOR WD                        
         BE    *+10                                                             
         CLC   CSHAGYCH,=C'SJ'     HARD CODE TEST FOR TRC1                      
         BE    *+10                                                             
         CLC   CSHAGYCH,=C'DM'     HARD CODE TEST FOR DOREMUS                   
         BE    *+10                                                             
         CLC   CSHAGYCH,=C'BN'     HARD CODE TEST FOR BN                        
         BE    *+10                                                             
         CLC   CSHAGYCH,=C'WT'     HARD CODE TEST FOR WITO                      
         BNE   *+14                                                             
         XC    ACCSALPH,ACCSALPH     NO AGENCY ALPH CD                          
         B     CTAUTHF2                                                         
*                                                                               
         CLC   CSHAGYCH,=C'OO'     OMD                                          
         BNE   CTAUTOON                                                         
*                                                                               
         CLC   CSHAGYCH,ACCSALPH   IF SAME AS ACC AGENCY ALPH CODE              
         BNE   *+10                                                             
         XC    ACCSALPH,ACCSALPH      NO AGENCY ALPH CODE                       
*                                                                               
         B     CTAUTHF2                                                         
*                                                                               
CTAUTOON DS    0H                                                               
*                                                                               
         CLC   CSHAGYCH,=C'H7'     HARD CODE TEST FOR MINDSHARE                 
         BE    *+10                                                             
         CLC   CSHAGYCH,=C'YN'     HARD CODE TEST FOR Y & R                     
         BE    *+10                                                             
         CLC   CSHAGYCH,=C'YR'     HARD CODE TEST FOR Y & R                     
         BE    *+10                                                             
         CLC   CSHAGYCH,=C'M2'     HARD CODE TEST FOR MEDIACOM                  
         BE    *+10                                                             
         CLC   CSHAGYCH,=C'OM'     HARD CODE TEST FOR OGILVY                    
         BNE   *+10                                                             
         XC    ACCSALPH,ACCSALPH     NO AGENCY ALPH CD FOR SPLIT FILES          
*                                                                               
         CLC   CSHAGYCH,=C'TH'     HARD CODE TEST FOR DFZNYA                    
         BNE   CTAUTHF2                                                         
*                                                                               
         CLC   A0PROF+14(2),=C'DW'                                              
         BE    *+10                                                             
         CLC   A0PROF+14(2),=C'TH'                                              
         BE    *+10                                                             
         CLC   A0PROF+14(2),=C'BS'                                              
         BNE   *+10                                                             
         XC    ACCSALPH,ACCSALPH   INIT AGENCY ALPH CD FOR SPLIT FILES          
*                                                                               
         CLC   ACCAGY,=C'DW'       IF ACC AGENCY OVERRIDE EXISTS                
         BE    *+10                                                             
         CLC   ACCAGY,=C'TH'                                                    
         BE    *+10                                                             
         CLC   ACCAGY,=C'BS'                                                    
         BNE   *+10                                                             
         XC    ACCSALPH,ACCSALPH   INIT AGENCY ALPH CD FOR SPLIT FILES          
*                                                                               
CTAUTHF2 DS    0H                                                               
*                                                                               
         CLC   CSHAGYCH,=C'OU'     HARD CODE TEST FOR OMDTO                     
         BNE   CTAUTHF3                                                         
*                                                                               
         XC    ACCSALPH,ACCSALPH   INIT AGENCY ALPH CD FOR SPLIT FILES          
*                                                                               
CTAUTHF3 DS    0H                                                               
*                                                                               
         MVC   ACCSE,CTSYSSE          SAVE ACC SE NUMBER                        
*                                                                               
         L     RF,CSHUTLA                                                       
         MVC   4(1,RF),ACCSE       SET ACCOUNTING SE NUMBER                     
*                                                                               
*        OPEN ACCOUNTING FILES                                                  
*                                                                               
         GOTO1 CSHDMGRA,DMCB,=C'DMOPEN',=C'ACCOUNT',ACCFILES,DMWRK              
*                                                                               
*        READ AND SET COMPANY RECORD VALUES                                     
*                                                                               
         MVI   SVNEWOF,C'N'       NEW OFFICE CODES DEFAULT TO NO                
*                                                                               
         LA    CPYRECRG,AKEY      READ COMPANY RECORD                           
         USING CPYRECD,R2                                                       
*                                                                               
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,ACCCD      COMPANY CODE                                  
*                                                                               
         MVC   AKEYSAVE,AKEY      SAVE START KEY                                
*                                                                               
         GOTO1 CSHDMGRA,DMCB,=CL8'DMRDHI',=CL8'ACCDIR',AKEY,AKEY                
*                                                                               
         CLC   CPYKEY,AKEYSAVE      MUST FIND RECORD                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 CSHDMGRA,DMCB,=CL8'GETREC',=CL8'ACCMST',AKEY+50,AIO1,   X        
               DMWRK                                                            
*                                                                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                MUST FIND RECORD                             
*                                                                               
         L     CPYRECRG,AIO1       NOW POINT TO RECORD                          
*                                                                               
         LA    CPYELDRG,CPYRFST    POINT TO FIRST ELM IN COMPANY REC            
         USING CPYELD,CPYELDRG                                                  
         SR    R0,R0                                                            
*                                                                               
*        FIND COMPANY ELEMENT                                                   
*                                                                               
CPYELMLP DS    0H                                                               
*                                                                               
         CLI   CPYEL,0            MUST FIND ELEMENT BEFORE END OF REC           
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   CPYEL,CPYELQ       MUST HAVE X'10' ELEMENT                       
         BE    CPYELMDN                                                         
*                                                                               
CPYELMCN DS    0H                                                               
*                                                                               
         ICM   R0,1,CPYLN         LENGTH OF ELEMENT                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    CPYELDRG,R0        BUMP TO NEXT ELEMENT                          
         B     CPYELMLP                                                         
*                                                                               
CPYELMDN DS    0H                                                               
*                                                                               
         TM    CPYSTAT4,CPYSOFF2                                                
         BNO   *+8                                                              
         MVI   SVNEWOF,C'Y'       NEW OFFICE CODES USED                         
*                                                                               
CTOPENX  DS    0H                                                               
*                                                                               
*        SOMETIMES WE NEED TO GET THE COMPANY CODE OF POSTING AGENCY            
*****                                                                           
*****    CLC   CSHAGYCH,=C'G+'     IF AGENCY G+                                 
*****    BNE   *+14                                                             
*****    CLC   SVBILLDT,SVGPDATE   IF AFTER CUT OFF DATE                        
*****    BNL   CTPOST                                                           
*                                                                               
         CLC   CSHAGYCH,=C'FM'     IF AGENCY FM                                 
         BNE   *+14                                                             
         CLC   SVBILLDT,SVFMDATE   IF AFTER CUT OFF DATE                        
         BNL   CTPOST                                                           
*                                                                               
         CLC   CSHAGYCH,=C'DF'     HARD CODE TEST FOR DF                        
         BNE   *+8                                                              
         CLI   CLTOFFSV,C'Z'                                                    
         BNE   *+8                                                              
         BE    CTPOST                                                           
*                                                                               
         CLC   CSHAGYCH,=C'OO'     HARD CODE TEST FOR OMD                       
         BE    CTPOST                                                           
*                                                                               
         CLC   CSHAGYCH,=C'RB'     HARD CODE TEST FOR RB                        
         BE    CTPOST                                                           
*                                                                               
         CLC   CSHAGYCH,=C'WD'     HARD CODE TEST FOR WD                        
         BE    CTPOST                                                           
*                                                                               
         CLC   CSHAGYCH,=C'FR'     HARD CODE TEST FOR FORD                      
         BE    CTPOST                                                           
*                                                                               
         CLC   CSHAGYCH,=C'BN'     HARD CODE TEST FOR BN                        
         BE    CTPOST                                                           
*                                                                               
         CLC   CSHAGYCH,=C'SJ'     HARD CODE TEST FOR SJR                       
         BE    CTPOST                                                           
*                                                                               
         CLC   CSHAGYCH,=C'DM'     HARD CODE TEST FOR DOREMUS                   
         BE    CTPOST                                                           
*                                                                               
         CLC   CSHAGYCH,=C'WT'     HARD CODE TEST FOR WITO                      
         BE    CTPOST                                                           
*                                                                               
         CLC   CSHAGYCH,=C'YN'     HARD CODE TEST FOR OMCO                      
         BE    CTPOST                                                           
*                                                                               
         CLC   CSHAGYCH,=C'H7'     HARD CODE TEST FOR MINDSHARE                 
         BE    CTPOST                                                           
*                                                                               
         CLC   CSHAGYCH,=C'M2'     HARD CODE TEST FOR MEDIACOM                  
         BE    CTPOST                                                           
*                                                                               
         CLC   CSHAGYCH,=C'OU'     HARD CODE TEST FOR PHDTO                     
         BE    CTPOST                                                           
*                                                                               
         CLC   CSHAGYCH,=C'YN'     HARD CODE TEST FOR YNR                       
         BNE   *+18                                                             
         OC    ACCAGACC,ACCAGACC   CHECK IF ACC AGENCY GIVEN                    
         BNZ   CTPOST                                                           
         B     CTPOSTX             ELSE SKIP POSTING                            
*                                                                               
****     CLC   SVBILLDT,SVYNDATE        IF AFTER CUT OFF DATE                   
****     BNL   CTPOST                                                           
*                                                                               
         CLC   CSHAGYCH,=C'TH'     HARD CODE TEST FOR DFZNYA                    
         BNE   CTPOSTX                                                          
*                                                                               
         CLC   A0PROF+14(2),=C'DW'                                              
         BE    CTPOST                                                           
         CLC   A0PROF+14(2),=C'TH'                                              
         BE    CTPOST                                                           
         CLC   A0PROF+14(2),=C'BS'                                              
         BE    CTPOST                                                           
*                                                                               
         CLC   ACCAGY,=C'DW'       IF ACC AGENCY OVERRIDE EXISTS                
         BE    *+10                                                             
         CLC   ACCAGY,=C'TH'       IF ACC AGENCY OVERRIDE EXISTS                
         BE    *+10                                                             
         CLC   ACCAGY,=C'BS'       IF ACC AGENCY OVERRIDE EXISTS                
         BNE   CTPOSTX                                                          
*                                                                               
CTPOST   DS    0H                                                               
*                                                                               
         L     RF,CSHUTLA          POINT TO UTL                                 
         MVI   4(RF),X'0A'         SET TO THE CONTROL SYSTEM                    
*                                                                               
         XC    KEY,KEY             INIT KEY AREA                                
         LA    CT5RECRG,KEY        ESTABLISH CT5REC (SYSTEM ACCESS REC)         
         USING CT5REC,CT5RECRG                                                  
*                                                                               
         MVI   CT5KTYP,CT5KTYPQ    RECORD ID                                    
         MVC   CT5KALPH,CSHAGYCH   SET AGENCY CODE                              
*                                                                               
         OC    ACCAGACC,ACCAGACC   IF ACC AGENCY GIVEN                          
         BZ    *+10                                                             
         MVC   CT5KALPH,ACCAGACC      USE IT                                    
*                                                                               
         CLC   CT5KALPH,=C'DF'     SPECIAL OVERRIDE FOR OFFICE Z                
         BNE   *+8                                                              
         CLI   CLTOFFSV,C'Z'                                                    
         BNE   *+10                                                             
         MVC   CT5KALPH,=C'DW'                                                  
*                                                                               
         CLC   CT5KALPH,=C'FR'     SPECIAL OVERRIDE FOR FORD                    
         BNE   CTPOST1                                                          
*                                                                               
         MVC   CT5KALPH,=C'H7'                                                  
*                                                                               
         CLC   SVBILLDT,=X'FAF8F0F5F0F1'    IF BEFORE SPECIAL DATE              
         BNL   *+10                                                             
         MVC   CT5KALPH,=C'JW'                 USE JWT                          
*                                                                               
CTPOST1  DS    0H                                                               
*                                                                               
         GOTO1 CSHDMGRA,DMCB,=CL8'DMREAD',=CL8'CTFILE',KEY,AIO1,0 READ          
         CLI   8(R1),0             NO ERRORS ALLOWED                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     CT5RECRG,AIO1       POINT TO FOUND RECORD                        
*                                                                               
         LA    CTSYSDRG,CT5DATA    POINT TO FIRST ELEMENT IN RECORD             
         SR    R0,R0                                                            
*                                                                               
CTPOSTLP DS    0H                                                               
*                                                                               
         USING CTSYSD,CTSYSDRG     ESTABLISH SYS AUTH ELEMENT                   
*                                                                               
         CLI   CTSYSEL,0           MUST FIND AUTH ELEMENT                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   CTSYSEL,CTSYSELQ    FIND SYSTEM AUTH ELEMENT                     
         BNE   CTPOSTCN                                                         
*                                                                               
         CLI   CTSYSNUM,6          MUST BE FOR ACC SYSTEM                       
         BE    CTPOSTFD                                                         
*                                                                               
CTPOSTCN DS    0H                                                               
*                                                                               
         IC    R0,CTSYSLEN         BUMP TO NEXT ELEMENT                         
         AR    CTSYSDRG,R0                                                      
         B     CTPOSTLP                                                         
*                                                                               
CTPOSTFD DS    0H                                                               
*                                                                               
         OC    ACCAGACC,ACCAGACC   IF DIFFERENT ACC AGENCY                      
         BZ    *+20                                                             
         MVC   ACCSEACC,CTSYSSE       SAVE ACC AGENCY SE                        
         MVC   ACCCDACC,CTSYSAGB      SAVE COMPANY CODE                         
         B     CTPOST10                                                         
*                                                                               
         XC    ACCSEACC,ACCSEACC   CLEAR ACC AGENCY FLDS                        
         XC    ACCCDACC,ACCCDACC                                                
*                                                                               
         MVC   ACCCDPST,CTSYSAGB   SAVE COMPANY CODE                            
         MVC   ACCSEPST,CTSYSSE    SAVE SE NUMBER                               
*                                                                               
         CLC   ACCSE,ACCSEPST      IF NEW POSTING FILES                         
         BE    CTPOSTX                                                          
*                                                                               
CTPOST10 DS    0H                                                               
*                                                                               
         L     RF,CSHUTLA                                                       
         MVC   4(1,RF),ACCSEPST    SET ACCOUNTING SE NUMBER                     
*                                                                               
         OC    ACCAGACC,ACCAGACC   IF DIFFERENT ACC AGENCY                      
         BZ    *+10                                                             
         MVC   4(1,RF),ACCSEACC       SET ACC AGENCY SE                         
*                                                                               
*        OPEN ACCOUNTING FILES                                                  
*                                                                               
         GOTO1 CSHDMGRA,DMCB,=C'DMOPEN',=C'ACCOUNT',ACCFILES,DMWRK              
*                                                                               
CTPOSTX  DS    0H                                                               
*                                                                               
         L     RF,CSHUTLA          POINT TO UTL                                 
         MVC   4(1,RF),SYSTEMSV    RESTORE SYSTEM NUMBER                        
*                                                                               
         B     CTAUTHX                                                          
*                                                                               
ACCFILES DS    0X                  ACCOUNTING FILES                             
         DC    CL8' ACCDIR'                                                     
         DC    CL8' ACCMST'                                                     
         DC    CL8'X'              EOL                                          
*                                                                               
CTAUTHX  DS    0H                                                               
*                                                                               
         TITLE 'DDCASHIER - READ B1X PROFILE - RDB1X'                           
***********************************************************************         
*                                                                     *         
*        READ B1X PROFILE FOR AGENCY AND MEDIA                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RDB1X    DS    0H                                                               
*                                                                               
*        READ B1X PROFILE                                                       
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(1),CSHSYS      SYSTEM                                       
*                                                                               
         CLI   CSHSYS,CSHNETQ      IF NETWORK                                   
         BNE   *+8                                                              
         MVI   WORK,CSHSPTQ           TREAT AS SPOT                             
*                                                                               
         NI    WORK,X'FF'-X'40'    FORCE LOWERCASE                              
         MVC   WORK+1(3),=C'B1X'                                                
         MVC   WORK+4(2),CSHAGYCH  AGENCY                                       
         MVC   WORK+6(1),CSHMED    MEDIA                                        
*                                                                               
         CLC   B1XIDSV,WORK        SKIP IF PROFILE ALREADY RETRIEVED            
         BE    RDB1XX                                                           
*                                                                               
         MVC   B1XIDSV,WORK        SAVE PROFILE ID                              
         XC    B1XPROF,B1XPROF     INIT B1X PROF                                
*                                                                               
         GOTO1 CSHGTPRA,DMCB,WORK,B1XPROF,CSHDMGRA                              
*                                                                               
RDB1XX   DS    0H                                                               
*                                                                               
         XC    CSHRECA,CSHRECA     INIT RETURNED RECORD ADDRESS                 
*                                                                               
         ICM   R0,15,CSHBLLA       POINT TO BILL RECORD                         
         JZ    CSHEBLLN            NEED BILL RECORD                             
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,CSHBLLKL       USE BILL RECORD KEY LENGTH                   
*                                                                               
         TM    CSHCTL,CSHCBLLQ     IF BILL DATA WANTED                          
         BNO   *+8                                                              
         LH    R1,CSHBLLL             USE BILL RECORD LENGTH                    
*                                                                               
         LA    RF,3(R1)            LRECL INCLUDING LENGTH BYTES                 
*                                  AND TRAILING NULL                            
         STCM  RF,3,WRECL          SAVE RECORD LENGTH                           
*                                                                               
         LR    RF,R1               COPY BILL RECORD LENGTH                      
         LA    RE,WREC             POINT TO TSAR RECORD BUILD AREA              
         MVCL  RE,R0               MOVE BILL RECORD TO TSAR RECORD              
*                                                                               
         ICM   RF,3,WRECL          BILL RECORD LENGTH                           
         LA    RF,WRECL-1(RF)      NEXT AVAILABLE SLOT IN RECORD                
         MVI   0(RF),0             SET TRAILING NULL                            
*                                                                               
         ST    RF,WRECNXTA         SAVE A(NEXT AVAILABLE SLOT)                  
*                                                                               
         TM    CSHCTL,CSHCCSHQ     SKIP UNLESS CASH DATA WANTED                 
         BNO   CSHCSHX                                                          
*                                                                               
         TITLE 'DDCASHIER - FORMAT INVOICE NUMBER - SETINV'                     
***********************************************************************         
*                                                                     *         
*        FORMAT TRUE INVOICE NUMBER                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SETINV   DS    0H                                                               
*                                                                               
         CLI   CSHSYS,CSHPRTQ      SKIP UNLESS PRINT SYSTEM                     
         BNE   SETINVPN                                                         
*                                                                               
         L     PBILRCRG,CSHBLLA    ESTABLISH PRINT BILL RECORD                  
         USING PBILLRCD,PBILRCRG                                                
*                                                                               
         CLC   PBILIMO,=CL16' '    IF INVOICE MONTH AVAILABLE                   
         BNH   *+14                                                             
         MVC   WORK(2),PBILIMO        USE IT                                    
         B     SETINV30                                                         
*                                                                               
         MVC   WORK(2),PBILLDAT+2  BILL MONTH                                   
*                                                                               
         CLI   B1XPROF+4,0                                                      
         BE    SETINV20                                                         
*                                                                               
         PACK  DUB,PBILLDAT(2)    YEAR OF BILL                                  
         CVB   R0,DUB                                                           
         ZIC   RF,B1XPROF+4       INVOICE BASE YEAR                             
         SR    R0,RF              DIFFERENCE                                    
         BNP   SETINV20                                                         
*                                                                               
         MH    R0,=H'12'                                                        
         PACK  DUB,PBILLDAT+2(2)  BILL MONTH                                    
         CVB   RF,DUB                                                           
         AR    R0,RF                                                            
*                                                                               
SETINV10 CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FULL,DUB                                                         
         MVC   WORK(2),FULL+2     SET NEW INVOICE MONTH                         
         B     SETINV30                                                         
*                                                                               
SETINV20 CLI   B1XPROF+5,0        SEE IF BUMPING INV MONTH                      
         BE    SETINV30                                                         
*                                                                               
         PACK  DUB,PBILLDAT+2(2)  MONTH                                         
         CVB   RF,DUB                                                           
         ZIC   R0,B1XPROF+5                                                     
         AR    R0,RF                                                            
*                                                                               
         CH    R0,=H'12'                                                        
         BNH   SETINV10                                                         
*                                                                               
         SH    R0,=H'12'                                                        
         B     SETINV10                                                         
*                                                                               
SETINV30 MVC   HALF,PBILKBNO        BILL NUMBER                                 
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+2(4),DUB                                                    
*                                                                               
         MVC   BINVOICE,WORK       SAVE ACTUAL INVOICE NUMBER                   
*                                                                               
SETINVPX DS    0H                                                               
         B     SETINVX                                                          
*                                                                               
SETINVPN DS    0H                  NOT PRINT SYSTEM                             
*                                                                               
SETINVS  DS    0H                  SPOT INVOICE NUMBER                          
*                                                                               
         CLI   CSHSYS,CSHSPTQ      SKIP UNLESS SPOT SYSTEM                      
         BE    *+8                                                              
         CLI   CSHSYS,CSHNETQ      OR          NET  SYSTEM                      
         BNE   SETINVSN                                                         
*                                                                               
         USING BILLRECD,PBILRCRG                                                
*                                                                               
         L     PBILRCRG,CSHBLLA    ESTABLISH SPOT BILL RECORD                   
         USING BILLRECD,PBILRCRG                                                
*                                                                               
         MVC   BINVOICE,BINVNO     SAVE ACTUAL INVOICE NUMBER                   
*                                                                               
         B     SETINVSX                                                         
******                                                                          
******   MVC   WORK(2),BDATE+2     BILL MONTH                                   
******                                                                          
******   CLI   B1XPROF+4,0                                                      
******   BE    SETINVS2                                                         
******                                                                          
******   PACK  DUB,BDATE(2)       YEAR OF BILL                                  
******   CVB   R0,DUB                                                           
******   ZIC   RF,B1XPROF+4       INVOICE BASE YEAR                             
******   SR    R0,RF              DIFFERENCE                                    
******   BNP   SETINVS2                                                         
******                                                                          
******   MH    R0,=H'12'                                                        
******   PACK  DUB,BDATE+2(2)      BILL MONTH                                   
******   CVB   RF,DUB                                                           
******   AR    R0,RF                                                            
******                                                                          
******   CVD   R0,DUB                                                           
******   OI    DUB+7,X'0F'                                                      
******   UNPK  FULL,DUB                                                         
******   MVC   WORK(2),FULL+2      SET NEW INVOICE MONTH                        
******                                                                          
SETINVS2 DS    0H                                                               
******                                                                          
******   MVC   HALF,BKEYINV         BILL NUMBER                                 
******   LH    R0,HALF                                                          
******   CVD   R0,DUB                                                           
******   OI    DUB+7,X'0F'                                                      
******   UNPK  WORK+2(4),DUB                                                    
******                                                                          
******   MVC   BINVOICE,WORK       SAVE ACTUAL INVOICE NUMBER                   
*                                                                               
SETINVSX DS    0H                                                               
         B     SETINVX                                                          
*                                                                               
SETINVSN DS    0H                                                               
*                                                                               
         J     CASHIERX            UNKNOWN SYSTEM                               
*                                                                               
SETINVX  DS    0H                                                               
*                                                                               
         TITLE 'SWITCH TO ACC SYSTEM - ACCSWT'                                  
***********************************************************************         
*                                                                     *         
*        SWITCH TO ACC SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ACCSWT   DS    0H                                                               
*                                                                               
         L     RF,CSHUTLA          POINT TO UTL                                 
         MVC   4(1,RF),ACCSEPST    SET TO POSTING ACC SYSTEM                    
*                                                                               
ACCSWTX  DS    0H                                                               
*                                                                               
         TITLE 'DDCASHIER - READ MEDIA POSTING REC(S) FOR BILL - RDDTL'         
***********************************************************************         
*                                                                     *         
*        READ MEDIA POSTING RECORD(S) FOR GIVEN BILL                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RDDTL    DS    0H                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    MPDRECRG,KEY       ESTABLISH KEY AS MEDIAPOST DTL REC            
         USING MPDRECD,MPDRECRG                                                 
*                                                                               
         MVI   MPDKTYP,MPDKTYPQ   X'2F' - RECORD TYPE                           
         MVI   MPDKSUB,MPDKSUBQ   X'00' - RECORD SUB-TYPE                       
*                                                                               
         MVC   MPDKCPY,ACCCDPST   POSTING COMPANY CODE                          
         MVC   MPDKALPH,ACCSALPH  AGY ALPHA FOR SPLIT MEDIA FILES               
         MVC   MPDKSYS,CSHSYS     SYSTEM                                        
         MVC   MPDKMED,CSHMED     MEDIA                                         
         CLI   CSHSYS,CSHPRTQ     SKIP IF NOT PRINT SYSTEM                      
         BNE   RDDTLKPN                                                         
*                                                                               
RDDTLKP  DS    0H                                                               
*                                                                               
         USING PBILLRCD,PBILRCRG                                                
*                                                                               
         MVC   MPDKCLI,PBILKCLT   CLIENT                                        
         MVC   MPDKPRD,PBILKPRD   PRODUCT                                       
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,PBILKEST                                                    
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MPDKEST,DUB        ESTIMATE #                                    
*                                                                               
         MVC   FULL(2),PBILKBMN   (YM) BILLING MONTH                            
         MVI   FULL+2,1                                                         
*                                                                               
         GOTO1 CSHDATCA,DMCB,(3,FULL),(0,WORK)                                  
*                                                                               
         MVC   MPDKPER,WORK       SET BILLING MONTH (YYMM)                      
*                                                                               
         MVC   FULL,PBILKMOS      YM OF SERVICE                                 
         MVI   FULL+2,1                                                         
*                                                                               
         GOTO1 CSHDATCA,DMCB,(3,FULL),(0,WORK)                                  
*                                                                               
         MVC   MPDKMOS,WORK       MONTH OF SERVICE (YYMM)                       
*                                                                               
         EDIT  PBILKBNO,(5,MPDKINV),FILL=0                                      
*                                                                               
RDDTLKPX DS    0H                                                               
*                                                                               
         B     RDDTLKX                                                          
*                                                                               
RDDTLKPN DS    0H                                                               
*                                                                               
         CLI   CSHSYS,CSHSPTQ      SKIP IF NOT SPOT SYSTEM                      
         BE    *+8                                                              
         CLI   CSHSYS,CSHNETQ      OR          NET  SYSTEM                      
         BNE   RDDTLKSN                                                         
*                                                                               
RDDTLKS  DS    0H                                                               
*                                                                               
         USING BILLRECD,PBILRCRG                                                
*                                                                               
         CLI   BLMED,C' '          IF POSTING TYPE PASSED                       
         BNH   *+10                                                             
         MVC   MPDKMED,BLMED          USE IT                                    
*                                                                               
         L     RF,CSHCLTA          POINT TO CLIENT RECORD                       
         USING CLTHDRD,RF                                                       
*                                                                               
         SR    R0,R0                                                            
         IC    R0,CPROF+6          GET AAN OPTION                               
*                                                                               
         DROP  RF                                                               
*                                                                               
         GOTO1 =V(CLUNPK),DMCB,((R0),BKEYCLT),MPDKCLI,RR=RELO CLT CODE          
         MVC   MPDKPRD,BKEYPRD    PRODUCT                                       
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,BKEYEST                                                     
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MPDKEST,DUB        ESTIMATE #                                    
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,BKEYMBIL       BILLING MONTH YM                             
*                                                                               
         LR    RE,RF               COPY                                         
*                                                                               
         SLL   RE,28                                                            
         SRL   RE,28                                                            
         CVD   RE,DUB              GET MONTH IN PACKED FORMAT                   
         SRP   DUB,1,0             PUT MONTH IN ITS OWN BYTE                    
         MVC   FULL+1(1),DUB+6                                                  
*                                                                               
         SRL   RF,4                YEAR DIGIT                                   
*                                                                               
         IC    RE,PTODAY           PACKED YEAR FOR TODAY                        
*                                                                               
         LR    R1,RE               COPY                                         
*                                                                               
         SLL   RE,28                                                            
         SRL   RE,28               GET ONES DIGIT OF CURRENT YEAR               
*                                                                               
         SRL   R1,4                START OF CURRENT DECADE                      
         SLL   R1,4                START OF CURRENT DECADE                      
*                                                                               
         CR    RF,RE               IF LARGER THAN THIS YEAR                     
         BNH   *+16                THEN PREVIOUS DECADE                         
         SH    R1,=H'16'              BACK UP DECADE (PACKED FORMAT)            
         BNM   *+8                    IF RESULT NEGATIVE                        
         LA    R1,X'90'                  MUST BE 90'S                           
*                                                                               
         OR    RF,R1               ADD IN DECADE                                
         STC   RF,FULL             SET YEAR                                     
         MVI   FULL+2,1                                                         
*                                                                               
         GOTO1 CSHDATCA,DMCB,(1,FULL),(0,WORK)                                  
*                                                                               
         MVC   MPDKPER,WORK       SET BILLING MONTH (YYMM)                      
*                                                                               
         MVC   FULL,BKEYYSRV      YM OF SERVICE                                 
         MVI   FULL+2,1                                                         
*                                                                               
         GOTO1 CSHDATCA,DMCB,(3,FULL),(0,WORK)                                  
*                                                                               
         MVC   MPDKMOS,WORK       MONTH OF SERVICE (YYMM)                       
*                                                                               
         MVC   MPDKINV+1(4),BINVOICE+2  INVOICE NUMBER                          
         MVI   MPDKINV,C'0'        LEADING CH MUST BE 0                         
*                                                                               
******   EDIT  BKEYINV,(5,MPDKINV),FILL=0                                       
*                                                                               
RDDTLKSX DS    0H                                                               
*                                                                               
         B     RDDTLKX                                                          
*                                                                               
RDDTLKSN DS    0H                                                               
*                                                                               
         J     CASHIERX            UNKNOWN SYSTEM                               
*                                                                               
RDDTLKX  DS    0H                                                               
*                                                                               
*        READ TRANSFER DETAIL RECORDS                                           
*                                                                               
         MVC   KEYSAVE,KEY         SAVE START KEY                               
*                                                                               
         GOTO1 CSHDMGRA,DMCB,=CL8'DMRDHI',=CL8'ACCDIR',KEY,KEY                  
*                                                                               
RDDTLLP  DS    0H                                                               
*                                                                               
         CLI   8(R1),0             CHECK FOR ERRORS                             
         JNE   CSHERNF             MUST FIND A DETAIL RECORD                    
*                                                                               
         CLC   KEY(MPDKSEQ-MPDRECD),KEYSAVE CHECK FOR END OF DETAILS            
         BNE   RDDTLDN                                                          
*                                                                               
         GOTO1 CSHDMGRA,DMCB,=CL8'GETREC',=CL8'ACCMST',KEY+50,AIO1,    X        
               DMWRK                                                            
*                                                                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                MUST FIND RECORD                             
*                                                                               
         L     MPDRECRG,AIO1       NOW POINT TO RECORD                          
*                                                                               
         TITLE 'DDCASHIER - FIND BILL TRANSFER ELEMENTS - MBT'                  
***********************************************************************         
*                                                                     *         
*        FIND BILL TRANSFER ELEMENT                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MBT      DS    0H                                                               
*                                                                               
         LA    MBTELDRG,MPDRFST    POINT TO FIRST ELEMENT                       
         USING MBTELD,MBTELDRG     ESTABLISH AS MEDIA BILL TRANS ELM            
*                                                                               
MBTLOOP  DS    0H                                                               
*                                                                               
         CLI   MBTEL,0             CHECK FOR END OF RECORD                      
         BE    MBTDONE                                                          
*                                                                               
         CLI   MBTEL,MBTELQ        IGNORE ALL BUT TRANSFER ELMS                 
         BNE   MBTCONT                                                          
         CLI   MBTTYP,MBTTRCV      IGNORE ALL BUT RECEIVABLES                   
         BNE   MBTCONT                                                          
*                                                                               
*        ADD BILLING TRANSFER ELEMENT TO TSAR RECORD                            
*                                                                               
         ICM   R1,15,WRECNXTA      POINT TO NEXT AVAILABLE SLOT                 
         SR    RF,RF                                                            
         IC    RF,MBTLLN           ELEMENT LENGTH                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),MBTEL       SAVE MEDIA TRANSFER ELEMENT                  
*                                                                               
         LA    R1,1(RF,R1)         UPDATE NEXT AVAILABLE SLOT                   
         STCM  R1,15,WRECNXTA                                                   
*                                                                               
         MVI   0(R1),0             FORCE ENDING NULL                            
*                                                                               
         ICM   R1,3,WRECL          UPDATE RECORD LENGTH                         
         LA    R1,1(RF,R1)                                                      
         STCM  R1,3,WRECL                                                       
*                                                                               
         TITLE 'DDCASHIER - READ TRANSACTION RECORD - RDTRA'                    
***********************************************************************         
*                                                                     *         
*        READ TRANSACTION RECORD(S) FOR GIVEN BILL                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RDTRA    DS    0H                                                               
*                                                                               
         XC    TRNWORK,TRNWORK     INIT UNAPPLIED TRANSACTION ELEMENT           
*                                                                               
         ZAP   TRNAMNT-TRNELD+TRNWORK,=P'0'   INIT UNAPPLIED AMOUNT             
*                                                                               
         XC    AKEY,AKEY                                                        
         LA    TRNRECRG,AKEY       ESTABLISH TRANSACTION RECORD KEY             
         USING TRNRECD,TRNRECRG                                                 
*                                                                               
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCPY,ACCCD         ACCOUNTING COMPANY CODE                    
*                                                                               
         OC    ACCAGACC,ACCAGACC   IF DIFFERENT ACC AGENCY                      
         BZ    *+10                                                             
         MVC   TRNKCPY,ACCCDACC       SET ACC COMPANY CODE                      
*                                                                               
         MVC   TRNKUNT(14),MBTULA    ACCOUNT                                    
*                                                                               
         CLC   =C'SJ',MBTULA       IF POSTING TO PRODUCTION                     
         BNE   *+16                                                             
         MVC   TRNKWORK,MBTOFFC       ADD WORK CODE - STORED IN OFFICE          
         MVC   TRNKCCPY,ACCCD         COMPANY CODE                              
*                                                                               
         MVC   TRNKCUNT(14),MBTCNTRA CONTRA ACCOUNT                             
*                                                                               
         CLI   SVNEWOF,C'Y'          IF NEWOFF                                  
         BNE   *+10                                                             
         MVC   TRNKOFF,MBTOFFC       USE ACC OFF                                
*                                                                               
         CLI   CSHSYS,CSHSPTQ      IF SPOT SYSTEM                               
         BE    *+8                                                              
         CLI   CSHSYS,CSHNETQ      OR          NET  SYSTEM                      
         BNE   RDTRA10                                                          
*                                                                               
         LA    RF,BQDATE-BILLRECD(PBILRCRG)     USE REQUESTED DATE              
*                                                                               
         GOTO1 CSHDATCA,DMCB,(0,0(RF)),(1,TRNKDATE)                             
*                                                                               
         B     RDTRA15                                                          
*                                                                               
RDTRA10  DS    0H                                                               
*                                                                               
         LA    RF,PBILINVD-PBILLRCD(PBILRCRG)   ASSUME PRINT SYSTEM             
*                                                                               
         GOTO1 CSHDATCA,DMCB,(3,0(RF)),(1,TRNKDATE)                             
*                                                                               
RDTRA15  DS    0H                                                               
*                                                                               
         MVC   TRNKREF,BINVOICE      BILL REF #                                 
         MVI   TRNKSBR,0             SUB ZERO                                   
*                                                                               
         MVC   AKEYSAVE,AKEY         SAVE TRANSACTIION KEY BUILD                
*                                                                               
         L     RF,CSHUTLA          SWITCH TO CORRECT ACC FILES                  
         ICM   R0,1,4(RF)          SAVE CURRENT SE NUMBER                       
         MVC   4(1,RF),ACCSE       SET POSTED FILES SE                          
*                                                                               
         OC    ACCSEACC,ACCSEACC   IF DIFFERENT ACC AGENCY                      
         BZ    *+10                                                             
         MVC   4(1,RF),ACCSEACC       SET DIFFERENT FILES                       
*                                                                               
         MVI   TRNFLAG,0           INIT TRANSACTION ELEM FLAG                   
*                                                                               
         GOTO1 CSHDMGRA,DMCB,=CL8'DMRDHI',=CL8'ACCDIR',AKEY,AKEY,DMWRK          
*                                                                               
RDTRALP  DS    0H                                                               
*                                                                               
         CLC   AKEY(TRNKSBR-TRNKEY),AKEYSAVE  DONE IF RECORD NOT FOUND          
         BNE   RDTRADN                                                          
*                                                                               
         GOTO1 CSHDMGRA,DMCB,=CL8'GETREC',=CL8'ACCMST',AKEY+50,AIO2,   X        
               DMWRK                                                            
         CLI   8(R1),0             MUST FIND RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     TRNRECRG,AIO2       POINT TO FOUND RECORD                        
*                                                                               
         CLC   =C'SJ',TRNKUNT      IF BILLING THRU PRODUCTION                   
         BNE   RDTRAPDX                                                         
*                                                                               
         GOTOR PDTRN                  HANDLE PRODUCTION TRANSACTION REC         
*                                                                               
         B     RDTRACN                                                          
*                                                                               
RDTRAPDX DS    0H                                                               
*                                                                               
         GOTOR TRN                 ELSE SAVE ACTIVITY IN TSAR RECORD            
*                                                                               
         TITLE 'CONTINUATION OF READING TRANSACTION RECORDS - RDTRACN'          
***********************************************************************         
*                                                                     *         
*        CONTINUATION OF READING TRANSACTION RECORDS                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RDTRACN  DS    0H                                                               
*                                                                               
         LA    TRNRECRG,AKEY       POINT TO KEY AREA                            
*                                                                               
         GOTO1 CSHDMGRA,DMCB,=CL8'DMRSEQ',=CL8'ACCDIR',AKEY,AKEY,DMWRK          
*                                                                               
         B     RDTRALP                                                          
*                                                                               
RDTRADN  DS    0H                                                               
*                                                                               
         L     RF,CSHUTLA          SWITCH TO CORRECT ACC FILES                  
         STCM  R0,1,4(RF)          RESTORE CURRENT SE NUMBER                    
*                                                                               
         CP    TRNAMNT-TRNELD+TRNWORK,=P'0' HAVE UNAPPLIED?                     
         BNE   RDTRADN5            YES, ADD X'FF' ELEMENT                       
         CLI   CSHSYS,CSHPRTQ      PRINT SYSTEM?                                
         BNE   RDTRAX              NO - SKIP IF NO UNAPPLIED                    
         TM    TRNFLAG,TRNHAVE     HAVE TRANSACTION ELEMENT?                    
         BZ    RDTRAX              NO - TRULY HAVE NO $                         
         TM    TRNFLAG,TRNAPLD     HAVE APPLIED X'30' ELEMENT?                  
         BNZ   RDTRAX              YES, DON'T ADD X'FF' ELEMENT                 
*                                                                               
RDTRADN5 LA    TRNELDRG,TRNWORK    ESTABLISH AS TRANSACTION ELEMENT             
         USING TRNELD,TRNELDRG                                                  
*                                                                               
         MVI   TRNEL,X'FF'         SET ELEMENT ID                               
         MVI   TRNLN,TRNLN1Q       SET ELEMENT LENGTH                           
*                                                                               
*        ADD UNAPPLIED ELEMENT TO TSAR RECORD                                   
*                                                                               
         ICM   R1,15,WRECNXTA      POINT TO NEXT AVAILABLE SLOT                 
         SR    RF,RF                                                            
         IC    RF,TRNLN            ELEMENT LENGTH                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),TRNEL       SAVE TRANSACTION ELEMENT                     
*                                                                               
         LA    R1,1(RF,R1)         UPDATE NEXT AVAILABLE SLOT                   
         STCM  R1,15,WRECNXTA                                                   
*                                                                               
         MVI   0(R1),0             FORCE ENDING NULL                            
*                                                                               
         ICM   R1,3,WRECL          UPDATE RECORD LENGTH                         
         LA    R1,1(RF,R1)                                                      
         STCM  R1,3,WRECL                                                       
*                                                                               
RDTRAX   DS    0H                                                               
*                                                                               
         TITLE 'CONTINUATION OF FINDING TRANSFER ELEMENTS - MBTCONT'            
***********************************************************************         
*                                                                     *         
*        CONTINUATION OF FINDING TRANSFER ELEMENTS                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MBTCONT  DS    0H                                                               
*                                                                               
         LLC   RF,MBTLLN           BUMP TO NEXT ELEMENT                         
         LA    MBTELDRG,MBTELD(RF)                                              
*                                                                               
         B     MBTLOOP                                                          
*                                                                               
MBTDONE  DS    0H                                                               
*                                                                               
         TITLE 'CONTINUATION OF READING TRANSFER RECORDS - RDDTLCN'             
***********************************************************************         
*                                                                     *         
*        CONTINUATION OF FINDING TRANSFER ELEMENTS                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RDDTLCN  DS    0H                                                               
*                                                                               
         LA    MPDRECRG,KEY        POINT TO KEY AREA                            
*                                                                               
         GOTO1 CSHDMGRA,DMCB,=CL8'DMRDHI',=CL8'ACCDIR',KEY,KEY RESET            
*                                                                               
         GOTO1 CSHDMGRA,DMCB,=CL8'DMRSEQ',=CL8'ACCDIR',KEY,KEY NEXT DTL         
*                                                                               
         B     RDDTLLP                                                          
*                                                                               
RDDTLDN  DS    0H                                                               
*                                                                               
CSHCSHX  DS    0H                                                               
*                                                                               
         J     TSARADD                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'DDCASHIER - ADD TSAR RECORD TO BUFFER - TSARADD'                
***********************************************************************         
*                                                                     *         
*        ADD TSAR RECORD TO BUFFER                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
TSARADD  DS    0H                                                               
*                                                                               
         MVI   TSOFFACT,TSAADD     SET ACTION CODE                              
         LA    RF,WRECL            SET TSAR RECORD ADDRESS                      
         ST    RF,TSAREC                                                        
*                                                                               
         GOTOR CSHTSARA,(TSARRG)                                                
         CLI   TSERRS,0            CHECK FOR ERRORS                             
         JNE   TSARERRS                                                         
*                                                                               
         LA    RF,WREC             SET FOUND RECORD ADDRESS                     
         ST    RF,CSHRECA                                                       
*                                                                               
         J     CASHIERX            DONE                                         
*                                                                               
         TITLE 'DDCASHIER - READ TSAR RECORD - READ'                            
***********************************************************************         
*                                                                     *         
*        READ TSAR RECORD                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
READ     DS    0H                  FREE GETMAIN AREA                            
*                                                                               
         XC    CSHRECA,CSHRECA     INIT RETURNED RECORD ADDRESS                 
*                                                                               
         MVI   TSOFFACT,TSARDH     SET ACTION TO READ HIGH                      
*                                                                               
         L     R1,CSHBLLA          POINT TO BILL RECORD KEY                     
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,CSHBLLKL       BILL RECORD KEY LENGTH                       
         BCTR  RF,0                                                             
         LAY   RE,READEX           POINT TO EXECUTE STATEMENT                   
         EX    RF,0(RE)                                                         
*                                                                               
         LA    RF,WRECL            SET READ IN AREA                             
         ST    RF,TSAREC                                                        
*                                                                               
         GOTO1 CSHTSARA,(TSARRG)   READ RECORD                                  
*                                                                               
         TM    TSERRS,TSERNF       IF RECORD FOUND                              
         JO    READ10                                                           
*                                                                               
         CLI   TSERRS,0               CHECK FOR OTHER ERRORS                    
         JNE   TSARERRS                                                         
*                                                                               
         LA    RF,WREC                IF NONE SET FOUND RECORD ADDRESS          
         ST    RF,CSHRECA                                                       
*                                                                               
         J     READX                                                            
*                                                                               
READ10   DS    0H                  RECORD NOT FOUND                             
*                                                                               
         L     R1,CSHBLLA          POINT TO BILL RECORD KEY                     
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,CSHBLLKL       BILL RECORD KEY LENGTH                       
         BCTR  RF,0                                                             
         LAY   RE,READEX2          POINT TO EXECUTE STATEMENT                   
         EX    RF,0(RE)                                                         
         JNH   TSARERRS            NO - RETURN ERROR                            
*                                                                               
         LA    RF,WREC             ELSE SET FOUND RECORD ADDRESS                
         ST    RF,CSHRECA                                                       
*                                                                               
READX    DS    0H                                                               
*                                                                               
         J     CASHIERX                                                         
*                                                                               
READEX   MVC   WREC(0),0(R1)       MOVE BILL RECORD KEY TO I/OAREA              
READEX2  CLC   WREC(0),0(R1)       CHECK IF A HIGHER KEY WAS FOUND              
*                                                                               
         TITLE 'DDCASHIER - READ NEXT TSAR RECORD - NEXT'                       
***********************************************************************         
*                                                                     *         
*        READ NEXT TSAR RECORD                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
NEXT     DS    0H                                                               
*                                                                               
         XC    CSHRECA,CSHRECA     INIT RETURNED RECORD ADDRESS                 
*                                                                               
         MVI   TSOFFACT,TSANXT     SET ACTION TO NEXT                           
*                                                                               
         LA    RF,WRECL            SET READ IN AREA                             
         ST    RF,TSAREC                                                        
*                                                                               
         GOTO1 CSHTSARA,(TSARRG)   READ RECORD                                  
*                                                                               
         TM    TSERRS,TSERNF       OKAY IF RECORD NOT FOUND                     
         JO    *+12                                                             
         CLI   TSERRS,0            CHECK FOR ERRORS                             
         JNE   TSARERRS                                                         
*                                                                               
         LA    RF,WREC             SET FOUND RECORD ADDRESS                     
         ST    RF,CSHRECA                                                       
*                                                                               
NEXTX    DS    0H                                                               
*                                                                               
         J     CASHIERX                                                         
*                                                                               
         TITLE 'DDCASHIER - FREE UP TSAR BUFFER - CLOSE'                        
***********************************************************************         
*                                                                     *         
*        FREE UP GETMAIN AREA                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CLOSETS  DS    0H                  FREE GETMAIN AREA                            
*                                                                               
         GOTOR CLOSE                                                            
*                                                                               
CLOSETSX DS    0H                                                               
*                                                                               
         J     CASHIERX                                                         
*                                                                               
         TITLE 'EXIT ROUTINES   '                                               
***********************************************************************         
*                                                                     *         
*        EXIT  ROUTINES                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CASHIERX DS    0H                                                               
*                                                                               
         L     RF,CSHUTLA          POINT TO UTL                                 
         MVC   4(1,RF),SYSTEMSV    RESTORE SYSTEM NUMBER                        
*                                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'ERROR ROUTINES '                                                
***********************************************************************         
*                                                                     *         
*        ERROR ROUTINES                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
TSARERRS DS    0H                  TSAROFF ERRORS                               
         TM    TSERRS,TSEEOF       END OF FILE                                  
         JNO   *+12                                                             
         MVI   CSHERR,CSHEEOFQ                                                  
         J     CSHIERER                                                         
*                                                                               
         TM    TSERRS,TSEDUP       DUPLICATE KEY ON ADD                         
         JNO   *+12                                                             
         MVI   CSHERR,CSHEDUPQ                                                  
         J     CSHIERER                                                         
*                                                                               
         TM    TSERRS,TSERNF       RECORD NOT FOUND                             
         JNO   *+12                                                             
         MVI   CSHERR,CSHERNFQ                                                  
         J     CSHIERER                                                         
*                                                                               
         DC    H'0'                CAN'T HANDLE ERROR                           
*                                                                               
CSHERNF  DS    0H                  RECORD NOT FOUND                             
         MVI   CSHERR,CSHERNFQ                                                  
         J     CSHIERER                                                         
*                                                                               
CSHEBLLN DS    0H                  NO BILL RECORD ADDRESS                       
         MVI   CSHERR,CSHEBLLQ                                                  
         J     CSHIERER                                                         
*                                                                               
CSHIERER DS    0H                  CASH FOR BILL NOT RECEIVED                   
*                                                                               
         J     CASHIERX            EXIT                                         
*                                                                               
         TITLE 'DDCASHIER - CUTOF DATES - CUTDTS'                               
***********************************************************************         
*                                                                     *         
*        CUT OFF DATES                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CUTDTS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   WORK(3),=AL1(110,01,01) JAN1/10 IN BINARY                        
         GOTOR CSHDATCA,DMCB,(3,WORK),(0,SVYNDATE)   YYMMDD FORMAT              
*                                                                               
         MVC   WORK(3),=AL1(111,01,01) JAN1/11 IN BINARY                        
         GOTOR CSHDATCA,DMCB,(3,WORK),(0,SVM2DATE)   YYMMDD FORMAT              
*                                                                               
         MVC   WORK(3),=AL1(112,01,01) JAN1/12 IN BINARY                        
         GOTOR CSHDATCA,DMCB,(3,WORK),(0,SVFMDATE)   YYMMDD FORMAT              
*                                                                               
         MVC   WORK(3),=AL1(111,01,01) JAN1/11 IN BINARY                        
         GOTOR CSHDATCA,DMCB,(3,WORK),(0,SVGPDATE)   YYMMDD FORMAT              
*                                                                               
         MVC   WORK(3),=AL1(115,01,01) JAN1/15 IN BINARY                        
         GOTOR CSHDATCA,DMCB,(3,WORK),(0,SVRBDATE)   YYMMDD FORMAT              
*                                                                               
         MVC   WORK(3),=AL1(113,01,01) JAN1/13 IN BINARY                        
         GOTOR CSHDATCA,DMCB,(3,WORK),(0,SVWDDATE)   YYMMDD FORMAT              
*                                                                               
CUTDTSX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'DDCASHIER - INITIALIZATION - INIT'                              
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INIT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CSHIERWD,CSHWRKRG   LOCAL WORKING STORAGE                        
         USING CSHIERD,CSHIERRG    CASHIER CONTROL BLOCK                        
         USING CSHSV,CSHSVRG       ESTABLISH SAVEAREA                           
         USING TSARD,TSARRG        TSAROFF CONTROL BLOCK                        
*                                                                               
***********************************************************************         
*                                                                     *         
*        GET BUFFER FOR TSAROFF                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GETBUFF  DS    0H                                                               
*                                                                               
         OC    TSARBUFA,TSARBUFA   SKIP IF TSAROFF BUFFER AREA PRESENT          
         BNZ   GETBUFFX                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,CSHBLLKL       USE BILL RECORD KEY LENGTH                   
*                                                                               
         TM    CSHCTL,CSHCBLLQ     IF BILL DATA NEEDED                          
         BNO   *+8                                                              
         LH    RF,CSHBLLL             MAX LENGTH OF BILL RECORD                 
*                                                                               
         LA    RF,3(RF)            VARIABLE LENGTH RECORD LENGTH FIELD          
*                                     AND TRAILING NULL                         
         TM    CSHCTL,CSHCCSHQ     SKIP IF CASH DATA NOT NEEDED                 
         BNO   *+16                                                             
         LA    RE,TRNLN1Q+200         MAX ELEMENT LENGTH                        
         MH    RE,=Y(MAXTRNNM)        MAX NUMBER OF ELEMENTS                    
         LA    RF,0(RE,RF)            TRNELD ELMS AREA                          
*                                                                               
         STH   RF,TSARRECL         MAX RECORD LENGTH                            
****                                                                            
****     TM    CSHCTL,CSHCCSHQ     SKIP IF CASH DATA NOT NEEDED                 
****     BNO   *+14                                                             
*                                                                               
         LH    RF,=H'8192'            MAX RECORD LENGTH                         
         MVC   TSARRECL,=H'8192'      MAX RECORD LENGTH                         
*                                                                               
         L     RE,CSHMAX           MAX NUMBER OF RECORDS                        
         MR    RE,RE               BUFFER SIZE                                  
         A     RF,=F'7'            CLOSEST DOUBLE WORD                          
         SRA   RF,3                                                             
         SLA   RF,3                                                             
         ST    RF,TSARBUFL         SAVE LENGTH OF TSAR BUFFER                   
*                                                                               
         LR    R0,RF               COPY BUFFER LENGTH                           
*                                                                               
         GETMAIN RU,LV=(0),LOC=(ANY,ANY)    GET STORAGE                         
         LTR   RF,RF               NO ERRORS TOLERATED                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ST    R1,TSARBUFA         SAVE BUFFER ADDRESS                          
*                                                                               
GETBUFFX DS    0H                                                               
*                                                                               
         TITLE 'DDCASHIER - INITIALIZATION - TSARINI'                           
***********************************************************************         
*                                                                     *         
*        INITIALIZATION OF TSAROFF                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
*        INITIALIZE TSAROFF CONTROL BLOCK                                       
*                                                                               
         XC    TSARDC,TSARDC                                                    
         LA    TSARRG,TSARDC                                                    
         USING TSARD,TSARRG        ESTABLISH TSAR AREA                          
*                                                                               
         MVI   TSOFFACT,TSAINI     SET TO INITIALIZE                            
*                                                                               
         MVC   TSABUF,TSARBUFA     SET BUFFER ADDRESS                           
         MVC   TSAREC,TSARBUFL     SET BUFFER LENGTH                            
         MVC   TSKEYL,CSHBLLKL     SET KEY LENGTH                               
         MVC   TSRECL,TSARRECL     SET MAX RECORD LENGTH                        
         OI    TSRECI,TSRVAR       VARIABLE LENGTH RECORDS                      
         OI    TSIND2,TSI2MANY     ALLOW FOR BIG TABLES                         
         OI    TSIND2,TSI2OBIG     ALLOW FOR BIG RECORDS                        
*                                                                               
         GOTO1 CSHTSARA,(TSARRG)   INITIALIZE                                   
         CLI   TSERRS,0            NO ERRORS TOLERATED                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
INITX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'DDCASHIER - FREE UP TSAR BUFFER - ACCTSTS'                      
***********************************************************************         
*                                                                     *         
*        TEST FOR AGENCY RECEIVABLE FILE OVERRIDES                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ACCTSTS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CSHIERWD,CSHWRKRG   LOCAL WORKING STORAGE                        
         USING CSHIERD,CSHIERRG    CASHIER CONTROL BLOCK                        
         USING CSHSV,CSHSVRG       ESTABLISH SAVEAREA                           
         USING TSARD,TSARRG        TSAROFF CONTROL BLOCK                        
*                                                                               
         XC    ACCAGACC,ACCAGACC   INIT ACC AGENCY CODE                         
*                                                                               
         CLC   CSHAGYCH,=C'FM'     IF AGENCY FM                                 
         BNE   CTAUTFMN                                                         
*                                                                               
         CLC   SVBILLDT,SVFMDATE   SKIP IF AFTER CUT OFF DATE                   
         BNL   *+10                                                             
         MVC   ACCAGY,=C'FM'          SET TO ACC AGENCY FM                      
*                                                                               
         B     CTAUTH10                                                         
*                                                                               
CTAUTFMN DS    0H                                                               
*                                                                               
         CLC   CSHAGYCH,=C'G+'     IF AGENCY G+                                 
         BNE   CTAUTGPN                                                         
*                                                                               
         CLC   SVBILLDT,SVGPDATE   IF AFTER CUT OFF DATE                        
         BL    *+10                                                             
         MVC   ACCAGY,=C'GP'          SET TO ACC AGENCY GP                      
*                                                                               
         B     CTAUTH10                                                         
*                                                                               
CTAUTGPN DS    0H                                                               
*                                                                               
         CLC   CSHAGYCH,=C'M2'     IF AGENCY M2                                 
         BNE   CTAUTM2N                                                         
*                                                                               
         MVC   ACCAGY,=C'H7'          SET TO ACC AGENCY H7                      
*                                                                               
         CLC   SVBILLDT,SVM2DATE      IF BEFORE CUT OFF DATE                    
         BNL   *+10                                                             
         MVC   ACCAGY,=C'M2'             SET TO ACC AGENCY M2                   
*                                                                               
         B     CTAUTH10                                                         
*                                                                               
CTAUTM2N DS    0H                                                               
*                                                                               
         CLC   CSHAGYCH,=C'DF'     IF AGENCY DF                                 
         BNE   CTAUTDFN                                                         
*                                                                               
         CLI   CLTOFFSV,C'Z'       AND OFFICE Z                                 
         BNE   *+10                                                             
         MVC   ACCAGY,=C'TH'          SET TO ACC AGENCY TH                      
*                                                                               
         B     CTAUTH10                                                         
*                                                                               
CTAUTDFN DS    0H                                                               
*                                                                               
         CLC   CSHAGYCH,=C'H7'     IF MINDSHARE                                 
         BNE   CTAUTH7N                                                         
*                                                                               
         CLI   CSHSYS,CSHPRTQ      IF PRINT                                     
         BNE   CTH7PRTN                                                         
*                                                                               
         CLI   CLTOFFSV,C'J'       'J'                                          
         BE    *+8                                                              
         CLI   CLTOFFSV,C'X'       'X'                                          
         BE    *+8                                                              
         CLI   CLTOFFSV,C'3'       '3'                                          
         BE    *+8                                                              
         CLI   CLTOFFSV,C'4'       '4'                                          
         BNE   *+10                                                             
         MVC   ACCAGY,=C'JW'       SET TO ACC AGENCY JW                         
*                                                                               
         B     CTAUTH10                                                         
*                                                                               
                                                                                
CTH7PRTN DS    0H                                                               
*                                                                               
         CLI   CLTOFFSV,C'B'       'B'                                          
         BE    *+8                                                              
         CLI   CLTOFFSV,C'D'       'D'                                          
         BE    *+8                                                              
*** THIS WAS COPIED FROM AGY FR & X'41' MAPS TO OFF OX WHICH IS WRONG!          
***      CLI   CLTOFFSV,X'41'      'FD'                                         
***      BE    *+8                                                              
         CLI   CLTOFFSV,C'J'       'J'                                          
         BE    *+8                                                              
         CLI   CLTOFFSV,X'49'      'JD'                                         
         BE    *+8                                                              
         CLI   CLTOFFSV,X'4A'      'WD'                                         
         BE    *+8                                                              
         CLI   CLTOFFSV,C'X'       'X'                                          
         BE    *+8                                                              
         CLI   CLTOFFSV,C'3'       '3'                                          
         BE    *+8                                                              
         CLI   CLTOFFSV,C'4'       '4'                                          
         BNE   *+14                                                             
         MVC   ACCAGY,=C'JW'       SET TO ACC AGENCY JW                         
         B     CTAUTH10                                                         
*                                                                               
*                                  M2 FILE                                      
*                                                                               
         CLI   CLTOFFSV,X'68'      'DC'                                         
         BE    *+8                                                              
         CLI   CLTOFFSV,X'45'      'X1'                                         
         BE    *+8                                                              
         CLI   CLTOFFSV,X'43'      'X4'                                         
         BE    *+8                                                              
         CLI   CLTOFFSV,X'4F'      'XD'                                         
         BNE   CTH7M2N                                                          
*                                                                               
         MVC   ACCAGY,=C'M2'       SET TO ACC AGENCY M2                         
*                                                                               
         CLC   SVBILLDT,SVM2DATE   SKIP IF BEFORE CUT OFF DATE                  
         BL    *+10                                                             
         MVC   ACCAGY,=C'H7'       SET TO ACC AGENCY HT                         
*                                                                               
         B     CTAUTH10                                                         
*                                                                               
CTH7M2N  DS    0H                                                               
*                                                                               
*                                  YN FILE                                      
*                                                                               
         CLI   CLTOFFSV,X'51'      'ZD'                                         
         BE    *+8                                                              
         CLI   CLTOFFSV,X'46'      'Z1'                                         
         BE    *+8                                                              
         CLI   CLTOFFSV,X'D8'      'Z2'                                         
         BE    *+8                                                              
         CLI   CLTOFFSV,X'6C'      'Z3'                                         
         BE    *+8                                                              
         CLI   CLTOFFSV,X'C5'      'Z4'                                         
         BE    *+8                                                              
         CLI   CLTOFFSV,X'D7'      'Z5'                                         
         BE    *+8                                                              
         CLI   CLTOFFSV,X'E3'      'Z6'                                         
         BNE   CTH7PR10                                                         
*                                                                               
         CLC   SVBILLDT,SVYNDATE   SKIP IF AFTER CUT OFF DATE                   
         BNL   *+10                                                             
         MVC   ACCAGY,=C'YN'          SET TO ACC AGENCY YN                      
*                                                                               
CTH7PR10 DS    0H                                                               
         B     CTAUTH10                                                         
*                                                                               
CTAUTH7N DS    0H                                                               
*                                                                               
CTAUTFR  DS    0H                                                               
*                                                                               
         CLC   CSHAGYCH,=C'FR'     FORD                                         
         BNE   CTAUTFRN                                                         
*                                                                               
         CLI   CSHSYS,CSHPRTQ      IF PRINT                                     
         BNE   CTFRPRTN                                                         
*                                                                               
         CLI   CLTOFFSV,C'A'       'A'                                          
         BE    *+10                                                             
         MVC   ACCAGY,=C'JW'       SET TO ACC AGENCY JW                         
*                                                                               
         B     CTAUTH10                                                         
*                                                                               
                                                                                
CTFRPRTN DS    0H                                                               
*                                                                               
         CLI   CLTOFFSV,C'B'       'B'                                          
         BE    *+8                                                              
         CLI   CLTOFFSV,C'D'       'D'                                          
         BE    *+8                                                              
         CLI   CLTOFFSV,X'41'      'FD'                                         
         BE    *+8                                                              
         CLI   CLTOFFSV,C'J'       'J'                                          
         BE    *+8                                                              
         CLI   CLTOFFSV,X'52'      'JD' FOR AGY FR                              
         BE    *+8                                                              
         CLI   CLTOFFSV,X'48'      'T'                                          
         BE    *+8                                                              
         CLI   CLTOFFSV,X'53'      'U'                                          
         BE    *+8                                                              
         CLI   CLTOFFSV,X'54'      'V'                                          
         BE    *+8                                                              
         CLI   CLTOFFSV,X'4A'      'WD'                                         
         BE    *+8                                                              
         CLI   CLTOFFSV,C'X'       'X'                                          
         BE    *+8                                                              
         CLI   CLTOFFSV,C'Y'       'Y'                                          
         BE    *+8                                                              
         CLI   CLTOFFSV,C'3'       '3'                                          
         BE    *+8                                                              
         CLI   CLTOFFSV,C'4'       '4'                                          
         BNE   *+14                                                             
         MVC   ACCAGY,=C'JW'       SET TO ACC AGENCY JW                         
         B     CTAUTH10                                                         
*                                                                               
         CLI   CLTOFFSV,C'O'       'O'                                          
         BNE   *+22                                                             
         CLI   CSHSYS,CSHNETQ      SYSTEM NET                                   
         BNE   CTAUTH10                                                         
         MVC   ACCAGY,=C'JW'       SET TO ACC AGENCY JW                         
         B     CTAUTH10                                                         
*                                                                               
*                                  M2 FILE                                      
*                                                                               
         CLI   CLTOFFSV,X'42'      'X1'                                         
         BE    *+8                                                              
         CLI   CLTOFFSV,X'46'      'X4'                                         
         BE    *+8                                                              
         CLI   CLTOFFSV,X'4F'      'XD'                                         
         BNE   CTFRM2N                                                          
*                                                                               
         MVC   ACCAGY,=C'M2'       SET TO ACC AGENCY M2                         
*                                                                               
         CLC   SVBILLDT,SVM2DATE   SKIP IF BEFORE CUT OFF DATE                  
         BL    *+10                                                             
         MVC   ACCAGY,=C'H7'       SET TO ACC AGENCY H7                         
*                                                                               
         B     CTAUTH10                                                         
*                                                                               
CTFRM2N  DS    0H                                                               
*                                  YN FILE                                      
*                                                                               
         CLI   CLTOFFSV,X'51'      'ZD'                                         
         BE    *+8                                                              
         CLI   CLTOFFSV,X'47'      'Z1'                                         
         BE    *+8                                                              
         CLI   CLTOFFSV,X'D8'      'Z2'                                         
         BE    *+8                                                              
         CLI   CLTOFFSV,X'6C'      'Z3'                                         
         BE    *+8                                                              
         CLI   CLTOFFSV,X'C5'      'Z4'                                         
         BE    *+8                                                              
         CLI   CLTOFFSV,X'D7'      'Z5'                                         
         BE    *+8                                                              
         CLI   CLTOFFSV,X'E3'      'Z6'                                         
         BNE   CTFRPR10                                                         
*                                                                               
         MVC   ACCAGY,=C'H7'          SET TO ACC AGENCY H7                      
*                                                                               
         CLC   SVBILLDT,SVYNDATE   SKIP IF AFTER CUT OFF DATE                   
         BNL   *+10                                                             
         MVC   ACCAGY,=C'YN'          SET TO ACC AGENCY YN                      
*                                                                               
CTFRPR10 DS    0H                                                               
         B     CTAUTH10                                                         
*                                                                               
CTAUTFRN DS    0H                                                               
*                                                                               
         CLC   CSHAGYCH,=C'YN'     IF YNR                                       
         BNE   CTAUTYNN                                                         
*                                                                               
         CLI   CLTOFFSV,C'A'       'A'                                          
         BE    *+8                                                              
         CLI   CLTOFFSV,C'E'       'E'                                          
         BE    *+8                                                              
         CLI   CLTOFFSV,C'P'       'P'                                          
         BE    *+8                                                              
         CLI   CLTOFFSV,C'Q'       'Q'                                          
         BE    *+8                                                              
         CLI   CLTOFFSV,C'R'       'R'                                          
         BE    *+8                                                              
         CLI   CLTOFFSV,C'S'       'S'                                          
         BE    *+8                                                              
         CLI   CLTOFFSV,C'T'       'T'                                          
         BE    *+8                                                              
         CLI   CLTOFFSV,C'%'       '%'                                          
         BE    *+8                                                              
         CLI   CLTOFFSV,C'&&'      '&'                                          
         BE    *+8                                                              
         CLI   CLTOFFSV,C'#'       '#'                                          
         BNE   CTAUTYNN                                                         
*                                                                               
         CLC   SVBILLDT,SVYNDATE   IF BEFORE CUT OFF DATE                       
         BNL   *+14                                                             
         MVC   ACCAGY,=X'0000'     SET TO ACC AGENCY OF CALLER                  
         B     CTAUTH10                                                         
*                                                                               
         MVC   ACCAGACC,=C'H7'     SET POSTING AGENCY                           
*                                                                               
         B     CTAUTH10                                                         
*                                                                               
CTAUTYNN DS    0H                                                               
         CLC   CSHAGYCH,=C'RB'     IF AGENCY RB                                 
         BNE   CTAUTRBN                                                         
*                                                                               
         MVC   ACCAGY,=C'RB'       SET TO ACC AGENCY RB                         
*                                                                               
         CLC   SVBILLDT,SVRBDATE   PRIOR TO CUT OFF DATE?                       
         BL    *+10                YES - POST TO RB PRIOR TO 1/1/15             
         MVC   ACCAGY,=X'0000'     NO - SET TO ACC AGENCY OF CALLER             
*                                                                               
         B     CTAUTH10                                                         
*                                                                               
CTAUTRBN DS    0H                                                               
         CLC   CSHAGYCH,=C'WD'     IF AGENCY WD                                 
         BNE   CTAUTWDN                                                         
*                                                                               
         MVC   ACCAGY,=C'WD'       SET TO ACC AGENCY WD                         
*                                                                               
         CLC   SVBILLDT,SVWDDATE   BEFORE CUT OFF DATE?                         
         BL    *+10                YES                                          
         MVC   ACCAGY,=C'WN'       NO - SET TO ACC AGENCY WN                    
*                                                                               
         B     CTAUTH10                                                         
*                                                                               
CTAUTWDN DS    0H                                                               
*                                                                               
CTAUTH10 DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'DDCASHIER - FREE UP TSAR BUFFER - CLOSE'                        
***********************************************************************         
*                                                                     *         
*        FREE UP GETMAIN AREA                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CLOSE    NTR1  BASE=*,LABEL=*      FREE GETMAIN AREA                            
*                                                                               
         USING CSHIERWD,CSHWRKRG   LOCAL WORKING STORAGE                        
         USING CSHIERD,CSHIERRG    CASHIER CONTROL BLOCK                        
         USING CSHSV,CSHSVRG       ESTABLISH SAVEAREA                           
         USING TSARD,TSARRG        TSAROFF CONTROL BLOCK                        
*                                                                               
         L     R0,TSARBUFL         BUFFER LENGTH                                
         L     R1,TSARBUFA         BUFFER ADDRESS                               
*                                                                               
         FREEMAIN RC,A=(1),LV=(0)        FREE STORAGE                           
         LTR   RF,RF               NO ERRORS TOLERATED                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    TSARBUFA,TSARBUFA   CLEAR BUFFER AREA ADDRESS                    
*                                                                               
CLOSEX   DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'READ TRANSACTION ELEMENTS -TRN'                                 
***********************************************************************         
*                                                                     *         
*        READ TRANSACTION ELEMENTS AND ADD TO TSAR RECORD             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
TRN      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CSHIERWD,CSHWRKRG  LOCAL WORKING STORAGE                         
         USING CSHIERD,CSHIERRG   CASHIER CONTROL BLOCK                         
         USING CSHSV,CSHSVRG       ESTABLISH SAVEAREA                           
         USING TSARD,TSARRG       TSAROFF CONTROL BLOCK                         
*                                                                               
         USING TRNRECD,TRNRECRG    ESTABLISH TRANSACTION RECORD                 
*                                                                               
         LA    TRNELDRG,TRNRFST    POINT TO FIRST ELEMENT IN RECORD             
*                                                                               
TRNLOOP  DS    0H                                                               
*                                                                               
         USING TRNELD,TRNELDRG     ESTABLISH AS OTHER TYPE ELEMENT              
*                                                                               
         CLI   TRNEL,0             END OF RECORD?                               
         BE    TRNDONE                                                          
*                                                                               
         CLI   TRNEL,TRNELQ        LOOK FOR TRANSACTION ELEMENT                 
         BNE   TRNLP00                                                          
*                                                                               
         TM    TRNSTAT,TRNSREV    IGNORE REVERSALS                              
         BO    TRNLP00                                                          
*                                                                               
         CLI   CSHSYS,CSHNETQ      IF NETWORK                                   
         BNE   *+14                                                             
         CP    TRNAMNT,=P'0'          IGNORE ZERO TRANSACTIONS                  
         BE    TRNCONT                                                          
*                                                                               
         SP    TRNAMNT-TRNELD+TRNWORK,TRNAMNT  DECREMENT UNAPPLIED              
*                                                                               
         TM    TRNSTAT,TRNSDR     IF A DEBIT TRANSACTION                        
         BNO   *+16                                                             
         AP    TRNAMNT-TRNELD+TRNWORK,TRNAMNT  ADD TO UNAPPLIED TOTAL           
         AP    TRNAMNT-TRNELD+TRNWORK,TRNAMNT                                   
*                                                                               
         OI    TRNFLAG,TRNHAVE     HAVE TRANSACTION ELEM                        
         CLI   TRNTYPE,X'09'       ORIGINAL POSTING ELEMENT?                    
         BE    *+8                 YES                                          
         CLI   TRNTYPE,X'06'       ORIGINAL POSTING ELEMENT?                    
         BE    *+8                 YES                                          
         OI    TRNFLAG,TRNAPLD     HAVE APPLIED ELEM                            
         B     TRNLP01             KEEP TRANSACTION ELEMENT                     
*                                                                               
TRNLP00  DS    0H                                                               
*                                                                               
         CLI   TRNEL,RALELQ        KEEP RECEIVABLE ALLOCATION ELEMENT           
         BNE   TRNCONT                                                          
*                                                                               
TRNLP01  DS    0H                                                               
*                                                                               
*        ADD TRANSACTION ELEMENT TO TSAR RECORD                                 
*                                                                               
****     CLI   CSHSYS,CSHNETQ      IF NETWORK                                   
****     BNE   *+12                                                             
****     CLI   TRNTYPE,X'09'          SKIP ADDING REGULAR POSTING ELM           
****     BE    TRNADDX                                                          
*                                                                               
         ICM   R1,15,WRECNXTA      POINT TO NEXT AVAILABLE SLOT                 
         SR    RF,RF                                                            
         IC    RF,TRNLN            ELEMENT LENGTH                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),TRNEL       SAVE TRANSACTION ELEMENT                     
*                                                                               
         LA    R1,1(RF,R1)         UPDATE NEXT AVAILABLE SLOT                   
         STCM  R1,15,WRECNXTA                                                   
*                                                                               
         MVI   0(R1),0             FORCE ENDING NULL                            
*                                                                               
         ICM   R1,3,WRECL          UPDATE RECORD LENGTH                         
         LA    R1,1(RF,R1)                                                      
         STCM  R1,3,WRECL                                                       
*                                                                               
TRNADDX  DS    0H                                                               
*                                                                               
TRNCONT  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,TRNLN            BUMP TO NEXT ELEMENT                         
         LA    TRNELDRG,TRNELD(RF)                                              
*                                                                               
         B     TRNLOOP                                                          
*                                                                               
TRNDONE  DS    0H                                                               
*                                                                               
TRNX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRODUCTION BILLING - FIND BILL REFERENCE # - PDTRN'             
***********************************************************************         
*                                                                     *         
*        EXAMINE TRANSACTION RECORD FOR PROD BILL REF #               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PDTRN    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CSHIERWD,CSHWRKRG  LOCAL WORKING STORAGE                         
         USING CSHIERD,CSHIERRG   CASHIER CONTROL BLOCK                         
         USING CSHSV,CSHSVRG       ESTABLISH SAVEAREA                           
         USING TSARD,TSARRG       TSAROFF CONTROL BLOCK                         
*                                                                               
         USING TRNRECD,TRNRECRG    ESTABLISH TRANSACTION RECORD                 
         USING MBTELD,MBTELDRG     ESTABLISH MEDIA TRANSFER ELEMENT             
*                                                                               
*        FIND BILL NUMBER USED FOR PRODUCTION JOB                               
*                                                                               
         MVI   BNDSW,0             INIT BILLED SWITCH                           
*                                                                               
         LA    BNDELDRG,TRNRFST    POINT TO FIRST ELEMENT IN RECORD             
*                                                                               
PDTRNLP  DS    0H                                                               
*                                                                               
         USING BNDELD,BNDELDRG     ESTABLISH AS BILL NUMBER/DATE ELM            
*                                                                               
         CLI   BNDEL,0             END OF RECORD?                               
         BE    PDTRNDN                                                          
*                                                                               
         CLI   BNDEL,BNDELQ        LOOK FOR BILL NUMBER/DATE ELEMENT            
         BNE   PDTRBNDN                                                         
*                                                                               
         CLC   BNDBNO,SPACES       IGNORE IF NOT BILLED                         
         BNH   PDTRNCN                                                          
*                                                                               
         MVI   BNDSW,X'FF'         INDICATE SOMETHING BILLED                    
*                                                                               
         MVC   SVBNO,BNDBNO        SAVE PROD BILL NUMBER                        
*                                                                               
         GOTOR PDRCV               GET PRODUCTION RECEIVABLES                   
*                                                                               
         B     PDTRNCN                                                          
*                                                                               
PDTRBNDN DS    0H                                                               
*                                                                               
         USING PTAELD,PTAELDRG     ESTABLISH PROD TRAN ACTIVITY ELM             
*                                                                               
         CLI   PTAEL,PTAELQ        PRODUCTION TRANSACTION ACTIVITY              
         BNE   PDTRPTAN                                                         
*                                                                               
         CLI   PTATYPE,PTATRAL     MUST BE ALLOCATION TO BILL                   
         BNE   PDTRPTAN                                                         
*                                                                               
         MVI   BNDSW,X'FF'         INDICATE SOMETHING BILLED                    
*                                                                               
         MVC   SVBNO,PTARBLNO      SAVE BILL NUMBER                             
*                                                                               
         GOTOR PDRCV               FIND RECEIVABLES                             
*                                                                               
         B     PDTRNCN                                                          
*                                                                               
PDTRPTAN DS    0H                                                               
*                                                                               
PDTRNCN  DS    0H                                                               
*                                                                               
         USING BNDELD,BNDELDRG     RE-ESTABLISH ELEMENT DSECT                   
*                                                                               
         SR    RF,RF                                                            
         IC    RF,BNDLN            BUMP TO NEXT ELEMENT                         
         LA    BNDELDRG,BNDELD(RF)                                              
*                                                                               
         B     PDTRNLP                                                          
*                                                                               
PDTRNDN  DS    0H                                                               
*                                                                               
         CLI   BNDSW,0             IF AMOUNT NOT BILLED                         
         BNE   PDTRNDN1                                                         
*                                                                               
         ZAP   TRNAMNT-TRNELD+TRNWORK,MBTPOST  USE POSTED AMOUNT                
*                                                                               
PDTRNDN1 DS    0H                                                               
*                                                                               
PDTRNX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRODUCTION BILLING - FIND RECEIVALBLES - PDRCV'                 
***********************************************************************         
*                                                                     *         
*        FIND RECEIVABLE TRANSACTIONS FOR THIS PD JOB BILL            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PDRCV    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CSHIERWD,CSHWRKRG  LOCAL WORKING STORAGE                         
         USING CSHIERD,CSHIERRG   CASHIER CONTROL BLOCK                         
         USING CSHSV,CSHSVRG       ESTABLISH SAVEAREA                           
         USING TSARD,TSARRG       TSAROFF CONTROL BLOCK                         
*                                                                               
         USING TRNRECD,TRNRECRG    ESTABLISH TRANSACTION RECORD                 
         USING MBTELD,MBTELDRG     ESTABLISH BILL TRANSFER ELM                  
*                                                                               
*        FIND RECEIVABLE ACCOUNTS BY SEARCHING UNDER WORK CODE 99               
*                                                                               
         LA    TRNRECRG,PKEY       POINT TO KEYAREA                             
*                                                                               
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCPY,ACCCD         COMPANY CODE                               
         MVC   TRNKUNT(14),MBTULA    ACCOUNT                                    
         MVC   TRNKWORK,=C'99'       ADD WORK CODE 99                           
         MVC   TRNKCCPY,ACCCD        CONTRA COMPANY CODE                        
*                                                                               
         MVC   PKEYSAVE,PKEY         SAVE TRANSACTIION KEY BUILD                
*                                                                               
         GOTO1 CSHDMGRA,DMCB,=CL8'DMRDHI',=CL8'ACCDIR',PKEY,PKEY,DMWRK          
*                                                                               
RDRCVLP  DS    0H                                                               
*                                                                               
         CLC   PKEY(TRNKCUNT-TRNKEY),PKEYSAVE  DONE IF WRONG MBTULA             
         BNE   RDRCVDN                                                          
*                                                                               
         CLC   TRNKREF,SVBNO       MATCH REFERENCE TO BILL NO.                  
         BNE   RDRCVCN                                                          
*                                                                               
         GOTOR PDSRA               PROCESS SALES RECEIVABLE ACCT'S              
*                                                                               
RDRCVCN  DS    0H                                                               
*                                                                               
         LA    TRNRECRG,PKEY       POINT TO KEY AREA                            
*                                                                               
         GOTO1 CSHDMGRA,DMCB,=CL8'DMRSEQ',=CL8'ACCDIR',PKEY,PKEY,DMWRK          
*                                                                               
         B     RDRCVLP                                                          
*                                                                               
RDRCVDN  DS    0H                                                               
*                                  RESTORE DIRECTORY POINTER                    
         GOTO1 CSHDMGRA,DMCB,=CL8'DMRDHI',=CL8'ACCDIR',AKEY,AKEY,DMWRK          
*                                                                               
RDRCVX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRODUCTION BILLING - FIND RECEIVALBLES - PDSRA'                 
***********************************************************************         
*                                                                     *         
*        FIND FIND ACTIVITY FOR SALES RECEIVABLE ACCOUNT              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PDSRA    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING CSHIERWD,CSHWRKRG  LOCAL WORKING STORAGE                         
         USING CSHIERD,CSHIERRG   CASHIER CONTROL BLOCK                         
         USING CSHSV,CSHSVRG       ESTABLISH SAVEAREA                           
         USING TSARD,TSARRG       TSAROFF CONTROL BLOCK                         
*                                                                               
         USING TRNRECD,TRNRECRG    ESTABLISH TRANSACTION RECORD                 
         USING MBTELD,MBTELDRG     ESTABLISH BILL TRANSFER ELM                  
*                                                                               
*        FIND RECEIVABLE ACCOUNTS BY SEARCHING UNDER WORK CODE 99               
*                                                                               
         LA    TRNRECRG,RKEY       POINT TO KEYAREA                             
*                                                                               
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCULA,TRNKCULC-TRNKEY+PKEY   CONTRA ACCOUNT                   
*                                                                               
         MVC   RKEYSAVE,RKEY         SAVE TRANSACTIION KEY BUILD                
*                                                                               
         GOTO1 CSHDMGRA,DMCB,=CL8'DMRDHI',=CL8'ACCDIR',RKEY,RKEY,DMWRK          
*                                                                               
RDSRALP  DS    0H                                                               
*                                                                               
         CLC   RKEY(TRNKCUNT-TRNKEY),RKEYSAVE  DONE IF WRONG SR ACCT            
         BNE   RDSRADN                                                          
*                                                                               
         CLC   TRNKREF,TRNKREF-TRNKEY+PKEY     MATCH REFERENCE NUMBER           
         BNE   RDSRACN                                                          
*                                                                               
         CLC   TRNKDATE,TRNKDATE-TRNKEY+PKEY   MATCH TRANSACTION DATE           
         BNE   RDSRACN                                                          
*                                                                               
         GOTO1 CSHDMGRA,DMCB,=CL8'GETREC',=CL8'ACCMST',RKEY+50,AIO3,   X        
               DMWRK                                                            
         CLI   8(R1),0             MUST FIND RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     TRNRECRG,AIO3       POINT TO FOUND RECORD                        
*                                                                               
         GOTOR TRN                 PROCESS TRANSACTION RECORD                   
*                                                                               
         LA    TRNRECRG,RKEY       POINT TO KEY AREA                            
*                                                                               
RDSRACN  DS    0H                                                               
*                                                                               
         GOTO1 CSHDMGRA,DMCB,=CL8'DMRSEQ',=CL8'ACCDIR',RKEY,RKEY,DMWRK          
*                                                                               
         B     RDSRALP                                                          
*                                                                               
RDSRADN  DS    0H                                                               
*                                  RESTORE DIRECTORY POINTER                    
         GOTO1 CSHDMGRA,DMCB,=CL8'DMRDHI',=CL8'ACCDIR',PKEY,PKEY,DMWRK          
*                                                                               
RDSRAX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'SAVEAREA'                                                       
***********************************************************************         
*                                                                     *         
*        SAVEAREAS                                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CSHSV    DS    0D                  CASHIER SAVEAREAS                            
*                                                                               
SPACES   DC    CL80' '             SPACES                                       
MAXTRNNM EQU   150                 MAXIMUM TRANSACTION ELEMENTS                 
VCLUNPK  DS    A                   V(DDCLUNPK)                                  
*                                                                               
ACCCD    DS    CL1                 COMPANY CODE                                 
ACCCDPST DS    CL1                 POSTING AGENCY COMPANY CODE                  
ACCSE    DS    XL1                 ACC POWER CODE                               
ACCSEPST DS    XL1                 POSTING ACC POWER CODE                       
ACCSALPH DS    CL2                                                              
ACCAGY   DS    CL2                 ACC OVERRIDE AGENCY                          
ACCAGACC DS    CL2                 ACC POSTING  AGENCY                          
ACCSEACC DS    XL1                 AAC SE                                       
ACCCDACC DS    CL1                 ACC COMPANY CODE                             
*                                                                               
SVNEWOF  DS    CL1                 C'Y'   NEW OFFICE CODES USED                 
CLTOFFSV DS    CL1                 CLIENT OFFICE SAVEAREA                       
         DS    CL6                 SPARE                                        
*                                                                               
B1XIDSV  DC    XL8'00'             B1X PROFILE ID SAVEAREA                      
B1XPROF  DS    XL16                B1X PROFILE                                  
*                                                                               
A0IDSV   DC    XL16'00'            A0 PROFILE ID SAVEAREA                       
A0PROF   DS    XL16                A0 PROFILE                                   
*                                                                               
CT5KEYSV DC    XL(L'KEY)'00'       CTAUTH RECORD KEY SAVEAREA                   
CT5POSTS DC    XL(L'KEY)'00'       CTAUTH RECORD KEY SAVEAREA                   
*                                                                               
TSARBUFA DC    A(0)                A(TSAROFF GETMAIN BUFFER)                    
TSARBUFL DC    F'0'                A(TSAROFF GETMAIN BUFFER LENGTH)             
*                                                                               
*        TSAR CONTROL BLOCK                                                     
*                                                                               
TSARRECL DC    H'0'                MAX TSAR RECORD LENGTH                       
         DS    0D                                                               
TSARDC   DS    XL(TSARDL)          TSAROFF CONTROL BLOCK                        
*                                                                               
WRECNXTA DS    A                   A(NEXT AVAILABLE TRNELD AREA)                
*                                                                               
TRNWORK  DS    XL(TRNLN1Q)         UNAPPLIED TRANSACCTION ELEMENT               
*                                                                               
WRECL    DS    H                   TSAR RECORD LENGTH                           
WREC     DS    8192X               TSAR RECORD BUILD AREA                       
*                                                                               
         TITLE 'CASHIER CONTROL BLOCK - CSHIERD'                                
       ++INCLUDE DDCASHIERD                                                     
*                                                                               
         TITLE 'DDCASHIER - CASH RECEIVED WORKING STORAGE - CSHIERWD'           
***********************************************************************         
*                                                                     *         
*        CASH RECEIVED WORKING STORAGE - CSHIERWD                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CSHIERWD DSECT                                                                  
DUB      DS    D                   WORK AREAS                                   
RELO     DS    F                   RELOCATION FACTOR                            
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF2    DS    H                                                                
WORK     DS    CL80                                                             
*                                                                               
DMCB     DS    6A                                                               
*                                                                               
DMWRK    DS    XL512               DATAMGR WORKAREA                             
*                                                                               
KEY      DS    CL64                KEY BUILD AREA                               
KEYSAVE  DS    CL64                KEY BUILD SAVEAREA                           
*                                                                               
AKEY     DS    CL64                ACCOUNTING KEY BUILD AREA                    
AKEYSAVE DS    CL64                ACCOUNTING KEY BUILD SAVEAREA                
*                                                                               
PKEY     DS    CL64                PRODUCTION KEY BUILD AREA                    
PKEYSAVE DS    CL64                PRODUCTION KEY BUILD SAVEAREA                
*                                                                               
RKEY     DS    CL64                RECEIVABLE KEY BUILD AREA                    
RKEYSAVE DS    CL64                RECEIVABLE KEY BUILD SAVEAREA                
*                                                                               
CSHWPRMS DS    0A                  PARAMETER LIST                               
CSHWBLKA DS    A                   A(CSHIER CONTROL BLOCK)                      
CSHWPRML EQU   *-CSHWPRMS          LENGTH OF PARAMETER LIST                     
*                                                                               
PTODAY   DS    XL3                 TODAY'S DATE PACKED - YMD                    
BNDSW    DS    XL1                 NON-NULLS MEANS PD JOB BILLED                
SVBNO    DS    XL6                 PRODUCTION BILL NUMBER                       
SVTRNKEY DS    XL(L'TRNKEY)        TRANSACTION KEY SAVEAREA                     
*                                                                               
BINVOICE DS    CL6                 TRUE INVOICE NUMBER                          
*                                                                               
SVBILLDT DS    CL6                 BILLING DATE SAVEAREA                        
SVYNDATE DS    CL6                 YN CUTOFF DATE                               
SVM2DATE DS    CL6                 M2 CUTOFF DATE                               
SVFMDATE DS    CL6                 FM CUTOFF DATE                               
SVGPDATE DS    CL6                 G+ CUTOFF DATE                               
SVRBDATE DS    CL6                 RB CUTOFF DATE                               
SVWDDATE DS    CL6                 WD CUTOFF DATE                               
*                                                                               
AIO1     DS    A                   A(IOAREA1)                                   
AIO2     DS    A                   A(IOAREA2)                                   
AIO3     DS    A                   A(IOAREA3)                                   
*                                                                               
SYSTEMSV DS    X                   SYSTEM NUMBER SAVEAREA                       
*                                                                               
TRNFLAG  DS    X                   TRANSACTION ELEM FLAG                        
TRNHAVE  EQU   X'80'               HAVE TRANSACTION ELEMENT                     
TRNAPLD  EQU   X'40'               HAVE APPLIED ELEMENT                         
*                                                                               
         DS    0D                                                               
IOAREA1  DS    CL2048              IOAREA1                                      
IOAREA2  DS    CL2048              IOAREA2                                      
IOAREA3  DS    CL2048              IOAREA3                                      
*                                                                               
CSHIERWL EQU   *-CSHIERWD          LENGTH OF WORKING STORAGE                    
*                                                                               
         TITLE 'DDCASHIER - TSAROFF CONTROL BLOCK - TSARD'                      
***********************************************************************         
*                                                                     *         
*        TSAROFF CONTROL BLOCK                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
       ++INCLUDE DDTSARD                                                        
*                                                                               
         TITLE 'PRINT BILLING RECORD - PBILLREC'                                
***********************************************************************         
*                                                                     *         
*        PRINT BILLING RECORD - PBILLREC                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PBILLRCD DSECT BILLING RECORD                                                   
PBILKIDQ EQU   X'08'               BILLING RECORD ID                            
       ++INCLUDE PBILLREC                                                       
*                                                                               
         TITLE 'PRINT CLIENT RECORD - PCLTREC'                                  
***********************************************************************         
*                                                                     *         
*        PRINT CLIENT  RECORD - PCLTREC                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PCLTRECD DSECT                     CLIENT RECORD                                
PCLTKRDQ EQU   X'02'               CLIENT RECORD ID                             
       ++INCLUDE PCLTREC                                                        
*                                                                               
         TITLE 'SPOT BILLING RECORD - BILLREC'                                  
***********************************************************************         
*                                                                     *         
*        SPOT BILLING RECORD - BILLREC                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BILLRECD DSECT                     BILLING RECORD                               
BILKIDQ  EQU   X'00'               BILLING RECORD ID                            
       ++INCLUDE SPGENBILL                                                      
*                                                                               
         TITLE 'SPOT CLIENT HEADER - CLRHDR'                                    
***********************************************************************         
*                                                                     *         
*        SPOT CLIENT HEADER - CLTHDR                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CLTHDRD  DSECT                     CLIENT RECORD                                
CLTHDRQ  EQU   X'00'               CLIENT RECORD ID                             
       ++INCLUDE SPGENCLT                                                       
*                                                                               
         TITLE 'OTHER INCLUDED DSECTS'                                          
***********************************************************************         
*                                                                     *         
*        OTHER INCLUDED DSECTS                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*  ACGENFILE                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*  CTGENFILE                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*                                                                               
*  DMDTFIS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038DDCASHIER 10/13/15'                                      
         END                                                                    
