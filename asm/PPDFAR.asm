*          DATA SET PPDFAR     AT LEVEL 046 AS OF 02/25/20                      
*PHASE PPDFARB                                                                  
PPDFAR   TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - PPNTRY'              
***********************************************************************         
*  ID  LVL   DATE    TICKET            COMMENTS                       *         
* ---- --- ------- ------------ --------------------------------------*         
* JSAY 046 26AUG19 <SPEC-37978> Added entry for extended allocations  *         
***********************************************************************         
         PRINT NOGEN                                                            
PPDFAR   CSECT                                                                  
                                                                                
         J     PPNTRY                                                           
                                                                                
         DC    A(PFLIST)           A(SYSTEM/FILE LIST)                          
         DC    A(FILTAB)           A(FILE DEFINITION TABLE)                     
         DC    A(PRGTAB)           A(PROGRAM TABLE)                             
                                                                                
PPNTRY   NMOD1 0,*PPDFAR*                                                       
*                                                                               
         LR    RA,R1                                                            
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R9,DI_AWRK                                                       
         USING WORKD,R9            R9=A(LOCAL WORKING STORAGE)                  
*                                                                               
         L     R8,ALITS                                                         
         USING LITERALS,R8         R8=A(GLOBAL LITERALS)                        
*                                                                               
         L     R7,DI_ACOM                                                       
         USING COMFACSD,R7         R7=A(COMFACS)                                
*                                                                               
         STM   R2,RB,DI_R2RB       SAVE REGISTERS                               
*                                                                               
*        ANALYZE CALLING MODE                                                   
*                                                                               
         CLI   DI_MODE,DI_MINIQ    TEST INITIALIZATION                          
         BNE   *+12                                                             
         BRAS  RE,PPINIT                                                        
         B     PPNTRYX                                                          
*                                                                               
         CLI   DI_MODE,DI_MAGFQ    TEST FIRST FOR NEW AGENCY                    
         BNE   *+12                                                             
         BRAS  RE,PPAGYF                                                        
         B     PPNTRYX                                                          
*                                                                               
         DC    H'0'                UNKNOWN CALLING MODE                         
*                                                                               
PPNTRYX  DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
ALITS    DC    A(LITERALS)                                                      
*                                                                               
         DROP  RB                                                               
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - PPINIT'              
***********************************************************************         
*                                                                     *         
*        Initialization                                               *         
*                                                                     *         
***********************************************************************         
                                                                                
PPINIT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        BUILD AGENCY/MEDIA TABLE                                               
*                                                                               
         L     R2,DI_AAGY          BUILD AGENCY/MEDIA TABLE                     
         USING DA_D,R2             R2=A(AGENCY/MEDIA TABLE)                     
*                                                                               
         SHI   R2,DA_LNQ           INIT FOR LOOP                                
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING PAGYRECD,R3          ESTABLISH AGENCY RECORD                     
*                                                                               
*        READ FIRST AGENCY RECORD                                               
*                                                                               
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
         GOTOR CDATAMGR,DMCB,DMRDHI,PRTDIR,PAGYKEY,PAGYKEY                      
*                                                                               
PPIAGYLP DS    0H                                                               
*                                                                               
         TM    DMCB+8,X'80'        DONE AT END OF FILE                          
         BO    PPIAGYDN                                                         
*                                                                               
         CLI   DMCB+8,0            TEST FOR DATAMGR ERRORS                      
         JE    *+6                                                              
         DC    H'0'                NO OTHER ERROS TOLERATED                     
*                                                                               
         CLI   PAGYKRCD,PAGYKIDQ   SKIP IF NOT AGENCY RECORD                    
         BNE   PPIAGYCN                                                         
*                                  READ IN AGENCY RECORD                        
         GOTOR CDATAMGR,DMCB,GETREC,PRTFIL,PAGYREC+27,IO,WORK                   
         JE    *+6                 TEST FOR DATAMGR ERRORS                      
         DC    H'0'                                                             
*                                                                               
         LA    R3,IO               POINT TO RECORD                              
*                                                                               
         CLC   DA_ALF,PAGYKAGY     IF NEW AGENCY                                
         BE    PPIAGY10                                                         
*                                                                               
         AHI   R2,DA_LNQ              BUMP TO NEXT AGENCY TABLE ENTRY           
*                                                                               
         LA    R4,DA_MEDS                                                       
MEDS     USING DA_MEDS,R4          R4=A(MEDIA LIST IN AGENCY TABLE)             
*                                                                               
         MVI   MEDS.DA_MEDS,DA_MEOTQ    SET END OF TABLE INDICATOR              
*                                                                               
         MVC   DA_ALF,PAGYKAGY        SET AGENCY ALPHA ID                       
*                                                                               
         MVI   DA_CTRY,CTRYUSAQ                                                 
         CLI   PAGYNAT,C'C'           TEST CANADIAN                             
         JNE   *+8                                                              
         MVI   DA_CTRY,CTRYCANQ                                                 
*                                                                               
PPIAGY10 DS    0H                                                               
*                                                                               
         MVC   MEDS.DA_MCOD,PAGYKMED    SAVE MEDIA CODE                         
         AHI   R4,DA_MLNQ          BUMP TO NEXT ENTRY IN TABLE                  
         MVI   MEDS.DA_MCOD,DA_MEOTQ    SET NEW END OF TABLE                    
*                                                                               
PPIAGYNX DS    0H                  READ NEXT AGENCY/MEDIA ON FILE               
*                                                                               
         LA    R3,KEY              RE-POINT TO DIRECTORY KEY                    
*                                                                               
         MVI   PAGYKRCD,X'FF'      FORCE TO NEXT MEDIA                          
*                                                                               
         MVC   KEYSAVE,KEY         SAVE STARTING KEY                            
         GOTOR CDATAMGR,DMCB,DMRDHI,PRTDIR,PAGYKEY,PAGYKEY                      
*                                                                               
         B     PPIAGYLP                                                         
*                                                                               
PPIAGYCN DS    0H                  READ NEXT RECORD ON FILE                     
         GOTOR CDATAMGR,DMCB,DMRSEQ,PRTDIR,PAGYKEY,PAGYKEY                      
         B     PPIAGYLP                                                         
*                                                                               
PPIAGYDN DS    0H                                                               
*                                                                               
         C     R2,DI_AAGY          TEST ANY AGENCY RECORDS FOUND                
         JNH   PPINITXN               NO                                        
*                                                                               
         DROP  R2,R3                                                            
         DROP  MEDS                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
         LA    R2,CORPHS           LOAD CORE RESIDENT PHASES                    
         LA    R3,APHASES                                                       
         LA    R4,CORPHSN                                                       
         SR    R0,R0                                                            
*                                                                               
         ICM   R0,B'1110',=X'D9000A'                                            
         LA    R1,DMCB                                                          
         L     RF,CCALLOV                                                       
*                                                                               
PPINI20  ICM   R0,B'0001',0(R2)                                                 
         GOTOR (RF),(R1),0,(R0)                                                 
         MVC   0(4,R3),0(R1)                                                    
         AHI   R2,L'CORPHS                                                      
         AHI   R3,L'APHASES                                                     
         JCT   R4,PPINI20                                                       
*                                                                               
         XC    DMCB(12),DMCB       GET AND STORE PSTVAL ADDRESS                 
         MVC   DMCB+4(4),=X'D9000A6B'                                           
         GOTOR (RF),DMCB                                                        
         L     RF,DMCB                                                          
         ST    RF,VPSTVAL                                                       
*                                                                               
         XC    DMCB(12),DMCB       GET AND STORE PUBEDIT ADDRESS                
         MVC   DMCB+4(4),=X'D9000AB9'                                           
         GOTOR CCALLOV,DMCB                                                     
         L     RF,DMCB                                                          
         ST    RF,VPUBEDIT                                                      
*                                                                               
PPINITXY DS    0H                  SET EQ ERROR CODE                            
         CR    RB,RB                                                            
         B     PPINITX                                                          
*                                                                               
PPINITXN DS    0H                  SET NE ERROR CODE                            
         LTR   RB,RB                                                            
         B     PPINITX                                                          
*                                                                               
PPINITX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
*                                                                               
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - PPAGYF'              
***********************************************************************         
*                                                                     *         
*        First time for new agency                                    *         
*                                                                     *         
***********************************************************************         
                                                                                
PPAGYF   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        INIALIZE BUFFER                                                        
*                                                                               
         GOTOR DI_ABFIN,DMCB,('BUFFAINI',ABUFF1),BUFFREC,COMFACSD               
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CR    RB,RB               SET EQ CC                                    
*                                                                               
PPAGYFX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
* TEST IF RECORD CAME FROM PUBDIR/PUBFIL                                        
PUBFLT   L     R2,DI_AIO1                                                       
         CLI   4(R2),X'41'         PUBDIR                                       
         BER   RE                                                               
         CLI   4(R2),X'43'         PUBFIL                                       
         BR    RE                                                               
*                                                                               
*                                                                               
***********************************************************************         
* Exits etc.                                                          *         
***********************************************************************         
                                                                                
SETCCC   JE    *+8                 SET CONVERSE CONDITION CODE                  
         CR    RE,RE               NOT EQUAL TO EQUAL/NOT ZERO TO ZERO          
         BR    RE                                                               
         LTR   RE,RE               EQUAL TO NOT EQUAL/ZERO TO NOT ZERO          
         BR    RE                                                               
                                                                                
EXITL    DS    0H                  SET CONDITION CODE TO LOW                    
EXITN    LHI   RE,0                SET CONDITION CODE TO NOT EQUAL              
         J     EXITCC                                                           
EXITY    LHI   RE,1                SET CONDITION CODE TO EQUAL                  
         J     EXITCC                                                           
EXITH    LHI   RE,2                SET CONDITION CODE TO HIGH                   
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* File table                                                          *         
***********************************************************************         
                                                                                
FILTAB   DS    0XL(DF_LNQ)                                                      
                                                                                
         DC    X'42'                   FILE NUMBER                              
         DC    CL(L'DF_NAMEF)'PRTFIL'  FILE NAME FOR FILTERING                  
         DC    AL1(DF_TDA)             FILE TYPE                                
         DC    AL1(L'PCLTKEY)          KEY LENGTH                               
         DC    AL1(L'PCLTCNTL)         CONTROL BYTES                            
         DC    AL1(PCLTELEM-PCLTREC)   DISPLACEMENT TO FIRST ELEM               
         DC    AL4(FILREC)             A(RECORD TABLE)                          
*                                                                               
         DC    X'43'                   FILE NUMBER                              
         DC    CL(L'DF_NAMEF)'PUBFIL'  FILE NAME FOR FILTERING                  
         DC    AL1(DF_TDA)             FILE TYPE                                
         DC    AL1(L'PUBKEY)           KEY LENGTH                               
         DC    AL1(4)                  CONTROL BYTES                            
         DC    AL1(L'PUBKEY+8)         DISPLACEMENT TO FIRST ELEM               
         DC    AL4(FILPUB)             A(RECORD TABLE)                          
                                                                                
FILTABX  DC    AL1(DF_EOTQ)                                                     
                                                                                
***********************************************************************         
* Record definition tables                                            *         
***********************************************************************         
                                                                                
FILREC   DS    0X                  ** PRTFIL RECORD DEFINITIONS **              
                                                                                
FILCLT   DS    0X                  ** CLIENT RECORD DEFINITION **               
         DC    AL1(FILCLTX-*)                                                   
CLTRECQ  EQU   1                   RECORD INDENTIFIER                           
         DC    AL2(CLTRECQ)        RECORD CODE FOR DFAR                         
         DC    AL1(0)              INDICATOR BYTE 1                             
         DC    AL1(0)              INDICATOR BYTE 2                             
         DC    CL(L'DR_NAMEF)'CLIENT'  REC NAME FOR FILTERING                   
         DC    CL(L'DR_NAMES)'Client'  REC NAME FOR SENDING                     
         DC    AL1(PCLTKAGY-PCLTKEY)   DISPLACEMENT TO AGENCY ID                
         DC    AL1(PCLTKMED-PCLTKEY)   DISPLACEMENT TO MEDIA CODE               
         DC    AL1(0)                  OVERRIDE DISPLACEMENT TO 1ST ELM         
         DC    AL1(DR_ITALF+DR_ITMED)   2 CHAR. AGENCY ALPHA CODE               
*                                       1 BYTE  MEDIA CODE                      
         DC    AL4(0)              A(RECORD FILTER ROUTINE                      
         DC    AL4(CLTKEY)         A(KEY DEFINITION TABLE) (DK_D) OR            
*                                  A(KEY FORMAT ROUTINE) (DR_IKFRQ)             
         DC    AL4(CLTFLD)         A(DATA DEFINITION TABLE) (DD_D) OR           
*                                  A(DATA FORMAT ROUTINE) (DR_IDFRQ)            
*                                                                               
*        ARGUMENTS FOR CLIENT RECORD                                            
*                                                                               
*        AGENCY                                                                 
*                                                                               
CLTAAGY  DC    AL1(CLTAAGYX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PCLTKAGY-PCLTKEY)  DISPLACEMENT TO KEY VALUE                 
         DC    AL1(DR_ATNBZ)       ARG TYPE - NOT NULLS                         
         DC    AL1(L'PCLTKAGY)     ARGUMENT LENGTH                              
CLTAAGYX DS    0XL1                                                             
*                                                                               
*        MEDIA                                                                  
*                                                                               
CLTAMED  DC    AL1(CLTAMEDX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PCLTKMED-PCLTKEY)  DISPLACEMENT TO KEY VALUE                 
         DC    AL1(DR_ATNBZ)       ARG TYPE - NOT NULLS                         
         DC    AL1(L'PCLTKMED)     ARGUMENT LENGTH                              
CLTAMEDX DS    0XL1                                                             
*                                                                               
*        RECORD ID                                                              
*                                                                               
CLTARID  DC    AL1(CLTARIDX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PCLTKRCD-PCLTKEY)  DISPLACEMENT TO KEY VALUE                 
         DC    AL1(DR_ATEQU)       ARG TYPE - EQUAL TO ARGUMENT                 
         DC    AL1(PCLTKIDQ)       ARGUMENT VALUE                               
CLTARIDX DS    0XL1                                                             
*                                                                               
*        CLIENT                                                                 
*                                                                               
CLTACLT  DC    AL1(CLTACLTX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PCLTKCLT-PCLTKEY)   DISPLACEMENT TO KEY VALUE                
         DC    AL1(DR_ATNSP)       ARG TYPE - NOT EQUAL TO SPACES               
         DC    AL1(L'PCLTKCLT)     ARGUMENT LENGTH                              
CLTACLTX DS    0XL1                                                             
*                                                                               
*        NULLS                                                                  
*                                                                               
CLTARST  DC    AL1(CLTARSTX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PCLTKCLT+L'PCLTKCLT-PCLTKEY) DISP TO KEY VALUE               
         DC    AL1(DR_ATBZ)        ARG TYPE - EQUAL TO BINAY ZEROS              
         DC    AL1(L'PCLTKEY-(PCLTKCLT+L'PCLTKCLT-PCLTKEY)) ARG LENGTH          
CLTARSTX DS    0XL1                                                             
                                                                                
FILCLTX  DS    0X                                                               
                                                                                
*        PRODUCT RECORD                                                         
                                                                                
FILPRD   DS    0X                  ** PRODUCT RECORD DEFINITION **              
         DC    AL1(FILPRDX-*)                                                   
PRDRECQ  EQU   2                   RECORD INDENTIFIER                           
         DC    AL2(PRDRECQ)        RECORD CODE FOR DFAR                         
         DC    AL1(0)              INDICATOR BYTE 1                             
         DC    AL1(0)              INDICATOR BYTE 2                             
         DC    CL(L'DR_NAMEF)'PRODUCT' REC NAME FOR FILTERING                   
         DC    CL(L'DR_NAMES)'Product' REC NAME FOR SENDING                     
         DC    AL1(PPRDKAGY-PPRDKEY)   DISPLACEMENT TO AGENCY ID                
         DC    AL1(PPRDKMED-PPRDKEY)   DISPLACEMENT TO MEDIA CODE               
         DC    AL1(0)                  OVERRIDE DISPLACEMENT TO 1ST ELM         
         DC    AL1(DR_ITALF+DR_ITMED)   2 CHAR. AGENCY ALPHA CODE               
*                                       1 BYTE  MEDIA CODE                      
         DC    AL4(0)              A(RECORD FILTER ROUTINE                      
         DC    AL4(PRDKEY)         A(KEY DEFINITION TABLE) (DK_D) OR            
*                                  A(KEY FORMAT ROUTINE) (DR_IKFRQ)             
         DC    AL4(PRDFLD)         A(DATA DEFINITION TABLE) (DD_D) OR           
*                                  A(DATA FORMAT ROUTINE) (DR_IDFRQ)            
*                                                                               
*        ARGUMENTS FOR PRODUCT RECORD                                           
*                                                                               
*        AGENCY                                                                 
*                                                                               
PRDAAGY  DC    AL1(PRDAAGYX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PPRDKAGY-PPRDKEY)  DISPLACEMENT TO KEY VALUE                 
         DC    AL1(DR_ATNBZ)       ARG TYPE - NOT NULLS                         
         DC    AL1(L'PPRDKAGY)     ARGUMENT LENGTH                              
PRDAAGYX DS    0XL1                                                             
*                                                                               
*        MEDIA                                                                  
*                                                                               
PRDAMED  DC    AL1(PRDAMEDX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PPRDKMED-PPRDKEY)   DISPLACEMENT TO KEY VALUE                
         DC    AL1(DR_ATNBZ)       ARG TYPE - NOT NULLS                         
         DC    AL1(L'PPRDKMED)     ARGUMENT LENGTH                              
PRDAMEDX DS    0XL1                                                             
*                                                                               
*        RECORD ID                                                              
*                                                                               
PRDARID  DC    AL1(PRDARIDX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PPRDKRCD-PPRDKEY)   DISPLACEMENT TO KEY VALUE                
         DC    AL1(DR_ATEQU)       ARG TYPE - EQUAL TO ARGUMENT                 
         DC    AL1(PPRDKIDQ)       ARGUMENT VALUE                               
PRDARIDX DS    0XL1                                                             
*                                                                               
*        CLIENT                                                                 
*                                                                               
PRDACLT  DC    AL1(PRDACLTX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PPRDKCLT-PPRDKEY)   DISPLACEMENT TO KEY VALUE                
         DC    AL1(DR_ATNSP)       ARG TYPE - NOT EQUAL TO SPACES               
         DC    AL1(L'PPRDKCLT)     ARGUMENT LENGTH                              
PRDACLTX DS    0XL1                                                             
*                                                                               
*        PRODUCT                                                                
*                                                                               
PRDAPRD  DC    AL1(PRDAPRDX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PPRDKPRD-PPRDKEY)   DISPLACEMENT TO KEY VALUE                
         DC    AL1(DR_ATNSP)       ARG TYPE - NOT EQUAL TO SPACES               
         DC    AL1(L'PPRDKPRD)     ARGUMENT LENGTH                              
PRDAPRDX DS    0XL1                                                             
*                                                                               
*        NULLS                                                                  
*                                                                               
PRDARST  DC    AL1(PRDARSTX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PPRDKPRD+L'PPRDKPRD-PPRDKEY) DISP TO KEY VALUE               
         DC    AL1(DR_ATBZ)        ARG TYPE - EQUAL TO BINAY ZEROS              
         DC    AL1(L'PPRDKEY-(PPRDKPRD+L'PPRDKPRD-PPRDKEY)) ARG LENGTH          
PRDARSTX DS    0XL1                                                             
                                                                                
FILPRDX  DS    0X                                                               
                                                                                
FILEST   DS    0X                  ** ESTIMATE RECORD DEFINITION **             
         DC    AL1(FILESTX-*)                                                   
ESTRECQ  EQU   3                   RECORD IDENTIFIERR                           
         DC    AL2(ESTRECQ)        RECORD CODE FOR DFAR                         
         DC    AL1(0)              INDICATOR BYTE 1                             
         DC    AL1(0)              INDICATOR BYTE 2                             
         DC    CL(L'DR_NAMEF)'ESTIMATE' REC NAME FOR FILTERING                  
         DC    CL(L'DR_NAMES)'Estimate' REC NAME FOR SENDING                    
         DC    AL1(PESTKAGY-PESTKEY)   DISPLACEMENT TO AGENCY ID                
         DC    AL1(PESTKMED-PESTKEY)   DISPLACEMENT TO MEDIA CODE               
         DC    AL1(0)                  OVERRIDE DISPLACEMENT TO 1ST ELM         
         DC    AL1(DR_ITALF+DR_ITMED)   2 CHAR. AGENCY ALPHA CODE               
*                                       1 BYTE  MEDIA CODE                      
         DC    AL4(0)              A(RECORD FILTER ROUTINE                      
         DC    AL4(ESTKEY)         A(KEY DEFINITION TABLE) (DK_D) OR            
*                                  A(KEY FORMAT ROUTINE) (DR_IKFRQ)             
         DC    AL4(ESTFLD)         A(DATA DEFINITION TABLE) (DD_D) OR           
*                                  A(DATA FORMAT ROUTINE) (DR_IDFRQ)            
*                                                                               
*        ARGUMENTS FOR ESTIMATE RECORD                                          
*                                                                               
*        AGENCY                                                                 
*                                                                               
ESTAAGY  DC    AL1(ESTAAGYX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PESTKAGY-PESTKEY)  DISPLACEMENT TO KEY VALUE                 
         DC    AL1(DR_ATNBZ)       ARG TYPE - NOT NULLS                         
         DC    AL1(L'PESTKAGY)     ARGUMENT LENGTH                              
ESTAAGYX DS    0XL1                                                             
*                                                                               
*        MEDIA                                                                  
*                                                                               
ESTAMED  DC    AL1(ESTAMEDX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PESTKMED-PESTKEY)    DISPLACEMENT TO KEY VALUE               
         DC    AL1(DR_ATNBZ)       ARG TYPE - NOT NULLS                         
         DC    AL1(L'PESTKMED)     ARGUMENT LENGTH                              
ESTAMEDX DS    0XL1                                                             
*                                                                               
*        RECORD ID                                                              
*                                                                               
ESTARID  DC    AL1(ESTARIDX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PESTKRCD-PESTKEY)  DISPLACEMENT TO KEY VALUE                 
         DC    AL1(DR_ATEQU)       ARG TYPE - EQUAL TO ARGUMENT                 
         DC    AL1(PESTKIDQ)       ARGUMENT VALUE                               
ESTARIDX DS    0XL1                                                             
*                                                                               
*        CLIENT                                                                 
*                                                                               
ESTACLT  DC    AL1(ESTACLTX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PESTKCLT-PESTKEY)   DISPLACEMENT TO KEY VALUE                
         DC    AL1(DR_ATNSP)       ARG TYPE - NOT EQUAL TO SPACES               
         DC    AL1(L'PESTKCLT)     ARGUMENT LENGTH                              
ESTACLTX DS    0XL1                                                             
*                                                                               
*        PRODUCT                                                                
*                                                                               
ESTAPRD  DC    AL1(ESTAPRDX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PESTKPRD-PESTKEY)   DISPLACEMENT TO KEY VALUE                
         DC    AL1(DR_ATNSP)       ARG TYPE - NOT EQUAL TO SPACES               
         DC    AL1(L'PESTKPRD)     ARGUMENT LENGTH                              
ESTAPRDX DS    0XL1                                                             
*                                                                               
*        ESTIMATE                                                               
*                                                                               
ESTAEST  DC    AL1(ESTAESTX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PESTKEST-PESTKEY)   DISPLACEMENT TO KEY VALUE                
         DC    AL1(DR_ATNSP)       ARG TYPE - NOT EQUAL TO SPACES               
         DC    AL1(L'PESTKEST)     ARGUMENT LENGTH                              
ESTAESTX DS    0XL1                                                             
*                                                                               
*        NULLS                                                                  
*                                                                               
ESTARST  DC    AL1(ESTARSTX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PESTKEST+L'PESTKEST-PESTKEY) DISP TO KEY VALUE               
         DC    AL1(DR_ATBZ)        ARG TYPE - EQUAL TO BINAY ZEROS              
         DC    AL1(L'PESTKEY-(PESTKEST+L'PESTKEST-PESTKEY)) ARG LENGTH          
ESTARSTX DS    0XL1                                                             
                                                                                
FILESTX  DS    0X                                                               
                                                                                
FILREP   DS    0X                  ** REP RECORD DEFINITION **                  
         DC    AL1(FILREPX-*)                                                   
REPRECQ  EQU   PREPKIDQ            RECORD INDENTIFIER                           
         DC    AL2(REPRECQ)        RECORD CODE FOR DFAR                         
         DC    AL1(0)              INDICATOR BYTE 1                             
         DC    AL1(0)              INDICATOR BYTE 2                             
         DC    CL(L'DR_NAMEF)'REP'  REC NAME FOR FILTERING                      
         DC    CL(L'DR_NAMES)'Rep'  REC NAME FOR SENDING                        
         DC    AL1(PREPKAGY-PREPKEY)   DISPLACEMENT TO AGENCY ID                
         DC    AL1(PREPKMED-PREPKEY)   DISPLACEMENT TO MEDIA CODE               
         DC    AL1(0)                  OVERRIDE DISPLACEMENT TO 1ST ELM         
         DC    AL1(DR_ITALF+DR_ITMED)   2 CHAR. AGENCY ALPHA CODE               
*                                       1 BYTE  MEDIA CODE                      
         DC    AL4(0)              A(RECORD FILTER ROUTINE                      
         DC    AL4(REPKEY)         A(KEY DEFINITION TABLE) (DK_D) OR            
*                                  A(KEY FORMAT ROUTINE) (DR_IKFRQ)             
         DC    AL4(REPFLD)         A(DATA DEFINITION TABLE) (DD_D) OR           
*                                  A(DATA FORMAT ROUTINE) (DR_IDFRQ)            
*                                                                               
*        ARGUMENTS FOR REP RECORD                                               
*                                                                               
*        AGENCY                                                                 
*                                                                               
REPAAGY  DC    AL1(REPAAGYX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PREPKAGY-PREPKEY)  DISPLACEMENT TO KEY VALUE                 
         DC    AL1(DR_ATNBZ)       ARG TYPE - NOT NULLS                         
         DC    AL1(L'PREPKAGY)     ARGUMENT LENGTH                              
REPAAGYX DS    0XL1                                                             
*                                                                               
*        MEDIA                                                                  
*                                                                               
REPAMED  DC    AL1(REPAMEDX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PREPKMED-PREPKEY)  DISPLACEMENT TO KEY VALUE                 
         DC    AL1(DR_ATNBZ)       ARG TYPE - NOT NULLS                         
         DC    AL1(L'PREPKMED)     ARGUMENT LENGTH                              
REPAMEDX DS    0XL1                                                             
*                                                                               
*        RECORD ID                                                              
*                                                                               
REPARID  DC    AL1(REPARIDX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PREPKRCD-PREPKEY)  DISPLACEMENT TO KEY VALUE                 
         DC    AL1(DR_ATEQU)       ARG TYPE - EQUAL TO ARGUMENT                 
         DC    AL1(PREPKIDQ)       ARGUMENT VALUE                               
REPARIDX DS    0XL1                                                             
*                                                                               
*        REP                                                                    
*                                                                               
REPAREP  DC    AL1(REPAREPX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PREPKREP-PREPKEY)   DISPLACEMENT TO KEY VALUE                
         DC    AL1(DR_ATNBZ)       ARG TYPE - NOT NULLS                         
         DC    AL1(L'PREPKREP)     ARGUMENT LENGTH                              
REPAREPX DS    0XL1                                                             
*                                                                               
*        NULLS                                                                  
*                                                                               
REPARST  DC    AL1(REPARSTX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PREPKREP+L'PREPKREP-PREPKEY) DISP TO KEY VALUE               
         DC    AL1(DR_ATBZ)        ARG TYPE - EQUAL TO BINAY ZEROS              
         DC    AL1(L'PREPKEY-(PREPKREP+L'PREPKREP-PREPKEY)) ARG LENGTH          
REPARSTX DS    0XL1                                                             
                                                                                
FILREPX  DS    0X                                                               
                                                                                
FILAOR   DS    0X                  ** AOR RECORD DEFINITION **                  
         DC    AL1(FILAORX-*)                                                   
AORRECQ  EQU   AORKIDQ             RECORD IDENTIFIERR                           
         DC    AL2(AORRECQ)        RECORD CODE FOR DFAR                         
         DC    AL1(0)              INDICATOR BYTE 1                             
         DC    AL1(0)              INDICATOR BYTE 2                             
         DC    CL(L'DR_NAMEF)'AOR' REC NAME FOR FILTERING                       
         DC    CL(L'DR_NAMES)'AOR' REC NAME FOR SENDING                         
         DC    AL1(AORKAGY-AORKEY)   DISPLACEMENT TO AGENCY ID                  
         DC    AL1(AORKMED-AORKEY)   DISPLACEMENT TO MEDIA CODE                 
         DC    AL1(0)                  OVERRIDE DISPLACEMENT TO 1ST ELM         
         DC    AL1(DR_ITALF+DR_ITMED)   2 CHAR. AGENCY ALPHA CODE               
*                                       1 BYTE  MEDIA CODE                      
         DC    AL4(0)              A(RECORD FILTER ROUTINE                      
         DC    AL4(AORKEYF)        A(KEY DEFINITION TABLE) (DK_D) OR            
*                                  A(KEY FORMAT ROUTINE) (DR_IKFRQ)             
         DC    AL4(AORFLD)         A(DATA DEFINITION TABLE) (DD_D) OR           
*                                  A(DATA FORMAT ROUTINE) (DR_IDFRQ)            
*                                                                               
*        ARGUMENTS FOR AOR RECORD                                               
*                                                                               
*        AGENCY                                                                 
*                                                                               
AORAAGY  DC    AL1(AORAAGYX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(AORKAGY-AORKEY)  DISPLACEMENT TO KEY VALUE                   
         DC    AL1(DR_ATNBZ)       ARG TYPE - NOT NULLS                         
         DC    AL1(L'AORKAGY)      ARGUMENT LENGTH                              
AORAAGYX DS    0XL1                                                             
*                                                                               
*        MEDIA                                                                  
*                                                                               
AORAMED  DC    AL1(AORAMEDX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(AORKMED-AORKEY)    DISPLACEMENT TO KEY VALUE                 
         DC    AL1(DR_ATNBZ)       ARG TYPE - NOT NULLS                         
         DC    AL1(L'AORKMED)      ARGUMENT LENGTH                              
AORAMEDX DS    0XL1                                                             
*                                                                               
*        RECORD ID                                                              
*                                                                               
AORARID  DC    AL1(AORARIDX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(AORKRCD-AORKEY)  DISPLACEMENT TO KEY VALUE                   
         DC    AL1(DR_ATEQU)       ARG TYPE - EQUAL TO ARGUMENT                 
         DC    AL1(AORKIDQ)        ARGUMENT VALUE                               
AORARIDX DS    0XL1                                                             
*                                                                               
*        CLIENT                                                                 
*                                                                               
AORACLT  DC    AL1(AORACLTX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(AORKCLT-AORKEY)   DISPLACEMENT TO KEY VALUE                  
         DC    AL1(DR_ATNSP)       ARG TYPE - NOT EQUAL TO SPACES               
         DC    AL1(L'AORKCLT)      ARGUMENT LENGTH                              
AORACLTX DS    0XL1                                                             
*                                                                               
*        PRODUCT                                                                
*                                                                               
AORAPRD  DC    AL1(AORAPRDX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(AORKPRD-AORKEY)   DISPLACEMENT TO KEY VALUE                  
         DC    AL1(DR_ATNSP)       ARG TYPE - NOT EQUAL TO SPACES               
         DC    AL1(L'AORKPRD)      ARGUMENT LENGTH                              
AORAPRDX DS    0XL1                                                             
*                                                                               
*        ESTIMATE                                                               
*                                                                               
AORAEST  DC    AL1(AORAESTX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(AORKEST-AORKEY)   DISPLACEMENT TO KEY VALUE                  
         DC    AL1(DR_ATNSP)       ARG TYPE - NOT EQUAL TO SPACES               
         DC    AL1(L'AORKEST)      ARGUMENT LENGTH                              
AORAESTX DS    0XL1                                                             
*                                                                               
*        NULLS                                                                  
*                                                                               
AORARST  DC    AL1(AORARSTX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(AORKEST+L'AORKEST-AORKEY) DISP TO KEY VALUE                  
         DC    AL1(DR_ATBZ)        ARG TYPE - EQUAL TO BINARY ZEROS             
         DC    AL1(L'AORKEY-(AORKEST+L'AORKEST-AORKEY)) ARG LENGTH              
AORARSTX DS    0XL1                                                             
                                                                                
FILAORX  DS    0X                                                               
                                                                                
FILPBF   DS    0X                  ** BFORM (PBF) RECORD DEFINITION **          
         DC    AL1(FILPBFX-*)                                                   
PBFRECQ  EQU   PBFKTYPQ            RECORD IDENTIFIERR                           
         DC    AL2(PBFRECQ)        RECORD CODE FOR DFAR                         
         DC    AL1(0)              INDICATOR BYTE 1                             
         DC    AL1(0)              INDICATOR BYTE 2                             
         DC    CL(L'DR_NAMEF)'BFORM' REC NAME FOR FILTERING                     
         DC    CL(L'DR_NAMES)'BFORM' REC NAME FOR SENDING                       
         DC    AL1(PBFKAGY-PBFKEY)   DISPLACEMENT TO AGENCY ID                  
         DC    AL1(PBFKMED-PBFKEY)   DISPLACEMENT TO MEDIA CODE                 
         DC    AL1(0)                  OVERRIDE DISPLACEMENT TO 1ST ELM         
         DC    AL1(DR_ITALF+DR_ITMED)   2 CHAR. AGENCY ALPHA CODE               
*                                       1 BYTE  MEDIA CODE                      
         DC    AL4(0)              A(RECORD FILTER ROUTINE                      
         DC    AL4(PBFKEYF)        A(KEY DEFINITION TABLE) (DK_D) OR            
*                                  A(KEY FORMAT ROUTINE) (DR_IKFRQ)             
         DC    AL4(PBFFLD)         A(DATA DEFINITION TABLE) (DD_D) OR           
*                                  A(DATA FORMAT ROUTINE) (DR_IDFRQ)            
*                                                                               
*        ARGUMENTS FOR BFORM RECORD                                             
*                                                                               
*        AGENCY                                                                 
*                                                                               
PBFAAGY  DC    AL1(PBFAAGYX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PBFKAGY-PBFKEY)  DISPLACEMENT TO KEY VALUE                   
         DC    AL1(DR_ATNBZ)       ARG TYPE - NOT NULLS                         
         DC    AL1(L'PBFKAGY)      ARGUMENT LENGTH                              
PBFAAGYX DS    0XL1                                                             
*                                                                               
*        MEDIA                                                                  
*                                                                               
PBFAMED  DC    AL1(PBFAMEDX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PBFKMED-PBFKEY)    DISPLACEMENT TO KEY VALUE                 
         DC    AL1(DR_ATNBZ)       ARG TYPE - NOT NULLS                         
         DC    AL1(L'PBFKMED)      ARGUMENT LENGTH                              
PBFAMEDX DS    0XL1                                                             
*                                                                               
*        RECORD ID                                                              
*                                                                               
PBFARID  DC    AL1(PBFARIDX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PBFKTYPE-PBFKEY)  DISPLACEMENT TO KEY VALUE                  
         DC    AL1(DR_ATEQU)       ARG TYPE - EQUAL TO ARGUMENT                 
         DC    AL1(PBFKTYPQ)       ARGUMENT VALUE                               
PBFARIDX DS    0XL1                                                             
*                                                                               
*        CLIENT                                                                 
*                                                                               
PBFACLT  DC    AL1(PBFACLTX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PBFKCLT-PBFKEY)   DISPLACEMENT TO KEY VALUE                  
         DC    AL1(DR_ATNSP)       ARG TYPE - NOT EQUAL TO SPACES               
         DC    AL1(L'PBFKCLT)      ARGUMENT LENGTH                              
PBFACLTX DS    0XL1                                                             
*                                                                               
*        PRODUCT                                                                
*                                                                               
PBFAPRD  DC    AL1(PBFAPRDX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PBFKPRD-PBFKEY)   DISPLACEMENT TO KEY VALUE                  
         DC    AL1(DR_ATNSP)       ARG TYPE - NOT EQUAL TO SPACES               
         DC    AL1(L'PBFKPRD)      ARGUMENT LENGTH                              
PBFAPRDX DS    0XL1                                                             
*                                                                               
*        ESTIMATE                                                               
*                                                                               
PBFAEST  DC    AL1(PBFAESTX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PBFKEST-PBFKEY)   DISPLACEMENT TO KEY VALUE                  
         DC    AL1(DR_ATNSP)       ARG TYPE - NOT EQUAL TO SPACES               
         DC    AL1(L'PBFKEST)      ARGUMENT LENGTH                              
PBFAESTX DS    0XL1                                                             
*                                                                               
*        NULLS                                                                  
*                                                                               
PBFARST  DC    AL1(PBFARSTX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PBFKEST+L'PBFKEST-PBFKEY) DISP TO KEY VALUE                  
         DC    AL1(DR_ATBZ)        ARG TYPE - EQUAL TO BINARY ZEROS             
         DC    AL1(L'PBFKEY-(PBFKEST+L'PBFKEST-PBFKEY)) ARG LENGTH              
PBFARSTX DS    0XL1                                                             
                                                                                
FILPBFX  DS    0X                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FILPUB   DS    0X                  ** PUB RECORD DEFINITION **                  
         DC    AL1(FILPUBX-*)                                                   
PUBRECQ  EQU   PUBKTYPQ            RECORD IDENTIFIERR                           
         DC    AL2(PUBRECQ)        RECORD CODE FOR DFAR                         
         DC    AL1(0)              INDICATOR BYTE 1                             
         DC    AL1(0)              INDICATOR BYTE 2                             
         DC    CL(L'DR_NAMEF)'PUBREC' REC NAME FOR FILTERING                    
         DC    CL(L'DR_NAMES)'PUBREC' REC NAME FOR SENDING                      
         DC    AL1(PUBKAGY-PUBKEY)   DISPLACEMENT TO AGENCY ID                  
         DC    AL1(PUBKMED-PUBKEY)   DISPLACEMENT TO MEDIA CODE                 
         DC    AL1(0)                  OVERRIDE DISPLACEMENT TO 1ST ELM         
         DC    AL1(DR_ITALF+DR_ITMED)   2 CHAR. AGENCY ALPHA CODE               
*                                       1 BYTE  MEDIA CODE                      
         DC    AL4(PUBFLT)         A(RECORD FILTER ROUTINE                      
         DC    AL4(PUBKEYF)        A(KEY DEFINITION TABLE) (DK_D) OR            
*                                  A(KEY FORMAT ROUTINE) (DR_IKFRQ)             
         DC    AL4(PUBFLD)         A(DATA DEFINITION TABLE) (DD_D) OR           
*                                  A(DATA FORMAT ROUTINE) (DR_IDFRQ)            
*                                                                               
*        ARGUMENTS FOR PUB RECORD                                               
*                                                                               
*        MEDIA                                                                  
*                                                                               
PUBMED   DC    AL1(PUBMEDX-*)     TABLE ENTRY LENGTH                            
         DC    AL1(PUBKMED-PUBKEY)    DISPLACEMENT TO KEY VALUE                 
         DC    AL1(DR_ATNBZ)       ARG TYPE - NOT NULLS                         
         DC    AL1(L'PUBKMED)      ARGUMENT LENGTH                              
PUBMEDX  DS    0XL1                                                             
*                                                                               
*        PUB                                                                    
*                                                                               
PUBPUB   DC    AL1(PUBPUBX-*)     TABLE ENTRY LENGTH                            
         DC    AL1(PUBKPUB-PUBKEY)   DISPLACEMENT TO KEY VALUE                  
         DC    AL1(DR_ATNSP)       ARG TYPE - NOT SPACES                        
         DC    AL1(L'PUBKPUB)      ARGUMENT LENGTH                              
PUBPUBX  DS    0XL1                                                             
*                                                                               
*        ZONE                                                                   
*                                                                               
PUBZON   DC    AL1(PUBZONX-*)     TABLE ENTRY LENGTH                            
         DC    AL1(PUBKZON-PUBKEY)   DISPLACEMENT TO KEY VALUE                  
         DC    AL1(DR_ATNSP)       ARG TYPE - NOT SPACES                        
         DC    AL1(L'PUBKZON)      ARGUMENT LENGTH                              
PUBZONX  DS    0XL1                                                             
*                                                                               
*        ED                                                                     
*                                                                               
PUBED    DC    AL1(PUBEDX-*)     TABLE ENTRY LENGTH                             
         DC    AL1(PUBKED-PUBKEY)   DISPLACEMENT TO KEY VALUE                   
         DC    AL1(DR_ATNSP)       ARG TYPE - NOT SPACES                        
         DC    AL1(L'PUBKED)      ARGUMENT LENGTH                               
PUBEDX   DS    0XL1                                                             
*                                                                               
*        AGENCY                                                                 
*                                                                               
PUBAGY   DC    AL1(PUBAGYX-*)     TABLE ENTRY LENGTH                            
         DC    AL1(PUBKAGY-PUBKEY)  DISPLACEMENT TO KEY VALUE                   
         DC    AL1(DR_ATNBZ)       ARG TYPE - NOT NULLS                         
         DC    AL1(L'PUBKAGY)      ARGUMENT LENGTH                              
PUBAGYX  DS    0XL1                                                             
*                                                                               
*        RECORD ID                                                              
*                                                                               
PUBARID  DC    AL1(PUBARIDX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PUBKCOD-PUBKEY)  DISPLACEMENT TO KEY VALUE                   
         DC    AL1(DR_ATEQU)       ARG TYPE - EQUAL TO ARGUMENT                 
         DC    AL1(PUBKTYPQ)       ARGUMENT VALUE                               
PUBARIDX DS    0XL1                                                             
*                                                                               
*        NULLS                                                                  
*                                                                               
PUBARST  DC    AL1(PUBARSTX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PUBKCOD+L'PUBKCOD-PUBKEY) DISP TO KEY VALUE                  
         DC    AL1(DR_ATBZ)        ARG TYPE - EQUAL TO BINARY ZEROS             
         DC    AL1(L'PUBKEY-(PUBKCOD+L'PUBKCOD-PUBKEY)) ARG LENGTH              
PUBARSTX DS    0XL1                                                             
                                                                                
FILPUBX  DS    0X                                                               
                                                                                
PUBKTYPQ EQU   X'81'               PUB RECORD TYPE EQUATE                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FILPPA   DS    0X                  ** PUB PAY ADDRESS DEFINITION **             
         DC    AL1(FILPPAX-*)                                                   
PUBPPAQ  EQU   PUBKADRQ            RECORD IDENTIFIERR (PUB PAY ADDRESS)         
         DC    AL2(PUBPPAQ)        RECORD CODE FOR DFAR                         
         DC    AL1(0)              INDICATOR BYTE 1                             
         DC    AL1(0)              INDICATOR BYTE 2                             
         DC    CL(L'DR_NAMEF)'PUBPAYAD'                                         
         DC    CL(L'DR_NAMES)'PUBPAYAD'                                         
         DC    AL1(PUBKAGY-PUBKEY)   DISPLACEMENT TO AGENCY ID                  
         DC    AL1(PUBKMED-PUBKEY)   DISPLACEMENT TO MEDIA CODE                 
         DC    AL1(0)                  OVERRIDE DISPLACEMENT TO 1ST ELM         
         DC    AL1(DR_ITALF+DR_ITMED)   2 CHAR. AGENCY ALPHA CODE               
*                                       1 BYTE  MEDIA CODE                      
         DC    AL4(PUBFLT)         A(RECORD FILTER ROUTINE                      
         DC    AL4(PPAKEYF)        A(KEY DEFINITION TABLE) (DK_D) OR            
*                                  A(KEY FORMAT ROUTINE) (DR_IKFRQ)             
         DC    AL4(PPAFLD)         A(DATA DEFINITION TABLE) (DD_D) OR           
*                                  A(DATA FORMAT ROUTINE) (DR_IDFRQ)            
*                                                                               
*        ARGUMENTS FOR PUB RECORD                                               
*                                                                               
*        MEDIA                                                                  
*                                                                               
PPAMED   DC    AL1(PPAMEDX-*)     TABLE ENTRY LENGTH                            
         DC    AL1(PUBAKMED-PUBAKEY)                                            
         DC    AL1(DR_ATNBZ)       ARG TYPE - NOT NULLS                         
         DC    AL1(L'PUBAKMED)     ARGUMENT LENGTH                              
PPAMEDX  DS    0XL1                                                             
*                                                                               
*        PUB                                                                    
*                                                                               
PPAPUB   DC    AL1(PPAPUBX-*)     TABLE ENTRY LENGTH                            
         DC    AL1(PUBAKPUB-PUBAKEY)                                            
         DC    AL1(DR_ATNSP)       ARG TYPE - NOT SPACES                        
         DC    AL1(L'PUBAKPUB)     ARGUMENT LENGTH                              
PPAPUBX  DS    0XL1                                                             
*                                                                               
*        ZONE                                                                   
*                                                                               
PPAZON   DC    AL1(PPAZONX-*)     TABLE ENTRY LENGTH                            
         DC    AL1(PUBAKZON-PUBAKEY)                                            
         DC    AL1(DR_ATNSP)       ARG TYPE - NOT SPACES                        
         DC    AL1(L'PUBAKZON)     ARGUMENT LENGTH                              
PPAZONX  DS    0XL1                                                             
*                                                                               
*        ED                                                                     
*                                                                               
PPAED    DC    AL1(PPAEDX-*)     TABLE ENTRY LENGTH                             
         DC    AL1(PUBAKED-PUBAKEY)                                             
         DC    AL1(DR_ATNSP)       ARG TYPE - NOT SPACES                        
         DC    AL1(L'PUBAKED)      ARGUMENT LENGTH                              
PPAEDX   DS    0XL1                                                             
*                                                                               
*        AGENCY                                                                 
*                                                                               
PPAAGY   DC    AL1(PPAAGYX-*)     TABLE ENTRY LENGTH                            
         DC    AL1(PUBAKAGY-PUBAKEY)                                            
         DC    AL1(DR_ATNBZ)       ARG TYPE - NOT NULLS                         
         DC    AL1(L'PUBAKAGY)     ARGUMENT LENGTH                              
PPAAGYX  DS    0XL1                                                             
*                                                                               
*        RECORD ID                                                              
*                                                                               
PPAARID  DC    AL1(PPAARIDX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PUBAKCOD-PUBAKEY)                                            
         DC    AL1(DR_ATEQU)       ARG TYPE - EQUAL TO ARGUMENT                 
         DC    AL1(PUBKADRQ)       ARGUMENT VALUE                               
PPAARIDX DS    0XL1                                                             
*                                                                               
*        ADDRESS TYPE                                                           
*                                                                               
PPAATYP  DC    AL1(PPAATYPX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PUBAKTYP-PUBAKEY)                                            
         DC    AL1(DR_ATEQU)       ARG TYPE - EQUAL TO ARGUMENT                 
         DC    AL1(PUBKPADQ)       ARGUMENT VALUE                               
PPAATYPX DS    0XL1                                                             
*                                                                               
*        CLIENT CODE                                                            
*                                                                               
PPACLT   DC    AL1(PPACLTX-*)     TABLE ENTRY LENGTH                            
         DC    AL1(PUBAKCLT-PUBAKEY)                                            
         DC    AL1(DR_ATNBZ)       ARG TYPE - NOT NULLS                         
         DC    AL1(L'PUBAKCLT)     ARGUMENT LENGTH                              
PPACLTX  DS    0XL1                                                             
*                                                                               
*        NULLS                                                                  
*                                                                               
PPAARST  DC    AL1(PPAARSTX-*)     TABLE ENTRY LENGTH                           
         DC    AL1(PUBAKCLT+L'PUBAKCLT-PUBAKEY)                                 
         DC    AL1(DR_ATBZ)        ARG TYPE - EQUAL TO BINARY ZEROS             
         DC    AL1(L'PUBAKEY-(PUBAKCLT+L'PUBAKCLT-PUBAKEY))                     
PPAARSTX DS    0XL1                                                             
                                                                                
FILPPAX  DS    0X                                                               
                                                                                
PUBKADRQ EQU   X'82'               PUB ADDRESS RECORD TYPE EQUATE               
PUBKPADQ EQU   X'08'               PUB ADDRESS RECORD - PAYING ADDRESS          
                                                                                
FILRECX  DC    AL1(DR_EOTQ)                                                     
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        Record key and data definitions                              *         
*                                                                     *         
***********************************************************************         
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - CLIENT'              
***********************************************************************         
*                                                                     *         
*        CLIENT KEY AND RECORD DEFINITIONS                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CLTKEY   DS    0X                  ** CLIENT KEY DEFINITIONS **                 
                                                                                
*        AGENCY                                                                 
****                                                                            
****     DC    AL1(DK_TCHRQ+DK_TSAVQ)  CHAR & SAVE IN WORKSTORE                 
****     DC    AL1(PCLTKAGY-PCLTKEY,L'PCLTKAGY),AL4(0) DISP/LEN/RTN             
****     DC    AL2(RAGY-WORKD)     SAVE AREA DISPLACEMENT                       
****     DC    XL8'00'                                                          
                                                                                
*        MEDIA                                                                  
                                                                                
         DC    AL1(DK_TCHRQ+DK_TSAVQ)  CHAR & SAVE IN WORKSTORE                 
         DC    AL1(PCLTKMED-PCLTKEY,L'PCLTKMED),AL4(0) DISP/LEN/RTN             
         DC    AL2(RMED-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
*        CLIENT                                                                 
                                                                                
         DC    AL1(DK_TCHRQ+DK_TSAVQ)  CHAR & SAVE IN WORKSTORE                 
         DC    AL1(PCLTKCLT-PCLTKEY,L'PCLTKCLT),AL4(0) DISP/LEN/RTN             
         DC    AL2(RCLT-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
CLTKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
CLTFLD   DS    0X                  ** CLIENT FIELD DEFINITIONS **               
                                                                                
*        CLIENT NAME                                                            
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Client Name'  FIELD NAME                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCLTNAME-PCLTREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PCLTNAME)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
                                                                                
*        BILL RECEIPT NAME                                                      
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Bill Receipt Name'  FIELD NAME                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCLTBNAM-PCLTREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PCLTBNAM)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        ADDRESS LINE 1                                                         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Address Line 1'  FIELD NAME                        
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCLTLIN1-PCLTREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PCLTLIN1)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        ADDRESS LINE 2                                                         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Address Line 2'  FIELD NAME                        
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCLTLIN2-PCLTREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PCLTLIN2)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        ATTENTION                                                              
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Attention of'  FIELD NAME                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCLTATTN-PCLTREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PCLTATTN)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        CLIENT OFFICE CODE                                                     
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Client Office'                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCLTOFF-PCLTREC)  DISPLACEMENT OF FIELD IN REC               
         DC    AL1(L'PCLTOFF)      FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDTCLOFC)       FORMAT 2CH OFFICE CODE                       
         DC    XL12'00'                                                         
                                                                                
*        ACC OFFICE CODE                                                        
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Acc Office Code'                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCLTAOFC-PCLTREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PCLTAOFC)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        PRD ASSIGN (Y/N)   ** BIT SETTING IN PCLTSTAT **                       
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Prd Assign (Y/N)'                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCLTSTAT-PCLTREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PCLTSTAT)     FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(PRDASNE)        ANALYZE PCLTSTAT FOR PRD ASSIGN              
         DC    XL12'00'                                                         
                                                                                
*        TRAFFIC OFFICE CODE                                                    
                                                                                
         DC    AL1(DD_LDISQ)       DISPLACEMENT INTO SINGLE ELEMENT             
         DC    CL(L'DD_NAME)'Traffic Office Code'                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    X'50'               ELEMENT CODE                                 
         DC    AL1(PCLTTOFC-PCLTTOEL)  DISPLACEMENT TO ELEM DATA                
         DC    AL1(L'PCLTTOFC)     FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDTCLOFC)       FORMAT 2CH OFFICE CODE                       
         DC    XL12'00'                                                         
                                                                                
*        INTERFACE NUMBER                                                       
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Interface Number'                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCLTNUM-PCLTREC)  DISPLACEMENT OF FIELD IN REC               
         DC    AL1(L'PCLTNUM)      FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDINTFN)        FORMAT UP TO 5-CHAR OUTPUT                   
         DC    XL12'00'                                                         
                                                                                
*        CONTRACT STANDARD COMMENT   * CARRIED IN ELEMENT *                     
                                                                                
         DC    AL1(DD_LDISQ)       DISPLACEMENT INTO SINGLE ELEMENT             
         DC    CL(L'DD_NAME)'Contract Std Com'                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    X'10'               ELEMENT CODE                                 
         DC    AL1(PCLTCNUM-PCLTCSCM)  DISPLACEMENT TO ELEM DATA                
         DC    AL1(L'PCLTCNUM)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        I/O STANDARD COMMENT 1      * CARRIED IN ELEMENT *                     
                                                                                
         DC    AL1(DD_LDISQ)       DISPLACEMENT INTO SINGLE ELEMENT             
         DC    CL(L'DD_NAME)'I/O Std Com 1'                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    X'11'               ELEMENT CODE                                 
         DC    AL1(PCLTINUM-PCLTISCM)  DISPLACEMENT TO ELEM DATA                
         DC    AL1(L'PCLTINUM)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        I/O STANDARD COMMENT 2      * CARRIED IN ELEMENT *                     
                                                                                
         DC    AL1(DD_LDISQ)       DISPLACEMENT INTO SINGLE ELEMENT             
         DC    CL(L'DD_NAME)'I/O Std Com 2'                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    X'11'               ELEMENT CODE                                 
         DC    AL1(PCLTINUM-PCLTISCM)  DISPLACEMENT TO ELEM DATA                
         DC    AL1(L'PCLTINUM)     FIELD LENGTH                                 
         DC    AL1(DD_TSHRQ)       SYSTEM HANDLER PROCESSED                     
         DC    AL4(GETIC2)         LOOK FOR POSSIBLE 2ND ELEM & PROCESS         
         DC    XL12'00'                                                         
                                                                                
*        BILLING GROUP                                                          
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Billing Group'                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCLTBLGP-PCLTREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PCLTBLGP)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        DRD OVERRIDE CLIENT      * CARRIED IN ELEMENT *                        
                                                                                
         DC    AL1(DD_LDISQ)       DISPLACEMENT INTO SINGLE ELEMENT             
         DC    CL(L'DD_NAME)'DRD Override Clt'                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    X'30'               ELEMENT CODE                                 
         DC    AL1(PCLTDRDC-PCLTDRD)  DISPLACEMENT TO ELEM DATA                 
         DC    AL1(L'PCLTDRDC)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        FINANCIAL                                                              
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Financial'                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCLTFIN-PCLTREC)  DISPLACEMENT OF FIELD IN REC               
         DC    AL1(L'PCLTFIN)      FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        CANADIAN GST TAX CODE                                                  
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'GST Code'                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCLTGST-PCLTREC)  DISPLACEMENT OF FIELD IN REC               
         DC    AL1(L'PCLTGST)      FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        CANADIAN PST TAX CODE     * CARRIED IN ELEMENT *                       
                                                                                
         DC    AL1(DD_LDISQ)       DISPLACEMENT INTO SINGLE ELEMENT             
         DC    CL(L'DD_NAME)'PST'                                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    X'25'               ELEMENT CODE                                 
         DC    AL1(PCLTPSTC-PCLTPST)  DISPLACEMENT TO ELEM DATA                 
         DC    AL1(L'PCLTPSTC)     FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDTPST)         FORMAT CANADIAN PROVINCES                    
         DC    XL12'00'                                                         
                                                                                
*        CLIENT DIVISIONS          CARRIED IN CLIENT PROFILE (PCLTPROF)         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Client Divisions'                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCLTPROF+00-PCLTREC)   DISPL.OF FIELD IN REC                 
         DC    AL1(1)              FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        BILL DATE CALC OVERRIDE   CARRIED IN CLIENT PROFILE (PCLTPROF)         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Bill Dt Calc Ovrride'                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCLTPROF+01-PCLTREC)   DISPL.OF FIELD IN REC                 
         DC    AL1(1)              FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        BILL DATE -> BASE DATE    CARRIED IN CLIENT PROFILE (PCLTPROF)         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Bill Dt -> Base Date'                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCLTPROF+02-PCLTREC)   DISPL.OF FIELD IN REC                 
         DC    AL1(1)              FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        BILL DATE -> DATE ADJ.    CARRIED IN CLIENT PROFILE (PCLTPROF)         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Bill Dt -> Date Adj.'                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCLTPROF+03-PCLTREC)   DISPL.OF FIELD IN REC                 
         DC    AL1(1)              FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        NON-NEWSPAPER ADJUSTMENT  CARRIED IN CLIENT PROFILE (PCLTPROF)         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Non-newspaper Adjust'                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCLTPROF+04-PCLTREC)   DISPL.OF FIELD IN REC                 
         DC    AL1(1)              FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        CLIENT TYPE               CARRIED IN CLIENT PROFILE (PCLTPROF)         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Client Type'                                       
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCLTPROF+05-PCLTREC)   DISPL.OF FIELD IN REC                 
         DC    AL1(1)              FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        IF SUB, MASTER CLIENT     CARRIED IN CLIENT PROFILE (PCLTPROF)         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'If Sub,Master Client'                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCLTPROF+06-PCLTREC)   DISPL.OF FIELD IN REC                 
         DC    AL1(L'PCLTKCLT)     FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(MSTCLTE)        ANALYZE PCLTPROF FOR MASTER CLIENT           
         DC    XL12'00'                                                         
                                                                                
*        PRD/EST BILLING FORMULAS  CARRIED IN CLIENT PROFILE (PCLTPROF)         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Prd/Est Bill Formula'                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCLTPROF+10-PCLTREC)   DISPL.OF FIELD IN REC                 
         DC    AL1(1)              FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        CONTRACTS REQUIRED        CARRIED IN CLIENT PROFILE (PCLTPROF)         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Contracts Required'                                
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCLTPROF+12-PCLTREC)   DISPL.OF FIELD IN REC                 
         DC    AL1(1)              FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(CONREQE)        ANALYZE PCLTPROF FOR CONTRACTS REQD          
         DC    XL12'00'                                                         
                                                                                
*        BUDGET $ TYPE             CARRIED IN CLIENT PROFILE (PCLTPROF)         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Budget $ Type'                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCLTPROF+14-PCLTREC)   DISPL.OF FIELD IN REC                 
         DC    AL1(1)              FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        ESTIMATE ROUNDING         CARRIED IN CLIENT PROFILE (PCLTPROF)         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Estimate Rounding'                                 
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCLTPROF+18-PCLTREC)   DISPL.OF FIELD IN REC                 
         DC    AL1(1)              FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        MEDIA NAME OVERRIDE      * CARRIED IN ELEMENT *                        
                                                                                
         DC    AL1(DD_LDISQ)       DISPLACEMENT INTO SINGLE ELEMENT             
         DC    CL(L'DD_NAME)'Media Name Override'                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    X'41'               ELEMENT CODE                                 
         DC    AL1(PCLTMNAM-PCLTMEL)  DISPLACEMENT TO ELEM DATA                 
         DC    AL1(L'PCLTMNAM)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        SPECIAL FINANCIAL HANDLING    ** BIT SETTING IN PCLTSTAT **            
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Spcl Financl Handlng'                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCLTSTAT-PCLTREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PCLTSTAT)     FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(SFHE)           ANALYZE PCLTSTAT FOR SFH                     
         DC    XL12'00'                                                         
                                                                                
*        FROZEN OPTIONS                * BIT IN PCLTSTAT PLUS MORE *            
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Frozen Options'                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCLTSTAT-PCLTREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PCLTSTAT)     FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(FRZOPTE)    ANALYZE PCLTSTAT AND "FREEZE STAT ELEM"          
         DC    XL12'00'                                                         
                                                                                
*        RLP GROUP                * CARRIED IN ELEMENT *                        
                                                                                
         DC    AL1(DD_LDISQ)       DISPLACEMENT INTO SINGLE ELEMENT             
         DC    CL(L'DD_NAME)'RLP Group'                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    X'46'               ELEMENT CODE                                 
         DC    AL1(PCLTTAGRP-PCLTTAEL)  DISPLACEMENT TO ELEM DATA               
         DC    AL1(L'PCLTTAGRP)    FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        COST 2                        * BIT IN PCLTSTAT PLUS MORE *            
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Cost 2'                                            
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PCLTSTAT-PCLTREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PCLTSTAT)     FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(COS2E)      ANALYZE PCLTSTAT AND "COST2 FACTOR ELEM"         
         DC    XL12'00'                                                         
                                                                                
*        SAP interface code          * CARRIED IN ELEMENT *                     
                                                                                
         DC    AL1(DD_LDISQ)       DISPLACEMENT INTO SINGLE ELEMENT             
         DC    CL(L'DD_NAME)'SAP interface code'                                
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    X'51'               ELEMENT CODE                                 
         DC    AL1(PSAPCODE-PSAPEL)    DISPLACEMENT TO ELEM DATA                
         DC    AL1(L'PSAPCODE)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
CLTFLDX  DC    AL1(DD_EOTQ)                                                     
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - PRODUCT'             
***********************************************************************         
*                                                                     *         
*        PRODUCT KEY AND RECORD DEFINITIONS                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRDKEY   DS    0X                  ** PRODUCT KEY DEFINITIONS **                
                                                                                
*        AGENCY                                                                 
****                                                                            
****     DC    AL1(DK_TCHRQ+DK_TSAVQ)  CHAR & SAVE IN WORKSTORE                 
****     DC    AL1(PPRDKAGY-PPRDKEY,L'PPRDKAGY),AL4(0) DISP/LEN/RTN             
****     DC    AL2(RAGY-WORKD)     SAVE AREA DISPLACEMENT                       
****     DC    XL8'00'                                                          
                                                                                
*        MEDIA                                                                  
                                                                                
         DC    AL1(DK_TCHRQ+DK_TSAVQ)  CHAR & SAVE IN WORKSTORE                 
         DC    AL1(PPRDKMED-PPRDKEY,L'PPRDKMED),AL4(0) DISP/LEN/RTN             
         DC    AL2(RMED-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
*        CLIENT                                                                 
                                                                                
         DC    AL1(DK_TCHRQ+DK_TSAVQ)  CHAR & SAVE IN WORKSTORE                 
         DC    AL1(PPRDKCLT-PPRDKEY,L'PPRDKCLT),AL4(0) DISP/LEN/RTN             
         DC    AL2(RCLT-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
*        PRODUCT                                                                
                                                                                
         DC    AL1(DK_TCHRQ+DK_TSAVQ)  CHAR & SAVE IN WORKSTORE                 
         DC    AL1(PPRDKPRD-PPRDKEY,L'PPRDKPRD),AL4(0) DISP/LEN/RTN             
         DC    AL2(RPRD-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
PRDKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
PRDFLD   DS    0X                  ** PRODUCT FIELD DEFINITIONS **              
                                                                                
*        PRODUCT NAME                                                           
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Product Name'  FIELD NAME                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PPRDNAME-PPRDREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PPRDNAME)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        BILL RECEIPT NAME                                                      
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Bill Receipt Name 1'  FIELD NAME                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PPRDBILL-PPRDREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PPRDBILL)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        BILL RECEIPT NAME - LINE 2                                             
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Bill Receipt Name 2'  FIELD NAME                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PPRDBIL2-PPRDREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PPRDBIL2)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        ADDRESS LINE 1                                                         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Address (Line 1)'  FIELD NAME                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PPRDLIN1-PPRDREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PPRDLIN1)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        ADDRESS LINE 2                                                         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Address (Line 2)'  FIELD NAME                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PPRDLIN2-PPRDREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PPRDLIN2)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        ATTENTION                                                              
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Attention of'  FIELD NAME                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PPRDATTN-PPRDREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PPRDATTN)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        ACCOUNT NUMBER                                                         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Account Number'   FIELD NAME                       
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PPRDACCT-PPRDREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PPRDACCT)     FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDACNO)         FORMAT ACCOUNT NUMBER                        
         DC    XL12'00'                                                         
                                                                                
*        DIVISION NUMBER                                                        
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Division Number'  FIELD NAME                       
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PPRDDIV-PPRDREC)  DISPLACEMENT OF FIELD IN REC               
         DC    AL1(L'PPRDDIV)      FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        OTHER AGENCY NAME                                                      
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Other Agency Name'  FIELD NAME                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PPRDOAN-PPRDREC)  DISPLACEMENT OF FIELD IN REC               
         DC    AL1(L'PPRDOAN)      FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        CANADIAN PST TAX CODE     * CARRIED IN ELEMENT *                       
                                                                                
         DC    AL1(DD_LDISQ)       DISPLACEMENT INTO SINGLE ELEMENT             
         DC    CL(L'DD_NAME)'PST'  FIELD NAME                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    X'25'               ELEMENT CODE                                 
         DC    AL1(PPRDPSTC-PPRDPST)  DISPLACEMENT TO ELEM DATA                 
         DC    AL1(L'PPRDPSTC)     FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDTPST)         FORMAT CANADIAN PROVINCES                    
         DC    XL12'00'                                                         
                                                                                
*        ADJACENCY CODES                                                        
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Adjacency Codes'  FIELD NAME                       
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PPRDEXCL-PPRDREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PPRDEXCL)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        EXCLUSION CLASS           DISPLACEMENT INTO RECORD                     
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Exclusion Class'  FIELD NAME                       
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PPRDEXCL-PPRDREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PPRDEXCL)     FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDEXCL)         FORMAT EXCLUSION CLASS                       
         DC    XL12'00'                                                         
                                                                                
*        BILL ON PLANNED COST EFFECTIVE DATE    * CARRIED IN ELEMENT *          
                                                                                
         DC    AL1(DD_LDISQ)       DISPLACEMENT INTO SINGLE ELEMENT             
         DC    CL(L'DD_NAME)'Bill P. Cost Eff:'   FIELD NAME                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    X'45'               ELEMENT CODE                                 
         DC    AL1(PPBPCEFF-PPRDBPCE)  DISPLACEMENT TO ELEM DATA                
         DC    AL1(L'PPBPCEFF)     FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDTEFD)         FORMAT MMM/YY DATE                           
         DC    XL12'00'                                                         
                                                                                
*        TRAFFIC?                  ** BIT SETTING IN PCLTSTAT **                
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Traffic?'                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PPRDSTAT-PPRDREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PPRDSTAT)     FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDPRTRF)        ANALYZE PPRDSTAT FOR TRAFFIC                 
         DC    XL12'00'                                                         
                                                                                
*        CANADIAN GST TAX CODE                                                  
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'GST'                                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PPRDGST-PPRDREC)  DISPLACEMENT OF FIELD IN REC               
         DC    AL1(L'PPRDGST)      FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        LEGAL WARNING ROTATION    * CARRIED IN ELEMENT *                       
                                                                                
         DC    AL1(DD_LDISQ)       DISPLACEMENT INTO SINGLE ELEMENT             
         DC    CL(L'DD_NAME)'LW Rotation'   FIELD NAME                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    X'40'               ELEMENT CODE                                 
         DC    AL1(PPRDROTA-PPRDLWEL)  DISPLACEMENT TO ELEM DATA                
         DC    AL1(L'PPRDROTA)     FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDTLWR)         FORMAT LEGAL WARNING ROTATION                
         DC    XL12'00'                                                         
                                                                                
*        ACC OFFICE CODE AND ACC OFFICE AGENCY   * CARRIED IN ELEMENT *         
                                                                                
         DC    AL1(DD_LDISQ)       DISPLACEMENT INTO SINGLE ELEMENT             
         DC    CL(L'DD_NAME)'Acc Office Code'   FIELD NAME                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    X'35'               ELEMENT CODE                                 
         DC    AL1(PPRDAOFC-PPRDAOEL)  DISPLACEMENT TO ELEM DATA                
         DC    AL1(L'PPRDAOFC)     FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDTAOF)         FORMAT ACC OFFICE CODE AND AGENCY            
         DC    XL12'00'                                                         
                                                                                
*        INTERFACE CODE            * CARRIED IN ELEMENT *                       
                                                                                
         DC    AL1(DD_LDISQ)       DISPLACEMENT INTO SINGLE ELEMENT             
         DC    CL(L'DD_NAME)'Interface Code'   FIELD NAME                       
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    X'30'               ELEMENT CODE                                 
         DC    AL1(PPRDINFC-PPRDICEL)  DISPLACEMENT TO ELEM DATA                
         DC    AL1(L'PPRDINFC)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        PRODUCT OFFICE CODE                                                    
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Office'    FIELD NAME                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PPRDOFFC-PPRDREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PPRDOFFC)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        PRODUCT TRAFFIC CODE                                                   
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Traffic'   FIELD NAME                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PPRDTRAF-PPRDREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PPRDTRAF)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        BILL BASIS                                                             
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Bill Basis'   FIELD NAME                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILBASA-BILPROF+PPRDBILP-PPRDREC)  DISP IN REC               
         DC    AL1(L'BILBASA)      FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDTBBAS)        FORMAT BILL BASIS                            
         DC    XL12'00'                                                         
                                                                                
*        PERCENT ADJUSTMENT                                                     
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Pct. Adj.'   FIELD NAME                            
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILADJ-BILPROF+PPRDBILP-PPRDREC)  DISP IN REC                
         DC    AL1(L'BILADJ)       FIELD LENGTH                                 
         DC    AL1(DD_TSHRQ)       SYSTEM HANDLER PROCESSED                     
         DC    AL4(EDTCOMP)        FORMAT PERCENTAGE ADJUSTMENT                 
         DC    XL12'00'                                                         
                                                                                
*        COMMISSION BASIS                                                       
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Commission Basis'   FIELD NAME                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILBASB-BILPROF+PPRDBILP-PPRDREC)  DISP IN REC               
         DC    AL1(L'BILBASB)      FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDTBBAS)        FORMAT BILL BASIS                            
         DC    XL12'00'                                                         
                                                                                
*        IF AC, DISPLAY AS                                                      
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'If AC, display as'   FIELD NAME                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILPADJ-BILPROF+PPRDBILP-PPRDREC)  DISP IN REC               
         DC    AL1(L'BILPADJ)      FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDPRCT)         FORMAT PERCENTAGE ADJUSTMENT                 
         DC    XL12'00'                                                         
                                                                                
*        "PCT. OF" COMMISSION BASIS                                             
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'pct. of'   FIELD NAME                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILPBASB-BILPROF+PPRDBILP-PPRDREC)  DISP IN REC              
         DC    AL1(L'BILPBASB)     FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDTBBAS)        FORMAT BILL BASIS                            
         DC    XL12'00'                                                         
                                                                                
*        EFFECTIVE DATE                                                         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Eff Date'   FIELD NAME                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILADAT-BILPROF+PPRDBILP-PPRDREC)  DISP IN REC               
         DC    AL1(L'BILADAT)      FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDTEFD)         FORMAT MMM/YY DATE                           
         DC    XL12'00'                                                         
                                                                                
*        COLUMNS TO PRINT ON BILL                                               
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Cols to prnt on bill'   FIELD NAME                 
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILDETS-BILPROF+PPRDBILP-PPRDREC)  DISP IN REC               
         DC    AL1(L'BILDETS)      FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        COMMENT CODES (1)                                                      
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Comment Codes (1)'   FIELD NAME                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILCMNTS-BILPROF+PPRDBILP-PPRDREC)  DISP IN REC              
         DC    AL1(L'BILCMNTS)      FIELD LENGTH                                
         DC    AL1(DD_TSHEQ)        EDIT ROUTINE                                
         DC    AL4(EDPCMT)          OUTPUT PRBILL COMMENT CODE                  
         DC    XL12'00'                                                         
                                                                                
*        CONTROLS (1)                                                           
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Controls (1)'   FIELD NAME                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILCMNTS-BILPROF+PPRDBILP-PPRDREC)  DISP IN REC              
         DC    AL1(L'BILCMNTS)      FIELD LENGTH                                
         DC    AL1(DD_TSHEQ)        EDIT ROUTINE                                
         DC    AL4(EDPCON)          FORMAT PRBILL CONTROLS                      
         DC    XL12'00'                                                         
                                                                                
*        BILL TYPES (1)                                                         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Bill Types (1)'   FIELD NAME                       
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILCMNTS-BILPROF+PPRDBILP-PPRDREC)  DISP IN REC              
         DC    AL1(L'BILCMNTS)      FIELD LENGTH                                
         DC    AL1(DD_TSHEQ)        EDIT ROUTINE                                
         DC    AL4(EDPTYP)          FORMAT PRBILL BILL TYPES                    
         DC    XL12'00'                                                         
                                                                                
*        COMMENT CODES (2)                                                      
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Comment Codes (2)'   FIELD NAME                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILCMNTS-BILPROF+PPRDBILP-PPRDREC+7)  DISP IN REC            
         DC    AL1(L'BILCMNTS)      FIELD LENGTH                                
         DC    AL1(DD_TSHEQ)        EDIT ROUTINE                                
         DC    AL4(EDPCMT)          OUTPUT PRBILL COMMENT CODE                  
         DC    XL12'00'                                                         
                                                                                
*        CONTROLS (2)                                                           
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Controls (2)'   FIELD NAME                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILCMNTS-BILPROF+PPRDBILP-PPRDREC+7)  DISP IN REC            
         DC    AL1(L'BILCMNTS)      FIELD LENGTH                                
         DC    AL1(DD_TSHEQ)        EDIT ROUTINE                                
         DC    AL4(EDPCON)          FORMAT PRBILL CONTROLS                      
         DC    XL12'00'                                                         
                                                                                
*        BILL TYPES (2)                                                         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Bill Types (2)'   FIELD NAME                       
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILCMNTS-BILPROF+PPRDBILP-PPRDREC+7)  DISP IN REC            
         DC    AL1(L'BILCMNTS)      FIELD LENGTH                                
         DC    AL1(DD_TSHEQ)        EDIT ROUTINE                                
         DC    AL4(EDPTYP)          FORMAT PRBILL BILL TYPES                    
         DC    XL12'00'                                                         
                                                                                
*        COMMENT CODES (3)                                                      
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Comment Codes (3)'   FIELD NAME                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILCMNTS-BILPROF+PPRDBILP-PPRDREC+14)  DISP IN REC           
         DC    AL1(L'BILCMNTS)      FIELD LENGTH                                
         DC    AL1(DD_TSHEQ)        EDIT ROUTINE                                
         DC    AL4(EDPCMT)          OUTPUT PRBILL COMMENT CODE                  
         DC    XL12'00'                                                         
                                                                                
*        CONTROLS (3)                                                           
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Controls (3)'   FIELD NAME                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILCMNTS-BILPROF+PPRDBILP-PPRDREC+14)  DISP IN REC           
         DC    AL1(L'BILCMNTS)      FIELD LENGTH                                
         DC    AL1(DD_TSHEQ)        EDIT ROUTINE                                
         DC    AL4(EDPCON)          FORMAT PRBILL CONTROLS                      
         DC    XL12'00'                                                         
                                                                                
*        BILL TYPES (3)                                                         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Bill Types (3)'   FIELD NAME                       
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILCMNTS-BILPROF+PPRDBILP-PPRDREC+14)  DISP IN REC           
         DC    AL1(L'BILCMNTS)      FIELD LENGTH                                
         DC    AL1(DD_TSHEQ)        EDIT ROUTINE                                
         DC    AL4(EDPTYP)          FORMAT PRBILL BILL TYPES                    
         DC    XL12'00'                                                         
                                                                                
PRDFLDX  DC    AL1(DD_EOTQ)                                                     
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - ESTIMATE'            
***********************************************************************         
*                                                                     *         
*        ESTIMATE KEY AND RECORD DEFINITIONS                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ESTKEY   DS    0X                  ** ESTIMATE KEY DEFINITIONS **               
                                                                                
*        AGENCY                                                                 
****                                                                            
****     DC    AL1(DK_TCHRQ+DK_TSAVQ)  CHAR & SAVE IN WORKSTORE                 
****     DC    AL1(PESTKAGY-PESTKEY,L'PESTKAGY),AL4(0) DISP/LEN/RTN             
****     DC    AL2(RAGY-WORKD)     SAVE AREA DISPLACEMENT                       
****     DC    XL8'00'                                                          
                                                                                
*        MEDIA                                                                  
                                                                                
         DC    AL1(DK_TCHRQ+DK_TSAVQ)  CHAR & SAVE IN WORKSTORE                 
         DC    AL1(PESTKMED-PESTKEY,L'PESTKMED),AL4(0) DISP/LEN/RTN             
         DC    AL2(RMED-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
*        CLIENT                                                                 
                                                                                
         DC    AL1(DK_TCHRQ+DK_TSAVQ)  CHAR & SAVE IN WORKSTORE                 
         DC    AL1(PESTKCLT-PESTKEY,L'PESTKCLT),AL4(0) DISP/LEN/RTN             
         DC    AL2(RCLT-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
*        PRODUCT                                                                
                                                                                
         DC    AL1(DK_TCHRQ+DK_TSAVQ)  CHAR & SAVE IN WORKSTORE                 
         DC    AL1(PESTKPRD-PESTKEY,L'PESTKPRD),AL4(0) DISP/LEN/RTN             
         DC    AL2(RPRD-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
*        ESTIMATE                                                               
                                                                                
         DC    AL1(DK_TBINQ+DK_TSAVQ)  BINARY & SAVE IN WORKSTORE               
         DC    AL1(PESTKEST-PESTKEY,L'PESTKEST),AL4(0) DISP/LEN/RTN             
         DC    AL2(REST-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
ESTKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
ESTFLD   DS    0X                  ** ESTIMATE FIELD DEFINITIONS **             
                                                                                
*        ESTIMATE NAME LINE 1                                                   
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Estimate Name Line 1'   FIELD NAME                 
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PESTNAME-PESTREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PESTNAME)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        BILLING REP                                                            
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Billing Rep'  FIELD NAME                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PESTBREP-PESTREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PESTBREP)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        ESTIMATE NAME LINE 2                                                   
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Estimate Name Line 2'   FIELD NAME                 
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PESTNAM2-PESTREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PESTNAM2)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        ESTIMATE START DATE                                                    
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Estimate Start Date'   FIELD NAME                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PESTST-PESTREC)  DISPLACEMENT OF FIELD IN REC                
         DC    AL1(L'PESTST)       FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDCHDT)         FORMAT YYMMDD TO MMMDD/YY                    
         DC    XL12'00'                                                         
                                                                                
*        ESTIMATE END DATE                                                      
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Estimate End Date'   FIELD NAME                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PESTEND-PESTREC)  DISPLACEMENT OF FIELD IN REC               
         DC    AL1(L'PESTEND)      FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDCHDT)         FORMAT YYMMDD TO MMMDD/YY                    
         DC    XL12'00'                                                         
                                                                                
*        FILTERS                                                                
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Filters'  FIELD NAME                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PESTGRPS-PESTREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PESTGRPS)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        RETAIL SCHEME                                                          
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Retail Scheme'  FIELD NAME                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PESTRSCH-PESTREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PESTRSCH)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        RATE TYPE                                                              
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Rate Type'  FIELD NAME                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PESTRTYP-PESTREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PESTRTYP)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        AD NUMBER                                                              
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Ad Number'  FIELD NAME                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PESTJOB-PESTREC)  DISPLACEMENT OF FIELD IN REC               
         DC    AL1(L'PESTJOB)      FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        STANDARD COMMENT(S)                                                    
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Standard Comments'  FIELD NAME                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PESTCOM-PESTREC)  DISPLACEMENT OF FIELD IN REC               
         DC    AL1(L'PESTCOM)      FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(FESCOM)         FORMAT "POSSIBLE" TWO COMMENT OUTPUT         
         DC    XL12'00'                                                         
                                                                                
*        REVISION NUMBER                                                        
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Revision Number'  FIELD NAME                       
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PESTREVN-PESTREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PESTREVN)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        SPECIAL REP                                                            
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Special Rep'  FIELD NAME                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PESTREP-PESTREC)  DISPLACEMENT OF FIELD IN REC               
         DC    AL1(L'PESTREP)      FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        STATUS                                                                 
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Status'   FIELD NAME                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PESTSTAT-PESTREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PESTSTAT)     FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(FESTAT)         FORMAT STATUS                                
         DC    XL12'00'                                                         
                                                                                
*        ALLOCATIONS                                                            
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Allocations'   FIELD NAME                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PESTZZZ-PESTREC)  DISPLACEMENT OF FIELD IN REC               
         DC    AL1(L'PESTZZZ)      FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        2nd allocations                                                        
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'2nd Allocations'   FIELD NAME                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PESTZZZ-PESTREC)  DISPLACEMENT OF FIELD IN REC               
         DC    AL1(L'PESTZZZ)      FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        USER DEFINED DESCRIPTION 1      * CARRIED IN ELEMENT *                 
                                                                                
         DC    AL1(DD_LDISQ)       DISPLACEMENT INTO SINGLE ELEMENT             
         DC    CL(L'DD_NAME)'USER DESCRIPTION 1'   FIELD NAME                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    X'08'               ELEMENT CODE                                 
         DC    AL1(PEUSER1-PESTUDEF)  DISPLACEMENT TO ELEM DATA                 
         DC    AL1(L'PEUSER1)      FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        USER DEFINED DESCRIPTION 2      * CARRIED IN ELEMENT *                 
                                                                                
         DC    AL1(DD_LDISQ)       DISPLACEMENT INTO SINGLE ELEMENT             
         DC    CL(L'DD_NAME)'USER DESCRIPTION 2'   FIELD NAME                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    X'08'               ELEMENT CODE                                 
         DC    AL1(PEUSER2-PESTUDEF)  DISPLACEMENT TO ELEM DATA                 
         DC    AL1(L'PEUSER2)      FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        BILL ON PLANNED COST EFFECTIVE DATE    * CARRIED IN ELEMENT *          
                                                                                
         DC    AL1(DD_LDISQ)       DISPLACEMENT INTO SINGLE ELEMENT             
         DC    CL(L'DD_NAME)'Bill P. Cost Eff:'   FIELD NAME                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    X'45'               ELEMENT CODE                                 
         DC    AL1(PEBPCEFF-PESTBPCE)  DISPLACEMENT TO ELEM DATA                
         DC    AL1(L'PEBPCEFF)     FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDTEFD)         FORMAT MMM/YY DATE                           
         DC    XL12'00'                                                         
                                                                                
*        ESTIMATE ACTUALIZATION DATE            * CARRIED IN ELEMENT *          
                                                                                
         DC    AL1(DD_LDISQ)       DISPLACEMENT INTO SINGLE ELEMENT             
         DC    CL(L'DD_NAME)'Act Bill Thru:'   FIELD NAME                       
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    X'46'               ELEMENT CODE                                 
         DC    AL1(PEACTDAT-PESTACTD)  DISPLACEMENT TO ELEM DATA                
         DC    AL1(L'PEACTDAT)     FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDTEFD)         FORMAT MMM/YY DATE                           
         DC    XL12'00'                                                         
                                                                                
*        BILL DATE CALC OVERRIDE   CARRIED IN EST PROFILE (PESTPROF)            
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Bill Dt Calc Ovrride'                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PESTPROF+01-PESTREC)   DISPL.OF FIELD IN REC                 
         DC    AL1(1)              FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        BILL DATE -> BASE DATE    CARRIED IN EST PROFILE (PESTPROF)            
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Bill Dt -> Base Date'                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PESTPROF+02-PESTREC)   DISPL.OF FIELD IN REC                 
         DC    AL1(1)              FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        BILL DATE -> DATE ADJ.    CARRIED IN EST PROFILE (PESTPROF)            
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Bill Dt -> Date Adj.'                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PESTPROF+03-PESTREC)   DISPL.OF FIELD IN REC                 
         DC    AL1(1)              FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        NON-NEWSPAPER ADJUSTMENT  CARRIED IN EST PROFILE (PESTPROF)            
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Non-newspaper Adjust'                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PESTPROF+04-PESTREC)   DISPL.OF FIELD IN REC                 
         DC    AL1(1)              FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        PURCHASE ORDER $                                                       
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Purchase Order $'  FIELD NAME                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PESTPURO-PESTREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PESTPURO)     FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDPURO)         FORMAT PURCHASE ORDER $                      
         DC    XL12'00'                                                         
                                                                                
*        SPECIAL FINANCIAL HANDLING                                             
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Specl Financl Hndlng'                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PESTTEST-PESTREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PESTTEST)     FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDESFH)         TEST FOR SFH FLAG                            
         DC    XL12'00'                                                         
                                                                                
*        COST 2                                                                 
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Cost 2'                                            
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PESTCF-PESTREC)  DISPLACEMENT OF FIELD IN REC                
         DC    AL1(L'PESTCF)       FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDCOS2)         FORMAT 6-DECIMAL COST 2 FACTOR               
         DC    XL12'00'                                                         
                                                                                
*        BILL BASIS                                                             
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Bill Basis'   FIELD NAME                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILBASA-BILPROF+PESTBILP-PESTREC)  DISP IN REC               
         DC    AL1(L'BILBASA)      FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDTBBAS)        FORMAT BILL BASIS                            
         DC    XL12'00'                                                         
                                                                                
*        PERCENT ADJUSTMENT                                                     
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Pct. Adj.'   FIELD NAME                            
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILADJ-BILPROF+PESTBILP-PESTREC)  DISP IN REC                
         DC    AL1(L'BILADJ)       FIELD LENGTH                                 
         DC    AL1(DD_TSHRQ)       SYSTEM HANDLER PROCESSED                     
         DC    AL4(EDTCOMP)        FORMAT PERCENTAGE ADJUSTMENT                 
         DC    XL12'00'                                                         
                                                                                
*        COMMISSION BASIS                                                       
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Commission Basis'   FIELD NAME                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILBASB-BILPROF+PESTBILP-PESTREC)  DISP IN REC               
         DC    AL1(L'BILBASB)      FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDTBBAS)        FORMAT BILL (COMMISSION) BASIS               
         DC    XL12'00'                                                         
                                                                                
*        IF AC, DISPLAY AS                                                      
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'If AC, display as'   FIELD NAME                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILPADJ-BILPROF+PESTBILP-PESTREC)  DISP IN REC               
         DC    AL1(L'BILPADJ)      FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDPRCT)         FORMAT PERCENTAGE ADJUSTMENT                 
         DC    XL12'00'                                                         
                                                                                
*        "PCT. OF" COMMISSION BASIS                                             
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'pct. of'   FIELD NAME                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILPBASB-BILPROF+PESTBILP-PESTREC)  DISP IN REC              
         DC    AL1(L'BILPBASB)     FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDTBBAS)        FORMAT BILL BASIS                            
         DC    XL12'00'                                                         
                                                                                
*        EFFECTIVE DATE                                                         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Eff Date'   FIELD NAME                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILADAT-BILPROF+PESTBILP-PESTREC)  DISP IN REC               
         DC    AL1(L'BILADAT)      FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDTEFD)         FORMAT MMM/YY DATE                           
         DC    XL12'00'                                                         
                                                                                
*        COLUMNS TO PRINT ON BILL                                               
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Cols to prnt on bill'   FIELD NAME                 
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILDETS-BILPROF+PESTBILP-PESTREC)  DISP IN REC               
         DC    AL1(L'BILDETS)      FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        COMMENT CODES (1)                                                      
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Comment Codes (1)'   FIELD NAME                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILCMNTS-BILPROF+PESTBILP-PESTREC)  DISP IN REC              
         DC    AL1(L'BILCMNTS)      FIELD LENGTH                                
         DC    AL1(DD_TSHEQ)        EDIT ROUTINE                                
         DC    AL4(EDPCMT)          OUTPUT PRBILL COMMENT CODE                  
         DC    XL12'00'                                                         
                                                                                
*        CONTROLS (1)                                                           
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Controls (1)'   FIELD NAME                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILCMNTS-BILPROF+PESTBILP-PESTREC)  DISP IN REC              
         DC    AL1(L'BILCMNTS)      FIELD LENGTH                                
         DC    AL1(DD_TSHEQ)        EDIT ROUTINE                                
         DC    AL4(EDPCON)          FORMAT PRBILL CONTROLS                      
         DC    XL12'00'                                                         
                                                                                
*        BILL TYPES (1)                                                         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Bill Types (1)'   FIELD NAME                       
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILCMNTS-BILPROF+PESTBILP-PESTREC)  DISP IN REC              
         DC    AL1(L'BILCMNTS)      FIELD LENGTH                                
         DC    AL1(DD_TSHEQ)        EDIT ROUTINE                                
         DC    AL4(EDPTYP)          FORMAT PRBILL BILL TYPES                    
         DC    XL12'00'                                                         
                                                                                
*        COMMENT CODES (2)                                                      
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Comment Codes (2)'   FIELD NAME                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILCMNTS-BILPROF+PESTBILP-PESTREC+7)  DISP IN REC            
         DC    AL1(L'BILCMNTS)      FIELD LENGTH                                
         DC    AL1(DD_TSHEQ)        EDIT ROUTINE                                
         DC    AL4(EDPCMT)          OUTPUT PRBILL COMMENT CODE                  
         DC    XL12'00'                                                         
                                                                                
*        CONTROLS (2)                                                           
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Controls (2)'   FIELD NAME                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILCMNTS-BILPROF+PESTBILP-PESTREC+7)  DISP IN REC            
         DC    AL1(L'BILCMNTS)      FIELD LENGTH                                
         DC    AL1(DD_TSHEQ)        EDIT ROUTINE                                
         DC    AL4(EDPCON)          FORMAT PRBILL CONTROLS                      
         DC    XL12'00'                                                         
                                                                                
*        BILL TYPES (2)                                                         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Bill Types (2)'   FIELD NAME                       
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILCMNTS-BILPROF+PESTBILP-PESTREC+7)  DISP IN REC            
         DC    AL1(L'BILCMNTS)      FIELD LENGTH                                
         DC    AL1(DD_TSHEQ)        EDIT ROUTINE                                
         DC    AL4(EDPTYP)          FORMAT PRBILL BILL TYPES                    
         DC    XL12'00'                                                         
                                                                                
*        COMMENT CODES (3)                                                      
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Comment Codes (3)'   FIELD NAME                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILCMNTS-BILPROF+PESTBILP-PESTREC+14)  DISP IN REC           
         DC    AL1(L'BILCMNTS)      FIELD LENGTH                                
         DC    AL1(DD_TSHEQ)        EDIT ROUTINE                                
         DC    AL4(EDPCMT)          OUTPUT PRBILL COMMENT CODE                  
         DC    XL12'00'                                                         
                                                                                
*        CONTROLS (3)                                                           
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Controls (3)'   FIELD NAME                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILCMNTS-BILPROF+PESTBILP-PESTREC+14)  DISP IN REC           
         DC    AL1(L'BILCMNTS)      FIELD LENGTH                                
         DC    AL1(DD_TSHEQ)        EDIT ROUTINE                                
         DC    AL4(EDPCON)          FORMAT PRBILL CONTROLS                      
         DC    XL12'00'                                                         
                                                                                
*        BILL TYPES (3)                                                         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Bill Types (3)'   FIELD NAME                       
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(BILCMNTS-BILPROF+PESTBILP-PESTREC+14)  DISP IN REC           
         DC    AL1(L'BILCMNTS)      FIELD LENGTH                                
         DC    AL1(DD_TSHEQ)        EDIT ROUTINE                                
         DC    AL4(EDPTYP)          FORMAT PRBILL BILL TYPES                    
         DC    XL12'00'                                                         
                                                                                
ESTFLDX  DC    AL1(DD_EOTQ)                                                     
                                                                                
***********************************************************************         
*                                                                     *         
*        REP KEY AND RECORD DEFINITIONS                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
REPKEY   DS    0X                  ** REP KEY DEFINITIONS **                    
                                                                                
*        MEDIA                                                                  
                                                                                
         DC    AL1(DK_TCHRQ+DK_TSAVQ)  CHAR & SAVE IN WORKSTORE                 
         DC    AL1(PREPKMED-PREPKEY,L'PREPKMED),AL4(0) DISP/LEN/RTN             
         DC    AL2(RMED-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
*        REP                                                                    
                                                                                
         DC    AL1(DK_TCHRQ+DK_TSAVQ)  CHAR & SAVE IN WORKSTORE                 
         DC    AL1(PREPKREP-PREPKEY,L'PREPKREP),AL4(0) DISP/LEN/RTN             
         DC    AL2(RREP-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
*        AGENCY                                                                 
                                                                                
         DC    AL1(DK_TCHRQ+DK_TSAVQ)  CHAR & SAVE IN WORKSTORE                 
         DC    AL1(PREPKAGY-PREPKEY,L'PREPKAGY),AL4(0) DISP/LEN/RTN             
         DC    AL2(RAGY-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
REPKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
REPFLD   DS    0X                  ** REP FIELD DEFINITIONS **                  
                                                                                
*        REP NAME                                                               
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Rep Name'     FIELD NAME                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PREPNAME-PREPREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PREPNAME)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        ADDRESS LINE 1                                                         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Address Line 1'  FIELD NAME                        
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PREPLIN1-PREPREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PREPLIN1)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        ADDRESS LINE 2                                                         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Address Line 2'  FIELD NAME                        
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PREPLIN2-PREPREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PREPLIN2)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        ATTENTION                                                              
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Attention'  FIELD NAME                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PREPATTN-PREPREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PREPATTN)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        TELEPHONE                                                              
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Telephone'                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PREPTEL-PREPREC)  DISPLACEMENT OF FIELD IN REC               
         DC    AL1(L'PREPTEL)      FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        FAX NUMBER                                                             
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Fax Number'                                        
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PREPFAX-PREPREC)  DISPLACEMENT OF FIELD IN REC               
         DC    AL1(L'PREPFAX)      FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        FOREIGN BANK CODE                                                      
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Foreign Bank Code'                                 
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PREPSTAC-PREPREC)   DISPLACEMENT OF FIELD IN REC             
         DC    AL1(L'PREPSTAC)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        PUBLISHER (Y/N)    ** BIT SETTING IN PREPSTAT **                       
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'Publisher?'                                        
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PREPSTAT-PREPREC)  DISPLACEMENT OF FIELD IN REC              
         DC    AL1(L'PREPSTAT)     FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(REPPUB)         ANALYZE PREPSTAT FOR PUBLISHER               
         DC    XL12'00'                                                         
                                                                                
                                                                                
REPFLDX  DC    AL1(DD_EOTQ)                                                     
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - AOR RECORD'          
***********************************************************************         
*                                                                     *         
*        AOR KEY AND RECORD DEFINITIONS                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
AORKEYF  DS    0X                  ** AOR REC KEY DEFINITIONS **                
                                                                                
*        AGENCY                                                                 
****                                                                            
****     DC    AL1(DK_TCHRQ+DK_TSAVQ)  CHAR & SAVE IN WORKSTORE                 
****     DC    AL1(AORKAGY-AORKEY,L'AORKAGY),AL4(0) DISP/LEN/RTN                
****     DC    AL2(RAGY-WORKD)     SAVE AREA DISPLACEMENT                       
****     DC    XL8'00'                                                          
                                                                                
*        MEDIA                                                                  
                                                                                
         DC    AL1(DK_TCHRQ+DK_TSAVQ)  CHAR & SAVE IN WORKSTORE                 
         DC    AL1(AORKMED-AORKEY,L'AORKMED),AL4(0) DISP/LEN/RTN                
         DC    AL2(RMED-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
*        CLIENT                                                                 
                                                                                
         DC    AL1(DK_TCHRQ+DK_TSAVQ)  CHAR & SAVE IN WORKSTORE                 
         DC    AL1(AORKCLT-AORKEY,L'AORKCLT),AL4(0) DISP/LEN/RTN                
         DC    AL2(RCLT-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
*        PRODUCT                                                                
                                                                                
         DC    AL1(DK_TCHRQ+DK_TSAVQ)  CHAR & SAVE IN WORKSTORE                 
         DC    AL1(AORKPRD-AORKEY,L'AORKPRD),AL4(0) DISP/LEN/RTN                
         DC    AL2(RPRD-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
*        ESTIMATE                                                               
                                                                                
         DC    AL1(DK_TSHEQ+DK_TSAVQ)  EDIT RTN & SAVE IN WORKSTORE             
         DC    AL1(AORKEST-AORKEY),AL1(3),AL4(EDAOREST) DISP/LEN/RTN            
         DC    AL2(REST-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
AORKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
AORFLD   DS    0X                  ** AOR REC FIELD DEFINITIONS **              
                                                                                
*        AGENCY NAME                                                            
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'AGENCY NAME'   FIELD NAME                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(AORLIN1-AORREC) DISPLACEMENT OF FIELD IN REC                 
         DC    AL1(L'AORLIN1)      FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        ADDRESS (1st)                                                          
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'ADDRESS (1st)'  FIELD NAME                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(AORLIN2-AORREC)  DISPLACEMENT OF FIELD IN REC                
         DC    AL1(L'AORLIN2)      FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        ADDRESS (2nd)                                                          
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'ADDRESS (2nd)'  FIELD NAME                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(AORLIN3-AORREC)  DISPLACEMENT OF FIELD IN REC                
         DC    AL1(L'AORLIN3)      FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        ADDRESS (3rd)                                                          
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'ADDRESS (3rd)'  FIELD NAME                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(AORLIN4-AORREC)  DISPLACEMENT OF FIELD IN REC                
         DC    AL1(L'AORLIN4)      FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        COMMISSION %                                                           
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'COMMISSION %'  FIELD NAME                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(AORPCT-AORREC)  DISPLACEMENT OF FIELD IN REC                 
         DC    AL1(L'AORPCT)       FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDPCT4)         FORMAT 4-DECIMAL OUTPUT                      
         DC    XL12'00'                                                         
                                                                                
*        BASE                                                                   
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'BASE' FIELD NAME                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(AORBAS-AORREC)  DISPLACEMENT OF FIELD IN REC                 
         DC    AL1(L'AORBAS)       FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDABAS)         FORMAT OUTPUT                                
         DC    XL12'00'                                                         
                                                                                
*        EFFECTIVE DATE                                                         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'EFFECTIVE DATE'   FIELD NAME                       
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(AOREFF-AORREC)  DISPLACEMENT OF FIELD IN REC                 
         DC    AL1(L'AOREFF)       FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDTEFD)         FORMAT MMM/YY DATE                           
         DC    XL12'00'                                                         
                                                                                
*        KILL DATE                                                              
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'KILL DATE'   FIELD NAME                            
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(AORKILL-AORREC)  DISPLACEMENT OF FIELD IN REC                
         DC    AL1(L'AORKILL)      FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDTEFD)         FORMAT MMM/YY DATE                           
         DC    XL12'00'                                                         
                                                                                
*        CANADIAN GST TAX CODE                                                  
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'GST CODE'                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(AORGSTCD-AORREC)  DISPLACEMENT OF FIELD IN REC               
         DC    AL1(L'AORGSTCD)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        CANADIAN PST TAX CODE                                                  
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'PST CODE'                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(AORPST-AORREC)  DISPLACEMENT OF FIELD IN REC                 
         DC    AL1(L'AORPST)       FIELD LENGTH                                 
         DC    AL1(DD_TSHEQ)       EDIT ROUTINE                                 
         DC    AL4(EDTPST)         FORMAT CANADIAN PROVINCES                    
         DC    XL12'00'                                                         
                                                                                
*        PAYABLE/RCVBL ACCT                                                     
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'PAYABLE/RCVBL ACCT'                                
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(AORRCVBL-AORREC)  DISPLACEMENT OF FIELD IN REC               
         DC    AL1(L'AORRCVBL)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        INCOME ACCOUNT                                                         
                                                                                
         DC    AL1(DD_LDIRQ)       DISPLACEMENT INTO RECORD                     
         DC    CL(L'DD_NAME)'INCOME ACCOUNT'                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(AORCOMM-AORREC)  DISPLACEMENT OF FIELD IN REC                
         DC    AL1(L'AORCOMM)      FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
*        BILLING STANDARD COMMENT    * CARRIED IN ELEMENT *                     
                                                                                
         DC    AL1(DD_LDISQ)       DISPLACEMENT INTO SINGLE ELEMENT             
         DC    CL(L'DD_NAME)'BILLING STD COM'                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    X'66'               ELEMENT CODE                                 
         DC    AL1(AORCOM1-AORCOMEL)  DISPLACEMENT TO ELEM DATA                 
         DC    AL1(L'AORCOM1)      FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
AORFLDX  DC    AL1(DD_EOTQ)                                                     
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - BFORM REC'           
***********************************************************************         
*                                                                     *         
*        BFORM - BILL FORMULA KEY AND RECORD DEFINITIONS              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PBFKEYF  DS    0X                  ** BFORM RECORD KEY DEFINITIONS **           
                                                                                
*        AGENCY                                                                 
****                                                                            
****     DC    AL1(DK_TCHRQ+DK_TSAVQ)  CHAR & SAVE IN WORKSTORE                 
****     DC    AL1(PBFKAGY-PBFKEY,L'PBFKAGY),AL4(0) DISP/LEN/RTN                
****     DC    AL2(RAGY-WORKD)     SAVE AREA DISPLACEMENT                       
****     DC    XL8'00'                                                          
                                                                                
*        MEDIA                                                                  
                                                                                
         DC    AL1(DK_TCHRQ+DK_TSAVQ)  CHAR & SAVE IN WORKSTORE                 
         DC    AL1(PBFKMED-PBFKEY,L'PBFKMED),AL4(0) DISP/LEN/RTN                
         DC    AL2(RMED-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
*        CLIENT                                                                 
                                                                                
         DC    AL1(DK_TCHRQ+DK_TSAVQ)  CHAR & SAVE IN WORKSTORE                 
         DC    AL1(PBFKCLT-PBFKEY,L'PBFKCLT),AL4(0) DISP/LEN/RTN                
         DC    AL2(RCLT-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
*        PRODUCT                                                                
                                                                                
         DC    AL1(DK_TSHEQ+DK_TSAVQ)  EDIT RTN & SAVE IN WORKSTORE             
         DC    AL1(PBFKPRD-PBFKEY,L'PBFKPRD),AL4(EDPBFPRD) DISP/LEN/RTN         
         DC    AL2(RPRD-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
*        ESTIMATE                                                               
                                                                                
         DC    AL1(DK_TSHEQ+DK_TSAVQ)  EDIT RTN & SAVE IN WORKSTORE             
         DC    AL1(PBFKEST-PBFKEY,L'PBFKEST),AL4(EDPBFEST) DISP/LEN/RTN         
         DC    AL2(REST-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
PBFKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
PBFFLD   DS    0X                  ** BFORM REC FIELD DEFINITIONS **            
                                                                                
         DC    AL1(DD_LDIMQ)                                                    
         DC    CL(L'DD_NAME)'Start MOS'                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PBFRCELQ)                                                    
         DC    AL1(PBFRCDTE-PBFRCELD)                                           
         DC    AL1(L'PBFRCDTE)                                                  
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(0)                                                           
         DC    AL4(BFMEL10K)                                                    
         DC    AL4(BFMEL10D)                                                    
         DC    XL4'00'                                                          
                                                                                
BFMFLDX  DC    AL1(DD_EOTQ)                                                     
                                                                                
BFMEL10K L     RF,DI_AREL          ** BFORM 10 ELEMENT KEY **                   
*                                                                               
         MVC   WORK(L'PBFRCDTE),PBFRCDTE-PBFRCELD(RF)                           
*                                                                               
         MVI   DI_EKEY,C'*'                                                     
         CLC   WORK(2),HIVALS      ELEM KEY=X'FFFF'  ?                          
         BER   RE                  YES - OUTPUT "*" ?                           
*                                                                               
         XC    WORK(2),HIVALS      IT WAS COMPLEMENTED                          
         MVI   WORK+2,X'01'                                                     
         LR    R0,RE                                                            
         GOTOR CDATCON,DMCB,(3,WORK),(9,DI_EKEY)                                
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
BFMEL10D DS    0X                  ** BFORM 10 ELEMENT DATA **                  
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Start MOS'                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PBFRCDTE-PBFRCELD)                                           
         DC    AL1(L'PBFRCDTE)                                                  
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDPBFDT)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Formula'                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PBFRCFML-PBFRCELD)                                           
         DC    AL1(L'PBFRCFML)                                                  
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDPBFF)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Show AC as %'                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PBFRCACP-PBFRCELD)                                           
         DC    AL1(L'PBFRCACP)                                                  
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDPBFAC)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Show AC as % Of'                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PBFRCACO-PBFRCELD)                                           
         DC    AL1(L'PBFRCACO)                                                  
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDPBFOF)                                                     
         DC    XL12'00'                                                         
                                                                                
PBFFLDX  DC    AL1(DD_EOTQ)                                                     
                                                                                
         SPACE 2                                                                
***********************************************************************         
*                                                                     *         
*        PUBREC- PUBREC KEY AND RECORD DEFITIONS                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PUBKEYF  DS    0X                  ** PUB RECORD KEY DEFINITIONS **             
                                                                                
*        MEDIA                                                                  
         DC    AL1(DK_TCHRQ+DK_TSAVQ)  CHAR & SAVE IN WORKSTORE                 
         DC    AL1(PUBKMED-PUBKEY,L'PUBKMED),AL4(0) DISP/LEN/RTN                
         DC    AL2(RMED-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
*        PUB                                                                    
         DC    AL1(DK_TSHEQ+DK_TSAVQ)                                           
         DC    AL1(PUBKPUB-PUBKEY,L'PUBKPUB),AL4(EDTPUB) DISP/LEN/RTN           
         DC    AL2(RPUB-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
*        AGENCY                                                                 
         DC    AL1(DK_TCHRQ+DK_TSAVQ)  CHAR & SAVE IN WORKSTORE                 
         DC    AL1(PUBKAGY-PUBKEY,L'PUBKAGY),AL4(0) DISP/LEN/RTN                
         DC    AL2(RAGY-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
PUBKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
PUBFLD   DS    0X                  ** PUBREC FIELD DEFINITIONS **               
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Pub Name'                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PUBNAMQ)                                                     
         DC    AL1(PUBNAME-PUBNAMEL)                                            
         DC    AL1(L'PUBNAME)                                                   
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Pub Zone Name'                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PUBNAMQ)                                                     
         DC    AL1(PUBZNAME-PUBNAMEL)                                           
         DC    AL1(L'PUBZNAME)                                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Pub Address Line 1'                                
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PUBNAMQ)                                                     
         DC    AL1(PUBLINE1-PUBNAMEL)                                           
         DC    AL1(L'PUBLINE1)                                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Pub Address Line 2'                                
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PUBNAMQ)                                                     
         DC    AL1(PUBLINE2-PUBNAMEL)                                           
         DC    AL1(L'PUBLINE2)                                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Pub City'                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PUBNAMQ)                                                     
         DC    AL1(PUBCITY-PUBNAMEL)                                            
         DC    AL1(L'PUBCITY)                                                   
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Pub State'                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PUBNAMQ)                                                     
         DC    AL1(PUBSTATE-PUBNAMEL)                                           
         DC    AL1(L'PUBSTATE)                                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Pub Zip'                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PUBNAMQ)                                                     
         DC    AL1(PUBZIP-PUBNAMEL)                                             
         DC    AL1(L'PUBZIP)                                                    
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Pub Lock'                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PUBNAMQ)                                                     
         DC    AL1(PUBLOCSW-PUBNAMEL)                                           
         DC    AL1(L'PUBLOCSW)                                                  
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDPLCK)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Pub Paying Rep'                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PUBREPQ)                                                     
         DC    AL1(PUBPAREP-PUBREPEL)                                           
         DC    AL1(L'PUBPAREP)                                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)       DISPLACEMENT INTO SINGLE ELEMENT             
         DC    CL(L'DD_NAME)'SAP interface code'                                
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PUBSAPQ)        ELEMENT CODE                                 
         DC    AL1(PUBSAPCD-PUBSAPEL)  DISPLACEMENT TO ELEM DATA                
         DC    AL1(L'PUBSAPCD)     FIELD LENGTH                                 
         DC    AL1(DD_TCHRQ)       CHARACTER FIELD                              
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
PUBFLDX  DC    AL1(DD_EOTQ)                                                     
*                                                                               
* Following multi-element structure is decomissioned                            
*                                                                               
PUBEL10K L     RF,DI_AREL          ** PUBREC 10 ELEMENT KEY **                  
         MVI   DI_EKEY,C'*'                                                     
         BR    RE                  YES - OUTPUT "*" ?                           
*                                                                               
PUBEL10D DS    0X                  ** PUBREC 10 ELEMENT DATA **                 
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Pub Lock'                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PUBLOCSW-PUBNAMEL)                                           
         DC    AL1(L'PUBLOCSW)                                                  
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDPLCK)                                                      
         DC    XL12'00'                                                         
PUBFFLDX DC    AL1(DD_EOTQ)                                                     
                                                                                
***********************************************************************         
*                                                                     *         
*        PUB ADDRESS RECORD - PAYING ADDRESS                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PPAKEYF  DS    0X                  ** PUB PAYING ADDRESS DEFINITION **          
                                                                                
*        MEDIA                                                                  
         DC    AL1(DK_TCHRQ+DK_TSAVQ)  CHAR & SAVE IN WORKSTORE                 
         DC    AL1(PUBAKMED-PUBAKEY,L'PUBAKMED),AL4(0) DISP/LEN/RTN             
         DC    AL2(RMED-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
*        PUB                                                                    
         DC    AL1(DK_TSHEQ+DK_TSAVQ)                                           
         DC    AL1(PUBAKPUB-PUBAKEY,L'PUBAKPUB),AL4(EDTPUB)                     
         DC    AL2(RPUB-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
*        AGENCY                                                                 
         DC    AL1(DK_TCHRQ+DK_TSAVQ)  CHAR & SAVE IN WORKSTORE                 
         DC    AL1(PUBAKAGY-PUBAKEY,L'PUBAKAGY),AL4(0)                          
         DC    AL2(RAGY-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
*        CLIENT                                                                 
         DC    AL1(DK_TSHEQ+DK_TSAVQ)                                           
         DC    AL1(PUBAKCLT-PUBAKEY,L'PUBAKCLT),AL4(EDTCLT)                     
         DC    AL2(RCLT-WORKD)     SAVE AREA DISPLACEMENT                       
         DC    XL8'00'                                                          
                                                                                
PPAKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
PPAFLD   DS    0X                  ** PUB PAYING ADDRESS FIELDS **              
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Paying Pub Name'                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PUBKPADQ)                                                    
         DC    AL1(PUBAONAM-PUBAOVEL)                                           
         DC    AL1(L'PUBAONAM)                                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Pay Address Line 1'                                
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PUBKPADQ)                                                    
         DC    AL1(PUBAOLN1-PUBAOVEL)                                           
         DC    AL1(L'PUBAOLN1)                                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Pub Address Line 2'                                
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PUBKPADQ)                                                    
         DC    AL1(PUBAOLN2-PUBAOVEL)                                           
         DC    AL1(L'PUBAOLN2)                                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Attention'                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PUBKPADQ)                                                    
         DC    AL1(PUBAOATN-PUBAOVEL)                                           
         DC    AL1(L'PUBAOATN)                                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
PPAFLDX  DC    AL1(DD_EOTQ)                                                     
*                                                                               
         SPACE 2                                                                
***********************************************************************         
*                                                                     *         
*        VARIOUS FIELD HANDLING ROUTINES FOLLOW (FOR ALL RECORDS)     *         
*                                                                     *         
***********************************************************************         
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - PRDASNE'             
***********************************************************************         
*                                                                     *         
*               ANALYZE PCLTSTAT FOR "Prd Assign (Y/N)"               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRDASNE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO INPUT FIELD (PCLTSTAT)              
         L     R3,DI_AOUT          POINT TO OUTPUT FIELD                        
*                                                                               
*****    L     R4,DI_ADSD          SORT HEADER                                  
*****    USING DS_D,R4                                                          
*****    CLI   DS_TYPE,DS_TADDQ    ADD ?                                        
*****    BNE   PRDASN20            NO - ASSUME CHANGE                           
*****    DROP  R4                                                               
*                                                                               
         MVI   0(R3),C'N'          SET OUTPUT TO "N"                            
         TM    0(R2),X'20'         PRD ASSIGN OK ?                              
         BNO   PRDASNX             NO - DONE                                    
         MVI   0(R3),C'Y'          SET TO "Y"                                   
*                                                                               
PRDASNX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - REPPUB'              
***********************************************************************         
*                                                                     *         
*               ANALYZE PREPSTAT FOR "Publisher (Y/N)"                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
REPPUB   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO INPUT FIELD (PREPSTAT)              
         L     R3,DI_AOUT          POINT TO OUTPUT FIELD                        
*                                                                               
*****    L     R4,DI_ADSD          SORT HEADER                                  
*****    USING DS_D,R4                                                          
*****    CLI   DS_TYPE,DS_TADDQ    ADD ?                                        
*****    BNE   REPPUB20            NO - ASSUME CHANGE                           
*****    DROP  R4                                                               
*                                                                               
         MVI   0(R3),C'N'          SET OUTPUT TO "N"                            
         TM    0(R2),X'01'         PUBLISHER?                                   
         BNO   REPPUBX             NO - DONE                                    
         MVI   0(R3),C'Y'          SET TO "Y"                                   
*                                                                               
REPPUBX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDINTFN'             
***********************************************************************         
*                                                                     *         
*        EDIT Interface Number                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDINTFN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO INTERNAL INTERFACE NUMBER           
*                                                                               
         OC    0(L'PCLTNUM,R2),0(R2)                                            
         BZ    EDINTFNX            SKIP IF NO NUMBER PRESENT                    
*                                                                               
         L     RF,DI_ADAD          POINT TO CURRENT AGENCY TABLE ENTRY          
         USING DA_D,RF             ESTABLISH AGENCY TABLE ENTRY                 
         MVC   WORK(2),DA_ALF      AGENCY ID                                    
*                                                                               
         DROP  RF                                                               
*                                                                               
         CLC   WORK(2),=C'JW'      CK FOR AGENCY EXCEPTIONS                     
         BE    EDINT20             EXCEPTION FOUND                              
         CLC   WORK(2),=C'LM'      CK FOR AGENCY EXCEPTIONS                     
         BE    EDINT20             EXCEPTION FOUND                              
         CLC   WORK(2),=C'LT'      CK FOR AGENCY EXCEPTIONS                     
         BE    EDINT20             EXCEPTION FOUND                              
         CLC   WORK(2),=C'DA'      CK FOR AGENCY EXCEPTIONS                     
         BE    EDINT20             EXCEPTION FOUND                              
         CLC   WORK(2),=C'KA'      CK FOR AGENCY EXCEPTIONS                     
         BE    EDINT20             EXCEPTION FOUND                              
*                                                                               
         L     R1,DI_AOUT          POINT TO OUTPUT AREA                         
*                                                                               
         MVC   0(L'PCLTNUM,R1),0(R2)    INPUT UNCHANGED TO OUTPUT               
         B     EDINTFNX                 DONE                                    
*                                                                               
EDINT20  DS    0H                                                               
         CLI   0(R2),X'FF'         TWO PWOS BYTES FOLLOW?                       
         BNE   EDINT40             NO                                           
         UNPK  WORK(5),1(3,R2)                                                  
         MVC   0(4,R1),WORK        NOTE: UNPK'S LAST BYTE IS NOT USED           
         B     EDINTFNX            DONE                                         
*                                                                               
EDINT40  DS    0H                                                               
*                                                                               
* CK FOR SPECIAL 3 BYTE BINARY FORMAT (IF X'80' SET ON IN FIRST BYTE)           
*                                                                               
         TM    0(R2),X'80'         NUMBER IS IN BINARY FORMAT?                  
         BZ    EDINTFNX            NO - DONE                                    
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),0(R2)                                                  
         NI    FULL+1,X'FF'-X'80'  SET OFF X'80' STATUS BIT                     
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         UNPK  WORK(5),DUB                                                      
         OI    WORK+4,X'F0'                                                     
         MVC   0(5,R1),WORK                                                     
*                                                                               
EDINTFNX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDTCLOFC'            
***********************************************************************         
*                                                                     *         
*        EDIT 2 CH OFFICE CODE                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDTCLOFC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO INTERNAL CLIENT OFFICE CODE         
*                                                                               
         OC    0(L'PCLTOFF,R2),0(R2)                                            
         BZ    EDTCLOFX            SKIP IF NO OFFICE CODE PRESENT               
*                                                                               
*        TRANSLATE 2 CHARACTER OFFICE CODE                                      
*                                                                               
         LA    R3,WORK                USE OFFICER                               
         XC    WORK,WORK                                                        
*                                                                               
         USING OFFICED,R3                                                       
*                                                                               
         MVI   OFCSYS,C'P'         SYSTEM                                       
*                                                                               
         L     RF,DI_ADAD          POINT TO CURRENT AGENCY TABLE ENTRY          
         USING DA_D,RF             ESTABLISH AGENCY TABLE ENTRY                 
         MVC   OFCAGY,DA_ALF       AGENCY ID                                    
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVC   OFCOFC,0(R2)        OFFICE CODE                                  
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'2',OFFICED),(0,DI_ACOM)                         
*                                                                               
         L     R1,DI_AOUT          POINT TO OUTPUT AREA                         
         MVC   0(2,R1),OFCOFC2     RETURN 2 CH OFFICE CODE                      
*                                                                               
EDTCLOFX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         LTORG                                                                  
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - GETIC2'              
***********************************************************************         
*                                                                     *         
*    HANDLE INPUT AND OUTPUT FOR POSSIBLE 2ND I/O STANDARD COMMENT    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GETIC2   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    WORK,WORK                                                        
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R3,DI_ADSD          SORT HEADER                                  
         USING DS_D,R3                                                          
*                                                                               
         L     R4,DI_AIO1          POINT R4 TO COPY OR ADD RECORD               
         ZICM  R1,25(R4),2         GET LENGTH OF RECORD                         
         AR    R1,R4               POINT R1 TO END OF RECORD                    
         XC    0(10,R1),0(R1)      CLEAR BYTES AT END                           
*                                                                               
         L     R5,DI_AIO2          POINT R5 TO CHANGE RECORD                    
*                                                                               
*SMY*    CLC   4(3,R4),=C'MC '                                                  
*SMY*    BNE   *+6                                                              
*SMY*    DC    H'0'                TESTINGTESTINGTESTINGTESTING                 
*                                                                               
         CLI   DS_TYPE,DS_TADDQ    ADD ?                                        
         BNE   GIC2CHG             NO - ASSUME CHANGE                           
*                                                                               
         DROP  R3                                                               
*                                  ADD ACTION                                   
         LA    R4,33(R4)                                                        
GICLUP1  DS    0H                  LOOK FOR SECOND I/O STD COM ELEM             
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0             END ?                                        
         BE    GETIC2X             YES - NOTHING FOUND                          
         CLI   0(R4),X'11'         I/O STD COMM ELEM ?                          
         BNE   GICLUP1             NO                                           
GICLUP2  DS    0H                                                               
         ZIC   R0,1(R4)            YES - LOOK FOR ANOTHER                       
         AR    R4,R0                                                            
         CLI   0(R4),0             END ?                                        
         BE    GETIC2X             YES - NO "SECOND" ELEM                       
         CLI   0(R4),X'11'         SECOND I/O STD COMM ELEM ?                   
         BNE   GICLUP2             NO                                           
         MVC   DI_NVAL(L'PCLTINUM),2(R4)                                        
         B     GIC2OUT                                                          
*                                                                               
GIC2CHG  DS    0H                  EVALUATE "BEFORE" REC FOR OUTPUT             
         LA    R4,33(R4)           LOOK FOR 2ND I/O STD COM ELEM                
GICLUP3  DS    0H                                                               
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0             END ?                                        
         BE    GIC2CG2             YES - NOTHING FOUND                          
         CLI   0(R4),X'11'         I/O STD COMM ELEM ?                          
         BNE   GICLUP3             NO                                           
GICLUP4  DS    0H                                                               
         ZIC   R0,1(R4)            YES - LOOK FOR ANOTHER                       
         AR    R4,R0                                                            
         CLI   0(R4),0             END ?                                        
         BE    GIC2CG2             YES - NO "SECOND" ELEM                       
         CLI   0(R4),X'11'         SECOND I/O STD COMM ELEM ?                   
         BNE   GICLUP4             NO                                           
         MVC   WORK(L'PCLTINUM),2(R4)       SAVE "BEFORE" IN WORK               
*                                                                               
GIC2CG2  DS    0H                  EVALUATE "AFTER" REC FOR OUTPUT              
         ZICM  R1,25(R5),2         GET LENGTH OF RECORD                         
         AR    R1,R5               POINT R1 TO END OF RECORD                    
         XC    0(10,R1),0(R1)      CLEAR BYTES AT END                           
*                                                                               
         LA    R5,33(R5)                                                        
GICLUP5  DS    0H                  LOOK FOR 2ND I/O STD COM ELEM                
         ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),0             END ?                                        
         BE    GIC2TST             YES - GO TEST FOR CHANGE                     
         CLI   0(R5),X'11'         I/O STD COMM ELEM ?                          
         BNE   GICLUP5             NO                                           
GICLUP6  DS    0H                                                               
         ZIC   R0,1(R5)            YES - LOOK FOR ANOTHER                       
         AR    R5,R0                                                            
         CLI   0(R5),0             END ?                                        
         BE    GIC2TST             YES - GO TEST FOR CHANGE                     
         CLI   0(R4),X'11'         SECOND I/O STD COMM ELEM ?                   
         BNE   GICLUP6             NO                                           
         MVC   WORK+6(L'PCLTINUM),2(R5)       SAVE "AFTER" IN WORK+6            
GIC2TST  DS    0H                  TEST FOR CHANGES IN FIELD(S)                 
         CLC   WORK(L'PCLTINUM),WORK+6                                          
         BE    GETIC2X             NO CHANGE - DONE                             
         OC    WORK(L'PCLTINUM),WORK          ANY DATA ?                        
         BZ    *+10                           NO                                
         MVC   DI_OVAL(L'PCLTINUM),WORK       "OLD" VALUE                       
         OC    WORK+6(L'PCLTINUM),WORK+6      ANY DATA ?                        
         BZ    *+10                           NO                                
         MVC   DI_NVAL(L'PCLTINUM),WORK+6     "NEW" VALUE                       
*                                                                               
GIC2OUT  DS    0H                  OUTPUT FIELDS                                
         GOTOR DI_APUT                                                          
*                                                                               
GETIC2X  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDTPST'              
***********************************************************************         
*                                                                     *         
*        EDIT PST (CANADIAN PROVINCIAL SALES TAX CODES)               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDTPST   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO INTERNAL PST ELEM DATA              
*                                                                               
         OC    0(L'PCLTPSTC,R2),0(R2)                                           
         BZ    EDTPSTX             SKIP IF NOTHING PRESENT                      
*                                                                               
         LA    R4,WORK                                                          
         USING PSTBLKD,R4                                                       
         XC    WORK,WORK           CLEAR INTERFACE BLOCK                        
         MVI   PSTACT,PSTFMTQ      ACTION = FORMAT                              
         ST    R2,PSTADIN          INPUT ADDRESS                                
         XC    PSTOUT,PSTOUT                                                    
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,DI_ACOM     A(COMFACS)                                   
         XC    DMCB(12),DMCB                                                    
         GOTOR VPSTVAL,DMCB,(R4)   FORMAT PST CODES IN PSTOUT                   
*                                                                               
         DROP  R4                                                               
*                                  DO EXECUTED MOVE OF FORMATTED CODES          
         LA    R2,PSTOUT           FORMATTED PST CODES                          
         LA    R3,0                LENGTH COUNTER                               
         LA    R4,19               MAX LENGTH OF FORMATTED CODES                
EDPSTLUP DS    0H                                                               
         CLI   0(R2),C' '          ANYTHING THERE ?                             
         BNH   EDPSTOUT            NO - END OF COUNT                            
         LA    R3,1(R3)            ADD TO LENGTH COUNT                          
         LA    R2,1(R2)            NEXT BYTE IN PSTOUT                          
         BCT   R4,EDPSTLUP                                                      
EDPSTOUT DS    0H                                                               
         L     R1,DI_AOUT          POINT TO OUTPUT AREA                         
         BCTR  R3,0                PREP FOR EXECUTED MOVE                       
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),PSTOUT      RETURN FORMATTED PST CODES                   
*                                                                               
EDTPSTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - MSTCLTE'             
***********************************************************************         
*                                                                     *         
*               ANALYZE PCLTPROF FOR "If Sub, Master Client"          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MSTCLTE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO INPUT FIELD (PCLTPROF+6)            
         L     R3,DI_AOUT          POINT TO OUTPUT FIELD                        
*                                                                               
         CLC   0(3,R2),=C'000'     MASTER CLIENT ENTERED ?                      
         BNE   MSTCLT20            YES - DONE                                   
         MVC   0(3,R3),=3C' '      CLEAR O/P - NO REAL INPUT                    
         B     MSTCLTX             DONE                                         
*                                                                               
MSTCLT20 DS    0H                                                               
         MVC   0(3,R3),0(R2)       I/P TO O/P                                   
         OC    0(3,R3),=3C' '                                                   
*                                                                               
MSTCLTX  DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - CONREQE'             
***********************************************************************         
*                                                                     *         
*               ANALYZE PCLTPROF+12 "Contracts Required"              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CONREQE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO INPUT FIELD (PCLTPROF+12)           
         L     R3,DI_AOUT          POINT TO OUTPUT FIELD                        
*                                                                               
         CLI   0(R2),C'0'          ANYTHING ENTERED ?                           
         BNE   CONREQX             YES - DONE                                   
         MVI   0(R3),C'Y'          SET AS "Y"                                   
*****    B     CONREQX             DONE                                         
*                                                                               
CONREQX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - SFHE'                
***********************************************************************         
*                                                                     *         
*               ANALYZE PCLTSTAT - "Special Financial Handling"       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SFHE     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO INPUT FIELD (PCLTSTAT)              
         L     R3,DI_AOUT          POINT TO OUTPUT FIELD                        
*                                                                               
         TM    0(R2),X'01'         SPECIAL FINANCIAL HANDLING ?                 
         BNO   SFH20               NO - SEE IF WE SHOW DEFAULT "N"              
         MVI   0(R3),C'Y'          SET AS "Y"                                   
         B     SFHX                DONE                                         
*                                                                               
SFH20    DS    0H       DO NOT DISPLAY DEFAULT VALUE FOR WESTERN AGENCY         
*                                                                               
         L     RF,DI_ADAD          POINT TO CURRENT AGENCY TABLE ENTRY          
         USING DA_D,RF             ESTABLISH AGENCY TABLE ENTRY                 
         MVC   WORK(2),DA_ALF      AGENCY ID                                    
*                                                                               
         DROP  RF                                                               
*                                                                               
         CLC   WORK(2),=C'WI'                                                   
         BE    SFHX                                                             
         CLC   WORK(2),=C'WJ'                                                   
         BE    SFHX                                                             
         CLC   WORK(2),=C'WT'                                                   
         BE    SFHX                                                             
         CLC   WORK(2),=C'WR'                                                   
         BE    SFHX                                                             
         MVI   0(R3),C'N'          DEFAULT SFH                                  
*****    B     SFHX                                                             
*                                                                               
SFHX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - FRZOPTE'             
***********************************************************************         
*                                                                     *         
*    ANALYZE PCLTSTAT AND FREEZE STATUS ELEMENT FOR "Frozen Options"  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FRZOPTE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO INPUT FIELD (PCLTSTAT)              
         L     R3,DI_AOUT          POINT TO OUTPUT FIELD                        
*                                                                               
         TM    0(R2),X'02'         FROZEN?                                      
         BNO   FRZOPTX             NO - DONE                                    
         MVC   0(3,R3),=C'FRZ'     SET OUTPUT                                   
         TM    0(R2),X'10'         FROZEN WITH DATE?                            
         BNO   FRZOPTX             NO - DONE                                    
*                                                                               
         LR    R4,R2               POINT R4 TO PCLTSTAT                         
         SH    R4,=Y(PCLTSTAT-PCLTREC)    SET BACK TO START OF RECORD           
         LA    R4,33(R4)           AND THEN TO FIRST ELEMENT                    
*                                                                               
FRZOPT20 DS    0H                                                               
         CLI   0(R4),0             END OF RECORD ?                              
         BE    FRZOPTX             YES - DONE                                   
         CLI   0(R4),X'47'         FREEZE STATUS ELEM ?                         
         BE    FRZOPT30            YES                                          
         ZIC   R0,1(R4)                                                         
         AR    R4,R0               NEXT ELEMENT                                 
         B     FRZOPT20                                                         
*                                                                               
FRZOPT30 DS    0H                                                               
         USING PCLTFEL,R4                                                       
         MVI   BYTE,C' '                                                        
         MVC   WORK(L'PCLTFDTE),PCLTFDTE                                        
         MVI   WORK+2,X'01'        DAY                                          
         TM    PCLTFIND,X'08'      + INDICATED?                                 
         BZ    *+8                 NO                                           
         MVI   BYTE,C'+'                                                        
         TM    PCLTFIND,X'04'      - INDICATED?                                 
         BZ    *+8                 NO                                           
         MVI   BYTE,C'-'                                                        
*                                                                               
         DROP  R4                                                               
*                                                                               
         L     RF,CDATCON          SET OUTPUT (MMM/YY)                          
         GOTOR (RF),DMCB,(3,WORK),(6,0(R3))                                     
*                                                                               
         CLI   BYTE,C' '           ANYTHING IN BYTE?                            
         BNH   *+10                NO                                           
         MVC   6(1,R3),BYTE        ADD TO OUTPUT                                
*                                                                               
FRZOPTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - COS2E'               
***********************************************************************         
*                                                                     *         
*    ANALYZE PCLTSTAT AND COST2 FACTOR ELEMENT FOR "Cost 2"           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
COS2E    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO INPUT FIELD (PCLTSTAT)              
         L     R3,DI_AOUT          POINT TO OUTPUT FIELD                        
*                                                                               
         LR    R4,R2               POINT R4 TO PCLTSTAT                         
         SH    R4,=Y(PCLTSTAT-PCLTREC)    SET BACK TO START OF RECORD           
         LA    R4,33(R4)           AND THEN TO FIRST ELEMENT                    
*                                                                               
         TM    0(R2),X'04'         COST 2 $?                                    
         BZ    COS220              NO                                           
         MVI   0(R3),C'Y'          OUTPUT "Y"                                   
*                                                                               
COS220   DS    0H                                                               
         CLI   0(R4),0             END OF RECORD ?                              
         BE    COS2X               YES - DONE                                   
         CLI   0(R4),X'45'         COST2 FACTOR ELEM ?                          
         BE    COS230              YES                                          
         ZIC   R0,1(R4)                                                         
         AR    R4,R0               NEXT ELEMENT                                 
         B     COS220                                                           
*                                                                               
COS230   DS    0H                                                               
         TM    0(R2),X'08'         COST 2 FACTOR?                               
         BZ    COS2X               NO - SHOULD NOT HAPPEN - DONE                
*                                                                               
         USING PCLTCFEL,R4                                                      
*                                                                               
         CP    PCLTCF,=P'0'        COST2 FACTOR IS ZERO?                        
         BE    COS2X               YES - SHOULD NOT HAPPEN - DONE               
*                                                                               
         UNPK  WORK(10),PCLTCF                                                  
         OI    WORK+9,X'F0'                                                     
         LA    RE,WORK                                                          
         MVC   0(1,R3),3(RE)       SKIP FIRST 3 DIGITS                          
         LA    R3,1(R3)                                                         
         LA    RE,4(RE)                                                         
         MVI   0(R3),C'.'          DECIMAL POINT                                
         LA    R3,1(R3)                                                         
         LA    RF,6                SIX DIGITS AFTER DECIMAL PT                  
         MVC   0(1,R3),0(RE)                                                    
         LA    R3,1(R3)                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,*-14                                                          
*                                                                               
         LA    RF,8                MAX LOOP IS 8 TIMES                          
COS2K    BCTR  R3,0                                                             
         CLI   0(R3),C'.'          DECIMAL POINT?                               
         BE    COS2M                                                            
*                                                                               
         CLI   0(R3),C'0'          TRAILING ZERO?                               
         BNE   COS2X               NO - DONE                                    
         MVI   0(R3),X'40'         CLEAR TRAILING ZERO                          
         BCT   RF,COS2K                                                         
*                                                                               
         DC    H'0'                SOMETHING IS WRONG...                        
*                                                                               
COS2M    AHI   R3,1                                                             
         MVI   0(R3),C'0'          NON-SIGNIFICANT ZERO                         
*                                                                               
         DROP  R4                                                               
*                                                                               
COS2X    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDACNO'              
***********************************************************************         
*                                                                     *         
*        FORMAT Account Number                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDACNO   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO ACCOUNT NUMBER                      
         L     R3,DI_AOUT          POINT TO OUTPUT FIELD                        
*                                                                               
         OC    0(L'PPRDACCT,R2),0(R2)  SKIP IF NONE                             
         BZ    EDACNOX                                                          
*                                                                               
         CLI   0(R2),X'FF'                                                      
         BNE   EDACN20                                                          
         MVC   FULL,0(R2)                                                       
         MVI   FULL,0                                                           
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(5,R3),DUB+5(3)                                                 
         B     EDACNOX                                                          
EDACN20  DS    0H                                                               
         MVC   0(L'PPRDACCT,R3),0(R2)                                           
*                                                                               
EDACNOX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDEXCL'              
***********************************************************************         
*                                                                     *         
*        FORMAT Exclusion Class                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDEXCL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO EXCLUSION CODE                      
         L     R3,DI_AOUT          POINT TO OUTPUT FIELD                        
*                                                                               
         OC    0(L'PPRDEXCL,R2),0(R2)  SKIP IF NONE                             
         BZ    EDEXCLX                                                          
*                                                                               
         MVC   BYTE,PPRDEXC-PPRDEXCL(R2)  POINT TO AND SAVE PPRDEXC             
*                                           (EXCLUSION CLASS CODE(S))           
         LA    R4,EDEXCLT          POINT TO TRANSLATION TABLE                   
*                                                                               
EDEXCL10 DS    0H                                                               
         CLI   0(R4),0             END OF TABLE?                                
         JE    EDEXCLX             YES - DONE                                   
         MVC   FULL(1),BYTE                                                     
         NC    BYTE,0(R4)          SET BIT TO BE COMPARED                       
         CLC   BYTE,FULL           EXCL CLASS BIT IS ON?                        
         BE    EDEXCL20            NO                                           
         MVC   0(1,R3),1(R4)       TRANSLATED BIT                               
         CLI   BYTE,0              ANY MORE EXCL CLASS BITS ON?                 
         JE    EDEXCLX             NO - DONE                                    
         MVI   1(R3),C','                                                       
         LA    R3,2(R3)            FOR NEXT EXCL CLASS CHAR                     
EDEXCL20 LA    R4,3(R4)            NEXT ENTRY IN TABLE                          
         CLI   BYTE,0              ANY MORE EXCL CLASS BITS ON?                 
         JE    EDEXCLX             NO - DONE                                    
         B     EDEXCL10                                                         
*                                                                               
* EXCLUSION CLASS TABLE: 1ST BYTE = COMPLIMENT OF EXCL CLASS CODE               
*                        2ND BYTE = BIT TO CHAR TRANSLATION                     
*                        3RD BYTE = EXCL CLASS BIT EQUATES                      
*                                                                               
EDEXCLT  DC    AL1(X'FF'-X'80'),C'B',X'80'                B = BEER              
         DC    AL1(X'FF'-X'40'),C'W',X'40'                W = WINE              
         DC    AL1(X'FF'-X'20'),C'L',X'20'                L = LIQUOR            
         DC    AL1(X'FF'-X'10'),C'T',X'10'                T = TOBACCO           
         DC    AL1(X'FF'-X'08'),C'C',X'08'                C = CIG               
         DC    X'00'                                      END OF TABLE          
*                                                                               
*                                                                               
EDEXCLX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDPRTRF'             
***********************************************************************         
*                                                                     *         
*               ANALYZE PPRDSTAT FOR TRAFFIC                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDPRTRF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO INPUT FIELD (PPRDSTAT)              
         L     R3,DI_AOUT          POINT TO OUTPUT FIELD                        
*                                                                               
         MVI   0(R3),C'Y'          SET OUTPUT TO "Y" (DEFAULT)                  
         TM    0(R2),X'20'         NO TRAFFIC ?                                 
         BNO   EDPRTRFX            NO - DONE                                    
         MVI   0(R3),C'N'          SET TO "N"                                   
*                                                                               
EDPRTRFX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDTLWR'              
***********************************************************************         
*                                                                     *         
*               FORMAT LEGAL WARNING ROTATION                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDTLWR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO INPUT FIELD (PPRDROTA)              
         L     R3,DI_AOUT          POINT TO OUTPUT FIELD                        
*                                                                               
         CLC   0(4,R2),=C'ABCD'                                                 
         BNE   *+10                                                             
         MVC   0(11,R3),=C'A1,B2,C3,D4'                                         
         CLC   0(4,R2),=C'BCDA'                                                 
         BNE   *+10                                                             
         MVC   0(11,R3),=C'B1,C2,D3,A4'                                         
         CLC   0(4,R2),=C'CDAB'                                                 
         BNE   *+10                                                             
         MVC   0(11,R3),=C'C1,D2,A3,B4'                                         
         CLC   0(4,R2),=C'DABC'                                                 
         BNE   *+10                                                             
         MVC   0(11,R3),=C'D1,A2,B3,C4'                                         
*                                                                               
EDTLWRX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDTAOF'              
***********************************************************************         
*                                                                     *         
*        FORMAT ACC OFFICE CODE OUTPUT                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDTAOF   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO INPUT FIELD (PPRDAOFC)              
*                                                                               
         CLI   0(R2),C' '          ACC OFFICE CODE PRESENT ?                    
         BNH   EDTAOFX             NO - DONE                                    
*                                                                               
         L     R3,DI_AOUT          POINT TO OUTPUT AREA                         
         MVC   0(L'PPRDAOFC,R3),0(R2)                                           
*                                                                               
         LR    RF,R2                                                            
         SH    RF,=Y(PPRDAOFC-PPRDACCA) POINT TO ACC OFFICE AGENCY              
*                                                                               
         CLI   0(RF),C' '          ACC OFFICE AGENCY PRESENT ?                  
         BNH   EDTAOFX             NO - DONE                                    
         MVI   2(R3),C'/'                                                       
         MVC   3(L'PPRDACCA,R3),0(RF)                                           
*                                                                               
EDTAOFX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDTBBAS'             
***********************************************************************         
*                                                                     *         
*        Edit bill basis                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDTBBAS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO BILL BASIS                          
*                                                                               
         OC    0(L'BILBASA,R2),0(R2)                                            
         BZ    EDTBBASX            SKIP IF NO BASIS                             
*                                                                               
         MVC   WORK(L'BILBASA),0(R2)  COPY BILL BASIS                           
*                                                                               
*        SEARCH TABLE FOR TRANSLATION                                           
*                                                                               
         LA    RF,BBASTAB          START OF TABLE                               
         LHI   R0,BBASTABN         NUMBER OF ENTRIES IN TABLE                   
*                                                                               
EDTBBSLP DS    0H                                                               
*                                                                               
         CLC   0(L'BILBASA,RF),WORK  FIND ENTRY IN TABLE                        
         BE    EDTBBSFD                                                         
*                                                                               
EDTBBSCN DS    0H                                                               
*                                                                               
         AHI   RF,BBASTABL         BUMP TO NEXT ENTRY IN TABLE                  
         BCT   R0,EDTBBSLP                                                      
*                                                                               
         DC    H'0'                SHOULD NOT BE HERE                           
*                                                                               
EDTBBSFD DS    0H                  FOUND MATCH IN TABLE                         
*                                                                               
         L     R1,DI_AOUT          POINT TO OUTPUT AREA                         
*                                  RETURN TRANSLATED BILL BASIS                 
         MVC   0(BBASTABL-L'BILBASA,R1),L'BILBASA(RF)                           
*                                                                               
EDTBBASX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDTCOMP'             
***********************************************************************         
*                                                                     *         
*        Analyse and Output Bill Basis Percentage Adjustment          *         
*           (handles "implied" zeroes on ADDS)                        *         
***********************************************************************         
         SPACE 2                                                                
EDTCOMP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         XC    OBILA(OBILLN+NBILLN),OBILA     CLEAR WORK FIELDS                 
*                                                                               
         L     R3,DI_ADSD          SORT HEADER                                  
         USING DS_D,R3                                                          
*                                                                               
         L     R4,DI_AIO1          COPY OR ADD RECORD                           
         L     R5,DI_AIO2          CHANGE RECORD                                
         CLI   3(R4),X'06'         PRODUCT RECORD ?                             
         BNE   ECOMP40             NO - ASSUME ESTIMATE                         
*                                  PRODUCT RECORD                               
         CLI   DS_TYPE,DS_TADDQ    ADD ?                                        
         BNE   ECOMP10             NO - ASSUME CHANGE                           
         MVC   NBILA(NBILLN),BILBASA-BILPROF+PPRDBILP-PPRDREC(R4) "NEW"         
         B     ECOMTST             EVALUATE FOR OUTPUT                          
ECOMP10  DS    0H                  CHANGE                                       
         MVC   OBILA(OBILLN),BILBASA-BILPROF+PPRDBILP-PPRDREC(R4) "OLD"         
         MVC   NBILA(NBILLN),BILBASA-BILPROF+PPRDBILP-PPRDREC(R5) "NEW"         
         B     ECOMTST             EVALUATE FOR OUTPUT                          
*                                                                               
ECOMP40  DS    0H                  ESTIMATE RECORD                              
         CLI   DS_TYPE,DS_TADDQ    ADD ?                                        
         BNE   ECOMP50             NO - ASSUME CHANGE                           
         MVC   NBILA(NBILLN),BILBASA-BILPROF+PESTBILP-PESTREC(R4) "NEW"         
         B     ECOMTST             EVALUATE FOR OUTPUT                          
ECOMP50  DS    0H                  CHANGE                                       
         MVC   OBILA(OBILLN),BILBASA-BILPROF+PESTBILP-PESTREC(R4) "OLD"         
         MVC   NBILA(NBILLN),BILBASA-BILPROF+PESTBILP-PESTREC(R5) "NEW"         
         B     ECOMTST             EVALUATE FOR OUTPUT                          
*                                                                               
ECOMTST  DS    0H                  EVALUATE FOR OUTPUT                          
         OC    OBILA(OBILLN+NBILLN),OBILA     ANYTHING IN BILBAS DATA?          
         BZ    EDTCOMPX                       NO - EXIT                         
*                                                                               
         CLI   DS_TYPE,DS_TADDQ    ADD ?                                        
         BNE   ECOMT10             NO - ASSUME CHANGE                           
         OC    NBILA(NBILLN),NBILA   ANYTHING IN "NEW" BILBAS DATA?             
         BZ    EDTCOMPX              NO - EXIT                                  
         B     ECNEWOUT                                                         
*                                                                               
         DROP  R3                                                               
*                                                                               
ECOMT10  DS    0H                  EVALUATE "CHANGE REC" FOR OUTPUT             
         OC    OBILA(OBILLN),OBILA  ANYTHING IN "OLD" BILBAS DATA ?             
         BZ    ECNEWOUT             NO                                          
*                                                                               
         CLC   OBILADJ,NBILADJ     PCT. AJUSTMENT CHANGED ?                     
         BE    EDTCOMPX            NO - EXIT                                    
*                                                                               
         OC    OBILADJ,OBILADJ     OLD PCT. ADJUST ZERO ?                       
         BNZ   ECOMT20             NO                                           
         MVC   DI_OVAL(2),=C'+0'                                                
         B     ECNEWOUT                                                         
*                                                                               
ECOMT20  DS    0H                                                               
         EDIT  (B3,OBILADJ),(8,WORK+1),4,ALIGN=LEFT                             
         OI    WORK+6,C'0'                                                      
         OI    WORK+7,C'0'                                                      
         OI    WORK+8,C'0'                                                      
         LA    RF,WORK+8                                                        
         CLI   0(RF),C'0'                                                       
         BH    *+20                                                             
         BL    *+12                                                             
         MVI   0(RF),C' '                                                       
         BCT   RF,*-16                                                          
         MVI   0(RF),C' '                                                       
         MVI   WORK,C'+'                                                        
         TM    OBILADJ,X'80'         IF NEGATIVE                                
         BNO   *+8                                                              
         MVI   WORK,C'-'                                                        
*                                                                               
         MVC   DI_OVAL(8),WORK    RETURN "OLD" PERCENTAGE                       
*                                                                               
ECNEWOUT DS    0H                                                               
         OC    NBILADJ,NBILADJ     NEW PCT. ADJUST ZERO ?                       
         BNZ   ECNEW20             NO                                           
         MVC   DI_NVAL(2),=C'+0'                                                
         B     ECOMOUT             OUTPUT FIELDS                                
*                                                                               
ECNEW20  DS    0H                                                               
         EDIT  (B3,NBILADJ),(8,WORK+1),4,ALIGN=LEFT                             
         OI    WORK+6,C'0'                                                      
         OI    WORK+7,C'0'                                                      
         OI    WORK+8,C'0'                                                      
         LA    RF,WORK+8                                                        
         CLI   0(RF),C'0'                                                       
         BH    *+20                                                             
         BL    *+12                                                             
         MVI   0(RF),C' '                                                       
         BCT   RF,*-16                                                          
         MVI   0(RF),C' '                                                       
         MVI   WORK,C'+'                                                        
         TM    NBILADJ,X'80'         IF NEGATIVE                                
         BNO   *+8                                                              
         MVI   WORK,C'-'                                                        
*                                                                               
         MVC   DI_NVAL(8),WORK    RETURN "NEW" PERCENTAGE                       
*                                                                               
ECOMOUT  DS    0H                  OUTPUT FIELDS                                
         GOTOR DI_APUT                                                          
*                                                                               
EDTCOMPX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDPRCT'              
***********************************************************************         
*                                                                     *         
*   Analyse and Output Bill Commission Percentage Adjustment          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDPRCT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO BILPADJ FIELD                       
         L     R3,DI_AOUT          POINT TO OUTPUT FIELD                        
*                                                                               
         OC    0(L'BILPADJ,R2),0(R2)                                            
         BNZ   EDPRCT20                                                         
*                                                                               
         LR    RF,R2                                                            
         SH    RF,=Y(BILPADJ-BILBASB)   POINT RF TO BILBASB FIELD               
         TM    0(RF),X'08'         AGY COMM (AC) ?                              
         BNO   EDPRCTX             NO - DONE                                    
         LR    RF,R2                                                            
         AH    RF,=Y(BILPBASB-BILPADJ)  POINT RF TO BILPBASB FIELD              
         CLI   0(RF),0             ANYTHING THERE ?                             
         BE    EDPRCTX             NO - DONE                                    
         MVC   0(2,R3),=C'+0'                                                   
         B     EDPRCTX             DONE                                         
*                                                                               
EDPRCT20 DS    0H                                                               
         EDIT  (B3,0(R2)),(8,WORK+1),4,ALIGN=LEFT                               
         OI    WORK+6,C'0'                                                      
         OI    WORK+7,C'0'                                                      
         OI    WORK+8,C'0'                                                      
         LA    RF,WORK+8                                                        
         CLI   0(RF),C'0'                                                       
         BH    *+20                                                             
         BL    *+12                                                             
         MVI   0(RF),C' '                                                       
         BCT   RF,*-16                                                          
         MVI   0(RF),C' '                                                       
         MVI   WORK,C'+'                                                        
         TM    0(R2),X'80'        IF NEGATIVE                                   
         BNO   *+8                                                              
         MVI   WORK,C'-'                                                        
*                                                                               
         MVC   0(8,R3),WORK       RETURN PERCENTAGE                             
*                                                                               
EDPRCTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
***********************************************************************         
* Edit pub lock                                                       *         
***********************************************************************         
                                                                                
EDPLCK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO INPUT FIELD (PPRDSTAT)              
         L     R3,DI_AOUT          POINT TO OUTPUT FIELD                        
*                                                                               
         MVI   0(R3),C'N'                                                       
         TM    0(R2),X'80'         TEST PUB IS LOCKED                           
         JZ    *+8                                                              
         MVI   0(R3),C'Y'                                                       
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDTEFD'              
***********************************************************************         
*                                                                     *         
*        FORMAT EFFECTIVE DATE (AND OTHER 2-CHARACTER YYMM DATES)     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDTEFD   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO DATE FIELD                          
         L     R3,DI_AOUT          POINT TO RETURN AREA                         
*                                                                               
         OC    0(2,R2),0(R2)       SKIP IF NONE                                 
         BZ    EDTEFDX                                                          
*                                                                               
         MVC   WORK(2),0(R2)                                                    
         MVI   WORK+2,1                                                         
         L     RF,CDATCON                                                       
         GOTOR (RF),DMCB,(3,WORK),(6,(R3))     MMM/YY                           
*                                                                               
EDTEFDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDPCMT'              
***********************************************************************         
*                                                                     *         
*        OUTPUT PRBILL Comment Code                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDPCMT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO BILCMNTS                            
         L     R3,DI_AOUT          POINT TO OUTPUT FIELD                        
*                                                                               
         OC    1(6,R2),1(R2)       SKIP IF NO COMMENT CODE                      
         BZ    EDPCMTX                                                          
*                                                                               
         MVC   WORK(6),1(R2)                                                    
EDPCM20  DS    0H                                                               
         CLI   WORK,C' '           NEED TO LEFT JUSTIFY?                        
         BH    EDPCM30             NO                                           
         MVC   WORK(5),WORK+1      OVERLAPPING MOVE                             
         MVI   WORK+5,C' '                                                      
         B     EDPCM20                                                          
*                                                                               
EDPCM30  DS    0H                                                               
         MVC   0(6,R3),WORK        RETURN COMMENT CODE LEFT-JUSTIFIED           
*                                                                               
EDPCMTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDPCON'              
***********************************************************************         
*                                                                     *         
*        OUTPUT PRBILL Controls                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDPCON   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO BILCMNTS                            
         L     R3,DI_AOUT          POINT TO OUTPUT FIELD                        
*                                                                               
         OC    0(1,R2),0(R2)       SKIP IF NO CONTROL BYTE                      
         BZ    EDPCONX                                                          
*                                                                               
         TM    0(R2),X'80'         REGULAR?                                     
         BZ    *+12                                                             
         MVI   0(R3),C'R'          R=REGULAR                                    
         LA    R3,1(R3)                                                         
         TM    0(R2),X'40'         CASH DISCOUNT?                               
         BZ    *+12                                                             
         MVI   0(R3),C'C'          C=CD                                         
         LA    R3,1(R3)                                                         
         TM    0(R2),X'20'         ADJUSTMENT?                                  
         BZ    *+8                                                              
         MVI   0(R3),C'A'          A=ADJ                                        
*                                                                               
EDPCONX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDPTYP'              
***********************************************************************         
*                                                                     *         
*        OUTPUT PRBILL Bill Types                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDPTYP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO BILCMNTS                            
         L     R3,DI_AOUT          POINT TO OUTPUT FIELD                        
*                                                                               
         OC    0(1,R2),0(R2)       SKIP IF NO CONTROL BYTE                      
         BZ    EDPTYPX                                                          
*                                                                               
         TM    0(R2),X'EE'                                                      
         BZ    EDPTYPX             NO COMMENT AND NO BILL TYPE                  
         TM    0(R2),X'0F'                                                      
         BZ    EDPTYP30            "ALL" BILL TYPES                             
         TM    0(R2),X'01'                                                      
         BO    *+12                                                             
         MVI   0(R3),C'4'                                                       
         LA    R3,1(R3)                                                         
         TM    0(R2),X'02'                                                      
         BO    *+12                                                             
         MVI   0(R3),C'5'                                                       
         LA    R3,1(R3)                                                         
         TM    0(R2),X'04'                                                      
         BO    *+12                                                             
         MVI   0(R3),C'6'                                                       
         LA    R3,1(R3)                                                         
         TM    0(R2),X'08'                                                      
         BO    EDPTYPX                                                          
         MVI   0(R3),C'7'                                                       
         B     EDPTYPX                                                          
*                                                                               
EDPTYP30 MVC   0(3,R3),=C'ALL'                                                  
*                                                                               
EDPTYPX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDCHDT'              
***********************************************************************         
*                                                                     *         
*        FORMAT YYMMDD DATES TO MMMDD/YY OUTPUT                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDCHDT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO DATE FIELD                          
         L     R3,DI_AOUT          POINT TO OUTPUT AREA                         
*                                                                               
         OC    0(3,R2),0(R2)       SKIP IF NONE                                 
         BZ    EDCHDTX                                                          
*                                                                               
         L     RF,CDATCON                                                       
         GOTOR (RF),DMCB,(0,(R2)),(5,(R3))     YYMMDD TO MMMDD/YY               
*                                                                               
EDCHDTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - FESCOM'              
***********************************************************************         
*                                                                     *         
*        FORMAT ESTIMATE STANDARD COMMENT OUTPUT (COULD BE TWO)       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FESCOM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO PESTCOM FIELD                       
         L     R3,DI_AOUT          POINT TO OUTPUT AREA                         
*                                                                               
         CLI   0(R2),0             SKIP IF NONE                                 
         BNH   FESCOMX                                                          
*                                                                               
         MVC   0(L'PESTCOM,R3),0(R2)     OUTPUT "FIRST" COMMENT                 
*                                                                               
         LA    R2,PESTCOM2-PESTCOM(R2)    BUMP TO PESTCOM2 FIELD                
         CLI   0(R2),0             ANYTHING THERE ?                             
         BNH   FESCOMX             NO - DONE                                    
*                                                                               
         MVI   6(R3),C','                                                       
         MVC   7(L'PESTCOM,R3),0(R2)     OUTPUT "SECOND" COMMENT                
*                                                                               
FESCOMX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - FESTAT'              
***********************************************************************         
*                                                                     *         
*        FORMAT ESTIMATE STATUS                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FESTAT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO PESTSTAT FIELD                      
         L     R3,DI_AOUT          POINT TO OUTPUT AREA                         
*                                                                               
         CLI   0(R2),1             LOCK OPTION ?                                
         BE    FESTAT20            YES                                          
         CLI   0(R2),2                                                          
         BNE   FESTAT30            YES                                          
*                                                                               
FESTAT20 DS    0H                                                               
*                                                                               
         MVC   0(1,R3),0(R2)       OUTPUT LOCK OPTION                           
         LA    R3,1(R3)            BUMP OVER 1                                  
*                                                                               
FESTAT30 DS    0H                                                               
*                                                                               
         LA    R2,PESTTEST-PESTSTAT(R2)    POINT TO PESTTEST FIELD              
         TM    0(R2),X'40'         STEWARDSHIP ESTIMATE?                        
         BZ    FESTAT40            NO                                           
*                                                                               
         MVC   0(4,R3),=C'STEW'                                                 
         B     FESTATX             DONE                                         
*                                                                               
FESTAT40 DS    0H                                                               
*                                                                               
         TM    0(R2),X'80'         TEST ESTIMATE?                               
         BZ    FESTAT50            NO                                           
*                                                                               
         MVC   0(4,R3),=C'TEST'                                                 
         B     FESTATX                                                          
*                                                                               
FESTAT50 DS    0H                                                               
*                                                                               
         MVC   0(4,R3),=C'LIVE'    DEFAULT TO LIVE                              
*                                                                               
FESTATX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDPURO'              
***********************************************************************         
*                                                                     *         
*    OUTPUT UP TO 11-CHAR. DOLLAR AMOUNT FROM 4-BYTE BINARY INPUT     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDPURO   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO INPUT FIELD                         
         L     R3,DI_AOUT          POINT TO OUTPUT AREA                         
*                                                                               
         OC    0(4,R2),0(R2)       SKIP IF NONE                                 
         BZ    EDPUROX                                                          
*                                                                               
         EDIT  (4,0(R2)),(11,0(R3)),2,ALIGN=LEFT,ZERO=BLANK                     
*                                                                               
EDPUROX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDESFH'              
***********************************************************************         
*                                                                     *         
*            ANALYZE ESTIMATE SPECIAL FINANCIAL HANDLING              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDESFH   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO INPUT FIELD (PESTTEST)              
         L     R3,DI_AOUT          POINT TO OUTPUT FIELD                        
*                                                                               
         TM    0(R2),X'01'         SFH FLAG SET ?                               
         BNO   EDESFHX             NO - DONE                                    
         MVI   0(R3),C'Y'          SET TO "Y"                                   
*                                                                               
EDESFHX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDCOS2'              
***********************************************************************         
*                                                                     *         
*                EDIT 6-DECIMAL COST2 FACTOR                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDCOS2   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO COST2 FIELD                         
         L     R3,DI_AOUT          POINT TO OUTPUT AREA                         
*                                                                               
         OC    0(L'PESTCF,R2),0(R2)       SKIP IF NONE                          
         BZ    EDCOS2X                                                          
*                                                                               
         CP    0(5,R2),=P'0'                                                    
         BNE   *+14                                                             
         MVC   0(3,R3),=C'0.0'     COS2=0.0                                     
         B     EDCOS2X             DONE                                         
         EDIT  (P5,0(R2)),(8,0(R3)),6,ALIGN=LEFT,FILL=0                         
         LA    R1,8(R3)                                                         
         LA    RF,8                CLR TRAILING ZEORS                           
DR32H    BCTR  R1,0                                                             
         CLI   0(R1),C'.'          DECIMAL POINT?                               
         BE    DR32M                                                            
         CLI   0(R1),C'0'          TRAILING ZERO?                               
         BNE   EDCOS2X             NO - DONE                                    
         MVI   0(R1),C' '          CLR TRAILING ZERO                            
         BCT   RF,DR32H                                                         
*                                                                               
         DC    H'0'                INCORRECT OUTPUT FROM EDIT                   
*                                                                               
DR32M    AHI   R1,1                BUMP PAST DECIMAL POINT                      
         MVI   0(R1),C'0'          NON-SIGNIFICANT ZERO                         
*                                                                               
EDCOS2X  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDPCT4'              
***********************************************************************         
*                                                                     *         
*        OUTPUT 4-DECIMAL NUMERIC FIELD FROM 4-BYTE BINARY INPUT      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDPCT4   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO INPUT FIELD                         
         L     R3,DI_AOUT          POINT TO OUTPUT AREA                         
*                                                                               
         OC    0(4,R2),0(R2)       SKIP IF NONE                                 
         BZ    EDPCT4X                                                          
*                                                                               
         EDIT  (4,0(R2)),(10,0(R3)),4,ALIGN=LEFT,FLOAT=-                        
*                                                                               
EDPCT4X  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDABAS'              
***********************************************************************         
*                                                                     *         
*               ANALYZE AORBAS FIELD (OF AORREC)                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDABAS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO INPUT FIELD (AORBAS)                
         L     R3,DI_AOUT          POINT TO OUTPUT FIELD                        
*                                                                               
         MVI   0(R3),C'G'          SET OUTPUT TO "G" (DEFAULT)                  
         CLI   0(R2),0                                                          
         BE    EDABASX             DONE                                         
         MVI   0(R3),C'A'          SET TO "A"                                   
         CLI   0(R2),X'80'                                                      
         BE    EDABASX             DONE                                         
         MVI   0(R3),C'N'          SET TO "N"                                   
         CLI   0(R2),X'40'                                                      
         BE    EDABASX             DONE                                         
         MVC   0(2,R3),=C'XX'      SET TO "XX" (SHOULD NEVER HAPPEN)            
*                                                                               
EDABASX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDAOREST'            
***********************************************************************         
*                                                                     *         
*    OUTPUT AOR RECORD ESTIMATE - BINARY TO 999 OR "ALL" IF X'FFFF'   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDAOREST NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO INPUT FIELD                         
         L     R3,DI_AOUT          POINT TO OUTPUT AREA                         
*                                                                               
         CLI   0(R2),X'FF'         OUTPUT "ALL" ?                               
         BNE   EDAORE20            NO                                           
         MVC   0(3,R3),=C'ALL'                                                  
         B     EDAORESX                                                         
*                                                                               
EDAORE20 DS    0H                                                               
*                                                                               
         EDIT  (2,0(R2)),(3,0(R3)),ALIGN=LEFT                                   
*                                                                               
EDAORESX DS    0H                                                               
*                                                                               
         MVI   DI_LOUT,3                                                        
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDPBFPRD'            
***********************************************************************         
*                                                                     *         
*    OUTPUT BFORM RECORD PRODUCT - "ALL" IF PRODUCT CODE=AAA          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDPBFPRD NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO INPUT FIELD                         
         L     R3,DI_AOUT          POINT TO OUTPUT AREA                         
*                                                                               
         CLC   0(3,R2),=C'AAA'     OUTPUT "ALL" ?                               
         BNE   EDPBFP20            NO                                           
         MVC   0(3,R3),=C'ALL'                                                  
         B     EDPBFPX                                                          
*                                                                               
EDPBFP20 DS    0H                                                               
*                                                                               
         MVC   0(3,R3),0(R2)                                                    
*                                                                               
EDPBFPX  DS    0H                                                               
*                                                                               
         MVI   DI_LOUT,3                                                        
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDPBFEST'            
***********************************************************************         
*                                                                     *         
*    OUTPUT BFORM RECORD ESTIMATE - BINARY TO 999 OR "ALL" IF NULLS   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDPBFEST NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO INPUT FIELD                         
         L     R3,DI_AOUT          POINT TO OUTPUT AREA                         
*                                                                               
         CLC   0(2,R2),=X'0000'    OUTPUT "ALL" ?                               
         BNE   EDPBFE20            NO                                           
         MVC   0(3,R3),=C'ALL'                                                  
         B     EDPBFESX                                                         
*                                                                               
EDPBFE20 DS    0H                                                               
*                                                                               
         EDIT  (2,0(R2)),(3,0(R3)),ALIGN=LEFT                                   
*                                                                               
EDPBFESX DS    0H                                                               
*                                                                               
         MVI   DI_LOUT,3                                                        
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
***********************************************************************         
*                                                                     *         
*    OUTPUT PUB NUMBER                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDTPUB   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO INPUT FIELD                         
         L     R3,DI_AOUT          POINT TO OUTPUT AREA                         
*                                                                               
         GOTOR VPUBEDIT,DMCB,(X'08',(R2)),(C'S',(R3))                           
         MVI   DI_LOUT,15          Example: 12345678,01,ABC -> 15 max           
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*    OUTPUT CLIENT CODE                                               *         
*                                                                     *         
***********************************************************************         
EDTCLT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO INPUT FIELD                         
         L     R3,DI_AOUT          POINT TO OUTPUT AREA                         
*                                                                               
         CLC   0(3,R2),=X'FFFFFF'  ALL CLIENT?                                  
         JNE   E_CLT20                                                          
         MVC   0(3,R3),=C'   '     Spaces indicate ALL client                   
         J     E_CLT50                                                          
*                                                                               
E_CLT20  CLI   0(R2),X'FF'         OFFICE CODE?                                 
         JNE   E_CLT30                                                          
         MVI   0(R3),C'*'                                                       
         MVC   1(2,R3),1(R2)                                                    
         J     E_CLT50                                                          
*                                                                               
E_CLT30  MVC   0(3,R3),0(R2)       CLIENT CODE                                  
*                                                                               
E_CLT50  MVI   DI_LOUT,3                                                        
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDPBFDT'             
***********************************************************************         
*                                                                     *         
*        OUTPUT BFORM RECORD Start MOS - MMM/YY OR * IF X'FFFF'       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDPBFDT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO INPUT FIELD                         
         L     R3,DI_AOUT          POINT TO OUTPUT AREA                         
*                                                                               
         CLC   0(2,R2),=X'FFFF'    OUTPUT "*" ?                                 
         BNE   EDPBFDTD            NO                                           
         MVI   0(R3),C'*'                                                       
         B     EDPBFDTX                                                         
*                                                                               
EDPBFDTD DS    0H                                                               
*                                                                               
         MVC   FULL,0(R2)                                                       
         XC    FULL(2),=2X'FF'     UNCOMPLEMENT THE MOS                         
         MVI   FULL+2,X'01'                                                     
         L     RF,CDATCON          SET OUTPUT (MMM/YY)                          
         GOTOR (RF),DMCB,(3,FULL),(9,0(R3))                                     
*                                                                               
EDPBFDTX DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDPBFF'              
***********************************************************************         
*                                                                     *         
*   OUTPUT BFORM RECORD "Formula" FIELD (EXAMPLE: NET,+3.0000,AGYCOM) *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDPBFF   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
                                                                                
         L     R2,DI_AINP          POINT TO INPUT FIELD                         
         L     R3,DI_AOUT          POINT TO OUTPUT AREA                         
*                                                                               
         MVC   OBILA(L'PBFRCFML),0(R2)   INPUT FLD TO 5-BYTE WORK AREA          
*                                                                               
         TM    OBILA,X'41'         FIGURE OUT WHICH BILL BASIS WE HAVE          
         BNO   DFML10                                                           
         MVC   0(6,R3),=C'CGROSS'                                               
         LA    R3,6(R3)                                                         
         B     DFML50                                                           
*                                                                               
DFML10   TM    OBILA,X'42'                                                      
         BNO   DFML20                                                           
         MVC   0(4,R3),=C'CNET'                                                 
         LA    R3,4(R3)                                                         
         B     DFML50                                                           
*                                                                               
DFML20   TM    OBILA,X'45'                                                      
         BNO   DFML25                                                           
         MVC   0(9,R3),=C'CGROSS-CD'                                            
         LA    R3,9(R3)                                                         
         B     DFML50                                                           
*                                                                               
DFML25   TM    OBILA,X'46'                                                      
         BNO   DFML27                                                           
         MVC   0(7,R3),=C'CNET-CD'                                              
         LA    R3,7(R3)                                                         
         B     DFML50                                                           
*                                                                               
DFML27   TM    OBILA,X'48'                                                      
         BNO   DFML29                                                           
         MVC   0(7,R3),=C'CAGYCOM'                                              
         LA    R3,7(R3)                                                         
         B     DFML50                                                           
*                                                                               
DFML29   TM    OBILA,X'05'                                                      
         BNO   DFML31                                                           
         MVC   0(8,R3),=C'GROSS-CD'                                             
         LA    R3,8(R3)                                                         
         B     DFML50                                                           
*                                                                               
DFML31   TM    OBILA,X'06'                                                      
         BNO   DFML33                                                           
         MVC   0(6,R3),=C'NET-CD'                                               
         LA    R3,6(R3)                                                         
         B     DFML50                                                           
*                                                                               
DFML33   TM    OBILA,X'01'                                                      
         BNO   DFML35                                                           
         MVC   0(5,R3),=C'GROSS'                                                
         LA    R3,5(R3)                                                         
         B     DFML50                                                           
*                                                                               
DFML35   TM    OBILA,X'02'                                                      
         BNO   DFML37                                                           
         MVC   0(3,R3),=C'NET'                                                  
         LA    R3,3(R3)                                                         
         B     DFML50                                                           
*                                                                               
DFML37   B     DFMLX               BAD BASIS - SKIP                             
*                                                                               
DFML50   OC    OBILADJ,OBILADJ     IF NO COMMISSION                             
         BZ    DFMLX               THEN JUST BILL BASIS                         
*                                                                               
         MVI   0(R3),C','          OTHERWISE SEPARATE FIELD WITH COMMAS         
         LA    R3,1(R3)                                                         
*                                                                               
         EDIT  (B3,OBILADJ),(8,1(R3)),4,ALIGN=LEFT                              
         MVI   0(R3),C'+'                                                       
         TM    OBILADJ,X'80'                                                    
         BZ    *+8                                                              
         MVI   0(R3),C'-'                                                       
*                                                                               
         LA    R1,8(R3)                                                         
DFML65   CLI   0(R1),C' '                                                       
         BNE   *+10                                                             
         BCTR  R1,0                                                             
         B     DFML65                                                           
*                                                                               
         LA    R3,1(R1)                                                         
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
*                                                                               
DFML66   TM    OBILB,X'08'                                                      
         BNO   DFML67                                                           
         MVC   0(6,R3),=C'AGYCOM'                                               
         LA    R3,6(R3)                                                         
         B     DFML80                                                           
*                                                                               
*                                                                               
DFML67   TM    OBILB,X'05'                                                      
         BNO   DFML69                                                           
         MVC   0(8,R3),=C'GROSS-CD'                                             
         LA    R3,8(R3)                                                         
         B     DFML80                                                           
*                                                                               
DFML69   TM    OBILB,X'06'                                                      
         BNO   DFML71                                                           
         MVC   0(6,R3),=C'NET-CD'                                               
         LA    R3,6(R3)                                                         
         B     DFML80                                                           
*                                                                               
DFML71   TM    OBILB,X'01'                                                      
         BNO   DFML73                                                           
         MVC   0(5,R3),=C'GROSS'                                                
         LA    R3,5(R3)                                                         
         B     DFML80                                                           
*                                                                               
DFML73   TM    OBILB,X'02'                                                      
         BNO   DFML75                                                           
         MVC   0(3,R3),=C'NET'                                                  
         LA    R3,3(R3)                                                         
         B     DFML80                                                           
*                                                                               
DFML75   DC    H'0'                BAD BASIS                                    
*                                                                               
DFML80   DS    0H                                                               
*                                                                               
DFMLX    DS    0H                                                               
*                                                                               
EDPBFFX  DS     0H                                                              
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDPBFAC'             
***********************************************************************         
*                                                                     *         
*        OUTPUT BFORM RECORD "Show AC as %" FIELD                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDPBFAC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO INPUT FIELD                         
         L     R3,DI_AOUT          POINT TO OUTPUT AREA                         
*                                                                               
*              MOVE 3-BYTE PCT AND 1-BYTE INDICATOR TO WORK FIELDS              
         MVC   OBILADJ(L'PBFRCACP+L'PBFRCACO),0(R2)                             
*                                                                               
         OC    OBILADJ,OBILADJ     SEE IF I HAVE                                
         BZ    EDPBFACX            NO - SKIP                                    
*                                                                               
         CLI   NBILA,X'01'         GROSS?                                       
         BE    EDPBFACD                                                         
         CLI   NBILA,X'02'         NET?                                         
         BE    EDPBFACD                                                         
         CLI   NBILA,X'05'         GROSS-CD                                     
         BE    EDPBFACD                                                         
         CLI   NBILA,X'06'         NET-CD                                       
*                                                                               
         BNE   EDPBFACX            INDICATORS NOT VALID - SKIP                  
*                                                                               
EDPBFACD DS    0H                                                               
*                                                                               
         EDIT  (B3,OBILADJ),(8,1(R3)),4,ALIGN=LEFT                              
         MVI   0(R3),C'+'                                                       
         TM    OBILADJ,X'80'                                                    
         BZ    *+8                                                              
         MVI   0(R3),C'-'                                                       
*                                                                               
EDPBFACX DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - EDPBFOF'             
***********************************************************************         
*                                                                     *         
*        OUTPUT BFORM RECORD "Show AC as % Of" FIELD                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDPBFOF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
*                                                                               
         L     R2,DI_AINP          POINT TO INPUT FIELD                         
         L     R3,DI_AOUT          POINT TO OUTPUT AREA                         
*                                                                               
         MVC   NBILA(L'PBFRCACO),0(R2)                                          
*                                                                               
         CLI   NBILA,0             ANYTHING ?                                   
         BE    EDPBFOFX            NO - SKIP                                    
*                                                                               
         TM    NBILA,X'08'                                                      
         BNO   *+14                                                             
         MVC   0(6,R3),=C'AGYCOM'                                               
         B     EDPBFOFX                                                         
*                                                                               
         TM    NBILA,X'05'                                                      
         BNO   *+14                                                             
         MVC   0(8,R3),=C'GROSS-CD'                                             
         B     EDPBFOFX                                                         
*                                                                               
         TM    NBILA,X'06'                                                      
         BNO   *+14                                                             
         MVC   0(6,R3),=C'NET-CD'                                               
         B     EDPBFOFX                                                         
*                                                                               
         TM    NBILA,X'01'                                                      
         BNO   *+14                                                             
         MVC   0(5,R3),=C'GROSS'                                                
         B     EDPBFOFX                                                         
*                                                                               
         TM    NBILA,X'02'                                                      
         BNO   EDPBFOFX            BAD "AC % OF BASIS" - SKIP                   
         MVC   0(3,R3),=C'NET'                                                  
*                                                                               
EDPBFOFX DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
                                                                                
         EJECT                                                                  
                                                                                
ESTCTRLM DS    0XL(DM_LNQ)         ** ECNTRL KEYWORDS **                        
         DC    CL(L'DM_Z)' ',CL(L'DM_O)' '                                      
         DC    CL(L'DM_Z)' ',CL(L'DM_O)' '                                      
         DC    CL(L'DM_Z)' ',CL(L'DM_O)' '                                      
         DC    CL(L'DM_Z)' ',CL(L'DM_O)' '                                      
         DC    CL(L'DM_Z)' ',CL(L'DM_O)'LOCK'                                   
         DC    CL(L'DM_Z)' ',CL(L'DM_O)'HOLD'                                   
         DC    CL(L'DM_Z)' ',CL(L'DM_O)' '                                      
         DC    CL(L'DM_Z)' ',CL(L'DM_O)' '                                      
                                                                                
ESTCONTM DS    0XL(DM_LNQ)         ** ECONTROL KEYWORDS **                      
         DC    CL(L'DM_Z)' ',CL(L'DM_O)'E'                                      
         DC    CL(L'DM_Z)' ',CL(L'DM_O)'NSC'                                    
         DC    CL(L'DM_Z)' ',CL(L'DM_O)' '                                      
         DC    CL(L'DM_Z)' ',CL(L'DM_O)' '                                      
         DC    CL(L'DM_Z)' ',CL(L'DM_O)' '                                      
         DC    CL(L'DM_Z)' ',CL(L'DM_O)' '                                      
         DC    CL(L'DM_Z)' ',CL(L'DM_O)' '                                      
         DC    CL(L'DM_Z)' ',CL(L'DM_O)' '                                      
                                                                                
         DC    AL1(DD_EOTQ)                                                     
         EJECT                                                                  
CTRYUSAQ EQU   1                   USA AGENCY                                   
CTRYCANQ EQU   2                   CANADIAN AGENCY                              
EOR      EQU   0                                                                
FF       EQU   X'FF'                                                            
COMMA    EQU   C','                                                             
                                                                                
LITERALS DS    0D                  ** GLOBAL LITERALS **                        
         LTORG                                                                  
                                                                                
ABUFF1   DC    A(BUFF1)            BUFFERIN CONTROL BLOCK 1                     
                                                                                
DMREAD   DC    C'DMREAD '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
GETREC   DC    C'GETREC '                                                       
                                                                                
CORPHS   DS    0AL1                ** CORE RESIDENT PHASE LIST **               
         DC    AL1(QGETINS)                                                     
         DC    AL1(QOFFICER)                                                    
CORPHSN  EQU   (*-CORPHS)/L'CORPHS                                              
                                                                                
DAYTAB   DS    0CL3                ** DAY TABLE **                              
         DC    C'MonTueWedThuFriSatSun'                                         
                                                                                
HIVALS   DC    4X'FF'                                                           
                                                                                
BBASTAB  DS    0XL9                ** BILL BASIS TABLE **                       
         DC    X'01',C'GROSS   '                                                
BBASTABL EQU   *-BBASTAB           TABLE ENTRY LENGTH                           
         DC    X'02',C'NET     '                                                
         DC    X'05',C'GROSS-CD'                                                
         DC    X'06',C'NET-CD  '                                                
         DC    X'08',C'AGY COMM'                                                
BBASTABN EQU   (*-BBASTAB)/BBASTABL                                             
                                                                                
CBASTAB  DS    0XL6                ** COMMISSION BASIS TABLE **                 
         DC    X'01',C'NET  '                                                   
         DC    X'00',C'GROSS'                                                   
CBASTABN EQU   (*-CBASTAB)/L'CBASTAB                                            
*                                                                               
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - PFLIST'              
***********************************************************************         
* System/File list                                                    *         
***********************************************************************         
                                                                                
PFLIST   DS    0D                                                               
                                                                                
         DC    C'PRINT  '                                                       
                                                                                
         DC    C'N'                                                             
PRTDIR   DC    C'PRTDIR '                                                       
         DC    C'N'                                                             
PRTFIL   DC    C'PRTFILE'                                                       
         DC    C'N'                                                             
PUBDIR   DC    C'PUBDIR '                                                       
         DC    C'N'                                                             
PUBFIL   DC    C'PUBFILE'                                                       
         DC    C'X'                                                             
*                                                                               
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - PRGTAB'              
***********************************************************************         
* Program table                                                       *         
***********************************************************************         
                                                                                
PRGTAB   DS    0XL(DP_LNQ)                                                      
         DC    X'01',CL(L'DP_NAME)'PFM'                                         
         DC    X'03',CL(L'DP_NAME)'PAY'                                         
         DC    X'07',CL(L'DP_NAME)'AFM'                                         
         DC    X'11',CL(L'DP_NAME)'BUY'                                         
         DC    X'14',CL(L'DP_NAME)'ADB'                                         
         DC    X'18',CL(L'DP_NAME)'MBC'                                         
         DC    X'1C',CL(L'DP_NAME)'SFM'                                         
         DC    X'04',CL(L'DP_NAME)'FILE'                                        
PRGTABX  DC    AL1(DP_EOTQ)                                                     
                                                                                
BUFF1    BUFFD TYPE=B,COLUMNS=0,KEYLEN=BUFFKEYL,BUFFERS=255                     
*                                                                               
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - WORKD'               
***********************************************************************         
*                                                                     *         
*        LOCAL WORKING STORAGE                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WORKD    DSECT                     ** LOCAL WORKING STORAGE **                  
DUB      DS    D                                                                
FULL     DS    F                                                                
DMCB     DS    6F                                                               
WORK     DS    XL256                                                            
                                                                                
APHASES  DS    0A                  ** CORE RESIDENT PHASE ADDRESSES **          
VGETINS  DS    A                                                                
VOFFICER DS    A                                                                
         DS    16A                                                              
                                                                                
VPSTVAL  DS    A                   A(CANADIAN PST HANDLER)                      
VPUBEDIT DS    A                   A(PUBEDIT)                                   
                                                                                
RAGY     DS    XL(L'PAGYKAGY)      EXTRACTED AGENCY ID                          
RMED     DS    XL(L'PCLTKMED)      EXTRACTED MEDIA                              
RCLT     DS    XL(L'PCLTKCLT)      EXTRACTED CLIENT   CODE                      
RPRD     DS    XL(L'PPRDKPRD)      EXTRACTED PRODUCT  CODE                      
REST     DS    XL(L'PESTKEST)      EXTRACTED ESTIMATE CODE                      
RPUB     DS    XL(L'PUBKPUB)       EXTRACTED PUB NUMBER                         
RREP     DS    XL(L'PREPKREP)      EXTRACTED REP CODE                           
                                                                                
KEY      DS    XL64                GENERAL KEY BUILDING AREA                    
KEYSAVE  DS    XL64                GENERAL KEY SAVE     AREA                    
                                                                                
BYTE     DS    X                                                                
                                                                                
*******************  DO NOT "SEPARATE" BELOW FIELDS  *****************          
OBILA    DS    X                   "OLD" BILL BASIS                             
OBILB    DS    X                   "OLD" COMMISSION BASIS                       
OBILADJ  DS    XL3                 "OLD" PERCENT ADJUSTMENT                     
OBILLN   EQU   *-OBILA                                                          
                                                                                
NBILA    DS    X                   "NEW" BILL BASIS                             
NBILB    DS    X                   "NEW" COMMISSION BASIS                       
NBILADJ  DS    XL3                 "NEW" PERCENT ADJUSTMENT                     
NBILLN   EQU   *-NBILA                                                          
*******************  DO NOT "SEPARATE" ABOVE FIELDS  *****************          
                                                                                
PSTOUT   DS    CL64                OUTPUT FOR CANADIAN PST CODES                
                                                                                
BUFFREC  DS    0X                  ** BUFFERIN RECORD **                        
BUFFKEY  DS    0X                                                               
BUFFRAGY DS    CL(L'DS_ALF)        AGENCY ALPHA ID                              
BUFFRKEY DS    XL(L'KEY)           RECORD KEY                                   
BUFFKEYL EQU   *-BUFFREC                                                        
BUFFFLAG DS    X                   ** FLAG BYTE **                              
BUFFFRNF EQU   X'80'               RECORD NOT FOUND                             
BUFFRECD DS    0XL1500             RECORD FOLLOWS (IF FOUND)                    
BUFFRECL EQU   *-BUFFREC                                                        
                                                                                
         ORG   BUFFRECD                                                         
                                                                                
IO       DS    XL4096                                                           
                                                                                
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE DDBUFFD                                                        
       ++INCLUDE GEDFARD                                                        
       ++INCLUDE DDCOMFACSD                                                     
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE PRGENFILE                                                      
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DDPSTBLK                                                       
         PRINT ON                                                               
******************* END END END END END END ***********************             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046PPDFAR    02/25/20'                                      
         END                                                                    
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Estimate name'                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(EDESC-ESTHDR)                                                
         DC    AL1(L'EDESC)                                                     
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Start date'                                        
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(ESTART-ESTHDR)                                               
         DC    AL1(L'ESTART)                                                    
         DC    AL1(DD_TEDTQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'End date'                                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(EEND-ESTHDR)                                                 
         DC    AL1(L'EEND)                                                      
         DC    AL1(DD_TEDTQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Bill Basis'                                        
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(EBILLBAS-ESTHDR)                                             
         DC    AL1(L'EBILLBAS)                                                  
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTCBAS)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Commission%'                                       
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(EBILLCOM-ESTHDR)                                             
         DC    AL1(L'EBILLCOM)                                                  
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTCOMP)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Commission Basis'                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(EBILLBAS-ESTHDR)                                             
         DC    AL1(L'EBILLBAS)                                                  
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTBBAS)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Filters'                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(EPROF-ESTHDR)                                                
         DC    AL1(3)                                                           
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Copy Code'                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(ECOPY-ESTHDR)                                                
         DC    AL1(L'ECOPY)                                                     
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'PPecial Rep'                                       
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(EREP-ESTHDR)                                                 
         DC    AL1(L'EREP)                                                      
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTREP)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Request Range'                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(EREQLO-ESTHDR)                                               
         DC    AL1(L'EREQLO)                                                    
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTREQR)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Daypart Menu'                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(EDAYMENU-ESTHDR)                                             
         DC    AL1(L'EDAYMENU)                                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Master/Slave'                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(EMSTRIND-ESTHDR)                                             
         DC    AL1(L'EMSTRIND)                                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'OOW Start Day'                                     
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(EOWSDAY-ESTHDR)                                              
         DC    AL1(L'EOWSDAY)                                                   
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTOWS)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Master Est.'                                       
         DC    AL1(0)                                                           
         DC    AL1(DD_IZEBQ)                                                    
         DC    AL1(0)                                                           
         DC    AL2(EMSTREST-ESTHDR)                                             
         DC    AL1(L'EMSTREST)                                                  
         DC    AL1(DD_TBINQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Status'                                            
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(DD_IFMFQ)                                                    
         DC    AL2(ECNTRL-ESTHDR)                                               
         DC    AL1(L'ECNTRL)                                                    
         DC    AL1(DD_TMSKQ)                                                    
         DC    AL4(ESTCTRLM)                                                    
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)' '                                                 
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(DD_ILMFQ)                                                    
         DC    AL2(ELOCKYM-ESTHDR)                                              
         DC    AL1(L'ELOCKYM)                                                   
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTLOCK)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Options'                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(DD_IFMFQ)                                                    
         DC    AL2(EFLAG1-ESTHDR)                                               
         DC    AL1(L'EFLAG1)                                                    
         DC    AL1(DD_TMSKQ)                                                    
         DC    AL4(ESTFLG1M)                                                    
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'DAILY='                                            
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(EDAILY-ESTHDR)                                               
         DC    AL1(L'EDAILY)                                                    
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    AL1(COMMA)                                                       
         DC    AL1(DD_LPFXN),AL2(0)                                             
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'SLN='                                              
         DC    AL1(0)                                                           
         DC    AL1(DD_IZEBQ)                                                    
         DC    AL1(0)                                                           
         DC    AL2(ESLN-ESTHDR)                                                 
         DC    AL1(L'ESLN)                                                      
         DC    AL1(DD_TBINQ)                                                    
         DC    AL4(0)                                                           
         DC    AL1(COMMA)                                                       
         DC    AL1(DD_LPFXN),AL2(0)                                             
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'COS2='                                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(ECOST2-ESTHDR)                                               
         DC    AL1(L'ECOST2)                                                    
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTCOST2)                                                    
         DC    AL1(COMMA)                                                       
         DC    AL1(DD_LPFXN),AL2(0)                                             
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'PW='                                               
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(EPWPCT-ESTHDR)                                               
         DC    AL1(L'EPWPCT)                                                    
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTPWP)                                                      
         DC    AL1(COMMA)                                                       
         DC    AL1(DD_LPFXN),AL2(0)                                             
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'CASH='                                             
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(ECASHPRD-ESTHDR)                                             
         DC    AL1(L'ECASHPRD)                                                  
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTPRD)                                                      
         DC    AL1(COMMA)                                                       
         DC    AL1(DD_LPFXN),AL2(0)                                             
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'TRD='                                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(DD_ILMFQ)                                                    
         DC    AL2(ETRDPRD-ESTHDR)                                              
         DC    AL1(L'ETRDPRD)                                                   
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTPRD)                                                      
         DC    AL1(COMMA)                                                       
         DC    AL1(DD_LPFXN),AL2(0)                                             
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Control'                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(ECONTROL-ESTHDR)                                             
         DC    AL1(L'ECONTROL)                                                  
         DC    AL1(DD_TMSKQ)                                                    
         DC    AL4(ESTCONTM)                                                    
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Rate type'                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(DD_IFMFQ)                                                    
         DC    AL2(ERATE-ESTHDR)                                                
         DC    AL1(L'ERATE)                                                     
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)' '                                                 
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(DD_ILMFQ)                                                    
         DC    AL2(ERATECST-ESTHDR)                                             
         DC    AL1(L'ERATECST)                                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    XL12'00'                                                         
                                                                                
         TITLE 'PRINT SYSTEM DAILY FILE ACTIVITY EXTRACT - PPAGYF'              
***********************************************************************         
*                                                                     *         
*        Get client record                                            *         
*                                                                     *         
***********************************************************************         
                                                                                
GETCLT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    BUFFKEY(BUFFKEYL),BUFFKEY   INIT BUFFER KEY                      
*                                                                               
         L     RF,DI_ADSD          EXTRACT AGENCY ALPHA                         
         MVC   BUFFRAGY,DS_ALF-DS_D(RF)  FROM CURRENT SORT KEY                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING PCLTRECD,R2         R2=A(CLIENT KEY)                             
*                                                                               
         MVC   PCLTKAGY,BUFFRAGY   SET AGENCY                                   
*                                                                               
         L     RF,DI_ADAD          CURRENT AGENCY TABLE ENTRY                   
         MVC   PCLTKMED,DI_ALF-DS_D(RF)  FROM CURRENT SORT KEY                  
*                                                                               
         L     RF,0(R1)                                                         
         MVC   PCLTKCLT,0(RF)      SET CLIENT CODE                              
         MVC   BUFFRKEY,KEY                                                     
         MVC   WORK(BUFFKEYL),BUFFKEY                                           
                                                                                
         GOTOR DI_ABFIN,DMCB,('BUFFAGET',ABUFF1),BUFFREC,COMFACSD               
         CLI   BUFFERRS-BUFFPARM(R1),0                                          
         JE    EXITY                                                            
                                                                                
         LA    R0,BUFFREC          BUILD NEW BUFFER RECORD                      
         LHI   R1,BUFFRECL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   BUFFKEY(BUFFKEYL),WORK                                           
         GOTOR CDATAMGR,DMCB,DMREAD,PRTDIR,CKEY,CKEY                            
         JE    GETCLT02                                                         
         MVI   BUFFFLAG,BUFFFRNF   SET NOT FOUND IF CAN'T READ                  
         J     GETCLT04                                                         
                                                                                
GETCLT02 GOTOR CDATAMGR,DMCB,GETREC,PRTFIL,CKDA,BUFFRECD                        
         JE    GETCLT04                                                         
         DC    H'0'                                                             
                                                                                
GETCLT04 GOTOR DI_ABFIN,DMCB,('BUFFAPUT',ABUFF1),BUFFREC,COMFACSD               
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  R2                                                               
                                                                                
EDTCOST2 L     RF,DI_AINP                                                       
         OC    0(L'ECOST2,RF),0(RF)                                             
         BZR   RE                                                               
         ICM   R0,15,0(RF)                                                      
         CLI   0(RF),X'80'                                                      
         JNE   *+6                                                              
         SR    R0,R0                                                            
         EDITR (R0),(8,WORK),6,ALIGN=LEFT,FILL=0,DROP=5                         
         L     RF,DI_AOUT                                                       
         MVC   0(8,RF),WORK                                                     
         BR    RE                                                               
                                                                                
                                                                                
***********************************************************************         
* Edit bill basis                                                     *         
***********************************************************************         
                                                                                
EDTBBAS  L     R1,DI_AINP                                                       
         OC    L'EBILLBAS(L'EBILLCOM,R1),L'EBILLBAS(R1)                         
         BZR   RE                                                               
         MVC   WORK(L'EBILLBAS),0(R1)                                           
         NI    WORK,X'F0'                                                       
         LA    RF,BBASTAB                                                       
         LHI   R0,BBASTABN                                                      
EDTBBAS2 CLC   0(L'EBILLBAS,RF),WORK                                            
         JE    EDTBBAS4                                                         
         AHI   RF,L'BBASTAB                                                     
         JCT   R0,EDTBBAS2                                                      
         DC    H'0'                                                             
EDTBBAS4 L     R1,DI_AOUT                                                       
         MVC   0(L'BBASTAB-L'EBILLBAS,R1),L'EBILLBAS(RF)                        
         BR    RE                                                               
                                                                                
***********************************************************************         
* Edit commission percentage                                          *         
***********************************************************************         
                                                                                
EDTCOMP  L     R1,DI_AINP                                                       
         OC    0(L'EPWPCT,R1),0(R1)                                             
         BZR   RE                                                               
         ICM   R0,15,0(R1)                                                      
         EDITR (R0),(9,WORK),4,ALIGN=LEFT,FLOAT=+                               
         L     RF,DI_AOUT                                                       
         MVC   0(9,RF),WORK                                                     
         TM    0(R1),X'80'                                                      
         JZ    *+8                                                              
         MVI   0(RF),C'-'                                                       
         BR    RE                                                               
                                                                                
***********************************************************************         
* Edit commission basis                                               *         
***********************************************************************         
                                                                                
EDTCBAS  L     R1,DI_AINP                                                       
         OC    L'EBILLBAS(L'EBILLCOM,R1),L'EBILLBAS(R1)                         
         BZR   RE                                                               
         MVC   WORK(L'EBILLBAS),0(R1)                                           
         NI    WORK,X'0F'                                                       
         LA    RF,CBASTAB                                                       
         LHI   R0,CBASTABN                                                      
EDTCBAS2 CLC   0(L'EBILLBAS,RF),WORK                                            
         JE    EDTCBAS4                                                         
         AHI   RF,L'CBASTAB                                                     
         JCT   R0,EDTCBAS2                                                      
         DC    H'0'                                                             
EDTCBAS4 L     R1,DI_AOUT                                                       
         MVC   0(L'CBASTAB-L'EBILLBAS,R1),L'EBILLBAS(RF)                        
         BR    RE                                                               
