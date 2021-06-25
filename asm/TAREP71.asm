*          DATA SET TAREP71    AT LEVEL 026 AS OF 02/09/17                      
*PHASE T70371C,*                                                                
*INCLUDE DLFLD                                                                  
         TITLE 'T70371 - 1099 REPORT AND DOWNLOAD'                              
T70371   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70371,R6                                                      
         L     RC,0(R1)            RC=A(CONTROLLER W/S)                         
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(SCREEN)                                 
         USING T703FFD,RA                                                       
         L     R9,ASUBSYSD         R9=A(SYSTEM W/S)                             
         USING SUBSYSD,R9                                                       
         L     R8,ASPOOLD          R8=A(SPOOL DSECT)                            
         USING SPOOLD,R8                                                        
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING MYD,R7                                                           
         EJECT                                                                  
***********************************************************************         
*        MODE CONTROLLED ROUTINES                                     *         
***********************************************************************         
**       L     R6,ATWA                                                          
**       USING CONHEADH-64,R6                                                   
         MVC   AMASTD,TWAMASTC     SAVE ADDRESS OF MASTER                       
                                                                                
         MVC   VSORTER,SORTER                                                   
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
*                                                                               
                                                                                
         CLI   MODE,VALKEY                                                      
         JNE   *+8                                                              
         BRAS  RE,VKEY                                                          
                                                                                
         L     R1,TWADCONS                                                      
         USING TWADCOND,R1                                                      
         MVC   DYNALLOC,TDYNALLO                                                
                                                                                
         MVC   ASPFUSER,TSPFUSER                                                
         MVC   ALOGOC,TLOGOC       SAVE A(LOGOC)                                
         L     R1,AMASTD                                                        
         USING MASTD,R1                                                         
         MVC   ALOGO,MCVLOGO       SAVE A(LOGO)                                 
         MVC   AREMOT,MCVREMOT     SAVE A(REMOTE)                               
         DROP  R1                                                               
         CLI   MODE,PRINTREP                                                    
         JNE   XIT                                                              
         MVC   ADCB,=A(T99TAPE)                                                 
         BRAS  RE,OPENTAPE                                                      
         CLI   RECNUM,TNNPRT                                                    
         BNE   *+12                                                             
         BRAS  RE,READ1099                                                      
         B     *+8                                                              
                                                                                
         BRAS  RE,PREP             PRINT REPORT                                 
                                                                                
         BRAS  RE,CLOSTAPE                                                      
         CLI   RECNUM,TNNPRT    print queue report                              
         BNE   *+8                                                              
         BRAS  RE,PREP2                                                         
                                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
FLDINV   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE RECORD                                                        
***********************************************************************         
VKEY     NTR1                                                                   
*                                                                               
         MVI   ALLEMP,C'N'                                                      
                                                                                
         CLI   RECNUM,TNNFCP                                                    
         BNE   VK30                                                             
                                                                                
         LA    R2,SNTPERH          PERIOD                                       
         LA    R3,BLOCK                                                         
         GOTO1 PDVAL,DMCB,(R3)                                                  
         USING PERVALD,R3                                                       
         MVC   TIQPSTR,PVALPSTA    SET DATES FOR SYSIO                          
         MVC   TIQPEND,PVALPEND                                                 
         CLC   PVALPSTA(1),PVALPEND                                             
         JNE   FLDINV                                                           
         MVC   SVRSTA,PVALPSTA                                                  
         MVC   SVREND,PVALPEND                                                  
         GOTO1 DATCON,DMCB,(1,SVRSTA),(20,SVRSTA)                               
         GOTO1 DATCON,DMCB,(1,SVREND),(20,SVREND)                               
         MVC   YEAR,SVRSTA                                                      
                                                                                
         LA    R2,SNTEMPH          EMPLOYER                                     
         CLI   5(R2),0                                                          
         BE    VK35                                                             
VK20     GOTO1 ANY                                                              
         B     VK36                                                             
*                                                                               
VK30     LA    R2,SPLYEARH         YEAR                                         
         GOTO1 ANY                                                              
         MVC   YEAR,WORK                                                        
         CLC   =C'0000',YEAR                                                    
         BE    FLDINV                                                           
         CLI   5(R2),4             MUST BE 4 CHARACTERS                         
         BNE   INVXIT                                                           
         MVC   WORK(4),ALLZEROS                                                 
         MVZ   WORK(4),8(R2)                                                    
         CLC   WORK(4),ALLZEROS                                                 
         BNE   INVXIT                                                           
         MVC   TIFYEAR(2),10(R2)                                                
*                                                                               
         LA    R2,SPLEMPH          EMPLOYER                                     
         CLI   5(R2),0                                                          
         BE    VK35                                                             
         GOTO1 ANY                                                              
         B     VK36                                                             
                                                                                
VK35     MVI   ALLEMP,C'Y'                                                      
         MVC   WORK(3),=C'TP '                                                  
                                                                                
VK36     MVC   TIFEMP,WORK                                                      
         BRAS  RE,EMPINFO          READ EMPLOYER INFO                           
                                                                                
* DONT  ALLOW TAX UNITS OR OPTIONS FIELDS                                       
VK80     CLI   RECNUM,TNNFCP                                                    
         JE    XIT                                                              
         LA    R2,SPLUNTH                                                       
         CLI   5(R2),0                                                          
         BNE   FLDINV                                                           
         LA    R2,SPLOPTH                                                       
         CLI   5(R2),0                                                          
         BNE   FLDINV                                                           
                                                                                
         J     XIT                                                              
         EJECT                                                                  
                                                                                
         LTORG                                                                  
INVERR   DC    C'** ERROR ** INVALID DATA'                                      
ALLZEROS DC    C'000000000000000'             15 ZEROS                          
INVXIT   MVC   CONHEAD(L'INVERR),INVERR                                         
BADXIT   GOTO1 ERREX2                                                           
         EJECT                                                                  
***********************************************************************         
* EMPLOYER INFO                                                                 
***********************************************************************         
EMPINFO  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   SYSFIL,=CL8'TALFIL'                                              
         MVC   SYSDIR,=CL8'TALDIR'                                              
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'A0',WORK)                                 
                                                                                
         MVI   ELCODE,TANAELQ                                                   
         L     R4,AIO                                                           
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         USING TANAD,R4                                                         
         ZIC   R1,TANALEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   EMPNAME(0),TANANAME                                              
*                                                                               
                                                                                
         MVI   ELCODE,TAADELQ                                                   
         L     R4,AIO                                                           
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         USING TAADD,R4                                                         
         ZIC   R1,TAADLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   EMPADD1(0),TAADADD                                               
*                                                                               
* irs number                                                                    
         MVI   ELCODE,TATIELQ      IRS NUMBER                                   
         MVI   WORK,TATITYUN                                                    
         MVC   WORK+1(3),=C'FD '                                                
         GOTO1 GETL,DMCB,(4,WORK)                                               
         JE    *+6                                                              
         DC    H'0'                MUST BE IRS NUMBER                           
         L     R4,TGELEM           GET IRS NUMBER FROM ID ELEM                  
         USING TATID,R4                                                         
         LA    RE,TATIID                                                        
         LA    RF,EMPTIN                                                        
         LA    R0,L'TATIID                                                      
EI50     CLI   0(RE),X'F0'                                                      
         JL    EI60                                                             
         MVC   0(1,RF),0(RE)                                                    
         AHI   RF,1                                                             
EI60     AHI   RE,1                                                             
         JCT   R0,EI50                                                          
                                                                                
         J     XIT                                                              
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PRINT REPORT                                                 *         
***********************************************************************         
                                                                                
PREP     NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVI   SORTFRST,C'Y'                                                    
         XC    SVP1CID,SVP1CID                                                  
         L     R1,=A(HEDSPECS)                                                  
         CLI   RECNUM,TNNBLD       ONLY SORT UNDER CORP/TYPE                    
         BNE   *+8                                                              
         L     R1,=A(HEDSPEC2)                                                  
         CLI   RECNUM,TNNFCP                                                    
         BNE   *+8                                                              
         L     R1,=A(HEDSPEC4)                                                  
         ST    R1,SPECS                                                         
                                                                                
                                                                                
                                                                                
         LA    R3,KEY              READ W4 PASSIVES                             
         USING TLW4PD,R3                                                        
         XC    TLW4PKEY,TLW4PKEY                                                
         MVI   TLW4PCD,TLW4LCDQ                                                 
PREP02   MVC   SYSFIL,=CL8'TALFIL'                                              
         MVC   SYSDIR,=CL8'TALDIR'                                              
         GOTO1 HIGH                                                             
         J     PREP06                                                           
PREP04   GOTO1 SEQ                                                              
PREP06   JNE   PREP200                                                          
                                                                                
         LA    RE,SVFIELDS                                                      
         LA    RF,SVFIELDL                                                      
         XCEF                                                                   
*&&DO                                                                           
         CLI   TLW4PCD,TLW4LCDQ                                                 
         JNE   PREP200                                                          
*                                                                               
* READ CORP W4 IF PASSIVE IS AN ESTATES W4 PASSIVE                              
         OC    TLW4LEST,TLW4LEST         ESTATES?                               
         BZ    PREP08                                                           
*                                                                               
         MVC   W4KEY,KEY                 SAVE OFF KEY                           
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING TLW4D,RE                                                         
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,W4KEY+(TLW4LEST-TLW4PD)                                  
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLW4PKEY),KEYSAVE                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  RE                                                               
         B     PREP10                                                           
*&&                                                                             
PREP08   MVC   W4KEY,KEY                                                        
         MVC   SVW4FID,TLW4LFID                                                 
                                                                                
PREP10   GOTO1 GETREC                                                           
                                                                                
         L     R4,AIO                                                           
         USING TAFLD,R4                                                         
         MVI   ELCODE,TAFLELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    PREP12                                                           
                                                                                
         OC    TLW4LEST,TLW4LEST         ESTATES?                               
         BNZ   PREP14                    DONT HAVE TO HAVE FILTERS              
         DC    H'0'                                                             
                                                                                
PREP12   MVC   SVW4FILT,TAFLFLT1                                                
                                                                                
PREP14   CLI   TLW4PCD,TLW4LCDQ                                                 
         JNE   PREP200                                                          
*                                                                               
         CLI   RECNUM,TNNFCP             FOREIGN CORP REPORT                    
         BNE   *+12                      ONLY SHOW FILTER F                     
         CLI   TAFLFLT1,C'F'                                                    
         BNE   PREP04                                                           
*                                                                               
         CLI   RECNUM,TNNFCP             REGULAR 1099 AND 1099 BLD              
         BE    *+12                      DONT PROCESS W4 TYPE F                 
         CLI   TAFLFLT1,C'F'                                                    
         BE    PREP04                                                           
*COMMENT OUT ESTATES CODE                                                       
                                                                                
* READ CORP W4 IF PASSIVE IS AN ESTATES W4 PASSIVE                              
         OC    TLW4LEST,TLW4LEST         ESTATES?                               
         BZ    PREP16                                                           
*LEAVE OUT ESTATES                                                              
         B     PREP04                                                           
*                                                                               
         MVC   W4KEY,KEY                 SAVE OFF KEY                           
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING TLW4D,RE                                                         
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,W4KEY+(TLW4LEST-TLW4PD)                                  
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLW4PKEY),KEYSAVE                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  RE                                                               
         GOTO1 GETREC                                                           
         MVI   SVW4FILT,C'E'              ESTATES                               
                                                                                
                                                                                
PREP16   L     R4,AIO                                                           
         USING TLW4D,R4                                                         
         MVI   ELCODE,TAW4ELQ                                                   
         USING TAW4D,R4                                                         
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   SVW4CRPN,TAW4CRPN      CORP NAME                                 
                                                                                
         L     R4,AIO                 CORP ADDRESS                              
         USING TAA2D,R4                                                         
         MVI   ELCODE,TAA2ELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVCPADD1,TAA2ADD1                                                
         MVC   SVCPADD2,TAA2ADD2                                                
         MVC   SVCPADD3,TAA2ADD3                                                
         MVC   SVCPCITY,TAA2CITY                                                
         MVC   SVCPST,TAA2ST                                                    
         MVC   SVCPZIP(2),=C', '                                                
         MVC   SVCPZIP+2(L'TAA2ZIP),TAA2ZIP                                     
         CLI   RECNUM,TNNFCP                                                    
         BNE   *+10                                                             
         MVC   SVCPCTRY,TAA2CTRY                                                
*----------- READ THE CHECK RECORDS --------------------------                  
                                                                                
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         LA    R3,KEY              READ CKE PASSIVES                            
         USING TLCKPD,R3                                                        
         XC    TLCKPKEY,TLCKPKEY                                                
         MVI   TLCKPCD,TLCKECDQ                                                 
         MVC   TLCKESSN,SVW4FID                                                 
         MVC   TLCKEEMP,TIFEMP                                                  
         MVI   TLCKECUR,C'U'                                                    
PREP030  GOTO1 HIGH                                                             
         J     PREP060                                                          
                                                                                
PREP040  GOTO1 SEQ                                                              
PREP060  JNE   PREP120                                                          
         CLI   TLCKPCD,TLCKECDQ                                                 
***      BNE   PREPX                                                            
         BNE   PREP200                                                          
*                                                                               
         MVC   SVCHKDTE,TLCKEDTE                                                
         OC    SVCHKDTE(3),SVCHKDTE       IGNORE ALL UNWRITTEN CHECKS           
         BZ    PREP040                                                          
         XC    SVCHKDTE,=X'FFFFFF'                                              
         GOTO1 DATCON,DMCB,(1,SVCHKDTE),(20,DUB)                                
                                                                                
         CLC   YEAR,DUB                   GET NEXT CHECK IF YEAR DOESNT         
         BNE   PREP040                    MATCH                                 
*                                                                               
         CLI   RECNUM,TNNFCP                                                    
         BNE   PREP070                                                          
         CLC   DUB,SVRSTA                 WITHIN REQUEST PERIOD?                
         BL    PREP040                                                          
         CLC   DUB,SVREND                                                       
         BH    PREP040                                                          
*                                                                               
*                                                                               
* IF NO MORE CHECKS MATCHING CORP FID THEN SEE IF WE                            
* HAVE ANY CHECKS READ IF SO WE NEED TO PRINT OUT LINE                          
* ELSE PREPARE TO READ NEXT W4 PASSIVE                                          
PREP070  CLC   KEY(TLCKEDTE-TLCKPKEY),KEYSAVE                                   
         JE    PREP100                                                          
         OC    SVCKSSN,SVCKSSN    SCCKSSN IS FILLED IN IF WE                    
         BZ    PREP120            HAVE CHECKS FOR THIS CORP                     
         B     PREP113                                                          
*                                                                               
*                                                                               
PREP100  GOTO1 GETREC                                                           
                                                                                
         L     R4,AIO                                                           
         USING TLCKD,R4                                                         
         MVC   SVCKSSN,TLCKSSN     CHECK SSN -CORP OR PERF                      
         MVC   SVINV,TLCKINV                                                    
         OC    PVCKSSN,PVCKSSN                                                  
         BNZ   *+10                                                             
         MVC   PVCKSSN,SVCKSSN                                                  
*                                                                               
PREP108  MVI   ELCODE,TACDELQ                                                   
         L     R4,AIO                                                           
         USING TACDD,R4                                                         
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVPDGRS,TACDNTAX                                                 
                                                                                
PREP110  MVI   ELCODE,TAPDELQ                                                   
         L     R4,AIO                                                           
         USING TAPDD,R4                                                         
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   SVPDREXP,TAPDREXP                                                
         ZICM  RE,SVPDGRS,(15)                                                  
         ZICM  RF,SVPDREXP,(15)                                                 
         SR    RE,RF                                                            
         STCM  RE,15,SVPDGRS                                                    
                                                                                
PREP113  XC    SVPFSSN,SVPFSSN                                                  
         MVC   CKKEY,KEY                                                        
         MVC   CKKEYSAV,KEYSAVE                                                 
         BRAS  RE,READW4                                                        
         MVC   KEY,CKKEY                                                        
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         GOTO1 HIGH                                                             
         MVC   KEYSAVE,CKKEYSAV                                                 
PREP114  BRAS  RE,PUTSRT                                                        
         MVC   PVCKSSN,SVCKSSN                                                  
                                                                                
*   IF ALREADY PUT OUT A GROSS AND WAGES SUMMARY LINE- THEN CLEAR               
         XC    SVPDGRS,SVPDGRS                                                  
         XC    SVPDREXP,SVPDREXP                                                
         XC    SVCKSSN,SVCKSSN                                                  
         XC    PVCKSSN,PVCKSSN                                                  
                                                                                
         J     PREP040        PROCESS THE CURRENT NEW CHECK SSN AGAIN           
                                                                                
PREP120  MVC   KEY,W4KEY                                                        
         MVC   SYSFIL,=CL8'TALFIL'                                              
         MVC   SYSDIR,=CL8'TALDIR'                                              
         GOTO1 HIGH                                                             
         J     PREP04                                                           
                                                                                
PREP200  DS    0C                                                               
         BRAS  RE,PUTOUT                                                        
PREPX    J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        PUT SORT RECORDS TO SORTER                                             
***********************************************************************         
PUTSRT   NTR1                                                                   
         XC    P,P                                                              
         LA    R1,P                                                             
         USING P1D,R1                                                           
         LA    R3,SRTPREC                                                       
         USING SRTPRECD,R3                                                      
*                                                                               
         MVC   P1CIDL(10),=C'Corp ID - '                                        
         MVC   P1CID,SVW4FID                                                    
         MVC   P1PSSNL(11),=C'Perf SSN - '                                      
**       CLC   P1CID,=C'800942980'                                              
**       JNE   XIT                                                              
* FOR 1099BLD JUST CLEAR OUT P1PSSN- WE ONLY GROUP BY CORP/TYPE                 
         CLI   SVW4FILT,C'L'       DONT DO IT FOR FILTER L WE NEED              
         BE    PUTSRT06            THE PERF SSN                                 
         CLI   RECNUM,TNNFCP       ONLY SORT UNDER CORP/TYPE                    
         BE    PUTSRT10                                                         
         CLI   RECNUM,TNNBLD       ONLY SORT UNDER CORP/TYPE                    
         BE    PUTSRT10                                                         
PUTSRT06 MVC   P1PSSN,SVPFSSN                                                   
                                                                                
PUTSRT10 MVC   P1GROSSB,SVPDGRS                                                 
         MVC   P1REXPB,SVPDREXP                                                 
         MVC   P1W4FILT,SVW4FILT                                                
         MVC   SRTPREC1,P                                                       
                                                                                
*                                                                               
         LA    R1,P                                                             
         XC    P,P                                                              
         USING P2D,R1                                                           
         MVC   P2CNAMEL(12),=C'Corp Name - '                                    
         MVC   P2CNAME,SVW4CRPN                                                 
         MVC   P2PNAMEL(12),=C'Perf Name - '                                    
         MVC   P2PNAME(L'SVW4NFST),SVW4NFST                                     
         XC    WORK,WORK                                                        
         MVC   WORK(L'P2PNAME),P2PNAME                                          
*  IF PAID TO CORP DIRECTLY PERF NAME=CORP NAME                                 
         CLC   SVPFSSN,SVW4FID                                                  
         BNE   *+10                                                             
         MVC   WORK(L'P2PNAME),P2CNAME                                          
         CLC   =C'203330576',SVPFSSN                                            
         BNE   *+8                                                              
         B     *+4                                                              
         GOTO1 SQUASHER,DMCB,WORK,60                                            
         LA    R1,P                                                             
         L     R0,DMCB+4                                                        
         LA    RE,WORK                                                          
         AR    RE,R0                                                            
         OC    SVW4NFST(L'TAW4NAM1),SVW4NFST                                    
         BZ    *+8                                                              
         AHI   RE,1                                                             
         MVC   0(L'TAW4NAM2,RE),SVW4NLST                                        
         MVC   P2PNAME,WORK                                                     
PUTSRT26 MVC   SRTPREC2,P                                                       
                                                                                
         LA    R1,P                                                             
         XC    P,P                                                              
         USING P3D,R1                                                           
         MVC   P3CADDRL(15),=C'Corp Address - '                                 
         MVC   P3CADDR(L'SVCPADD1),SVCPADD1                                     
         MVC   P3PFADDL(15),=C'Perf Address - '                                 
         MVC   P3PFADDR(L'SVPFADD1),SVPFADD1                                    
PUTSRT36 MVC   SRTPREC3,P                                                       
                                                                                
         LA    R1,P                                                             
         XC    P,P                                                              
         USING P4D,R1                                                           
         MVC   P4CADDR(L'SVCPADD2),SVCPADD2                                     
         MVC   P4PFADDR(L'SVPFADD2),SVPFADD2                                    
PUTSRT46 MVC   SRTPREC4,P                                                       
                                                                                
         LA    R1,P                                                             
         XC    P,P                                                              
         USING P5D,R1                                                           
         MVC   P5CADDR(L'SVCPADD3),SVCPADD3                                     
         MVC   P5PFADDR(L'SVPFADD3),SVPFADD3                                    
PUTSRT56 MVC   SRTPREC5,P                                                       
* CITY, STATE, ZIP                                                              
         LA    R1,P                                                             
         XC    P,P                                                              
         USING P6D,R1                                                           
         MVC   P6PFADDR(L'SVPFCSZ),SVPFCSZ                                      
         OC    P6PFADDR,SPACES                                                  
         MVC   WORK(L'SVPFCSZ),P6PFADDR                                         
         GOTO1 SQUASHER,DMCB,WORK,45                                            
         LA    R1,P                                                             
         XC    P6PFADDR(L'SVPFCSZ),P6PFADDR                                     
         MVC   P6PFADDR(L'SVPFCSZ),WORK                                         
         MVC   WORK(L'SVCPCSZ),SVCPCSZ                                          
         GOTO1 SQUASHER,DMCB,WORK,45                                            
         LA    R1,P                                                             
         MVC   P6CADDR,WORK                                                     
PUTSRT66 MVC   SRTPREC6,P                                                       
         MVC   SRTCSSN,SVW4FID        1099 REPORT/BLD SORT BY                   
         MVC   SRTPSSN,SVPFSSN        W4 TYPE/CORP ID ORDER                     
         MVC   SRTINV,SVINV                                                     
         MVC   SRTW4FLT,SVW4FILT                                                
*                                                                               
         CLI   RECNUM,TNNRPT          1099 REPORT CONTINUE TO SORT              
         BNE   *+16                   BY CORP ID ORDER                          
         MVC   SRTW4FLT2,SVW4FILT                                               
         MVC   SRTCSSN2,SVW4FID                                                 
*                                                                               
*&&DO                                                                           
*  CHECK FOR FILTER L IF THE CORP WAS PAID DIRECTLY                             
*  WHICH CORP SSN AND PERFORMER SSN ARE THE SAME AS THIS POINT                  
  ADD CODE TO CHECK IF THE CORP HAS AN ATTACHED INDIVIDUAL                      
  IF SO THEN USE THE INDIVIDUAL PID - IF THE CORP DOES NOT HAVE                 
  INDIVIDUAL ATTACHED THEN USE THE CORP ID                                      
*                                                                               
         CLI   SVW4FILT,C'L'       FOR FILTER L IF WE ARE PAYING THE            
         BNE   PUTSRT80            CORP DIRECTLY- DO NOT SET THE PF SSN         
         CLI   RECNUM,TNNBLD       WE WILL CHECK FOR THIS IN PUTOUT             
         BNE   PUTSRT80            AND ACCOUNT FOR THIS AS A SEPERATE           
         CLC   SRTCSSN,SRTPSSN     LINE                                         
         BNE   PUTSRT80            IF CSSN=PSSN =CORP DIRECT PAYMENT            
         XC    SRTPSSN,SRTPSSN     CLEAR OUT PID SO WE CAN TREAT IT             
*                                  AS A SEPERATE LINE                           
*&&                                                                             
PUTSRT80 BRAS  RE,SORTPUT          ADD RECORD TO SORT                           
                                                                                
                                                                                
         DROP  R1,R3                                                            
         J     XIT                                                              
*********************************************************************           
* GET RECORDS FROM SORTER AND SPOOL                                             
*********************************************************************           
PUTOUT   NTR1                                                                   
         XC    SVSRTKEY,SVSRTKEY                                                
         XC    SVPDGRS,SVPDGRS                                                  
         XC    SVPDREXP,SVPDREXP                                                
         XC    STGROSS,STGROSS                                                  
         XC    TOTGROSS,TOTGROSS                                                
POUT200  OC    RECCOUNT,RECCOUNT                                                
***      BZ    POUT490                                                          
         JZ    XIT                                                              
         MVI   NOMOREF,C'N'                                                     
         GOTO1 VSORTER,DMCB,=C'GET' GET A SORT RECORD                           
         ICM   R2,15,DMCB+4        POINT TO RETURNED RECORD                     
         BNZ   POUT204                                                          
         MVI   NOMOREF,C'Y'                                                     
         B     POUT340                                                          
POUT204  STCM  R2,15,ASRTREC                                                    
         USING SRTPRECD,R2                                                      
* FOR 1099 REPORT WE SORT BY CORPID FIRST, FOR 1099BLD AND 1099 REPORT          
* WE SORT BY W4TYPE/ CORPID ORDER..RE ORDET THE FIELDS SO                       
* WE ARE ALWAYS COMPARING THE SAME FIELDS                                       
*                                                                               
         CLI   RECNUM,TNNRPT                                                    
         BNE   POUT210                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(L'SRTW4FLT2),SRTW4FLT2                                      
         MVC   WORK+L'SRTW4FLT2(L'SRTCSSN2),SRTCSSN2                            
         MVC   SRTCSSN,WORK+L'SRTW4FLT2                                         
         MVC   SRTW4FLT,WORK                                                    
                                                                                
                                                                                
* WHEN PROCESSING CORP ID LINES WHICH WE HAVE SPOOLED ALREADY                   
* WE CLEAR OUT THE CORP FIELDS IN THE OUTPUT LINE                               
* ONLY DISPLAY CORP INFORMATION ONCE                                            
POUT210  CLC   SRTCSSN,SVP1CID                                                  
         BNE   POUT220                                                          
*  CLEAR OUT THE CORP FIELDS                                                    
         CLI   RECNUM,TNNBLD       1099BLD AND 1099PRT ONLY HAVE 1              
         BE    POUT220             LINE PER CORP/PSSN COMBO                     
         CLI   RECNUM,TNNFCP       FOREIGN CORP ONLY HAVE 1                     
         BE    POUT220             LINE PER CORP/PSSN COMBO                     
         CLI   RECNUM,TNNPRT       SO WE SHOULD ALWAYS PRINT THE FIELD          
         BE    POUT220             LABELS                                       
*                                                                               
                                                                                
         LA    RE,SRTPREC1                                                      
         XC    0(P1CORPL,RE),0(RE)                                              
         LA    RE,SRTPREC2                                                      
         XC    0(P2CORPL,RE),0(RE)                                              
         LA    RE,SRTPREC3                                                      
         XC    0(P3CORPL,RE),0(RE)                                              
*                                                                               
         LA    RE,SRTPREC4                                                      
         XC    0(P4CORPL,RE),0(RE)                                              
         LA    RE,SRTPREC5                                                      
         XC    0(P5CORPL,RE),0(RE)                                              
         LA    RE,SRTPREC6                                                      
         XC    0(P6CORPL,RE),0(RE)                                              
*                                                                               
* CLEAR CERTAIN PERFORMER FIELDS IF PAID THROUGH CORP                           
* SVPSSN WOULD BE EMPTY IN THIS CASE                                            
POUT220  DS    0C                                                               
         CLC   SRTPSSN,SRTCSSN                                                  
         BNE   POUT280                                                          
         LA    RE,SRTPREC1                                                      
         USING P1D,RE                                                           
         MVC   SVW4FILT,P1W4FILT                                                
POUT230  XC    P1PSSN,P1PSSN                                                    
         LA    RE,SRTPREC2                                                      
         USING P2D,RE                                                           
         XC    P2PNAME,P2PNAME                                                  
         LA    RE,SRTPREC3                                                      
         USING P3D,RE                                                           
         XC    P3PFADDR,P3PFADDR                                                
         LA    RE,SRTPREC4                                                      
         USING P4D,RE                                                           
         XC    P4PFADDR,P4PFADDR                                                
         LA    RE,SRTPREC5                                                      
         USING P5D,RE                                                           
         XC    P5PFADDR,P5PFADDR                                                
         LA    RE,SRTPREC6                                                      
         USING P6D,RE                                                           
         XC    P6PFADDR,P6PFADDR                                                
         DROP  RE                                                               
                                                                                
POUT280  OC    SVSRTKEY,SVSRTKEY                                                
         BZ    POUT320                                                          
         CLI   RECNUM,TNNFCP      ONLY SORT UNDER CORP/TYPE                     
         BE    *+8                FOR FOREIGN CORP                              
         CLI   RECNUM,TNNBLD      ONLY SORT UNDER CORP/TYPE                     
         BNE   POUT285            FOR 1099 BLD DEFAULT                          
         LA    RE,SRTPREC1                                                      
         USING P1D,RE                                                           
* REMOVE NEXT TWO LINES WE ARE NOT SEPERATING THE CORP AND PERFORMER            
* CEHCKS INTO SEPERATE LINES ANY AS PER DANIELLE                                
* THEY WILL MANUALLY ADD THE 1099 RECORD                                        
*****    CLI   P1W4FILT,C'L'      FOR 1099BLD AND FILTER GROUP                  
*****    BE    POUT285            BY CID,PSSN,TYPE                              
         B     POUT290                                                          
         DROP  RE                                                               
POUT285  CLC   SVSRTKEY(SRTKEYL2),SRTREC   SAME CORP SSN, PERF SSN              
         BNE   POUT340                     FOR 1099                             
         B     POUT320                                                          
POUT290  CLC   SVSRTKEY(SRTKEYL1),SRTREC   SAME CORP SSN, TYPE                  
         BNE   POUT340                     FOR 1099BLD                          
         LA    RE,SRTPREC1                                                      
         USING P1D,RE                                                           
         MVC   SVW4FILT,P1W4FILT                                                
         DROP  RE                                                               
POUT320  ZICM  RE,SVPDGRS,(15)                                                  
         LA    R3,SRTPREC1                                                      
         USING P1D,R3                                                           
         ZICM  RF,P1GROSSB,(15)                                                 
         AR    RE,RF                                                            
         STCM  RE,15,SVPDGRS                                                    
         ZICM  RE,SVPDREXP,(15)                                                 
         ZICM  RF,P1REXPB,(15)                                                  
         AR    RE,RF                                                            
         STCM  RE,15,SVPDREXP                                                   
         MVC   SVSRTKEY,SRTREC                                                  
         LA    R0,SVSRTREC                                                      
         LA    R1,1000                                                          
         LA    RE,SRTREC                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         B     POUT200            GET NEXT SORT RECORD                          
         DROP  R3                                                               
                                                                                
*  WE HAVE A CHANGE IN SORT KEY - PUT OUT THE PREVIOUS RECORD                   
                                                                                
POUT340  MVC   SVSRTKEY,SRTREC                                                  
         MVI   PRINTF,C'N'                                                      
         OC    SVPDGRS,SVPDGRS     SKIP ZERO GROSSS WAGE LINES                  
         BZ    POUT490                                                          
         ZICM  RE,SVPDGRS,(15)                                                  
*                                                                               
***      CLI   RECNUM,TNNFCP                                                    
***      BE    *+8                                                              
         CLI   RECNUM,TNNBLD       1099 BLD DONT INCLUIDE <600                  
         BNE   *+12                                                             
         C     RE,=F'60000'        SKIP GROSS <600                              
         BL    POUT490                                                          
         C     RE,=F'0'            SKIP NEG GROSS WAGE LINES                    
         BL    POUT490                                                          
*                                                                               
         DROP  R2                                                               
         MVI   PRINTF,C'Y'                                                      
*                                                                               
         ZICM  RE,STGROSS,(15)                                                  
         ZICM  RF,SVPDGRS,(15)                                                  
         AR    RE,RF                                                            
         STCM  RE,15,STGROSS                                                    
*                                                                               
* FIGURE OUT WHICH ADDRESS LINE TO PUT CITY, STATE, ZIP CODE                    
* FOR CORP AND PERFORMER                                                        
         LA    R3,SVSRTREC                                                      
         USING SRTPRECD,R3                                                      
POUT360  OC    P4CADDR-P4D+SRTPREC4(L'P4CADDR),P4CADDR-P4D+SRTPREC4             
         BNZ   POUT362                                                          
         MVC   P4CADDR-P4D+SRTPREC4(L'P6CADDR),P6CADDR-P6D+SRTPREC6             
         XC    P6CADDR-P6D+SRTPREC6(L'P6CADDR),P6CADDR-P6D+SRTPREC6             
         B     POUT370                                                          
POUT362  OC    P5CADDR-P5D+SRTPREC5(L'P5CADDR),P5CADDR-P5D+SRTPREC5             
         BNZ   POUT370                                                          
         MVC   P5CADDR-P5D+SRTPREC5(L'P6CADDR),P6CADDR-P6D+SRTPREC6             
         XC    P6CADDR-P6D+SRTPREC6(L'P6CADDR),P6CADDR-P6D+SRTPREC6             
         B     POUT370                                                          
*                                                                               
                                                                                
* FIGURE OUT WHICH ADDRESS LINE TO PUT CITY, STATE, ZIP CODE                    
* FOR CORP AND PERFORMER                                                        
POUT370  OC    P1PSSN-P1D+SRTPREC1(L'P1PSSN),P1PSSN-P1D+SRTPREC1                
         BZ    POUT380                                                          
         OC    P4PFADDR-P4D+SRTPREC4(L'P4PFADDR),P4PFADDR-P4D+SRTPREC4          
         BNZ   POUT372                                                          
         MVC   P4PFADDR-P4D+SRTPREC4(L'P4PFADDR),P6PFADDR-P6D+SRTPREC6          
         XC    P6PFADDR-P6D+SRTPREC6(L'P6PFADDR),P6PFADDR-P6D+SRTPREC6          
         B     POUT380                                                          
POUT372  OC    P5PFADDR-P5D+SRTPREC5(L'P5PFADDR),P5PFADDR-P5D+SRTPREC5          
         BNZ   POUT380                                                          
         MVC   P5PFADDR-P5D+SRTPREC5(L'P6PFADDR),P6PFADDR-P6D+SRTPREC6          
         XC    P6PFADDR-P6D+SRTPREC6(L'P6PFADDR),P6PFADDR-P6D+SRTPREC6          
         DROP  R3                                                               
*                                                                               
POUT380  LA    R2,SVSRTREC                                                      
         USING SRTPRECD,R2                                                      
         LA    R3,SRTPREC1                                                      
         USING P1D,R3                                                           
         MVC   P,SPACES                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         MVC   SVW4FID,P1PSSN-P1D+SRTPREC1                                      
         MVC   SVP1CID,SRTCSSN                                                  
         EDIT  (B4,SVPDGRS),(11,P1GROSS),2,ALIGN=RIGHT,ZERO=NOBLANK             
                                                                                
         EDIT  (B4,SVPDREXP),(11,P1REXP),2,ALIGN=RIGHT,ZERO=NOBLANK             
                                                                                
         DROP  R3                                                               
*                                                                               
         CLI   RECNUM,TNNFCP     ONLY WRITE RECORD TO CHKFIL FOR                
         BE    *+8               1099BLD                                        
         CLI   RECNUM,TNNBLD     ONLY WRITE RECORD TO CHKFIL FOR                
         BNE   POUT400           1099BLD                                        
         LA    RE,SRTPREC1                                                      
         USING P1D,RE                                                           
*                                                                               
*  FOR FILTER L SHOW PERFORMER INFO IN CORP SECTION IN 1099BLD                  
         CLI   P1W4FILT,C'L'                                                    
         BNE   POUT390                                                          
         MVC   P1CID,P1PSSN                                                     
         LA    RE,SRTPREC2                                                      
         USING P2D,RE                                                           
         MVC   P2CNAME,P2PNAME                                                  
         LA    RE,SRTPREC3                                                      
         USING P3D,RE                                                           
         MVC   P3CADDR,P3PFADDR                                                 
         LA    RE,SRTPREC4                                                      
         USING P4D,RE                                                           
         MVC   P4CADDR,P4PFADDR                                                 
         LA    RE,SRTPREC5                                                      
         USING P5D,RE                                                           
         MVC   P5CADDR,P5PFADDR                                                 
         LA    RE,SRTPREC6                                                      
         USING P6D,RE                                                           
         MVC   P6CADDR,P6PFADDR                                                 
         DROP  RE                                                               
*                                                                               
*                                                                               
POUT390  CLI   RECNUM,TNNFCP     ONLY WRITE RECORD TO CHKFIL FOR                
         BE    *+8               1099BLD                                        
         BRAS  RE,ADD1099                                                       
         LA    RE,SRTPREC1                                                      
         USING P1D,RE                                                           
         XC    P1PSSN,P1PSSN                                                    
         XC    P1PSSNL,P1PSSNL                                                  
         XC    P1REXP,P1REXP                                                    
         LA    RE,SRTPREC2                                                      
         USING P2D,RE                                                           
         XC    P2PNAME,P2PNAME                                                  
         XC    P2PNAMEL,P2PNAMEL                                                
         LA    RE,SRTPREC3                                                      
         USING P3D,RE                                                           
         XC    P3PFADDR,P3PFADDR                                                
         XC    P3PFADDL,P3PFADDL                                                
         LA    RE,SRTPREC4                                                      
         USING P4D,RE                                                           
         XC    P4PFADDR,P4PFADDR                                                
         XC    P4PFADDL,P4PFADDL                                                
         LA    RE,SRTPREC5                                                      
         USING P5D,RE                                                           
         XC    P5PFADDR,P5PFADDR                                                
         XC    P5PFADDL,P5PFADDL                                                
         LA    RE,SRTPREC6                                                      
         USING P6D,RE                                                           
         XC    P6PFADDR,P6PFADDR                                                
         XC    P6PFADDL,P6PFADDL                                                
         DROP  RE                                                               
*                                                                               
POUT400  MVC   P,SRTPREC1                                                       
**       CLI   RECNUM,TNNFCP                                                    
**       BNE   *+10                                                             
**       XC    P1W4FILT-P1D+SRTPREC1(L'P1W4FILT),P1W4FILT-P1D+SRTPREC1          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P,SRTPREC2                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P,SRTPREC3                                                       
         OC    SRTPREC3,SRTPREC3                                                
         BZ    POUT440                                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
POUT440  MVC   P,SRTPREC4                                                       
         OC    SRTPREC4,SRTPREC4                                                
         BZ    POUT450                                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
POUT450  MVC   P,SRTPREC5                                                       
         OC    SRTPREC5,SRTPREC5                                                
         BZ    POUT480                                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
POUT480  DS    0H                                                               
         MVC   P,SRTPREC6                                                       
         OC    SRTPREC6,SRTPREC6                                                
         BZ    POUT490                                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
POUT490  DS    0C                                                               
* SEE IF WE HAVE A CHANGE ON THE FILTER STATUS                                  
* IF SO PRINT OUT A SUMMARY LINE                                                
*                                                                               
         CLI   RECNUM,TNNRPT      1099 REPORT DONT PRINT TOTAL LINES            
         BE    POUT500                                                          
**       CLC   SVSRTKEY(L'SRTW4FLT),SRTREC                                      
**       BE    POUT500                                                          
*                                                                               
         CLI   NOMOREF,C'Y'                                                     
         BE    POUT496                                                          
         CLI   PRINTF,C'N'                                                      
         BE    POUT500                                                          
         L     RE,ASRTREC                                                       
         CLC   SVSRTREC(L'SRTW4FLT),0(RE)                                       
         BE    POUT500                                                          
*                                                                               
POUT496  XC    P,P                                                              
         LA    R3,P                                                             
         USING P1D,R3                                                           
         MVC   P1CIDL(17),=C'Total Gross Wages'                                 
         EDIT  (B4,STGROSS),(11,P1GROSS),2,ALIGN=RIGHT,ZERO=NOBLANK             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         ZICM  RE,STGROSS,(15)                                                  
         ZICM  RF,TOTGROSS,(15)                                                 
         AR    RE,RF                                                            
         STCM  RE,15,TOTGROSS                                                   
         XC    STGROSS,STGROSS                                                  
*                                                                               
*                                                                               
POUT500  XC    SVPDGRS,SVPDGRS                                                  
         XC    SVPDREXP,SVPDREXP                                                
         ZICM  R2,ASRTREC,(15)                                                  
         CLI   NOMOREF,C'Y'                                                     
         BE    PUTOUTX                                                          
         B     POUT210                                                          
PUTOUTX  CLI   RECNUM,TNNRPT      1099 REPORT DONT PRINT TOTAL LINE             
         JE    XIT                                                              
         CLI   RECNUM,TNNFCP      FOREIGN CORP DONT NEED GRAND TOTAL            
         JE    XIT                                                              
         XC    P,P                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    P,P                                                              
         LA    R3,P                                                             
         USING P1D,R3                                                           
         MVC   P1CIDL(11),=C'Grand Total'                                       
         EDIT  (B4,TOTGROSS),(11,P1GROSS),2,ALIGN=RIGHT,ZERO=NOBLANK            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
         DROP  R2                                                               
***********************************************************************         
* WRITE OUT 1099 RECORDS TO CHKFIL                                              
***********************************************************************         
ADD1099  NTR1                                                                   
         L     R0,AIO                                                           
         LHI   R1,2000                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR BUFFER FOR ACCUMULATING RECDS          
*                                                                               
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVI   RDUPDATE,C'Y'                                                    
         NI    GENSTAT1,ALL-RDUPAPPL                                            
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING TL99D,R3                                                         
         MVI   TL99CD,TL99CDQ                                                   
         MVI   TL99SCD,TL99SCDQ                                                 
         MVC   TL99YEAR,YEAR                                                    
         MVI   TL99CUR,C'U'                                                     
         MVC   TL99EMP,TIFEMP                                                   
         LA    RF,SVSRTREC                                                      
         USING SRTPRECD,RF                                                      
         MVC   TL99FID,(P1CID-P1D)+SRTPREC1                                     
         MVC   TL99W4TY,(P1W4FILT-P1D)+SRTPREC1                                 
         DROP  RF                                                               
                                                                                
         GOTO1 HIGH                                                             
* IF WE FOUND KEY ALREADY DONT ADD IT AGAIN                                     
         DROP  R3                                                               
                                                                                
         CLC   KEY(L'TL99KEY),KEYSAVE                                           
         BE    ADD99_10                                                         
                                                                                
         L     RE,AIO                                                           
         MVC   0(L'KEYSAVE,RE),KEYSAVE                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    RE,ELEM                                                          
         USING TA99D,RE                                                         
         MVI   TA99EL,TA99ELQ                                                   
         MVI   TA99LEN,TA99LNQ                                                  
         MVC   TA99EARN,SVPDGRS                                                 
         MVC   TA99REXP,SVPDREXP                                                
         DROP  RE                                                               
         GOTO1 ADDELEM                                                          
         GOTO1 ACTVIN,DMCB,0       ADD ACTIVITY ELEMENT                         
         GOTO1 ADDREC                                                           
*                                                                               
         J     ADD1099X                                                         
ADD99_10 DS    0C                                                               
         GOTO1 GETREC                                                           
         MVI   ELCODE,TA99ELQ                                                   
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    RE,ELEM                                                          
         USING TA99D,RE                                                         
         MVI   TA99EL,TA99ELQ                                                   
         MVI   TA99LEN,TA99LNQ                                                  
         MVC   TA99EARN,SVPDGRS                                                 
         MVC   TA99REXP,SVPDREXP                                                
         DROP  RE                                                               
         GOTO1 ADDELEM                                                          
         GOTO1 ACTVIN,DMCB,0       ADD ACTIVITY ELEMENT                         
         GOTO1 PUTREC                                                           
*                                                                               
ADD1099X MVC   SYSFIL,=CL8'TALFIL'                                              
         MVC   SYSDIR,=CL8'TALDIR'                                              
         J     XIT                                                              
***********************************************************************         
*        READ  W4 RECORD  FOR A GIVEN SSN                                       
*        INPUT = PVCKSSN                                                        
***********************************************************************         
READW4   NTR1  BASE=*,LABEL=*                                                   
         LA    R3,KEY              READ W4 RECORD                               
         USING TLW4D,R3                                                         
         XC    TLW4KEY,TLW4KEY                                                  
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,PVCKSSN                                                  
         MVC   SVPFSSN,PVCKSSN                                                  
         MVC   SYSFIL,=CL8'TALFIL'                                              
         MVC   SYSDIR,=CL8'TALDIR'                                              
         GOTO1 HIGH                                                             
         JE    READW410                                                         
         DC    H'0'                                                             
READW410 CLC   KEY(L'TLW4KEY),KEYSAVE                                           
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,TAW4ELQ                                                   
         USING TAW4D,R4                                                         
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVW4NLST,TAW4CRPN                                                
         MVC   SVW4NFST,TAW4NAM1                                                
         MVC   SVW4TYPE,TAW4TYPE                                                
         L     R4,AIO                 CORP ADDRESS                              
         USING TAA2D,R4                                                         
         MVI   ELCODE,TAA2ELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVPFADD1,TAA2ADD1                                                
         MVC   SVPFADD2,TAA2ADD2                                                
         MVC   SVPFADD3,TAA2ADD3                                                
         MVC   SVPFCITY,TAA2CITY                                                
         MVC   SVPFCTRY,TAA2CTRY                                                
         MVC   SVPFST,TAA2ST                                                    
         MVC   SVPFZIP(2),=C', '                                                
         MVC   SVPFZIP+2(L'TAA2ZIP),TAA2ZIP                                     
READW4X  J     XIT                                                              
         DROP  R3,R4                                                            
*                                                                               
***********************************************************************         
*  READ CHKFIL 1099 RECORDS CREATED FROM 1099 BUILD                             
***********************************************************************         
READ1099 NTR1  BASE=*,LABEL=*                                                   
         L     R1,=A(HEDSPEC2)                                                  
         ST    R1,SPECS                                                         
         MVI   SORTFRST,C'Y'                                                    
READ1_05 MVC   SVCONT3,=PL8'0'                                                  
         MVC   SVCONT7,=PL8'0'                                                  
         XC    BRECNUM,BRECNUM                                                  
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         XC    KEY,KEY                                                          
         XC    PREVKEY,PREVKEY                                                  
         LA    RE,KEY                                                           
         USING TL99D,RE                                                         
         MVI   TL99CD,TL99CDQ                                                   
         MVI   TL99SCD,TL99SCDQ                                                 
         MVC   TL99YEAR,YEAR                                                    
         MVI   TL99CUR,C'U'                                                     
         MVC   TL99EMP,TIFEMP                                                   
         DROP  RE                                                               
         MVI   RDUPDATE,C'Y'         SET TO READ FOR UPDATE                     
         NI    GENSTAT1,ALL-RDUPAPPL                                            
**       NI    GENSTAT1,ALL-RDUPAPPL                                            
                                                                                
         GOTO1 HIGH                                                             
         CLI   ALLEMP,C'Y'                                                      
         BNE   READ1_07                                                         
         CLC   =C'PP ',TIFEMP                                                   
         BE    READ1_09                                                         
READ1_07 BRAS  RE,PUTRECT                                                       
READ1_09 BRAS  RE,PUTRECA                                                       
         J     READ1_20                                                         
READ1_10 GOTO1 SEQ                                                              
READ1_20 CLC   KEY(TL99FID-TL99D),KEYSAVE                                       
         JNE   READ99X                                                          
         CLC   KEY(TL99CDTE-TL99D),PREVKEY   ONLY GET LATEST 1099 FOR           
         BE    READ1_10                      CORP                               
         OC    KEY+(TL99FID-TL99D)(L'TL99FID),KEY+(TL99FID-TL99D)               
         BZ    READ1_10                                                         
         MVC   PREVKEY,KEY                                                      
         GOTO1 GETREC                                                           
****     BRAS  RE,PUTRECB                                                       
*                                                                               
* ADD PRINT DATE ELEMENT                                                        
         MVI   ELCODE,TAWSELQ                                                   
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    RE,ELEM                                                          
         USING TAWSD,RE                                                         
         MVI   TAWSEL,TAWSELQ                                                   
         MVI   TAWSLEN,TAWSLNQ                                                  
         MVC   TAWSDPRT,TGTODAY1                                                
         DROP  RE                                                               
         GOTO1 ADDELEM                                                          
         GOTO1 PUTREC                                                           
*                                                                               
         BRAS  RE,PUTRECB                                                       
         J     READ1_10                                                         
                                                                                
READ99X  BRAS  RE,PUTRECC                                                       
         CLI   ALLEMP,C'Y'                                                      
         BNE   READ99Y                                                          
         CLC   =C'PP ',TIFEMP                                                   
         BE    READ99Y                                                          
         MVC   WORK(3),=C'PP '                                                  
         MVC   TIFEMP,=C'PP '                                                   
         BRAS  RE,EMPINFO                                                       
                                                                                
         B     READ1_05                                                         
                                                                                
READ99Y  BRAS  RE,PUTRECF                                                       
         BRAS  RE,PUTBOUT                                                       
         J     XIT                                                              
         LTORG                                                                  
         DROP  RB                                                               
***********************************************************************         
*           TRANSMITTER RECORD                                                  
***********************************************************************         
*                                                                               
*                                                                               
PUTRECT  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   RF,1                                                             
         ST    RF,SEQNUM                                                        
                                                                                
         LA    R0,SRTPREC                                                       
         LA    R1,1000                                                          
         LA    RE,SRTPREC                                                       
         XR    RF,RF                                                            
         ICM   RF,B'1000',=C' '                                                 
         MVCL  R0,RE                                                            
****     LA    R2,SRTPREC+L'REC_TSEQ                                            
* SORT HEADER =8 BYTE SEQUENCE NUMBER + 1 BYTE FOR W4TYPE FOR PAYEE REC         
         LA    R2,SRTPREC+L'REC_TSEQ+L'REC_BW4T                                 
         USING REC_TD,R2             TO SORT CORRECTLY                          
         MVI   REC_TID,REC_TIDQ                                                 
         MVC   REC_TYR,YEAR                                                     
         MVI   REC_TPYI,X'40'                                                   
         MVC   REC_TTIN,EMPTIN                                                  
         MVC   REC_TTCC,=C'09U84'                                               
         MVC   REC_TTNM(15),=C'Talent Partners'                                 
         MVC   REC_TCNM(L'EMPNAME),=CL36'TALENT PARTNERS'                       
         OC    REC_TCNM(L'EMPNAME),SPACES                                       
                                                                                
         MVC   REC_TADD(L'EMPADD1),EMPADD1                                      
         OC    REC_TADD,SPACES                                                  
         MVC   SCANLINE,SPACES                                                  
         MVC   SCANLINE(L'EMPADD2),EMPADD2                                      
*                                                                               
                                                                                
         USING SCAND,R3                                                         
         LA    R3,BLOCK                                                         
         GOTO1 SCANNER,DMCB,(=C'C',SCANLINE),(R3),=C',='                        
         CLI   4(R1),0                                                          
         JNE   *+6                                                              
         DC    H'00'                                                            
                                                                                
* CITY                                                                          
***      ZIC   R1,0(R3)                                                         
         CLI   32(R3),0                                                         
         BNE   PUTT28                                                           
         MVC   SCANLINE,SPACES                                                  
         MVC   SCANLINE(L'EMPADD3),EMPADD3                                      
         GOTO1 SCANNER,DMCB,(=C'C',SCANLINE),(R3),=C',='                        
         CLI   4(R1),0                                                          
         JNE   *+6                                                              
         DC    H'00'                                                            
         CLI   32(R3),0                                                         
         BE    PUTT60                                                           
PUTT28   ZIC   R1,0(R3)                                                         
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   REC_TCTY(0),12(R3)                                               
         OC    REC_TCTY,SPACES                                                  
         AHI   R3,32                                                            
*   GET STATE                                                                   
         ZIC   RE,0(R3)                                                         
         LA    R3,12(R3)                                                        
         LA    RF,REC_TSTA                                                      
PUTT30   CLI   0(R3),X'F0'                                                      
         JNL   PUTT40                                                           
         CLI   0(R3),X'40'                                                      
         BNH   PUTT35                                                           
         MVC   0(1,RF),0(R3)                                                    
         AHI   RF,1                                                             
PUTT35   AHI   R3,1                                                             
         JCT   RE,PUTT30                                                        
*   GET ZIP                                                                     
PUTT40   LA    RF,REC_TZIP                                                      
PUTT50   MVC   0(1,RF),0(R3)                                                    
         AHI   R3,1                                                             
         AHI   RF,1                                                             
         JCT   RE,PUTT50                                                        
         OC    REC_TZIP,SPACES                                                  
*                                                                               
PUTT60   MVC   REC_TCNN(15),=C'Julie Hegelmann'                                 
         MVC   REC_TTEL(10),=C'3129237900'                                      
         MVC   REC_TEML(29),=C'JHegelmann@Talentpartners.com'                   
                                                                                
         MVC   REC_TSEQ,=C'00000001'                                            
         MVC   SRTPREC(L'REC_TSEQ),REC_TSEQ                                     
                                                                                
         MVI   REC_TVEN,C'I'                                                    
         MVC   SRTPREC(2),=X'0000'                                              
         BRAS  RE,SORTPUT          ADD RECORD TO SORT                           
         J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*           RECORD TYPE A                                                       
***********************************************************************         
*                                                                               
PUTRECA  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     RF,SEQNUM                                                        
         AHI   RF,1                                                             
         ST    RF,SEQNUM                                                        
                                                                                
         LA    R0,SRTPREC                                                       
         LA    R1,750                                                           
         LA    RE,SRTPREC                                                       
         XR    RF,RF                                                            
         ICM   RF,B'1000',=C' '                                                 
         MVCL  R0,RE                                                            
*                                                                               
******   LA    R2,SRTPREC+L'REC_ASEQ                                            
* HEADER = 8 BYTE SEQUENCE NUMBER + W4 TYPE ONLY FOR RECTYPE B                  
         LA    R2,SRTPREC+L'REC_ASEQ+L'REC_BW4T                                 
*                                                                               
         USING REC_AD,R2                                                        
                                                                                
         MVI   REC_AID,REC_AIDQ                                                 
         MVC   REC_AYR,YEAR                                                     
         MVC   REC_ATIN,EMPTIN                                                  
         MVC   REC_ATYR,=C'A '                                                  
         MVI   REC_ATAI,C'0'                                                    
         MVC   REC_ATEL(10),=C'3129237903'                                      
         OC    REC_ATEL,SPACES                                                  
                                                                                
*                                                                               
         MVC   REC_AFPN(L'EMPNAME),EMPNAME                                      
         OC    REC_AFPN,SPACES                                                  
*---------------------------------------------------------------------          
* AS PER JOHN BASSETT OF EXTREME REACH (2016 DEC 21)                            
* HARDCODE THE ADDRESS INSTEAD OF USING THE EMPLOYER RECORD'S ADDRESS           
*---------------------------------------------------------------------          
*        MVC   REC_AADD(L'EMPADD1),EMPADD1                                      
*        OC    REC_AADD,SPACES                                                  
         MVC   REC_AADD,=CL40'111 W JACKSON BLVD STE 1525'                      
         MVC   REC_ACTY,=CL40'CHICAGO'                                          
         MVC   REC_ASTA,=C'IL'                                                  
         MVC   REC_AZIP,=CL9'60604'                                             
         MVC   SCANLINE,SPACES                                                  
         MVC   SCANLINE(L'EMPADD2),EMPADD2                                      
         EDIT  (B4,SEQNUM),(8,REC_ASEQ),ALIGN=RIGHT,ZERO=NOBLANK,      +        
               ,FILL=0                                                          
         OC    REC_AACD,SPACES                                                  
         MVC   REC_AACD(16),=C'123456789ABCDE  '                                
*                                                                               
                                                                                
         USING SCAND,R3                                                         
         LA    R3,BLOCK                                                         
         GOTO1 SCANNER,DMCB,(=C'C',SCANLINE),(R3),=C',='                        
         CLI   4(R1),0                                                          
         JNE   *+6                                                              
         DC    H'00'                                                            
*****                                                                           
         CLI   32(R3),0                                                         
         BNE   PUTA28                                                           
         MVC   SCANLINE,SPACES                                                  
         MVC   SCANLINE(L'EMPADD3),EMPADD3                                      
         GOTO1 SCANNER,DMCB,(=C'C',SCANLINE),(R3),=C',='                        
         CLI   4(R1),0                                                          
         JNE   *+6                                                              
         DC    H'00'                                                            
         CLI   32(R3),0                                                         
         JE    XIT                                                              
PUTA28   ZIC   R1,0(R3)                                                         
*&&DO                                                                           
*---------------------------------------------------------------------          
* AS PER JOHN BASSETT OF EXTREME REACH (2016 DEC 21)                            
* HARDCODE THE ADDRESS INSTEAD OF USING THE EMPLOYER RECORD'S ADDRESS           
* COMMENTING OUT THE CODE BELOW                                                 
*---------------------------------------------------------------------          
* CITY                                                                          
         ZIC   R1,0(R3)                                                         
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   REC_ACTY(0),12(R3)                                               
         OC    REC_ACTY,SPACES                                                  
         AHI   R3,32                                                            
*   GET STATE                                                                   
         ZIC   RE,0(R3)                                                         
         LA    R3,12(R3)                                                        
         LA    RF,REC_ASTA                                                      
PUTA30   CLI   0(R3),X'F0'                                                      
         JNL   PUTA40                                                           
         CLI   0(R3),X'40'                                                      
         BNH   PUTA35                                                           
         MVC   0(1,RF),0(R3)                                                    
         AHI   RF,1                                                             
PUTA35   AHI   R3,1                                                             
         JCT   RE,PUTA30                                                        
*   GET ZIP                                                                     
PUTA40   LA    RF,REC_AZIP                                                      
PUTA50   MVC   0(1,RF),0(R3)                                                    
         AHI   R3,1                                                             
         AHI   RF,1                                                             
         JCT   RE,PUTA50                                                        
         OC    REC_AZIP,SPACES                                                  
*&&                                                                             
         MVC   SRTPREC(2),TIFEMP                                                
         XI    SRTPREC,X'FF'                                                    
         MVC   SRTPREC+2(6),=C'000002'                                          
         BRAS  RE,SORTPUT          ADD RECORD TO SORT                           
         J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*           RECORD TYPE B                                                       
***********************************************************************         
*                                                                               
PUTRECB  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,BRECNUM                                                       
         AHI   RF,1                                                             
         ST    RF,BRECNUM                                                       
         L     RF,SEQNUM                                                        
         AHI   RF,1                                                             
         ST    RF,SEQNUM                                                        
                                                                                
         CHI   RF,203                                                           
         BNE   PUT_B05                                                          
         B     PUT_B05                                                          
                                                                                
PUT_B05  LA    R0,SRTPREC                                                       
         LA    R1,1000                                                          
         LA    RE,SRTPREC                                                       
         XR    RF,RF                                                            
         ICM   RF,B'1000',=C' '                                                 
         MVCL  R0,RE                                                            
****     LA    R2,SRTPREC+L'REC_BSEQ                                            
* HEADER = 8 BYTE SEQUENCE NUMBER + W4 TYPE ONLY FOR RECTYPE B                  
         LA    R2,SRTPREC+L'REC_BSEQ+L'REC_BW4T                                 
         USING REC_BD,R2                                                        
                                                                                
         MVI   REC_BID,REC_BIDQ                                                 
         MVC   REC_BYR,YEAR                                                     
         L     R4,AIO                                                           
         USING TL99D,R4                                                         
                                                                                
         CLI   TL99W4TY,C'F'       REGULAR 1099 PRINT                           
         JE    XIT                 DONT PROCESS W4 TYPE F                       
*                                                                               
                                                                                
PUT_B10  MVC   CKKEY,KEY                                                        
         MVC   CKKEYSAV,KEYSAVE                                                 
         MVC   PVCKSSN,TL99FID      INPUT SSN TOP READW4 ROUTINE                
         MVC   AIO,AIO3                                                         
         BRAS  RE,READW4                                                        
         MVC   KEY,CKKEY            RESET READ BACK                             
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         GOTO1 HIGH                                                             
         MVC   KEYSAVE,CKKEYSAV                                                 
         MVC   AIO,AIO1                                                         
         EDIT  (B4,SEQNUM),(8,REC_BSEQ),ALIGN=RIGHT,ZERO=NOBLANK,      +        
               ,FILL=0                                                          
*****    MVC   SRTPREC(L'REC_BSEQ),REC_BSEQ                                     
* SORT PAYEE RECORDS BY SEQUENCE NUMBER/W4TYPE IN HEADER                        
* REST OF SORT IS FOLLOW BY NATURAL CORP ID                                     
         MVC   SRTPREC(L'REC_BSEQ),=C'00000003'                                 
         MVC   SRTPREC+L'REC_BSEQ(1),TL99W4TY                                   
*                                                                               
                                                                                
         MVC   REC_BPNM(L'SVW4NLST),SVW4NLST                                    
*                                                                               
         CLI   SVW4TYPE,C'I'                                                    
         BNE   PUT_B18                                                          
         MVC   REC_BPNM,SPACES                                                  
         MVC   REC_BPNM(L'TAW4NAM1),SVW4NFST                                    
         MVC   REC_BPNM+L'TAW4NAM1+1(L'TAW4NAM2),SVW4NLST                       
         GOTO1 SQUASHER,DMCB,REC_BPNM,L'REC_BPNM                                
*                                                                               
PUT_B18  OC    REC_BPNM,SPACES                                                  
         MVC   SCANLINE,SPACES                                                  
         MVC   SCANLINE,SVPFADD1                                                
         MVC   SCANLINE+L'SVPFADD1,SVPFADD2                                     
         MVC   SCANLINE+L'SVPFADD1+L'SVPFADD2(L'SVPFADD3),SVPFADD3              
         MVC   REC_BAD1,SVPFADD1                                                
         MVC   REC_BAD2,SVPFADD2                                                
         MVC   REC_BAD3,SVPFADD3                                                
                                                                                
         OC    SCANLINE,SPACES                                                  
         MVC   WORK(L'SCANLINE),SCANLINE                                        
         GOTO1 SQUASHER,DMCB,WORK,L'REC_BPAD                                    
         MVC   REC_BPAD,WORK                                                    
*                                                                               
         MVC   REC_BCIT(L'SVPFCITY),SVPFCITY                                    
         MVC   REC_BSTA,SVPFST                                                  
         LA    RE,SVPFZIP                                                       
         MVC   REC_BZPC,SVPFZIP                                                 
         LA    RF,REC_BZIP                                                      
         LA    R0,L'SVPFZIP                                                     
PUT_B20  CLI   0(RE),X'F0'                                                      
         BL    PUT_B26                                                          
         MVC   0(1,RF),0(RE)                                                    
         AHI   RF,1                                                             
PUT_B26  AHI   RE,1                                                             
         BCT   R0,PUT_B20                                                       
*                                                                               
         MVC   REC_BPA1(REC_BPAX-REC_BPA1),=200C'0'                             
*                                                                               
                                                                                
         MVC   REC_BW4T,TL99W4TY                                                
         MVC   SV99W4TY,TL99W4TY                                                
         MVC   REC_BTIN,TL99FID                                                 
         CLI   TL99W4TY,TL99TYB                                                 
         BNE   PUT_B28                                                          
         LA    R3,REC_BPA3                                                      
         B     PUT_B30                                                          
*                                                                               
PUT_B28  CLI   TL99W4TY,TL99TYC                                                 
         BE    *+8                                                              
         CLI   TL99W4TY,TL99TYP                                                 
         BE    *+8                                                              
         CLI   TL99W4TY,TL99TYL                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,REC_BPA7                                                      
*                                                                               
PUT_B30  L     R4,AIO                                                           
         MVI   ELCODE,TA99ELQ                                                   
         BRAS  RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TA99D,R4                                                         
*                                                                               
         MVC   SV99EARN,TA99EARN                                                
         MVC   REC_BGRS,TA99EARN                                                
         EDIT  (B4,TA99EARN),(12,(R3)),ALIGN=RIGHT,ZERO=NOBLANK,       +        
               FILL=0                                                           
*                                                                               
                                                                                
         XC    DUB,DUB                                                          
         LA    RE,REC_BPA3                                                      
         CR    R3,RE                                                            
         BNE   PUT_B34                                                          
         MVC   DUB,SVCONT3                                                      
         OI    DUB+7,X'0C'                                                      
         LA    R1,SVCONT3                                                       
         B     PUT_B36                                                          
                                                                                
PUT_B34  LA    RE,REC_BPA7                                                      
         CR    R3,RE                                                            
         BNE   PUT_B36                                                          
         MVC   DUB,SVCONT7                                                      
         OI    DUB+7,X'0C'                                                      
         LA    R1,SVCONT7                                                       
         B     PUT_B36                                                          
         DC    H'0'                                                             
                                                                                
PUT_B36  ZICM  RF,TA99EARN,(15)                                                 
         CVD   RF,DUB2                                                          
         OI    DUB2+7,X'0C'                                                     
         AP    DUB,DUB2                                                         
         MVC   0(8,R1),DUB                                                      
                                                                                
         MVI   REC_BFCI,C' '                                                    
         CLC   SVPFCTRY(2),=C'US'                                               
         BE    *+8                                                              
         MVI   REC_BFCI,C'1'                                                    
*                                                                               
         MVC   SRTPREC(2),TIFEMP                                                
         XI    SRTPREC,X'FF'                                                    
         MVC   SRTPREC+2(6),=C'000005'                                          
         BRAS  RE,SORTPUT          ADD RECORD TO SORT                           
*******  BAS   RE,PRINT99                                                       
         J     XIT                                                              
         DROP  R4                                                               
*&&DO                                                                           
*                                                                               
PRINT99  NTR1                                                                   
*                                                                               
         XC    P,P                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    P,P                                                              
         LA    R3,P                                                             
         USING P1D,R3                                                           
         MVC   P1CIDL(10),=C'Corp ID - '                                        
         MVC   P1CID,REC_BTIN                                                   
         MVC   P1W4FILT,SV99W4TY                                                
         EDIT  (B4,SV99EARN),(11,P1GROSS),2,ALIGN=RIGHT,ZERO=NOBLANK            
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         XC    P,P                                                              
         LA    R3,P                                                             
         USING P2D,R3                                                           
         MVC   P2CNAMEL(12),=C'Corp Name - '                                    
         MVC   P2CNAME,REC_BPNM                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         XC    P,P                                                              
         LA    R3,P                                                             
         USING P3D,R3                                                           
         MVC   P3CADDRL(15),=C'Corp Address - '                                 
         MVC   P3CADDR(L'SVPFADD1),SVPFADD1                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
*                                                                               
         XC    P,P                                                              
         LA    R3,P                                                             
         USING P4D,R3                                                           
         LA    R2,P4CADDR                                                       
         OC    SVPFADD2,SVPFADD2                                                
         BZ    PRNT9901                                                         
         MVC   0(L'SVPFADD2,R2),SVPFADD2                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    P,P                                                              
         LA    R3,P                                                             
         USING P5D,R3                                                           
         LA    R2,P5CADDR                                                       
         OC    SVPFADD3,SVPFADD3                                                
         BZ    PRNT9901                                                         
         MVC   0(L'SVPFADD2,R3),SVPFADD3                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    P,P                                                              
         LA    R3,P                                                             
         USING P6D,R3                                                           
         LA    R2,P6CADDR                                                       
*                                                                               
PRNT9901 XC    P,P                                                              
         MVC   SCANLINE,SPACES                                                  
         LA    RE,SCANLINE                                                      
         MVC   0(L'SVPFCITY,RE),SVPFCITY                                        
         AHI   RE,L'REC_BCIT+1                                                  
         MVC   0(L'SVPFST,RE),SVPFST                                            
         AHI   RE,L'REC_BSTA                                                    
         MVC   0(L'SVPFZIP,RE),SVPFZIP                                          
         OC    SCANLINE,SPACES                                                  
         MVC   WORK(L'SCANLINE),SCANLINE                                        
         GOTO1 SQUASHER,DMCB,WORK,L'SCANLINE                                    
         MVC   0(L'P6CADDR,R2),WORK                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    P,P                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
*&&                                                                             
*******************************************************************             
*         RECORD C                                                              
*******************************************************************             
*                                                                               
PUTRECC  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     RF,SEQNUM                                                        
         AHI   RF,1                                                             
         ST    RF,SEQNUM                                                        
                                                                                
         LA    R0,SRTPREC                                                       
         LA    R1,1000                                                          
         LA    RE,SRTPREC                                                       
         XR    RF,RF                                                            
         ICM   RF,B'1000',=C' '                                                 
         MVCL  R0,RE                                                            
****     LA    R2,SRTPREC+L'REC_CSEQ                                            
* HEADER = 8 BYTE SEQUENCE NUMBER + W4 TYPE ONLY FOR RECTYPE B                  
         LA    R2,SRTPREC+L'REC_CSEQ+L'REC_BW4T                                 
         USING REC_CD,R2                                                        
                                                                                
         MVI   REC_CID,REC_CIDQ                                                 
*        ZICM  RE,SEQNUM,(15)                                                   
*        SHI   RE,2          MINUS T AND A RECORD TO GET NUM OF B RECS          
*        STCM  RE,15,SEQNUM                                                     
         EDIT  (B4,BRECNUM),(8,REC_CNPY),ALIGN=RIGHT,ZERO=NOBLANK,     +        
               ,FILL=0                                                          
*        ZICM  RE,SEQNUM,(15)                                                   
*        AHI   RE,3                                                             
*        STCM  RE,15,SEQNUM                                                     
         EDIT  (B4,SEQNUM),(8,REC_CSEQ),ALIGN=RIGHT,ZERO=NOBLANK,      +        
               ,FILL=0                                                          
         EDIT  (P8,SVCONT3),(18,REC_CCT3),ALIGN=RIGHT,ZERO=NOBLANK,    +        
               ,FILL=0                                                          
         EDIT  (P8,SVCONT7),(18,REC_CCT7),ALIGN=RIGHT,ZERO=NOBLANK,    +        
               ,FILL=0                                                          
         MVC   SRTPREC(L'REC_CSEQ),REC_CSEQ                                     
*   DONT KNOW WHY LEADING CHARACTER IS FUNNY AFTER EDIT                         
         CLI   REC_CCT3,X'F0'                                                   
         BNL   *+8                                                              
         MVI   REC_CCT3,X'F0'                                                   
         CLI   REC_CCT7,X'F0'                                                   
         BNL   *+8                                                              
         MVI   REC_CCT7,X'F0'                                                   
                                                                                
                                                                                
         MVC   SRTPREC(2),TIFEMP                                                
         XI    SRTPREC,X'FF'                                                    
         MVC   SRTPREC+2(6),=C'000020'                                          
         BRAS  RE,SORTPUT          ADD RECORD TO SORT                           
                                                                                
         L     RF,TOTNUM                                                        
         A     RF,BRECNUM                                                       
         ST    RF,TOTNUM                                                        
         J     XIT                                                              
*                                                                               
*********************************************************************           
*         RECORD F                                                              
*********************************************************************           
PUTRECF  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     RF,SEQNUM                                                        
         AHI   RF,1                                                             
         ST    RF,SEQNUM                                                        
                                                                                
         LA    R0,SRTPREC                                                       
         LA    R1,1000                                                          
         LA    RE,SRTPREC                                                       
         XR    RF,RF                                                            
         ICM   RF,B'1000',=C' '                                                 
         MVCL  R0,RE                                                            
***      LA    R2,SRTPREC+L'REC_FSEQ                                            
* HEADER = 8 BYTE SEQUENCE NUMBER + W4 TYPE ONLY FOR RECTYPE B                  
         LA    R2,SRTPREC+L'REC_FSEQ+L'REC_BW4T                                 
         USING REC_FD,R2                                                        
         MVC   REC_FNMA,=C'00000001'  ONLY 1 EMPLOYER RECORD                    
         CLI   ALLEMP,C'Y'                                                      
         JNE   *+8                                                              
         MVI   REC_FNMA+7,C'2'                                                  
         MVC   REC_FZ1,=21C'0'                                                  
                                                                                
         MVI   REC_FID,REC_FIDQ                                                 
*        ZICM  RE,SEQNUM,(15)                                                   
*        SHI   RE,3          MINUS T AND A RECORD TO GET NUM OF B RECS          
*        STCM  RE,15,SEQNUM                                                     
         EDIT  (B4,TOTNUM),(8,REC_FNMB),ALIGN=RIGHT,ZERO=NOBLANK,      +        
               ,FILL=0                                                          
*        ZICM  RE,SEQNUM,(15)                                                   
*        AHI   RE,4                                                             
*        STCM  RE,15,SEQNUM                                                     
         EDIT  (B4,SEQNUM),(8,REC_FSEQ),ALIGN=RIGHT,ZERO=NOBLANK,      +        
               ,FILL=0                                                          
         MVC   SRTPREC(L'REC_FSEQ),REC_FSEQ                                     
                                                                                
         MVC   SRTPREC(2),=X'FFFF'                                              
         MVC   SRTPREC+2(6),=C'000300'                                          
         BRAS  RE,SORTPUT          ADD RECORD TO SORT                           
         J     XIT                                                              
*********************************************************************           
* GET RECORDS FROM SORTER AND SPOOL                                             
*********************************************************************           
PUTBOUT  NTR1  BASE=*,LABEL=*                                                   
         XC    SV99W4TY,SV99W4TY                                                
         XC    SVSRTKEY,SVSRTKEY                                                
         XC    SVPDGRS,SVPDGRS                                                  
         XC    SVPDREXP,SVPDREXP                                                
         MVI   NOMOREF,C'N'                                                     
         XC    SEQNUM,SEQNUM                                                    
         MVC   BSEQNUM,=F'3'   SEQUENCE NUMBER FOR B RECORDS START AT 3         
PBOUT200 GOTO1 VSORTER,DMCB,=C'GET' GET A SORT RECORD                           
         ICM   R3,15,DMCB+4        POINT TO RETURNED RECORD                     
         BNZ   PBOUT204                                                         
         MVI   NOMOREF,C'Y'                                                     
         B     PUTBOUTX                                                         
PBOUT204 STCM  R3,15,ASRTREC                                                    
         AHI   R3,L'REC_TSEQ   BUMP PASS SORT KEY (SEQ NUMBER)                  
         AHI   R3,L'REC_BW4T   W4 TYPE                                          
*                                                                               
         LA    R0,REC_OUT                                                       
         LA    R1,750                                                           
         LR    RE,R3                                                            
         LA    RF,750                                                           
         MVCL  R0,RE                                                            
         LA    R2,REC_OUT                                                       
*                                                                               
         CLI   0(R2),REC_TIDQ                                                   
         BNE   PBOUT220                                                         
         USING REC_TD,R2                                                        
*        ZICM  RE,SEQNUM,(15)                                                   
*        SHI   RE,4            NUMBER OF PAYEES = SEQNUM - 4                    
*        STCM  RE,15,SEQNUM    (T,A,C,F RECS)                                   
         EDIT  (B4,TOTNUM),(8,REC_TNPY),ALIGN=RIGHT,ZERO=NOBLANK,      +        
               ,FILL=0                                                          
*                                                                               
PBOUT220 CLI   0(R2),REC_BIDQ                                                   
         BNE   PBOUT260                                                         
         BAS   RE,PRINT99                                                       
*                                                                               
         LA    R0,REC_OUT                                                       
         LA    R1,750                                                           
         LR    RE,R3                                                            
         LA    RF,750                                                           
         MVCL  R0,RE                                                            
         LA    R2,REC_OUT                                                       
*                                                                               
******                                                                          
PBOUT260 CLI   RECNUM,TNNRPT   1099 REPORT DONT PRINT TOTAL LINE                
         BE    PBOUT300                                                         
         CLI   RECNUM,TNNFCP   FOREIGN CORP DONT RINT TOTAL LINE                
         BE    PBOUT300                                                         
         CLI   0(R2),REC_CIDQ  IF WE ARE DONE PROCESSING ALL PAYEE              
         BNE   PBOUT300        TYPE B RECORDS - PRINT OUT LAST SUBTOTAL         
         XC    P,P             AND TOTAL LINES                                  
         LA    R3,P                                                             
         USING P1D,R3                                                           
         MVC   P1CIDL(17),=C'Total Gross Wages'                                 
         EDIT  (B4,STGROSS),(11,P1GROSS),2,ALIGN=RIGHT,ZERO=NOBLANK             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    P,P             AND TOTAL LINES                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    P,P             AND TOTAL LINES                                  
         LA    R3,P                                                             
         USING P1D,R3                                                           
         MVC   P1CIDL(11),=C'Grand Total'                                       
         EDIT  (B4,TOTGROSS),(11,P1GROSS),2,ALIGN=RIGHT,ZERO=NOBLANK            
         GOTO1 SPOOL,DMCB,(R8)                                                  
*******                                                                         
PBOUT300 DS    0H                                                               
* READJUST  SEQ NUMBER ON FOR B RECORDSECORD                                    
         CLI   0(R2),REC_BIDQ                                                   
         BNE   PBOUT400                                                         
         ZICM  RE,BSEQNUM,(15)                                                  
         AHI   RE,1                                                             
         STCM  RE,15,BSEQNUM                                                    
PBOUT400 ZICM  RE,SEQNUM,(15)                                                   
         AHI   RE,1                                                             
         STCM  RE,15,SEQNUM                                                     
         USING REC_BD,R2                                                        
         EDIT  (B4,SEQNUM),(8,REC_BSEQ),ALIGN=RIGHT,ZERO=NOBLANK,               
               ,FILL=0                                                          
         BRAS  RE,PUTTAPE                                                       
         B     PBOUT200           GET NEXT SORT RECORD                          
                                                                                
PUTBOUTX J     XIT                                                              
*                                                                               
         DROP  R2                                                               
****************************************************************                
* print to report                                                               
****************************************************************                
PRINT99  NTR1                                                                   
*                                                                               
***      LA    R5,REC_OUT                                                       
         L     R5,ASRTREC                                                       
         AHI   R5,L'REC_BSEQ     RECORD HEADER HAS W4 TYPE/SEQUENCE             
         AHI   R5,L'REC_BW4T     NUMBER FOR SORTING PURPOSE                     
         USING REC_BD,R5                                                        
         CLI   RECNUM,TNNRPT   1099 REPORT DONT PRINT TOTAL LINE                
         BE    PRNT99_1                                                         
         BE    PBOUT300                                                         
         CLC   SV99W4TY,REC_BW4T                                                
         BE    PRNT99_1                                                         
         CLI   SV99W4TY,0                                                       
         BE    PRNT99_1                                                         
         XC    P,P                                                              
         LA    R3,P                                                             
         USING P1D,R3                                                           
         MVC   P1CIDL(17),=C'Total Gross Wages'                                 
         EDIT  (B4,STGROSS),(11,P1GROSS),2,ALIGN=RIGHT,ZERO=NOBLANK             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    STGROSS,STGROSS                                                  
PRNT99_1 ZICM  RE,REC_BGRS,(15)                                                 
         ZICM  RF,STGROSS,(15)                                                  
         AR    RE,RF                                                            
         STCM  RE,15,STGROSS                                                    
         ZICM  RF,TOTGROSS,(15)                                                 
         ZICM  RE,REC_BGRS,(15)                                                 
         AR    RE,RF                                                            
         STCM  RE,15,TOTGROSS                                                   
         MVC   SV99W4TY,REC_BW4T                                                
*                                                                               
         XC    P,P                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
*                                                                               
         XC    P,P                                                              
         LA    R3,P                                                             
         USING P1D,R3                                                           
         MVC   P1CIDL(10),=C'Corp ID - '                                        
         MVC   P1CID,REC_BTIN                                                   
         MVC   P1W4FILT,REC_BW4T                                                
         CLI   REC_BW4T,TL99TYB                                                 
         BNE   PRNT99_3                                                         
         EDIT  (C12,REC_BPA3),(11,P1GROSS),2,ALIGN=RIGHT,ZERO=NOBLANK           
*PRNT99_1 MVC   P1GROSS,REC_BPA3                                                
         B     PRNT99_5                                                         
PRNT99_3 EDIT  (C12,REC_BPA7),(11,P1GROSS),2,ALIGN=RIGHT,ZERO=NOBLANK           
*PRNT99_3 MVC   P1GROSS,REC_BPA7                                                
PRNT99_5 GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         XC    P,P                                                              
         LA    R3,P                                                             
         USING P2D,R3                                                           
         MVC   P2CNAMEL(12),=C'Corp Name - '                                    
         MVC   P2CNAME,REC_BPNM                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         XC    P,P                                                              
         LA    R3,P                                                             
         USING P3D,R3                                                           
         MVC   P3CADDRL(15),=C'Corp Address - '                                 
         MVC   P3CADDR(L'REC_BAD1),REC_BAD1                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
*                                                                               
         XC    P,P                                                              
         LA    R3,P                                                             
         USING P4D,R3                                                           
         LA    R2,P4CADDR                                                       
         OC    REC_BAD2,REC_BAD2                                                
         BZ    PRNT99_6                                                         
         MVC   0(L'REC_BAD2,R2),REC_BAD2                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    P,P                                                              
         LA    R3,P                                                             
         USING P5D,R3                                                           
         LA    R2,P5CADDR                                                       
         OC    REC_BAD3,REC_BAD3                                                
         BZ    PRNT99_6                                                         
         MVC   0(L'REC_BAD3,R2),REC_BAD3                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    P,P                                                              
         LA    R3,P                                                             
         USING P6D,R3                                                           
         LA    R2,P6CADDR                                                       
*                                                                               
PRNT99_6 XC    P,P                                                              
         MVC   SCANLINE,SPACES                                                  
         LA    RE,SCANLINE                                                      
         MVC   0(L'REC_BCIT,RE),REC_BCIT                                        
         AHI   RE,L'REC_BCIT+1                                                  
         MVC   0(L'REC_BSTA,RE),REC_BSTA                                        
         AHI   RE,L'REC_BSTA                                                    
         MVC   0(L'REC_BZPC,RE),REC_BZPC                                        
         OC    SCANLINE,SPACES                                                  
         MVC   WORK(L'SCANLINE),SCANLINE                                        
         GOTO1 SQUASHER,DMCB,WORK,L'SCANLINE                                    
         MVC   0(L'P6CADDR,R2),WORK                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
*                                                                               
         XC    P,P                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   REC_BTMP(REC_BTMQ),SPACES                                        
         J     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
         DROP  R5                                                               
*******************************************************************             
                                                                                
                                                                                
                                                                                
PUTTAPE  NTR1  BASE=*,LABEL=*                                                   
         L     R1,ADCB                                                          
         PUT   (1),(2)             (R2)=A(FROM AREA)                            
         J     XIT                                                              
*--------------------------------------------------------------------           
* SORT UTILITIES                                                                
*--------------------------------------------------------------------           
SORTPUT  NTR1  BASE=*,LABEL=*                                                   
         LA    R5,SRTPREC          POINT TO SORT RECORD AREA                    
         USING SRTPRECD,R5                                                      
*                                                                               
         CLI   SORTFRST,C'Y'       IF FIRST TIME TO SORT                        
         BNE   SORTPUT8                                                         
*                                  OPEN SORT                                    
         XC    RECCOUNT,RECCOUNT                                                
         CLI   RECNUM,TNNPRT                                                    
         BE    SRTPUT4                                                          
         GOTO1 VSORTER,DMCB,SORTCARD,RECCARD                                    
         B     SRTPUT6                                                          
                                                                                
*                                                                               
SRTPUT4  GOTO1 VSORTER,DMCB,SORTCRD2,RECCARD                                    
*                                                                               
SRTPUT6  MVI   SORTFRST,C'N'       INDICATE SORT OPEN                           
*                                                                               
SORTPUT8 GOTO1 VSORTER,DMCB,=C'PUT',SRTPREC ADD RECORD TO SORT                  
         L     RE,RECCOUNT                                                      
         AHI   RE,1                                                             
         ST    RE,RECCOUNT                                                      
*                                                                               
SORTPUTX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DS    0F                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,18,A),FORMAT=BI,WORK=1'                      
SORTCRD2 DC    CL80'SORT FIELDS=(1,28,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(1000)'                                
*                                                                               
         LTORG                                                                  
*                                                                               
TNNRPT   EQU   133                 1099 REPORT                                  
TNNBLD   EQU   137                 1099 BUILD                                   
TNNPRT   EQU   138                 1099 PRINT                                   
TNNFCP   EQU   156                 FOREIGN CORP 1042 REPORT                     
         DROP  R5                                                               
                                                                                
***********************************************************************         
*              TAPE SUPPORT                                           *         
***********************************************************************         
OPENTAPE NTR1  BASE=*,LABEL=*                                                   
         CLI   RECNUM,TNNPRT       ONLY SORT UNDER CORP/TYPE                    
         JNE   XIT                                                              
         L     R2,ASPFUSER         SAVE GENTAB IN USER AREA                     
         USING SPFUSERD,R2                                                      
*                                                                               
         LA    R1,GENTAB           R1=A(GENERATION TABLE)                       
         USING GENTBLD,R1                                                       
OPENT20  OC    GENEMP,GENEMP      ANY EMPLOYERS?                                
         BNZ   OPENT25                                                          
         MVC   GENEMP,TIFEMP      SAVE NEW EMPLOYER                             
         B     OPENT30                                                          
OPENT25  CLC   GENEMP,TIFEMP      GENERATING NEW TAPE FOR SAME EMP?             
         BNE   *+8                                                              
         BE    OPENT30                                                          
         LA    R1,GENTBLL(R1)                                                   
         B     OPENT20                                                          
OPENT30  ZIC   RF,GENNUM          BUMP GENERATION NUMBER FOR NEW TAPE           
         LA    RF,1(RF)                                                         
         STC   RF,GENNUM                                                        
         DROP  R1,R2                                                            
*                                                                               
         MVC   WORK(20),=CL20'BPOO.TA0WESS1'                                    
         MVC   WORK+12(1),TIFEMP          REPLACE 'E' WITH EMPLOYER             
         MVC   WORK+13(2),TIFYEAR         REPLACE 'SS' WITH YEAR                
         MVC   DUB,=CL8'T99TAPE'                                                
*                                                                               
         GOTO1 DYNALLOC,DMCB,(0,DUB),((RF),WORK)                                
OPENT40  L     R2,ADCB                                                          
         OPEN  ((2),OUTPUT)                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
OPENTX   XIT1                                                                   
         LTORG                                                                  
                                                                                
***********************************************************************         
*        INITIALIZE DOWNLOAD SETTINGS                                 *         
*        ON ENTRY ... R5=A(DOWNLOAD BLOCK)                            *         
***********************************************************************         
                                                                                
INITDOWN NTR1  BASE=*,LABEL=*                                                   
         USING DLCBD,R5                                                         
         XC    DLCBD(DLCBXLX),DLCBD                                             
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,PRTDOWN          A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         MVC   P,SPACES                                                         
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)    MAXIMUM LENGTH OF PRINT LINE                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
***      MVI   DLCXEOLC,X'0D'      CR FOR END OF LINE                           
         MVI   DLCXEOLC,X'5E'      CR FOR END OF LINE                           
         MVI   DLCXEORC,C':'       END OF REPORT                                
         OI    DLCBFLG1,DLCBFXTN                                                
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
*                                                                               
***********************************************************************         
*        USER SUPPLIED PRINT ROUTINE                                  *         
***********************************************************************         
                                                                                
PRTDOWN  NTR1                                                                   
         MVI   LINE,1              PREVENT PAGE BREAK                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
*                                                                               
*                                                                               
***********************************************************************         
*        OUTPUT DOWNLOAD ENTRY                                        *         
*        ON ENTRY ... P1, B0    = TYPE TO PASS                        *         
*                         B1-B3 = ADDRESS OF DATA                     *         
*                     P2        = LENGTH                              *         
***********************************************************************         
                                                                                
OUTPDOWN NTR1                                                                   
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBTYP,0(R1)                                                    
         OI    DLCBFLG1,DLCBFXFL                                                
*                                                                               
         L     RF,0(R1)            RF=A(DOWNLOAD FIELD)                         
         L     RE,4(R1)            RE=L'DOWNLOAD FIELD                          
*                                                                               
         LR    R1,RF                                                            
         AR    R1,RE                                                            
OPD10    SHI   R1,1                                                             
         CLI   0(R1),C' '                                                       
         JNE   OPD20                                                            
         BCT   RE,OPD10                                                         
         LHI   RE,1                                                             
OPD20    STC   RE,DLCBLEN                                                       
*                                                                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLCBFLX(0),0(RF)                                                 
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
*                                                                               
***********************************************************************         
*        FINISH LINE OF DOWNLOAD OUPUT                                *         
***********************************************************************         
                                                                                
EOLDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOL      END OF LINE                                 
         GOTO1 =V(DLFLD),DLCBD      (DON'T USE RF, BASR RE,RF BELOW)            
         J     XIT                                                              
*                                                                               
***********************************************************************         
*        END DOWNLOAD                                                 *         
***********************************************************************         
                                                                                
ENDDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOR      END OF REPORT                               
         GOTO1 =V(DLFLD),DLCBD      LAST FOR REPORT                             
         J     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*======================================================================         
PREP2    NTR1  BASE=*,LABEL=*                                                   
         MVI   TAPEOPT,C'Y'                                                     
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
**       L     R2,=A(T99TAPE)                                                   
         L     R2,ADCB                                                          
         OPEN  ((2),INPUT)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R5,DLBLOCK                                                       
         USING DLCBD,R5                                                         
         BRAS  RE,INITDOWN                                                      
*                                                                               
PREP22   L     R4,AIO1                                                          
         GET   (R2),(R4)           GET FROM DISK AND PRINT                      
         L     R4,AIO1                                                          
*                                                                               
         SR    R0,R0                                                            
         LA    R1,750              RECORD LENGTH IN DCBRLN                      
         D     R0,=F'40'           R1  = # OF 40 BYTE CHUNKS                    
         STC   R0,REM              REM = LEFT OVER BYTES                        
*                                                                               
         LTR   R1,R1                                                            
         BZ    PREP24                                                           
*                                                                               
PREP23   STC   R1,QUO                                                           
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
*                                                                               
         MVI   DLCBLEN,40                                                       
         MVC   DLCBFLD(40),0(R4)    PASS DATA                                   
         GOTO1 =V(DLFLD),DLCBD                                                  
*                                                                               
         LA    R4,40(R4)           BUMP TO NEXT TAPE FIELD                      
         ZIC   R1,QUO                                                           
         BCT   R1,PREP23                                                        
*                                                                               
PREP24   CLI   REM,0                                                            
         BE    PREP25                                                           
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
PREP25   MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 =V(DLFLD),DLCBD                                                  
         B     PREP22                                                           
*                                                                               
NOMORE   L     R2,ADCB                                                          
         CLOSE ((2))               CLOSE THE DATASET                            
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         J     NOMORE2                                                          
         CLI   ACTEQU,ACTDOWN                                                   
         BE    NOMORE1                                                          
         CLI   TAPEOPT,C'Y'                                                     
         BNE   NOMORE2                                                          
NOMORE1  L     R1,ALOGO            NO-OP SO DON'T END AGAIN                     
         MVC   0(2,R1),=X'07FE'                                                 
         J     XIT                                                              
NOMORE2  MVI   DLCBACT,DLCBEOR                                                  
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
         EJECT                                                                  
*          DATA SET TAREP2F    AT LEVEL 054 AS OF 11/21/13                      
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
         MVC   REMOTJID,=C'T99'                                                 
NPRT30   MVC   REMOTKEY(11),SPACES                                              
         MVC   REMOTSYS(6),=C'T99DAT'                                           
         MVC   REMOTFRM(4),=C'DATA'                                             
         XIT1                                                                   
         DROP  R2,RF                                                            
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
         EJECT                                                                  
CLOSTAPE NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   RECNUM,TNNPRT       ONLY SORT UNDER CORP/TYPE                    
         JNE   XIT                                                              
         L     R2,ADCB                                                          
         CLOSE ((2))                                                            
CTAPEX   XIT1                                                                   
         LTORG                                                                  
                                                                                
         GETEL2 R4,DATADISP,ELCODE                                              
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
HEDSPECS DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,100,C'REPORT TNN'                                             
         SSPEC H2,120,PAGE                                                      
         SSPEC H4,50,C'1099 FORM REPORT'                                        
         SSPEC H5,50,C'----------------'                                        
         SPACE 1                                                                
         SSPEC H6,1,C'Corporation'                                              
         SSPEC H6,46,C'Filter'                                                  
         SSPEC H6,54,C'Performer'                                               
         SSPEC H6,106,C'Gross Wages'                                            
         SSPEC H6,121,C'Reimbursed'                                             
         SSPEC H7,121,C'Expenses'                                               
         DC    X'00'                                                            
         PRINT ON                                                               
HEDSPEC2 DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,100,C'REPORT TNN'                                             
         SSPEC H2,120,PAGE                                                      
         SSPEC H4,50,C'1099 FORM REPORT'                                        
         SSPEC H5,50,C'----------------'                                        
         SPACE 1                                                                
         SSPEC H6,1,C'Corporation'                                              
         SSPEC H6,46,C'Filter'                                                  
         SSPEC H6,107,C'Gross Wages'                                            
         SSPEC H7,121,C'.'                                                      
         DC    X'00'                                                            
         PRINT ON                                                               
                                                                                
HEDSPEC3 DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,100,C'REPORT TNN'                                             
         SSPEC H2,120,PAGE                                                      
         SSPEC H4,50,C'1099 FORM REPORT'                                        
         SSPEC H5,50,C'----------------'                                        
         SPACE 1                                                                
         SSPEC H6,1,C'Corporation'                                              
         SSPEC H6,107,C'Gross Wages'                                            
         SSPEC H7,121,C'.'                                                      
         DC    X'00'                                                            
         PRINT ON                                                               
HEDSPEC4 DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,100,C'REPORT TFC'                                             
         SSPEC H2,120,PAGE                                                      
         SSPEC H4,50,C'FGNCORP REPORT'                                          
         SSPEC H5,50,C'--------------'                                          
         SPACE 1                                                                
         SSPEC H6,1,C'Corporation'                                              
         SSPEC H6,46,C'Filter'                                                  
         SSPEC H6,107,C'Gross Wages'                                            
         SSPEC H7,121,C'.'                                                      
         DC    X'00'                                                            
         PRINT ON                                                               
                                                                                
                                                                                
T99TAPE  DCB   DDNAME=T99TAPE,DSORG=PS,MACRF=(GM,PM),                  X        
               RECFM=FB,LRECL=750,BUFNO=2,BLKSIZE=30000,EODAD=NOMORE            
****           RECFM=FB,LRECL=512,BUFNO=2,BLKSIZE=23040,EODAD=NOMORE            
*                                                                               
                                                                                
*                                                                               
*---------------------------------------------------------------*               
*       TRASMITTER RECORD                                                       
*---------------------------------------------------------------*               
REC_TD   DSECT                                                                  
REC_TID  DS    C                                                                
REC_TIDQ EQU   C'T'                                                             
REC_TYR  DS    CL4                                                              
REC_TPYI DS    CL1                                                              
REC_TTIN DS    CL9                                                              
REC_TTCC DS    CL5                                                              
         DS    CL7                                                              
         DS    CL1      TEST FILE INDICATOR                                     
         DS    CL1      FOREIGN ENTITY INDICATOR                                
REC_TTNM DS    CL40     TRANSMITTER NAME                                        
         DS    CL40     TRASMITTER NAME CONTINUATION                            
REC_TCNM DS    CL40     COMPANY  NAME                                           
         DS    CL40     COMPANY  NAME  CONTINUATION                             
REC_TADD DS    CL40     COMPANY  MAILING ADDRESS                                
REC_TCTY DS    CL40     COMPANY  CITY                                           
REC_TSTA DS    CL2      COMPANY  STATE                                          
REC_TZIP DS    CL9      COMPANY  ZIP                                            
         DS    CL15     BLANK                                                   
REC_TNPY DS    CL8      TOTAL NUMBER OF PAYEE RECORDS                           
REC_TCNN DS    CL40     CONTACT NAME                                            
REC_TTEL DS    CL15     CONTACT TELEPHONE                                       
REC_TEML DS    CL50     CONTACT EMAIL                                           
         DS    CL91     BLANK                                                   
REC_TSEQ DS    CL8      SEQUENCE NUMBER                                         
         DS    CL10     BLANK                                                   
REC_TVEN DS    CL1      VENDOR                                                  
         DS    CL40     VENDOR NAME                                             
         DS    CL40     VENDOR MAILNG ADDRESS                                   
         DS    CL40     VENDOR CITY                                             
         DS    CL2      VENDOR STATE                                            
         DS    CL9      VENDOR ZIP                                              
         DS    CL40     VENDOR CONTACT                                          
         DS    CL15     VENDOR CONTACT TELEPHONE                                
         DS    CL35     BLANK                                                   
         DS    CL1      VENDOR FOREIGN ENTITY INDICATOR                         
         DS    CL8      BLANK                                                   
         DS    CL2      BLANK                                                   
*                                                                               
*---------------------------------------------------------------*               
*       PAYER RECORD                                                            
*---------------------------------------------------------------*               
REC_AD   DSECT                                                                  
REC_AID  DS    C                                                                
REC_AIDQ EQU   C'A'                                                             
REC_AYR  DS    CL4                                                              
         DS    CL1      COMBINED FEDERAL STATE FLIER                            
         DS    CL5      BLANK                                                   
REC_ATIN DS    CL9      PAYER TIN                                               
         DS    CL4      PAYER NAME CONTROL                                      
         DS    CL1      LAST FILING INDICATOR                                   
REC_ATYR DS    CL2      TYPE OF RETURN                                          
REC_AACD DS    CL16     AMOUNT CODES      NEED TO BE VISITED                    
         DS    CL8      BLANK                                                   
         DS    CL1      FORGEIGN ENTITY INDICATOR                               
REC_AFPN DS    CL40     FIRST PLAYER NAME LINE                                  
         DS    CL40     SECOND PLAYER NAME LINE                                 
REC_ATAI DS    CL1      TRANSFER AGENT INDICATIOR                               
REC_AADD DS    CL40     PAYER SHIPPING ADDRESS                                  
REC_ACTY DS    CL40     PAYER CITY                                              
REC_ASTA DS    CL2      PAYER STATE                                             
REC_AZIP DS    CL9      PAYER ZIP CODE                                          
REC_ATEL DS    CL15     PAYERS TELEPHONE                                        
         DS    CL260    BLANK                                                   
REC_ASEQ DS    CL8      RECORD SEQUENCE NUMBER                                  
         DS    CL241    BLANK                                                   
         DS    CL2      BLANK OR CR/LF                                          
*                                                                               
*---------------------------------------------------------------*               
*       PAYEE RECORD                                                            
*---------------------------------------------------------------*               
REC_BD   DSECT                                                                  
REC_BID  DS    C                                                                
REC_BIDQ EQU   C'B'                                                             
REC_BYR  DS    CL4      PAYMENT YEAR                                            
         DS    CL1      CORRECT RETURN INDICATOR                                
         DS    CL4      NAME CONTROL                                            
         DS    CL1      TYPE OF TIN                                             
REC_BTIN DS    CL9      PAYEE'S TIN                                             
         DS    CL20     PAYERS ACCOUNT NUMBER FOR PAYEE                         
         DS    CL4      PAYERS OFFICE CODE                                      
         DS    CL10     BLANK                                                   
REC_BPA1 DS    CL12     PAYMENT AMOUNT 1                                        
         DS    CL12     PAYMENT AMOUNT 2                                        
REC_BPA3 DS    CL12     PAYMENT AMOUNT 3                                        
         DS    CL12     PAYMENT AMOUNT 4                                        
         DS    CL12     PAYMENT AMOUNT 5                                        
         DS    CL12     PAYMENT AMOUNT 6                                        
REC_BPA7 DS    CL12     PAYMENT AMOUNT 7                                        
         DS    CL12     PAYMENT AMOUNT 8                                        
         DS    CL12     PAYMENT AMOUNT 9                                        
         DS    CL12     PAYMENT AMOUNT A                                        
         DS    CL12     PAYMENT AMOUNT B                                        
         DS    CL12     PAYMENT AMOUNT C                                        
         DS    CL12     PAYMENT AMOUNT D                                        
         DS    CL12     PAYMENT AMOUNT E                                        
         DS    CL12     PAYMENT AMOUNT F                                        
         DS    CL12     PAYMENT AMOUNT G                                        
REC_BPAX DS    0C                                                               
REC_BFCI DS    CL1      FOREIGN COUNTRY INDICATOR                               
REC_BPNM DS    CL40     FIRST PAYEE NAME LINE                                   
         DS    CL40     SECOND PAYEE NAME LINE                                  
         DS    CL40     BLANK                                                   
REC_BPAD DS    CL40     PAYEE MAILING ADDRESS                                   
         DS    CL40     BLANK                                                   
REC_BCIT DS    CL40     PAYEE CITY                                              
REC_BSTA DS    CL2      PAYEE STATE                                             
REC_BZIP DS    CL9      ZIP CODE                                                
         DS    CL1      BLANK                                                   
REC_BSEQ DS    CL8      RECORD SEQUENCE NUMBER                                  
         DS    CL36     BLANK                                                   
         DS    CL1      SECOND TIN                                              
         DS    CL2      BLANK                                                   
         DS    CL1      DIRECT SALES INDICATOR                                  
         DS    CL115    BLANK                                                   
         DS    CL60     SPECIAL DATA ENTRIES                                    
         DS    CL12     STATE ICOME TAX WITHHELD                                
         DS    CL12     LOCAL INCOME TAX WITHHELD                               
         DS    CL2      COMBINED FEDERAL STATE CODE                             
*                                                                               
REC_BTMP DS    0C  TEMP STORAG FOR STORAGE TO CARRY OVER TO PRINTING            
REC_BGRS DS    CL(L'TA99EARN)                                                   
REC_BW4T DS    CL(L'TAW4TYPE)                                                   
REC_BAD1 DS    CL(L'SVPFADD1)                                                   
REC_BAD2 DS    CL(L'SVPFADD2)                                                   
REC_BAD3 DS    CL(L'SVPFADD3)                                                   
REC_BZPC DS    CL(L'SVPFZIP)  FULL PRINTABLE ZIP CODE                           
REC_BTMQ EQU   *-REC_BTMP                                                       
                                                                                
*                                                                               
*---------------------------------------------------------------*               
*       END OF PAYER                                                            
*---------------------------------------------------------------*               
REC_CD   DSECT                                                                  
REC_CID  DS    C                                                                
REC_CIDQ EQU   C'C'                                                             
REC_CNPY DS    CL8      NUMBER OF PAYEES         # OF B RECORDS                 
         DS    CL6      BLANK                                                   
         DS    CL18     CONTROL TOTAL1                                          
         DS    CL18     CONTROL TOTAL2                                          
REC_CCT3 DS    CL18     CONTROL TOTAL3                                          
         DS    CL18     CONTROL TOTAL4                                          
         DS    CL18     CONTROL TOTAL5                                          
         DS    CL18     CONTROL TOTAL6                                          
REC_CCT7 DS    CL18     CONTROL TOTAL7                                          
         DS    CL18     CONTROL TOTAL8                                          
         DS    CL18     CONTROL TOTAL9                                          
         DS    CL18     CONTROL TOTALA                                          
         DS    CL18     CONTROL TOTALB                                          
         DS    CL18     CONTROL TOTALC                                          
         DS    CL18     CONTROL TOTALD                                          
         DS    CL18     CONTROL TOTALE                                          
         DS    CL18     CONTROL TOTALF                                          
         DS    CL18     CONTROL TOTALG                                          
         DS    CL196     BLANK                                                  
REC_CSEQ DS    CL8      RECORD SEQUENCE NUMBER                                  
         DS    CL241    BLANK                                                   
*                                                                               
*                                                                               
*---------------------------------------------------------------*               
*       END OF TRANSMISSION F RECORD                                            
*---------------------------------------------------------------*               
REC_FD   DSECT                                                                  
REC_FID  DS    C                                                                
REC_FIDQ EQU   C'F'                                                             
REC_FNMA DS    CL8      NUMBER OF A RECORDS                                     
REC_FZ1  DS    CL21     ZEROS                                                   
         DS    CL19     BLANKS                                                  
REC_FNMB DS    CL8      NUMBER OF B RECORDS                                     
         DS    CL442    BLANKS                                                  
REC_FSEQ DS    CL8      SEQUENCE NUMBERS                                        
         DS    CL241    BLANKS                                                  
         DS    CL2      BLANKS                                                  
*                                                                               
*                                                                               
*              DSECT TO COVER TSPFUSER                                          
SPFUSERD DSECT                                                                  
****ABUFFER  DS    A                  EMPLOYER                                  
GENTAB   DS    255CL(GENTBLL)     GENERATION TABLE                              
*                                                                               
*              DSECT TO COVER GENTAB (IN TSPFUSER)                              
*                                                                               
GENTBLD  DSECT                                                                  
GENEMP   DS    CL1                EMPLOYER                                      
GENNUM   DS    XL1                LATEST GENERATION NUM. FOR EMP/STATE          
GENTBLL  EQU   *-GENTBLD                                                        
         EJECT                                                                  
***********************************************************************         
*        WORKING STORAGE                                                        
***********************************************************************         
                                                                                
P1D      DSECT                                                                  
P1CIDL   DS    CL15                                                             
P1CID    DS    CL9                                                              
P1CORPL  EQU   *-P1D                                                            
         DS    CL19                                                             
***      DS    CL10                                                             
         DS    CL5                                                              
P1W4FILT DS    CL1                                                              
         DS    CL4                                                              
P1PSSNL  DS    CL11                                                             
         DS    CL4                                                              
P1PSSN   DS    CL9                                                              
         DS    CL29                                                             
P1GROSSB DS    CL(L'TAPDGRS)                                                    
         ORG   P1GROSSB                                                         
P1GROSS  DS    CL12                                                             
         DS    CL02                                                             
P1REXPB  DS    CL(L'TAPDREXP)                                                   
         ORG   P1REXPB                                                          
P1REXP   DS    CL12                                                             
P1RECL   EQU   *-P1D                                                            
*                                                                               
P2D       DSECT                                                                 
P2CNAMEL  DS    CL15                                                            
P2CNAME   DS    CL25                                                            
P2CORPL  EQU   *-P2D                                                            
          DS    CL1                                                             
          DS    CL12                                                            
P2PNAMEL  DS    CL12                                                            
          DS    CL3                                                             
P2PNAME   DS    CL30                                                            
*                                                                               
P3D       DSECT                                                                 
P3CADDRL  DS    CL15                                                            
**P3CADDR   DS    CL35                                                          
P3CADDR   DS    CL33                                                            
P3CORPL  EQU   *-P3D                                                            
          DS    CL5                                                             
P3PFADDL  DS    CL15                                                            
P3PFADDR  DS    CL35                                                            
*                                                                               
P4D       DSECT                                                                 
P4CADDRL  DS    CL15                                                            
***P4CADDR   DS    CL27                                                         
P4CADDR   DS    CL30                                                            
P4CORPL  EQU   *-P4D                                                            
***       DS    CL11                                                            
          DS    CL8                                                             
P4PFADDL  DS    CL15                                                            
P4PFADDR  DS    CL35                                                            
*                                                                               
P5D       DSECT                                                                 
P5CADDRL  DS    CL15                                                            
**P5CADDR   DS    CL27                                                          
P5CADDR   DS    CL30                                                            
P5CORPL  EQU   *-P5D                                                            
***       DS    CL11                                                            
          DS    CL8                                                             
P5PFADDL  DS    CL15                                                            
P5PFADDR  DS    CL35                                                            
P6D       DSECT                                                                 
P6CADDRL  DS    CL15                                                            
**P6CADDR   DS    CL27                                                          
P6CADDR   DS    CL30                                                            
P6CORPL  EQU   *-P6D                                                            
***       DS    CL11                                                            
          DS    CL8                                                             
P6PFADDL  DS    CL15                                                            
P6PFADDR  DS    CL35                                                            
          DS    CL10           EXTENDED AREA FOR LONGER ZIP                     
*                                                                               
SRTPRECD  DSECT                                                                 
SRTREC    DS    0C                                                              
SRTW4FLT  DS    CL1                                                             
SRTCSSN   DS    CL(L'TLCKSSN)                                                   
          ORG   SRTREC                                                          
SRTCSSN2  DS    CL(L'TLCKSSN)                                                   
SRTW4FLT2 DS    CL1                                                             
                                                                                
*                                                                               
SRTKEYL1  EQU   *-SRTPRECD                                                      
SRTPSSN   DS    CL(L'TLCKSSN)                                                   
SRTKEYL2  EQU   *-SRTPRECD                                                      
SRTPREC1  DS    CL(L'P)                                                         
SRTPREC2  DS    CL(L'P)                                                         
SRTPREC3  DS    CL(L'P)                                                         
SRTPREC4  DS    CL(L'P)                                                         
SRTPREC5  DS    CL(L'P)                                                         
SRTPREC6  DS    CL(L'P)                                                         
SRTINV    DS    CL(L'TLCKINV)                                                   
*                                                                               
MYD      DSECT                                                                  
DLCB     DS    CL(DLCBXLX)         DOWNLOAD BLOCK                               
YEAR     DS    CL4                                                              
YEAR2    DS    CL4                                                              
VSORTER  DS    V                                                                
*----------------------------------------------------------------               
* DO NOT INSERT FIELDS IN HERE UNLESS YOU KNOW WHAT YOU ARE DOING               
*                                                                               
SVFIELDS DS    0C                                                               
SVFNAME  DS    CL16                                                             
SVLNAME  DS    CL16                                                             
SVW4FID  DS    CL(L'TLW4FID)                                                    
SVW4CRPN DS    CL(L'TAW4CRPN)                                                   
SVCKSSN  DS    CL(L'TLCKSSN)                                                    
SVPDGRS  DS    CL(L'TAPDGRS)                                                    
SVPDREXP DS    CL(L'TAPDREXP)                                                   
***SVW4NLST DS    CL(L'TLW4NLST)                                                
SVW4NLST DS    CL(L'TAW4CRPN)                                                   
SVW4NFST DS    CL(L'TLW4NFST)                                                   
SVW4TYPE DS    CL(L'TAW4TYPE)                                                   
SVCPADD1 DS    CL(L'TAA2ADD1)                                                   
SVCPADD2 DS    CL(L'TAA2ADD2)                                                   
SVCPADD3 DS    CL(L'TAA2ADD3)                                                   
SVINV    DS    CL(L'TLCKINV)                                                    
SVPFCTRY DS    CL(L'TAA2CTRY)                                                   
SVW4FILT DS    CL1                                                              
*                                                                               
                                                                                
*SVCPCSZ  DS    0CL(L'SVCPCITY+L'SVCPST+L'SVCPZIP)                              
SVCPCSZ  DS    0CL(L'SVCPCITY+L'SVCPST+L'SVCPZIP+L'SVCPCTRY+1)                  
SVCPCITY DS    CL(L'TAA2CITY)                                                   
         DS    CL1                                                              
SVCPST   DS    CL(L'TAA2ST)                                                     
SVCPZIP  DS    CL(L'TAA2ZIP+3)                                                  
SVCPCTRY DS    CL(L'TAA2CTRY)                                                   
*                                                                               
SVPFADD1 DS    CL(L'TAA2ADD1)                                                   
SVPFADD2 DS    CL(L'TAA2ADD2)                                                   
SVPFADD3 DS    CL(L'TAA2ADD3)                                                   
*                                                                               
SVPFCSZ  DS    0CL(L'SVPFCITY+L'SVPFST+L'SVPFZIP)                               
SVPFCITY DS    CL(L'TAA2CITY)                                                   
         DS    CL1                                                              
SVPFST   DS    CL(L'TAA2ST)                                                     
SVPFZIP  DS    CL(L'TAA2ZIP+3)                                                  
*                                                                               
PVCKSSN  DS    CL(L'TLCKSSN)                                                    
SVPFSSN  DS    CL(L'TLCKSSN)                                                    
*                                                                               
SVCHKDTE DS    CL8                                                              
SVSRTKEY DS    CL(SRTKEYL2)                                                     
STGROSS  DS    CL(L'SVPDGRS)                                                    
TOTGROSS DS    CL(L'SVPDGRS)                                                    
SVFIELDL EQU   *-SVFIELDS                                                       
*----------------------------------------------------------------               
*                                                                               
SVRSTA   DS    CL8                                                              
SVREND   DS    CL8                                                              
SV99W4TY DS    CL1                                                              
SV99EARN DS    CL(L'TA99EARN)                                                   
SVP1CID  DS    CL(L'TLW4FID)                                                    
W4KEY    DS    XL(L'TLW4KEY)                                                    
CKKEY    DS    XL(L'TLCKKEY)                                                    
CKKEYSAV DS    XL(L'TLCKKEY)                                                    
TOTALGRS DS    CL10                                                             
TAPEOPT  DS    CL1                                                              
DLBLOCK  DS    CL(DLCBXLX)                                                      
QUO      DS    XL1                                                              
REM      DS    XL1                                                              
DYNALLOC DS    A                                                                
ASPFUSER DS    A                                                                
AMASTD   DS    A                                                                
ALOGOC   DS    A                                                                
ALOGO    DS    A                                                                
AREMOT   DS    A                                                                
ADCB     DS    A                                                                
CORPGRS  DS    F                                                                
*TAPEOPT  DS    CL1                 TAPE=Y                                      
INDIGRS  DS    F                                                                
RECCOUNT DS    F                                                                
NOMOREF  DS    C                                                                
SORTFRST DS    C                                                                
EMPADD1  DS    CL(L'TAADADD)                                                    
EMPADD2  DS    CL(L'TAADADD)                                                    
EMPADD3  DS    CL(L'TAADADD)                                                    
EMPCITY  DS    CL(L'TAADADD)                                                    
EMPSTATE DS    CL(L'TAADADD)                                                    
EMPZIP   DS    CL(L'TAADADD)                                                    
EMPNAME  DS    CL(L'TANANAME)                                                   
EMPADD   DS    CL(L'TAADADD)                                                    
EMPTIN   DS    CL(L'REC_ATIN)                                                   
PRINTF   DS    C                                                                
SEQNUM   DS    F                                                                
BRECNUM  DS    F                                                                
TOTNUM   DS    F                                                                
BSEQNUM  DS    F                                                                
SVCONT3  DS    D                                                                
SVCONT7  DS    D                                                                
DUB2     DS    D                                                                
ALLEMP   DS    C                                                                
SCANLINE DS    CL80                                                             
PREVKEY  DS    CL(L'KEY)                                                        
ASRTREC  DS    A                                                                
SRTPREC  DS    CL1000              SORT PRINT RECORD                            
REC_OUT  DS    CL750                                                            
SVSRTREC DS    CL1000              SORT PRINT RECORD                            
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPCFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPCBD                                                       
         EJECT                                                                  
*DDGENTWA  (MUST FOLLOW LAST SCREEN)                                            
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*DDPERVALD                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*DDDLCB                                                                         
*DDTWADCONS                                                                     
*TAREPWORKD                                                                     
         PRINT ON                                                               
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDREMOTED                                                      
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026TAREP71   02/09/17'                                      
         END                                                                    
