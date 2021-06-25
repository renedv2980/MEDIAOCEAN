*          DATA SET SEACS25    AT LEVEL 002 AS OF 12/12/18                      
*PHASE TA0D25A                                                                  
*INCLUDE SCINKEY                                                                
*INCLUDE CHOPPER                                                                
*INCLUDE SQUASHER                                                               
         TITLE 'SEACS25 - USER ID SYSTEMS MAINTENANCE'                          
ACS25    CSECT                                                                  
*                                                                               
         NMOD1 0,*ACS25**,RA,R8,RR=RE                                           
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING CTIREC,R2           R2=A(RECORD KEY)                             
         L     RC,ASYSWORK                                                      
         USING SYSWORK,RC          RC=A(LOCAL W/S)                              
*                                                                               
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         LLC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY              APMVALK                                      
         B     EXIT                APMVALR                                      
         B     DISKEY              APMDISK                                      
         B     DISREC              APMDISR                                      
         B     EXIT                APMDELR                                      
         B     EXIT                APMRESR                                      
         B     VALSEL              APMVALP                                      
         B     GETSEL              APMGETS                                      
         B     DISSEL              APMDISS                                      
         B     EXIT                APMVALS                                      
         B     EXIT                APMFLST                                      
         B     EXIT                APMPROC                                      
         B     EXIT                APMFSCR                                      
         B     LSTSCR              APMLSCR                                      
         B     EXIT                APMVALQ                                      
         B     EXIT                APMREPP                                      
         B     SETTWA              APMSETT                                      
         B     EXIT                APMPUTK                                      
         B     EXIT                APMNEWK                                      
         B     EXIT                APMFRP                                       
         B     EXIT                APMDISS2                                     
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF USER ID RECORD                           *         
***********************************************************************         
VALKEY   LA    R2,IOKEY                                                         
         XC    CTIKEY,CTIKEY                                                    
*                                                                               
         MVC   LSAGY,CUAALF        SECURITY AGENCY                              
         MVC   LAAGY,CUAALF        AGENCY ALPHA                                 
         XC    AAGY,AAGY                                                        
         XC    SAGY,SAGY                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         GOTO1 AFVAL,IDSIDH        VALIDATE USER ID                             
         BNE   VALKEYX                                                          
         CLI   FVIFLD,X'80'        1ST CHR CAN'T BE A SPECIAL CHR               
         BL    EIIF                                                             
*&&US*&& CLC   FVIFLD(2),=C'OX'    OX... USER IDS NOT ALLOWED                   
*&&US*&& BE    EIIF                                                             
         TM    FVIIND,FVINUM       CHECK FOR NUMERIC INPUT                      
         BO    VKUSR10                                                          
         CLI   FVILEN,3            USER ID TOO SHORT                            
         BL    EFTS                                                             
         MVC   CTIKID,FVIFLD                                                    
         B     VKUSRX                                                           
*                                  BUILD KEY OF NUMERIC REC & READ              
VKUSR10  OC    SCFULL(4),SCFULL    NUMBER IN SCFULL FROM AFVAL                  
         BZ    EIIF                CHECK NUMBER IN RANGE                        
         OC    SCFULL(2),SCFULL                                                 
         BNZ   EFTB                CHECK NUMBER IN RANGE                        
         MVC   CTIKNUM,SCFULL+2                                                 
         LA    R1,IORD+IOCONFIL+IO2                                             
         GOTO1 AIO                 READ ID RECORD                               
         BNE   ERNF                RECORD MUST BE PRESENT & CORRECT             
         L     R2,AIOAREA2                                                      
         LA    R3,CTIDATA                                                       
         SR    RF,RF                                                            
VKUSR20  CLI   0(R3),0             FIND PASSIVE POINTER                         
         JE    *+2                                                              
         CLI   0(R3),X'02'                                                      
         BE    *+14                                                             
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VKUSR20                                                          
         MVC   IDSID,2(R3)                                                      
         MVI   IDSIDH+5,8                                                       
         NI    IDSIDH+4,X'F7'                                                   
         OI    IDSIDH+6,X'80'                                                   
         B     VALKEY                                                           
VKUSRX   EQU   *                                                                
*                                                                               
***********************************************************************         
* VALIDATE THIS/LAST ACTIONS                                          *         
***********************************************************************         
VKIO     MVC   APRECKEY(L'CTIKEY),CTIKEY                                        
         LA    R1,IORD+IOCONFIL+IO1                                             
         GOTO1 AIO                                                              
         BNE   VALKEYX             I/O ERROR EXIT                               
         MVI   APINDS,APIOKDIS                                                  
         CLI   APACTN,ACTDIS                                                    
         BNE   EXITOK                                                           
*                                                                               
         L     R1,AIOAREA1                                                      
A        USING CTIREC,R1                                                        
         LA    R3,A.CTIDATA                                                     
         SR    RF,RF                                                            
VKUSR20A CLI   0(R3),0             FIND PASSIVE POINTER                         
         JE    *+2                                                              
         CLI   0(R3),X'06'                                                      
         BE    *+14                                                             
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VKUSR20A                                                         
         USING CTAGYD,R3                                                        
         MVC   AAGY,CTAGYID                                                     
         DROP  R3                                                               
         DROP  A                                                                
**                                                                              
         MVC   LCLAGY,AAGY        | GET SECURITY AGENCY FOR                     
         BAS   RE,EXTSECID        | THE ALPHA AGENCY                            
         BNE   ENTA               | ERROR - NOT AUTHORIZED                      
         MVC   SAGY,LCLAGY        |                                             
**                                                                              
         MVC   LCLAGY,LAAGY       | GET SECURITY AGENCY FOR                     
         BAS   RE,EXTSECID        | THE ALPHA AGENCY                            
         BNE   ENTA               | ERROR - NOT AUTHORIZED                      
         MVC   LSAGY,LCLAGY       |                                             
**                                                                              
         CLC   LAAGY,LSAGY         LOGGED IN W/ SECURITY AGYENCY?               
         BNE   CA020               NO                                           
         CLC   LSAGY,SAGY          SETUP AGY HAS SAME SECURITY AGY?             
         BNE   ENTA                ERROR - NOT AUTHORIZED                       
         B     EXITOK              YES: ALLOWED                                 
*                                                                               
CA020    CLC   LAAGY,AAGY          LOGGED IN W/ SETUP AGENCY?                   
         BNE   ENTA                ERROR - NOT AUTHORIZED                       
*                                                                               
EXITOK   EQU   *                                                                
VKSYS    MVI   SYSTEM,0            SET SYSTEM NOT INPUT                         
         GOTO1 AFVAL,IDSSYSH                                                    
         BNE   VKSYS12                                                          
         USING SYSLSTD,RE                                                       
         L     RE,ASYSLST          CHECK IN TABLE OF VALID SYSTEMS              
         LA    RE,6(RE)                                                         
         LA    RE,SYSLLEN(RE)      GO PAST SERVICE SYSTEM ENTRY                 
         LLC   RF,FVXLEN                                                        
VKSYS10  CLI   SYSLNUM,0                                                        
         BE    EIIF                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),SYSLNAME                                               
         BE    VKSYS20                                                          
         LA    RE,SYSLLEN(RE)                                                   
         B     VKSYS10                                                          
*                                  DEFAULT TO LIST SYSTEM FILTER OR             
VKSYS12  MVC   SYSTEM,SELSYS       FIRST VALID SYSTEM IN RECORD                 
         CLI   SYSTEM,0                                                         
         BE    VKSYSX                                                           
         GOTO1 ADISSYS,SYSTEM      GET SYSTEM LIST ENTRY FROM NUMBER            
         ICM   RE,15,APPARM                                                     
         BZ    VKSYSX                                                           
*                                                                               
VKSYS20  MVC   SYSTEM,SYSLNUM      SET SYSTEM NUMBER FROM LIST                  
         MVC   SFLAGS,SYSLIND1     AND SET INPUT FLAGS                          
         CLI   SYSLRPLT,C'C'       NO AGENCY BINARY FOR CONTROL SYSTEM          
         BNE   *+8                                                              
         MVI   SFLAGS,0                                                         
         MVC   IDSSYS(7),SYSLNAME  DISPLAY FULL SYSTEM NAME                     
         OI    IDSSYSH+6,X'80'                                                  
VKSYSX   EQU   *                                                                
         DROP  RE                                                               
         MVC   SAVKEY,APRECKEY                                                  
         MVC   FVMSGNO,=AL2(FVFOK)                                              
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO EXTRACT SECURTY ALPHA ID USING AGENCY ALPHA              *         
* WORK ON RECORD TYPE '05'                                                      
***********************************************************************         
EXTSECID NTR1                                                                   
         LA    R1,IOKEY                                                         
         USING CT5REC,R1             R1=A(AGENCY ACCESS RECORD)                 
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,LCLAGY       AGENCY ALPHA                               
         GOTO1 AIO,IORD+IOCONFIL+IO3                                            
         BNE   GA0XX                                                            
*                                                                               
         L     R1,AIOAREA3                                                      
         LA    R3,CT5DATA            R3=A(FIRST ELEMENT)                        
GA010    CLI   0(R3),0               TEST EOR                                   
         JE    GA0XX                                                            
         CLI   0(R3),CTSEAELQ        X'B8' SECURITY AGENCY ALPHA ID             
         BE    GA020                                                            
         CLI   0(R3),CTAADELQ        X'B9' AGENCY ACCESS DETAILS                
         BE    GA030                                                            
GAX10    LLC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GA010                                                            
*                                                                               
         USING CTSEAD,R3                                                        
GA020    MVC   LCLAGY,CTSEAAID       SECURITY AGENCY ALPHA ID                   
         B     GAX10                                                            
*                                                                               
         USING CTAADD,R3                                                        
GA030    MVC   PASEXP,CTAADPTO       PASSWORD EXPIRE DAYS                       
         B     GAX10                                                            
         DROP  R3                                                               
GA0XX    XIT1                                                                   
         EJECT                                                                  
                                                                                
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF USER ID RECORD                            *         
***********************************************************************         
DISKEY   LA    R2,APRECKEY                                                      
         USING CTIREC,R2                                                        
         MVC   IDSID,CTIKID                                                     
*                                                                               
DISKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY USER ID RECORD                                   *         
***********************************************************************         
DISREC   L     R2,AIOAREA1                                                      
         USING CTIREC,R2                                                        
         TWAXC IDSAGYAH                                                         
         XC    IDSSEAG,IDSSEAG     CLEAR SENAME/AGENCY# PROT. FIELD             
         OI    IDSSEAGH+6,X'80'                                                 
*                                                                               
         XC    XXCNT(XXCNTL),XXCNT ZERO ALL COUNTERS                            
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTIDOELQ                                                  
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R1,15,APPARM                                                     
         BZ    DREC10                                                           
         USING CTIDOEL,R1                                                       
         CLI   CTIDOLEN,CTIDOLNQ                                                
         BNE   DREC10                                                           
         TM    CTIDOFL1,CTIDOFSN                                                
         BZ    DREC10                                                           
         MVI   IDSPID,C'='                                                      
         MVC   IDSPID+1(8),CTIDOSID                                             
         B     DRDAT100                                                         
         DROP  R1                                                               
*                                                                               
DREC10   CLI   SYSTEM,0                                                         
         BE    DREC20                                                           
         GOTO1 DISSE,SYSTEM        GET A(SELIST) ENTRY INTO ASE                 
         OC    APPARM,APPARM                                                    
         JZ    *+2                                                              
         MVC   ASE,APPARM                                                       
*                                                                               
DREC20   MVC   IDSAGYA,=C'??'      FUDGE FOR CONVERSION PROBLEM                 
         XC    ASYSEL,ASYSEL       SET A(SYSTEM ELEMENT)                        
         LA    R3,CTIDATA                                                       
*                                                                               
DRDAT20  CLI   0(R3),0                                                          
         BE    DRDAT50                                                          
         CLI   0(R3),X'03'         X'03' PRINCIPAL ID                           
         BE    DRPID                                                            
         CLI   0(R3),CTDSTELQ      X'30' DESTINATION DETAIL                     
         BE    DRDDT                                                            
         CLI   0(R3),CTORGELQ      X'36' ORIGIN DETAIL                          
         BE    DRORG                                                            
         CLI   0(R3),CTSYSELQ      X'21' SYSTEM                                 
         BE    DRSYS                                                            
         CLI   0(R3),CTIDOELQ      X'07' ID OPTIONS ELEMENT                     
         BE    DROPT                                                            
         CLI   0(R3),CTAGYELQ      X'06' AGENCY ID                              
         BE    DRAGY                                                            
         CLI   0(R3),X'02'         X'02' ID# POINTER                            
         BNE   DRDAT40                                                          
         CLI   1(R3),X'04'         CHECK NUMBER POINTER                         
         BNE   *+10                                                             
         MVC   IDNUM,2(R3)                                                      
         XC    IDSIDN,IDSIDN                                                    
         OI    IDSIDNH+6,X'80'                                                  
         EDIT  (B2,IDNUM),(5,IDSIDN),ALIGN=LEFT                                 
*                                                                               
DRDAT40  LLC   R4,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R4                                                            
         B     DRDAT20                                                          
         EJECT                                                                  
***********************************************************************         
* PUT BLOCKS TO TWA                                                   *         
***********************************************************************         
DRDAT50  EQU   *                                                                
*                                                                               
         MVC   IDSSEID,SAGY                                                     
         OI    IDSSEIDH+6,X'80'                                                 
*                                                                               
         EDIT  PASEXP,(3,IDSPEXP),WRK=APWORK,DUB=APDUB,ALIGN=LEFT               
*                                                                               
         OC    ASYSEL,ASYSEL       PROGRAM ACCESS                               
         BNZ   DRDAT70                                                          
         CLI   SYSTEM,0                                                         
         BE    DRDAT100                                                         
         MVC   IDSPA1(10),=CL10'NO ACCESS'                                      
         OI    IDSPA1H+6,X'80'                                                  
         B     DRDAT100                                                         
DRDAT70  L     RF,=A(DISPSYS)                                                   
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         L     R3,ASYSEL                                                        
         USING CTSYSD,R3                                                        
         MVC   AGNUM,CTSYSAGB                                                   
         BAS   RE,DISPSEAG         DISPLAY SENAME/AGENCY BINARY#                
*                                                                               
DRDAT90  GOTO1 ADISLACC,APPARM,(CTSYSNUM,CTSYSLMT),AGAID                        
*                                                                               
         OC    APWORK,APWORK                                                    
         BZ    DRDAT95                                                          
*&&US                                                                           
         CLC   APWORK(2),=C'**'                                                 
         BNE   CHKSTR                                                           
         MVC   IDSACCS(7),=CL7'OFFICE='                                         
         MVC   IDSACCS+7(2),APWORK+2                                            
         B     DRDAT95                                                          
*                                                                               
CHKSTR   CLC   =C'CG=',APWORK                                                   
         BE    CHKST10                                                          
         CLI   APWORK,C'*'                                                      
         BNE   CHK$                                                             
         CLI   APWORK+2,C' '                                                    
         BNH   CHKO                                                             
         MVC   IDSACCS+13(4),APWORK                                             
         B     *+10                                                             
CHKST10  MVC   IDSACCS+13(6),APWORK+3                                           
         MVC   IDSACCS(13),=CL13'CLIENT GROUP='                                 
         B     DRDAT95                                                          
*                                                                               
CHKO     MVC   IDSACCS(7),=CL7'OFFICE='                                         
         MVC   IDSACCS+7(2),APWORK+1                                            
         B     DRDAT95                                                          
CHK$     CLI   APWORK,C'$'                                                      
         BNE   CHKPLS                                                           
         MVC   IDSACCS(12),=CL12'OFFICE LIST='                                  
         MVC   IDSACCS+12(2),APWORK+1                                           
         B     DRDAT95                                                          
CHKPLS   CLI   APWORK,C'+'                                                      
         BNE   CHKEQL                                                           
         MVC   IDSACCS(7),=CL7'MARKET='                                         
         MVC   IDSACCS+7(2),APWORK+1                                            
         B     DRDAT95                                                          
*                                                                               
CHKEQL   CLC   APWORK(2),=C'=='                                                 
         BNE   ITSCLNT                                                          
         MVC   IDSACCS(11),=CL11'DATA ACCESS'                                   
         B     DRDAT95                                                          
*                                                                               
ITSCLNT  MVC   IDSACCS(7),=CL7'CLIENT='                                         
         MVC   IDSACCS+7(4),APWORK                                              
*&&                                                                             
*&&UK*&& MVC   IDSACCS,APWORK                                                   
DRDAT95  EQU   *                                                                
*                                                                               
         LLC   R0,PGCNT                                                         
         GOTO1 =V(SCINKEY),APPARM,(3,IDSPA1H),(20,BLOCK1),(R0),        *        
               RR=APRELO                                                        
*                                                                               
DRDAT100 MVI   IDSSHD,C'-'         DISPLAY LIST OF SYSTEMS                      
         MVC   IDSSHD+1(L'IDSSHD-1),IDSSHD                                      
         CLI   SYCNT,0                                                          
         BE    DRDAT110                                                         
         SR    RF,RF                                                            
         ICM   RF,1,SYSNAMSL       RF=L'SYSTEM NAMES LIST                       
         BZ    DRDAT110                                                         
         LA    R1,L'IDSSHD                                                      
         SR    R1,RF                                                            
         BNP   DRDAT110                                                         
         SRL   R1,1                                                             
         LA    RE,IDSSHD(R1)                                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),SYSNAMS                                                  
DRDAT110 OI    IDSSHDH+6,X'80'                                                  
*                                                                               
DRDATX   MVC   SAVSYS,SYSTEM       SAVE LAST DISPLAYED SYSTEM                   
         CLI   LIDFNDX,0           CHECK COMPATIBLE ID DUPE MESSAGE             
         BNE   EXIT                                                             
         GOTO1 ADISACT,CTIREC                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SAVE A(SYSTEM ELEMENT) IF IT'S THE ONE REQUESTED                    *         
***********************************************************************         
DRSYS    CLI   SYSTEM,0                                                         
         BE    DRSYS10                                                          
         USING CTSYSD,R3                                                        
         CLC   CTSYSNUM,SYSTEM                                                  
         BNE   *+8                                                              
         ST    R3,ASYSEL                                                        
DRSYS10  MVC   SYSNUMS,CTSYSNUM                                                 
         L     RF,=A(GETSEN)       GET SE NAME AND ADD TO DISPLAY LIST          
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         B     DRDAT40                                                          
         DROP  R3                                                               
                                                                                
***********************************************************************         
* DISPLAY PRINCIPAL ID                                                *         
***********************************************************************         
DRPID    EDIT  (B2,2(R3)),(5,IDSPID),FILL=0                                     
         L     R4,AIOAREA2         SWITCH IO AREAS                              
         DROP  R2                                                               
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY       BUILD KEY OF ID RECORD                       
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,2(R3)                                                    
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BNE   DRPIDX                                                           
         MVC   IOKEY,APRECKEY                                                   
         LA    R1,CTIDATA                                                       
         SR    RE,RE                                                            
DRPID20  CLI   0(R1),0             FIND POINTER ELEMENT AND DISPLAY ID          
         BE    DRPIDX                                                           
         CLI   0(R1),X'02'                                                      
         BE    *+14                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     DRPID20                                                          
         MVC   IDSPID,2(R1)                                                     
DRPIDX   B     DRDAT40                                                          
         DROP  R4                                                               
         USING CTIREC,R2                                                        
         EJECT                                                                  
***********************************************************************         
* DISPLAY DESTINATION DETAIL ELEMENT                                  *         
***********************************************************************         
         USING CTDSTD,R3                                                        
DRDDT    MVC   IDSDNAM,CTDSTNAM                                                 
         MVI   IDSDNAMH+5,33                                                    
         MVI   IDSDNAMH+7,33                                                    
         OI    IDSDNAMH+6,X'80'                                                 
         MVC   IDSDADD,CTDSTADD                                                 
         MVI   IDSDADDH+5,33                                                    
         MVI   IDSDADDH+7,33                                                    
         OI    IDSDADDH+6,X'80'                                                 
         MVC   IDSDAD2,SPACES                                                   
         MVI   IDSDAD2H+5,33                                                    
         MVI   IDSDAD2H+7,33                                                    
         OI    IDSDAD2H+6,X'80'                                                 
         MVC   IDSDAD3,SPACES                                                   
         MVI   IDSDAD3H+5,33                                                    
         MVI   IDSDAD3H+7,33                                                    
         OI    IDSDAD3H+6,X'80'                                                 
         CLI   CTDSTLEN,166                                                     
         BL    DRDDT10                                                          
         MVC   IDSDAD2,CTDSTAD2                                                 
         MVC   IDSDAD3,CTDSTAD3                                                 
DRDDT10  MVC   IDSDLO1,CTDSTLG1                                                 
         MVI   IDSDLO1H+5,7                                                     
         MVI   IDSDLO1H+7,7                                                     
         OI    IDSDLO1H+6,X'80'                                                 
         OC    CTDSTLG2,CTDSTLG2                                                
         BZ    DRDDT20                                                          
         MVC   IDSDLO2,CTDSTLG2                                                 
         MVI   IDSDLO2H+5,7                                                     
         MVI   IDSDLO2H+7,7                                                     
         OI    IDSDLO2H+6,X'80'                                                 
         SPACE 1                                                                
DRDDT20  MVC   IDSDPWR,CTDSTPOW                                                 
         MVI   IDSDPWRH+5,4                                                     
         MVI   IDSDPWRH+7,4                                                     
         OI    IDSDPWRH+6,X'80'                                                 
         B     DRDAT40                                                          
***********************************************************************         
* DISPLAY ORIGIN DETAIL ELEMENT                                       *         
***********************************************************************         
         USING CTORGD,R3                                                        
DRORG    MVC   IDSONAM,CTORGNAM                                                 
         MVI   IDSONAMH+5,33                                                    
         MVI   IDSONAMH+7,33                                                    
         OI    IDSONAMH+6,X'80'                                                 
         MVC   IDSOADD,CTORGADD                                                 
         MVI   IDSOADDH+5,33                                                    
         MVI   IDSOADDH+7,33                                                    
         OI    IDSOADDH+6,X'80'                                                 
         B     DRDAT40                                                          
         EJECT                                                                  
***********************************************************************         
* DISPLAY ID OPTIONS                                                  *         
***********************************************************************         
         USING CTIDOEL,R3                                                       
DROPT    MVC   IDOPTS(1),CTIDOFL1  PROCESS USERID OPTIONS ELEMENT               
         MVC   IDOPTS+1(1),CTIDOFL2                                             
*                                                                               
DROP020  TM    IDOPTS,CTIDOFGN     TEST FOR GEBERIC USER-ID                     
         BZ    DROP030                                                          
         MVC   IDSPID(7),=C'GENERIC'                                            
DROP030  CLI   CTIDOLEN,CTIDOLNQ                                                
         BNE   DROPX                                                            
         MVC   SYNNUM,CTIDOSNU                                                  
         MVC   SYNID,CTIDOSID                                                   
*                                                                               
DROP040  TM    IDOPTS,CTIDOFSN     TEST FOR USERID IS SYNONYM                   
         BZ    DROPX                                                            
         MVI   IDSPID,C'='                                                      
         MVC   IDSPID+1(8),SYNID                                                
         MVC   IDSAGYA,=C'  '      CLEAR AGENCY ID FIELD                        
*                                                                               
DROPX    B     DRDAT40                                                          
         DROP  R3                                                               
         USING CTIREC,R2                                                        
         EJECT                                                                  
***********************************************************************         
* DISPLAY AGENCY ALPHA AND LANGUAGE                                   *         
***********************************************************************         
         USING CTAGYD,R3                                                        
DRAGY    MVC   IDSAGYA,CTAGYID                                                  
         MVC   AGAID,CTAGYID                                                    
         L     RF,=A(GETACC)       GET ACCESS RECORD INTO IOAREA3               
         A     RF,APRELO           AND SAVE ID COUNTRY CODE                     
         BASR  RE,RF                                                            
         BNE   EADL                                                             
         L     R1,ALANG                                                         
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING LANGTABD,R1         R1=A(LANGUAGE TABLE)                         
         CLC   CTAGYLNG,LANGCODE   MATCH CODE TO TABLE                          
         BE    *+10                                                             
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
         MVC   IDSLANG,LANGFUL                                                  
         B     DRDAT40                                                          
         DROP  R1,R3                                                            
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY SENAME AND AGENCY BINARY NUMBER IN ONE FIELD     *         
***********************************************************************         
DISPSEAG NTR1                                                                   
         XC    IDSSEAG,IDSSEAG                                                  
         OI    IDSSEAGH+6,X'80'                                                 
         L     RF,ASE                                                           
         USING SELISTD,RF                                                       
         MVC   IDSSEAG(L'SENAME),SENAME                                         
         LA    R0,L'SENAME                                                      
         DROP  RF                                                               
         LA    R4,IDSSEAG                                                       
DSEAG10  CLI   0(R4),0                                                          
         BE    DSEAG20                                                          
         CLI   0(R4),C' '                                                       
         BE    DSEAG20                                                          
         LA    R4,1(R4)                                                         
         BCT   R0,DSEAG10                                                       
*                                                                               
DSEAG20  OC    AGNUM,AGNUM                                                      
         BZ    DSEAGX                                                           
         MVI   0(R4),C'/'                                                       
         LA    R4,1(R4)                                                         
         GOTO1 VHEXOUT,APPARM,AGNUM,(R4),1,=C'TOG'                              
*                                                                               
DSEAGX   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                               *         
***********************************************************************         
VALSEL   LA    R2,APRECKEY                                                      
         USING CTIREC,R2                                                        
         XC    SELDATA,SELDATA                                                  
*                                                                               
         LA    R4,LSTIDH                                                        
         L     RF,=A(VALPARS)                                                   
         A     RF,APRELO                                                        
         BASR  RE,RF               GO VALIDATE I/P PARAMETERS                   
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   EXIT                INVALID                                      
         XC    APPARM(24),APPARM                                                
*                                                                               
         XC    CTIKEY,CTIKEY       BUILD AN INITIAL KEY                         
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID(L'SELID),SELID                                            
*                                                                               
         MVI   GETSEQF,0           INTERNAL READ SEQUENCE FLAG                  
         LA    R0,LSTACT1H         SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM+0                                                      
         MVC   APPARM+4(2),=H'0'   SET NUMBER OF LIST LINES                     
         LA    R1,LSTACT2H         SET LIST LINE LENGTH                         
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
*                                                                               
VALSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
GETSEL   LA    R2,IOKEY            READ NEXT LIST RECORD                        
         USING CTIREC,R2                                                        
         MVC   CTIKEY,APRECKEY                                                  
         TM    GETSEQF,APILRERD    TEST GETSEL READ SEQUENCE BROKEN             
         BZ    GSEL02                                                           
         NI    GETSEQF,X'FF'-APILRERD                                           
         B     GSEL04                                                           
GSEL02   TM    APINDS,APILRERD     TEST LIST READ SEQUENCE BROKEN               
         BZ    GSEL10                                                           
         NI    APINDS,X'FF'-APILRERD                                            
GSEL04   GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    GSEL20                                                           
         B     GETSELN                                                          
GSEL10   TM    APINDS,APILNSEQ     TEST FIRST LINE IN LIST SEQUENCE             
         BNZ   GSEL20A                                                          
*                                  READ PAST PASSIVE # RECORDS                  
         OC    CTIKID(L'CTIKID-L'CTIKNUM),CTIKID                                
         BNZ   *+8                                                              
         MVI   CTIKID+L'CTIKID-L'CTIKNUM-1,1                                    
*                                                                               
         LA    R1,IOCONFIL+IOHI+IO1                                             
         L     RF,=A(GETREC)       GO SELECT NEXT RECORD                        
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   GETSELN                                                          
         B     GETSELY                                                          
GSEL20A  NI    APINDS,X'FF'-APILRERD                                            
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BNE   GETSELN                                                          
GSEL20   LA    R1,IOCONFIL+IOSQ+IO1                                             
         L     RF,=A(GETREC)       GO SELECT NEXT RECORD                        
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         BNE   GETSELN             (EOF)                                        
*                                                                               
GETSELY  L     R2,AIOAREA1                                                      
         MVC   APRECKEY(L'CTIKEY),CTIKEY                                        
         MVC   APRECDA,IODA        SAVE DISK ADDRESS                            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GETSELN  MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
*                                                                               
GETSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                            *         
***********************************************************************         
         USING CTIREC,R2                                                        
DISSEL   CLI   APACTN,ACTLST       FOR LIST ONLY                                
         BNE   DISSEL2                                                          
         OI    LSTHDLNH+6,X'80'                                                 
         MVC   LSTHDLN+24(18),=C'Systems Authorised'                            
         MVC   LSTHDLN+44(8),SPACES                                             
         OI    LSTHDLUH+6,X'80'                                                 
         MVC   LSTHDLU+44(8),SPACES                                             
*                                                                               
DISSEL2  L     R4,APPARM           BUILD A LINE OF USER ID REC DATA             
         L     RF,=A(LINE)                                                      
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
*                                                                               
DISSELX  B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO INITIALIZE TWA / SCREEN BEFORE DISPLAYING IT BACK        *         
***********************************************************************         
SETTWA   XC    SAVOVER(SAVCLRL),SAVOVER                                         
         TM    CUSTAT,CUSDDS                                                    
         BNZ   SETEXIT                                                          
         OI    IDSIDLH+1,FHATLO            LOW INTENSITY                        
         OI    IDSIDNH+1,FHATLO            LOW INTENSITY                        
         MVC   IDSIDN,SPACES                                                    
         OI    IDSIDNH+1,FHATPR            PROTECTED                            
         OI    IDSSEHH+1,FHATLO            LOW INTENSITY                        
         OI    IDSSEAGH+1,FHATLO           LOW INTENSITY                        
         MVC   IDSSEAG,SPACES                                                   
         OI    IDSSEAGH+1,FHATPR           PROTECTED                            
SETEXIT  EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GETTXT MESSAGE NUMBER ERROR EXITS                                   *         
***********************************************************************         
EIIF     MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  INPUT FIELD ERROR                            
ENTA     MVC   FVMSGNO,=AL2(FVFNAREC)                                           
         B     NO                  NOT AUTHORISED FOR RECORD                    
EFTS     MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     NO                  INPUT FIELD TOO SHORT                        
ERNF     MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     NO                  RECORD NOT FOUND                             
EFTB     MVC   FVMSGNO,=AL2(CE#FVMAX)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  FIELD VALUE > MAX                            
EADL     MVC   FVMSGNO,=AL2(CE#ACDEL)                                           
         MVC   FVOSYS,ASSYSE                                                    
         LA    R1,IDSAGYAH         SET CURSOR                                   
         ST    R1,FVADDR                                                        
         B     NO                  ACCESS RECORD IS DELETED                     
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND LITERALS                                              *         
***********************************************************************         
SPACES   DC    16C' '                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB,RA,R8                                                         
***********************************************************************         
* FOLLOWING TABLES/SUBROUTINES ARE ONLY ADDRESSABLE VIA =A(.....)     *         
***********************************************************************         
*                                                                               
***********************************************************************         
* ROUTINE TO READ ACCESS RECORD INTO IOAREA3                          *         
* NTRY - AGAID=AGENCY ALPHA ID                                        *         
* EXIT - IDCTRY=COUNTRY CODE                                          *         
***********************************************************************         
GETACC   NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         MVI   IDCTRY,0                                                         
         MVC   KEYSAVE,IOKEY                                                    
         L     R1,AIOAREA3                                                      
         USING CT5REC,R1                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGAID                                                   
         MVC   IOKEY(L'CT5KEY),CT5KEY                                           
         DROP  R1                                                               
         GOTO1 AIO,IOREAD+IOCONFIL+IO3                                          
         MVC   IOKEY,KEYSAVE                                                    
         JL    *+2                                                              
         BH    GETACCNO                                                         
         L     R1,AIOAREA3                                                      
         LA    R1,CT5DATA-CT5REC(R1)                                            
         SR    RF,RF                                                            
*                                  SAVE AGENCY COUNTRY CODE                     
         USING CTAGDD,R1                                                        
GETACC4  CLI   CTAGDEL,0                                                        
         JE    *+2                                                              
         CLI   CTAGDEL,CTAGDELQ                                                 
         BE    *+14                                                             
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     GETACC4                                                          
         CLI   CTAGDLEN,CTAGDLNQ                                                
         BNH   GETACCOK                                                         
         MVC   IDCTRY,CTAGDCTY                                                  
         DROP  R1                                                               
         B     GETACCOK                                                         
*                                                                               
GETACCOK SR    RC,RC               RETURN CC EQUAL                              
GETACCNO LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD PROGRAM ACCESS LIST INTO BLOCK1 AND SET PGCNT TO NUMBER OF    *         
* ENTRIES IN BLOCK.                                                   *         
***********************************************************************         
DISPSYS  NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         L     R3,ASYSEL                                                        
         USING CTSYSEL,R3                                                       
         GOTO1 DISSE,CTSYSSE                                                    
         OC    APPARM,APPARM                                                    
         JZ    *+2                                                              
         MVC   ASE,APPARM                                                       
         L     RE,ASE                                                           
         MVC   APGM,SEPGMS-SELISTD(RE)                                          
         LA    R4,CTSYSPGM         R4=A(AUTHS)                                  
         LLC   R9,CTSYSLEN         R9=LENGTH                                    
         MVI   PGCNT,0             SET COUNT                                    
         CLI   CTSYSLEN,16         CHECK FOR ALL=VALUE ONLY                     
         BE    DISPSYS8                                                         
*                                                                               
DISPSYS2 CHI   R9,16                                                            
         BNH   DISPSYS8                                                         
         MVC   PROGRAM,0(R4)                                                    
         LA    R4,1(R4)            POINT TO THE AUTHORIZATION CODE              
*                                  GET PROGRAM NAME                             
         L     RF,=A(GETPGAN)                                                   
         A     RF,APRELO                                                        
         BASR  RE,RF                                                            
         CLI   PROGRAM,X'FF'       CHECK IF NOT FOUND                           
         BE    DISPSYS6                                                         
*                                                                               
DISPSYS4 LLC   R1,PGCNT            BUMP BLOCK COUNT                             
         LA    RE,1(R1)                                                         
         STC   RE,PGCNT                                                         
         MHI   R1,20                                                            
         LA    R1,BLOCK1(R1)       GET A(BLOCK ENTRY)                           
         MVI   0(R1),C' '                                                       
         MVC   1(19,R1),0(R1)                                                   
         LR    R8,R1                                                            
         MVC   0(4,R8),PGNAME                                                   
         LA    R8,4(R8)                                                         
         CLI   0(R8),C' '                                                       
         BNE   *+8                                                              
         BCT   R8,*-8                                                           
         MVI   1(R8),C'='          AUTH IS Y N OR XXXX                          
         MVI   2(R8),C'Y'          AUTH IS Y N OR XXXX                          
         CLC   0(2,R4),=XL2'000F'                                               
         BE    DISPSYS6                                                         
         MVI   2(R8),C'N'                                                       
         CLC   0(2,R4),=XL2'0000'                                               
         BE    DISPSYS6                                                         
         GOTO1 VHEXOUT,APPARM,(R4),2(R8),2,=C'TOG'                              
*                                                                               
DISPSYS6 LA    R8,CTSYSALL         EXIT IF ALL=VALUE JUST DONE                  
         CR    R8,R4                                                            
         BE    DISPSYSX                                                         
         LA    R4,2(R4)            NEXT PROGRAM                                 
         AHI   R9,-3                                                            
         B     DISPSYS2                                                         
*                                                                               
DISPSYS8 LA    R4,CTSYSALL                                                      
         MVC   PGNAME,=CL8'ALL'                                                 
         B     DISPSYS4                                                         
*                                  DISPLAY REST OF ELEMENT                      
DISPSYSX XIT1                                                                   
         DROP  R3                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SET PROGRAM NAME FROM PROGRAM ACCESS NUMBER                         *         
***********************************************************************         
GETPGAN  NTR1  BASE=*                                                           
         L     R3,APGM                                                          
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING PGMLSTD,R3                                                       
*                                                                               
GETPGAN2 CLC   PGMNUM,PROGRAM      MATCH ON PROGRAM NUMBER                      
         BE    GETPGANY                                                         
         CLI   PGMALNUM,0                                                       
         BE    *+14                                                             
         CLC   PGMALNUM,PROGRAM    OR ACCESS OVERRIDE (IF SET)                  
         BE    GETPGANY                                                         
GETPGAN4 BXLE  R3,RE,GETPGAN2                                                   
         MVI   PROGRAM,X'FF'       SET PROGRAM=X'FF' IF N/F                     
         B     GETPGANX                                                         
GETPGANY CLC   PGMCTRY,IDCTRY                                                   
         BE    *+12                                                             
         CLI   PGMCTRY,0                                                        
         BNE   GETPGAN4                                                         
         TM    CUSTAT,CUSDDS               CHECK IF INTERNAL DDS                
         BNZ   GETPGDD                     YES, DISPLAY ALL PROGRAM             
         TM    PGMIND,PGMIACC              IF INTERNAL PGM                      
         BO    GETPGAN4                    THEN SKIP IT                         
GETPGDD  MVC   PGNAME,PGMNAME                                                   
GETPGANX XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD LIST OF SYSTEM SHORT NAMES IN DISPLAY HEADER BUFFER           *         
***********************************************************************         
GETSEN   NTR1  LABEL=*,BASE=*                                                   
*                                                                               
GETSEN1  CLI   SYCNT,0             TEST FIRST CALL - NUMBER OF ITEMS            
         BNE   GETSEN2                                                          
         MVI   SYSNAMS,C' '                                                     
         MVC   SYSNAMS+1(L'SYSNAMS-1),SYSNAMS                                   
         MVC   SYSNAMS(7),=C'USER ID'                                           
         NC    SYSNAMS+1(3),=8X'BF'                                             
         MVC   SYSNAMS+9(7),=C'SYSTEMS'                                         
         NC    SYSNAMS+10(6),=8X'BF'                                            
         LA    RE,SYSNAMS+18       SET A(NEXT ENTRY)                            
         ST    RE,ASYSNAMS                                                      
*                                                                               
GETSEN2  L     R3,ASYS             SEARCH SE LIST FOR SE NUM AT SYSNUMS         
         L     R3,VSELIST-SYSFACD(R3)                                           
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)            R3=A(SELIST ENTRY)                           
         USING SELISTD,R3                                                       
         CLC   SYSNUMS,SEOVSYS                                                  
         BE    GETSEN3                                                          
         BXLE  R3,RE,*-10                                                       
         LA    R3,=CL7'XXX    '    SET UNKNOWN SYSTEM NAME                      
*                                                                               
GETSEN3  L     RE,ASYSNAMS         MOVE NAME TO LIST                            
         SR    R1,R1                                                            
         ICM   R1,1,SYCNT          TEST COUNT OF ITEMS IN LIST                  
         BZ    *+12                                                             
         MVI   0(RE),C'/'                                                       
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,SYCNT            BUMP ITEM COUNT                              
         LA    RF,SYSNAMS+L'SYSNAMS-3                                           
         CR    RE,RF                                                            
         BH    GETSENX                                                          
         MVC   0(3,RE),SENAME      EXTRACT SE NAME                              
         NC    1(2,RE),=8X'BF'     SET TO LOWER CASE                            
         LA    RE,3(RE)                                                         
         ST    RE,ASYSNAMS                                                      
         LA    R1,SYSNAMS                                                       
         SR    RE,R1                                                            
         STC   RE,SYSNAMSL         SET LIST LENGTH                              
*                                                                               
GETSENX  XIT1                                                                   
         DROP  R3                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE I/P PARAMETERS FOR LIST/REPORT                             *         
* R4=A(FIRST FIELD HEADER IN STANDARD DISPLAY)                        *         
* APPLICABLE TO BOTH LIST AND REPORT SCREEN FIELD OFFSETS             *         
***********************************************************************         
         USING CTIREC,R2                                                        
         USING LSTIDH,R4                                                        
VALPARS  NTR1  LABEL=*,BASE=*                                                   
*                                                                               
VPID     GOTO1 AFVAL,LSTIDH        STORE USER ID IF ENTERED                     
         BNE   VPIDX                                                            
         LLC   R1,FVILEN                                                        
         L     RE,=F'-1'                                                        
         LA    RF,FVIFLD                                                        
VPID1    CLI   0(RF),C'A'          FIND LENGTH TO 1ST SP CHAR                   
         BL    VPID2               FOR KEY COMPARE IN GETREC                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,VPID1                                                         
VPID2    STC   RE,SELKEYCL                                                      
         MVC   SELID,FVIFLD                                                     
         MVC   SELIDL,FVILEN                                                    
         MVC   SELIDSP,0(RF)                                                    
VPIDX    EQU   *                                                                
*                                                                               
VPSYS    GOTO1 AFVAL,LSTSYSH       VALIDATE SYSTEM                              
         BNE   VPSYSX                                                           
         L     RF,ASYSLST          LOOK UP NAME IN SYSLST                       
         LA    RF,6(RF)                                                         
         USING SYSLSTD,RF                                                       
         LLC   RE,FVILEN                                                        
         BCTR  RE,0                                                             
VPSYS3   CLI   SYSLNUM,0                                                        
         BE    VPSYS1                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SYSLNAME(0),FVIFLD                                               
         BE    VPSYS4                                                           
         LA    RF,SYSLLEN(RF)                                                   
         B     VPSYS3                                                           
VPSYS4   MVC   SELSYS,SYSLNUM                                                   
         B     VPSYSX                                                           
*                                                                               
VPSYS1   CLI   FVILEN,L'SENAME                                                  
         BH    VPSYSE                                                           
         MVC   APWORK(L'SENAME),FVIFLD                                          
         GOTO1 GETSE               GET SYSTEM LIST INFO                         
         BNE   VALPARSX                                                         
         MVC   SELSEN,APWORK                                                    
         B     VPSYSX                                                           
VPSYSE   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALPARSX                                                         
VPSYSX   EQU   *                                                                
* WE ARE REMOVING PROGRAM FILTER ON LIST SCREEN                                 
*                                                                               
**VPPGM    GOTO1 AFVAL,LSTPGMH       VALIDATE PROGRAM                           
**       BNE   VPPGMX                                                           
**       OC    SELSYS,SELSYS                                                    
**       BNZ   VPPGM2                                                           
**       MVC   FVMSGNO,=AL2(CE#PFNVS)                                           
**       MVC   FVOSYS,ASSYSE                                                    
**       B     VALPARSX            PROGRAM NAME INVALID WITHOUT SYS             
**VPPGM2   GOTO1 AVALPGM,APPARM,(SELSYS,LSTPGMH)                                
**       BNE   VPPGM1                                                           
**       MVC   SELPGM,APWORK                                                    
**       B     VPPGMX                                                           
**VPPGM1   MVC   FVMSGNO,=AL2(FVFNOTV)                                          
**       B     VALPARSX                                                         
**VPPGMX   EQU   *                                                              
*                                                                               
VPAGY    GOTO1 AFVAL,LSTAGYAH      VALIDATE AGENCY ALPHA ID                     
         BNE   VPAGYX                                                           
         USING CT5REC,R1                                                        
         LA    R1,IOKEY                                                         
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,FVIFLD                                                  
         DROP  R1                                                               
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         JL    *+2                                                              
         BE    VPAGY2                                                           
         OI    IDSIDH+6,X'40'                                                   
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     VALPARSX            ACCESS RECORD NOT FOUND                      
VPAGY2   MVC   SELAGY,FVIFLD                                                    
         B     VPAGYX                                                           
VPAGY1   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALPARSX                                                         
VPAGYX   EQU   *                                                                
*                                                                               
VALPARSX XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* GET SYSTEM EXECUTIVE LIST VALUES                                    *         
***********************************************************************         
GETSE    NTR1                                                                   
         L     R1,ASYS             GET SYSTEM LIST INFO                         
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
         CLC   SENAME,APWORK                                                    
         BE    GETSEOK                                                          
         BXLE  R1,RE,*-10                                                       
         MVC   FVMSGNO,=AL2(FVFESYS)                                            
         LA    R0,1                                                             
         B     GETSEX                                                           
*                                                                               
GETSEOK  MVC   APWORK(2),SESYS                                                  
         SR    R0,R0                                                            
*                                                                               
GETSEX   LTR   R0,R0                                                            
         XIT1                                                                   
         DROP  R1                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY SE NAME                                          *         
*                                                                     *         
* NTRY - R1=A(SE NUMBER)                                              *         
* EXIT - APWORK+0(7)=SE NAME OR 'SYS=XX' IF NOT FOUND                 *         
*        APPARM(4)=A(SELIST ENTRY) OR ZEROES IF NOT FOUND             *         
***********************************************************************         
DISSE    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   APBYTE(1),0(R1)                                                  
         L     R1,ASYS                                                          
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
         CLC   SESYS,APBYTE        MATCH ON SE NUMBER                           
         BE    DISSE2                                                           
         BXLE  R1,RE,*-10                                                       
         XC    APPARM(4),APPARM                                                 
         MVI   APWORK,C' '                                                      
         MVC   APWORK+1(L'SENAME-1),APWORK                                      
         MVC   APWORK(4),=C'SYS='                                               
         XOUT  APBYTE,APWORK+4,1                                                
         B     DISSEX                                                           
*                                                                               
DISSE2   MVC   APWORK(L'SENAME),SENAME                                          
         ST    R1,APPARM                                                        
*                                                                               
DISSEX   J     EXIT                                                             
         DROP  R1                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET NEXT RECORD FOR LIST/REPORT, FILTERING ON I/P PARAMETERS        *         
***********************************************************************         
         USING CTIREC,R2                                                        
GETREC   NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         B     GETRECIO            PRESERVE VALUE OF R1 ON ENTRY                
GETRECRD NI    GETSEQF,X'FF'-APILRERD                                           
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BNE   GETRECN                                                          
GETRECSQ LA    R1,IOCONFIL+IOSQ+IO1                                             
GETRECIO GOTO1 AIO                                                              
         BNE   GETRECN                                                          
         L     R2,AIOAREA1                                                      
*                                  CHECK STILL CORRECT RECORD TYPE              
         CLC   IOKEYSAV(CTIKID-CTIKEY),CTIKEY                                   
         BNE   GETRECN                                                          
         OC    CTIKID-CTIKEY(10,R2),CTIKID-CTIKEY(R2)                           
         BZ    GETRECN                                                          
*                                                                               
GRID     EQU   *                                                                
*                                                                               
         MVC   LSAGY,CUAALF        SECURITY AGENCY                              
         MVC   LAAGY,CUAALF        AGENCY ALPHA                                 
         XC    AAGY,AAGY                                                        
         XC    SAGY,SAGY                                                        
*                                                                               
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTAGYELQ     GET AGENCY ELEMENT                           
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD            READ NEXT RECORD                             
         USING CTAGYD,R3                                                        
         MVC   AAGY,CTAGYID                                                     
**                                                                              
         MVC   LCLAGY,AAGY        | GET SECURITY AGENCY FOR                     
         BAS   RE,GRSECID         | THE ALPHA AGENCY                            
         BNE   GETRECRD           |                                             
         MVC   SAGY,LCLAGY        |                                             
**                                                                              
         MVC   LCLAGY,LAAGY       | GET SECURITY AGENCY FOR                     
         BAS   RE,GRSECID         | THE ALPHA AGENCY                            
         BNE   GETRECRD           |                                             
         MVC   LSAGY,LCLAGY       |                                             
**                                                                              
         CLC   LAAGY,LSAGY         LOGGED IN W/ SECURITY AGYENCY?               
         BNE   GRID10              NO                                           
         CLC   LSAGY,SAGY          SETUP AGY HAS SAME SECURITY AGY?             
         BNE   GETRECRD            NO: NO ACCESS TO SETUP AGENCY                
         B     GRID20              YES: ALLOWED                                 
*                                                                               
GRID10   CLC   LAAGY,AAGY          LOGGED IN W/ SETUP AGENCY?                   
         BE    GRID20              YES: ALLOWED                                 
         B     GETRECRD            NO: GET NEXT RECORD                          
*                                                                               
GRID20   EQU   *                                                                
         CLI   SELIDSP,C' '       USER ID - FILTER ONLY IF IT                   
         BNH   GRIDX                 CONTAINS SPECIAL (WILD) CHARS.             
         XR    R1,R1                                                            
         ICM   R1,1,SELKEYCL                                                    
         BM    GRID3                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CTIKID(0),SELID                                                  
         BH    GETRECN             (NO MORE RELEVENT RECORDS)                   
GRID3    GOTO1 ATXTFLT,APPARM,(SELIDL,SELID),(8,CTIKID)                         
         BNE   GETRECRD            READ NEXT RECORD                             
GRIDX    EQU   *                                                                
*                                                                               
GRSYS    OC    SELSYS,SELSYS       FILTER ON SYSTEM                             
         BZ    GRSYSX                                                           
         LR    R3,R2                                                            
         MVI   APELEM,X'21'        GET SYSTEM ELEMS                             
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD            READ NEXT RECORD                             
         B     GRSYS3                                                           
GRSYS2   LLC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),0                                                          
         BE    GETRECRD            READ NEXT RECORD                             
         CLI   0(R3),X'21'                                                      
         BNE   GRSYS2                                                           
         USING CTSYSD,R3                                                        
GRSYS3   CLC   CTSYSNUM,SELSYS                                                  
         BNE   GRSYS2                                                           
         B     GRPGM                                                            
GRSYSX   EQU   *                                                                
*                                                                               
GRSEN    OC    SELSEN,SELSEN       FILTER ON SE NUMBER                          
         BZ    GRPGMX                                                           
         LR    R3,R2                                                            
         MVI   APELEM,X'21'        GET SYSTEM ELEMS                             
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD            READ NEXT RECORD                             
         B     GRSEN3                                                           
GRSEN2   LLC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),0                                                          
         BE    GETRECRD            READ NEXT RECORD                             
         CLI   0(R3),X'21'                                                      
         BNE   GRSEN2                                                           
         USING CTSYSD,R3                                                        
GRSEN3   CLC   CTSYSSE,SELSEN                                                   
         BNE   GRSEN2                                                           
GRSENX   EQU   *                                                                
*                                                                               
GRPGM    DS    0H                                                               
* WE ARE REMOVING PROGRAM FILTER ON LIST SCREEN SO COMMENTING CODE              
**       OC    SELPGM,SELPGM       FILTER ON PROGRAM                            
**       BZ    GRPGMX                                                           
**       MVC   PROGRAM,SELPGM                                                   
**       LA    R1,CTSYSPGM         POINT TO SYSTEM ELEMENT                      
**       LLC   RE,CTSYSLEN                                                      
*                                  FIND PROGRAM IN ELEMENT                      
**GRPGM10  CH    RE,=Y(CTSYSL1Q)                                                
**       BNH   GRPGM30             END OF ELEMENT                               
**       CLC   SELPGM,0(R1)                                                     
**       BE    GRPGM20             PROGRAM FOUND                                
**       LA    R1,L'CTSYSPGM(R1)   GET NEXT PROGRAM                             
**       SH    RE,=Y(L'CTSYSPGM)                                                
**       B     GRPGM10                                                          
**GRPGM20  OC    1(2,R1),1(R1)       CHECK PROGRAM ACCESS=N                     
**       BZ    GETRECRD                                                         
**       B     GRPGMX                                                           
**GRPGM30  OC    CTSYSALL,CTSYSALL   CHECK ALL ACCESS=N                         
**       BZ    GETRECRD                                                         
**       B     GRPGMX                                                           
*                                                                               
GRPGMX   EQU   *                                                                
*                                                                               
GRAGY    OC    SELAGY,SELAGY       FILTER ON AGENCY ALPHA ID                    
         BZ    GRAGYX                                                           
         XC    APELEM,APELEM                                                    
         MVI   APELEM,CTAGYELQ     GET AGENCY ELEMENT                           
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    GETRECRD            READ NEXT RECORD                             
         USING CTAGYD,R3                                                        
         CLC   SELAGY,CTAGYID                                                   
         BNE   GETRECRD                                                         
GRAGYX   EQU   *                                                                
         DROP  R3                                                               
*                                                                               
GETRECY  SR    RC,RC               RETURN CC EQUAL RECORD OK                    
GETRECN  LTR   RC,RC               RETURN CC NOT EQUAL END OF RECORDS           
         XIT1                                                                   
***********************************************************************         
* EXTRACT SECURITY ALPHA ID USING AGENCY ALPHA                                  
***********************************************************************         
GRSECID  NTR1                                                                   
         LA    R1,IOKEY                                                         
         USING CT5REC,R1             R1=A(AGENCY ACCESS RECORD)                 
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,LCLAGY       AGENCY ALPHA                               
         GOTO1 AIO,IORD+IOCONFIL+IO3                                            
         BNE   GR0XX                                                            
*                                                                               
         L     R1,AIOAREA3                                                      
         LA    R3,CT5DATA            R3=A(FIRST ELEMENT)                        
GR010    CLI   0(R3),0               TEST EOR                                   
         JE    GR0XX                                                            
         CLI   0(R3),CTSEAELQ        X'B8' SECURITY AGENCY ALPHA ID             
         BE    GR020                                                            
         LLC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GR010                                                            
*                                                                               
         USING CTSEAD,R3                                                        
GR020    MVC   LCLAGY,CTSEAAID       SECURITY AGENCY ALPHA ID                   
*                                                                               
         DROP  R3                                                               
GR0XX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD A LINE OF USER ID RECORD DATA                                 *         
***********************************************************************         
         USING CTIREC,R2                                                        
LINE     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AIOAREA1                                                      
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
         MVC   LISTID,CTIKID                                                    
*                                                                               
         MVC   LISTALP,AAGY                                                     
         MVC   LISTSEC,SAGY                                                     
*                                                                               
LNAGYA   CLI   APMODE,APMREPP      AGENCY ALPHA ID                              
         BNE   LNAGYAX                                                          
         MVI   APELEM,CTAGYELQ                                                  
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LNAGYAX                                                          
         USING CTAGYD,R3                                                        
         MVC   LISTAGYA,CTAGYID                                                 
         DROP  R3                                                               
LNAGYAX  EQU   *                                                                
*                                                                               
LNSYS    EQU   *                   SYSTEMS AUTHORISED                           
         LA    R8,APWORK                                                        
         MVI   0(R8),C' '                                                       
         MVC   1(63,R8),0(R8)                                                   
         LA    R3,CTIDATA                                                       
LNSYS1   CLI   0(R3),X'21'                                                      
         BE    LNSYS2A                                                          
LNSYS2   LLC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),0                                                          
         BE    LNSYS5                                                           
         B     LNSYS1                                                           
         USING CTSYSD,R3                                                        
LNSYS2A  L     RF,ASYSLST          LOOK UP NAME IN SYSLST                       
         LA    RF,6(RF)                                                         
         USING SYSLSTD,RF                                                       
LNSYS3   CLI   SYSLNUM,0                                                        
         BE    LNSYS2                                                           
         CLC   SYSLNUM,CTSYSNUM                                                 
         BE    LNSYS4                                                           
         LA    RF,SYSLLEN(RF)                                                   
         B     LNSYS3                                                           
LNSYS4   MVC   0(3,R8),SYSLNAME                                                 
         LA    R8,4(R8)                                                         
         B     LNSYS2                                                           
         DROP  RF                                                               
LNSYS5   GOTO1 =V(SQUASHER),APPARM,APWORK,64,RR=RB                              
         GOTO1 =V(CHOPPER),APPARM,(64,APWORK),(27,LISTSYS),1,RR=RB              
LNSYSX   EQU   *                                                                
LNIDS    LA    R8,APWORK           ID LIST                                      
         MVI   0(R8),C' '                                                       
         MVC   1(79,R8),0(R8)                                                   
         LA    R9,7                                                             
         LR    R3,R2                                                            
         MVI   APELEM,X'1F'        GET PRINCIPAL ID ELEM                        
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LNIDS1                                                           
         USING CTPID,R3                                                         
         MVC   0(10,R8),CTPID                                                   
         LA    R8,11(R8)                                                        
LNIDS1   LR    R3,R2                                                            
         MVI   APELEM,X'20'        GET ORDINARY ID ELEMS                        
         GOTO1 AGETELS,CTIREC                                                   
         ICM   R3,15,APPARM                                                     
         BZ    LNIDS5                                                           
         B     LNIDS4                                                           
LNIDS2   LLC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),0                                                          
         BE    LNIDS5                                                           
         CLI   0(R3),X'20'                                                      
         BNE   LNIDS2                                                           
         USING CTIDD,R3                                                         
LNIDS4   CLC   CTID(2),=X'0000'    L=LISTNAME ELEMENT                           
         BNE   LNIDS4A                                                          
         MVC   0(2,R8),=C'L='                                                   
         MVC   2(8,R8),CTID+2                                                   
         B     LNIDS4C                                                          
LNIDS4A  CLC   CTID(2),=X'0001'    A=AG ELEMENT                                 
         BNE   LNIDS4B                                                          
         MVC   0(2,R8),=C'A='                                                   
         MVC   2(2,R8),CTIDAGY                                                  
         B     LNIDS4C                                                          
LNIDS4B  MVC   0(10,R8),CTID                                                    
LNIDS4C  LA    R8,11(R8)                                                        
*NIDS4   MVC   0(10,R8),CTID                                                    
*        LA    R8,11(,R8)                                                       
         BCT   R9,LNIDS2                                                        
LNIDS5   GOTO1 =V(SQUASHER),APPARM,APWORK,80,RR=RB                              
         GOTO1 =V(CHOPPER),APPARM,(80,APWORK),(23,LISTIDS),1,RR=RB              
*                                                                               
LINEX    XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER TEMP W/S                                             *         
***********************************************************************         
*      SEACSWRK                                                                 
       ++INCLUDE SEACSWRK                                                       
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER SCREENS AND SAVED STORAGE                            *         
***********************************************************************         
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
* SEACSC9D                                                                      
       ++INCLUDE SEACSC9D                                                       
         ORG   ACSTABH                                                          
* SEACSC8D                                                                      
       ++INCLUDE SEACSC8D                                                       
         ORG                                                                    
*                                  WORKING STORAGE SAVED IN TWA                 
         ORG   SAVOVER                                                          
SAVSYS   DS    XL1                 LAST SYSTEM DISPLAYED                        
SAVKEY   DS    XL(L'CTIKEY)        SAVE LAST RECORD KEY READ FOR COPY           
SAVOAGY  DS    CL2                 OLD AGENCY ALPHA-ID                          
SAVNAGY  DS    CL2                 NEW AGENCY ALPHA-ID                          
SAGY     DS    CL2                 SECURITY AGENCY                              
AAGY     DS    CL2                 ALPHA AGENCY                                 
LSAGY    DS    CL2                 LOGIN SECURITY AGENCY                        
LAAGY    DS    CL2                 LOGIN ALPHA AGENCY                           
LCLAGY   DS    CL2                 LOCAL VARIABLE FOR AGENCY ALPHA              
PASEXP   DS    XL1                 PASSWORD EXPIRE DAYS                         
SAVCLRL  EQU   *-SAVOVER                                                        
*                                                                               
***********************************************                                 
*                                                                               
*   LIST/SELECT LINE LAYOUT                                                     
*                                                                               
***********************************************                                 
LISTD    DSECT                     LIST/SELECT LINE LAYOUT                      
LISTACTH DS    XL8                                                              
LISTACT  DS    CL3                 ACTION FIELD                                 
LISTLINH DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
         ORG   *-3                                                              
LISTAGYA DS    CL2                                                              
         ORG                                                                    
LISTID   DS    CL8                                                              
         DS    CL1                                                              
LISTALP  DS    CL2                                                              
         DS    CL4                                                              
LISTSEC  DS    CL2                                                              
         DS    CL3                                                              
LISTSYS  DS    CL30                                                             
         ORG   LISTSYS                                                          
LISTLIXA DS    CL2                                                              
LISTLIMA DS    CL8                                                              
         DS    CL1                                                              
LISTLIXM DS    CL2                                                              
LISTLIMM DS    CL17                                                             
         DS    CL1                                                              
LISTIDS  DS    CL23                                                             
         ORG   LISTLIN+L'LISTLIN                                                
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER LOCAL W/S                                            *         
***********************************************************************         
WORKD    DSECT                                                                  
         ORG   APLOCAL                                                          
DUB      DS    D                                                                
RETURN   DS    F                                                                
ASYSEL   DS    A                                                                
ASE      DS    A                                                                
APGM     DS    A                                                                
WORK     DS    CL(L'APWORK)                                                     
*                                                                               
SELDATA  DS    0XL(SELDATAL)                                                    
SELID    DS    CL8                 USER ID                                      
SELIDSP  DS    CL1                 1ST SPECIAL CHAR                             
SELIDL   DS    CL1                 L'DATA ENTERED                               
SELKEYCL DS    XL1                 KEY COMPARE LENGTH                           
SELSYS   DS    XL1                 SYSTEM SEOV NUMBER                           
SELSEN   DS    XL1                 SYSTEM SE NUMBER                             
SELPGM   DS    XL1                 PROGRAM NUMBER                               
SELAGY   DS    XL2                 AGENCY ALPHA ID                              
SELDATAL EQU   *-SELID                                                          
*                                                                               
SYSTEM   DS    CL1                                                              
SFLAGS   DS    CL1                                                              
PROGRAM  DS    CL1                                                              
PGNAME   DS    CL8                                                              
*                                                                               
IDNUM    DS    XL(L'CTIKNUM)                                                    
AGNUM    DS    XL(L'CTSYSAGB)                                                   
AGAID    DS    XL(L'CT5KALPH)                                                   
IDOPTS   DS    XL2                                                              
SYNNUM   DS    XL2                                                              
SYNID    DS    CL10                                                             
IDCTRY   DS    XL1                 COUNTRY CODE                                 
*                                                                               
XXCNT    DS    0C                                                               
PGCNT    DS    CL1                                                              
SYCNT    DS    CL1                                                              
XXCNTL   EQU   *-XXCNT                                                          
*                                                                               
ASYSNAMS DS    A                                                                
SYSNAMS  DS    CL78                                                             
SYSNAMSL DS    XL1                                                              
SYSNUMS  DS    XL1                                                              
*                                                                               
GETSEQF  DS    XL1                                                              
*                                                                               
LIDFNDX  DS    XL1                                                              
*                                                                               
BLOCK1   DS    20CL32                                                           
*                                                                               
KEYSAVE  DS    CL(L'IOKEY)                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SEACS25   12/12/18'                                      
         END                                                                    
