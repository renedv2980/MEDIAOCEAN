*          DATA SET SEDFAR     AT LEVEL 011 AS OF 11/18/20                      
*PHASE SEDFARA                                                                  
*INCLUDE FATABOFF                                                               
*INCLUDE DICTATE                                                                
*INCLUDE LISTIO                                                                 
*&&US                                                                           
*INCLUDE CLUNPK                                                                 
*&&                                                                             
SEDFAR   TITLE 'Security System Daily File Activity Extract'                    
         PRINT NOGEN                                                            
SEDFAR   CSECT                                                                  
                                                                                
         J     SPNTRY                                                           
                                                                                
         DC    A(0)                A(SYSTEM/FILE LIST) already open             
         DC    A(FILTAB)           A(FILE DEFINITION TABLE)                     
         DC    A(PRGTAB)           A(PROGRAM TABLE)                             
                                                                                
SPNTRY   NMOD1 0,*SEDFAR*                                                       
         LR    RA,R1                                                            
         USING DI_D,RA             RA=A(GEDFAR INTERFACE BLOCK)                 
         L     R9,DI_AWRK                                                       
         USING WORKD,R9            R9=A(LOCAL WORKING STORAGE)                  
         L     R8,ALITS                                                         
         USING LITERALS,R8         R8=A(GLOBAL LITERALS)                        
         L     R7,DI_ACOM                                                       
         USING COMFACSD,R7         R7=A(COMFACS)                                
                                                                                
         STM   R2,RB,DI_R2RB       SAVE REGISTERS                               
                                                                                
         CLI   DI_MODE,DI_MINIQ    TEST INITIALIZATION                          
         JE    SPINIT                                                           
         CLI   DI_MODE,DI_MAGFQ    TEST FIRST FOR NEW AGENCY                    
         JE    SPAGYF                                                           
         DC    H'0'                                                             
         DROP  RB                                                               
                                                                                
ALITS    DC    A(LITERALS)                                                      
         EJECT                                                                  
***********************************************************************         
* Initialization                                                      *         
***********************************************************************         
                                                                                
SPINIT   L     R2,DI_AAGY          BUILD AGENCY/MEDIA TABLE                     
                                                                                
SPINIT20 LA    R2,CORPHS           LOAD CORE RESIDENT PHASES                    
         LA    R3,APHASES                                                       
         LA    R4,CORPHSN                                                       
         LTR   R4,R4                                                            
         JZ    SPINIT30                                                         
         SR    R0,R0                                                            
         ICM   R0,B'1110',=X'D9000A'                                            
         LA    R1,DMCB                                                          
         L     RF,CCALLOV                                                       
SPINIT22 ICM   R0,B'0001',0(R2)                                                 
         GOTOR (RF),(R1),0,(R0)                                                 
         MVC   0(4,R3),0(R1)                                                    
         AHI   R2,L'CORPHS                                                      
         AHI   R3,L'APHASES                                                     
         JCT   R4,SPINIT22                                                      
                                                                                
SPINIT30 BRAS  RE,INIBUF           initialise buffer                            
                                                                                
SPINITX  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* First time for new agency                                           *         
***********************************************************************         
                                                                                
SPAGYF   BRAS  RE,INIBUF           reinitialise buffer for new agency           
                                                                                
SPAGYFX  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* (Re)initialise Buffer                                               *         
***********************************************************************         
                                                                                
INIBUF   LR    R0,RE                                                            
         GOTOR DI_ABFIN,DMCB,('BUFFAINI',ABUFF1),BUFFREC,COMFACSD               
         LR    RE,R0                                                            
         BER   RE                                                               
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* Common Edit subroutines                                             *         
***********************************************************************         
                                                                                
***********************************************************************         
* Edit compressed complemented effective date                         *         
***********************************************************************         
EDTDEF   L     R2,DI_AINP                                                       
         L     R3,DI_AOUT                                                       
         MVC   DUB(2),0(R2)                                                     
         XC    DUB(2),=2X'FF'                                                   
         LR    R0,RE                                                            
         GOTOR CDATCON,DMCB,(2,DUB),(23,(R3)) yyyy-mm-dd ISO 8601               
         LR    RE,R0                                                            
         MVI   DI_LOUT,10                                                       
EDTDEFX  J     EXITYRE                                                          
                                                                                
***********************************************************************         
* Edit compressed effective date which may be null or FFFF            *         
***********************************************************************         
EDTDF0   L     R2,DI_AINP                                                       
         L     R3,DI_AOUT                                                       
         MVC   0(4,R3),=C'Null'                                                 
         MVI   DI_LOUT,4                                                        
         CLC   0(2,R2),=2X'FF'                                                  
         JE    EXITYRE                                                          
         OC    0(2,R2),0(R2)                                                    
         JZ    EXITYRE                                                          
         LR    R0,RE                                                            
         GOTOR CDATCON,DMCB,(2,DUB),(23,(R3)) yyyy-mm-dd ISO 8601               
         LR    RE,R0                                                            
         MVI   DI_LOUT,10                                                       
EDTDF0X  J     EXITYRE                                                          
                                                                                
***********************************************************************         
* Edit PIN (person number) to PID (8 char person id)                  *         
***********************************************************************         
EDTPIN   L     R2,DI_AINP                                                       
         L     R3,DI_AOUT                                                       
         LR    R0,RE                                                            
         GOTOR GETPID,DMCB,(R2),(R3)                                            
         LR    RE,R0                                                            
         MVI   DI_LOUT,L'SAPEPID                                                
EDTPINX  J     EXITYRE                                                          
                                                                                
***********************************************************************         
* Edit OV System (1 byte OLAI sys)                                    *         
***********************************************************************         
EDTOVS   L     R2,DI_AINP                                                       
         L     R3,DI_AOUT                                                       
         LR    R0,RE                                                            
         GOTOR GETSYS,DMCB,(R2),(R3)                                            
         LR    RE,R0                                                            
         MVI   DI_LOUT,L'SYSLNAME                                               
EDTOVSX  J     EXITYRE                                                          
                                                                                
***********************************************************************         
* Edit Program (1 byte OLAI sys, 1 byte program number)               *         
***********************************************************************         
EDTPGM   L     R2,DI_AINP                                                       
         L     R3,DI_AOUT                                                       
         MVC   SYSPGM,0(R2)        save system and program                      
         LR    R0,RE                                                            
         GOTOR GETPGM,DMCB,(R2),(R3)                                            
         LR    RE,R0                                                            
         MVI   DI_LOUT,L'PGMNAME                                                
EDTPGMX  J     EXITYRE                                                          
                                                                                
***********************************************************************         
* Edit single user id number to 8 char user id                        *         
***********************************************************************         
EDTUID   L     R2,DI_AINP                                                       
         L     R3,DI_AOUT                                                       
         MVC   0(4,R3),=C'None'    Assume zero means none                       
         OC    0(2,R2),0(R2)                                                    
         JZ    EDTUIDX                                                          
         LR    R0,RE                                                            
         GOTOR GETUID,DMCB,(R2),(R3)                                            
         LR    RE,R0                                                            
EDTUIDX  MVI   DI_LOUT,L'CTID                                                   
         J     EXITYRE                                                          
                                                                                
***********************************************************************         
* Edit access group number to 8 char access group code                *         
***********************************************************************         
EDTAGN   L     R2,DI_AINP                                                       
         L     R3,DI_AOUT                                                       
         MVC   0(4,R3),=C'None'    Assume zero means none                       
         OC    0(2,R2),0(R2)                                                    
         JZ    EDTAGNX                                                          
         LR    R0,RE                                                            
         GOTOR GETAGC,DMCB,(R2),(R3)                                            
         LR    RE,R0                                                            
EDTAGNX  MVI   DI_LOUT,L'SAAGAGR                                                
         J     EXITYRE                                                          
                                                                                
                                                                                
***********************************************************************         
* Edit Defer to Company value                                         *         
***********************************************************************         
EDTDCID  L     R2,DI_AINP                                                       
         L     R3,DI_AOUT                                                       
         MVC   0(3,R3),=C'No '     Assume zero means No                         
         TM    0(R2),SALASIDF                                                   
         JZ    EDTDCIDX                                                         
         MVC   0(3,R3),=C'Yes'                                                  
EDTDCIDX MVI   DI_LOUT,3                                                        
         J     EXITYRE                                                          
                                                                                
                                                                                
***********************************************************************         
* Edit data group, data value, right aligned                          *         
***********************************************************************         
EDTLDCOD L     R2,DI_AINP                                                       
         L     R3,DI_AOUT                                                       
         MVC   0(4,R3),=C'None'    Assume zero means none                       
         MVI   DI_LOUT,4                                                        
*                                                                               
         XC    LISTIOBK,LISTIOBK                                                
         LA    R4,LISTIOBK                                                      
         USING LISTIOD,R4                                                       
         ST    R7,LISTACOM             A(COMFAC)                                
         MVI   LISTACTN,LISTACON   CONVERT DATA TO PRINTABLE VALUE              
         MVC   LISTINUM,BUFFDNUM-BUFFDDLN+WORK  LIST NUMBER                     
         MVC   LISTISYS,BUFFDNSY-BUFFDDLN+WORK  LIST SYSTEM                     
         MVC   LISTITYP,BUFFDNTY-BUFFDDLN+WORK  TYPE NUMBER                     
         MVC   LISTDVAL,0(R2)                   DATA                            
         LR    R0,RE                                                            
         GOTOR VLISTIO,LISTIOBK                                                 
         LR    RE,R0                                                            
         JNE   EXITYRE                                                          
*                                                                               
         MVC   0(L'LISTDVAL,R3),LISTDVAL                                        
         MVI   DI_LOUT,L'LISTDVAL                                               
         DROP  R4                                                               
         J     EXITYRE                                                          
                                                                                
***********************************************************************         
* Edit data group list number to PID/DataGroup/Sys/ListType           *         
***********************************************************************         
EDTDGLN  L     R2,DI_AINP                                                       
         L     R3,DI_AOUT                                                       
         MVC   0(4,R3),=C'None'    Assume zero means none                       
         OC    0(2,R2),0(R2)                                                    
         JZ    EDTDGLNX                                                         
*                                                                               
         ST    RE,FULL             SAVE RE                                      
*                                                                               
         GOTOR GETDLN,DMCB,(R2),(R3)                                            
         JNE   ETDGLN99                                                         
*                                                                               
         MVC   WORK(L'BUFFDDLN),0(R3)                                           
*                                                                               
         LA    R2,BUFFDNGP-BUFFDDLN+WORK                                        
         GOTOR GETDGC,DMCB,(R2),(R3)                                            
         LA    R3,L'SALANCOD-1(,R3)                                             
         CLI   0(R3),C' '          BACKUP TO LAST NON-BLANK CHAR                
         JH    *+10                                                             
         BCTR  R3,0                                                             
         J     *-10                                                             
         MVI   1(R3),C','                                                       
         LA    R3,2(R3)                                                         
*                                                                               
         LA    R2,BUFFDNSY-BUFFDDLN+WORK                                        
         GOTOR GETSYS,DMCB,(R2),(R3)                                            
         LA    R3,L'SYSLNAME-1(,R3)                                             
         CLI   0(R3),C' '          BACKUP TO LAST NON-BLANK CHAR                
         JH    *+10                                                             
         BCTR  R3,0                                                             
         J     *-10                                                             
         MVI   1(R3),C','                                                       
         LA    R3,2(R3)                                                         
*                                                                               
         LA    R2,WORK                                                          
         GOTOR GETDTY,DMCB,(R2),(R3)                                            
         LA    R3,L'LISTITNM-1(,R3)                                             
         CLI   0(R3),C' '          BACKUP TO LAST NON-BLANK CHAR                
         JH    *+10                                                             
         BCTR  R3,0                                                             
         J     *-10                                                             
*                                                                               
         LA    R2,BUFFDNPN-BUFFDDLN+WORK                                        
         OC    0(R2,L'LNUMKPSN),0(R2)                                           
         JZ    ETDGLN99            PID CAN BE OPTIONAL                          
*                                                                               
         MVI   1(R3),C','                                                       
         LA    R3,2(R3)                                                         
*                                                                               
         GOTOR GETPID,DMCB,(R2),(R3)                                            
*                                                                               
ETDGLN99 L     RE,FULL             RESTORE RE                                   
EDTDGLNX MVI   DI_LOUT,L'SAPALPID+L'SALANCOD+L'SYSLNAME+12+3                    
         J     EXITYRE                                                          
                                                                                
***********************************************************************         
* Edit Media Office code (1 byte number)                              *         
***********************************************************************         
EDTMOFF  L     R2,DI_AINP                                                       
         L     R3,DI_AOUT                                                       
                                                                                
         L     R2,DI_AIO1                                                       
         MVC   0(L'MOFFC2OF,R3),MOFFC2OF-MOFRECD(R2)                            
                                                                                
         MVI   DI_LOUT,L'MOFFC2OF                                               
EDTMOFFX J     EXITYRE                                                          
                                                                                
         EJECT                                                                  
***********************************************************************         
* General subroutines                                                 *         
***********************************************************************         
                                                                                
***********************************************************************         
* Get User Id from User Id number.                                    *         
* P1=a(uidnum), P2=a(o/p)                                             *         
***********************************************************************         
GETUID   NTR1  LABEL=NO                                                         
         LM    R2,R3,0(R1)                                                      
         XC    BUFFKEY(BUFFKEYL),BUFFKEY                                        
         MVI   BUFFKTYP,BUFFUIDQ                                                
         MVC   BUFFKUIN,0(R2)                                                   
         MVC   KEY(BUFFKEYL),BUFFKEY                                            
         GOTOR DI_ABFIN,DMCB,('BUFFAGET',ABUFF1),BUFFREC,COMFACSD               
         CLI   BUFFERRS-BUFFPARM(R1),0                                          
         JE    GETUID90                                                         
*                                                                               
         MVC   BUFFKEY(BUFFKEYL),KEY                                            
         LA    R4,KEY              not in buffer, get from disk                 
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,BUFFKUIN                                                 
         DROP  R4                                                               
GETUID10 GOTOR CDATAMGR,DMCB,DMREAD,CTFILE,KEY,IO                               
         CLI   8(R1),0                                                          
         JNE   GETUID20            record not found                             
         MVI   ELCODE,CTDSCELQ     description element                          
         LA    R1,IO                                                            
         BRAS  RE,GETEL                                                         
         JNE   GETUID20            element not found                            
         CLI   CTDSCLEN-CTDSCD(R1),4 length must be >4                          
         JNH   GETUID20                                                         
         MVI   BUFFFLAG,0          indicate known user id                       
         MVC   BUFFDUID,CTDSC-CTDSCD(R1)                                        
         J     GETUID40                                                         
GETUID20 OI    BUFFFLAG,BUFFFRNF   indicate unknown user id                     
         MVC   BUFFDUID(4),=C'UID='                                             
         GOTOR CHEXOUT,DMCB,BUFFKUIN,BUFFDUID+4,2                               
GETUID40 GOTOR DI_ABFIN,DMCB,('BUFFAPUT',ABUFF1),BUFFREC,COMFACSD               
         JE    GETUID90                                                         
         DC    H'0'                                                             
GETUID90 MVC   0(L'BUFFDUID,R3),BUFFDUID return user id                         
         TM    BUFFFLAG,BUFFFRNF   EXITN if unknown user id                     
         JO    EXITN                                                            
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Get Alpha Id from User Id number.                                   *         
* P1=a(uidnum), P2=a(o/p)                                             *         
***********************************************************************         
GETUIA   NTR1  LABEL=NO                                                         
         LM    R2,R3,0(R1)                                                      
         XC    BUFFKEY(BUFFKEYL),BUFFKEY                                        
         MVI   BUFFKTYP,BUFFUIAQ                                                
         MVC   BUFFKUIN,0(R2)                                                   
         MVC   KEY(BUFFKEYL),BUFFKEY                                            
         GOTOR DI_ABFIN,DMCB,('BUFFAGET',ABUFF1),BUFFREC,COMFACSD               
         CLI   BUFFERRS-BUFFPARM(R1),0                                          
         JE    GETUIA90                                                         
*                                                                               
         MVC   BUFFKEY(BUFFKEYL),KEY                                            
         LA    R4,KEY              not in buffer, get from disk                 
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,BUFFKUIN                                                 
         DROP  R4                                                               
GETUIA10 GOTOR CDATAMGR,DMCB,DMREAD,CTFILE,KEY,IO                               
         CLI   8(R1),0                                                          
         JNE   GETUIA20            record not found                             
         MVI   ELCODE,CTAGYELQ     Agency alpha id element                      
         LA    R1,IO                                                            
         BRAS  RE,GETEL                                                         
         JNE   GETUIA20            element not found                            
         MVI   BUFFFLAG,0          indicate known user id                       
         MVC   BUFFDUIA,CTAGYID-CTAGYD(R1)                                      
         J     GETUIA40                                                         
GETUIA20 OI    BUFFFLAG,BUFFFRNF   indicate unknown user id                     
         MVC   BUFFDUIA,=C'??'                                                  
GETUIA40 GOTOR DI_ABFIN,DMCB,('BUFFAPUT',ABUFF1),BUFFREC,COMFACSD               
         JE    GETUIA90                                                         
         DC    H'0'                                                             
GETUIA90 MVC   0(L'BUFFDUIA,R3),BUFFDUIA return user id                         
         TM    BUFFFLAG,BUFFFRNF   EXITN if unknown user id                     
         JO    EXITN                                                            
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Get Person Id from Person Id number.                                *         
* P1=a(pin), P2=a(o/p)                                                *         
***********************************************************************         
GETPID   NTR1  LABEL=NO                                                         
         LM    R2,R3,0(R1)                                                      
         XC    BUFFKEY(BUFFKEYL),BUFFKEY                                        
         L     RF,DI_ADSD                                                       
         MVC   BUFFKAGY,DS_ALF-DS_D(RF)                                         
         MVI   BUFFKTYP,BUFFPIDQ                                                
         MVC   BUFFKPIN,0(R2)                                                   
         MVC   KEY(BUFFKEYL),BUFFKEY                                            
         GOTOR DI_ABFIN,DMCB,('BUFFAGET',ABUFF1),BUFFREC,COMFACSD               
         CLI   BUFFERRS-BUFFPARM(R1),0                                          
         JE    GETPID90                                                         
*                                                                               
         MVC   BUFFKEY(BUFFKEYL),KEY                                            
         LA    R4,KEY              not in buffer, get from disk                 
         USING SA0REC,R4                                                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,BUFFKAGY                                                 
         MVC   SA0KNUM,BUFFKPIN                                                 
         CLC   SA0KNUM,=Y(4096)                                                 
         JNL   GETPID10                                                         
*&&UK*&& CLC   SA0KAGY,=C'#E'                                                   
*&&US*&& CLC   SA0KAGY,=C'#N'      test dds security agency                     
         JNE   GETPID10                                                         
         MVC   SA0KNUM,=Y(1)       yes, use first (generic) entry               
         DROP  R4                                                               
GETPID10 GOTOR CDATAMGR,DMCB,DMREAD,CTFILE,KEY,IO                               
         CLI   8(R1),0                                                          
         JNE   GETPID20            record not found                             
         MVI   ELCODE,SAPALELQ     Personal ID element                          
         LA    R1,IO                                                            
         BRAS  RE,GETEL                                                         
         JNE   GETPID20            element not found                            
         MVI   BUFFFLAG,0          indicate known person                        
         MVC   BUFFDPID,SAPALPID-SAPALD(R1)                                     
         J     GETPID40                                                         
GETPID20 OI    BUFFFLAG,BUFFFRNF   indicate unknown person                      
         MVC   BUFFDPID(4),=C'PID='                                             
         GOTOR CHEXOUT,DMCB,BUFFKPIN,BUFFDPID+4,2                               
GETPID40 GOTOR DI_ABFIN,DMCB,('BUFFAPUT',ABUFF1),BUFFREC,COMFACSD               
         JE    GETPID90                                                         
         DC    H'0'                                                             
GETPID90 MVC   0(L'BUFFDPID,R3),BUFFDPID return pid                             
         TM    BUFFFLAG,BUFFFRNF   EXITN if unknown person                      
         JO    EXITN                                                            
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Get Access Group code from Access Group number                      *         
* P1=a(agn), P2=a(o/p)                                                *         
***********************************************************************         
GETAGC   NTR1  LABEL=NO                                                         
         LM    R2,R3,0(R1)                                                      
         XC    BUFFKEY(BUFFKEYL),BUFFKEY                                        
         L     RF,DI_ADSD                                                       
         MVC   BUFFKAGY,DS_ALF-DS_D(RF)                                         
         MVI   BUFFKTYP,BUFFAGNQ                                                
         MVC   BUFFKAGN,0(R2)                                                   
         MVC   KEY(BUFFKEYL),BUFFKEY                                            
         GOTOR DI_ABFIN,DMCB,('BUFFAGET',ABUFF1),BUFFREC,COMFACSD               
         CLI   BUFFERRS-BUFFPARM(R1),0                                          
         JE    GETAGC90                                                         
*                                                                               
         MVC   BUFFKEY(BUFFKEYL),KEY                                            
         LA    R4,KEY              not in buffer, get from disk                 
         USING SAAGREC,R4                                                       
         XC    SAAGKEY,SAAGKEY                                                  
         MVI   SAAGTYP,SAAGTYPQ                                                 
         MVI   SAAGSUB,SAAGSUBQ                                                 
         L     RF,DI_ADSD                                                       
         MVC   SAAGAGY,DS_ALF-DS_D(RF)                                          
         MVI   SAAGAGR+L'SAAGAGR-1,X'01' ignore header record                   
         DROP  R4                                                               
         GOTOR CDATAMGR,DMCB,DMRDHI,CTFILE,KEY,IO                               
         J     GETAGC30            scan access group records for number         
GETAGC20 GOTOR CDATAMGR,DMCB,DMRSEQ,CTFILE,KEY,IO                               
GETAGC30 CLC   KEY(SAAGAGR-SAAGKEY),IO                                          
         JNE   GETAGC40            not found                                    
         MVI   ELCODE,SAAGNELQ     Access group name element                    
         LA    R1,IO                                                            
         BRAS  RE,GETEL                                                         
         JNE   GETAGC20                                                         
         CLC   SAAGNNUM-SAAGNEL(,R1),0(R2) match on access group number         
         JNE   GETAGC20                                                         
         MVI   BUFFFLAG,0          indicate known group                         
         MVC   BUFFDAGC,IO+(SAAGAGR-SAAGKEY)                                    
         J     GETAGC60                                                         
GETAGC40 OI    BUFFFLAG,BUFFFRNF   indicate unknown group                       
         MVC   BUFFDAGC(4),=C'AGR='                                             
         GOTOR CHEXOUT,DMCB,BUFFKAGN,BUFFDAGC+4,2                               
GETAGC60 GOTOR DI_ABFIN,DMCB,('BUFFAPUT',ABUFF1),BUFFREC,COMFACSD               
         JE    GETAGC90                                                         
         DC    H'0'                                                             
GETAGC90 MVC   0(L'BUFFDAGC,R3),BUFFDAGC return pid                             
         TM    BUFFFLAG,BUFFFRNF   EXITN if unknown group                       
         JO    EXITN                                                            
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Get Data Group List Info (PID/DataGroup/Sys/ListType)               *         
* P1=a(dgln), P2=a(o/p)                                               *         
***********************************************************************         
GETDLN   NTR1  LABEL=NO                                                         
         LM    R2,R3,0(R1)                                                      
*                                                                               
         MVI   BUFFFLAG,0          CLEAR FLAG                                   
*                                                                               
         XC    BUFFKEY(BUFFKEYL),BUFFKEY                                        
         MVI   BUFFKTYP,BUFFDLNQ                                                
         MVC   BUFFKDLN,0(R2)                                                   
         MVC   KEY(BUFFKEYL),BUFFKEY                                            
         GOTOR DI_ABFIN,DMCB,('BUFFAGET',ABUFF1),BUFFREC,COMFACSD               
         CLI   BUFFERRS-BUFFPARM(R1),0                                          
         JE    GETDLN50                                                         
*                                                                               
         MVC   BUFFKEY(BUFFKEYL),KEY                                            
         LA    R4,KEY              not in buffer, get from disk                 
         USING LNUMD,R4                                                         
         XC    LNUMKEY,LNUMKEY                                                  
         MVI   LNUMKMAJ,LNUMKMAQ         C'L' - LIST SYSTEM                     
         MVI   LNUMKMIN,LNUMKMIQ         C'S' - SECURITY DATA ACCESS            
         MVI   LNUMKREC,LNUMKREQ         C'N' - NUMBER RECORD                   
         MVC   LNUMKNUM,BUFFKDLN                                                
         DROP  R4                                                               
*                                                                               
         GOTOR CDATAMGR,DMCB,DMRDHI,GENDIR,KEY,IO,0                             
*                                                                               
         CLC   KEY(LNUMKNUM-LNUMKEY+L'LNUMKNUM),IO                              
         JNE   GETDLN20            record not found                             
         MVC   BUFFDDLN,LNUMKNUM-LNUMKEY+IO                                     
         J     GETDLN40                                                         
GETDLN20 OI    BUFFFLAG,BUFFFRNF   indicate unknown data group                  
         MVC   BUFFDDLN(5),=C'LIST='                                            
         GOTOR CHEXOUT,DMCB,BUFFKDLN,BUFFDDLN+5,4                               
GETDLN40 GOTOR DI_ABFIN,DMCB,('BUFFAPUT',ABUFF1),BUFFREC,COMFACSD               
         JE    GETDLN50                                                         
         DC    H'0'                                                             
GETDLN50 MVC   0(L'BUFFDDLN,R3),BUFFDDLN return data group info                 
         TM    BUFFFLAG,BUFFFRNF   if unknown data group                        
         JO    EXITN                                                            
         J     EXITY                                                            
*                                                                               
***********************************************************************         
* Get Data Group List type                                            *         
* P1=a(dglt), P2=a(o/p)                                               *         
***********************************************************************         
GETDTY   NTR1  LABEL=NO                                                         
         LM    R2,R3,0(R1)                                                      
*                                                                               
         XC    LISTIOBK,LISTIOBK                                                
         LA    R4,LISTIOBK                                                      
         USING LISTIOD,R4                                                       
         ST    R7,LISTACOM             A(COMFAC)                                
         MVI   LISTACTN,LISTATYP   CONVERT DATA TO GET TYPE VALUE               
         MVC   LISTISYS,BUFFDNSY-BUFFDDLN(R2)   LIST SYSTEM                     
         MVC   LISTITYP,BUFFDNTY-BUFFDDLN(R2)   TYPE NUMBER                     
         GOTOR VLISTIO,LISTIOBK                                                 
         JNE   GETDTY20                                                         
*                                                                               
         MVC   0(L'LISTITNM,R3),LISTITNM                                        
         J     EXITY                                                            
         DROP  R4                                                               
*                                                                               
GETDTY20 DS    0H                                                               
         MVC   0(3,R3),=C'TY='                                                  
         GOTOR CHEXOUT,DMCB,0(R2),3(R3),4                                       
         J     EXITN                                                            
                                                                                
***********************************************************************         
* Get Data  Group code from Data Group number                         *         
* P1=a(dgn), P2=a(o/p)                                                *         
***********************************************************************         
GETDGC   NTR1  LABEL=NO                                                         
         LM    R2,R3,0(R1)                                                      
         MVI   BUFFFLAG,0                                                       
         XC    BUFFKEY(BUFFKEYL),BUFFKEY                                        
         L     RF,DI_ADSD                                                       
         MVC   BUFFKAGY,DS_ALF-DS_D(RF)                                         
         MVI   BUFFKTYP,BUFFDGNQ                                                
         MVC   BUFFKDGN,0(R2)                                                   
         MVC   KEY(BUFFKEYL),BUFFKEY                                            
         GOTOR DI_ABFIN,DMCB,('BUFFAGET',ABUFF1),BUFFREC,COMFACSD               
         CLI   BUFFERRS-BUFFPARM(R1),0                                          
         JE    GETDGC90                                                         
*                                                                               
         MVC   BUFFKEY(BUFFKEYL),KEY                                            
         LA    R4,KEY              not in buffer, get from disk                 
         USING SALAREC,R4                                                       
         XC    SALAKEY,SALAKEY                                                  
         MVI   SALATYP,SALATYPQ                                                 
         MVI   SALASUB,SALASUBQ                                                 
         L     RF,DI_ADSD                                                       
         MVC   SALAAGY,DS_ALF-DS_D(RF)                                          
         MVC   SALAAGN,0(R2)                                                    
         DROP  R4                                                               
                                                                                
         GOTOR CDATAMGR,DMCB,DMREAD,CTFILE,KEY,IO                               
         CLI   8(R1),0                                                          
         JNE   GETDGC40            record not found                             
         MVI   ELCODE,SALANELQ     Data group name element                      
         LA    R1,IO                                                            
         BRAS  RE,GETEL                                                         
         JNE   GETDGC40                                                         
         MVC   BUFFDDGC,SALANCOD-SALAND(R1)                                     
         J     GETDGC60                                                         
GETDGC40 OI    BUFFFLAG,BUFFFRNF   indicate unknown Data group                  
         MVC   BUFFDDGC(4),=C'DGR='                                             
         GOTOR CHEXOUT,DMCB,BUFFKDGN,BUFFDDGC+4,2                               
GETDGC60 GOTOR DI_ABFIN,DMCB,('BUFFAPUT',ABUFF1),BUFFREC,COMFACSD               
         JE    GETDGC90                                                         
         DC    H'0'                                                             
GETDGC90 MVC   0(L'BUFFDDGC,R3),BUFFDDGC return Data group                      
         TM    BUFFFLAG,BUFFFRNF   EXITN if unknown Data group                  
         JO    EXITN                                                            
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Get System name from OVSYS                                          *         
* P1=a(sys), P2=a(o/p)                                                *         
***********************************************************************         
GETSYS   NTR1  LABEL=NO                                                         
         LM    R2,R3,0(R1)                                                      
         L     RE,ASYSLST                                                       
         LH    R0,0(,RE)                                                        
         L     R1,2(,RE)                                                        
         AHI   RE,6                                                             
         USING SYSLSTD,RE                                                       
                                                                                
         BASR  RF,0                                                             
         CLC   SYSLNUM,0(R2)       look up system                               
         JE    GETSYS10                                                         
         BXLE  RE,R0,0(RF)                                                      
                                                                                
         MVC   0(4,R3),=C'SYS='                                                 
         GOTOR CHEXOUT,DMCB,(R2),4(R3),1                                        
         J     EXITN               not found, so return hex sys                 
                                                                                
GETSYS10 MVC   0(L'SYSLNAME,R3),SYSLNAME found, return system name              
         J     EXITY                                                            
         DROP  RE                                                               
                                                                                
***********************************************************************         
* Get Program name from OVSYS and Program number                      *         
* P1=a(sys/prog), P2=a(o/p)                                           *         
***********************************************************************         
GETPGM   NTR1  LABEL=NO                                                         
         LM    R2,R3,0(R1)                                                      
         L     RE,VSELIST                                                       
         LH    R0,0(,RE)                                                        
         L     R1,2(,RE)                                                        
         AHI   RE,6                                                             
         USING SELISTD,RE                                                       
                                                                                
         BASR  RF,0                                                             
         CLC   SEOVSYS,0(R2)                                                    
         JE    *+12                                                             
         BXLE  RE,R0,0(RF)                                                      
         J     GETPGM20            system not found                             
                                                                                
         L     RE,SEPGMS           find program entry                           
         LH    R0,0(,RE)                                                        
         L     R1,2(,RE)                                                        
         AHI   RE,6                                                             
         USING PGMLSTD,RE                                                       
                                                                                
         BASR  RF,0                                                             
         CLC   PGMNUM,1(R2)                                                     
         JNE   GETPGM10                                                         
         L     R4,DI_ADAD                                                       
         CLC   PGMCTRY,DA_CTRY-DA_D(R4) match on country                        
         JE    GETPGM30                                                         
         CLI   PGMCTRY,0           or default country (comes last)              
         JE    GETPGM30                                                         
GETPGM10 BXLE  RE,R0,0(RF)                                                      
                                                                                
GETPGM20 MVC   0(3,R3),=C'PG='                                                  
         GOTOR CHEXOUT,DMCB,(R2),3(R3),2                                        
         J     EXITN               not found, so return hex sys/prog            
                                                                                
GETPGM30 MVC   0(L'PGMNAME,R3),PGMNAME found program                            
         J     EXITY                                                            
         DROP  RE                                                               
                                                                                
***********************************************************************         
* Get 8 char Record name from OVSYS, Program number and Record number *         
* P1=a(record number), P2=a(o/p), SYSPGM=OVSYS and Program number     *         
***********************************************************************         
GETRCD   NTR1  LABEL=NO                                                         
         LM    R2,R3,0(R1)                                                      
         XC    BUFFKEY(BUFFKEYL),BUFFKEY                                        
         MVI   BUFFKTYP,BUFFRCDQ                                                
         MVC   BUFFKSYP,SYSPGM                                                  
         MVC   BUFFKRCD,0(R2)                                                   
         MVC   KEY(BUFFKEYL),BUFFKEY                                            
         GOTOR DI_ABFIN,DMCB,('BUFFAGET',ABUFF1),BUFFREC,COMFACSD               
         CLI   BUFFERRS-BUFFPARM(R1),0                                          
         JE    GETRCD90                                                         
*                                                                               
         MVC   BUFFKEY(BUFFKEYL),KEY                                            
         LA    R4,KEY              not in buffer, get from disk                 
         USING SARCREC,R4                                                       
         XC    SARCKEY,SARCKEY                                                  
         MVI   SARCTYP,SARCTYPQ                                                 
         MVI   SARCSUB,SARCSUBQ                                                 
         MVC   SARCOVPG,BUFFKSYP                                                
         MVC   SARCRCD,BUFFKRCD                                                 
         DROP  R4                                                               
GETRCD10 GOTOR CDATAMGR,DMCB,DMREAD,CTFILE,KEY,IO                               
         CLI   8(R1),0                                                          
         JNE   GETRCD20            record not found                             
         MVI   ELCODE,SARCDELQ     Record element                               
         LA    R1,IO                                                            
         BRAS  RE,GETEL                                                         
         JNE   GETRCD20            element not found                            
         MVI   BUFFFLAG,0          indicate known record                        
         MVI   BUFFDRCD,34         expand name 34=dd escape left                
         MVC   BUFFDRCD+1(2),SARCDWRD-SARCDD(R1)                                
         MVI   BUFFDRCD+3,L'BUFFDRCD                                            
         MVC   DMCB(2),=C'SL'                                                   
         MVC   DMCB+2(1),SYSPGM                                                 
         MVI   DMCB+3,1            always English                               
         GOTOR VDICTATE,DMCB,,BUFFDRCD,0                                        
         J     GETRCD40                                                         
GETRCD20 OI    BUFFFLAG,BUFFFRNF   indicate unknown record                      
         MVC   BUFFDRCD(5),=C'RCRD='                                            
         LLC   R0,BUFFKRCD                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BUFFDRCD+5(3),DUB+6(2)                                           
GETRCD40 GOTOR DI_ABFIN,DMCB,('BUFFAPUT',ABUFF1),BUFFREC,COMFACSD               
         JE    GETRCD90                                                         
         DC    H'0'                                                             
GETRCD90 MVC   0(L'BUFFDRCD,R3),BUFFDRCD return pid                             
         TM    BUFFFLAG,BUFFFRNF   EXITN if unknown record                      
         JO    EXITN                                                            
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Get 8 char Action name from OVSYS, Program number and Action number *         
* P1=a(action number), P2=a(o/p), SYSPGM=OVSYS and Program number     *         
***********************************************************************         
GETACT   NTR1  LABEL=NO                                                         
         LM    R2,R3,0(R1)                                                      
         XC    BUFFKEY(BUFFKEYL),BUFFKEY                                        
         MVI   BUFFKTYP,BUFFACTQ                                                
         MVC   BUFFKSYP,SYSPGM                                                  
         MVC   BUFFKACT,0(R2)                                                   
         MVC   KEY(BUFFKEYL),BUFFKEY                                            
         GOTOR DI_ABFIN,DMCB,('BUFFAGET',ABUFF1),BUFFREC,COMFACSD               
         CLI   BUFFERRS-BUFFPARM(R1),0                                          
         JE    GETACT90                                                         
*                                                                               
         MVC   BUFFKEY(BUFFKEYL),KEY                                            
         LA    R4,KEY              not in buffer, get from disk                 
         USING SAACREC,R4                                                       
         XC    SAACKEY,SAACKEY                                                  
         MVI   SAACTYP,SAACTYPQ                                                 
         MVI   SAACSUB,SAACSUBQ                                                 
         MVC   SAACOVPG,BUFFKSYP                                                
         MVC   SAACACT,BUFFKACT                                                 
         DROP  R4                                                               
GETACT10 GOTOR CDATAMGR,DMCB,DMREAD,CTFILE,KEY,IO                               
         CLI   8(R1),0                                                          
         JNE   GETACT20            record not found                             
         MVI   ELCODE,SAACTELQ     Action element                               
         LA    R1,IO                                                            
         BRAS  RE,GETEL                                                         
         JNE   GETACT20            element not found                            
         MVI   BUFFFLAG,0          indicate known record                        
         MVI   BUFFDACT,34         expand name 34=dd escape left                
         MVC   BUFFDACT+1(2),SAACTWRD-SAACTD(R1)                                
         MVI   BUFFDACT+3,L'BUFFDACT                                            
         MVC   DMCB(2),=C'SL'                                                   
         MVC   DMCB+2(1),SYSPGM                                                 
         MVI   DMCB+3,1            always English                               
         GOTOR VDICTATE,DMCB,,BUFFDACT,0                                        
         J     GETACT40                                                         
GETACT20 OI    BUFFFLAG,BUFFFRNF   indicate unknown action                      
         MVC   BUFFDACT(5),=C'ACTN='                                            
         LLC   R0,BUFFKACT                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BUFFDACT+5(3),DUB+6(2)                                           
GETACT40 GOTOR DI_ABFIN,DMCB,('BUFFAPUT',ABUFF1),BUFFREC,COMFACSD               
         JE    GETACT90                                                         
         DC    H'0'                                                             
GETACT90 MVC   0(L'BUFFDRCD,R3),BUFFDRCD return pid                             
         TM    BUFFFLAG,BUFFFRNF   EXITN if unknown action                      
         JO    EXITN                                                            
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Set PGMACT from program record                                      *         
* No parms. SYSPGM=OVSYS and Program number                           *         
***********************************************************************         
SETPGMAC CLC   PGMACTSP,SYSPGM     do we already have the right one             
         JE    EXITYRE             yes, exit                                    
SETPGA10 NTR1  LABEL=NO                                                         
         MVC   PGMACTSP,SYSPGM     set SYSPGM of PGMACT table                   
         LA    R4,KEY              get from disk                                
         USING SAPGREC,R4                                                       
         XC    SAPGKEY,SAPGKEY                                                  
         MVI   SAPGTYP,SAPGTYPQ                                                 
         MVI   SAPGSUB,SAPGSUBQ                                                 
         MVC   SAPGOVPG,SYSPGM                                                  
         DROP  R4                                                               
         GOTOR CDATAMGR,DMCB,DMREAD,CTFILE,KEY,IO                               
         CLI   8(R1),0                                                          
         JNE   SETPGA20            record not found                             
         MVI   ELCODE,SAPGMELQ     Program element                              
         LA    R1,IO                                                            
         BRAS  RE,GETEL                                                         
         JNE   SETPGA20            element not found                            
         MVC   PGMACT,SAPGMACT-SAPGMD(R1)                                       
         J     EXITY                                                            
SETPGA20 XC    PGMACT,PGMACT       not found                                    
         J     EXITN                                                            
                                                                                
***********************************************************************         
* Get 8 char Field name from OVSYS, Program number and Field number   *         
* P1=a(field number), P2=a(o/p), SYSPGM=OVSYS and Program number      *         
***********************************************************************         
GETFLD   NTR1  LABEL=NO                                                         
         LM    R2,R3,0(R1)                                                      
         XC    BUFFKEY(BUFFKEYL),BUFFKEY                                        
         MVI   BUFFKTYP,BUFFFLDQ                                                
         MVC   BUFFKSYP,SYSPGM                                                  
         MVC   BUFFKFLD,0(R2)                                                   
         MVC   KEY(BUFFKEYL),BUFFKEY                                            
         GOTOR DI_ABFIN,DMCB,('BUFFAGET',ABUFF1),BUFFREC,COMFACSD               
         CLI   BUFFERRS-BUFFPARM(R1),0                                          
         JE    GETFLD90                                                         
*                                                                               
         MVC   BUFFKEY(BUFFKEYL),KEY                                            
         LA    R4,KEY              not in buffer, get from disk                 
         USING SAFDREC,R4                                                       
         XC    SAFDKEY,SAFDKEY                                                  
         MVI   SAFDTYP,SAFDTYPQ                                                 
         MVI   SAFDSUB,SAFDSUBQ                                                 
         MVC   SAFDOVPG,BUFFKSYP                                                
         MVC   SAFDFCD,BUFFKFLD                                                 
         DROP  R4                                                               
GETFLD10 GOTOR CDATAMGR,DMCB,DMREAD,CTFILE,KEY,IO                               
         CLI   8(R1),0                                                          
         JNE   GETFLD20            record not found                             
         MVI   ELCODE,SAFLDELQ     Field element                                
         LA    R1,IO                                                            
         BRAS  RE,GETEL                                                         
         JNE   GETFLD20            element not found                            
         MVI   BUFFFLAG,0          indicate known record                        
         MVI   BUFFDFLD,34         expand name 34=dd escape left                
         MVC   BUFFDFLD+1(2),SAFLDWRD-SAFLDD(R1)                                
         MVI   BUFFDFLD+3,L'BUFFDFLD                                            
         MVC   DMCB(2),=C'SL'                                                   
         MVC   DMCB+2(1),SYSPGM                                                 
         MVI   DMCB+3,1            always English                               
         GOTOR VDICTATE,DMCB,,BUFFDFLD,0                                        
         J     GETFLD40                                                         
GETFLD20 OI    BUFFFLAG,BUFFFRNF   indicate unknown field                       
         MVC   BUFFDFLD(5),=C'FLDN='                                            
         LLC   R0,BUFFKFLD                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BUFFDFLD+5(3),DUB+6(2)                                           
GETFLD40 GOTOR DI_ABFIN,DMCB,('BUFFAPUT',ABUFF1),BUFFREC,COMFACSD               
         JE    GETFLD90                                                         
         DC    H'0'                                                             
GETFLD90 MVC   0(L'BUFFDRCD,R3),BUFFDRCD return pid                             
         TM    BUFFFLAG,BUFFFRNF   EXITN if unknown field                       
         JO    EXITN                                                            
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Get 8 char Option name from OVSYS, Program number and Option number *         
* P1=a(option number), P2=a(o/p), SYSPGM=OVSYS and Program number     *         
***********************************************************************         
GETOPT   NTR1  LABEL=NO                                                         
         LM    R2,R3,0(R1)                                                      
         XC    BUFFKEY(BUFFKEYL),BUFFKEY                                        
         MVI   BUFFKTYP,BUFFOPTQ                                                
         MVC   BUFFKSYP,SYSPGM                                                  
         MVC   BUFFKOPT,0(R2)                                                   
         MVC   KEY(BUFFKEYL),BUFFKEY                                            
         GOTOR DI_ABFIN,DMCB,('BUFFAGET',ABUFF1),BUFFREC,COMFACSD               
         CLI   BUFFERRS-BUFFPARM(R1),0                                          
         JE    GETOPT90                                                         
*                                                                               
         MVC   BUFFKEY(BUFFKEYL),KEY                                            
         LA    R4,KEY              not in buffer, get from disk                 
         USING SAOPREC,R4                                                       
         XC    SAOPKEY,SAOPKEY                                                  
         MVI   SAOPTYP,SAOPTYPQ                                                 
         MVI   SAOPSUB,SAOPSUBQ                                                 
         MVC   SAOPOVPG,BUFFKSYP                                                
         MVC   SAOPOCD,BUFFKOPT                                                 
         DROP  R4                                                               
GETOPT10 GOTOR CDATAMGR,DMCB,DMREAD,CTFILE,KEY,IO                               
         CLI   8(R1),0                                                          
         JNE   GETOPT20            record not found                             
         MVI   ELCODE,SAOPTELQ     Option element                               
         LA    R1,IO                                                            
         BRAS  RE,GETEL                                                         
         JNE   GETOPT20            element not found                            
         MVI   BUFFFLAG,0          indicate known record                        
         MVI   BUFFDOPT,34         expand name 34=dd escape left                
         MVC   BUFFDOPT+1(2),SAOPTWRD-SAOPTD(R1)                                
         MVI   BUFFDOPT+3,L'BUFFDOPT                                            
         MVC   DMCB(2),=C'SL'                                                   
         MVC   DMCB+2(1),SYSPGM                                                 
         MVI   DMCB+3,1            always English                               
         GOTOR VDICTATE,DMCB,,BUFFDOPT,0                                        
         J     GETOPT40                                                         
GETOPT20 OI    BUFFFLAG,BUFFFRNF   indicate unknown option                      
         MVC   BUFFDOPT(5),=C'OPTN='                                            
         LLC   R0,BUFFKOPT                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BUFFDOPT+5(3),DUB+6(2)                                           
GETOPT40 GOTOR DI_ABFIN,DMCB,('BUFFAPUT',ABUFF1),BUFFREC,COMFACSD               
         JE    GETOPT90                                                         
         DC    H'0'                                                             
GETOPT90 MVC   0(L'BUFFDRCD,R3),BUFFDRCD return pid                             
         TM    BUFFFLAG,BUFFFRNF   EXITN if unknown option                      
         JO    EXITN                                                            
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Output changes to action, field or option elements                  *         
* P1=a(name edit routine), P2=max number of bytes of bits in elements *         
* BLOCK1/2 contain 1 byte of old/new output data per bit, see BLOCKIN *         
* Routine checks action and blanks old or new values as appropriate   *         
***********************************************************************         
BLOCKOUT NTR1  LABEL=NO                                                         
         LM    R2,R3,0(R1)         R2=a(edit rtn), R3=#bytes of bits            
         SLA   R3,3                #bytes*8 = #bits = #bytes in BLOCK1          
         JZ    EXITY               exit if none                                 
         LA    R4,BLOCK1           R4=A(first bit value)                        
         XR    R5,R5               Action number (1st bit = 0 - unused)         
         L     RF,DI_ADSD          pick up sort key                             
         CLI   DS_TYPE-DS_D(RF),4  make quite sure action valid                 
         JNH   *+6                                                              
         DC    H'0'                                                             
         LLC   R6,DS_TYPE-DS_D(RF) pick up action                               
         SLL   R6,2                times 4 for branch                           
BLOCKO10 CLC   0(1,R4),BLOCK2-BLOCK1(R4) this value changed?                    
         JE    BLOCKO40            no, next                                     
         STC   R5,BITIX            set bit number for edit routine              
         GOTOR (R2)                edit out action                              
         JNE   BLOCKO40            no longer exists, so ignore this one         
BLOCKO20 BASR  RF,0                                                             
         B     0(R6,RF)                                                         
         J     BLOCKO21            R6=4*1 - add                                 
         J     BLOCKO22            R6=4*2 - delete                              
         J     BLOCKO21            R6=4*3 - restore same as add                 
         J     BLOCKO24            R6=4*4 - change                              
BLOCKO21 MVI   DI_OVAL,C' '        add/restore - no old                         
         MVC   DI_NVAL(1),BLOCK2-BLOCK1(R4) new value                           
         J     BLOCKO30                                                         
BLOCKO22 MVC   DI_OVAL(1),0(R4)    delete - no new                              
         MVI   DI_NVAL,C' '                                                     
         J     BLOCKO30                                                         
BLOCKO24 MVC   DI_OVAL(1),0(R4)    change has old and new                       
         MVC   DI_NVAL(1),BLOCK2-BLOCK1(R4)                                     
BLOCKO30 GOTOR DI_APUT                                                          
BLOCKO40 AHI   R4,1                next action byte                             
         AHI   R5,1                                                             
         CR    R5,R3               all done?                                    
         JL    BLOCKO10                                                         
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Convert bits in action, field or option elements to output bytes    *         
* P1=al1(number of bytes of bits),al3(first byte of bits)             *         
* P2=cl1'o/p char if on',al3(256 byte output block) one byte per bit  *         
***********************************************************************         
BLOCKIN  NTR1  LABEL=NO                                                         
         LM    R2,R3,0(R1)         R2=current i/p, R3=current o/p               
         LLC   R4,0(R1)            R4=#i/p bytes                                
         LLC   R5,4(R1)            R5=o/p character                             
         BASR  RF,0                RF=Byte loop                                 
         LA    R0,1                make sure R0 always non zero                 
         ICM   R0,8,0(R2)          pick up next i/p byte in hob                 
         LA    R1,8                8 bits per byte                              
         BASR  RE,0                RE=Bit loop                                  
         LTR   R0,R0               test sign (hi bit)                           
         JP    *+8                 positive if current bit is off               
         STC   R5,0(R3)            bit on, set o/p byte                         
         SLL   R0,1                next bit                                     
         AHI   R3,1                next o/p byte                                
         BCTR  R1,RE               do all 8 bits                                
         AHI   R2,1                next i/p byte                                
         BCTR  R4,RF               do all bytes                                 
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Get user profile format data                                        *         
* P1=a(system/program CTUREC format), P2=a(o/p)                       *         
* Returns 16 bytes, one per profile. Each is null if profile not used *         
* or C,N or H as per CTFDTYPE field for the profile.                  *         
***********************************************************************         
GETUPF   NTR1  LABEL=NO                                                         
         LM    R2,R3,0(R1)                                                      
         MVI   0(R3),C'H'          Default to all hex                           
         MVC   1(15,R3),0(R3)                                                   
         XC    BUFFKEY(BUFFKEYL),BUFFKEY                                        
         MVI   BUFFKTYP,BUFFUPFQ                                                
         MVC   BUFFKUSP,0(R2)                                                   
         MVI   BUFFKUSQ,C'1'                                                    
         MVC   KEY(BUFFKEYL),BUFFKEY                                            
         GOTOR DI_ABFIN,DMCB,('BUFFAGET',ABUFF1),BUFFREC,COMFACSD               
         CLI   BUFFERRS-BUFFPARM(R1),0                                          
         JE    GETUPF80                                                         
*                                                                               
         MVC   BUFFKEY(BUFFKEYL),KEY                                            
         LA    R4,KEY              not in buffer, get from disk                 
         USING CTUREC,R4                                                        
         XC    CTUKEY,CTUKEY                                                    
         MVI   CTUKTYP,CTUKTYPQ                                                 
         MVC   CTUKSYS(L'BUFFKUSP),BUFFKUSP                                     
         DROP  R4                                                               
GETUPF10 GOTOR CDATAMGR,DMCB,DMREAD,CTFILE,KEY,IO                               
         CLI   8(R1),0                                                          
         JNE   GETUPF20            record not found                             
         XC    0(16,R3),0(R3)      found, preset none used                      
         MVI   ELCODE,CTFDELQ      definition element                           
         LA    R1,IO                                                            
         BRAS  RE,GETEL                                                         
         JNE   GETUPF20            no elements found                            
GETUPF12 LLC   RE,CTFDNUM-CTFDD(R1) pick up number                              
         BCTR  RE,0                                                             
         AR    RE,R3               locate corresponding byte                    
         MVC   0(1,RE),CTFDTYPE-CTFDD(R1) copy format                           
         BRAS  RE,NEXTEL                                                        
         JE    GETUPF12                                                         
         J     GETUPF40            no more elements                             
GETUPF20 OI    BUFFFLAG,BUFFFRNF   indicate unknown                             
GETUPF40 MVC   BUFFDUPF,0(R3)                                                   
         GOTOR DI_ABFIN,DMCB,('BUFFAPUT',ABUFF1),BUFFREC,COMFACSD               
         JE    *+6                                                              
         DC    H'0'                                                             
         OC    8(8,R3),8(R3)       is the second 8 used?                        
         JZ    GETUPF90            no, skip                                     
         MVI   BUFFKUSQ,C'2'       add second record                            
         MVC   BUFFDUPF,8(R3)                                                   
         GOTOR DI_ABFIN,DMCB,('BUFFAPUT',ABUFF1),BUFFREC,COMFACSD               
         JE    GETUPF90                                                         
         DC    H'0'                                                             
GETUPF80 MVC   0(8,R3),BUFFDUPF    found first 8                                
         XC    8(8,R3),8(R3)       clear second 8                               
         MVI   BUFFKUSQ,C'2'       read second 8                                
         IC    R0,BUFFFLAG                                                      
         GOTOR DI_ABFIN,DMCB,('BUFFAGET',ABUFF1),BUFFREC,COMFACSD               
         STC   R0,BUFFFLAG         preserve BUFFFLAG                            
         CLI   BUFFERRS-BUFFPARM(R1),0                                          
         JNE   GETUPF90            second 8 not found                           
         MVC   8(8,R3),BUFFDUPF    copy second 8                                
GETUPF90 TM    BUFFFLAG,BUFFFRNF   EXITN if unknown                             
         JO    EXITN                                                            
         J     EXITY                                                            
         EJECT                                                                  
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
                                                                                
EXITYRE  CR    RE,RE               SET CONDITION CODE TO EQUAL FOR RE           
         BR    RE                                                               
EXITNRE  LTR   RE,RE               SET CONDITION CODE TO NEQ FOR RE             
         BR    RE                                                               
                                                                                
         GETEL R1,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* File table                                                          *         
***********************************************************************         
                                                                                
FILTAB   DS    0XL(DF_LNQ)                                                      
                                                                                
         DC    X'A1',CL(L'DF_NAMEF)'CTFILE'                                     
         DC    AL1(DF_TVIS)        VARIABLE LENGTH I/S                          
         DC    AL1(L'SAPEKEY),AL1(L'SAPESTAT),AL1(SAPEDATA-SAPEREC)             
         DC    AL4(CTFREC)                                                      
                                                                                
         DC    X'AF',CL(L'DF_NAMEF)'GENFIL'                                     
         DC    AL1(DF_TDA)                                                      
         DC    AL1(L'MOFKEY),AL1(L'MOFKSTAT),AL1(MOFFIRST)                      
         DC    AL4(GEFREC)                                                      
                                                                                
FILTABX  DC    AL1(DF_EOTQ)                                                     
         EJECT                                                                  
***********************************************************************         
* Record definition tables (CTFILE)                                   *         
***********************************************************************         
                                                                                
CTFREC   DS    0X                  ** CTFILE RECORD DEFINITIONS **              
                                                                                
FILPER   DS    0X                  ** PERSON RECORD DEFINITION **               
         DC    AL1(FILPERX-*)                                                   
PERRECQ  EQU   1                   RECORD INDENTIFIER                           
         DC    AL2(PERRECQ)                                                     
         DC    AL1(DR_IMCRQ,0)                                                  
         DC    CL(L'DR_NAMEF)'PERSON'                                           
         DC    CL(L'DR_NAMES)'Person'                                           
         DC    AL1(SAPEAGY-SAPEKEY,0)                                           
         DC    AL1(0)                                                           
         DC    AL1(DR_ITALF)                                                    
         DC    AL4(PERFLT,PERKEY,PERFLD)                                        
                                                                                
PERARG1  DC    AL1(PERARG2-*)                                                   
         DC    AL1(SAPETYP-SAPEKEY)                                             
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(SAPETYPQ)                                                    
                                                                                
PERARG2  DC    AL1(PERARG3-*)                                                   
         DC    AL1(SAPESUB-SAPEKEY)                                             
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(SAPESUBQ)                                                    
                                                                                
PERARG3  DS    0X                                                               
                                                                                
FILPERX  DS    0X                                                               
                                                                                
FILSYS   DS    0X                  ** SYSTEM RECORD DEFINITION **               
         DC    AL1(FILSYSX-*)                                                   
SYSRECQ  EQU   2                   RECORD INDENTIFIER                           
         DC    AL2(SYSRECQ)                                                     
         DC    AL1(0,0)                                                         
         DC    CL(L'DR_NAMEF)'SYSTEM'                                           
         DC    CL(L'DR_NAMES)'System'                                           
         DC    AL1(SA0KAGY-SA0KEY,0)                                            
         DC    AL1(0)                                                           
         DC    AL1(DR_ITALF)                                                    
         DC    AL4(0,SYSKEY,SYSFLD)                                             
                                                                                
SYSARG1  DC    AL1(SYSARG2-*)                                                   
         DC    AL1(SA0KTYP-SA0KEY)                                              
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(SA0KTYPQ)                                                    
                                                                                
SYSARG2  DC    AL1(SYSARG3-*)                                                   
         DC    AL1(SA0KEYS-SA0KEY)                                              
         DC    AL1(DR_ATBZ)                                                     
         DC    AL1(SA0KNUM-SA0KEYS)                                             
                                                                                
SYSARG3  DS    0X                                                               
                                                                                
FILSYSX  DS    0X                                                               
                                                                                
FILOFF   DS    0X                  ** OFFICE RECORD DEFINITION **               
         DC    AL1(FILOFFX-*)                                                   
OFFRECQ  EQU   3                   RECORD INDENTIFIER                           
         DC    AL2(OFFRECQ)                                                     
         DC    AL1(0,0)                                                         
         DC    CL(L'DR_NAMEF)'OFFICE'                                           
         DC    CL(L'DR_NAMES)'Office'                                           
         DC    AL1(SAOFAGY-SAOFKEY,0)                                           
         DC    AL1(0)                                                           
         DC    AL1(DR_ITALF)                                                    
         DC    AL4(0,OFFKEY,OFFFLD)                                             
                                                                                
OFFARG1  DC    AL1(OFFARG2-*)                                                   
         DC    AL1(SAOFTYP-SAOFKEY)                                             
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(SAOFTYPQ)                                                    
                                                                                
OFFARG2  DC    AL1(OFFARG3-*)                                                   
         DC    AL1(SAOFSUB-SAOFKEY)                                             
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(SAOFSUBQ)                                                    
                                                                                
OFFARG3  DS    0X                                                               
                                                                                
FILOFFX  DS    0X                                                               
                                                                                
FILDPT   DS    0X                  ** DEPARTMENT RECORD DEFINITION **           
         DC    AL1(FILDPTX-*)                                                   
DPTRECQ  EQU   4                   RECORD INDENTIFIER                           
         DC    AL2(DPTRECQ)                                                     
         DC    AL1(0,0)                                                         
         DC    CL(L'DR_NAMEF)'DEPT'                                             
         DC    CL(L'DR_NAMES)'Dept'                                             
         DC    AL1(SADPAGY-SADPKEY,0)                                           
         DC    AL1(0)                                                           
         DC    AL1(DR_ITALF)                                                    
         DC    AL4(0,DPTKEY,DPTFLD)                                             
                                                                                
DPTARG1  DC    AL1(DPTARG2-*)                                                   
         DC    AL1(SADPTYP-SADPKEY)                                             
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(SADPTYPQ)                                                    
                                                                                
DPTARG2  DC    AL1(DPTARG3-*)                                                   
         DC    AL1(SADPSUB-SADPKEY)                                             
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(SADPSUBQ)                                                    
                                                                                
DPTARG3  DS    0X                                                               
                                                                                
FILDPTX  DS    0X                                                               
                                                                                
FILGRP   DS    0X                  ** GROUP RECORD DEFINITION **                
         DC    AL1(FILGRPX-*)                                                   
GRPRECQ  EQU   5                   RECORD INDENTIFIER                           
         DC    AL2(GRPRECQ)                                                     
         DC    AL1(0,0)                                                         
         DC    CL(L'DR_NAMEF)'GROUP'                                            
         DC    CL(L'DR_NAMES)'Group'                                            
         DC    AL1(SAAGAGY-SAAGKEY,0)                                           
         DC    AL1(0)                                                           
         DC    AL1(DR_ITALF)                                                    
         DC    AL4(0,GRPKEY,GRPFLD)                                             
                                                                                
GRPARG1  DC    AL1(GRPARG2-*)                                                   
         DC    AL1(SAAGTYP-SAAGKEY)                                             
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(SAAGTYPQ)                                                    
                                                                                
GRPARG2  DC    AL1(GRPARG3-*)                                                   
         DC    AL1(SAAGSUB-SAAGKEY)                                             
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(SAAGSUBQ)                                                    
                                                                                
GRPARG3  DC    AL1(GRPARG4-*)                                                   
         DC    AL1(SAAGAGR-SAAGKEY)                                             
         DC    AL1(DR_ATNBZ)                                                    
         DC    AL1(L'SAAGAGR)                                                   
                                                                                
GRPARG4  DS    0X                                                               
                                                                                
FILGRPX  DS    0X                                                               
                                                                                
FILACC   DS    0X                  ** ACCESS RECORD DEFINITION **               
         DC    AL1(FILACCX-*)                                                   
ACCRECQ  EQU   6                   RECORD INDENTIFIER                           
         DC    AL2(ACCRECQ)                                                     
         DC    AL1(0,0)                                                         
         DC    CL(L'DR_NAMEF)'ACCESS'                                           
         DC    CL(L'DR_NAMES)'Access'                                           
         DC    AL1(SAASAGY-SAASKEY,0)                                           
         DC    AL1(0)                                                           
         DC    AL1(DR_ITALF)                                                    
         DC    AL4(0,ACCKEY,ACCFLD)                                             
                                                                                
ACCARG1  DC    AL1(ACCARG2-*)                                                   
         DC    AL1(SAASTYP-SAASKEY)                                             
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(SAASTYPQ)                                                    
                                                                                
ACCARG2  DC    AL1(ACCARG3-*)                                                   
         DC    AL1(SAASSUB-SAASKEY)                                             
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(SAASSUBQ)                                                    
                                                                                
ACCARG3  DS    0X                                                               
                                                                                
FILACCX  DS    0X                                                               
                                                                                
FILFCN   DS    0X                  ** FCON RECORD DEFINITION **                 
         DC    AL1(FILFCNX-*)                                                   
FCNRECQ  EQU   7                   RECORD INDENTIFIER                           
         DC    AL2(FCNRECQ)                                                     
         DC    AL1(0,0)                                                         
         DC    CL(L'DR_NAMEF)'FCON'                                             
         DC    CL(L'DR_NAMES)'FCon'                                             
         DC    AL1(SAFCAGY-SAFCKEY,0)                                           
         DC    AL1(0)                                                           
         DC    AL1(DR_ITALF)                                                    
         DC    AL4(0,FCNKEY,FCNFLD)                                             
                                                                                
FCNARG1  DC    AL1(FCNARG2-*)                                                   
         DC    AL1(SAFCTYP-SAFCKEY)                                             
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(SAFCTYPQ)                                                    
                                                                                
FCNARG2  DC    AL1(FCNARG3-*)                                                   
         DC    AL1(SAFCSUB-SAFCKEY)                                             
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(SAFCSUBQ)                                                    
                                                                                
FCNARG3  DS    0X                                                               
                                                                                
FILFCNX  DS    0X                                                               
                                                                                
FILOCN   DS    0X                  ** OCON RECORD DEFINITION **                 
         DC    AL1(FILOCNX-*)                                                   
OCNRECQ  EQU   8                   RECORD INDENTIFIER                           
         DC    AL2(OCNRECQ)                                                     
         DC    AL1(0,0)                                                         
         DC    CL(L'DR_NAMEF)'OCON'                                             
         DC    CL(L'DR_NAMES)'OCon'                                             
         DC    AL1(SAOCAGY-SAOCKEY,0)                                           
         DC    AL1(0)                                                           
         DC    AL1(DR_ITALF)                                                    
         DC    AL4(0,OCNKEY,OCNFLD)                                             
                                                                                
OCNARG1  DC    AL1(OCNARG2-*)                                                   
         DC    AL1(SAOCTYP-SAOCKEY)                                             
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(SAOCTYPQ)                                                    
                                                                                
OCNARG2  DC    AL1(OCNARG3-*)                                                   
         DC    AL1(SAOCSUB-SAOCKEY)                                             
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(SAOCSUBQ)                                                    
                                                                                
OCNARG3  DS    0X                                                               
                                                                                
FILOCNX  DS    0X                                                               
                                                                                
FILTSA   DS    0X                  ** T/S APP RECORD DEFINITION **              
         DC    AL1(FILTSAX-*)                                                   
TSARECQ  EQU   9                   RECORD INDENTIFIER                           
         DC    AL2(TSARECQ)                                                     
         DC    AL1(0,0)                                                         
         DC    CL(L'DR_NAMEF)'TSAPP'                                            
         DC    CL(L'DR_NAMES)'TSApp'                                            
         DC    AL1(SAAPAGY-SAAPKEY,0)                                           
         DC    AL1(0)                                                           
         DC    AL1(DR_ITALF)                                                    
         DC    AL4(0,TSAKEY,TSAFLD)                                             
                                                                                
TSAARG1  DC    AL1(TSAARG2-*)                                                   
         DC    AL1(SAAPTYP-SAAPKEY)                                             
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(SAAPTYPQ)                                                    
                                                                                
TSAARG2  DC    AL1(TSAARG3-*)                                                   
         DC    AL1(SAAPSUB-SAAPKEY)                                             
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(SAAPSUBQ)                                                    
                                                                                
TSAARG3  DC    AL1(TSAARG4-*)                                                   
         DC    AL1(SAAPAGR-SAAPKEY)                                             
         DC    AL1(DR_ATNBZ)                                                    
         DC    AL1(L'SAAPAGR)                                                   
                                                                                
TSAARG4  DS    0X                                                               
                                                                                
FILTSAX  DS    0X                                                               
                                                                                
*&&DO                                                                           
FILUP1   DS    0X                  ** PROFILE RECORD DEFINITION **              
         DC    AL1(FILUP1X-*)      ** ALPHA ID in KEY           **              
UP1RECQ  EQU   10                  RECORD INDENTIFIER                           
         DC    AL2(UP1RECQ)                                                     
         DC    AL1(0,0)                                                         
         DC    CL(L'DR_NAMEF)'USRPROF'                                          
         DC    CL(L'DR_NAMES)'UserProf'                                         
         DC    AL1(CTUKAGY-CTUKEY,0)                                            
         DC    AL1(0)                                                           
         DC    AL1(DR_ITALF)                                                    
         DC    AL4(0,UPRKEY,UPRFLD)                                             
                                                                                
UP1ARG1  DC    AL1(UP1ARG2-*)                                                   
         DC    AL1(CTUKTYP-CTUKEY)                                              
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(CTUKTYPQ)                                                    
                                                                                
UP1ARG2  DC    AL1(UP1ARG3-*)      Hex 50 and above is agy alpha                
         DC    AL1(CTUKAGY-CTUKEY)                                              
         DC    AL1(DR_ATGRQ)                                                    
         DC    X'4F'                                                            
                                                                                
UP1ARG3  DS    0X                                                               
                                                                                
FILUP1X  DS    0X                                                               
                                                                                
FILUP2   DS    0X                  ** PROFILE RECORD DEFINITION **              
         DC    AL1(FILUP2X-*)      ** USER ID in KEY           **               
UP2RECQ  EQU   UP1RECQ             RECORD INDENTIFIER                           
         DC    AL2(UP2RECQ)                                                     
         DC    AL1(0,0)                                                         
         DC    CL(L'DR_NAMEF)'USRPROF'                                          
         DC    CL(L'DR_NAMES)'UserProf'                                         
         DC    AL1(0,0)            agency alpha set in UP2FLT                   
         DC    AL1(0)                                                           
         DC    AL1(DR_ITALF)                                                    
         DC    AL4(UP2FLT,UPRKEY,UPRFLD)                                        
                                                                                
UP2ARG1  DC    AL1(UP2ARG2-*)                                                   
         DC    AL1(CTUKTYP-CTUKEY)                                              
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(CTUKTYPQ)                                                    
                                                                                
UP2ARG2  DC    AL1(UP2ARG3-*)      less than X'50' is a user id.                
         DC    AL1(CTUKAGY-CTUKEY)                                              
         DC    AL1(DR_ATLEQ)                                                    
         DC    AL1(L'CTUKAGY)                                                   
                                                                                
UP2ARG3  DC    AL1(UP2ARG4-*)      but ignore zero user id (field rec)          
         DC    AL1(CTUKAGY-CTUKEY)                                              
         DC    AL1(DR_ATNBZ)                                                    
         DC    AL1(L'CTUKAGY)                                                   
                                                                                
UP2ARG4  DS    0X                                                               
                                                                                
FILUP2X  DS    0X                                                               
*&&                                                                             
                                                                                
FILTLA   DS    0X                  ** TEXT LIMIT ACCESS LIST RECORD **          
         DC    AL1(FILTLAX-*)                                                   
TLARECQ  EQU   11                  RECORD INDENTIFIER                           
         DC    AL2(TLARECQ)                                                     
         DC    AL1(0,0)                                                         
         DC    CL(L'DR_NAMEF)'TXTLAL'                                           
         DC    CL(L'DR_NAMES)'TxtLAL'                                           
         DC    AL1(SATLAGY-SATLKEY,0)                                           
         DC    AL1(0)                                                           
         DC    AL1(DR_ITALF)                                                    
         DC    AL4(0,TLAKEY,TLAFLD)                                             
                                                                                
TLAARG1  DC    AL1(TLAARG2-*)                                                   
         DC    AL1(SATLTYP-SATLKEY)                                             
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(SATLTYPQ)                                                    
                                                                                
TLAARG2  DC    AL1(TLAARG3-*)                                                   
         DC    AL1(SATLSUB-SATLKEY)                                             
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(SATLSUBQ)                                                    
                                                                                
TLAARG3  DS    0X                                                               
                                                                                
FILTLAX  DS    0X                                                               
                                                                                
FILDAT   DS    0X                  ** DATA GROUP RECORD DEFINITION **           
         DC    AL1(FILDATX-*)                                                   
DATRECQ  EQU   12                  RECORD INDENTIFIER                           
         DC    AL2(DATRECQ)                                                     
         DC    AL1(0,0)                                                         
         DC    CL(L'DR_NAMEF)'DATA'                                             
         DC    CL(L'DR_NAMES)'Data'                                             
         DC    AL1(SALAAGY-SALAKEY,0)                                           
         DC    AL1(0)                                                           
         DC    AL1(DR_ITALF)                                                    
         DC    AL4(0,DATKEY,DATFLD)                                             
                                                                                
DATARG1  DC    AL1(DATARG2-*)                                                   
         DC    AL1(SALATYP-SALAKEY)                                             
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(SALATYPQ)                                                    
                                                                                
DATARG2  DC    AL1(DATARG3-*)                                                   
         DC    AL1(SALASUB-SALAKEY)                                             
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(SALASUBQ)                                                    
                                                                                
DATARG3  DC    AL1(DATARG4-*)                                                   
         DC    AL1(SALAAGR-SALAKEY)                                             
         DC    AL1(DR_ATNBZ)                                                    
         DC    AL1(L'SALAAGR)                                                   
                                                                                
DATARG4  DC    AL1(DATARG5-*)      ignored passive number record                
         DC    AL1(SALAAGR-SALAKEY)                                             
         DC    AL1(DR_ATNBZ)                                                    
         DC    AL1(SALAAGN-SALAAGR)                                             
                                                                                
DATARG5  DS    0X                                                               
                                                                                
FILDATX  DS    0X                                                               
                                                                                
FILRECX  DC    AL1(DR_EOTQ)                                                     
                                                                                
***********************************************************************         
* Record definition tables (GENFIL)                                   *         
***********************************************************************         
                                                                                
GEFREC   DS    0X                  ** GENFIL RECORD DEFINITIONS **              
                                                                                
GEFMOFF  DS    0X                  ** MEDIA OFFICE RECORD DEFINITION **         
         DC    AL1(GEFMOFFX-*)                                                  
MOFFRECQ EQU   1                   RECORD INDENTIFIER                           
         DC    AL2(MOFFRECQ)                                                    
         DC    AL1(0,0)                                                         
         DC    CL(L'DR_NAMEF)'MOFFICE'                                          
         DC    CL(L'DR_NAMES)'M_Office'                                         
         DC    AL1(MOFKAGY-MOFKEY,0)                                            
         DC    AL1(0)                                                           
         DC    AL1(DR_ITALF)                                                    
         DC    AL4(0,MOFFKEY,MOFFFLD)                                           
                                                                                
MOFFARG1 DC    AL1(MOFFARG2-*)                                                  
         DC    AL1(MOFKTYP-MOFKEY)                                              
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(MOFKTYPQ)                                                    
                                                                                
MOFFARG2 DC    AL1(MOFFARG3-*)                                                  
         DC    AL1(MOFKSUB-MOFKEY)                                              
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(MOFKS1Q)                                                     
                                                                                
MOFFARG3 DS    0X                                                               
                                                                                
GEFMOFFX DS    0X                                                               
                                                                                
GEFDAL   DS    0X                  **DATA ACCESS LIST REC DEFINITION**          
         DC    AL1(GEFDALX-*)                                                   
DALRECQ  EQU   2                   RECORD INDENTIFIER                           
         DC    AL2(DALRECQ)                                                     
         DC    AL1(0,0)                                                         
         DC    CL(L'DR_NAMEF)'DATA2'                                            
         DC    CL(L'DR_NAMES)'Data2'                                            
         DC    AL1(0,0)            AGENCY set in DALFLT                         
         DC    AL1(0)                                                           
         DC    AL1(DR_ITALF)                                                    
         DC    AL4(DALFLT,DALKEY,DALFLD)                                        
                                                                                
DALARG1  DC    AL1(DALARG2-*)                                                   
         DC    AL1(LDTAKMAJ-LDTAKEY)                                            
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(LDTAKMAQ)                                                    
                                                                                
DALARG2  DC    AL1(DALARG3-*)                                                   
         DC    AL1(LDTAKMIN-LDTAKEY)                                            
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(LDTAKMIQ)                                                    
                                                                                
DALARG3  DC    AL1(DALARG4-*)                                                   
         DC    AL1(LDTAKREC-LDTAKEY)                                            
         DC    AL1(DR_ATEQU)                                                    
         DC    AL1(LDTAKREQ)                                                    
                                                                                
DALARG4  DC    AL1(DALARG5-*)                                                   
         DC    AL1(LDTAKNUM-LDTAKEY)                                            
         DC    AL1(DR_ATNBZ)       ignore zero list number                      
         DC    AL1(L'LDTAKNUM)                                                  
DALARG5  DS    0X                                                               
                                                                                
GEFDALX  DS    0X                                                               
                                                                                
                                                                                
GEFRECX  DC    AL1(DR_EOTQ)                                                     
         EJECT                                                                  
***********************************************************************         
* Record filter routines                                              *         
***********************************************************************         
                                                                                
PERFLT   CLI   DI_MODE,DI_MMCRQ    ** FILPER DR_AFLT Routine **                 
         JE    PERFLT10            skip if create copy/change mode              
         J     EXITYRE             else exit cc eq                              
                                                                                
PERFLT10 L     R1,DI_AIO1          add recovery record is in io1                
         MVC   KEY,4+L'RECVHDR(R1) copy key from it                             
         LA    R2,KEY                                                           
         USING SAPEREC,R2                                                       
         XR    R0,R0                                                            
         ICM   R0,3,SAPEDEF        increment complemented effective             
         AHI   R0,1                date to get previous copy                    
         STCM  R0,3,SAPEDEF                                                     
         LR    R0,RE                                                            
         GOTOR CDATAMGR,DMCB,(0,DMRDHI),CTFILE,KEY,IO                           
         LR    RE,R0                                                            
         CLI   8(R1),0                                                          
         JNE   EXITYRE             ignore io errors                             
         CLC   SAPEKEY(SAPEDEF-SAPEKEY),IO                                      
         JNE   EXITYRE             different person, so not previous            
         DROP  R2                                                               
                                                                                
         ST    RE,DMCB                                                          
                                                                                
         L     RE,DI_AIO1          IO1 has the add recovery rec                 
         LH    RF,0(,RE)                                                        
         MVI   4+RRECTY-RECVHDR(RE),2 make it into a change                     
         L     R0,DI_AIO2                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE               move it to IO2                               
                                                                                
         LA    R0,IO               IO contains previous copy                    
         XR    R1,R1                                                            
         ICM   R1,3,IO+SAPELEN-SAPEREC                                          
         L     RE,DI_AIO1                                                       
         LA    RF,4+L'RECVHDR(,R1) get new total length                         
         STH   RF,0(,RE)                                                        
         MVI   4+RRECTY-RECVHDR(RE),1 make it into a copy                       
         AHI   RE,4+L'RECVHDR                                                   
         LR    RF,R1                                                            
         MVCL  RE,R0               move previous to IO1 as copy                 
                                                                                
         L     RE,DMCB                                                          
         J     EXITYRE                                                          
                                                                                
UP2FLT   CLI   DI_MODE,0           ** FILUP2 DR_AFLT Routine **                 
         JNE   EXITYRE             exit cc eq if wrong mode                     
         L     R2,DI_AREC          point to record                              
         USING CTUREC,R2                                                        
         LR    R0,RE                                                            
         GOTOR GETUIA,DMCB,CTUKAGY,DI_AGY get agy alph from uid                 
         LR    RE,R0                                                            
         J     EXITYRE             accept                                       
         DROP  R2                                                               
                                                                                
DALFLT   CLI   DI_MODE,0           ** DALFLT DR_AFLT Routine **                 
         JNE   EXITYRE             exit cc eq if wrong mode                     
         L     R2,DI_AREC          point to record                              
         USING LDTAD,R2                                                         
         LR    R0,RE                                                            
         GOTOR GETDLN,DMCB,LDTAKNUM,WORK     get agy alph from LSN              
         JNE   *+10                          unknown LSN                        
         MVC   DI_AGY,WORK+BUFFDNAG-BUFFDDLN                                    
         LR    RE,R0                                                            
         J     EXITYRE             accept                                       
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Person record key and data definitions followed by routines         *         
***********************************************************************         
                                                                                
PERKEY   DS    0X                  ** PERSON KEY DEFINITIONS **                 
                                                                                
         DC    AL1(DK_TCHRQ)                                                    
         DC    AL1(SAPEPID-SAPEKEY,L'SAPEPID),AL4(0)                            
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(SAPEDEF-SAPEKEY,L'SAPEDEF),AL4(EDTDEF)                       
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
PERKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
PERFLD   DS    0X                  ** PERSON FIELD DEFINITIONS **               
                                                                                
* ..... SAPEDEF Effective Date (from Key)                                       
* X'01' SAACVD  Activity Date                                                   
* X'C4' SAPWDD  Current Password                                                
* X'C5' SANAMD  Person Name                                                     
* X'C6' SAPERD  Person Details                                                  
* X'C7' SAADRD  Address (etc.)                                                  
* X'C8' SAAGCD  Access Group                                                    
* X'D0' SAOPID  Old Person Id (not handled here)                                
* X'DD' SADDSD  DDS Person Attributes (not handled here)                        
* X'DE' SADDLD  DDS Agency list (not handled here)                              
* X'E0' SAAPCD  T/S Approver                                                    
* X'E5' SAPEED  Email Id                                                        
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Effective Date'                                    
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(SAPEDEF-SAPEKEY),AL1(L'SAPEDEF)                              
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTDEF)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Activity Date'                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAACVELQ,SAACVDT-SAACVD,L'SAACVDT)                           
         DC    AL1(DD_TBDTQ)                                                    
         DC    XL16'00'                                                         
*                                                                               
* DON'T WANT TO SHOW THIS TO CLIENT, 7/22/11, YYUN                              
*        DC    AL1(DD_LDISQ)                                                    
*        DC    CL(L'DD_NAME)'Password Number'                                   
*        DC    AL1(0)                                                           
*        DC    AL1(0,0)                                                         
*        DC    AL1(SAPWDELQ,SAPWDNUM-SAPWDD,L'SAPWDNUM)                         
*        DC    AL1(DD_TBINQ)                                                    
*        DC    XL16'00'                                                         
*                                                                               
         DC    AL1(DD_LDIMQ)                                                    
         DC    CL(L'DD_NAME)'SAPWDEL'                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAPWDELQ,SAPWDNUM-SAPWDD,L'SAPWDNUM)                         
         DC    AL1(0)                                                           
         DC    AL4(0,PERFPWDK,PERFPWDD)                                         
         DC    XL4'00'                                                          
                                                                                
         DC    AL1(DD_LDIMQ)                                                    
         DC    CL(L'DD_NAME)'SANAMEL' (replaced in PERFNAME routine)            
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SANAMELQ,2,1)   unused part of element for key               
         DC    AL1(0)                                                           
         DC    AL4(0,PERFNAMK,PERFNAMD)                                         
         DC    XL4'00'                                                          
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Office'                                            
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAPERELQ,SAPEROFF-SAPERD,L'SAPEROFF)                         
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Department'                                        
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAPERELQ,SAPERDID-SAPERD,L'SAPERDID)                         
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Extension'                                         
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAPERELQ,SAPEREXT-SAPERD,L'SAPEREXT)                         
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Staff Code'                                        
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAPERELQ,SAPERSTA-SAPERD,L'SAPERSTA)                         
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Profs Id'                                          
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAPERELQ,SAPERPRO-SAPERD,L'SAPERPRO)                         
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Insurance Code'                                    
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAPERELQ,SAPERINC-SAPERD,L'SAPERINC)                         
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Marital Status'                                    
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAPERELQ,SAPERMAR-SAPERD,L'SAPERMAR)                         
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Sex'                                               
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAPERELQ,SAPERSEX-SAPERD,L'SAPERSEX)                         
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Hire Date'                                         
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAPERELQ,SAPERDHI-SAPERD,L'SAPERDHI)                         
         DC    AL1(DD_TCDTQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Term Date'                                         
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAPERELQ,SAPERDTE-SAPERD,L'SAPERDTE)                         
         DC    AL1(DD_TCDTQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Date of Birth'                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAPERELQ,SAPERDOB-SAPERD,L'SAPERDOB)                         
         DC    AL1(DD_TCDTQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Ins Code Extension'                                
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAPERELQ,SAPERINX-SAPERD,L'SAPERINX)                         
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
*                                                                               
* DON'T WANT TO SHOW THIS TO CLIENT, 7/22/11, YYUN                              
*        DC    AL1(DD_LDISQ)                                                    
*        DC    CL(L'DD_NAME)'Sec Admin'                                         
*        DC    AL1(0)                                                           
*        DC    AL1(0,0)                                                         
*        DC    AL1(SAPERELQ,SAPEROVF-SAPERD,L'SAPEROVF)                         
*        DC    AL1(DD_TCHRQ)                                                    
*        DC    XL16'00'                                                         
*                                                                               
* DON'T WANT TO SHOW THIS TO CLIENT, 7/22/11, YYUN                              
*        DC    AL1(DD_LDISQ)                                                    
*        DC    CL(L'DD_NAME)'Expire'                                            
*        DC    AL1(0)                                                           
*        DC    AL1(0,0)                                                         
*        DC    AL1(SAPERELQ,SAPERPCN-SAPERD,L'SAPERPCN)                         
*        DC    AL1(DD_TCHRQ)                                                    
*        DC    XL16'00'                                                         
*                                                                               
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Title'                                             
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAPERELQ,SAPERTIT-SAPERD,0)                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDIMQ)                                                    
         DC    CL(L'DD_NAME)'Address / Phone'                                   
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAADRELQ,SAADRTYP-SAADRD,L'SAADRTYP)                         
         DC    AL1(0)                                                           
         DC    AL4(0,PERFADRK,PERFADRD)                                         
         DC    XL4'00'                                                          
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Security Group'                                    
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAAGCELQ,SAAGCCOD-SAAGCD,L'SAAGCCOD)                         
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Data Group'                                        
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SALACELQ,SALACCOD-SALACD,L'SALACCOD)                         
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'T/S Approver Group'                                
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAAPCELQ,SAAPCCOD-SAAPCD,L'SAAPCCOD)                         
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Email'                                             
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAPEEELQ,SAPEEID-SAPEED,0)                                   
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
PERFLDX  DC    AL1(DD_EOTQ)                                                     
                                                                                
PERFPWDK J     EXITYRE             ** PERFLD SAPWDEL Element key **             
                                                                                
PERFPWDD DS    0X                  ** PERFLD SAPWDEL Element data **            
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Password'                                          
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(0,SAPWDCOD-SAPWDD,0)                                         
         DC    AL1(DD_TSHRQ)       handle whole pair                            
         DC    AL4(PERFPWDE)                                                    
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_EOTQ)                                                     
                                                                                
PERFPWDE NTR1  LABEL=NO            ** PERFLD SAPWDEL Element data **            
         XC    BLOCK(2*L'SAPWDCOD),BLOCK                                        
         ICM   R2,15,DI_AOEL       pick up old element                          
         JZ    *+10                not there                                    
         MVC   BLOCK(L'SAPWDCOD),SAPWDCOD-SAPWDD(R2)                            
         ICM   R2,15,DI_ANEL       pick up new element                          
         JZ    *+10                not there                                    
         MVC   BLOCK+L'SAPWDCOD(L'SAPWDCOD),SAPWDCOD-SAPWDD(R2)                 
         CLC   BLOCK(L'SAPWDCOD),BLOCK+L'SAPWDCOD                               
         JE    EXITY               password not changed                         
         MVI   DI_OVAL,C' '                                                     
         MVC   DI_OVAL+1(L'DI_OVAL-1),DI_OVAL                                   
         MVC   DI_NVAL,DI_OVAL                                                  
         OC    BLOCK(L'SAPWDCOD),BLOCK                                          
         JZ    *+10                no old password                              
         MVC   DI_OVAL(14),=C'Old suppressed'                                   
         OC    BLOCK+L'SAPWDCOD(L'SAPWDCOD),BLOCK+L'SAPWDCOD                    
         JZ    *+10                no new password                              
         MVC   DI_NVAL(14),=C'New suppressed'                                   
         GOTOR DI_APUT                                                          
         J     EXITY                                                            
                                                                                
PERFNAMK J     EXITYRE             ** PERFLD SANAMEL Element key **             
                                                                                
PERFNAMD DS    0X                  ** PERFLD SANAMEL Element data **            
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Text' (not used)                                   
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(0,SANAMIND-SANAMD,0)                                         
         DC    AL1(DD_TSHRQ)       handle whole pair                            
         DC    AL4(PERFNAME)                                                    
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_EOTQ)                                                     
                                                                                
PERFNAME NTR1  LABEL=NO            ** PERFLD SANAMEL Element data **            
         XC    BLOCK(3*58),BLOCK                                                
         XC    BLOCK+3*58(3*58),BLOCK+3*58                                      
         MVI   BYTE,0              first time                                   
         LARL  R5,PERFNEX1         locate executed MVC                          
         ICM   R2,15,DI_AOEL       pick up old element                          
         JZ    PERFNE20            not there                                    
         LA    R3,BLOCK                                                         
         USING SANAMEL,R2                                                       
PERFNE10 LA    R4,SANAMES                                                       
         XR    R1,R1                                                            
         TM    SANAMIND,SANAMIFN                                                
         JZ    PERFNE12                                                         
         IC    R1,0(,R4)           length first name                            
         EX    R1,0(,R5)           copy length and first name                   
         LA    R4,1(R1,R4)         point to next name                           
PERFNE12 LA    R3,58(,R3)          middle name slot                             
         TM    SANAMIND,SANAMIMN                                                
         JZ    PERFNE14                                                         
         IC    R1,0(,R4)                                                        
         EX    R1,0(,R5)                                                        
         LA    R4,1(R1,R4)                                                      
PERFNE14 LA    R3,58(,R3)          last name slot                               
         TM    SANAMIND,SANAMILN                                                
         JZ    PERFNE20                                                         
         IC    R1,0(,R4)                                                        
         EX    R1,0(,R5)                                                        
         DROP  R2                                                               
PERFNE20 CLI   BYTE,0              test first time                              
         JNE   PERFNE30                                                         
         MVI   BYTE,1              second time                                  
         LA    R3,BLOCK+3*58       point to new output area                     
         ICM   R2,15,DI_ANEL       pick up new element                          
         JNZ   PERFNE10                                                         
PERFNE30 CLC   BLOCK+0*58(58),BLOCK+3*58 test first name has changed            
         JE    PERFNE40            no, skip                                     
         MVI   DI_OVAL,C' '                                                     
         MVC   DI_OVAL+1(L'DI_OVAL-1),DI_OVAL                                   
         MVC   DI_NVAL,DI_OVAL                                                  
         MVC   DI_FLDN,DI_OVAL                                                  
         MVC   DI_FLDN(10),=C'First Name'                                       
         ICM   R1,1,BLOCK+0*58                                                  
         JZ    PERFNE32            old is empty                                 
         LA    R3,DI_OVAL                                                       
         LA    R4,BLOCK+0*58+1                                                  
         BCTR  R1,0                                                             
         EX    R1,0(,R5)                                                        
PERFNE32 ICM   R1,1,BLOCK+3*58                                                  
         JZ    PERFNE34            new is empty                                 
         LA    R3,DI_NVAL                                                       
         LA    R4,BLOCK+3*58+1                                                  
         BCTR  R1,0                                                             
         EX    R1,0(,R5)                                                        
PERFNE34 GOTOR DI_APUT                                                          
PERFNE40 CLC   BLOCK+1*58(58),BLOCK+4*58 test middle name has changed           
         JE    PERFNE50            no, skip                                     
         MVI   DI_OVAL,C' '                                                     
         MVC   DI_OVAL+1(L'DI_OVAL-1),DI_OVAL                                   
         MVC   DI_NVAL,DI_OVAL                                                  
         MVC   DI_FLDN,DI_OVAL                                                  
         MVC   DI_FLDN(11),=C'Middle Name'                                      
         ICM   R1,1,BLOCK+1*58                                                  
         JZ    PERFNE42            old is empty                                 
         LA    R3,DI_OVAL                                                       
         LA    R4,BLOCK+1*58+1                                                  
         BCTR  R1,0                                                             
         EX    R1,0(,R5)                                                        
PERFNE42 ICM   R1,1,BLOCK+4*58                                                  
         JZ    PERFNE44            new is empty                                 
         LA    R3,DI_NVAL                                                       
         LA    R4,BLOCK+4*58+1                                                  
         BCTR  R1,0                                                             
         EX    R1,0(,R5)                                                        
PERFNE44 GOTOR DI_APUT                                                          
PERFNE50 CLC   BLOCK+2*58(58),BLOCK+5*58 test last name has changed             
         JE    PERFNE60            no, skip                                     
         MVI   DI_OVAL,C' '                                                     
         MVC   DI_OVAL+1(L'DI_OVAL-1),DI_OVAL                                   
         MVC   DI_NVAL,DI_OVAL                                                  
         MVC   DI_FLDN,DI_OVAL                                                  
         MVC   DI_FLDN(9),=C'Last Name'                                         
         ICM   R1,1,BLOCK+2*58                                                  
         JZ    PERFNE52            old is empty                                 
         LA    R3,DI_OVAL                                                       
         LA    R4,BLOCK+2*58+1                                                  
         BCTR  R1,0                                                             
         EX    R1,0(,R5)                                                        
PERFNE52 ICM   R1,1,BLOCK+5*58                                                  
         JZ    PERFNE54            new is empty                                 
         LA    R3,DI_NVAL                                                       
         LA    R4,BLOCK+5*58+1                                                  
         BCTR  R1,0                                                             
         EX    R1,0(,R5)                                                        
PERFNE54 GOTOR DI_APUT                                                          
PERFNE60 J     EXITY                                                            
PERFNEX1 MVC   0(0,R3),0(R4)                                                    
                                                                                
PERFADRK L     RF,DI_AREL          ** PERFLD SAADREL Element key **             
         LARL  R1,PERFADRT                                                      
PERFAD10 CLI   0(R1),0                                                          
         JE    PERFAD20                                                         
         CLC   SAADRTYP-SAADRD(,RF),0(R1)                                       
         JE    PERFAD20                                                         
         LA    R1,1+L'DI_FLDN(,R1)                                              
         J     PERFAD10                                                         
PERFAD20 MVC   DI_EKEY(L'DI_FLDN),1(R1)                                         
         J     EXITYRE                                                          
                                                                                
PERFADRT DC    AL1(SAADLINQ+0),CL(L'DI_FLDN)'Address Line 1'                    
         DC    AL1(SAADLINQ+1),CL(L'DI_FLDN)'Address Line 2'                    
         DC    AL1(SAADLINQ+2),CL(L'DI_FLDN)'Address Line 3'                    
         DC    AL1(SAADCITQ),CL(L'DI_FLDN)'City'                                
         DC    AL1(SAADCODQ),CL(L'DI_FLDN)'State/Zip'                           
         DC    AL1(SAADCTRQ),CL(L'DI_FLDN)'Country Code'                        
         DC    AL1(SAADPHOQ+0),CL(L'DI_FLDN)'Telephone 1'                       
         DC    AL1(SAADPHOQ+1),CL(L'DI_FLDN)'Telephone 2'                       
         DC    AL1(SAADTLXQ),CL(L'DI_FLDN)'Telex'                               
         DC    AL1(SAADFAXQ),CL(L'DI_FLDN)'FAX'                                 
         DC    AL1(0),CL(L'DI_FLDN)'Unknown Data'                               
                                                                                
PERFADRD DS    0X                  ** PERFLD SAADREL Element data **            
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Text' (replaced in PERFADRK routine)               
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(0,SAADRDAT-SAADRD,0)                                         
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(PERFADRE)                                                    
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_EOTQ)                                                     
                                                                                
PERFADRE L     R2,DI_AINP          R2 is A(SAADRDAT)                            
         L     R3,DI_AOUT                                                       
         XR    R1,R1                                                            
         ICM   R1,1,DI_LINP        should be actual length of data              
         JNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         BASR  RF,0                                                             
         MVC   0(0,R3),0(R2)                                                    
         EX    R1,0(,RF)                                                        
         MVC   DI_FLDN,DI_EKEY     Move element key to field name and           
         MVI   DI_EKEY,C' '        clear key. That's reason for routine         
         MVC   DI_EKEY+1(L'DI_EKEY-1),DI_EKEY                                   
         J     EXITYRE                                                          
         EJECT                                                                  
***********************************************************************         
* System record key and data definitions followed by routines         *         
***********************************************************************         
                                                                                
SYSKEY   DS    0X                  ** SYSTEM KEY DEFINITIONS **                 
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(SA0KNUM-SA0KEY,L'SA0KNUM),AL4(EDTPIN)                        
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
SYSKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
SYSFLD   DS    0X                  ** SYSTEM FIELD DEFINITIONS **               
                                                                                
* X'01' SAACVD  Activity Date                                                   
* X'02' SADSCD  Description                                                     
* X'03' SAPASD  Number/Name passive (ignore as it's the password)               
* X'20' SAIDD   User Id                                                         
* X'21' SASYSD  System Auth                                                     
* X'C3' SAPALD  Personal Id                                                     
* X'CA' SAPEFD  Password Effective Dates                                        
* X'E1' SACLAD  Client Limit Access                                             
* X'E4' SAPWHD  Password History (not handled here)                             
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Activity Date'                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAACVELQ,SAACVDT-SAACVD,L'SAACVDT)                           
         DC    AL1(DD_TBDTQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Description'                                       
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SADSCELQ,SADSC-SADSCD,0)                                     
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDIMQ)                                                    
         DC    CL(L'DD_NAME)'SAIDEL' (replaced in SYSFUIDE routine)             
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAIDELQ,SAID-SAIDD,L'SAID)                                   
         DC    AL1(0)                                                           
         DC    AL4(0,SYSFUIDK,SYSFUIDD)                                         
         DC    XL4'00'                                                          
                                                                                
         DC    AL1(DD_LDIMQ)                                                    
         DC    CL(L'DD_NAME)'SASYSEL' (replaced in SYSFSYSE routine)            
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SASYSELQ,SASYSNUM-SASYSD,L'SASYSNUM)                         
         DC    AL1(0)                                                           
         DC    AL4(0,SYSFSYSK,SYSFSYSD)                                         
         DC    XL4'00'                                                          
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Personal Id'                                       
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAPALELQ,SAPALPID-SAPALD,L'SAPALPID)                         
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Password Start Date'                               
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAPEFELQ,SAPEFSTA-SAPEFD,L'SAPEFSTA)                         
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTDF0)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Password End Date'                                 
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAPEFELQ,SAPEFEND-SAPEFD,L'SAPEFEND)                         
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTDF0)                                                      
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIMQ)                                                    
         DC    CL(L'DD_NAME)'SACLAEL' (replaced in SYSFCLAE routine)            
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SACLAELQ,SACLASYS-SACLAD,L'SACLASYS)                         
         DC    AL1(0)                                                           
         DC    AL4(0,SYSFCLAK,SYSFCLAD)                                         
         DC    XL4'00'                                                          
                                                                                
SYSFLDX  DC    AL1(DD_EOTQ)                                                     
                                                                                
SYSFUIDK J     EXITYRE             ** SYSFLD SAIDEL Element key **              
                                                                                
SYSFUIDD DS    0X                  ** SYSFLD SAIDEL Element data **             
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'User Id'                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(0,SAID-SAIDD,0)                                              
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(SYSFUIDE)                                                    
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_EOTQ)                                                     
                                                                                
SYSFUIDE L     R2,DI_AINP          R2 is A(SAID)                                
         L     R3,DI_AOUT                                                       
         CLI   0(R2),0             test new style (0 means it isn't)            
         JE    *+12                                                             
         TM    0(R2),SAIDNEQ       test new style (off if it is)                
         JZ    SYSFUID2                                                         
         MVC   0(L'SAID,R3),0(R2)  old style fixed length, always write         
         CLC   0(2,R2),=X'0001'    test L= or A=                                
         JH    EXITYRE             no, skip                                     
         MVC   0(2,R3),=C'L='                                                   
         JL    EXITYRE                                                          
         MVC   0(2,R3),=C'A='                                                   
         J     EXITYRE                                                          
SYSFUID2 TM    0(R2),SAIDROQ       new style                                    
         JZ    *+10                                                             
         MVC   DI_FLDN(11),=C'User Id R/O' show read only                       
         TM    0(R2),SAIDAGQ       A=                                           
         JZ    *+14                                                             
         MVC   0(2,R3),=C'A='                                                   
         LA    R3,2(,R3)                                                        
         TM    0(R2),SAIDLIQ       L=                                           
         JZ    *+14                                                             
         MVC   0(2,R3),=C'L='                                                   
         LA    R3,2(,R3)                                                        
         XR    R1,R1                                                            
         ICM   R1,1,DI_LINP        should be actual length +1 of data           
         JNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                actual length                                
         TM    0(R2),SAIDNUQ       test uid number on end                       
         JZ    *+8                                                              
         SHI   R1,2                yes, ignore it                               
         BCTR  R1,0                less 1 for MVC                               
         BASR  RF,0                                                             
         MVC   0(0,R3),1(R2)                                                    
         EX    R1,0(,RF)                                                        
         J     EXITYRE                                                          
                                                                                
SYSFSYSK L     RF,DI_AREL          ** SYSFLD SASYSEL Element key **             
         MVC   SYSPGM(1),SASYSNUM-SASYSD(RF) save system for SYSFSYPE           
         LA    RF,SASYSNUM-SASYSD(,RF)                                          
         LR    R0,RE                                                            
         GOTOR GETSYS,DMCB,(RF),DI_EKEY get system name as element key          
         LR    RE,R0                                                            
         J     EXITYRE                                                          
                                                                                
SYSFSYSD DS    0X                  ** SYSFLD SASYSEL Element data **            
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Limit Access'                                      
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(0,SASYSLMT-SASYSD,L'SASYSLMT)                                
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(SYSFSYLE)                                                    
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Authorisation'                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(0,SASYSALL-SASYSD,L'SASYSALL)                                
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(SYSFSYAE)                                                    
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Text' (not used)                                   
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(0,SASYSPGM-SASYSD,0)                                         
         DC    AL1(DD_TSHRQ)       handle whole pair                            
         DC    AL4(SYSFSYPE)                                                    
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_EOTQ)                                                     
                                                                                
SYSFSYLE L     R2,DI_AINP          R2 is A(SASYSLMT)                            
         L     R3,DI_AOUT                                                       
         L     R4,DI_AREL          R4 is A(SASYSEL)                             
         OC    0(L'SASYSLMT,R2),0(R2)                                           
         JZ    EXITYRE                                                          
         MVC   0(L'SASYSLMT,R3),0(R2)                                           
         CLC   0(2,R2),=X'FFFF'                                                 
         JNE   *+14                                                             
         MVC   0(2,R2),=C'L='                                                   
         J     EXITYRE                                                          
*&&UK                                                                           
         CLI   SASYSNUM-SASYSD(R4),4 UK Media system has special code           
         JNE   SYSFSL10              format is nnn,nnn,nnn,nnn                  
         LHI   R0,4                                                             
         BASR  RF,0                                                             
         ZIC   R1,0(R2)                                                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R3),DUB+6(2)                                                 
         MVI   3(R3),C','                                                       
         AHI   R2,1                                                             
         AHI   R3,4                                                             
         BCTR  R0,RF                                                            
         BCTR  R3,0                                                             
         MVI   0(R3),C' '                                                       
         J     EXITYRE                                                          
*&&                                                                             
SYSFSL10 CLI   SASYSNUM-SASYSD(R4),14 Person system has special code            
         JNE   EXITYRE                format is nnn-nnn                         
         ZIC   R1,2(R2)                                                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R3),DUB+6(2)                                                 
         MVI   3(R3),C'-'                                                       
         ZIC   R1,3(R2)                                                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(3,R3),DUB+6(2)                                                 
         J     EXITYRE                                                          
                                                                                
SYSFSYAE L     R2,DI_AINP          R2 is A(SASYSALL)                            
         L     R3,DI_AOUT                                                       
         MVI   DI_FLDN,C' '                                                     
         MVC   DI_FLDN+1(L'DI_FLDN-1),DI_FLDN                                   
         MVC   DI_FLDN(14),=C'Other Programs'                                   
         LR    R0,RE                                                            
         GOTOR SYSFSYAU,DMCB,(R2),(R3) edit out value                           
         LR    RE,R0                                                            
         J     EXITYRE                                                          
                                                                                
SYSFSYPE NTR1  LABEL=NO            ** SYSFLD SASYSPGM Element data **           
         USING SASYSD,R2                                                        
         XC    BLOCK1,BLOCK1       BLOCK1 contains old program data             
         ICM   R2,15,DI_AOEL       pick up old element                          
         JZ    SYSFSP10            not there                                    
         LLC   RF,SASYSLN                                                       
         SHI   RF,SASYSLNQ         RF=length of SASYSPGM subels                 
         STC   RF,BLOCK1                                                        
         XR    RE,RE                                                            
         D     RE,=A(L'SASYSPGM)   div len by subel len gives count             
         LTR   RE,RE               sanity check                                 
         JZ    *+6                                                              
         DC    H'0'                must not be a remainder                      
         MVC   BLOCK1+1(255-SASYSLNQ),SASYSPGM copy all poss subels             
         GOTOR VXSORT,DMCB,BLOCK1+1,(RF),3,1,0                                  
SYSFSP10 XC    BLOCK2,BLOCK2                                                    
         ICM   R2,15,DI_ANEL       pick up new element                          
         JZ    SYSFSP20            not there                                    
         LLC   RF,SASYSLN                                                       
         SHI   RF,SASYSLNQ         RF=length of SASYSPGM subels                 
         STC   RF,BLOCK2                                                        
         XR    RE,RE                                                            
         D     RE,=A(L'SASYSPGM)   div len by subel len gives count             
         LTR   RE,RE               sanity check                                 
         JZ    *+6                                                              
         DC    H'0'                must not be a remainder                      
         MVC   BLOCK2+1(255-SASYSLNQ),SASYSPGM copy all poss subels             
         GOTOR VXSORT,DMCB,BLOCK2+1,(RF),3,1,0                                  
         DROP  R2                                                               
SYSFSP20 LLC   R2,BLOCK1           R2=length of old programs                    
         LA    R3,BLOCK1+1         R3=A(first old program)                      
         LLC   R4,BLOCK2           R4=length of new programs                    
         LA    R5,BLOCK2+1         R5=A(first new program)                      
SYSFSP22 MVI   SYSPGM+1,0          clear program                                
         LTR   R2,R2               finished with old?                           
         JNZ   *+14                no                                           
         LTR   R4,R4               finished with new too?                       
         JZ    SYSFSP30            yes, exit                                    
         J     SYSFSP28            else just do rest of new                     
         LTR   R4,R4               finished with new, but not old?              
         JZ    SYSFSP26            yes, just do rest of old                     
         CLC   0(L'SASYSPGM,R3),0(R5) compare programs and auths                
         JE    SYSFSP24            unchanged, so ignore                         
         CLC   0(L'SASYSPNM,R3),0(R5) test same program                         
         JL    SYSFSP26            old is lower, so old is a delete             
         JH    SYSFSP28            new is lower, so new is an add               
         MVC   SYSPGM+1(1),0(R3)   same program so it's been changed            
         GOTOR SYSFSYPG            edit out program                             
         GOTOR SYSFSYAU,DMCB,1(R3),DI_OVAL edit out old value                   
         GOTOR SYSFSYAU,DMCB,1(R5),DI_NVAL edit out new value                   
         GOTOR DI_APUT                                                          
SYSFSP24 SHI   R2,L'SASYSPGM       next old                                     
         AHI   R3,L'SASYSPGM                                                    
         SHI   R4,L'SASYSPGM       next new                                     
         AHI   R5,L'SASYSPGM                                                    
         J     SYSFSP22                                                         
SYSFSP26 MVC   SYSPGM+1(1),0(R3)   old program has been deleted                 
         GOTOR SYSFSYPG            edit out program                             
         GOTOR SYSFSYAU,DMCB,1(R3),DI_OVAL edit out old value                   
         GOTOR DI_APUT                                                          
         SHI   R2,L'SASYSPGM       next old                                     
         AHI   R3,L'SASYSPGM                                                    
         J     SYSFSP22                                                         
SYSFSP28 MVC   SYSPGM+1(1),0(R5)   new program has been added                   
         GOTOR SYSFSYPG            edit out program                             
         GOTOR SYSFSYAU,DMCB,1(R5),DI_NVAL edit out new value                   
         GOTOR DI_APUT                                                          
         SHI   R4,L'SASYSPGM       next new                                     
         AHI   R5,L'SASYSPGM                                                    
         J     SYSFSP22                                                         
SYSFSP30 J     EXITY                                                            
                                                                                
SYSFSYPG NTR1  LABEL=NO            Edit out program name for SYSFSYPE           
         MVI   DI_FLDN,C' '                                                     
         MVC   DI_FLDN+1(L'DI_FLDN-1),DI_FLDN                                   
         GOTOR GETPGM,DMCB,SYSPGM,DI_FLDN                                       
         J     EXITY                                                            
                                                                                
SYSFSYAU NTR1  LABEL=NO            Edit out auth bytes for SYSFSYPE             
         LM    R2,R3,0(R1)         parms = A(source),A(target)                  
         MVI   1(R3),C' '                                                       
         MVC   2(2,R3),1(R3)                                                    
         MVI   0(R3),C'Y'                                                       
         CLC   0(L'SASYSAUT,R2),YAUTH                                           
         JE    EXITY                                                            
         MVI   0(R3),C'N'                                                       
         CLC   0(L'SASYSAUT,R2),NAUTH                                           
         JE    EXITY                                                            
         GOTOR CHEXOUT,(R1),,,2    P1 and P2 already set                        
         J     EXITY                                                            
                                                                                
SYSFCLAK L     RF,DI_AREL          ** SYSFLD SACLAEL Element key **             
         MVC   SYSPGM(1),SACLASYS-SACLAD(RF) save system for SYSFCLCE           
         LA    RF,SASYSNUM-SASYSD(,RF)                                          
         LR    R0,RE                                                            
         GOTOR GETSYS,DMCB,(RF),DI_EKEY get system name                         
         LR    RE,R0                                                            
         J     EXITYRE                                                          
                                                                                
SYSFCLAD DS    0X                  ** SYSFLD SACLAEL Element data **            
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Limit Access Client'                               
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(0,SACLACOD-SACLAD,0)                                         
         DC    AL1(DD_TSHRQ)       handle whole pair                            
         DC    AL4(SYSFCLCE)                                                    
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_EOTQ)                                                     
                                                                                
SYSFCLCE NTR1  LABEL=NO            ** SYSFLD SASYSCLA Element data **           
         USING SACLAD,R2                                                        
         XC    BLOCK1,BLOCK1       BLOCK1 contains old clients                  
         XC    BLOCK1+256(400-256),BLOCK1+256                                   
         ICM   R2,15,DI_AOEL       pick up old element                          
         JZ    SYSFCC10            not there                                    
SYSFCC02 LLC   RF,SACLALN                                                       
         LR    R0,RF               save element length                          
         SHI   RF,SACLACOD-SACLAD  RF=length of SACLACOD subels                 
         LR    R1,RF                                                            
         LH    RE,BLOCK1           get length so far                            
         AR    RF,RE                                                            
         STH   RF,BLOCK1           add this one                                 
         LA    RE,BLOCK1+2(RE)     get next free location                       
         BCTR  R1,0                                                             
         LARL  RF,SYSFCCX                                                       
         EX    R1,0(,RF)           copy all subels                              
         AR    R2,R0               next element                                 
         CLI   SACLAEL,SACLAELQ    is it same type?                             
         JNE   SYSFCC08            no                                           
         CLC   SACLASYS,SYSPGM     same system?                                 
         JE    SYSFCC02            yes                                          
SYSFCC08 LH    RF,BLOCK1           get total length                             
         CHI   RF,2*L'SACLACOD     test more than 1                             
         JL    SYSFCC10            no, no need to sort                          
         XR    RE,RE                                                            
         D     RE,=A(L'SACLACOD)   div len by subel len gives count             
         LTR   RE,RE               sanity check                                 
         JZ    *+6                                                              
         DC    H'0'                must not be a remainder                      
         GOTOR VXSORT,DMCB,BLOCK1+2,(RF),3,3,0                                  
SYSFCC10 XC    BLOCK2,BLOCK2       BLOCK2 contains old clients                  
         XC    BLOCK2+256(400-256),BLOCK2+256                                   
         ICM   R2,15,DI_ANEL       pick up new element                          
         JZ    SYSFCC20            not there                                    
SYSFCC12 LLC   RF,SACLALN                                                       
         LR    R0,RF               save element length                          
         SHI   RF,SACLACOD-SACLAD  RF=length of SACLACOD subels                 
         LR    R1,RF                                                            
         LH    RE,BLOCK2           get length so far                            
         AR    RF,RE                                                            
         STH   RF,BLOCK2           add this one                                 
         LA    RE,BLOCK2+2(RE)     get next free location                       
         BCTR  R1,0                                                             
         LARL  RF,SYSFCCX                                                       
         EX    R1,0(,RF)           copy all subels                              
         AR    R2,R0               next element                                 
         CLI   SACLAEL,SACLAELQ    is it same type?                             
         JNE   SYSFCC18            no                                           
         CLC   SACLASYS,SYSPGM     same system?                                 
         JE    SYSFCC12            yes                                          
SYSFCC18 LH    RF,BLOCK2           get total length                             
         CHI   RF,2*L'SACLACOD     test more than 1                             
         JL    SYSFCC20            no, no need to sort                          
         XR    RE,RE                                                            
         D     RE,=A(L'SACLACOD)   div len by subel len gives count             
         LTR   RE,RE               sanity check                                 
         JZ    *+6                                                              
         DC    H'0'                must not be a remainder                      
         GOTOR VXSORT,DMCB,BLOCK2+2,(RF),3,3,0                                  
         J     SYSFCC20                                                         
SYSFCCX  MVC   0(0,RE),SACLACOD                                                 
         DROP  R2                                                               
SYSFCC20 LH    R2,BLOCK1           R2=LENGTH OF OLD CLIENTS                     
         LA    R3,BLOCK1+2         R3=A(first old client)                       
         LH    R4,BLOCK2           R4=length of new clients                     
         LA    R5,BLOCK2+2         R5=A(first new client)                       
SYSFCC22 LTR   R2,R2               finished with old?                           
         JNZ   *+14                no                                           
         LTR   R4,R4               finished with new too?                       
         JZ    SYSFCC30            yes, exit                                    
         J     SYSFCC28            else just do rest of new                     
         LTR   R4,R4               finished with new, but not old?              
         JZ    SYSFCC26            yes, just do rest of old                     
         CLC   0(L'SACLACOD,R3),0(R5) compare clients                           
         JL    SYSFCC26            old is lower, so old is a delete             
         JH    SYSFCC28            new is lower, so new is an add               
SYSFCC24 CLI   0(R3),0             was old an LAL list code                     
         JNE   *+10                                                             
         XR    R2,R2               yes indicate end                             
         J     *+8                                                              
         SHI   R2,L'SACLACOD       next old                                     
         AHI   R3,L'SACLACOD                                                    
         CLI   0(R5),0             was new an LAL list code                     
         JNE   *+10                                                             
         XR    R4,R4               yes indicate end                             
         J     *+8                                                              
         SHI   R4,L'SACLACOD       next new                                     
         AHI   R5,L'SACLACOD                                                    
         J     SYSFCC22                                                         
SYSFCC26 GOTOR SYSFCCCL,DMCB,0(R3),DI_OVAL edit out old value                   
         GOTOR DI_APUT                                                          
         CLI   0(R3),0             was old an LAL list code                     
         JNE   *+10                                                             
         XR    R2,R2               yes indicate end                             
         J     *+8                                                              
         SHI   R2,L'SASYSPGM       next old                                     
         AHI   R3,L'SASYSPGM                                                    
         J     SYSFCC22                                                         
SYSFCC28 GOTOR SYSFCCCL,DMCB,0(R5),DI_NVAL edit out new value                   
         GOTOR DI_APUT                                                          
         CLI   0(R5),0             was new an LAL list code                     
         JNE   *+10                                                             
         XR    R4,R4               yes indicate end                             
         J     *+8                                                              
         SHI   R4,L'SASYSPGM       next new                                     
         AHI   R5,L'SASYSPGM                                                    
         J     SYSFCC22                                                         
SYSFCC30 J     EXITY                                                            
                                                                                
SYSFCCCL NTR1  LABEL=NO            Edit out client for SYSFCLCE                 
         LM    R2,R3,0(R1)         parms = A(source),A(target)                  
         MVI   0(R3),C' '                                                       
         MVC   1(5,R3),0(R3)       clear 6 bytes                                
*&&US                                                                           
         CLI   SYSPGM,X'04'        PRINT system                                 
         JNE   *+14                                                             
         MVC   0(3,R3),0(R2)       use 'as is' if PRINT                         
         J     EXITY                                                            
         GOTOR VCLUNPK,DMCB,(R2),(R3) unpack it if not PRINT                    
         J     EXITY                                                            
*&&                                                                             
*&&UK                                                                           
         CLI   0(R2),0             test LAL list code                           
         JNE   *+20                no, skip                                     
         MVC   0(2,R3),=C'L='                                                   
         MVC   2(3,R3),1(R2)                                                    
         J     EXITY                                                            
         TM    0(R2),X'80'         test negative                                
         JO    *+12                no, skip                                     
         MVI   0(R3),C'-'                                                       
         AHI   R3,1                                                             
         MVC   0(3,R3),0(R2)       move three char client code                  
         TM    1(R3),X'C0'         test 5 char packed                           
         JNZ   EXITY               no                                           
         XR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  1(4,R3),DUB                                                      
         J     EXITY                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* Office record key and data definitions followed by routines         *         
***********************************************************************         
                                                                                
OFFKEY   DS    0X                  ** OFFICE KEY DEFINITIONS **                 
                                                                                
         DC    AL1(DK_TCHRQ)                                                    
         DC    AL1(SAOFOID-SAOFKEY,L'SAOFOID),AL4(0)                            
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
OFFKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
OFFFLD   DS    0X                  ** OFFICE FIELD DEFINITIONS **               
                                                                                
* X'01' SAACVD  Activity Date                                                   
* X'C0' SAOFFD  Office                                                          
* X'C2' SAPCTD  Person Count (not handled here)                                 
* X'CF' SAMAND  Manager Id                                                      
* X'E0' SAAPCD  T/S Approver Group                                              
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Activity Date'                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAACVELQ,SAACVDT-SAACVD,L'SAACVDT)                           
         DC    AL1(DD_TBDTQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Name'                                              
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAOFFELQ,SAOFFNAM-SAOFFD,0)                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Manager Id' (not used I think, see SAMAND)         
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAOFFELQ,SAOFFMID-SAOFFD,L'SAOFFMID)                         
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDIMQ)                                                    
         DC    CL(L'DD_NAME)'SAMANELQ'                                          
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAMANELQ,SAMANORD-SAMAND,L'SAMANORD)                         
         DC    AL1(0)                                                           
         DC    AL4(0,ALLFMANK,ALLFMAND) use common routines/table               
         DC    XL4'00'                                                          
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'T/S Approver Group'                                
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAAPCELQ,SAAPCCOD-SAAPCD,L'SAAPCCOD)                         
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
OFFFLDX  DC    AL1(DD_EOTQ)                                                     
         EJECT                                                                  
***********************************************************************         
* Department record key and data definitions followed by routines     *         
***********************************************************************         
                                                                                
DPTKEY   DS    0X                  ** DEPARTMENT KEY DEFINITIONS **             
                                                                                
         DC    AL1(DK_TCHRQ)                                                    
         DC    AL1(SADPOID-SADPKEY,L'SADPOID),AL4(0)                            
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TCHRQ)                                                    
         DC    AL1(SADPDID-SADPKEY,L'SADPDID),AL4(0)                            
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
DPTKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
DPTFLD   DS    0X                  ** DEPARTMENT FIELD DEFINITIONS **           
                                                                                
* X'01' SAACVD  Activity Date                                                   
* X'C1' SADPTD  Department                                                      
* X'C2' SAPCTD  Person Count (not handled here)                                 
* X'CF' SAMAND  Manager Id                                                      
* X'E0' SAAPCD  T/S Approver Group                                              
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Activity Date'                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAACVELQ,SAACVDT-SAACVD,L'SAACVDT)                           
         DC    AL1(DD_TBDTQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Name'                                              
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SADPTELQ,SADPTNAM-SADPTD,0)                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Manager Id' (not used I think, see SAMAND)         
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SADPTELQ,SADPTMID-SADPTD,L'SADPTMID)                         
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDIMQ)                                                    
         DC    CL(L'DD_NAME)'SAMANELQ'                                          
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAMANELQ,SAMANORD-SAMAND,L'SAMANORD)                         
         DC    AL1(0)                                                           
         DC    AL4(0,ALLFMANK,ALLFMAND) use common routines/table               
         DC    XL4'00'                                                          
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'T/S Approver Group'                                
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAAPCELQ,SAAPCCOD-SAAPCD,L'SAAPCCOD)                         
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
DPTFLDX  DC    AL1(DD_EOTQ)                                                     
         EJECT                                                                  
***********************************************************************         
* Group record key and data definitions followed by routines          *         
***********************************************************************         
                                                                                
GRPKEY   DS    0X                  ** GROUP KEY DEFINITIONS **                  
                                                                                
         DC    AL1(DK_TCHRQ)                                                    
         DC    AL1(SAAGAGR-SAAGKEY,L'SAAGAGR),AL4(0)                            
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
GRPKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
GRPFLD   DS    0X                  ** GROUP FIELD DEFINITIONS **                
                                                                                
* X'01' SAACVD  Activity Date                                                   
* X'C2' SAPCTD  Person Count (not handled here)                                 
* X'C9' SAAGND  Group                                                           
* X'CE' SACOMD  Comment                                                         
* X'CF' SAMAND  Manager Id                                                      
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Activity Date'                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAACVELQ,SAACVDT-SAACVD,L'SAACVDT)                           
         DC    AL1(DD_TBDTQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Name'                                              
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAAGNELQ,SAAGNNAM-SAAGND,0)                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDIMQ)                                                    
         DC    CL(L'DD_NAME)'SACOMELQ'                                          
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SACOMELQ,SACOMOPT-SACOMD,L'SACOMOPT+L'SACOMLIN)              
         DC    AL1(0)                                                           
         DC    AL4(0,ALLFCOMK,ALLFCOMD) use common routines/table               
         DC    XL4'00'                                                          
                                                                                
         DC    AL1(DD_LDIMQ)                                                    
         DC    CL(L'DD_NAME)'SAMANELQ'                                          
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAMANELQ,SAMANORD-SAMAND,L'SAMANORD)                         
         DC    AL1(0)                                                           
         DC    AL4(0,ALLFMANK,ALLFMAND) use common routines/table               
         DC    XL4'00'                                                          
                                                                                
GRPFLDX  DC    AL1(DD_EOTQ)                                                     
         EJECT                                                                  
***********************************************************************         
* Data Group record key and data definitions followed by routines     *         
***********************************************************************         
                                                                                
DATKEY   DS    0X                  ** DATA GROUP KEY DEFINITIONS **             
                                                                                
         DC    AL1(DK_TCHRQ)                                                    
         DC    AL1(SALAAGR-SALAKEY,L'SALAAGR),AL4(0)                            
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
DATKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
DATFLD   DS    0X                  ** DATA GROUP FIELD DEFINITIONS **           
                                                                                
* X'01' SAACVD  Activity Date                                                   
* X'CB' SALAND  Data Group Name                                                 
* X'CC' SALASD  Limit Access System element (not in use yet)                    
* X'CE' SACOMD  Comment                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Activity Date'                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAACVELQ,SAACVDT-SAACVD,L'SAACVDT)                           
         DC    AL1(DD_TBDTQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Name'                                              
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SALANELQ,SALANNAM-SALAND,0)                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Defer to Company ID'                               
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SALASELQ,SALASIND-SALASD,L'SALASIND)                         
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTDCID)                                                     
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_LDIMQ)                                                    
         DC    CL(L'DD_NAME)'SACOMELQ'                                          
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SACOMELQ,SACOMOPT-SACOMD,L'SACOMOPT+L'SACOMLIN)              
         DC    AL1(0)                                                           
         DC    AL4(0,ALLFCOMK,ALLFCOMD) use common routines/table               
         DC    XL4'00'                                                          
                                                                                
DATFLDX  DC    AL1(DD_EOTQ)                                                     
         EJECT                                                                  
***********************************************************************         
* Access record key and data definitions followed by routines         *         
***********************************************************************         
                                                                                
ACCKEY   DS    0X                  ** ACCESS KEY DEFINITIONS **                 
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(SAASOVS-SAASKEY,L'SAASOVS),AL4(EDTOVS)                       
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(SAASOVPG-SAASKEY,L'SAASOVPG),AL4(EDTPGM)                     
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(SAASUID-SAASKEY,L'SAASUID),AL4(EDTUID)                       
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(SAASAGN-SAASKEY,L'SAASAGN),AL4(EDTAGN)                       
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
ACCKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
ACCFLD   DS    0X                  ** ACCESS FIELD DEFINITIONS **               
                                                                                
* X'01' SAACVD  Activity Date                                                   
* X'BA' SAMIXD  Record/Actions                                                  
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Activity Date'                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAACVELQ,SAACVDT-SAACVD,L'SAACVDT)                           
         DC    AL1(DD_TBDTQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDIMQ)                                                    
         DC    CL(L'DD_NAME)'SAMIXEL' (replaced in ACCFMXAE routine)            
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAMIXELQ,SAMIXRCD-SAMIXD,L'SAMIXRCD)                         
         DC    AL1(0)                                                           
         DC    AL4(0,ACCFMIXK,ACCFMIXD)                                         
         DC    XL4'00'                                                          
                                                                                
ACCFLDX  DC    AL1(DD_EOTQ)                                                     
                                                                                
ACCFMIXK L     RF,DI_AREL          ** ACCFLD SAMIXEL element key **             
         LA    RF,SAMIXRCD-SAMIXD(,RF)                                          
         LR    R0,RE                                                            
         GOTOR GETRCD,DMCB,(RF),DI_EKEY get record name as element key          
         LR    RE,R0                                                            
         J     EXITYRE                                                          
                                                                                
ACCFMIXD DS    0X                  ** ACCFLD SAMIXEL element data **            
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Text' (not used)                                   
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(0,SAMIXACT-SAMIXD,0)                                         
         DC    AL1(DD_TSHRQ)       handle whole pair                            
         DC    AL4(ACCFMXAE)                                                    
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_EOTQ)                                                     
                                                                                
ACCFMXAE NTR1  LABEL=NO            ** ACCFLD SAMIXACT Element data **           
         USING SAMIXD,R2                                                        
         MVI   BLOCK1,C'N'         BLOCK1 contains old action data              
         MVC   BLOCK1+1(255),BLOCK1 fill with 'N's                              
         MVC   BLOCK2(256),BLOCK1  BLOCK2 contains new action data              
         XR    R5,R5               R5=number of SAMIXACT bytes                  
         ICM   R2,15,DI_AOEL       pick up old element                          
         JZ    ACCFMA10            not there                                    
         LLC   R4,SAMIXLN                                                       
         SHI   R4,SAMIXLNQ         R4=length of SAMIXACT bytes                  
         JNP   ACCFMA10            all done if none                             
         LR    R5,R4               save length                                  
         GOTOR BLOCKIN,DMCB,((R4),SAMIXACT),(C'Y',BLOCK1)                       
ACCFMA10 ICM   R2,15,DI_ANEL       pick up new element                          
         JZ    ACCFMA20            not there                                    
         LLC   R4,SAMIXLN                                                       
         SHI   R4,SAMIXLNQ         R4=length of SAMIXACT bytes                  
         JNP   ACCFMA20            all done if none                             
         CR    R4,R5               test more bytes than old                     
         JNH   *+6                                                              
         LR    R5,R4               yes, use longer                              
         GOTOR BLOCKIN,DMCB,((R4),SAMIXACT),(C'Y',BLOCK2)                       
         DROP  R2                                                               
ACCFMA20 LARL  RF,ACCFMAAC         edit routine for BLOCKOUT                    
         GOTOR BLOCKOUT,DMCB,(RF),(R5) write out differences                    
         J     EXITY                                                            
                                                                                
ACCFMAAC NTR1  LABEL=NO            Edit out action name for ACCFMXAE            
         MVI   DI_FLDN,C' '                                                     
         MVC   DI_FLDN+1(L'DI_FLDN-1),DI_FLDN                                   
         GOTOR SETPGMAC            get PGMACT action list for program           
         LLC   RF,BITIX            get index to PGMACT                          
         LA    RF,PGMACT(RF)       Index into it                                
         GOTOR GETACT,DMCB,(RF),DI_FLDN                                         
         J     EXIT                return GETACT cc                             
         EJECT                                                                  
***********************************************************************         
* FCon record key and data definitions followed by routines           *         
***********************************************************************         
                                                                                
FCNKEY   DS    0X                  ** FCON KEY DEFINITIONS **                   
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(SAFCOVS-SAFCKEY,L'SAFCOVS),AL4(EDTOVS)                       
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(SAFCOVPG-SAFCKEY,L'SAFCOVPG),AL4(EDTPGM)                     
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(SAFCUID-SAFCKEY,L'SAFCUID),AL4(EDTUID)                       
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(SAFCAGN-SAFCKEY,L'SAFCAGN),AL4(EDTAGN)                       
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
FCNKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
FCNFLD   DS    0X                  ** FCON FIELD DEFINITIONS **                 
                                                                                
* X'01' SAACVD  Activity Date                                                   
* X'BD' SAFCWD  Field Write/Read   handled by X'BE' SAFCRD routine              
* X'BE' SAFCRD  Field Read                                                      
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Activity Date'                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAACVELQ,SAACVDT-SAACVD,L'SAACVDT)                           
         DC    AL1(DD_TBDTQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Text' (not used)                                   
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAFCRELQ,SAFCRTAB-SAFCRD,0)                                  
         DC    AL1(DD_TSHRQ)       handle whole pair                            
         DC    AL4(FCNFFCTE)                                                    
         DC    XL12'00'                                                         
                                                                                
FCNFLDX  DC    AL1(DD_EOTQ)                                                     
                                                                                
FCNFFCTE NTR1  LABEL=NO            ** FCNFLD SAFCRTAB Element data **           
         MVI   BLOCK1,C'N'         BLOCK1 contains old field data               
         MVC   BLOCK1+1(255),BLOCK1 fill with 'N's                              
         MVC   BLOCK2(256),BLOCK1  BLOCK2 contains new field data               
         XR    R5,R5               R5=number of SAFCRTAB bytes                  
         L     RF,DI_ADSD          pick up sort key                             
         L     R3,DI_AIO1                                                       
         CLI   DS_TYPE-DS_D(RF),DS_TCPYQ old in IO1 if change                   
         JE    FCNFFT05                                                         
         L     R3,DI_AIO2                                                       
         CLI   DS_TYPE-DS_D(RF),DS_TDELQ old in IO2 if delete                   
         JNE   FCNFFT20            else no old record                           
FCNFFT05 MVI   ELCODE,SAFCRELQ     read element                                 
         LR    R1,R3                                                            
         BRAS  RE,GETEL                                                         
         JNE   FCNFFT10            element not found                            
         LR    R2,R1                                                            
         USING SAFCRD,R2                                                        
         LLC   R4,SAFCRLN                                                       
         SHI   R4,SAFCRLNQ         R4=length of SAFCRTAB bytes                  
         JNP   FCNFFT10            all done if none                             
         LR    R5,R4               save length                                  
         GOTOR BLOCKIN,DMCB,((R4),SAFCRTAB),(C'R',BLOCK1)                       
FCNFFT10 MVI   ELCODE,SAFCWELQ     read/write element                           
         LR    R1,R3                                                            
         BRAS  RE,GETEL                                                         
         JNE   FCNFFT20            element not found                            
         LR    R2,R1                                                            
         USING SAFCWD,R2                                                        
         LLC   R4,SAFCWLN                                                       
         SHI   R4,SAFCWLNQ         R4=length of SAFCWTAB bytes                  
         JNP   FCNFFT20            all done if none                             
         CR    R4,R5               test more bytes than read element            
         JNH   *+6                                                              
         LR    R5,R4               yes, use longer                              
         GOTOR BLOCKIN,DMCB,((R4),SAFCWTAB),(C'W',BLOCK1)                       
FCNFFT20 L     RF,DI_ADSD          pick up sort key                             
         L     R3,DI_AIO2                                                       
         CLI   DS_TYPE-DS_D(RF),DS_TCPYQ new in IO2 if change                   
         JE    FCNFFT25                                                         
         L     R3,DI_AIO1                                                       
         CLI   DS_TYPE-DS_D(RF),DS_TADDQ new in IO1 if add                      
         JE    FCNFFT25                                                         
         CLI   DS_TYPE-DS_D(RF),DS_TRESQ new in IO1 if restore                  
         JNE   FCNFFT40            else no new record (delete)                  
FCNFFT25 MVI   ELCODE,SAFCRELQ     read element                                 
         LR    R1,R3                                                            
         BRAS  RE,GETEL                                                         
         JNE   FCNFFT30            element not found                            
         LR    R2,R1                                                            
         USING SAFCRD,R2                                                        
         LLC   R4,SAFCRLN                                                       
         SHI   R4,SAFCRLNQ         R4=length of SAFCRTAB bytes                  
         JNP   FCNFFT30            all done if none                             
         CR    R4,R5               test more bytes than old elements            
         JNH   *+6                                                              
         LR    R5,R4               yes, use longer                              
         GOTOR BLOCKIN,DMCB,((R4),SAFCRTAB),(C'R',BLOCK2)                       
FCNFFT30 MVI   ELCODE,SAFCWELQ     read/write element                           
         LR    R1,R3                                                            
         BRAS  RE,GETEL                                                         
         JNE   FCNFFT40            element not found                            
         LR    R2,R1                                                            
         USING SAFCWD,R2                                                        
         LLC   R4,SAFCWLN                                                       
         SHI   R4,SAFCWLNQ         R4=length of SAFCWTAB bytes                  
         JNP   FCNFFT40            all done if none                             
         CR    R4,R5               test more bytes than other elements          
         JNH   *+6                                                              
         LR    R5,R4               yes, use longer                              
         GOTOR BLOCKIN,DMCB,((R4),SAFCWTAB),(C'W',BLOCK2)                       
         DROP  R2                                                               
FCNFFT40 LARL  RF,FCNFFTFL         edit routine for BLOCKOUT                    
         GOTOR BLOCKOUT,DMCB,(RF),(R5) write out differences                    
         J     EXITY                                                            
                                                                                
FCNFFTFL NTR1  LABEL=NO            Edit out field name for FCNFFCTE             
         MVI   DI_FLDN,C' '                                                     
         MVC   DI_FLDN+1(L'DI_FLDN-1),DI_FLDN                                   
         GOTOR GETFLD,DMCB,BITIX,DI_FLDN                                        
         J     EXIT                return GETFLD cc                             
         EJECT                                                                  
***********************************************************************         
* OCon record key and data definitions followed by routines           *         
***********************************************************************         
                                                                                
OCNKEY   DS    0X                  ** OCON KEY DEFINITIONS **                   
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(SAOCOVS-SAOCKEY,L'SAOCOVS),AL4(EDTOVS)                       
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(SAOCOVPG-SAOCKEY,L'SAOCOVPG),AL4(EDTPGM)                     
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(SAOCUID-SAOCKEY,L'SAOCUID),AL4(EDTUID)                       
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(SAOCAGN-SAOCKEY,L'SAOCAGN),AL4(EDTAGN)                       
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
OCNKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
OCNFLD   DS    0X                  ** OCON FIELD DEFINITIONS **                 
                                                                                
* X'01' SAACVD  Activity Date                                                   
* X'BF' SAOCTD  Options                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Activity Date'                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAACVELQ,SAACVDT-SAACVD,L'SAACVDT)                           
         DC    AL1(DD_TBDTQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Text' (not used)                                   
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAOCTELQ,SAOCTTAB-SAOCTD,0)                                  
         DC    AL1(DD_TSHRQ)       handle whole pair                            
         DC    AL4(OCNFOCTE)                                                    
         DC    XL12'00'                                                         
                                                                                
OCNFLDX  DC    AL1(DD_EOTQ)                                                     
                                                                                
OCNFOCTE NTR1  LABEL=NO            ** OCNFLD SAOCTTAB Element data **           
         USING SAOCTD,R2                                                        
         MVI   BLOCK1,C'N'         BLOCK1 contains old option data              
         MVC   BLOCK1+1(255),BLOCK1 fill with 'N's                              
         MVC   BLOCK2(256),BLOCK1  BLOCK2 contains new option data              
         XR    R5,R5               R5=number of SAOCTTAB bytes                  
         L     RF,DI_ADSD          pick up sort key                             
         L     R1,DI_AIO1                                                       
         CLI   DS_TYPE-DS_D(RF),DS_TCPYQ old is in IO1 for change               
         JE    OCNFOT05                                                         
         L     R1,DI_AIO2                                                       
         CLI   DS_TYPE-DS_D(RF),DS_TDELQ old is in IO2 for delete               
         JNE   OCNFOT20            else no old record                           
OCNFOT05 MVI   ELCODE,SAOCTELQ     option element                               
         BRAS  RE,GETEL                                                         
         JNE   OCNFOT10            element not found                            
         LR    R2,R1                                                            
         LLC   R4,SAOCTLN                                                       
         SHI   R4,SAOCTLNQ         R4=length of SAOCTTAB bytes                  
         JNP   OCNFOT10            all done if none                             
         LR    R5,R4               save length                                  
         GOTOR BLOCKIN,DMCB,((R4),SAOCTTAB),(C'Y',BLOCK1)                       
OCNFOT10 L     RF,DI_ADSD          pick up sort key                             
         L     R1,DI_AIO2                                                       
         CLI   DS_TYPE-DS_D(RF),DS_TCPYQ new in IO2 if change                   
         JE    OCNFOT15                                                         
         L     R1,DI_AIO1                                                       
         CLI   DS_TYPE-DS_D(RF),DS_TADDQ new in IO1 if add                      
         JE    OCNFOT15                                                         
         CLI   DS_TYPE-DS_D(RF),DS_TRESQ new in IO1 if restore                  
         JNE   OCNFOT20            else no new record (delete)                  
OCNFOT15 MVI   ELCODE,SAOCTELQ     option element                               
         BRAS  RE,GETEL                                                         
         JNE   OCNFOT20            element not found                            
         LR    R2,R1                                                            
         LLC   R4,SAOCTLN                                                       
         SHI   R4,SAOCTLNQ         R4=length of SAOCTTAB bytes                  
         JNP   OCNFOT20            all done if none                             
         CR    R4,R5               test more bytes than old element             
         JNH   *+6                                                              
         LR    R5,R4               yes, use longer                              
         GOTOR BLOCKIN,DMCB,((R4),SAOCTTAB),(C'Y',BLOCK2)                       
         DROP  R2                                                               
OCNFOT20 LARL  RF,OCNFOTOP         edit routine for BLOCKOUT                    
         GOTOR BLOCKOUT,DMCB,(RF),(R5) write out differences                    
         J     EXITY                                                            
                                                                                
OCNFOTOP NTR1  LABEL=NO            Edit out option name for FCNFFCTE            
         MVI   DI_FLDN,C' '                                                     
         MVC   DI_FLDN+1(L'DI_FLDN-1),DI_FLDN                                   
         GOTOR GETOPT,DMCB,BITIX,DI_FLDN                                        
         J     EXIT                return GETOPT cc                             
         EJECT                                                                  
***********************************************************************         
* T/S approver record key and data definitions followed by routines   *         
***********************************************************************         
                                                                                
TSAKEY   DS    0X                  ** T/S APP KEY DEFINITIONS **                
                                                                                
         DC    AL1(DK_TCHRQ)                                                    
         DC    AL1(SAAPAGR-SAAPKEY,L'SAAPAGR),AL4(0)                            
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
TSAKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
TSAFLD   DS    0X                  ** T/S APP FIELD DEFINITIONS **              
                                                                                
* X'01' SAACVD  Activity Date                                                   
* X'C2' SAPCTD  Person Count (not handled here)                                 
* X'CE' SACOMD  Comment                                                         
* X'CF' SAMAND  Manager Id                                                      
* X'DF' SAAPGD  Approver Group                                                  
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Activity Date'                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAACVELQ,SAACVDT-SAACVD,L'SAACVDT)                           
         DC    AL1(DD_TBDTQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Name'                                              
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAAPGELQ,SAAPGNAM-SAAPGD,0)                                  
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDIMQ)                                                    
         DC    CL(L'DD_NAME)'SACOMELQ'                                          
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SACOMELQ,SACOMOPT-SACOMD,L'SACOMOPT+L'SACOMLIN)              
         DC    AL1(0)                                                           
         DC    AL4(0,ALLFCOMK,ALLFCOMD) use common routines/table               
         DC    XL4'00'                                                          
                                                                                
         DC    AL1(DD_LDIMQ)                                                    
         DC    CL(L'DD_NAME)'SAMANELQ'                                          
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAMANELQ,SAMANORD-SAMAND,L'SAMANORD)                         
         DC    AL1(0)                                                           
         DC    AL4(0,ALLFMANK,ALLFMAND) use common routines/table               
         DC    XL4'00'                                                          
                                                                                
TSAFLDX  DC    AL1(DD_EOTQ)                                                     
         EJECT                                                                  
***********************************************************************         
* User Profile record key and data definitions followed by routines   *         
***********************************************************************         
                                                                                
UPRKEY   DS    0X                  ** PROFILE KEY DEFINITIONS **                
                                                                                
         DC    AL1(DK_TCHRQ)                                                    
         DC    AL1(CTUKSYS-CTUKEY,L'CTUKSYS),AL4(0)                             
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(CTUKPROG-CTUKEY,L'CTUKPROG),AL4(UPRKPRG)                     
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(CTUKAGY-CTUKEY,L'CTUKAGY),AL4(UPRKAGY)                       
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(CTUKMED-CTUKEY,7),AL4(UPRKOTH)                               
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
UPRKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
UPRKPRG  L     R2,DI_AINP          Edit profile program field                   
         L     R3,DI_AOUT                                                       
         MVC   0(2,R3),1(R2)                                                    
         MVI   DI_LOUT,2                                                        
         CLI   0(R2),0             if 1st byte is null, use last two            
         JE    EXITYRE                                                          
         MVC   0(3,R3),0(R2)       else all 3                                   
         MVI   DI_LOUT,3                                                        
         J     EXITYRE                                                          
                                                                                
UPRKAGY  L     R2,DI_AINP          Edit profile agency/user id field            
         L     R3,DI_AOUT                                                       
         CLI   0(R2),X'50'         user id rather than alpha id                 
         JNH   UPRKAG10            yes                                          
         MVC   0(3,R3),=C'All'     else it's basically an 'All' item            
         MVI   DI_LOUT,3                                                        
         J     EXITYRE                                                          
UPRKAG10 LR    R0,RE                                                            
         GOTOR GETUID,DMCB,(R2),(R3)                                            
         LR    RE,R0                                                            
         MVI   DI_LOUT,L'CTID                                                   
         J     EXITYRE                                                          
                                                                                
UPRKOTH  L     R2,DI_AINP          Edit profile 'other data'                    
         L     R3,DI_AOUT                                                       
         OC    0(7,R2),0(R2)                                                    
         JZ    EXITYRE             nothing to display                           
         L     R4,DI_AIO1          contains key                                 
         USING CTUREC,R4                                                        
         CLI   CTUKPROG,0          offline profile?                             
         JNE   UPRKOT30            no, can't be writer style                    
         L     RF,=A(WRITBLE)      writer style program table                   
UPRKOT10 CLC   CTUKSYS,0(RF)                                                    
         JNE   *+14                                                             
         CLC   CTUKPROG+1(2),1(RF) test valid writer type report                
         JE    UPRKOT20            writer code present                          
         LA    RF,21(,RF)          Note: wrong length on table                  
         CLI   0(RF),X'FF'                                                      
         JNE   UPRKOT10                                                         
         J     UPRKOT30                                                         
UPRKOT20 MVC   0(7,R3),=C'Writer='                                              
         MVC   7(L'CTUKNAM,R3),CTUKNAM                                          
         MVI   DI_LOUT,7+L'CTUKNAM                                              
         J     EXITYRE                                                          
UPRKOT30 CLI   CTUKSYS,C'A'        if ACCPAK, 'ula' form                        
         JNE   UPRKOT40                                                         
         MVC   0(L'CTUKUNT+L'CTUKLDG+L'CTUKACT,R3),CTUKUNT                      
         MVI   DI_LOUT,L'CTUKUNT+L'CTUKLDG+L'CTUKACT                            
         J     EXITYRE                                                          
UPRKOT40 MVC   0(L'CTUKMED,R3),CTUKMED if not ACCPAK, 'Med,Cli' form            
         LA    RF,1(,R3)                                                        
         CLI   CTUKMED,0                                                        
         JNE   *+14                                                             
         MVC   0(3,R3),=C'All'                                                  
         LA    RF,3(,R3)                                                        
         OC    CTUKCLT,CTUKCLT                                                  
         JZ    UPRKOT45                                                         
         MVI   0(RF),C','                                                       
         MVC   1(L'CTUKCLT,RF),CTUKCLT                                          
*&&US*&& LA    RF,1+L'CTUKCLT(,RF)                                              
*&&UK                                                                           
         CLI   CTUKCLT+1,C' '                                                   
         JNH   *+12                                                             
         LA    RF,1+L'CTUKCLT(,RF)                                              
         J     UPRKOT45                                                         
         XR    R1,R1                                                            
         ICM   R1,3,CTUKCLT+1                                                   
         CVD   R1,DUB                                                           
         UNPK  2(4,RF),DUB+5(3)                                                 
         OI    5(RF),X'F0'                                                      
         LA    RF,1+5(,RF)                                                      
*&&                                                                             
UPRKOT45 SR    RF,R3                                                            
         STC   RF,DI_LOUT                                                       
         J     EXITYRE                                                          
         DROP  R4                                                               
                                                                                
UPRFLD   DS    0X                  ** PROFILE FIELD DEFINITIONS **              
                                                                                
* X'01' SAACVD  Activity Date                                                   
* X'72' CTPVD   Profile Value (CTGENFILE)                                       
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Activity Date'                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAACVELQ,SAACVDT-SAACVD,L'SAACVDT)                           
         DC    AL1(DD_TBDTQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDIMQ)                                                    
         DC    CL(L'DD_NAME)'CTPVELQ'                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(CTPVELQ,CTPVPAGE-CTPVD,L'CTPVPAGE)                           
         DC    AL1(0)                                                           
         DC    AL4(0,UPRFPVK,UPRFPVD)                                           
         DC    XL4'00'                                                          
                                                                                
UPRFLDX  DC    AL1(DD_EOTQ)                                                     
                                                                                
UPRFPVK  L     RF,DI_AREL          ** UPRFLD CTPVEL element key **              
         XR    R0,R0                                                            
         ICM   R0,1,CTPVPAGE-CTPVD(RF) page number                              
         JZ    EXITYRE                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DI_EKEY(3),DUB+6(2)                                              
         CLI   DI_EKEY,C'0'                                                     
         JNE   EXITYRE                                                          
         MVC   DI_EKEY(3),DI_EKEY+1                                             
         J     *-14                                                             
                                                                                
UPRFPVD  DS    0X                  ** UPRFLD CTPVEL element data **             
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Text' (not used)                                   
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(0,CTPVALUE-CTPVD,0)                                          
         DC    AL1(DD_TSHRQ)       handle whole pair                            
         DC    AL4(UPRFPVVE)                                                    
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_EOTQ)                                                     
                                                                                
UPRFPVVE NTR1  LABEL=NO            ** UPRFLD CTPVALUE Element data **           
         L     RF,DI_AIO1          contains key                                 
         LA    RF,CTUKSYS-CTUKEY(RF) get sys/prog from key                      
         GOTOR GETUPF,DMCB,(RF),BLOCK2 get field formats for sys/prog           
         XC    BLOCK1(32),BLOCK1   16 old, 16 new values                        
         ICM   RF,15,DI_AOEL                                                    
         JZ    *+10                                                             
         MVC   BLOCK1+00(16),CTPVALUE-CTPVD(RF) 16 old values                   
         ICM   RF,15,DI_ANEL                                                    
         JZ    *+10                                                             
         MVC   BLOCK1+16(16),CTPVALUE-CTPVD(RF) 16 new values                   
         LA    R2,BLOCK1           R2=16+16 bytes old/new values                
         LA    R3,BLOCK2           R3=16 bytes format info                      
         LA    R4,1                R4=number                                    
         L     R5,DI_ADSD          R5=sort key                                  
UPRFPV10 CLI   0(R3),0             is this one used?                            
         JE    UPRFPV50            no, next                                     
         CLC   0(1,R2),16(R2)      has this one changed?                        
         JE    UPRFPV50            no, next                                     
         MVC   DI_FLDN(8),=C'Profile#'                                          
         CVD   R4,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DI_FLDN+8(2),DUB+6(2)                                            
UPRFPV20 CLI   DS_TYPE-DS_D(R5),DS_TADDQ is it add                              
         JE    UPRFPV30            yes, no old                                  
         CLI   DS_TYPE-DS_D(R5),DS_TRESQ is it restore                          
         JE    UPRFPV30            yes, no old                                  
         MVC   DI_OVAL(1),0(R2)    old value                                    
         CLI   0(R3),C'C'          character format?                            
         JE    UPRFPV30            yes, done                                    
         CLI   0(R3),C'N'          numeric format?                              
         JE    UPRFPV22            yes, skip                                    
         GOTOR CHEXOUT,DMCB,(R2),DI_OVAL,1                                      
         J     UPRFPV30                                                         
UPRFPV22 LLC   R0,0(R2)            numeric                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DI_OVAL(3),DUB+6(2)                                              
UPRFPV30 CLI   DS_TYPE-DS_D(R5),DS_TDELQ is it delete                           
         JE    UPRFPV40            yes, no new                                  
         MVC   DI_NVAL(1),16(R2)   new value                                    
         CLI   0(R3),C'C'          character format?                            
         JE    UPRFPV40            yes, done                                    
         CLI   0(R3),C'N'          numeric format?                              
         JE    UPRFPV32            yes, skip                                    
         GOTOR CHEXOUT,DMCB,16(R2),DI_NVAL,1                                    
         J     UPRFPV40                                                         
UPRFPV32 LLC   R0,16(R2)           numeric                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DI_NVAL(3),DUB+6(2)                                              
UPRFPV40 GOTOR DI_APUT                                                          
UPRFPV50 AHI   R4,1                                                             
         CHI   R4,16                                                            
         JH    EXITY               exit after 16                                
         AHI   R2,1                                                             
         AHI   R3,1                                                             
         J     UPRFPV10                                                         
         EJECT                                                                  
***********************************************************************         
* TxtLAL record key and data definitions followed by routines         *         
***********************************************************************         
                                                                                
TLAKEY   DS    0X                  ** TEXT LAL KEY DEFINITIONS **               
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(SATLSYS-SATLKEY,L'SATLSYS),AL4(EDTOVS)                       
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TCHRQ)                                                    
         DC    AL1(SATLLID-SATLKEY,L'SATLLID),AL4(0)                            
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
TLAKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
TLAFLD   DS    0X                  ** TEXT LAL FIELD DEFINITIONS **             
                                                                                
* X'01' SAACVD  Activity Date                                                   
* X'02' SADSCD  Description                                                     
* X'07' SATLMD  Text Limit Access List Elements                                 
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Activity Date'                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SAACVELQ,SAACVDT-SAACVD,L'SAACVDT)                           
         DC    AL1(DD_TBDTQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Description'                                       
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SADSCELQ,SADSC-SADSCD,0)                                     
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDIMQ)                                                    
         DC    CL(L'DD_NAME)'SATLMEL' (replaced in TLAFTLME routine)            
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(SATLMELQ,SATLMTYP-SATLMD,L'SATLMTYP)                         
         DC    AL1(0)                                                           
         DC    AL4(0,TLAFTLMK,TLAFTLMD)                                         
         DC    XL4'00'                                                          
                                                                                
TLAFLDX  DC    AL1(DD_EOTQ)                                                     
                                                                                
TLAFTLMK L     RF,DI_AREL          ** TLAFLD SATLMEL Element key **             
         MVC   DI_EKEY(L'SATLMTYP),SATLMTYP-SATLMD(RF)                          
         TM    SATLMCTL-SATLMD(RF),SATLMRDQ                                     
         JO    EXITYRE                                                          
         MVC   DI_EKEY+L'SATLMTYP(6),=C'(Read)'                                 
         J     EXITYRE                                                          
                                                                                
TLAFTLMD DS    0X                  ** TLAFLD SATLMEL Element data **            
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Value'                                             
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(0,SATLMDTA-SATLMD,0)                                         
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(TLAFTLVE)                                                    
         DC    XL12'00'                                                         
         DC    AL1(DD_EOTQ)                                                     
                                                                                
TLAFTLVE NTR1  LABEL=NO            ** TLAFLD SATLMDTA Element data **           
         USING SATLMD,R2                                                        
         ICM   R2,15,DI_AOEL       pick up old element                          
         JZ    TLAFTE10            not there                                    
         LA    RF,DI_OVAL                                                       
         TM    SATLMCTL,SATLMNFQ   test negative                                
         JZ    *+12                                                             
         MVI   0(RF),C'-'                                                       
         AHI   RF,1                                                             
         LLC   R1,SATLMLEN                                                      
         SHI   R1,(SATLMDTA-SATLMD)+1 R1=length of data                         
         LARL  RE,TLAFTE40                                                      
         EX    R1,0(RE)                                                         
TLAFTE10 ICM   R2,15,DI_ANEL       pick up new element                          
         JZ    TLAFTE20            not there                                    
         LA    RF,DI_NVAL                                                       
         TM    SATLMCTL,SATLMNFQ   test negative                                
         JZ    *+12                                                             
         MVI   0(RF),C'-'                                                       
         AHI   RF,1                                                             
         LLC   R1,SATLMLEN                                                      
         SHI   R1,(SATLMDTA-SATLMD)+1 R1=length of data                         
         LARL  RE,TLAFTE40                                                      
         EX    R1,0(RE)                                                         
TLAFTE20 J     EXITY                                                            
                                                                                
TLAFTE40 MVC   0(0,RF),SATLMDTA    executed copy data                           
         EJECT                                                                  
***********************************************************************         
* Media office record key and data definitions followed by routines   *         
***********************************************************************         
                                                                                
MOFFKEY  DS    0X                  ** MEDIA OFFEIC 1 KEY DEFINITIONS **         
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(MOFK1OF-MOFKEY,L'MOFK1OF),AL4(EDTMOFF)                       
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(MOFKSYS-MOFKEY,L'MOFKSYS),AL4(EDTOVS)                        
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
MOFFKEYX DC    AL1(DK_EOTQ)                                                     
                                                                                
MOFFFLD  DS    0X                  **MEDIA OFFICE 1 FIELD DEFINITIONS**         
                                                                                
* X'0A' MONAMD  Office Name                                                     
* X'0B' MOOLDD  2 Character Office Code                                         
* X'0C' MOUIPD  WebAdv User ID and Password Element                             
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Office Short Name'                                 
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(MONAMELQ,MONAMSH-MONAMD,L'MONAMSH)                           
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Office Long Name'                                  
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(MONAMELQ,MONAMLO-MONAMD,L'MONAMLO)                           
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'Old Office Code'                                   
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(MOOLDELQ,MOOLDOFF-MOOLDD,L'MOOLDOFF)                         
         DC    AL1(DD_TBISQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'WebDAV User ID'                                    
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(MOUIPELQ,MOUIPUID-MOUIPD,L'MOUIPUID)                         
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_LDISQ)                                                    
         DC    CL(L'DD_NAME)'WebDAV Password'                                   
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(MOUIPELQ,MOUIPASS-MOUIPD,L'MOUIPASS)                         
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
MOFFFLDX DC    AL1(DD_EOTQ)                                                     
                                                                                
         EJECT                                                                  
***********************************************************************         
* Data Access List rec key and data definitions followed by routines  *         
***********************************************************************         
                                                                                
DALKEY   DS    0X                  **DATA ACCESS LIST KEY DEFINITIONS**         
                                                                                
         DC    AL1(DK_TSHEQ)                                                    
         DC    AL1(LDTAKNUM-LDTAKEY,L'LDTAKNUM),AL4(EDTDGLN)                    
         DC    AL2(0)                                                           
         DC    XL8'00'                                                          
                                                                                
DALKEYX  DC    AL1(DK_EOTQ)                                                     
                                                                                
DALFLD   DS    0X                  **DATA ACCESS LIST FLD DEFINITIONS**         
                                                                                
* ..... LDTAKCOD Data Value (from Key)                                          
* X'FE' SAACVD   Activity Date      (no need to report this)                    
                                                                                
         DC    AL1(DD_LDIRQ)                                                    
         DC    CL(L'DD_NAME)'Values'                                            
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL2(LDTAKCOD-LDTAKEY),AL1(L'LDTAKCOD)                            
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(EDTLDCOD)                                                    
         DC    XL12'00'                                                         
                                                                                
DALFLDX  DC    AL1(DD_EOTQ)                                                     
         EJECT                                                                  
***********************************************************************         
* General data definitions and routines                               *         
***********************************************************************         
                                                                                
ALLFMANK L     RF,DI_AREL          ** general SAMANEL Element key **            
         MVC   DI_EKEY(1),SAMANORD-SAMAND(RF)                                   
         OI    DI_EKEY,X'F0'       one byte binary 1 to 6 -> alpha              
         J     EXITYRE                                                          
                                                                                
ALLFMAND DS    0X                  ** general SAMANEL Element data **           
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Manager Id'                                        
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(0,SAMANID-SAMAND,L'SAMANID)                                  
         DC    AL1(DD_TSHEQ)                                                    
         DC    AL4(ALLFMAIE)                                                    
         DC    XL12'00'                                                         
                                                                                
         DC    AL1(DD_EOTQ)                                                     
                                                                                
ALLFMAIE L     R2,DI_AINP          R2 is A(SAMANID)                             
         L     R3,DI_AOUT                                                       
         LR    R0,RE                                                            
         GOTOR GETPID,DMCB,(R2),(R3) get person id                              
         LR    RE,R0                                                            
         J     EXITYRE                                                          
                                                                                
ALLFCOMK L     RF,DI_AREL          ** general SACOMEL Element key **            
         USING SACOMD,RF                                                        
         LA    R1,DI_EKEY                                                       
         CLI   SACOMOPT,C' '       if option present                            
         JNH   *+18                                                             
         MVC   0(1,R1),SACOMOPT    do opt(1)||"/"||line(3), e.g. A/001          
         MVI   1(R1),C'/'                                                       
         LA    R1,2(,R1)                                                        
         LLC   R0,SACOMLIN         else just line(3), e.g. 001                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R1),DUB+6(2)                                                 
         DROP  RF                                                               
         CLI   0(R1),C'0'                                                       
         JNE   EXITYRE                                                          
         MVC   0(3,R1),1(R1)                                                    
         J     *-14                                                             
                                                                                
ALLFCOMD DS    0X                  ** general SACOMEL Element data **           
                                                                                
         DC    AL1(DD_LDIEQ)                                                    
         DC    CL(L'DD_NAME)'Comment'                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
         DC    AL1(0,SACOMDAT-SACOMD,0)                                         
         DC    AL1(DD_TCHRQ)                                                    
         DC    XL16'00'                                                         
                                                                                
         DC    AL1(DD_EOTQ)                                                     
         EJECT                                                                  
EOR      EQU   0                                                                
FF       EQU   X'FF'                                                            
COMMA    EQU   C','                                                             
                                                                                
LITERALS DS    0D                  ** GLOBAL LITERALS **                        
         LTORG                                                                  
                                                                                
ABUFF1   DC    A(BUFF1)            BUFFERIN CONTROL BLOCK 1                     
ASYSLST  DC    A(SYSLST)                                                        
                                                                                
VSELIST  DC    V(SELIST)                                                        
VDICTATE DC    V(DICTATE)                                                       
VLISTIO  DC    V(LISTIO)                                                        
*&&US                                                                           
VCLUNPK  DC    V(CLUNPK)                                                        
*&&                                                                             
                                                                                
DATADISP DC    Y(SAPEDATA-SAPEKEY)                                              
                                                                                
YAUTH    DC    X'000F'                                                          
NAUTH    DC    X'0000'                                                          
                                                                                
DMREAD   DC    C'DMREAD '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
GETREC   DC    C'GETREC '                                                       
                                                                                
CORPHS   DS    0AL1                ** CORE RESIDENT PHASE LIST **               
         DC    X'0D'               SQUASHER                                     
         DC    X'12'               XSORT                                        
CORPHSN  EQU   (*-CORPHS)/L'CORPHS                                              
                                                                                
         EJECT                                                                  
***********************************************************************         
* System/File list                                                    *         
* Not used as CONTROL already opened by GEDFAR, but literals used     *         
***********************************************************************         
*                                                                               
*SFLIST   DS    0C                                                              
*                                                                               
*         DC    C'CONTROL'                                                      
*                                                                               
*         DC    C'N'                                                            
CTFILE   DC    C'CTFILE '                                                       
*         DC    C'N'                                                            
GENDIR   DC    C'GENDIR '                                                       
*         DC    C'N'                                                            
GENFIL   DC    C'GENFIL '                                                       
*         DC    C'X'                                                            
         EJECT                                                                  
***********************************************************************         
* Program table                                                       *         
***********************************************************************         
                                                                                
PRGTAB   DS    0XL(DP_LNQ)                                                      
         DC    X'01',CL(L'DP_NAME)'PFM'                                         
         DC    X'03',CL(L'DP_NAME)'PROFILE'                                     
         DC    X'08',CL(L'DP_NAME)'FUS'                                         
         DC    X'0D',CL(L'DP_NAME)'SECURE'                                      
         DC    X'1E',CL(L'DP_NAME)'SECURE'                                      
PRGTABX  DC    AL1(DP_EOTQ)                                                     
***********************************************************************         
* Buffers                                                             *         
***********************************************************************         
                                                                                
BUFF1    BUFFD TYPE=D,KEYLEN=BUFFKEYL,COMLEN=BUFFDATL,BUFFERS=255               
***********************************************************************         
* Included tables                                                     *         
***********************************************************************         
                                                                                
*FASYSLST                                                                       
*SRDQUPROF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSLST                                                       
       ++INCLUDE SRDQUPROF                                                      
         PRINT ON                                                               
         EJECT                                                                  
WORKD    DSECT                     ** LOCAL WORKING STORAGE **                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
FULL     DS    F                                                                
BYTE     DS    X                                                                
ELCODE   DS    X                   element code for getel                       
WORK     DS    XL256                                                            
         DS    0F                                                               
LISTIOBK DS    XL(LISTIOBL)                                                     
         DS    0H                                                               
BLOCK    DS    0CL512                                                           
BLOCK1   DS    CL256                                                            
         DS    CL(400-256)                                                      
         DS    0H                                                               
BLOCK2   DS    CL256                                                            
         DS    CL(400-256)                                                      
                                                                                
APHASES  DS    0A                  ** CORE RESIDENT PHASE ADDRESSES **          
VSQUASH  DS    A                   SQUASHER                                     
VXSORT   DS    A                   XSORT                                        
                                                                                
SYSPGM   DS    XL2                 system and program work area                 
BITIX    DS    X                   bit index for bit tables                     
                                                                                
PGMACTSP DS    XL2                 system and program for PGMACT                
PGMACT   DS    CL(L'SAPGMACT)      action list for program in PGMACTSP          
                                                                                
KEY      DS    XL64                GENERAL KEY BUILDING AREA                    
                                                                                
BUFFREC  DS    0X                  ** BUFFERIN RECORD **                        
BUFFKEY  DS    0X                                                               
BUFFKAGY DS    CL(L'DS_ALF)        Agency Alpha (but all cleard on chg)         
BUFFKTYP DS    X                   Buffer record type                           
BUFFUIDQ EQU   1                   User Id num/name                             
BUFFUIAQ EQU   2                   User Id num/alpha id                         
BUFFPIDQ EQU   3                   Pin/Pid                                      
BUFFAGNQ EQU   4                   Access Group                                 
BUFFRCDQ EQU   5                   Record num/name                              
BUFFACTQ EQU   6                   Action num/name                              
BUFFFLDQ EQU   7                   Field num/name                               
BUFFOPTQ EQU   8                   Option num/name                              
BUFFUPFQ EQU   9                   User profile field formats                   
BUFFDGNQ EQU   10                  Data Group                                   
BUFFDLNQ EQU   11                  Data Group List Number (AGY=X'0000')         
BUFFRKEY DS    0X                  User (Record) Key area                       
BUFFKUIN DS    CL(L'CTIKNUM)       User ID number as key                        
         ORG   BUFFRKEY                                                         
BUFFKPIN DS    CL(L'SA0KNUM)       Person ID number as key                      
         ORG   BUFFRKEY                                                         
BUFFKAGN DS    CL(L'SAAGNNUM)      Access Group number as key                   
         ORG   BUFFRKEY                                                         
BUFFKDGN DS    CL(L'LLSTKGRP)      Data Group number as key                     
         ORG   BUFFRKEY                                                         
BUFFKDLN DS    CL(L'LNUMKNUM)      Data Group List number as key                
         ORG   BUFFRKEY                                                         
BUFFKSYP DS    CL2                 Sys/Prog as key for rec/act                  
BUFFKRCD DS    0CL1                Record number as key *or*                    
BUFFKACT DS    0CL1                Action as key *or*                           
BUFFKFLD DS    0CL1                Field as key *or*                            
BUFFKOPT DS    CL1                 Option as key                                
         ORG   BUFFRKEY                                                         
BUFFKUSP DS    CL(L'CTUKSYS+L'CTUKPROG) User profile Sys/Prog                   
BUFFKUSQ DS    CL1                 User profile seq - 1 or 2                    
         ORG   ,                                                                
BUFFKEYL EQU   *-BUFFKEY                                                        
BUFFFLAG DS    X                   ** FLAG BYTE **                              
BUFFFRNF EQU   X'80'               Record defined by key not found              
BUFFDATA DS    0X                  Data area (if not BUFFFRNF)                  
BUFFDUID DS    CL(L'CTID)          User ID as data                              
         ORG   BUFFDATA                                                         
BUFFDUIA DS    CL(L'CTAGYID)       Agency alpha id                              
         ORG   BUFFDATA                                                         
BUFFDPID DS    CL(L'SAPEPID)       Person ID as data                            
         ORG   BUFFDATA                                                         
BUFFDAGC DS    CL(L'SAAGAGR)       Access Group Code as data                    
         ORG   BUFFDATA                                                         
BUFFDDGC DS    CL(L'SALANCOD)      Data Group Code as data                      
         ORG   BUFFDATA                                                         
*                                  Data Group List number info                  
*(l'LNUMKNUM+L'LNUMKAGY+L'LNUMKPSN+L'LNUMKGRP+L'LNUMKSYS+L'LNUMKTYP)            
BUFFDDLN DS    0CL13               Longest Length for LIST=xxxxxxxx             
BUFFDNUM DS    CL(L'LNUMKNUM)                                                   
BUFFDNAG DS    CL(L'LNUMKAGY)                                                   
BUFFDNPN DS    CL(L'LNUMKPSN)                                                   
BUFFDNGP DS    CL(L'LNUMKGRP)                                                   
BUFFDNSY DS    CL(L'LNUMKSYS)                                                   
BUFFDNTY DS    CL(L'LNUMKTYP)                                                   
         ORG   BUFFDATA                                                         
BUFFDRCD DS    0CL8                Record name as data                          
BUFFDACT DS    0CL8                Action name as data                          
BUFFDFLD DS    0CL8                Field name as data                           
BUFFDOPT DS    CL8                 Option name as data                          
         ORG   BUFFDATA                                                         
BUFFDUPF DS    CL8                 User profile field formats CTFDTYPE          
         ORG   ,                                                                
BUFFRECL EQU   *-BUFFREC                                                        
BUFFDATL EQU   BUFFRECL-BUFFKEYL                                                
                                                                                
IO       DS    XL4096                                                           
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
* INCLUDED DSECTS                                                               
*        PRINT OFF                                                              
       ++INCLUDE DDBUFFD                                                        
       ++INCLUDE GEDFARD                                                        
       ++INCLUDE GEGENFILE                                                      
       ++INCLUDE DDCOMFACSD                                                     
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDCTRYEQUS                                                     
       ++INCLUDE DMRCVRHDR                                                      
       ++INCLUDE SEACSFILE                                                      
LISTIOD  DSECT                                                                  
       ++INCLUDE DDLISTD                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FAPGMLST                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011SEDFAR    11/18/20'                                      
         END                                                                    
