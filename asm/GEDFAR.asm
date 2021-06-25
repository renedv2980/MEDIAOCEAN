*          DATA SET GEDFAR     AT LEVEL 012 AS OF 05/29/19                      
*PHASE GEDFARA                                                                  
*INCLUDE ADDAY                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE BUFFERIN                                                               
*INCLUDE CARDS                                                                  
*INCLUDE CUREDIT                                                                
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE DLFLD                                                                  
*INCLUDE EDITOR                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE GETPROF                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE DMDMGRL                                                                
*INCLUDE SORTER                                                                 
GEDFAR   TITLE 'Generalized Daily File Activity Extract'                        
GEDFAR   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKL,*GEDFAR*,AWORK,CLEAR=YES                                   
                                                                                
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         ENTRY COMFACS                                                          
                                                                                
         LR    RA,RC                                                            
         USING WORKD,RA            RA=A(working storage)                        
         USING RECHDD,RECHD        Extracted recovery header/trailer            
         LARL  R9,LITERALS                                                      
         USING LITERALS,R9         R9=A(global literals)                        
         USING DI_D,DI_BLOCK                                                    
         USING DLCBD,DLCB                                                       
         USING COMFACSD,COMFACS                                                 
         L     R8,VCPRINT                                                       
         USING DPRINT,R8           R8=A(print block)                            
         OI    VPRINT,X'80'        Set to use 198 characters                    
         STM   R8,RA,PREGS         Save pointer registers                       
         MVC   TITLE(L'TITLIT),TITLIT                                           
         B     MAIN                                                             
                                                                                
AWORK    DC    A(WORKAREA)         A(working storage area)                      
         EJECT                                                                  
***********************************************************************         
* Main processing                                                     *         
***********************************************************************         
                                                                                
MAIN     GOTOR VALPAR              Read parameter cards                         
         BNE   MAINX               Exit on error                                
         OC    TODAYB,TODAYB       Do we know today's date                      
         BNZ   MAIN02                                                           
         GOTOR VDATCON,DMCB,(5,0),(3,TODAYB)                                    
                                                                                
MAIN02   GOTOR VDATCON,DMCB,(3,TODAYB),(20,TODAYE)                              
         GOTOR GETINP              Read input file                              
         GOTOR PUTOUT              Read sorted records/put output               
                                                                                
MAINX    XBASE ,                   Exit back to mvs                             
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* Read and validate input parameter cards                             *         
***********************************************************************         
                                                                                
VALPAR   NTR1  BASE=*,LABEL=*,WORK=(RC,VPWORKL)                                 
         USING VPWORKD,RC          RC=A(local w/s)                              
         XC    VPWORKD(VPWORKL),VPWORKD                                         
         MVC   P(L'VALLIT1),VALLIT1                                             
         GOTOR VPRINT                                                           
         MVC   P(L'VALLIT2),VALLIT2                                             
         GOTOR (RF)                                                             
         GOTOR (RF)                                                             
                                                                                
VALPAR02 GOTOR VCARDS,VPPARA,VPCARD,=C'R'                                       
         MVC   P(L'VPCARD),VPCARD                                               
         GOTOR VPRINT                                                           
         CLI   VPCARD,SLASH        Test end of input stream                     
         BE    VALPAR14                                                         
         CLI   VPCARD,SPACE        Ignore comments                              
         BE    VALPAR02                                                         
         CLI   VPCARD,ASTERISK                                                  
         BE    VALPAR02                                                         
                                                                                
         LA    RE,VPCARD                                                        
         LHI   R0,L'VALTKWRD+1                                                  
         SR    RF,RF                                                            
VALPAR04 CLI   0(RE),EQUAL                                                      
         BE    VALPAR06                                                         
         CLI   0(RE),SPACE                                                      
         BE    VALPAR06                                                         
         AHI   RE,1                                                             
         AHI   RF,1                                                             
         BCT   R0,VALPAR04                                                      
         MVC   P(L'VALLIT3),VALLIT3                                             
         GOTOR VPRINT                                                           
         J     EXITN                                                            
                                                                                
VALPAR06 LTR   RF,RF                                                            
         JZ    EXITN                                                            
         BCTR  RF,0                                                             
         LA    R1,1(RE)            R1=A(keyword data)                           
                                                                                
         LARL  RE,VALTAB                                                        
         USING VALTABD,RE                                                       
VALPAR08 CLI   VALTABD,VALTEOTQ                                                 
         BNE   VALPAR10                                                         
         MVC   P(L'VALLIT4),VALLIT4                                             
         GOTOR VPRINT                                                           
         J     EXITN                                                            
                                                                                
VALPAR10 CLC   VALTKWRD(0),VPCARD                                               
         EX    RF,*-6                                                           
         BE    VALPAR12                                                         
         AHI   RE,VALTABL                                                       
         B     VALPAR08                                                         
                                                                                
VALPAR12 SR    RF,RF                                                            
         ICM   RF,7,VALTROUT                                                    
         CR    RF,RF                                                            
         GOTOR (RF),(R1)                                                        
         BE    VALPAR02                                                         
         MVC   P(L'VALLIT5),VALLIT5                                             
         GOTOR VPRINT                                                           
         J     EXITN                                                            
                                                                                
VALPAR14 CLI   SYS,0               Check system card processed                  
         JNE   VALPAR18                                                         
         MVC   P(L'VALLIT6),VALLIT6                                             
         GOTOR VPRINT                                                           
         J     EXITN                                                            
*                                                                               
*For System=Secure and OMode!=U, if there are agency filters given,             
*we will build the DI_AAGY table with the agency filters list.                  
*This is so that we can send out null reports.                                  
*Because in SEDFAR, we never pre-build prebuild DI_AAGY table for all           
*agencies in INIT mode.  (Almost all agencies has access to control,            
*and the table has max of 512, see AGYTAB.)                                     
*                                                                               
VALPAR18 CLI   SYS,SYSSECQ         System=Secure                                
         JNE   VALPAR50                                                         
         TM    DI_OMODE,DI_OMUPD   OMode!=U                                     
         JO    VALPAR50                                                         
         CLI   AGYFILTS,0          Any Agecny filters                           
         JE    VALPAR50                                                         
*                                                                               
         L     R2,DI_AAGY          Agency table                                 
         USING DA_D,R2                                                          
         LA    R3,AGYFILTS         Agency filter list                           
         LHI   R0,AGYFILTM                                                      
VALPAR20 MVC   DA_ALF,0(R3)        Move in Agency Alpha to the table            
         GOTOR RESAGY,DA_D         Resolve agency values                        
         AHI   R3,L'DA_ALF         Bump to next agency filter                   
         OC    0(L'DA_ALF,R3),0(R3)                                             
         JZ    VALPAR50            no more agency filter                        
         AHI   R2,DA_LNQ           Bump to next agency table entry              
         JCT   R0,VALPAR20                                                      
         DROP  R2                                                               
*                                                                               
*                                                                               
VALPAR50 OC    AKCDLAST,AKCDLAST   Test KEYCOL card processed                   
         JNZ   VALPARX                                                          
         MVC   P(L'VALLIT7),VALLIT7                                             
         GOTOR VPRINT                                                           
         J     EXITN                                                            
                                                                                
VALPARX  ZAP   LINE,MAXLINE                                                     
         OC    REPTITLE,REPTITLE                                                
         JZ    EXITY                                                            
         MVC   TITLE,REPTITLE                                                   
         J     EXITY                                                            
         DROP  RB                                                               
                                                                                
VPWORKD  DSECT                     ** VALPAR local w/s **                       
VPPARM   DS    A                                                                
VPPARA   DS    6F                                                               
VPCARD   DS    CL80                                                             
VPWORKL  EQU   *-VPWORKD                                                        
GEDFAR   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Validate DDSIO=Value                                                *         
***********************************************************************         
                                                                                
VALDIO   L     RF,VDDSIO                                                        
         MVC   0(8,RF),0(R1)                                                    
         BR    RE                                                               
                                                                                
***********************************************************************         
* Validate DSPACE=Value                                               *         
***********************************************************************         
                                                                                
VALDSP   CLI   SSODSPAC,0                                                       
         BNER  RE                                                               
         MVC   SSODSPAC,0(R1)                                                   
         BR    RE                                                               
                                                                                
***********************************************************************         
* Validate TODAY=Value                                                *         
***********************************************************************         
                                                                                
VALTOD   LR    R0,RE                                                            
         LR    RF,R1                                                            
         GOTOR VDATVAL,DMCB,(0,(RF)),DUB                                        
         OC    0(4,R1),0(R1)                                                    
         JNZ   *+8                                                              
         LTR   RE,R0                                                            
         BR    RE                                                               
                                                                                
         GOTOR VDATCON,DMCB,(0,DUB),(3,TODAYB)                                  
         LR    RE,R0                                                            
         CR    RE,RE                                                            
         BR    RE                                                               
                                                                                
***********************************************************************         
* Validate DATES=Range of recovery file dates                         *         
***********************************************************************         
                                                                                
VALDAT   NTR1  LABEL=NO                                                         
         LR    R2,R1                                                            
         LHI   R0,QPERVAL                                                       
         ICM   R0,B'1110',T00A                                                  
         GOTOR GETPHS,DMCB,0,(R0)                                               
         L     RF,0(R1)            RF=A(PERVAL)                                 
         USING PERVALD,WORK                                                     
         GOTOR (RF),(R1),(32,(R2)),PERVALD                                      
         CLI   4(R1),PVRCOK                                                     
         JNE   EXITN                                                            
         LHI   R0,L'DAYMASK*8                                                   
         CLM   R0,3,PVALNDYS       Ensure not too many days                     
         JL    EXITN                                                            
         MVC   STRDATEB,PVALBSTA   Set recovery start date filter               
         MVC   ENDDATEB,PVALBEND   Set recovery end date filter                 
         GOTOR VDATCON,(R1),(3,STRDATEB),(15,STRDATEJ)                          
         GOTOR VDATCON,(R1),(3,STRDATEB),(20,STRDATEE)                          
         GOTOR (RF),(R1),(3,ENDDATEB),(15,ENDDATEJ)                             
         GOTOR (RF),(R1),(3,ENDDATEB),(20,ENDDATEE)                             
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Validate LOAD=Phasename(/X)(+DDD=XXXX)                              *         
***********************************************************************         
                                                                                
VALLOD   LR    R0,RE                                                            
         LR    RF,R1                                                            
         GOTOR GETPHS,VPPARA,0,('LOADQ',(RF))                                   
         LR    RE,R0                                                            
         BR    RE                  (CC set by GETPHS routine)                   
                                                                                
***********************************************************************         
* Validate VERIFY=Phasename(/X)(+DDD=XXXX), VERIFY=*+DDD=XXX          *         
***********************************************************************         
                                                                                
VALVFY   LR    R0,RE                                                            
         LR    RF,R1                                                            
         GOTOR GETPHS,VPPARA,0,('VERIFYQ',(RF))                                 
         LR    RE,R0                                                            
         BR    RE                  (CC set by GETPHS routine)                   
                                                                                
***********************************************************************         
* Validate PATCH=Phasename(/X)(+DDD=XXXX), PATCH=*+DDD=XXX            *         
***********************************************************************         
                                                                                
VALPCH   LR    R0,RE                                                            
         LR    RF,R1                                                            
         GOTOR GETPHS,VPPARA,0,('PATCHQ',(RF))                                  
         LR    RE,R0                                                            
         BR    RE                  (CC set by GETPHS routine)                   
         EJECT                                                                  
***********************************************************************         
* Validate SYSTEM=Value                                               *         
***********************************************************************         
                                                                                
VALSYS   NTR1  LABEL=NO,WORK=(RC,VSWORKL)                                       
         USING VSWORKD,RC                                                       
         CLI   SYS,0               Only one SYSTEM= card allowed                
         JNE   EXITN                                                            
         MVC   VSNAME,0(R1)        Extract input system name                    
         LARL  RE,SYSTAB                                                        
         USING SYSTABD,RE                                                       
         LHI   R0,SYSTABN                                                       
                                                                                
VALSYS02 CLC   SYSTPFX,VSNAME      Match on system prefix                       
         JE    VALSYS04                                                         
         AHI   RE,SYSTABL                                                       
         JCT   R0,VALSYS02                                                      
         J     EXITN                                                            
                                                                                
VALSYS04 MVC   SYS,SYSTSYS         Extract system number                        
         MVC   IND,SYSTIND         Extract system indicators                    
         MVC   PROF,SYSTPROF       Extract profile system/program               
         MVC   PSPP,SYSTPSPP       Extract system/program file name             
         MVC   EDIN,SYSTEDIN       Extract EDICT system value                   
         MVC   PHASE,SYSTPHAS      Extract system handler phase name            
         MVC   SYSNAME,SYSTNAME    Extract system name                          
                                                                                
         GOTOR VDMGR,VSPARA,DMOPEN,CONSYS,CONFILES,WORK,0                       
         CLI   SYS,SYSSECQ         Test security system                         
         JE    VALSYS12                                                         
                                                                                
         USING CTWREC,IO           Read system list record                      
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,CTWKTYPQ                                                 
         MVI   CTWKREC,CTWKRSYS                                                 
         MVC   CTWKSYSN,SYS        Set native system number                     
         GOTOR VDMGR,VSPARA,DMREAD,CTFILE,CTWREC,CTWREC                         
         JE    VALSYS06                                                         
         MVC   P(L'SYSLIT1),SYSLIT1                                             
         GOTOR VPRINT                                                           
         J     EXITN                                                            
                                                                                
VALSYS06 LA    R2,CTWDATA          Look-up system name                          
         USING CTLSTD,R2                                                        
         SR    R0,R0                                                            
VALSYS08 CLI   CTLSTD,0            Test end of record                           
         JE    EXITN                                                            
         CLI   CTLSTEL,CTLSTELQ                                                 
         JNE   *+14                                                             
         CLC   CTLSTNAM,VSNAME     Match on system name                         
         JE    VALSYS10                                                         
         IC    R0,CTLSTLEN         Bump to next element                         
         AR    R2,R0                                                            
         J     VALSYS08            And try again                                
                                                                                
VALSYS10 CLC   CTLSTSYS,SYS        System number must match                     
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   SEN,CTLSTSEN        Extract SE number                            
                                                                                
VALSYS12 GOTOR GETPHS,VSPARA,0,('LOADQ',PHASE)                                  
         JNE   EXITN                                                            
         MVC   APHASE,0(R1)        Set A(system handler phase)                  
         L     RF,APHASE                                                        
         MVC   DH_WORK,0(RF)       Extract header values                        
         USING DH_D,DH_WORK                                                     
                                                                                
         ICM   RF,15,DH_ASFL       Set/test system file list                    
         JZ    VALSYS18                                                         
         MVC   VSSYSTEM,0(RF)      Set system name for open call                
         AHI   RF,L'VSSYSTEM                                                    
                                                                                
         LA    R1,VSFILES          Build file open list                         
         LHI   R0,20               R0=maximum number of files in list           
VALSYS14 CLI   0(RF),C'X'          Test end of file list                        
         JE    VALSYS16                                                         
         MVC   0(8,R1),0(RF)                                                    
         AHI   RF,8                                                             
         AHI   R1,8                                                             
         JCT   R0,VALSYS14                                                      
         DC    H'0'                                                             
                                                                                
VALSYS16 MVC   0(CONFILEL,R1),CONFILES                                          
         MVC   UTLSE,SEN           Set SE number for open                       
         GOTOR VDMGR,VSPARA,DMOPEN,VSSYSTEM,VSFILES,WORK,0                      
                                                                                
VALSYS18 GOTOR CALLSH,DI_MINIQ     Call system handler to initialize            
                                                                                
         TM    IND,SYSTIAAQ        Test all agencies are valid                  
         JNZ   VALSYS22                                                         
                                                                                
         L     R2,DI_AAGY          Set other agency values                      
         USING DA_D,R2                                                          
VALSYS20 CLI   DA_D,DA_EOTQ        Test end of agency table                     
         JE    VALSYS22                                                         
         GOTOR RESAGY,DA_D         Resolve agency values                        
         AHI   R2,DA_LNQ           Bump to next agency table entry              
         J     VALSYS20                                                         
                                                                                
VALSYS22 J     EXITY                                                            
         DROP  R2,RC                                                            
                                                                                
VSWORKD  DSECT                     ** VALSYS local w/s **                       
VSPARA   DS    6F                  Parameter list                               
VSNAME   DS    CL(L'CTLSTNAM)      Input system name                            
VSSYSTEM DS    CL7                 System name for open call                    
VSFILES  DS    XL256               File open list                               
VSWORKL  EQU   *-VSWORKD                                                        
GEDFAR   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Validate AGENCY=List of agency alpha ids separated by commas        *         
***********************************************************************         
                                                                                
*VALAGY   CLI   AGYFILTS,0          May only be one filter card                 
*         BNER  RE                                                              
                                                                                
VALAGY   LA    RF,AGYFILTS                                                      
         LHI   R0,AGYFILTM                                                      
                                                                                
VALAGY01 CLI   0(RF),SPACE         FIND FIRST EMPTY SLOT IN TABLE               
         JNH   VALAGY02                                                         
         AHI   RF,L'AGYFILTS                                                    
         JCT   R0,VALAGY01                                                      
                                                                                
VALAGY02 CLI   0(R1),SPACE                                                      
         JE    VALAGYN                                                          
         CLI   1(R1),SPACE                                                      
         JE    VALAGYN                                                          
         MVC   0(L'AGYFILTS,RF),0(R1)                                           
         AHI   R1,L'AGYFILTS                                                    
         AHI   RF,L'AGYFILTS                                                    
         CLI   0(R1),SPACE                                                      
         BER   RE                                                               
         CLI   0(R1),COMMA                                                      
         JE    *+6                                                              
         BR    RE                                                               
         AHI   R1,1                                                             
         JCT   R0,VALAGY02                                                      
                                                                                
VALAGYN  LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Validate FILE=File name                                             *         
***********************************************************************         
                                                                                
VALFIL   ICM   RF,15,DH_AFIL       Must have processed SYSTEM= card             
         JZ    VALFILN                                                          
                                                                                
         USING DF_D,RF             RF=A(system file table)                      
         SR    R0,R0                                                            
VALFIL02 CLI   DF_D,DF_EOTQ        Test end of file table                       
         JE    VALFILN                                                          
         CLC   DF_NAMEF,0(R1)      Match file name                              
         JE    *+12                                                             
         AHI   RF,DF_LNQ           Bump to next table entry                     
         J     VALFIL02                                                         
                                                                                
         LARL  R1,FILFILT                                                       
         USING FILFD,R1            R1=A(file filter table)                      
         CLI   FILFD,FILFEOTQ      Test end of table                            
         JE    VALFIL04                                                         
         LLC   R0,FILFLEN          No - advance to next                         
         AR    R1,R0                                                            
VALFIL04 MVI   FILFLEN,FILFLNQ     Set length of this entry                     
         MVC   FILFFNUM,DF_NUM     Set file number                              
         MVC   FILFAREC,DF_AREC    Set A(record table)                          
         CR    RE,RE                                                            
         BR    RE                                                               
         DROP  R1,RF                                                            
                                                                                
VALFILN  LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Validate RECORD=Record name (for FILE= previously specified)        *         
***********************************************************************         
                                                                                
VALREC   LARL  RF,FILFILT                                                       
         USING FILFD,RF                                                         
         CLI   FILFLEN,0           Must have FILE= parameter already            
         JE    VALRECN                                                          
         ICM   RF,15,FILFAREC                                                   
         USING DR_D,RF                                                          
         LHI   R2,1                R2=record index number                       
         SR    R0,R0                                                            
                                                                                
VALREC02 CLI   DR_D,DR_EOTQ        Test end of record table                     
         JE    VALRECN                                                          
         CLC   DR_NAMEF,0(R1)      Match on record name                         
         JE    VALREC04                                                         
         AHI   R2,1                Bump record index number                     
         IC    R0,DR_LEN           Bump to next record table entry              
         AR    RF,R0                                                            
         J     VALREC02                                                         
                                                                                
VALREC04 LARL  RF,FILFILT          Add entry to record filter list              
         USING FILFD,RF                                                         
         IC    R0,FILFLEN          Bump filter entry length                     
         AHI   R0,1                                                             
         CHI   R0,255                                                           
         JH    VALRECN                                                          
         STC   R0,FILFLEN                                                       
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
         STC   R2,0(RF)            Set record index number in list              
         CR    RE,RE                                                            
         BR    RE                                                               
         DROP  RF                                                               
                                                                                
VALRECN  LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Validate KEYCOL=Keyword(length modifier)                            *         
***********************************************************************         
                                                                                
VALKCD   NTR1  LABEL=NO                                                         
         LR    RF,R1                                                            
         LHI   R0,L'KCDTNAME+1                                                  
                                                                                
VALKCD02 CLI   0(RF),SPACE         Locate end of keyword                        
         JE    VALKCD04                                                         
         CLI   0(RF),OPAREN        Test length modifier indicator               
         JE    VALKCD04                                                         
         AHI   RF,1                                                             
         JCT   R0,VALKCD02                                                      
         J     EXITN                                                            
                                                                                
VALKCD04 LR    R0,RF                                                            
         SR    R0,R1                                                            
         JZ    EXITN                                                            
         BCTR  R0,0                                                             
         STC   R0,WORK             Save length of input keyword-1               
         MVI   WORK+1,0            Set length not modified                      
                                                                                
         CLI   0(RF),OPAREN        Test length modifier present                 
         JNE   VALKCD10                                                         
         TM    1(RF),X'F0'         Test first length character                  
         JNO   EXITN                                                            
         CLI   2(RF),CPAREN        Test one byte length modifier                
         JE    VALKCD06                                                         
         TM    2(RF),X'F0'         Test second length character                 
         JNO   EXITN                                                            
         CLI   3(RF),CPAREN                                                     
         JNE   EXITN                                                            
         PACK  DUB,1(2,RF)         Two byte length modifier                     
         J     VALKCD08                                                         
                                                                                
VALKCD06 PACK  DUB,1(1,RF)         One byte length modifier                     
                                                                                
VALKCD08 CVB   R0,DUB                                                           
         STC   R0,WORK+1           Set override length                          
                                                                                
VALKCD10 LARL  R2,KCDTAB                                                        
         USING KCDTABD,R2          R2=A(key column definition table)            
         LLC   RF,WORK             RF=length of input keyword-1                 
                                                                                
VALKCD12 CLI   KCDTABD,KCDTEOTQ    Test end of table                            
         JE    EXITN                                                            
         BASR  RE,0                                                             
         CLC   KCDTNAME(0),0(R1)                                                
         EX    RF,0(RE)            Match input to table                         
         JE    VALKCD14                                                         
         AHI   R2,KCDTABL          Bump to next table entry                     
         J     VALKCD12                                                         
                                                                                
VALKCD14 ICM   R3,15,AKCDLAST      Get A(next KCDEFN entry)                     
         JNZ   VALKCD16                                                         
         LARL  R3,KCDEFN           Point to first entry if first time           
         LHI   R0,2                                                             
         STCM  R0,3,KEYDISP        Set initial field displacement               
                                                                                
         USING KCDEFND,R3          R3=A(KCEEFN entry)                           
VALKCD16 MVI   KCDEFTYP,KCDEFTKC   Set keyword type                             
         MVC   KCDEFDSP,KEYDISP    Set displacement to output value             
         MVC   KCDEFLEN,WORK+1     Set override output length                   
         LA    R0,KCDTABD                                                       
         STCM  R0,15,KCDEFTAB      Set A(KCDTAB entry)                          
         SR    R0,R0                                                            
         ICM   R0,1,KCDEFLEN       Get width of output value                    
         JNZ   *+8                                                              
         IC    R0,KCDTOLEN                                                      
         SR    RF,RF                                                            
         ICM   RF,3,KEYDISP                                                     
         AR    RF,R0                                                            
         STCM  RF,3,KEYDISP        Set displacement to next value               
         AHI   R3,KCDEFNL                                                       
         ST    R3,AKCDLAST         Set A(next KCDEFN entry)                     
         J     EXITY                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* Validate KEYLIT=&literal value& (where & is delimiter character)    *         
***********************************************************************         
                                                                                
VALKLV   NTR1  LABEL=NO                                                         
         CLI   0(R1),SPACE         Delimiter can't be a space                   
         JE    EXITN                                                            
         LA    RF,62(R1)           Maximum length of literal=60                 
                                                                                
VALKLV02 CR    RF,R1               Locate end of literal string                 
         JE    EXITN                                                            
         CLC   0(1,RF),0(R1)       Match end delimter with start                
         JE    *+8                                                              
         JCT   RF,VALKLV02                                                      
                                                                                
         AHI   R1,1                R1=A(literal value)                          
         SR    RF,R1               RF=actual length of literal                  
         JNP   EXITN                                                            
         ICM   R2,15,ALITLAST      Get/test next literal address                
         JNZ   *+8                                                              
         L     R2,ALITTAB          Point to start of table if first             
         LR    R4,RF               Save length of literal                       
         LR    R0,R2                                                            
         LR    RE,R1                                                            
         LR    R1,RF                                                            
         MVCL  R0,RE               Move literal to LITTAB                       
         LR    R0,R2                                                            
         AR    R0,R4                                                            
         ST    R0,ALITLAST         Set A(next literal)                          
                                                                                
         ICM   R3,15,AKCDLAST      Get A(next KCDEFN entry)                     
         JNZ   VALKLV04                                                         
         LARL  R3,KCDEFN           Point to first entry if first time           
         LHI   R0,2                                                             
         STCM  R0,3,KEYDISP        Set initial field displacement               
                                                                                
         USING KCDEFND,R3          R3=A(KCDEFN entry)                           
VALKLV04 MVI   KCDEFTYP,KCDEFTLV   Set literal value type                       
         MVC   KCDEFDSP,KEYDISP    Set displacement to output value             
         STC   R4,KCDEFLEN         Set length of literal                        
         STCM  R2,15,KCDEFLIT      Set A(literal value)                         
         SR    RF,RF                                                            
         ICM   RF,3,KEYDISP                                                     
         AR    RF,R4                                                            
         STCM  RF,3,KEYDISP        Set displacement to next value               
         AHI   R3,KCDEFNL                                                       
         ST    R3,AKCDLAST         Set A(next KCDEFN entry)                     
         J     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* Validate REPORT=Report-ID(/User-ID)                                 *         
***********************************************************************         
                                                                                
VALPQR   NTR1  LABEL=NO                                                         
         MVC   REPID,0(R1)                                                      
         LA    RE,REPID                                                         
         LHI   R0,L'REPID                                                       
VALPQR02 CLI   0(RE),C' '                                                       
         JNE   *+8                                                              
         MVI   0(RE),C'.'                                                       
         AHI   RE,1                                                             
         JCT   R0,VALPQR02                                                      
         CLI   L'REPID(R1),C'/'    Test user-id override                        
         JNE   EXITY                                                            
                                                                                
         USING CTIREC,IO                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,L'REPID+1(R1)                                             
         GOTOR VDMGR,DMCB,DMREAD,CTFILE,CTIREC,CTIREC                           
         JNE   EXITN                                                            
         LA    R1,CTIDATA                                                       
         USING CTDSCD,R1                                                        
         SR    R0,R0                                                            
VALPQR04 CLI   CTDSCEL,0           Test end of record                           
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CTDSCEL,CTDSCELQ                                                 
         JE    *+14                                                             
         IC    R0,CTDSCLEN                                                      
         AR    R1,R0                                                            
         J     VALPQR04                                                         
         MVC   REPUSER,CTDSC       Set override user-id                         
         J     EXITY                                                            
         DROP  R1                                                               
                                                                                
***********************************************************************         
* Validate REPDESC=Report description                                 *         
***********************************************************************         
                                                                                
VALPQD   MVC   REPDESC,0(R1)                                                    
         BR    RE                                                               
                                                                                
***********************************************************************         
* Validate REPTITLE=Report title                                      *         
***********************************************************************         
                                                                                
VALPQT   MVC   REPTITLE,0(R1)                                                   
         BR    RE                                                               
                                                                                
***********************************************************************         
* Validate REPCLASS=Value                                             *         
***********************************************************************         
                                                                                
VALPQC   MVC   REPCLASS,0(R1)                                                   
         BR    RE                                                               
                                                                                
***********************************************************************         
* Validate REPREF=Value                                               *         
***********************************************************************         
                                                                                
VALPRF   MVC   REPREF,0(R1)                                                     
         BR    RE                                                               
                                                                                
***********************************************************************         
* Validate RETAIN=Value                                               *         
***********************************************************************         
                                                                                
VALRTN   MVC   RETAIN,0(R1)                                                     
         BR    RE                                                               
                                                                                
***********************************************************************         
* Validate TRACE=Value                                                *         
***********************************************************************         
                                                                                
VALTRC   MVC   TRACE,0(R1)                                                      
         BR    RE                                                               
                                                                                
***********************************************************************         
* Validate DELIMS=Override delimiters                                 *         
***********************************************************************         
                                                                                
VALDEL   MVC   DELIMS,0(R1)                                                     
         BR    RE                                                               
                                                                                
***********************************************************************         
* Validate OMODE=value                                                *         
***********************************************************************         
                                                                                
VALOMO   CLI   DI_OMODE,0                                                       
         BNER  RE                                                               
         CLI   0(R1),C'U'          Test OMODE=U(pdate)                          
         BNER  RE                                                               
         OI    DI_OMODE,DI_OMUPD   Yes - set global indicator                   
         CR    RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Validate REPWIDTH=Value (1 to L'P-1)                                *         
***********************************************************************         
                                                                                
VALRWD   NTR1  LABEL=NO                                                         
         LR    RF,R1                                                            
         SR    R0,R0                                                            
VALRWD02 CLI   0(RF),SPACE                                                      
         JE    VALRWD04                                                         
         TM    0(RF),X'F0'                                                      
         JNO   EXITN                                                            
         AHI   RF,1                                                             
         AHI   R0,1                                                             
         CHI   R0,3                                                             
         JH    EXITN                                                            
         J     VALRWD02                                                         
VALRWD04 LTR   RF,R0                                                            
         JZ    EXITN                                                            
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         PACK  DUB,0(0,R1)                                                      
         EX    RF,0(RE)                                                         
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         JZ    EXITN                                                            
         CHI   R0,L'P-1                                                         
         JH    EXITN                                                            
         STC   R0,REPWIDTH                                                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Get input records and put to sort                                   *         
***********************************************************************         
                                                                                
GETINP   NTR1  BASE=*,LABEL=*                                                   
         L     R2,ATAPEIN                                                       
         OPEN  ((R2),INPUT)        Open input tape                              
                                                                                
         LHI   R0,DS_KEY-DS_D      Initialize sort                              
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  SORTKDSP,DUB                                                     
         LHI   R0,DS_KLNQ                                                       
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  SORTKLEN,DUB                                                     
         LHI   R0,L'DS_WORK+L'IO                                                
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  SORTRLEN,DUB                                                     
         GOTOR VSORTER,DMCB,SORTPAR1,SORTPAR2                                   
                                                                                
GETINP02 GOTOR GETREC,DI_AIO1      Get add/copy record                          
         JNE   GETINPX                                                          
         L     R2,DI_ADFD                                                       
         USING DF_D,R2             R2=A(file definition entry)                  
         CLI   RRECTY,RRECADDQ     Test add                                     
         JE    GETINP04                                                         
         CLI   RRECTY,RRECCPYQ     Test copy                                    
         JNE   GETINP02            (ignore singleton changes)                   
         GOTOR GETREC,DI_AIO2      Get change record                            
         CLI   RRECTY,RRECCHGQ     Test change                                  
         JNE   GETINP02                                                         
         MVI   RRECTY,RRECCPYQ     Reset to copy                                
         C     R2,DI_ADFD          Copy/change must be for same file            
         JNE   GETINP02                                                         
         TM    DF_TYPE,DF_TDA+DF_TIS                                            
         JZ    GETINP04                                                         
         SR    R1,R1                                                            
         ICM   R1,1,DF_KEYLN                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                R1=key length-1                              
         L     R3,AIO1DATA         Point to copy record key                     
         L     R4,AIO2DATA         Point to change record key                   
         BASR  RE,0                                                             
         CLC   0(0,R3),0(R4)                                                    
         EX    R1,0(RE)            Test copy/change keys match                  
         JNE   GETINP02                                                         
                                                                                
GETINP04 ICM   R3,15,DF_AREC                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         USING DR_D,R3             R3=A(record definition entry)                
         LHI   R5,1                R5=record index number                       
                                                                                
GETINP06 CLI   DR_D,DR_EOTQ        Test end of table                            
         JE    GETINP02                                                         
         LLC   RF,DR_LEN                                                        
         LA    RF,DR_D(RF)                                                      
         ST    RF,ARECNEXT         Set A(next record table entry)               
                                                                                
         LA    R4,DR_ARGS                                                       
         USING DR_ARGS,R4          R4=A(record argument list)                   
         CR    R4,RF               Must be at least one argument                
         JL    GETINP08                                                         
         DC    H'0'                                                             
                                                                                
GETINP08 C     R4,ARECNEXT         Test end of argument table                   
         JNE   GETINP16                                                         
                                                                                
         ST    R3,DI_ADRD          Save A(record entry)                         
         MVC   DI_AREC,AIO1DATA    Set actual record address                    
                                                                                
         ICM   RF,15,DR_AFLT       Test record filter routine present           
         JZ    GETINP44                                                         
         MVI   DI_MODE,0           Set mode for filter routine                  
         LARL  RE,GETINP10         (also called when DR_IMCRQ is set)           
         NTR1  LABEL=NO                                                         
         LA    R1,DI_D                                                          
         LM    R2,RB,DI_R2RB                                                    
         GOTOR (RF),(R1)           Call handler record filter routine           
         J     EXIT                                                             
                                                                                
GETINP10 JNE   GETINP42            Record rejected                              
         J     GETINP44            Record accepted                              
                                                                                
GETINP16 JNH   *+6                                                              
         DC    H'0'                Bad record table entry                       
         LLC   RF,DR_ADSP                                                       
         A     RF,AIO1DATA         Point to key value                           
         SR    R1,R1                                                            
         ICM   R1,1,DR_ARGL                                                     
         CLI   DR_ATYPE,DR_ATEQU                                                
         JL    GETINP18                                                         
         LLC   R1,DR_ALEN                                                       
         SHI   R1,DR_LNAQ                                                       
         JP    GETINP18                                                         
         DC    H'0'                Argument entry too short                     
                                                                                
GETINP18 LTR   R1,R1                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                R1=length of argument-1                      
                                                                                
         CLI   DR_ATYPE,DR_ATBZ    Go to argument handler                       
         JE    GETINP20                                                         
         CLI   DR_ATYPE,DR_ATNBZ                                                
         JE    GETINP22                                                         
         CLI   DR_ATYPE,DR_ATSP                                                 
         JE    GETINP24                                                         
         CLI   DR_ATYPE,DR_ATNSP                                                
         JE    GETINP26                                                         
         CLI   DR_ATYPE,DR_ATEQU                                                
         JE    GETINP28                                                         
         CLI   DR_ATYPE,DR_ATNEQ                                                
         JE    GETINP30                                                         
         CLI   DR_ATYPE,DR_ATGRQ                                                
         JE    GETINP32                                                         
         CLI   DR_ATYPE,DR_ATLEQ                                                
         JE    GETINP34                                                         
         DC    H'0'                                                             
                                                                                
GETINP20 BASR  RE,0                Equal to binary zeroes                       
         OC    0(0,RF),0(RF)                                                    
         EX    R1,0(RE)                                                         
         JZ    GETINP40                                                         
         J     GETINP42                                                         
                                                                                
GETINP22 BASR  RE,0                Not equal to binary zeroes                   
         OC    0(0,RF),0(RF)                                                    
         EX    R1,0(RE)                                                         
         JNZ   GETINP40                                                         
         J     GETINP42                                                         
                                                                                
GETINP24 BASR  RE,0                Equal to spaces                              
         CLC   0(0,RF),SPACES                                                   
         EX    R1,0(RE)                                                         
         JE    GETINP40                                                         
         J     GETINP42                                                         
                                                                                
GETINP26 BASR  RE,0                Not equal to spaces                          
         CLC   0(0,RF),SPACES                                                   
         EX    R1,0(RE)                                                         
         JNE   GETINP40                                                         
         J     GETINP42                                                         
                                                                                
GETINP28 BASR  RE,0                Equal to argument                            
         CLC   0(0,RF),DR_AVAL                                                  
         EX    R1,0(RE)                                                         
         JE    GETINP40                                                         
         J     GETINP42                                                         
                                                                                
GETINP30 BASR  RE,0                Not equal to argument                        
         CLC   0(0,RF),DR_AVAL                                                  
         EX    R1,0(RE)                                                         
         JNE   GETINP40                                                         
         J     GETINP42                                                         
                                                                                
GETINP32 BASR  RE,0                Greater than argument                        
         CLC   0(0,RF),DR_AVAL                                                  
         EX    R1,0(RE)                                                         
         JH    GETINP40                                                         
         J     GETINP42                                                         
                                                                                
GETINP34 BASR  RE,0                Less than argument                           
         CLC   0(0,RF),DR_AVAL                                                  
         EX    R1,0(RE)                                                         
         JL    GETINP40                                                         
         J     GETINP42                                                         
                                                                                
GETINP40 IC    R0,DR_ALEN          Bump to next argument                        
         AR    R4,R0                                                            
         J     GETINP08                                                         
                                                                                
GETINP42 L     R3,ARECNEXT         Bump to next record                          
         AHI   R5,1                Bump record index number                     
         J     GETINP06                                                         
         DROP  R4                                                               
                                                                                
GETINP44 ICM   RF,15,AFIFNTRY      Test any record filters                      
         JZ    GETINP54                                                         
         USING FILFD,RF            RF=A(file filter table entry)                
         LLC   R0,FILFLEN                                                       
         SHI   R0,FILFLNQ          R0=number of record filters                  
         LA    RF,FILFRECS                                                      
         DROP  RF                                                               
GETINP46 CLM   R5,1,0(RF)          Match record index numbers                   
         JE    GETINP54                                                         
         AHI   RF,L'FILFRECS       Bump to next table entry                     
         JCT   R0,GETINP46         Do for number of record filters              
         J     GETINP02            Drop record if not in filter list            
                                                                                
GETINP54 XC    AGYVALS(AGYVALL),AGYVALS                                         
         OC    DI_AGY,DI_AGY       Test DR_AFLT supplied DI_AGY                 
         JZ    GETINP55                                                         
         MVC   AGYALPHA,DI_AGY     Yes - use it                                 
         XC    DI_AGY,DI_AGY       and clear it                                 
         J     GETINP58                                                         
                                                                                
GETINP55 LLC   R4,DR_AGYD          Extract agency id from record                
         A     R4,AIO1DATA         R1=A(agency id in key)                       
         TM    DR_ITYPE,DR_ITALF   Test agency alpha in key                     
         JZ    *+14                                                             
         MVC   AGYALPHA,0(R4)      Set agency alpha id                          
         J     GETINP56                                                         
         MVC   AGYBINRY,0(R4)                                                   
         TM    DR_ITYPE,DR_ITAGY   Test agency number in key                    
         JNZ   GETINP56                                                         
         NI    AGYBINRY,DA_ABITQ   Set agency number                            
         TM    DR_ITYPE,DR_ITAGM                                                
         JNZ   GETINP56                                                         
         DC    H'0'                                                             
                                                                                
GETINP56 TM    IND,SYSTIXAQ+SYSTIAAQ                                            
         JNZ   GETINP58                                                         
         OC    AGYVALS(AGYVALL),AGYVALS                                         
         JNZ   GETINP58                                                         
         DC    H'0'                                                             
                                                                                
GETINP58 GOTOR SETAGY              Set agency values                            
         DROP  R3                                                               
                                                                                
         CLI   AGYFILTS,0          Test any agency filters                      
         JE    GETINP70                                                         
         LA    R1,AGYFILTS         Apply agency filter                          
         LHI   R0,AGYFILTM                                                      
GETINP66 CLI   0(R1),0             Test end of list                             
         JE    GETINP02                                                         
         CLC   AGYALPHA,0(R1)      Match agency alpha to filter list            
         JE    GETINP70                                                         
         AHI   R1,L'AGYALPHA                                                    
         JCT   R0,GETINP66                                                      
         J     GETINP02                                                         
                                                                                
         USING DS_D,DS_WORK        Build and put sort record(s)                 
GETINP70 LA    R0,DS_D                                                          
         ST    R0,DI_ADSD          Set A(sort header) for handler               
         L     R3,DI_AIO1          Point to copy/add record                     
GETINP72 XC    DS_D(DS_REC-DS_D),DS_D                                           
         MVC   DS_ALF,AGYALPHA     Set agency alpha id                          
         MVC   DS_RFIL,RFILTY      Set file number                              
                                                                                
         SR    R1,R1               Set record key                               
         ICM   R1,1,DF_KEYLN                                                    
         JNZ   *+8                                                              
         LHI   R1,L'DS_KEY         Use maximum length if no key                 
         BCTR  R1,0                                                             
         LA    RF,4+L'RECVHDR(R3)                                               
         BASR  RE,0                                                             
         MVC   DS_RKEY(0),0(RF)                                                 
         EX    R1,0(RE)                                                         
                                                                                
         MVC   DS_DATE,RDATE       Set date                                     
         MVC   DS_TIME,RTIME       Set time                                     
         AP    DS_TIME,ADJTIME     Adjust to actual time                        
         CP    DS_TIME,MAXHOUR     Deal with midnight                           
         JNH   GETINP74                                                         
         SP    DS_TIME,MAXHOUR     Adjust time and day                          
         GOTOR VDATCON,DMCB,(3,DS_DATE),(0,WORK)                                
         GOTOR VADDAY,(R1),WORK,WORK+6,1                                        
         GOTOR VDATCON,(R1),(0,WORK+6),(3,DS_DATE)                              
                                                                                
GETINP74 OC    STRDATEB,STRDATEB   Test recovery date range given               
         JZ    GETINP76                                                         
         CLC   DS_DATE,STRDATEB    Yes - apply date filters                     
         JL    GETINP02                                                         
         CLC   DS_DATE,ENDDATEB                                                 
         JH    GETINP02                                                         
                                                                                
GETINP76 ICM   R0,15,SORTUSEQ      Bump/set unique sequence number              
         AHI   R0,1                                                             
         STCM  R0,15,SORTUSEQ                                                   
         MVC   DS_USEQ,SORTUSEQ    (will be the same for copy/change)           
                                                                                
         CLI   RRECTY,RRECCPYQ     Set record type                              
         JNE   *+12                                                             
         MVI   DS_TYPE,DS_TCPYQ    Copy record                                  
         J     GETINP78                                                         
         CLI   RRECTY,RRECCHGQ                                                  
         JNE   *+12                                                             
         MVI   DS_TYPE,DS_TCHGQ    Change record                                
         J     GETINP78                                                         
         CLI   RRECTY,RRECADDQ                                                  
         JNE   *+12                                                             
         MVI   DS_TYPE,DS_TADDQ    Add record                                   
         J     GETINP78                                                         
         DC    H'0'                                                             
                                                                                
GETINP78 MVC   DS_AGB,AGYBINRY     Set agency binary value                      
         MVC   DS_UID,RUSER        Set user id                                  
         MVC   DS_SECAG,RAGYSEC    Set security agency                          
         MVC   DS_PID,RPERSON      Set person                                   
         MVC   DS_PGM,RPRG         Set program                                  
         MVC   DS_CSTAT,RCTSTAT    Set connect status                           
         MVC   DS_LUID,RLUID       Set LUID/ticket                              
         MVC   DS_ADF_D,DI_ADFD    Set A(file table entry)                      
         MVC   DS_ADR_D,DI_ADRD    Set A(record table entry)                    
                                                                                
GETINP80 SR    R1,R1               Move recovery record to sort record          
         ICM   R1,3,0(R3)                                                       
         SHI   R1,4+L'RECVHDR      R1=L'data on recovery record                 
         AHI   R1,DS_LNQ           R1=sort record length                        
         STCM  R1,3,DS_LEN                                                      
         SHI   R1,DS_LNQ                                                        
         LA    R0,DS_REC                                                        
         LA    RE,4+L'RECVHDR(R3)                                               
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     RF,DI_ADRD                                                       
         USING DR_D,RF                                                          
         TM    DR_INDS1,DR_IMCRQ   Test create copy/change from add             
         JZ    GETINP84                                                         
         CLI   DS_TYPE,DS_TADDQ    Test adding a record                         
         JNE   GETINP84                                                         
         ICM   RF,15,DR_AFLT       Point to filter routine                      
         JNZ   *+6                                                              
         DC    H'0'                                                             
         DROP  RF                                                               
                                                                                
         MVI   DI_MODE,DI_MMCRQ    Set mode for filter routine                  
         LARL  RE,GETINP82                                                      
         NTR1  LABEL=NO                                                         
         LA    R1,DI_D                                                          
         LM    R2,RB,DI_R2RB                                                    
         GOTOR (RF),(R1)           Call handler record filter routine           
         J     EXIT                                                             
                                                                                
GETINP82 CLI   4+RRECTY-RECVHDR(R3),RRECCPYQ Test add changed to copy           
         JNE   GETINP84                                                         
         MVI   DS_TYPE,DS_TCPYQ    Yes - reset action to copy and move          
         SR    R1,R1               new recovery rec to sort rec again           
         ICM   R1,3,0(R3)                                                       
         SHI   R1,4+L'RECVHDR      R1=L'data on recovery record                 
         AHI   R1,DS_LNQ           R1=sort record length                        
         STCM  R1,3,DS_LEN                                                      
         SHI   R1,DS_LNQ                                                        
         LA    R0,DS_REC                                                        
         LA    RE,4+L'RECVHDR(R3)                                               
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
GETINP84 GOTOR VSORTER,DMCB,SORTPUT,DS_D                                        
         AP    SORTPUTS,PONE                                                    
                                                                                
         CLI   4+(RRECTY-RECVHDR)(R3),RRECCPYQ Test copy/change pair            
         JNE   GETINP02            Next record if not a copy                    
         MVI   DS_TYPE,DS_TCHGQ    Set to change                                
         L     R3,DI_AIO2          Point to change record                       
         J     GETINP80            Put change record                            
                                                                                
GETINPX  L     R2,ATAPEIN          Close input file                             
         CLOSE ((R2))                                                           
         J     EXIT                                                             
         DROP  R2,RB                                                            
         EJECT                                                                  
***********************************************************************         
* Get a record from input recovery file and extract recovery header   *         
* and trailer                                                         *         
*                                                                     *         
* Pointer copies/changes and recovery records for files not defined   *         
* in system file table are dropped as are any files not defined in    *         
* the file filter table (if any are present)                          *         
*                                                                     *         
* Ntry:- R1=A(A(I/O)) area                                            *         
*                                                                     *         
* Exit:- CC=equal if record read, not equal if end of input file      *         
***********************************************************************         
                                                                                
GETREC   NTR1  LABEL=NO                                                         
         LR    R4,R1               R4=A(A(I/O)) area                            
                                                                                
GETREC02 L     R2,ATAPEIN                                                       
         L     R3,0(R4)            R3=A(I/O area)                               
         GET   (R2),(R3)           Get next input record                        
                                                                                
         SR    RE,RE                                                            
         ICM   RE,3,0(R3)          RE=record length                             
         LR    RF,RE                                                            
         AR    RF,R3                                                            
         XC    0(L'DS_RKEY,RF),0(RF)                                            
         BCTR  RF,0                RF=A(extension length) if present            
         XC    RECHD,RECHD                                                      
         MVC   RECVHDR,4(R3)       Extract recovery header                      
                                                                                
         TM    RRECTY,RRECPTRQ     Ignore pointer copies/changes                
         JNZ   GETREC02                                                         
         CLI   RSIN,X'FF'          Ignore deleted records                       
         JE    GETREC02                                                         
                                                                                
         TM    RTIME,X'40'         Test trailer exists                          
         JZ    GETREC04                                                         
         NI    RTIME,FF-X'40'                                                   
         LLC   R1,0(RF)            R0=length of extension                       
         SR    RF,R1                                                            
         AHI   RF,1                RF=A(recovery trailer)                       
         MVC   RECVEXT(RXLENQ),0(RF)                                            
         SR    RE,R1               Subtract length of trailer                   
         STCM  RE,3,0(R3)          Set adjusted record length                   
         BASR  RE,0                                                             
         XC    0(0,RF),0(RF)                                                    
         EX    R1,0(RE)            Clear trailer in record                      
                                                                                
GETREC04 TM    RTIME,X'80'         Test new style time format                   
         JZ    GETREC06                                                         
         NI    RTIME,FF-X'80'      Sets new style time to old                   
         MVI   RTIME+3,X'C0'       Style time for output                        
         ICM   R0,15,RTIME                                                      
         SRL   R0,4                                                             
         STCM  R0,15,RTIME                                                      
                                                                                
GETREC06 ICM   R2,15,DH_AFIL       Point to system file list                    
         JNZ   GETREC08                                                         
         DC    H'0'                                                             
                                                                                
         USING DF_D,R2             R2=A(system file table)                      
GETREC08 CLI   DF_D,DF_EOTQ        Test end of table                            
         JE    GETREC02                                                         
         CLC   DF_NUM,RFILTY       Match file number                            
         JE    *+12                                                             
         AHI   R2,DF_LNQ           Bump to next table entry                     
         J     GETREC08                                                         
                                                                                
         XC    AFIFNTRY,AFIFNTRY   Clear A(file filter entry)                   
         LARL  RF,FILFILT          Point to file filters                        
         USING FILFD,RF            RF=A(file filter table)                      
         CLI   FILFD,FILFEOTQ      Test any file filters present                
         JE    GETREC14                                                         
         SR    R0,R0                                                            
                                                                                
GETREC10 CLI   FILFD,FILFEOTQ      Test end of filter table                     
         JE    GETREC02                                                         
         CLC   FILFFNUM,DF_NUM     Match file number                            
         JE    GETREC12                                                         
         IC    R0,FILFLEN          Bump to next filter entry                    
         AR    RF,R0                                                            
         J     GETREC10                                                         
                                                                                
GETREC12 CLI   FILFLEN,FILFLNQ     Test any record filters                      
         JE    GETREC14                                                         
         ST    RF,AFIFNTRY         Yes - save A(file filter entry)              
                                                                                
GETREC14 ST    R2,DI_ADFD          Save A(file table entry)                     
         DROP  R2,RF                                                            
                                                                                
GETRECX  J     EXITY                                                            
                                                                                
GETEOF   J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* Get records from sort and call output routine for each field that   *         
* has changed                                                         *         
***********************************************************************         
                                                                                
PUTOUT   NTR1  LABEL=NO                                                         
                                                                                
PUTOUT02 GOTOR VSORTER,DMCB,SORTGET                                             
         ICM   RE,15,4(R1)         Get/test A(sort record)                      
         JZ    PUTOUT60                                                         
                                                                                
         MVC   DS_WORK,0(RE)       Extract header into save area                
                                                                                
         MVC   DI_ADFD,DS_ADF_D    Set A(file table entry)                      
         L     RF,DI_ADFD          Extract file name                            
         MVC   FILNAME,DF_NAMEF-DF_D(RF)                                        
         MVC   DI_ADRD,DS_ADR_D    Set A(record table entry)                    
         L     RF,DI_ADRD          Extract record name                          
         MVC   RECNAME,DR_NAMES-DR_D(RF)                                        
                                                                                
         MVC   TICKET,SPACES       Extract ticket or LUID                       
         MVC   LUID,SPACES                                                      
         LA    RF,LUID                                                          
         TM    DS_CSTAT,RCTSTTKT                                                
         JZ    *+8                                                              
         LA    RF,TICKET                                                        
         MVC   0(L'DS_LUID,RF),DS_LUID                                          
                                                                                
         CLI   DS_TYPE,DS_TADDQ    Must be add...                               
         JE    *+8                                                              
         CLI   DS_TYPE,DS_TCPYQ    ...or copy                                   
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R0,DI_AIO1          Move add/copy record to IO1                  
         SR    R1,R1                                                            
         ICM   R1,3,DS_LEN                                                      
         SHI   R1,DS_LNQ                                                        
         STH   R1,HALF1                                                         
         AHI   RE,DS_LNQ                                                        
         LR    RF,R1                                                            
         AHI   R1,1                Pad X'00' at end of record                   
         MVCL  R0,RE                                                            
         CLI   DS_TYPE,DS_TCPYQ    Test this is a copy                          
         JNE   PUTOUT08                                                         
                                                                                
         GOTOR VSORTER,DMCB,SORTGET                                             
         ICM   RE,15,4(R1)         Get/test A(change record)                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   DS_KEY(DS_TYPE-DS_KEY),DS_KEY-DS_D(RE)                           
         JE    *+6                                                              
         DC    H'0'                                                             
         CLI   DS_TYPE-DS_D(RE),DS_TCHGQ                                        
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R0,DI_AIO2          Move change record to IO2                    
         SR    R1,R1                                                            
         ICM   R1,3,DS_LEN-DS_D(RE)                                             
         SHI   R1,DS_LNQ                                                        
         STH   R1,HALF2            Save length of change record                 
         AHI   RE,DS_LNQ                                                        
         LR    RF,R1                                                            
         AHI   R1,1                Pad X'00' at end of record                   
         MVCL  R0,RE                                                            
                                                                                
         L     R1,DI_ADFD          Test delete/restore                          
         TM    DF_TYPE-DF_D(R1),DF_TIS+DF_TDA                                   
         JZ    PUTOUT06                                                         
         LLC   R0,DF_KEYLN-DF_D(R1)                                             
         AHI   R0,2                Adjust for record length                     
         L     RE,DI_AIO1                                                       
         AR    RE,R0               RE=A(copy record status byte)                
         L     RF,DI_AIO2                                                       
         AR    RF,R0               RF=A(change record status byte)              
                                                                                
         TM    0(RE),X'80'         Test copy record is deleted                  
         JZ    PUTOUT04                                                         
         TM    0(RF),X'80'         Test change record is deleted too            
         JNZ   PUTOUT02            Yes - ignore                                 
         MVI   DS_TYPE,DS_TRESQ    Set type to restore                          
         L     R0,DI_AIO1                                                       
         LH    R1,HALF2            R1=L'record (saved above)                    
         L     RE,DI_AIO2          Report on data from change record            
         LR    RF,R1                                                            
         AHI   R1,1                Pad X'00' at end of record                   
         MVCL  R0,RE               Move change record over copy                 
         J     PUTOUT08                                                         
                                                                                
PUTOUT04 TM    0(RF),X'80'         Test change record is deleted                
         JZ    PUTOUT06                                                         
         MVI   DS_TYPE,DS_TDELQ    Set type to delete                           
         J     PUTOUT08                                                         
                                                                                
PUTOUT06 CLI   DS_TYPE,DS_TCPYQ    Test copy/change pair                        
         JNE   PUTOUT08                                                         
         CLC   HALF1,HALF2         Test record length changed                   
         JNE   PUTOUT08                                                         
         L     R0,DI_AIO1                                                       
         LH    R1,HALF1                                                         
         L     RE,DI_AIO2                                                       
         LR    RF,R1                                                            
         CLCL  R0,RE               Test record changed at all                   
         JE    PUTOUT02            No - ignore                                  
                                                                                
PUTOUT08 L     R1,DI_ADRD          Set agency(/media)                           
         USING DR_D,R1             R1=A(record table entry)                     
         MVI   DI_RAGY,0                                                        
         TM    DR_ITYPE,DR_ITAGY+DR_ITAGM                                       
         JZ    PUTOUT10                                                         
         LLC   RF,DR_AGYD                                                       
         A     RF,DI_AIO1          RF=A(agency(/media)) in key                  
         MVC   DI_RAGY,0(RF)                                                    
         DROP  R1                                                               
                                                                                
PUTOUT10 L     RF,DI_AAGY                                                       
         USING DA_D,RF             RF=A(agency table)                           
PUTOUT12 CLI   DA_D,DA_EOTQ                                                     
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   DA_ALF,DS_ALF       Match agency alpha                           
         JE    *+12                                                             
         AHI   RF,DA_LNQ                                                        
         J     PUTOUT12                                                         
         ST    RF,DI_ADAD          Save A(agency table entry)                   
         DROP  RF                                                               
                                                                                
         CLC   DS_ALF,LALF         Test change of agency alpha                  
         JE    PUTOUT14                                                         
         GOTOR CALLSH,DI_MAGFQ     Call system handler to initialize            
                                                                                
PUTOUT14 GOTOR EDTKEY              Edit record key                              
         JNE   PUTOUT02            Ignore if record rejected                    
                                                                                
         NI    OUTFLAG,OUTFCALL    Initialize flag byte                         
                                                                                
         L     R2,DI_ADRD                                                       
         USING DR_D,R2             R2=A(record definition)                      
         TM    DR_INDS1,DR_IDFRQ   Test handler formats data                    
         JZ    PUTOUT16                                                         
         LARL  RE,PUTOUT02                                                      
         NTR1  LABEL=NO                                                         
         ICM   RF,15,DR_ADAT       RF=A(handler data edit routine)              
         JNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R1,DI_D             R1=A(interface block)                        
         LM    R2,RB,DI_R2RB       Restore handler's R2 thru RB                 
         GOTOR (RF),(R1)           Handler will call output                     
         J     EXIT                                                             
                                                                                
PUTOUT16 ICM   R2,15,DR_ADAT                                                    
         JZ    PUTOUT02                                                         
                                                                                
         USING DD_D,R2             R2=A(data definition table)                  
PUTOUT18 CLI   DD_D,DD_EOTQ        Test end of data definition                  
         JE    PUTOUT50                                                         
                                                                                
         CLI   DD_CTRYF,0          Test country filter                          
         JE    PUTOUT20                                                         
         L     RF,DI_ADAD                                                       
         CLC   DD_CTRYF,DA_CTRY-DA_D(RF)                                        
         JNE   PUTOUT56                                                         
                                                                                
PUTOUT20 LA    R0,DD_D                                                          
         ST    R0,DI_ADDD          Set A(current data entry)                    
         MVC   DI_FLDN,DD_NAME     Extract field name                           
                                                                                
         TM    DD_INDS2,DD_IFMFQ   Test start of multi field                    
         JZ    PUTOUT22                                                         
         TM    PUTFLAG,PUTFMFMQ    Test multi field mode on                     
         JZ    *+6                                                              
         DC    H'0'                                                             
         OI    PUTFLAG,PUTFMFMQ    Set multi field mode on                      
                                                                                
         MVC   JOINNAME,DI_FLDN    Initialize multi fields                      
         MVC   JOINDLIM,DD_DELIM                                                
         MVC   JOINOVAL,SPACES                                                  
         MVI   JOINOLEN,0                                                       
         MVC   JOINNVAL,SPACES                                                  
         MVI   JOINNLEN,0                                                       
                                                                                
PUTOUT22 CLI   DD_TYPE,DD_TSHRQ    Test system handler routine                  
         JNE   PUTOUT24                                                         
         MVC   DI_OVAL,SPACES                                                   
         MVC   DI_NVAL,SPACES                                                   
         LARL  RE,PUTOUT56                                                      
         NTR1  LABEL=NO                                                         
         ICM   RF,15,DD_AROUT      RF=A(system handler routine)                 
         JNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R1,DI_D             R1=A(interface block)                        
         LM    R2,RB,DI_R2RB       Restore handler's R2 thru RB                 
         GOTOR (RF),(R1)           (handler will call output itself)            
         J     EXIT                                                             
                                                                                
PUTOUT24 CLI   DD_LOCN,DD_LDIMQ    Test multi element                           
         JE    PUTOUT32                                                         
                                                                                
         GOTOR GETFLD,GETFOLD      Get old field value                          
         GOTOR GETFLD,GETFNEW      Get new field value                          
                                                                                
         TM    PUTFLAG,PUTFMFMQ    Test multi field mode                        
         JZ    PUTOUT30                                                         
                                                                                
         CLI   DD_TYPE,DD_TMSKQ    Test bit mask                                
         JNE   PUTOUT26                                                         
         CLC   DI_OVAL,DI_NVAL     Test old value=new value                     
         JE    PUTOUT28                                                         
         GOTOR EDTMSK              Edit mask bits                               
         JE    PUTOUT28                                                         
                                                                                
PUTOUT26 CLC   DI_OVAL,DI_NVAL     Test no change to field                      
         JE    PUTOUT28                                                         
         GOTOR PREFIX,DI_OVAL      Prefix and join old field value              
         GOTOR JOINIT,DMCB,DI_OVAL,JOINOLEN                                     
         GOTOR PREFIX,DI_NVAL      Prefix and join new field value              
         GOTOR JOINIT,DMCB,DI_NVAL,JOINNLEN                                     
                                                                                
PUTOUT28 TM    DD_INDS2,DD_ILMFQ   Test last multi field                        
         JZ    PUTOUT56                                                         
         TM    PUTFLAG,PUTFMFMQ    Test multi field mode on                     
         JNZ   *+6                                                              
         DC    H'0'                No - bad table                               
         NI    PUTFLAG,FF-PUTFMFMQ Turn multi field mode off                    
         MVC   DI_FLDN,JOINNAME                                                 
         MVC   DI_OVAL,JOINOVAL                                                 
         MVC   DI_NVAL,JOINNVAL                                                 
                                                                                
PUTOUT30 CLC   DI_OVAL,DI_NVAL     Test old value=new value                     
         JE    PUTOUT56                                                         
         TM    DD_INDS2,DD_ILMFQ   Test last multi field                        
         JNZ   PUTOUT54                                                         
                                                                                
         CLI   DD_TYPE,DD_TMSKQ    Test bit mask                                
         JNE   *+12                                                             
         GOTOR EDTMSK              Edit mask bits                               
         JE    PUTOUT56                                                         
                                                                                
         GOTOR PREFIX,DI_OVAL      Prefix old field value                       
         GOTOR PREFIX,DI_NVAL      Prefix new field value                       
         J     PUTOUT54                                                         
                                                                                
PUTOUT32 TM    PUTFLAG,PUTFMEMQ+PUTFMFMQ                                        
         JZ    *+6                                                              
         DC    H'0'                Can't be in special modes here               
                                                                                
         XC    ELTABN,ELTABN       Handle multi elements                        
         GOTOR GETELS,GETEOLD      Get old elements                             
         GOTOR GETELS,GETENEW      Get new elements                             
         OC    ELTABN,ELTABN       Test any elements found                      
         JZ    PUTOUT56            No                                           
                                                                                
         L     R3,AELTAB           R3=A(element table)                          
         LH    R4,ELTABN           R4=number of table entries                   
PUTOUT34 MVC   ELWORK(ELWORKL),0(R3)                                            
         ICM   R0,15,ELAOLD        Point to old element                         
         JZ    PUTOUT36                                                         
         ICM   RE,15,ELANEW        Point to new element                         
         JZ    PUTOUT36                                                         
         LLC   RF,1(RE)            RF=length of element                         
         LR    R1,RF                                                            
         CLCL  R0,RE               Compare old element to new                   
         JE    PUTOUT52            Element was not changed - ignore             
                                                                                
PUTOUT36 ICM   RE,15,ELAOLD        Point to element                             
         JNZ   *+8                                                              
         ICM   RE,15,ELANEW                                                     
         ST    RE,DI_AREL          Set element addresses for handler            
         MVC   DI_AOEL,ELAOLD                                                   
         MVC   DI_ANEL,ELANEW                                                   
                                                                                
         ICM   RF,15,DD_AFLT       Test element filter present                  
         JZ    PUTOUT40                                                         
         LARL  RE,PUTOUT38                                                      
         NTR1  LABEL=NO                                                         
         LA    R1,DI_D                                                          
         LM    R2,RB,DI_R2RB                                                    
         GOTOR (RF),(R1)           Call handler element filter routine          
         J     EXIT                                                             
                                                                                
PUTOUT38 JNE   PUTOUT52            Handler sets not equal to reject             
                                                                                
PUTOUT40 LARL  RE,PUTOUT42         Edit element key                             
         NTR1  LABEL=NO                                                         
         MVC   DI_EKEY,SPACES                                                   
         ICM   RF,15,DD_AKEY       RF=A(element key format routine)             
         JNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R1,DI_D                                                          
         LM    R2,RB,DI_R2RB                                                    
         GOTOR (RF),(R1)                                                        
         J     EXITY                                                            
                                                                                
PUTOUT42 ST    R2,ADDENTRY         Save current entry address                   
         OI    PUTFLAG,PUTFMEMQ                                                 
         ICM   R2,15,DD_ADAT       Point to element data table                  
         JNZ   PUTOUT18                                                         
         DC    H'0'                                                             
                                                                                
PUTOUT50 TM    PUTFLAG,PUTFMFMQ    Test multi field mode                        
         JZ    *+6                                                              
         DC    H'0'                                                             
         XC    DI_EKEY,DI_EKEY     Clear element key                            
         TM    PUTFLAG,PUTFMEMQ    Test multi element mode on                   
         JZ    PUTOUT02                                                         
         NI    PUTFLAG,FF-PUTFMEMQ                                              
         L     R2,ADDENTRY         Restore last entry address                   
                                                                                
PUTOUT52 AHI   R3,ELWORKL          Bump to next element table entry             
         JCT   R4,PUTOUT34         Do for number of entries                     
         J     PUTOUT56                                                         
                                                                                
PUTOUT54 GOTOR OUTPUT              Output data record                           
                                                                                
PUTOUT56 AHI   R2,DD_LNQ           Bump to next data definition entry           
         J     PUTOUT18                                                         
                                                                                
PUTOUT60 TM    DI_OMODE,DI_OMUPD   Test OMODE=U(pdate)                          
         JNZ   PUTOUTX             Yes - don't create null entries              
                                                                                
         MVI   OUTMODE,OUTMNULL    Set sending null report mode                 
                                                                                
         L     R3,DI_AAGY                                                       
         USING DA_D,R3             R3=A(agency table)                           
PUTOUT62 CLI   DA_D,DA_EOTQ        Test end of agency table                     
         JE    PUTOUTX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,AGYPUT#                                                     
         JZ    PUTOUT66                                                         
         LA    R1,AGYPUT           Test report sent for agency                  
PUTOUT64 CLC   DA_ALF,0(R1)                                                     
         JE    PUTOUT72                                                         
         AHI   R1,L'AGYPUT                                                      
         JCT   R0,PUTOUT64                                                      
                                                                                
PUTOUT66 CLI   AGYFILTS,0          Test any agency filters                      
         JE    PUTOUT70                                                         
         LA    R1,AGYFILTS         Test agency in filter list                   
         LHI   R0,AGYFILTM                                                      
PUTOUT68 OC    0(L'DA_ALF,R1),0(R1)                                             
         JZ    PUTOUT72                                                         
         CLC   DA_ALF,0(R1)        Agency is in filter list                     
         JE    PUTOUT70                                                         
         AHI   R1,L'DA_ALF                                                      
         JCT   R0,PUTOUT68                                                      
         J     PUTOUT72                                                         
*                                                                               
PUTOUT70 NI    OUTFLAG,OUTFCALL    Initialize flag byte                         
         MVC   DS_ALF,DA_ALF       Send null report                             
         MVC   DS_DATE,TODAYB      for today                                    
         OC    STRDATEB,STRDATEB   or start of period                           
         JZ    *+10                                                             
         MVC   DS_DATE,STRDATEB                                                 
         ST    R3,DI_ADAD                                                       
         GOTOR OUTPUT                                                           
                                                                                
PUTOUT72 AHI   R3,DA_LNQ           Bump to next agency table entry              
         J     PUTOUT62                                                         
                                                                                
PUTOUTX  MVI   OUTMODE,OUTMLAST                                                 
         GOTOR OUTPUT              Close current report                         
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Locate and edit a field from an input record or element into either *         
* DI_OVAL or DI_NVAL                                                  *         
*                                                                     *         
* Ntry:- R1=Calling mode (see below)                                  *         
*                                                                     *         
* Exit:- DI_OVAL or DI_NVAL cleared or set to printable field value.  *         
*        Mask fields (DD_TMSKQ) are not edited so that the resulting  *         
*        raw data fields can be compared and if different edited a    *         
*        bit at a time by the EDTMSK routine                          *         
***********************************************************************         
                                                                                
GETFOLD  EQU   1                   Get old field value                          
GETFNEW  EQU   2                   Get new field value                          
                                                                                
GETFLD   NTR1  LABEL=NO                                                         
                                                                                
         TM    PUTFLAG,PUTFMEMQ    Test multi element mode on                   
         JNZ   GETFLD30                                                         
                                                                                
         L     RE,DI_AIO1          Point to copy/add record                     
         CHI   R1,GETFOLD          Test get old data                            
         JNE   GETFLD02                                                         
         MVC   DI_OVAL,SPACES      Clear and point to old data                  
         CLI   DD_TYPE,DD_TMSKQ    Test bit mask                                
         JNE   *+10                                                             
         XC    DI_OVAL,DI_OVAL                                                  
         LA    RF,DI_OVAL                                                       
         CLI   DS_TYPE,DS_TCPYQ    Test copy/change                             
         JE    GETFLD04                                                         
         CLI   DS_TYPE,DS_TDELQ    Test delete                                  
         JE    GETFLD04                                                         
         J     EXIT                                                             
                                                                                
GETFLD02 MVC   DI_NVAL,SPACES      Clear and point to new data                  
         CLI   DD_TYPE,DD_TMSKQ    Test bit mask                                
         JNE   *+10                                                             
         XC    DI_NVAL,DI_NVAL                                                  
         LA    RF,DI_NVAL                                                       
         CLI   DS_TYPE,DS_TADDQ    Test add                                     
         JE    GETFLD04                                                         
         L     RE,DI_AIO2          Point to change record                       
         CLI   DS_TYPE,DS_TCPYQ    Test copy/change pair                        
         JE    GETFLD04                                                         
         CLI   DS_TYPE,DS_TRESQ    Test restore                                 
         JE    GETFLD04                                                         
         J     EXIT                                                             
                                                                                
GETFLD04 ST    RE,DI_AREC          Set A(input record)                          
         ST    RF,DI_AOUT          Set A(output field)                          
                                                                                
         CLI   DD_LOCN,DD_LDIRQ    Test displacement into record                
         JNE   GETFLD06                                                         
         CLI   DD_FLEN,0           Can't be zero length                         
         JNE   *+6                                                              
         DC    H'0'                                                             
         MVC   DI_LINP,DD_FLEN     Set input field length                       
         XC    DI_AREL,DI_AREL     Clear A(record element)                      
         SR    R0,R0                                                            
         ICM   R0,3,DD_RDSP        R0=displacement into record                  
         A     R0,DI_AREC                                                       
         ST    R0,DI_AINP          Set A(input field)                           
         J     GETFLD44                                                         
                                                                                
GETFLD06 CLI   DD_LOCN,DD_LDISQ    Test displacement into element               
         JNE   GETFLD20                                                         
         L     RF,DI_ADFD                                                       
         LLC   RE,DF_FSTEL-DF_D(RF)                                             
         L     RF,DI_ADRD                                                       
         CLI   DR_FSTEL-DR_D(RF),0                                              
         JE    *+8                                                              
         IC    RE,DR_FSTEL-DR_D(RF)                                             
         LTR   RE,RE                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         A     RE,DI_AREC          Re=A(first element on record)                
         SR    R0,R0                                                            
                                                                                
GETFLD08 CLI   0(RE),0             Test end of input record                     
         JE    EXIT                                                             
         CLC   DD_ELEM,0(RE)       Match element code to table                  
         JE    GETFLD40                                                         
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         J     GETFLD08                                                         
                                                                                
GETFLD20 DC    H'0'                                                             
                                                                                
GETFLD30 CHI   R1,GETFOLD          Multi element mode                           
         JNE   GETFLD32                                                         
         ICM   RE,15,ELAOLD        Point to old element                         
         MVC   DI_OVAL,SPACES                                                   
         CLI   DD_TYPE,DD_TMSKQ    Test bit mask                                
         JNE   *+10                                                             
         XC    DI_OVAL,DI_OVAL                                                  
         LA    RF,DI_OVAL          Point to old value                           
         J     GETFLD34                                                         
                                                                                
GETFLD32 ICM   RE,15,ELANEW        Point to new element                         
         MVC   DI_NVAL,SPACES                                                   
         CLI   DD_TYPE,DD_TMSKQ    Test bit mask                                
         JNE   *+10                                                             
         XC    DI_NVAL,DI_NVAL                                                  
         LA    RF,DI_NVAL          Point to new value                           
                                                                                
GETFLD34 LTR   RE,RE               Test element present                         
         JZ    EXIT                                                             
         ST    RF,DI_AOUT          Set A(output)                                
         CLI   DD_LOCN,DD_LDIEQ    Test displacement into element               
         JE    GETFLD40                                                         
         DC    H'0'                No - bad table                               
                                                                                
GETFLD40 ST    RE,DI_AREL          Set A(input record element)                  
         LLC   R0,DD_EDSP                                                       
         CHI   R0,2                                                             
         JNL   *+6                                                              
         DC    H'0'                                                             
         CLM   R0,1,1(RE)          Is data beyond end of element                
         JNL   EXIT                                                             
         AR    R0,RE                                                            
         ST    R0,DI_AINP                                                       
         MVC   DI_LINP,DD_FLEN                                                  
         CLI   DD_FLEN,0           Test variable length data                    
         JNE   GETFLD44                                                         
         LLC   R1,1(RE)                                                         
         AR    R1,RE                                                            
         SR    R1,R0                                                            
         STC   R1,DI_LINP          Set length of input                          
                                                                                
GETFLD44 CLI   DD_TYPE,DD_TSHEQ    Test system handler edit routine             
         JNE   GETFLD52                                                         
         LARL  RE,EXIT                                                          
         NTR1  LABEL=NO                                                         
         ICM   RF,15,DD_AEDIT                                                   
         JNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R1,DI_D                                                          
         LM    R2,RB,DI_R2RB                                                    
         GOTOR (RF),(R1)           Call system handler edit routine             
         J     EXIT                                                             
                                                                                
GETFLD52 CLI   DD_TYPE,DD_TCHRQ    Test character field                         
         JE    *+8                                                              
         CLI   DD_TYPE,DD_TMSKQ    Test bit mask                                
         JNE   GETFLD54                                                         
         L     R0,DI_AOUT                                                       
         SR    R1,R1                                                            
         ICM   R1,1,DI_LINP                                                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CHI   R1,L'DI_OVAL                                                     
         JNH   *+8                                                              
         LHI   R1,L'DI_OVAL                                                     
         L     RE,DI_AINP                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         CLI   DD_TYPE,DD_TMSKQ    Test bit mask                                
         JE    EXIT                (yes - need to compare masks)                
         L     RF,DI_AOUT                                                       
         LLC   R0,DI_LINP                                                       
         BASR  RE,0                Remove unprintable characters                
         CLI   0(RF),SPACE                                                      
         JNL   *+8                                                              
         MVI   0(RF),SPACE                                                      
         AHI   RF,1                                                             
         BCTR  R0,RE                                                            
         J     EXIT                                                             
                                                                                
GETFLD54 CLI   DD_TYPE,DD_TBINQ    Unsigned binary value                        
         JNE   GETFLD56                                                         
         LLC   RE,DI_LINP                                                       
         LHI   RF,1                                                             
         SLL   RF,0(RE)                                                         
         BCTR  RF,0                RF=ICM mask                                  
         L     R1,DI_AINP                                                       
         SR    R0,R0                                                            
         BASR  RE,0                                                             
         ICM   R0,0,0(R1)          Get value into R0 and edit                   
         EX    RF,0(RE)                                                         
         N     R0,HOBOFF           Ensure high order bit is off                 
         TM    DD_INDS1,DD_IZEBQ   Test ZERO=BLANK                              
         JZ    *+10                                                             
         LTR   R0,R0               Yes - test zero                              
         JZ    EXIT                                                             
         XC    DUB,DUB                                                          
         MVC   DUB+3(1),DD_NDECS                                                
         L     RF,DI_AOUT                                                       
         CURED (R0),(14,0(RF)),DUB,ALIGN=LEFT,ZERO=NOBLANK                      
         J     EXIT                                                             
                                                                                
GETFLD56 CLI   DD_TYPE,DD_TBISQ    Signed binary value                          
         JNE   GETFLD58                                                         
         LLC   RE,DI_LINP                                                       
         LHI   RF,1                                                             
         SLL   RF,0(RE)                                                         
         BCTR  RF,0                RF=ICM mask                                  
         L     R1,DI_AINP                                                       
         SR    R0,R0                                                            
         TM    0(R1),X'80'         Test value is negative                       
         JZ    *+8                                                              
         ICM   R0,15,EFFS                                                       
         BASR  RE,0                                                             
         ICM   R0,0,0(R1)          Get value into R0 and edit                   
         EX    RF,0(RE)                                                         
         TM    DD_INDS1,DD_IZEBQ   Test ZERO=BLANK                              
         JZ    *+10                                                             
         LTR   R0,R0               Yes - test zero                              
         JZ    EXIT                                                             
         XC    DUB,DUB                                                          
         MVC   DUB+3(1),DD_NDECS                                                
         L     RF,DI_AOUT                                                       
         CURED (R0),(14,0(RF)),DUB,ALIGN=LEFT,ZERO=NOBLANK,FLOAT=-              
         J     EXIT                                                             
                                                                                
GETFLD58 CLI   DD_TYPE,DD_TPAKQ    Signed packed value                          
         JNE   GETFLD60                                                         
         LLC   R1,DI_LINP                                                       
         L     RF,DI_AINP                                                       
         BASR  RE,0                                                             
         OC    0(0,RF),0(RF)                                                    
         EX    R1,0(RE)                                                         
         JZ    EXIT                                                             
         BASR  RE,0                                                             
         ZAP   WORK(8),0(0,RF)                                                  
         EX    R1,0(RE)                                                         
         TM    DD_INDS1,DD_IZEBQ   Test ZERO=BLANK                              
         JZ    *+14                                                             
         CP    DUB,PZERO           Yes - test zero                              
         JE    EXIT                                                             
         XC    DUB,DUB                                                          
         MVC   DUB+3(1),DD_NDECS                                                
         L     RF,DI_AOUT                                                       
         CURED (P8,WORK),(14,0(RF)),DUB,ALIGN=LEFT,FLOAT=-,ZERO=NOBLANK         
         J     EXIT                                                             
                                                                                
GETFLD60 CLI   DD_TYPE,DD_TEDTQ    EBCDIC date                                  
         JNE   GETFLD62                                                         
         L     RF,DI_AINP                                                       
         OC    0(6,RF),0(RF)                                                    
         JZ    EXIT                                                             
         GOTOR VDATCON,DMCB,(0,(RF)),('ODATTYPE',DI_AOUT)                       
         J     EXIT                                                             
                                                                                
GETFLD62 CLI   DD_TYPE,DD_TPDTQ    Packed date                                  
         JNE   GETFLD64                                                         
         L     RF,DI_AINP                                                       
         OC    0(3,RF),0(RF)                                                    
         JZ    EXIT                                                             
         GOTOR VDATCON,DMCB,(1,(RF)),('ODATTYPE',DI_AOUT)                       
         J     EXIT                                                             
                                                                                
GETFLD64 CLI   DD_TYPE,DD_TCDTQ    Comressed date                               
         JNE   GETFLD66                                                         
         L     RF,DI_AINP                                                       
         OC    0(2,RF),0(RF)                                                    
         JZ    EXIT                                                             
         GOTOR VDATCON,DMCB,(2,(RF)),('ODATTYPE',DI_AOUT)                       
         J     EXIT                                                             
                                                                                
GETFLD66 CLI   DD_TYPE,DD_TBDTQ    Binary date                                  
         JNE   GETFLD68                                                         
         L     RF,DI_AINP                                                       
         OC    0(3,RF),0(RF)                                                    
         JZ    EXIT                                                             
         GOTOR VDATCON,DMCB,(3,(RF)),('ODATTYPE',DI_AOUT)                       
         J     EXIT                                                             
                                                                                
GETFLD68 CLI   DD_TYPE,DD_TBMOQ    Binary month                                 
         JNE   GETFLD70                                                         
         L     RF,DI_AINP                                                       
         OC    0(2,RF),0(RF)                                                    
         JZ    EXIT                                                             
         MVC   WORK(2),0(RF)                                                    
         MVI   WORK+2,1                                                         
         GOTOR VDATCON,DMCB,(3,(RF)),('ODATTYPE',WORK+3)                        
         L     RF,DI_AOUT                                                       
         MVC   0(6,RF),WORK+3                                                   
         J     EXIT                                                             
                                                                                
GETFLD70 DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* Attach prefix text to output string if necessary                    *         
*                                                                     *         
* Ntry:- R1=A(Output string)                                          *         
*        R2=A(DD_D entry)                                             *         
***********************************************************************         
                                                                                
PREFIX   NTR1  LABEL=NO                                                         
                                                                                
         CLI   DD_LPFX,0           Test no prefix/empty string                  
         JE    EXIT                                                             
         CLC   0(L'DI_OVAL,R1),SPACES                                           
         JE    EXIT                                                             
                                                                                
         LR    R3,R1               R3=A(output field)                           
         MVC   WORK(L'DI_OVAL),0(R3)                                            
         MVC   0(L'DI_OVAL,R3),SPACES                                           
                                                                                
         CLI   DD_LPFX,DD_LPFXN    Test using DD_NAME as prefix                 
         JNE   PREFIX02                                                         
         LA    R4,DD_NAME+L'DD_NAME-1                                           
         LHI   R0,L'DD_NAME                                                     
         CLI   0(R4),SPACE                                                      
         JH    *+12                                                             
         BCTR  R4,0                                                             
         JCT   R0,*-10                                                          
         DC    H'0'                                                             
         AHI   R4,1                                                             
         LA    R0,DD_NAME                                                       
         SR    R4,R0               R4=length of prefix text                     
         LA    RE,DD_NAME                                                       
         J     PREFIX04                                                         
                                                                                
PREFIX02 LLC   R4,DD_LPFX          Get length of prefix text                    
         MVC   HALF,DD_DPFX                                                     
         LH    R0,HALF             R0=displacement to prefix literal            
         LA    RE,DD_DPFX                                                       
         AR    RE,R0               RE=A(prefix text)                            
                                                                                
PREFIX04 LR    R0,R3               R0=A(output feld)                            
         LR    R1,R4                                                            
         LR    RF,R4                                                            
         MVCL  R0,RE               Move prefix into output string               
                                                                                
         LA    R0,0(R3,R4)         R0=A(next output string character)           
         LA    R1,L'DI_OVAL                                                     
         SR    R1,R4               R1=length of string to attach                
         LR    RF,R1                                                            
         LA    RE,WORK                                                          
         MVCL  R0,RE               Move field value to output string            
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Build element table                                                 *         
*                                                                     *         
* Ntry:- R1=Calling mode (see below)                                  *         
*                                                                     *         
* Exit:- ELTAB entries created/updated for all elements found on the  *         
*        input record                                                 *         
***********************************************************************         
                                                                                
GETEOLD  EQU   1                   Get elements from old record                 
GETENEW  EQU   2                   Get elements from new record                 
                                                                                
GETELS   NTR1  LABEL=NO                                                         
                                                                                
         L     RE,DI_AIO1          Point to copy/add record                     
         CHI   R1,GETEOLD          Test get old elements                        
         JNE   GETELS02                                                         
         LHI   RF,ELAOLD-ELWORK                                                 
         CLI   DS_TYPE,DS_TCPYQ    Test copy/change                             
         JE    GETELS04                                                         
         CLI   DS_TYPE,DS_TDELQ    Test delete                                  
         JE    GETELS04                                                         
         J     EXITY                                                            
                                                                                
GETELS02 LHI   RF,ELANEW-ELWORK                                                 
         CLI   DS_TYPE,DS_TADDQ    Test add                                     
         JE    GETELS04                                                         
         L     RE,DI_AIO2          Point to change record                       
         CLI   DS_TYPE,DS_TCPYQ    Test copy/change pair                        
         JE    GETELS04                                                         
         CLI   DS_TYPE,DS_TRESQ    Test restore                                 
         JE    GETELS04                                                         
         J     EXITY                                                            
                                                                                
GETELS04 STH   RF,HALF1            Save displacement to address                 
                                                                                
         L     RF,DI_ADFD                                                       
         LLC   R3,DF_FSTEL-DF_D(RF)                                             
         L     RF,DI_ADRD                                                       
         CLI   DR_FSTEL-DR_D(RF),0                                              
         JE    *+8                                                              
         IC    R3,DR_FSTEL-DR_D(RF)                                             
         LTR   R3,R3                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,RE               R3=A(first element on record)                
         SR    R0,R0                                                            
                                                                                
GETELS06 CLI   0(R3),0             Test end of input record                     
         JE    EXIT                                                             
         CLC   DD_ELEM,0(R3)       Match element code to table                  
         JNE   GETELS14                                                         
                                                                                
         XC    ELWORK(ELWORKL),ELWORK                                           
         LLC   RE,DD_KDSP          RE=displacement to element key               
         CHI   RE,1                                                             
         JH    *+6                                                              
         DC    H'0'                                                             
         SR    RF,RF                                                            
         ICM   RF,1,DD_KLEN        RF=length of element key                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CHI   RF,L'ELKEY          Can't be longer than ELKEY                   
         JNH   *+6                                                              
         DC    H'0'                                                             
         AR    RF,RE                                                            
         CLM   RF,1,1(R3)          Test element long enough for key             
         JH    GETELS14            No - ignore                                  
         SR    RF,RE                                                            
         LA    R0,ELKEY                                                         
         LR    R1,RF                                                            
         AR    RE,R3                                                            
         MVCL  R0,RE               Extract element key into ELKEY               
                                                                                
GETELS08 LH    RF,ELTABN           Test if element exists                       
         GOTOR VBINSRCH,DMCB,ELWORK,AELTAB,(RF),ELWORKL,               *        
               ('ELKEY-ELWORK',ELKEYL),ELMAXN                                   
         CLI   0(R1),1             Test key not found                           
         JE    GETELS10            Yes - add new table entry                    
                                                                                
         ICM   RF,7,1(R1)          Point to record in table                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AH    RF,HALF1            Point RF to appropriate address              
         OC    0(L'ELAOLD,RF),0(RF)                                             
         JZ    GETELS12            Set address if not set                       
         LLC   RF,ELSEQ            Yes - bump sequence number                   
         AHI   RF,1                                                             
         STC   RF,ELSEQ                                                         
         CHI   RF,255                                                           
         JNH   GETELS08            and try again                                
         DC    H'0'                                                             
                                                                                
GETELS10 LH    RF,ELTABN           Add new entry to element table               
         GOTOR VBINSRCH,DMCB,(1,ELWORK),AELTAB,(RF),ELWORKL,           *        
               ('ELKEY-ELWORK',ELKEYL),ELMAXN                                   
         MVC   ELTABN,10(R1)       Save number of entries in table              
         ICM   RF,7,1(R1)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AH    RF,HALF1            Point RF to appropriate address              
                                                                                
GETELS12 STCM  R3,15,0(RF)         Set A(element in table)                      
                                                                                
GETELS14 LLC   R0,1(R3)            Bump to next element on record               
         AR    R3,R0                                                            
         J     GETELS06                                                         
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Concatenate field to output string in multi field mode              *         
*                                                                     *         
* Ntry:- R1=A(Parameter list) as follows:-                            *         
*                                                                     *         
*        P1    A(Input string)                                        *         
*        P2    A(Output field length followed by output field value)  *         
*                                                                     *         
*        R2=A(Current DD_D entry)                                     *         
*        JOINDLIM may contain a default field delimiter value         *         
*                                                                     *         
* Exit:- Output field length and field value updated if any input     *         
***********************************************************************         
                                                                                
         USING DD_D,R2                                                          
JOINIT   NTR1  LABEL=NO                                                         
         LM    R3,R4,0(R1)         R3=A(input), R4=A(output)                    
         LLC   R5,0(R4)            R5=length used in output field               
         LA    RF,L'DI_OVAL-1(R3)  Point to end of input string                 
                                                                                
JOINIT02 CR    RF,R3               Get length of input string                   
         JL    EXIT                (input string is empty)                      
         CLI   0(RF),SPACE                                                      
         JH    *+8                                                              
         JCT   RF,JOINIT02                                                      
         AHI   RF,1                                                             
         SR    RF,R3               RF=length of input string                    
                                                                                
         CLI   DD_DELIM,SPACE      Test override delimiter present              
         JH    *+12                                                             
         CLI   JOINDLIM,SPACE      Test default delimter present                
         JNH   JOINIT04                                                         
         LTR   R5,R5               Test first output field                      
         JZ    JOINIT04                                                         
         AHI   R5,L'JOINDLIM       Add delimter to string                       
         LA    RE,0(R4,R5)                                                      
         MVC   0(L'JOINDLIM,RE),JOINDLIM                                        
         CLI   DD_DELIM,SPACE                                                   
         JNH   JOINIT04                                                         
         MVC   0(L'DD_DELIM,RE),DD_DELIM                                        
                                                                                
JOINIT04 LA    R0,0(RF,R5)         R0=total output field length                 
         CHI   R0,L'DI_OVAL        Test new data will fit in output             
         JNH   *+6                                                              
         DC    H'0'                                                             
         STC   R0,0(R4)            Set new output length                        
         LA    R0,1(R4,R5)                                                      
         LR    R1,RF                                                            
         LR    RE,R3                                                            
         MVCL  R0,RE               Add input to end of output string            
         J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Output a header record (if necessary) and a data record - also      *         
* handles report opening and closing when change of agency occurs     *         
* and end-of-file                                                     *         
***********************************************************************         
                                                                                
OUTPUT   NTR1  LABEL=NO                                                         
                                                                                
         LARL  R9,LITERALS         Restore registers                            
         LARL  R8,PREGS                                                         
         LM    R8,RA,0(R8)                                                      
                                                                                
         TM    OUTMODE,OUTMLAST    Test last time call                          
         JZ    OUTPUT02                                                         
         TM    OUTFLAG,OUTFCALL    Test any other calls                         
         JZ    EXIT                No                                           
         XC    DS_ALF,DS_ALF       Yes - force change of agency                 
         J     OUTPUT06                                                         
                                                                                
OUTPUT02 OI    OUTFLAG,OUTFCALL    Set this routine has been called             
         OC    STRDATEB,STRDATEB   Test filter dates given                      
         JZ    OUTPUT04                                                         
         GOTOR VDATCON,DMCB,(3,DS_DATE),(15,DATEJ)                              
         ZAP   DUB,DATEJ                                                        
         SP    DUB,STRDATEJ                                                     
         JNM   *+6                                                              
         DC    H'0'                                                             
         CVB   R0,DUB                                                           
         STH   R0,RELDAY           Set relative day mask bit                    
                                                                                
OUTPUT04 OC    DLCBAPR,DLCBAPR     Test first time                              
         JNZ   OUTPUT06                                                         
         LARL  RE,PUTLIN           Initialize DLFLD control block               
         STCM  RE,15,DLCBAPR       Set A(print routine)                         
         LA    RE,P+1                                                           
         STCM  RE,15,DLCBAPL       Set A(print line)                            
         MVC   DLCBAED,VEDITOR     Set A(EDITOR)                                
         LHI   RE,L'P-1                                                         
         CLI   REPWIDTH,0                                                       
         JE    *+8                                                              
         IC    RE,REPWIDTH                                                      
         STH   RE,DLCXMAXL         Set width of print line                      
         MVI   DLCXDELC,SPACE                                                   
         MVI   DLCXEOTC,QUOTE                                                   
         MVI   DLCXEOTA,DINK                                                    
         MVI   DLCXEOLC,SCOLON                                                  
         MVI   DLCXEORC,COLON                                                   
         OC    DELIMS,DELIMS       Test override delimiters given               
         JZ    OUTPUT06            No - use defaults                            
         MVC   DLCXDELC(L'DELIMS),DELIMS                                        
                                                                                
OUTPUT06 OC    LALF,LALF           Test first time                              
         JZ    OUTPUT14                                                         
         CLC   DS_ALF,LALF         Test change of agency alpha                  
         JE    OUTPUT34                                                         
         OC    STRDATEB,STRDATEB   Test range of recovery days                  
         JZ    OUTPUT12                                                         
         ZAP   DATEJ,STRDATEJ      Send null line for missing days              
         MVC   SVALF,DS_ALF        Save current agency alpha                    
         MVC   SVDATE,DS_DATE      Save current date                            
         MVC   SVMODE,OUTMODE      Save current calling mode                    
         MVC   DS_ALF,LALF         Set previous agency alpha                    
                                                                                
OUTPUT08 ZAP   DUB,DATEJ                                                        
         SP    DUB,STRDATEJ                                                     
         CVB   RE,DUB                                                           
         SRDL  RE,3                                                             
         SRL   RF,32-3                                                          
         LA    RE,DAYMASK(RE)                                                   
         IC    RF,BITLIST(RF)                                                   
         BASR  R1,0                                                             
         TM    0(RE),0                                                          
         EX    RF,0(R1)            Test day mask bit is on                      
         JNZ   OUTPUT10                                                         
         GOTOR VDATCON,DMCB,(6,DATEJ),(3,DS_DATE)                               
         MVI   OUTMODE,OUTMNULL+OUTMDAYS                                        
         J     OUTPUT34                                                         
                                                                                
OUTPUT10 AP    DATEJ,PONE          Bump to next day                             
         CP    DATEJ,ENDDATEJ      Test all days inspected                      
         JNH   OUTPUT08                                                         
         MVC   DS_ALF,SVALF        Restore agency alpha                         
         MVC   DS_DATE,SVDATE      Restore saved date                           
         MVC   OUTMODE,SVMODE      Restore saved calling mode                   
                                                                                
OUTPUT12 GOTOR CLOREP              Close current report                         
         TM    OUTMODE,OUTMLAST    Test last time call                          
         JNZ   EXITY                                                            
                                                                                
OUTPUT14 LLC   R1,AGYPUT#          Add entry to AGYPUT                          
         AHI   R1,1                                                             
         STC   R1,AGYPUT#                                                       
         MHI   R1,L'AGYPUT                                                      
         LA    R1,AGYPUT-L'AGYPUT(R1)                                           
         MVC   0(L'DS_ALF,R1),DS_ALF                                            
         XC    DAYMASK,DAYMASK     Clear day mask                               
                                                                                
         GOTOR OPNREP              Open a new report                            
                                                                                
         L     RF,DI_ADAD          Test regular download/edit send              
         TM    DA_INDS-DA_D(RF),DA_IOEDI                                        
         JZ    OUTPUT26                                                         
         MVC   P,SPACES            Output EDICT header records                  
         MVC   P+5(12),=C'*HDR*EDICT=+'                                         
         MVC   P+17(L'DS_ALF),DS_ALF                                            
         MVI   P+35,C'W'                                                        
         GOTOR PUTLIN                                                           
         MVC   P+1(5),=C'++DDS'                                                 
         MVC   P+7(L'EDIN),EDIN                                                 
         MVC   P+7+L'EDIN(2),PSPP+1                                             
         MVC   P+12(3),=C'TRN'                                                  
         GOTOR PUTLIN                                                           
         MVC   P+1(5),=C'++DDS'                                                 
         MVC   P+12(3),=C'SUB'                                                  
         MVC   P+16(L'PSPP),PSPP                                                
         GOTOR PUTLIN                                                           
         MVC   P+1(5),=C'++DDS'                                                 
         MVC   P+12(3),=C'FIL'                                                  
         MVC   P+16(L'PSPP),PSPP                                                
         L     RF,DI_ADAD          Test have agency label available             
         OC    DA_LABEL-DA_D(,RF),DA_LABEL-DA_D(RF)                             
         JNZ   OUTPUT16            Yes                                          
         MVC   P+16+L'PSPP+1(L'DS_ALF),DS_ALF                                   
         MVC   P+16+L'PSPP+L'DS_ALF+2(L'TODAYE),TODAYE                          
         OC    STRDATEB,STRDATEB                                                
         JZ    OUTPUT24                                                         
         MVC   P+16+L'PSPP+L'DS_ALF+2(L'STRDATEE),STRDATEE                      
         MVC   P+16+L'PSPP+L'DS_ALF+L'STRDATEE+3(L'ENDDATEE),ENDDATEE           
         J     OUTPUT24                                                         
                                                                                
OUTPUT16 MVC   P+16+L'PSPP+1(L'DA_LABEL),DA_LABEL-DA_D(RF)                      
         MVC   P+16+L'PSPP+L'DA_LABEL+2(L'TODAYE),TODAYE                        
         OC    STRDATEB,STRDATEB                                                
         JZ    OUTPUT24                                                         
         MVC   P+16+L'PSPP+L'DA_LABEL+2(L'STRDATEE),STRDATEE                    
         MVC   P+16+L'PSPP+L'DA_LABEL+L'STRDATEE+3(L'ENDDATEE),ENDDATEE         
                                                                                
OUTPUT24 GOTOR PUTLIN                                                           
         MVC   P+1(5),=C'++DDS'                                                 
         MVC   P+12(3),=C'EXT'                                                  
         MVC   P+16(3),=C'TXT'                                                  
         GOTOR PUTLIN                                                           
         J     OUTPUT28                                                         
                                                                                
OUTPUT26 MVC   P,SPACES            Output regular download request page         
         MVC   P+1(L'TITLE),TITLE                                               
         GOTOR PUTLIN              Put report title to page 1                   
         MVI   P,CCPAGE                                                         
         GOTOR PUTLIN              Skip to second page                          
                                                                                
OUTPUT28 MVC   LALF,DS_ALF         Set current agency alpha id                  
                                                                                
         LARL  R3,KCDEFN           Send column headings                         
         USING KCDEFND,R3          R3=A(key column definitions)                 
OUTPUT30 CLI   KCDEFND,KCDEEOTQ    Test end of table                            
         JE    OUTPUT32                                                         
         ICM   R2,15,KCDEFTAB                                                   
         USING KCDTABD,R2          R2=A(key column definition entry)            
         MVC   DLCBFLX,SPACES                                                   
         MVC   DLCBFLX(L'KCDTCOLN),KCDTCOLN                                     
         MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT     Set action to put                            
         GOTOR VDLFLD,DLCBD        Put column name                              
         AHI   R3,KCDEFNL          Bump to next column definition               
         J     OUTPUT30                                                         
                                                                                
OUTPUT32 MVC   DLCBFLX,SPACES                                                   
         MVC   DLCBFLX(L'DATAHED1),DATAHED1                                     
         MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT     Set action to put                            
         GOTOR VDLFLD,DLCBD        Put data column heading 1                    
                                                                                
         MVC   DLCBFLX,SPACES                                                   
         MVC   DLCBFLX(L'DATAHED2),DATAHED2                                     
         MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT     Set action to put                            
         GOTOR (RF),(R1)           Put data column heading 2                    
                                                                                
         MVC   DLCBFLX,SPACES                                                   
         MVC   DLCBFLX(L'DATAHED3),DATAHED3                                     
         MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT     Set action to put                            
         GOTOR (RF),(R1)           Put data column heading 3                    
                                                                                
         MVC   DLCBFLX,SPACES                                                   
         MVC   DLCBFLX(L'DATAHED4),DATAHED4                                     
         MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBACT,DLCBPUT     Set action to put                            
         GOTOR (RF),(R1)           Put data column heading 4                    
                                                                                
         MVI   DLCBACT,DLCBEOL     Set action to end of line                    
         GOTOR (RF),(R1)           Put end of line                              
                                                                                
OUTPUT34 LH    R0,SUBSEQN          Set sub sequence number                      
         TM    OUTFLAG,OUTFHEAD                                                 
         JNZ   *+10                                                             
         SR    R0,R0               Sequence=0 for header                        
         J     *+8                                                              
         AHI   R0,1                Sequence=1 or greater for data               
         STH   R0,SUBSEQN                                                       
                                                                                
         LARL  R3,KCDEFN           R3=A(key definitions)                        
OUTPUT36 CLI   KCDEFND,KCDEEOTQ    Test end of table                            
         JE    OUTPUT86                                                         
         MVC   DLCBFLD,SPACES                                                   
         MVC   DLCBFLX,SPACES                                                   
         ICM   R2,15,KCDEFTAB                                                   
         CLI   KCDEFTYP,KCDEFTKC                                                
         JE    OUTPUT38                                                         
         DC    H'0'                                                             
                                                                                
OUTPUT38 MVI   DLCBTYP,DLCBTXT     Set default data type                        
                                                                                
         CLI   KCDTCOL#,KCDTCAGY   Always print agency                          
         JE    OUTPUT40                                                         
         CLI   KCDTCOL#,KCDTCSYS   Always print system                          
         JE    OUTPUT40                                                         
         CLI   KCDTCOL#,KCDTCDAT   Always print date                            
         JE    OUTPUT40                                                         
         CLI   KCDTCOL#,KCDTCSEQ   Always print sequence number                 
         JE    OUTPUT40                                                         
         TM    OUTMODE,OUTMNULL    Test creating null agency report             
         JNZ   OUTPUT84                                                         
                                                                                
OUTPUT40 SR    RF,RF                                                            
         ICM   RF,3,KCDTFDSP                                                    
         LA    RE,WORKD                                                         
         CLI   KCDTFBLK,KCDTFWRK   Test data in WORKD                           
         JE    OUTPUT70                                                         
         LA    RE,DI_D                                                          
         CLC   =C'STXALL',106(RE)                                               
         JNE   *+8                                                              
         CLI   KCDTFBLK,KCDTFDID   Test data in DI_D                            
         CLI   KCDTFBLK,KCDTFDID   Test data in DI_D                            
         JE    OUTPUT70                                                         
         DC    H'0'                                                             
                                                                                
OUTPUT70 AR    RF,RE               RF=A(data)                                   
         CLI   KCDTTYPE,KCDTEDRQ   Edit routine                                 
         JE    OUTPUT74                                                         
         CLI   KCDTTYPE,KCDTCHRQ   Character string                             
         JE    OUTPUT76                                                         
         CLI   KCDTTYPE,KCDTBDTQ   Binary date                                  
         JE    OUTPUT78                                                         
         CLI   KCDTTYPE,KCDTPTMQ   Packed time                                  
         JE    OUTPUT80                                                         
         CLI   KCDTTYPE,KCDTBINQ   Binary value                                 
         JE    OUTPUT82                                                         
         DC    H'0'                                                             
                                                                                
OUTPUT74 LR    R1,RF               Edit routine                                 
         ICM   RF,15,KCDTAEDT                                                   
         GOTOR (RF),(R1)                                                        
         J     OUTPUT84                                                         
                                                                                
OUTPUT76 LLC   R1,KCDTOLEN         Character string                             
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         MVC   DLCBFLX(0),0(RF)                                                 
         EX    R1,0(RE)                                                         
         J     OUTPUT84                                                         
                                                                                
OUTPUT78 GOTOR VDATCON,DMCB,(3,(RF)),('ODATTYPE',DLCBFLX)                       
         J     OUTPUT84                                                         
                                                                                
OUTPUT80 OI    3(RF),X'0F'         Packed time                                  
         UNPK  WORK(6),0(4,RF)                                                  
         MVC   DLCBFLX(2),WORK                                                  
         MVI   DLCBFLX+2,C':'                                                   
         MVC   DLCBFLX+3(2),WORK+2                                              
         MVI   DLCBFLX+5,C':'                                                   
         MVC   DLCBFLX+6(2),WORK+4                                              
         J     OUTPUT84                                                         
                                                                                
OUTPUT82 MVI   DLCBTYP,DLCBBIN     Binary value                                 
         SR    R1,R1                                                            
         ICM   R1,1,KCDTFLEN                                                    
         STC   R1,DLCBLEN                                                       
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         MVC   DLCBFLD(0),0(RF)                                                 
         EX    R1,0(RE)                                                         
         J     OUTPUT84                                                         
                                                                                
OUTPUT84 MVI   DLCBACT,DLCBPUT     Set action to put                            
         GOTOR VDLFLD,DLCBD        Put key data field                           
         AHI   R3,KCDEFNL                                                       
         J     OUTPUT36                                                         
         DROP  R2,R3                                                            
                                                                                
OUTPUT86 TM    OUTFLAG,OUTFHEAD    Test header record put                       
         JNZ   OUTPUT88            No                                           
         TM    OUTMODE,OUTMNULL    Test creating null agency report             
         JZ    *+10                                                             
         MVC   DLCBFLX(L'NODATA),NODATA                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTOR VDLFLD,DLCBD                                                     
                                                                                
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTOR (RF),(R1)                                                        
                                                                                
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTOR (RF),(R1)                                                        
                                                                                
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTOR (RF),(R1)                                                        
         J     OUTPUT90                                                         
                                                                                
OUTPUT88 OC    DI_EKEY,DI_EKEY                                                  
         JZ    *+10                                                             
         MVC   DLCBFLX(L'DI_EKEY),DI_EKEY                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTOR VDLFLD,DLCBD                                                     
                                                                                
         MVC   DLCBFLX(L'DI_FLDN),DI_FLDN                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTOR (RF),(R1)                                                        
                                                                                
         CLC   DI_OVAL,SPACES                                                   
         JE    *+10                                                             
         MVC   DLCBFLX,DI_OVAL                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTOR (RF),(R1)                                                        
                                                                                
         CLC   DI_NVAL,SPACES                                                   
         JE    *+10                                                             
         MVC   DLCBFLX,DI_NVAL                                                  
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTOR (RF),(R1)                                                        
                                                                                
OUTPUT90 MVI   DLCBACT,DLCBEOL     Send end of line                             
         GOTOR VDLFLD,DLCBD                                                     
                                                                                
         TM    OUTMODE,OUTMDAYS    Test sending null days                       
         JNZ   OUTPUT10            Yes - continue looking for them              
         OC    STRDATEB,STRDATEB   Test range of recovery days                  
         JZ    OUTPUT92                                                         
         LH    RE,RELDAY                                                        
         SRDL  RE,3                                                             
         SRL   RF,32-3                                                          
         LA    RE,DAYMASK(RE)                                                   
         IC    RF,BITLIST(RF)                                                   
         BASR  R1,0                                                             
         OI    0(RE),0             Yes - turn on day bit in mask                
         EX    RF,0(R1)                                                         
                                                                                
OUTPUT92 TM    OUTMODE,OUTMNULL    Test sending null agency report              
         JNZ   EXIT                                                             
                                                                                
         TM    OUTFLAG,OUTFHEAD    Test header record just put                  
         JNZ   EXIT                                                             
         OI    OUTFLAG,OUTFHEAD    Yes - set header record just put             
         J     OUTPUT34            Go back and do data record                   
         EJECT                                                                  
***********************************************************************         
* Open a new report                                                   *         
***********************************************************************         
                                                                                
OPNREP   NTR1  LABEL=NO                                                         
         MVI   DLCBACT,DLCBINIT    Set action to initialize                     
         GOTOR VDLFLD,DLCBD        Initialize new report                        
         OI    DLCBFLG1,DLCBFXFL   Turn on extended DLCB flag                   
         OC    REPID,REPID         Test output to prtque                        
         JZ    EXITY                                                            
                                                                                
         USING PQPLD,WORK                                                       
         XC    PQPLD(QLSOFEND-PQPLD),PQPLD                                      
         MVI   QLEXTRA,FF          Set extended open list                       
         L     RF,DI_ADAD                                                       
         OC    QLSRCID,DA_PRNID-DA_D(RF)                                        
         JNZ   *+6                                                              
         DC    H'0'                                                             
         OC    REPUSER,REPUSER     Test override user-id set                    
         JZ    *+10                                                             
         MVC   QLSRCID,REPUSER                                                  
         MVC   QLSUBID,REPID                                                    
         MVC   QLSYS(L'QLSYS+L'QLPRG),REPREF                                    
         MVC   QLCLASS,REPCLASS                                                 
         TM    DA_INDS-DA_D(RF),DA_IOEDI                                        
         JZ    *+8                                                              
         MVI   QLCLASS,C'G'        Set class to 'G' for EDICT reports           
         CLI   QLCLASS,C' '                                                     
         JH    *+8                                                              
         MVI   QLCLASS,C'X'        Default class is 'X'                         
         MVI   QLTYPE,QLTYDL                                                    
         MVC   QLDESC,REPDESC                                                   
         MVI   QLLINET,QLLTCC+QLLTFL                                            
         SR    RE,RE                                                            
         ICM   RE,1,REPWIDTH                                                    
         JNZ   *+8                                                              
         LHI   RE,L'P-1                                                         
         AHI   RE,1                Add one for control character                
         STC   RE,QLLINEW                                                       
*                                                                               
         L     RF,=A(PQRETTAB)     SET DEFAULT RETAIN CLASS                     
         MVC   QLRETNL,0(RF)                                                    
         MVC   QLRETND,2(RF)                                                    
         OC    RETAIN,RETAIN       ANY RETAIN CLASS GIVEN?                      
         JZ    OPNREP20                                                         
*                                                                               
OPNREP10 LA    RF,L'PQRETTAB(RF)   SEARCH RETAIN CLASS TABLE                    
         OC    0(2,RF),0(RF)                                                    
         JZ    OPNREP20                                                         
         CLC   RETAIN,4(RF)        MATCH ON RETAIN CLASS                        
         JNE   OPNREP10                                                         
         MVC   QLRETNL,0(RF)       COPY VALUES FOR CLASS                        
         MVC   QLRETND,2(RF)                                                    
OPNREP20 DS    0H                                                               
*                                                                               
         GOTOR VDMGR,DMCB,PQOPEN,PQFILE,0,PQPLD,APQBUFF                         
         JE    EXITY                                                            
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* Put output line to print queue and/or printer                       *         
***********************************************************************         
                                                                                
PUTLIN   NTR1  LABEL=NO                                                         
         OI    RUNFLAG,RUNFPUTQ    Set this routine called                      
         OC    REPID,REPID         Test output to print queue                   
         JZ    PUTLIN02                                                         
                                                                                
         CLI   P,SPACE             Test normal print line (not ccpage)          
         JNE   *+8                                                              
         MVI   P,CCLINE            Yes - set to print a line                    
         GOTOR VDMGR,DMCB,PQPRINT,PQFILE,0,P,APQBUFF                            
         JE    PUTLIN02                                                         
         DC    H'0'                                                             
                                                                                
PUTLIN02 CLI   TRACE,0             Test tracing                                 
         JE    PUTLIN04                                                         
         GOTOR VPRINT                                                           
                                                                                
PUTLIN04 MVC   P,SPACES                                                         
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Close current report (will call PRTLIN via DLFLD to put last line)  *         
***********************************************************************         
                                                                                
CLOREP   NTR1  LABEL=NO                                                         
         TM    RUNFLAG,RUNFPUTQ    Test putlin routine called                   
         JZ    EXITY                                                            
         MVI   DLCBACT,DLCBEOR     Set action to end of report                  
         GOTOR VDLFLD,DLCBD        Put end of report                            
                                                                                
         OC    REPID,REPID         Test output to prtque                        
         JZ    EXITY                                                            
         GOTOR VDMGR,DMCB,PQCLOSE,PQFILE,P,P,APQBUFF                            
         JE    *+6                                                              
         DC    H'0'                                                             
         NI    RUNFLAG,FF-RUNFPUTQ                                              
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit bit mask                                                       *         
*                                                                     *         
* Ntry:- DI_LINP=Mask length                                          *         
*        DI_OVAL=Old mask raw data value                              *         
*        DI_NVAL=New mask raw data value                              *         
*                                                                     *         
* Exit:- DI_OVAL=Old mask printable value                             *         
*        DI_NVAL=New mask printable value                             *         
***********************************************************************         
                                                                                
         USING DD_D,R2                                                          
EDTMSK   NTR1  LABEL=NO                                                         
                                                                                
         MVC   DUB1,DI_OVAL                                                     
         MVC   DI_OVAL,SPACES                                                   
         LA    R3,DI_OVAL          R3=A(old value)                              
                                                                                
         MVC   DUB2,DI_NVAL                                                     
         MVC   DI_NVAL,SPACES                                                   
         LA    R4,DI_NVAL          R4=A(new value)                              
                                                                                
         LLC   R0,DD_FLEN                                                       
         CHI   R0,8                                                             
         JNH   *+6                                                              
         DC    H'0'                                                             
         MHI   R0,8                R0=number of bits to test                    
         ICM   R5,15,DD_AMASK                                                   
         USING DM_D,R5             R5=A(mask table)                             
         SR    R6,R6               R6=bit index number                          
                                                                                
EDTMSK02 CLC   DM_D(DM_LNQ),SPACES Test ignore this bit                         
         JNH   EDTMSK14                                                         
                                                                                
         LR    RE,R6               Test old bit value                           
         SRDA  RE,3                                                             
         SRL   RF,32-3                                                          
         LA    RE,DUB1(RE)                                                      
         IC    RF,BITLIST(RF)                                                   
         MVI   BYTE1,0                                                          
         BASR  R1,0                                                             
         TM    0(RE),0                                                          
         EX    RF,0(R1)                                                         
         JZ    *+8                                                              
         MVI   BYTE1,1                                                          
                                                                                
         LR    RE,R6               Test new bit value                           
         SRDA  RE,3                                                             
         SRL   RF,32-3                                                          
         LA    RE,DUB2(RE)                                                      
         IC    RF,BITLIST(RF)                                                   
         MVI   BYTE2,0                                                          
         BASR  R1,0                                                             
         TM    0(RE),0                                                          
         EX    RF,0(R1)                                                         
         JZ    *+8                                                              
         MVI   BYTE2,1                                                          
                                                                                
         CLC   BYTE1,BYTE2         Test any change to this bit                  
         JE    EDTMSK14                                                         
                                                                                
         CLI   DS_TYPE,DS_TADDQ    Test add                                     
         JE    EDTMSK08                                                         
         CLI   DS_TYPE,DS_TRESQ    Test restore                                 
         JE    EDTMSK08                                                         
                                                                                
         CLI   BYTE1,0             Format old value                             
         JNE   EDTMSK04                                                         
         CLC   DM_Z,SPACES                                                      
         JNH   EDTMSK08                                                         
         MVC   0(L'DM_Z,R3),DM_Z                                                
         J     EDTMSK06                                                         
EDTMSK04 CLC   DM_O,SPACES                                                      
         JNH   EDTMSK08                                                         
         MVC   0(L'DM_O,R3),DM_O                                                
EDTMSK06 AHI   R3,L'DM_Z-1                                                      
         CLI   0(R3),SPACE                                                      
         JH    *+8                                                              
         JCT   R3,*-8                                                           
         MVI   1(R3),COMMA                                                      
         AHI   R3,2                                                             
                                                                                
EDTMSK08 CLI   DS_TYPE,DS_TDELQ    Test delete                                  
         JE    EDTMSK14                                                         
                                                                                
         CLI   BYTE2,0             Format new value                             
         JNE   EDTMSK10                                                         
         CLC   DM_Z,SPACES                                                      
         JNH   EDTMSK14                                                         
         MVC   0(L'DM_Z,R4),DM_Z                                                
         J     EDTMSK12                                                         
EDTMSK10 CLC   DM_O,SPACES                                                      
         JNH   EDTMSK14                                                         
         MVC   0(L'DM_O,R4),DM_O                                                
EDTMSK12 AHI   R4,L'DM_Z-1                                                      
         CLI   0(R4),SPACE                                                      
         JH    *+8                                                              
         JCT   R4,*-8                                                           
         MVI   1(R4),COMMA                                                      
         AHI   R4,2                                                             
                                                                                
EDTMSK14 AHI   R5,DM_LNQ           Bump to next mask entry                      
         AHI   R6,1                Bump bit index number                        
         JCT   R0,EDTMSK02         Do for number of bits in mask                
                                                                                
         BCTR  R3,0                Remove trailing commas                       
         CLI   0(R3),COMMA                                                      
         JNE   *+8                                                              
         MVI   0(R3),SPACE                                                      
         BCTR  R4,0                                                             
         CLI   0(R4),COMMA                                                      
         JNE   *+8                                                              
         MVI   0(R4),SPACE                                                      
         CLC   DI_OVAL,DI_NVAL     Test anything to print                       
         J     EXIT                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* Edit User ID                                                        *         
*                                                                     *         
* Ntry:- R1=A(User ID number)                                         *         
*                                                                     *         
* Exit:- DLCBFLX set to User-ID code                                  *         
***********************************************************************         
                                                                                
EDTUID   NTR1  LABEL=NO                                                         
         OC    0(L'CTIKNUM,R1),0(R1)                                            
         JZ    EDTUID06                                                         
         CLC   LUSERID#,0(R1)      Test same as previous                        
         JE    EDTUID08                                                         
         MVC   LUSERID#,0(R1)      Set last user id                             
                                                                                
         USING CTIREC,IO           Read user-id number record                   
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,LUSERID#                                                 
         GOTOR VDMGR,DMCB,DMREAD,CTFILE,CTIREC,CTIREC                           
         JNE   EDTUID04                                                         
         LA    R1,CTIDATA                                                       
         USING CTDSCD,R1                                                        
         SR    R0,R0                                                            
EDTUID02 CLI   CTDSCEL,0                                                        
         JE    EDTUID04                                                         
         CLI   CTDSCEL,CTDSCELQ                                                 
         JE    *+14                                                             
         IC    R0,CTDSCLEN                                                      
         AR    R1,R0                                                            
         J     EDTUID02                                                         
         CLI   CTDSCLEN,L'CTIKID+(CTDSC-CTDSCD)                                 
         JNE   EDTUID04                                                         
         MVC   LUSERID,CTDSC                                                    
         J     EDTUID08                                                         
         DROP  R1                                                               
                                                                                
EDTUID04 MVC   LUSERID,SPACES                                                   
         MVI   LUSERID,C'#'                                                     
         SR    R0,R0                                                            
         ICM   R0,3,LUSERID#                                                    
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  LUSERID+1(5),DUB                                                 
         J     EDTUID08                                                         
                                                                                
EDTUID06 MVC   LUSERID,SPACES                                                   
         XC    LUSERID#,LUSERID#                                                
                                                                                
EDTUID08 MVC   DLCBFLX(L'LUSERID),LUSERID                                       
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Edit Person ID                                                      *         
*                                                                     *         
* Ntry:- R1=A(Person ID number)                                       *         
*                                                                     *         
* Exit:- DLCBFLX set to Person ID                                     *         
***********************************************************************         
                                                                                
EDTPID   NTR1  LABEL=NO                                                         
         OC    0(L'SA0KNUM,R1),0(R1)                                            
         JZ    EXITY                                                            
         MVC   WORK(L'DS_SECAG),DS_SECAG                                        
         OC    DS_SECAG,DS_SECAG                                                
         JNZ   *+14                                                             
         L     RF,DI_ADAD                                                       
         MVC   WORK(L'DA_SECAG),DA_SECAG-DA_D(RF)                               
                                                                                
         CLC   LAGYSEC,WORK        Test change of security agency               
         JNE   *+14                                                             
         CLC   LPID#,0(R1)         Test change of person                        
         JE    EDTPID90                                                         
                                                                                
         MVC   LAGYSEC,WORK                                                     
         MVC   LPID#,0(R1)                                                      
         MVC   LPID,SPACES                                                      
         MVC   LPIDFNAM,SPACES                                                  
         MVC   LPIDMNAM,SPACES                                                  
         MVC   LPIDLNAM,SPACES                                                  
                                                                                
         USING SA0REC,IO                                                        
         XC    SA0KEY,SA0KEY       Read person record                           
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,LAGYSEC                                                  
         MVC   SA0KNUM,LPID#                                                    
         GOTOR VDMGR,DMCB,DMREAD,CTFILE,SA0REC,SA0REC                           
         JNE   EDTPID30                                                         
                                                                                
         LA    R1,SA0DATA                                                       
         SR    R0,R0                                                            
EDTPID10 CLI   0(R1),0                                                          
         JE    EDTPID30                                                         
         CLI   0(R1),SAPALELQ                                                   
         JE    EDTPID20                                                         
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         J     EDTPID10                                                         
         USING SAPALD,R1                                                        
EDTPID20 MVC   LPID,SAPALPID       Set person id                                
         DROP  R1                                                               
         J     EDTPID50                                                         
                                                                                
EDTPID30 MVI   LPID,C'#'           Unknown person                               
         GOTOR VHEXOUT,DMCB,LPID#,LPID+1,L'LPID#,HEXTOG                         
         J     EDTPID90                                                         
                                                                                
         USING SAPEREC,R3                                                       
EDTPID50 LA    R3,WORK                                                          
         XC    SAPEKEY,SAPEKEY     Read person record                           
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,LAGYSEC                                                  
         MVC   SAPEPID,LPID                                                     
*                                                                               
         LA    RE,DS_WORK+(DS_DATE-DS_D)                                        
         GOTOR VDATCON,DMCB,(3,(RE)),(2,HALF)                                   
         MVC   SAPEDEF,=X'FFFF'                                                 
         XC    SAPEDEF,HALF                                                     
         DROP  R3                                                               
*                                                                               
         USING SAPEREC,IO                                                       
         GOTOR VDMGR,DMCB,DMREAD,CTFILE,WORK,SAPEREC                            
         JL    EDTPID90                                                         
         CLC   SAPEREC(SAPEDEF-SAPEKEY),WORK                                    
         JNE   EDTPID90                                                         
                                                                                
         LA    R1,SAPEDATA                                                      
         SR    R0,R0                                                            
EDTPID60 CLI   0(R1),0                                                          
         JE    EDTPID90                                                         
         CLI   0(R1),SANAMELQ                                                   
         JE    EDTPID70                                                         
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         J     EDTPID60                                                         
                                                                                
         USING SANAMD,R1                                                        
EDTPID70 LARL  R5,PERFNEX1         locate executed MVC                          
         LA    R4,SANAMES                                                       
         SR    RE,RE                                                            
         TM    SANAMIND,SANAMIFN                                                
         JZ    EDTPID73                                                         
         LA    R3,LPIDFNAM                                                      
         IC    RE,0(,R4)           length first name                            
         BCTR  RE,0                                                             
         EX    RE,0(,R5)           copy length and first name                   
         LA    R4,2(RE,R4)         point to next name                           
EDTPID73 TM    SANAMIND,SANAMIMN                                                
         JZ    EDTPID75                                                         
         LA    R3,LPIDMNAM                                                      
         IC    RE,0(,R4)                                                        
         BCTR  RE,0                                                             
         EX    RE,0(,R5)                                                        
         LA    R4,2(RE,R4)                                                      
EDTPID75 TM    SANAMIND,SANAMILN                                                
         JZ    EDTPID80                                                         
         LA    R3,LPIDLNAM                                                      
         IC    RE,0(,R4)                                                        
         BCTR  RE,0                                                             
         EX    RE,0(,R5)                                                        
EDTPID80 DS    0H                                                               
         DROP  R1                                                               
                                                                                
EDTPID90 MVC   DLCBFLX(L'LPID),LPID                                             
         J     EXITY                                                            
                                                                                
PERFNEX1 MVC   0(0,R3),1(R4)                                                    
         EJECT                                                                  
                                                                                
***********************************************************************         
* Edit Person ID Name                                                 *         
* Ntry:- R1=A(Person ID number), Names fields already set by EDTPID   *         
* Exit:- DLCBFLX set to Person ID Name (Fisrt/Middle/Last)            *         
***********************************************************************         
EDTPFNM  NTR1  LABEL=NO                                                         
         CLC   LPID#,0(R1)         Test change of person                        
         JE    EDTPFN90                                                         
         BRAS  RE,EDTPID                                                        
EDTPFN90 MVC   DLCBFLX(L'LPIDFNAM),LPIDFNAM                                     
         J     EXITY                                                            
                                                                                
EDTPMNM  NTR1  LABEL=NO                                                         
         CLC   LPID#,0(R1)         Test change of person                        
         JE    EDTPMN90                                                         
         BRAS  RE,EDTPID                                                        
EDTPMN90 MVC   DLCBFLX(L'LPIDMNAM),LPIDMNAM                                     
         J     EXITY                                                            
                                                                                
EDTPLNM  NTR1  LABEL=NO                                                         
         CLC   LPID#,0(R1)         Test change of person                        
         JE    EDTPLN90                                                         
         BRAS  RE,EDTPID                                                        
EDTPLN90 MVC   DLCBFLX(L'LPIDLNAM),LPIDLNAM                                     
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Edit Progam                                                         *         
*                                                                     *         
* Ntry:- R1=A(Program number)                                         *         
*                                                                     *         
* Exit:- DLCBFLX set to Program name                                  *         
***********************************************************************         
EDTPGM   NTR1                                                                   
         MVC   BYTE1,0(R1)         PROGRAM NUMBER                               
         CLI   BYTE1,0                                                          
         JE    EDTPGM05                                                         
*                                                                               
         CLI   SYS,SYSSPTQ                                                      
         JNE   *+14                                                             
         LARL  RF,SPOTPGM                                                       
         J     EDTPGM01                                                         
*                                                                               
         CLI   SYS,SYSNETQ                                                      
         JNE   *+14                                                             
         LARL  RF,NETPGM                                                        
         J     EDTPGM01                                                         
*                                                                               
         CLI   SYS,SYSPRTQ                                                      
         JNE   *+14                                                             
         LARL  RF,PRNTPGM                                                       
         J     EDTPGM01                                                         
*                                                                               
         CLI   SYS,SYSTALQ                                                      
         JNE   *+14                                                             
         LARL  RF,TALPGM                                                        
         J     EDTPGM01                                                         
*                                                                               
         CLI   SYS,SYSSECQ                                                      
         JNE   *+14                                                             
         LARL  RF,CTLPGM                                                        
         J     EDTPGM01                                                         
*                                                                               
         CLI   SYS,SYSCONQ                                                      
         JNE   *+14                                                             
         LARL  RF,CTLPGM                                                        
         J     EDTPGM01                                                         
*                                                                               
         DC    H'0'                UNKNOWN SYSTEM                               
*                                                                               
EDTPGM01 DS    0H                                                               
         XR    R0,R0                                                            
         ICM   R0,3,0(RF)          LENGTH OF A PROGRAM TABLE ENTRY              
         L     RE,2(RF)            END OF PROGRAMS TABLE                        
         LA    RF,6(RF)            SKIP TABLE ENTRY LENGTH, TABLE END           
*                                                                               
         USING PGMLSTD,RF                                                       
*                                                                               
         CLI   0(R1),0                                                          
         JE    EDTPGM05                                                         
*                                                                               
EDTPGM02 CR    RE,RF               PAST EOT?                                    
         JL    EDTPGM05                                                         
*                                                                               
         CLC   BYTE1,PGMNUM                                                     
         JE    *+10                                                             
         AR    RF,R0                                                            
         J     EDTPGM02                                                         
*                                                                               
* *NB*                                                                          
* PROGRAM NAME(PGMNAME) IN FATABPGUS(PGMLSTD) IS 7 CHARACTERS LONG,             
* IN EXTRACT IT IS 8 (DP_NAME), SO LAST CHARACTER WILL BE POPULATED             
* BY C' '                                                                       
         MVC   DLCBFLX(L'PGMNAME),PGMNAME                                       
         MVI   DLCBFLX+L'DP_NAME-1,C' '                                         
         J     EDTPGM10            CHECK FOR EXCEPTIONS                         
         DROP  RF                                                               
*                                                                               
* PROGRAM NOT IN THE TABLE HERE                                                 
*                                                                               
EDTPGM05 DS    0H                                                               
         MVC   DLCBFLX(4),=CL4'PGM='                                            
         GOTOR VHEXOUT,DMCB,BYTE1,DLCBFLX+4,1                                   
*                                                                               
* HANDLE EXCEPTIONS HERE                                                        
*                                                                               
EDTPGM10 DS    0H                                                               
         L     RF,DI_AIO1          A(RECORD)                                    
*                                                                               
* FOR CLIENT AND PRODUCT RECORDS, FOR SPOT AND NET                              
* ALL CHANGES BY EXTERNAL APPLICATIONS WILL RESULT IN PROGRAM NAME              
* "ORGNZR"                                                                      
*                                                                               
         CLI   SYS,SYSSPTQ         SYSTEM=SPOT?                                 
         JNE   EDTPGM11            CHECK IF SYSTEM=NET                          
         CLI   BYTE1,X'1E'         PGM=LINK?                                    
         JNE   EDTPGM20            NEXT EXCEPTION                               
         J     EDTPGM12            CHECK RECORD TYPE                            
*                                                                               
EDTPGM11 CLI   SYS,SYSNETQ         NET?                                         
         JNE   EDTPGM20            NEXT EXCEPTION                               
         CLI   BYTE1,X'18'         PGM=NNAV?                                    
         JNE   EDTPGM20            NEXT EXCEPTION                               
*                                                                               
EDTPGM12 CLI   0(RF),X'00'         RECORD TYPE = X'00'?                         
         JNE   EDTPGM20            NEXT EXCEPTION                               
         OC    7(6,RF),7(RF)       6 BINARY ZEROES AFTER PKEYPRD                
         JNZ   EDTPGM20            NEXT EXCEPTION                               
*                                                                               
         MVC   DLCBFLX(L'DP_NAME),=CL8'ORGNZR'                                  
         J     EXIT                                                             
*                                                                               
EDTPGM20 DS    0H                                                               
* FOR CLIENT AND PRODUCT RECORDS, FOR PRINT SYSTEM                              
* ALL CHANGES BY EXTERNAL APPLICATIONS WILL RESULT IN PROGRAM NAME              
* "ORGNZR"                                                                      
         CLI   SYS,SYSPRTQ                                                      
         JNE   EDTPGM30            NEXT EXCEPTION                               
         CLI   BYTE1,X'14'         PGM=ADB?                                     
         JNE   EDTPGM30            NEXT EXCEPTION                               
         CLI   3(RF),X'02'         PRINT CLIENT RECORD?                         
         JE    *+12                                                             
         CLI   3(RF),X'06'         PRINT PRODUCT RECORD?                        
         JNE   EDTPGM30            NEXT EXCEPTION                               
*                                                                               
         MVC   DLCBFLX(L'DP_NAME),=CL8'ORGNZR'                                  
         J     EXIT                                                             
*                                                                               
EDTPGM30 DS    0H                                                               
* FOR CONTROL, PERSONAL AUTH, AND PERSON RECORDS, PROGRAM IS "SECURE"           
         CLI   SYS,SYSCONQ                                                      
         JNE   EDTPGM40            NEXT EXCEPTION                               
         CLI   0(RF),SA0KTYPQ                                                   
         JE    *+14                                                             
         CLC   0(2,RF),=AL1(SAPETYPQ,SAPESUBQ)                                  
         JNE   EDTPGM40            NEXT EXCEPTION                               
*                                                                               
         MVC   DLCBFLX(L'DP_NAME),=CL8'SECURE'                                  
         J     EXIT                                                             
*                                                                               
EDTPGM40 DS    0H                                                               
* SPOT AND PRINT PROG NAME TABLES IN SPDFAR AND PPDFAR, RESPECTIVELY            
* HAD PROGRAM NAMES THAT DID NOT MATCH THOSE IN FATABPGUS                       
* I WILL STICK TO THE ALREADY ESTABLISHED NAMING SO NOT TO UPSET                
* THE CLIENTS                                                                   
         CLI   SYS,SYSSPTQ                                                      
         JNE   EDTPGM50            NEXT EXCEPTION                               
         CLI   BYTE1,X'31'         PGM=SUPERDESK?                               
         JNE   EDTPGM50            NEXT EXCEPTION                               
         MVC   DLCBFLX(L'DP_NAME),=CL8'SUPERDSK'                                
         J     EXIT                                                             
*                                                                               
EDTPGM50 DS    0H                                                               
         CLI   SYS,SYSPRTQ                                                      
         JNE   EDTPGM60            NEXT EXCEPTION                               
         CLI   BYTE1,X'14'         PGM=SUPERDESK?                               
         JNE   EDTPGM60            NEXT EXCEPTION                               
         MVC   DLCBFLX(L'DP_NAME),=CL8'ADB'                                     
         J     EXIT                                                             
*                                                                               
EDTPGM60 DS    0H                                                               
         CLI   BYTE1,X'D4'         PGM=D4, market fix?                          
         JNE   EDTPGM70            NEXT EXCEPTION                               
         MVC   DLCBFLX(L'DP_NAME),=CL8'MKTFIX'                                  
         J     EXIT                                                             
*                                                                               
EDTPGM70 DS    0H                                                               
         CLI   SYS,SYSSPTQ         SYSTEM=SPOT?                                 
         JNE   EDTPGM80            NEXT EXCEPTION                               
         CLI   BYTE1,X'00'         PGM=0, OVERNIGHT REPORT?                     
         JNE   EDTPGM80            NEXT EXCEPTION                               
         CLI   0(RF),C'S'          RECORD = STA MASTER?                         
         JNE   EDTPGM80            NEXT EXCEPTION                               
         CLI   2(RF),C'0'          CABLE RECORD?                                
         JL    EDTPGM80            NEXT EXCEPTION                               
         MVC   DLCBFLX(L'DP_NAME),=CL8'CABLUPD'                                 
         J     EXIT                                                             
*                                                                               
EDTPGM80 DS    0H                                                               
         J     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
*&&DO                                                                           
EDTPGM   ICM   RF,15,DH_APGM                                                    
         BZR   RE                                                               
         CLI   0(R1),0                                                          
         JE    EDTPGM05                                                         
         MVC   BYTE1,0(R1)                                                      
         USING DP_D,RF             RF=A(program table)                          
EDTPGM02 CLI   DP_D,DP_EOTQ                                                     
         JE    EDTPGM05                                                         
         CLC   DP_PGM,BYTE1                                                     
         JE    *+12                                                             
         AHI   RF,DP_LNQ                                                        
         J     EDTPGM02                                                         
         MVC   DLCBFLX(L'DP_NAME),DP_NAME                                       
         BR    RE                                                               
         DROP  RF                                                               
                                                                                
EDTPGM05 L     RF,DI_AIO2          IS THIS '0'PWD OR PID RECORD                 
         CLI   0(RF),SA0KTYPQ                                                   
         JE    EDTPGM08                                                         
         CLC   0(2,RF),=AL1(SAASTYPQ,SAPESUBQ)                                  
         JNE   EDTPGM09                                                         
EDTPGM08 MVI   BYTE1,X'0D'         ASSUME 'SECURE' PROGRAM                      
         L     RF,DH_APGM          RELOAD A(PROGRAM LIST TABLE)                 
         J     EDTPGM02                                                         
EDTPGM09 DS    0H                                                               
         MVC   DLCBFLX(4),=CL4'PGM='                                            
         LR    R0,RE                                                            
         GOTOR VHEXOUT,DMCB,BYTE1,DLCBFLX+4,1                                   
         LR    RE,R0                                                            
         BR    RE                                                               
*&&                                                                             
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* Edit Record Action                                                  *         
*                                                                     *         
* Ntry:- R1=A(DS_TYTPE)                                               *         
*                                                                     *         
* Exit:- DLCBFLX set to record action                                 *         
***********************************************************************         
                                                                                
EDTACT   LLC   RF,0(R1)                                                         
         MHI   RF,L'ACTTAB                                                      
         LA    RF,ACTTAB-L'ACTTAB(RF)                                           
         MVC   DLCBFLX(L'ACTTAB),0(RF)                                          
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* Edit record key                                                     *         
*                                                                     *         
* Exit:- DI_RKEY set to printable record key                          *         
***********************************************************************         
                                                                                
EDTKEY   NTR1  LABEL=NO                                                         
         L     R2,DI_ADRD                                                       
         USING DR_D,R2             R2=A(record definition)                      
         MVC   DI_RKEY,SPACES      Set record key to spaces                     
                                                                                
         TM    DR_INDS1,DR_IKFRQ   Test handler formats key                     
         JZ    EDTKEY02                                                         
         LARL  RE,EXIT             Call handler key edit routine                
         NTR1  LABEL=NO                                                         
         ICM   RF,15,DR_AKEY       RF=A(handler key edit routine)               
         JNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R1,DI_D             R1=A(interface block)                        
         LM    R2,RB,DI_R2RB       Restore handler's R2 thru RB                 
         GOTOR (RF),(R1)           Handler may reject record by setting         
         J     EXIT                The condition code                           
                                                                                
EDTKEY02 ICM   R2,15,DR_AKEY                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING DK_D,R2             R2=A(key definition table)                   
         LA    R3,DI_RKEY                                                       
EDTKEY04 CLI   DK_D,DK_EOTQ        Test end of key definition table             
         JE    EDTKEY26                                                         
         LLC   R1,DK_DSP                                                        
         A     R1,DI_AIO1          Point to field                               
         ST    R1,DI_AINP          Set A(input)                                 
         ST    R3,DI_AOUT          Set A(output)                                
         MVI   DI_LOUT,0           Initialize output length                     
                                                                                
         TM    DK_TYPE,DK_TSAVQ    Test saving this value                       
         JZ    EDTKEY06                                                         
         SR    R0,R0                                                            
         ICM   R0,3,DK_WDSP                                                     
         A     R0,DI_AWRK          R0=A(value in handler work area)             
         SR    R1,R1                                                            
         ICM   R1,1,DK_LEN         R1=length of field                           
         JNZ   *+6                                                              
         DC    H'0'                                                             
         L     RE,DI_AINP                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     R1,DI_AINP                                                       
                                                                                
EDTKEY06 MVC   BYTE1,DK_TYPE       Isolate data type                            
         NI    BYTE1,LOBITS                                                     
         CLI   BYTE1,DK_TEXOQ      Test extract value only                      
         JE    EDTKEY24                                                         
         CLI   BYTE1,DK_TSHEQ      Test edit routine                            
         JE    EDTKEY08                                                         
         CLI   BYTE1,DK_TMEBQ      Test media binary                            
         JE    EDTKEY12                                                         
         CLI   BYTE1,DK_TCHRQ      Test characters                              
         JE    EDTKEY16                                                         
         CLI   BYTE1,DK_TBINQ      Test unsigned binary                         
         JE    EDTKEY18                                                         
         DC    H'0'                                                             
                                                                                
EDTKEY08 LARL  RE,EDTKEY10         Call system handler edit routine             
         NTR1  LABEL=NO                                                         
         ICM   RF,15,DK_ROUT       RF=A(edit routine)                           
         JNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R1,DI_D             R1=A(interface block)                        
         LM    R2,RB,DI_R2RB       Restore handler's R2 thru RB                 
         GOTOR (RF),(R1)                                                        
         J     EXIT                                                             
                                                                                
EDTKEY10 LLC   R0,DI_LOUT          Get edited field length                      
         AR    R3,R0               Point to next output character               
         J     EDTKEY20                                                         
                                                                                
EDTKEY12 L     RE,DI_ADAD          Edit media                                   
         AHI   RE,DA_MEDS-DA_D                                                  
         USING DA_MEDS,RE          RE=A(media table)                            
         MVC   WORK,0(R1)                                                       
         NI    WORK,DA_MBITQ                                                    
         LHI   R0,DA_MMAXQ                                                      
EDTKEY14 CLI   DA_MEDS,DA_MEOTQ    Test end of table                            
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   DA_MNUM,WORK        Match media number                           
         JE    *+14                                                             
         AHI   RE,DA_MLNQ                                                       
         JCT   R0,EDTKEY14                                                      
         DC    H'0'                                                             
         MVC   0(L'DA_MCOD,R3),DA_MCOD                                          
         AHI   R3,L'DA_MCOD                                                     
         J     EDTKEY20                                                         
                                                                                
EDTKEY16 SR    RF,RF               Edit characters                              
         ICM   RF,1,DK_LEN                                                      
         JNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R3),0(R1)                                                    
         EX    RF,0(RE)                                                         
         LA    R3,1(RF,R3)                                                      
         J     EDTKEY20                                                         
                                                                                
EDTKEY18 SR    RE,RE               Edit unsigned binary                         
         ICM   RE,1,DK_LEN                                                      
         LHI   RF,1                                                             
         SLL   RF,0(RE)                                                         
         BCTR  RF,0                                                             
         SR    R0,R0                                                            
         BASR  RE,0                                                             
         ICM   R0,0,0(R1)                                                       
         EX    RF,0(RE)            Get value into R0                            
         LTR   R0,R0                                                            
         JZ    EDTKEY22            Nothing output if null                       
         EDITR (R0),(10,(R3)),0,ALIGN=LEFT                                      
         AR    R3,R0                                                            
         J     EDTKEY20                                                         
                                                                                
EDTKEY20 BCTR  R3,0                Locate end of data                           
         CLI   0(R3),SPACE                                                      
         JNH   EDTKEY20                                                         
         AHI   R3,1                Point to next character                      
                                                                                
EDTKEY22 MVI   0(R3),COMMA         Set key delimiter                            
         AHI   R3,1                                                             
                                                                                
EDTKEY24 AHI   R2,DK_LNQ           Bump to next table entry                     
         J     EDTKEY04                                                         
                                                                                
EDTKEY26 LA    R0,DI_RKEY                                                       
         CR    R3,R0               Ensure something formatted                   
         JNE   *+6                                                              
         DC    H'0'                Record must have a key                       
         BCTR  R3,0                                                             
                                                                                
EDTKEY28 CLI   0(R3),COMMA         Remove trailing delimter(s)                  
         JNE   EXITY                                                            
         MVI   0(R3),SPACE                                                      
         JCT   R3,EDTKEY28                                                      
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Routine to load phases                                              *         
*                                                                     *         
* Ntry:- R1=A(Parameter list) as follows:-                            *         
*                                                                     *         
*        P1       - N/A                                               *         
*        P2 B0    - See equates below                                 *         
*        P2 B1-3  - X'0SPPOO' if CALLOV, AL3(phase name) if EBCDIC    *         
***********************************************************************         
                                                                                
CALLOVQ  EQU   C'R'                CALLOV style load                            
LOADQ    EQU   C'E'                Regular style load (EBCDIC name)             
PATCHQ   EQU   C'P'                Patch card                                   
VERIFYQ  EQU   C'V'                Verify card                                  
                                                                                
GETPHS   NTR1  BASE=*,LABEL=*,WORK=(RC,GPWORKL)                                 
         USING GPWORKD,RC          RC=A(local w/s)                              
         XC    GPWORKD(GPWORKL),GPWORKD                                         
                                                                                
         LARL  R9,LITERALS         Restore registers                            
         LARL  R8,PREGS                                                         
         LM    R8,RA,0(R8)                                                      
                                                                                
         LR    R4,R1               R4=A(calling parameter list)                 
         MVC   GPPHSID,SPACES      Set unknown phase id                         
         XC    0(4,R4),0(R4)       Clear P1                                     
                                                                                
         CLI   4(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         SR    R5,R5                                                            
         MVC   GPPHASE,SPACES                                                   
         MVI   GPTEST,SPACE                                                     
         ICM   R1,7,5(R4)                                                       
         CLI   4(R4),LOADQ         Test EBCDIC phase name                       
         BE    GETPHS02                                                         
         CLI   4(R4),PATCHQ        Test processing patch card                   
         BE    GETPHS02                                                         
         CLI   4(R4),VERIFYQ       Test processing verify card                  
         BE    GETPHS02                                                         
         CLI   4(R4),CALLOVQ       Test regular call                            
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR VHEXOUT,GPMCPARA,5(R4),GPPHASE,3,HEXTOG                          
         MVI   GPPHASE,C'T'                                                     
         CLC   T00A(3),4(R4)       Test loading T00A phase                      
         BNE   GETPHS08                                                         
         LARL  RF,T00ATAB          Yes - look up name in table                  
         LHI   R0,T00ATABN                                                      
         BASR  RE,0                                                             
         CLC   0(1,RF),7(R4)                                                    
         BE    *+14                                                             
         AHI   RF,L'T00ATAB                                                     
         BCTR  R0,RE                                                            
         B     GETPHS08                                                         
         MVC   GPPHSID,1(RF)                                                    
         B     GETPHS08                                                         
                                                                                
GETPHS02 LA    RF,0(R1)            RF=A(phase name(test level)+patch)           
         LA    RE,GPPHASE                                                       
         LHI   R0,GPPHASEM                                                      
GETPHS04 CLI   0(RF),SPACE         Look for space                               
         BE    GETPHS08            Found - no test version                      
         CLI   0(RF),SLASH         Look for delimiters                          
         BE    GETPHS06                                                         
         CLI   0(RF),COMMA                                                      
         BE    GETPHS06                                                         
         CLI   0(RF),PLUS                                                       
         BNE   *+10                                                             
         LR    R5,RF                                                            
         B     GETPHS08                                                         
         MVC   0(1,RE),0(RF)                                                    
         AHI   RE,1                                                             
         AHI   RF,1                                                             
         BCT   R0,GETPHS04                                                      
         B     GETPHS08                                                         
                                                                                
GETPHS06 MVC   GPTEST,1(RF)        Move in test version                         
         MVC   0(1,RE),GPTEST      Set it in phase name                         
         CLI   2(RF),PLUS          Test followed by patch                       
         BNE   GETPHS08                                                         
         LA    R5,2(RF)                                                         
                                                                                
GETPHS08 LR    R0,RE               Save A(test level) in name                   
         LA    R1,GPPHASE                                                       
         CLI   4(R4),PATCHQ        Test processing a patch card                 
         BE    *+12                                                             
         CLI   4(R4),VERIFYQ       Test processing a verify card                
         BNE   *+8                                                              
         ICM   R1,8,=AL1(GPMXLOAD) Yes - phase must be loaded already           
         GOTOR LOADIT,(R1)         Try to load phase                            
         BNZ   GETPHS12            Found it                                     
         CLI   4(R4),PATCHQ        Test processing a patch card                 
         BE    GETPHSN             Yes - error                                  
         CLI   4(R4),VERIFYQ       Test processing a verify card                
         BE    GETPHSN             Yes - error                                  
         MVC   P(L'LODLIT1),LODLIT1                                             
         MVC   P+L'LODLIT1(GPPHASEM),GPPHASE                                    
         MVC   P+L'LODLIT1+GPPHASEM+1(L'GPPHSID),GPPHSID                        
         CLI   GPTEST,SPACE                                                     
         BE    GETPHS10                                                         
         MVC   P(L'LODLIT3),LODLIT3                                             
         MVC   P+L'LODLIT3(GPPHASEM),GPPHASE                                    
         MVC   P+L'LODLIT3+GPPHASEM+1(L'GPPHSID),GPPHSID                        
                                                                                
GETPHS10 GOTOR VPRINT                                                           
         CLI   GPTEST,SPACE        Was it a test version?                       
         BE    GETPHS12                                                         
         LR    RE,R0                                                            
         MVI   0(RE),SPACE         Yes - go for production version              
         MVI   GPTEST,SPACE                                                     
         GOTOR LOADIT,GPPHASE                                                   
         BNZ   GETPHS12                                                         
         MVC   P(L'LODLIT1),LODLIT1                                             
         MVC   P+L'LODLIT1(GPPHASEM),GPPHASE                                    
         MVC   P+L'LODLIT1+GPPHASEM+1(L'GPPHSID),GPPHSID                        
         GOTOR VPRINT                                                           
                                                                                
GETPHS12 CLI   GPTEST,SPACE        Remove test level from phase name            
         JE    *+10                                                             
         LR    RE,R0                                                            
         MVI   0(RE),SPACE                                                      
         MVC   0(L'GPADDR,R4),GPADDR                                            
         MVC   8(L'GPPHLN,R4),GPPHLN                                            
         OC    GPADDR,GPADDR                                                    
         BZ    GETPHSN2                                                         
         LTR   R5,R5                                                            
         BZ    GETPHSY                                                          
         AHI   R5,1                                                             
         LA    RF,1(R5)                                                         
         LA    R1,L'GPDUB-1                                                     
         CLI   0(RF),EQUAL                                                      
         BE    *+16                                                             
         AHI   RF,1                                                             
         BCT   R1,*-12                                                          
         B     GETPHSN                                                          
                                                                                
         MVI   GPDUB,C'0'                                                       
         MVC   GPDUB+1(L'GPDUB-1),GPDUB                                         
         LA    RE,GPDUB(R1)                                                     
         LHI   RF,L'GPDUB                                                       
         SR    RF,R1                                                            
         BCTR  RF,0                                                             
         MVC   0(0,RE),0(R5)                                                    
         EX    RF,*-6                                                           
         LA    R5,2(R5,RF)                                                      
         GOTOR VHEXIN,DMCB,GPDUB,GPWORK,L'GPDUB                                 
         OC    12(4,R1),12(R1)                                                  
         BZ    GETPHSN                                                          
         CLC   GPPHLN,GPWORK                                                    
         BL    GETPHSN                                                          
         MVC   GPDUB(4),GPWORK                                                  
                                                                                
         LR    RF,R5                                                            
         LHI   R1,48                                                            
         SR    R0,R0                                                            
         BASR  RE,0                                                             
         CLI   0(RF),SPACE                                                      
         BE    GETPHS14                                                         
         AHI   RF,1                                                             
         AHI   R0,1                                                             
         BCTR  R1,RE                                                            
         B     GETPHSN                                                          
                                                                                
GETPHS14 LTR   R0,R0                                                            
         BZ    GETPHSN                                                          
         GOTOR VHEXIN,DMCB,(R5),GPWORK,(R0)                                     
         ICM   RF,15,12(R1)                                                     
         BZ    GETPHSN                                                          
         L     RE,GPDUB                                                         
         A     RE,GPADDR                                                        
         BCTR  RF,0                                                             
         STM   RE,RF,GPSVRERF                                                   
         CLI   4(R4),LOADQ         Test processing a load card                  
         BE    *+12                                                             
         CLI   4(R4),PATCHQ        Test processing a patch card                 
         BNE   GETPHS16                                                         
         LR    R0,RE                                                            
         MVC   P(L'PCHLIT),PCHLIT                                               
         GOTOR VHEXOUT,DMCB,(R0),P+L'PCHLIT,1(RF),HEXTOG                        
         GOTOR VPRINT                                                           
         LM    RE,RF,GPSVRERF                                                   
         MVC   0(0,RE),GPWORK      Set new value                                
         EX    RF,*-6                                                           
         B     GETPHSY                                                          
                                                                                
GETPHS16 CLI   4(R4),VERIFYQ       Test processing a verify card                
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   0(0,RE),GPWORK      Compare actual value with string             
         EX    RF,*-6                                                           
         JE    EXIT                                                             
         LR    R0,RE                                                            
         MVC   P(L'VERLIT),VERLIT                                               
         GOTOR VHEXOUT,DMCB,(R0),P+L'VERLIT,1(RF),HEXTOG                        
         GOTOR VPRINT                                                           
         J     EXITN                                                            
                                                                                
GETPHSN  MVC   P(L'LODLIT4),LODLIT4                                             
         GOTOR VPRINT                                                           
         XC    GPADDR,GPADDR                                                    
                                                                                
GETPHSN2 J     EXITN                                                            
                                                                                
GETPHSY  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Look up phase table to see if the phase has already been loaded -   *         
* if it has pass back its known address, if not load the phase and    *         
* create a phase table entry for it if successful                     *         
***********************************************************************         
                                                                                
LOADIT   NTR1  LABEL=NO                                                         
         STCM  R1,8,GPMODE         Save calling mode                            
         LA    R4,0(R1)            R4=A(phase name)                             
         XC    GPVALS(GPVALSL),GPVALS                                           
         CLI   0(R4),ASTERISK      Test want to patch using map name            
         BE    LOADIT08                                                         
                                                                                
         LARL  R5,PHSTAB           Test phase aready loaded                     
         USING PHSTABD,R5                                                       
         LHI   R0,PHSTABN          R0=N'entries in phase table                  
         LHI   RE,L'PHSTNAME-1     Set up compare length                        
         LA    RF,L'PHSTNAME-1(R4)                                              
         CLI   0(RF),SPACE                                                      
         BNE   LOADIT02                                                         
         BCTR  RF,0                                                             
         BCT   RE,*-10                                                          
                                                                                
LOADIT02 CLI   PHSTABD,PHSTEOTQ    Test end of phase table                      
         BE    LOADIT04                                                         
         CLC   PHSTNAME(0),0(R4)                                                
         EX    RE,*-6              Match name to table                          
         BE    LOADIT06            Found - return address only                  
         AHI   R5,PHSTABL          Bump to next phase table entry               
         BCT   R0,LOADIT02                                                      
         DC    H'0'                Need more space in phstab                    
                                                                                
LOADIT04 TM    GPMODE,GPMXLOAD     Test don't issue load                        
         BNZ   LOADITX                                                          
         GOTOR LOADER,GPMCPARA,(R4),0,0                                         
         OC    4(4,R1),4(R1)                                                    
         BZ    LOADITX                                                          
         MVC   PHSTNAME,0(R4)      Set phase name in table                      
         MVC   PHSTTEST,GPTEST     Set test phase                               
         MVC   PHSTADD,4(R1)       Set phase address in table                   
         MVC   PHSTLEN,0(R1)       Set phase length in table                    
                                                                                
         MVC   LL2PHSN,GPPHASE                                                  
         MVC   LL2PHAS,GPPHSID                                                  
         MVC   P(LODLIT2L),LODLIT2                                              
         LA    R0,P+LODLIT2L                                                    
         GOTOR VHEXOUT,DMCB,PHSTADDH,(R0),L'PHSTADDH,HEXTOG                     
                                                                                
         ICM   RF,15,PHSTADD                                                    
         ICM   R0,15,PHSTLEN       Locate first book, level & date info         
         SHI   R0,PHSINFLQ         Subtract length of BOOK= etc. data           
         AR    RF,R0                                                            
         SR    R1,R1                                                            
         BASR  RE,0                                                             
         CLC   BOOKPFX,00(RF)      BOOK=  at +00                                
         BNE   *+10                                                             
         CLC   LEVLPFX,16(RF)      LEVEL= at +16                                
         BNE   *+10                                                             
         CLC   DATEPFX,26(RF)      DATE=  at +26                                
         BNE   *+6                                                              
         LR    R1,RF                                                            
         BCTR  RF,0                                                             
         BCTR  R0,RE                                                            
         LTR   R1,R1               Test book info found                         
         BZ    *+10                                                             
         MVC   P+LODLIT2L+(L'PHSTADDH*2)+1(PHSINFLQ),0(R1)                      
         GOTOR VPRINT                                                           
                                                                                
         MVC   GPADDR,PHSTADD      Set output phase address                     
         MVC   GPPHLN,PHSTLEN      Set output phase lenth                       
         ICM   R1,15,PHSTNUM       Bump phase usage counter                     
         AHI   R1,1                                                             
         STCM  R1,15,PHSTNUM                                                    
         B     LOADITX                                                          
                                                                                
LOADIT06 MVC   GPADDR,PHSTADD      Set output phase address                     
         MVC   GPPHLN,PHSTLEN      Set output phase lenth                       
         MVC   P(L'LODLIT5),LODLIT5                                             
         MVC   P+L'LODLIT5(L'PHSTNAME),PHSTNAME                                 
         MVC   P+L'LODLIT5+L'PHSTNAME+1(L'GPPHSID),GPPHSID                      
         GOTOR VPRINT                                                           
         J     LOADITX                                                          
                                                                                
LOADIT08 MVC   GPADDR,AGEDFAR      Set address for gedfar patch                 
         MVC   GPPHLN,=A(GEDFARX-GEDFAR)                                        
                                                                                
LOADITX  OC    GPADDR,GPADDR       Set CC=equal if not loaded                   
         J     EXIT                                                             
         DROP  RB,RC                                                            
                                                                                
GPWORKD  DSECT                     ** GETPHS local w/s **                       
GPDUB    DS    D                                                                
GPMCPARA DS    6F                                                               
GPWORK   DS    XL80                                                             
GPSVRERF DS    2F                                                               
                                                                                
GPVALS   DS    0X                  ** Phase values **                           
GPADDR   DS    A                   A(loaded phase)                              
GPPHLN   DS    F                   L'loaded phase                               
GPVALSL  EQU   *-GPVALS                                                         
                                                                                
GPPHSID  DS    CL8                 Phase id (eg DDLINK)                         
GPPHASEM EQU   8                   Maximum length of phase name                 
GPPHASE  DS    CL12                Phase name                                   
GPTEST   DS    C                   Test level                                   
                                                                                
GPMODE   DS    X                   ** Call mode for LOADIT **                   
GPMXLOAD EQU   X'80'               Don't process load (PATCH= support)          
                                                                                
GPWORKL  EQU  *-GPWORKD                                                         
GEDFAR   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Load or remove a phase                                              *         
*                                                                     *         
* Ntry:- R1=A(Parameter list) as follows:-                            *         
*                                                                     *         
*        P1       - AL4(Phase name)                                   *         
*        P2       - AL4(0) Load phase and return address or           *         
*                   X'FFFFFFFF' to delete named phase                 *         
*                                                                     *         
* Exit:- Parameter list updated as follows if phase load (not delete) *         
*                                                                     *         
*        P1       - AL4(Phase length)                                 *         
*        P2       - AL4(Phase load address)                           *         
***********************************************************************         
                                                                                
LOADER   NTR1  BASE=*,LABEL=*                                                   
         LR    R3,R1               R3=A(parameter list)                         
         SR    R2,R2                                                            
         ICM   R2,7,1(R3)          R2=A(phase name)                             
         CLC   DELPHASE,4(R3)      Test for delete mode                         
         BE    LOADER02                                                         
         XC    0(8,R3),0(R3)       Clear values for error exit                  
         LOAD  EPLOC=(2),ERRET=LOADERX                                          
         SLL   R1,3                R1=length of module in bytes                 
         STCM  R1,7,1(R3)          Set length for user                          
         STCM  R0,15,4(R3)         Set entry address for user                   
         J     LOADERX                                                          
                                                                                
LOADER02 DELETE EPLOC=(2)          Delete phase                                 
                                                                                
LOADERX  J     EXIT                                                             
         DROP  RB                                                               
                                                                                
SETAGY   NTR1  LABEL=*                                                          
         L     R2,DI_AAGY          Find agency table entry                      
         USING DA_D,R2                                                          
         TM    IND,SYSTIXAQ        Test agency not in record                    
         JZ    SETAGY02                                                         
         MVC   AGYALPHA,DA_ALF     Use agency from first entry                  
         J     EXITY                                                            
                                                                                
SETAGY02 CLI   DA_D,DA_EOTQ        Test end of agency table                     
         JNE   SETAGY04                                                         
         TM    IND,SYSTIAAQ        Test all agencies valid                      
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   DA_ALF,AGYALPHA     Create a new entry                           
         MVC   DA_AGB,AGYBINRY                                                  
         GOTOR RESAGY,DA_D         Resolve agency values                        
         J     EXITY                                                            
                                                                                
SETAGY04 OC    AGYALPHA,AGYALPHA   Test agency alpha present                    
         JZ    SETAGY06                                                         
         CLC   DA_ALF,AGYALPHA     Yes - match agency alpha                     
         JNE   SETAGY08                                                         
         J     SETAGY10                                                         
SETAGY06 CLC   DA_AGB,AGYBINRY     Else match agency number                     
         JE    SETAGY10                                                         
SETAGY08 AHI   R2,DA_LNQ           Bump to next table entry                     
         J     SETAGY02                                                         
                                                                                
SETAGY10 MVC   AGYALPHA,DA_ALF     Set agency alpha id                          
         MVC   AGYBINRY,DA_AGB     Set agency number                            
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Resolve agency alpha values                                         *         
*                                                                     *         
* Ntry:- R1=A(DA_D)                                                   *         
***********************************************************************         
                                                                                
RESAGY   NTR1  LABEL=*                                                          
         LR    R2,R1                                                            
         USING DA_D,R2                                                          
         MVC   DA_SECAG,DA_ALF     Preset security agency                       
                                                                                
         USING CT5REC,IO           Read access record                           
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,DA_ALF                                                  
         GOTOR VDMGR,DMCB,DMREAD,CTFILE,CT5REC,CT5REC                           
         JNE   RESAGY18                                                         
                                                                                
         LA    R1,CT5DATA                                                       
         USING CTDSCD,R1                                                        
         SR    R0,R0                                                            
RESAGY02 CLI   CTDSCEL,0           Test end of record                           
         JE    RESAGY14                                                         
         CLI   CTDSCEL,CTDSCELQ    Test principal id element                    
         JE    RESAGY06                                                         
         CLI   CTDSCEL,CTSEAELQ    Test principal control id element            
         JE    RESAGY08                                                         
         CLI   CTDSCEL,CTSPUELQ    Test principal security id elelent           
         JE    RESAGY10                                                         
         CLI   CTDSCEL,CTAGCELQ    Test agency label element                    
         JE    RESAGY12                                                         
RESAGY04 IC    R0,CTDSCLEN         Bump to next element on record               
         AR    R1,R0                                                            
         J     RESAGY02                                                         
                                                                                
RESAGY06 MVC   DA_PRNID,CTDSC      Extract principal id number                  
         J     RESAGY04                                                         
                                                                                
         USING CTSEAD,R1                                                        
RESAGY08 MVC   DA_SECAG,CTSEAAID   Extract principal control id                 
         J     RESAGY04                                                         
                                                                                
         USING CTSPUD,R1                                                        
RESAGY10 MVC   DA_PRNID,CTSPUNUM   Extract principal security id                
         J     RESAGY04                                                         
                                                                                
         USING CTAGCD,R1                                                        
RESAGY12 MVC   DA_LABEL,CTAGCCOD   Extract agency label                         
         J     RESAGY04                                                         
                                                                                
         USING CTPREC,IO           Read profile record                          
RESAGY14 XC    CTPKEY,CTPKEY                                                    
         MVI   CTPKTYP,CTPKTYPQ                                                 
         MVC   CTPKSYS(L'PROF),PROF                                             
         MVC   CTPKORIG,DA_PRNID                                                
         GOTOR VDMGR,DMCB,DMREAD,CTFILE,CTPREC,CTPREC                           
         JNE   RESAGY18                                                         
                                                                                
         LA    R1,CTPDATA                                                       
         USING CTOCOD,R1           Locate permanent output type element         
         SR    R0,R0                                                            
RESAGY16 CLI   CTOCOEL,0           Test end of record                           
         JE    RESAGY18                                                         
         CLI   CTOCOEL,CTOCOELQ    Test output type element                     
         JNE   *+12                                                             
         CLI   CTOCOTYP,C'P'       Test permanent                               
         JE    *+14                                                             
         IC    R0,CTOCOLEN                                                      
         AR    R1,R0                                                            
         J     RESAGY16                                                         
         CLC   EDIOUT,CTOCODE      Test EDICT output required                   
         JNE   RESAGY18                                                         
         OI    DA_INDS,DA_IOEDI    Yes - set EDICT output indicator             
                                                                                
RESAGY18 J     EXITY                                                            
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* Call system handler routine passing A(interface block)              *         
*                                                                     *         
* Ntry:- R1=Mode to call system handler with                          *         
***********************************************************************         
                                                                                
CALLSH   NTR1  LABEL=NO                                                         
         STC   R1,DI_MODE          Set calling mode                             
         GOTOR APHASE,DI_D         Call handler passing A(DI_D) in R1           
         J     EXIT                Exit with CC intact                          
         EJECT                                                                  
EXITL    DS    0H                                                               
EXITN    LHI   RE,0                                                             
         J     EXITCC                                                           
EXITY    LHI   RE,1                                                             
         J     EXITCC                                                           
EXITH    LHI   RE,2                                                             
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
         EJECT                                                                  
*&&US                                                                           
ODATTYPE EQU   20                  DATCON output type                           
*&&                                                                             
*&&UK                                                                           
ODATTYPE EQU   23                  DATCON output type                           
*&&                                                                             
PHSINFLQ EQU   39                  L'phase info (BOOK, LEVEL & DATE)            
ONEK     EQU   1024                                                             
SLASH    EQU   C'/'                                                             
COMMA    EQU   C','                                                             
SPACE    EQU   C' '                                                             
QUOTE    EQU   C'"'                                                             
DINK     EQU   C''''                                                            
SCOLON   EQU   C';'                                                             
COLON    EQU   C':'                                                             
ASTERISK EQU   C'*'                                                             
PLUS     EQU   C'+'                                                             
EQUAL    EQU   C'='                                                             
OPAREN   EQU   C'('                                                             
CPAREN   EQU   C')'                                                             
FF       EQU   X'FF'                                                            
CCLINE   EQU   X'09'                                                            
CCPAGE   EQU   X'89'                                                            
HIBITS   EQU   X'F0'                                                            
LOBITS   EQU   X'0F'                                                            
CUREDIT  EQU   CCUREDIT            (for CURED macros)                           
                                                                                
LITERALS DS    0D                  ** Literals etc. **                          
         LTORG                                                                  
                                                                                
PREGS    DC    3A(0)               Pointer register save area                   
AIO1DATA DC    A(IO1+4+L'RECVHDR)                                               
AIO2DATA DC    A(IO2+4+L'RECVHDR)                                               
AELTAB   DC    A(ELTAB)                                                         
AGEDFAR  DC    A(GEDFAR)                                                        
ALITTAB  DC    A(LITTAB)                                                        
APQBUFF  DC    A(PQBUFF)                                                        
ATAPEIN  DC    A(RCVTAPE)                                                       
VBINSRCH DC    V(BINSRCH)                                                       
VCARDS   DC    V(CARDS)                                                         
VCPRINT  DC    V(CPRINT2)                                                       
VDATCON  DC    V(DATCON)                                                        
VDATVAL  DC    V(DATVAL)                                                        
VADDAY   DC    V(ADDAY)                                                         
VDDSIO   DC    V(DDSIO)                                                         
VDLFLD   DC    V(DLFLD)                                                         
VDMGR    DC    V(DATAMGR)                                                       
VEDITOR  DC    V(EDITOR)                                                        
VPRINT   DC    V(PRINTER)                                                       
VHEXIN   DC    V(HEXIN)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VSORTER  DC    V(SORTER)                                                        
                                                                                
PONE     DC    P'1'                                                             
PZERO    DC    P'0'                                                             
HOBOFF   DC    A(X'7FFFFFFF')                                                   
EFFS     DC    X'FFFFFFFF'                                                      
BITLIST  DC    X'8040201008040201'                                              
                                                                                
ADJTIME  DS    0PL4                ** System/real time adjustment **            
*&&US*&& DC    PL(L'ADJTIME)'60000'                                             
*&&UK*&& DC    PL(L'ADJTIME)'0'                                                 
MAXHOUR  DC    P'240000'           Midnight                                     
                                                                                
COMFACS  DC    (COMFACSN)A(0)      ** Common facilites **                       
         ORG   COMFACS+(CBINSRCH-COMFACSD)                                      
         DC    V(BINSRCH)                                                       
         ORG   COMFACS+(CDATAMGR-COMFACSD)                                      
         DC    V(DATAMGR)                                                       
         ORG   COMFACS+(CCALLOV-COMFACSD)                                       
         DC    A(GETPHS)                                                        
         ORG   COMFACS+(CHELLO-COMFACSD)                                        
         DC    V(HELLO)                                                         
         ORG   COMFACS+(CHEXIN-COMFACSD)                                        
         DC    V(HEXIN)                                                         
         ORG   COMFACS+(CHEXOUT-COMFACSD)                                       
         DC    V(HEXOUT)                                                        
         ORG   COMFACS+(CDATVAL-COMFACSD)                                       
         DC    V(DATVAL)                                                        
         ORG   COMFACS+(CDATCON-COMFACSD)                                       
         DC    V(DATCON)                                                        
         ORG   COMFACS+(CADDAY-COMFACSD)                                        
         DC    V(ADDAY)                                                         
         ORG   COMFACS+(CGETDAY-COMFACSD)                                       
         DC    V(GETDAY)                                                        
         ORG   COMFACS+(CGETPROF-COMFACSD)                                      
         DC    V(GETPROF)                                                       
         ORG   COMFACS+(CCUREDIT-COMFACSD)                                      
         DC    V(CUREDIT)                                                       
         ORG                                                                    
                                                                                
DI_BLOCK DS    0D                  ** Handler interface block **                
         DC    (DI_LNQ)X'00'                                                    
         ORG   DI_BLOCK+(DI_AWRK-DI_D)                                          
         DC    A(SYSWRK)                                                        
         ORG   DI_BLOCK+(DI_ACOM-DI_D)                                          
         DC    A(COMFACS)                                                       
         ORG   DI_BLOCK+(DI_APUT-DI_D)                                          
         DC    A(OUTPUT)                                                        
         ORG   DI_BLOCK+(DI_AAGY-DI_D)                                          
         DC    A(AGYTAB)                                                        
         ORG   DI_BLOCK+(DI_AIO1-DI_D)                                          
         DC    A(IO1)                                                           
         ORG   DI_BLOCK+(DI_AIO2-DI_D)                                          
         DC    A(IO2)                                                           
         ORG   DI_BLOCK+(DI_ABFIN-DI_D)                                         
         DC    V(BUFFERIN)                                                      
         ORG                                                                    
                                                                                
DMOPEN   DC    C'DMOPEN '                                                       
DMREAD   DC    C'DMREAD '                                                       
                                                                                
PQFILE   DC    C'PRTQUE '                                                       
PQOPEN   DC    C'OPEN   '                                                       
PQPRINT  DC    C'DMPRINT'                                                       
PQCLOSE  DC    C'CLOSE  '                                                       
                                                                                
BOOKPFX  DC    C'BOOK='                                                         
LEVLPFX  DC    C'LEVEL='                                                        
DATEPFX  DC    C'DATE='                                                         
                                                                                
DATAHED1 DC    C'Element Key'                                                   
DATAHED2 DC    C'Data Name'                                                     
DATAHED3 DC    C'Old Value'                                                     
DATAHED4 DC    C'New Value'                                                     
                                                                                
NODATA   DC    C'No activity today'                                             
                                                                                
EDIOUT   DC    C'EDICT'                                                         
                                                                                
HEXTOG   DC    C'TOG'                                                           
DELPHASE DC    X'FFFFFFFF'                                                      
T00A     DC    AL1(CALLOVQ),X'000A'                                             
                                                                                
TITLIT   DC    C'Generalized Daily File Activity Extract'                       
                                                                                
VALLIT1  DC    C'Input parameter cards'                                         
VALLIT2  DC    C'---------------------'                                         
VALLIT3  DC    C' - Invalid parameter card format - s/b KEYWORD=DATA'           
VALLIT4  DC    C' - Control card not recognised'                                
VALLIT5  DC    C' - Invalid parameter value'                                    
VALLIT6  DC    C'SYSTEM= parameter card missing'                                
VALLIT7  DC    C'KEYCOL= parameter card missing'                                
                                                                                
LODLIT1  DC    C'Request for load failed:- '                                    
                                                                                
LODLIT2  DS    0C                                                               
         DC    C'Phase '                                                        
LL2PHSN  DC    C'xxxxxxxx'                                                      
         DC    C' '                                                             
LL2PHAS  DC    C'yyyyyyyy'                                                      
         DC    C' successfully loaded at '                                      
LODLIT2L EQU   *-LODLIT2                                                        
                                                                                
LODLIT3  DC    C'Request for test phase load failed:- '                         
LODLIT4  DC    C'Invalid patch format s/b +disp(1-8)=hex value'                 
LODLIT5  DC    C'Load of phase avoided:- '                                      
                                                                                
PCHLIT   DC    C'Patching storage - current value='                             
VERLIT   DC    C'Verify failed - actual value='                                 
                                                                                
SYSLIT1  DC    C'Can''t read system list record - run aborted'                  
                                                                                
CONSYS   DC    C'CONTROL'          ** Control system name **                    
                                                                                
CONFILES DC    C'N'                ** Control system file list **               
CTFILE   DC    C'CTFILE '                                                       
         DC    C'N'                                                             
GENDIR   DC    C'GENDIR '                                                       
         DC    C'N'                                                             
GENFIL   DC    C'GENFIL '                                                       
         DC    C'X'                                                             
CONFILEL EQU   *-CONFILES                                                       
                                                                                
SORTPAR1 DS    0C                  ** Sort parameter card 1 **                  
         DC    C'SORT FIELDS=('                                                 
SORTKDSP DS    CL3                 Displacement to start of key                 
         DC    C','                                                             
SORTKLEN DS    CL3                 Length of sort key                           
         DC    C',A),FORMAT=BI,WORK=1 '                                         
                                                                                
SORTPAR2 DS    0C                  ** Sort parameter card 2 **                  
         DC    C'RECORD TYPE=V,LENGTH=('                                        
SORTRLEN DS    CL4                 Maximum record length                        
         DC    C',,,,) '                                                        
                                                                                
SORTPUT  DC    C'PUT'              Put record to sort                           
SORTGET  DC    C'GET'              Get record from sort                         
SORTEND  DC    C'END'              Terminate sort                               
                                                                                
SORTPUTS DC    PL4'0'              Number of records put to sort                
                                                                                
ACTTAB   DS    0CL8                ** Action table **                           
         DC    CL(L'ACTTAB)'Add'                                                
         DC    CL(L'ACTTAB)'Delete'                                             
         DC    CL(L'ACTTAB)'Restore'                                            
         DC    CL(L'ACTTAB)'Change'                                             
                                                                                
UTL      DC    XL256'00'                                                        
         ORG   UTL+4                                                            
UTLSE    DC    AL1(SYSCONQ)        Control system                               
         ORG                                                                    
                                                                                
SSB      DS    0F                                                               
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSB                                                              
         DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    AL1(FF)             Set extended offline SSB                     
         ORG                                                                    
         EJECT                                                                  
RCVTAPE  DCB   DDNAME=TAPEIN,DSORG=PS,RECFM=VB,LRECL=8200,             +        
               BLKSIZE=0,MACRF=GM,EODAD=GETEOF                                  
                                                                                
         DS    0H                                                               
SYSTAB   DS    0XL(SYSTABL)        ** System table **                           
*&&US                                                                           
         DC    C'SP'                                                            
         DC    AL1(SYSSPTQ)                                                     
         DC    AL1(0)                                                           
         DC    CL(L'SYSTPROF)'SFE'                                              
         DC    CL(L'SYSTPSPP)'SFE'                                              
         DC    CL(L'SYSTNAME)'Spot'                                             
         DC    CL(L'SYSTEDIN)'SPT'                                              
         DC    CL(L'SYSTPHAS)'SPDFAR'                                           
                                                                                
         DC    C'NE'                                                            
         DC    AL1(SYSNETQ)                                                     
         DC    AL1(0)                                                           
         DC    CL(L'SYSTPROF)'SFE'                                              
         DC    CL(L'SYSTPSPP)'NFE'                                              
         DC    CL(L'SYSTNAME)'Net'                                              
         DC    CL(L'SYSTEDIN)'NET'                                              
         DC    CL(L'SYSTPHAS)'NEDFAR'                                           
                                                                                
         DC    C'PR'                                                            
         DC    AL1(SYSPRTQ)                                                     
         DC    AL1(0)                                                           
         DC    CL(L'SYSTPROF)'PFE'                                              
         DC    CL(L'SYSTPSPP)'PFE'                                              
         DC    CL(L'SYSTNAME)'Print'                                            
         DC    CL(L'SYSTEDIN)'PRT'                                              
         DC    CL(L'SYSTPHAS)'PPDFAR'                                           
                                                                                
         DC    C'TA'                                                            
         DC    AL1(SYSTALQ)                                                     
         DC    AL1(SYSTIXAQ)                                                    
         DC    CL(L'SYSTPROF)'TFE'                                              
         DC    CL(L'SYSTPSPP)'TFE'                                              
         DC    CL(L'SYSTNAME)'Talent'                                           
         DC    CL(L'SYSTEDIN)'TAL'                                              
         DC    CL(L'SYSTPHAS)'TLDFAR'                                           
*&&                                                                             
         DC    C'SE'                                                            
         DC    AL1(SYSSECQ)                                                     
         DC    AL1(SYSTIAAQ)                                                    
         DC    CL(L'SYSTPROF)'CFE'                                              
         DC    CL(L'SYSTPSPP)'CFE'                                              
         DC    CL(L'SYSTNAME)'Security'                                         
         DC    CL(L'SYSTEDIN)'SEC'                                              
         DC    CL(L'SYSTPHAS)'SEDFAR'                                           
                                                                                
SYSTABN  EQU   (*-SYSTAB)/SYSTABL                                               
                                                                                
SYSTABD  DSECT                     ** Table of system prefixes **               
SYSTPFX  DS    CL2                 System prefix                                
SYSTSYS  DS    X                   Native system number                         
SYSSPTQ  EQU   X'02'               Spot system                                  
SYSNETQ  EQU   X'03'               Network system                               
SYSPRTQ  EQU   X'04'               Print system                                 
SYSTALQ  EQU   X'07'               Talent system                                
SYSSECQ  EQU   X'0A'               Security system                              
SYSCONQ  EQU   X'0A'               Control system                               
SYSTIND  DS    X                   ** Indicator byte **                         
SYSTIAAQ EQU   X'80'               All agency alphas valid                      
SYSTIXAQ EQU   X'40'               Use first entry in agency table              
SYSTPROF DS    CL3                 System/program for profile record            
SYSTPSPP DS    CL3                 System/program for file name                 
SYSTNAME DS    CL8                 System name                                  
SYSTEDIN DS    CL3                 System value for EDICT                       
SYSTPHAS DS    CL8                 System handler phase name                    
SYSTABL  EQU   *-SYSTABD                                                        
GEDFAR   CSECT                                                                  
                                                                                
         DS    0H                                                               
VALTAB   DS    0XL(VALTABL)        ** Parameter keyword table **                
         DC    C'DDSIO       ',AL3(VALDIO)                                      
         DC    C'DSPACE      ',AL3(VALDSP)                                      
         DC    C'TODAY       ',AL3(VALTOD)                                      
         DC    C'DATES       ',AL3(VALDAT)                                      
         DC    C'SYSTEM      ',AL3(VALSYS)                                      
         DC    C'LOAD        ',AL3(VALLOD)                                      
         DC    C'VERIFY      ',AL3(VALVFY)                                      
         DC    C'PATCH       ',AL3(VALPCH)                                      
         DC    C'AGENCY      ',AL3(VALAGY)                                      
         DC    C'FILE        ',AL3(VALFIL)                                      
         DC    C'RECORD      ',AL3(VALREC)                                      
         DC    C'KEYCOL      ',AL3(VALKCD)                                      
         DC    C'KEYLIT      ',AL3(VALKLV)                                      
         DC    C'REPORT      ',AL3(VALPQR)                                      
         DC    C'REPDESC     ',AL3(VALPQD)                                      
         DC    C'REPTITLE    ',AL3(VALPQT)                                      
         DC    C'REPWIDTH    ',AL3(VALRWD)                                      
         DC    C'REPCLASS    ',AL3(VALPQC)                                      
         DC    C'REPREF      ',AL3(VALPRF)                                      
         DC    C'RETAIN      ',AL3(VALRTN)                                      
         DC    C'TRACE       ',AL3(VALTRC)                                      
         DC    C'DELIMS      ',AL3(VALDEL)                                      
         DC    C'OMODE       ',AL3(VALOMO)                                      
VALTABX  DC    AL1(VALTEOTQ)                                                    
                                                                                
VALTABD  DSECT                     ** Dsect for VALTAB above **                 
VALTEOTQ EQU   0                   End of table indicator                       
VALTKWRD DS    CL12                Keyword                                      
VALTROUT DS    AL3                 Validation routine                           
VALTABL  EQU   *-VALTABD           Length of table entry                        
GEDFAR   CSECT                                                                  
                                                                                
         DS    0H                                                               
       ++INCLUDE DMPRTQC                                                        
                                                                                
         DS    0H                                                               
KCDTAB   DS    0XL(KCDTABL)        ** Key column definitions **                 
                                                                                
         DC    AL1(KCDTCAGY)                                                    
         DC    CL(L'KCDTNAME)'AGENCY'                                           
         DC    CL(L'KCDTCOLN)'Agency'                                           
         DC    AL1(0)                                                           
         DC    AL1(KCDTFWRK)                                                    
         DC    AL2(DS_WORK+(DS_ALF-DS_D)-WORKD)                                 
         DC    AL1(L'DS_ALF)                                                    
         DC    AL1(KCDTCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    AL1(L'DS_ALF)                                                    
                                                                                
         DC    AL1(KCDTCSYS)                                                    
         DC    CL(L'KCDTNAME)'SYSTEM'                                           
         DC    CL(L'KCDTCOLN)'System'                                           
         DC    AL1(0)                                                           
         DC    AL1(KCDTFWRK)                                                    
         DC    AL2(SYSNAME-WORKD)                                               
         DC    AL1(L'SYSNAME)                                                   
         DC    AL1(KCDTCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    AL1(L'SYSNAME)                                                   
                                                                                
         DC    AL1(KCDTCFIL)                                                    
         DC    CL(L'KCDTNAME)'FILE'                                             
         DC    CL(L'KCDTCOLN)'File'                                             
         DC    AL1(0)                                                           
         DC    AL1(KCDTFWRK)                                                    
         DC    AL2(FILNAME-WORKD)                                               
         DC    AL1(L'FILNAME)                                                   
         DC    AL1(KCDTCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    AL1(L'FILNAME)                                                   
                                                                                
         DC    AL1(KCDTCREC)                                                    
         DC    CL(L'KCDTNAME)'RECORD'                                           
         DC    CL(L'KCDTCOLN)'Record'                                           
         DC    AL1(0)                                                           
         DC    AL1(KCDTFWRK)                                                    
         DC    AL2(RECNAME-WORKD)                                               
         DC    AL1(L'RECNAME)                                                   
         DC    AL1(KCDTCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    AL1(L'RECNAME)                                                   
                                                                                
         DC    AL1(KCDTCUID)                                                    
         DC    CL(L'KCDTNAME)'USERID'                                           
         DC    CL(L'KCDTCOLN)'User-ID'                                          
         DC    AL1(0)                                                           
         DC    AL1(KCDTFWRK)                                                    
         DC    AL2(DS_WORK+(DS_UID-DS_D)-WORKD)                                 
         DC    AL1(L'DS_UID)                                                    
         DC    AL1(KCDTEDRQ)                                                    
         DC    AL4(EDTUID)                                                      
         DC    AL1(L'CTIKID)                                                    
                                                                                
         DC    AL1(KCDTCPID)                                                    
         DC    CL(L'KCDTNAME)'PID'                                              
         DC    CL(L'KCDTCOLN)'Person ID'                                        
         DC    AL1(0)                                                           
         DC    AL1(KCDTFWRK)                                                    
         DC    AL2(DS_WORK+(DS_PID-DS_D)-WORKD)                                 
         DC    AL1(L'DS_PID)                                                    
         DC    AL1(KCDTEDRQ)                                                    
         DC    AL4(EDTPID)                                                      
         DC    AL1(L'SAPALPID)                                                  
                                                                                
         DC    AL1(KCDTCFNM)                                                    
         DC    CL(L'KCDTNAME)'FNAME'                                            
         DC    CL(L'KCDTCOLN)'First Name'                                       
         DC    AL1(0)                                                           
         DC    AL1(KCDTFWRK)                                                    
         DC    AL2(DS_WORK+(DS_PID-DS_D)-WORKD)                                 
         DC    AL1(L'DS_PID)                                                    
         DC    AL1(KCDTEDRQ)                                                    
         DC    AL4(EDTPFNM)                                                     
         DC    AL1(L'LPIDFNAM)                                                  
                                                                                
         DC    AL1(KCDTCMNM)                                                    
         DC    CL(L'KCDTNAME)'MNAME'                                            
         DC    CL(L'KCDTCOLN)'Middle Name'                                      
         DC    AL1(0)                                                           
         DC    AL1(KCDTFWRK)                                                    
         DC    AL2(DS_WORK+(DS_PID-DS_D)-WORKD)                                 
         DC    AL1(L'DS_PID)                                                    
         DC    AL1(KCDTEDRQ)                                                    
         DC    AL4(EDTPMNM)                                                     
         DC    AL1(L'LPIDMNAM)                                                  
                                                                                
         DC    AL1(KCDTCMNM)                                                    
         DC    CL(L'KCDTNAME)'LNAME'                                            
         DC    CL(L'KCDTCOLN)'Last Name'                                        
         DC    AL1(0)                                                           
         DC    AL1(KCDTFWRK)                                                    
         DC    AL2(DS_WORK+(DS_PID-DS_D)-WORKD)                                 
         DC    AL1(L'DS_PID)                                                    
         DC    AL1(KCDTEDRQ)                                                    
         DC    AL4(EDTPLNM)                                                     
         DC    AL1(L'LPIDLNAM)                                                  
                                                                                
         DC    AL1(KCDTCPRG)                                                    
         DC    CL(L'KCDTNAME)'PROGRAM'                                          
         DC    CL(L'KCDTCOLN)'Program'                                          
         DC    AL1(0)                                                           
         DC    AL1(KCDTFWRK)                                                    
         DC    AL2(DS_WORK+(DS_PGM-DS_D)-WORKD)                                 
         DC    AL1(L'DS_PGM)                                                    
         DC    AL1(KCDTEDRQ)                                                    
         DC    AL4(EDTPGM)                                                      
         DC    AL1(L'DP_NAME)                                                   
                                                                                
         DC    AL1(KCDTCACT)                                                    
         DC    CL(L'KCDTNAME)'ACTION'                                           
         DC    CL(L'KCDTCOLN)'Action'                                           
         DC    AL1(0)                                                           
         DC    AL1(KCDTFWRK)                                                    
         DC    AL2(DS_WORK+(DS_TYPE-DS_D)-WORKD)                                
         DC    AL1(L'DS_TYPE)                                                   
         DC    AL1(KCDTEDRQ)                                                    
         DC    AL4(EDTACT)                                                      
         DC    AL1(8)                                                           
                                                                                
         DC    AL1(KCDTCDAT)                                                    
         DC    CL(L'KCDTNAME)'DATE'                                             
         DC    CL(L'KCDTCOLN)'Date'                                             
         DC    AL1(0)                                                           
         DC    AL1(KCDTFWRK)                                                    
         DC    AL2(DS_WORK+(DS_DATE-DS_D)-WORKD)                                
         DC    AL1(L'DS_DATE)                                                   
         DC    AL1(KCDTBDTQ)                                                    
         DC    AL4(0)                                                           
         DC    AL1(8)                                                           
                                                                                
         DC    AL1(KCDTCTIM)                                                    
         DC    CL(L'KCDTNAME)'TIME'                                             
         DC    CL(L'KCDTCOLN)'Time'                                             
         DC    AL1(0)                                                           
         DC    AL1(KCDTFWRK)                                                    
         DC    AL2(DS_WORK+(DS_TIME-DS_D)-WORKD)                                
         DC    AL1(L'DS_TIME)                                                   
         DC    AL1(KCDTPTMQ)                                                    
         DC    AL4(0)                                                           
         DC    AL1(8)                                                           
                                                                                
         DC    AL1(KCDTCLID)                                                    
         DC    CL(L'KCDTNAME)'LUID'                                             
         DC    CL(L'KCDTCOLN)'LUID'                                             
         DC    AL1(0)                                                           
         DC    AL1(KCDTFWRK)                                                    
         DC    AL2(LUID-WORKD)                                                  
         DC    AL1(L'LUID)                                                      
         DC    AL1(KCDTCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    AL1(L'LUID)                                                      
                                                                                
         DC    AL1(KCDTCTKT)                                                    
         DC    CL(L'KCDTNAME)'TICKET'                                           
         DC    CL(L'KCDTCOLN)'Ticket#'                                          
         DC    AL1(0)                                                           
         DC    AL1(KCDTFWRK)                                                    
         DC    AL2(TICKET-WORKD)                                                
         DC    AL1(L'TICKET)                                                    
         DC    AL1(KCDTCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    AL1(L'TICKET)                                                    
                                                                                
         DC    AL1(KCDTCKEY)                                                    
         DC    CL(L'KCDTNAME)'KEY'                                              
         DC    CL(L'KCDTCOLN)'Record Key'                                       
         DC    AL1(0)                                                           
         DC    AL1(KCDTFDID)                                                    
         DC    AL2(DI_RKEY-DI_D)                                                
         DC    AL1(L'DI_RKEY)                                                   
         DC    AL1(KCDTCHRQ)                                                    
         DC    AL4(0)                                                           
         DC    AL1(L'DI_RKEY)                                                   
                                                                                
         DC    AL1(KCDTCSEQ)                                                    
         DC    CL(L'KCDTNAME)'SEQNO'                                            
         DC    CL(L'KCDTCOLN)'Sequence#'                                        
         DC    AL1(0)                                                           
         DC    AL1(KCDTFWRK)                                                    
         DC    AL2(SUBSEQN-WORKD)                                               
         DC    AL1(L'SUBSEQN)                                                   
         DC    AL1(KCDTBINQ)                                                    
         DC    AL4(0)                                                           
         DC    AL1(0)                                                           
                                                                                
KCDTABX  DC    AL1(KCDTEOTQ)                                                    
                                                                                
KCDTABD  DSECT                     ** Key column definition table **            
KCDTEOTQ EQU   0                   End of table indicator                       
                                                                                
KCDTCOL# DS    X                   ** Column number **                          
KCDTCAGY EQU   1                   Agency aplha                                 
KCDTCSYS EQU   2                   System                                       
KCDTCFIL EQU   3                   File                                         
KCDTCREC EQU   4                   Record                                       
KCDTCUID EQU   5                   User-id                                      
KCDTCPID EQU   6                   PID                                          
KCDTCFNM EQU   7                   First Name                                   
KCDTCMNM EQU   8                   Middle Name                                  
KCDTCLNM EQU   9                   Last Name                                    
KCDTCPRG EQU   10                  Program                                      
KCDTCACT EQU   11                  Action                                       
KCDTCDAT EQU   12                  Date                                         
KCDTCTIM EQU   13                  Time                                         
KCDTCLID EQU   14                  LUID                                         
KCDTCTKT EQU   15                  Ticket                                       
KCDTCKEY EQU   16                  Key                                          
KCDTCSEQ EQU   17                  Sequence number                              
                                                                                
KCDTNAME DS    CL8                 Key field id                                 
KCDTCOLN DS    CL12                Column name                                  
                                                                                
KCDTINDS DS    X                   ** Indicators **                             
                                                                                
KCDTFBLK DS    AL1                 ** Block data is in **                       
KCDTFWRK EQU   1                   Data is in WORKD                             
KCDTFDID EQU   2                   Data is in DI_D                              
                                                                                
KCDTFDSP DS    AL2                 Displacement to data in block                
KCDTFLEN DS    AL1                 Length of data                               
                                                                                
KCDTTYPE DS    AL1                 ** Data type **                              
KCDTEDRQ EQU   0                   Edit routine (KCDTAEDT)                      
KCDTCHRQ EQU   1                   Character string                             
KCDTBDTQ EQU   2                   Binary date                                  
KCDTPTMQ EQU   3                   Packed time (hhmmss)                         
KCDTBINQ EQU   4                   Binary value                                 
                                                                                
KCDTAEDT DS    AL4                 A(edit routine) (if KCDTEDTQ)                
KCDTOLEN DS    AL1                 Length of output                             
KCDTABL  EQU   *-KCDTABD                                                        
GEDFAR   CSECT                                                                  
                                                                                
         DS    0H                                                               
T00ATAB  DS    0CL9                ** Core resident phase names **              
         DC    AL1(QBOOKVAL),CL8'BOOKVAL '                                      
         DC    AL1(QCENTER),CL8'CENTER  '                                       
         DC    AL1(QCHOPPER),CL8'CHOPPER '                                      
         DC    AL1(QDAYVAL),CL8'DAYVAL  '                                       
         DC    AL1(QDEMCON),CL8'DEMCON  '                                       
         DC    AL1(QDEMEX),CL8'DEMEX   '                                        
         DC    AL1(QDEMOTAB),CL8'DEMOTAB '                                      
         DC    AL1(QDEMVAL),CL8'DEMVAL  '                                       
         DC    AL1(QREDEMUP),CL8'REDEMUP '                                      
         DC    AL1(QINVEDIT),CL8'INVEDIT '                                      
         DC    AL1(QCFMIO),CL8'CFMIO   '                                        
         DC    AL1(QPAVEXPL),CL8'PAVEXPL '                                      
         DC    AL1(QSPOOL),CL8'SPOOL   '                                        
         DC    AL1(QSQUASH),CL8'SQUASH  '                                       
         DC    AL1(QTIMVAL),CL8'TIMVAL  '                                       
         DC    AL1(QUNDAY),CL8'UNDAY   '                                        
         DC    AL1(QDAYUNPK),CL8'DAYUNPK '                                      
         DC    AL1(QUNDRLIN),CL8'UNDRLIN '                                      
         DC    AL1(QUNTIME),CL8'UNTIME  '                                       
         DC    AL1(QXSORT),CL8'XSORT   '                                        
         DC    AL1(QUPVAL),CL8'UPVAL   '                                        
         DC    AL1(QCLPACK),CL8'CLPACK  '                                       
         DC    AL1(QCLUNPK),CL8'CLUNPK  '                                       
         DC    AL1(QNETUNIV),CL8'NETUNIV '                                      
         DC    AL1(QNETWEEK),CL8'NETWEEK '                                      
         DC    AL1(QNETGTSP),CL8'NETGTSP '                                      
         DC    AL1(QNEUTIL),CL8'NEUTIL  '                                       
         DC    AL1(QPARSNIP),CL8'PARSNIP '                                      
         DC    AL1(QMSPACK),CL8'MSPACK  '                                       
         DC    AL1(QMSUNPK),CL8'MSUNPK  '                                       
         DC    AL1(QGETBROD),CL8'GETBROAD'                                      
         DC    AL1(QGETBRD),CL8'GETBRD  '                                       
         DC    AL1(QNETUNBK),CL8'NETUNBK '                                      
         DC    AL1(QDARREPS),CL8'DARETAB '                                      
         DC    AL1(QGETDEM1),CL8'GETDEM1 '                                      
         DC    AL1(QGETDEM2),CL8'GETDEM2 '                                      
         DC    AL1(QSPDEMUP),CL8'SPDEMUP '                                      
         DC    AL1(QNETSPVL),CL8'NETSPVL '                                      
         DC    AL1(QSPGTIUN),CL8'SPGETIUN'                                      
         DC    AL1(QSPOON),CL8'SPOON   '                                        
         DC    AL1(QDEFINE),CL8'DEFINE  '                                       
         DC    AL1(QNETIO),CL8'NETIO   '                                        
         DC    AL1(QNETVALU),CL8'NETVALU '                                      
         DC    AL1(QREGTIUN),CL8'REGETIUN'                                      
         DC    AL1(QGENCON),CL8'GENCON  '                                       
         DC    AL1(QDICTCON),CL8'DICTCON '                                      
         DC    AL1(QGETNUN),CL8'GETNUN  '                                       
         DC    AL1(QGETHUT),CL8'GETHUT  '                                       
         DC    AL1(QNETGOAL),CL8'NETGOAL '                                      
         DC    AL1(QDRIVAL),CL8'DRIVAL  '                                       
         DC    AL1(QGENPRG),CL8'GENPRG  '                                       
         DC    AL1(QOFFICER),CL8'OFFICER '                                      
         DC    AL1(QDRONE),CL8'DRONE   '                                        
         DC    AL1(QDRIVER),CL8'DRIVER  '                                       
         DC    AL1(QBUFFOON),CL8'BUFFOON '                                      
         DC    AL1(QPRVAL),CL8'PRVAL   '                                        
         DC    AL1(QMEDGEN),CL8'MEDGEN  '                                       
         DC    AL1(QPRVAL2),CL8'PRVAL2  '                                       
         DC    AL1(QSPACNVL),CL8'SPACNVL '                                      
         DC    AL1(QSPOTIO),CL8'SPOTIO  '                                       
         DC    AL1(QSPOTDRV),CL8'SPOTDRV '                                      
         DC    AL1(QRANSID),CL8'RANSID  '                                       
         DC    AL1(QSPOTBUY),CL8'SPOTBUY '                                      
         DC    AL1(QSPOTBK),CL8'SPOTBK  '                                       
         DC    AL1(QNEWRIGN),CL8'NEWRIGEN'                                      
         DC    AL1(QWBSIO),CL8'WBSIO   '                                        
         DC    AL1(QPRNTIO),CL8'PRNTIO  '                                       
         DC    AL1(QSPOTMKR),CL8'SPOTMAKR'                                      
         DC    AL1(QPRWRIGN),CL8'PRWRIGEN'                                      
         DC    AL1(QQSORT),CL8'QSORT   '                                        
         DC    AL1(QSPOTGL),CL8'SPOTGL  '                                       
         DC    AL1(QSPWRIGN),CL8'SPWRIGEN'                                      
         DC    AL1(QPODDRIV),CL8'PODDRIV '                                      
         DC    AL1(QPDWRIGN),CL8'PDWRIGEN'                                      
         DC    AL1(QSPOTSLK),CL8'SPOTSLK '                                      
         DC    AL1(QTSAR),CL8'TSAR    '                                         
         DC    AL1(QSPADINT),CL8'SPADINT '                                      
         DC    AL1(QGETRATE),CL8'GETRATE '                                      
         DC    AL1(QOFFAL),CL8'OFFAL   '                                        
         DC    AL1(QADDTRN),CL8'ADDTRN  '                                       
         DC    AL1(QSETLOCK),CL8'SETLOCK '                                      
         DC    AL1(QRFPIO),CL8'RFPIO   '                                        
         DC    AL1(QSTAVAL),CL8'STAVAL  '                                       
         DC    AL1(QPRORATA),CL8'PRORATA '                                      
         DC    AL1(QWLEVEL),CL8'WLEVEL  '                                       
         DC    AL1(QPSTVAL),CL8'PSTVAL  '                                       
         DC    AL1(QBATFAC4),CL8'BATFAC4 '                                      
         DC    AL1(QBATFAC3),CL8'BATFAC3 '                                      
         DC    AL1(QBATFAC2),CL8'BATFAC2 '                                      
         DC    AL1(QBATFAC1),CL8'BATFAC1 '                                      
         DC    AL1(QNODIO),CL8'NODIO   '                                        
         DC    AL1(QEDITOR),CL8'EDITOR  '                                       
         DC    AL1(QMINIO),CL8'MINIO   '                                        
         DC    AL1(QMOBILE),CL8'MOBILE  '                                       
         DC    AL1(QREQTWA),CL8'REQTWA  '                                       
         DC    AL1(QSPGETBU),CL8'SPGETBUY'                                      
         DC    AL1(QDBLBOOK),CL8'DBLBOOK '                                      
         DC    AL1(QPWCALC),CL8'PWCALC  '                                       
         DC    AL1(QSTAPACK),CL8'STAPACK '                                      
         DC    AL1(QBLDMGA),CL8'BLDMGA  '                                       
         DC    AL1(QGETCTA),CL8'GETCTA  '                                       
         DC    AL1(QTSAROFF),CL8'TSAROFF '                                      
         DC    AL1(QLOCKET),CL8'LOCKET  '                                       
         DC    AL1(QBLDMGE),CL8'BLDMGE  '                                       
         DC    AL1(QACCGEN),CL8'ACCGEN  '                                       
         DC    AL1(QPROGEN),CL8'PROGEN  '                                       
         DC    AL1(QGETOPT),CL8'GETOPT  '                                       
         DC    AL1(QJOBBER),CL8'JOBBER  '                                       
         DC    AL1(QTASYSIO),CL8'TASYSIO '                                      
         DC    AL1(QTASYSVL),CL8'TASYSVL '                                      
         DC    AL1(QTASYSTB),CL8'TASYSTB '                                      
         DC    AL1(QTAREPGN),CL8'TAREPGN '                                      
         DC    AL1(QTASYSDR),CL8'TASYSDR '                                      
         DC    AL1(QDROOL),CL8'DROOL   '                                        
         DC    AL1(QTASYSES),CL8'TASYSES '                                      
         DC    AL1(QTASYSCA),CL8'TASYSCA '                                      
         DC    AL1(QTACONCU),CL8'TACONCU '                                      
         DC    AL1(QTACONPR),CL8'TACONPR '                                      
         DC    AL1(QCABLETB),C'CABLETAB'                                        
         DC    AL1(QSCROLL),CL8'SCROLL  '                                       
         DC    AL1(QPRWROFF),CL8'PRWROFF '                                      
         DC    AL1(QPRHELP),CL8'PRHELP  '                                       
         DC    AL1(QREFETCH),CL8'REFETCH '                                      
         DC    AL1(QCASHIER),CL8'CASHIER '                                      
         DC    AL1(QREVAL),CL8'REVAL   '                                        
         DC    AL1(QREVAL2),CL8'REVAL2  '                                       
         DC    AL1(QRENWIO),CL8'RENWIO  '                                       
         DC    AL1(QRENWOFF),CL8'RENWOFF '                                      
         DC    AL1(QRENWTER),CL8'RENWTER '                                      
         DC    AL1(QGETINS),CL8'GETINS  '                                       
         DC    AL1(QREPFACS),CL8'REPFACS '                                      
         DC    AL1(QRENWGEN),CL8'RENWGEN '                                      
         DC    AL1(QSOFDAT),CL8'SOFDAT  '                                       
         DC    AL1(QEDIMAP),CL8'EDIMAP  '                                       
         DC    AL1(QSPAUTH),CL8'SPAUTH  '                                       
         DC    AL1(QPUBVAL),CL8'PUBVAL  '                                       
         DC    AL1(QPUBEDIT),CL8'PUBEDIT '                                      
         DC    AL1(QPPGETCG),CL8'PPGETCG '                                      
         DC    AL1(QPGETADR),CL8'PGETADR '                                      
         DC    AL1(QRCPACK),CL8'RCPACK  '                                       
         DC    AL1(QPOSTWRK),CL8'POSTWRK '                                      
         DC    AL1(QREKFACS),CL8'REKFACS '                                      
         DC    AL1(QGETCAP),CL8'GETCAP  '                                       
         DC    AL1(QSUPERD),CL8'SUPERD  '                                       
         DC    AL1(QDDISP),CL8'DDISP   '                                        
         DC    AL1(QDMAST),CL8'DMASTER '                                        
         DC    AL1(QDFORM),CL8'DFORMULA'                                        
         DC    AL1(QDNAME),CL8'DNAME   '                                        
         DC    AL1(QDCODE),CL8'DCODE   '                                        
         DC    AL1(QDCONT),CL8'DCONTROL'                                        
         DC    AL1(QDADJS),CL8'DADJUST '                                        
         DC    AL1(QDEMOVAL),CL8'DEMOVAL '                                      
         DC    AL1(QDEMOMTH),CL8'DEMOMTH '                                      
         DC    AL1(QDEMEL),CL8'DEMEL   '                                        
         DC    AL1(QDEMAINT),CL8'DEMAINT '                                      
         DC    AL1(QDEMAND),CL8'DEMAND  '                                       
         DC    AL1(QDEMADDR),CL8'DEMADDR '                                      
         DC    AL1(QDEMOUT),CL8'DEMOUT  '                                       
         DC    AL1(QDEMOCON),CL8'DEMOCON '                                      
         DC    AL1(QDEMAND1),CL8'DEMAND1 '                                      
         DC    AL1(QFALINK),CL8'FALINK  '                                       
         DC    AL1(QDDLINK),CL8'DDLINK  '                                       
         DC    AL1(QREPORT),CL8'REPORT  '                                       
         DC    AL1(QGETIDS),CL8'GETIDS  '                                       
         DC    AL1(QPERVAL),CL8'PERVAL  '                                       
         DC    AL1(QGENIDS),CL8'GENIDS  '                                       
         DC    AL1(QLINKIO),CL8'LINKIO '                                        
T00ATABN EQU   (*-T00ATAB)/L'T00ATAB                                            
                                                                                
GEDFARX  DS    0X                  End of patchable area                        
                                                                                
         DS    0H                                                               
FILFILT  DC    (4*ONEK)X'00'       File filters                                 
                                                                                
FILFD    DSECT                     ** File/record filter table **               
FILFEOTQ EQU   0                   End of file filter table                     
FILFLEN  DS    AL1                 Length of this entry                         
FILFFNUM DS    AL1                 File number                                  
FILFAREC DS    AL(L'DF_AREC)       A(record table)                              
FILFLNQ  EQU   *-FILFD                                                          
FILFRECS DS    0AL1                Record (index) number(s)                     
GEDFAR   CSECT                                                                  
                                                                                
         DS    0D                                                               
         DC    C'*PHSTAB**PHSTAB**PHSTAB**PHSTAB*'                              
PHSTAB   DC    (PHSTABN)XL(PHSTABL)'00'                                         
                                                                                
PHSTABD  DSECT                     ** Dsect for phase table above **            
PHSTEOTQ EQU   0                   End of table indicator                       
PHSTNAME DS    CL8                 Phase name                                   
PHSTTEST DS    C                   Test level loaded                            
PHSTADDH DS    0XL(L'PHSTADD)      For HEXOUT calls                             
PHSTADD  DS    AL4                 A(phase)                                     
PHSTLEN  DS    AL4                 L'phase                                      
PHSTNUM  DS    XL4                 N'times called to load                       
PHSTABL  EQU   *-PHSTABD           Length of table entry                        
PHSTABN  EQU   256                 N'loadable phases                            
GEDFAR   CSECT                                                                  
                                                                                
         DS    0D                                                               
         DC    C'*AGYTAB**AGYTAB**AGYTAB**AGYTAB*'                              
AGYMAXQ  EQU   512                 Maximum N'agency table entries               
AGYTAB   DC    (AGYMAXQ)XL(DA_LNQ)'00',X'00'                                    
                                                                                
         DS    0D                                                               
         DC    C'*KCDEFN**KCDEFN**KCDEFN**KCDEFN*'                              
KCDEFNQ  EQU   64                  Maximum N'KCDEFN table entries               
KCDEFN   DC    (KCDEFNQ)XL(KCDEFNL)'00'                                         
                                                                                
KCDEFND  DSECT                     ** Key column definitions **                 
KCDEEOTQ EQU   0                   End of table indictator                      
KCDEFTYP DS    AL1                 Entry type                                   
KCDEFTKC EQU   1                   Keyword column (KEYCOL)                      
KCDEFTLV EQU   2                   Literal value  (KEYLIT)                      
KCDEFDSP DS    AL2                 Displacement to output field                 
KCDEFLEN DS    AL1                 Override output length                       
KCDEFLIT DS    0AL4                A(literal value) (in LITTAB)                 
KCDEFTAB DS    AL4                 A(KCDTAB entry)                              
KCDEFNL  EQU   *-KCDEFND           Length of table entry                        
GEDFAR   CSECT                                                                  
                                                                                
         DS    0D                                                               
         DC    C'**ELTAB***ELTAB***ELTAB***ELTAB*'                              
ELTAB    DC    (ELMAXN)XL(ELWORKL)'00'                                          
                                                                                
         DS    0D                                                               
         DC    C'*LITTAB**LITTAB**LITTAB**LITTAB*'                              
LITTAB   DC    (4*ONEK)X'00'                                                    
                                                                                
         DS    0D                                                               
         DC    C'**IO1*****IO1*****IO1*****IO1***'                              
IO1      DC    (8*ONEK)X'00'                                                    
                                                                                
         DS    0D                                                               
         DC    C'**IO2*****IO2*****IO2*****IO2***'                              
IO2      DC    (8*ONEK)X'00'                                                    
                                                                                
         DS    0D                                                               
         DC    C'*PQBUFF**PQBUFF**PQBUFF**PQBUFF*'                              
PQBUFF   DC    (14*ONEK)X'00'      Print queue buffer                           
                                                                                
         DS    0D                                                               
         DC    C'**WORK****WORK****WORK****WORK**'                              
WORKAREA DS    (300*ONEK)X                                                      
                                                                                
         DS    0D                                                               
         DC    C'*SYSWRK**SYSWRK**SYSWRK**SYSWRK*'                              
SYSWRK   DC    (64*ONEK)X'00'                                                   
*                                                                               
*                                                                               
*                                                                               
       ++INCLUDE FATABPGUS                                                      
*                                                                               
*                                                                               
*                                                                               
WORKD    DSECT                     ** Working storage **                        
                                                                                
DUB      DS    0D                                                               
DUB1     DS    D                                                                
DUB2     DS    D                                                                
FULL     DS    F                                                                
DMCB     DS    6F                                                               
PARA     DS    6F                                                               
HALF     DS    0H                                                               
HALF1    DS    H                                                                
HALF2    DS    H                                                                
BYTE1    DS    X                                                                
BYTE2    DS    X                                                                
BYTE3    DS    X                                                                
BYTE4    DS    X                                                                
WORK     DS    XL256                                                            
                                                                                
APHASE   DS    A                   A(system handler phase)                      
AFIFNTRY DS    A                   A(file filter table entry)                   
ARECNEXT DS    A                   A(next DR_D entry in table)                  
AKCDLAST DS    A                   A(end of KCDEFN table)                       
ALITLAST DS    A                   A(end of LITTAB)                             
ADDENTRY DS    A                   A(DD entry) (multi element mode)             
                                                                                
TODAYB   DS    XL3                 Today's date (or DATE) (binary)              
TODAYE   DS    CL8                 Today's date           (ejulian)             
STRDATEB DS    XL3                 Start date for records (binary)              
ENDDATEB DS    XL3                 End date for records   (binary)              
STRDATEJ DS    PL4                 Start date for records (julian)              
ENDDATEJ DS    PL4                 End date for records   (julian)              
ENDDATEE DS    CL8                 End date for records   (ejulian)             
STRDATEE DS    CL8                 Start date for records (ejulian)             
DATEJ    DS    PL4                 Current recovery day   (ejulian)             
                                                                                
RUNFLAG  DS    X                   ** Run flag **                               
RUNFPUTQ EQU   X'80'               PUTLIN s/r called                            
                                                                                
RELDAY   DS    H                   Relative day for recovery record             
DAYMASK  DS    XL64                Recovery day mask (max. 512 days)            
SVALF    DS    CL(L'DS_ALF)        Saved DS_ALF value (OUTPUT s/r)              
SVDATE   DS    CL(L'DS_DATE)       Saved DS_DATE value (OUTPUT s/r)             
SVMODE   DS    XL(L'OUTMODE)       Saved mode (OUTPUT s/r)                      
                                                                                
SYSNAME  DS    CL(L'SYSTNAME)      System name                                  
FILNAME  DS    CL(L'DF_NAMEF)      File name                                    
RECNAME  DS    CL(L'DR_NAMES)      Record name                                  
TICKET   DS    CL(L'DS_LUID)       Ticket                                       
LUID     DS    CL(L'DS_LUID)       LUID                                         
SUBSEQN  DS    H                   Record sub-sequence number                   
                                                                                
OUTMODE  DS    X                   ** OUTPUT s/r mode **                        
OUTMNULL EQU   X'80'               Send 'null' agency report                    
OUTMLAST EQU   X'40'               Last time call to output                     
OUTMDAYS EQU   X'20'               Sending null days                            
                                                                                
OUTFLAG  DS    X                   ** OUTPUT s/r flag byte **                   
OUTFHEAD EQU   X'80'               Header record has been put                   
OUTFCALL EQU   X'40'               OUTPUT s/r called                            
                                                                                
PUTFLAG  DS    X                   ** PUTOUT s/r flag byte **                   
PUTFMEMQ EQU   X'80'               Multi element mode                           
PUTFMFMQ EQU   X'40'               Multi field mode                             
                                                                                
LALF     DS    CL(L'DS_ALF)        Last agency alpha                            
LUSERID# DS    XL(L'CTIKNUM)       Last user-id number                          
LUSERID  DS    CL(L'CTIKID)        Last user-id code                            
LPID#    DS    XL(L'SA0KNUM)       Last person id number                        
LPID     DS    CL(L'SAPALPID)      Last person id                               
LPIDFNAM DS    CL(20)              LAST person id's first name                  
LPIDMNAM DS    CL(20)              Last person id's middle name                 
LPIDLNAM DS    CL(58)              Last person id's last name                   
LAGYSEC  DS    CL(L'DA_SECAG)      Last security agency                         
                                                                                
AGYPUT#  DS    X                   Number of entries in AGYPUT                  
AGYPUT   DS    (AGYMAXQ)CL(L'DS_ALF)                                            
                                                                                
KEYDISP  DS    AL2                 Displacement to next output field            
                                                                                
ELTABN   DS    H                   Number of entries in ELTAB                   
ELMAXN   EQU   256                 Maximum number of entries in ELTAB           
                                                                                
ELWORK   DS    0X                  ** ELTAB work area **                        
ELAOLD   DS    AL4                 A(element on old record)                     
ELANEW   DS    AL4                 A(element on new record)                     
ELKEY    DS    XL32                Element key                                  
ELSEQ    DS    X                   Duplicate element sequence number            
ELKEYL   EQU   *-ELKEY                                                          
ELWORKL  EQU   *-ELWORK                                                         
                                                                                
JOINNAME DS    CL(L'DI_FLDN)       Joined field name                            
JOINDLIM DS    X                   Joined field delimiter character             
                                                                                
JOINOLEN DS    X                   Length used so far in JOINOVAL               
JOINOVAL DS    CL(L'DI_OVAL)       Joined old field value                       
                                                                                
JOINNLEN DS    X                   Length used so far in JOINNVAL               
JOINNVAL DS    CL(L'DI_NVAL)       Joined new field value                       
                                                                                
SYS      DS    XL(L'SYSTSYS)       Native system number                         
IND      DS    XL(L'SYSTIND)       System indicator byte                        
PROF     DS    CL(L'SYSTPROF)      Profile system/program                       
PSPP     DS    CL(L'SYSTPSPP)      File name system/program                     
EDIN     DS    CL(L'SYSTEDIN)      EDICT system value                           
SEN      DS    XL(L'CTLSTSEN)      Actual system number                         
PHASE    DS    CL(L'SYSTPHAS)      System handler phase                         
                                                                                
REPTITLE DS    CL(L'TITLE)         Print queue report title                     
REPID    DS    CL(L'QLSUBID)       Print queue report id                        
REPUSER  DS    XL(L'CTIKNUM)       Print queue report user-id override          
REPDESC  DS    CL(L'QLDESC)        Print queue report description               
REPCLASS DS    CL(L'QLCLASS)       Print queue report class                     
REPREF   DS    CL(L'QLSYS+L'QLPRG) PRINT QUEUE REPORT FORMS                     
RETAIN   DS    C                   PRINT QUEUE RETAIN CLASS                     
REPWIDTH DS    XL(L'QLLINEW)       Report width                                 
                                                                                
TRACE    DS    C                   Trace option                                 
DELIMS   DS    CL5                 Override delimiters                          
                                                                                
SORTUSEQ DS    AL4                 Unique sort sequence number                  
                                                                                
*AGYFILTM EQU   15                  MAXIMUM NUMBER OF AGENCY FILTERS            
AGYFILTM EQU   50                  MAXIMUM NUMBER OF AGENCY FILTERS             
AGYFILTS DS    (AGYFILTM)CL2       Agency filters                               
                                                                                
AGYVALS  DS    0X                  ** Extracted agency values **                
AGYALPHA DS    CL2                 Agency alpha id                              
AGYBINRY DS    X                   Agency number                                
AGYVALL  EQU   *-AGYVALS                                                        
                                                                                
DLCB     DS    XL(DLCBXLX)         DLFLD control block                          
                                                                                
DH_WORK  DS    XL(DH_LNQ)          System handler phase header                  
RECHD    DS    XL(RECHDLNQ)        Extracted recovery header                    
                                                                                
DS_WORK  DS    XL(DS_LNQ)          Sort record key                              
IO       DS    XL(5*ONEK)          General I/O area                             
                                                                                
WORKL    EQU   *-WORKD                                                          
                                                                                
       ++INCLUDE GEDFARD                                                        
                                                                                
* Other included books follow                                                   
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
COMFACSN EQU   (*-COMFACSD)/4                                                   
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE DDDPRINTL                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE CTGENFILE                                                      
CTLSTD   DSECT                     ** Define SE list entry **                   
         ORG   CTLSTDTA                                                         
CTLSTNAM DS    CL7                 SE name                                      
CTLSTSYS DS    X                   Callov system number                         
CTLSTSEN DS    X                   SE number                                    
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
                                                                                
RECHDD   DSECT                     ** Recovery header/trailer **                
       ++INCLUDE DMRCVRHDR                                                      
RRECPTRQ EQU   X'80'               Pointer copy/change                          
RRECCPYQ EQU   1                   Copy                                         
RRECCHGQ EQU   2                   Change                                       
RRECADDQ EQU   3                   ADD                                          
       ++INCLUDE DMRCVREXT                                                      
RECHDLNQ EQU   *-RECHDD                                                         
*                                                                               
       ++INCLUDE FAPGMLST                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012GEDFAR    05/29/19'                                      
         END                                                                    
