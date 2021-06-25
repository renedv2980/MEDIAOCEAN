*          DATA SET ACLNK09    AT LEVEL 002 AS OF 08/20/15                      
*PHASE T61F09A                                                                  
ACLNK09  TITLE '- AccPak Organizer downloads'                                   
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,REQUEST=*,CODE=CODE,SYSTEM=ACCSYSQ,FILES=FILES,  +        
               LOADFACSOFF=Y,SERVERTYPE=TSTAPAL,IDF=Y,WORKERKEY=ACCF,  +        
               SYSPHASE=SYSPHASE,                                      +        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED,B#SECD,SECD,        +        
               B#LPD,LP_D,B#ACC,ACTRECD,B#CPY,CPYRECD,B#LDG,LDGRECD)            
                                                                                
B#SECD   EQU   3                   SECD                                         
B#LPD    EQU   4                   LP_D                                         
B#CPY    EQU   5                   Company record - i/o area 1                  
B#LDG    EQU   6                   Ledger record - i/o area 2                   
B#ACC    EQU   7                   Account record - i/o area 3                  
B#OFF    EQU   8                   Office record - i/o area 4                   
                                                                                
CODE     NMOD1 0,**AL09**                                                       
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(global literals)                        
                                                                                
         LR    R5,R1                                                            
         USING LP_D,R5             R5=A(DDLINK control block)                   
         L     RE,LP_ARUNP                                                      
         USING RUNPARMD,RE         RE=A(RUNPARMS)                               
         SR    RF,RF                                                            
         ICM   RF,7,RUNPARUN                                                    
         USING RUNFACSD,RF         RF=A(RUNFACS)                                
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         BNZ   INIT02                                                           
         L     R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         L     R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
         B     INIT04                                                           
                                                                                
INIT02   L     R9,RSVRSAVE                                                      
         USING WORKD,R9            R9=A(global w/s)                             
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         LAY   R8,WORKD+OWORKL                                                  
         USING SAVED,R8            R8=A(save w/s)                               
         USING OFFALD,OFFWORK      OFFAL control block                          
         ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
                                                                                
INIT04   MVC   GETUID,RGETUID      Set A(GETUID)                                
         MVC   ACOMFACS,LP_ACOM    Set A(COMFACS)                               
         LA    R0,LP_D                                                          
         ST    R0,ALP              Save A(LP_D) for sub-routines                
         STM   R2,RB,LP_R2RB       Save registers for sub-routines              
         DROP  RB,RE,RF                                                         
         EJECT                                                                  
***********************************************************************         
* Initialize for running                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   LP_CMODE,RRUNSTRQ   Test first for run                           
         JNE   PRCWRK                                                           
                                                                                
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    RUNSTR02            No                                           
         L     RF,LP_ACOM                                                       
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('P#ROUTS1',0),0,0                                     
         MVC   LP_AUIR1,0(R1)      Set A(index routines 1)                      
         MVC   AROUTS1,LP_AUIR1                                                 
         GOTOR (RF),DMCB,('P#ROUTS2',0),0,0                                     
         MVC   LP_AUIR2,0(R1)      Set A(index routines 2)                      
         MVC   AROUTS2,LP_AUIR2                                                 
                                                                                
         GOTOR (#WRKINI,AWRKINI)   Initialize working storage                   
                                                                                
RUNSTR02 MVC   LP_BLKS+((B#SECD-1)*L'LP_BLKS),LP_ASECD                          
         MVC   LP_BLKS+((B#LPD-01)*L'LP_BLKS),ALP                               
         MVC   LP_BLKS+((B#CPY-01)*L'LP_BLKS),AIO1                              
         MVC   LP_BLKS+((B#LDG-01)*L'LP_BLKS),AIO2                              
         MVC   LP_BLKS+((B#ACC-01)*L'LP_BLKS),AIO3                              
         MVC   LP_BLKS+((B#OFF-01)*L'LP_BLKS),AIO4                              
         MVI   SYSLET,ACCLETQ      Set account system letter                    
         J     EXITY                                                            
                                                                                
***********************************************************************         
* First for new work                                                  *         
***********************************************************************         
                                                                                
PRCWRK   CLI   LP_CMODE,RPRCWRKQ   Test 'process work' mode                     
         JNE   RUNREQ                                                           
         LA    R0,QVALUES          Clear request values                         
         LHI   R1,QVALUEL                                                       
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   COMPANY,LP_AGYB     Set company code                             
         MVC   LDGLST2,LDGLST2L    Set ledger lookup list                       
         MVC   OFFRNG1,OFFRNG1L    Set 1 character office range                 
         MVC   OFFRNG2,OFFRNG2L    Set 2 character office range                 
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Run a download request                                              *         
***********************************************************************         
                                                                                
RUNREQ   CLI   LP_CMODE,RRUNREQQ   Test 'run request' mode                      
         JNE   EXITY                                                            
         TM    LP_FLAG,LP_FOFFL    Test off-line                                
         JZ    RUNREQ02                                                         
         CLC   LP_QMAPN,=AL2(I#CFMCDL)                                          
         JE    RUNREQ02            Don't issue DMKEY calls for client           
         GOTOR VDATAMGR,DMCB,DMKEY,ACCDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,ACCMST,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,ACCARC,(4,0),0                               
                                                                                
RUNREQ02 GOTOR LP_APUTO,LP_D       Call DDLINK output processor                 
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Organizer initial download (X'FFF1')                                *         
***********************************************************************         
                                                                                
REQCFMI  LKREQ *,I#CFMIDL,CFMICON,NEXTREQ=REQCFMC                               
                                                                                
CFMICON  LKOUT H                                                                
                                                                                
CFMI     LKOUT R,X'0001'           Connect values                               
LimAcs   LKOUT C,001,(D,B#LPD,LP_ACCS),HEXD                                     
UserId   LKOUT C,002,(D,B#LPD,LP_USRID),HEXD                                    
PIDNum   LKOUT C,003,(D,B#SECD,SECOPASS),HEXD                                   
AccGrp   LKOUT C,004,(D,B#SECD,SECOSAGN),HEXD                                   
PerAgy   LKOUT C,005,(D,B#SECD,SECOAGPE),CHAR                                   
SecSys   LKOUT C,006,(D,B#SECD,SECOSYS),HEXD                                    
SecPrg   LKOUT C,007,(D,B#SECD,SECOPRG),HEXD                                    
SysLet   LKOUT C,008,(D,B#SAVED,SYSLET),CHAR                                    
         LKOUT E                                                                
                                                                                
CFMICPY  LKOUT R,X'0061'           Company record values                        
PRout    LKOUT P,,SETACPY          Point to company record                      
         LKOUT C,010,(D,B#CPY,CPYKCPY),HEXD                                     
Array    LKOUT C,015,(A,ARYCNAM)                                                
Array    LKOUT C,020,(A,ARYCADR)                                                
Array    LKOUT C,060,(A,ARYARST)                                                
Array    LKOUT C,150,(A,ARYCCPY)                                                
         LKOUT E                                                                
                                                                                
CFMIOFF  LKOUT R,X'0062'           Office records                               
Array    LKOUT C,X'0062',(A,ARYAOFC)                                            
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
SETACPY  MVC   LP_ADATA,AIO1       Point LP_ADATA to company record             
         BR    RE                                                               
                                                                                
ARYCNAM  LKOUT A,(*,CPYRFST),EOT=EOR,ROWID=(NAMEL,NAMELQ),             +        
               ROWWIDTH=(V,NAMLN),ROWNAME=CPYRECD                               
CpyName  LKOUT C,015,NAMEREC,CHAR,LEN=V                                         
         LKOUT E                                                                
                                                                                
ARYCADR  LKOUT A,(*,CPYRFST),EOT=EOR,ROWID=(ADREL,ADRELQ),             +        
               ROWWIDTH=(V,ADRLN),ROWNAME=CPYRECD                               
RegAdr1  LKOUT C,020,ADRADD1,CHAR,ND=Y                                          
RegAdr2  LKOUT C,021,ADRADD2,CHAR,ND=Y                                          
RegAdr3  LKOUT C,022,ADRADD3,CHAR,ND=Y                                          
RegAdr4  LKOUT C,023,ADRADD4,CHAR,ND=Y                                          
RegAdr5  LKOUT C,024,ADRADD5,CHAR,ND=Y                                          
         LKOUT E                                                                
                                                                                
ARYCCPY  LKOUT A,(*,CPYRFST),EOT=EOR,ROWID=(CPYEL,CPYELQ),             +        
               ROWWIDTH=(V,CPYLN),ROWNAME=CPYRECD                               
         LKOUT C,150,CPYLOGO,CHAR,ND=Y                                          
         LKOUT C,151,CPYGLU,CHAR,ND=Y                                           
         LKOUT C,152,CPYBSL,CHAR,ND=Y                                           
         LKOUT C,153,CPYBANK,CHAR,ND=Y                                          
         LKOUT C,154,CPYPETY,CHAR,ND=Y                                          
         LKOUT C,155,CPYPROD,CHAR,ND=Y                                          
         LKOUT C,156,CPYRECV,CHAR,ND=Y                                          
         LKOUT C,157,CPYSUPP,CHAR,ND=Y                                          
         LKOUT C,158,CPYSUPX,CHAR,ND=Y                                          
         LKOUT C,159,CPYPANL,CHAR,ND=Y                                          
         LKOUT C,160,CPYANAL,CHAR,ND=Y                                          
         LKOUT C,161,CPYWRKSI,CHAR,ND=Y                                         
         LKOUT C,162,CPYVATR,CHAR,LEN=20,ND=Y                                   
                                                                                
         LKOUT C,163,CPYSTAT1,MB80,ND=Y                                         
         LKOUT C,164,CPYSTAT1,MB40,ND=Y                                         
         LKOUT C,165,CPYSTAT1,MB20,ND=Y                                         
         LKOUT C,166,CPYSTAT1,MB10,ND=Y                                         
         LKOUT C,167,CPYSTAT1,MB08,ND=Y                                         
         LKOUT C,168,CPYSTAT1,MB04,ND=Y                                         
         LKOUT C,169,CPYSTAT1,MB02,ND=Y                                         
         LKOUT C,170,CPYSTAT1,MB01,ND=Y                                         
                                                                                
         LKOUT C,171,CPYUID,LBIN,ND=Y                                           
         LKOUT C,172,CPYCDC,CHAR,ND=Y                                           
                                                                                
         LKOUT C,173,CPYSTAT2,MB80,ND=Y                                         
         LKOUT C,174,CPYSTAT2,MB40,ND=Y                                         
         LKOUT C,175,CPYSTAT2,MB20,ND=Y                                         
         LKOUT C,176,CPYSTAT2,MB10,ND=Y                                         
         LKOUT C,177,CPYSTAT2,MB08,ND=Y                                         
         LKOUT C,178,CPYSTAT2,MB04,ND=Y                                         
         LKOUT C,179,CPYSTAT2,MB02,ND=Y                                         
         LKOUT C,180,CPYSTAT2,MB01,ND=Y                                         
                                                                                
         LKOUT C,181,CPYMOSX,CHAR,ND=Y                                          
         LKOUT C,182,CPYTENO,LBIN,ND=Y                                          
                                                                                
         LKOUT C,183,CPYSTAT3,MB80,ND=Y                                         
         LKOUT C,184,CPYSTAT3,MB40,ND=Y                                         
         LKOUT C,185,CPYSTAT3,MB20,ND=Y                                         
         LKOUT C,186,CPYSTAT3,MB10,ND=Y                                         
         LKOUT C,187,CPYSTAT3,MB08,ND=Y                                         
         LKOUT C,188,CPYSTAT3,MB04,ND=Y                                         
         LKOUT C,189,CPYSTAT3,MB02,ND=Y                                         
         LKOUT C,190,CPYSTAT3,MB01,ND=Y                                         
                                                                                
         LKOUT C,191,CPYSFST,LBIN,ND=Y                                          
         LKOUT C,192,CPYBSEC,LBIN,ND=Y                                          
         LKOUT C,193,CPYREPC,LBIN,ND=Y                                          
                                                                                
         LKOUT C,194,CPYSTAT4,MB80,ND=Y                                         
         LKOUT C,195,CPYSTAT4,MB40,ND=Y                                         
         LKOUT C,196,CPYSTAT4,MB20,ND=Y                                         
         LKOUT C,197,CPYSTAT4,MB10,ND=Y                                         
         LKOUT C,198,CPYSTAT4,MB08,ND=Y                                         
         LKOUT C,199,CPYSTAT4,MB04,ND=Y                                         
         LKOUT C,200,CPYSTAT4,MB02,ND=Y                                         
         LKOUT C,201,CPYSTAT4,MB01,ND=Y                                         
                                                                                
         LKOUT C,202,CPYSBILL,SPAK,ND=Y                                         
         LKOUT C,203,CPYALPHA,CHAR,ND=Y                                         
         LKOUT C,204,CPYDEPTL,LBIN,ND=Y                                         
         LKOUT C,205,CPYPCFY,SPAK,ND=Y                                          
         LKOUT C,206,CPYCCFY,SPAK,ND=Y                                          
                                                                                
         LKOUT C,207,CPYSTAT5,MB80,ND=Y                                         
         LKOUT C,208,CPYSTAT5,MB40,ND=Y                                         
         LKOUT C,209,CPYSTAT5,MB20,ND=Y                                         
         LKOUT C,210,CPYSTAT5,MB10,ND=Y                                         
         LKOUT C,211,CPYSTAT5,MB08,ND=Y                                         
         LKOUT C,212,CPYSTAT5,MB04,ND=Y                                         
         LKOUT C,213,CPYSTAT5,MB02,ND=Y                                         
         LKOUT C,214,CPYSTAT5,MB01,ND=Y                                         
                                                                                
         LKOUT C,215,CPYSTAT6,MB80,ND=Y                                         
         LKOUT C,216,CPYSTAT6,MB40,ND=Y                                         
         LKOUT C,217,CPYSTAT6,MB20,ND=Y                                         
         LKOUT C,218,CPYSTAT6,MB10,ND=Y                                         
         LKOUT C,219,CPYSTAT6,MB08,ND=Y                                         
         LKOUT C,220,CPYSTAT6,MB04,ND=Y                                         
         LKOUT C,221,CPYSTAT6,MB02,ND=Y                                         
         LKOUT C,222,CPYSTAT6,MB01,ND=Y                                         
                                                                                
         LKOUT C,223,CPYSTAT7,MB80,ND=Y                                         
         LKOUT C,224,CPYSTAT7,MB40,ND=Y                                         
         LKOUT C,225,CPYSTAT7,MB20,ND=Y                                         
         LKOUT C,226,CPYSTAT7,MB10,ND=Y                                         
         LKOUT C,227,CPYSTAT7,MB08,ND=Y                                         
         LKOUT C,228,CPYSTAT7,MB04,ND=Y                                         
         LKOUT C,229,CPYSTAT7,MB02,ND=Y                                         
         LKOUT C,230,CPYSTAT7,MB01,ND=Y                                         
                                                                                
         LKOUT C,231,CPYSTAT8,MB80,ND=Y                                         
         LKOUT C,232,CPYSTAT8,MB40,ND=Y                                         
         LKOUT C,233,CPYSTAT8,MB20,ND=Y                                         
         LKOUT C,234,CPYSTAT8,MB10,ND=Y                                         
         LKOUT C,235,CPYSTAT8,MB08,ND=Y                                         
         LKOUT C,236,CPYSTAT8,MB04,ND=Y                                         
         LKOUT C,237,CPYSTAT8,MB02,ND=Y                                         
         LKOUT C,238,CPYSTAT8,MB01,ND=Y                                         
                                                                                
         LKOUT C,239,CPYTAX,CHAR,ND=Y                                           
         LKOUT C,240,CPYOFFC,CHAR,ND=Y                                          
         LKOUT C,242,CPYTSD,LBIN,ND=Y                                           
         LKOUT C,243,CPYCURR,CHAR,ND=Y                                          
         LKOUT C,244,CPYTBIND,LBIN,ND=Y                                         
         LKOUT C,245,CPYXSUPP,CHAR,ND=Y                                         
         LKOUT C,246,CPYCTFIL,CHAR,ND=Y                                         
         LKOUT C,247,CPYTCMP,LBIN,ND=Y                                          
         LKOUT C,248,CPYTMSSD,CDAT,ND=Y                                         
                                                                                
         LKOUT C,249,CPYSTAT9,MB80,ND=Y                                         
         LKOUT C,250,CPYSTAT9,MB40,ND=Y                                         
         LKOUT C,251,CPYSTAT9,MB20,ND=Y                                         
         LKOUT C,252,CPYSTAT9,MB10,ND=Y                                         
         LKOUT C,253,CPYSTAT9,MB08,ND=Y                                         
         LKOUT C,254,CPYSTAT9,MB04,ND=Y                                         
         LKOUT C,255,CPYSTAT9,MB02,ND=Y                                         
         LKOUT C,256,CPYSTAT9,MB01,ND=Y                                         
                                                                                
         LKOUT C,257,CPYCURRS,CHAR,ND=Y                                         
         LKOUT C,258,CPYSCMOA,PMON,ND=Y                                         
                                                                                
         LKOUT C,259,CPYSTATA,MB80,ND=Y                                         
         LKOUT C,260,CPYSTATA,MB40,ND=Y                                         
         LKOUT C,261,CPYSTATA,MB20,ND=Y                                         
         LKOUT C,262,CPYSTATA,MB10,ND=Y                                         
         LKOUT C,263,CPYSTATA,MB08,ND=Y                                         
         LKOUT C,264,CPYSTATA,MB04,ND=Y                                         
         LKOUT C,265,CPYSTATA,MB02,ND=Y                                         
         LKOUT C,266,CPYSTATA,MB01,ND=Y                                         
                                                                                
         LKOUT C,268,CPYSTATB,MB80,ND=Y                                         
         LKOUT C,269,CPYSTATB,MB40,ND=Y                                         
         LKOUT C,270,CPYSTATB,MB20,ND=Y                                         
         LKOUT C,271,CPYSTATB,MB10,ND=Y                                         
         LKOUT C,272,CPYSTATB,MB08,ND=Y                                         
         LKOUT C,273,CPYSTATB,MB04,ND=Y                                         
         LKOUT C,274,CPYSTATB,MB02,ND=Y                                         
         LKOUT C,275,CPYSTATB,MB01,ND=Y                                         
                                                                                
         LKOUT C,278,CPYSTATC,MB80,ND=Y                                         
         LKOUT C,279,CPYSTATC,MB40,ND=Y                                         
         LKOUT C,270,CPYSTATC,MB20,ND=Y                                         
         LKOUT C,271,CPYSTATC,MB10,ND=Y                                         
         LKOUT C,282,CPYSTATC,MB08,ND=Y                                         
         LKOUT C,283,CPYSTATC,MB04,ND=Y                                         
         LKOUT C,284,CPYSTATC,MB02,ND=Y                                         
         LKOUT C,285,CPYSTATC,MB01,ND=Y                                         
         LKOUT E                                                                
                                                                                
ARYAOFC  LKOUT A,(R,NXTOFF),MULTIROW=Y,ROWNAME=OFFRECD                          
         LKOUT C,010,(D,B#SAVED,OFFCODE),CHAR                                   
         LKOUT C,011,(D,B#SAVED,OFFNAME),CHAR                                   
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Read 2D high level accounts or office records                       *         
***********************************************************************         
                                                                                
NXTOFF   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTOFF10                                                         
         GOTOR BLDOFF              Build office list                            
         LA    R4,KEYWORK                                                       
         USING LK_D,R4             R4=A(office driver)                          
         TM    CPYINDS,CPYIOFF2    Test 2 character offices                     
         JZ    NXTOFF04                                                         
                                                                                
***********************************************************************         
* Build driver table to read office records/2 character office        *         
***********************************************************************         
                                                                                
         MVI   LK_KDISP,OFFKTYP-OFFRECD                                         
         MVI   LK_KLEN,L'OFFKTYP-1                                              
         MVI   LK_KLIT,OFFKTYPQ                                                 
         MVI   LK_INDS,LK_ILITQ                                                 
         AHI   R4,LK_LNQ                                                        
         MVI   LK_KDISP,OFFKCPY-OFFRECD                                         
         MVI   LK_KLEN,L'OFFKCPY-1                                              
         MVC   LK_BDISP,=AL2(COMPANY-SAVED)                                     
         MVI   LK_INDS,LK_ISINQ                                                 
         AHI   R4,LK_LNQ                                                        
         MVI   LK_KDISP,OFFKSP1-OFFRECD                                         
         MVI   LK_KLEN,L'OFFKSP1-1                                              
         MVI   LK_KLIT,C' '                                                     
         MVI   LK_INDS,LK_ILITQ                                                 
         AHI   R4,LK_LNQ                                                        
         MVI   LK_KDISP,OFFKOFF-OFFRECD                                         
         MVI   LK_KLEN,L'OFFKOFF-1                                              
         MVC   LK_BDISP,=AL2(OFFNUM-SAVED)                                      
         MVI   LK_INDS,LK_ILSTQ                                                 
         OC    OFFNUM,OFFNUM       Test we have list of offices                 
         JNZ   NXTOFF06                                                         
         MVC   LK_BDISP,=AL2(OFFRNG2-SAVED)                                     
         MVI   LK_INDS,LK_IRNGQ                                                 
         LHI   R0,L'OFFKEY         Set length of key                            
         J     NXTOFF06                                                         
                                                                                
***********************************************************************         
* Build driver table to read 2D account records/1 character office    *         
***********************************************************************         
                                                                                
NXTOFF04 MVI   LK_KDISP,ACTKCPY-ACTRECD                                         
         MVI   LK_KLEN,L'ACTKCPY-1                                              
         MVC   LK_BDISP,=AL2(COMPANY-SAVED)                                     
         MVI   LK_INDS,LK_ISINQ                                                 
         AHI   R4,LK_LNQ                                                        
         MVI   LK_KDISP,ACTKUNT-ACTRECD                                         
         MVI   LK_KLEN,L'ACTKUNT-1                                              
         MVI   LK_KLIT,C'2'                                                     
         MVI   LK_INDS,LK_ILITQ                                                 
         AHI   R4,LK_LNQ                                                        
         MVI   LK_KDISP,ACTKLDG-ACTRECD                                         
         MVI   LK_KLEN,L'ACTKLDG-1                                              
         MVI   LK_KLIT,C'D'                                                     
         MVI   LK_INDS,LK_ILITQ                                                 
         AHI   R4,LK_LNQ                                                        
         MVI   LK_KDISP,ACTKACT-ACTRECD                                         
         MVI   LK_KLEN,1-1                                                      
         MVC   LK_BDISP,=AL2(OFFNUM-SAVED)                                      
         MVI   LK_INDS,LK_ILSTQ                                                 
         LHI   R0,ACTKACT+1-ACTRECD                                             
         OC    OFFNUM,OFFNUM       Test any limit access                        
         JNZ   NXTOFF10                                                         
         MVC   LK_BDISP,=AL2(OFFRNG1-SAVED)                                     
         MVI   LK_INDS,LK_IRNGQ                                                 
                                                                                
NXTOFF06 AHI   R4,LK_LNQ           Bump to next entry                           
         MVI   LK_D,LK_EOTQ        Set end of driver table                      
         STCM  R0,3,KEYWORKL       Set total key length                         
                                                                                
NXTOFF10 LARL  R0,FLTOFK           Point to key filter routine                  
         GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',KEYWORKL),('B#OFF',0),   +        
               SAVED,(R0),0                                                     
         JNE   EXITY               Exit of all records read                     
                                                                                
         LA    R0,OFFVALS                                                       
         STCM  R0,15,LP_ADATA      Point to OFFVALS                             
         MVC   OFFNAME,SPACES                                                   
         L     R2,IOADDR                                                        
         USING OFFRECD,R2                                                       
         CLI   OFFKTYP,OFFKTYPQ                                                 
         JNE   *+14                                                             
         MVC   OFFCODE,OFFKOFF                                                  
         J     NXTOFF12                                                         
                                                                                
         USING ACTRECD,R2                                                       
         MVC   OFFCODE,ACTKACT     Set office code                              
                                                                                
NXTOFF12 LA    R2,ACTRFST                                                       
         USING NAMELD,R2                                                        
         SR    R0,R0                                                            
NXTOFF14 CLI   NAMEL,0             Extract office name                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   NAMEL,NAMELQ                                                     
         JE    *+14                                                             
         IC    R0,NAMLN                                                         
         AR    R3,R0                                                            
         J     NXTOFF14                                                         
         SR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SHI   RF,NAMLN1Q+1                                                     
         LARL  RB,NXTOMVC                                                       
         EX    RF,0(RB)                                                         
         J     EXITY                                                            
                                                                                
NXTOMVC  MVC   OFFNAME(0),NAMEREC                                               
         DROP  R2                                                               
                                                                                
K        USING OFFRECD,IOKEY       Apply filter to office record                
FLTOFK   CLI   K.OFFKTYP,OFFKTYPQ  Test have office record                      
         JNE   *+10                                                             
         TM    K.OFFKSTAT,OFFSLIST Yes - drop office lists                      
         BR    RE                                                               
         CR    RE,RE               Set CC to equal                              
         BR    RE                                                               
         DROP  K                                                                
         EJECT                                                                  
***********************************************************************         
* Organizer client download (X'FFF2')                                 *         
***********************************************************************         
                                                                                
REQCFMC  LKREQ H,I#CFMCDL,CFMCCLI,NEXTREQ=REQEND                                
                                                                                
***********************************************************************         
* Agency connection array                                             *         
***********************************************************************         
                                                                                
System   LKREQ F,01,(I,B#SAVED,QAGYIND),CHAR,TEXT=(*,SYS1LIT),         +        
               OLEN=L'AA_SYS,ARRAY=S                                            
Agency   LKREQ F,02,,CHAR,TEXT=(*,AGY1LIT),OLEN=L'AA_OAGY                       
LimAccs  LKREQ F,03,,CHAR,TEXT=(*,LIMALIT),OLEN=L'AA_ACCS                       
UserId#  LKREQ F,04,,HEXD,TEXT=(*,USIDLIT),OLEN=L'AA_USID                       
PID#     LKREQ F,05,,HEXD,TEXT=(*,PIDNLIT),OLEN=L'AA_PASS                       
SAGRP    LKREQ F,06,,HEXD,TEXT=(*,SAGRLIT),OLEN=L'AA_SAGN                       
PSAGY    LKREQ F,07,,CHAR,TEXT=(*,PSAGLIT),OLEN=L'AA_AGPE                       
SSYSM    LKREQ F,08,,HEXD,TEXT=(*,SSYSLIT),OLEN=L'AA_OSYS                       
SPRGM    LKREQ F,09,,HEXD,TEXT=(*,SPRGLIT),OLEN=L'AA_OPRG,ARRAY=E               
                                                                                
AA_D     DSECT ,                   ** Agency connection array **                
AA_SYS   DS    C                   System letter                                
AA_OAGY  DS    CL(L'SECOAGY)       Agency alpha id                              
AA_ACCS  DS    CL(L'LP_ACCS)       Limit access value                           
AA_USID  DS    XL(L'LP_USRID)      User id number                               
AA_PASS  DS    XL(L'SECOPASS)      PID number                                   
AA_SAGN  DS    XL(L'SECOSAGN)      Security agency group                        
AA_AGPE  DS    CL(L'SECOAGPE)      Person security agency                       
AA_OSYS  DS    XL(L'SECOSYS)       Security system number                       
AA_OPRG  DS    XL(L'SECOPRG)       Security program number                      
AA_LNQ   EQU   *-AA_D              Width of array row                           
SVRDEF   CSECT ,                                                                
                                                                                
***********************************************************************         
* Client request array                                                *         
***********************************************************************         
                                                                                
System   LKREQ F,10,(I,B#SAVED,QCLTIND),CHAR,TEXT=(*,SYS2LIT),         +        
               OLEN=L'CA_SYS,ARRAY=S                                            
Agency   LKREQ F,11,,CHAR,TEXT=(*,AGY2LIT),OLEN=L'CA_AGYA                       
Media    LKREQ F,12,,CHAR,TEXT=(*,MEDCLIT),OLEN=L'CA_MEDC                       
Client   LKREQ F,13,,CHAR,TEXT=(*,CLTCLIT),OLEN=L'CA_CLTC                       
Product  LKREQ F,14,,CHAR,TEXT=(*,PRDCLIT),OLEN=L'CA_PRDC                       
Token    LKREQ F,30,,CHAR,TEXT=(*,TOKNLIT),OLEN=L'CA_TOKEN,ARRAY=E              
                                                                                
CA_D     DSECT ,                   ** Client request array **                   
CA_SYS   DS    C                   System letter                                
CA_AGYA  DS    CL2                 Agency alpha id                              
CA_MEDC  DS    C                   Media code                                   
CA_CLTC  DS    CL5                 Client code                                  
CA_PRDC  DS    CL5                 Product code                                 
CA_TOKEN DS    CL8                 PC token value                               
CA_LNQ   EQU   *-CA_D              Width of array row                           
SVRDEF   CSECT ,                                                                
                                                                                
***********************************************************************         
* Other options                                                       *         
***********************************************************************         
                                                                                
DLClt?   LKREQ F,40,(D,B#SAVED,QCLTOPT),CHAR,TEXT=(*,DCLTLIT)                   
DLPrds?  LKREQ F,41,(D,B#SAVED,QPRDOPT),CHAR,TEXT=(*,DPRDLIT)                   
         LKREQ F,42,(D,B#SAVED,QTRASH1),CHAR,TEXT=(*,OPT3LIT)                   
DLLdgs?  LKREQ F,43,(D,B#SAVED,QLDGOPT),CHAR,TEXT=(*,DLDGLIT)                   
DLAccs?  LKREQ F,44,(D,B#SAVED,QACCOPT),CHAR,TEXT=(*,DACCLIT)                   
         LKREQ F,45,(D,B#SAVED,QTRASH1),CHAR,TEXT=(*,OPT6LIT)                   
         LKREQ F,46,(D,B#SAVED,QTRASH1),CHAR,TEXT=(*,OPT7LIT)                   
         LKREQ F,47,(D,B#SAVED,QTRASH1),CHAR,TEXT=(*,OPT8LIT)                   
         LKREQ F,48,(D,B#SAVED,QTRASH1),CHAR,TEXT=(*,OPT9LIT)                   
         LKREQ F,49,(D,B#SAVED,QTRASH1),CHAR,TEXT=(*,OPTALIT)                   
         LKREQ F,50,(D,B#SAVED,QTRASH1),CHAR,TEXT=(*,OPTBLIT)                   
         LKREQ F,51,(D,B#SAVED,QTRASH1),CHAR,TEXT=(*,OPTCLIT)                   
         LKREQ F,52,(D,B#SAVED,QTRASH1),CHAR,TEXT=(*,OPTDLIT)                   
         LKREQ F,53,(D,B#SAVED,QTRASH1),CHAR,TEXT=(*,OPTELIT)                   
         LKREQ F,54,(D,B#SAVED,QTRASH1),CHAR,TEXT=(*,OPTFLIT)                   
         LKREQ F,55,(D,B#SAVED,QTRASH1),CHAR,TEXT=(*,OPTGLIT)                   
                                                                                
         LKREQ E                                                                
                                                                                
CFMCCLI  LKOUT H                                                                
                                                                                
CFMC     LKOUT R,X'0061'           Clients, products and accounts               
Array    LKOUT C,X'0061',(A,ARYCLT)                                             
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
ARYCLT   LKOUT A,(R,NXTCLT),MULTIROW=Y,ROWNAME=ACTRECD                          
CltCod   LKOUT C,002,ACTRECD,(R,EDTCLT),FILTROUT=TSTCLT,               +        
               SKIPCOLS=CLTSKIPN                                                
                                                                                
CLTSKIPS DS    0X                  Start of columns to skip                     
                                                                                
Array    LKOUT C,005,(A,ARYANAM)                                                
Array    LKOUT C,014,(A,ARYANUM)                                                
Array    LKOUT C,017,(A,ARYAADR)                                                
Array    LKOUT C,022,(A,ARYAOAD)                                                
Array    LKOUT C,040,(A,ARYAPPR)                                                
Array    LKOUT C,060,(A,ARYARST)                                                
Array    LKOUT C,140,(A,ARYAPAC)                                                
Array    LKOUT C,160,(A,ARYASCM)                                                
Array    LKOUT C,180,(A,ARYAOME)                                                
Array    LKOUT C,210,(A,ARYAPMD)                                                
Array    LKOUT C,220,(A,ARYAFFT)                                                
                                                                                
CReqTok  LKOUT C,300,(D,B#SAVED,REQTOK),CHAR                                    
                                                                                
CLTSKIPN EQU   (*-CLTSKIPS)/LX_COLSL                                            
                                                                                
Array    LKOUT C,X'0062',(A,ARYPRD),FILTROUT=TSTPRD                             
ARRAY    LKOUT C,X'0063',(A,ARYLDG),FILTROUT=TSTLDG                             
Array    LKOUT C,X'0064',(A,ARYACC),FILTROUT=TSTACC                             
         LKOUT E                                                                
                                                                                
TSTCLT   CLI   DUMMYCLI,YESQ       Test dummy client                            
         JNE   *+8                                                              
         LTR   RE,RE               Yes - don't send these                       
         BR    RE                                                               
         CLI   QCLTOPT,YESQ        Test downloading client details              
         BR    RE                                                               
                                                                                
TSTPRD   CLI   QPRDOPT,YESQ        Test downloading product details             
         BR    RE                                                               
                                                                                
TSTACC   CLI   QACCOPT,YESQ        Test downloading account details             
         BR    RE                                                               
                                                                                
TSTLDG   CLI   ANYCLI,YESQ         Test have any client requests                
         BNER  RE                  No - can't download ledgers                  
         CLI   QLDGOPT,YESQ        Test downloading ledger details              
         BR    RE                                                               
                                                                                
***********************************************************************         
* Process first/next client array entry                               *         
***********************************************************************         
                                                                                
NXTCLT   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTCLT02                                                         
         MVI   ANYCLI,NOQ          Set have no clients                          
         SR    RE,RE                                                            
         ICM   RE,7,QACLT                                                       
         JZ    NOMORE                                                           
         MVC   NUMCLT,LW_NUMN-LW_D(RE)                                          
         AHI   RE,LW_LN2Q                                                       
         ST    RE,ANXTCLT          Set A(next client request)                   
         XC    LAGY,LAGY           Initialize agency control                    
                                                                                
NXTCLT02 SR    R0,R0                                                            
         ICM   R0,3,NUMCLT         R0=remaining client count                    
         JZ    NOMORE              Exit if all done                             
         BCTR  R0,0                                                             
         STCM  R0,3,NUMCLT                                                      
         L     R2,ANXTCLT                                                       
         USING CA_D,R2             R2=A(client row)                             
         MVC   REQTOK,CA_TOKEN     Set return token                             
         LA    R0,CA_D+CA_LNQ                                                   
         ST    R0,ANXTCLT                                                       
         CLC   CA_SYS,SYSLET       Test for my system                           
         JNE   NXTCLT02                                                         
         CLC   LAGY,CA_AGYA        Test change of agency                        
         JE    NXTACLT10                                                        
         MVC   LAGY,CA_AGYA        Set current agency code                      
                                                                                
         SR    R3,R3                                                            
         ICM   R3,7,QAAGY                                                       
         JNZ   *+6                                                              
         DC    H'0'                Agency definitions not passed                
         SR    R0,R0                                                            
         ICM   R0,3,LW_NUMN-LW_D(R3)                                            
         AHI   R3,LW_LN2Q                                                       
         USING AA_D,R3             Look up connection table entry               
                                                                                
NXTCLT04 CLC   AA_SYS,CA_SYS       Match system                                 
         JNE   *+14                                                             
         CLC   AA_OAGY,CA_AGYA     and agency alpha id                          
         JE    NXTCLT06                                                         
         AHI   R3,AA_LNQ           Bump to next entry                           
         JCT   R0,NXTCLT04                                                      
         DC    H'0'                System/agency not provided                   
                                                                                
NXTCLT06 TM    LP_FLAG,LP_FOFFL    Test offline                                 
         JZ    NXTCLT08                                                         
         GOTOR GETUID,DMCB,AA_USID,AA_PASS                                      
         JNE   NXTCLT02                                                         
         GOTOR VDATAMGR,DMCB,DMKEY,ACCDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,ACCMST,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,ACCARC,(4,0),0                               
         MVC   LP_ACCS,AA_ACCS     Set limit access value                       
         MVC   COMPANY,LP_AGYB     Set company code                             
         GOTOR (#GETCPY,AGETCPY)   and read company record                      
                                                                                
NXTCLT08 GOTOR BLDOFF              Build office list                            
                                                                                
         GOTOR (#GETLDG,AGETLDG),DMCB,LDGLSJ,LDGSJ                              
         GOTOR (#GETLDG,AGETLDG),DMCB,LDGLSR,LDGSR                              
         GOTOR (#GETLDG,AGETLDG),DMCB,LDGL1C,LDG1C                              
         MVI   FIRSTCLI,YESQ       Set first client for agency                  
         MVI   ANYCLI,YESQ         Set have a client request                    
         MVI   DUMMYCLI,NOQ        Set not a dummy client request               
         CLC   =C'ALL',CA_CLTC                                                  
         JE    *+12                                                             
         CLI   CA_CLTC,C' '                                                     
         JH    NXTACLT10                                                        
         MVI   DUMMYCLI,YESQ       Set dummy client request                     
         MVC   LP_ADATA,AIO1                                                    
         J     EXITY                                                            
                                                                                
K        USING ACTKEY,IOKEY                                                     
NXTACLT10 MVC  PRDCODE,CA_PRDC     Set product filter                           
                                                                                
         MVC   K.ACTKEY,SPACES                                                  
         MVC   K.ACTKCPY,LP_AGYB                                                
         MVC   K.ACTKUNT(L'LDGLSJ),LDGLSJ                                       
         MVC   K.ACTKACT(L'CA_CLTC),CA_CLTC                                     
         DROP  K                                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOACCDIR+IO3'                            
         JNE   NXTCLT02                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         JNE   NXTCLT02                                                         
                                                                                
         LA    R4,KEYWORK                                                       
         USING LK_D,R4             R4=A(key driver table)                       
         MVI   LK_KDISP,ACTKCPY-ACTRECD                                         
         LLC   R0,LDGSJ+(LDGTLVA-LDGTABD)                                       
         AHI   R0,ACTKACT-ACTKEY-1                                              
         STC   R0,LK_KLEN                                                       
         MVC   LK_BDISP,=AL2(SVCLTKEY-SAVED)                                    
         MVI   LK_INDS,LK_ISINQ    Set single value                             
         AHI   R4,LK_LNQ                                                        
         AHI   R0,1                R0=Displacement to product code              
         STC   R0,LK_KDISP                                                      
         MVC   LK_BDISP,=AL2(PRDCODE-SAVED)                                     
         LLC   RE,LDGSJ+(LDGTLVB-LDGTABD)                                       
         LLC   RF,LDGSJ+(LDGTLVA-LDGTABD)                                       
         SR    RE,RF                                                            
         SHI   RE,1                RF=L'product code-1                          
         STC   RE,LK_KLEN                                                       
         LA    RF,ACTKACT-ACTKEY+1(RE,RF)                                       
         STCM  RF,3,KEYWORKL       Set length of key argument                   
         MVI   LK_INDS,LK_ISINQ                                                 
         CLC   PRDCODE,SPACES      Test all products required                   
         JH    NXTCLT12                                                         
         MVC   LK_BDISP,=AL2(PRDRNGE-SAVED)                                     
         LA    RF,PRDRNGE                                                       
         LARL  RB,NXTCMVC1                                                      
         EX    RE,0(RB)                                                         
         MVI   0(RF),X'41'                                                      
         LA    RF,1(RE,RF)                                                      
         LARL  RB,NXTCMVC2                                                      
         EX    RE,0(RB)                                                         
         MVI   LK_INDS,LK_IRNGQ                                                 
                                                                                
NXTCLT12 AHI   R4,LK_LNQ                                                        
         MVI   LK_D,LK_EOTQ                                                     
         MVC   LP_ADATA,IOADDR     Point to client record                       
         J     EXITY                                                            
                                                                                
NXTCMVC1 MVC   0(0,RF),SPACES                                                   
NXTCMVC2 MVC   0(0,RF),EFFS                                                     
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* Product record download                                             *         
***********************************************************************         
                                                                                
ARYPRD   LKOUT A,(R,NXTPRD),MULTIROW=Y,ROWNAME=ACTRECD                          
CltCod   LKOUT C,002,ACTRECD,(R,EDTCLT)                                         
PrdCod   LKOUT C,003,ACTRECD,(R,EDTPRD)                                         
                                                                                
Array    LKOUT C,005,(A,ARYANAM)                                                
Array    LKOUT C,017,(A,ARYAADR)                                                
Array    LKOUT C,022,(A,ARYAOAD)                                                
Array    LKOUT C,040,(A,ARYAPPR)                                                
Array    LKOUT C,060,(A,ARYARST)                                                
Array    LKOUT C,140,(A,ARYAPAC)                                                
Array    LKOUT C,150,(A,ARYAOTH)                                                
Array    LKOUT C,160,(A,ARYASCM)                                                
Array    LKOUT C,180,(A,ARYAOME)                                                
Array    LKOUT C,190,(A,ARYASAN)                                                
Array    LKOUT C,210,(A,ARYAPMD)                                                
Array    LKOUT C,220,(A,ARYAFFT)                                                
                                                                                
PReqTok  LKOUT C,300,(D,B#SAVED,REQTOK),CHAR                                    
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Read product records for current client                             *         
***********************************************************************         
                                                                                
NXTPRD   GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',KEYWORKL),               +        
               ('B#ACC',SVCLTKEY),SAVED,0,0                                     
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Account record download                                             *         
***********************************************************************         
                                                                                
ARYACC   LKOUT A,(R,NXTACC),MULTIROW=Y,ROWNAME=ACTRECD                          
ULCode   LKOUT C,007,ACTKUNT,CHAR,LEN=L'ACTKUNT+L'ACTKLDG                       
AccCode  LKOUT C,009,ACTKACT,CHAR                                               
                                                                                
Array    LKOUT C,020,(A,ARYACNM)                                                
Array    LKOUT C,060,(A,ARYARST)                                                
                                                                                
AReqTok  LKOUT C,300,(D,B#SAVED,REQTOK),CHAR                                    
         LKOUT E                                                                
                                                                                
ARYACNM  LKOUT A,(*,ACTRFST),EOT=EOR,ROWID=(NAMEL,NAMELQ),             +        
               ROWWIDTH=(V,NAMLN),ROWNAME=ACTRECD                               
AccName  LKOUT C,020,NAMEREC,CHAR,LEN=V                                         
         LKOUT E                                                                
                                                                                
ARYANAM  LKOUT A,(*,ACTRFST),EOT=EOR,ROWID=(NAMEL,NAMELQ),             +        
               ROWWIDTH=(V,NAMLN),ROWNAME=ACTRECD                               
CltName  LKOUT C,005,NAMEREC,CHAR,LEN=V                                         
         LKOUT E                                                                
                                                                                
ARYARST  LKOUT A,(*,ACTRFST),EOT=EOR,ROWID=(RSTEL,RSTELQ),             +        
               ROWWIDTH=(V,RSTLN),ROWNAME=ACTRECD                               
         LKOUT C,060,RSTSECY,LBIN,ND=Y                                          
         LKOUT C,061,RSTFILT1,CHAR,ND=Y                                         
         LKOUT C,062,RSTFILT2,CHAR,ND=Y                                         
         LKOUT C,063,RSTFILT3,CHAR,ND=Y                                         
         LKOUT C,064,RSTFILT4,CHAR,ND=Y                                         
         LKOUT C,065,RSTFILT5,CHAR,ND=Y                                         
         LKOUT C,066,RSTCCTR,LBIN,ND=Y                                          
                                                                                
         LKOUT C,069,RSTSTAT1,MB80,ND=Y                                         
         LKOUT C,070,RSTSTAT1,MB40,ND=Y                                         
         LKOUT C,071,RSTSTAT1,MB20,ND=Y                                         
         LKOUT C,072,RSTSTAT1,MB10,ND=Y                                         
         LKOUT C,073,RSTSTAT1,MB08,ND=Y                                         
         LKOUT C,074,RSTSTAT1,MB04,ND=Y                                         
         LKOUT C,075,RSTSTAT1,MB02,ND=Y                                         
         LKOUT C,076,RSTSTAT1,MB01,ND=Y                                         
                                                                                
         LKOUT C,077,RSTSTAT2,MB80,ND=Y                                         
         LKOUT C,078,RSTSTAT2,MB40,ND=Y                                         
         LKOUT C,079,RSTSTAT2,MB20,ND=Y                                         
         LKOUT C,080,RSTSTAT2,MB10,ND=Y                                         
         LKOUT C,081,RSTSTAT2,MB08,ND=Y                                         
         LKOUT C,082,RSTSTAT2,MB04,ND=Y                                         
         LKOUT C,083,RSTSTAT2,MB02,ND=Y                                         
         LKOUT C,084,RSTSTAT2,MB01,ND=Y                                         
                                                                                
         LKOUT C,085,RSTSTAT3,MB80,ND=Y                                         
         LKOUT C,086,RSTSTAT3,MB40,ND=Y                                         
         LKOUT C,087,RSTSTAT3,MB20,ND=Y                                         
         LKOUT C,088,RSTSTAT3,MB10,ND=Y                                         
         LKOUT C,089,RSTSTAT3,MB08,ND=Y                                         
         LKOUT C,090,RSTSTAT3,MB04,ND=Y                                         
         LKOUT C,091,RSTSTAT3,MB02,ND=Y                                         
         LKOUT C,092,RSTSTAT3,MB01,ND=Y                                         
                                                                                
         LKOUT C,093,RSTSTAT4,MB80,ND=Y                                         
         LKOUT C,094,RSTSTAT4,MB40,ND=Y                                         
         LKOUT C,095,RSTSTAT4,MB20,ND=Y                                         
         LKOUT C,096,RSTSTAT4,MB10,ND=Y                                         
         LKOUT C,097,RSTSTAT4,MB08,ND=Y                                         
         LKOUT C,098,RSTSTAT4,MB04,ND=Y                                         
         LKOUT C,099,RSTSTAT4,MB02,ND=Y                                         
         LKOUT C,100,RSTSTAT4,MB01,ND=Y                                         
                                                                                
         LKOUT C,101,RSTSTAT5,MB80,ND=Y                                         
         LKOUT C,102,RSTSTAT5,MB40,ND=Y                                         
         LKOUT C,103,RSTSTAT5,MB20,ND=Y                                         
         LKOUT C,104,RSTSTAT5,MB10,ND=Y                                         
         LKOUT C,105,RSTSTAT5,MB08,ND=Y                                         
         LKOUT C,106,RSTSTAT5,MB04,ND=Y                                         
         LKOUT C,107,RSTSTAT5,MB02,ND=Y                                         
         LKOUT C,108,RSTSTAT5,MB01,ND=Y                                         
                                                                                
         LKOUT C,109,RSTSTAT6,MB80,ND=Y                                         
         LKOUT C,110,RSTSTAT6,MB40,ND=Y                                         
         LKOUT C,111,RSTSTAT6,MB20,ND=Y                                         
         LKOUT C,112,RSTSTAT6,MB10,ND=Y                                         
         LKOUT C,113,RSTSTAT6,MB08,ND=Y                                         
         LKOUT C,114,RSTSTAT6,MB04,ND=Y                                         
         LKOUT C,115,RSTSTAT6,MB02,ND=Y                                         
         LKOUT C,116,RSTSTAT6,MB01,ND=Y                                         
                                                                                
         LKOUT C,117,RSTCCTRR,CHAR,ND=Y                                         
         LKOUT C,118,RSTBDATE,PDAT,ND=Y                                         
         LKOUT C,119,RSTTDATE,PDAT,ND=Y                                         
         LKOUT C,120,RSTCOSTG,CHAR,ND=Y                                         
         LKOUT C,121,RSTSYSME,CHAR,ND=Y                                         
         LKOUT C,122,RSTOFFC,CHAR,ND=Y                                          
         LKOUT C,123,RSTX1099,UBIN,ND=Y                                         
         LKOUT C,124,RSTDFTSK,CHAR,ND=Y                                         
         LKOUT C,125,RSTMAIL,UBIN,ND=Y                                          
         LKOUT C,127,RSTLSBD,CDAT,ND=Y                                          
                                                                                
         LKOUT C,128,RSTLSTAT,MB80,ND=Y                                         
         LKOUT C,129,RSTLSTAT,MB40,ND=Y                                         
         LKOUT C,130,RSTLSTAT,MB20,ND=Y                                         
         LKOUT C,131,RSTLSTAT,MB10,ND=Y                                         
         LKOUT C,132,RSTLSTAT,MB08,ND=Y                                         
         LKOUT C,133,RSTLSTAT,MB04,ND=Y                                         
         LKOUT C,134,RSTLSTAT,MB02,ND=Y                                         
         LKOUT C,135,RSTLSTAT,MB01,ND=Y                                         
         LKOUT E                                                                
                                                                                
ARYANUM  LKOUT A,(*,ACTRFST),EOT=EOR,ROWID=(NUMEL,NUMELQ),             +        
               ROWWIDTH=(V,NUMLN),ROWNAME=ACTRECD                               
BefNumb  LKOUT C,014,NUMBEF,CHAR,ND=Y                                           
AftNumb  LKOUT C,015,NUMAFT,CHAR,ND=Y                                           
OptType  LKOUT C,016,NUMTYPE,CHAR,ND=Y                                          
         LKOUT E                                                                
                                                                                
ARYAADR  LKOUT A,(*,ACTRFST),EOT=EOR,ROWID=(ADREL,ADRELQ),             +        
               ROWWIDTH=(V,ADRLN),ROWNAME=ACTRECD                               
RegAdr1  LKOUT C,017,ADRADD1,CHAR,ND=Y                                          
RegAdr2  LKOUT C,018,ADRADD2,CHAR,ND=Y                                          
RegAdr3  LKOUT C,019,ADRADD3,CHAR,ND=Y                                          
RegAdr4  LKOUT C,020,ADRADD4,CHAR,ND=Y                                          
RegAdr5  LKOUT C,021,ADRADD5,CHAR,ND=Y                                          
         LKOUT E                                                                
                                                                                
ARYAOAD  LKOUT A,(*,ACTRFST),EOT=EOR,ROWID=(ADREL,OADELQ),             +        
               ROWWIDTH=(V,ADRLN),ROWNAME=ACTRECD                               
OthAdr1  LKOUT C,022,ADRADD1,CHAR,ND=Y                                          
OthAdr2  LKOUT C,023,ADRADD2,CHAR,ND=Y                                          
OthAdr3  LKOUT C,024,ADRADD3,CHAR,ND=Y                                          
OthAdr4  LKOUT C,025,ADRADD4,CHAR,ND=Y                                          
OthAdr5  LKOUT C,026,ADRADD5,CHAR,ND=Y                                          
         LKOUT E                                                                
                                                                                
ARYAPPR  LKOUT A,(*,ACTRFST),EOT=EOR,ROWID=(PPREL,PPRELQ),             +        
               ROWWIDTH=(V,PPRLN),ROWNAME=ACTRECD                               
BilGrup  LKOUT C,040,PPRGRUP,CHAR,ND=Y                                          
         LKOUT C,041,PPRRECVC,HEXD,ND=Y                                         
         LKOUT C,042,PPRRECVU,CHAR,ND=Y,LEN=2                                   
         LKOUT C,044,PPRRECVA,CHAR,ND=Y                                         
         LKOUT C,045,PPRCOSTC,HEXD,ND=Y                                         
         LKOUT C,046,PPRCOSTU,CHAR,ND=Y,LEN=2                                   
         LKOUT C,048,PPRCOSTA,CHAR,ND=Y                                         
         LKOUT C,049,PPRGAOFF,CHAR,ND=Y                                         
         LKOUT C,050,PPRBTYPE,CHAR,ND=Y                                         
         LKOUT C,051,PPRBLAMT,LBIN,ND=Y                                         
         LKOUT C,052,PPRUFORA,CHAR,ND=Y                                         
         LKOUT C,053,PPRUWRK,CHAR,LEN=12,ND=Y                                   
         LKOUT C,054,PPRBILLP,CHAR,ND=Y                                         
         LKOUT C,055,PPRNARRP+000,CHAR,LEN=50,ND=Y                              
         LKOUT C,056,PPRNARRP+050,CHAR,LEN=50,ND=Y                              
         LKOUT C,057,PPRNARRP+100,CHAR,LEN=50,ND=Y                              
         LKOUT E                                                                
                                                                                
ARYAPAC  LKOUT A,(*,ACTRFST),EOT=EOR,ROWID=(PACEL,PACELQ),             +        
               ROWWIDTH=(V,PACLN),ROWNAME=ACTRECD                               
         LKOUT C,140,PACPERS,CHAR,ND=Y                                          
         LKOUT C,141,PACDATE,PDAT,ND=Y                                          
         LKOUT C,142,PACPERS2,CHAR,ND=Y                                         
         LKOUT C,143,PACDATE2,PDAT,ND=Y                                         
         LKOUT E                                                                
                                                                                
ARYASCM  LKOUT A,(*,ACTRFST),EOT=EOR,ROWID=(SCMEL,SCMELQ),             +        
               ROWWIDTH=(V,SCMLN),ROWNAME=ACTRECD                               
         LKOUT C,160,SCMSEQ,UBIN                                                
                                                                                
         LKOUT C,161,SCMTYPE,MB80,ND=Y                                          
         LKOUT C,162,SCMTYPE,MB40,ND=Y                                          
         LKOUT C,163,SCMTYPE,MB20,ND=Y                                          
         LKOUT C,164,SCMTYPE,MB10,ND=Y                                          
         LKOUT C,165,SCMTYPE,MB08,ND=Y                                          
         LKOUT C,166,SCMTYPE,MB04,ND=Y                                          
         LKOUT C,167,SCMTYPE,MB02,ND=Y                                          
         LKOUT C,168,SCMTYPE,MB01,ND=Y                                          
                                                                                
PROUT    LKOUT P,SCMEL,SETSCMCD                                                 
         LKOUT C,169,(D,B#WORKD,TEMP),CHAR,LEN=16,ND=Y                          
*        LKOUT C,169,SCMCODE,CHAR,LEN=V,ND=Y                                    
         LKOUT E                                                                
                                                                                
SETSCMCD L     R1,LP_AINP          Covert SCMCODE to BIL= if needed             
         USING SCMELD,R1                                                        
         MVC   TEMP,SPACES                                                      
         LA    RE,TEMP                                                          
         TM    SCMTYPE,SCMTPRBI+SCMTPRAD                                        
         JNO   SETSCM10                                                         
         MVC   TEMP(4),=C'Bil='                                                 
         LA    RE,4(RE)                                                         
SETSCM10 MVC   0(L'SCMCODE,RE),SCMCODE                                          
         J     EXIT                                                             
         DROP  R1                                                               
                                                                                
ARYAOME  LKOUT A,(*,ACTRFST),EOT=EOR,ROWID=(OMEEL,OMEELQ),             +        
               ROWWIDTH=(V,OMELN),ROWNAME=ACTRECD                               
         LKOUT C,180,OMEMO,CHAR,LEN=V,ND=Y                                      
         LKOUT E                                                                
                                                                                
ARYAPMD  LKOUT A,(*,ACTRFST),EOT=EOR,ROWID=(PMDEL,PMDELQ),             +        
               ROWWIDTH=(V,PMDLN),ROWNAME=ACTRECD                               
         LKOUT C,210,PMDFBILL,CHAR,ND=Y                                         
         LKOUT C,211,PMDLBILL,CHAR,ND=Y                                         
         LKOUT C,212,PMDRBILL,CHAR,ND=Y                                         
         LKOUT E                                                                
                                                                                
ARYAOTH  LKOUT A,(*,ACTRFST),EOT=EOR,ROWID=(OTHEL,OTHELQ),             +        
               ROWWIDTH=(V,OTHLN),ROWNAME=ACTRECD                               
         LKOUT C,150,OTHNUM,CHAR,ND=Y                                           
         LKOUT C,151,OTHPROF,CHAR,ND=Y,LEN=2                                    
         LKOUT C,153,OTHNET,CHAR,ND=Y                                           
         LKOUT E                                                                
                                                                                
ARYASAN  LKOUT A,(*,ACTRFST),EOT=EOR,ROWID=(SANEL,SANELQ),             +        
               ROWWIDTH=(V,SANLN),ROWNAME=ACTRECD                               
         LKOUT C,190,SANCODE+1,CHAR,ND=Y,LEN=L'SANCODE-1                        
         LKOUT C,191,SANNAME,CHAR,ND=Y                                          
         LKOUT E                                                                
                                                                                
ARYAFFT  LKOUT A,(*,ACTRFST),EOT=EOR,ROWID=(FFTEL,FFTELQ),             +        
               ROWWIDTH=(V,FFTLN),ROWNAME=ACTRECD                               
         LKOUT C,220,FFTTYPE,UBIN                                               
         LKOUT C,221,FFTSEQ,UBIN                                                
         LKOUT C,222,FFTDATA,CHAR,LEN=V                                         
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Read account records                                                *         
***********************************************************************         
                                                                                
NXTACC   CLI   LP_RMODE,LP_RFRST   Test first time                              
         JNE   NXTACC04                                                         
         MVC   SVCLTKEY,IOKEY      Save client key                              
         CLI   FIRSTCLI,YESQ       Test first client for agency                 
         JNE   NOMORE                                                           
         MVI   FIRSTCLI,NOQ        Set not first client for agency              
         XC    UNITLDGR,UNITLDGR   Initialize for new agency                    
         MVC   LDGN,LDGLST1N       Number of account unit/ledgers               
         LA    R0,LDGLUL1                                                       
         ST    R0,ALDG             Point to first unit/ledger in list           
                                                                                
NXTACC04 OC    UNITLDGR,UNITLDGR   Test initialized                             
         JNZ   NXTACC16                                                         
         LLH   R0,LDGN                                                          
         SHI   R0,1                Decrement counter                            
         JM    NXTACCX                                                          
         STCM  R0,3,LDGN                                                        
                                                                                
         L     R1,ALDG                                                          
         MVC   UNITLDGR,0(R1)      Extract ledger code                          
         AHI   R1,L'UNITLDGR       Bump and set pointer to next                 
         ST    R1,ALDG                                                          
                                                                                
         LA    R4,KEYWORK          Build driver table in KEYWORK                
         USING LK_D,R4                                                          
         MVI   LK_KDISP,ACTKCPY-ACTRECD                                         
         MVI   LK_KLEN,ACTKACT-ACTRECD-1                                        
         MVC   LK_BDISP,=AL2(COMPANY-SAVED)                                     
         MVI   LK_INDS,LK_ISINQ    Single company, unit, ledger                 
         AHI   R4,LK_LNQ                                                        
                                                                                
         MVI   LK_KDISP,ACTKACT-ACTRECD                                         
                                                                                
         LA    R2,LDGSR            Point to debtors ledger entry                
         CLC   UNITLDGR,LDGSR+(LDGTUL-LDGTABD)                                  
         JE    NXTACC06                                                         
         LA    R2,LDG1C            Point to client costing ledger entry         
         CLC   UNITLDGR,LDG1C+(LDGTUL-LDGTABD)                                  
         JNE   NXTACC12                                                         
                                                                                
L        USING LDGTABD,R2                                                       
NXTACC06 MVI   LK_KDISP,ACTKACT-ACTRECD                                         
                                                                                
         CLI   L.LDGTOFFP,LDGONONE Test office in key                           
         JE    NXTACC12            No - simple range                            
         OC    OFFNUM,OFFNUM       Test limit access office list                
         JZ    NXTACC12            No - simple range                            
                                                                                
         MVC   BYTE1,L.LDGTOFFP    Extract office position                      
         NI    BYTE1,X'0F'                                                      
         CLI   BYTE1,1             Test office position is (+)1                 
         JNE   NXTACC08                                                         
         LLC   RF,L.LDGTLVA        Level 1 length s/b=to office length          
         BCTR  RF,0                RF=office key length-1                       
         STC   RF,LK_KLEN                                                       
         MVC   LK_BDISP,=AL2(OFFNUM-SAVED)                                      
         MVI   LK_INDS,LK_ILSTQ    Set this is a list                           
         AHI   R4,LK_LNQ                                                        
                                                                                
         LA    R0,ACTKACT-ACTRECD+1(RF)                                         
         STC   R0,LK_KDISP         Set displacement to rest of account          
         LHI   R1,ACTKEND                                                       
         SR    R1,R0                                                            
         SHI   R1,1                                                             
         JNM   *+6                                                              
         DC    H'0'                                                             
         STC   R1,LK_KLEN                                                       
         J     NXTACC10                                                         
                                                                                
NXTACC08 MVC   LK_BDISP,=AL2(LVARNGE-SAVED)                                     
         LLC   R1,BYTE1            Get office position (2-?)                    
         SHI   R1,2                                                             
         STC   R1,LK_KLEN                                                       
         MVI   LK_INDS,LK_IRNGQ    Single range of values                       
         LARL  RB,NXTAMVC1                                                      
         EX    R1,0(RB)                                                         
         LA    RF,LVARNGE(R1)                                                   
         MVI   0(RF),X'41'                                                      
         LARL  RB,NXTAMVC2                                                      
         EX    R1,0(RB)                                                         
         AHI   R4,LK_LNQ                                                        
                                                                                
         MVC   LK_BDISP,=AL2(OFFNUM-SAVED)                                      
         LA    R1,ACTKACT-ACTRECD+1(R1)                                         
         STC   R1,LK_KDISP         Set displacement to office in key            
         LHI   RF,2-1              Set 2 character office length                
         TM    CPYINDS,CPYIOFF2                                                 
         JNZ   *+8                                                              
         LHI   RF,1-1              Set 1 character office length                
         STC   RF,LK_KLEN                                                       
         MVI   LK_INDS,LK_ILSTQ                                                 
         AHI   R4,LK_LNQ                                                        
                                                                                
         LA    RF,1(RF,R1)         Add length of office code                    
         STC   RF,LK_KDISP                                                      
         LHI   R1,ACTKEND                                                       
         SR    R1,RF                                                            
         BCTR  R1,0                                                             
         STC   R1,LK_KLEN                                                       
                                                                                
NXTACC10 MVI   LK_INDS,LK_IRNGQ    Build variable range                         
         MVC   LK_BDISP,=AL2(ACCRNGE-SAVED)                                     
         LARL  RB,NXTAMVC3                                                      
         EX    R1,0(RB)                                                         
         LA    RF,ACCRNGE(R1)                                                   
         MVI   0(RF),X'41'                                                      
         LARL  RB,NXTAMVC4                                                      
         EX    R1,0(RB)                                                         
         LHI   R0,ACTKEND                                                       
         J     NXTACC14                                                         
                                                                                
NXTACC12 MVC   ACCRNGE(L'ACTKACT),SPACES                                        
         MVI   ACCRNGE+L'ACTKACT-1,X'41'                                        
         MVC   ACCRNGE+L'ACTKACT(L'ACTKACT),EFFS                                
         MVI   LK_KLEN,L'ACTKACT-1                                              
         MVC   LK_BDISP,=AL2(ACCRNGE-SAVED)                                     
         MVI   LK_INDS,LK_IRNGQ                                                 
                                                                                
NXTACC14 AHI   R4,LK_LNQ           Bump pointer and set end of table            
         MVI   LK_D,LK_EOTQ                                                     
         MVC   KEYWORKL,=AL2(ACTKEND)                                           
         MVI   LP_RMODE,LP_RFRST   Set first time for NXTREC call               
                                                                                
NXTACC16 GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',KEYWORKL),               +        
               ('B#ACC',0),SAVED,0,0                                            
         JE    EXITY                                                            
         XC    UNITLDGR,UNITLDGR   Set to get next unit/ledger                  
         J     NXTACC04                                                         
                                                                                
NXTACCX  MVC   IOKEY,SVCLTKEY      Restore saved client key                     
         J     NOMORE                                                           
                                                                                
NXTAMVC1 MVC   LVARNGE(0),SPACES   Build level A range                          
NXTAMVC2 MVC   1(0,RF),EFFS                                                     
NXTAMVC3 MVC   ACCRNGE(0),SPACES                                                
NXTAMVC4 MVC   1(0,RF),EFFS                                                     
         DROP  R4                                                               
                                                                                
***********************************************************************         
* Ledger record download                                              *         
***********************************************************************         
                                                                                
ARYLDG   LKOUT A,(R,NXTLDG),MULTIROW=Y,ROWNAME=LDGRECD                          
ULCode   LKOUT C,007,LDGKUNT,CHAR,LEN=L'LDGKUNT+L'LDGKLDG                       
Array    LKOUT C,020,(A,ARYLNAM)                                                
Array    LKOUT C,040,(A,ARYLACL)                                                
Array    LKOUT C,060,(A,ARYLAPR)                                                
Array    LKOUT C,085,(A,ARYLLDG)                                                
                                                                                
LReqTok  LKOUT C,300,(D,B#SAVED,REQTOK),CHAR                                    
         LKOUT E                                                                
                                                                                
ARYLNAM  LKOUT A,(D,B#LDG,LDGRFST),EOT=EOR,ROWID=(NAMEL,NAMELQ),       +        
               ROWWIDTH=(V,NAMLN)                                               
LdgName  LKOUT C,020,NAMEREC,CHAR,LEN=V                                         
         LKOUT E                                                                
                                                                                
ARYLACL  LKOUT A,(D,B#LDG,LDGRFST),EOT=EOR,ROWID=(ACLEL,ACLELQ),       +        
               ROWWIDTH=(V,ACLLN)                                               
PRout    LKOUT P,ACLEL,SETLVL                                                   
         LKOUT C,040,(D,B#WORKD,WORK+0),LBIN,LEN=1                              
         LKOUT C,041,ACLELLNA,CHAR                                              
         LKOUT C,042,(D,B#WORKD,WORK+1),LBIN,LEN=1,ND=Y                         
         LKOUT C,043,ACLELLNB,CHAR,ND=Y                                         
         LKOUT C,044,(D,B#WORKD,WORK+2),LBIN,LEN=1,ND=Y                         
         LKOUT C,045,ACLELLNC,CHAR,ND=Y                                         
         LKOUT C,046,(D,B#WORKD,WORK+3),LBIN,LEN=1,ND=Y                         
         LKOUT C,047,ACLELLND,CHAR,ND=Y                                         
         LKOUT E                                                                
                                                                                
SETLVL   L     R1,LP_AINP                                                       
         USING ACLELD,R1                                                        
         XC    WORK(4),WORK                                                     
         LLC   RE,ACLELLVA                                                      
         STC   RE,WORK+0           Length of level A                            
         SR    RF,RF                                                            
         ICM   RF,1,ACLELLVB                                                    
         JZ    EXIT                                                             
         SR    RF,RE                                                            
         STC   RF,WORK+1           Length of level B                            
         SR    RF,RF                                                            
         ICM   RF,1,ACLELLVC                                                    
         JZ    EXIT                                                             
         LLC   RE,ACLELLVB                                                      
         SR    RF,RE                                                            
         STC   RF,WORK+2           Length of level C                            
         SR    RF,RF                                                            
         ICM   RF,1,ACLELLVD                                                    
         JZ    EXIT                                                             
         LLC   RE,ACLELLVC                                                      
         SR    RF,RE                                                            
         STC   RF,WORK+3           Length of level D                            
         J     EXIT                                                             
         DROP  R1                                                               
                                                                                
ARYLAPR  LKOUT A,(D,B#LDG,LDGRFST),EOT=EOR,ROWID=(APRELD,APRELQ),      +        
               ROWWIDTH=(V,APRLN)                                               
         LKOUT C,060,APRSEQ,LBIN                                                
         LKOUT C,061,APRTLEN,LBIN,ND=Y                                          
         LKOUT C,062,APRNLEVS,LBIN,ND=Y                                         
         LKOUT C,063,APRDESC,CHAR,ND=Y                                          
         LKOUT C,064,APRELD,(R,EDTMSK)                                          
         LKOUT E                                                                
                                                                                
ARYLLDG  LKOUT A,(D,B#LDG,LDGRFST),EOT=EOR,ROWID=(LDGELD,LDGELQ),      +        
               ROWWIDTH=(V,LDGLN)                                               
         LKOUT C,085,LDGTYPE,CHAR,ND=Y                                          
         LKOUT C,086,LDGLIKE,CHAR,ND=Y                                          
         LKOUT C,087,LDGSTAT,MB80,ND=Y                                          
         LKOUT C,088,LDGSTAT,MB40,ND=Y                                          
         LKOUT C,089,LDGSTAT,MB20,ND=Y                                          
         LKOUT C,090,LDGSTAT,MB10,ND=Y                                          
         LKOUT C,091,LDGSTAT,MB08,ND=Y                                          
         LKOUT C,092,LDGSTAT,MB04,ND=Y                                          
         LKOUT C,093,LDGSTAT,MB02,ND=Y                                          
         LKOUT C,094,LDGSTAT,MB01,ND=Y                                          
         LKOUT C,095,LDGCPOS,LBIN,ND=Y                                          
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Read ledger records                                                 *         
***********************************************************************         
                                                                                
NXTLDG   GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',LDGKEYT),('B#LDG',0),    +        
               SAVED,0,0                                                        
         J     EXITY                                                            
                                                                                
REQEND   LKREQ X                                                                
         EJECT                                                                  
EDTCLT   LM    R2,R4,LP_AINP       Edit client code                             
         LLC   R1,LDGSJ+(LDGTLVA-LDGTABD)                                       
         BCTR  R1,0                                                             
         LARL  RB,EDTCMVC                                                       
         EX    R1,0(RB)                                                         
         LLC   R0,LDGSJ+(LDGTLVA-LDGTABD)                                       
         J     SETOLEN                                                          
                                                                                
EDTCMVC  MVC   0(0,R4),ACTKACT-ACTRECD(R2)                                      
         EJECT                                                                  
EDTPRD   LM    R2,R4,LP_AINP       Edit product code                            
         CLC   0(L'ACTKACT,R2),SPACES                                           
         JNH   XCOLEN                                                           
         LLC   R1,LDGSJ+(LDGTLVA-LDGTABD)                                       
         AHI   R1,ACTKACT-ACTRECD                                               
         LA    RF,0(R2,R1)                                                      
         LLC   R1,LDGSJ+(LDGTLVB-LDGTABD)                                       
         LLC   R0,LDGSJ+(LDGTLVA-LDGTABD)                                       
         SR    R1,R0                                                            
         BCTR  R1,0                                                             
         LARL  RB,EDTPMVC                                                       
         EX    R1,0(RB)                                                         
         LA    R0,1(R1)                                                         
         J     SETOLEN                                                          
                                                                                
EDTPMVC  MVC   0(0,R4),0(RF)                                                    
                                                                                
EDTMSK   LM    R2,R4,LP_AINP       Edit equivalent accounts list                
         USING APRELD,R2           R2=A(a/c equivalent rule element)            
         SR    R1,R1                                                            
         ICM   R1,1,APRNLEVS       R1=number of rules                           
         JZ    XCOLEN                                                           
         SR    R0,R0               R0=length of output                          
         LA    R3,APRMLEN                                                       
         USING APRMLEN,R3          R3=A(input array)                            
EDTMSK02 LLC   RF,APRMLEN          Get length of input entry                    
         AR    R0,RF               Bump output length by input length           
         ST    R0,LP_OLEN                                                       
         SHI   RF,1                                                             
         JNM   *+6                                                              
         DC    H'0'                Zero length entry                            
         LARL  RB,EDTMMVC                                                       
         EX    RF,0(RB)                                                         
         LA    R4,1(RF,R4)         Point to next output slot                    
         JCT   R1,*+8              Do for number of entries                     
         J     SETOLEN             Exit when all done                           
         MVI   0(R4),C','          Insert comma into output string              
         AHI   R4,1                                                             
         AHI   R0,1                and account for it in the length             
         LA    R3,2(RF,R3)         Point to next input entry                    
         J     EDTMSK02                                                         
                                                                                
EDTMMVC  MVC   0(0,R4),APRMASK     Move text to output string                   
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* Call OFFAL to build a list of offices if user has limit access      *         
***********************************************************************         
                                                                                
BLDOFF   NTR1  LABEL=*                                                          
                                                                                
         LA    R0,OFFWORK          Initialize for OFFAL call                    
         LHI   R1,L'OFFWORK                                                     
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR GETCPY              Locate company element                       
         LR    R4,R1                                                            
         USING CPYELD,R4                                                        
         MVC   OFFACST1,CPYSTAT1                                                
         MVC   OFFACST2,CPYSTAT2                                                
         MVC   OFFACST3,CPYSTAT3                                                
         MVC   OFFACST4,CPYSTAT4                                                
         MVC   OFFACST5,CPYSTAT5                                                
         MVC   OFFACST6,CPYSTAT6                                                
         MVC   OFFACST7,CPYSTAT7                                                
         MVC   OFFACST8,CPYSTAT8                                                
         MVI   OFFAACT,OFFAINI                                                  
         MVI   OFFACTRL,OFFACCNV                                                
         MVI   CPYINDS,0                                                        
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+8                                                              
         OI    CPYINDS,CPYIOFF2    Set 2 character office codes                 
                                                                                
         XC    OFFNUM,OFFNUM       Set no office limit access                   
         LA    R1,LP_ACCS          Deal with limit access                       
         CLC   0(2,R1),=C'**'      Test for office list                         
         JNE   *+8                                                              
         AHI   R1,2                                                             
         OC    0(2,R1),0(R1)       Test any limit access                        
         JZ    EXITY                                                            
                                                                                
         MVC   OFFACOMF,LP_ACOM                                                 
         MVC   OFFAALPH,LP_AGY                                                  
         MVC   OFFAAUTH,LP_AUTH                                                 
         MVC   OFFACPY,LP_AGYB                                                  
         MVC   OFFALIMA,LP_ACCS                                                 
                                                                                
         GOTOR VOFFAL,OFFALD       Call OFFAL to build office list              
         JE    *+6                                                              
         DC    H'0'                                                             
         LHI   R0,32               Max # of old offices (16/page*2)             
         TM    CPYINDS,CPYIOFF2                                                 
         JZ    BLDOFF02            # of offices is not incl in list             
         SR    R1,R1                                                            
         ICM   R1,3,OFFAWORK       R1=number of offices in list                 
         JZ    EXITY                                                            
         STCM  R1,3,OFFNUM         Set in saved storage                         
         LA    RE,OFFAWORK+2       Point to list of offices                     
         SLL   R1,1                R1=length of office list                     
         LR    RF,R1                                                            
         LA    R0,OFFLST                                                        
         MVCL  R0,RE                                                            
         J     BLDOFF08                                                         
                                                                                
BLDOFF02 LA    R2,OFFLST                                                        
         LA    R3,OFFAWORK+2                                                    
         SR    R0,R0               R0=number of offices in list                 
BLDOFF04 CLI   0(R3),0             Test for end of office list                  
         JE    BLDOFF06                                                         
         CLI   0(R3),C'0'          this ends the list too                       
         JE    BLDOFF06                                                         
         MVC   0(1,R2),0(R3)       Move entry to list                           
         AHI   R3,1                Bump input pointer                           
         AHI   R2,1                Bump output pointer                          
         AHI   R0,1                Bump number of offices                       
         J     BLDOFF04                                                         
BLDOFF06 STCM  R0,3,OFFNUM         Set number of offices in list                
                                                                                
BLDOFF08 LA    RF,OFFLST                                                        
         LLH   R0,OFFNUM                                                        
         LHI   R1,1                Width for 1 character offices                
         TM    CPYINDS,CPYIOFF2                                                 
         JZ    *+8                                                              
         LHI   R1,2                Width for 2 character offices                
         GOTOR SORTIT              Sort offices into ascending sequence         
         J     EXITY                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* Sort a list of values into ascending sequence                       *         
*                                                                     *         
* Ntry:- RF=A(list of values)                                         *         
*        R1=L'entry                                                   *         
*        R0=N'entries                                                 *         
***********************************************************************         
                                                                                
SORTIT   STM   RE,RC,12(RD)                                                     
         BASR  RB,0                                                             
         USING *,RB                                                             
         SHI   R0,1                R0=N'entries-1                               
         JNP   EXITY                                                            
         BCTR  R1,0                R1=L'data-1                                  
         LR    R2,RF                                                            
SORTIT02 LA    RF,1(R1,R2)                                                      
         LR    RE,R0                                                            
SORTIT04 EX    R1,SORTCLC          Test in sequence                             
         JNH   SORTIT06                                                         
         EX    R1,SORTXC2F         Swap entries if out of sequence              
         EX    R1,SORTXCF2                                                      
         EX    R1,SORTXC2F                                                      
SORTIT06 LA    RF,1(RF,R1)                                                      
         JCT   RE,SORTIT04                                                      
         LA    R2,1(R2,R1)                                                      
         JCT   R0,SORTIT02                                                      
SORTITX  LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
                                                                                
SORTCLC  CLC   0(0,R2),0(RF)                                                    
SORTXC2F XC    0(0,R2),0(RF)                                                    
SORTXCF2 XC    0(0,RF),0(R2)                                                    
         DROP  RB                                                               
                                                                                
***********************************************************************         
* Locate company element on company record                            *         
* Exit:- R1=A(company element)                                        *         
***********************************************************************         
                                                                                
GETCPY   L     R1,ACPYREC                                                       
         AHI   R1,CPYRFST-CPYRECD                                               
         USING CPYELD,R1                                                        
         SR    R0,R0                                                            
GETCPY02 CLI   CPYEL,0             Test end of record                           
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CPYEL,CPYELQ        Test company element                         
         BER   RE                                                               
         IC    R0,CPYLN            Bump to next element                         
         AR    R1,R0                                                            
         J     GETCPY02                                                         
         DROP  R1                                                               
                                                                                
NOMORE   MVI   LP_RMODE,LP_RLAST   Set end of data                              
         J     EXITY                                                            
                                                                                
XCOLEN   XC    LP_OLEN,LP_OLEN     Clear output field length & exit             
         J     EXITY                                                            
                                                                                
SETOLEN  STCM  R0,15,LP_OLEN       Set output field length & exit               
         J     EXITY                                                            
                                                                                
EXITN    LA    RE,1                                                             
         J     EXITCC                                                           
EXITY    SR    RE,RE                                                            
EXITCC   LTR   RE,RE                                                            
EXIT     XIT1  ,                                                                
         EJECT                                                                  
SYS1LIT  DC    C'Agency: system'                                                
AGY1LIT  DC    C'        agency alpha id'                                       
LIMALIT  DC    C'        limit access'                                          
USIDLIT  DC    C'        user id#'                                              
PIDNLIT  DC    C'        PID#'                                                  
SAGRLIT  DC    C'        access group#'                                         
PSAGLIT  DC    C'        person agency'                                         
SSYSLIT  DC    C'        security system#'                                      
SPRGLIT  DC    C'        security program#'                                     
SYS2LIT  DC    C'Client: system'                                                
AGY2LIT  DC    C'        agency alpha id'                                       
MEDCLIT  DC    C'        media code'                                            
CLTCLIT  DC    C'        client code'                                           
PRDCLIT  DC    C'        product code'                                          
TOKNLIT  DC    C'        request token'                                         
DCLTLIT  DC    C'Download clients?'                                             
DPRDLIT  DC    C'Download products?'                                            
DACCLIT  DC    C'Download accounts?'                                            
DCOMLIT  DC    C'Download comments?'                                            
DLDGLIT  DC    C'Download ledgers?'                                             
OPT3LIT  DC    C'Download option  3'                                            
OPT5LIT  DC    C'Download option  5'                                            
OPT6LIT  DC    C'Download option  6'                                            
OPT7LIT  DC    C'Download option  7'                                            
OPT8LIT  DC    C'Download option  8'                                            
OPT9LIT  DC    C'Download option  9'                                            
OPTALIT  DC    C'Download option 10'                                            
OPTBLIT  DC    C'Download option 11'                                            
OPTCLIT  DC    C'Download option 12'                                            
OPTDLIT  DC    C'Download option 13'                                            
OPTELIT  DC    C'Download option 14'                                            
OPTFLIT  DC    C'Download option 15'                                            
OPTGLIT  DC    C'Download option 16'                                            
         EJECT                                                                  
GLOBALS  DS    0D                  ** Global literals **                        
         LTORG ,                                                                
                                                                                
OFFRNG1L DC    X'41',C'9'          One character office range                   
OFFRNG2L DC    X'4041',C'99'       Two character office range                   
                                                                                
LDGKEYT  LKKEY H,LDGKEY,SAVED      ** Ledger key driver **                      
         LKKEY SIN,LDGKCPY,COMPANY                                              
         LKKEY LST,LDGKUNT,LDGLST2,L'LDGKUNT+L'LDGKLDG                          
         LKKEY LIT,LDGKEY+LDGKEND,C' ',L'LDGKEY-LDGKEND                         
         LKKEY E                                                                
                                                                                
LDGLST1L DS    0H                  ** Ledger list for accounts **               
LDGLST1N DC    AL2(LDGLULL1/L'LDGLUL1)                                          
LDGLUL1  DS    0CL(L'ACTKUNT+L'ACTKLDG)                                         
         DC    C'SR'               Debtors ledger                               
         DC    C'1C'               Costing client ledger                        
LDGLULL1 EQU   *-LDGLUL1                                                        
LDGLSTL1 EQU   *-LDGLST1L                                                       
                                                                                
LDGLST2L DS    0H                  ** Ledger list for ledgers **                
LDGLST2N DC    AL2(LDGLULL2/L'LDGLUL2)                                          
LDGLUL2  DS    0CL(L'ACTKUNT+L'ACTKLDG)                                         
LDGLSJ   DC    C'SJ'               Production ledger                            
LDGLSR   DC    C'SR'               Debtors ledger                               
LDGL1C   DC    C'1C'               Costing client ledger                        
LDGLULL2 EQU   *-LDGLUL2                                                        
LDGLSTL2 EQU   *-LDGLST2L                                                       
                                                                                
FILES    DS    0C                  ** System/file list **                       
         DC    C'ACCOUNT'                                                       
         DC    C'N'                                                             
ACCDIR   DC    C'ACCDIR '                                                       
         DC    C'N'                                                             
ACCMST   DC    C'ACCMST '                                                       
         DC    C'N'                                                             
ACCARC   DC    C'ACCARC '                                                       
         DC    C'N'                                                             
CTFILE   DC    C'CTFILE '                                                       
         DC    C'X'                                                             
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
                                                                                
EOR      EQU   0                   End of record element code                   
                                                                                
SAVED    DSECT ,                   ** Dsect to cover saved storage **           
                                                                                
GETUID   DS    A                   A(GETUID)                                    
                                                                                
ALDG     DS    A                   A(current unit)                              
LDGSJ    DS    XL(LDGTABL2)        Production ledger values                     
LDGSR    DS    XL(LDGTABL2)        Debtors ledger values                        
LDG1C    DS    XL(LDGTABL2)        Costing client ledger values                 
LDGN     DS    AL2                 Number of ledgers to process                 
                                                                                
SYSLET   DS    C                   ** System letter **                          
SYSACCQ  EQU   C'A'                Account system letter                        
                                                                                
COMPANY  DS    XL(L'LP_AGYB)       Company code                                 
UNITLDGR DS    CL(L'ACTKUNT+L'ACTKLDG)                                          
                                                                                
LVARNGE  DS    XL(L'ACTKACT*2)     Level A account range                        
ACCRNGE  DS    XL(L'ACTKACT*2)     Account range                                
LDGLST2  DS    XL(LDGLSTL2)        Ledger list for ledgers                      
OFFRNG1  DS    XL(1*2)             Office range - 1 character offices           
OFFRNG2  DS    XL(L'OFFKOFF*2)     Office range - 2 character offices           
PRDRNGE  DS    XL(L'CA_PRDC*2)     Product range built here                     
REQTOK   DS    CL(L'CA_TOKEN)      Client request array token                   
                                                                                
KEYWORKL DS    XL2                 Total width of key                           
KEYWORK  DS    XL64                Key driver tables built here                 
                                                                                
QVALUES  DS    0F                  ** Request values **                         
                                                                                
QAGYIND  DS    X                   Agency array                                 
QAAGY    DS    AL3                                                              
                                                                                
QCLTIND  DS    X                   Client array                                 
QACLT    DS    AL3                                                              
                                                                                
QCLTOPT  DS    C                   Download client?                             
QPRDOPT  DS    C                   Download product?                            
QACCOPT  DS    C                   Download account?                            
QLDGOPT  DS    C                   Download ledgers?                            
QTRASH1  DS    C                   One byte trash goes here                     
                                                                                
CPYINDS  DS    X                   ** Company indicators **                     
CPYIOFF2 EQU   X'80'               2 character office codes in use              
                                                                                
PRDCODE  DS    CL(L'CA_PRDC)       Current product code                         
FIRSTCLI DS    C                   First client for agency flag                 
DUMMYCLI DS    C                   Current client is 'dummy'                    
ANYCLI   DS    C                   Any client requests run                      
                                                                                
QVALUEL  EQU   *-QVALUES                                                        
                                                                                
LAGY     DS    CL(L'CA_AGYA)       Last agency processed                        
ANXTCLT  DS    A                   A(next client code in work map pool)         
NUMCLT   DS    XL(L'LW_NUMN)       N'client codes left to process               
                                                                                
SVCLTKEY DS    CL(L'ACTKEY)        Client record key                            
                                                                                
OFFVALS  DS    0X                  ** Office values **                          
OFFCODE  DS    CL2                 Office code                                  
OFFNAME  DS    CL(L'NAMEREC)       Office name                                  
OFFVALL  EQU   *-OFFVALS                                                        
                                                                                
OFFNUM   DS    XL2                 Number of offices                            
OFFLST   DS    XL(ONEK)            List of valid office                         
OFFLSTL  EQU   *-OFFLST                                                         
         DS    0F                                                               
OFFWORK  DS    XL(OFFALLEN)        OFFAL control block                          
                                                                                
* Other included books follow                                                   
                                                                                
         PRINT OFF                                                              
       ++INCLUDE ACLNKWRKD                                                      
       ++INCLUDE GEMAPEQUS                                                      
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACLNK09   08/20/15'                                      
         END                                                                    
