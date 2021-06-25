*          DATA SET NELNK11    AT LEVEL 031 AS OF 10/31/17                      
*PROCESS USING(WARN(15))                                                        
*PHASE T30211E                                                                  
NELNK11  TITLE '- NETWORK CONSTRUCTOR -'                                        
                                                                                
SVRDEF   CSECT                                                                  
         LKSVR IDF=Y,REQUEST=*,CODE=CODE,FILES=FILES,FACS=FACS,        *        
               ABENDLIST=ABENDS,SLOWLIST=SLOWS,WORKERKEY=NEES,         *        
               SERVERTYPE=C'X',SEGMENT=Y,SYSTEM=NETSYSQ,APPEND=Y,      *        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED,B#DEMTAB,DEMOENTD), *        
               TYPE=D,SYSPHASE=SYSPHASE,AUTOCLEAR=Y,LOADFACSOFF=Y               
                                                                                
***********************************************************************         
* NETWORK CONSTRUCTOR SERVER CODE                                     *         
*                                                                               
* The first 1500 bytes of IO5 is being used for the "PREMIER TABLE"             
* this area should not be used.                              NSIL               
***********************************************************************         
                                                                                
CODE     NMOD1 0,**NL11*,CLEAR=YES,RR=RE                                        
         LR    R5,R1                                                            
         USING LP_D,R5                                                          
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BNZ   INIT02              YES                                          
         L     R9,LP_ABLK1         NO - ROOT PROVIDES WORKD/SAVED               
         L     R8,LP_ABLK2                                                      
         B     INIT04                                                           
                                                                                
INIT02   L     R9,RSVRSAVE                                                      
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         SR    R8,R8                                                            
         ICM   R8,3,WORKLEN                                                     
         LA    R8,WORKD(R8)                                                     
         USING SAVED,R8,RA         R8=A(SAVE W/S)                               
         ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
                                                                                
INIT04   LR    RA,R8                                                            
         AHI   RA,4096                                                          
         ST    R5,ALP              SAVE A(DDLINK PARAMETER LIST)                
         ST    RE,RELO             SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
                                                                                
         MVC   AGYALPH,LP_AGY                                                   
                                                                                
         USING TSARD,TSARBLK                                                    
         USING SUMRECD,TSARREC                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE FOR RUNNING                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNPMODE,RRUNSTRQ   TEST FIRST FOR RUN                           
         BNE   PRCWRK                                                           
                                                                                
         LA    R0,SAVECLR          CLEAR DOWN SAVE AREA                         
         LHI   R1,SAVECLRL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         BASR  RE,0                MOVE LITERALS TO SAVED STORAGE               
         AHI   RE,LVALUES-*                                                     
         LA    R0,WVALUES                                                       
         LHI   R1,WVALUEL                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R1,ADCONS           RELOCATE ADDRESS CONSTANTS                   
         LHI   R0,ADCONSN                                                       
         L     RF,RELO                                                          
         BASR  RE,0                                                             
         L     R2,0(R1)                                                         
         AR    R2,RF                                                            
         ST    R2,0(R1)                                                         
         AHI   R1,L'ADCONS                                                      
         BCTR  R0,RE                                                            
                                                                                
B#DEMTAB EQU   3                   TABLE OF DEMOGRAPHICS BY MODIFIER            
         LA    RE,DEMOTAB                                                       
         STCM  RE,15,LP_BLKS+((B#DEMTAB-1)*L'LP_BLKS)                           
                                                                                
         L     RF,RCOMFACS         LOAD FACILITIES OVERLAYS                     
         ST    RF,ACOMFACS                                                      
                                                                                
         L     RF,CDEMTABS-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,NENACWKS  GET A(MONTH<->WEEK) TABLE                    
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                 TEMP                                         
         DC    H'0'                BAD ID PASSED                                
         ST    RE,ANADWKS                                                       
                                                                                
         L     RF,ACOMFACS                                                      
         L     RF,CDEMTABS-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,NENTIQRT  GET NTI QUARTER CALENDAR                     
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                 TEMP                                         
         DC    H'0'                BAD ID PASSED                                
         ST    RE,ANTIQTAB                                                      
                                                                                
         TM    LP_FLAG1,LP_FOFFL   TEST OFF-LINE                                
         BZ    RUNST20                                                          
         MVC   ALET,RALET          EXTRACT RUNNER ALET                          
         B     RUNST30                                                          
RUNST20  MVC   DMCB,EFFS           EXTRACT ALET FROM SSB                        
         MVI   DMCB,X'FE'                                                       
         GOTOR SWITCH,DMCB                                                      
         L     R1,0(R1)                                                         
         L     R1,VSSB-SYSFACD(R1)                                              
         MVC   ALET,SSBTBLET-SSBD(R1)                                           
                                                                                
RUNST30  TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JZ    EXITY                                                            
         MVC   ATWA,LP_ATWA                                                     
         MVC   AMASTC,RMASTC                                                    
                                                                                
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('P#ROUTS1',0),0,0                                     
         MVC   AROUTS1,0(R1)                                                    
         MVC   LP_AUIR1,AROUTS1    SET A(INDEX ROUTINES 1)                      
         GOTOR (RF),DMCB,('P#ROUTS2',0),0,0                                     
         MVC   AROUTS2,0(R1)                                                    
         MVC   LP_AUIR2,AROUTS2    SET A(INDEX ROUTINES 2)                      
                                                                                
         GOTOR (#WRKINI,AWRKINI)   INITIALIZE WORKING STORAGE                   
                                                                                
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* FIRST FOR NEW WORK                                                  *         
***********************************************************************         
                                                                                
PRCWRK   CLI   RUNPMODE,RPRCWRKQ   TEST 'PROCESS WORK' MODE                     
         BNE   RUNREQ                                                           
                                                                                
         LA    R0,SAVECLR          CLEAR DOWN SAVE AREA                         
         LHI   R1,SAVECLRL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* RUN A DOWNLOAD REQUEST                                              *         
***********************************************************************         
                                                                                
RUNREQ   CLI   RUNPMODE,RRUNREQQ   TEST 'RUN REQUEST' MODE                      
         JNE   EXITY                                                            
                                                                                
         ICM   RF,15,AMASTC        SET TRACE OPTION IF OFFLINE                  
         BZ    *+10                                                             
         MVC   IOTRACE,MCTRACE-MASTD(RF)                                        
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BZ    RUNREQ02            CLEAR DATA MANAGER BUFFERS                   
         GOTOR DATAMGR,DMCB,DMKEY,SPTDIR,(4,0),0                                
         GOTOR DATAMGR,DMCB,DMKEY,SPTFIL,(4,0),0                                
                                                                                
RUNREQ02 CLC   LP_QMAPN,CNSREP#                                                 
         JE    RUNCNSR                                                          
         CLC   LP_QMAPN,PDOWN#                                                  
         JE    RUNPDOWN                                                         
         CLC   LP_QMAPN,DEFORM#                                                 
         JE    RUNDEFO                                                          
         J     EXITY                                                            
                                                                                
RUNCNSR  GOTOR CNSDOWN                                                          
         J     EXITY                                                            
RUNPDOWN GOTOR PRGDOWN                                                          
         J     EXITY                                                            
RUNDEFO  GOTOR DEFODOWN                                                         
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* SET ADDRESS OF CURRENT DEMO LIST VALUES                             *         
***********************************************************************         
                                                                                
SETDEM   L     R1,LP_AINP                                                       
         USING DEMOENTD,R1                                                      
         LA    RF,DEMSP8                                                        
         ST    RF,ADEMVS                                                        
         XC    LP_OLEN,LP_OLEN                                                  
         DROP  R1                                                               
         J     EXITY                                                            
                                                                                
***********************************************************************         
* SET ADDRESS OF CURRENT DEMO LIST VALUES FROM PROGRAM RECORD         *         
***********************************************************************         
                                                                                
SETPDEM  L     R1,LP_AINP                                                       
         USING DEMPENTD,R1                                                      
         LA    RF,DEMPVAL                                                       
         ST    RF,ADEMVS                                                        
         XC    LP_OLEN,LP_OLEN                                                  
         DROP  R1                                                               
         J     EXITY                                                            
                                                                                
***********************************************************************         
* BUMP THROUGH COMPUTATIONAL FORMULAS ONE BY ONE                      *         
***********************************************************************         
                                                                                
         USING DEMFDEMD,R2                                                      
NXTCOMP  LA    R0,ODEMNO                                                        
         ST    R0,LP_ADATA                                                      
                                                                                
         STAR  CLEAR=Y,ARS=ON                                                   
         LAM   AR2,AR2,ALET                                                     
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   NXTCMP10                                                         
         L     R2,ACOMPUTS                                                      
         B     NXTCMP20                                                         
                                                                                
NXTCMP10 L     R2,ALASTCMP                                                      
         SR    RE,RE                                                            
         ICM   RE,3,DEMFDLN                                                     
         AR    R2,RE                                                            
                                                                                
NXTCMP20 ST    R2,ALASTCMP                                                      
         CLI   0(R2),FF                                                         
         JE    XRNOMORE                                                         
                                                                                
         LA    RE,DEMFDILS                                                      
         ST    RE,AINDEXES         A(INDEXES OF DEMOS IN FORMULA)               
         MVC   ODEMNO,DEMFDNUM                                                  
                                                                                
         SR    RF,RF                                                            
         ICM   RF,3,DEMFDLN                                                     
         SHI   RF,L'DEMFDLN+L'DEMFDNUM                                          
         SR    RE,RE                                                            
         SR    R0,R0                                                            
         LHI   R0,L'DEMFDILS                                                    
         DR    RE,R0                                                            
         STCM  RF,3,INDXS#         NUMBER OF INDEXES IN THE LIST                
         J     XREXITY                                                          
         DROP  R2                                                               
                                                                                
***********************************************************************         
* BUMP THROUGH INDEXES ONE BY ONE                                     *         
***********************************************************************         
                                                                                
NXTIND   LA    R0,OINDEX                                                        
         ST    R0,LP_ADATA                                                      
                                                                                
         STAR  CLEAR=Y,ARS=ON                                                   
         LAM   AR2,AR2,ALET                                                     
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   NXTIND10                                                         
         L     R2,AINDEXES                                                      
         B     NXTIND20                                                         
                                                                                
NXTIND10 L     R2,ALASTIND                                                      
         LA    R2,L'DEMFDILS(R2)                                                
                                                                                
NXTIND20 ST    R2,ALASTIND                                                      
                                                                                
         SR    RE,RE             UPDATE NUMBER OF INDEXES LEFT TO               
         ICM   RE,3,INDXS#       PROCESS                                        
         BZ    XRNOMORE          FINISHED PROCESSING. EXIT                      
         BCTR  RE,0                                                             
         STCM  RE,3,INDXS#                                                      
                                                                                
         MVC   OINDEX,0(R2)                                                     
                                                                                
         MVI   LP_RMODE,LP_RMORE                                                
         J     XREXITY                                                          
                                                                                
***********************************************************************         
* BUMP THROUGH BUILDING BLOCKS ONE BY ONE                             *         
***********************************************************************         
                                                                                
NXTBLDBK LA    R0,OBLDBLK                                                       
         ST    R0,LP_ADATA                                                      
                                                                                
         STAR  CLEAR=Y,ARS=ON                                                   
         LAM   AR2,AR2,ALET                                                     
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   NXTBLD10                                                         
         L     R2,ABLDBLKS                                                      
         B     NXTBLD20                                                         
                                                                                
NXTBLD10 L     R2,ALASTBLD                                                      
         LA    R2,L'DEMFB4DL(R2)                                                
                                                                                
NXTBLD20 ST    R2,ALASTBLD                                                      
         CLI   0(R2),FF                                                         
         JE    XRNOMORE            XA MODE OFF AND DONE                         
                                                                                
         MVC   OBLDBLK,0(R2)       MOVE BUILDING BLOCK TO OUTPUT                
                                                                                
         MVI   LP_RMODE,LP_RMORE                                                
         J     XREXITY             XA MODE OFF AND EXIT                         
                                                                                
***********************************************************************         
* BUMP THROUGH SPECIFIC MACRO FORMULAS ONE BY ONE                     *         
***********************************************************************         
                                                                                
         USING DEMFSMCD,R2                                                      
NXTSMAC  LA    R0,OFSMD                                                         
         ST    R0,LP_ADATA                                                      
                                                                                
         STAR  CLEAR=Y,ARS=ON                                                   
         LAM   AR2,AR2,ALET                                                     
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   NXTSMC10                                                         
         ICM   R2,15,AFSMACRO                                                   
         JZ    XRNOMORE                                                         
         B     NXTSMC20                                                         
                                                                                
NXTSMC10 L     R2,ALASTSMC                                                      
         LA    R2,DEMFSMLQ(R2)                                                  
                                                                                
NXTSMC20 ST    R2,ALASTSMC                                                      
         CLI   0(R2),FF                                                         
         JE    XRNOMORE            XA MODE OFF AND DONE                         
                                                                                
         MVC   OFSMD,DEMFSMD       MOVE MODIFIER TO OUTPUT                      
         MVC   OFSEXP,DEMFSEXP     MOVE MACRO EXPRESSION TO OUTPUT              
         MVC   OFSDNO,DEMFSDNO     MOVE DEMO NUMBER TO OUTPUT                   
                                                                                
         MVI   LP_RMODE,LP_RMORE                                                
         J     XREXITY             XA MODE OFF AND EXIT                         
         DROP  R2                                                               
                                                                                
***********************************************************************         
* BUMP THROUGH GENERAL MACRO FORMULAS ONE BY ONE                      *         
***********************************************************************         
                                                                                
         USING DEMFGMCD,R2                                                      
NXTGMAC  LA    R0,OFSMD                                                         
         ST    R0,LP_ADATA                                                      
                                                                                
         STAR  CLEAR=Y,ARS=ON                                                   
         LAM   AR2,AR2,ALET                                                     
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   NXTGMC10                                                         
         ICM   R2,15,AFGMACRO                                                   
         JZ    XRNOMORE                                                         
         B     NXTGMC20                                                         
                                                                                
NXTGMC10 L     R2,ALASTGMC                                                      
         LA    R2,DEMFGMLQ(R2)                                                  
                                                                                
NXTGMC20 ST    R2,ALASTGMC                                                      
         CLI   0(R2),FF                                                         
         JE    XRNOMORE            XA MODE OFF AND DONE                         
                                                                                
         MVC   OFGMD,DEMFGMD       MOVE MODIFIER TO OUTPUT                      
         MVC   OFGEXP,DEMFGEXP     MOVE MACRO EXPRESSION TO OUTPUT              
                                                                                
         MVI   LP_RMODE,LP_RMORE                                                
         J     XREXITY             XA MODE OFF AND EXIT                         
         DROP  R2                                                               
                                                                                
***********************************************************************         
* BUMP THROUGH OVERRIDE RELATIONAL FORMULAS ONE BY ONE                *         
***********************************************************************         
                                                                                
         USING DEMFOVRD,R2                                                      
NXTOVRF  LA    R0,OFSMD                                                         
         ST    R0,LP_ADATA                                                      
                                                                                
         STAR  CLEAR=Y,ARS=ON                                                   
         LAM   AR2,AR2,ALET                                                     
                                                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   NXTOVR10                                                         
         ICM   R2,15,AFOVRELS                                                   
         JZ    XRNOMORE                                                         
         B     NXTOVR20                                                         
                                                                                
NXTOVR10 L     R2,ALASTOVR                                                      
         LA    R2,DEMFOVLQ(R2)                                                  
                                                                                
NXTOVR20 ST    R2,ALASTOVR                                                      
         CLI   0(R2),FF                                                         
         JE    XRNOMORE            XA MODE OFF AND DONE                         
                                                                                
         MVC   OFOMD,DEMFOMD       MOVE RESULT MODIFIER TO OUTPUT               
         MVC   OFOFMD,DEMFOFMD     MOVE FROM MODIFIER TO OUTPUT                 
         MVC   OFOEXP,DEMFOEXP     MOVE EXPRESSION TO OUTPUT                    
                                                                                
         MVI   LP_RMODE,LP_RMORE                                                
         J     XREXITY             XA MODE OFF AND EXIT                         
         DROP  R2                                                               
***********************************************************************         
* Edit comScore Program Rotation                                                
***********************************************************************         
                                                                                
EDTROT   L     R1,LP_AINP                                                       
         MVI   DUB,C'N'                                                         
         TM    0(R1),NPCPSUNQ      SUNDAY                                       
         JZ    *+8                                                              
         MVI   DUB,C'Y'                                                         
         MVI   DUB+1,C'N'                                                       
         TM    0(R1),NPCPMONQ      MONDAY                                       
         JZ    *+8                                                              
         MVI   DUB+1,C'Y'                                                       
         MVI   DUB+2,C'N'                                                       
         TM    0(R1),NPCPTUEQ      TUESDAY                                      
         JZ    *+8                                                              
         MVI   DUB+2,C'Y'                                                       
         MVI   DUB+3,C'N'                                                       
         TM    0(R1),NPCPWEDQ      WEDNESDAY                                    
         JZ    *+8                                                              
         MVI   DUB+3,C'Y'                                                       
         MVI   DUB+4,C'N'                                                       
         TM    0(R1),NPCPTHUQ      THURSDAY                                     
         JZ    *+8                                                              
         MVI   DUB+4,C'Y'                                                       
         MVI   DUB+5,C'N'                                                       
         TM    0(R1),NPCPFRIQ      FRIDAY                                       
         JZ    *+8                                                              
         MVI   DUB+5,C'Y'                                                       
         MVI   DUB+6,C'N'                                                       
         TM    0(R1),NPCPSATQ      SATURDAY                                     
         MVI   DUB+6,C'Y'                                                       
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Edit comScore Program Viewing Type                                            
***********************************************************************         
                                                                                
EDTVT    L     R1,LP_AINP                                                       
         MVC   HALF1,=C'RL'                                                     
         CLI   0(R1),NPCVTRCQ      RC?                                          
         JNE   *+10                                                             
         MVC   HALF1,=C'RC'                                                     
         CLI   0(R1),NPCVTR3Q      R3?                                          
         JNE   *+10                                                             
         MVC   HALF1,=C'R3'                                                     
         CLI   0(R1),NPCVTR7Q      R7?                                          
         JNE   *+10                                                             
         MVC   HALF1,=C'R7'                                                     
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Edit comScore Program Source                                                  
***********************************************************************         
                                                                                
EDTSRCE  L     R1,LP_AINP                                                       
         MVC   FULL1,=CL3'PAV'                                                  
         CLI   0(R1),NPCVTPQ       TIME PERIOD?                                 
         JNE   *+10                                                             
         MVC   FULL1,=CL3'TP '                                                  
         J     EXITY                                                            
                                                                                
***********************************************************************         
* Edit comScore Program Times                                                   
***********************************************************************         
                                                                                
EDTTIME  L     R1,LP_AINP                                                       
         MVC   FULL1,0(R1)                                                      
         MVC   PTIMES,SPACES                                                    
         GOTOR UNTIME,DMCB,FULL1,PTIMES    TIMES                                
         J     EXITY                                                            
                                                                                
***********************************************************************         
* EDIT NEGATIVE INDEXES                                               *         
***********************************************************************         
                                                                                
EDTINDX  L     R1,LP_AINP                                                       
         L     RE,LP_AOUT                                                       
         EDIT  (2,(R1)),(INDXOLEN,(RE)),ALIGN=LEFT,FLOAT=-,ZERO=NOBLANK         
         LHI   R0,INDXOLEN                                                      
         ST    R0,LP_OLEN                                                       
         J     EXITY                                                            
INDXOLEN EQU   5                                                                
                                                                                
***********************************************************************         
* EDIT OVERRIDE MODIFIER                                              *         
***********************************************************************         
                                                                                
EDTOMOD  LM    R2,R4,LP_AINP                                                    
                                                                                
         MVI   0(R4),OVIDQ         OVERRIDE IDENTIFIER                          
         MVC   1(1,R4),0(R2)       MODIFIER FROM ELEMENT                        
         TM    PFLAG1,NPGAIGAQ     GAA-BASED RECORD?                            
         BZ    EDTOMODX                                                         
         CLI   1(R4),VPHMOD        YES                                          
         BNE   *+8                                                              
         MVI   1(R4),GAVPHMOD      SEND GAA-VPH MODOFIER                        
                                                                                
EDTOMODX J     EXITY                                                            
                                                                                
OMODLN   EQU   2                                                                
OVIDQ    EQU   C'O'                                                             
         EJECT                                                                  
***********************************************************************         
* EDIT USER DEMO VALUE                                                *         
***********************************************************************         
                                                                                
EDTUVAL  LM    R2,R4,LP_AINP                                                    
                                                                                
         USING UDEVPD,R2                                                        
         MVC   DUB(2),UDEVPH                                                    
         CLI   UDEVPFMT,UTHOUSQ                                                 
         BE    EDTUV10                                                          
         LH    R0,DUB                                                           
         MHI   R0,10                                                            
         STH   R0,DUB                                                           
                                                                                
EDTUV10  EDIT  (B2,DUB),(UVALN,(R4)),0,ALIGN=LEFT                               
                                                                                
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
UTHOUSQ  EQU   C'T'                NUMBER FORMAT T=THOUSANDS                    
         EJECT                                                                  
***********************************************************************         
* CONVERT TYPE-3 TO TYPE-4 DEMO NUMBERS                               *         
***********************************************************************         
GETYP4D  LM    R2,R4,LP_AINP                                                    
                                                                                
         GOTOR (#TYP3TO4,ATYP3TO4),DMCB,0(R2),HALF1                             
         EDIT  HALF1,(5,(R4)),ALIGN=LEFT                                        
                                                                                
         SR    R0,R0               GET LENGTH OF OUTPUT                         
         LR    RE,R4                                                            
GET4_10  CLI   0(RE),C' '                                                       
         BE    GET4_20                                                          
         AHI   R0,1                                                             
         LA    RE,1(RE)                                                         
         B     GET4_10                                                          
GET4_20  STCM  R0,15,LP_OLEN                                                    
         J     EXITY                                                            
                                                                                
         EJECT                                                                  
                                                                                
                                                                                
XRNOMORE LAM   AR0,ARF,ARZEROS     CLEAR XR'S                                   
         LA    RE,*+6                                                           
         BSM   0,RE                                                             
         REAR  ARS=OFF             XA MODE OFF                                  
NOMORE   MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS AND EXIT                 
         J     EXITY                                                            
                                                                                
XREXITY  LAM   AR0,ARF,ARZEROS     CLEAR XR'S                                   
         LA    RE,*+6                                                           
         BSM   0,RE                                                             
         REAR  ARS=OFF             XA MODE OFF                                  
EXITY    LHI   RE,1                CC=EQUAL FOR YES                             
         J     EXITCC                                                           
                                                                                
XREXITN  LAM   AR0,ARF,ARZEROS     CLEAR XR'S                                   
         LA    RE,*+6                                                           
         BSM   0,RE                                                             
         REAR  ARS=OFF             XA MODE OFF                                  
EXITN    LHI   RE,0                CC=NOT EQUAL FOR NO                          
                                                                                
EXITCC   CHI   RE,1                SET CONDITION CODE                           
                                                                                
EXIT     XIT1  ,                                                                
                                                                                
GETEL    AH    R4,DATADISP                                                      
         J     NEXTEL2                                                          
NEXTEL   CLI   0(R4),0                                                          
         JE    NEXTELX                                                          
         ZIC   R0,1(R4)                                                         
         LTR   R0,R0                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
NEXTEL2  CLI   0(R4),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCODE,0(R4)                                                     
         JNE   NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SYSTEM/FILE OPEN LIST                                               *         
***********************************************************************         
                                                                                
FILES    DS    0X                                                               
         DC    C'SPOT   '                                                       
                                                                                
         DC    C'NSPTDIR '                                                      
         DC    C'NSPTFILE'                                                      
         DC    C'NCTFILE '                                                      
         DC    C'NGENDIR '                                                      
         DC    C'NGENFIL '                                                      
         DC    C'NSTAFILE'                                                      
         DC    C'NNTIDIR '                                                      
         DC    C'NL=NTIFL'                                                      
         DC    C'NDEMDIRA'                                                      
         DC    C'NDEMDIRN'                                                      
         DC    C'NDEMDIRR'                                                      
         DC    C'NL=DEMFA'                                                      
         DC    C'NL=DEMFN'                                                      
         DC    C'NL=DEMFR'                                                      
         DC    C'X'                                                             
                                                                                
***********************************************************************         
* SYSTEM FACILITIES LIST                                              *         
***********************************************************************         
                                                                                
FACS     DS    0XL(RFACTABL)       ** SYSTEM FACILITIES **                      
         DC    AL1(QDEMAND),AL2(CDEMAND-COMFACSD,DEMAND-COMADDR)                
         DC    AL1(QDEMOUT),AL2(CDEMOUT-COMFACSD,DEMOUT-COMADDR)                
         DC    AL1(QDEMAINT),AL2(CDEMAINT-COMFACSD,0)                           
         DC    AL1(QDEMADDR),AL2(CDEMADDR-COMFACSD,DEMADDR-COMADDR)             
         DC    AL1(QDDISP),AL2(CT00AD0-COMFACSD,0)                              
         DC    AL1(QDEMEL),AL2(CDEMEL-COMFACSD,0)                               
         DC    AL1(QDEMOMTH),AL2(CDEMOMTH-COMFACSD,0)                           
         DC    AL1(QDEMTABS),AL2(CDEMTABS-COMFACSD,0)                           
         DC    AL1(0),AL2(CDATAMGR-COMFACSD,DATAMGR-COMADDR)                    
         DC    AL1(0),AL2(CADDAY-COMFACSD,ADDAY-COMADDR)                        
         DC    AL1(0),AL2(CDATCON-COMFACSD,DATCON-COMADDR)                      
         DC    AL1(0),AL2(CGETDAY-COMFACSD,GETDAY-COMADDR)                      
         DC    AL1(0),AL2(CSWITCH-COMFACSD,SWITCH-COMADDR)                      
         DC    AL1(RFACEOTQ)                                                    
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
SPTDIR   DC    C'SPTDIR '                                                       
SPTFIL   DC    C'SPTFILE'                                                       
                                                                                
ABENDS   DC    C'BPOO,DEIS,WHOA:'                                               
SLOWS    DC    C'BPOO,DEIS,WHOA:'                                               
                                                                                
WORKLEN  DC    AL2(((WORKL+7)/8)*8)                                             
                                                                                
CNSREP#  DC    AL2(M#CNRP)         NET CONSTRUCTOR REPORT                       
PDOWN#   DC    AL2(M#PDOWN)        PROGRAM RECORDS DOWNLOAD                     
DEFORM#  DC    AL2(M#DEFORM)       DEMO ENGINE DOWNLOAD - FORMULAS              
         EJECT                                                                  
                                                                                
         EJECT                                                                  
LVALUES  DS    0D                                                               
         DC    A(SUMBUF)                                                        
         DC    A(DAYTAB)                                                        
         DC    A(QTRTAB)                                                        
         DC    A(LVDFTAB)                                                       
         DC    A(TABLESL)                                                       
         DC    A(VIEWTYPT)                                                      
         DC    A(NADDERV)                                                       
         DC    F'100'                                                           
         DC    F'30'                                                            
         DC    F'0'                                                             
         DC    H'60'                                                            
         DC    CL8'TYPE'                                                        
         DC    CL8'PTYP4'                                                       
         DC    CL8'DAY'                                                         
         DC    CL8'TIME'                                                        
         DC    CL8'NTI'                                                         
         DC    CL8'PROG25'                                                      
         DC    CL8'WEEK'                                                        
         DC    CL8'NTIL'                                                        
         DC    CL8'BKOUT'                                                       
         DC    CL8'GAA'                                                         
         DC    CL8'FEED'                                                        
         DC    CL8'COMS'                                                        
         DC    CL8'COMT'                                                        
         DC    CL8'NLIV'                                                        
         DC    CL8'PREM'                                                        
         DC    PL1'0'                                                           
         DC    PL8'0'                                                           
         DC    H'600'                                                           
         DC    H'559'                                                           
         DC    AL2(0600,0545)                                                   
         DC    C'CAB'                                                           
         DC    C'CB2'                                                           
         DC    C'HUT '                                                          
         DC    C'WY'                                                            
         DC    C'WU'                                                            
         DC    C'WB'                                                            
         DC    CL4'NLIV',XL5'00'   4(DBXLIVNX) + 1(DBXLIVE)                     
         DC    CL4'CAVG',XL5'00'   4(DBXCAVNX) + 1(DBXCAV)                      
LVALUESX DS    0X                                                               
         LTORG                                                                  
         EJECT                                                                  
SUMBUF   BUFFD TYPE=D,KEYLEN=SUMKEYL,COMLEN=SUMDATAL,BUFFERS=1000               
         EJECT                                                                  
                                                                                
TABLESL  DC    X'D0',3X'00'        DEMDISP TABLE                                
         DC    X'E7',3X'00'        TABLE OF NEW FORMULAS FOR AE                 
         DC    X'FF'                                                            
                                                                                
DAYTAB   DC    X'40',CL10'Mon'     WEEK DAYS                                    
         DC    X'20',CL10'Tue'                                                  
         DC    X'10',CL10'Wed'                                                  
         DC    X'08',CL10'Thu'                                                  
         DC    X'04',CL10'Fri'                                                  
         DC    X'02',CL10'Sat'                                                  
         DC    X'01',CL10'Sun'                                                  
         DC    X'7C',CL10'M-F'                                                  
         DC    X'7F',CL10'M-Su'                                                 
         DC    X'FF',C'Various'                                                 
                                                                                
QTRTAB   DS    0CL10               BROADCAST QUARTERS                           
         DC    C'Q1',C'01150316'                                                
         DC    C'Q2',C'04150616'                                                
         DC    C'Q3',C'07150911'                                                
         DC    C'Q4',C'09111216'                                                
         DC    X'FF'                                                            
                                                                                
LVDFTAB  DC    AL1(LVPROG),AL4(PGNAME-SAVED),AL1(L'PGNAME)                      
         DC    AL1(LVPROG),AL4(NTINO-SAVED),AL1(L'NTINO)                        
         DC    AL1(LVPROG),AL4(LNTINO-SAVED),AL1(L'LNTINO)                      
         DC    AL1(LVTRAK),AL4(TRACK-SAVED),AL1(L'TRACK)                        
         DC    AL1(LVEPIS),AL4(TCAST-SAVED),AL1(L'TCAST)                        
         DC    AL1(LVPTYP),AL4(PTYPE-SAVED),AL1(L'PTYPE)                        
         DC    AL1(LVSPTYP),AL4(SBPTYP-SAVED),AL1(L'SBPTYP)                     
         DC    X'FF'                                                            
                                                                                
VIEWTYPT DC    AL1(TSLIV),AL1(DBXLLQ),AL1(NPG2VLV),AL1(SRCLIVE)    LIVE         
         DC    AL1(TSLSD),AL1(DBXLL1Q),AL1(NPG2VSD),AL1(SRCLIVSD)  L+SD         
         DC    AL1(TSLP7),AL1(DBXLL7Q),AL1(NPG2VL7),AL1(SRCLIVE7)  L+7          
         DC    AL1(TSLP1),AL1(DBXLLF1Q),AL1(NPG2VL1),AL1(SRCLIVE1) L+1          
         DC    AL1(TSLP2),AL1(DBXLL2Q),AL1(NPG2VL2),AL1(SRCLIVE2)  L+2          
         DC    AL1(TSLP3),AL1(DBXLL3Q),AL1(NPG2VL3),AL1(SRCLIVE3)  L+3          
         DC    AL1(TSALV),AL1(DBXLALVQ),AL1(NPG2VLV),AL1(SRCALV)   A/LV         
         DC    AL1(TSALS),AL1(DBXLALSQ),AL1(NPG2VSD),AL1(SRCALS)   A/LS         
         DC    AL1(TSAL7),AL1(DBXLAL7Q),AL1(NPG2VL7),AL1(SRCAL7)   A/L7         
         DC    AL1(TSLN3),AL1(DBXLNL3),AL1(NPG2VL3),AL1(SRCNTIL3)  N/L3         
         DC    X'FF'                                                            
         EJECT                                                                  
                                                                                
* OPUP FEB/09                                                                   
* REMOVED INVALID ENTRIES BELOW. V6-8,V9-11,V12-14,AND V9-11                    
* ARE NOT BUILDING BLOCKS AND DON'T BELONG IN THIS TABLE.                       
                                                                                
NADDERV  DC    C'Y',AL2(M611_4),AL1(2),AL1(M68),AL1(M911)                       
         DC    C'Y',AL2(W611_4),AL1(2),AL1(W68),AL1(W911)                       
*INVALID DC    C'Y',AL2(V611_4),AL1(2),AL1(V68),AL1(V911)                       
         DC    C'Y',AL2(M1217_4),AL1(2),AL1(M1214),AL1(M1517)                   
         DC    C'Y',AL2(W1217_4),AL1(2),AL1(W1214),AL1(W1517)                   
*INVALID DC    C'Y',AL2(V1217_4),AL1(2),AL1(V1214),AL1(V1517)                   
         DC    C'Y',AL2(WW1849_4),AL1(5),AL1(WW1820),AL1(WW2124)                
         DC    AL1(WW2534),AL1(WW3544),AL1(WW4549)                              
         DC    C'Y',AL2(WW50P_4),AL1(2),AL1(WW5054),AL1(WW55P)                  
                                                                                
         DC    C'B',AL2(M611_4),AL1(2),AL1(M68),AL1(M911)                       
         DC    C'B',AL2(W611_4),AL1(2),AL1(W68),AL1(W911)                       
*INVALID DC    C'B',AL2(V611_4),AL1(2),AL1(V68),AL1(V911)                       
         DC    C'B',AL2(M1217_4),AL1(2),AL1(M1214),AL1(M1517)                   
         DC    C'B',AL2(W1217_4),AL1(2),AL1(W1214),AL1(W1517)                   
*INVALID DC    C'B',AL2(V1217_4),AL1(2),AL1(V1214),AL1(V1517)                   
         DC    C'B',AL2(WW1849_4),AL1(5),AL1(WW1820),AL1(WW2124)                
         DC    AL1(WW2534),AL1(WW3544),AL1(WW4549)                              
         DC    C'B',AL2(WW50P_4),AL1(2),AL1(WW5054),AL1(WW55P)                  
         DC    X'FFFF'                                                          
                                                                                
M611_4   EQU   22                                                               
M68      EQU   218                                                              
M911     EQU   219                                                              
W611_4   EQU   21                                                               
W68      EQU   198                                                              
W911     EQU   199                                                              
V611_4   EQU   123                                                              
V68      EQU   161                                                              
V911     EQU   162                                                              
                                                                                
M1217_4  EQU   75                                                               
M1214    EQU   74                                                               
M1517    EQU   76                                                               
W1217_4  EQU   25                                                               
W1214    EQU   24                                                               
W1517    EQU   26                                                               
V1217_4  EQU   125                                                              
V1214    EQU   124                                                              
V1517    EQU   126                                                              
                                                                                
WW1849_4 EQU   365                                                              
WW1820   EQU   230                                                              
WW2124   EQU   231                                                              
WW2534   EQU   232                                                              
WW3544   EQU   233                                                              
WW4549   EQU   234                                                              
WW50P_4  EQU   367                                                              
WW5054   EQU   235                                                              
WW55P    EQU   236                                                              
                                                                                
         EJECT                                                                  
                                                                                
         EJECT                                                                  
***********************************************************************         
* AE REPORT DOWNLOAD                                                  *         
***********************************************************************         
                                                                                
CNSDOWN  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     RE,ATABLESL                                                      
         MVC   DTABLES(DTABLELQ),0(RE)                                          
         GOTOR DEMADDR,DMCB,(X'FF',DTABLES),ACOMFACS                            
                                                                                
         XC    TSARD(TSARDL),TSARD DEFINE TSAR BLOCK                            
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSKEYL,SUMKEYL                                                   
         MVI   TSPAGL,4            START WITH PAGE 1 OF TEMPSTR                 
         MVI   TSPAGN,20           USE FOUR PAGES OF TEMPSTR                    
         OI    TSINDS,TSINODSK+TSIXTTWA                                         
         LHI   R0,SUMRECL                                                       
         STCM  R0,3,TSRECL                                                      
         LA    R0,TSARREC                                                       
         STCM  R0,15,TSAREC                                                     
                                                                                
         MVI   USETYP,0                                                         
         SR    R4,R4                                                            
         ICM   R4,7,AMFID                                                       
         BZ    CNSDW10                                                          
         MVI   USETYP,USEMFID                                                   
         GOTOR USMFIDS             USE MAINFRAME ID'S IF ENTERED                
         B     CNSDOWNX                                                         
                                                                                
CNSDW10  MVI   USETYP,USEINP                                                    
         GOTOR USINPUT             OTHERWISE USE INPUT FIELDS                   
                                                                                
CNSDOWNX J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* AE REPORT DOWNLOAD USING MAINFRAME ID'S                             *         
***********************************************************************         
                                                                                
USMFIDS  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVI   TSACTN,TSAINI       INITIALIZE TSAR BUFFER                       
         GOTOR BUFFER                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R0,MYSAVE           CLEAR DOWN SAVE AREA                         
         LHI   R1,MYSAVELN                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         SR    R4,R4               1.FOR EVERY MAINFRAME ID                     
         ICM   R4,7,AMFID                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R2,R2                                                            
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
                                                                                
UMFID10  ST    R4,MYMIDPTR                                                      
         STCM  R2,3,MYMIDCTR                                                    
         USING MFIDD,R4                                                         
         MVC   MYNET,MFNET                                                      
         MVC   MYNTI,MFNTINUM                                                   
         MVC   MYMEDIA,MFMED                                                    
         MVC   MYSOURCE,MFSRC                                                   
         GOTOR INITAVG             INITIALIZE/RESET AVG RECORD                  
                                                                                
         MVC   MYBOOK,MFSBK        2.FOR ALL BOOKS IN THE ID                    
         XC    PVNADMN,PVNADMN     CLEAR PREVIOUS NAD MONTHLY BOOK              
UMFID20  MVC   MYDAY,MFDAYS                                                     
         MVC   MYTIME,MFTIMES                                                   
         XC    MYDTPTR,MYDTPTR                                                  
                                                                                
         OC    MYDAY,MYDAY         IF ACTUAL DAYS AND TIMES ARE GIVEN           
         BZ    *+14                USE THEM                                     
         OC    MYTIME,MYTIME                                                    
         BNZ   UMFID40                                                          
                                                                                
         OC    MFDTREQ,MFDTREQ                                                  
         BZ    UMFID40             ALL DAYS AND TIMES REQ, NO FILTERING         
                                                                                
         LA    R3,MFDTREQ          3.FOR ALL DAYS/TIMES IN THE BLOCK            
UMFID30  ST    R3,MYDTPTR                                                       
         USING DYTMD,R3                                                         
         OC    MFDAYS,MFDAYS                                                    
         BZ    UMFID33                                                          
         MVC   WORK,MFDAYS         MAKE SURE WE MATCH REQ W/ACTUAL DAY          
         NC    WORK(L'MFDAYS),DTDAYS                                            
         BZ    UMFIDXDT                                                         
         B     *+10                                                             
UMFID33  MVC   MYDAY,DTDAYS                                                     
                                                                                
         OC    MFTIMES,MFTIMES                                                  
         BZ    UMFID37                                                          
         CLC   MFSTIME,DTETIME     MAKE SURE WE MATCH REQ W/ACTUAL TIME         
         BNL   UMFIDXDT                                                         
         CLC   MFETIME,DTSTIME                                                  
         BL    UMFIDXDT                                                         
         B     *+10                                                             
UMFID37  MVC   MYTIME,DTTIMES                                                   
         DROP  R3                                                               
                                                                                
UMFID40  GOTOR INDBLOCK            INITIALIZE DBLOCK                            
         GOTOR INITMATH            INITIALIZE DEMOMATH BLOCK                    
                                                                                
         CLI   INADSW,INSEST                                                    
         BNE   UMFID50                                                          
         GOTOR GETNAD              GET NAD RECORDS AND ADD TO BUFFER            
         MVC   PVNADMN,DBSELBK     SAVE LAST NAD BOOK                           
         B     UMFIDXDT                                                         
                                                                                
UMFID50  GOTOR DEMAND,DMCB,DBLOCK,HOOK  GET NTI RECORDS                         
                                                                                
UMFIDXDT OC    MYDTPTR,MYDTPTR     NEXT DAY/TIME IN THE BLOCK                   
         BZ    UMFIDXBK            USE ACTUAL DAYS/TIMES                        
         LA    R3,DYTMLN(R3)                                                    
         CLI   0(R3),X'FF'                                                      
         BE    UMFIDXBK                                                         
         B     UMFID30                                                          
                                                                                
UMFIDXBK GOTOR NXBOOK              GET NEXT BOOK IN MYBOOK                      
         CLC   MYBOOK,MFEBK                                                     
         BH    UMIDRELS                                                         
         B     UMFID20                                                          
                                                                                
UMIDRELS L     RE,TSAREC           SEND OUTPUT RECORDS                          
         XC    0(SUMKEYL,RE),0(RE)                                              
UMIDREL5 GOTOR NXTSUMR                                                          
         BNE   UMFIDNXT                                                         
         GOTOR LP_APUTO,LP_D                                                    
         B     UMIDREL5                                                         
                                                                                
UMFIDNXT MVI   TSACTN,TSAINI       INITIALIZE TSAR BUFFER                       
         GOTOR BUFFER                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         SR    R2,R2               NEXT MAINFRAME ID                            
         ICM   R2,3,MYMIDCTR                                                    
         BCTR  R2,0                                                             
         LTR   R2,R2                                                            
         BZ    UMFIDX                                                           
         L     R4,MYMIDPTR                                                      
         LA    R4,MPPLN(R4)                                                     
         B     UMFID10                                                          
                                                                                
UMFIDX   J     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* AE REPORT DOWNLOAD USING INPUT FIELDS                               *         
***********************************************************************         
                                                                                
USINPUT  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R0,MYSAVE           CLEAR DOWN SAVE AREA                         
         LHI   R1,MYSAVELN                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR RECNTBK             DETERMINE THE MOST RECENT BK REQSTD          
                                                                                
         MVC   MYMEDIA,IMEDIA      USE INPUT MEDIA                              
         MVC   MYSOURCE,ISOURCE    USE INPUT SOURCE                             
                                                                                
         SR    R4,R4               1.FOR EVERY NETWORK                          
         ICM   R4,7,ANET                                                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R2,R2                                                            
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
UINP10   STCM  R2,3,MYNETCTR                                                    
         ST    R4,MYNETPTR                                                      
         MVC   MYNET,0(R4)                                                      
                                                                                
         MVI   TSACTN,TSAINI       INITIALIZE TSAR BUFFER. START NEW            
         GOTOR BUFFER                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         SR    R4,R4               2.FOR EVERY DATE RANGE                       
         ICM   R4,7,ADAT                                                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R2,R2                                                            
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
UINP20   STCM  R2,3,MYDATCTR                                                    
         ST    R4,MYDATPTR                                                      
                                                                                
         L     R3,MYDATPTR         3.FOR EVERY BOOK IN THE RANGE                
         USING DATD,R3                                                          
         MVC   MYBOOK,STBK                                                      
         DROP  R3                                                               
UINP30   DS    0X                                                               
                                                                                
         SR    R4,R4               4.FOR EVERY DAY/TIME BLOCK                   
         ICM   R4,7,ADYTM                                                       
         BZ    UINP60                                                           
         SR    R2,R2                                                            
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
UINP40   LA    R4,DTSTRLN(R4)                                                   
         ST    R4,MYDTBPTR                                                      
         STCM  R2,3,MYDTBCTR                                                    
                                                                                
         L     R3,MYDTBPTR         5.FOR EVERY DAY/TIME IN THE BLOCK            
         ST    R3,MYDTPTR                                                       
         USING DYTMD,R3                                                         
UINP50   MVC   MYDAY,DTDAYS                                                     
         MVC   MYTIME,DTTIMES                                                   
         DROP  R3                                                               
                                                                                
UINP60   DS    0X                                                               
                                                                                
         SR    R4,R4                                                            
         ICM   R4,7,ANTIN          6.FOR EVERY NTI NUMBER                       
         BZ    UINP80                                                           
         SR    R2,R2                                                            
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
UINP70   STCM  R2,3,MYNTICTR                                                    
         ST    R4,MYNTIPTR                                                      
         MVC   MYNTI,0(R4)                                                      
                                                                                
                                                                                
UINP80   GOTOR INDBLOCK            INITIALIZE DBLOCK FIELDS                     
                                                                                
         GOTOR DEMAND,DMCB,DBLOCK,HOOK  GET RECDS AND ADD TO BUFFER             
                                                                                
                                                                                
UINPXNTI ICM   R4,15,MYNTIPTR      NEXT NTI NUMBER                              
         BZ    UINPXDT                                                          
         SR    R2,R2                                                            
         ICM   R2,3,MYNTICTR                                                    
         BCTR  R2,0                                                             
         LTR   R2,R2                                                            
         BZ    UINPXDT                                                          
         LA    R4,NTINLN(R4)                                                    
         B     UINP70                                                           
                                                                                
UINPXDT  ICM   R3,15,MYDTPTR       NEXT DAY/TIME IN THE BLOCK                   
         BZ    UINPXDTB                                                         
         LA    R3,DYTMLN(R3)                                                    
         CLI   0(R3),X'FF'                                                      
         BE    UINPXDTB                                                         
         ST    R3,MYDTPTR                                                       
         B     UINP50                                                           
                                                                                
UINPXDTB ICM   R4,15,MYDTBPTR      NEXT DAY/TIME BLOCK                          
         BZ    UINPXBK                                                          
         SR    R2,R2                                                            
         ICM   R2,3,MYDTBCTR                                                    
         BCTR  R2,0                                                             
         LTR   R2,R2                                                            
         BZ    UINPXBK                                                          
         LA    R4,DTBLN(R4)                                                     
         B     UINP40                                                           
                                                                                
UINPXBK  GOTOR NXBOOK              NEXT BOOK                                    
         L     R3,MYDATPTR                                                      
         USING DATD,R3                                                          
         CLC   MYBOOK,ENDBK                                                     
         BH    UINPXDAT                                                         
         B     UINP30                                                           
         DROP  R3                                                               
                                                                                
UINPXDAT SR    R2,R2               NEXT DATE RANGE                              
         ICM   R2,3,MYDATCTR                                                    
         BCTR  R2,0                                                             
         LTR   R2,R2                                                            
         BZ    UINPRELS                                                         
         L     R4,MYDATPTR                                                      
         LA    R4,DATLN(R4)                                                     
         B     UINP20                                                           
                                                                                
UINPRELS L     RE,TSAREC           SEND OUTPUT RECORDS                          
         XC    0(SUMKEYL,RE),0(RE)                                              
UINPREL5 GOTOR NXTSUMR                                                          
         BNE   UINPXNET                                                         
         GOTOR LP_APUTO,LP_D                                                    
         B     UINPREL5                                                         
                                                                                
UINPXNET SR    R2,R2               NEXT NETWORK                                 
         ICM   R2,3,MYNETCTR                                                    
         BCTR  R2,0                                                             
         LTR   R2,R2                                                            
         BZ    UINPX                                                            
         L     R4,MYNETPTR                                                      
         LA    R4,NETLN(R4)                                                     
         B     UINP10                                                           
                                                                                
UINPX    J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DETERMINE THE MOST RECENT BOOK OF THE REQUEST                       *         
***********************************************************************         
                                                                                
RECNTBK  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         SR    R4,R4               FOR EVERY DATE RANGE                         
         ICM   R4,7,ADAT                                                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R2,R2                                                            
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
                                                                                
         USING DATD,R4                                                          
         MVC   RECENTBK,ENDBK                                                   
                                                                                
RECN20   CLC   RECENTBK(1),ENDBK   COMPARE TO END BOOK                          
         BH    RECN40                                                           
         BL    *+14                                                             
         CLC   RECENTBK+1(1),ENDBK+1                                            
         BH    RECN40                                                           
         MVC   RECENTBK,ENDBK                                                   
                                                                                
RECN40   LA    R4,DATLN(R4)        NEXT DATE RANGE                              
         BCT   R2,RECN20                                                        
                                                                                
RECNTX   J     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* HOOK FOR DEMAND - COME HERE WITH EVERY RECORD                       *         
***********************************************************************         
                                                                                
HOOK     NTR1  BASE=*,LABEL=*                                                   
                                                                                
         GOTOR DEMOUT,DMCB,(C'D',NTIHOMEQ),DBLOCK,FULL1                         
         OC    FULL1,FULL1                                                      
         BZ    HOOKX               EXCLUDE RECORDS WITH ZERO HOMES              
                                                                                
         GOTOR EXTRINFO            EXTRACT RECORD INFO                          
                                                                                
         GOTOR FILTERS             FILTER OUT UNWANTED RECORDS                  
         BNE   HOOKX                                                            
                                                                                
         CLI   INADSW,INSEST                                                    
         BNE   HOOK20                                                           
         GOTOR AVGREC              USE DEMOMATH TO AVERAGE RECORDS              
         B     HOOKX               DON'T CREATE OUTPUT RECORDS                  
                                                                                
HOOK20   LA    R0,TSARREC                                                       
         LHI   R1,L'TSARREC                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR BLDDEMOS            BUILD DEMO VALUES                            
                                                                                
         GOTOR BFKEY               BUILD BUFFER KEY                             
                                                                                
         GOTOR ADDTOBUF            ADD A RECORD TO TSAR BUFFER                  
                                                                                
HOOKX    J     EXIT                                                             
NTIHOMEQ DC    XL3'00E801'         NTI HOMES (FOR DEMOUT)  'Y'                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD OR UPDATE RECORD IN TSAR BUFFER                                 *         
***********************************************************************         
                                                                                
ADDTOBUF NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVI   TSACTN,TSARDH       READ BUFFER TO SEE IF KEY EXISTS             
         XC    TSRNUM,TSRNUM                                                    
         GOTOR BUFFER                                                           
         BNE   ADDB50                                                           
                                                                                
         LA    R1,SUMDEMOS         ACCUMULATE WEIGHTED DEMOS                    
SUMD     USING DEMOENTD,R1                                                      
         LA    R2,DEMOTAB                                                       
         USING DEMOENTD,R2                                                      
         LA    R0,MAXMODQ          FOR ALL MODIFIERS                            
                                                                                
ADDB25   CLC   DMODIF,SUMD.DMODIF                                               
         BE    *+6                                                              
         DC    H'0'                MODIFIER DOESN'T MATCH                       
         OC    DMODIF,DMODIF                                                    
         BZ    ADDB35                                                           
         LA    R3,SUMD.DEMSP8                                                   
         LA    R4,DEMSP8                                                        
         SR    RF,RF                                                            
         ICM   RF,3,SUMD.DEMCOUNT                                               
         SR    RE,RE                                                            
ADDB30   CR    RE,RF                                                            
         BNL   ADDB35                                                           
         AP    0(L'DEMSP8,R3),0(L'DEMSP8,R4)                                    
         LA    R3,L'DEMSP8(R3)                                                  
         LA    R4,L'DEMSP8(R4)                                                  
         LA    RE,1(RE)                                                         
         B     ADDB30                                                           
                                                                                
ADDB35   LA    R1,L'DEMOTAB(R1)    NEXT MODIFIER                                
         LA    R2,L'DEMOTAB(R2)                                                 
         BCT   R0,ADDB25                                                        
         DROP  R2                                                               
                                                                                
ADDB40   GOTOR BFACCUM             ADD ACCUMULATORS                             
                                                                                
         CLC   RECBOOK,SUMUSBK     UPDATE UNIVERSE START/END BOOKS              
         BNL   *+10                                                             
         MVC   SUMUSBK,RECBOOK                                                  
         CLC   RECBOOK,SUMUEBK                                                  
         BNH   *+10                                                             
         MVC   SUMUEBK,RECBOOK                                                  
                                                                                
         MVI   TSACTN,TSAWRT       WRITE RECORD BACK TO BUFFER                  
         GOTOR BUFFER                                                           
         TM    TSERRS,TSERNF                                                    
         BNO   *+6                                                              
         DC    H'0'                                                             
         B     ADDBX                                                            
                                                                                
ADDB50   DS    0X                  ADD NEW RECORD TO BUFFER                     
         MVC   SUMKEY(SUMKEYL),SUMKEYSV   RESTORE KEY                           
         GOTOR BFDESC              BUILD DECRPTIVE INFO                         
                                                                                
         XC    SUMACCS(SUMACCSL),SUMACCS                                        
         GOTOR BFACCUM             ADD ACCUMULATORS                             
                                                                                
         LA    R0,SUMDEMOS         PUT DEMOS ON THE RECORD                      
         LA    R1,DEMOTABL                                                      
         LA    RE,DEMOTAB                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVI   TSACTN,TSAADD                                                    
         GOTOR BUFFER                                                           
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
ADDBX    J     EXIT                                                             
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE NTI AVERAGE RECORD                                       *         
***********************************************************************         
                                                                                
INITAVG  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     RE,ANTAVREC             NTI AVERAGE RECORD                       
         XC    0(25,RE),0(RE)          CLEAR SKELETON PAV                       
         MVI   0(RE),C'P'              ID IT AS A PAV                           
         MVC   20(2,RE),=H'24'         LENGTH OF RECORD                         
                                                                                
INITAVGX J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE DEMOMATH BLOCK                                           *         
***********************************************************************         
                                                                                
INITMATH NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    MATHFACS(MATHFACL),MATHFACS                                      
         LA    R1,DBLOCK                                                        
         ST    R1,MATHABLK                                                      
         MVC   MATHIFIL,DBFILE                                                  
         MVC   MATHOFIL,DBFILE                                                  
         MVC   MATHOSRC(1),DBSELSRC                                             
         CLI   DBSELSTA+4,C'Q'                                                  
         BE    *+8                                                              
         CLI   DBSELSTA+4,C'C'                                                  
         BNE   INITMATX                                                         
         MVC   MATHIFIL,=C'CAB'    CABLE HAS DIFFERENT FORMULAS                 
         MVC   MATHOFIL,=C'CAB'    AND RECORD FORMAT                            
                                                                                
INITMATX J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* USE DEMOMATH TO CREATE AN AVERAGE DEMO RECORD                       *         
* THIS AVERAGE WILL BE USED AT THE TIME WE EXTRACT NAD DATA           *         
***********************************************************************         
                                                                                
AVGREC   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   MYMEDIA,MEDCAB                                                   
         BNE   AVGRC20                                                          
*        GOTOR GETCNTI             SAVE CABLE NTI NO'S IN SAVNTIPR              
         ICM   RE,15,DBSPANAD                                                   
         BZ    AVGRC20                                                          
         CLC   0(3,RE),=C'CAB'     DIG OUT THE LEVEL/CONVERT TO ALPHA           
         BNE   AVGRC20                                                          
         MVI   DBSELPTT,C'P'       DEFAULT TO PROGRAM                           
         CLI   3(RE),1                                                          
         BNE   *+8                                                              
         MVI   DBSELPTT,C'T'       TRACK                                        
         CLI   3(RE),2                                                          
                                                                                
AVGRC20  MVC   MATHFCTR+2(2),DBFACTOR                                           
         L     R3,ANTAVREC                                                      
         GOTOR DEMOMATH,DMCB,=C'MAD',DBAREC,(R3),MATHFACS                       
                                                                                
AVGRECX  J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET NAD DATA AND CREATE NAD TSAR RECORDS                            *         
***********************************************************************         
                                                                                
GETNAD   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   MYMEDIA,MEDCAB                                                   
         BE    GETNADX             CABLE NAD NOT SUPPORTED YET                  
                                                                                
         L     R4,MYMIDPTR                                                      
         USING MFIDD,R4                                                         
                                                                                
         CLI   MYMEDIA,MEDCAB                                                   
         BNE   GNAD10                                                           
*        MVC   NTIDIVSR,DBDIVSOR   FOR CABLE NAD                                
*        GOTOR ADDSB               ADD SPEC/BKOUTS FOR LAST PROGRAM             
                                                                                
GNAD10   MVC   MATHFCTR+2(2),DBDIVSOR                                           
         L     R3,ANTAVREC                                                      
         GOTOR DEMOMATH,DMCB,=C'DIV',(R3),(R3),MATHFACS                         
*&&DO                                                                           
         XC    DBLOCK,DBLOCK       SET DBLOCK FOR DEMOUT FROM AVG RECRD         
         MVC   DBFILE,=C'NTI'      GET ACTUAL DEMOS                             
         MVC   DBCOMFCS,ACOMFACS   A(COMFACS)                                   
         MVC   DBAREC,ANTAVREC     NTI AVERAGE RECORD                           
         L     R1,ANTAVREC                                                      
         LA    R1,23(R1)           FIRST ELEMENT OF DUMMYREC                    
         ST    R1,DBAQUART         A(FIRST ELEM)                                
         GOTOR DEMOUT,DMCB,(C'D',NTIHOMEQ),DBLOCK,NTIHOMES                      
*&&                                                                             
         GOTOR INDBLOCK                                                         
                                                                                
         MVI   DBFUNCT,DBGETNTI    ALWAYS?                                      
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBDAYOPT,0                                                       
         MVI   DBBTYPE,0                                                        
                                                                                
         MVI   DBSELSTA+3,C'P'     PROGRAM AVE                                  
         MVI   DBSELSTA+4,C'N'     NAD                                          
         CLI   MYMEDIA,MEDSYN                                                   
         BNE   *+10                                                             
         MVC   DBSELSTA+3(2),=C' M'                                             
                                                                                
         TM    MFFLAGS,BKOTQ       REQUEST BREAKOUTS IF NEED TO                 
         BNO   *+8                                                              
         MVI   DBSTYPE,C'B'                                                     
                                                                                
         OC    DBSELPRG,DBSELPRG   IF WE HAVE NTI NUMBER                        
         BZ    GNAD20                                                           
         XC    DBSELDAY,DBSELDAY   DON'T USE DAYS AND TIMES                     
         XC    DBSELTIM,DBSELTIM                                                
                                                                                
GNAD20   CLI   IMWSRC,ISWKLYQ                                                   
         BNE   GNAD30                                                           
         MVI   DBSELMED,C'W'       WEEKLY NAD                                   
         B     GNAD40                                                           
                                                                                
GNAD30   GOTOR WKTOMON             PUT MONTHLY BOOK IN DBSELBK                  
         MVC   DBSELBK,HALF1                                                    
                                                                                
GNAD40   GOTOR DEMAND,DMCB,DBLOCK,NADHOOK                                       
                                                                                
         OC    DBDIVSOR,DBDIVSOR                                                
         BNZ   GNAD50                                                           
         CLI   DBBTYPE,0           IF 1ST RECD NOT FOUND, WE'RE DONE            
         BE    GETNADX                                                          
         B     GNAD60              ELSE LK FOR OPIONAL MARKET BREAKS            
                                                                                
GNAD50   CLI   DBBTYPE,18          SCAN ALL RECORDS W/REG MKTBRK DATA           
         BNL   GNAD60              NOW SCAN OPTIONAL MKT BRK RECORDS            
         ZIC   RE,DBBTYPE                                                       
         LA    RE,1(RE)                                                         
         STC   RE,DBBTYPE                                                       
         B     GNAD40              GO CALL DEMAND AGAIN                         
                                                                                
GNAD60   CLI   DBBTYPE,C'P'        WE'VE READ ALL REG+OPT MKT BRKS              
         BE    GETNADX                                                          
         CLI   DBBTYPE,C'O'        DONE W/1ST SET OF OPT'LS?                    
         BNE   *+12                                                             
         MVI   DBBTYPE,C'P'        NOW READ NEXT SET OF OPT'LS                  
         B     GNAD40                                                           
         MVI   DBBTYPE,C'O'        READ 1ST OPTIONAL RECD                       
         B     GNAD40                                                           
                                                                                
GETNADX  J     EXIT                                                             
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONVERT WEEKLY BOOK TO NAD MONTHLY BOOK                                       
* AT ENTRY: MYBOOK HAS WEEKLY BOOK                                              
* AT EXIT : HALF1 HAS MONTHLY BOOK                                              
***********************************************************************         
                                                                                
WKTOMON  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   HALF1,MYBOOK                                                     
                                                                                
         SR    R0,R0                                                            
         ICM   R0,3,MYBOOK         WEEKLY BOOK                                  
         BZ    WKXIT                                                            
*                                                                               
         L     RE,ANADWKS                                                       
         USING NEWKSD,RE                                                        
                                                                                
WKTOM5   CLC   MYBOOK(1),NEWKSYR   MATCH YEAR                                   
         BNE   WKTOM10                                                          
         CLC   MYBOOK+1(1),NEWKSLST                                             
         BH    WKTOM10                                                          
         MVC   HALF1+1(1),NEWKSMO  SET MONTH                                    
         B     WKXIT                                                            
                                                                                
WKTOM10  LA    RE,NEWKSQ(RE)       BUMP TO NEXT WK ENTRY                        
         CLI   0(RE),X'FF'         END OF TABLE?                                
         BNE   WKTOM5                                                           
                                                                                
WKXIT    J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
         DROP  RE                                                               
***********************************************************************         
* NAD HOOK FOR DEMAND                                                 *         
***********************************************************************         
                                                                                
NADHOOK  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         GOTOR EXTRINFO            EXTRACT RECORD INFO                          
                                                                                
         OC    DBSELPRG,DBSELPRG   IF WE HAVE NTI NUMBER                        
         BNZ   NADH10              DON'T DO OTHER FILTERING                     
         GOTOR FILTERS                                                          
         BNE   NADHOOKX            FILTER OUT UNWANTED RECORDS                  
                                                                                
NADH10   MVI   MULTREAD,MUTRDYES                                                
         CLI   MYMEDIA,MEDCAB      DON'T ADD DURATION FOR SPANNING RECS         
         BNE   NADH20                                                           
         L     RF,DBAQUART                                                      
         CLI   0(RF),X'0F'                                                      
         BNE   NADH40                                                           
         B     NADH30                                                           
NADH20   CLI   DBBTYPE,0                                                        
         BNE   NADH40                                                           
                                                                                
NADH30   MVI   MULTREAD,MUTRDNO                                                 
         B     *+10                                                             
NADH40   XC    RECDUR,RECDUR       NO DURATION FOR SPANNING RECDS               
                                                                                
         ICM   R4,7,ANAD                                                        
         BZ    NADHOOKX                                                         
         SR    R2,R2                                                            
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
                                                                                
NADH50   ST    R4,MYNADPTR         FOR EVERY NAD CATEGORY                       
         STCM  R2,3,MYNADCTR                                                    
         MVC   MYNAD,0(R4)                                                      
                                                                                
         GOTOR BLDDEMOS            BUILD DEMO VALUES                            
                                                                                
         GOTOR NADMISD             CREATE MISSING NAD DEMOS                     
                                                                                
         GOTOR DEMOUT,DMCB,(C'D',NADHOMEQ),DBLOCK,NADHOMES                      
                                                                                
NADH60   GOTOR BFKEY               BUILD TSAR BUFFER KEY                        
                                                                                
         GOTOR ADDTOBUF            ADD A RECORD TO TSAR BUFFER                  
                                                                                
         SR    R2,R2               NEXT NAD CATEGORY                            
         ICM   R2,3,MYNADCTR                                                    
         BCTR  R2,0                                                             
         LTR   R2,R2                                                            
         BZ    NADHOOKX                                                         
         L     R4,MYNADPTR                                                      
         LA    R4,NADLN(R4)                                                     
         B     NADH50                                                           
                                                                                
NADHOOKX J     EXIT                                                             
                                                                                
NADHOMEQ DC    XL3'01E801'         NAD USA HOMES (FOR DEMOUT)  'Y'              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD DEMO VALUES FROM RECORD                                       *         
***********************************************************************         
                                                                                
BLDDEMOS NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    RE,DEMOTAB          CLEAR ALL DEMOS                              
         LA    RF,DEMOTABL                                                      
         XCEF                                                                   
                                                                                
         CLI   USETYP,USEMFID      FOR MAIFRAME ID'S                            
         BNE   BLDD10              ALREADY HAVE THE TABLE ID                    
         L     R4,MYMIDPTR                                                      
         USING MPPD,R4                                                          
         LA    R2,MPPMID                                                        
         USING MFIDD,R2                                                         
                                                                                
         ICM   R3,15,AAEFORMS      READ AE FORMULA TABLES FROM DTASPACE         
         BZ    BLDDEMX             A(TABLE) NOT AVAILABLE                       
         STAR  CLEAR=Y,ARS=ON                                                   
         LAM   AR3,AR3,ALET                                                     
         USING DEMFFMSD,R3                                                      
                                                                                
BLDD05   CLC   0(2,R3),EFFS        FIND ENTRY FOR TABLE ID                      
         BE    BLDDEMX             INVALID TABLE ID                             
         CLC   DEMFFTID,MFTABID    MATCH ON TABLE ID                            
         BE    BLDD30                                                           
         ICM   RE,15,DEMFFLQ(R3)   DISPLACEMENT TO NEXT F/M/S/BK ENTRY          
         AR    R3,RE                                                            
         B     BLDD05                                                           
         DROP  R2,R4                                                            
*                                  IF NOT A MAINFRAME ID,                       
BLDD10   MVC   FORMBK,RECENTBK     USE MOST RECENT BOOK IN THE REQUEST          
                                                                                
         L     RE,ABBTAB           FIND BLDG BLOCK TABLE FOR THIS FILE          
         USING BBTABD,RE                                                        
BLDD15   CLI   0(RE),FF                                                         
         BE    BLDDEMX             F/M/S COMBO NOT DEFINED IN BBTAB             
         CLC   BBOFMS,REC5E                                                     
         BE    *+12                                                             
         LA    RE,BBTABL(RE)                                                    
         B     BLDD15                                                           
         MVC   FORMFMS,BBBFMS                                                   
         DROP  RE                                                               
                                                                                
         ICM   R3,15,AAEFORMS      READ AE FORMULA TABLES FROM DTASPACE         
         BZ    BLDDEMX             A(TABLE) NOR AVAILABLE                       
         STAR  CLEAR=Y,ARS=ON                                                   
         LAM   AR3,AR3,ALET                                                     
                                                                                
BLDD23   CLC   0(2,R3),EFFS        FIND ENTRY FOR FILEMED/SOURCE/BOOK           
         BE    BLDDEMX             INVALID FILE/MED/SOURCE/BOOK                 
         CLC   FORMFMS,DEMFFTID    MATCH ON FILE,MEDIA,SOURCE                   
         BNE   BLDD28                                                           
         CLC   FORMBK,DEMFFBK      MATCH ON BOOK                                
         BNL   BLDD30                                                           
                                                                                
BLDD28   ICM   RE,15,DEMFFLQ(R3)   DISPLACEMENT TO NEXT F/M/S/BK ENTRY          
         AR    R3,RE                                                            
         B     BLDD23                                                           
                                                                                
BLDD30   MVC   TABID,DEMFFTID      FORMULA TABLE ID                             
         DROP  R3                                                               
                                                                                
         LA    R3,DEMFFLQ+4(R3)    POINT TO FORMULA TABLE                       
         ST    R3,ABOOKFRM         SAVE A(FORMULA TABLE)                        
                                                                                
         USING DEMFFRMD,R3                                                      
         ICM   RF,15,DEMFFFOR                                                   
         AR    R3,RF               A(FORMULA TABLES BY MODIFIER)                
         USING DEMFMHDD,R3                                                      
                                                                                
         LA    R2,DEMOTAB          FILL IN MODIFIERS & ZERO VALUES IN           
         USING DEMOENTD,R2         OUTPUT DEMO AREA                             
                                                                                
BLDD40   CLC   DEMFMLN,=AL4(0)                                                  
         BE    BLDD70                                                           
                                                                                
         CLI   INADSW,INSEST       SKIP PROGRAM PUTS FOR NAD                    
         BNE   BLDD40N                                                          
         CLI   DEMFM1MD,PPUTMOD                                                 
         BE    BLDD65                                                           
                                                                                
BLDD40N  CLI   MYMEDIA,MEDSYN                                                   
         BNE   BLDD41                                                           
         CLI   IGAA,0                                                           
         BE    *+8                                                              
         CLI   IGAA,GAANONE                                                     
         BNE   BLDD42                                                           
BLDD41   CLI   DEMFM1MD,GWIMPMOD   SKIP GAA DEMOS                               
         BE    BLDD65                                                           
         B     BLDD50                                                           
BLDD42   CLI   IGAA,GAAONLY                                                     
         BNE   BLDD50                                                           
         CLI   DEMFM1MD,RWIMPMOD   SKIP REGULAR DEMOS                           
         BE    BLDD65                                                           
                                                                                
BLDD50   MVC   DMODIF,DEMFM2MD     WEIGHTED MODIFIER: WY,WU...                  
         CPYA  ARF,AR3                                                          
         ICM   RF,15,DEMFMAB3      TABLE OF TYPE-3 BUILDING BLOCKS              
         AR    RF,R3                                                            
         USING DEMFBL3D,RF                                                      
         MVC   DEMCOUNT,DEMFB3CT   DEMO COUNT                                   
         LAM   ARF,ARF,ARZEROS                                                  
         DROP  RF                                                               
                                                                                
         LA    RE,DEMSP8           ZERO OUT DEMOS                               
         LA    R0,DEMSP8X                                                       
         SR    R1,R1                                                            
         ICM   R1,3,DEMCOUNT                                                    
BLDD60   CR    RE,R0                                                            
         BL    *+6                                                              
         DC    H'0'                MORE DEMOS THAN SPACE ALOTED                 
         ZAP   0(L'DEMSP8,RE),P0                                                
         LA    RE,L'DEMSP8(RE)                                                  
         BCT   R1,BLDD60                                                        
         MVI   0(RE),FF            END OF DEMOS                                 
                                                                                
         GOTOR EXTRDEMS            EXTRACT DEMO VALUES USING DEMOUT             
                                                                                
BLDD63   CLI   DEMFM1MD,UNVMOD                                                  
         BNE   *+8                                                              
         ST    R2,AUNVOUT                                                       
                                                                                
         LA    R2,DEMOENTL(R2)     ADVANCE IN OUTPUT AREA                       
                                                                                
BLDD65   ICM   R0,15,DEMFMLN       ADVANCE IN FORMULA TABLES                    
         AR    R3,R0                                                            
         CLC   DEMFMLN,=AL4(0)                                                  
         BE    BLDD70                                                           
         LA    R0,DEMOTABX                                                      
         CR    R2,R0                                                            
         BL    *+6                                                              
         DC    H'0'                OVERFLOWING OUTPUT TABLE                     
         B     BLDD40                                                           
         DROP  R2,R3                                                            
                                                                                
BLDD70   DS    0X                                                               
                                                                                
BLDDEMX  LAM   AR3,AR3,ARZEROS     CLEAR AR3                                    
         LA    RE,*+6                                                           
         BSM   0,RE                                                             
         REAR  ARS=OFF             XA MODE OFF                                  
         J     EXIT                                                             
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXTRACT DEMO VALUES USING DEMOUT                                    *         
* AR3 -> TABLE OF FORMULAS BY MDIFIER                                 *         
* R2 -> OUTPUT DEMO AREA                                              *         
* MYNAD = NAD CATEGORY, IF REQUESTED                                  *         
***********************************************************************         
                                                                                
EXTRDEMS NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING DEMOENTD,R2                                                      
         USING DEMFMHDD,R3                                                      
                                                                                
         ICM   RF,15,DEMFMAB3      A(BUILDING BLOCKS BY TYPE-3 ID)              
         CPYA  ARF,AR3                                                          
         AR    RF,R3                                                            
         USING DEMFBL3D,RF                                                      
         LA    RE,DEMFB3DL         A(BUILDING BLOCKS)                           
         DROP  RF                                                               
         LAM   ARF,ARF,ARZEROS                                                  
                                                                                
         CLI   MULTREAD,MUTRDYES   DON'T EXTRACT UNIVS FOR SPANNG RECDS         
         BNE   EXDEM08                                                          
         CLI   DMODIF+1,UNVMOD                                                  
         BNE   EXDEM08                                                          
         LA    R0,DEMS4            CLEAR DEMOS AREA                             
         LHI   R1,DEMS4X-DEMS4                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     EXDEM15                                                          
                                                                                
EXDEM08  CPYA  ARE,AR3                                                          
         LA    RF,DEMOLIST                                                      
         SR    R1,R1                                                            
         ICM   R1,3,DEMCOUNT                                                    
EXDEM10  MVC   0(L'MYNAD,RF),MYNAD NAD CATEGORY                                 
                                                                                
         MVC   1(1,RF),DMODIF+1    UNWEIGHTED MODIFIER                          
         CLI   DMODIF+1,GWIMPMOD                                                
         BNE   EXDEM12                                                          
         CLI   INADSW,INSEST                                                    
         BNE   EXDEM12                                                          
         MVI   1(RF),C'F'          NAD GAA DEMOS ARE 'F' INTERNALLY             
                                                                                
EXDEM12  MVC   2(L'DEMFB3DL,RF),0(RE)  DEMO CATEGORY                            
         LA    RE,L'DEMFB3DL(RE)                                                
         LA    RF,3(RF)                                                         
         BCT   R1,EXDEM10                                                       
         MVI   0(RF),FF                                                         
         LAM   ARE,ARE,ARZEROS     CLEAR ARE                                    
                                                                                
         LAM   AR3,AR3,ARZEROS     CLEAR AR3 BEFORE CALL TO DEMOUT              
         GOTOR DEMOUT,DMCB,(C'L',DEMOLIST),DBLOCK,DEMS4,0                       
                                                                                
         LAM   AR3,AR3,ALET        RESTORE AR3 AFTER DEMOUT                     
         SAC   512                 RESTORE XA MODE ON                           
                                                                                
EXDEM15  LA    R6,DEMS4            BINARY TO PACKED WEIGHTED DEMOS              
         LA    R4,DEMSP8                                                        
         SR    R1,R1                                                            
         ICM   R1,3,DEMCOUNT                                                    
         SR    RE,RE                                                            
                                                                                
         ICM   R0,15,SVDUR                                                      
         CLI   IMWT,MWTYES         UNLESS MINUTE WEIGHTING REQUESTED,           
         BE    EXDEM18                                                          
         CLI   IACM,ACMYES                                                      
         BNE   *+8                                                              
         ICM   R0,15,SVCMSEC       WEIGHT ACM BY COMMERCIAL SECONDS             
EXDEM18  CVD   R0,DUB              DUB = WEIGHT                                 
                                                                                
EXDEM20  ZAP   WORK(16),P0                                                      
         ICM   R0,15,0(R6)                                                      
         CVD   R0,WORK+8                                                        
         MP    WORK(16),DUB                                                     
         MVC   0(8,R4),WORK+8      PACKED (DEMO * WEIGHT)                       
         LA    R6,L'DEMS4(R6)                                                   
         LA    R4,L'DEMSP8(R4)                                                  
         BCT   R1,EXDEM20                                                       
         MVI   0(R4),FF                                                         
                                                                                
EXDEMX   J     EXITY                                                            
         DROP  R2,R3                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXTRACT BUILDING BLOCKS FROM PROGRAM RECORD                         *         
***********************************************************************         
                                                                                
BLDPBLKS NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    RE,DEMPTAB          CLEAR PUTPUT AREA                            
         ST    RE,ADEMPTAB                                                      
         LA    RF,DEMPTABL                                                      
         XCEF                                                                   
                                                                                
         OC    FORMTBID,FORMTBID   NOT AN AE-ORIGINATED PROGRAM RECORD          
         BZ    BLDPBLXX            MEANS NO BUILDING BLOCKS                     
                                                                                
         ICM   R3,15,AAEFORMS      READ AE FORMULA TABLES FROM DTASPACE         
         BZ    BLDPBLXX            A(TABLE) NOR AVAILABLE                       
         STAR  CLEAR=Y,ARS=ON                                                   
         LAM   AR3,AR3,ALET                                                     
         USING DEMFFMSD,R3                                                      
                                                                                
BLDP10   CLC   0(2,R3),EFFS                                                     
         BE    BLDPBLX             INVALID TABLE ID                             
         CLC   DEMFFTID,FORMTBID   MATCH ON TABLE ID                            
         BE    BLDP20                                                           
         ICM   RE,15,DEMFFLQ(R3)   DISPLACEMENT TO NEXT F/M/S/BK ENTRY          
         AR    R3,RE                                                            
         B     BLDP10                                                           
BLDP20   MVC   HALF1,DEMFFBK       SAVE BOOK OF FORMULA TABLE                   
         XC    HALF1,EFFS          REVERSE IT                                   
                                                                                
         LA    R3,DEMFFLQ+4(R3)    POINT TO FORMULA TABLE                       
                                                                                
         USING DEMFFRMD,R3                                                      
         ICM   RE,15,DEMFFFOR                                                   
         AR    R3,RE                                                            
         ST    R3,AFORMTBL         BEGINNING OF FORMULA TABLES BY MODIF         
         DROP  R3                                                               
         LAM   AR3,AR3,ARZEROS                                                  
                                                                                
         ICM   R3,15,ADEMDISP                                                   
         USING DSPHDRD,R3                                                       
BLDP30   CLC   =XL2'00',0(R3)                                                   
         BNE   *+6                                                              
         DC    H'0'                DEMDISP TABLE NOT FOUND                      
         CLC   DSPFILE,FORMTBID    COMPARE ON FILE/MEDIA/SOURCE                 
         BNE   BLDP35                                                           
         CLC   DSPSBOOK,HALF1                                                   
         BE    BLDP40                                                           
BLDP35   SR    R1,R1               TRY NEXT DEMDISP TABLE                       
         ICM   R1,7,DSPAET                                                      
         LA    R3,1(R1,R3)                                                      
         B     BLDP30                                                           
BLDP40   ST    R3,ADISPTBL         A(DEMDISP TABLE FOR FILE/MED/SRC/BK)         
         SR    R1,R1                                                            
         ICM   R1,7,DSPAET                                                      
         LA    R3,1(R1,R3)                                                      
         ST    R3,ADISPEND         BYTE AFTER END OF THE TABLE                  
         DROP  R3                                                               
                                                                                
         LA    R2,DEMPTAB          BUILD OUTPUT BLD BLOCKS HERE                 
         USING DEMPENTD,R2                                                      
                                                                                
         ICM   R5,15,AIO4                                                       
         AH    R5,DATADISP                                                      
BLDP60   CLI   0(R5),0             LOOK FOR BUILDING BLOCK ELEMENTS             
         BE    BLDP140                                                          
         CLI   0(R5),NPGA1CDQ      X'A1' IMPRESSION BUILDING BLOCKS             
         BNE   *+12                                                             
         MVI   BYTE1,X'41'         DEMDISP ELEMENT                              
         B     BLDP70                                                           
         CLI   0(R5),NPGA2CDQ      X'A2' GAA IMPSSN BUILDING BLOCKS             
         BNE   *+12                                                             
         MVI   BYTE1,X'55'         DEMDISP ELEMENT                              
         B     BLDP70                                                           
         CLI   0(R5),NPGA9CDQ      X'A9' UNIVERSE BUILDING BLOCKS               
         BNE   *+12                                                             
         MVI   BYTE1,X'49'         DEMDISP ELEMENT                              
         B     BLDP70                                                           
         ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     BLDP60                                                           
BLDP70   MVC   DVLEN,2(R5)                                                      
         NI    DVLEN,X'0F'         LENGTH OF DEMO VALUES                        
         ZIC   R0,1(R5)                                                         
         AR    R0,R5                                                            
         ST    R0,AELEND           END OF DEMO ELEMENT                          
         LA    R5,3(R5)            POINT TO FIRST DEMO VALUE                    
                                                                                
         L     R3,ADISPTBL         A(DEMDISP TABLE)                             
         LA    R3,DSPHDRLN(R3)     POINT TO ELEMENT DEFINITION                  
         USING DSPDTAD,R3                                                       
BLDP75   L     RE,ADISPEND                                                      
         CR    R3,RE                                                            
         BL    *+6                                                              
         DC    H'0'                ELEMENT CODE NOT FOUND IN DEMDISP            
         CLC   DSPELCD,BYTE1                                                    
         BE    BLDP80                                                           
         LA    R3,DSPDTALN(R3)                                                  
         B     BLDP75                                                           
                                                                                
BLDP80   MVC   DPMODIF,DSPMOD      STORE MODIFIER IN OUTPUT TABLE               
                                                                                
         LAM   AR4,AR4,ALET                                                     
         L     R4,AFORMTBL         A(FORMULA TABLE)                             
         USING DEMFMHDD,R4                                                      
BLDP90   CLC   =XL4'00',0(R4)                                                   
         BNE   *+6                                                              
         DC    H'0'                MODIFIER NOT FOUND IN FORMULA TBL            
         CLC   DSPMOD,DEMFM1MD                                                  
         BE    BLDP95                                                           
         ICM   R0,15,DEMFMLN                                                    
         AR    R4,R0                                                            
         B     BLDP90                                                           
BLDP95   ICM   R0,15,DEMFMAB3                                                   
         AR    R4,R0                                                            
         ST    R4,AFRMBBK          A(LIST OF BUILDING BLOCKS)                   
         DROP  R4                                                               
                                                                                
         LA    R6,DEMPVAL                                                       
         SR    R0,R0                                                            
         ICM   R0,3,DEMFB3CT-DEMFBL3D(R4)                                       
         MHI   R0,L'DEMPVAL                                                     
         AR    R6,R0                                                            
         MVI   0(R6),FF            MARK END OF VALUES                           
                                                                                
BLDP97   CLC   DSPELCD,BYTE1                                                    
         BE    *+6                                                              
         DC    H'0'                DEMDISP MISSMATCH                            
         C     R3,ADISPEND                                                      
         BL    *+6                                                              
         DC    H'0'                DEMDISP MISSMATCH                            
         LA    R6,DEMPVAL          START OF DEMO OUTPUT AREA                    
         L     R4,AFRMBBK          A(START OF BLD CLOCKS)                       
         LA    R4,DEMFB3DL-DEMFBL3D(R4)  A(FIRST DEMO CODE)                     
                                                                                
BLDP100  CLC   DSPDEMO,0(R4)                                                    
         BE    BLDP110                                                          
         LA    R4,L'DEMFB3DL(R4)   NEXT BUILDING BLOCK                          
         LA    R6,L'DEMPVAL(R6)    NEXT OUTPUT SLOT                             
         B     BLDP100                                                          
                                                                                
BLDP110  CLI   DVLEN,1             GET DEMO VALUE BASED ON LENGTH               
         BNE   BLDP112                                                          
         LHI   R1,1                                                             
         B     BLDP120                                                          
BLDP112  CLI   DVLEN,2                                                          
         BNE   BLDP114                                                          
         LHI   R1,3                                                             
         B     BLDP120                                                          
BLDP114  CLI   DVLEN,3                                                          
         BNE   BLDP116                                                          
         LHI   R1,7                                                             
         B     BLDP120                                                          
BLDP116  CLI   DVLEN,4                                                          
         BE    *+6                                                              
         DC    H'0'                INVALID DEMO LENGTH                          
         LHI   R1,15                                                            
                                                                                
BLDP120  SR    R0,R0                                                            
         EX    R1,*+8                                                           
         B     *+8                                                              
         ICM   R0,0,0(R5)          GET DEMO VALUE FROM ELEMENT                  
         STCM  R0,15,0(R6)         PUT IT IN OUTPUT SLOT                        
                                                                                
         CLI   DSPMOD,RWIMPMOD     SAVE YHOMES VALUES FROM PROG RECORD          
         BNE   BLDP125                                                          
         CLI   DSPDEMO,HOMESQ                                                   
         BNE   BLDP125                                                          
         STCM  R0,15,PGHOMES                                                    
                                                                                
BLDP125  ZIC   R0,DVLEN            ADVANCE TO NEXT VALUE IN ELEMENT             
         AR    R5,R0                                                            
         C     R5,AELEND                                                        
         BE    BLDP130                                                          
         BL    *+6                                                              
         DC    H'0'                                                             
         LA    R3,DSPDTALN(R3)     NEXT DEMO IN DEMDISP                         
         B     BLDP97                                                           
                                                                                
BLDP130  LA    R2,DEMPENTL(R2)     OUTPUT SLOT FOR NEXT MODIFIER                
         B     BLDP60                                                           
                                                                                
BLDP140  DS    0X                                                               
                                                                                
         DROP  R2,R3                                                            
                                                                                
BLDPBLX  LAM   AR4,AR4,ARZEROS     CLEAR AR4                                    
         LA    RE,*+6                                                           
         BSM   0,RE                                                             
         REAR  ARS=OFF             XA MODE OFF                                  
                                                                                
BLDPBLXX J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXTRACT DEMO VALUES FROM PROGRAM RECORD ('92' OR '93' ELEMENT)      *         
***********************************************************************         
                                                                                
BLDPDEMS NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   APDEMS,AIO3         DEFINE OUTPUT TABLE                          
         L     RE,APDEMS           CLEAR OUTPUT TABLE                           
         LA    RF,PMAXDEMQ*PDEMOUTL                                             
         XCEF                                                                   
         L     RE,APDEMS                                                        
         MVI   0(RE),FF            END OF TABLE IF NO VALUES                    
         MVI   OMODIF,0            CLEAR MODIFIER                               
                                                                                
         OC    FORMTBID,FORMTBID   AE-PROGRAM RECORD?                           
         BZ    BLDPD05             NO. SEND DEMOS FROM ELEM 92 OR 93            
         TM    PFLAG1,NPGAIMFQ     YES. DON'T SEND UNLESS MF-EDITED             
         BZ    BLDPDMSX                                                         
                                                                                
BLDPD05  MVI   OMODIF,VPHMOD       ALWAYS VPH'S FOR NOW                         
                                                                                
*        L     RE,APDEMS                                                        
*        MVI   0(RE),0             REMOVE EOT IF NO VALUES                      
*        LA    RE,PMAXDEMQ*PDEMOUTL(RE)                                         
*        MVI   0(RE),FF            MARK END OF DEMOS                            
                                                                                
         LHI   R5,PMAXDEMQ         MAX NO OF DEMOS ON PROGRAM RECORD            
         ICM   R4,15,AIO4                                                       
         MVC   DATADISP,=AL2(24)                                                
         MVI   ELCODE,ELEM93                                                    
         BRAS  RE,GETEL            IS '93' ELEMENT AVAILABLE? USE IT.           
         BE    BLDPD10                                                          
         ICM   R4,15,AIO4                                                       
         MVI   ELCODE,ELEM92                                                    
         BRAS  RE,GETEL                                                         
         BE    BLDPD10                                                          
         DC    H'0'                ELEM '92' SHOULD BE AVAILABLE                
                                                                                
BLDPD10  L     R2,ADISPTAB         TABLE OF DISPLACEMENTS WITHIN ELEM           
         L     R6,APRGDTAB         TABLE OF DEMO CATEGORIES                     
         L     R3,APDEMS           OUTPUT AREA                                  
         USING PDEMOUTD,R3                                                      
                                                                                
         CLI   ELCODE,ELEM93                                                    
         BNE   BLDPD15                                                          
         LA    R4,NPG2VPHS-NPGEL93(R4) INPUT DEMO VALUES                        
         B     BLDPD20                                                          
                                                                                
BLDPD15  CLI   ELCODE,ELEM92                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,NPGVPHS-NPGEL92(R4)  INPUT DEMO VALUES                        
                                                                                
BLDPD20  ZIC   R1,0(R2)                                                         
         CLI   ELCODE,ELEM92       1=LENGTH OF DEMO VALUE ON '92' ELEM          
         BNE   BLDPD25                                                          
         SR    RE,RE                                                            
         CHI   R1,32               92 ELEMENT ONLY HAS 32 DEMOS                 
         BH    BLDPD30                                                          
         AR    R1,R4               1-BYTE DEMOS                                 
         ZIC   RE,0(R1)                                                         
         B     BLDPD30                                                          
                                                                                
BLDPD25  MHI   R1,2                2=LENGTH OF DEMO VALUE ON '93' ELEM          
         AR    R1,R4                                                            
         SR    RE,RE                                                            
         ICM   RE,3,0(R1)          2-BYTE DEMOS                                 
                                                                                
BLDPD30  STCM  RE,3,PDEMSV                                                      
         GOTOR (#TYP3TO4,ATYP3TO4),DMCB,2(R6),PDEMNUM                           
         LA    R3,PDEMOUTL(R3)     ADVANCE IN OUTPUT AREA                       
         LA    R2,3(R2)            ADVANCE IN DISPTAB                           
         LA    R6,3(R6)            ADVANCE IN DEMO NUMBER TABLE                 
         BCT   R5,BLDPD20                                                       
         MVI   0(R3),FF            EOT                                          
                                                                                
BLDPDMSX J     EXITY                                                            
         DROP  R3                                                               
                                                                                
PDMLNQ   EQU   2                   LENGTH OF OUTPUT CELL                        
PRGDN92  EQU   32                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CREATE NAD MISSING DEMOS                                            *         
***********************************************************************         
                                                                                
NADMISD  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   MYNAD,0             DERIVED DEMOS DONT APPLY TO TOTAL US         
         BE    NADMISDX                                                         
         CLI   MYNAD,1                                                          
         BE    NADMISDX                                                         
                                                                                
         L     R3,ANADDERV                                                      
         USING NADDERVD,R3                                                      
                                                                                
NDMIS10  CLC   0(2,R3),EFFS                                                     
         BE    NADMISDX                                                         
         MVC   WORK(1),MYNAD       GET VALUE OF DEMO TO SPLIT AND               
         MVC   WORK+1(1),NDDMOD    ITS UNIVERSE                                 
                                                                                
         CLI   NDDMOD,GWIMPMOD     IS THIS A GAA DEMO?                          
         BNE   NDMIS15                                                          
         CLI   IGAA,0              YES                                          
         BE    NDMIS60             NO GAA DEMOS REQUESTED. SKIP                 
         CLI   IGAA,GAANONE                                                     
         BE    NDMIS60                                                          
         MVI   WORK+1,C'F'         GAA IS 'F' ON NAD FILE INTERNALLY            
                                                                                
NDMIS15  MVC   WORK+2(2),NDDSPL                                                 
         MVI   WORK+4,1                                                         
         MVI   WORK+5,UNVMOD                                                    
         MVC   WORK+6(2),NDDSPL                                                 
         MVI   WORK+8,FF                                                        
         MVI   DBDEMTYP,C'4'       TYPE-4 DEMO REQUEST FOR DEMOUT               
         GOTOR DEMOUT,DMCB,(C'L',WORK),DBLOCK,SPLDEMO,0,0                       
                                                                                
         ZIC   R5,NDDNUM           NUMBER OF DEMOS TO DERIVE                    
         LA    R4,NDDDEMS          START OF DEMOS TO DERIVE                     
         XC    FULL2,FULL2         ACCUMULATE DERIVED DEMOS HERE                
NDMIS20  MVI   WORK,1                                                           
         MVI   WORK+1,UNVMOD       GET UNIVERSE OF THIS DEMO                    
         MVC   WORK+2(1),0(R4)                                                  
         MVI   DBDEMTYP,0          TYPE-3 DEMO REQUEST FOR DEMOUT               
         GOTOR DEMOUT,DMCB,(C'D',WORK),DBLOCK,FULL1,0,0                         
                                                                                
         ZAP   WORK(16),=PL16'0'                                                
         ICM   R0,15,SPLVALU       DEMO TO SPLIT                                
         CVD   R0,WORK+8                                                        
         ICM   R0,15,FULL1                                                      
         CVD   R0,DUB                                                           
         MP    WORK(16),DUB        * UNIVERSE OF DEMO TO DERIVE                 
         ICM   R0,15,SPLUNIV                                                    
         BNZ   NDMIS30                                                          
         SR    R2,R2                                                            
         B     NDMIS40                                                          
NDMIS30  CVD   R0,DUB                                                           
         DP    WORK(16),DUB        /UNIVERSE OF DEMO TO SPLIT                   
         CVB   R2,WORK                                                          
         MP    WORK+8(8),=PL1'2'                                                
         CP    WORK+8(8),DUB                                                    
         BL    *+8                                                              
         AHI   R2,1                ROUND IT                                     
                                                                                
NDMIS40  LR    R0,R2                                                            
         A     R0,FULL2                                                         
         ST    R0,FULL2            FULL2=SUM OF DERIVED DEMOS                   
                                                                                
         GOTOR PUTDERIV            PUT DERIVED DEMO IN ITS SLOT                 
                                                                                
         LA    R4,L'NDDDEMS(R4)                                                 
         BCT   R5,NDMIS20                                                       
                                                                                
         ICM   R0,15,SPLVALU       ADJUST LAST VALUE TO MAKE SURE               
         ICM   R1,15,FULL2         SUM OF SPLIT DEMOS = DEMO TO SPLIT           
         BZ    NDMIS50             NO DEMO DERIVED                              
         SR    R0,R1                                                            
         LTR   R0,R0                                                            
         BZ    NDMIS50             MATCH. DON'T NEED TO ADJUST                  
         AR    R2,R0               NO MATCH. ADJUST BY THE DIFFERENCE           
         SHI   R4,L'NDDDEMS        BACK UP TO LAST DERIVED DEMO                 
         GOTOR PUTDERIV            PUT ADJUSTED VALUE                           
         LA    R4,L'NDDDEMS(R4)                                                 
                                                                                
NDMIS50  LR    R3,R4               NEXT ENTRY IN NADDERV                        
         B     NDMIS10                                                          
                                                                                
NDMIS60  LA    R4,NDDDEMS          SKIP THIS ENTRY IN NADDERV                   
         ZIC   R5,NDDNUM                                                        
NDMIS65  LA    R4,L'NDDDEMS(R4)                                                 
         BCT   R5,NDMIS65                                                       
         LR    R3,R4                                                            
         B     NDMIS10                                                          
                                                                                
NADMISDX J     EXITY                                                            
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PUT DERIVED DEMO VALUE IN ITS SLOT                                  *         
* R2 = DERIVED DEMO VALUE                                             *         
* R3 -> ENTRY IN NADDERV                                              *         
* R4 -> DEMO NUMBER OF DERIVED DEMO                                   *         
***********************************************************************         
                                                                                
PUTDERIV NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING NADDERVD,R3                                                      
                                                                                
         LA    R1,DEMOTAB                                                       
         USING DEMOENTD,R1                                                      
PTDER10  OC    DMODIF,DMODIF                                                    
         BNZ   *+6                                                              
         DC    H'0'                BAD MODIFIER                                 
         CLC   NDDMOD,DMODIF+1                                                  
         BE    PTDER20                                                          
         LA    R1,DEMOENTL(R1)                                                  
         B     PTDER10                                                          
PTDER20  LA    R1,DEMSP8           BUILDING BLOCK SLOTS FOR MODIFIER            
         DROP  R1                                                               
                                                                                
         STAR  CLEAR=Y,ARS=ON      XA MODE ON                                   
         LAM   ARE,ARE,ALET                                                     
         L     RE,ABOOKFRM                                                      
         USING DEMFFRMD,RE                                                      
         ICM   R0,15,DEMFFFOR                                                   
         AR    RE,R0               A(FORMULA TABLES BY MODIFIER)                
         USING DEMFMHDD,RE                                                      
PTDER30  CLC   DEMFM1MD,NDDMOD     CK MODIFIER IN FORMULA TABLE                 
         BE    PTDER40                                                          
         ICM   R0,15,DEMFMLN                                                    
         BNZ   *+6                                                              
         DC    H'0'                BAD MODIFIER                                 
         AR    RE,R0                                                            
         B     PTDER30                                                          
                                                                                
PTDER40  ICM   R0,15,DEMFMAB3                                                   
         AR    RE,R0                                                            
         USING DEMFBL3D,RE                                                      
         CPYA  ARF,ARE                                                          
         LA    RF,DEMFB3DL         TYPE-3 BUILDING BLOCKS                       
         DROP  RE                                                               
                                                                                
PTDER50  CLC   0(L'DEMFB3DL,RF),0(R4)  COMPARE ON DEMO NUMBER                   
         BE    PTDER60                                                          
         LA    RF,L'DEMFB3DL(RF)   NEXT BUILDING BLOCK                          
         LA    R1,L'DEMSP8(R1)     NEXT SLOT                                    
         B     PTDER50                                                          
                                                                                
PTDER60  ICM   R0,15,SVDUR                                                      
         CLI   IMWT,MWTYES         UNLESS MINUTE WEIGHTING REQUESTED,           
         BE    PTDER62                                                          
         CLI   IACM,ACMYES                                                      
         BNE   *+8                                                              
         ICM   R0,15,SVCMSEC       WEIGHT ACM ON COMMERCIAL SECONDS             
PTDER62  CVD   R0,DUB              DUB = WEIGHT                                 
                                                                                
         ZAP   WORK(16),P0                                                      
         CVD   R2,WORK+8                                                        
         MP    WORK(16),DUB        DEMO VALUE*WEIGHT                            
         MVC   0(8,R1),WORK+8      PUT IT IN ITS SLOT                           
                                                                                
         LAM   ARE,ARF,ARZEROS     CLEAR ARE,ARF                                
         LA    RE,*+6                                                           
         BSM   0,RE                                                             
         REAR  ARS=OFF             XA MODE OFF                                  
                                                                                
         J     EXITY                                                            
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READ RECORDS FROM BUFFER AND RELEASE                                *         
***********************************************************************         
                                                                                
NXTSUMR  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R0,ORECORD                                                       
         ST    R0,LP_ADATA                                                      
         L     RE,TSAREC                                                        
         OC    0(SUMKEYL,RE),0(RE)                                              
         BNZ   NXTSUM10                                                         
         MVI   TSACTN,TSARDH       GET FIRST RECORD                             
         B     *+8                                                              
NXTSUM10 MVI   TSACTN,TSANXT       GET NEXT RECORD                              
                                                                                
         GOTOR BUFFER                                                           
                                                                                
         TM    TSERRS,TSEEOF                                                    
         JO    EXITN                                                            
                                                                                
         GOTOR SUMFILT             FILTER SUMMARY RECORDS                       
         BNE   NXTSUM10                                                         
                                                                                
         GOTOR OUTRECD             CREATE OUTPUT RECORD                         
                                                                                
         J     EXITY                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXTRACT INFORMATION FORM DEMO RECORD                                *         
***********************************************************************         
                                                                                
EXTRINFO NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R0,RECINFO                                                       
         LHI   R1,RECINFOL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   RECNET,DBSELSTA     NETWORK                                      
         CLC   RECNET,=C'PAR T'    PAR SHOULD DISPLAY AS UPN                    
         BE    *+14                                                             
         CLC   RECNET,=C'PAR H'                                                 
         BNE   *+10                                                             
         MVC   RECNET(3),=C'UPN'                                                
         CLC   RECNET,=C'PAX T'    PAX SHOULD DISPLAY AS ION                    
         BE    *+14                                                             
         CLC   RECNET,=C'PAX H'                                                 
         BNE   *+10                                                             
         MVC   RECNET(3),=C'ION'                                                
         CLC   RECNET,=C'TF  T'    TF SHOULD DISPLAY AS UMA                     
         BE    *+14                                                             
         CLC   RECNET,=C'TF  H'                                                 
         BNE   *+10                                                             
         MVC   RECNET(3),=C'UMA'                                                
                                                                                
         MVC   RECBOOK,DBSELBK     BOOK                                         
                                                                                
         CLC   HUT,DBSELSTA        TYPES DON'T APPLY TO TP HUTS                 
         BE    EXTRI28                                                          
                                                                                
         GOTOR DEFINE,DMCB,DPTYP4,DBLOCK,WORK                                   
         MVC   RECSPTYP,WORK+4     SUB-PROGRAM TYPE                             
                                                                                
         GOTOR DEFINE,DMCB,DTYPE,DBLOCK,WORK                                    
         MVC   RECPTYP,WORK        PROGRAM TYPE                                 
         MVC   RECCONT,WORK+2      CONTENT TYPE                                 
         MVC   RECAIRNG,WORK+3     AIRING TYPE                                  
                                                                                
         MVI   RECFLAG,0                                                        
         GOTOR DEFINE,DMCB,DBKOUT,DBLOCK,WORK                                   
         CLI   WORK,BKOUT                                                       
         BNE   *+8                                                              
         OI    RECFLAG,BKOTQ       BREAKOUT INDICATOR                           
                                                                                
         GOTOR DEFINE,DMCB,=C'PREM',DBLOCK,WORK                                 
         GOTO1 (RF),(R1),=C'NTI',DBLOCK,DUB                                     
*                                                                               
         L     RE,AIO5                                                          
         LR    RF,RE                                                            
         AHI   RF,1501             L'BUFFER                                     
         CLI   PREMIND,0                                                        
         BNE   EXTRI10                                                          
         MVI   0(RE),X'FF'                                                      
         MVI   PREMIND,X'FF'                                                    
*                                                                               
EXTRI10  CR    RE,RF               DON'T OVERFLOW BUFFER                        
         BNL   EXTRI25             STOP ADDING PROGRAMS                         
         CLI   0(RE),X'FF'                                                      
         BE    EXTRI15                                                          
         CLC   DUB(6),0(RE)                                                     
         BE    EXTRI17                                                          
         LA    RE,6(RE)                                                         
         B     EXTRI10                                                          
*                                                                               
EXTRI15  CLI   WORK,C'Y'         NEW ENTRIES MUST BE PREMIER                    
         BNE   EXTRI25                                                          
         MVC   0(6,RE),DUB                                                      
         MVI   6(RE),X'FF'                                                      
EXTRI17  OI    RECFLAG,PREMQ       PREMIER INDICATOR                            
                                                                                
EXTRI25  CLI   MYMEDIA,MEDSYN                                                   
         BNE   EXTRI28                                                          
         GOTOR DEFINE,DMCB,DGAA,DBLOCK,WORK                                     
         CLI   WORK,YESQ                                                        
         BNE   *+8                                                              
         OI    RECFLAG,GAAQ        GAA INDICATOR FOR SYNDICATION                
                                                                                
EXTRI28  GOTOR DEFINE,DMCB,DDAY,DBLOCK,WORK                                     
         MVC   RECDAYS,WORK        DAYS                                         
         GOTOR DEFINE,DMCB,DTIME,DBLOCK,WORK                                    
         MVC   RECTIMES,WORK+2     TIMES                                        
         GOTOR DEFINE,DMCB,DNTI,DBLOCK,WORK                                     
         PACK  DUB,WORK(5)                                                      
         CVB   R1,DUB                                                           
         STCM  R1,3,RECNTI         NTI NUMBER (HEX)                             
         GOTOR DEFINE,DMCB,DNTIL,DBLOCK,WORK                                    
         MVC   RECLNTI,WORK        LONG NTI NUMBER (CHAR)                       
         GOTOR DEFINE,DMCB,DPROG25,DBLOCK,WORK                                  
         MVC   RECPNAME,WORK       PROGRAM NAME                                 
                                                                                
         GOTOR DEFINE,DMCB,DNLIV,DBLOCK,WORK                                    
         L     RE,AVIEWTYP                                                      
         USING VIEWTABD,RE                                                      
EXTRI33  CLI   0(RE),FF                                                         
         BE    EXTRI40             UNKNOWN VIEWING TYPE                         
         CLC   VWTDEMR,WORK                                                     
         BE    EXTRI35                                                          
         LA    RE,VWTLEN(RE)                                                    
         B     EXTRI33                                                          
EXTRI35  MVC   RECVTYP,VWTINP      VIEWING TYPE FOR PC                          
         DROP  RE                                                               
                                                                                
EXTRI40  XC    RECCMSEC,RECCMSEC                                                
         XC    RECCMNUM,RECCMNUM                                                
         ICM   RE,15,DBAREC                                                     
         CLI   1(RE),C'A'          LOWER CASE PMMEDIA OR PRMEDIA                
         BNL   EXTRI50             IS ACM DATA                                  
         MVI   RECACMI,ACMYESQ                                                  
         GOTOR DEFINE,DMCB,DCOMS,DBLOCK,WORK                                    
         MVC   RECCMSEC,WORK       COMMERCIAL SECONDS                           
         XC    SVCMSEC,SVCMSEC                                                  
         MVC   SVCMSEC+2(2),RECCMSEC  SAVED FULL WORD VALUE                     
         GOTOR DEFINE,DMCB,DCOMT,DBLOCK,WORK                                    
         MVC   RECCMNUM,WORK       COMMERCIAL TELECASTS                         
                                                                                
EXTRI50  LA    R1,1                NUMBER OF AIRINGS                            
         STCM  R1,15,RECAIR                                                     
                                                                                
EXTRI60  GOTOR EXTRDUR             EXTRACT TOTAL DURATION IN RECDUR             
                                                                                
EXTRI70  ICM   RE,15,DBSPANAD      FOR CABLE                                    
         BZ    EXTRI80                                                          
         CLC   0(3,RE),CAB                                                      
         BE    *+10                                                             
         CLC   0(3,RE),CB2                                                      
         BNE   EXTRI80                                                          
         MVC   WORK(5),109(RE)                                                  
         OC    WORK(5),WORK                                                     
         BNZ   *+8                                                              
         MVI   WORK+4,X'F0'                                                     
         PACK  DUB,WORK(5)                                                      
         CVB   R1,DUB                                                           
         STCM  R1,3,RECTRACK       TRACK NUMBER (HEX)                           
         MVC   WORK(10),125(RE)                                                 
         OC    WORK(10),WORK                                                    
         BNZ   *+8                                                              
         MVI   WORK+9,X'F0'                                                     
         PACK  DUB,WORK(10)                                                     
         CVB   R1,DUB                                                           
         STCM  R1,15,RECTCAST      TELECAST NUMBER (HEX)                        
                                                                                
EXTRI80  XC    REC5E,REC5E         CONTROL INFO FROM 5E ELEMENT                 
         L     RE,DBAQUART                                                      
EXTRI83  CLI   0(RE),X'5E'                                                      
         BE    EXTRI85                                                          
         CLI   0(RE),0                                                          
         BE    EXTRI90                                                          
         ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     EXTRI83                                                          
EXTRI85  MVC   REC5E,2(RE)                                                      
                                                                                
EXTRI90  GOTOR DEFINE,DMCB,DFEED,DBLOCK,WORK                                    
         MVC   RECFEED,WORK                                                     
                                                                                
EXTRIX   J     EXIT                                                             
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXTRACT TOTAL DURATION FROM A DEMO RECORD                           *         
* PUT RESULT IN RECDUR                                                *         
***********************************************************************         
                                                                                
EXTRDUR  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R1,30                                                            
         STCM  R1,15,RECDUR                                                     
         OC    RECTIMES,RECTIMES                                                
         BZ    EXTRDURX            DEFAULT IS 30 MINUTES                        
         XC    DUB,DUB                                                          
         MVC   FULL1,RECTIMES                                                   
         LA    R2,RECSTIME                                                      
         LA    R3,DUB                                                           
         GOTOR GETMINS                                                          
         LA    R2,RECETIME                                                      
         LA    R3,DUB+4                                                         
         CLC   0(2,R2),H600AM      DEAL WITH 6AM END TIME                       
         BNE   *+10                                                             
         MVC   0(2,R2),H559AM                                                   
         GOTOR GETMINS                                                          
         LM    R0,R1,DUB           START R0, END R1                             
                                                                                
         CLC   FULL1,RECTIMES      ADJUST AND RESTORE IF 6AM END                
         BE    *+14                                                             
         MVC   RECTIMES,FULL1                                                   
         LA    R1,1(R1)                                                         
         SR    R1,R0                                                            
         BNP   EXTRDURX                                                         
         STCM  R1,15,RECDUR                                                     
                                                                                
         CLI   IMWSRC,ISMTLYQ      FOR MONTHLY NAD REQUESTS                     
         BNE   EXDUR30             TOTL DUR = DURATION * NO OF WEEKS            
         CLI   INADSW,0                                                         
         BE    EXDUR30                                                          
         GOTOR DEFINE,DMCB,DWEEK,DBLOCK,WORK                                    
         LA    RE,WORK+1                                                        
         LA    R1,4                NAD HAS MAX 4 WEEKS FROM DEFINE              
         SR    R0,R0               !! NEED SEPARATE CODE FOR CABLE NAD          
EXDUR20  TM    0(RE),X'F0'         IS THIS WEEK ACTIVE?                         
         BNO   *+8                                                              
         AHI   R0,1                YES. INCREASE NO OF WEEKS                    
         LA    RE,1(RE)                                                         
         BCT   R1,EXDUR20                                                       
                                                                                
         LTR   R0,R0                                                            
         BZ    EXDUR30             WEEKS NOT PROVIDED                           
         SR    RE,RE                                                            
         ICM   RF,15,RECDUR                                                     
         MR    RE,R0                                                            
         STCM  RF,15,RECDUR        DURATION * NO OF WEEKS                       
                                                                                
EXDUR30  MVC   SVDUR,RECDUR        SAVE TO USE AS WEIGHT FOR SPANNING R         
                                                                                
EXTRDURX J     EXIT                                                             
***********************************************************************         
* CONVERT MILITARY TIME TO MINUTES                                    *         
***********************************************************************         
                                                                                
GETMINS  DS    0H                                                               
         LH    R1,0(R2)                                                         
         SR    R0,R0                                                            
         D     R0,F100                                                          
         CHI   R1,6                                                             
         JNL   *+8                                                              
         LA    R1,24(R1)                                                        
         MH    R1,H60                                                           
         AR    R1,R0                                                            
         ST    R1,0(R3)                                                         
         BR    RE                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER OUT UNWANTED RECORDS                                         *         
***********************************************************************         
                                                                                
FILTERS  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   RECFEED,EASTFEED    EXCLUDE EAST AND WEST FEEDS                  
         BE    FILTNO                                                           
         CLI   RECFEED,WESTFEED                                                 
         BE    FILTNO                                                           
                                                                                
         GOTOR INPFILT             BASED ON INPUT FILTERS                       
         BNE   FILTNO                                                           
                                                                                
         GOTOR MIDFILT             BASED ON MFID FILTERS                        
         BNE   FILTNO                                                           
                                                                                
FILTYES  J     EXITY                                                            
FILTNO   J     EXITN                                                            
                                                                                
EASTFEED EQU   C'E'                EAST FEED                                    
WESTFEED EQU   C'W'                WEST FEED                                    
                                                                                
         EJECT                                                                  
***********************************************************************         
* FILTER OUT UNWANTED RECORDS USING INPUT FILTERS                     *         
***********************************************************************         
                                                                                
INPFILT  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         OC    IMXDUR,IMXDUR       MAXIMUM DURATION                             
         BZ    *+14                                                             
         CLC   SVDUR,IMXDUR                                                     
         BH    INPFNO                                                           
                                                                                
         CLC   HUT,RECNET          THESE FILTERS DON'T APPLY TO TP HUTS         
         BE    INPFYES                                                          
                                                                                
         SR    R4,R4               CONTENT TYPES TO EXCLUDE                     
         ICM   R4,7,ACONT                                                       
         BZ    INPF20                                                           
         SR    R2,R2                                                            
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
INPF10   CLC   RECCONT,0(R4)                                                    
         BE    INPFNO                                                           
         LA    R4,CONTLN(R4)                                                    
         BCT   R2,INPF10                                                        
                                                                                
INPF20   CLI   IBKOT,BKOUT         EXCLUDE BREAKOUTS                            
         BNE   INPF30                                                           
         TM    RECFLAG,BKOTQ                                                    
         BO    INPFNO                                                           
                                                                                
INPF30   CLI   IPREM,PREMYES       EXCLUDE EVERYTHING BUT PREMIER               
         BNE   INPF40                                                           
         TM    RECFLAG,PREMQ                                                    
         BZ    INPFNO                                                           
                                                                                
INPF40   SR    R4,R4               AIRING TYPES TO EXCLUDE                      
         ICM   R4,7,AAIRN                                                       
         BZ    INPF60                                                           
         SR    R2,R2                                                            
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
INPF50   CLC   RECAIRNG,0(R4)                                                   
         BE    INPFNO                                                           
         LA    R4,AIRNLN(R4)                                                    
         BCT   R2,INPF50                                                        
                                                                                
INPF60   SR    R4,R4               FILTER ON PNAME EXPRESSION                   
         ICM   R4,7,APNAM                                                       
         BZ    INPF100                                                          
         USING LW_D,R4                                                          
         LHI   R2,1                                                             
         LA    RF,LW_DATA1                                                      
         CLI   LW_TYPE,LQ_TSINQ                                                 
         BE    *+12                                                             
         ICM   R2,3,LW_NUMN                                                     
         LA    RF,LW_DATA2                                                      
         DROP  R4                                                               
         LR    R4,RF                                                            
INPF70   LA    R3,PNAMLN           COMPUTE ACTUAL LENGTH                        
         LA    R1,PNAMLN(R4)                                                    
         BCTR  R1,0                                                             
INPF80   CLI   0(R1),C' '                                                       
         BNE   INPF90                                                           
         BCTR  R3,0                                                             
         BCTR  R1,0                                                             
         B     INPF80                                                           
INPF90   GOTOR VPNAME,DMCB,((R3),(R4)),('PNAMLN',RECPNAME),SCANNER              
         BE    INPF100                                                          
         LA    R4,PNAMLN(R4)                                                    
         BCT   R2,INPF70                                                        
         B     INPFNO                                                           
                                                                                
INPF100  SR    R4,R4               INCLUDE PROGRAM TYPES                        
         ICM   R4,7,AIPTY                                                       
         BZ    INPF120                                                          
         CLI   MYMEDIA,MEDCAB      IGNORE PTYP FILTERS FOR CABLE TP             
         BNE   *+8                                                              
         CLI   MYSOURCE,DBGETDEM                                                
         BE    INPF120                                                          
         SR    R2,R2                                                            
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
INPF110  GOTOR CKPTYP                                                           
         BE    INPF120                                                          
         LA    R4,PTYPLN(R4)                                                    
         BCT   R2,INPF110                                                       
         B     INPFNO                                                           
                                                                                
INPF120  SR    R4,R4               EXCLUDE PROGRAM TYPES                        
         ICM   R4,7,AEPTY                                                       
         BZ    INPF140                                                          
         CLI   MYMEDIA,MEDCAB      IGNORE PTYP FILTERS FOR CABLE TP             
         BNE   *+8                                                              
         CLI   MYSOURCE,DBGETDEM                                                
         BE    INPF140                                                          
         SR    R2,R2                                                            
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
INPF130  GOTOR CKPTYP                                                           
         BE    INPFNO                                                           
         LA    R4,PTYPLN(R4)                                                    
         BCT   R2,INPF130                                                       
                                                                                
INPF140  SR    R4,R4               INCLUDE SUB-PROGRAM TYPES                    
         ICM   R4,7,AISTY                                                       
         BZ    INPF160                                                          
         CLI   MYMEDIA,MEDCAB      IGNORE SPTYP FILTERS FOR CABLE TP            
         BNE   *+8                                                              
         CLI   MYSOURCE,DBGETDEM                                                
         BE    INPF160                                                          
         SR    R2,R2                                                            
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
INPF150  CLC   RECSPTYP,0(R4)                                                   
         BE    INPF160                                                          
         LA    R4,STYPLN(R4)                                                    
         BCT   R2,INPF150                                                       
         B     INPFNO                                                           
                                                                                
INPF160  SR    R4,R4               EXCLUDE SUB-PROGRAM TYPES                    
         ICM   R4,7,AESTY                                                       
         BZ    INPF180                                                          
         CLI   MYMEDIA,MEDCAB      IGNORE SPTYP FILTERS FOR CABLE TP            
         BNE   *+8                                                              
         CLI   MYSOURCE,DBGETDEM                                                
         BE    INPF180                                                          
         SR    R2,R2                                                            
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
INPF170  CLC   RECSPTYP,0(R4)                                                   
         BE    INPFNO                                                           
         LA    R4,STYPLN(R4)                                                    
         BCT   R2,INPF170                                                       
                                                                                
INPF180  SR    R4,R4               FILTER ON NTI NUMBERS                        
         ICM   R4,7,ANTIN                                                       
         BZ    INPF1000                                                         
         SR    R2,R2                                                            
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
INPF190  CLC   RECNTI,0(R4)                                                     
         BE    INPF1000                                                         
         LA    R4,NTINLN(R4)                                                    
         BCT   R2,INPF190                                                       
         B     INPFNO                                                           
                                                                                
INPF1000 DS    0X                                                               
                                                                                
INPFYES  J     EXITY                                                            
INPFNO   J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK MATCH ON PROGRAM TYPE.  FILTER CAN HAVE WILD CARD AS SECOND   *         
* CHARACTER.  EXAMPLE: A* = PROGRAM TYPES STARTING WITH A             *         
***********************************************************************         
                                                                                
CKPTYP   DS    0H                                                               
         CLC   RECPTYP,0(R4)                                                    
         JE    CKPTYPX             EXACT MATCH                                  
         CLI   1(R4),C'*'                                                       
         JNE   CKPTYPX                                                          
         CLC   RECPTYP(1),0(R4)    MATCH ON FIRST CHAR + WILD CARD              
CKPTYPX  BR    RE                  EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* FILTER OUT UNWANTED RECORDS USING MFID FILTERS                      *         
***********************************************************************         
                                                                                
MIDFILT  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         CLI   USETYP,USEMFID                                                   
         BNE   MIDFYES                                                          
                                                                                
         L     R4,MYMIDPTR                                                      
         USING MPPD,R4                                                          
         LA    R2,MPPMID                                                        
         USING MFIDD,R2                                                         
                                                                                
         CLC   RECIND,MFIND        MATCH ON INDICATORS                          
         BNE   MIDFNO                                                           
                                                                                
         OC    MFPTYP,MFPTYP       MATCH ON PROGRAM TYPE                        
         BZ    *+14                                                             
         CLC   RECPTYP,MFPTYP                                                   
         BNE   MIDFNO                                                           
                                                                                
         OC    MFSTYP,MFSTYP       MATCH ON SUB-PROGRAM TYPE                    
         BZ    *+14                                                             
         CLC   RECSPTYP,MFSTYP                                                  
         BNE   MIDFNO                                                           
                                                                                
         OC    MFNTINUM,MFNTINUM   MATCH ON NTI NUMBER                          
         BZ    *+14                                                             
         CLC   RECNTI,MFNTINUM                                                  
         BNE   MIDFNO                                                           
                                                                                
         OC    MFCTRACK,MFCTRACK   MATCH ON TRACK NUMBER                        
         BZ    *+14                                                             
         CLC   RECTRACK,MFCTRACK                                                
         BNE   MIDFNO                                                           
                                                                                
         OC    MFCTELEC,MFCTELEC   MATCH ON TELECAST NUMBER                     
         BZ    *+14                                                             
         CLC   RECTCAST,MFCTELEC                                                
         BNE   MIDFNO                                                           
                                                                                
         OC    MPPPRO,MPPPRO       MATCH ON SELECTED PROGRAME NAME              
         BZ    MIDFYES                                                          
         CLC   RECPNAME,MPPPRO                                                  
         BNE   MIDFNO                                                           
                                                                                
MIDFYES  J     EXITY                                                            
MIDFNO   J     EXITN                                                            
         DROP  R4,R2                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD BUFFER KEY                                                    *         
***********************************************************************         
                                                                                
BFKEY    NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R0,SUMRECD                                                       
         LHI   R1,SUMRECL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
SUMKY    USING MFIDD,SUMMFID                                                    
                                                                                
         CLI   USETYP,USEMFID                                                   
         BNE   BFKEY05                                                          
         L     R4,MYMIDPTR                                                      
         USING MPPD,R4                                                          
         MVC   SUMMFID,MPPMID                                                   
         MVC   SUMPNAME,MPPPRO                                                  
         DROP  R4                                                               
                                                                                
         MVC   SUMNAD,MYNAD        USE NAD CATEGORY, IF REQUIRED                
                                                                                
         OC    SUMKY.MFDAYS,SUMKY.MFDAYS                                        
         BZ    *+8                                                              
         XI    SUMKY.MFDAYS,FF     REVERSE DAY ORDER TO SORT M->S               
                                                                                
         CLI   INADSW,INSEST                                                    
         BE    BFKEYX                                                           
                                                                                
BFKEY05  MVC   SUMKY.MFNET,RECNET  NETWORK - REQUIRED                           
         MVC   SUMKY.MFIND,RECIND  INDICATORS - REQUIRED                        
         MVC   SUMHUNIV,RECHUNIV   HOME UNIVERSE - REQUIRED                     
         MVC   SUMKY.MFTABID,TABID FORMULA TABLE ID - REQUIRED                  
                                                                                
         CLI   USETYP,USEINP                                                    
         BNE   BFKEY10                                                          
         MVC   SUMKY.MFMED,MYMEDIA   MEDIA                                      
         MVC   SUMKY.MFSRC,MYSOURCE  SOURCE                                     
         ICM   RF,15,MYDTBPTR      DAY/TIME EXPRESSION                          
         BZ    *+10                                                             
         MVC   SUMKY.MFDTREQ,0(RF)                                              
                                                                                
BFKEY10  GOTOR CKLVEL,DMCB,LVWEEK  START AND END BOOKS - REQUIRED               
         BE    BFKEY15                                                          
         GOTOR CKLVEL,DMCB,LVMONTH                                              
         BE    BFKEY15                                                          
         GOTOR CKLVEL,DMCB,LVQUART                                              
         BE    BFKEY15                                                          
         B     BFKEY90             DEFAULT IS TO SUMMRZE BY REQST DATES         
                                                                                
BFKEY15  GOTOR CKLVEL,DMCB,LVWEEK                                               
         BNE   BFKEY20             EITHER SUMMARIZE                             
         MVC   SUMKY.MFSBK,RECBOOK ... BY WEEK                                  
         MVC   SUMKY.MFEBK,RECBOOK                                              
                                                                                
BFKEY20  GOTOR CKLVEL,DMCB,LVMONTH ... OR BY MONTH                              
         BNE   BFKEY50                                                          
                                                                                
         CLI   ICALEN,CALENNTI                                                  
         BE    BFKEY30                                                          
         CLI   ICALEN,CALENBRD                                                  
         BE    *+6                                                              
         DC    H'0'                USING BROADCAST CALENDAR                     
BFKEY25  GOTOR NETUNBK,DMCB,(C'W',RECBOOK),DUB,GETDAY,ADDAY,GETBROAD            
         GOTOR GETBROAD,DMCB,(1,DUB),WORK,GETDAY,ADDAY                          
         GOTOR NETWEEK,DMCB,WORK,GETDAY,ADDAY                                   
         MVC   HALF1(1),DMCB+4                                                  
         MVC   HALF1+1(1),DMCB+12  START WEEK OF MONTH                          
         GOTOR NETWEEK,DMCB,WORK+6,GETDAY,ADDAY                                 
         MVC   HALF2(1),DMCB+4                                                  
         MVC   HALF2+1(1),DMCB+12  END WEEK OF MONTH                            
         GOTOR UPDBOOKS            UPDATE KEY START/END BOOKS                   
         B     BFKEY50                                                          
                                                                                
BFKEY30  L     RE,ANADWKS          USING NTI CALENDAR                           
         USING NEWKSD,RE                                                        
         CLC   RECBOOK(1),NEWKSYR  NOT IN TABLE                                 
         BL    BFKEY25                                                          
         BH    BFKEY35                                                          
         CLC   RECBOOK+1(1),NEWKSFRS                                            
         BL    BFKEY25                                                          
BFKEY35  CLI   0(RE),X'FF'         EOT                                          
         BE    BFKEY25                                                          
         CLC   RECBOOK(1),NEWKSYR                                               
         BNE   BFKEY40                                                          
         CLC   RECBOOK+1(1),NEWKSLST                                            
         BH    BFKEY40                                                          
         MVC   HALF1(1),RECBOOK                                                 
         MVC   HALF1+1(1),NEWKSFRS START WEEK OF THE MONTH                      
         MVC   HALF2(1),RECBOOK                                                 
         MVC   HALF2+1(1),NEWKSLST END WEEK OF THE MONTH                        
         B     BFKEY45             GO MAKE SURE WE'RE WITHIN REQSTD BKS         
BFKEY40  LA    RE,NEWKSQ(RE)                                                    
         B     BFKEY35                                                          
BFKEY45  GOTOR UPDBOOKS            UPDATE KEY START/END BOOKS                   
         DROP  RE                                                               
                                                                                
BFKEY50  DS    0H                                                               
         GOTOR CKLVEL,DMCB,LVQUART ... OR BY QUARTERS                           
         BNE   BFKEY100                                                         
         CLI   ICALEN,CALENNTI                                                  
         BE    BFKEY80                                                          
         CLI   ICALEN,CALENBRD                                                  
         BE    *+6                                                              
         DC    H'0'                USING BROADCAST CALENDAR                     
BFKEY60  GOTOR NETUNBK,DMCB,(C'Q',RECBOOK),TEMP,GETDAY,ADDAY,          *        
               GETBROAD                                                         
         L     RE,AQTRTAB                                                       
BFKEY62  CLI   0(RE),FF                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   TEMP(2),0(RE)                                                    
         BE    *+12                                                             
         LA    RE,L'QTRTAB(RE)                                                  
         B     BFKEY62                                                          
         MVC   DUB1(2),TEMP+3                                                   
         MVC   DUB1+2(4),2(RE)     START DATE FOR QUARTER                       
         MVC   DUB2(2),TEMP+3                                                   
         MVC   DUB2+2(4),6(RE)     END DATE FOR QUARTER                         
         CLI   TEMP+1,C'4'         -Q4-                                         
         BNE   BFKEY68                                                          
         GOTOR NETWEEK,DMCB,DUB1,GETDAY,ADDAY                                   
         MVC   HALF1(1),DMCB+4                                                  
         ZIC   RF,DMCB+12                                                       
         LA    RF,1(RF)                                                         
         STC   RF,HALF1+1          START WEEK FOR QUARTER                       
         B     BFKEY70                                                          
                                                                                
BFKEY68  GOTOR DATCON,DMCB,(0,DUB1),(3,FULL1)  -Q1,Q2,Q3-                       
         GOTOR DATCON,DMCB,(3,FULL1),(0,DUB)                                    
         GOTOR GETBROAD,DMCB,(1,DUB),WORK,GETDAY,ADDAY                          
         GOTOR NETWEEK,DMCB,WORK,GETDAY,ADDAY                                   
         MVC   HALF1(1),DMCB+4                                                  
         MVC   HALF1+1(1),DMCB+12  START WEEK FOR QUARTER                       
                                                                                
BFKEY70  CLI   TEMP+1,C'3'         END WEEK OF QUARTER                          
         BNE   BFKEY73             -Q3-                                         
         GOTOR NETWEEK,DMCB,DUB2,GETDAY,ADDAY                                   
         MVC   HALF2(1),DMCB+4                                                  
         MVC   HALF2+1(1),DMCB+12                                               
         B     BFKEY75                                                          
                                                                                
BFKEY73  GOTOR DATCON,DMCB,(0,DUB2),(3,FULL1)  -Q1,Q2,Q4-                       
         GOTOR DATCON,DMCB,(3,FULL1),(0,DUB)                                    
         GOTOR GETBROAD,DMCB,(1,DUB),WORK,GETDAY,ADDAY                          
         GOTOR NETWEEK,DMCB,WORK+6,GETDAY,ADDAY                                 
         MVC   HALF2(1),DMCB+4                                                  
         MVC   HALF2+1(1),DMCB+12                                               
                                                                                
BFKEY75  GOTOR UPDBOOKS                                                         
         B     BFKEY100                                                         
                                                                                
BFKEY80  DS    0H                  USING NTI CALENDAR                           
         GOTOR NETUNBK,DMCB,(C'W',RECBOOK),DUB,GETDAY,ADDAY,GETBROAD            
         L     RE,ANTIQTAB                                                      
         CLC   DUB(6),4(RE)                                                     
         BL    BFKEY60             NOT IN TABLE                                 
BFKEY82  CLI   0(RE),FF                                                         
         BE    BFKEY60                                                          
         CLC   DUB(6),10(RE)                                                    
         BH    BFKEY85                                                          
         MVC   DUB1(6),4(RE)                                                    
         MVC   DUB2(6),10(RE)                                                   
         GOTOR NETWEEK,DMCB,DUB1,GETDAY,ADDAY                                   
         MVC   HALF1(1),DMCB+4                                                  
         MVC   HALF1+1(1),DMCB+12  START WEEK OF MONTH                          
         GOTOR NETWEEK,DMCB,DUB2,GETDAY,ADDAY                                   
         MVC   HALF2(1),DMCB+4                                                  
         MVC   HALF2+1(1),DMCB+12  END WEEK OF MONTH                            
         GOTOR UPDBOOKS            UPDATE KEY START/END BOOKS                   
         B     BFKEY100                                                         
BFKEY85  LA    RE,16(RE)                                                        
         B     BFKEY82                                                          
                                                                                
BFKEY90  DS    0H                  ... OR BY DATES (DEFAULT)                    
         OC    SUMKY.MFSBK(L'MFSBK+L'MFEBK),SUMKY.MFSBK                         
         BNZ   BFKEY100            USING MAINFRAME ID'S                         
         ICM   R3,15,MYDATPTR                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING DATD,R3             USING INPUT REQUEST DATES                    
         MVC   SUMKY.MFSBK,STBK                                                 
         MVC   SUMKY.MFEBK,ENDBK                                                
         DROP  R3                                                               
                                                                                
BFKEY100 GOTOR CKLVEL,DMCB,LVDAY   SUMMARIZE BY DAY?                            
         BNE   *+14                                                             
         MVC   SUMKY.MFDAYS,RECDAYS                                             
         XI    SUMKY.MFDAYS,FF     REVERSE ORDER TO SORT M->S                   
                                                                                
         GOTOR CKLVEL,DMCB,LVTIME  SUMMARIZE BY TIME?                           
         BNE   *+10                                                             
         MVC   SUMKY.MFTIMES,RECTIMES                                           
                                                                                
         GOTOR CKLVEL,DMCB,LVHH    SUMMARIZE BY HALF HOUR?                      
         BE    BFKEY110            OR                                           
         GOTOR CKLVEL,DMCB,LVQH    SUMMARIZE BY QUATER HOUR?                    
         BNE   BFKEY120                                                         
         CLI   DBFUNCT,DBGETDEM    VALID FOR TIME PERIOD ONLY                   
         BE    *+6                                                              
         DC    H'0'                                                             
BFKEY110 MVC   SUMKY.MFTIMES,RECTIMES                                           
                                                                                
BFKEY120 GOTOR CKLVEL,DMCB,LVPROG  SUMMARIZE BY PROGRAM?                        
         BNE   BFKEY130                                                         
         MVC   SUMKY.MFNTINUM,RECNTI NTI NUMBER                                 
         MVC   SUMPNAME,RECPNAME    PROGRAM NAME                                
                                                                                
BFKEY130 GOTOR CKLVEL,DMCB,LVTRAK  SUMMARIZE BY TRACK?                          
         BNE   BFKEY140                                                         
         MVC   SUMKY.MFCTRACK,RECTRACK TRACK NUMBER (HEX)                       
                                                                                
BFKEY140 GOTOR CKLVEL,DMCB,LVEPIS  SUMMARIZE BY TELECAST?                       
         BNE   BFKEY150                                                         
         MVC   SUMKY.MFCTELEC,RECTCAST TELECAST NUMBER (HEX)                    
                                                                                
BFKEY150 GOTOR CKLVEL,DMCB,LVPTYP  SUMMARIZE BY PROGRAM TYPE?                   
         BNE   BFKEY160                                                         
         MVC   SUMKY.MFPTYP,RECPTYP   PROG TYPE - REQUIRED                      
                                                                                
BFKEY160 GOTOR CKLVEL,DMCB,LVSPTYP SUMMARIZE BY SUB-PROGRAM TYPE?               
         BNE   BFKEY170                                                         
         MVC   SUMKY.MFSTYP,RECSPTYP  SUB-PROG TYPE - REQUIRED                  
                                                                                
BFKEY170 DS    0H                                                               
                                                                                
BFKEYX   J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* UPDATE KEY START AND END BOOKS FOR A QUARTER OR MONTH               *         
* HALF1 = START BOOK OF QUARTER/MONTH                                 *         
* HALF2 = END BOOK OF QUARTER/MONTH                                   *         
* INTERSECT QUARTER/MONTH BOOKS WITH EXISTING KEY BOOKS AND           *         
*   WITH REQUESTED DATE RANGE                                         *         
***********************************************************************         
                                                                                
UPDBOOKS NTR1  BASE=*,LABEL=*      INTERSECT WITH EXISTING KEY BOOKS            
                                                                                
         OC    SUMKY.MFSBK(L'MFSBK+L'MFEBK),SUMKY.MFSBK                         
         BNZ   UPDBK10                                                          
         MVC   SUMKY.MFSBK,HALF1                                                
         MVC   SUMKY.MFEBK,HALF2                                                
         B     UPDBK30                                                          
                                                                                
UPDBK10  CLC   HALF1,SUMKY.MFSBK                                                
         BNH   *+10                                                             
         MVC   SUMKY.MFSBK,HALF1                                                
         CLC   HALF2,SUMKY.MFEBK                                                
         BNL   *+10                                                             
         MVC   SUMKY.MFEBK,HALF2                                                
                                                                                
UPDBK30  OC    MYDATPTR,MYDATPTR   INTERSECT WITH REQUESTED DATE RANGE          
         BZ    UPDBKX                                                           
         L     R3,MYDATPTR                                                      
         USING DATD,R3                                                          
         CLC   STBK,SUMKY.MFSBK                                                 
         BNH   *+10                                                             
         MVC   SUMKY.MFSBK,STBK                                                 
         CLC   ENDBK,SUMKY.MFEBK                                                
         BNL   *+10                                                             
         MVC   SUMKY.MFEBK,ENDBK                                                
         DROP  R3                                                               
                                                                                
UPDBKX   J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD BUFFER DESCRIPTIVE INFORMATION                                *         
***********************************************************************         
                                                                                
BFDESC   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         GOTOR CKLVEL,DMCB,LVPROG  SUMMARIZE BY PROGRAM?                        
         BNE   BFDSC10                                                          
         MVC   SUMLNTI,RECLNTI                                                  
                                                                                
BFDSC10  MVC   SUMUSBK,RECBOOK     UNIVERSE START BOOK                          
         MVC   SUMUEBK,RECBOOK     UNIVERSE END BOOK                            
                                                                                
         MVC   SUMACMI,RECACMI     ACM INDICATOR                                
         MVC   SUMVTYP,RECVTYP     VIEWING TYPE                                 
                                                                                
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ACCUMULATE BUFFER NON-DEMO VALUES                                   *         
***********************************************************************         
                                                                                
BFACCUM  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         ICM   R0,15,RECAIR        NUMBER OF AIRINGS                            
         ICM   R1,15,SUMNOAIR                                                   
         AR    R1,R0                                                            
         STCM  R1,15,SUMNOAIR                                                   
                                                                                
         ICM   R0,15,RECDUR        DURATION                                     
         ICM   R1,15,SUMTODUR                                                   
         AR    R1,R0                                                            
         STCM  R1,15,SUMTODUR                                                   
                                                                                
         SR    R0,R0                                                            
         ICM   R0,3,RECCMSEC       COMMERCIAL SECONDS                           
         ICM   R1,15,SUMCMSEC                                                   
         AR    R1,R0                                                            
         STCM  R1,15,SUMCMSEC                                                   
                                                                                
         ZIC   R0,RECCMNUM         NUM OF COMMERCIAL TELECASTS                  
         ICM   R1,15,SUMCMNUM                                                   
         AR    R1,R0                                                            
         STCM  R1,15,SUMCMNUM                                                   
                                                                                
         ICM   R0,15,SVDUR         NAD HOMES                                    
         CVD   R0,DUB              SVDUR IS WEIGHT ALWAYS FILLED IN             
         ZAP   WORK(16),P0         (INCLUDING SPANNING RECORDS)                 
         ICM   R0,15,NADHOMES                                                   
         CVD   R0,WORK+8                                                        
         MP    WORK(16),DUB                                                     
         OC    SUMNADHO,SUMNADHO                                                
         BNZ   BFAC10                                                           
         MVC   SUMNADHO,WORK+8     NAD US HOMES * WEIGHT (PACKED)               
         B     BFAC20                                                           
BFAC10   AP    SUMNADHO,WORK+8(8)                                               
                                                                                
BFAC20   DS    0X                                                               
*&&DO                                                                           
BFAC20   ICM   R0,15,RECDUR        NTI HOMES                                    
         CVD   R0,DUB              RECDUR IS WEIGHT FOR NON-SPANNING            
         ZAP   WORK(16),P0                                                      
         ICM   R0,15,NTIHOMES                                                   
         CVD   R0,WORK+8                                                        
         MP    WORK(16),DUB                                                     
         OC    SUMNTIHO,SUMNTIHO                                                
         BNZ   BFAC30                                                           
         MVC   SUMNTIHO,WORK+8     NTI HOMES * WEIGHT (PACKED)                  
         B     BFAC40                                                           
BFAC30   AP    SUMNTIHO,WORK+8(8)                                               
*&&                                                                             
BFAC40   DS    0X                                                               
                                                                                
BFACCUMX J     EXIT                                                             
***********************************************************************         
* CHECK IF PC REQUESTED LEVEL IN DMCB                                 *         
***********************************************************************         
                                                                                
CKLVEL   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         SR    R4,R4                                                            
         ICM   R4,7,ASUM                                                        
         BZ    CKLVELN                                                          
         SR    R2,R2                                                            
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
CKLVEL10 CLC   0(SUMLN,R4),DMCB+3                                               
         BE    CKLVELY                                                          
         LA    R4,SUMLN(R4)                                                     
         BCT   R2,CKLVEL10                                                      
         B     CKLVELN                                                          
                                                                                
CKLVELY  J     EXITY                                                            
CKLVELN  J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALIZE DBLOCK FIELDS                                            *         
***********************************************************************         
                                                                                
INDBLOCK NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R0,DBLOCK                                                        
         LHI   R1,DBLOCKL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     RE,AMEDIAT          FILL IN DBLOCK FIELDS                        
         USING MEDIATD,RE                                                       
INDB05   CLI   0(RE),FF                                                         
         BNE   *+6                                                              
         DC    H'0'                INVALID MEDIA INPUT                          
         CLC   MYMEDIA,MDREQMED                                                 
         BE    *+12                                                             
         LA    RE,MEDIATL(RE)                                                   
         B     INDB05                                                           
         MVC   DBSELSRC,MDSELSRC                                                
         MVC   DBSELMED,MDSELMED                                                
         MVC   DBFILE,MDFILE                                                    
         MVC   DBSELSTA+4(1),MDSELST4                                           
                                                                                
         MVC   DBBTYPE,MDBTYPE                                                  
         CLI   IVCR,VCREXCL                                                     
         BNE   INDB07                                                           
         CLI   MYMEDIA,MEDNET      NON-VCR FOR BROADCAST AND SYND ONLY          
         BE    *+8                                                              
         CLI   MYMEDIA,MEDSYN                                                   
         BNE   INDB07                                                           
         MVI   DBBTYPE,EVCRBTYQ    SET BOOKTYPE 'I' FOR EXCLUDE VCR             
                                                                                
INDB07   MVC   DBAREC,AIO1                                                      
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFUNCT,MYSOURCE    TP OR PROGRAM DATA                           
                                                                                
         GOTOR CKLVEL,DMCB,LVHH                                                 
         BNE   INDB10                                                           
         GOTOR CKLVEL,DMCB,LVQH                                                 
         BNE   INDB10                                                           
         MVI   DBFUNCT,DBGETDEM                                                 
                                                                                
INDB10   MVC   DBSELAGY,AGYALPH                                                 
                                                                                
         MVC   DBSELSTA(4),MYNET                                                
         CLI   DBSELSTA+4,CBLHH    CABLE HALF HOUR DATA                         
         BNE   INDB20                                                           
         GOTOR CKLVEL,DMCB,LVQH    SUMMARIZE BY QUATER HOUR?                    
         BNE   INDB20                                                           
         MVI   DBSELSTA+4,CBLQH    READ CABLE QUARTER HOUR DATA                 
                                                                                
INDB20   CLC   HUT,DBSELSTA                                                     
         BNE   INDB30                                                           
         MVI   DBFUNCT,DBGETDEM    HUTS ARE TIME PERIOD                         
         CLI   DBSELSTA+4,SYNSTAQ                                               
         BE    INDB25                                                           
         CLI   DBSELSTA+4,CBLSTAQ                                               
         BNE   INDB30                                                           
INDB25   MVI   DBSELSTA+4,NETSTAQ  AND ALWAYS NATIONAL                          
                                                                                
INDB30   MVC   DBSELBK,MYBOOK                                                   
         MVC   DBSELPRG,MYNTI                                                   
                                                                                
         MVI   DBDAYOPT,DBDYOPI    INDIVIDUAL DAYS IS DEFAULT                   
         CLC   HUT,DBSELSTA        HUT REQUEST?                                 
         BE    INDB35              YES. OPTIONS ARE SET                         
         CLI   MYMEDIA,MEDSYN      NO. SYNDICATION HAS NO INDIV DAYS            
         BNE   INDB35                                                           
         MVI   DBDAYOPT,0                                                       
                                                                                
INDB35   OC    DBSELPRG,DBSELPRG                                                
         BZ    *+8                                                              
         MVI   DBDAYOPT,DBDYOPE    EXACT PROGRAM AND DAY/TIME                   
                                                                                
         CLI   IROTN,ROTNY                                                      
         BNE   INDB36                                                           
         CLI   MYDAY,M_F           FOR M-F OR M-S REQUESTS                      
         BE    *+12                                                             
         CLI   MYDAY,M_S                                                        
         BNE   INDB36                                                           
         MVI   DBDAYOPT,0          ROTATIONS REQUESTED                          
         MVI   DBBEST,C'L'         RETURN ONE ENTRY FOR M-F,M-S AVRAGES         
                                                                                
INDB36   MVC   DBSELDAY,MYDAY                                                   
         MVC   DBSELTIM,MYTIME                                                  
         CLI   DBSELDAY,0          DALL?                                        
         BNE   INDB37                                                           
         MVI   DBSELDAY,M_S                                                     
         CLI   IROTN,ROTNY                                                      
         BNE   *+12                                                             
         MVI   DBDAYOPT,0          ROTATIONS REQUESTED                          
         MVI   DBBEST,C'A'         RETURN ALL W/INDIVIDUAL DAYS                 
                                                                                
INDB37   OC    DBSELTIM,DBSELTIM   SET DEFAULT TIMES (ALL)                      
         BNZ   *+10                                                             
         MVC   DBSELTIM,H6A545A                                                 
                                                                                
         MVC   DBSELDUR,IMNDUR     MINIMUM DURATION                             
         CLI   DBSELDUR,0                                                       
         BNE   *+8                                                              
         MVI   DBSELDUR,DEFMINQ    DEFAULT MINIMUM DURATION                     
                                                                                
         MVI   DBSELPTT,C'E'       TELECAST LEVEL ALWAYS                        
                                                                                
         CLI   IVIEWT,0            CREATE 'NLIV' EXTEND                         
         BE    INDB60                                                           
         L     R4,AVIEWTYP                                                      
         USING VIEWTABD,R4                                                      
INDB40   CLI   0(R4),FF                                                         
         BNE   *+6                                                              
         DC    H'0'                INVALID INPUT FROM PC                        
         CLC   IVIEWT,VWTINP       COMPARE ON INPUT CHARACTER                   
         BE    *+12                                                             
         LA    R4,VWTLEN(R4)                                                    
         B     INDB40                                                           
                                                                                
         ICM   RE,15,DBEXTEND                                                   
         BZ    INDB45              GO ADD IT                                    
         USING DBXLIVD,RE                                                       
INDB42   CLC   DBXLIVID,NLIVEXT                                                 
         BE    INDB50              GO CHANGE IT                                 
         ICM   RE,15,DBXLIVNX                                                   
         BNZ   INDB42                                                           
INDB45   ICM   RF,15,DBEXTEND      ADD NEW EXTENT TO BEGINNING OF LIST          
         LA    RE,NLIVEXT                                                       
         STCM  RE,15,DBEXTEND                                                   
         STCM  RF,15,4(RE)                                                      
INDB50   MVC   DBXLIVE,VWTOUT      UPDATE THE EXTENT DATA                       
         DROP  RE,R4                                                            
                                                                                
INDB60   CLI   IACM,0              CREATE 'CAVG' EXTEND                         
         BE    INDB80                                                           
         CLI   IACM,ACMYES                                                      
         BNE   INDB80                                                           
         ICM   RE,15,DBEXTEND      IF NO EXTENT THERE,                          
         BZ    INDB65              GO ADD IT                                    
         USING DBXCAVD,RE                                                       
INDB62   CLC   DBXCAVID,CAVGEXT    IF 'CAVG' EXTENT EXISTS,                     
         BE    INDB70              GO CHANGE IT                                 
         ICM   RE,15,DBXCAVNX                                                   
         BNZ   INDB62                                                           
INDB65   ICM   RF,15,DBEXTEND      ADD NEW EXTENT TO BEGINNING OF LIST          
         LA    RE,CAVGEXT                                                       
         STCM  RE,15,DBEXTEND                                                   
         STCM  RF,15,4(RE)                                                      
INDB70   MVI   DBXCAV,DBXCVYQ      FLAG FOR ACM (AVG COMMERCIAL) DATA           
         DROP  RE                                                               
                                                                                
INDB80   DS    0X                                                               
                                                                                
INDBX    J     EXIT                                                             
                                                                                
CBLHH    EQU   C'C'                                                             
CBLQH    EQU   C'Q'                                                             
EVCRBTYQ EQU   C'I'                                                             
         EJECT                                                                  
***********************************************************************         
* ADVANCE TO NEXT WEEKLY BOOK                                         *         
* AT ENTRY: MYBOOK HAS OLD BOOK                                       *         
* AT EXIT : MYBOOK HAS NEW BOOK                                       *         
***********************************************************************         
                                                                                
NXBOOK   NTR1  BASE=*,LABEL=*      NEXT BOOK                                    
                                                                                
NXBOOK10 GOTOR NETUNBK,DMCB,(C'W',MYBOOK),WORK,GETDAY,ADDAY,GETBROAD            
         GOTOR ADDAY,DMCB,(C'D',WORK),WORK+6,7                                  
         GOTOR NETWEEK,DMCB,WORK+6,GETDAY,ADDAY                                 
         MVC   MYBOOK(1),DMCB+4                                                 
         MVC   MYBOOK+1(1),DMCB+12                                              
                                                                                
         CLI   IMWSRC,ISMTLYQ      IF MONTHLY NAD REQUEST                       
         BNE   NXBOOKX                                                          
         CLI   INADSW,0                                                         
         BE    NXBOOKX                                                          
         GOTOR WKTOMON             GET MONTHLY BOOK IN HALF1                    
         CLC   HALF1,PVNADMN       SAME AS PREVIOUS NAD MONTH?                  
         BE    NXBOOK10            YES. DON'T REQUEST IT AGAIN.                 
                                                                                
NXBOOKX J      EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INTERFACE TO TSAR (ONLINE) OR BUFFERIN (OFFLINE)                    *         
***********************************************************************         
                                                                                
BUFFER   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JNZ   BUFFER02            YES                                          
         MVC   SUMKEYSV,SUMKEY                                                  
         GOTOR TSAR,TSARD         YES - USE TSAR                                
         CLI   TSERRS,0                                                         
         J     EXIT                                                             
                                                                                
BUFFER02 LHI   R0,BUFFAINI         CONVERT TSAR ACTION CODE TO BUFFERIN         
         CLI   TSACTN,TSAINI                                                    
         JE    BUFFER04                                                         
         LHI   R0,BUFFAPUT                                                      
         CLI   TSACTN,TSAADD                                                    
         JE    BUFFER04                                                         
         CLI   TSACTN,TSAWRT                                                    
         JE    BUFFER04                                                         
         LHI   R0,BUFFASEQ                                                      
         CLI   TSACTN,TSANXT                                                    
         JE    BUFFER04                                                         
         MVC   SUMKEYSV,SUMKEY                                                  
         LHI   R0,BUFFARDH                                                      
         CLI   TSACTN,TSARDH                                                    
         JE    BUFFER04                                                         
         DC    H'0'                                                             
                                                                                
BUFFER04 GOTOR RBUFFRIN,DMCB,((R0),ASUMBUF),SUMRECD,ACOMFACS                    
         CLI   TSACTN,TSARDH                                                    
         JNE   BUFFER06                                                         
         CLC   SUMKEYSV,SUMKEY     EMULATE TSAR READ HIGH                       
         JE    BUFFER06                                                         
         OI    BUFFERRS-BUFFPARM(R1),BUFFERNF                                   
                                                                                
BUFFER06 MVC   TSERRS,BUFFERRS-BUFFPARM(R1)                                     
         CLI   TSERRS,0                                                         
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FILTER SUMMARY RECORDS                                              *         
***********************************************************************         
                                                                                
SUMFILT  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         OC    IMNAIR,IMNAIR       MINIMUM NUMBER OF AIRINGS                    
         BZ    *+14                                                             
         CLC   SUMNOAIR,IMNAIR                                                  
         JL    EXITN                                                            
                                                                                
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* CREATE OUTPUT RECORD BY FILLING IN OUTPUT SLOTS                     *         
***********************************************************************         
                                                                                
OUTRECD  NTR1  BASE=*,LABEL=*                                                   
         LA    R0,ORECORD                                                       
         LHI   R1,ORECLN                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
SUMKY    USING MFIDD,SUMMFID                                                    
                                                                                
         OC    SUMKY.MFDAYS,SUMKY.MFDAYS                                        
         BZ    *+8                                                              
         XI    SUMKY.MFDAYS,FF     RESTORE ACTUAL DAYS IN THE MF ID             
                                                                                
         MVC   NADCAT,SUMNAD       NAD CATEGORY                                 
                                                                                
         MVC   WEIGHT,SUMTODUR     TOTAL DURATION IS WEIGHT FOR NON-ACM         
         CLI   IMWT,MWTYES         UNLESS MINUTE WEIGHTING REQUESTED,           
         BE    OUTR04                                                           
         CLI   IACM,ACMYES                                                      
         BNE   *+10                                                             
         MVC   WEIGHT,SUMCMSEC     TOTAL COMM SECDS IS WEIGHT FOR ACM           
                                                                                
OUTR04   MVC   FORMTBID,SUMKY.MFTABID     FORMULA TABLE ID                      
                                                                                
         MVC   ACMIND,SUMACMI      ACM INDICATOR                                
         MVC   VTYP,SUMVTYP        VIEWING TYPE                                 
                                                                                
         ICM   R4,15,MYMIDPTR      SEND PC ID ONLY ONCE                         
         BZ    OUTR07A                                                          
         USING MPPD,R4                                                          
         OC    PREVPCID,PREVPCID                                                
         BZ    OUTR07                                                           
         CLC   PREVPCID,MPPPCI                                                  
         BNE   OUTR07              SAME PC ID                                   
         CLI   INADSW,INSEST       AND NAD ESTIMATING.                          
         BNE   OUTR10                                                           
         XC    ACMIND,ACMIND       SEND ACM INDICATOR ONLY ONCE                 
         XC    VTYP,VTYP           SEND VIEWING TYPE ONLY ONCE                  
         B     OUTR140                                                          
OUTR07   MVC   OPCID,MPPPCI        RETURN PC ID                                 
         MVC   PREVPCID,MPPPCI     SAVE AS PREVIOUS PC ID                       
         DROP  R4                                                               
                                                                                
OUTR07A  CLI   INADSW,INSEST       FOR NAD SKIP MOST DESCRIPTIVE INFO           
         BE    OUTR140                                                          
                                                                                
OUTR10   CLC   SUMUSBK,SUMKY.MFSBK ACTUAL START/END BOOKS ARE                   
         BNH   *+10                AT THE INTERSECTION WITH                     
         MVC   SUMKY.MFSBK,SUMUSBK UNIVERSE START/END BOOKS                     
         CLC   SUMUEBK,SUMKY.MFEBK                                              
         BNL   *+10                                                             
         MVC   SUMKY.MFEBK,SUMUEBK                                              
                                                                                
         CLC   PREVNET,SUMKY.MFNET NETWORK ONLY IF DIFFERENT                    
         BE    *+10                                                             
         MVC   NETWORK,SUMKY.MFNET                                              
         MVC   PREVNET,SUMKY.MFNET                                              
         MVC   PTYPE,SUMKY.MFPTYP  PROGRAM TYPE                                 
         MVC   SBPTYP,SUMKY.MFSTYP SUB-PROGRAM TYPE                             
                                                                                
         MVC   CONTENT,SUMKY.MFCONT    CONTENT TYPE                             
         MVC   AIRNSMS,SUMKY.MFAIRNG   AIRING TYPE                              
         MVC   FLAG1,SUMKY.MFFLAGS     VARIOUS FLAGS                            
                                                                                
OUTR30   OC    SUMKY.MFDTREQ,SUMKY.MFDTREQ   ALL DAYS/TIMES                     
         BNZ   OUTR40                                                           
         MVC   DAYPART(9),=C'DALL/TALL'                                         
         B     OUTR50                                                           
                                                                                
OUTR40   LA    R3,SUMKY.MFDTREQ    DISPLAY DAYS/TIMES                           
         GOTOR DISPDYTM                                                         
                                                                                
OUTR50   MVC   FORMTBID,SUMKY.MFTABID     FORMULA TABLE ID                      
                                                                                
         GOTOR CKLVEL,DMCB,LVWEEK  SUMMARIZE BY WEEK?                           
         BNE   OUTR60                                                           
         GOTOR NETUNBK,DMCB,(C'W',SUMKY.MFSBK),WEEK,GETDAY,ADDAY,      *        
               GETBROAD                                                         
                                                                                
OUTR60   GOTOR CKLVEL,DMCB,LVMONTH SUMMARIZE BY MONTH?                          
         BNE   OUTR70                                                           
         CLI   ICALEN,CALENNTI                                                  
         BE    OUTR62                                                           
         CLI   ICALEN,CALENBRD                                                  
         BE    *+6                 USING BROADCAST CALENDAR                     
         DC    H'0'                                                             
OUTR61   GOTOR NETUNBK,DMCB,(C'M',SUMKY.MFSBK),DUB,GETDAY,ADDAY,       *        
               GETBROAD                                                         
         GOTOR DATCON,DMCB,(0,DUB),(6,MONTH)                                    
         B     OUTR70                                                           
                                                                                
OUTR62   L     RE,ANADWKS          USING NTI CALENDAR                           
         USING NEWKSD,RE                                                        
         CLC   SUMKY.MFSBK(1),NEWKSYR NOT IN TABLE                              
         BL    OUTR61                                                           
         BH    OUTR63                                                           
         CLC   SUMKY.MFSBK+1(1),NEWKSFRS                                        
         BL    OUTR61                                                           
OUTR63   CLI   0(RE),X'FF'                                                      
         BE    OUTR61                                                           
         CLC   SUMKY.MFSBK(1),NEWKSYR                                           
         BNE   OUTR65                                                           
         CLC   SUMKY.MFSBK+1(1),NEWKSLST                                        
         BH    OUTR65                                                           
         CLC   SUMKY.MFEBK+1(1),NEWKSLST                                        
         BNH   *+6                                                              
         DC    H'0'                                                             
         MVC   DUB(1),SUMKY.MFSBK  YEAR                                         
         MVC   DUB+1(1),NEWKSMO    MONTH                                        
         MVI   DUB+2,1             DAY (JUST SET TO 1 SO IT IS VALID)           
         GOTOR DATCON,DMCB,(3,DUB),(6,MONTH)                                    
         B     OUTR70                                                           
OUTR65   LA    RE,NEWKSQ(RE)                                                    
         B     OUTR63                                                           
         DROP  RE                                                               
                                                                                
OUTR70   GOTOR CKLVEL,DMCB,LVQUART  SUMMARIZE BY QUARTERS?                      
         BNE   OUTR90                                                           
         CLI   ICALEN,CALENNTI                                                  
         BE    OUTR80                                                           
         CLI   ICALEN,CALENBRD                                                  
         BE    *+6                 USING BROADCAST CALENDAR                     
         DC    H'0'                                                             
OUTR75   GOTOR NETUNBK,DMCB,(C'Q',SUMKY.MFSBK),QUART,GETDAY,ADDAY,     *        
               GETBROAD                                                         
         CLI   QUART+3,C'9'                                                     
         BNH   *+16                                                             
         IC    R0,QUART+3                                                       
         SHI   R0,10                                                            
         STC   R0,QUART+3                                                       
         B     OUTR90                                                           
                                                                                
OUTR80   GOTOR NETUNBK,DMCB,(C'W',SUMKY.MFSBK),DUB,GETDAY,ADDAY,       *        
               GETBROAD                                                         
         L     RE,ANTIQTAB                                                      
         CLC   DUB(6),4(RE)        NOT IN TABLE                                 
         BL    OUTR75                                                           
OUTR82   CLI   0(RE),X'FF'                                                      
         BE    OUTR75                                                           
         CLC   DUB(6),10(RE)                                                    
         BNH   *+12                                                             
         LA    RE,16(RE)                                                        
         B     OUTR82                                                           
         MVC   QUART(2),2(RE)      QUARTER                                      
         MVI   QUART+2,C'/'                                                     
         MVC   QUART+3(2),0(RE)    YEAR                                         
                                                                                
OUTR90   GOTOR NETUNBK,DMCB,(C'W',SUMKY.MFSBK),STWEEK,GETDAY,ADDAY,    *        
               GETBROAD                                                         
         GOTOR NETUNBK,DMCB,(C'W',SUMKY.MFEBK),ENDWEEK,GETDAY,ADDAY,   *        
               GETBROAD                                                         
                                                                                
OUTR100  OC    SUMKY.MFNTINUM,SUMKY.MFNTINUM                                    
         BZ    OUTR105                                                          
         EDIT  SUMKY.MFNTINUM,NTINO,ALIGN=LEFT    NTI NUMBER                    
                                                                                
OUTR105  MVC   LNTINO,SUMLNTI      LONG NTI NUMBER                              
         MVC   PGNAME,SUMPNAME     PROGRAM NAME                                 
                                                                                
         CLI   SUMKY.MFDAYS,0      ACTUAL DAYS                                  
         BE    OUTR120                                                          
         L     RE,ADAYTAB                                                       
         USING DAYTABD,RE                                                       
OUTR110  CLI   0(RE),X'FF'                                                      
         BE    OUTR115                                                          
         CLC   SUMKY.MFDAYS,DAYTXDAY                                            
         BE    OUTR115                                                          
         LA    RE,DAYTABL(RE)                                                   
         B     OUTR110                                                          
OUTR115  MVC   DAY,DAYTCDAY                                                     
         DROP  RE                                                               
                                                                                
OUTR120  OC    SUMKY.MFTIMES,SUMKY.MFTIMES                                      
         BZ    OUTR127                                                          
         EDIT  SUMKY.MFSTIME,(4,WORK),FILL=0                                    
         MVC   STIME(2),WORK       START TIME HH:MM                             
         MVI   STIME+2,C':'                                                     
         MVC   STIME+3(2),WORK+2                                                
         CLC   =C'24',STIME        SEND HOUR 24 AS 00                           
         BNE   *+10                                                             
         MVC   STIME(2),=C'00'                                                  
                                                                                
         EDIT  SUMKY.MFETIME,(4,WORK),FILL=0                                    
         MVC   ETIME(2),WORK       END TIME HH:MM                               
         MVI   ETIME+2,C':'                                                     
         MVC   ETIME+3(2),WORK+2                                                
         CLC   =C'24',ETIME        SEND HOUR 24 AS 00                           
         BNE   *+10                                                             
         MVC   ETIME(2),=C'00'                                                  
                                                                                
OUTR127  OC    SUMKY.MFCTRACK,SUMKY.MFCTRACK                                    
         BZ    OUTR128                                                          
         EDIT  SUMKY.MFCTRACK,TRACK,ALIGN=LEFT                                  
                                                                                
OUTR128  OC    SUMKY.MFCTELEC,SUMKY.MFCTELEC                                    
         BZ    OUTR129                                                          
         EDIT  SUMKY.MFCTELEC,TCAST,ALIGN=LEFT                                  
                                                                                
OUTR129  MVC   NAIRED,SUMNOAIR     NUMBER OF AIRINGS                            
         MVC   TOTDUR,SUMTODUR     TOTAL DURATION                               
         MVC   COMSEC,SUMCMSEC     TOTAL COMMERCIAL SECONDS                     
         MVC   COMNUM,SUMCMNUM     TOTAL COMMERCIAL TELECASTS                   
                                                                                
         GOTOR HEXOUT,DMCB,SUMKEY,MFID,MFIDLN,0    MAINFRAME ID                 
         LA    RE,MFID+MFIDLN*2-1  LAST CHARACTER OF OUTPUT MFID                
         LA    RF,MFID             FIRST CHARACTER                              
         XC    HALF1,HALF1                                                      
OUTRM10  CLI   0(RE),C'0'                                                       
         BNE   OUTRM40                                                          
         SR    R0,R0                                                            
         ICM   R0,3,HALF1          UPDATE COUNTER                               
         AHI   R0,1                                                             
         STCM  R0,3,HALF1                                                       
         TM    HALF1+1,X'01'                                                    
         BO    OUTRM30             ODD                                          
         MVC   0(2,RE),SPACES                                                   
OUTRM30  SHI   RE,1                BACKOUT ONE CHARACTER                        
         CR    RE,RF                                                            
         BH    OUTRM10                                                          
                                                                                
OUTRM40  GOTOR CKLVEL,DMCB,LVWEEK                                               
         BNE   OUTR140                                                          
         GOTOR CKLVEL,DMCB,LVDAY                                                
         BNE   OUTR140                                                          
         GOTOR CKLVEL,DMCB,LVPROG                                               
         BNE   OUTR140                                                          
         LA    RF,X'40'                                                         
         LA    R4,0                                                             
OUTR130  OR    RF,RF                                                            
         BZ    OUTR140                                                          
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    SUMKY.MFDAYS,0                                                   
         BO    *+16                                                             
         LA    R4,1(R4)                                                         
         SRL   RF,1                                                             
         B     OUTR130                                                          
         GOTOR ADDAY,DMCB,WEEK,RDATE,(R4)                                       
                                                                                
OUTR140  MVC   UHOMES,SUMHUNIV                                                  
                                                                                
         LA    RE,DEMOTAB          DEMOGRAPHICS (MODIF + VALUES)                
         LA    RF,DEMOTABL                                                      
         LA    R0,SUMDEMOS                                                      
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
                                                                                
         CLI   INADSW,INSEST       FOR NAD ESTIMATING                           
         BNE   OUTRDEF                                                          
         GOTOR NADAIMP             ADJUST NAD IMPRESSIONS BY NTI HOMES          
                                                                                
OUTRDEF  L     R3,ALVDFTAB         SEND NULL INDICATORS FOR BLANK LEVLS         
         USING LVDFTABD,R3                                                      
OUTRD10  CLI   0(R3),X'FF'                                                      
         BE    OUTRDEFX                                                         
         MVC   DMCB+4-L'LVDLEVEL(L'LVDLEVEL),LVDLEVEL                           
         GOTOR CKLVEL,DMCB                                                      
         BNE   OUTRD20                                                          
         L     RE,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
         ICM   RF,15,LVDFIELD                                                   
         AR    RE,RF                                                            
         ZIC   R1,LVDLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,RE),0(RE)                                                    
         BNZ   OUTRD20                                                          
         MVC   0(L'NULLCHRS,RE),NULLCHRS   NULL INDICATOR 'N/A'                 
OUTRD20  LA    R3,LVDFTABL(R3)                                                  
         B     OUTRD10                                                          
OUTRDEFX DS    0X                                                               
         DROP  R3                                                               
                                                                                
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* OUTPUT DISPLAYBLE DAY/TIME EXPRESSIONS                              *         
* R3 -> TABLE OF 5 BYTE DAYS/TIMES                                              
***********************************************************************         
                                                                                
DISPDYTM NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R4,DAYPART          OUTPUT AREA FOR DAYS/TIMES                   
                                                                                
         CLI   0(R3),X'FF'         NO DAY/TIME EXPRESSIONS                      
         BE    DISPDTX                                                          
                                                                                
         USING DYTMD,R3                                                         
DSPDT05  L     RE,ADAYTAB          DISPLAY DAYS                                 
         USING DAYTABD,RE                                                       
         OC    DTDAYS,DTDAYS                                                    
         BNZ   DSPDT10                                                          
         MVC   0(4,R4),=C'DALL'                                                 
         LA    R4,4(R4)                                                         
         B     DSPDT40                                                          
DSPDT10  CLI   0(RE),FF            SINGLE DAYS AND M-F/M-S                      
         BE    DSPDT20                                                          
         CLC   DTDAYS,DAYTXDAY                                                  
         BE    *+12                                                             
         LA    RE,DAYTABL(RE)                                                   
         B     DSPDT10                                                          
         MVC   0(L'DAYTCDAY,R4),DAYTCDAY                                        
         DROP  RE                                                               
DSPDT15  CLI   0(R4),C' '          GO TO END OF EXPRESSION                      
         BE    DSPDT40                                                          
         CLI   0(R4),0                                                          
         BE    DSPDT40                                                          
         LA    R4,1(R4)                                                         
         B     DSPDT15                                                          
                                                                                
DSPDT20  L     RE,ADAYTAB          VARIOUS DAYS                                 
         USING DAYTABD,RE                                                       
         LHI   R0,7                                                             
DSPDT25  MVI   0(R4),C'.'                                                       
         ZIC   RF,DAYTXDAY                                                      
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    DTDAYS,0                                                         
         BNO   *+10                                                             
         MVC   0(1,R4),DAYTCDAY                                                 
         LA    R4,1(R4)                                                         
         LA    RE,DAYTABL(RE)                                                   
         BCT   R0,DSPDT25                                                       
         DROP  RE                                                               
                                                                                
DSPDT40  MVI   0(R4),C'/'          DISPLAY TIMES                                
         LA    R4,1(R4)                                                         
         OC    DTTIMES,DTTIMES                                                  
         BNZ   DSPDT45                                                          
         MVC   0(4,R4),=C'TALL'                                                 
         LA    R4,4(R4)                                                         
         B     DSPDT60                                                          
DSPDT45  GOTOR UNTIME,DMCB,DTTIMES,0(R4)                                        
                                                                                
DSPDT50  CLI   0(R4),C' '          GO TO END OF EXPRESSION                      
         BE    DSPDT60                                                          
         CLI   0(R4),0                                                          
         BE    DSPDT60                                                          
         LA    R4,1(R4)                                                         
         B     DSPDT50                                                          
                                                                                
DSPDT60  LA    R3,DYTMLN(R3)                                                    
         CLI   0(R3),X'FF'                                                      
         BE    DISPDTX                                                          
         MVI   0(R4),C'+'          CONCATENATE NEXT D/T EXPRESSION              
         LA    R4,1(R4)                                                         
         B     DSPDT05                                                          
                                                                                
DISPDTX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* COMPUTE ADJUSTED NAD IMPRESSIONS                                    *         
* (WEIGHTED) AJDUSTED IMPS = (WEIGHTED)IMPS * NTI HOMES/ NAD US HOMES *         
* DEMOS ARE IN DEMOTAB                                                *         
***********************************************************************         
                                                                                
NADAIMP  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R2,DEMOTAB                                                       
         USING DEMOENTD,R2                                                      
                                                                                
NADAI10  CLC   DMODIF,WY                                                        
         BE    NADAI15                                                          
         CLC   DMODIF,WB                                                        
         BNE   NADAI30                                                          
                                                                                
NADAI15  LA    R4,DEMSP8                                                        
         SR    R1,R1                                                            
         ICM   R1,3,DEMCOUNT     NO OF DEMOS                                    
                                                                                
NADAI20  ZAP   WORK(16),=PL16'0'                                                
         MVC   WORK+8(8),0(R4)     RAW NAD IMPRESSIONS                          
                                                                                
         L     R3,MYMIDPTR                                                      
         USING MPPD,R3                                                          
         LA    RE,MPPHOM           NTI HOMES                                    
         CLC   DMODIF,WB                                                        
         BNE   *+8                                                              
         LA    RE,MPPGHO           GAA HOMES                                    
         OC    0(HOMELN,RE),0(RE)                                               
         BZ    NADAI30             DON'T ADJUST IF HOMES NOT PROVIDED           
         LA    RE,HOMELN-1(RE)     LAST CHAR OF HOMES                           
                                                                                
         LHI   RF,HOMELN           RF = ACTUAL LENGTH OF HOMES                  
         LHI   R0,HOMELN                                                        
NADAI25  CLI   0(RE),C' '                                                       
         BNE   NADAI27                                                          
         SHI   RF,1                                                             
         SHI   RE,1                                                             
         BCT   R0,NADAI25                                                       
         B     NADAI30             DON'T ADJUST IF HOMES NOT PROVIDED           
                                                                                
NADAI27  LA    RE,MPPHOM           NTI HOMES                                    
         CLC   DMODIF,WB                                                        
         BNE   *+8                                                              
         LA    RE,MPPGHO           GAA HOMES                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RE)                                                      
         NI    DUB+7,X'FC'         MAKE POSITIVE PACK                           
         MP    WORK(16),DUB        *NTI HOMES                                   
***      MP    WORK(16),SUMNTIHO   *WEIGHTED NTI HOMES                          
         DROP  R3                                                               
                                                                                
         ZAP   WORK+16(16),=PL16'0'                                             
         MVC   WORK+24(8),SUMNADHO                                              
         ICM   R0,15,SUMTODUR                                                   
         CVD   R0,DUB                                                           
         DP    WORK+16(16),DUB     WEIGHTED NAD HOMES/WEIGHT                    
         MVC   DUB1,WORK+16                                                     
         MP    WORK+24(8),=PL1'2'                                               
         CP    WORK+24(8),DUB                                                   
         BL    *+10                                                             
         AP    DUB1,=PL1'1'        DUB1=ROUNDED NAD HOMES                       
                                                                                
         DP    WORK(16),DUB1       /NAD HOMES                                   
         MVC   0(8,R4),WORK                                                     
         MP    WORK+8(8),=PL1'2'                                                
         CP    WORK+8(8),DUB1                                                   
         BL    *+10                                                             
         AP    0(8,R4),=PL1'1'     ROUND IT                                     
                                                                                
         LA    R4,8(R4)                                                         
         BCT   R1,NADAI20                                                       
                                                                                
NADAI30  LA    R2,DEMOENTL(R2)                                                  
         CLI   0(R2),0                                                          
         BNE   NADAI10                                                          
                                                                                
NADAIMPX J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROGRAM RECORDS DOWNLOAD                                            *         
***********************************************************************         
                                                                                
PRGDOWN  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     RE,ATABLESL                                                      
         MVC   DTABLES(DTABLELQ),0(RE)                                          
         GOTOR DEMADDR,DMCB,(X'FF',DTABLES),ACOMFACS                            
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BZ    PRGD05                                                           
         GOTOR (#GETAGY,AGETAGY),LP_AGY     GET AGENCY RECORD                   
                                                                                
PRGD05   SR    R4,R4               1.FOR EVERY NETWORK                          
         ICM   R4,7,ANET                                                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R2,R2                                                            
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
PRGD10   STCM  R2,3,MYNETCTR                                                    
         ST    R4,MYNETPTR                                                      
         MVC   MYNET,0(R4)                                                      
         GOTOR NETBIN              GET BINARY NETWORK CODE NETCODE              
                                                                                
         SR    R4,R4               2.FOR EVERY PROGRAM CODE                     
         XC    MYPROG,MYPROG                                                    
         ICM   R4,7,APCOD                                                       
         BZ    PRGD50                                                           
         SR    R2,R2                                                            
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
PRGD20   STCM  R2,3,MYPRGCTR                                                    
         ST    R4,MYPRGPTR                                                      
         MVC   MYPROG,0(R4)                                                     
                                                                                
PRGD50   XC    IOKEY,IOKEY         BUILD PROGRAM RECORD KEY                     
         LA    R2,IOKEY                                                         
         USING NPGRECD,R2                                                       
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKNET,NETCODE                                                  
                                                                                
         L     R4,AAGYREC          GET AGENCY/MEDIA CODE                        
         MVI   ELCODE,AGYMEDEQ                                                  
         MVC   DATADISP,=AL2(24)                                                
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
PRGD60   BRAS  RE,NEXTEL                                                        
         BNE   PRGDOWNX                                                         
         USING AGYMEDEL,R4                                                      
         CLI   AGYMEDCD,NETMEDQ    MEDIA 'N'                                    
         BNE   PRGD60                                                           
         MVC   NPGKAM,AGYMEDBT                                                  
         DROP  R4                                                               
                                                                                
         MVC   NPGKPROG,MYPROG                                                  
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOSPTDIR+IO4'  READ HIGH                 
PRGD65   BNE   PRGNXPRG                                                         
                                                                                
         CLC   IOKEY(NPGKPROG-NPGKEY),IOKEYSAV                                  
         BNE   PRGNXPRG                                                         
                                                                                
         MVC   KEYTEMP,IOKEY                                                    
                                                                                
         OC    MYPROG,MYPROG                                                    
         BZ    PRGD70                                                           
         LA    RE,MYPROG           COMPARE ON PROGRAM CODE                      
         LA    RF,NPGKPROG                                                      
         LA    R1,L'MYPROG(RE)                                                  
PRGD68   CLI   0(RE),C' '          END OF STRING TO MATCH?                      
         BE    PRGD70              YES                                          
         CR    RE,R1                                                            
         BNL   PRGD70              YES                                          
         CLC   0(1,RE),0(RF)                                                    
         BNE   PRGNXPRG                                                         
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         B     PRGD68                                                           
                                                                                
PRGD70   OC    IPSDATE,IPSDATE     COMPARE START DATE                           
         BZ    PRGD75                                                           
         GOTOR DATCON,DMCB,(0,IPSDATE),(2,WORK)                                 
         CLC   NPGKEND,WORK                                                     
         BL    PRGD120                                                          
                                                                                
PRGD75   OC    IPEDATE,IPEDATE     COMPARE END DATE                             
         BNZ   PRGD76                                                           
         OC    IPSDATE,IPSDATE     NO END DATE. IF START DATE IS AVAIL,         
         BZ    PRGD80               USE IT AS END DATE AS WELL                  
         B     PRGD77              WORK HAS CONVERTED START DATE                
PRGD76   GOTOR DATCON,DMCB,(0,IPEDATE),(2,WORK)                                 
PRGD77   CLC   NPGKEND,WORK                                                     
         BH    PRGD120                                                          
         DROP  R2                                                               
                                                                                
PRGD80   DS    0X                  GET PROGRAM RECORD FROM FILE                 
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOSPTFIL+IO4'                           
                                                                                
         ICM   R0,15,AIO4                                                       
                                                                                
         LR    RE,R0                                                            
         CLC   KEYTEMP,0(RE)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         TM    NPGCNTL-NPGRECD(RE),X'80'   SKIP DELETED RECORDS                 
         BO    PRGD120                                                          
                                                                                
         MVC   DATADISP,=AL2(24)                                                
         AH    R0,DATADISP                                                      
         STCM  R0,15,AFRSTEL                                                    
                                                                                
         GOTOR PRGFILTS            APPLY PROGRAM RECORD FILTERS                 
         BNE   PRGD120                                                          
                                                                                
PRGD110  GOTOR EXTRPINF            EXTRACT PROGRAM RECORD INFO                  
                                                                                
         GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
                                                                                
PRGD120  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOSPTDIR+IO4'    READ SEQUENTIAL         
         B     PRGD65                                                           
                                                                                
PRGNXPRG OC    MYPROG,MYPROG       NO SPECIFIC PROGRAM CODES REQUESTED          
         BZ    PRGNXNET                                                         
         SR    R2,R2               NEXT PROGRAM CODE                            
         ICM   R2,3,MYPRGCTR                                                    
         BCTR  R2,0                                                             
         LTR   R2,R2                                                            
         BZ    PRGNXNET                                                         
         L     R4,MYPRGPTR                                                      
         LA    R4,PRGCDLN(R4)                                                   
         B     PRGD20                                                           
                                                                                
PRGNXNET SR    R2,R2               NEXT NETWORK                                 
         ICM   R2,3,MYNETCTR                                                    
         BCTR  R2,0                                                             
         LTR   R2,R2                                                            
         BZ    PRGDOWNX                                                         
         L     R4,MYNETPTR                                                      
         LA    R4,NETLN(R4)                                                     
         B     PRGD10                                                           
                                                                                
PRGDOWNX J     EXITY                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* APPLY PROGRAM RECORD FILTERS                                        *         
***********************************************************************         
                                                                                
PRGFILTS NTR1  BASE=*,LABEL=*                                                   
                                                                                
         ICM   R4,15,AIO4          GET '92' ELEMENT                             
         MVC   DATADISP,=AL2(24)                                                
         MVI   ELCODE,ELEM92                                                    
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                SHOULD ALWAYS HAVE IT                        
         USING NPGEL92,R4                                                       
                                                                                
         SR    R3,R3               COMPARE ON DAYS/TIMES                        
         ICM   R3,7,ADYTM                                                       
         BZ    PRGF60                                                           
         SR    R2,R2               FOR EVERY DAY/TIME BLOCK                     
         ICM   R2,3,LW_NUMN-LW_D(R3)                                            
         LA    R3,LW_DATA2-LW_D(R3)                                             
PRGF20   LA    R3,DTSTRLN(R3)                                                   
                                                                                
         LR    R6,R3               FOR EVERY DAY/TIME IN THE BLOCK              
         USING DYTMD,R6                                                         
PRGF30   OC    DTDAYS,DTDAYS                                                    
         BZ    PRGF35              NO DAY FILTERING 'DALL'                      
         MVC   BYTE2,DTDAYS                                                     
         NC    BYTE2,NPGDAY        CK OVERLAP ON DAYS                           
         BZ    PRGF40                                                           
PRGF35   OC    DTTIMES,DTTIMES                                                  
         BZ    PRGF60              NO TIME FILTERING 'TALL'                     
         SR    R1,R1               CK OVERLAP ON TIMES                          
         ICM   R1,3,DTSTIME                                                     
         CH    R1,=H'2400'         ADJUST 12M-6A                                
         BNH   *+8                                                              
         SH    R1,=H'2400'                                                      
         SR    R0,R0                                                            
         ICM   R0,3,NPGTIME+2                                                   
         CR    R1,R0                                                            
         BH    PRGF40              REQ START TIME > RECD END TIME               
         SR    R1,R1                                                            
         ICM   R1,3,DTETIME                                                     
         CH    R1,=H'2400'                                                      
         BNH   *+8                                                              
         SH    R1,=H'2400'                                                      
         SR    R0,R0                                                            
         ICM   R0,3,NPGTIME                                                     
         CR    R1,R0               REQ END TIME < RECD START TIME               
         BL    PRGF40                                                           
         B     PRGF60              MATCHED ON DAY AND TIME                      
                                                                                
PRGF40   LA    R6,DYTMLN(R6)       NEXT DAY/TIME IN THE BLOCK                   
         CLI   0(R6),FF                                                         
         BNE   PRGF30                                                           
         DROP  R6                                                               
                                                                                
PRGF50   LA    R3,DTBLN(R3)        NEXT DAY/TIME BLOCK                          
         BCT   R2,PRGF20                                                        
         B     PRGFNO              NO MATCH ON DAYS/TIMES                       
                                                                                
PRGF60   OC    IPFILTR,IPFILTR     COMPARE ON FILTER FIELD                      
         BZ    PRGF70                                                           
         MVC   WORK(L'NPGFILT),NPGFILT                                          
         OC    WORK(L'NPGFILT),SPACES    PAD WITH SPACES                        
         CLC   IPFILTR,WORK                                                     
         BNE   PRGFNO                                                           
                                                                                
         DROP  R4                                                               
                                                                                
PRGF70   ICM   R4,15,AIO4          GET '93' ELEMENT                             
         MVC   DATADISP,=AL2(24)                                                
         MVI   ELCODE,ELEM93                                                    
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         SR    R4,R4                                                            
         USING NPGEL93,R4                                                       
                                                                                
         OC    APDPT,APDPT         FILTER ON DAYPART CODES                      
         BZ    PRGF100                                                          
         LTR   R4,R4                                                            
         BZ    PRGFNO              NO ELEMENT MEANS NO DAYPART                  
         SR    RE,RE                                                            
         ICM   RE,7,APDPT          CHECK REQUESTED DAYPARTS,ONE BY ONE          
         SR    RF,RF                                                            
         ICM   RF,3,LW_NUMN-LW_D(RE)                                            
         LA    RE,LW_DATA2-LW_D(RE)                                             
PRGF80   OC    NPG2DYPA,NPG2DYPA                                                
         BZ    PRGF90                                                           
         CLC   NPG2DYPA,0(RE)                                                   
         BE    PRGF100                                                          
         B     PRGF95                                                           
PRGF90   CLC   NPG2DYP,0(RE)                                                    
         BE    PRGF100                                                          
PRGF95   LA    RE,PDPLN(RE)                                                     
         BCT   RF,PRGF80                                                        
         B     PRGFNO                                                           
                                                                                
PRGF100  OC    IPVWSRC,IPVWSRC     FILTER ON VIEWING SOURCE                     
         BZ    PRGF110                                                          
         LTR   R4,R4                                                            
         BZ    PRGFNO              NO ELEMENT MEANS NO MATCH                    
         CLC   IPVWSRC,NPG2SRC                                                  
         BE    PRGF110                                                          
         OC    IPVWTYP,IPVWTYP     IF NO VIEWING TYPE FILTER                    
         BNZ   PRGFNO                                                           
         CLI   IPVWSRC,NPG2SPGM    FOR REQUESTED 'P'                            
         BNE   PRGFNO                                                           
         CLI   NPG2SRC,NPG2SAPG    ALLOW 'A' AS WELL                            
         BNE   PRGFNO                                                           
                                                                                
PRGF110  OC    IPVWTYP,IPVWTYP     FILTER ON VIEWING TYPE                       
         BZ    PRGF120                                                          
         LTR   R4,R4                                                            
         BZ    PRGFNO              NO ELEMENT MEANS NO MATCH                    
         L     RE,AVIEWTYP                                                      
         USING VIEWTABD,RE                                                      
PRGF115  CLI   0(RE),FF                                                         
         BNE   *+6                                                              
         DC    H'0'                INVALID VIEWING TYPE FROM PC                 
         CLC   IPVWTYP,VWTINP                                                   
         BE    PRGF117                                                          
         LA    RE,VWTLEN(RE)                                                    
         B     PRGF115                                                          
PRGF117  CLC   VWTPROG,NPG2VTYP                                                 
         BNE   PRGFNO                                                           
         DROP  R4,RE                                                            
                                                                                
PRGF120  DS    0X                                                               
         B     PRGFYES                                                          
                                                                                
PRGFYES  J     EXITY                                                            
PRGFNO   J     EXITN                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXTRACT PROGRAM RECORD INFO                                         *         
***********************************************************************         
                                                                                
EXTRPINF NTR1  BASE=*,LABEL=*                                                   
         XC    COMDEF,COMDEF                                                    
         XC    CSERNUM,CSERNUM                                                  
         XC    CSVTYPE,CSVTYPE                                                  
                                                                                
         ICM   R2,15,AIO4                                                       
         USING NPGRECD,R2                                                       
                                                                                
         NI    PRECFLAG,X'FF'-PREC1DEC    SET PRECISION BITS                    
         NI    PRECFLAG,X'FF'-PREC2DEC                                          
         ICM   R4,15,AIO4                                                       
         MVI   ELCODE,ELEM5D                                                    
         MVC   DATADISP,=AL2(24)                                                
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   5(2,R4),=X'5901'                                                 
         BNE   EXTRP10                                                          
         OI    PRECFLAG,PREC2DEC   2 DECIMAL PRECISION                          
         B     *+8                                                              
EXTRP10  OI    PRECFLAG,PREC1DEC   1 DECIMAL PRECISION                          
                                                                                
         MVC   NETWORK,MYNET       NETWORK                                      
                                                                                
         MVC   PCODE,NPGKPROG      PROGRAM CODE                                 
                                                                                
         GOTOR DATCON,DMCB,(2,NPGKEND),(0,PEDATE)    END DATE                   
                                                                                
         XC    PADATE,PADATE                                                    
         XC    PACTION,PACTION                                                  
         CLI   NPGMAINL,X'01'                                                   
         BNE   EXTRP15             NO ACTIVITY ELEMENT                          
         GOTOR DATCON,DMCB,(3,NPGACTD),(0,PADATE)    LAST ACTIVTY DATE          
         MVC   PACTION,NPGACT                                                   
                                                                                
EXTRP15  MVI   PBLANK,0            PC NEEDS A BLANK IDENTIFIER                  
                                                                                
         XC    PCRDATE,PCRDATE                                                  
         XC    PCRPID,PCRPID                                                    
         XC    PLSDATE,PLSDATE                                                  
         XC    PLSPID,PLSPID                                                    
         XC    PTIMSTP,PTIMSTP                                                  
         ICM   R4,15,AIO4                                                       
         MVI   ELCODE,NPG0ACDQ     EXTENDED ACTIVITY ELEMENT                    
         MVC   DATADISP,=AL2(24)                                                
         BRAS  RE,GETEL                                                         
         BNE   EXTRP40                                                          
         USING NPGEL0A,R4                                                       
         OC    NPG0ACDT,NPG0ACDT                                                
         BZ    EXTRP20                                                          
         GOTOR DATCON,DMCB,(3,NPG0ACDT),(0,PCRDATE)  CREATION DATE              
         MVC   PCRPID,NPG0ACPD     CREATION PID                                 
EXTRP20  OC    NPG0ALDT,NPG0ALDT                                                
         BZ    EXTRP30                                                          
         GOTOR DATCON,DMCB,(3,NPG0ALDT),(0,PLSDATE)  LAST CHG DATE              
         MVC   PLSPID,NPG0ALPD     LAST CHG PID                                 
EXTRP30  MVC   PTIMSTP,NPG0ATIM    TIME STAMP                                   
                                                                                
EXTRP40  XC    FORMTBID,FORMTBID                                                
         XC    PMETHOD,PMETHOD                                                  
         MVI   PFLAG1,0            CLEAR FLAGS                                  
         MVI   PFLAG2,0                                                         
         OI    PFLAG2,PF2HNOT      HUT NOT AVAILABLE INDICATOR                  
         XC    HUTOUT,HUTOUT                                                    
         ICM   R4,15,AIO4                                                       
         MVI   ELCODE,NPGAICDQ     AE INFO ELEMENT                              
         MVC   DATADISP,=AL2(24)                                                
         BRAS  RE,GETEL                                                         
         BNE   EXTRP50                                                          
         USING NPGAEIFD,R4                                                      
         TM    NPGAIFLG,NPGAIMFQ   AE RECD CHANGED FROM MF                      
         BO    EXTRP50                                                          
         MVC   FORMTBID,NPGAITID   FORMULA TABLE ID                             
         OI    PFLAG2,PF2AE        AE-RECORD INDICATOR                          
         MVC   PFLAG1,NPGAIFLG     FLAG                                         
         CLI   NPGAILN,NPGAEIL1           IS METHODOLOGY AVAILABLE?             
         BNH   EXTRP50                                                          
         MVC   PMETHOD,NPGAIMET    YES. SEND METHODOLOGY INDICATOR              
         CLI   NPGAILN,NPGAEIL2           IS HUT AVAILABLE?                     
         BNH   EXTRP50                                                          
         OC    NPGAHUT,NPGAHUT                                                  
         BZ    EXTRP50                                                          
         MVI   HMODIF,PPUTMOD      YES. SEND HUT MODIFIER AND VALUE             
         MVC   HDTYP4,=AL2(HOMESQ)                                              
         MVC   HVALUE,NPGAHUT                                                   
         NI    PFLAG2,X'FF'-PF2HNOT  HUT IS AVAILABLE                           
         DROP  R4                                                               
                                                                                
EXTRP50  ICM   R4,15,AIO4                                                       
         MVI   ELCODE,ELEM92        '92' ELEMENT                                
         MVC   DATADISP,=AL2(24)                                                
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPGEL92,R4                                                       
                                                                                
         GOTOR UNDAY,DMCB,NPGDAY,WORK        DAYS                               
         MVC   DAY,SPACES                                                       
         MVC   DAY(4),WORK                                                      
         GOTOR PROPER,DMCB,DAY,4    CONVERT DAY TO PROPER CASE                  
                                                                                
         MVC   PTIMES,SPACES                                                    
         GOTOR UNTIME,DMCB,NPGTIME,PTIMES    TIMES                              
                                                                                
         GOTOR DRATSHR              DISPLAY RATING/SHARE                        
                                                                                
         MVC   PPNAME,NPGNAME       PROGRAM NAME                                
         MVC   PFILTER,NPGFILT      PROGRAM RECORD FILTER                       
                                                                                
         XC    PROTN,PROTN                                                      
         OC    NPGROT,NPGROT        ROTATION                                    
         BZ    EXTRP60                                                          
         GOTOR UNDAY,DMCB,NPGROT,PROTN                                          
         GOTOR PROPER,DMCB,PROTN,L'PROTN  CONVERT ROTN TO PROPER CASE           
                                                                                
EXTRP60  XC    PNTINO,PNTINO                                                    
         OC    NPGPPNO,NPGPPNO                                                  
         BZ    EXTRP100                                                         
         MVC   PNTINO,NPGPPNO                                                   
                                                                                
         DROP  R4                                                               
                                                                                
EXTRP100 XC    PSDATE,PSDATE                                                    
         XC    PDAYPT,PDAYPT                                                    
         MVI   PVWSRC,0                                                         
         MVI   PVWTYP,0                                                         
                                                                                
         ICM   R4,15,AIO4                                                       
         MVI   ELCODE,ELEM93       '93' ELEMENT                                 
         MVC   DATADISP,=AL2(24)                                                
         BRAS  RE,GETEL                                                         
         BNE   EXTRP200                                                         
         USING NPGEL93,R4                                                       
                                                                                
         CLI   NPG2SRC,0           VIEWING SOURCE                               
         BE    *+10                                                             
         MVC   PVWSRC,NPG2SRC                                                   
         CLI   NPG2VTYP,0                                                       
         BE    EXTRP106                                                         
         L     RE,AVIEWTYP         VIEWING TYPE                                 
         USING VIEWTABD,RE                                                      
EXTRP105 CLI   0(RE),FF                                                         
         BE    EXTRP106            UNKNOWN VIEWING TYPE                         
         CLC   NPG2VTYP,VWTPROG                                                 
         BE    *+12                                                             
         LA    RE,VWTLEN(RE)                                                    
         B     EXTRP105                                                         
         MVC   PVWTYP,VWTINP                                                    
         DROP  RE                                                               
                                                                                
EXTRP106 OC    NPG2STD,NPG2STD     START DATE                                   
         BZ    EXTRP110                                                         
         GOTOR DATCON,DMCB,(2,NPG2STD),(0,PSDATE)                               
                                                                                
EXTRP110 MVC   PDAYPT(1),NPG2DYP   1-CHAR DAYPART                               
         OC    NPG2DYPA,NPG2DYPA                                                
         BZ    *+10                                                             
         MVC   PDAYPT,NPG2DYPA     2-CHAR DAYPART                               
                                                                                
         DROP  R4                                                               
                                                                                
EXTRP200 XC    PCONT,PCONT         CLEAR OUTPUT FIELDS                          
         XC    PNEW,PNEW                                                        
         XC    PTYPE,PTYPE                                                      
         XC    SBPTYP,SBPTYP                                                    
         XC    PDEMDEF,PDEMDEF                                                  
         XC    PFAX,PFAX                                                        
         XC    PWINDW,PWINDW                                                    
         XC    PSUBDPT,PSUBDPT                                                  
         XC    PTCRWB1,PTCRWB1                                                  
         XC    PMLTRUN,PMLTRUN                                                  
         XC    PMIRROR,PMIRROR                                                  
         XC    PTIER,PTIER                                                      
         XC    PRCP,PRCP                                                        
         XC    PGHOMES,PGHOMES                                                  
                                                                                
         ICM   R4,15,AIO4                                                       
         MVI   ELCODE,ELEM03       '03' ELEMENT                                 
         MVC   DATADISP,=AL2(24)                                                
         BRAS  RE,GETEL                                                         
         BNE   EXTRP300                                                         
         USING NPGEL03,R4                                                       
                                                                                
         MVC   PCONT,NPPRGRAT      CONTENT                                      
         MVC   PNEW,NPPRNEW        NEW STATUS                                   
         MVC   PTYPE,NPPRGTYP      PROGRAM TYPE                                 
         MVC   SBPTYP,NPPRGSTP     SUBPROGRAM TYPE                              
         MVC   PDEMDEF,NPGNADDM    DEMDEF CODE                                  
         MVC   PFAX,NPGTRFAX       FAX                                          
         MVC   PSUBDPT,NPGSDPT     SUB-DAYPART                                  
         MVC   PMLTRUN,NPGMRUN     MULTI-RUN                                    
         MVC   PMIRROR,NPGMIRCD    MIRROR CODE                                  
         MVC   PTIER,NPTIER        TIER                                         
         CLI   NPGLEN3,NPG3LNQ1                                                 
         BE    EXTRP202                                                         
         MVC   COMDEF,NPGCDEF      COMDEF CODE                                  
         MVC   CSERNUM,NPGCSN      COMSCORE SERIES #                            
                                                                                
         OC    NPGCSVT,NPGCSVT     COMSCORE VIEWING TYPE                        
         JZ    EXTRP202                                                         
         MVC   CSVTYPE,=C'RL'      DEFAULT TO LIVE                              
         CLI   NPGCSVT,NPCVTRCQ                                                 
         JNE   *+10                                                             
         MVC   CSVTYPE,=C'RC'      LIVE COMMERCIAL?                             
         CLI   NPGCSVT,NPCVTR3Q                                                 
         JNE   *+10                                                             
         MVC   CSVTYPE,=C'R3'      LIVE + 3?                                    
         CLI   NPGCSVT,NPCVTR7Q                                                 
         JNE   *+10                                                             
         MVC   CSVTYPE,=C'R7'      LIVE + 7                                     
                                                                                
EXTRP202 TM    NPGSTATB,X'80'      WINDOW                                       
         BNO   EXTRP210                                                         
         CLI   NPWINNUM,0                                                       
         BNE   EXTRP207                                                         
         MVI   PWINDW,C'W'         NO VALUE, JUST SEND 'W'                      
         B     EXTRP210                                                         
EXTRP207 EDIT  (1,NPWINNUM),(3,PWINDW),ALIGN=LEFT    VALUE                      
                                                                                
EXTRP210 OC    NPGTCAR,NPGTCAR     TCAR/WB1 LEVEL                               
         BZ    EXTRP220                                                         
         MVI   PTCRWB1,C'W'                                                     
         TM    NPGSTATB,TCAR5                                                   
         BZ    *+8                                                              
         MVI   PTCRWB1,C'T'                                                     
         MVC   PTCRWB1+1(1),NPGTCAR                                             
                                                                                
EXTRP220 TM    NPGSTATB,RCPREC                                                  
         BZ    EXTRP230                                                         
         MVI   PRCP,C'Y'                                                        
                                                                                
EXTRP230 DS    0X                                                               
         DROP  R4                                                               
                                                                                
EXTRP300 GOTOR BLDPBLKS            BLDNG BLOCKS FROM AE-RECORDS                 
                                                                                
         GOTOR BLDPDEMS            DEMOS FROM NON-AE RECORDS                    
                                                                                
EXTRP350 OC    FORMTBID,FORMTBID   FOR OLD AE RECORDS WHERE                     
         BZ    EXTRP360                                                         
         OC    HUTOUT,HUTOUT       HUT IS NOT AVAILABLE                         
         BNZ   EXTRP360                                                         
         SR    RE,RE                                                            
         ICM   RE,3,PGSHARE        BUT SHARE IS                                 
         BZ    EXTRP360                                                         
         SR    R0,R0               COMPUTE Z=Y/S                                
         ICM   R1,15,PGHOMES                                                    
         M     R0,=F'1000'                                                      
         DR    R0,RE                                                            
         SLA   R0,1                CHECK THE REMAINDER                          
         CR    R0,RE                                                            
         BL    *+8                                                              
         AHI   R1,1                ROUND IT                                     
         STCM  R1,15,HVALUE                                                     
         MVI   HMODIF,PPUTMOD      HUT MODIFIER AND VALUE                       
         MVC   HDTYP4,=AL2(HOMESQ)                                              
         NI    PFLAG2,X'FF'-PF2HNOT  FLAG THAT HUT IS AVAILABLE                 
         OI    PFLAG2,PF2CHUT        BUT IS COMPUTED                            
                                                                                
EXTRP360 DS    0X                                                               
                                                                                
EXTRPIFX J     EXITY                                                            
         DROP  R2                                                               
HOMESQ   EQU   1                   HOMES DEMO NUMBER                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY RATING/SHARE FROM PROGRAM RECORD                            *         
* ON ENTRY, R2 POINTS TO PROGRAM RECORD                               *         
*           R4 POINTS TO '92' ELEMENT                                 *         
* ROUTINE COPIED FROM NESFM13                                         *         
***********************************************************************         
                                                                                
DRATSHR  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    PGSHARE,PGSHARE                                                  
                                                                                
         USING NPGEL92,R4                                                       
         USING NPGRECD,R2                                                       
         L     R3,AAGYREC                                                       
         USING AGYRECD,R3                                                       
                                                                                
         XC    RSHRFLD,RSHRFLD                                                  
         OC    NPGSHARE,NPGSHARE       ANY RATING OR SHARE?                     
         BZ    DRATSHRX                                                         
                                                                                
         LA    R1,RSHRFLD                                                       
         TM    NPGSTAT,X'80'           RATING?                                  
         BZ    DRS200                  NO - SHARE, DISPLAY 1 DEC PREC.          
                                                                                
         TM    AGYFLAG2,AGYFLAG2_2DP   2 DEC PRECISION?                         
         BNZ   DRS20                                                            
         TM    AGYFLAG2,AGYFLAG2_BDP   USER DEFINED PRECISION?                  
         BNZ   DRS50                                                            
         MVI   0(R1),C'R'              MUST BE 1 DECIMAL PRECISION              
         LA    R1,1(R1)                                                         
         B     DRS210                                                           
                                                                                
DRS20    TM    AGYFLAG2,AGYFLAG2_2DP   2 DEC PRECISION?                         
         BZ    DRS50                                                            
         MVI   0(R1),C'R'                                                       
         LA    R1,1(R1)                                                         
         B     DRS220                                                           
                                                                                
DRS50    TM    AGYFLAG2,AGYFLAG2_BDP   2 DEC PRECISION?                         
         BNZ   *+6                                                              
         DC    H'0'                    SOMETHING'S WRONG                        
         TM    PRECFLAG,PREC1DEC       1 DEC PRECSION?                          
         BZ    DRS60                                                            
         MVC   0(2,R1),=C'1R'                                                   
         LA    R1,2(R1)                                                         
         B     DRS210                                                           
                                                                                
DRS60    TM    PRECFLAG,PREC2DEC       2 DEC PRECISION?                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   0(2,R1),=C'2R'                                                   
         LA    R1,2(R1)                                                         
         B     DRS220                                                           
                                                                                
DRS200   MVC   PGSHARE,NPGSHARE        SAVE PROG RECORD SHARE                   
         TM    AGYFLAG2,AGYFLAG2_BDP   BOTH PRECISION MODE                      
         BZ    DRS210                                                           
         TM    PRECFLAG,PREC2DEC       2 DEC PRECISION?                         
         BO    *+12                                                             
         MVI   0(R1),C'*'                                                       
         LA    R1,1(R1)                                                         
         DROP  R3                                                               
                                                                                
DRS210   EDIT  (B2,NPGSHARE),(5,0(R1)),1,ALIGN=LEFT                             
         B     DRATSHRX                                                         
                                                                                
DRS220   DS    0H                      DISPLAY 2 DECIMAL PRECISION              
         EDIT  (B2,NPGSHARE),(5,0(R1)),2,ALIGN=LEFT                             
         B     DRATSHRX                                                         
                                                                                
DRATSHRX J     EXITY                                                            
         DROP  R2,R4                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DEMO ENGINE DOWNLOAD - DEMO FORMULAS                                *         
***********************************************************************         
                                                                                
DEFODOWN NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     RE,ATABLESL                                                      
         MVC   DTABLES(DTABLELQ),0(RE)                                          
         GOTOR DEMADDR,DMCB,(X'FF',DTABLES),ACOMFACS                            
                                                                                
         ICM   R2,15,AAEFORMS      READ AE FORMULA TABLES FROM DTASPACE         
         BZ    DEFDWNXX                                                         
         STAR  CLEAR=Y,ARS=ON                                                   
         LAM   AR2,AR2,ALET                                                     
         USING DEMFFMSD,R2                                                      
                                                                                
DEFOD10  CLC   0(2,R2),EFFS                                                     
         BE    DEFDWNX             FINISHED DOWNLOADING ALL TBL IDS             
                                                                                
         SR    RE,RE               FILTER ON A LIST OF TABLE ID'S               
         ICM   RE,7,ATBID                                                       
         BZ    DEFOD50                                                          
         SR    R1,R1                                                            
         ICM   R1,3,LW_NUMN-LW_D(RE)                                            
         LA    RE,LW_DATA2-LW_D(RE)                                             
DEFOD20  CLC   DEMFFTID,0(RE)                                                   
         BE    DEFOD50                                                          
         LA    RE,TBIDLN(RE)                                                    
         BCT   R1,DEFOD20                                                       
         B     DEFOD70             TABLE NOT REQUESTED. SKIP IT.                
                                                                                
DEFOD50  MVC   OTABID,DEMFFTID                                                  
                                                                                
         MVC   HALF1,DEMFFBK                                                    
         LA    RE,HALF1                                                         
         ST    RE,DMCB             BOOK                                         
         MVI   DMCB,C'W'           WEEKLY BOOK INDICATOR                        
         CLI   DEMFFIND,DEMFWKQ                                                 
         BE    *+8                                                              
         MVI   DMCB,C'M'           MONTHLY BOOK INDICATOR                       
                                                                                
         LAM   AR0,ARF,ARZEROS     CLEAR XR'S                                   
         GOTOR NETUNBK,DMCB,,OSTBOOK,GETDAY,ADDAY,GETBROAD                      
         LAM   AR2,AR2,ALET        RESTORE AR2                                  
         SAC   512                 RESTORE XA MODE ON                           
                                                                                
         CLI   DEMFFIND,DEMFWKQ                                                 
         BE    DEFOD53             WEEKLY, DONE                                 
         GOTOR DATCON,DMCB,(X'30',OSTBOOK),(0,DUB),(0,0)                        
         MVC   OSTBOOK,DUB         RETURN THE BEGINNING OF THE MONTH            
                                                                                
DEFOD53  LA    R4,DEMFFLQ+4(R2)    POINT TO FORMULA TABLE                       
         CPYA  AR4,AR2                                                          
         USING DEMFFRMD,R4                                                      
         ICM   RF,15,DEMFGMAC                                                   
         CPYA  ARF,AR2                                                          
         AR    RF,R4                                                            
         STCM  RF,15,AFGMACRO      A(GENERAL MACROS TABLE)                      
                                                                                
         ICM   RF,15,DEMFSMAC                                                   
         BNZ   DEFOD55                                                          
         XC    AFSMACRO,AFSMACRO   NO SPECIFIC MACROS                           
         B     DEFOD57                                                          
DEFOD55  AR    RF,R4                                                            
         ST    RF,AFSMACRO         A(SPECIFIC MACROS TABLE)                     
                                                                                
DEFOD57  ICM   RF,15,DEMFFOVR                                                   
         BNZ   DEFOD58                                                          
         XC    AFOVRELS,AFOVRELS   NO OVERRIDE RELATIONALS                      
         B     DEFOD59                                                          
DEFOD58  AR    RF,R4                                                            
         ST    RF,AFOVRELS         A(OVERRIDE RELATIONAL FORMULAS)              
                                                                                
DEFOD59  ICM   RE,15,DEMFFFOR                                                   
         AR    R4,RE                                                            
         USING DEMFMHDD,R4                                                      
DEFOD60  CLC   DEMFMLN,=AL4(0)                                                  
         BE    DEFOD70             FINISHED ALL MODIFIERS IN THIS TABLE         
         MVC   TABMOD,DEMFM2MD                                                  
         ICM   RF,15,DEMFMAB4                                                   
         AR    RF,R4                                                            
         USING DEMFBL4D,RF                                                      
         CPYA  ARE,AR2                                                          
         LA    RE,DEMFB4DL                                                      
         ST    RE,ABLDBLKS         A(TABLE OF TYPE-4 BUILDING BLOCKS)           
         DROP  RF                                                               
                                                                                
         ICM   RF,15,DEMFMAFT                                                   
         AR    RF,R4                                                            
         ST    RF,ACOMPUTS         A(COMPUTATIONAL FORMULAS)                    
                                                                                
         LAM   AR0,ARF,ARZEROS     CLEAR XRS                                    
                                                                                
         GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
                                                                                
         XC    OTABID,OTABID       SEND THIS INFO ONLY ONCE                     
         XC    OSTBOOK,OSTBOOK                                                  
         XC    AFGMACRO,AFGMACRO                                                
         XC    AFSMACRO,AFSMACRO                                                
         XC    AFOVRELS,AFOVRELS                                                
                                                                                
         LAM   AR2,AR2,ALET        RESTORE AR3 AFTER DEMOUT                     
         SAC   512                 RESTORE XA MODE ON                           
         CPYA  AR4,AR2                                                          
                                                                                
         ICM   RE,15,DEMFMLN                                                    
         AR    R4,RE                                                            
         B     DEFOD60                                                          
                                                                                
DEFOD70  ICM   RE,15,DEMFFLQ(R2)   DISPLACEMENT TO NEXT TABLE ID                
         AR    R2,RE                                                            
         B     DEFOD10                                                          
                                                                                
DEFDWNX  LAM   AR0,ARF,ARZEROS     CLEAR XR'S                                   
         LA    RE,*+6                                                           
         BSM   0,RE                                                             
         REAR  ARS=OFF             XA MODE OFF                                  
DEFDWNXX J     EXITY                                                            
         DROP  R2,R4                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONVERT NETWORK ALPHA TO BINARY NETWORK CODE                        *         
* RETURN NETWORK CODE IN NETCODE                                      *         
***********************************************************************         
                                                                                
NETBIN   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING STAREC,R2           BUILD STATION RECORD KEY                     
         MVI   STAKTYPE,STAKTYPQ                                                
         MVI   STAKMED,NETMEDQ                                                  
         MVC   STAKCALL,MYNET                                                   
         MVI   STAKCALL+4,NETMEDQ                                               
         MVC   STAKAGY,AGYALPH                                                  
         MVC   STAKCLT(6),=6X'F0'                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSTAFIL+#STAREC'                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   IOKEY(STAKCLT-STAKEY),IOKEYSAV                                   
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         ICM   R2,15,ASTAREC       CONVERT MARKET TO BINARY                     
         PACK  DUB,SMKT                                                         
         CVB   R1,DUB                                                           
         STCM  R1,3,NETCODE                                                     
                                                                                
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
NETMEDQ  EQU   C'N'                                                             
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONVERT STRING FROM UPPER CASE TO PROPER CASE                       *         
* P1 = A(STRING) TO CONVERT                                           *         
* P2 = LENGTH OF STRING                                               *         
***********************************************************************         
                                                                                
PROPER   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     RE,0(R1)            START OF STRING                              
         ICM   R0,15,4(R1)         LENGTH OF STRING                             
         BNZ   *+6                                                              
         DC    H'0'                CAN'T BE 0                                   
         MVI   BYTE1,UPPCASE       FIRST CHAR SHOULD BE UPPERCASE               
                                                                                
PROP10   CLI   0(RE),C'A'          FOR CHARACTERS                               
         BL    PROP30                                                           
         CLI   0(RE),C'Z'                                                       
         BH    PROP30                                                           
         CLI   BYTE1,UPPCASE       WANT UPPER CASE?                             
         BNE   PROP20                                                           
         MVI   BYTE1,LOWCASE       YES.KEEP IT AND NEXT WIL BE LOWER CS         
         B     PROP40                                                           
PROP20   ZIC   RF,0(RE)            NO.                                          
         SHI   RF,X'40'            SWITCH TO LOWER CASE                         
         STC   RF,0(RE)            ANC CONTINUE WITH LOWER CASE                 
         B     PROP40                                                           
                                                                                
PROP30   MVI   BYTE1,UPPCASE       NOT A CHARACTER. NEXT WILL BE UPCASE         
                                                                                
PROP40   LA    RE,1(RE)                                                         
         BCT   R0,PROP10                                                        
                                                                                
         J     EXIT                                                             
                                                                                
UPPCASE  EQU   0                                                                
LOWCASE  EQU   1                                                                
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* REQUEST MAPS                                                        *         
***********************************************************************         
         PRINT GEN                                                              
REQCNRP  LKREQ H,M#CNRP,OUTCNRP       ** AE REPORT DOWNLOAD **                  
Media    LKREQ F,1,(D,B#SAVED,IMEDIA),CHAR,COL=*,TEXT=NE#MED                    
Srce     LKREQ F,2,(D,B#SAVED,ISOURCE),(U,#VALSRC,$VALSRC),COL=*,      *        
               MAXLEN=1,TEXT=NE#SRC                                             
Dates    LKREQ F,4,(I,B#SAVED,DATIND),(U,#VALDATS,$VALDATS),COL=*,     *        
               LIST=F,OLEN=DATLN,TEXT=NE#SEDAT                                  
DyTim    LKREQ F,5,(I,B#SAVED,DYTMIND),(U,#VALDYTM,$VALDYTM),COL=*,    *        
               LIST=F,SORT=N,MAXLEN=DTSTRLN,OLEN=DTBLN+DTSTRLN,        *        
               TEXT=NE#DYTM                                                     
Ntwks    LKREQ F,6,(I,B#SAVED,NETIND),CHAR,COL=*,LIST=F,OLEN=NETLN,    *        
               MAXLEN=NETLN,TEXT=NE#NTWKS                                       
NadCt    LKREQ F,7,(I,B#SAVED,NADIND),(U,#VALNAD,$VALNAD),COL=*,       *        
               LIST=F,OLEN=NADLN,MAXLEN=3,TEXT=NE#NADCT                         
SumLv    LKREQ F,8,(I,B#SAVED,SUMIND),CHAR,COL=*,LIST=F,OLEN=SUMLN,    *        
               MAXLEN=SUMLN,TEXT=NE#SUML                                        
MfId     LKREQ F,10,(I,B#SAVED,MFIDIND),(U,#VALMFID,$VALMFID),COL=*,   *        
               TEXT=NE#MID,SORT=N,OLEN=MFIDLN,                         *        
               ARRAY=S                                                          
SPNam    LKREQ F,11,,CHAR,COL=*,OLEN=PNAMLN,TEXT=NE#SPNAM,DELIM=X'FF'           
PcId     LKREQ F,13,,CHAR,COL=*,OLEN=PCIDLN,TEXT=NE#PCID                        
NtiHm    LKREQ F,16,,CHAR,COL=*,OLEN=HOMELN,TEXT=NE#NTIHM                       
GaaHm    LKREQ F,17,,CHAR,COL=*,OLEN=HOMELN,TEXT=NE#GAAHM,             *        
               ARRAY=E                                                          
Calen    LKREQ F,12,(D,B#SAVED,ICALEN),CHAR,COL=*,TEXT=NE#CALND                 
NadSw    LKREQ F,14,(D,B#SAVED,INADSW),CHAR,COL=*,TEXT=NE#NADSW                 
MWSrc    LKREQ F,15,(D,B#SAVED,IMWSRC),CHAR,COL=*,TEXT=NE#MWSRC                 
MnAir    LKREQ F,20,(D,B#SAVED,IMNAIR),UBIN,COL=*,MAXLEN=9,TEXT=NE#MNAR         
MnDur    LKREQ F,21,(D,B#SAVED,IMNDUR),(U,#VALMIND,$VALMIND),COL=*,    *        
               MAXLEN=3,TEXT=NE#MNDR                                            
MxDur    LKREQ F,22,(D,B#SAVED,IMXDUR),UBIN,COL=*,MAXLEN=9,TEXT=NE#MXDR         
Cont     LKREQ F,23,(I,B#SAVED,CONTIND),CHAR,COL=*,OLEN=CONTLN,LIST=F, *        
               MAXLEN=CONTLN,TEXT=NE#ECONT                                      
AirTy    LKREQ F,24,(I,B#SAVED,AIRNIND),CHAR,COL=*,OLEN=AIRNLN,LIST=F, *        
               MAXLEN=AIRNLN,TEXT=NE#ETRAN                                      
IPtyp    LKREQ F,25,(I,B#SAVED,IPTYIND),CHAR,COL=*,OLEN=PTYPLN,LIST=F, *        
               MAXLEN=PTYPLN,TEXT=NE#PTYI                                       
IStyp    LKREQ F,26,(I,B#SAVED,ISTYIND),CHAR,COL=*,OLEN=STYPLN,LIST=F, *        
               MAXLEN=STYPLN,TEXT=NE#STYI                                       
Pname    LKREQ F,27,(I,B#SAVED,PNAMIND),(U,#VALPNAM,$VALPNAM),COL=*,   *        
               OLEN=PNAMLN,MAXLEN=PNAMLN,LIST=F,TEXT=NE#PNAM                    
NtiNo    LKREQ F,28,(I,B#SAVED,NTININD),UBIN,COL=*,OLEN=NTINLN,LIST=F, *        
               MAXLEN=5,TEXT=NE#NTIN                                            
GAA      LKREQ F,29,(D,B#SAVED,IGAA),CHAR,COL=*,TEXT=NE#GAA                     
Brkt     LKREQ F,30,(D,B#SAVED,IBKOT),CHAR,COL=*,TEXT=NE#IBKT                   
Rotn     LKREQ F,31,(D,B#SAVED,IROTN),CHAR,COL=*,TEXT=NE#ROTN2                  
VType    LKREQ F,32,(D,B#SAVED,IVIEWT),CHAR,COL=*,TEXT=NE#VTYP                  
VCR      LKREQ F,33,(D,B#SAVED,IVCR),CHAR,COL=*,TEXT=NE#VCR                     
ACM      LKREQ F,34,(D,B#SAVED,IACM),CHAR,COL=*,TEXT=NE#ACM                     
MinWgt   LKREQ F,35,(D,B#SAVED,IMWT),CHAR,COL=*,TEXT=NE#MWT                     
Premier  LKREQ F,36,(D,B#SAVED,IPREM),CHAR,COL=*,TEXT=NE#PREM                   
EPtyp    LKREQ F,70,(I,B#SAVED,EPTYIND),CHAR,COL=*,OLEN=PTYPLN,LIST=F, *        
               MAXLEN=PTYPLN,TEXT=NE#PTYE                                       
EStyp    LKREQ F,71,(I,B#SAVED,ESTYIND),CHAR,COL=*,OLEN=STYPLN,LIST=F, *        
               MAXLEN=STYPLN,TEXT=NE#STYE                                       
         LKREQ E                                                                
                                                                                
                                                                                
REQPDWN  LKREQ H,M#PDOWN,OUTPDOWN     ** PROGRAM RECORDS DOWNLOAD **            
Ntwks    LKREQ F,1,(I,B#SAVED,NETIND),CHAR,COL=*,LIST=F,OLEN=NETLN,    *        
               MAXLEN=NETLN,TEXT=NE#NTWKS                                       
PCode    LKREQ F,2,(I,B#SAVED,PCDIND),CHAR,COL=*,LIST=F,OLEN=PRGCDLN,  *        
               MAXLEN=PRGCDLN,TEXT=NE#PRGMS                                     
PerSt    LKREQ F,3,(D,B#SAVED,IPSDATE),(U,#VALDATE,$VALDATE),COL=*,    *        
               TEXT=NE#SDATE                                                    
PerEn    LKREQ F,4,(D,B#SAVED,IPEDATE),(U,#VALDATE,$VALDATE),COL=*,    *        
               TEXT=NE#EDATE                                                    
PFilt    LKREQ F,5,(D,B#SAVED,IPFILTR),CHAR,COL=*,TEXT=NE#FILT                  
PDayp    LKREQ F,6,(I,B#SAVED,PDPIND),CHAR,COL=*,LIST=F,OLEN=PDPLN,    *        
               MAXLEN=PDPLN,TEXT=NE#DPRTS                                       
PDyTm    LKREQ F,7,(I,B#SAVED,DYTMIND),(U,#VALDYTM,$VALDYTM),COL=*,    *        
               LIST=F,SORT=N,MAXLEN=DTSTRLN,OLEN=DTBLN+DTSTRLN,        *        
               TEXT=NE#DYTM                                                     
VSrc     LKREQ F,8,(D,B#SAVED,IPVWSRC),CHAR,COL=*,TEXT=NE#VSRC                  
VTyp     LKREQ F,9,(D,B#SAVED,IPVWTYP),CHAR,COL=*,TEXT=NE#VTYP                  
         LKREQ E                                                                
                                                                                
                                                                                
REQDEFO  LKREQ H,M#DEFORM,OUTDEFO     ** DEMO ENGINE DWNLD- FORMULAS **         
TabId    LKREQ F,1,(I,B#SAVED,TBIDIND),CHAR,COL=*,LIST=F,OLEN=TBIDLN,  *        
               MAXLEN=TBIDLN,TEXT=NE#TABID                                      
         LKREQ E                                                                
                                                                                
                                                                                
         LKREQ X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAPS                                                         *         
***********************************************************************         
                                                                                
OUTCNRP  LKOUT H                   ** NET CONSTRUCTOR DOWNLOAD **               
                                                                                
OUTINFO  LKOUT R,1                 DISPLAY INFORMATION                          
Netwk    LKOUT C,1,(D,B#SAVED,NETWORK),CHAR,ND=Y                                
SWeek    LKOUT C,2,(D,B#SAVED,STWEEK),EDAT,ND=Y                                 
EWeek    LKOUT C,3,(D,B#SAVED,ENDWEEK),EDAT,ND=Y                                
Week     LKOUT C,4,(D,B#SAVED,WEEK),EDAT,ND=Y                                   
Month    LKOUT C,5,(D,B#SAVED,MONTH),CHAR,ND=Y                                  
Quart    LKOUT C,6,(D,B#SAVED,QUART),CHAR,ND=Y                                  
NtiNo    LKOUT C,7,(D,B#SAVED,NTINO),CHAR,ND=Y                                  
LNti     LKOUT C,8,(D,B#SAVED,LNTINO),CHAR,ND=Y                                 
PgNam    LKOUT C,9,(D,B#SAVED,PGNAME),CHAR,ND=Y                                 
Ptype    LKOUT C,10,(D,B#SAVED,PTYPE3),CHAR,ND=Y                                
Sptyp    LKOUT C,11,(D,B#SAVED,SBPTYP),CHAR,ND=Y                                
WkDay    LKOUT C,12,(D,B#SAVED,DAY),CHAR,ND=Y                                   
Stime    LKOUT C,13,(D,B#SAVED,STIME),CHAR,ND=Y                                 
Etime    LKOUT C,14,(D,B#SAVED,ETIME),CHAR,ND=Y                                 
RDate    LKOUT C,15,(D,B#SAVED,RDATE),EDAT,ND=Y                                 
NoAir    LKOUT C,16,(D,B#SAVED,NAIRED),UBIN,ND=Y                                
ToDur    LKOUT C,17,(D,B#SAVED,TOTDUR),UBIN,ND=Y                                
DayPt    LKOUT C,18,(D,B#SAVED,DAYPART),CHAR,ND=Y                               
Track    LKOUT C,19,(D,B#SAVED,TRACK),CHAR,ND=Y                                 
Tcast    LKOUT C,20,(D,B#SAVED,TCAST),CHAR,ND=Y                                 
Cont     LKOUT C,21,(D,B#SAVED,CONTENT),CHAR,ND=Y                               
Airng    LKOUT C,22,(D,B#SAVED,AIRNSMS),CHAR,ND=Y                               
Flag1    LKOUT C,23,(D,B#SAVED,FLAG1),HEXD,ND=Y                                 
UHome    LKOUT C,25,(D,B#SAVED,UHOMES),UBIN,ND=Y                                
CmSec    LKOUT C,26,(D,B#SAVED,COMSEC),UBIN,ND=Y                                
CmNum    LKOUT C,27,(D,B#SAVED,COMNUM),UBIN,ND=Y                                
PCid     LKOUT C,29,(D,B#SAVED,OPCID),CHAR,ND=Y                                 
Mnfid    LKOUT C,30,(D,B#SAVED,MFID),CHAR,ND=Y                                  
AcmI     LKOUT C,31,(D,B#SAVED,ACMIND),UBIN,ND=Y                                
VTyp     LKOUT C,32,(D,B#SAVED,VTYP),CHAR,ND=Y                                  
         LKOUT E                                                                
                                                                                
OUTDEMO  LKOUT R,2                 DEMOGRAPHIC INFORMATION                      
TabId    LKOUT C,1,(D,B#SAVED,FORMTBID),CHAR                                    
Weigt    LKOUT C,2,(D,B#SAVED,WEIGHT),UBIN                                      
NadC     LKOUT C,3,(D,B#SAVED,NADCAT),UBIN                                      
Array    LKOUT C,100,(A,ARYDEMOS)                                               
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
                                                                                
OUTPDOWN LKOUT H                   ** PROGRAM RECORDS DOWNLOAD **               
                                                                                
OUTPINF  LKOUT R,1                 PROGRAM INFORMATION                          
Netw     LKOUT C,1,(D,B#SAVED,NETWORK),CHAR                                     
PCode    LKOUT C,2,(D,B#SAVED,PCODE),CHAR                                       
Edate    LKOUT C,3,(D,B#SAVED,PEDATE),EDAT                                      
Day      LKOUT C,4,(D,B#SAVED,DAY),CHAR                                         
Times    LKOUT C,5,(D,B#SAVED,PTIMES),CHAR                                      
SDate    LKOUT C,6,(D,B#SAVED,PSDATE),EDAT,ND=Y                                 
PName    LKOUT C,7,(D,B#SAVED,PPNAME),CHAR,ND=Y                                 
RSFld    LKOUT C,8,(D,B#SAVED,RSHRFLD),CHAR,ND=Y                                
Filter   LKOUT C,9,(D,B#SAVED,PFILTER),CHAR,ND=Y                                
Rotn     LKOUT C,10,(D,B#SAVED,PROTN),CHAR,ND=Y                                 
NtiNo    LKOUT C,11,(D,B#SAVED,PNTINO),UBIN,ND=Y                                
Daypt    LKOUT C,12,(D,B#SAVED,PDAYPT),CHAR,ND=Y                                
Cont     LKOUT C,13,(D,B#SAVED,PCONT),CHAR,ND=Y                                 
New      LKOUT C,14,(D,B#SAVED,PNEW),CHAR,ND=Y                                  
Ptyp     LKOUT C,15,(D,B#SAVED,PTYPE),CHAR,ND=Y                                 
SPtyp    LKOUT C,16,(D,B#SAVED,SBPTYP),CHAR,ND=Y                                
DemDf    LKOUT C,17,(D,B#SAVED,PDEMDEF),CHAR,ND=Y                               
Fax      LKOUT C,18,(D,B#SAVED,PFAX),CHAR,ND=Y                                  
Windw    LKOUT C,19,(D,B#SAVED,PWINDW),CHAR,ND=Y                                
SbDayp   LKOUT C,20,(D,B#SAVED,PSUBDPT),CHAR,ND=Y                               
TcrWb1   LKOUT C,21,(D,B#SAVED,PTCRWB1),CHAR,ND=Y                               
MltRun   LKOUT C,22,(D,B#SAVED,PMLTRUN),UBIN,ND=Y                               
Mirror   LKOUT C,23,(D,B#SAVED,PMIRROR),CHAR,ND=Y                               
Tier     LKOUT C,24,(D,B#SAVED,PTIER),CHAR,ND=Y                                 
RCP      LKOUT C,25,(D,B#SAVED,PRCP),CHAR,ND=Y                                  
Flag1    LKOUT C,26,(D,B#SAVED,PFLAG1),HEXD,ND=Y                                
Method   LKOUT C,27,(D,B#SAVED,PMETHOD),CHAR,ND=Y                               
ActDat   LKOUT C,28,(D,B#SAVED,PADATE),EDAT,ND=Y                                
ActTyp   LKOUT C,29,(D,B#SAVED,PACTION),CHAR,ND=Y                               
CrDate   LKOUT C,30,(D,B#SAVED,PCRDATE),EDAT,ND=Y                               
CrPID    LKOUT C,31,(D,B#SAVED,PCRPID),CHAR,ND=Y                                
LsDate   LKOUT C,32,(D,B#SAVED,PLSDATE),EDAT,ND=Y                               
LsPID    LKOUT C,33,(D,B#SAVED,PLSPID),CHAR,ND=Y                                
TimSt    LKOUT C,34,(D,B#SAVED,PTIMSTP),UBIN,ND=Y                               
Array    LKOUT C,35,(A,ARYUSDM)                                                 
*NAVAIL  LKOUT C,40,(D,B#SAVED,HNOTAVL),UBIN,ND=Y                               
Flag2    LKOUT C,40,(D,B#SAVED,PFLAG2),HEXD,ND=Y                                
VSrc     LKOUT C,41,(D,B#SAVED,PVWSRC),CHAR,ND=Y                                
VTyp     LKOUT C,42,(D,B#SAVED,PVWTYP),CHAR,ND=Y                                
ComDef   LKOUT C,43,(D,B#SAVED,COMDEF),CHAR,ND=Y                                
CSerNum  LKOUT C,44,(D,B#SAVED,CSERNUM),CHAR,ND=Y                               
CSVtype  LKOUT C,53,(D,B#SAVED,CSVTYPE),CHAR,ND=Y                               
Array    LKOUT C,45,(A,ARYCSP)                                                  
         LKOUT E                                                                
                                                                                
OUTPDEM  LKOUT R,2                 DEMO VALUES                                  
TabId    LKOUT C,1,(D,B#SAVED,FORMTBID),CHAR,ND=Y                               
Blank    LKOUT C,3,(D,B#SAVED,PBLANK),UBIN                                      
Method   LKOUT C,4,(D,B#SAVED,PMETHOD),CHAR,ND=Y                                
Array    LKOUT C,100,(A,ARYPBBLK)                                               
HutMd    LKOUT C,106,(D,B#SAVED,HMODIF),CHAR,ND=Y                               
HutDm    LKOUT C,107,(D,B#SAVED,HDTYP4),UBIN,ND=Y                               
HutVa    LKOUT C,108,(D,B#SAVED,HVALUE),UBIN,ND=Y                               
Modif    LKOUT C,111,(D,B#SAVED,OMODIF),CHAR,ND=Y                               
Array    LKOUT C,110,(A,ARYPDMS)                                                
Array    LKOUT C,120,(A,ARYPOVS)                                                
Array    LKOUT C,125,(A,ARYCDEM)                                                
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
                                                                                
OUTDEFO  LKOUT H                   ** DEMO ENGINE DWNLD - FORMULAS **           
                                                                                
DFORM    LKOUT R,1                                                              
TblId    LKOUT C,1,(D,B#SAVED,OTABID),CHAR,ND=Y                                 
StDat    LKOUT C,2,(D,B#SAVED,OSTBOOK),EDAT,ND=Y                                
         LKOUT E                                                                
                                                                                
GMAC     LKOUT R,2                                                              
Array    LKOUT C,1,(A,ARYGMAC)     GENERAL MACRO FORMULAS                       
         LKOUT E                                                                
                                                                                
SMAC     LKOUT R,2                                                              
Array    LKOUT C,1,(A,ARYSMAC)     SPECIFIC MACRO FORMULAS                      
         LKOUT E                                                                
*&&DO                                                                           
OVRL     LKOUT R,5                                                              
Array    LKOUT C,1,(A,ARYOVRL)     OVERRIDE RELATIONAL FORMULAS                 
         LKOUT E                                                                
*&&                                                                             
BBLD     LKOUT R,3                                                              
BBMOD    LKOUT C,1,(D,B#SAVED,TABMOD),CHAR,LEN=L'MODCHAR                        
Array    LKOUT C,2,(A,ARYBLDC)     BUILDING BLOCKS FOR THIS MODIFIER            
         LKOUT E                                                                
                                                                                
COMP     LKOUT R,4                                                              
Array    LKOUT C,1,(A,ARYCOMP)     COMPUTATIONAL FORMULAS FOR MODIFIER          
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR DEMOGRAPHICS                                   *         
***********************************************************************         
                                                                                
ARYDEMOS LKOUT A,(D,B#DEMTAB,DEMOENTD),ROWWIDTH=DEMOENTL,EOT=00                 
DModf    LKOUT C,101,(D,,DMODIF),CHAR                                           
Dummy    LKOUT C,255,(D,,DEMOENTD),(R,SETDEM),ND=Y                              
Array    LKOUT C,255,(A,ARYDVALS)                                               
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR DEMOGRAPHIC VALUES                             *         
***********************************************************************         
                                                                                
ARYDVALS LKOUT A,(I,B#SAVED,ADEMVS),ROWNAME=DEMSP8,ROWWIDTH=L'DEMSP8,  *        
               EOT=FF                                                           
DemVa    LKOUT C,102,(D,,DEMSP8),SPAK,LEN=L'DEMSP8                              
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR OVERRIDE RELATIONAL FORMULAS                   *         
***********************************************************************         
                                                                                
ARYOVRL  LKOUT A,(R,NXTOVRF)                                                    
OMod     LKOUT C,1,(D,B#SAVED,OFOMD),CHAR,ND=Y                                  
OFMod    LKOUT C,2,(D,B#SAVED,OFOFMD),CHAR,ND=Y                                 
OExp     LKOUT C,3,(D,B#SAVED,OFOEXP),CHAR,ND=Y                                 
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR GENERAL MACRO FORMULAS                         *         
***********************************************************************         
                                                                                
ARYGMAC  LKOUT A,(R,NXTGMAC)                                                    
GMod     LKOUT C,1,(D,B#SAVED,OFGMD),CHAR,ND=Y                                  
GExp     LKOUT C,2,(D,B#SAVED,OFGEXP),CHAR,ND=Y                                 
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR SPECIFIC MACRO FORMULAS                        *         
***********************************************************************         
                                                                                
ARYSMAC  LKOUT A,(R,NXTSMAC)                                                    
SMod     LKOUT C,1,(D,B#SAVED,OFSMD),CHAR,ND=Y                                  
SExp     LKOUT C,2,(D,B#SAVED,OFSEXP),CHAR,ND=Y                                 
SDno     LKOUT C,3,(D,B#SAVED,OFSDNO),UBIN,ND=Y                                 
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR BUILDING BLOCK LIST                            *         
***********************************************************************         
                                                                                
ARYBLDC  LKOUT A,(R,NXTBLDBK)                                                   
BLDGB    LKOUT C,2,(D,B#SAVED,OBLDBLK),UBIN,LEN=L'DEMFB4DL                      
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR ONE COMPUTATIONAL FORMULA                      *         
***********************************************************************         
                                                                                
ARYCOMP  LKOUT A,(R,NXTCOMP),MULTIROW=Y                                         
DemNo    LKOUT C,1,(D,B#SAVED,ODEMNO),UBIN,ND=Y                                 
Array    LKOUT C,2,(A,ARYINDS)    INDEXES AS COMPONENTS OF FORMULA              
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR LIST OF INDEXES                                *         
***********************************************************************         
                                                                                
ARYINDS  LKOUT A,(R,NXTIND)                                                     
Index    LKOUT C,2,(D,B#SAVED,OINDEX),(R,EDTINDX),LEN=L'OINDEX                  
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR PROGRAM RECORD DEMOGRAPHICS                    *         
***********************************************************************         
                                                                                
ARYPBBLK LKOUT A,(I,B#SAVED,ADEMPTAB),ROWNAME=DEMPENTD,                *        
               ROWWIDTH=DEMPENTL,EOT=00                                         
DModf    LKOUT C,101,(D,,DPMODIF),CHAR                                          
Dummy    LKOUT C,255,(D,,DEMPENTD),(R,SETPDEM),ND=Y                             
Array    LKOUT C,255,(A,ARYBBVLS)                                               
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR BLDNG BLOCK VALUES FROM PROGRAM RECORD         *         
***********************************************************************         
                                                                                
ARYBBVLS LKOUT A,(I,B#SAVED,ADEMVS),ROWNAME=DEMPVAL,                   *        
               ROWWIDTH=L'DEMPVAL,EOT=FF                                        
DemVa    LKOUT C,102,(D,,DEMPVAL),UBIN,LEN=L'DEMPVAL                            
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR PROGRAM RECORD DEMOS ('92' OR '93' ELEM)       *         
***********************************************************************         
                                                                                
ARYPDMS  LKOUT A,(I,B#SAVED,APDEMS),ROWNAME=PDEMOUTD,                  *        
               ROWWIDTH=PDEMOUTL,EOT=FF                                         
DPNum    LKOUT C,112,(D,,PDEMNUM),UBIN                                          
DPVal    LKOUT C,113,(D,,PDEMSV),UBIN                                           
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR PROGRAM RECORD OVERRIDES/NADS                  *         
***********************************************************************         
                                                                                
ARYPOVS  LKOUT A,(I,B#SAVED,AFRSTEL),EOT=0,                            *        
               ROWID=(NPGELDD,DDELEMQ),ROWWIDTH=(V,NPGDELEN)                    
OMod     LKOUT C,121,(D,,NPGDMOD),(R,EDTOMOD),LEN=OMODLN                        
ONad     LKOUT C,122,(D,,NPGDCAT),UBIN,ND=Y                                     
ODem     LKOUT C,123,(D,,NPGDNUM),(R,GETYP4D),LEN=L'NPGDNUM                     
OVal     LKOUT C,124,(D,,NPGDAMT),UBIN                                          
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR USER DEMOS                                     *         
***********************************************************************         
                                                                                
ARYUSDM  LKOUT A,(I,B#SAVED,AFRSTEL),EOT=0,                            *        
               ROWID=(UDEVPD,ELEMC3),ROWWIDTH=(V,UDEVPLEN)                      
UDemo    LKOUT C,35,(D,,UDEVPNAM),CHAR                                          
UValu    LKOUT C,36,(D,,UDEVPD),(R,EDTUVAL),LEN=UVALN                           
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR COMSCORE PROGRAMS                              *         
***********************************************************************         
ARYCSP   LKOUT A,(I,B#SAVED,AFRSTEL),EOT=0,                            *        
               ROWID=(NPCPELD,NPCPELQ),ROWWIDTH=(V,NPCPLEN)                     
NetNum   LKOUT C,45,(D,,NPCPNET),CHAR                                           
Series   LKOUT C,46,(D,,NPCPSER),CHAR                                           
StartDte LKOUT C,47,(D,,NPCPSDTE),CDAT,ND=Y                                     
EndDate  LKOUT C,48,(D,,NPCPEDTE),CDAT,ND=Y                                     
RotOut   LKOUT P,NPCPROT,EDTROT                                                 
Rotation LKOUT C,49,(D,B#WORKD,DUB),CHAR,LEN=7                                  
SrceOut  LKOUT P,NPCVSRCE,EDTSRCE                                               
Source   LKOUT C,50,(D,B#WORKD,FULL1),CHAR,LEN=3                                
TimeOut  LKOUT P,NPCPSTIM,EDTTIME                                               
Times    LKOUT C,51,(D,B#SAVED,PTIMES),CHAR                                     
VTOut    LKOUT P,NPCVT,EDTVT                                                    
ViewType LKOUT C,52,(D,B#WORKD,HALF1),CHAR                                      
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR COMSCORE DEMOS                                 *         
***********************************************************************         
ARYCDEM  LKOUT A,(I,B#SAVED,AFRSTEL),EOT=0,                            *        
               ROWID=(NPGCELDD,NPGCELQ),ROWWIDTH=(V,NPGCELEN)                   
Category LKOUT C,125,(D,,NPGCCAT),CHAR                                          
OVal     LKOUT C,126,(D,,NPGCAMT),UBIN                                          
         LKOUT E                                                                
                                                                                
         LKARY T                                                                
                                                                                
                                                                                
         EJECT                                                                  
         PRINT NOGEN                                                            
SAVED    DSECT                     ** SERVER SAVED W/S **                       
                                                                                
DDELEMQ  EQU   X'DD'              'DD' ELEMENT CODE                             
                                                                                
WVALUES  DS    0X                  ** LOCAL LITERAL VALUES **                   
ADCONS   DS    0A                                                               
ASUMBUF  DS    A                                                                
ADAYTAB  DS    A                                                                
AQTRTAB  DS    A                                                                
ALVDFTAB DS    A                                                                
ATABLESL DS    A                                                                
AVIEWTYP DS    A                                                                
ANADDERV DS    A                                                                
ADCONSN  EQU   (*-ADCONS)/L'ADCONS                                              
F100     DS    F                                                                
F30      DS    F                                                                
F0       DS    F                                                                
H60      DS    H                                                                
DTYPE    DS    CL8                                                              
DPTYP4   DS    CL8                                                              
DDAY     DS    CL8                                                              
DTIME    DS    CL8                                                              
DNTI     DS    CL8                                                              
DPROG25  DS    CL8                                                              
DWEEK    DS    CL8                                                              
DNTIL    DS    CL8                                                              
DBKOUT   DS    CL8                                                              
DGAA     DS    CL8                                                              
DFEED    DS    CL8                                                              
DCOMS    DS    CL8                                                              
DCOMT    DS    CL8                                                              
DNLIV    DS    CL8                                                              
DPREM    DS    CL8                                                              
P0       DS    PL1                                                              
PL80     DS    PL8                                                              
H600AM   DS    H                                                                
H559AM   DS    H                                                                
H6A545A  DS    AL4                                                              
CAB      DS    CL3                                                              
CB2      DS    CL3                                                              
HUT      DS    CL4                                                              
WY       DS    CL2                                                              
WU       DS    CL2                                                              
WB       DS    CL2                                                              
NLIVEXT  DS    CL9                                                              
CAVGEXT  DS    CL9                                                              
WVALUEL  EQU   *-WVALUES                                                        
                                                                                
RELO     DS    A                   RELOCATION FACTOR                            
ADEMPTAB DS    A                   A(DEMPTAB)                                   
AFRSTEL  DS    A                   A(FIRST ELEMENT)                             
APDEMS   DS    A                   A(OUTPUT PROG DEMOS)                         
AMASTC   DS    A                   A(MASTC)                                     
ANADWKS  DS    A                                                                
ANTIQTAB DS    A                                                                
                                                                                
TABID    DS    CL(L'MFTABID)       ** VARIOUS SAVE AREAS **                     
DDISPLN  DS    H                   LENGTH OF DEMDISP TABLE ENTRY                
UNIVMISS DS    X                                                                
DEMOLIST DS    (MAXDEMQ)XL3                                                     
DEMLSTX  DS    XL1                 FOR FF WHEN TABLE IS FULL                    
RECENTBK DS    XL2                 MOST RECENT BOOK IN THE REQUEST              
FORMFMS  DS    CL3                 FILE,MEDIA,SOURCE TO LOOK FOR                
FORMBK   DS    XL2                 BOOK TO LOOK FOR IN FORMULA TABLES           
MODIFIER DS    C                   MODIFIER OF DEMO TO BE EXTRACTED             
DEMONUM  DS    X                   DEMO NUMBER OF DEMO TO BE EXTRACTED          
DATADISP DS    H                                                                
ELCODE   DS    X                                                                
NETCODE  DS    HL2                 BINARY NETWORK CODE                          
KEYTEMP  DS    XL(L'NPGKEY)                                                     
                                                                                
SPLDEMO  DS    0AL4                                                             
SPLVALU  DS    AL4                                                              
SPLUNIV  DS    AL4                                                              
                                                                                
DTABLES  DS    0F                                                               
ADEMDISP DS    A                   A(DEMDISP TABLES)                            
AAEFORMS DS    A                   A(NEW FORMULA TABLES FOR AE)                 
TABEOT   DS    X                   EOT                                          
DTABLELQ EQU   *-DTABLES                                                        
                                                                                
AUNVOUT  DS    A                   A(UNIVERSE OUTPUT AREA)                      
ABOOKFRM DS    A                   A(FORMULAS FOR A BOOK/FILE/MED/SRC)          
ADEMVALU DS    A                   A(DEMO VALUE TO BE EXTRACTED)                
ABLDBLKS DS    A                   A(BUILDING BLOCKS TABLE)                     
ACOMPUTS DS    A                   A(TABLE OF COMPUTATIONAL FORMULAS)           
AINDEXES DS    A                   A(LIST OF INDEXES IN THE FORMULA)            
AFGMACRO DS    A                   A(GENERAL MACROS TABLE)                      
AFSMACRO DS    A                   A(SPECIFIC MACROS TABLE)                     
AFOVRELS DS    A                   A(OVERRIDE RELATIONAL FORMULAS)              
ALASTCMP DS    A                   A(LAST COMP FORMULA ENTRY)                   
ALASTOV  DS    A                   A(LAST OVERRIDE ELEMENT)                     
ALASTEL  DS    A                   A(LAST ELEMENT)                              
ALASTBLD DS    A                   A(LAST BUILDING BLOCK)                       
ALASTSMC DS    A                   A(LAST SPECIFIC MACRO FORMULA)               
ALASTGMC DS    A                   A(LAST SPECIFIC MACRO FORMULA)               
ALASTOVR DS    A                   A(LAST OVERRIDE RELATIONAL FORMULA)          
ALASTIND DS    A                   A(LAST INDEX)                                
                                                                                
SAVECLR  DS    0X                  ** START OF CLEARED AREA **                  
                                                                                
TSARBLK  DS    XL(TSARDL)          TSAR BLOCK                                   
SUMKEYSV DS    XL(SUMKEYL)         BUFFER RECORD KEY SAVE AREA                  
                                                                                
REQVALS  DS    0F                  ** REQUEST VALUE POINTERS **                 
DATIND   DS    X                   DATES INDICATOR                              
ADAT     DS    AL3                 A(DATES)                                     
DYTMIND  DS    X                   DAYS/TIMES INDICATOR                         
ADYTM    DS    AL3                 A(DAYS/TIMES)                                
NETIND   DS    X                   NETWORKS INDICATOR                           
ANET     DS    AL3                 A(NETWORKS)                                  
NADIND   DS    X                   NAD-CATEGORIES INDICATOR                     
ANAD     DS    AL3                 A(NAD CATEGORIES)                            
SUMIND   DS    X                   SUMMARY-LEVELS INDICATOR                     
ASUM     DS    AL3                 A(SUMMARY LEVELS)                            
MFIDIND  DS    X                   MAINFRAME-ID'S INDICATOR                     
AMFID    DS    AL3                 A(MAINFRAME ID'S)                            
PNAMIND  DS    X                   PROGRAM-NAME-FILTER INDICATOR                
APNAM    DS    AL3                 A(PROGRAM NAME FILTER)                       
NTININD  DS    X                   NTI-NUMBERS INDICATOR                        
ANTIN    DS    AL3                 A(NTI NUMBERS LIST)                          
IPTYIND  DS    X                   INCLUDED-PROG-TYPES INDICATOR                
AIPTY    DS    AL3                 A(INCLUDED PROGRAM TYPES)                    
EPTYIND  DS    X                   EXCLUDED-PROG-TYPES INDICATOR                
AEPTY    DS    AL3                 A(EXCLUDED PROGRAM TYPES)                    
ISTYIND  DS    X                   INCLUDED-SUB-PROG-TYPES INDICATOR            
AISTY    DS    AL3                 A(INCLUDED SUB-PROGRAM TYPES)                
ESTYIND  DS    X                   EXCLUDED-SUB-PROG-TYPES INDICATOR            
AESTY    DS    AL3                 A(EXCLUDED SUB-PROGRAM TYPES)                
CONTIND  DS    X                   EXCLUDED-CONTENT-TYPES INDICATOR             
ACONT    DS    AL3                 A(EXCLUDED CONTENT TYPES)                    
AIRNIND  DS    X                   EXCLUDED-AIRING-TYPES INDICATR               
AAIRN    DS    AL3                 A(EXCLUDED AIRING TYPES)                     
TBIDIND  DS    X                   TABLE ID INDICATOR                           
ATBID    DS    AL3                 A(TABLE ID LIST)                             
PCDIND   DS    X                   PROGRAM CODE INDICATOR                       
APCOD    DS    AL3                 A(PROGRAM CODE LIST)                         
PDPIND   DS    X                   PROGRAM RECD DAYPART INDICATOR               
APDPT    DS    AL3                 A(PROGRAM RECD DAYPART LIST)                 
                                                                                
IMEDIA   DS    C                   MEDIA                                        
ISOURCE  DS    C                   SOURCE                                       
                                                                                
ICALEN   DS    C                                                                
CALENBRD EQU   C'B'                BROADCAST CALENDAR                           
CALENNTI EQU   C'N'                NTI CALENDAR                                 
                                                                                
IGAA     DS    C                                                                
GAAONLY  EQU   C'Y'                ONLY GAA                                     
GAANONE  EQU   C'N'                NO GAA, JUST REGULAR (DEFAULT)               
GAABOTH  EQU   C'B'                BOTH GAA AND REGULAR                         
                                                                                
IBKOT    DS    C                                                                
BKOUT    EQU   C'B'                EXCLUDE BREAKOUTS                            
                                                                                
IROTN    DS    C                                                                
ROTNY    EQU   C'Y'                RETURN ROTATION DATA                         
                                                                                
IVIEWT   DS    C                   TIME SHIFTED REQUEST FLAVORS                 
TSLIV    EQU   C'L'                LIVE                                         
TSLSD    EQU   C'1'                LIVE+SD                                      
TSLP7    EQU   C'7'                LIVE+7                                       
TSLP1    EQU   C'0'                LIVE+1                                       
TSLP2    EQU   C'2'                LIVE+2                                       
TSLP3    EQU   C'3'                LIVE+3  PROG DATA FROM ACM TAPES             
TSALV    EQU   C'A'                LIVE    PROG DATA FROM ACM TAPES             
TSALS    EQU   C'B'                LIVE+SD PROG DATA FROM ACM TAPES             
TSAL7    EQU   C'C'                LIVE+7  PROG DATA FROM ACM TAPES             
TSLN3    EQU   C'D'                LIVE+3  NTI LIVE+3                           
                                                                                
IVCR     DS    C                   VCR OPTION                                   
VCRINCL  EQU   C'Y'                INCLUDE VCR                                  
VCREXCL  EQU   C'N'                EXCLUDE VCR                                  
                                                                                
IACM     DS    C                   AVERAGE COMMERCIAL FLAG                      
ACMYES   EQU   C'Y'                GET AVG COMMERCIAL DATA                      
ACMNO    EQU   C'N'                GET PROGRAM AVG/TP DATA                      
                                                                                
IMWT     DS    C                                                                
MWTYES   EQU   C'Y'                WEIGHT ACM ON PROGRAM MINUTES                
MWTNO    EQU   C'N'                WEIGHT ACM ON COMMERCIAL SECS(DEFLT)         
                                                                                
IPREM    DS    C                                                                
PREMYES  EQU   C'Y'                FILTER PREMIER DATA ONLY                     
PREMNO   EQU   C'N'                NO PREMIER DATA FILTER                       
                                                                                
IMWSRC   DS    C                   MONTHLY/WEEKLY NAD SOURCE                    
ISMTLYQ  EQU   C'M'                MONTHLY                                      
ISWKLYQ  EQU   C'W'                WEEKLY                                       
                                                                                
INADSW   DS    C                   NAD SWITCH                                   
INSEST   EQU   C'E'                ESTIMATE-STYLE                               
INSRES   EQU   C'R'                RESEARCH-STYLE                               
                                                                                
IMNAIR   DS    HL4                 MIN NO OF AIRINGS                            
IMNDUR   DS    HL1                 MIN DURATION                                 
IMXDUR   DS    HL4                 MAX DURATION                                 
IPSDATE  DS    CL6                 PERIOD START DATE YYMMDD                     
IPEDATE  DS    CL6                 PERIOD END DATE YYMMDD                       
IPFILTR  DS    CL3                 PROGRAM RECORD FILTER                        
IPVWSRC  DS    C                   PROGRAM RECORD VIEWING SOURCE                
IPVWTYP  DS    C                   PROGRAM RECORD VIEWING TYPE                  
                                                                                
REQVALSL EQU   *-REQVALS                                                        
                                                                                
OUTVALS  DS    0X                                                               
                                                                                
ORECORD  DS    0X                                                               
NETWORK  DS    CL(NETLN)                                                        
STWEEK   DS    CL6                 YYMMDD                                       
ENDWEEK  DS    CL6                 YYMMDD                                       
WEEK     DS    CL6                 YYMMDD                                       
MONTH    DS    CL6                 MMM/YY                                       
QUART    DS    CL5                 Qn/YY                                        
NTINO    DS    CL5                                                              
LNTINO   DS    CL10                                                             
PGNAME   DS    CL(PNAMLN)                                                       
PTYPE    DS    CL2                                                              
         DS    C                   THIS IS TO ACCOMODATE 'N/A'                  
         ORG   PTYPE               PROGRAM TYPE NOT AVAILABLE                   
PTYPE3   DS    CL3                 'N/A' IS 3 CHARS INSTEAD OF 2                
SBPTYP   DS    CL(STYPLN)                                                       
DAY      DS    CL10                                                             
STIME    DS    CL5                 HH:MM                                        
ETIME    DS    CL5                 HH:MM                                        
RDATE    DS    CL6                 YYMMDD                                       
NAIRED   DS    HL4                                                              
TOTDUR   DS    HL4                                                              
WEIGHT   DS    HL4                                                              
COMSEC   DS    HL4                                                              
COMNUM   DS    HL4                                                              
DAYPART  DS    CL(DTSTRLN)                                                      
TRACK    DS    CL5                                                              
TCAST    DS    CL6                                                              
NADCAT   DS    HL1                                                              
MODIF    DS    CL(L'DMODIF)                                                     
PCODE    DS    CL(PRGCDLN)                                                      
PEDATE   DS    CL6                 YYMMDD END DATE                              
PSDATE   DS    CL6                 YYMMDD START DATE                            
PTIMES   DS    CL11                PROGRAM RECORD TIMES                         
PPNAME   DS    CL16                PROGRAM RECORD PROGRAM NAME                  
RSHRFLD  DS    CL7                 PROGRAM RECORD RATING/SHARE FIELD            
PFILTER  DS    CL3                 PROGRAM RECORD FILTER                        
PROTN    DS    CL16                PROGRAM RECORD ROTATION                      
PNTINO   DS    HL2                 PROGRAM RECORD NTI NUMBER                    
PDAYPT   DS    CL2                 PROGRAM RECORD DAYPART                       
PCONT    DS    CL2                 PROGRAM RECORD CONTENT CODE                  
PNEW     DS    CL1                 PROGRAM RECORD NEW STATUS                    
PDEMDEF  DS    CL6                 PROGRAM RECORD DEMDEF CODE                   
PFAX     DS    CL12                PROGRAM RECORD FAX NUMBER                    
PWINDW   DS    CL4                 PROGRAM RECORD WINDOW                        
PSUBDPT  DS    CL3                 PROGRAM RECORD SUB-DAYPART                   
PTCRWB1  DS    CL2                 PROGRAM RECORD TCAR/WB1 VALUE                
PMLTRUN  DS    HL1                 PROGRAM RECORD MULTI RUN COUNT               
PMIRROR  DS    CL1                 PROGRAM RECORD MIRROR CODE                   
PTIER    DS    CL1                 PROGRAM RECORD TIER                          
PRCP     DS    CL1                 PROGRAM RECORD RCP                           
PFLAG1   DS    X                   PROGRAM RECORD FLAG 1 (NPGAIFLG)             
PFLAG2   DS    X                   PROGRAM RECORD FLAG 2 (VARIOUS)              
PF2HNOT  EQU   X'80'               HUT NOT AVAILABLE INDICATOR                  
PF2AE    EQU   X'40'               AE-RECD (EXCL AE-RECS CHGD FROM MF)          
PF2CHUT  EQU   X'20'               INDICTR THAT HUT WAS COMPTD FROM SHR         
PMETHOD  DS    CL1                 PROGRAM RECORD METHODOLOGY                   
PADATE   DS    CL6                 YYMMDD LAST ACTIVITY DATE                    
PACTION  DS    CL1                 LAST ACTION (A=ADD,C=CHANGE)                 
PCRDATE  DS    CL6                 NEWER RECDS ONLY - CREATION DATE             
PCRPID   DS    CL8                 (AFTER FEB/06)     CREATION PID              
PLSDATE  DS    CL6                                    LAST CHG DATE             
PLSPID   DS    CL8                                    LAST CHG PID              
PTIMSTP  DS    AL4                                    TIME STAMP                
PUDEMO   DS    CL7                 USER DEMO NAME                               
PUVALU   DS    HL2                 USER DEMO VALUE                              
PBLANK   DS    HL1                 BLANK IDENTIFIER NEEDED BY PC DEMENG         
PREMIND  DS    X                   PREMIER INDICATOR                            
PGHOMES  DS    AL4                 SAVED YHOMES VALUE FROM PROG RECORD          
PGSHARE  DS    AL2                 SAVED SHARE VALUE FROM PROG RECORD           
PVWSRC   DS    C                   PROGRAM RECD VIEWING SOURCE                  
PVWTYP   DS    C                   PROGRAM RECD VIEWING TYPE                    
HUTOUT   DS    0XL7                                                             
HMODIF   DS    C                   HUT MODIFIER                                 
HDTYP4   DS    HL2                 HUT DEMO NUMBER                              
HVALUE   DS    AL4                 HUT VALUE                                    
ACMIND   DS    X                   ACM INDOCATOR                                
VTYP     DS    C                   VIEWING TYPE                                 
COMDEF   DS    CL(L'NPGCDEF)       COMDEF                                       
CSERNUM  DS    CL(L'NPGCSN)        COMSCORE SERIES NUMBER                       
CSVTYPE  DS    CL2                 COMSCORE VIEWING TYPE                        
                                                                                
FLAG1    DS    X                   * INDICATOR FLAG 1 *                         
GAAQ     EQU   X'01'               IS GAA (FOR SYNDICATION ONLY)                
BKOTQ    EQU   X'02'               IS BREAKOUT                                  
PREMQ    EQU   X'04'               IS PREMIER                                   
                                                                                
MFID     DS    XL(MFIDLN*2)                                                     
FORMTBID DS    CL(L'DEMFFTID)                                                   
CONTENT  DS    CL(CONTLN)                                                       
AIRNSMS  DS    CL(AIRNLN)                                                       
UHOMES   DS    AL4                                                              
OPCID    DS    CL(PCIDLN)                                                       
                                                                                
DEMOTAB  DS    (MAXMODQ)XL(DEMOENTL)  AREA FOR MODIFS AND DEMO VALUES           
DEMOTABL EQU   *-DEMOTAB                                                        
DEMOTABX DS    XL1                    FOR FF WHEN TABLE IS FULL                 
         ORG   DEMOTAB                                                          
DEMPTAB  DS    (MAXMODQ)XL(DEMPENTL)  AREA FOR PROGRAM RECORD DEMOS             
DEMPTABL EQU   *-DEMPTAB                                                        
DEMPTABX DS    XL1                    FOR FF WHEN TABLE IS FULL                 
         ORG                                                                    
                                                                                
ADEMVS   DS    A                                                                
                                                                                
ORECLN   EQU   *-ORECORD                                                        
                                                                                
OMODTAB  DS    (MAXMODQ)CL2        LIST OF MODIFIERS                            
OMODTABX DS    X                   EOT                                          
ODEMNO   DS    AL2                                                              
OTABID   DS    CL(L'DEMFFTID)      TABLE ID                                     
OSTBOOK  DS    CL6                 START BOOK OF FORMULA TABLE YYMMDD           
INDXS#   DS    XL2                 NUMBER OF INDEXES IN A FORMULA LIST          
TABMOD   DS    CL(L'DEMFM2MD)      MODIFIER FROM FORMULA TABLES                 
OMODIF   DS    C                   OUTPUT MODIFIER                              
                                                                                
OBLDBLK  DS    XL(L'DEMFB4DL)      OUTPUT BUILDING BLOCK TYPE 4                 
OFSMD    DS    CL(L'DEMFSMD)       OUTPUT SPECIFIC MACRO MODIFIER               
OFSEXP   DS    CL(L'DEMFSEXP)      OUTPUT SPECIFIC MACRO EXPRESSION             
OFSDNO   DS    AL(L'DEMFSDNO)      OUTPUT SPECIFIC MACRO DEMO NUMBER            
OFGMD    DS    CL(L'DEMFGMD)       OUTPUT GENERAL MACRO MODIFIER                
OFGEXP   DS    CL(L'DEMFGEXP)      OUTPUT GENERAL MACRO EXPRESSION              
OFOMD    DS    CL(L'DEMFOMD)       OUTPUT OVERRIDE RESULT MODIFIER              
OFOFMD   DS    CL(L'DEMFOFMD)      OUTPUT OVERRIDE FROM MODIFIER                
OFOEXP   DS    CL(L'DEMFOEXP)      OUTPUT OVERRIDE EXPRESSION                   
OINDEX   DS    XL(L'DEMFDILS)      OUTPUT INDEX NUMBER                          
                                                                                
OUTVALSL EQU   *-OUTVALS                                                        
                                                                                
                                                                                
PRVVALS  DS    0X                                                               
PREVNET  DS    CL(L'NETWORK)                                                    
PREVPCID DS    CL(L'OPCID)                                                      
PRVVALSL EQU   *-PRVVALS                                                        
                                                                                
SVDUR    DS    XL(L'RECDUR)        SAVED WEIGHT/DURATION                        
SVCMSEC  DS    AL4                 SAVED WEIGHT/COMMERCIAL SECONDS              
                                                                                
         DS    0F                                                               
DEMS4    DS    (MAXDEMQ)XL4        RAW DEMOGRAPHICS FROM FILE                   
DEMS4X   DS    0X                                                               
                                                                                
MULTREAD DS    X                   MULTIREAD FLAG FOR SPANNING RECORDS          
MUTRDNO  EQU   0                   FIRST RECORD                                 
MUTRDYES EQU   1                   CONTINUATION RECORDS                         
                                                                                
PRECFLAG DS    X                   RATING PRECISION FLAG ON PROG RECORD         
PREC1DEC EQU   X'01'               1 DECIMAL PRECISION                          
PREC2DEC EQU   X'02'               2 DECIMAL PRECISION                          
                                                                                
       ++INCLUDE DEDBLOCK                                                       
DBLOCKL  EQU   *-DBLOCK                                                         
                                                                                
MATHFACS DS    0F                  DEMOMATH BLOCK                               
MATHABLK DS    A                   A(DBLOCK)                                    
MATHFCTR DC    F'0'                WEIGHTING FAVTOR                             
MATHIFIL DS    CL3                 INPUT FILE FORMAT                            
MATHOFIL DS    CL3                 OUTPUT FILE FORMAT                           
MATHOSRC DS    CL3                 OUTPUT SOURCE FORMAT                         
MATHFACL EQU   *-MATHFACS                                                       
                                                                                
MYSAVE   DS    0A                                                               
MYNETPTR DS    A                   POINTER TO NETWORK                           
MYDATPTR DS    A                   POINTER TO DATE RANGE                        
MYDTBPTR DS    A                   POINTER TO DAY/TIME BLOCK                    
MYDTPTR  DS    A                   POINTER TO DAY/TIME WITHIN BLOCK             
MYMIDPTR DS    A                   POINTER TO MAINFRAME ID                      
MYNTIPTR DS    A                   POINTER TO NTI NUMBER                        
MYNADPTR DS    A                   POINTER TO NAD CATEGORY                      
MYPRGPTR DS    A                   POINTER TO PROGRAM CODE                      
MYEDTPTR DS    A                   POINTER TO END DATE                          
MYNETCTR DS    AL(L'LW_NUMN)       NO OF NETWORKS LEFT                          
MYDATCTR DS    AL(L'LW_NUMN)       NO OF DATE-RANGES LEFT                       
MYMIDCTR DS    AL(L'LW_NUMN)       NO OF MAINFRAME ID'S LEFT                    
MYDTBCTR DS    AL(L'LW_NUMN)       NO OF DAY/TIME BLOCKS LEFT                   
MYNTICTR DS    AL(L'LW_NUMN)       NO OF NTI NUMBERS LEFT                       
MYNADCTR DS    AL(L'LW_NUMN)       NO OF NAD CATEGORIES LEFT                    
MYPRGCTR DS    AL(L'LW_NUMN)       NO OF PROGRAM CODES LEFT                     
MYEDTCTR DS    AL(L'LW_NUMN)       NO OF END DATES LEFT                         
MYNET    DS    CL4                                                              
MYBOOK   DS    XL2                                                              
MYDAY    DS    X                                                                
MYTIME   DS    0HL4                                                             
MYSTIM   DS    HL2                                                              
MYETIM   DS    HL2                                                              
MYNTI    DS    XL2                                                              
MYMEDIA  DS    C                                                                
MYSOURCE DS    X                                                                
MYNAD    DS    HL1                                                              
MYPROG   DS    CL(PRGCDLN)                                                      
MYSAVELN EQU   *-MYSAVE                                                         
                                                                                
DVLEN    DS    HL1                 LENGTH OF ONE DEMO VALUE IN ELEMENT          
ADISPTBL DS    A                   POINTER TO DEMDISP TBLE FOR F/M/S/BK         
ADISPEND DS    A                   POINTER TO NEXT DEMDISP HEADER               
AELEND   DS    A                   POINTER TO END OF DEMO ELEMENT               
AFORMTBL DS    A                   POINTER TO BEGINNING OF FORMULA TBLE         
AFRMBBK  DS    A                   POINTER TO BUILDING BLOCKS FOR MODIF         
                                                                                
SAVECLRL EQU   *-SAVECLR                                                        
                                                                                
USETYP   DS    X                                                                
USEMFID  EQU   1                                                                
USEINP   EQU   2                                                                
                                                                                
NUMDEMOS DS    HL2                                                              
*NTIHOMES DS    F                                                               
NADHOMES DS    F                                                                
PVNADMN  DS    XL2                                                              
                                                                                
RECINFO  DS    0X                  INFORMATION FROM DEMO RECORD                 
RECNET   DS    CL(L'DBSELSTA)      NETWORK                                      
RECBOOK  DS    CL(L'DBSELBK)       BOOK                                         
RECPTYP  DS    CL(PTYPLN)          PROGRAM TYPE                                 
RECSPTYP DS    CL(STYPLN)          SUB-PROGRAM TYPE                             
RECIND   DS    0CL3                INDICATORS                                   
RECCONT  DS    C                   -CONTENT TYPE                                
RECAIRNG DS    C                   -AIRING TYPE                                 
RECFLAG  DS    X                   -SEE FLAG1 FOR VALUES                        
RECDAYS  DS    X                   DAYS                                         
RECTIMES DS    0A                  TIMES                                        
RECSTIME DS    H                   START TIME                                   
RECETIME DS    H                   END TIME                                     
RECNTI   DS    XL(NTINLN)          NTI NUMBER                                   
RECLNTI  DS    CL(L'LNTINO)        LONG NTI NUMBER                              
RECTRACK DS    HL2                 CABLE TRACK NUMBER                           
RECTCAST DS    HL4                 CABLE TELECAST NUMBER                        
RECPNAME DS    CL(PNAMLN)          PROGRAM NAME                                 
REC5E    DS    CL5                 CONTROL INFO FROM 5E ELEMENT                 
RECHUNIV DS    F                   HOME UNIVERSE                                
RECAIR   DS    F                   NO OF AIRINGS                                
RECDUR   DS    F                   DURATION                                     
RECFEED  DS    C                   FEED                                         
RECCMSEC DS    HL2                 CONTRIBUTING COMMERCIAL SECONDS              
RECCMNUM DS    HL1                 NUM OF COMMERCIAL TELECASTS                  
RECACMI  DS    X                   ACM INDICATOR                                
ACMYESQ  EQU   1                   IS ACM DATA                                  
RECVTYP  DS    X                   VIEWING TYPE INDICATOR (DBXLIVE)             
RECINFOL EQU   *-RECINFO                                                        
                                                                                
TSARREC  DS    XL(SUMRECL)         TSAR RECORD                                  
         EJECT                                                                  
                                                                                
SUMRECD  DSECT                     BUFFER SUMMARY RECORDS                       
                                                                                
SUMKEY   DS    0X                  - KEY -                                      
SUMMFID  DS    XL(MFIDLN)          KEY MATCHES MAINFRAME-ID UP TO PNAME         
SUMHUNIV DS    AL4                 HOME UNIVERSE                                
SUMPNAME DS    CL25                PROGRAM NAME                                 
SUMNAD   DS    HL1                 NAD CATEGORY                                 
SUMKEYL  EQU   *-SUMKEY                                                         
                                                                                
SUMDATA  DS    0X                                                               
SUMDESCR DS    0X                  - DESCRIPTIVE INFORMATION -                  
SUMLNTI  DS    CL(L'LNTINO)        LONG NTI NUMBER                              
SUMUSBK  DS    XL2                 UNIVERSE START BOOK                          
SUMUEBK  DS    XL2                 UNIVERSE END BOOK                            
SUMACMI  DS    X                   ACM INDICATOR                                
SUMVTYP  DS    X                   VIEWING TYPE                                 
                                                                                
SUMACCS  DS    0X                  - VARIOUS ACCUMULATORS -                     
SUMNOAIR DS    HL4                 NUMBER OF AIRINGS                            
SUMTODUR DS    HL4                 TOTAL DURATION                               
SUMCMSEC DS    HL4                 TOTAL COMMERCIAL SECONDS                     
SUMCMNUM DS    HL4                 TOTAL COMMERCIAL TELECASTS                   
*SUMNTIHO DS    PL8                 NTI HOMES (WEIGHTED)                        
SUMNADHO DS    PL8                 NAD HOMES (WEIGHTED)                         
SUMACCSL EQU   *-SUMACCS                                                        
                                                                                
SUMDEMOS DS    XL(DEMOTABL)        TOTAL SUM OF DEMO VALUES                     
                                                                                
SUMDATAL EQU   *-SUMDATA                                                        
SUMRECL  EQU   *-SUMRECD                                                        
                                                                                
DEMOENTD DSECT                     DSECT DEFINES ONE ENTRY IN DEMOTAB           
DMODIF   DS    CL2                 DEMO MODIFIER                                
DEMCOUNT DS    HL2                 NUMBER OF DEMOS OF THIS TYPE                 
DEMSP8   DS    (MAXDEMQ)PL8        PACKED 8 DEMO VALUES                         
DEMSP8X  DS    X                   ROOM FOR 'FF' WHEN MAX NO OF DEMOS           
DEMOENTL EQU   *-DEMOENTD                                                       
                                                                                
DEMPENTD DSECT                     DSECT DEFINES ONE ENTRY IN DEMOTAB           
DPMODIF  DS    CL1                 DEMO MODIFIER                                
DEMPVAL  DS    (MAXDEMQ)AL4        BINARY DEMO VALUES                           
DEMPP8X  DS    X                   ROOM FOR 'FF' WHEN MAX NO OF DEMOS           
DEMPENTL EQU   *-DEMPENTD                                                       
                                                                                
MODTABD  DSECT                     DSECT FOR MODIFIER TABLE                     
MODCHAR  DS    CL(L'DEMFM2MD)                                                   
                                                                                
DAYTABD  DSECT                     DSECT FOR DAY TABLE                          
DAYTXDAY DS    X                   DAY IN HEX FORMAT                            
DAYTCDAY DS    CL10                DAY IN CHARACTER FORMAT                      
DAYTABL  EQU   *-DAYTABD                                                        
                                                                                
LVDFTABD DSECT                     DSECT FOR LEVEL DEFAULT TABLE                
LVDLEVEL DS    CL(SUMLN)           LEVEL TO SUMMARIZE ON                        
LVDFIELD DS    AL4                 DISPLACEMET TO FIELD TO FILL IN              
LVDLEN   DS    AL1                 LENGTH OF FIELD                              
LVDFTABL EQU   *-LVDFTABD                                                       
                                                                                
VIEWTABD DSECT                     DSECT FOR TABLE OF VIEWING TYPES             
VWTINP   DS    C                   INPUT VIEWING TYPE (FROM PC)                 
VWTOUT   DS    C                   OUTPUT FOR DBEXTEND                          
VWTPROG  DS    C                   VIEWING TYPE ON PROGRAM RECORD               
VWTDEMR  DS    C                   VIEWING TYPE ON DEMO RECORD                  
VWTLEN   EQU   *-VIEWTABD                                                       
                                                                                
NADDERVD DSECT                     DSECT FOR TABLE OF DERIVED DEMOS             
NDDMOD   DS    C                   MODIFIER                                     
NDDSPL   DS    AL2                 DEMO TO SPLIT                                
NDDNUM   DS    AL1                 NUM OF PIECES TO SPLIT INTO                  
NDDDEMS  DS    0AL1                START OF DEMOS TO SPLIT INTO                 
                                                                                
PDEMOUTD DSECT                     DSECT FOR OUTPUT PROGRAM DEMOS               
PDEMNUM  DS    HL2                 DEMO TYPE4 NUMBER                            
PDEMSV   DS    XL(PDMLNQ)          DEMO VALUE                                   
PDEMOUTL EQU   *-PDEMOUTD                                                       
                                                                                
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NELNKWRK                                                       
       ++INCLUDE DDBUFFD                                                        
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE DEDBEXTRAD                                                     
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE NEGENUSER                                                      
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FASSB                                                          
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031NELNK11   10/31/17'                                      
         END                                                                    
